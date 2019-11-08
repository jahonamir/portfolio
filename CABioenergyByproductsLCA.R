##This code was written by Hanna Breunig and Jahon Amirebrahimi (Amir)
# Uses Breunig et al inventory to calculate byproduct yields, 
# This code determines total biochar, digestate, and composted digestate yields (BDT/yr), the carbon in the byproducts (both labile and recalcitrant), the amount of C from the byproduct that remains in soils after 100 years (or other study lifetime chosen), and the emissions that result from composting digestate (note the CO2 emission is biogenic so not a true source), transporting byproducts to grasslands, tilling byproducts, decay of byproducts (note the CO2 emission is biogenic so not a true source), and soil response including increase SOC from improved above and belowground NPP (true C sequestration). This is an attributional LCA that starts at byproduct existence (not comparing disposal scenario to disposal scenarios for the original biomass residue). The analysis does not subtract emissions from other end of life options for byproducts such as the disposal in landfills or burning of biochar. The analysis also does not assume the land treated with these materials are fertilized (no displacement of fertilizer) or grazed on (no avoided emissions from switching from feed to forage and impact on enteric emissions).

# Install packages from CRAN.
install.packages(c("tidyverse", "readxl", "reshape2", "stringr", "rstudioapi", "udunits2", "dplyr","ggplot2"))

# Hide the many messages and possible warnings from loading all these packages
suppressMessages(suppressWarnings({  
  library(tidyverse)        # Data cleaning
  library(readxl)           # 
  library(reshape2)         # 
  library(stringr)          # 
  library(rstudioapi)       # 
  library(udunits2)         # 
  library(dplyr)            #
  library(ggplot2)
}))


#set wd to bioenergy_byproduct_model folder

setwd()

#read in biomass inventory from Breunig et al

biomass.inventory <- read.csv('data/biomass.inventory.csv')


#load Agile LCA model

source('model.config.R')
source('input_output.R')

#load scenario guiding csv

scenarios <- read.csv('byproduct.scenario.csv', header = T)

#add distances in feet from county centroid to california rangeland. rangeland shapefile sourced from https://databasin.org/datasets/4e24786c97f04bdf8f91d0fc16013a88
distances <- read.csv('data/distance.csv', header = T)
suppressMessages(suppressWarnings({biomass.inventory <- left_join(biomass.inventory, distances, by = 'COUNTY')}))
  
#renames biomass categories for MSW and biosolids based on biomass feedstock attribute
biomass.inventory$biomass.category <- as.character(biomass.inventory$biomass.category)
biomass.inventory$biomass.category <- ifelse(test = 
                                               biomass.inventory$biomass.feedstock == "FOOD" | 
                                               biomass.inventory$biomass.feedstock == "FOG" |
                                               biomass.inventory$biomass.feedstock == "OTHER",
                                             yes = 'wet ofmsw',
                                             no = ifelse(test = biomass.inventory$biomass.feedstock == "LUMBER" | 
                                                           biomass.inventory$biomass.feedstock == "PAPER" |
                                                           biomass.inventory$biomass.feedstock == "CARDBOARD",
                                                         yes = 'dry ofmsw',
                                                         no = biomass.inventory$biomass.category))

biomass.inventory$biomass.category <- ifelse(test = biomass.inventory$biomass.feedstock == "GREEN",
                                             yes = 'Green Waste',
                                             no = ifelse(test = biomass.inventory$biomass.feedstock == "Biosolids [Avg Flow]",
                                                         yes = 'Biosolids [Avg Flow]',
                                                         no = ifelse(biomass.inventory$biomass.feedstock == "Biosolids [Flow Design]",
                                                                     yes = "Biosolids [Flow Design]",
                                                                     no = biomass.inventory$biomass.category)))


#above code does not affect biomass feedstock types for high recycling scenarios which have .hr after name. Therefore they still have /
# unchanged biomass category name. This section changes the name, then removes the high recycling scenario values.

biomass.inventory$biomass.category <- ifelse(test = biomass.inventory$biomass.category == "organic fraction municipal solid waste",
                                             yes = 'ofmsw high recycling',
                                             no = biomass.inventory$biomass.category)

biomass.inventory <- biomass.inventory %>%
  filter(biomass.category != 'ofmsw high recycling')

#removes flow design biosolids

biomass.inventory <- biomass.inventory %>%
  filter(biomass.feedstock != 'Biosolids [Flow Design]')

#creates attribute defining the bioenergy byproduct
biomass.inventory$product <- ifelse(test = 
                                      biomass.inventory$biomass.category == "high moisture solids" | 
                                      biomass.inventory$biomass.category == "orchard vineyard culls" |
                                      biomass.inventory$biomass.category == "manure"|
                                      biomass.inventory$biomass.category == "row residue" |
                                      biomass.inventory$biomass.category == "row culls" |
                                      biomass.inventory$biomass.category == "wet ofmsw" | 
                                      biomass.inventory$biomass.category == "Biosolids [Avg Design]",
                                    yes = 'digestate',
                                    no = 'biochar')

biomass.inventory$product <- ifelse(test = 
                                      biomass.inventory$biomass.feedstock == "BAKERY" | 
                                      biomass.inventory$biomass.feedstock == "TORTILLA",
                                    yes = 'digestate',
                                    no = biomass.inventory$product)

#add product of digested compost to feedstocks that are converted to digestate
digestate.compost.indicator <- data.frame(product = 'digestate', byproduct = c('digestate', 'digestate.compost'))

biomass.inventory.digestate <- merge(biomass.inventory, digestate.compost.indicator)

biomass.inventory.digestate$product <- biomass.inventory.digestate$byproduct
biomass.inventory.digestate <- biomass.inventory.digestate[-8]
biomass.inventory.digestate.compost <- biomass.inventory.digestate %>%
  filter(product == 'digestate.compost')

biomass.inventory <- rbind(biomass.inventory, biomass.inventory.digestate.compost)

# Load biochar yield coefficients for each feedstock type, removes columns without values, renames values low, average, high/
# and calculates yields

biochar.yield.coefficients <- read.csv('data/biochar.feedstock.assumptions.csv', header = T)
biochar.yield.coefficients <- biochar.yield.coefficients[-c(3:5)]
colnames(biochar.yield.coefficients)[3:5] <- c('low','average','high')

#tidies up biochar yield coefficient table
biochar.yield.coefficients <- reshape2::melt(data = biochar.yield.coefficients, 
                                   id.vars = c('product', 'biomass.feedstock'), 
                                   variable.name = 'byproduct.yield.level', 
                                   value.name = 'byproduct.yield.coefficient')

#creates data frame for digestate low high average yield factors
digestate.yield.coefficients <- data.frame(product = 'digestate', byproduct.yield.level = c('average','low','high'), byproduct.yield.coefficient = c(0.7,0.6,0.8))

#creates data frame for digestate compost low high average yield factors assuming 40% loss compost DeLonge et al 2013
digestate.compost.yield.coefficients <- data.frame(product = 'digestate.compost', byproduct.yield.level = c('average','low','high'), byproduct.yield.coefficient = c(0.7*0.4,0.6*0.4,0.8*0.4))

#creates new table of just feedstock names that are AD and converted to digestate
digestate.biomass.feedstock <- biomass.inventory %>%
  filter(product == 'digestate') %>%
  group_by(biomass.feedstock) %>%
  summarize()

#creates new table of just feedstock names that are AD and converted to digestate composted
digestate.compost.biomass.feedstock <- digestate.biomass.feedstock

#creates new table coupling digestate yields to feedstocks
digestate.yield.coefficients <- merge(digestate.biomass.feedstock, digestate.yield.coefficients)

#creates new table coupling digestate compost yields to feedstocks
digestate.compost.yield.coefficients <- merge(digestate.compost.biomass.feedstock, digestate.compost.yield.coefficients)

#creates new table coupling biochar and digestate yields to feedstocks
yield.coefficients <- rbind(digestate.yield.coefficients,digestate.compost.yield.coefficients, biochar.yield.coefficients)

#creates table using the table just build which matching feedstocks and byproduct yields to the inventory
byproduct.yield <- merge(yield.coefficients, biomass.inventory)

#creates a new column in the byproduct yield table that has the total byproduct generated
byproduct.yield <- byproduct.yield %>% 
  mutate(byproduct.yield.bdt = disposal.yields*byproduct.yield.coefficient)

write.csv(byproduct.yield, file = 'data/byproduct.yield.csv', row.names = FALSE)
byproduct.yield <- read.csv('data/byproduct.yield.csv')


#### BYPRODUCT FIXED CARBON YIELDS. Fixed carbon in this analysis is the carbon that remains in the byproduct immediately after gasification or AD process####

#create table of fix carbon fractions for feedstock types and references
biochar.fixed.carbon.content <- read_excel('data/biochar.fixed.carbon.content.xlsx')

#remove information on references and proxy values
biochar.fixed.carbon.content <- biochar.fixed.carbon.content[-c(4:5)]

#clarify values in table are averages
colnames(biochar.fixed.carbon.content)[3] <- 'average'


#add a low and high value for fixed carbon. just a sensitivity variation
biochar.fixed.carbon.content$low <- biochar.fixed.carbon.content$average*0.85 #15% decrease sensitivity of real values
biochar.fixed.carbon.content$high <- biochar.fixed.carbon.content$average*1.15 #15% increase sensitivity of real values

#tidy the table so that there is only one column with values
biochar.fixed.carbon.content <- melt(data = biochar.fixed.carbon.content, 
                                     id.vars = c('product', 'biomass.feedstock'), 
                                     variable.name = 'fixed.carbon.level', 
                                     value.name = 'fixed.carbon.content')


#create data frame with assumptions for digestate fixed carbon
digestate.fixed.carbon.content <- data.frame(product = 'digestate', 
                                             fixed.carbon.level = c('average','low','high'), 
                                             fixed.carbon.content = c(0.215,0.18,0.25)) #range from 2 manure and 2 ofmsw digestate samples from ECN labs 

#create data frame with assumptions for composted digestate fixed carbon
digestate.compost.fixed.carbon.content <- data.frame(product = 'digestate.compost', 
                                             fixed.carbon.level = c('average','low','high'), 
                                             fixed.carbon.content = c(0.213,0.177,0.247))#range derived from 0.6-0.7 g/kg feedstock Ch4 and 7-10 g/kg feedstock Co2 loss from compost pile California's Fourth Climate Change Assessment Silver et al. Subtract c losses in these emissions from the assumed fixed carbon in digestate above

#edit data frame to match fixed carbon content assumptions with feedstock types
digestate.fixed.carbon.content <- merge(digestate.biomass.feedstock, digestate.fixed.carbon.content)
digestate.compost.fixed.carbon.content <- merge(digestate.compost.biomass.feedstock, digestate.compost.fixed.carbon.content)

#create table with all feedstocks and assumed fixed carbon contents
fixed.carbon.content <- rbind(biochar.fixed.carbon.content, digestate.fixed.carbon.content, digestate.compost.fixed.carbon.content)

# assign carbon contents to the table with the byproduct yields calculated
fixed.carbon.yield <- merge(fixed.carbon.content, byproduct.yield)

#calculate fixed carbon from inventory
fixed.carbon.yield <- fixed.carbon.yield %>% 
  mutate(fixed.carbon.yield.bdt = byproduct.yield.bdt*fixed.carbon.content) 


#creates table of possible combinations of biochar yield, fixed carbon, and recalcitrant carbon. The SOC defines the recalcitrant carbon that we assume is sequestered by adding biochar to land
SOC.coefficients.biochar <- expand.grid(product = c('biochar'),
                                        byproduct.yield.level = c('average', 'low', 'high'),
                                        fixed.carbon.level = c('average', 'low', 'high'),
                                        recalcitrant.carbon.level = c('average', 'low', 'high'))

#assigns recalcitrant factor to the combinations. For biochar, recalcitrant ranges from 0.7 to 0.95 with average of 0.85 following Woolf et al SI pg 14 of fixed carbon
SOC.coefficients.biochar$recalcitrant.carbon.fraction <- 
  ifelse(SOC.coefficients.biochar$product == 'biochar' & 
           SOC.coefficients.biochar$recalcitrant.carbon.level == 'low',
 yes = 0.7,
 no = ifelse(SOC.coefficients.biochar$product == 'biochar' & SOC.coefficients.biochar$recalcitrant.carbon.level == 'average',
yes = 0.85,
no = 0.95))

#creates table of possible combinations of digestate yield, fixed carbon, and recalcitrant carbon. The SOC defines the recalcitrant carbon that we assume is sequestered by adding biochar to land
SOC.coefficients.digestate <- expand.grid(product = c('digestate'),
                                          byproduct.yield.level = c('average', 'low', 'high'),
                                          fixed.carbon.level = c('average', 'low', 'high'),
                                          recalcitrant.carbon.level = c('average', 'low', 'high'))

SOC.coefficients.digestate.compost <- expand.grid(product = c('digestate.compost'),
                                          byproduct.yield.level = c('average', 'low', 'high'),
                                          fixed.carbon.level = c('average', 'low', 'high'),
                                          recalcitrant.carbon.level = c('average', 'low', 'high'))

#assigns recalcitrant factor to the combinations. For digestate, recalcitrant ranges from 0 to 0.2 with average of 0.05
SOC.coefficients.digestate$recalcitrant.carbon.fraction <- 
  ifelse(SOC.coefficients.digestate$product == 'digestate' & SOC.coefficients.digestate$recalcitrant.carbon.level == 'low',
yes = 0,
no = ifelse(SOC.coefficients.digestate$product == 'digestate' & SOC.coefficients.digestate$recalcitrant.carbon.level == 'average',
yes = 0.05,
no = 0.2))

SOC.coefficients.digestate.compost$recalcitrant.carbon.fraction <- 
  ifelse(SOC.coefficients.digestate.compost$product == 'digestate.compost' & SOC.coefficients.digestate.compost$recalcitrant.carbon.level == 'low',
         yes = 0,
         no = ifelse(SOC.coefficients.digestate.compost$product == 'digestate.compost' & SOC.coefficients.digestate.compost$recalcitrant.carbon.level == 'average',
                     yes = 0.05,
                     no = 0.2))

# create table of coefficients for all byproducts and write it as csv
SOC.coefficients <- rbind(SOC.coefficients.digestate, SOC.coefficients.digestate.compost, SOC.coefficients.biochar)

write.csv(SOC.coefficients, file.path(getwd(), 'SOC.coefficients.csv'))

#join the recalcitrant coefficients to the inventory table converted to fixed carbon
byproduct.emissions <- merge(SOC.coefficients, fixed.carbon.yield)

byproduct.coefficients.summary <- byproduct.emissions %>%
  group_by(product, biomass.category) %>%
  summarize(yield.min = min(byproduct.yield.coefficient),
            yield.average = mean(byproduct.yield.coefficient),
            yield.max = max(byproduct.yield.coefficient),
            fixed.min = min(fixed.carbon.content),
            fixed.average = mean(fixed.carbon.content),
            fixed.max = max(fixed.carbon.content),
            recalcitrant.min = min(recalcitrant.carbon.fraction),
            recalcitrant.average = mean(recalcitrant.carbon.fraction),
            recalcitrant.max = max(recalcitrant.carbon.fraction))

write.csv(byproduct.coefficients.summary, file = 'data/byproduct.coefficients.summary.csv', row.names = FALSE)

#### CARBON EMISSIONS AND SINKS FROM LAND APP ####
#IPCC multipliers - 100 year 
# https://www.ipcc.ch/publications_and_data/ar4/wg1/en/ch2s2-10-2.html change to 6th assessment https://agupubs.onlinelibrary.wiley.com/doi/full/10.1002/2016GL071930

ipcc.ch4.100 <- 32
ipcc.n2o.100 <- 298
#### CH4 emissions from composting digestate ####

byproduct.emissions$ch4.digestate.compost.tonne.co2eq <- ifelse(test=
                                                byproduct.emissions$product == 'digestate.compost' &
                                                byproduct.emissions$fixed.carbon.level == 'low',
                                              yes = 0.6*ipcc.ch4.100*byproduct.emissions$byproduct.yield.bdt/1000,
                                              no = ifelse(test = byproduct.emissions$product == 'digestate.compost' &
                                                                 byproduct.emissions$fixed.carbon.level == 'average',
                                                              yes = 0.65*ipcc.ch4.100*byproduct.emissions$byproduct.yield.bdt/1000,
                                                              no =  ifelse(test = byproduct.emissions$product == 'digestate.compost' &
                                                                                  byproduct.emissions$fixed.carbon.level == 'high',
                                                                                yes = 0.7*ipcc.ch4.100*byproduct.emissions$byproduct.yield.bdt/1000,
                                                                                no = 0)))


#### CO2 emissions from composting digestate assume biogenic. N2O emissions assumed negligible  ####
byproduct.emissions$co2.digestate.compost.tonne.co2eq <- ifelse(test=
                                                                byproduct.emissions$product == 'digestate.compost' &
                                                                byproduct.emissions$fixed.carbon.level == 'low',
                                                              yes = 7*byproduct.emissions$byproduct.yield.bdt/1000,
                                                              no = ifelse(test = byproduct.emissions$product == 'digestate.compost' &
                                                                            byproduct.emissions$fixed.carbon.level == 'average',
                                                                          yes = 8.5*byproduct.emissions$byproduct.yield.bdt/1000,
                                                                          no =  ifelse(test = byproduct.emissions$product == 'digestate.compost' &
                                                                                         byproduct.emissions$fixed.carbon.level == 'high',
                                                                                       yes = 10*byproduct.emissions$byproduct.yield.bdt/1000,
                                                                                       no = 0)))




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#timeline
lifecycle.time <- 100 #years

summary.graph <- data.frame(year = NA, biomass.category = NA, process = NA, value = NA, scenario = NA)

results.summary <- data.frame()
results.summary.co2eq <- data.frame()

byproduct.emissions.df.total <- byproduct.emissions
for (index in 1:nrow(scenarios)) {
  
  #define app.rate as 2, 4 or 50 and define change in npp as 0, 2.1 or 4.7 MgC/ha Ryal and Silver 2013
  app.rate <- scenarios$app.rate[index] #bdt / hectare
  npp.change <- scenarios$digestate.npp.change[index] #tonnesC/hectare
  forage.change <- scenarios$forage.change[index] # tonnesforage/hectare
  npp.change.life <- scenarios$npp.change.life[index] #years enhanced NPP from single application
  biochar.soil.change.life <- scenarios$biochar.soil.change.life[index] #years reduced ch4, n20 from single application biochar
  lifecycle.time <- 100 #years
  
  byproduct.emissions <- byproduct.emissions.df.total %>%
    filter(byproduct.yield.level == scenarios$byproduct.yield.level[index] &
             fixed.carbon.level == scenarios$fixed.carbon.level[index] &
             recalcitrant.carbon.level == scenarios$recalcitrant.carbon.level[index])
    
  # ----  CH4 SOIL ----
  
  #biochar and compost : immediate CH4 release from byproduct assumed 0. however immediate CH4 release assumed from digestate
  fraction.ch4.c.emitted <- 0.0005 #Yoshida 2018 from (Ambus et al., 2001)
  
  byproduct.emissions$ch4.emit.digestate.soil.tonne.co2eq <- ifelse(byproduct.emissions$product == 'digestate',
                                                             yes = fraction.ch4.c.emitted*ipcc.ch4.100*byproduct.emissions$fixed.carbon.yield.bdt*16/12,
                                                             no = 0)
  
  

  #woolf et al equation to forecast 100 year timeline for carbon dioxide equivalents of decay. subtract out ch4 from fixed carbon fraction
  
  byproduct.emissions$co2.emit.soil.tonne.co2eq <- ifelse(byproduct.emissions$product == 'digestate'| byproduct.emissions$product == 'digestate.compost',
                                                             yes = byproduct.emissions$fixed.carbon.yield.bdt*(1-(byproduct.emissions$recalcitrant.carbon.fraction*exp(-(log(2, base = exp(1))*lifecycle.time/100))+(1-byproduct.emissions$recalcitrant.carbon.fraction-fraction.ch4.c.emitted)*exp(-(log(2, base = exp(1))*lifecycle.time/1.5))))*(44/12),
                                                             no = byproduct.emissions$fixed.carbon.yield.bdt*(1-(byproduct.emissions$recalcitrant.carbon.fraction*exp(-(log(2, base = exp(1))*lifecycle.time/300))+(1-byproduct.emissions$recalcitrant.carbon.fraction)*exp(-(log(2, base = exp(1))*lifecycle.time/20))))*(44/12))
  
  
  
  #woolf et al equation to forecast 100 year timeline for carbon decay
  byproduct.emissions$soil.C.accumulated.tonne.c <- ifelse(byproduct.emissions$product == 'digestate'| byproduct.emissions$product == 'digestate.compost',
                                                          yes = byproduct.emissions$fixed.carbon.yield.bdt*((byproduct.emissions$recalcitrant.carbon.fraction*exp(-(log(2, base = exp(1))*lifecycle.time/100)))+(1-byproduct.emissions$recalcitrant.carbon.fraction-fraction.ch4.c.emitted)*exp(-(log(2, base = exp(1))*lifecycle.time/1.5))),
                                                          no = byproduct.emissions$fixed.carbon.yield.bdt*((byproduct.emissions$recalcitrant.carbon.fraction*exp(-(log(2, base = exp(1))*lifecycle.time/300))+(1-byproduct.emissions$recalcitrant.carbon.fraction)*exp(-(log(2, base = exp(1))*lifecycle.time/20)))))
  
  byproduct.emissions$soil.C.accumulated.tonne.co2 <- ifelse(byproduct.emissions$product == 'digestate'| byproduct.emissions$product == 'digestate.compost',
                                            yes = -byproduct.emissions$fixed.carbon.yield.bdt*((byproduct.emissions$recalcitrant.carbon.fraction*exp(-(log(2, base = exp(1))*lifecycle.time/100)))+(1-byproduct.emissions$recalcitrant.carbon.fraction-fraction.ch4.c.emitted)*exp(-(log(2, base = exp(1))*lifecycle.time/1.5)))*(44/12),
                                            no = -byproduct.emissions$fixed.carbon.yield.bdt*((byproduct.emissions$recalcitrant.carbon.fraction*exp(-(log(2, base = exp(1))*lifecycle.time/300))+(1-byproduct.emissions$recalcitrant.carbon.fraction)*exp(-(log(2, base = exp(1))*lifecycle.time/20)))))*(44/12)
  
  #increased CH4 reoxidation by soil biochar application over lifetime. assume this is not going to happen on grasslands as they are ch4 negative
  
  #Woolf et al 2010 SI Rondon and colleagues
  # biochar ch4
  # 0-150 mg ch4-c / m2 yr
  # median 75 mg ch4-c / m2 yr
  
  #enhanced.ch4.sink <- -75 * ud.convert(1,'mg','tonne')*(16/12)/ud.convert(1,'m2','ha') #ton ch4 / ha yr
  
  #byproduct.emissions$ch4.sink.biochar.soil.tonne.co2eq <- ifelse(byproduct.emissions$product == 'biochar',
  #                                                         yes = enhanced.ch4.sink*ipcc.ch4.100*biochar.soil.change.life*byproduct.emissions$byproduct.yield.bdt/app.rate,
  #                                                         no = 0)
  
  
  # avoided N2O from biochar additions
  
  # Woolf 2010 SI
  # biochar n2o
  # En = Rn * (2.5 kg n2o / ha yr) * Area (ha)
  # annual avoided n2o emissions = reduction factor * 2.5 * area
  # Rn = 0.25 - conservative number
  
  enhanced.n2o <- 2.5 * ud.convert(1,'kg','tonne')
  reduction.factor <- 0.25

  byproduct.emissions$n2o.sink.biochar.soil.tonne.co2eq <- ifelse(byproduct.emissions$product == 'biochar',
                                                           yes = -enhanced.n2o*reduction.factor*ipcc.n2o.100*biochar.soil.change.life*byproduct.emissions$byproduct.yield.bdt/app.rate,
                                                           no = 0)
  
  #immediate emissions n2o from compost and biochar assumed zero , emissions from digestate n2o nonzero
  
  #from Holly, Michael A., et al. "Greenhouse gas and ammonia emissions from digested and separated dairy manure during storage and after land application." Agriculture, Ecosystems & Environment 239 (2017): 410-419.
  #Using D1 stream
  # digestate.n2o.emit <- 10*ud.convert(1,'g','tonne')/0.051 #g n2o/tonne raw digestate (D1 TS of 5.1%, convert to dry weight)
  
  
  byproduct.emissions$n2o.digestate.tonne.co2eq <- ifelse(byproduct.emissions$product == 'digestate',
                                                             yes = 0.00013*ipcc.n2o.100*byproduct.emissions$byproduct.yield.bdt,
                                                             no = 0)
  
  # ---- TRUCKING & TILLAGE EMISSIONS ----
  
  #assume each truck travels x amount of miles and carries 75% max capacity
  distance <- byproduct.emissions$dist.ft*scenarios$distance.level[index] * ud.convert(1,'ft','km') # km traveled / truck
  class.7.payload <- (80000 - 28000) * 0.75 * ud.convert(1,'lb','tonne') #payload ton / truck
  
  #translate byproducts bdt to wet tonnes. assume 0.3 average TS content of digestate after dewatering from Guilayn et al 2018
  byproduct.emissions$trucking.mt.km <- ifelse(byproduct.emissions$product == 'digestate',
                                               yes =(byproduct.emissions$byproduct.yield.bdt/0.3)*distance/class.7.payload,
                                               no = ifelse(byproduct.emissions$product == 'digestate.compost',
                                                            yes =(byproduct.emissions$byproduct.yield.bdt/0.4)*distance/class.7.payload,
                                                            no = byproduct.emissions$byproduct.yield.bdt*distance/class.7.payload))

  tillage.fuel <- 19.6 #kg diesel / hectare
  diesel.GJ <- 45 #GJ / kg diesel
  
  #dont adjust byproduct yield from bdt/y as this is simply calculating hectares treated using app.rate
  
  byproduct.emissions$tillage.diesel.MJ <- ifelse(byproduct.emissions$product == 'digestate',
                                                  yes =(byproduct.emissions$byproduct.yield.bdt)*tillage.fuel*ud.convert(diesel.GJ, 'GJ', 'MJ')/app.rate,
                                                  no = ifelse(byproduct.emissions$product == 'digestate.compost',
                                                              yes =(byproduct.emissions$byproduct.yield.bdt)*tillage.fuel*ud.convert(diesel.GJ, 'GJ', 'MJ')/app.rate,
                                                              no = byproduct.emissions$byproduct.yield.bdt*tillage.fuel*ud.convert(diesel.GJ, 'GJ', 'MJ')/app.rate))
    

  co2.filepath <- CO2VectorFilepath()
  ch4.filepath <- CH4VectorFilepath()
  n2o.filepath <- N2OVectorFilepath()
  A <- IOTableSetup(IOTableFilepath())
  y <- YSetUp(co2.filepath)
  time.horizon <- 100
  
  years <- c('2014', '2020', '2050')
  
  trucking.emissions.co2eq <- data.frame()
  
  for (time in 1:length(years)) {
    
    y["flatbedtruck.mt_km", "y"] <- sum(byproduct.emissions$trucking.mt.km[byproduct.emissions$year == years[time]], na.rm = T)
    
    y["diesel.MJ", "y"] <- sum(byproduct.emissions$tillage.diesel.MJ[byproduct.emissions$year == years[time]], na.rm = T)
    
    y1 <- y
    y1[y1>0] <- 0
    y1 <- abs(y1)
    y[y<0] <- 0
    
    results.kg.co2eq <- TotalGHGEmissions(A, data.matrix(y, rownames.force = NA),
                                          co2.filepath,
                                          ch4.filepath, n2o.filepath,
                                          time.horizon)
    results.kg.co2eq.credits <- TotalGHGEmissions(A, data.matrix(y1, rownames.force = NA),
                                                  co2.filepath,
                                                  ch4.filepath, n2o.filepath,
                                                  time.horizon)
    results.kg.co2eq <- results.kg.co2eq - results.kg.co2eq.credits # Subtract off credits
    
    sum.results <- data.frame(year = years[time], sum = sum(results.kg.co2eq$r, na.rm = T))
    
    trucking.emissions.co2eq <- rbind(trucking.emissions.co2eq, sum.results)
    
  }
  
  trucking.emissions.co2eq$sum <- trucking.emissions.co2eq$sum*ud.convert(1,'kg','tonne')
  
  yield <- byproduct.emissions %>%
    group_by(year) %>%
    summarize(yield = sum(byproduct.yield.bdt, na.rm = T))
  
  trucking.emissions.co2eq <- merge(trucking.emissions.co2eq, yield)
  
  trucking.emissions.co2eq$co2eq.per.yield.bdt <- trucking.emissions.co2eq$sum / trucking.emissions.co2eq$yield
  
  byproduct.emissions$trucking.co2eq <- ifelse(byproduct.emissions$year == 2014,
                                               byproduct.emissions$byproduct.yield.bdt*
                                                 trucking.emissions.co2eq$co2eq.per.yield.bdt[trucking.emissions.co2eq$year == 2014],
                                               ifelse(byproduct.emissions$year == 2020,
                                                      byproduct.emissions$byproduct.yield.bdt*
                                                        trucking.emissions.co2eq$co2eq.per.yield.bdt[trucking.emissions.co2eq$year == 2020],
                                                      byproduct.emissions$byproduct.yield.bdt*
                                                        trucking.emissions.co2eq$co2eq.per.yield.bdt[trucking.emissions.co2eq$year == 2050]))
  
  
  
  # ----  CROP RESPONSE EMISSIONS ----
  
  # Ryals and Silver 2013 compost increases NPP belowground by 2.1±0.8 to 4.7 ± 0.7 Mg C/ha over three years.
  #literature on crop yield from biochar very limited and do not assume increase from biochar
  
  # calculate new forage production associated with NPP change
  
  # https://anrcatalog.ucanr.edu/pdf/8018.pdf 
  # sum 2013-2014 season for forage production Dec - May Table 2
  
  forage.2014 <- 3801 #lb / acre
  forage.ratio.2014 <- forage.2014 * (ud.convert(1,'lb','tonne')/ud.convert(1,'acre','hectare')) # 4.26 tonne / ha
  #0 to 70 percent change in forage associated with change in npp
  
  byproduct.emissions$npp.change.tonne.c <- ifelse(byproduct.emissions$product == 'digestate'| byproduct.emissions$product == 'digestate.compost',
                                                          yes = npp.change*byproduct.emissions$byproduct.yield.bdt*npp.change.life/app.rate,
                                                          no = 0)
  
  byproduct.emissions$npp.change.tonne.co2eq <- ifelse(byproduct.emissions$product == 'digestate'| byproduct.emissions$product == 'digestate.compost',
                                            yes = -npp.change*byproduct.emissions$byproduct.yield.bdt*npp.change.life/app.rate,
                                            no = 0)*44/12
  
  # assume methane and n2o from grass decomposition minimal and co2 is biogenic. Grasslands are often Ch4 negative.
  # Jahon use woolf SI 2010 mean methane emission factor 0.076 kg CH4-C / kg rice straw
  #ch4.forage <- 0.076*16/12
  
  #byproduct.emissions$ch4.forage.change.tonne.co2eq <- ifelse(byproduct.emissions$product == 'digestate'| byproduct.emissions$product == 'digestate.compost',
 #                                                      yes = forage.change*npp.change.life*forage.ratio.2014*ch4.forage*ipcc.ch4.100*byproduct.emissions$byproduct.yield.bdt/app.rate,
 #                                                      no = 0)
  # additional forage growth assumptions based on Zhang et al 2013, Alburguergue et al 2012 and Nabel et al 2014
  # assumptions placed in scenarios dataframe
  
  
  # Alburguergue et al 2012
  # https://www.sciencedirect.com/science/article/pii/S1161030112000834
  # calculated mean relative biomass yield per experimental yield ( Mg / ha ) from four trials, 2 watermelon, 2 cauliflower
  
  # 0.38 % N
  # 0.05 % P2O5
  # 0.24 % K2O
  
  # kg.digestate <- c(0.0038, 0.0005, 0.0024)
  # digestate.applied <- c(240,90,250) / kg.digestate
  # this corroborates that they applied at ~64 tonne / ha (referenced later in the econ analysis)
  #Applied at 240N, 90 P2O5, 250 K2O kg / ha
  
  #if all coproducts are applied to rangelands, assume growth is relative to ratio of total coproduct yields
  # scenarios$biochar.crop[index] #tons additional forage per hectare (given biochar app rate)
  # scenarios$digestate.crop[index]  #tons additional forage per hectare (given digestate app rate)
  
  #coproduct.yield.ratio.digesate.to.biochar <- sum(byproduct.emissions$byproduct.yield.bdt[byproduct.emissions$byproduct.yield.level == scenarios$byproduct.yield.level[index] &
 #                                                                                            byproduct.emissions$fixed.carbon.level == #scenarios$fixed.carbon.level[index] &
  #                                                                                           byproduct.emissions$recalcitrant.carbon.level #== scenarios$recalcitrant.carbon.level[index] &
  #                                                                                           byproduct.emissions$product == 'digestate' &
 #                                                                                            byproduct.emissions$year == 2014], na.rm = T) /
 #   
 #   sum(byproduct.emissions$byproduct.yield.bdt[byproduct.emissions$byproduct.yield.level == scenarios$byproduct.yield.level[index] &
  #                                                byproduct.emissions$fixed.carbon.level == scenarios$fixed.carbon.level[index] &
 #                                                 byproduct.emissions$recalcitrant.carbon.level == scenarios$recalcitrant.carbon.level[index] &
 #                                                 byproduct.emissions$product == 'biochar' &
 #                                                 byproduct.emissions$year == 2014], na.rm = T)
 # 
 # crop.yield.average.weighted <- mean(scenarios$digestate.crop[index]*coproduct.yield.ratio.digesate.to.biochar, scenarios$biochar.crop[index]*coproduct.yield.ratio.digesate.to.biochar^-1)
  
  
 # crop.response.bdt <- available.rangeland[3] *crop.yield.average.weighted # additional tonnes of forage per hectare
  
  # methane from straw
  # use woolf SI 2010 mean methane emission factor 0.076 kg CH4-C / kg rice straw
 #crop.response.co2eq <- 0.076*(16/12)*crop.response.bdt*ipcc.ch4.100
  
 # crop.response.per.yield <- crop.response.co2eq / sum(byproduct.emissions$byproduct.yield.bdt[byproduct.emissions$byproduct.yield.level == scenarios$byproduct.yield.level[index] & byproduct.emissions$fixed.carbon.level == scenarios$fixed.carbon.level[index] & byproduct.emissions$recalcitrant.carbon.level == scenarios$recalcitrant.carbon.level[index] & byproduct.emissions$year == 2014], na.rm = T)
  
 # byproduct.emissions$crop.response.co2eq <- byproduct.emissions$byproduct.yield.bdt * crop.response.per.yield
  
  
  #summarize results
  
  emissions.summary <- byproduct.emissions %>%
    group_by(year, biomass.category, product) %>%
    summarize(ch4.emitted.compost.digestate.co2eq = sum(ch4.digestate.compost.tonne.co2eq, na.rm= T),
              co2.emitted.compost.digestate.co2eq = sum(co2.digestate.compost.tonne.co2eq, na.rm= T),
              ch4.emitted.digestate.decay.co2eq = sum(ch4.emit.digestate.soil.tonne.co2eq, na.rm = T),
              n2o.emitted.digestate.decay.co2eq = sum(n2o.digestate.tonne.co2eq, na.rm = T),
              co2.emitted.biochar.digestate.decay.co2eq = sum(co2.emit.soil.tonne.co2eq, na.rm = T),
              carbon.accumulation.biochar.digestate.c = sum(soil.C.accumulated.tonne.c, na.rm = T),
              carbon.accumulation.biochar.digestate.co2eq = sum(soil.C.accumulated.tonne.co2, na.rm = T),
              carbon.sink.npp.digestate.c = sum(npp.change.tonne.c, na.rm= T),
              carbon.sink.npp.digestate.co2eq = sum(npp.change.tonne.co2eq, na.rm= T),
              n2o.sink.biochar.soil.co2eq = sum(n2o.sink.biochar.soil.tonne.co2eq, na.rm = T),
              #ch4.sink.biochar.soil.co2eq = sum(ch4.sink.biochar.soil.tonne.co2eq, na.rm = T),     
              ghg.trucking.tillage.co2eq = sum(trucking.co2eq, na.rm = T),
              scenario = scenarios$scenario[index])
  
  emissions.summary <- melt(emissions.summary, 
                            id.vars = c('year','biomass.category','product', 'scenario'), 
                            variable.name = 'process', 
                            value.name = 'value')
  
  
  # byproduct.emissions.graph$scenario <- as.character(scenarios$scenario[index])
  
  results.summary <- rbind(results.summary, emissions.summary)
  
  
  emissions.summary.co2eq <- byproduct.emissions %>%
    group_by(year, biomass.category, product) %>%
    summarize(ch4.emitted.compost.digestate.co2eq = sum(ch4.digestate.compost.tonne.co2eq, na.rm= T),
              co2.emitted.compost.digestate.co2eq = sum(co2.digestate.compost.tonne.co2eq, na.rm= T),
              ch4.emitted.digestate.decay.co2eq = sum(ch4.emit.digestate.soil.tonne.co2eq, na.rm = T),
              n2o.emitted.digestate.decay.co2eq = sum(n2o.digestate.tonne.co2eq, na.rm = T),
              co2.emitted.biochar.digestate.decay.co2eq = sum(co2.emit.soil.tonne.co2eq, na.rm = T),
              carbon.accumulation.biochar.digestate.co2eq = sum(soil.C.accumulated.tonne.co2, na.rm = T),
              carbon.sink.npp.digestate.co2eq = sum(npp.change.tonne.co2eq, na.rm= T),
              n2o.sink.biochar.soil.co2eq = sum(n2o.sink.biochar.soil.tonne.co2eq, na.rm = T),
              #ch4.sink.biochar.soil.co2eq = sum(ch4.sink.biochar.soil.tonne.co2eq, na.rm = T),     
              ghg.trucking.tillage.co2eq = sum(trucking.co2eq, na.rm = T),
              scenario = scenarios$scenario[index])
  
  emissions.summary.co2eq <- melt(emissions.summary.co2eq, 
                            id.vars = c('year','biomass.category','product', 'scenario'), 
                            variable.name = 'process', 
                            value.name = 'value')
  
  
  # byproduct.emissions.graph$scenario <- as.character(scenarios$scenario[index])
  
  results.summary.co2eq <- rbind(results.summary.co2eq, emissions.summary.co2eq)
}



write.csv(results.summary, 'results.tonnes.csv')
write.csv(results.summary.co2eq, 'results.tonnes.co2eq.csv')




plot_list <- c()

x.labs <- results.summary.co2eq %>%
  group_by(biomass.category, product) %>%
  summarize()

x.labs$new <- c('Dry OFMSW Biochar',
                'Field Residue Biochar',
                'Forestry Biochar',
                'Green Waste Biochar',
                'Processing (High Moisture) D Raw',
                'Processing (High Moisture) D Compost',
                'Processing (Low Moisture) D Raw',
                'Processing (Low Moisture) D Composted',
                'Processing (Low Moisture) Biochar',
                'Manure D Raw',
                'Manure D Compost',
                'Orchard & Vineyard Culls D Raw',
                'Orchard & Vineyard Culls D Compost',
                'Orchard & Vineyard Residue Biochar',
                'Crop Row Culls D Raw',
                'Crop Row Culls D Compost',
                'Crop Row Residue D Raw',
                'Crop Row Residue D Compost',
                'Wet OFMSW D Raw',
                'Wet OFMSW D Compost')

results.summary.co2eq <- merge(results.summary.co2eq, x.labs)

results.summary.co2eq$wrap <- str_wrap(results.summary.co2eq$new, width = '3')
process.labels <- c('Composting CH4',
                    'Composting CO2',
                    'Decay CH4',
                    'Decay N2O',
                    'Decay CO2',
                    'Soil C Accumulation',
                    'NPP C Sequestration',
                    'Soil N2O Reduction',
                    'Trucking & Tillage')

for(i in 1:length(years)) {
  temp <- filter(results.summary.co2eq, year == years[i])
  p <- ggplot(temp) +
    geom_col(aes(x = wrap, y = value / 1E6, fill = process)) +
    facet_wrap(vars(scenario), ncol = 1) +
    scale_fill_discrete(name = 'Emission Source',
                        labels = process.labels) +
    ylab('Million Tonnes CO2 Eq') +
    ggtitle(paste('Carbon Dioxide Equivalents', years[i])) +
    xlab('Disposed Biomass Byproduct Type') +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5)) +
    #  theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
    theme(panel.grid.major = element_line(color = 'grey88'))
  plot_list[[i]] <- p
}

for (i in 1:length(years)) {
  ggsave(plot_list[[i]],
         filename = paste('byproduct.emissions','.',years[i],".jpeg", sep=""),
         path = getwd(),
         device = 'jpeg',
         width = 24,
         height = 16,
         units = "cm")
}

results.summary.co2eq$year <- as.character(results.summary.co2eq$year)
ggplot(results.summary.co2eq) +
  geom_col(aes(x = year, y = ud.convert(value, 'tonne','Tg'), fill = process)) +
  facet_wrap(vars(scenario), ncol = 1) +
  scale_fill_discrete(name = 'Emission Source',
                      labels = process.labels) +
  ylab('Tg CO2 Eq.') +
  ggtitle('Lifecycle Emissions: Carbon Dioxide Equivalents') +
  xlab('Year') +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  #  theme(axis.text.x=element_text(angle = 45, hjust = 1)) +
  theme(panel.grid.major = element_line(color = 'grey88')) +
  ggsave(filename = 'byproduct.emissions.by.year.jpeg',
         path = getwd(),
         device = 'jpeg',
         width = 16,
         height = 24,
         units = "cm")



##### SENSITIVITY YIELDS, FIXED CARBON, CARBON SEQUESTERED #####

# defines scenarios for varying levels of yields, fixed carbon, and sequestered carbon
# compares sensitivities on accumulated C in soils from byproduct for each 

SOC.sensitivity <- byproduct.emissions.df.total %>%
  filter(year == 2050) %>%
  group_by(byproduct.yield.level, fixed.carbon.level, recalcitrant.carbon.level) %>%
  summarize(CO2eq.total = -sum(soil.C.accumulated.tonne.co2, na.rm = T))

SOC.sensitivity$scenario <- ifelse(SOC.sensitivity$byproduct.yield.level == 'average' &
                                     SOC.sensitivity$fixed.carbon.level == 'average' & 
                                     SOC.sensitivity$recalcitrant.carbon.level == 'average',
                                   yes = 'Base: Average Byproduct Yields, Fixed Carbon, Sequestration',
                                   no = ifelse(SOC.sensitivity$byproduct.yield.level == 'low' &
                                                 SOC.sensitivity$fixed.carbon.level == 'average' & 
                                                 SOC.sensitivity$recalcitrant.carbon.level == 'average',
                                               yes = 'Low Byproduct Yields',
                                               no = ifelse(SOC.sensitivity$byproduct.yield.level == 'low' &
                                                             SOC.sensitivity$fixed.carbon.level == 'low' & 
                                                             SOC.sensitivity$recalcitrant.carbon.level == 'average',
                                                           yes = 'Low Byproduct Yields, Fixed Carbon',
                                                           no = ifelse(SOC.sensitivity$byproduct.yield.level == 'low' &
                                                                         SOC.sensitivity$fixed.carbon.level == 'low' & 
                                                                         SOC.sensitivity$recalcitrant.carbon.level == 'low',
                                                                       yes = 'Low Byproduct Yields, Fixed Carbon, Sequestration',
                                                                       no = ifelse(SOC.sensitivity$byproduct.yield.level == 'high' &
                                                                                     SOC.sensitivity$fixed.carbon.level == 'average' & 
                                                                                     SOC.sensitivity$recalcitrant.carbon.level == 'average',
                                                                                   yes = 'High Byproduct Yields',
                                                                                   no = ifelse(SOC.sensitivity$byproduct.yield.level == 'high' &
                                                                                                 SOC.sensitivity$fixed.carbon.level == 'high' & 
                                                                                                 SOC.sensitivity$recalcitrant.carbon.level == 'average',
                                                                                               yes = 'High Byproduct Yields, Fixed Carbon',
                                                                                               no = ifelse(SOC.sensitivity$byproduct.yield.level == 'high' &
                                                                                                             SOC.sensitivity$fixed.carbon.level == 'high' & 
                                                                                                             SOC.sensitivity$recalcitrant.carbon.level == 'high',
                                                                                                           yes = 'High Byproduct Yields, Fixed Carbon, Sequestration',
                                                                                                           no = NA )))))))

SOC.sensitivity <- filter(SOC.sensitivity, !is.na(scenario))

SOC.sensitivity$sensitivity[7] <- 0

SOC.sensitivity$sensitivity[6] <- 
  SOC.sensitivity$CO2eq.total[7] - 
  SOC.sensitivity$CO2eq.total[6] -
  1 #slight variation for graphing

SOC.sensitivity$sensitivity[5] <- 
  SOC.sensitivity$CO2eq.total[7] - 
  SOC.sensitivity$sensitivity[6] -
  SOC.sensitivity$CO2eq.total[5]

SOC.sensitivity$sensitivity[1] <- 
  SOC.sensitivity$CO2eq.total[7] - 
  SOC.sensitivity$sensitivity[6] -
  SOC.sensitivity$sensitivity[5] -
  SOC.sensitivity$CO2eq.total[1]

SOC.sensitivity$sensitivity[2] <- 
  SOC.sensitivity$CO2eq.total[7] - 
  SOC.sensitivity$sensitivity[6] -
  SOC.sensitivity$sensitivity[5] -
  SOC.sensitivity$sensitivity[1] -
  SOC.sensitivity$CO2eq.total[2]

SOC.sensitivity$sensitivity[3] <- 
  SOC.sensitivity$CO2eq.total[7] - 
  SOC.sensitivity$sensitivity[6] -
  SOC.sensitivity$sensitivity[5] -
  SOC.sensitivity$sensitivity[1] -
  SOC.sensitivity$sensitivity[2] -
  SOC.sensitivity$CO2eq.total[3]

SOC.sensitivity$sensitivity[4] <- 
  SOC.sensitivity$CO2eq.total[7] - 
  SOC.sensitivity$sensitivity[6] -
  SOC.sensitivity$sensitivity[5] -
  SOC.sensitivity$sensitivity[1] -
  SOC.sensitivity$sensitivity[2] -
  SOC.sensitivity$sensitivity[3] -
  SOC.sensitivity$CO2eq.total[4]



SOC.sensitivity <- SOC.sensitivity[-c(1:3)]
SOC.sensitivity <- melt(SOC.sensitivity, id.vars = 'scenario', measure.vars = c('CO2eq.total','sensitivity'))  

SOC.sensitivity$variable <- as.character(SOC.sensitivity$variable)
SOC.sensitivity$variable[7] <- 'high.CO2eq'
SOC.sensitivity$variable <- as.factor(SOC.sensitivity$variable)

SOC.sensitivity$scenario_wrap <- str_wrap(SOC.sensitivity$scenario, width = 10) 

SOC.sensitivity$label <- ifelse(test = SOC.sensitivity$variable == 'sensitivity' & SOC.sensitivity$value != 0,
                                yes = SOC.sensitivity$value,
                                no = NA)

SOC.sensitivity$value.mil <- SOC.sensitivity$value / 1000000

ggplot(SOC.sensitivity, aes(x = reorder(scenario_wrap, -value), y = value.mil, fill = variable)) +
  geom_col(position = position_stack(reverse = TRUE)) + 
  scale_fill_manual(values = c(NA, "dodgerblue2", "firebrick1")) +
  xlab('Sensitivity Scenarios') +
  ylab('Million Tons CO2 Eq per Application') +
  guides(fill = FALSE) +
  theme_classic() +
  theme(panel.grid.minor = element_line(color = 'grey')) +
  ggsave(filename = 'sequestered.carbon.sensitivity.jpeg',
         path = getwd(),
         device = 'jpeg',
         width = 32,
         height = 32,
         units = "cm")
# geom_text(aes(label = label))

