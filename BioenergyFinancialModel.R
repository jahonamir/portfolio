# Financial Model for dry anaerobic digestion facility
# Written by Jahon Amirebrahimi and Sarah Smith

####Load Packages and source functions####

packages <- c('udunits2', 'Rcpp', 'tidyverse', 'reshape2', 'FinCal', 'rstudioapi', 'RColorBrewer')
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}
lapply(packages, library, character.only = TRUE)

#defines model directory depending on whether user runs from RStudio, R, or console
if (tryCatch(dirname(sys.frame(1)$ofile), error = function(e) {return(0)}) == 0) {
  model_path <- dirname(getSourceEditorContext()$path)
} else {
  model_path <- dirname(sys.frame(1)$ofile) 
}

setwd(model_path)
source(file.path(model_path,'src','ops_functions_USA.R')) #loads CalculatesOpsData()
source(file.path(model_path,'src',"ops_inputs_USA.R")) #loads assumptions for CalculateOpsData
source(file.path(model_path,'src',"cost_functions_USA.R")) #loads CalculateCostData()
source(file.path(model_path,'src',"econ_functions_USA.R")) #loads CalculateNPPandIRR
source(file.path(model_path,'src',"plotting_functions.R")) #loads plotting functions
source(file.path(model_path, 'src','cost_inputs_USA.R'))
#loads scenario_setup for base and clean waste input cases, as well as location for unitCosts and wasteData
scenarioDataFrame <- read.csv(file.path(model_path,'inputs',"scenario_setup_USA.csv"), row.names='name', stringsAsFactors = FALSE)
#loads scalars for several facility amendment scenarios
scenarioScalars <- read.csv(file.path(model_path,'inputs', 'scenario_scalars_USA.csv'), row.names='cost')
wasteData <- read.csv(file.path(model_path, 'inputs', 'waste_data_USA.csv'))

#### Operation, Cost, and Economic Calculations ####
opData <- CalculateOpsData(wasteData, 'base')
write.csv(opData, file = file.path(model_path,'outputs','Apr2019','opData.csv'))
before <- Sys.time()
unitCost <- read.csv(file.path(model_path, 'inputs', 'unit_costs_USA.csv'))
costList <- list(); n <- 1; m <- 1
for (scale_scenario in names(scenarioScalars)){
  costList[[m]] <- CalculateCostData(opData, unitCost)
  m <- m + 1
}
costDataFrame <- do.call("rbind", costList)
write.csv(costDataFrame, file = file.path(model_path,'outputs','Apr2019','costData.csv'))
profit.npv.irr <- CalculateStateNPVandIRR(costDataFrame)
results.df <- data.frame(
  digester.mass = profit.npv.irr[['npv']][['digester.mass']],
  average.cash = profit.npv.irr[['average']][['value']],
  npv = profit.npv.irr[['npv']][['value']],
  irr = profit.npv.irr[['irr']][['value']],
  state = profit.npv.irr[['npv']][['state']],
  scenario2 = profit.npv.irr[['npv']][['scenario2']]
)
after <- Sys.time()
after - before

write.csv(results.df,file = file.path(model_path,'outputs','Apr2019','base.results.csv'))
results.df <- read.csv(file = file.path(model_path,'outputs','Apr2019','base.results.csv'), row.names = 1)

scenario.labels <- data.frame(scenario2 = c('base-base',
                                            'base-cng.offsite',
                                            'base-cng.onsite',
                                            'base-compost.onsite',
                                            'base-district.heating',
                                            'base-fertilizer'),
                              scenario.label = c('Electricity',
                                                 'Off site Fuel Sales',
                                                 'On-site Fuel Sales',
                                                 'Electricity & On-site Composting',
                                                 'Electricity & District Heating',
                                                 'Electricity & Nutrient Recovery'))

results.df <- merge(results.df, scenario.labels)


results.df$scenario.label.f = factor(results.df$scenario.label, levels=c('Electricity',
                                                                         'On-site Fuel Sales',
                                                                         'Off site Fuel Sales',
                                                                         'Electricity & Nutrient Recovery',
                                                                         'Electricity & On-site Composting',
                                                                         'Electricity & District Heating'))

nrow(results.df) #504 existing state and scenario possibilities
results.df$ind <- paste(results.df$digester.mass,
                        results.df$state,
                        results.df$scenario2,
                           sep = '-')

base.solvency.npv <- filter(results.df,npv >= 0)
base.solvency.npv.sum <-base.solvency.npv %>%
  group_by(state,scenario.label) %>%
  summarize(ratio.of.solvent.capacity = n()/12,
            min.capacity = min(digester.mass),
            max.capacity = max(digester.mass))
write.csv(base.solvency.npv.sum,file = file.path(model_path,'outputs','Apr2019','base.solvency.npv.sum.csv'))


base.solvency.irr <- filter(results.df, irr >= 0.07)
base.solvency.irr.sum <- base.solvency.irr %>%
  group_by(state,scenario.label) %>%
  summarize(capacity.ratio = n()/12,
            min.capacity = min(digester.mass),
            max.capacity = max(digester.mass))
write.csv(base.solvency.irr.sum,file = file.path(model_path,'outputs','Apr2019','base.solvency.irr.sum.csv'))
nrow(base.solvency.npv) #145 npv solvent facilities
nrow(base.solvency.irr) #137 irr solvent facilities



#remove the already solvent facilities 
base.insolvency.npv <- results.df[!(results.df$ind %in% base.solvency.npv$ind),]
base.insolvency.irr <- results.df[!(results.df$ind %in% base.solvency.irr$ind),]
nrow(base.insolvency.npv) # 359 npv non solvent facilities
nrow(base.insolvency.irr) # 367 irr non solvent facilities



####sensitivity analysis####
#second, of the non solvent base cases - at what sensitivity do they become solvent?

sensitivity.vars <- c('facility.capital', 'digestate.tipping', 'electricity.sell','inbound.tipping','rng.sales','compost.sales', 'fuel.credits')
sensitivities <- c(0.5,0.5,3,2,2,3.5,3.5)
sens.guide <- data.frame(vars = sensitivity.vars,
                         sens = sensitivities)

sensitivity_results <- data.frame()

for(guide in 1:nrow(sens.guide)) {
  unitCostSens <- unitCost
  states <- colnames(unitCostSens[,2:ncol(unitCostSens)])
  scenarioScalars <- read.csv(file.path(model_path,'inputs', 'scenario_scalars_USA.csv'), row.names='cost')
  if(sens.guide$vars[guide] == 'fuel.credits') {
    scenarioScalars['fuel.credits','cng.offsite'] <- -1
    scenarioScalars['fuel.credits','cng.onsite'] <- -1
  }
  if(sens.guide$vars[guide] != 'facility.capital'){
    unitCostSens[unitCostSens$type == as.character(sens.guide$vars[guide]),states] <- unitCostSens[unitCostSens$type == as.character(sens.guide$vars[guide]),states]*as.numeric(sens.guide$sens[guide])
  }
  var.cost <- data.frame(state = states,
                         orig.price = as.numeric(unitCost[unitCost$type == as.character(sens.guide$vars[guide]),states]),
                         new.price = as.numeric(unitCostSens[unitCostSens$type == as.character(sens.guide$vars[guide]),states]),
                         variable = as.character(sens.guide$vars[guide]))
  costList <- list(); n <- 1; m <- 1
  for (scale_scenario in names(scenarioScalars)){
    costList[[m]] <- CalculateCostData(opData, unitCostSens)
    m <- m + 1
  }
  costDataFrame <- do.call("rbind", costList)
  if(sens.guide$vars[guide] == 'facility.capital'){
    costDataFrame$facility.capital <- costDataFrame$facility.capital*as.numeric(sens.guide$sens[guide])
    var.cost$new.price <- as.numeric(unitCostSens[unitCostSens$type == as.character(sens.guide$vars[guide]),states])*as.numeric(sens.guide$sens[guide])
  }
  profit.npv.irr <- CalculateStateNPVandIRR(costDataFrame)
  sensitivity.df <- data.frame(
    sensitivity = as.numeric(sens.guide$sens[guide]),
    digester.mass = profit.npv.irr[['npv']][['digester.mass']],
    average.cash = profit.npv.irr[['average']][['value']],
    npv = profit.npv.irr[['npv']][['value']],
    irr = profit.npv.irr[['irr']][['value']],
    state = profit.npv.irr[['npv']][['state']],
    scenario = profit.npv.irr[['npv']][['scenario2']]
  )
  sensitivity.df <- merge(sensitivity.df, var.cost)
  sensitivity.df$change <- (sensitivity.df$new.price - sensitivity.df$orig.price) / sensitivity.df$orig.price
  sensitivity_results <- rbind(sensitivity_results, sensitivity.df)
}

sensitivity_results$ind <- paste(sensitivity_results$digester.mass,
                                    sensitivity_results$state,
                                    sensitivity_results$scenario,
                              sep = '-')
write.csv(sensitivity_results, file = file.path(model_path,'outputs','Apr2019','sensitivity_results.csv'))
sensitivity_results <- read.csv(file = file.path(model_path,'outputs','Apr2019','sensitivity_results.csv'), row.names = 1)
sensitivity.vars <- c('facility.capital', 'digestate.tipping', 'electricity.sell','inbound.tipping','rng.sales','compost.sales', 'fuel.credits')

colnames(scenario.labels)[1] <- 'scenario'
sensitivity_results <- merge(sensitivity_results, scenario.labels)
sensitivity_results$scenario.label.f <- factor(sensitivity_results$scenario.label, levels=c('Electricity',
                                                                                           'On-site Fuel Sales',
                                                                                           'Off site Fuel Sales',
                                                                                           'Electricity & Nutrient Recovery',
                                                                                           'Electricity & On-site Composting',
                                                                                           'Electricity & District Heating'))

base.npv <- data.frame(base.npv = results.df$npv, ind = results.df$ind) #creates dataframe with original npv results
sensitivity_results <- merge(sensitivity_results, base.npv) #merges original results to sensitivity results for comparisons


vars.labs <- data.frame(variable = sensitivity.vars,
                        variable.labs = c('Facility Capital (0.5X Price)',
                                          'Digestate Tipping (0.5X Price)',
                                          'Electricity Sales (3X Price)',
                                          'Inbound Tipping (2X Price)',
                                          'Thermal Sales (+ FiT)',
                                          'Compost Sales (3.5X Price)',
                                          'Fuel Credits (+ D3 RiN)')
                                          ) #creates variable labels for graphing
sensitivity_results <- merge(sensitivity_results, vars.labs)

sensitivity_results %>%
  group_by(sensitivity,variable) %>%
  summarize(count = n()) #check: 504 = 504

#NPV to variable change elasticity

sensitivity_results$change[which(sensitivity_results$variable == 'rng.sales' | sensitivity_results$variable == 'fuel.credits')] <- 
  sensitivity_results$new.price[which(sensitivity_results$variable == 'rng.sales' | sensitivity_results$variable == 'fuel.credits')]


sensitivity_results <- sensitivity_results %>%
  mutate(npv.change = (abs(npv-base.npv))/abs(base.npv),
         price.npv.elas = npv.change/change)


# sensitivity_results_npv <- sensitivity_results[!(sensitivity_results$ind %in% base.solvency.npv$ind),] #removes the already solvent facility cases
# sensitivity_results_npv %>%
#   group_by(sensitivity,variable) %>%
#   summarize(count = n()) #check: 414 = 414
# sensitivity_results_irr <- sensitivity_results[!(sensitivity_results$ind %in% base.solvency.irr$ind),] #removes the already solvent facility cases
# sensitivity_results_irr %>%
#   group_by(sensitivity,variable) %>%
#   summarize(count = n()) #check: 367 = 367

sensitivity_results_npv_solvency_90 <- filter(sensitivity_results, npv >= 0, digester.mass == 90000)
View(sensitivity_results_npv_solvency_90 %>%
  group_by(sensitivity,variable,state,scenario.label) %>%
  summarize(count = n(),
            minimum.cap = min(digester.mass)))

View(sensitivity_results %>%
       filter(digester.mass == 90000) %>%
       group_by(state) %>%
       summarize(count = n(),
                 avg.elas = mean(price.npv.elas)))

# sensitivity_results_irr_solvency <- filter(sensitivity_results_irr, irr >= 0.07)
# sensitivity_results_irr_solvency %>%
#   group_by(sensitivity,variable) %>%
#   summarize(count = n()) 

# sens.price.df <- sensitivity_results3 %>%
#   group_by(price,sensitivity) %>%
#   summarize()
# 
# minimum.df <- sensitivity_results3 %>%
#   group_by(state,variable,digester.mass,scenario) %>%
#   summarize(price = min(price))

minimum.df <- sensitivity_results3 %>%
  group_by(state,variable,digester.mass,scenario) %>%
  summarize(sensitivity = max(sensitivity))


minimum.df <- merge(minimum.df, sens.price.df)




####plots####


                             

#Input sensitivities per state

unitCostGraph <- melt(unitCost, id.vars = 'type', variable.name = 'state')
temp <- filter(unitCostGraph, type == 'facility.capital' | type == 'site.capital')
temp.x.labs <- c("Facility Capital", "Site Capital")
ggplot(data = temp) +
  geom_point(mapping = aes(x = type, y = value, colour = state), size = 2) +
  xlab('Type') +
  ylab('Dollars Per 90,000 Ton Per Year Facility') + 
  labs(colour = 'States') +
  scale_x_discrete(labels= temp.x.labs) +
  theme_bw()

temp2 <- filter(unitCostGraph, type == 'inbound.tipping' | type == 'digestate.tipping' | type == 'compost.sales')
temp2.x.labs <- c("Compost Sales", "Digestate Tipping", "Inbound Tipping")
ggplot(data = temp2) +
  geom_point(mapping = aes(x = type, y = value, colour = state), size = 2) +
  xlab('Type') +
  ylab('Dollars Per Ton') + 
  labs(colour = 'States') +
  scale_x_discrete(labels= temp2.x.labs) +
  theme_bw()

#do electricity, cng, rng sales
#1kwh = 0.0034095106405145 MMBTU
temp3 <- filter(unitCostGraph, type == 'electricity.sell' | type == 'cng.sales' | type == 'rng.sales')
temp3[temp3$type == 'electricity.sell',]$value <- temp3[temp3$type == 'electricity.sell',]$value/0.0034095
temp3.x.labs <- c("CNG Off Site", "Electricity Export", "CNG Onsite")
ggplot(data = temp3) +
  geom_point(mapping = aes(x = type, y = value, colour = state), size = 2) +
  xlab('Type') +
  ylab('Dollars Per MMBTU') + 
  labs(colour = 'States') +
  scale_x_discrete(labels= temp3.x.labs) +
  theme_bw()

temp4 <- filter(unitCostGraph, type == 'labor.fte')
temp4.x.labs <- c("Labor")
ggplot(data = temp4) +
  geom_point(mapping = aes(x = type, y = value, colour = state), size = 2) +
  xlab('Type') +
  ylab('Dollars Per Full Time Equivalent') + 
  labs(colour = 'States') +
  scale_x_discrete(labels= temp4.x.labs) +
  theme_bw()


#Base results

p_r <- ggplot(results.df) +
  geom_line(mapping = aes(x=digester.mass/1000, y = (npv/1000000), colour = state), size = 1.25) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  facet_wrap(~scenario.label.f, scales = 'free') +
  xlab('Digester Capacity (1000 Tons Processed per Year)') +
  ylab('Net Present Value (Million $)') +
  labs(color = 'States') +
  expand_limits(y = 0) +
  theme_bw()

ggsave(filename = 'Base NPV Results.jpeg', 
       plot = p_r,
       path = file.path(model_path,'outputs','Apr2019'),
       device = 'jpeg',
       width = 8.5,
       height = 6,
       units = 'in',
       dpi = 300)

p_i <- ggplot(results.df[which(results.df$irr != 0),]) +
  geom_line(mapping = aes(x=digester.mass/1000, y = irr*100, colour = state), size = 1.25) +
  facet_wrap(~scenario.label.f, scales = 'free') +
  xlab('Digester Capacity (1000 Tons Processed per Year)') +
  ylab('Internal Rate of Return (%)') +
  labs(color = 'States') +
  expand_limits(y = 0) +
  theme_bw()

ggsave(filename = 'Base IRR Results.jpeg', 
       plot = p_i,
       path = file.path(model_path,'outputs','Apr2019'),
       device = 'jpeg',
       width = 8.5,
       height = 6,
       units = 'in',
       dpi = 300)

#sensitivity plot


sensitivity_plot <- filter(sensitivity_results,
                           digester.mass == 90000)

sensitivity_plot$ind2 <- paste(sensitivity_plot$variable,
                               sensitivity_plot$scenario,
                          sep = '-')

# remove <- sensitivity_plot[(sensitivity_plot$scenario == 'base-cng.onsite' &
#                     sensitivity_plot$variable == 'electricity.sell') |
#                    (sensitivity_plot$scenario == 'base-cng.offsite' &
#                       sensitivity_plot$variable == 'electricity.sell'),]
# sensitivity_plot <- sensitivity_plot[!(sensitivity_plot$ind2 %in% remove$ind2),]

remove <- sensitivity_plot[(sensitivity_plot$scenario != 'base-district.heating' &
                              sensitivity_plot$variable == 'rng.sales'),]
sensitivity_plot <- sensitivity_plot[!(sensitivity_plot$ind2 %in% remove$ind2),]
remove2 <- sensitivity_plot[(sensitivity_plot$scenario == 'base-cng.offsite' &
                              sensitivity_plot$variable == 'electricity.sell') |
                             (sensitivity_plot$scenario == 'base-cng.onsite' &
                                sensitivity_plot$variable == 'electricity.sell'),]
sensitivity_plot <- sensitivity_plot[!(sensitivity_plot$ind2 %in% remove2$ind2),]


states.lab <- data.frame(name = c('California', 'Connecticut','Hawaii','Minnesota','Oregon','Rhode Island','Vermont'),
                         abv = c('CA', 'CT', 'HI', 'MN', 'OR', 'RI', 'VT'))

vars.labs.2 <- data.frame(variable = sensitivity.vars,
                        variable.labs.2 = c('Facility Capital',
                                          'Digestate Tipping',
                                          'Electricity Sales',
                                          'Inbound Tipping',
                                          'Thermal Sales',
                                          'Compost Sales',
                                          'Fuel Credits')
)
# p_s <- ggplot() +
#   geom_linerange(data = sensitivity_plot,
#                  mapping = aes(x = state,
#                                ymin = base.npv/1000000,
#                                ymax = npv/1000000,
#                                color = variable.labs),
#                  size = 4,
#                  position = position_dodge(0.8)) +
#   geom_hline(yintercept = 0, linetype = 'dashed') +
#   facet_wrap(~scenario.label.f) +
#   scale_x_discrete(labels=states.lab$abv) +
#   xlab('States') +
#   ylab('Net Present Value (million $)') +
#   labs(color = 'Sensitivities') +
#   theme_bw()

#which(sensitivity_plot$variable!='fuel.credits'&
#sensitivity_plot$variable!='rng.sales' &

sensitivity_plot <- merge(sensitivity_plot,vars.labs.2)
p_s <- ggplot(data = sensitivity_plot[which(sensitivity_plot$price.npv.elas!=0),]) +
  geom_linerange(mapping = aes(x = state,
                               ymin = 0,
                               ymax = abs(price.npv.elas),
                               color = variable.labs.2),
                 size = 2.5,
                 position = position_dodge(0.75)) +
  facet_wrap(~scenario.label.f) +
  scale_x_discrete(labels=states.lab$abv) +
  xlab('States') +
  ylab('Price Elasticity of Net Present Value (%)') +
  labs(color = 'Sensitivity Scenario') +
  theme_bw() +
  theme(legend.text = element_text(size = 6),
        legend.title = element_text(size = 8))

ggsave(filename = 'Price Elasticity Net Present Value Report.jpeg', 
       plot = p_s,
       path = file.path(model_path,'outputs','Apr2019'),
       device = 'jpeg',
       width = 7.75,
       height = 5,
       units = 'in',
       dpi = 300)

#cost view

costMelt <- melt(costDataFrame, id.vars = c('digester.mass', 'scenario2', 'state'), measure.vars = colnames(costDataFrame)[3:33])

cost_plot <- filter(costMelt, scenario2 == 'base-base', state =='California', value != 0 )
# op_plot <-filter(opData, state =='California', value != 0 )
results_plot <- filter(results.df, scenario2 == 'base-base', state == 'California')
colourCount = length(unique(cost_plot$variable))
getPalette = colorRampPalette(brewer.pal(8, "RdYlBu")) # 'Spectral', 'Set3', 'Accent', 'YlGnBl', "RdYlBu"

ggplot(cost_plot) +
  geom_area(data = cost_plot, mapping = aes(x = digester.mass/1000, 
                                            y = value*-1/1000000, 
                                            fill = variable), 
            position = 'stack') +
  geom_line(data = results_plot, mapping = aes(x = digester.mass/1000,
                                              y = npv/1000000), 
                                              size =1, linetype = 'dashed') +
  # geom_line(data = results_plot, mapping = aes(x = digester.mass/1000,
  #                                              y = average.cash/1000000), size =1.5, linetype = 'dashed') +
  xlab('Digester Mass (1000tons)') +
  ylab('Million $') +
  scale_fill_manual(values = getPalette(colourCount)) 

#break even


opData_be<- select(opData, total.mass, digester.mass, kwh)
costData_be <- select(costDataFrame, total.mass, digester.mass, total, colnames(costDataFrame)[3:33], state, scenario2)

opcost_be <- merge(opData_be,costData_be)
colnames(opcost_be)[37] <- 'scenario'

breakeven_df <- merge(sensitivity_results, opcost_be)

breakeven_df <- breakeven_df %>%
  mutate(tipping.waste.new = -(new.price*total.mass),
         breakeven.tipping = (total+tipping.waste-tipping.waste.new) / total.mass,
         tipping.waste = NULL)
breakeven_sel <- select(breakeven_df, digester.mass,total.mass,scenario,state,breakeven.tipping,variable,colnames(breakeven_df)[22:52])
breakevenMelt <- melt(breakeven_sel, id.vars = c('digester.mass', 'scenario', 'state','breakeven.tipping','variable', 'total.mass'), measure.vars = colnames(breakeven_df)[22:52], variable.name = 'type')

# breakeven_df <- breakeven_df %>%
#   mutate(elec.sale.new = -(new.price*kwh),
#          breakeven.elec = (total+electricity.sales-elec.sale.new)/kwh,
#          electricity.sales = NULL)
# breakeven_sel <- select(breakeven_df, digester.mass,scenario,state,breakeven.elec,variable,kwh,colnames(breakeven_df)[21:51])
# breakevenMelt <- melt(breakeven_sel, id.vars = c('digester.mass', 'scenario', 'state','breakeven.elec','variable','kwh'), measure.vars = colnames(breakeven_df)[21:51], variable.name = 'type')
# 

breakevenMelt <- merge(breakevenMelt, scenario.labels)
breakevenMelt$scenario.label.f <- factor(breakevenMelt$scenario.label, levels=c('Electricity',
                                                                                            'On-site Fuel Sales',
                                                                                            'Off site Fuel Sales',
                                                                                            'Electricity & Nutrient Recovery',
                                                                                            'Electricity & On-site Composting',
                                                                                            'Electricity & District Heating'))

breakeven_plot <- filter(breakevenMelt,
                         variable == 'inbound.tipping',
                         state == 'Oregon',
                         scenario == 'base-base' |
                           scenario == 'base-cng.onsite' |
                           scenario == 'base-district.heating',
                         value != 0)

# breakeven_plot <- filter(breakevenMelt,
#                          variable == 'electricity.sell',
#                          state == 'Oregon',
#                          scenario == 'base-fertilizer',
#                          value != 0)
cost.labs <- read.csv(file = file.path(model_path,'inputs','cost.labs.csv'))
breakeven_plot <- merge(breakeven_plot,cost.labs)

colourCount = length(unique(breakeven_plot$cost.lab))
getPalette = colorRampPalette(brewer.pal(11, 'RdYlBu')) # 'Spectral', 'Set3', 'Accent', 'RdYlGn', "RdYlBu"

#graph not accurate as using 90K NPV breakeven rather than running NPV across 22.5K-270K

#breakeven tipping
# p_b <- ggplot(breakeven_plot) +
#   geom_area(aes(x = digester.mass/1000,
#                 y = -value/digester.mass,
#                 fill = cost.lab),
#             position = 'stack') +
#   geom_point(mapping = aes(x = digester.mass/1000,
#                            y = breakeven.tipping),
#              size = 1.5) +
#   geom_line(mapping = aes(x = digester.mass/1000,
#                           y = breakeven.tipping),
#             size =0.5,
#             linetype = 'dotted') +
#   geom_text(mapping = aes(x = digester.mass/1000,
#                             y = breakeven.tipping + 15,
#                            label = round(breakeven.tipping)),
#              size = 2.25) +
#   facet_wrap(~scenario.label.f) +
#   xlab('Annual Digester Mass (Thousand Tons)') +
#   ylab('Dollars Per Ton Inbound Waste') +
#   labs(fill = 'Costs / Revenues') +
#   theme_bw() +
#   scale_fill_manual(values = getPalette(colourCount)) +
#   theme(legend.text = element_text(size = 5),
#         legend.title = element_text(size = 8),
#         legend.title.align = 0.5,
#         legend.direction = 'vertical')

#breakeven tipping percent change

p_b <- ggplot(breakeven_plot) +
  geom_point(mapping = aes(x = digester.mass/1000,
                           y = ((breakeven.tipping-69.58)/69.58)*100),
             size = 1.5) +
  geom_line(mapping = aes(x = digester.mass/1000,
                          y = ((breakeven.tipping-69.58)/69.58)*100),
            size =0.5,
            linetype = 'dotted') +
  geom_text(mapping = aes(x = digester.mass/1000,
                          y = ((((breakeven.tipping-69.58)/69.58)+0.05)*100),
                          label = round((((breakeven.tipping-69.58)/69.58)*100))),
            size = 2.25) +
  facet_wrap(~scenario.label.f) +
  xlab('Annual Digester Mass (Thousand Tons)') +
  ylab('Percent Change (%)') +
  labs(fill = 'Costs / Revenues') +
  theme_bw() +
  scale_fill_manual(values = getPalette(colourCount)) +
  theme(legend.text = element_text(size = 5),
        legend.title = element_text(size = 8),
        legend.title.align = 0.5,
        legend.direction = 'vertical')

#breakeven electricity 

# p_b <- ggplot(breakeven_plot) +
#   geom_area(aes(x = digester.mass/1000,
#                 y = -value/kwh,
#                 fill = cost.lab),
#             position = 'stack') + 
#   geom_point(mapping = aes(x = digester.mass/1000,
#                            y = breakeven.elec),
#              size = 1.5) +
#   geom_line(mapping = aes(x = digester.mass/1000,
#                           y = breakeven.elec),
#             size =0.5,
#             linetype = 'dotted') +
#   geom_text(mapping = aes(x = digester.mass/1000,
#                           y = breakeven.elec + 0.2,
#                           label = round(breakeven.elec, digits = 2)),
#             size = 2.5) +
#   facet_wrap(~scenario.label.f) +
#   xlab('Annual Digester Mass (Thousand Tons)') +
#   ylab('Dollars Per Kilowatt-Hour') +
#   labs(fill = 'Costs / Revenues') +
#   theme_bw() +
#   scale_fill_manual(values = getPalette(colourCount)) +
#   theme(legend.text = element_text(size = 6),
#         legend.title = element_text(size = 8),
#         legend.title.align = 0.5,
#         legend.direction = 'vertical')


ggsave(filename = 'Breakeven Oregon Percent Difference.jpeg', 
       plot = p_b,
       path = file.path(model_path,'outputs','Apr2019'),
       device = 'jpeg',
       width = 9,
       height = 5,
       units = 'in',
       dpi = 300)




#### unused code ####


# for(vars in sensitivity.vars) {
#   for(sens in c(0.5,1,2,3,3.5)) {
#     unitCostSens <- unitCost
#     states <- colnames(unitCostSens[,2:ncol(unitCostSens)])
#     if(vars == 'fuel.credits' & sens >= 1) {
#       scenarioScalars['fuel.credits','cng.offsite'] <- -1
#       scenarioScalars['fuel.credits','cng.onsite'] <- -1
#     } else if (vars != 'fuel.credits' & sens < 1){
#       next
#     }
#     if((vars != 'facility.capital' & sens > 1) | 
#        ((vars == 'digestate.tipping' & sens < 1))){
#     unitCostSens[unitCostSens$type == vars,states] <- unitCostSens[unitCostSens$type == vars,states]*sens
#     } else if ((vars != 'facility.capital' & sens < 1) |
#                (vars == 'digestate.tipping' & sens > 1)){
#                
#       next
#     }
#     
#     var.cost <- data.frame(state = states,
#                            orig.price = as.numeric(unitCost[unitCost$type == vars,states]),
#                            new.price = as.numeric(unitCostSens[unitCostSens$type == vars,states]),
#                            variable = vars)
#     costList <- list(); n <- 1; m <- 1
#     for (scale_scenario in names(scenarioScalars)){
#       costList[[m]] <- CalculateCostData(opData, unitCostSens)
#       m <- m + 1
#     }
#     costDataFrame <- do.call("rbind", costList)
#     if((vars == 'facility.capital' & sens < 1)){
#       costDataFrame$facility.capital <- costDataFrame$facility.capital*sens
#       var.cost$new.price <- as.numeric(unitCostSens[unitCostSens$type == vars,states])*sens
#      } else if ((vars == 'facility.capital' & sens > 1)){
#       next
#     }
#     profit.npv.irr <- CalculateStateNPVandIRR(costDataFrame)
#     sensitivity.df <- data.frame(
#       sensitivity = sens,
#       digester.mass = profit.npv.irr[['npv']][['digester.mass']],
#       average.cash = profit.npv.irr[['average']][['value']],
#       npv = profit.npv.irr[['npv']][['value']],
#       irr = profit.npv.irr[['irr']][['value']],
#       state = profit.npv.irr[['npv']][['state']],
#       scenario = profit.npv.irr[['npv']][['scenario2']]
#     )
#     sensitivity.df <- merge(sensitivity.df, var.cost)
#     sensitivity.df$change <- (sensitivity.df$new.price - sensitivity.df$orig.price) / sensitivity.df$orig.price
#     sensitivity_results <- rbind(sensitivity_results, sensitivity.df)
#     }
#   }

# ####grid.search####
# 
# facCap.elec.grid <- expand.grid(facility.capital = c(0.5,0.75,1),
#                           electricity.sell = c(1,1.5,2,2.5,3))
# 
# facCap.tip.grid <- expand.grid(facility.capital = c(0.5,0.75,1),
#                                 inbound.tipping = c(1,3,5,7))
# 
# facCap.elec.therm.grid <- expand.grid(facility.capital = c(0.5,0.75,1),
#                                  electricity.sell = c(1, 1.5,2,2.5,3),
#                                  cng.sales = c(1, 1.5,2)) #for district heating scenario
# 
# #need to enable fuel credits for this one.
# facCap.elec.gas.grid <- expand.grid(facility.capital = c(0.5,0.75, 1),
#                                     electricity.sell = c(1, 1.5,2,2.5,3),
#                                     fuel.credits = c(1,1.5,2,2.5,3,3.5)) #for cng onsite and offsite scenarios
# 
# grid_results <- data.frame()
# grid.search <- facCap.elec.gas.grid 
#   for(sens in 1:nrow(grid.search)) {
#     unitCostSens <- unitCost
#     states <- colnames(unitCostSens[,2:ncol(unitCostSens)])
#     row.names(unitCostSens) <- unitCostSens$type
#     unitCostSens[colnames(grid.search),states] <- unitCostSens[colnames(grid.search),states]*t(grid.search[sens,])
#     costList <- list(); n <- 1; m <- 1
#     for (scale_scenario in names(scenarioScalars)){
#       costList[[m]] <- CalculateCostData(opData, unitCostSens)
#       m <- m + 1
#     }
#     costDataFrame <- do.call("rbind", costList)
#     profit.npv.irr <- CalculateStateNPVandIRR(costDataFrame)
#     grid.df <- data.frame(
#       indicator = str_c(paste(colnames(grid.search),grid.search[sens,],sep='_'),collapse = ';'),
#       digester.mass = profit.npv.irr[['npv']][['digester.mass']],
#       average.cash = profit.npv.irr[['average']][['value']],
#       npv = profit.npv.irr[['npv']][['value']],
#       irr = profit.npv.irr[['irr']][['value']],
#       state = profit.npv.irr[['npv']][['state']],
#       scenario = profit.npv.irr[['npv']][['scenario2']]
#     )
#     grid_results <- rbind(grid_results, grid.df)
#   }
# 
# write.csv(grid_results, file = file.path(model_path,'outputs','Apr2019','grid_results_facCap_elec_gas.csv'))
# 
# facCap_elec <- read.csv(file.path(model_path,'grid_results_facCap_elec.csv'), row.names = 1) #serves all scenarios
# facCap_elec_therm <- read.csv(file.path(model_path,'grid_results_facCap_elec_therm.csv'), row.names = 1) #filter to thermal scenarios
# facCap_elec_gas <- read.csv(file.path(model_path,'grid_results_facCap_elec_gas.csv'), row.names = 1) #filter to cng scenarios
# 
# 
# grid_results <- facCap_elec_therm
# 
# grid_results$ind <- paste(grid_results$digester.mass,
#                                  grid_results$state,
#                                  grid_results$scenario,
#                                  sep = '-')
# 
# grid_results <- merge(grid_results, scenario.labels)
# 
# grid_results_npv <- grid_results[!(grid_results$ind %in% base.solvency.npv$ind),] #removes the already solvent facility cases
# grid_results_npv %>%
#   group_by(indicator) %>%
#   summarize(count = n()) #check: 366 = 366
# 
# grid_results_irr <- grid_results[!(grid_results$ind %in% base.solvency.irr$ind),] #removes the already solvent facility cases
# grid_results_irr %>%
#   group_by(indicator) %>%
#   summarize(count = n()) #check: 377 = 377
# 
# grid_results_npv_solvency <- filter(grid_results_npv, npv >= 0)
# grid_results_npv_solvency_sum <- grid_results_npv_solvency %>%
#   group_by(indicator,scenario.label,state) %>%
#   summarize(capacity.ratio = n()/12,
#             min.capacity = min(digester.mass),
#             max.capacity = max(digester.mass))
# write.csv(grid_results_npv_solvency_sum, file = file.path(model_path,'grid_results_facCap_elec_therm_sum.csv'))
# grid_results_irr_solvency <- filter(grid_results_irr, irr >= 0.07)
# grid_results_irr_solvency %>%
#   group_by(indicator,scenario) %>%
#   summarize(count = n()) 

