# Code written by Jahon Amirebrahimi
# Data Point of Contact: Hanna Breunig & Corinne Scown 
# Use restrictions: must give credit to <https://doi.org/10.1016/j.resconrec.2018.08.022>. 
# Files developed for California biositing tool 2018 developed by Hanna Breunig 
# and adapted by Olga Kavvada and Tyler Huntington for use in webbased tool.


#config ----
required.pkg <- c("RColorBrewer", "sf","sp","raster","dplyr","ggplot2", "rgdal", "gdistance",
                  "vegan", "devtools", "githubinstall",
                  "reshape2", "maps", "udunits2")
pkgs.not.installed <- required.pkg[!sapply(required.pkg, function(p) require(p, character.only=T))]
if (length(pkgs.not.installed) > 0) install.packages(pkgs.not.installed, dependencies=TRUE)
lapply(required.pkg, library, character.only = TRUE) 

wd_path <- paste0(getwd(),"/BiomassLeastCostPath/")
biomass_path <- paste(wd_path,'/biomass', sep = '')
facility_path <- paste(wd_path,'/facilities', sep = '')
proclabs <- read.csv(paste(wd_path,'/proclabs.csv',sep=''))
manurelabs <- read.csv(paste(wd_path,'/manure_labs.csv',sep=''))
mswlabs <- read.csv(paste(wd_path,'/mswlabs.csv',sep=''))
biosolid <- st_read(paste(wd_path,'/Biomass_Shapefile_Drafts','/all_wwtf_ventyx2018.shp', sep = ''))
proc_nonpts <- read.csv(paste(wd_path,'/proc_nonpts.csv',sep=''), header = T)
proc_pts <- read.csv(paste(biomass_path,'/proc_pts.csv',sep=''))
countylegend <- read.csv(paste(wd_path,'/countyleg.csv',sep='')) #county legend import (some county names are zerod out)

#Biomass data formating and consoldation ----

biomass_list <- dir(biomass_path)
for (i in biomass_list) {
  name <- gsub('.csv','',i)
  df <- read.csv(paste(biomass_path,'/',i,sep=''), header = T)
  assign(name, df)
}

#crops - residue and cull wastes from agriculture

crp_vars <- c('RES_lg_dry_16_gross', 'CULL_wet_dryad_wetad_16_gross')
crp2016_pts <- select(crp2016_pts, Type, geometry, CROP, COUNTY, crp_vars)
crp2016_pts <- melt(crp2016_pts, id.vars = c('Type', 'geometry', 'CROP', 'COUNTY'))
crp2016_pts$category <- ifelse(grepl('RES', crp2016_pts$variable), 'Residue', 'Cull')
crp2016_pts$process <- ifelse(grepl('wet', crp2016_pts$variable), 'wetAD', 'COMB_pts')
colnames(crp2016_pts)[grep('CROP',colnames(crp2016_pts))] <- 'type'
crp2016_pts$geometry <- st_as_sfc(crp2016_pts$geometry)
crp2016_pts <- st_sf(crp2016_pts)
st_crs(crp2016_pts) <- "+proj=longlat"

#manure - from hog, cattle, sheep, calf, chickens
manure_vars <- c('Mclf_wet_dryad_wetad_16_gross',
              'Mdy_wet_dryad_wetad_16_gross',
              'Mbf_wet_dryad_wetad_16_gross',
              'Mhg_wet_dryad_wetad_16_gross',
              'Mly_wet_dryad_wetad_16_gross')
manure_pts <- select(manure_pts, Type, geometry, County, manure_vars)
manure_pts <- melt(manure_pts, id.vars = c('Type', 'geometry', 'County'))


manure_pts$type <- NA
for (i in 1:nrow(manurelabs)){
  manure_pts$type <- ifelse(test = grepl(pattern = as.character(manurelabs$labs[i]), 
                      x = manure_pts$variable), 
         yes = as.character(manurelabs$biomass_type[i]), 
         no = manure_pts$type)
}
manure_pts$process <- 'wetAD'
colnames(manure_pts)[grep('Type',colnames(manure_pts))] <- 'category'
colnames(manure_pts)[grep('County',colnames(manure_pts))] <- 'COUNTY'
manure_pts$geometry <- st_as_sfc(manure_pts$geometry)
manure_pts <- st_sf(manure_pts)
st_crs(manure_pts) <- "+proj=longlat"

#MSW - municipal solid wastes - print paper, cardboard, green, fog, lumber...
msw_vars <- c('MSWlb_lg_dry_16_gross',
              'MSWpp_dry_16_gross',
              'MSWcd_dry_16_gross',
              'MSWgn_dry_dryad_16_gross',
              'MSWfog_wet_dryad_wetad_16_gross',
              'MSWot_wet_16_gross',
              'MSWfd_wet_dryad_wetad_16_gross')
msw_CBGcntrd <- select(msw_CBGcntrd, County, Type, geometry, msw_vars)
msw_CBGcntrd <- melt(msw_CBGcntrd, id.vars = c('Type', 'geometry', 'County'))

msw_CBGcntrd$type <- NA
for (i in 1:nrow(mswlabs)){
  msw_CBGcntrd$type <- ifelse(test = grepl(pattern = as.character(mswlabs$labs[i]), 
                                                 x = msw_CBGcntrd$variable) == T, 
                                    yes = as.character(mswlabs$biomass_type[i]), 
                                    no = msw_CBGcntrd$type)
}

msw_CBGcntrd$process <- ifelse(grepl('_dry_16', msw_CBGcntrd$variable), 'COMB_pts',
                               ifelse(grepl('_dryad_16_', msw_CBGcntrd$variable), 'dryAD',
                                      'wetAD'))
colnames(msw_CBGcntrd)[grep('Type',colnames(msw_CBGcntrd))] <- 'category'
colnames(msw_CBGcntrd)[grep('County',colnames(msw_CBGcntrd))] <- 'COUNTY'
msw_CBGcntrd$geometry <- st_as_sfc(msw_CBGcntrd$geometry)
msw_CBGcntrd <- st_sf(msw_CBGcntrd)
st_crs(msw_CBGcntrd) <- "+proj=longlat"

#processor
#values are at county level
#disaggregating county level data to averages based on # of facilities, county, and biomass type
#some counties in facility pts are 0'd out - scrub wiki city/county table to fill
proc_vars <- c('Pcn_wet_dryad_wetad_16_gross','Pdeh_wet_dryad_wetad_16_gross',
               'Pff_wet_dryad_wetad_16_gross','Pwn_wet_dryad_wetad_16_gross',
               'Pbry_wet_dryad_wetad_16_gross','Pdst_wet_dryad_wetad_16_gross',
               'Prm_wet_dryad_wetad_16_gross','Ppm_wet_dryad_wetad_16_gross',
               'Pah_lg_dry_dryad_16_gross','Pas_lg_dry_16_gross','Pws_lg_dry_16_gross',
               'Potns_lg_dry_16_gross','Prh_lg_dry_dryad_16_gross','Pcgt_dry_dryad_16_gross',
               'Pcn_dry_dryad_16_gross','Pdeh_dry_dryad_16_gross','Pbky_dry_dryad_wetad_16_gross',
               'Ptll_dry_dryad_wetad_16_gross','Pgrc_dry_16_gross','PPt_dry_16_gross')

proc_nonpts <- select(proc_nonpts, COUNTY, proc_vars)
proc_nonpts <- melt(proc_nonpts, id.vars = "COUNTY")


proc_nonpts$type <- NA
for (i in 1:nrow(mswlabs)){
  proc_nonpts$type <- ifelse(test = grepl(pattern = as.character(proclabs$labs[i]), 
                                                   x = proc_nonpts$variable), 
                                      yes = as.character(proclabs$biomass_type[i]), 
                                      no = proc_nonpts$type)
}

proc_nonpts$process <- ifelse(grepl('_lg_dry_', proc_nonpts$variable), 'COMB_pts',
                               ifelse(grepl('_dry_dryad_', proc_nonpts$variable), 'dryAD',
                                      ifelse(grepl('_dry_16', proc_nonpts$variable), 'dryAD',
                                      'wetAD')))

proc_pts$COUNTY <- NULL
countylegend <- as_tibble(countylegend)
countylegend <- countylegend %>%
  select(city, County)
proc_pts <- merge(proc_pts, countylegend)
proc_pts <- select(proc_pts, County, geometry, MASTERTYPE, Type)
colnames(proc_pts)[grep('MASTERTYPE',colnames(proc_pts))] <- "type"
colnames(proc_pts)[grep('County',colnames(proc_pts))] <- 'COUNTY'
colnames(proc_pts)[grep('Type',colnames(proc_pts))] <- 'category'
#percounty and biomass type count
proc_count <- proc_pts %>%
  group_by(COUNTY, type) %>%
  summarize(count = n())

proc_pts <- merge(proc_pts, proc_nonpts, by = c("COUNTY","type"))
proc_pts <- merge(proc_pts, proc_count, by.x = c("COUNTY","type"))
proc_pts$value <- proc_pts$value / proc_pts$count

proc_pts$geometry <- st_as_sfc(proc_pts$geometry)
proc_pts <- st_sf(proc_pts)
st_crs(proc_pts) <- "+proj=longlat"

#Biosolids 

biosolid$FLWRTMGALD <- ifelse(biosolid$FLWRTMGALD == -99.0000,
                             0,
                             biosolid$FLWRTMGALD)
#calculate yearly biosolids from flow rate of million gallons per day
#uses CA wide AD facility mean conversion MGD to BDTday value of 92.33414 (calculated below)
biosolid$value <- biosolid$FLWRTMGALD*92.33414*365
biosolid$process <- 'wetAD'
biosolid$category <- 'biosolids'
biosolid$type <- 'biosolids'
biosolid <- st_transform(biosolid, crs = "+proj=longlat")

#stack

biomass_list <- list(crp2016_pts, manure_pts, msw_CBGcntrd, proc_pts, biosolid)
biomass_stack <- st_sf(st_sfc())
st_crs(biomass_stack) <- "+proj=longlat"
for(i in 1:length(biomass_list)) {
  df <- select(biomass_list[[i]], geometry, type, process, value, category, COUNTY)
  biomass_stack <- rbind(biomass_stack, df)
}

biomass_stack$data <- 'biomass'
biomass_stack$COUNTY <- toupper(biomass_stack$COUNTY)
biomass_coords <- as.data.frame(st_coordinates(biomass_stack))

biomass_stack_df <- biomass_stack
st_geometry(biomass_stack_df) <- NULL

biomass_stack_df <- cbind(biomass_stack_df, biomass_coords)
biomass_stack_df$COUNTY <- toupper(biomass_stack_df$COUNTY)

#map biomass types
# ggplot() +
#   geom_sf(data = cali, mapping = aes()) +
#   geom_sf(data = biomass_stack, mapping = aes(color = biomass_type))

#Facility data formating and consoldation ----

facility_list <- dir(facility_path)
for (i in facility_list) {
  name <- gsub('.csv','',i)
  df <- read.csv(paste(facility_path,'/',i,sep=''), header = T)
  assign(name, df)
}

# W2E_pts, AD_pts, DES_CBGcntrd, COMB_pts
  
AD_pts <- transmute(AD_pts,
                    COUNTY = COUNTY,
                    geometry = geometry,
                    value = DayloadBDT,
                    process = Facility_type,
                    type = Feedstock,
                    category = Type)

mean(AD_pts$DayloadBDT/AD_pts$Flow.Average..MGD.) #92.33414 BDT/MGD WWTP Flow used above

W2E_pts <- transmute(W2E_pts,
                    COUNTY = County,
                    geometry = geometry,
                    value = DayLoadBDT,
                    process = Facility_type,
                    type = Feedstock,
                    category = Type)

W2E_pts$COUNTY[8] <- 'Sacramento'
W2E_pts$process[W2E_pts$process == 'high solids'] <- 'dryAD'

facility_stack <- rbind(AD_pts, W2E_pts)
facility_stack$value <- facility_stack$value*365
facility_stack$data <- 'facility'

(average.wetAD.capacity <- mean(facility_stack$value[facility_stack$process == 'wetAD']))
(average.dryAD.capacity <- mean(facility_stack$value[facility_stack$process == 'dryAD']))
#this average is strangely low given San Jose's ZBEST takes 90K BDT per year... assuming other measurements are equally conservative

facility_stack$geometry <- st_as_sfc(facility_stack$geometry)
facility_stack <- st_sf(facility_stack)
st_crs(facility_stack) <- "+proj=longlat"
facility_stack$COUNTY <- toupper(facility_stack$COUNTY)

# total_stack <- rbind(facility_stack, biomass_stack)
# total_stack$COUNTY <- toupper(total_stack$COUNTY)

# analysis ----

# map import ----

cali <- st_as_sf(map(database = 'state', regions = 'California', plot = F, fill = T))
county <- st_as_sf(map(database = 'county', regions = 'California', plot = F, fill = T))
county$ID <- gsub("california,","",county$ID)
county$ID <- toupper(county$ID)
colnames(county)[2] <- 'COUNTY'

cali_dry_ad_locs <- data.frame()
cali_wet_ad_locs <- data.frame()
for (i in 1:nrow(county)) {
  cty_pts <- biomass_stack_df[biomass_stack_df$COUNTY == county$COUNTY[i],]
  cty_pts_dry <- cty_pts[cty_pts$process == 'dryAD',]
  cty_pts_wet <- cty_pts[cty_pts$process == 'wetAD',]
  if (sum(cty_pts_dry$value, na.rm = T) > average.dryAD.capacity) {
  cty_pts_dry$x_weight <- cty_pts_dry$X * cty_pts_dry$value
  cty_pts_dry$y_weight <- cty_pts_dry$Y * cty_pts_dry$value
  cty_dry_wmc_x <- sum(cty_pts_dry$x_weight, na.rm = T) / sum(cty_pts_dry$value, na.rm = T)
  cty_dry_wmc_y <- sum(cty_pts_dry$y_weight, na.rm = T) / sum(cty_pts_dry$value, na.rm = T)
  results_dry <- data.frame(COUNTY = county$COUNTY[i],
                        cty_dry_wmc_x = cty_dry_wmc_x,
                        cty_dry_wmc_y = cty_dry_wmc_y)
  cali_dry_ad_locs <- rbind(cali_dry_ad_locs, results_dry)
  }
  if (sum(cty_pts_wet$value, na.rm = T) > average.wetAD.capacity) {
  cty_pts_wet$x_weight <- cty_pts_wet$X * cty_pts_wet$value
  cty_pts_wet$y_weight <- cty_pts_wet$Y * cty_pts_wet$value
  cty_wet_wmc_x <- sum(cty_pts_wet$x_weight, na.rm = T) / sum(cty_pts_wet$value, na.rm = T)
  cty_wet_wmc_y <- sum(cty_pts_wet$y_weight, na.rm = T) / sum(cty_pts_wet$value, na.rm = T)
  results_wet <- data.frame(COUNTY = county$COUNTY[i],
                            cty_wet_wmc_x = cty_wet_wmc_x,
                            cty_wet_wmc_y = cty_wet_wmc_y)
  cali_wet_ad_locs <- rbind(cali_wet_ad_locs, results_wet)
  }
}

ggplot() +
  geom_sf(data = county, mapping = aes()) +
  geom_point(data = cali_wet_ad_locs, aes(x = cty_wet_wmc_x, y = cty_wet_wmc_y), color = 'blue', size = 1) +
  xlab("Latitude") +
  ylab("Longitude") +
  ggtitle(label = "Potential Wet AD Facilities", subtitle = "Given Available Biomass Capacity")


ggplot() +
  geom_sf(data = county, mapping = aes()) +
  geom_point(data = cali_dry_ad_locs, aes(x = cty_dry_wmc_x, y = cty_dry_wmc_y), color = 'green', size = 1) +
  xlab("Latitude") +
  ylab("Longitude") +
  ggtitle(label = "Potential Dry AD Facilities", subtitle = "Given Available Biomass Capacity")

#create raster layer of biomass
wet_or_dry <- c('wetAD', 'dryAD')
biomass_stack_sp <- as(biomass_stack, 'Spatial')
facility_stack_sp <- as(facility_stack, 'Spatial')
county_sp <- as(county, 'Spatial')
dry_paths <- list()
wet_paths <- list()
for (i in 1:nrow(county_sp)) {
  cty_sp <- county_sp[county_sp$COUNTY == county_sp$COUNTY[i],]
  for(j in 1:length(wet_or_dry)) {
  biomass_pts <- biomass_stack_sp[biomass_stack_sp$COUNTY == county_sp$COUNTY[i] &
                                    biomass_stack_sp$process == wet_or_dry[j],]
  facility_pts <- facility_stack_sp[facility_stack_sp$COUNTY == county_sp$COUNTY[i] &
                                      facility_stack_sp$process == wet_or_dry[j],]
  if(nrow(facility_pts) > 0) {
    if(nrow(biomass_pts) > 0) {
      biomass_pts <- crop(biomass_pts, cty_sp)
      facility_pts <- crop(facility_pts, cty_sp)
      if(is.null(facility_pts) == F) {
        if (is.null(biomass_pts) == F) {
          cty_r <- raster()
          extent(cty_r) <- extent(cty_sp)
          ncol(cty_r) <- 10
          nrow(cty_r) <- 10
          cty_r <- rasterize(coordinates(biomass_pts), cty_r, values = biomass_pts$value, background = 0, fun = sum)
          cond_mat <- matrix(c(
            0, 0.1, 0.001,
            0.11, max(getValues(cty_r)), 1), 
            ncol = 3, byrow = TRUE)
          
          cond_surface <- reclassify(cty_r, cond_mat, include.lowest = TRUE, right = FALSE)
          res_surface <- 1/cond_surface
          tr_surface <- transition(res_surface, transitionFunction = mean, directions = 8)
          tr_surface <- geoCorrection(tr_surface, type="c", scl = FALSE)
          # cost_surface <- accCost(tr_surface, facility_pts_wet_sf_sp)
          paths <- shortestPath(x = tr_surface, 
                                origin = biomass_pts, 
                                goal = facility_pts, 
                                output="SpatialLines")
          if (wet_or_dry[j] == 'wetAD') {
            wet_paths[[i]] <- paths
            plot(cty_sp, main = paste(county_sp$COUNTY[i], wet_or_dry[j]), col = 'grey85') 
            plot(biomass_pts, col = 'green', add = T)
            plot(facility_pts, col = 'red', add = T)
            lines(paths, col = 'yellow',lwd=2)
            Sys.sleep(0.5)
          } else {
            dry_paths[[i]] <- paths
            plot(cty_sp, main = paste(county_sp$COUNTY[i], wet_or_dry[j]), col = 'grey85') 
            plot(biomass_pts, col = 'green', add = T)
            plot(facility_pts, col = 'red', add = T)
            lines(paths, col = 'yellow', lwd=2)
            Sys.sleep(0.5)
          }
        }
      }
    }
  }
  }
}

plot(cali, main = paste('California Least Cost Paths to Existing Wet AD Facilities'), col = 'grey85')
#plot(biomass_stack_sp[biomass_stack_sp$process == wet_or_dry[i],], add = T, col = 'green')
plot(facility_stack_sp[facility_stack_sp$process == wet_or_dry[i],], add = T, col = 'green')
for(j in 1:length(wet_paths)) {
      lines(wet_paths[[j]], col = 'yellow', lwd = 3)
}

plot(cali, main = paste('California Least Cost Paths to Existing Dry AD Facilities'), col = 'grey85')
# plot(biomass_stack_sp[biomass_stack_sp$process == wet_or_dry[i],], add = T, col = 'green')
plot(facility_stack_sp[facility_stack_sp$process == wet_or_dry[i],], add = T, col = 'green')
for(j in 1:length(dry_paths)) {
  lines(dry_paths[[j]], col = 'yellow', lwd = 3)
}

