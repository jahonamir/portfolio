#Title: NHANES Data Consolidation Script
#Authors: Jahon Amirebrahimi, Karen Jetter

#CONFIGURATION ####
# the following R chunk runs two scripts that defines functions and loads packages

source(paste0(getwd(),'/script/functions.R')) #runs a script which defined functions
source(paste0(getwd(),'/script/config.R'))

#FILE PATH IDENTIFICATION ####
#the following R chunk creates a list of nhanes survey cycle years and lists of path locations for datasets grouped by dataset name

# create list of nhanes study cycle years 
start_years <- c('99','01','03','05','07','09','11','13','15') #first a list of starting years /yy/
end_years <- c('00','02','04','06','08','10','12','14','16') #then a list of ending years /yy/
file_year <- paste0(start_years,end_years) #both starting and ended are combined by the paste function

#several lists of dataframe path locations are defined
#nhanes, fped, day 1 intakes and equivalents, day 2 intakes and equivalents, day X intakes and equivalents, food security questions, demographics, income
#the paste function combines our working directory with the target location: NHANES, FPED, FNDDS
#the dir function shows all the files inside that locations: NHANES, FPED, FNDDS
#subset the nhanes paths based on day and individual intakes (DR1IFF, DR2IFF, DRXIFF, FSQ, DEMO, INC), filetype (.XPT)
#subset the fped paths for individual equivalents files (dr1iff, dr2iff, drxiff)

path_nhanes <- paste0(getwd(),'/input/NHANES/',dir(paste0(getwd(),'/input/NHANES'))) #here using dir to list all filenames
path_fped <- paste0(getwd(),'/input/FPED/',dir(paste0(getwd(),'/input/FPED'))) 
path_fndds <- paste0(getwd(),'/input/FNDDS/',dir(paste0(getwd(),'/input/FNDDS')))
path_intake_day1 <- map(c('DR1IFF','.XPT'), str_subset, string = path_nhanes) %>% #here subsetting nhanes folder files based on two indicators 'dr1iff' and '.xpt'
  reduce(intersect)
path_intake_day2 <- map(c('DR2IFF','.XPT'), str_subset, string = path_nhanes) %>% 
  reduce(intersect)
path_intake_dayX <- map(c('DRXIFF','.XPT'), str_subset, string = path_nhanes) %>% 
  reduce(intersect)
path_fsq <- map(c('FSQ','.XPT'), str_subset, string = path_nhanes) %>% 
  reduce(intersect)
path_demo <-  map(c('DEMO','.XPT'), str_subset, string = path_nhanes) %>% 
  reduce(intersect)
path_inc <-  map(c('INQ','.XPT'), str_subset, string = path_nhanes) %>% 
  reduce(intersect)
path_equiv_day1 <- str_subset(path_fped, 'dr1iff') #here subsetting fped folder files based on one indicator 'dr1iff'
path_equiv_day2 <- str_subset(path_fped, 'dr2iff')
path_equiv_dayX <- str_subset(path_fped, 'drxiff')

#NHANES TIME SERIES DATAFRAME CREATION ####
#the following R chunk creates a series of time series dataframes and exports them to the output folder
#1: per individual per intake food group equivalents from 1999 - 2016
#2: per individual demographics from 1999-2016
#3: per individual income from 2007-2016
#4: per individual food security questions 1999-2014 
#In cases with mismatched variables, both are maintained, NAs produced for missing information

#creates empty dataframes to fill
intake_equiv_df <- data.frame()
demo_df <- data.frame()
fsq_df <- data.frame()
income_df <- data.frame()


for (cycle in file_year) { 
  #loops through through cycle years to identify filenames
    if (cycle == '9900' | cycle == '0102') { 
      #cycle years 9900 or 0102 have specified operations
      #both cycles have a single day of data
      #the equivalents dataset provided (9902) is combined for both years
      #the following subsets and merges the equiv1 to each individual record 
    intake1 <- read_xpt(str_subset(path_intake_dayX, cycle)) #reads the intake records for the specified cycle year (9900 or 0102)
    equiv1 <- read_sas(str_subset(path_equiv_dayX, '9902')) #reads the combined equivalents data 
    intake_equiv <- merge(intake1,equiv1, all.x = T) # merges both into a single object intake_equiv, specificying all.x = T to retain all intake data.
    intake_equiv$CYCLE <- cycle #create variable for cycle year
    intake_equiv$DAY <- 1 #create variable for day number
  }
  if (cycle != '9900' & cycle != '0102') {
    #cycle years OTHER THAN 9900 and 0102 have specified operations 
    #these cycles have two days of intakes
    #these cycles have equivalents per day
    #intakes and equivalents are merged by day
    #As columns are identical, day2 column names are changed to day1 
    intake1 <- read_xpt(str_subset(path_intake_day1, cycle)) #read in day 1 intakes
    equiv1 <- read_sas(str_subset(path_equiv_day1, cycle)) # read in day 1 equivalents
    intake_equiv1 <- merge(intake1,equiv1) #merge intake and equivalents
    intake_equiv1$DAY <- 1 #create variable for day number
    intake2 <- read_xpt(str_subset(path_intake_day2, cycle)) #read in day 2 intakes
    equiv2 <- read_sas(str_subset(path_equiv_day2, cycle)) #read in day 2 equivalents
    intake_equiv2 <- merge(intake2,equiv2) #merge intake and equivalents
    intake_equiv2$DAY <- 2 #create variable for day number
    colnames(intake_equiv2) <- colnames(intake_equiv1) #change day2 colnames to day1
    intake_equiv <- rbind(intake_equiv1,intake_equiv2) #combine rows of day 2 to day 1
    intake_equiv$CYCLE <- cycle #create a variable for cycle year
  }
  if (cycle %in% c('9900','0102')) {
    #cycle years 9900 0102 and 0304 have specified operations 
    #unlike the subsequent years, each omit food code descriptions 
    desc <- read_fwf(str_subset(path_fndds, cycle),fwf_widths(c(8,8,NA), c('DRDIFDCD','N','DESCRIPTION'))) #read foodcode descriptions using read fixed width file function
    desc$N <- NULL #remove redundant variable
    intake_equiv <- merge(intake_equiv, desc, all.x = T) #merge descriptions to intake equiv data
  } else if (cycle == '0304') {
    #this cycle handled alternatively due to difference in food code variable name (DRDIFDCD versus DR1IFDCD)
    desc <- read_fwf(str_subset(path_fndds, cycle),fwf_widths(c(8,8,NA), c('DR1IFDCD','N','DESCRIPTION'))) #read foodcode descriptions using read fixed width file function
    desc$N <- NULL #remove redundant variable
    intake_equiv <- merge(intake_equiv, desc, all.x = T) #merge descriptions to intake equiv data
  }
 
  intake_equiv_df <- rbind.fill(intake_equiv_df,intake_equiv)  #bind current cycle dataframe to master dataframe, filling NAs for mismatched variables between each
  
  demo <- read_xpt(str_subset(path_demo, cycle)) #reads the demographic data for the specified cycle year 
  demo$CYCLE <- cycle #create variable for cycle year
  demo_df <- rbind.fill(demo_df, demo) #bind current cycle dataframe to master dataframe, filling NAs for mismatched variables between each

if (cycle %in% file_year[1:length(file_year)-1]) { #chooses only cycles where data exists 
  fsq <- read_xpt(str_subset(path_fsq, cycle)) #reads the food security questions data for the specified cycle year 
  fsq$CYCLE <- cycle #create variable for cycle year
  fsq_df <- rbind.fill(fsq_df, fsq) #bind current cycle dataframe to master dataframe, filling NAs for mismatched variables between each
}
if (cycle %in% file_year[5:length(file_year)]) { #chooses only cycles where data exists 
    income <- read_xpt(str_subset(path_inc, cycle)) #reads the income data for the specified cycle year 
    income$CYCLE <- cycle #create variable for cycle year
    income_df <- rbind.fill(income_df,income) #bind current cycle dataframe to master dataframe, filling NAs for mismatched variables between each
  }
} 

#export data
write_csv(intake_equiv_df, paste0(getwd(),'/output/intake_equiv.csv'))
write_csv(demo_df, paste0(getwd(),'/output/demo.csv'))
write_csv(fsq_df, paste0(getwd(),'/output/fsq.csv'))
write_csv(income_df, paste0(getwd(),'/output/income.csv'))

#generate and write benchmarks
# write_csv(NhanesBenchmark(intake_equiv_df), paste0(getwd(),'/output/intake_equiv_b.csv'))
# write_csv(NhanesBenchmark(demo_df), paste0(getwd(),'/output/demo_b.csv'))
# write_csv(NhanesBenchmark(fsq_df), paste0(getwd(),'/output/fsq_b.csv'))
# write_csv(NhanesBenchmark(income_df), paste0(getwd(),'/output/income_b.csv'))
