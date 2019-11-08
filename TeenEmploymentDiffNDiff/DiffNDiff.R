library(tidyverse)
library(haven)
library(readxl)
library(maps)
library(foreign)
library(stargazer)


# read in writing assignment
teen.employment <- read_stata(paste0(getwd(),"/TeenEmploymentDiffNDiff/teen_employment_1990_2013.dta") )
# which states have an increase in minimum wage over time?


fips <- read_xlsx(paste0(getwd(),"/TeenEmploymentDiffNDiff/fips.xlsx"))
fips$statename <- ifelse(test = fips$statename == 'Geogia', yes = "Georgia", no = fips$statename)
teen.employment <- merge(teen.employment, fips, by='state', all.x = TRUE)
fedminwage <- read_xlsx(paste0(getwd(),"/TeenEmploymentDiffNDiff/fedminwage.xlsx"))
teen.employment <- merge(teen.employment, fedminwage, by='year', all.x = TRUE)

teen.employment$fedstatemindiff <- teen.employment$minwage - teen.employment$fedminwage

#create real wages variable dividing march cpi 2018 (249.554) by cpi variable, and multiplying with minwage

teen.employment$realstateminwage <- (249.554/teen.employment$cpi)*teen.employment$minwage
teen.employment$realfedminwage <- (249.554/teen.employment$cpi)*teen.employment$fedminwage
teen.employment$realfedstatediff <- teen.employment$realstateminwage - teen.employment$realfedminwage

#graph of state growth of minwage

ggplot(data = teen.employment,
       mapping = aes(x=year, y=minwage)) +
  geom_point() +
  facet_wrap(~ statename)

#plot difference in state and fed min wage all states

ggplot(data = teen.employment,
       mapping = aes(x=year, y=fedstatemindiff, color = statename)) +
  geom_point() +
  ylab("Difference between federal and state minimum wage") +
  facet_wrap(~ statename) +
  guides(color=FALSE)


# --------------------------------------

#Mid West East Divide, Treatment East, Control West

teen.employment.subset <- rbind(teen.employment[which(teen.employment$statename == "Minnesota"),], 
                                teen.employment[which(teen.employment$statename == "Wisconsin"),], 
                                teen.employment[which(teen.employment$statename == "Illinois"),],
                                teen.employment[which(teen.employment$statename == "Missouri"),], 
                                teen.employment[which(teen.employment$statename == "Arkansas"),], 
                                teen.employment[which(teen.employment$statename == "Iowa"),], 
                                teen.employment[which(teen.employment$statename == "North Dakota"),], 
                                teen.employment[which(teen.employment$statename == "South Dakota"),],
                                teen.employment[which(teen.employment$statename == "Nebraska"),], 
                                teen.employment[which(teen.employment$statename == "Kansas"),],
                                teen.employment[which(teen.employment$statename == "Oklahoma"),], 
                                teen.employment[which(teen.employment$statename == "Texas"),],
                                teen.employment[which(teen.employment$statename == "Louisiana"),], 
                                teen.employment[which(teen.employment$statename == "Wyoming"),]) 

teen.employment.subset$treatment <- 
  teen.employment.subset$statename == "Minnesota" | 
  teen.employment.subset$statename == "Wisconsin" |
  teen.employment.subset$statename == "Illinois" | 
  teen.employment.subset$statename == "Missouri" |
  teen.employment.subset$statename == "Arkansas" | 
  teen.employment.subset$statename == "Iowa" 


teen.employment.subset$post <- teen.employment.subset$year > 2004
teen.employment.subset$treatment.post <- teen.employment.subset$post * teen.employment.subset$treatment


#summary statistics

summarize <- data.frame(employed = teen.employment.subset$employed, employed1617 = teen.employment.subset$employed1617, employedw = teen.employment.subset$employedw, workft = teen.employment.subset$workft, workpt = teen.employment.subset$workpt, treatment = teen.employment.subset$treatment)
summarizetreatment <- summarize[which(summarize$treatment == TRUE),]
summarizetreatment$treatment <- NULL
summarizecontrol <- summarize[which(summarize$treatment == FALSE),]
summarizecontrol$treatment <- NULL

stargazer(summarizetreatment, type = 'text',
          title='Treated States Dependent Variable Summary Statistics', align=TRUE, 
          covariate.labels=c('Employed Ages 16-19', 'Employed Ages 16-17', 'Employed White Ages 16-19', 'Works Full Time', 'Works Part Time'))

stargazer(summarizecontrol, type = 'text',
          title='Control States Dependent Variable Summary Statistics', align=TRUE, 
          covariate.labels=c('Employed Ages 16-19', 'Employed Ages 16-17', 'Employed White Ages 16-19', 'Works Full Time', 'Works Part Time'))

teen.employment.subset$analysis <- ifelse(test = teen.employment.subset$treatment == TRUE, yes = "Treatment States", no = "Control States")

ggplot(data = teen.employment.subset, mapping = aes(x = analysis)) +
  geom_col(mapping = aes(y = employed))

ggplot(data = teen.employment.subset, mapping = aes(x = employed, y = log(realstateminwage))) +
  geom_point() +
  geom_smooth(se = F)


#regression models
#employed

Model.2 <- lm(employed~treatment+post+treatment.post, teen.employment.subset)
summary(Model.2)

Model.2.Pred <- data.frame(employed.pred = predict(Model.2, teen.employment.subset), treatment.post = teen.employment.subset$treatment.post)

ggplot(data = teen.employment.subset, mapping = aes(x = employed, y = Model.2$fitted.values)) +
  geom_point() +
  geom_smooth(se = F) +
  xlab("Real Fraction Employed Ages 16-19") +
  ylab('Predicted Fraction Employed Ages 16-19')

ggplot(Model.2)

# workft

Model.4 <- lm(workft~treatment+post+treatment.post, teen.employment.subset)
summary(Model.4)

Model.5 <- lm(workft~treatment+post+treatment.post+inschool, teen.employment.subset)
summary(Model.5)

ggplot(data = teen.employment.subset, mapping = aes(x = workft, y = Model.5$fitted.values)) +
  geom_point() +
  geom_smooth(se = F) +
  xlab("Real Fraction Working Full Time Ages 16-19") +
  ylab('Predicted Fraction Working Full Time Ages 16-19')

#workpt

Model.6 <- lm(workpt~treatment+post+treatment.post, teen.employment.subset)
summary(Model.6)

#employed1617

Model.8 <- lm(employed1617~treatment+post+treatment.post+inschool, teen.employment.subset)
summary(Model.8)

ggplot(data = teen.employment.subset, mapping = aes(x = employed1617, y = Model.8$fitted.values)) +
  geom_point() +
  geom_smooth(se = F) +
  xlab("Real Fraction Employed Ages 16-17") +
  ylab('Predicted Fraction Employed Ages 16-17')

#employedw

Model.10 <- lm(employedw~treatment+post+treatment.post, teen.employment.subset)
summary(Model.10)

ggplot(data = teen.employment.subset, mapping = aes(x = employedw, y = Model.10$fitted.values)) +
  geom_point() +
  geom_smooth(se = F) +
  xlab("Real Fraction Employed White Ages 16-19") +
  ylab('Predicted Fraction Employed White Ages 16-19')


# fixed effects (accounting for depression)

Model.X <- lm(employed~treatment+post+treatment.post+log(realstateminwage)+factor(teen.employment.subset$year), teen.employment.subset)
summary(Model.X)

# regression table

stargazer(Model.1, Model.2, Model.8, Model.9, Model.10, Model.11, type = 'text', 
          title='Regression Results', align=TRUE, 
          dep.var.labels=c('Employed Ages 16-19', 'Employed Ages 16-17', 'Employed White Ages 16-19'),
          covariate.labels=c('Treatment States','Post 2004','Treatment X Post 2004', 'Log Real State Minimum Wage'))

# parallel trends test

teen.employment.subset$pre <- teen.employment.subset$year < 2004
teen.employment.subset$treatment.pre <- teen.employment.subset$pre * teen.employment.subset$treatment

parallel.test <- lm(employed~treatment+pre+treatment.pre, teen.employment.subset)
summary(parallel.test)

#summary graphs

ggplot(data = teen.employment.subset,
       mapping = aes(x=year, 
                     y=realfedstatediff, 
                     color= analysis)) +
  geom_point() +
  facet_wrap(~statename) +
  geom_line() +
  theme(legend.position = 'top') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  scale_color_discrete("") + 
  ylab('Real Difference in Federal and State Wages') +
  xlab('Year')
  

ggplot(data = teen.employment.subset,
       mapping = aes(x=treatment, y=employed, fill = statename)) +
  geom_bar(stat = "identity")

#map of decided states
install.packages('maps')
library('maps')
usa <- map_data("state")
teen.employment.subset$statenamelower <- sapply(teen.employment.subset$statename, tolower)

usa.subset <- rbind(usa[which(usa$region == "minnesota"),],
                    usa[which(usa$region == "wisconsin"),],
                    usa[which(usa$region == "illinois"),],
                    usa[which(usa$region == "missouri"),],
                    usa[which(usa$region == "arkansas"),],
                    usa[which(usa$region == "iowa"),],
                    usa[which(usa$region == "north dakota"),],
                    usa[which(usa$region == "south dakota"),],
                    usa[which(usa$region == "nebraska"),],
                    usa[which(usa$region == "kansas"),],
                    usa[which(usa$region == "oklahoma"),],
                    usa[which(usa$region == "texas"),],
                    usa[which(usa$region == "louisiana"),],
                    usa[which(usa$region == "wyoming"),]) 

usa.subset$treatment <- 
  usa.subset$region == "minnesota" |
  usa.subset$region == "wisconsin" |
  usa.subset$region == "illinois" |
  usa.subset$region == "missouri" |
  usa.subset$region == "arkansas" |
  usa.subset$region == "iowa"

ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

usa.subset$treatment <- ifelse(test = usa.subset$treatment == TRUE, yes = "Treatment States", no = "Control States")

ggplot(data = usa) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = NA, color = 'black') + 
  coord_fixed(1.3) +
  geom_polygon(data = usa.subset, 
               aes(x = long, 
                   y = lat, 
                   fill = treatment, 
                   group = group), color = 'black') +
  theme(legend.position = 'top') +
  guides(fill=guide_legend(title=NULL)) +
  ditch_the_axes +
  ggtitle(label = "Selected States for Analysis") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))
  

#map mean wage change

#dummy variable for those states with min wage raises, and those who do not

meandifsubset <- data.frame(tapply(teen.employment.subset$realfedstatediff,teen.employment.subset$statename,mean))
meandifsubset$statename <- row.names(meandifsubset)
colnames(meandifsubset)[1] <- "meandif"
colnames(meandifsubset)[2] <- "region"
meandifsubset$region <- sapply(meandifsubset$region, tolower)
usa.subset <- merge(usa.subset, meandifsubset, by='region', all.x = TRUE)

ggplot(data = usa) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = NA, color = 'black') + 
  coord_fixed(1.3) +
  geom_polygon(data = usa.subset, 
               aes(x = long, 
                   y = lat, 
                   fill = meandif, 
                   group = group), color = 'black') +
  theme(legend.position = 'top') +
  scale_fill_gradient(low = 'grey', high = 'blue') +
  guides(fill=guide_legend(title=NULL)) +
  ditch_the_axes +
  ggtitle(label = "Average Difference Between State and Federal Minimum Wage", subtitle = "In Real Wages of Selected States Between 1990 and 2013") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))


#basic model national level

basic.model.6 <- lm(employed~log(realstateminwage)+fedstatediffdummy, teen.employment)
summary(basic.model.6)

#mean difference all states, map

meandif <- data.frame(tapply(teen.employment$realfedstatediff,teen.employment$statename,mean))
meandif$statename <- row.names(meandif)
colnames(meandif)[1] <- "meandif"
colnames(meandif)[2] <- "region"
meandif$region <- sapply(meandif$region, tolower)
usa <- merge(usa, meandif, by='region', all.x = TRUE)

ggplot(data = usa) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = NA, color = 'black') + 
  coord_fixed(1.3) +
  geom_polygon(aes(x = long, 
                   y = lat, 
                   fill = meandif, 
                   group = group), color = 'black') +
  theme(legend.position = 'top') +
  scale_fill_gradient(low = 'grey', high = 'blue') +
  guides(fill=guide_legend(title=NULL)) +
  ditch_the_axes +
  ggtitle(label = "Average Difference Between State and Federal Minimum Wage", subtitle = "In Real Wages of All States Between 1990 and 2013") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))


#----------------------------------------------

#subset data by new york and connecticut

teen.employment.nyct <- rbind(teen.employment[which(teen.employment$statename == "Connecticut"),], teen.employment[which(teen.employment$statename == "New York"),])

#plot real wage difference

ggplot(data = teen.employment.nyct,
       mapping = aes(x=year, 
                     y=realfedstatediff, 
                     color= statename)) +
  geom_point(show.legend = FALSE) +
  geom_line(show.legend = FALSE) +
  facet_grid(~ statename) +
  ggtitle(label = "Difference Between State and Federal Minimum Wage", subtitle = "Connecticut and New York") +
  ylab("Real Wage Difference March 2018 CPI ($)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))

#choose 1998 as dividor as NY doesn't implement until 2004
#create dummy variable for pre post 1998

teen.employment.nyct$post.1998 <- teen.employment.nyct$year >= 1998

#create state dummy variable

teen.employment.nyct$connecticut <- teen.employment.nyct$statename == "Connecticut"

#create interaction dummy variable between time period and state

teen.employment.nyct$post.1998.connecticut <- teen.employment.nyct$post.1998 * teen.employment.nyct$connecticut

#basic dif dif regression

basic.model.1 <- lm(formula = employed~post.1998+connecticut+post.1998.connecticut,data = teen.employment.nyct)
summary(basic.model.1)


#bivariate regression

bivariate.model <- lm(formula = workpt~realstateminwage,data = teen.employment.nyct)
summary(bivariate.model)

#linear-log bivariate regression

linear.log.bivariate.model <- lm(formula = workpt~log(realstateminwage),data = teen.employment.nyct)
summary(linear.log.bivariate.model)

#log-log bivariate regression

log.log.bivariate.model <- lm(formula = log(workpt)~log(realstateminwage),data = teen.employment.nyct)
summary(log.log.bivariate.model)

#basic linear model including difference and difference with state and time

basic.model <- lm(formula = workpt~realstateminwage+post.1998+connecticut+post.1998.connecticut,data = teen.employment.nyct)
summary(basic.model)

#log-linear basic model

log.basic.model <- lm(formula = log(employed)~realstateminwage+post.1998+connecticut+post.1998.connecticut,data = teen.employment.nyct)
summary(log.basic.model)

#linear-log

log.basic.model <- lm(formula = employed~log(realstateminwage)+post.1998+connecticut+post.1998.connecticut,data = teen.employment.nyct)
summary(log.basic.model)

#log-log basic model

log.log.basic.model <- lm(formula = log(employed)~log(realstateminwage)+post.1998+connecticut+post.1998.connecticut,data = teen.employment.nyct)
summary(log.log.basic.model)

#log-log work part time


log.log.basic.model2 <- lm(formula = log(workpt)~realstateminwage+post.1998+connecticut+post.1998.connecticut,data = teen.employment.nyct)
summary(log.log.basic.model2)


#--------------------------

#isolate nyct subset by years 1993 - 2004, decreases variation in results, less reliable model

teen.employment.nyct.9304 <- subset(x = teen.employment.nyct, subset = year >= 1993 & year <= 2004)
#---------------------------

#run regression of vermont connecticut & massachusetts against new york

teen.employment.nyctvtma <- rbind(teen.employment[which(teen.employment$statename == "Connecticut"),], teen.employment[which(teen.employment$statename == "New York"),], teen.employment[which(teen.employment$statename == "Vermont"),], teen.employment[which(teen.employment$statename == "Massachusetts"),])

ggplot(data = teen.employment.nyctvtma,
       mapping = aes(x=year, 
                     y=realfedstatediff, 
                     color= statename)) +
  geom_point(show.legend = FALSE) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~ statename, nrow = 2) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))

#instead of post 1998, now try 2004, new.york dummy, and interact

teen.employment.nyctvtma$pre.2004 <- teen.employment.nyctvtma$year <= 2004
teen.employment.nyctvtma$new.york <- teen.employment.nyctvtma$statename == "New York"
teen.employment.nyctvtma$pre.2004.new.york <- teen.employment.nyctvtma$pre.2004 * teen.employment.nyctvtma$new.york

#model

basic.model4 <- lm(formula = employed~realstateminwage+pre.2004+new.york+pre.2004.new.york,data = teen.employment.nyctvtma)
summary(basic.model4)

#---------------------------

#instead of pre 2004, now post 1994, connecticut, massachusetts and vermont dummy.

teen.employment.nyctvtma$post.1994 <- teen.employment.nyctvtma$year >= 1994
teen.employment.nyctvtma$ctvtma <- teen.employment.nyctvtma$statename == "Connecticut" | teen.employment.nyctvtma$statename == "Vermont" | teen.employment.nyctvtma$statename == "Massachusetts"
teen.employment.nyctvtma$post.1994.ctvtma <- teen.employment.nyctvtma$post.1994 * teen.employment.nyctvtma$ctvtma

basic.model.5 <- lm(formula = employed~post.1994+ctvtma+post.1994.ctvtma,data = teen.employment.nyctvtma)
summary(basic.model.5)

basic.model5 <- lm(formula = employed~realstateminwage+post.1994+ctvtma+post.1994.ctvtma,data = teen.employment.nyctvtma)
summary(basic.model5)

#isolate period to 1990 - 2004

teen.employment.nyctvtma.9004 <- subset(x = teen.employment.nyctvtma, subset = year >= 1990 & year <= 2004)

basic.model6 <- lm(formula = employed~realstateminwage+post.1994+ctvtma+post.1994.ctvtma,data = teen.employment.nyctvtma.9004)
summary(basic.model6)

#---------------------------

#New df: isolate data from immediately before and after policy change in connecticut, 1997 and 1999

teen.employment.nyct.9799 <- rbind(teen.employment.nyct[which(teen.employment.nyct$year == "1997"),], teen.employment.nyct[which(teen.employment.nyct$year == "1999"),])

#post dummy (1999)

teen.employment.nyct.9799$post.1999 <- teen.employment.nyct.9799$year == 1999

#interact 1999 & connecticut

teen.employment.nyct.9799$post.1999.connecticut <- teen.employment.nyct.9799$post.1999 * teen.employment.nyct.9799$connecticut

basic.model3 <- lm(formula = employed~realstateminwage+post.1999+connecticut+post.1999.connecticut,data = teen.employment.nyct.9799)
summary(basic.model3)

#result, not enough variation!!!! (duh)

#----------------------------

#create pre.2004 variable

teen.employment.nyct$pre.2004 <- teen.employment.nyct$year <= 2004

#interact pre.2004 with connecticut

teen.employment.nyct$pre.2004.connecticut <- teen.employment.nyct$pre.2004 * teen.employment.nyct$connecticut

#another basic model with 2004 time span

basic.model2 <- lm(formula =realstateminwage~employed+pre.2004+connecticut+pre.2004.connecticut,data = teen.employment.nyct)
summary(basic.model2)

#another basic model log log

log.log.basic.model2 <- lm(formula = log(realstateminwage)~log(employed)+pre.2004+connecticut+pre.2004.connecticut,data = teen.employment.nyct)
summary(log.log.basic.model2)

#-----------------------------
#washington and oregon vs Idaho

#subset washington oregon Idaho
teen.employment.waorid <- rbind(teen.employment[which(teen.employment$statename == "Washington"),], teen.employment[which(teen.employment$statename == "Oregon"),], teen.employment[which(teen.employment$statename == "Idaho"),])

#ggplot differences

ggplot(data = teen.employment.waorid,
       mapping = aes(x=year, 
                     y=realfedstatediff, 
                     color= statename)) +
  geom_point(show.legend = FALSE) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~ statename, nrow = 2) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))

#--------------------------

#dummy variable for those states with min wage raises, and those who do not

meandif <- data.frame(tapply(teen.employment$realfedstatediff,teen.employment$statename,mean))
meandif$statename <- row.names(meandif)
colnames(meandif)[1] <- "meandif"
teen.employment <- merge(teen.employment, meandif, by='statename', all.x = TRUE)
teen.employment$fedstatediffdummy <- teen.employment$meandif != 0

#basic model national level

basic.model.6 <- lm(employed~log(realstateminwage)+fedstatediffdummy, teen.employment)
summary(basic.model.6)

#-----------------------


teen.employment.subset <- rbind(teen.employment[which(teen.employment$statename == "Maine"),], 
                                teen.employment[which(teen.employment$statename == "Connecticut"),], 
                                teen.employment[which(teen.employment$statename == "Vermont"),],
                                teen.employment[which(teen.employment$statename == "Massachusetts"),], 
                                teen.employment[which(teen.employment$statename == "Rhode Island"),], 
                                teen.employment[which(teen.employment$statename == "Delaware"),],
                                teen.employment[which(teen.employment$statename == "New York"),], 
                                teen.employment[which(teen.employment$statename == "Virginia"),], 
                                teen.employment[which(teen.employment$statename == "West Virginia"),],
                                teen.employment[which(teen.employment$statename == "Pennsylvania"),], 
                                teen.employment[which(teen.employment$statename == "New Hampshire"),]) 

teen.employment.subset$treatment <- teen.employment.subset$statename == "Maine" | 
  teen.employment.subset$statename == "Connecticut" |
  teen.employment.subset$statename == "Vermont" | 
  teen.employment.subset$statename == "Massachusetts" |
  teen.employment.subset$statename == "Rhode Island" | 
  teen.employment.subset$statename == "Delaware" 

teen.employment.subset$post <- teen.employment.subset$year > 1999
teen.employment.subset$treatment.post <- teen.employment.subset$post * teen.employment.subset$treatment

basic.model.subset <- lm(employed~treatment+post+treatment.post, teen.employment.subset)
summary(basic.model.subset)

basic.model.subset.1 <- lm(workpt~treatment+post+treatment.post, teen.employment.subset)
summary(basic.model.subset.1)

teen.employment.subset.9213 <- subset(x = teen.employment.subset, subset = year >= 1992)

basic.model.subset <- lm(employed~treatment+post+treatment.post, teen.employment.subset.9213)
summary(basic.model.subset)

basic.model.subset.1 <- lm(workpt~treatment+post+treatment.post, teen.employment.subset.9213)
summary(basic.model.subset.1)





