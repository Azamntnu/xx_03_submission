library(tidyverse)
library(lubridate)
library(fhidata)
library(fhi)
library(data.table)
library(plm)
library(tseries)
library(gplots)
library(lmtest)    # For hetoroskedasticity analysis
library(car) 
library(surveillance)
library(outbreaks)
library('ggplot2')
library(hhh4contacts)
library(xlsx)
library(openxlsx)

setwd("D:/azam local/xx_03_submission/xx_03-main/data_raw")
list.files()
df_fhi = readRDS("individual_level_data.RDS") 
head(df_fhi,10)
dim(df_fhi)
colnames(df_fhi)
df_fhi$value
####################################################
# Distinct values
####################################################

df_fhi %>% summarise(n_date = n_distinct(date), n_value= n_distinct(value),n_location_code = n_distinct(location_code))

###################################################
#Create a dataset that contains the aggregated number of sick people per day per municipality.
#Ensure that your aggregated dataset includes rows/days with zero sick people (e.g. if there were no rows for 2010-01-01/municip0301 in data_raw/individual_level_data.RDS then your aggregated dataset will still need to have one row for 2010-01-01/municip0301 with the value 0).

#group by location code
#As the dates are not complete so i used complete function to complete the dates.
#If there are 100 rows for municip0301 on 2010-01-01 it means that there were 100 sick people in municip0301 on 2010-01-01.
#If there are 0 rows for municip0301 on 2010-01-01 it means that there were 0 sick people in municip0301 on 2010-01-01.


df_location_code<-df_fhi %>% select( date, location_code, value)%>%group_by(location_code, date ) %>% summarise(disease_count = n()) 
df_location_code
dim(df_location_code)
df_complete_df<-df_location_code %>%
  complete(date = seq.Date(from = ymd(20100101),
                           to   = ymd(20201231),
                           by   = "day"), fill = list(disease_count = 0))
dim(df_complete_df)
table(df_complete_df$disease_count)
head(df_complete_df, 60)
print(tibble(df_complete_df), n=40)
#######
#checking
df22<-df_complete_df%>% select( date, location_code, disease_count)%>%group_by(date ) %>% tally() 
print(tibble(df22), n=4018)
##############################################################################################################

##Collapse your data down to iso-year/iso-weeks for each municipality. 
df_complete_df$week=isoweek(df_complete_df$date)
df_complete_df$year=isoyear(df_complete_df$date)
df_complete_df$yearW=isoyearweek(df_complete_df$date)
#str(df_complete_df$yearW)

head(df_complete_df)


########################################################



#data(package="fhidata")
locations=as.data.frame(norway_locations_b2020)  #read the dataset Auto in the package ISLR
head(locations)
head(df_complete_df)
main_df<-left_join(df_complete_df, locations, by = c("location_code" = "municip_code"))
head(main_df)
dim(main_df)
###########################################

colnames((main_df))
main_df$location_code[1]
####################################################




TrainData<-main_df %>% 
  select(location_code, date , disease_count, week , year, county_name, yearW) %>%
  filter(date  <= "2019-12-31")

TestData<-main_df %>% 
  select(location_code, date , disease_count, week , year, county_name, yearW) %>%
  filter(date  > "2019-12-31")
dim(TrainData)
dim(TestData)

#####################################################################################333

#for one municipality


reg.conf.intervals <- function(x, y, xnew, ytest) {
  n <- length(y) # Find length of y to use as sample size
  lm.model <- lm((y) ~ x) # Fit linear model
  summary(lm.model)
  # Extract fitted coefficients from model object
  b0 <- lm.model$coefficients[1]
  b1 <- lm.model$coefficients[2]
  
  # Find SSE and MSE
  sse <- sum((y - lm.model$fitted.values)^2)
  mse <- sse / (n - 2)
  
  t.val <- qt(0.975, n - 2) # Calculate critical t-value
  
  # Fit linear model with extracted coefficients
  
  y.fit <- b1 * x + b0
  
  # Find the standard error of the regression line
  se <- sqrt(sum((y - y.fit)^2) / (n - 2)) * sqrt(1 / n + (x - mean(x))^2 / sum((x - mean(x))^2))
  
  # Fit a new linear model that extends past the given data points (for plotting)
  
  y.fit2 <- b1 * xnew + b0
  
  # Warnings of mismatched lengths are suppressed'
  slope.upper <- suppressWarnings(y.fit + t.val * 2*se)
  slope.lower <- suppressWarnings(y.fit - t.val * 2*se)
  
  # Collect the computed confidence bands into a data.frame and name the colums
  bands <- data.frame(cbind(slope.lower, slope.upper,  y))
  bands<-bands[!(bands$y > bands$slope.upper),]
  head(bands)
  colnames(bands) <- c('train Lower Confidence Band', 'train Upper Confidence Band', "sick")
  testband<-reg.conf.intervals.test(y.fit2,  ytest)
  print(testband)
  
  
  return(testband)
}


reg.conf.intervals.test <- function(y.fit2,  ytest) {
  
  n <- length(ytest)
  
  sse <- sum((ytest - y.fit2)^2)
  mse <- sse / (n - 2)
  
  t.val <- qt(0.975, n - 2) # Calculate critical t-value
  # Warnings of mismatched lengths are suppressed'
  slope.upper <- suppressWarnings(y.fit2 + t.val * 2*se)
  slope.lower <- suppressWarnings(y.fit2 - t.val * 2*se)
  
  # Collect the computed confidence bands into a data.frame and name the colums
  bands.test <- data.frame(cbind(slope.lower, slope.upper,  ytest))
  bands.test<-bands.test[(bands.test$y > bands.test$slope.upper),]
  head(bands.test)
  colnames(bands.test) <- c('test Lower Confidence Band', 'test Upper Confidence Band', "test sick")
  #write.xlsx(bands.test, 'D:/azam local/xx_03_submission/xx_03-main/results_task1/municip/YourExcelFile.xlsx')
  
  return((bands.test))
  
}


conf.intervals <- reg.conf.intervals(x,y,xnew, ytest)


  head(conf.intervals)

########################################################################################

for_municipalities <- function() {
  municipalities <- fhidata::norway_locations_b2020$municip_code
  data <- vector("list", length = length(municipalities))
  for (i in seq_along(municipalities)) {
    
    Trdata <- subset(TrainData, location_code ==municipalities[i], select = c("location_code","week","disease_count"))
    Tedata <- subset(TestData, location_code ==municipalities[i], select = c("location_code","week","disease_count"))
    y<-Trdata $disease_count
    x<-Trdata $week
    xnew<-Tedata $week
    ytest<-Tedata $disease_count
    conf.intervals <- reg.conf.intervals(x,y,xnew,ytest)
    list_of_datasets <- list("Name of DataSheet1" = conf.intervals[i])
    #write.xlsx(conf.intervals[i], file = "D:/azam local/xx_03_submission/xx_03-main/results_task1/municip/municipalities.xlsx")
    #write.xlsx(conf.intervals, 'D:/azam local/xx_03_submission/xx_03-main/results_task1/municip/municipalities.xlsx', sheetName = municipalities[i])
    write.xlsx(conf.intervals[[i]], paste(names(conf.intervals)[i], "D:/azam local/xx_03_submission/xx_03-main/results_task1/municip/municipalities.xlsx"))
    
     print(conf.intervals ) }
  }
 

for_municipalities()



########################################################################################
head(TrainData)
for_counties <- function() {
  counties <- fhidata::norway_locations_b2020$county_name
  data <- vector("list", length = length(counties))
  for (i in seq_along(counties)) {
    
    Trdata <- subset(TrainData, location_code == counties[i], select = c("county_name","week","disease_count"))
    Tedata <- subset(TestData, location_code == counties[i], select = c("county_name","week","disease_count"))
    y<-Trdata $disease_count
    x<-Trdata $week
    xnew<-Tedata $week
    ytest<-Tedata $disease_count
    conf.intervals <- reg.conf.intervals(x,y,xnew,ytest)
    list_of_datasets <- list("Name of DataSheet1" = conf.intervals[i])
    #write.xlsx(conf.intervals[i], file = "D:/azam local/xx_03_submission/xx_03-main/results_task1/municip/municipalities.xlsx")
    #write.xlsx(conf.intervals, 'D:/azam local/xx_03_submission/xx_03-main/results_task1/municip/municipalities.xlsx', sheetName = municipalities[i])
    write.xlsx(conf.intervals[[i]], paste(names(conf.intervals)[i], "D:/azam local/xx_03_submission/xx_03-main/results_task1/county/counties.xlsx"))
    
    print(conf.intervals ) }
}


for_counties()

