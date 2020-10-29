# Module challenge
# Linear regression analysis on MPG

#setting up potential libraries
library(easypackages)
library("tidyr", "dplyr", "ggplot2", "readxl")

#read in the data that we need
mechampg <- read.csv('MechaCar_mpg.csv',check.names = F,stringsAsFactors = F) #import mpg dataset

#rename the columns to remove white spaces 
mechampg2 <- mechampg%>%rename(
  vehicleLength = `vehicle length`,
  vehicleWeight = `vehicle weight`,
  spoilerAngle = `spoiler angle`,
  groundClearance = `ground clearance`
)
#preview and validate the column name and data
head (mechampg2)

#liner model on multiple continuous data that we have against mpg 
lm(mpg ~ vehicleLength + vehicleWeight + spoilerAngle + groundClearance, mechampg2)

#we will perform a summary function to get more insight on the multiple LR model
summary(lm(mpg ~ vehicleLength + vehicleWeight + spoilerAngle + groundClearance, mechampg2))

#Suspension Coil Summary

#reading the csv into a variable
mechacoil <- read.csv('Suspension_coil.csv',check.names = F,stringsAsFactors = F) #import mpg dataset

#create the summary table by grouping our data by lots, and perform the necessary stat calculation against PSI
coilsummary <- mechacoil %>% group_by(Manufacturing_Lot) %>% summarize(mean=mean(PSI),
                                                                       median = median(PSI), variance= var(PSI), std_dev = sd(PSI))

T-Test 

#generate a random sample of 50 data points from population
sample_table <- mechacoil %>% sample_n(50) 

#performing T-Test of our sample against population PSI
t.test(sample_table$PSI,mu=mean(mechacoil$PSI)) 

