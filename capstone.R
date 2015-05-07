# Research question

# Hypothesis: In the DM species, the average weight of males
# is higher than females

# Load in the libraries that we'll need

library(RSQLite)
library(ggplot2)

# Set the working directory
setwd("/Users/tracyt/data-carpentry")

# Connect to the portals database

portalDB <- "data/portal_mammals.sqlite"
portalConn <- dbConnect(drv = SQLite(), dbname= portalDB)

# Look at the first 10 rows of the survey table
dbGetQuery(portalConn,"SELECT * from surveys limit 10")

# create an SQL query that just gets the weight and sex for the 'DM' species
speciesDMQuery <- "SELECT weight, sex FROM surveys WHERE species_id='DM' AND weight IS NOT NULL AND (sex='M' OR sex='F')"

speciesDMData <- dbGetQuery(portalConn, speciesDMQuery)

# Look at the structure of the data
str(speciesDMData)

# Look at the beginning of the data
head(speciesDMData)

# Calculate the mean and sd weight for males and females

sex_mean <- tapply(speciesDMData$weight, speciesDMData$sex, mean)
sex_mean

sex_sd <- tapply(speciesDMData$weight, speciesDMData$sex, sd)
sex_sd

# See if the weight of the Males and Females are 
# significantly different from each other with a t-test
# t.test is a built in R function

t.test(speciesDMData$weight~speciesDMData$sex)


# Make a bar plot of average weight and std dev
# and color by sex

# first make a data frame with the sex and average weight

surveys_wgt_summary <- data.frame(sex=unique(speciesDMData$sex), 
                                  mean=sex_mean, 
                                  sd=sex_sd)

surveys_wgt_summary

# plot the mean with standard deviation error bars
# modified from 
# R-cookbook plots http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/

ggplot(data=surveys_wgt_summary, aes(x=sex, y=mean, fill=sex)) + 
  geom_bar(stat="identity") + 
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), colour="black", width=.1, position=position_dodge(.9)) 




