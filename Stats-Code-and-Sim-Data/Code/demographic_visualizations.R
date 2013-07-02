####################################################################################
#                                                                                  #
# Author: Nicholas Mader <nmader@chapinhall.org>                                   #
#         Breanna Miller <breanna.a.miller@gmail.com>                              #
#         Jette Henderson <jette.henderson@gmail.com>                              #
#         Vidhur Vohra <vvohra@gatech.edu>                                         #
#                                                                                  #
# Program Description: Creates demographic visualizations using the simulated data #
#                                                                                  #
####################################################################################

setwd("~/after-hours/Stats-Code-and-Sim-Data/")

rm(list = ls())
library(ggplot2)

# load the simulated data
load("Simulated-Data/dfMyData.Rda")
attach(dfMyData)

#* "Standard" variables: avg % by race, % male/female, % free/reduced price lunch, % English Language learners, avg attendance rate, avg math score, 
# avg reading score, GPA this year, "on-track" status if in HS, binary indicator of being suspended for > 5 days this year.  


# Avg % by race bar graph

# with legend to the side, no labels on x-axis
race <- ggplot(data=dfMyData, mapping= aes(x = fRace, fill = factor(fRace))) +
  labs(title = 'Percentage of Students by Race', x = '', y = 'Percent of Student Population') +
  geom_histogram(aes(y=..count../sum(..count..)))

race +
  scale_fill_discrete(name="Race", breaks=attributes(fRace)$levels) +
  theme(axis.ticks = element_blank(), axis.text.x = element_blank())

# without legend, label on x-axis
race + 
  theme(axis.ticks = element_blank(), legend.position="none", axis.text.x=element_text(size=12, color="black"))


# Gender percentage bar graph

# with legend to the side, no labels on x-axis
gender <- ggplot(data = dfMyData, mapping = aes(x = cGender, fill = factor(cGender))) +
  labs(title = 'Percentage of Males and Females', x = '', y = 'Percent of Student Population') + 
  geom_histogram(aes(y=..count../sum(..count..)))

gender + 
  scale_fill_discrete(name="Gender", breaks=attributes(cGender)$levels) + 
  theme(axis.ticks = element_blank(), axis.text.x = element_blank())

# without legend, label on x-axis
gender + 
  theme(axis.ticks = element_blank(), legend.position="none", axis.text.x=element_text(size=12, color="black"))


# Free/Reduced Lunch bar graph

# with legend to the side, no labels on x-axis
frl <- ggplot(data = dfMyData, mapping = aes(x = fFrl, fill = factor(fFrl))) +
  labs(title = 'Percentage of Students Receiving Free or Reduced Price Lunch', x = '', y = 'Percent of Student Population') + 
  geom_histogram(aes(y=..count../sum(..count..)))

frl + 
  scale_fill_discrete(name="Free Reduced Lunch Status", breaks=attributes(fFrl)$levels) + 
  theme(axis.ticks = element_blank(), axis.text.x = element_blank())

# without legend, label on x-axis
frl + 
  theme(axis.ticks = element_blank(), legend.position="none", axis.text.x=element_text(size=12, color="black"))