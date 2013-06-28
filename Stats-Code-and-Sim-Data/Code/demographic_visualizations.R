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
load("Simulated-Data/dfFinalData.Rda")

#* "Standard" variables: avg % by race, % male/female, % free/reduced price lunch, % English Language learners, avg attendance rate, avg math score, 
# avg reading score, GPA this year, "on-track" status if in HS, binary indicator of being suspended for > 5 days this year.  

# Avg % by race
# Gender percentage bar graph
fGender <- data.frame(as.factor(bGender))
gender <- ggplot(data = fGender, 
               mapping = aes(x = as.factor.bGender., fill = factor(as.factor.bGender.)))

gender + geom_histogram(aes(y=..count../sum(..count..))) +
  labs(title = 'Percentage of Males and Females', x = '',
       y = 'Percent of Student Population') + scale_fill_discrete(
         name="Gender",
         breaks=c("0", "1"),
         labels=c("Male", "Female")) + theme(axis.ticks = element_blank(), axis.text.x = element_blank())