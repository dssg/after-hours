####################################################################################
#                                                                                  #
# Authors: Nicholas Mader <nmader@chapinhall.org>                                  #
#         Breanna Miller <breanna.a.miller@gmail.com>                              #
#         Jette Henderson <jette.henderson@gmail.com>                              #
#         Vidhur Vohra <vvohra@gatech.edu>                                         #
#                                                                                  #
# Program Description: This code contains a function that takes as input the       #
# number of organizations, and constructs a list of intervention organization      #
# names from a list of positive adjectives                                         #
#                                                                                  #
####################################################################################

setwd("C:/Users/nmader/Documents/GitHub/dssg-after-hours/after-hours/Stats-Code-and-Sim-Data")

makeOrgNames <- function(numOrg = nOrg){
  sOrgName <- read.table("./Raw-Data/adjectives.txt", header=F, quote="\"")
  
  nPossibleOrgs <- nrow(sOrgName)
  colnames(sOrgName) <- 'Word'
  OrgTypeDraw <- cut(runif(nPossibleOrgs), breaks = c(0.0, 0.3, 0.5, 0.7, 0.8, 0.9, 1.0), include.lowest = TRUE)
  sOrgType <- as.character(factor(OrgTypeDraw, labels = c('Camp','Program','Center','Initiative','Club','House')))
  sPossibleOrgName <- paste(sOrgName$Word, sOrgType)
  
  # Randomly select the number organization you want
  nOrgs <- numOrg
  sOrgName <- sPossibleOrgName[sample(1:nPossibleOrgs,nOrgs,replace=F)]
  return(sOrgName)
}

makeOrgNames(5)