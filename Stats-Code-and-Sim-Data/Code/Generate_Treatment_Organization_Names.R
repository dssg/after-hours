####################################################################################
#                                                                                  #
# Authors: Nicholas Mader <nmader@chapinhall.org>                                   #
#         Breanna Miller <breanna.a.miller@gmail.com>                              #
#         Jette Henderson <jette.henderson@gmail.com>                              #
#         Vidhur Vohra <vvohra@gatech.edu>                                         #
#                                                                                  #
# Program Description: This code contains a function that takes as input the       #
# number of organizations, and constructs a list of intervention organization      #
# names from a list of positive adjectives                                         #
#                                                                                  #
####################################################################################


makeOrgNames <- function(numOrg = nOrg){
  sOrgName <- read.table("~/after-hours/Stats-Code-and-Sim-Data/Raw-Data/adjectives.txt", header=F, quote="\"")
  
  nPossibleOrgs <- 372
  # Construct names for Orgs (comes from http://www.fanfiction.net/s/7739576/1/106-Stories-of-Us)
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