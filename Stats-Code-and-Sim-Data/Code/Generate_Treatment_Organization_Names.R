makeOrgNames <- function(numOrg = nOrg){
  sOrgName <- read.table("~/adjectives.txt", header=F, quote="\"")
  
  nPossibleOrgs <- 372
  # Construct names for Orgs (comes from http://www.fanfiction.net/s/7739576/1/106-Stories-of-Us)
  colnames(sOrgName) <- 'Word'
  OrgTypeDraw <- cut(runif(nPossibleOrgs), breaks = c(0.0, 0.3, 0.5, 0.7, 0.8, 0.9, 1.0), include.lowest = TRUE)
  sOrgType <- as.character(factor(OrgTypeDraw, labels = c('Camp','Program','Center','Initiative','Club','House')))
  sPossibleOrgName <- paste(sOrgName$Word, sOrgType)
  
  # Randomly select the number organization you want
  nOrgs <- numTrt
  sOrgName <- sPossibleOrgName[sample(1:nPossibleOrgs,nOrgs,replace=F)]
  return(sOrgName)
}