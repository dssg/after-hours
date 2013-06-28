# read in the file of adjectives

makeTrtNames <- function(n = nTrt) {
  sCenterName <- read.table("~/adjectives.txt", header=F, quote="\"")

  nPossibleCenters <- 372
  # Construct names for centers (comes from http://www.fanfiction.net/s/7739576/1/106-Stories-of-Us)
  colnames(sCenterName) <- 'Word'
  CenterTypeDraw <- cut(runif(nPossibleCenters), breaks = c(0.0, 0.3, 0.5, 0.7, 0.8, 0.9, 1.0), include.lowest = TRUE)
  sCenterType <- as.character(factor(CenterTypeDraw, labels = c('Camp','Program','Center','Initiative','Club','House')))
  sPossibleCenterName <- paste(sCenterName$Word, sCenterType)

  # Randomly select the number centers you want
  nCenters <- n
  sCenterName <- sPossibleCenterName[sample(1:nPossibleCenters,nCenters,replace=F)]
  return(sCenterName)
}

makeTrtNames(n = 5)