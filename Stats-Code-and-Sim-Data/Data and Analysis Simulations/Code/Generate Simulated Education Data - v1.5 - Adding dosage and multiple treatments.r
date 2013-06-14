####################################################################################
#                                                                                  #
# Develop Simulation Dataset for Analysis Mock-ups in YMCA-Chapin Collaboration    #
#                                                                                  #
# Author: Nicholas Mader <nmader@chapinhall.org>                                   #
#                                                                                  #
####################################################################################

  # Clear the work space
    rm(list=ls())

#--------------------#
# LIST OF PRIORITIES #
#--------------------#

  # 1. Dosage in treatment
  #   1a. Generate multiple treatment sites
  # 2. School attendance outcome (modeled as negative binomial)
  # 3. Multiple interventions
  # 4. Geocoding (XY draws)


#---------------------------------#
# SPECIFY DATA GENERATING PROCESS #
#---------------------------------#

# Data is ~loosely~ reflective of a 9th grade class in CPS

MyDir <- "C:/Users/nmader/Documents/My Dropbox/Center for Impact Measurment/DataCamp/Data Cleaning and Analysis Materials/"
setwd(MyDir)

set.seed(60637) # The seed value could be any number. I'm just picking an auspicious one.
library("MASS")
library("plyr")
library("ggplot2")
library("foreign")
"%&%" <- function(...){paste(..., sep = "")}
comment <- function(...){}

nKids     <- 24268 # To make the sample equivalent to a year of HS enrollment in CPS
nSchools  <- 106
sTrtName <- "Learn Good! Enrollment"

# Set skeleton for the data draws

sDataFeatures <- c("PretestDraw", "StudFactor", "TreatmentDraw" , "BpiDraw", "RaceDraw", "SchDraw", "GenderDraw") #
nDataFeatures <- length(sDataFeatures) 

# Generate a variance-Covariance matrix for all data features. These features will be transformed below into new distributions and magnitudes.

mVCV <- matrix(nrow = nDataFeatures, ncol = nDataFeatures, dimnames = list(sDataFeatures, sDataFeatures))
for (i in 1:nDataFeatures) {
  for (j in 1:i) {
    if (i == j) mVCV[i, j] <- 1 else {
      rho <- max(min(rnorm(1, sd = 0.2), 1), -1) # This will result in correlations generally close to 0, and truncated within [0,1]
      mVCV[i,j] <- rho
      mVCV[j,i] <- rho # These assignments ensure that the variance covariance matrix is symmetric
    }
  }
}


# Make adjustments to the random draws of variable relationships, so emphasize the variable relationships that we want.

NudgeVCV <- function(x1, x2, mult=NULL, newval=NULL) {
  # Note: the "<<-" assignment operator ensures that the assignment is applied to mVCV in the global environment (i.e. that the change persists after the function is run)
  
  mVCV[x1, x2] <<- ifelse(is.null(newval), as.numeric(mult*abs(mVCV[x1, x2])), newval)
  mVCV[x2, x1] <<- ifelse(is.null(newval), as.numeric(mult*abs(mVCV[x1, x2])), newval)
}

NudgeVCV("TreatmentDraw", "PretestDraw",   mult = -1)
NudgeVCV("TreatmentDraw", "BpiDraw",       mult = +1)
NudgeVCV("PretestDraw",   "BpiDraw",       mult = -1)
NudgeVCV("PretestDraw",   "RaceDraw",      mult = +1)
NudgeVCV("RaceDraw",      "StudFactor",    newval = +0.15)
NudgeVCV("RaceDraw",      "BpiDraw",       newval = -0.20)
NudgeVCV("RaceDraw",      "SchDraw",       newval = +0.45)
# The randomly draw correlation is so low that it's worth just reassigning, rather than multiplying by a huge number
mVCV

#--------------------------------------------------#
# DRAW SAMPLE AND GENERATE STUDENT CHARACTERISTICS #
#--------------------------------------------------#

#### Draw a sample of kids from the above

vMu = as.vector(rep(0, nDataFeatures))

KidData <- mvrnorm(n = nKids, mu = vMu, Sigma = mVCV)
colnames(KidData) <- sDataFeatures
StudId <- 1:nKids
dfKidData <- data.frame(StudId, KidData)

#### Set values and scales for each variable

# Create Gender
bGender <- as.numeric(pnorm(dfKidData$GenderDraw) <= 0.51) # Without the "as.numeric()" function, bGender would be a series of "true/false" values. Another way to accomplish the "true/false" to "1/0" conversion is to multiply by 1.
cGender <- ifelse(1==bGender, "Treble", "Bass")  # Convert the gender distinction into an abstract one

# Create Race
cRaceDraw <- cut(pnorm(dfKidData$RaceDraw), breaks = c(0.0, 0.4, 0.5, 0.6, 1.0), include.lowest = TRUE)
cRace <- factor(cRaceDraw, labels = c("Sour", "Salty", "Bitter", "Sweet"))

# Rescale Pretest to Mean 100, and broader standard deviation
Pretest <- round(dfKidData$PretestDraw*20 + 100)
#hist(PretestScaled)

# Rescale Bpi
Bpi <- round(exp(dfKidData$BpiDraw/1.5 + 2)/3)  # NSM: this is messing with things to get something relatively flat, with an interesting tail
#hist(Bpi, breaks = 0:ceiling(max(Bpi)))

# Assign to treatment
bTreated <- as.numeric(-1.0 + (-0.01)*Pretest + (0.1)*Bpi + ifelse(cRace=="Sour", 0.5, 0.0) +
  ifelse(cRace=="Salty", 0.25, 0.0) + rnorm(nKids) > 0)
mean(bTreated)


#---------------------------------------------------------------#
# GENERATE SCHOOL CHARACTERISTICS AND COMBINE WITH STUDENT DATA #
#---------------------------------------------------------------#

# Construct names for schools (comes from http://www.fanfiction.net/s/7739576/1/106-Stories-of-Us)

sSchNamesData <- read.csv2(file = paste(MyDir, "Raw Data/Random Words for School Names.csv", sep=""), sep = ",", header = TRUE)
SchTypeDraw <- cut(runif(nSchools), breaks = c(0.0, 0.3, 0.5, 0.7, 0.8, 0.9, 1.0), include.lowest = TRUE)
sSchType <- as.character(factor(SchTypeDraw, labels = c("Academy", "School", "High School", "Preparatory", "Charter", "International")))
sSchName <- paste(sSchNamesData$SchNamesList, sSchType)

# Combine information

cSchYinYang <- factor(runif(nSchools)<.2, labels = c("Yin", "Yang"))
dSchEffect <- ((1:nSchools*5)/nSchools + rnorm(nSchools, sd = sqrt(5)) + ifelse(cSchYinYang=="Yin",0,4))
dfSchData <- data.frame(1:nSchools, sSchName, dSchEffect, as.character(cSchYinYang))
colnames(dfSchData) <- c("SchNum", "SchName", "SchEffect", "SchType")

# Assign Kids to Schools

SchAssignmentDraws <- cumsum((runif(nSchools)+1.0)/3)
SchAssignmentCuts <- SchAssignmentDraws/max(SchAssignmentDraws)
AssignedSchNum <- cut(pnorm(dfKidData$SchDraw), c(0,SchAssignmentCuts), labels=1:nSchools)

# Merge Student and School Data

dfKidData <- data.frame(KidData, cGender, cRace, Pretest, bTreated, Bpi, AssignedSchNum)
dfMyData <- merge(x = dfKidData, y = dfSchData, by.x = "AssignedSchNum", by.y = "SchNum")
rm(AssignedSchNum, Bpi, cGender, cRace, Pretest, bTreated)
attach(dfMyData)
aggregate(x = cbind(SchEffect, RaceDraw), by = list(cRace),    FUN = "mean")


#-----------------------------#
# CONSTRUCT OUTCOME VARIABLES #
#-----------------------------#

# Draw errors to get a random component
e <- data.frame(mvrnorm(n = nKids, mu = c(0, 0, 0, 0), Sigma = matrix(c(1.0, 0.3, 0.2, 0.2,
                                                                        0.3, 1.0, 0.2, 0.2,
                                                                        0.2, 0.2, 1.0, 0.2,
                                                                        0.2, 0.2, 0.2, 1.0), nrow = 4)))
colnames(e) <- c("e1", "e2", "e3", "e4")
attach(e)

# Generate several years of post-test data, where the Data Generating Process is the same except for increasing mean and increasing error variance

Posttest1 <- 50 + 0.9*Pretest   + (-4.0)*Bpi + 3.0*bGender + dfMyData$SchEffect + 15*StudFactor + 15*bTreated  + e2*30 #   + (-0.0008)*Pretest^2 + 3.0*(bTreated*Bpi)
Posttest2 <- 60 + 0.9*Posttest1 + (-4.0)*Bpi + 3.0*bGender + dfMyData$SchEffect + 15*StudFactor + 15*bTreated  + e3*40 #   + (-0.0008)*Pretest^2 + 3.0*(bTreated*Bpi)
Posttest1 <- round(Posttest1)
Posttest2 <- round(Posttest2)
#summary(Posttest1)
#hist(Posttest1)

# Generate binary outcome, with instrument that can be used for selection. Interpretation is dropping out, shocked by ... pregnancy?

ystar <- 0.5 + (-0.03)*Pretest + 0.5*Bpi + ifelse(cRace == "Sour", 1.0, 0) + e2
DroppedOut <- as.numeric(ystar > 0)
mean(DroppedOut)

# Generate continuous outcome observed conditional on binary outcome
# Interpretation is Act conditional on reaching an age where student would apply to college.

ActTrue <- 15 + 0.05*Pretest + (-0.5)*Bpi + e3
Act <- ActTrue
is.na(Act) <- (DroppedOut == 1)
summary(Act)


#----------------------------------------#
# INSPECT THE PROPERTIES OF THE DATA SET #
#----------------------------------------#

# # # Construct conditional averages and descriptives # # #

aggregate(x = cbind(Pretest, Posttest1, Posttest2, Bpi, StudFactor, SchEffect, bTreated), by = list(cRace),    FUN = "mean")
aggregate(x = cbind(Pretest, Posttest1, Posttest2, Bpi, StudFactor, SchEffect, bTreated), by = list(cGender),  FUN = "mean")
aggregate(x = cbind(Pretest, Posttest1, Posttest2, Bpi, StudFactor, SchEffect, bGender),  by = list(bTreated), FUN = "mean")

table(bTreated, cRace)

cor(cbind(Pretest, Posttest1, Posttest2, Bpi, bTreated))
var(SchEffect); var(Posttest1); var(Posttest2)


# # # Draw quantile curve of how pretests, Bpi, and different racial composition varies across schools # # #

# ....

# # # Can we recover the true parameters? # # #

# NSM: I need to return to this section. There are more visualizations to explore here, and I've changed the data structure a decent amount since
# first writing this section up

Pretest2 <- Pretest^2
dTreated.Bpi <- bTreated * Bpi
SchInds  <- model.matrix(~AssignedSchNum)
MyReg1  <- lm(Posttest1 ~ Pretest   + Bpi + bGender + bTreated + SchInds)
summary(MyReg1)
MyReg2  <- lm(Posttest2 ~ Posttest1 + Bpi + bGender + bTreated + SchInds)
summary(MyReg2)

# Inspect bias if Bpi is not controlled for (since we may initially withhold this from the Data Campers)
MyReg    <- lm(Posttest1 ~ Pretest + bGender + bTreated + SchInds)
summary(MyReg)


#---------------------#
# Leftovers for Later #
#---------------------#

  # double the sample and reverse only the signs of the errors to ensure that regressions give exact parameters back (in linear regression)
  # continuous treatment effect
  # multiple treatments
  # endogeneity of the treatment, which would come with instruments
  # count outcome
  # floor effects for tests (for application of tobit)




