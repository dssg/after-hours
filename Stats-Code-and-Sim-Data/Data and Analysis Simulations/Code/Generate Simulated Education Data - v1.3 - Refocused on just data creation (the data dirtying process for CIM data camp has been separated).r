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
  # 2. School attendance outcome
  # 3. Multiple interventions
  # 4. Geocoding


#---------------------------------#
# SPECIFY DATA GENERATING PROCESS #
#---------------------------------#

  # Data is ~loosely~ reflective of a 9th grade class in CPS
  
    MyDir <- "C:/Users/nmader/Google Drive/Chapin Hall - YMCA Project Collaboration/Data and Analysis Simulations/Code"

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
    sTrtName <- "After School Programming"
  
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

  # Ensure that we have the relationships that we want

    FlipSigns <- function(x1, x2, mult) {
      # Note: the "<<-" assignment operator ensures that the assignment is applied to mVCV in the global environment (i.e. that it persists after the function is run)
      mVCV[x1, x2] <<- mult*abs(mVCV[x1, x2]) 
      mVCV[x2, x1] <<- mult*abs(mVCV[x2, x1])
    }
    
    FlipSigns("TreatmentDraw", "PretestDraw", -1)
    FlipSigns("TreatmentDraw", "BpiDraw",     +2)
    FlipSigns("PretestDraw",   "BpiDraw",     -1)


#--------------------------------------------------#
# DRAW SAMPLE AND GENERATE STUDENT CHARACTERISTICS #
#--------------------------------------------------#

#### Draw a sample of kids from the above

  vMu = as.vector(rep(0, nDataFeatures))
  
  KidData <- mvrnorm(n = nKids, mu = vMu, Sigma = mVCV)
  colnames(KidData) <- sDataFeatures
  StudId <- 1:nKids
  dfKidData <- data.frame(StudId, KidData)
  attach(dfKidData)


#### Set values and scales for each variable

  # Create Gender
    bFemale <- as.numeric(pnorm(GenderDraw) <= 0.51) # Without the "as.numeric()" function, bGender would be a series of "true/false" values. Another way to accomplish the "true/false" to "1/0" conversion is to multiply by 1.
    cGender <- ifelse(1==bFemale, "Treble", "Bass")  # Convert the gender distinction into an abstract one
  
  # Create Race
    cRaceDraw <- cut(pnorm(RaceDraw), breaks = c(0.0, 0.4, 0.5, 0.6, 1.0), include.lowest = TRUE)
    cRace <- factor(cRaceDraw, labels = c("Sour", "Salty", "Bitter", "Sweet"))

  # Rescale Pretest to Mean 100, and broader standard deviation
    Pretest <- round(PretestDraw*20 + 100)
    #hist(PretestScaled)

  # Rescale Bpi
    Bpi <- round(exp(BpiDraw/1.5 + 2)/3)  # NSM: this is messing with things to get something relatively flat, with an interesting tail
    #hist(Bpi, breaks = 0:ceiling(max(Bpi)))
    #table(Bpi)

  # Assign to treatment
    bTreated <- as.numeric(pnorm(TreatmentDraw)<=0.2)


#---------------------------------------------------------------#
# GENERATE SCHOOL CHARACTERISTICS AND COMBINE WITH STUDENT DATA #
#---------------------------------------------------------------#

  # Construct names for schools (comes from http://www.fanfiction.net/s/7739576/1/106-Stories-of-Us)

    sSchNamesData <- read.csv2(file = paste(MyDir, "Raw Data/Random Words for School Names.csv", sep=""), sep = ",", header = TRUE)
    attach(sSchNamesData) # NSM: although I like attaching tons of stuff to the working space (because I don't know any better), R doesn't seem to like it much. Thoughts on cleaner practices?
    SchTypeDraw <- cut(runif(nSchools), breaks = c(0.0, 0.3, 0.5, 0.7, 0.8, 0.9, 1.0), include.lowest = TRUE)
    sSchType <- as.character(factor(SchTypeDraw, labels = c("Academy", "School", "High School", "Preparatory", "Charter", "International")))
    sSchName <- paste(SchNamesList, sSchType)

  # Combine information

    cSchYinYang <- factor(runif(nSchools)<.2, labels = c("Yin", "Yang"))
    dfSchData <- data.frame(1:nSchools, sSchName, (rnorm(nSchools, sd = sqrt(10)) + ifelse(cSchYinYang=="Yin",0,4)), as.character(cSchYinYang))
    colnames(dfSchData) <- c("SchNum", "SchName", "SchEffect", "SchType")

  # Assign Kids to Schools

    AssignedSchNum <- cut(pnorm(SchDraw), nSchools, labels=1:nSchools)
  
  # Merge Student and School Data

    dfKidData_Aug <- data.frame(dfKidData, bFemale, cGender, cRace, Pretest, Bpi, AssignedSchNum)
    dfMyData <- merge(x = dfKidData_Aug, y = dfSchData, by.x = "AssignedSchNum", by.y = "SchNum")
    attach(dfMyData)


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

    Posttest1 <- 50 + 0.90*Pretest   + (-2.5)*Bpi + 3.0*bFemale + SchEffect + 15*StudFactor + 20*bTreated + 3.0*(bTreated*Bpi)  + e2*30 #   + (-0.0008)*Pretest^2
    Posttest2 <- 60 + 0.90*Posttest1 + (-2.5)*Bpi + 3.0*bFemale + SchEffect + 15*StudFactor + 20*bTreated + 3.0*(bTreated*Bpi)  + e3*40 #   + (-0.0008)*Pretest^2
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

    aggregate(x = cbind(Pretest, Posttest1, Posttest2, Bpi, SchEffect, bTreated), by = list(cRace),   FUN = "mean")
    aggregate(x = cbind(Pretest, Posttest1, Posttest2, Bpi, SchEffect, bTreated), by = list(cGender), FUN = "mean")
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
    MyReg1  <- lm(Posttest1 ~ Pretest   + Bpi + bFemale + bTreated + SchInds)
    summary(MyReg1)
    MyReg2  <- lm(Posttest2 ~ Posttest1 + Bpi + bFemale + bTreated + SchInds)
    summary(MyReg2)
  
    # Inspect bias if Bpi is not controlled for (since we may initially withhold this from the Data Campers)
    MyReg    <- lm(Posttest1 ~ Pretest + bFemale + bTreated + SchInds)
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




