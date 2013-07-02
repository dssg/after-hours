####################################################################################
#                                                                                  #
# Author: Nicholas Mader <nmader@chapinhall.org>                                   #
#         Breanna Miller <breanna.a.miller@gmail.com>                              #
#         Jette Henderson <jette.henderson@gmail.com>                              #
#         Vidhur Vohra <vvohra@gatech.edu>                                         #
#                                                                                  #
# Program Description: This code generates simulated academic, demographic, and    #
# outcome data for a single cohort of students                                     #
#                                                                                  #
####################################################################################

   #-----------------------#
   # Adapted Hungarian Notation #
   #-----------------------#

  # n number (as a counter)
  # b binary
  # c categorical
  # s string
  # df dataframe
  # m matrix
  # v vector
  # d decimal (maybe we should change this to be clearer)
  # f factor
  
  #-----------------------#
  # Set up the work space #
  #-----------------------#

  rm(list=ls())
  options("error")
 # MyDir <- "C:/Users/nmader/Documents/GitHub/dssg-after-hours/Stats-Code-and-Sim-Data/Code"
 # setwd(MyDir)
  setwd("~/after-hours/Stats-Code-and-Sim-Data/")
#  CopaDir <- "C:/Users/nmader/Documents/Econ/Chapin Hall Projects/Chicago GIS Files/copa data/"
  help("Sweave", package="utils")
  # Import function that generates the n treatment centers randomly
  source("Code/Generate_Treatment_Organization_Names.R")
  
  set.seed(60637) # The seed value could be any number. I'm just picking an auspicious one.
  library("MASS")
  library("plyr")
  library("ggplot2")
  library("foreign")
  "%&%" <- function(...){paste(..., sep = "")}
  comment <- function(...){}

#---------------------------------#
#---------------------------------#
# SPECIFY DATA GENERATING PROCESS #
#---------------------------------#
#---------------------------------#


  # Data is ~loosely~ reflective of a 9th grade class in CPS
    nKids     <- 24268 # To make the sample equivalent to a year of HS enrollment in CPS
    nSchools  <- 106
    # TO DO: add in the generalized program names
  
  # Set skeleton for the data draws
  
    sDataFeatures <- c("InitTestDraw", "StudFactor", "TreatmentDraw", "RaceDraw", "SchDraw", "GenderDraw", "FrlDraw") # Give a name to each one of the features we have for each of the kids. THis needs to be updated because any given kid will have tests back to the 3rd grade
  # StudFactor is a fixed effect
  # Note: define the rest of the features
    nDataFeatures <- length(sDataFeatures) 
  
  # Generate a variance-Covariance matrix for all data features. These features will be transformed below into new distributions and magnitudes.

    mVCV <- matrix(nrow = nDataFeatures, ncol = nDataFeatures, dimnames = list(sDataFeatures, sDataFeatures)) # this is more of a correlation matrix
    for (i in 1:nDataFeatures) {
      for (j in 1:i) {
        if (i == j) mVCV[i, j] <- 1 else {
          rho <- max(min(rnorm(1, sd = 0.2), 1), -1) # This will result in correlations generally close to 0, and truncated within [0,1]. The .2 is an arbitrary choice.
          mVCV[i,j] <- rho
          mVCV[j,i] <- rho # These assignments ensure that the variance covariance matrix is symmetric
        }
      }
    }


  # Make adjustments to the random draws of variable relationships, so emphasize the variable relationships that we want.
  # This will take care of impossible correlations (e.g., feature a is positively and highly correlated with b and c, but b is highly and negatively correlated with c and a)
    NudgeVCV <- function(x1, x2, mult=NULL, newval=NULL) {
      # Note: the "<<-" assignment operator ensures that the assignment is applied to mVCV in the global environment (i.e. that the change persists after the function is run)
      # These newval scalars are approximate (i.e., not calibrated)
      mVCV[x1, x2] <<- ifelse(is.null(newval), as.numeric(mult*abs(mVCV[x1, x2])), newval)
      mVCV[x2, x1] <<- ifelse(is.null(newval), as.numeric(mult*abs(mVCV[x1, x2])), newval)
    }
    
    NudgeVCV("TreatmentDraw", "InitTestDraw",   mult = -1) 
    NudgeVCV("InitTestDraw",   "RaceDraw",      mult = +1)
    NudgeVCV("RaceDraw",      "StudFactor",    newval = +0.15) 
    NudgeVCV("RaceDraw",      "SchDraw",       newval = +0.45) # This is intended to reflect strong segregation across schools
    # The randomly draw correlation is so low that it's worth just reassigning, 
    # rather than multiplying by a huge number
    mVCV


#--------------------------------------------------#
#--------------------------------------------------#
# DRAW SAMPLE AND GENERATE STUDENT CHARACTERISTICS #
#--------------------------------------------------#
#--------------------------------------------------#

#### Draw a sample of kids using the correlation matrix generated above

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
    fRace <- factor(cRaceDraw, labels = c("Sour", "Salty", "Bitter", "Sweet"))

  # Create Free/Reduced Price Lunch
    bFrl <- as.numeric(pnorm(dfKidData$FrlDraw) <= 0.87)

  # Rescale Pretest to Mean 100, and broader standard deviation
    Pretest <- round(dfKidData$InitTestDraw*20 + 100)
    #hist(PretestScaled)

  # Assign to treatment status 
    bTreated <- as.numeric(-1.0 + (-0.01)*Pretest + ifelse(fRace=="Sour", 0.5, 0.0) +
                  ifelse(fRace=="Salty", 0.25, 0.0) + rnorm(nKids) > 0)
    mean(bTreated) # this is a sanity check because we would expect the number of treated people to be low


#---------------------------------------------------------------#
#---------------------------------------------------------------#
# GENERATE SCHOOL CHARACTERISTICS AND COMBINE WITH STUDENT DATA #
#---------------------------------------------------------------#
#---------------------------------------------------------------#

  # Construct names for schools (comes from http://www.fanfiction.net/s/7739576/1/106-Stories-of-Us)
    sSchNamesData <- read.table("Raw-Data/Random_Words_for_School_Names.csv", header=T, quote="\"")
    SchTypeDraw <- cut(runif(nSchools), breaks = c(0.0, 0.3, 0.5, 0.7, 0.8, 0.9, 1.0), include.lowest = TRUE)
    sSchType <- as.character(factor(SchTypeDraw, labels = c("Academy", "School", "High School", "Preparatory", "Charter", "International")))
    sSchName <- paste(sSchNamesData$SchNamesList, sSchType)

  # Combine information - this is supposed to emulate charter vs. non-charter

    cSchYinYang <- factor(runif(nSchools)<.2, labels = c("Yin", "Yang"))
    dSchEffect <- ((1:nSchools*5)/nSchools + rnorm(nSchools, sd = sqrt(5)) + ifelse(cSchYinYang=="Yin",0,4))
    dfSchData <- data.frame(1:nSchools, sSchName, dSchEffect, as.character(cSchYinYang))
    colnames(dfSchData) <- c("SchNum", "SchName", "SchEffect", "SchType")

  # Assign Kids to Schools

    vSchAssignmentDraws <- cumsum((runif(nSchools)+1.0)/3) # The scalars here are a way of tinkering with the school sizes
    vSchAssignmentCuts <- vSchAssignmentDraws/max(vSchAssignmentDraws)
    AssignedSchNum <- cut(pnorm(dfKidData$SchDraw), c(0,vSchAssignmentCuts), labels=1:nSchools)
  
  # Merge Student and School Data

    dfKidData <- data.frame(KidData, cGender, fRace, Pretest, bTreated, AssignedSchNum)
    dfMyData <- merge(x = dfKidData, y = dfSchData, by.x = "AssignedSchNum", by.y = "SchNum")
    rm(AssignedSchNum, cGender, fRace, Pretest, bTreated)
    attach(dfMyData)
    aggregate(x = cbind(SchEffect, RaceDraw), by = list(fRace),    FUN = "mean")


#-------------------------------------------------------------------------#
#-------------------------------------------------------------------------#
# GENERATE TREATMENT CENTER CHARACTERISTICS AND COMBINE WITH STUDENT DATA #
#-------------------------------------------------------------------------#
#-------------------------------------------------------------------------#

  # Generate Names and Treatment Parameters for Treatment Centers
  # Note: Need to make treatment center code a function and import it in
    #sTrtNames  <- c("Davonale", "Albany Lawn", "Loganwood", "Engle Park", "East Parkdale");
    nOrg <- 20
    sOrgNames <- makeOrgNames(n = nOrg)
    mOrgParams <- cbind(runif(nOrg)*10, runif(nOrg))
      names(mOrgParams) <- c("Intercept", "Interaction") 
                        
  # Draw Treatment Center Locations

    TrtLocSource <- read.csv("Raw-Data/Extracted Addresses for Simulated Students.csv", header=TRUE)
    # X and Y are the latitude and longitude of the possible treatment centers
    TrtLocXYData <- cbind(TrtLocSource$LATITUDE, TrtLocSource$LONGITUDE)
      colnames(TrtLocXYData) <- c("X", "Y")
      TrtLocXYData <- TrtLocXYData[(!is.na(TrtLocXYData[,"X"])) & (!is.na(TrtLocXYData[,"Y"])),]
    # Select the number of treatment centers from the list of possible treatment centers
    TrtXY <- TrtLocXYData[ceiling(runif(nOrg)*nrow(TrtLocXYData)), ]

    dfTrtData <- data.frame(mOrgParams, TrtXY)

  # Draw Student Location Data
    StudLocSource <- read.csv("Raw-Data/Extracted Addresses for Simulated Students.csv", header = TRUE)
    StudLocXYData <- cbind(TrtLocSource$LATITUDE, TrtLocSource$LONGITUDE)
    colnames(StudLocXYData) <- c("X", "Y")
    StudLocXYData <- StudLocXYData[(!is.na(StudLocXYData[,"X"])) & (!is.na(StudLocXYData[,"Y"])),]
    StudXY <- StudLocXYData[ceiling(runif(nKids)*nrow(StudLocXYData)), ]                     

  # Generate Distances to Treatment
    
    dDegMileConv <- 9.5
    vOnesStud <- as.vector(rep(1, nKids))
    vOnesTrt  <- as.vector(rep(1, nOrg))
    vOnesTrt.Plus1 <- as.vector(rep(1, nOrg+1))
    #L1 distance, L2 is commented out below
    mStudTrtDist <- (abs(StudXY[, "X"] %*% t(vOnesTrt) - vOnesStud %*% t(TrtXY[, "X"]))
                        + abs(StudXY[, "Y"] %*% t(vOnesTrt) - vOnesStud %*% t(TrtXY[, "Y"])))*dDegMileConv
    #mStudTrtDist <- sqrt( (StudXY[, "X"] %*% t(vOnesTrt) - vOnesStud %*% t(TrtXY[, "X"]))^2
     #                 + (StudXY[, "Y"] %*% t(vOnesTrt) - vOnesStud %*% t(TrtXY[, "Y"]))^2 )*dDegMileConv
    colnames(mStudTrtDist) <- "Dist to " %&% sOrgNames                

  # Generate Student-to-Treatment Assignments
                        
    dfMyDataLoc <- data.frame(dfMyData, mStudTrtDist)
    mValErr     <- matrix(rlogis(nKids*nOrg), ncol = nOrg)
    mTrtValue   <- cbind(0,
                    -3.0 + (-0.1)*mStudTrtDist + (-0.01)*mStudTrtDist^2 + (-0.01)*Pretest + ifelse(fRace=="Sour", 0.5, 0.0) +
      ifelse(fRace=="Salty", 0.25, 0.0) + mValErr)
    cTrt        <- max.col(mTrtValue)
    x <- rep(seq(1:(1+nOrg)), nKids)
    
    mTrtInd     <- ( (cTrt%*%t(vOnesTrt.Plus1)) == (vOnesStud %*% t(seq(1:(1+nOrg)))) )*1
    table(cTrt)

    dfMyDataTrt <- data.frame(dfMyData, mTrtInd)
    names(dfMyDataTrt) <- c(names(dfMyData), "No Treat", sOrgNames)
    detach(dfMyData)
    attach(dfMyDataTrt)


#-----------------------------#
#-----------------------------#
# CONSTRUCT OUTCOME VARIABLES #
#-----------------------------#
#-----------------------------#

  # Draw errors to get a random component
    e <- data.frame(mvrnorm(n = nKids, mu = c(0, 0, 0, 0), Sigma = matrix(c(1.0, 0.3, 0.2, 0.2,
                                                                            0.3, 1.0, 0.2, 0.2,
                                                                            0.2, 0.2, 1.0, 0.2,
                                                                            0.2, 0.2, 0.2, 1.0), nrow = 4)))
    colnames(e) <- c("e_Posttest1", "e_Posttest2", "e_DO", "e_Act")
    attach(e)

  # Generate Dosage Data and Test Score Data
  # The Data Generating Process is the same for both posttests except for increasing mean and increasing error variance

    bTreat <- (cTrt > 1)
    iTrtDose1  <- round((15.0 + (0.07)*(Pretest   - mean(Pretest)) + rnorm(nKids)*3))
    dTrtEff1 <- ((cbind(1, iTrtDose1) %*% t(mOrgParams))*mTrtInd[ , -1] ) %*% vOnesTrt
    Posttest1 <- 50 + 0.9*Pretest + 3.0*bGender + dfMyData$SchEffect + 15*StudFactor + dTrtEff1 + e_Posttest1*30

    iTrtDose2  <- round((15.0 + (0.07)*(Posttest1 - mean(Posttest1)) + rnorm(nKids)*3))
    dTrtEff2 <- ((cbind(1, iTrtDose2) %*% t(mOrgParams))*mTrtInd[ , -1] ) %*% vOnesTrt
    Posttest2 <- 60 + 0.9*Posttest1 + 3.0*bGender + dfMyData$SchEffect + 15*StudFactor + dTrtEff2 + e_Posttest2*40

    summary(dTrtEff1[bTreat==0])
    summary(dTrtEff1[bTreat==1])
  
    Posttest1 <- round(Posttest1)
    Posttest2 <- round(Posttest2)

  # Generate binary outcome, with instrument that can be used for selection. Interpretation is dropping out, shocked by ... pregnancy?

    ystar <- 0.5 + (-0.03)*Pretest + ifelse(fRace == "Sour", 1.0, 0) + e_DO
    DroppedOut <- as.numeric(ystar > 0)
    mean(DroppedOut)

  # Generate continuous outcome observed conditional on binary outcome
    # Interpretation is Act conditional on reaching an age where student would apply to college.

    ActTrue <- 15 + 0.05*Pretest + e_Act
    Act <- ActTrue
    is.na(Act) <- (DroppedOut == 1)
    summary(Act)


#----------------------------------------#
#----------------------------------------#
# INSPECT THE PROPERTIES OF THE DATA SET #
#----------------------------------------#
#----------------------------------------#

  # # # Construct conditional averages and descriptives # # #

    aggregate(x = cbind(Pretest, Posttest1, Posttest2, StudFactor, SchEffect, bTreated), by = list(fRace),    FUN = "mean")
    aggregate(x = cbind(Pretest, Posttest1, Posttest2, StudFactor, SchEffect, bTreated), by = list(cGender),  FUN = "mean")
    aggregate(x = cbind(Pretest, Posttest1, Posttest2, StudFactor, SchEffect, bGender),  by = list(bTreated), FUN = "mean")

    table(bTreated, fRace)

    cor(cbind(Pretest, Posttest1, Posttest2, bTreated))
    var(SchEffect); var(Posttest1); var(Posttest2)


  # # # Draw quantile curve of how pretests and different racial composition varies across schools # # #

    # ....

  # # # Can we recover the true parameters? # # #

    # NSM: I need to return to this section. There are more visualizations to explore here, and I've changed the data structure a decent amount since
      # first writing this section up

    SchInds  <- model.matrix(~AssignedSchNum)
    #MyReg1  <- lm(Posttest1 ~ Pretest + bGender + bTreated + SchInds)
    #summary(MyReg1)
    #MyReg2  <- lm(Posttest2 ~ Posttest1 + bGender + bTreated + SchInds)
    #summary(MyReg2)
  
    # Inspect bias if Bpi is not controlled for (since we may initially withhold this from the Data Campers)
    # When removing Bpi (since it will not be included in our dataset) not sure whether to remove this
    MyReg    <- lm(Posttest1 ~ Pretest + bGender + bTreated + SchInds)
    summary(MyReg)


  #-------------------#
  # Save and Clean Up #
  #-------------------#

    dfFinalData <- data.frame(StudId, cGender, fRace, bTreat, AssignedSchNum, SchType, Pretest, Posttest1, Posttest2)
    #write.csv
    save(dfFinalData,file="~/after-hours/Stats-Code-and-Sim-Data/Simulated-Data/dfFinalData.Rda")
    save(dfMyData,file="~/after-hours/Stats-Code-and-Sim-Data/Simulated-Data/dfMyData.Rda")
    detach(e)
    detach(dfMyDataTrt)

