####################################################################################
#                                                                                  #
# Authors: Nicholas Mader <nmader@chapinhall.org>                                  #
#         Breanna Miller <breanna.a.miller@gmail.com>                              #
#         Jette Henderson <jette.henderson@gmail.com>                              #
#         Vidhur Vohra <vvohra@gatech.edu>                                         #
#                                                                                  #
# Program Description: This code runs an analysis of enrollment patterns of        #
#      youth in afterschool and community school programs to study which factors   #
#      predict successful enrollment/outreach                                      #
#                                                                                  #
####################################################################################

#-----------------------------
### Set Up Working Environment
#-----------------------------

  rm(list=ls())
  setwd("C:/Users/nmader/Documents/GitHub/dssg-after-hours/after-hours/Stats-Code-and-Sim-Data")
  try(setwd("~/after-hours/Stats-Code-and-Sim-Data/"))
  "%&%" <- function(...){paste(..., sep = "")}
  library("survival")
  library("mclogit")

#------------------------
### Set Up Run Parameters
#------------------------

  SetupData   <- 0
  RunAnalysis <- 1


#----------------------------------------------
### Load and prepare data needed for estimation
#----------------------------------------------
  # The structure required for estimation requires a data set defined by each student/alternative combination
  # By design, the salient predictors of enrollment are: distance, distance squared, race, and pre-test score. 

if (1 == SetupData) {

  # Load all required files
    load("./Simulated-Data/dfFinalSimulatedData.Rda")
    str(dfMyDataTrt)
    dfMyDataTrt$ChoiceName <- dfMyDataTrt[ , c("Treatment Center Name")]
    
    load("./Simulated-Data/Student to Treatment Distances.Rda")
    str(mStudTrtDist)
  
    load("./Simulated-Data/Treatment Center Data.Rda")
    str(dfTrtData)
    dfTrtData$AltName <- rownames(dfTrtData)
  
  # Generate the skeleton framework for estimation
  
    dfChoiceSkel <- merge(dfMyDataTrt$StudId, unique(dfMyDataTrt$ChoiceName))
      colnames(dfChoiceSkel) <- c("StudId", "AltName")
    dfChoiceSkel <- merge(dfChoiceSkel, dfMyDataTrt[,c("StudId", "ChoiceName")], by = c("StudId"))
    dfChoiceSkel$ChosenAlt <- 1*(dfChoiceSkel$AltName == dfChoiceSkel$ChoiceName)
    dfChoiceSkel$NoTrtInd  <- 1*(dfChoiceSkel$AltName == "No Treatment")
    dfChoiceSkel$TrtInd    <- 1 - dfChoiceSkel$NoTrtInd

  # Merge in predictors
  
    # Youth characteristics, including residential location
    youthKeepVars <- c("StudId", "fRace", "fFrl", "cGender", "Pretest", "StudX", "StudY")
    dfChoice <- merge(dfChoiceSkel, dfMyDataTrt[, youthKeepVars], by=c("StudId"), all = TRUE)
  
    # Location of program
    dfChoice <- merge(dfChoice, dfTrtData[ , c("AltName", "TrtX", "TrtY")], by = c("AltName"), all = TRUE)
  
  # Generate constructions of predictors
  
    dfChoiceEst <- dfChoice
  
    # Interaction of youth characteristics with program characteristics
    dfChoiceEst$bFrl      <- 1*(dfChoiceEst$fFrl == "Yes")
    dfChoiceEst$bBass     <- 1*(dfChoiceEst$cGender == "Bass")
    
    dfChoiceEst$Race.Sweet  <- 1*(dfChoiceEst$fRace == "Sweet")
    dfChoiceEst$Race.Sour   <- 1*(dfChoiceEst$fRace == "Sour")
    dfChoiceEst$Race.Bitter <- 1*(dfChoiceEst$fRace == "Bitter")
    dfChoiceEst$Race.Salty  <- 1*(dfChoiceEst$fRace == "Salty")
    #dfChoiceEst$RaceCtrls <- model.matrix(~ -1 + dfChoiceEst$fRace)
    
    #for (v in c("bFrl", "bBass", "RaceCtrls")) {
    #  print( "dfChoiceEst$" %&% v %&% "[dfChoiceEst$NoTrtInd == 1]")
    #  assign("dfChoiceEst$" %&% v %&% "[dfChoiceEst$NoTrtInd == 1]", 0)
    #}
    dfChoiceEst$bFrl[dfChoiceEst$NoTrtInd==1] <- 0
    dfChoiceEst$bBass[dfChoiceEst$NoTrtInd==1] <- 0
    
    dfChoiceEst$Race.Sweet[dfChoiceEst$NoTrtInd==1] <- 0
    dfChoiceEst$Race.Sour[dfChoiceEst$NoTrtInd==1] <- 0
    dfChoiceEst$Race.Bitter[dfChoiceEst$NoTrtInd==1] <- 0
    dfChoiceEst$Race.Salty[dfChoiceEst$NoTrtInd==1] <- 0
  
    dfChoiceEst$Pretest[dfChoiceEst$NoTrtInd==1] <- 0 
    
    
    # Distance between youth and alternative
    dDegMileConv <- 69
    dfChoiceEst$DistMi <- (abs(dfChoiceEst$StudX - dfChoiceEst$TrtX) + abs(dfChoiceEst$StudY - dfChoiceEst$TrtY))*dDegMileConv
      dfChoiceEst$DistMi[dfChoiceEst$NoTrtInd==1] <- 0 
    dfChoiceEst$DistMiSq <- dfChoiceEst$DistMi^2
    
    attach(dfChoiceEst)
    OutreachData <- data.frame(ChosenAlt, AltName, StudId, DistMi, DistMiSq, bFrl, bBass, RaceCtrls, Pretest)
    save(OutreachData, file="./Simulated-Data/OutreachData.Rda")
    detach(dfChoiceEst)
}

#------------------------------------
### Run analysis of youth enrollments
#------------------------------------

# The true parameters in estimation are
#   * -3.0   "intercept" for all "no treatment" options
#   * -0.1   coefficient on distance between student & alternative
#   * -0.01  coefficient on squared distance between student & alternative
#   * -0.02  coefficient on pretest score (representing the fact that treatment centers are more likely to attract at-risk (judged by lower-performing) youth)
#   *  0.5   intercept shift for "Sour"  youth to participate in any type of programming
#   *  0.25  intercept shift for "Salty" youth to participate in any type of programming

if (1 == RunAnalysis) {

  rm(list=ls())
  load("./Simulated-Data/OutreachData.Rda")
  attach(OutreachData)
  #clogit(ChosenAlt ~ bFrl + bBass + RaceCtrls + Pretest + DistMi + DistMiSq, OutreachData)
  
  Est <- mclogit(cbind(ChosenAlt, AltName) ~ TrtInd + Race.Sour + Race.Salty + Race.Bitter + Pretest + DistMi + DistMiSq)
  summary(Est)
}
