#########################################################################################
#                                                                                       #
# Develop Simulation Dataset for YMCA Analysis and CIM Data Camp                        #
#                                                                                       #
# Author: Nicholas Mader <nmader@chapinhall.org>                                        #
#                                                                                       #
#########################################################################################



#########################################################################################
# SPECIFY DATA GENERATING PROCESS 
#########################################################################################

  # Data is ~loosely~ reflective of a 9th grade class in CPS
  
    MyDir <- "C:/Users/nmader/Google Drive/Chapin Hall - YMCA Project Collaboration/Data and Analysis Simulations/"
    setwd(MyDir)

    set.seed(60637) # The seed value could be any number. I'm just picking an auspicious one.
    library("MASS")
    library("plyr")
    library("ggplot2")
    
    nKids     <- 24268 # To make the sample equivalent to a year of HS enrollment in CPS
    nSchools  <- 106
    sTrtName <- "After School Programming"
  
  # Draw student characteristics
  
    sDataFeatures <- c("PretestDraw", "BpiDraw", "GenderDraw", "RaceDraw", "SchDraw", "TreatmentDraw")
    nDataFeatures <- length(sDataFeatures) 
  
  # Generate a variance-Covariance matrix for all data features

    mVCV <- matrix(nrow = nDataFeatures, ncol = nDataFeatures, dimnames = list(sDataFeatures, sDataFeatures))
    for (i in 1:nDataFeatures) {
      for (j in 1:i) {
        if (i == j) mVCV[i, j] <- 1 else {
          rho <- max(min(rnorm(1, sd = .3),1), -1) # This will result in correlations generally close to 0, and truncated within [0,1]
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
    #mVCV


#########################################################################################
# DRAW SAMPLE AND GENERATE STUDENT CHARACTERISTICS
#########################################################################################

#### Draw a sample of kids from the above

  vMu = as.vector(seq(0,0,length.out = nDataFeatures))
  KidData <- mvrnorm(n = nKids, mu = vMu, Sigma = mVCV)
  colnames(KidData) <- sDataFeatures
  StudId <- 1:nKids
  dfKidData <- data.frame(StudId, KidData)
  attach(dfKidData)


#### Set values and scales for each variable

  # Create Gender
    bFemale <- as.numeric(pnorm(GenderDraw) <= 0.51) # Without the "as.numeric()" function, bGender would be a series of "true/false" values. Another way to accomplish the "true/false" to "1/0" conversion is to multiply by 1.
    cGender <- ifelse(1==bFemale, "Treble", "Bass")        # Convert the gender distinction into an abstract one
  
  # Create Race
    cRaceDraw <- cut(pnorm(RaceDraw), breaks = c(0.0, 0.4, 0.5, 0.6, 1.0), include.lowest = TRUE)
    cRace <- factor(cRaceDraw, labels = c("Sour", "Salty", "Bitter", "Sweet"))

  # Rescale Pretest to Mean 100, and broader standard deviation
    Pretest <- round(PretestDraw*20 + 100)
    #hist(PretestScaled)

  # Rescale Bpi
    Bpi <- round(exp(BpiDraw/1.5 + 2)/3)  # NSM: this is messing with things to get something relatively flat, with an interesting tail
    #hist(Bpi, breaks = 0:ceiling(max(Bpi)))
    table(Bpi)

  # Assign to treatment
    bTreated <- as.numeric(pnorm(TreatmentDraw)<=.22)


#########################################################################################
# GENERATE SCHOOL CHARACTERISTICS 
#########################################################################################


  # Construct names for schools (comes from http://www.fanfiction.net/s/7739576/1/106-Stories-of-Us)

    sSchNamesData <- read.csv2(file = paste(MyDir, "Raw Data/Random Words for School Names.csv", sep=""), sep = ",", header = TRUE)
    attach(sSchNamesData)
    SchTypeDraw <- cut(runif(nSchools), breaks = c(0.0, 0.3, 0.5, 0.7, 0.8, 0.9, 1.0), include.lowest = TRUE)
    sSchType <- as.character(factor(SchTypeDraw, labels = c("Academy", "School", "High School", "Preparatory", "Charter", "International")))
    sSchName <- paste(SchNamesList, sSchType)

  # Combine information
    cSchYinYang <- factor(runif(nSchools)<.2, labels = c("Yin", "Yang"))
    dfSchData <- data.frame(1:nSchools, sSchName, 1*(runif(nSchools)*5 + ifelse(cSchYinYang=="Yin",0,1)), as.character(cSchYinYang))
    colnames(dfSchData) <- c("SchNum", "SchName", "SchEffect", "SchType")


#########################################################################################
# MERGE SCHOOL AND KID DATA, AND CONSTRUCT OUTCOMES
#########################################################################################

  # Assign Kids to Schools
    AssignedSchNum <- as.vector(cut(SchDraw, nSchools, labels=FALSE)) # NSM: This was changed from cut(pnorm(SchDraw)...) as we don't necessarily want even school enrollments
    sort(table(AssignedSchNum))
  # Shuffle assignment, since I'm accidentally 
    AssignedSchNum <- AssignedSchNum[rank(runif(nKids))]

  # Merge Data

    dfKidData_Aug <- data.frame(dfKidData, bFemale, cGender, cRace, Pretest, Bpi, AssignedSchNum)
    dfMyData <- merge(x = dfKidData_Aug, y = dfSchData, by.x = "AssignedSchNum", by.y = "SchNum")
    detach()
    attach(dfMyData)
    
                            
  # Generate the (continuously-valued) outcome
    
    e <- data.frame(mvrnorm(n = nKids, mu = c(0, 0, 0), Sigma = matrix(c(1.0, 0.3, 0.2,
                                                                      0.3, 1.0, 0.2,
                                                                      0.2, 0.2, 1.0), nrow = 3)))
    colnames(e) <- c("e1", "e2", "e3")
    attach(e)

    Posttest <- 50 + 0.90*Pretest + (-2.5)*Bpi + 3.0*bFemale + SchEffect + 20*bTreated + 3.0*(bTreated*Bpi)  + e2*30 #   + (-0.0008)*Pretest^2
    Posttest <- round(Posttest)
    summary(Posttest)
    hist(Posttest)

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

    

  
#########################################################################################
# INSPECT THE PROPERTIES OF THE DATA SET
#########################################################################################


  # # # Construct conditional averages and descriptives # # #

    aggregate(x = cbind(Posttest, Pretest, Bpi, SchEffect, bTreated), by = list(cRace), FUN = "mean")
    aggregate(x = cbind(Posttest, Pretest, Bpi, SchEffect, bTreated), by = list(cGender), FUN = "mean")
    cor(cbind(Posttest, Pretest, Bpi, bTreated))
    var(SchEffect)
    var(Posttest)

    # Draw quantile curve of how pretests, Bpi, and different racial composition varies across schools


  # # # Can we recover the true parameters? # # #

    Pretest2 <- Pretest^2
    dTreated.Bpi <- bTreated * Bpi
    SchInds  <- model.matrix(~AssignedSchNum)
    MyReg    <- lm(Posttest ~ Pretest + Bpi + bFemale + bTreated + SchInds)
    summary(MyReg)
  
    # Inspect bias if Bpi is not controlled for (since we may initially withhold this from the Data Campers)
    MyReg    <- lm(Posttest ~ Pretest + bFemale + bTreated + SchInds)
    summary(MyReg)


#########################################################################################
## BREAK APART AND DIRTY THE DATA FOR THE CIM DATA CAMP
#########################################################################################

  #############################
  # Generate separate data sets
  #############################

    dfStudData   <- data.frame(StudId, cRace, cGender, Pretest, Posttest)
    AssignedSchName <- dfMyData$SchName
    dfStudEnroll <- data.frame(StudId, AssignedSchName)  # NSM: Having to make this assignment was a little awkward, as a result of poor handling of which data sets are attached
    dfSchData    <- dfSchData[c("SchNum", "SchName", "SchType")]
    dfStudTreat  <- data.frame(StudId, bTreated)
    dfStudBpi    <- data.frame(StudId, Bpi)
  
  
  ############
  # Add dirt
  ############
    
    # Different variable types for the same variable. In one, variable is string, the other is character.
  
    dfStudData.Dirty <- dfStudData
      dfStudData.Dirty$Pretest[ runif(nKids)<0.05] <- 999
      dfStudData.Dirty$Pretest[ runif(nKids)<0.02] <- NA
      dfStudData.Dirty$Posttest[runif(nKids)<0.05] <- 999
      colnames(dfStudData.Dirty) <- c("Student Id", "Student Race", "Student Gender", "Pretest Score", "Posttest Score")

    dfStudEnroll.Dirty <- dfStudEnroll
      attach(dfStudEnroll.Dirty)
      dfStudEnroll.Dirty <- dfStudEnroll.Dirty[runif(nKids)<0.94,]   # Delete several records
        n <- nrow(dfStudEnroll.Dirty)

      dfStudEnroll.Dirty$AssignedSchName <- as.character(dfStudEnroll.Dirty$AssignedSchName)

      # NSM: this is pretty awkward. There's got to be a better way to do conditional transformation on a subset of rows
      IsAcad   <- grep("Acad", dfStudEnroll.Dirty$AssignedSchName, fixed = TRUE)
      IsCenter <- grep("Center", dfStudEnroll.Dirty$AssignedSchName, fixed = TRUE)
      IsInt    <- grep("International", dfStudEnroll.Dirty$AssignedSchName, fixed = TRUE)
      IsPrep   <- grep("Preparatory", dfStudEnroll.Dirty$AssignedSchName, fixed = TRUE)
      TheDraw  <- runif(n) < 0.4
      
      dfStudEnroll.Dirty$AssignedSchName <-
        ifelse(runif(n)<0.10, tolower(dfStudEnroll.Dirty$AssignedSchName), dfStudEnroll.Dirty$AssignedSchName)
      dfStudEnroll.Dirty$AssignedSchName[IsAcad] <-
        ifelse(runif(length(IsAcad))<0.10,  sub("Academy", "Acad", dfStudEnroll.Dirty$AssignedSchName[IsAcad]), dfStudEnroll.Dirty$AssignedSchName[IsAcad])
      dfStudEnroll.Dirty$AssignedSchName[IsCenter] <-
        ifelse(runif(length(IsAcad))<0.10,  sub("Center", "Ctr.", dfStudEnroll.Dirty$AssignedSchName[IsCenter]), dfStudEnroll.Dirty$AssignedSchName[IsCenter])
      dfStudEnroll.Dirty$AssignedSchName[IsInt] <-
        ifelse(runif(length(IsInt))<0.10,  sub("International", "Int", dfStudEnroll.Dirty$AssignedSchName[IsInt]), dfStudEnroll.Dirty$AssignedSchName[IsInt])
      dfStudEnroll.Dirty$AssignedSchName[TheDraw] <- paste("The", dfStudEnroll.Dirty$AssignedSchName[TheDraw], sep = " ")

      dfStudEnroll.Dirty$AssignedSchName[runif(n)<0.03] <- "XX"
      
      LowerCaseDraw <- runif(n) < 0.2
      dfStudEnroll.Dirty$AssignedSchName[LowerCaseDraw] <- tolower(dfStudEnroll.Dirty$AssignedSchName[LowerCaseDraw])


    dfSchData.Dirty <- dfSchData
      dfSchData.Dirty$SchNum <- as.character(dfSchData.Dirty$SchNum)
      colnames(dfSchData.Dirty) <- c("School Number", "School Name")

    dfStudBpi.Dirty   <- dfStudBpi
      dfStudBpi.Dirty$Bpi <- as.character(dfStudBpi$Bpi)
      dfStudBpi.Dirty$Bpi[runif(nKids)<0.2] <- "NA"
      dfStudBpi.Dirty$Bpi[runif(nKids)<0.2] <- ""
      dfStudBpi.Dirty$StudId <- as.character(dfStudBpi.Dirty$StudId)

    dfStudTreat.Dirty  <- dfStudTreat
      dfStudTreat.Dirty$bTreated[runif(nKids)<.05]  <- (-1)
      colnames(dfStudTreat.Dirty) <- c("STUDID", sTrtName)


  ###################
  # Export data sets
  ###################

    write.table(dfStudData.Dirty,   paste(MyDir, "Prepped Data/Master Student Data System 2012-10-05.csv", sep=""), sep=",",  row.names = FALSE)
    write.table(dfStudTreat.Dirty,  paste(MyDir, "Prepped Data/After School Student Assignments.csv",      sep=""), sep=",",  row.names = FALSE)
    write.table(dfStudBpi.Dirty,    paste(MyDir, "Prepped Data/SC410 Extr 11800234928cnvkP.csv",           sep=""), sep=",",  row.names = FALSE)
    write.table(dfStudEnroll.Dirty, paste(MyDir, "Prepped Data/Enrollment System StSch Oct 12.txt",        sep=""), sep="\t", row.names = FALSE)
    write.table(dfSchData.Dirty,    paste(MyDir, "Prepped Data/School Directory System 2011-2012",         sep=""), sep=",",  row.names = FALSE)


##########################
# Leftovers for Later
##########################

  
  # separate student pretest and posttest characteristics
  # double the sample and reverse only the signs of the errors to ensure that regressions give exact parameters back (in linear regression)
  # continuous treatment effect
  # multiple treatments
  # endogeneity of the treatment, which would come with instruments
  # longitudinal data with non-iid error within individuals
  # multiple outcomes variables that have correlated unobservable determinants
  # count outcome
  # floor effects for tests (for application of tobit)



