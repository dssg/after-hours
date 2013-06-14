# Develop Mockup Dataset for YMCA project

#################################
# SPECIFY DATA GENERATING PROCESS 
#################################

  # Data is ~loosely~ reflective of a 9th grade class in CPS
  
    set.seed(60637) # The seed value could be any number. I'm just picking an auspicious one.
    library("MASS", lib.loc = "C:/Program Files/R/R-2.15.1/library")
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
      print(mVCV[x1, x2])
      mVCV[x1, x2] <<- mult*abs(mVCV[x1, x2]) # The "<<-" assignment operator ensures that the assignment is applied to mVCV 
                                              # in the global environment (i.e. that it persists after the function is run)
      mVCV[x2, x1] <<- mult*abs(mVCV[x2, x1])
      print(mVCV[x1, x2])
    }
    
    FlipSigns("TreatmentDraw", "PretestDraw", -1)
    FlipSigns("TreatmentDraw", "BpiDraw",     +1)
    FlipSigns("PretestDraw",   "BpiDraw",     -1)
    #mVCV


###################################################
# DRAW SAMPLE AND GENERATE STUDENT CHARACTERISTICS
###################################################

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


##################################
# GENERATE SCHOOL CHARACTERISTICS 
##################################


  # Construct names for schools (comes from http://www.fanfiction.net/s/7739576/1/106-Stories-of-Us)

    sSchNamesData <- read.csv2(file = "C:/Users/nmader/Google Drive/Chapin Hall - YMCA Project Collaboration/Code/Random Words for School Names.csv", sep = ",", header = TRUE)
    attach(sSchNamesData)
    SchTypeDraw <- cut(runif(nSchools), breaks = c(0.0, 0.3, 0.5, 0.7, 0.8, 0.9, 1.0), include.lowest = TRUE)
    sSchType <- as.character(factor(SchTypeDraw, labels = c("Academy", "School", "High School", "Preparatory", "Charter", "International")))
    sSchName <- paste(SchNamesList, sSchType)

  # Combine information
    cSchYinYang <- factor(runif(nSchools)<.2, labels = c("Yin", "Yang"))
    dfSchData <- data.frame(1:nSchools, sSchName, 1*(runif(nSchools)*5 + ifelse(cSchYinYang=="Yin",0,1)), as.character(cSchYinYang))
    colnames(dfSchData) <- c("SchNum", "SchName", "SchEffect", "SchType")

####################################################
# MERGE SCHOOL AND KID DATA, AND CONSTRUCT OUTCOMES
####################################################


  # Assign Kids to Schools
    AssignedSchNum <- cut(pnorm(SchDraw), nSchools, labels=1:nSchools)

  # Merge Data

    dfKidData_Aug <- data.frame(dfKidData, bFemale, cGender, cRace, Pretest, Bpi, AssignedSchNum)
    dfMyData <- merge(x = dfKidData_Aug, y = dfSchData, by.x = "AssignedSchNum", by.y = "SchNum")
    attach(dfMyData)
                            
  # Generate the (continuously-valued) outcome
    
    e <- data.frame(mvrnorm(n = nKids, mu = c(0, 0), Sigma = matrix(c(1.0, 0.3, 0.2
                                                                      0.3, 1.0, 0.2,
                                                                      0.2, 0.2, 1.0), nrow = 3)))
    colnames(e) <- c("e1", "e2", "e3")
    attach(e)

    Posttest <- 50 + 0.90*Pretest + (-0.1)*Bpi + 3.0*bFemale + SchEffect + 10*bTreated + 0.1*(bTreated*Bpi)  + e2*30 #   + (-0.0008)*Pretest^2
    summary(Posttest)
    hist(Posttest)

  # Generate binary outcome, with instrument that can be used for selection

    

  # Generate continuous outcome observed conditional on binary outcome

    Act <- 15 + 0.05*Pretest + (-0.5)*Bpi + e3
    is.na(Act) <- (DropOut == 1)

    

  
#########################################
# INSPECT THE PROPERTIES OF THE DATA SET
#########################################

  Pretest2 <- Pretest^2
  SchInds  <- model.matrix(~AssignedSchNum)
  MyReg    <- lm(Posttest ~ Pretest + Bpi + bFemale + bTreated + SchInds)
  summary(MyReg)


#################################
## BREAK APART AND DIRTY THE DATA
#################################

# Generate data set of student to school mappings

# Add dirt:
  # Change some schools to "XX"
  # Change some numeric values to "999"
  # Change some numeric values to "-1"
  # Change some numeric values to missing
  # Delete several records needed for matching
  # Different variable types for the same variable. In one, variable is string, the other is character.


##########################
# Leftovers for Other Days
##########################

  # floor effects for tests
  # continuous treatment effect
  # multiple treatments
  # endogeneity of the treatment, which would come with instruments
  # longitudinal data with non-iid error within individuals
  # multiple outcomes variables that have correlated unobservable determinants
  # count outcome



