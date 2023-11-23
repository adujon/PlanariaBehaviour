rm(list=ls()) #remove all files in memory

#load packages
library(readxl)
library(brms)
library(tidybayes)
library(coda)
library(parallel)

#change working directory
setwd("G:/My Drive/Postdoc/Planarian/Manuscript PROCB")

#import the dataset with activity data
data <- read.csv("planaria_raw_dataset.csv")

#####prepare the data form modeling
#creation of a dummy variable and convert the categorical variables as factors
#an upper C at the end of a variable indicates the variable has been 
#modified before fitting the model (i.e. centered or dummy variable)
data$Phase <- as.factor(data$Phase)
data$Treatment <- as.factor(data$Treatment)
data$Batch <- paste("B", data$Batch, sep = "") #create a categorical variable coding for batch number
data$Treatment <- ifelse(data$Treatment == "Treatment", "Cadmium", "Control")
data$TreatmentC <- paste(data$Treatment, data$Phase)

#center the predictive variables variables to help with convergence
mdist <- mean(data$Distance) #compute the average activity
data$DistanceC <- data$Distance - mdist #center the activity measurements
mlength <- mean(as.numeric(data$Length_mm)) #compute the average planaria length
data$Length_mmC <- as.numeric(data$Length_mm) - mlength #center the length measurements
mtemp <- mean(data$Temperature) #compute average temperature
data$TemperatureC <- data$Temperature - mtemp #center the temperature measurements

#group planarias based on their treatment and phase
#this variable s used to compute the unstructured correlation matrix of the model
#useful to get the correlations between intercept, slopes, and rIIV.
data$Group <- paste(data$Treatment, data$Phase)
data$Group <- factor(data$Group)

#rename the planarias to help with fitting the model
data$ID2 <- paste(data$ID, data$Group)

#Standardize the days between the two phases of the experiment
data$DayP <- ifelse(data$Day > 7, data$Day - 7, data$Day) #DayP = DayPhase
data$DayPC <- data$DayP - mean(data$DayP) #center the day variable

###################################################################
########### Fit the model #########################################
###################################################################

#model formula with unstructured correlation matrix
mod <- bf(DistanceC ~ 0+DayPC+TreatmentC+DayP+TreatmentC*DayP+Length_mmC+Batch+TemperatureC+(DayP |j| gr(ID2,by = Group)),
          sigma ~ 0+DayPC+TreatmentC+DayP+TreatmentC*DayPC+Length_mmC+Batch+TemperatureC+(1|j|gr(ID2,by = Group)))

#detect the number of cores available on the computer
my.cores <- detectCores()

#fit the model using th brms package
m1_brm <- brm(mod,
              data = data,
              warmup = 500,iter = 5000, thin=2,
              chains = 4, init = "random",
              seed = 12345,
              cores = my.cores,
              verbose = 1,
              control = list(adapt_delta = 0.97, max_treedepth = 12))

#export the model as a RDS file
saveRDS(m1_brm, file = "model_planaria.rds")
