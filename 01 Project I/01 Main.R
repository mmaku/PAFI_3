rm(list = ls()) 

Timer <- proc.time()

# install.packages("ggplot2")
# install.packages("plotly")
# install.packages("RSEIS")

library(ggplot2)
library(plotly)
library(RSEIS)

# Working directory
MyPath         <- "C:/Users/mchl/Dropbox/01 UWr/01 Szkoła - projekty/11 WAiF III/01 Projekt I/" 
MyData         <- paste(MyPath,"01 Data/", sep="") 
MyDeliverables <- paste(MyPath,"02 Deliverables/", sep="") 

setwd(MyPath)

# Load source files
source("02 Functions.R")
source("03 Model_Parametrization.R")

