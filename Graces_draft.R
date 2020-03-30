#Program: Drafting for project 1
#By: Grace Rogers
#############################################################
library(ggplot2)
library(readxl)
library(dplyr)
library(stats)
library(skimr)
library(car)
library(standardize)
Housing <- read_excel("Housing.xlsx")
Housing$elemfactor <- as.numeric(as.factor(elem))
Housing$statusfactor <- as.numeric(as.factor(status))
Housing$edison <- ifelse(elem == 'edison', 1, 0)
Housing$parker <- ifelse(elem == 'parker', 1, 0)
#size: pr = .081 R^2 = .04058
#Lot: pr = .0335 R^2 = .05965
#bath:pr = .131 R^2 = .03051
#bedrooms: pr = .0122 R^2 = .08191 #
#yearbuilt: pr = .184 R^2 = .02375
#garagesize: pr = .00148 R^2 = 0.1284 #
#status: pr = .0361 R^2 = .07674 #
#elem: pr = edison:.0192, parker: .0308, R^2= .2219 #
#statusfactor: pr = 0.0286, R^2 = 0.06312 
#elemfactor: pr = .589, R^2 = .0039
#edison: pr = .0088, R^2 = .089
#parker: pr = .0414 R^2 = .05501

scaled_data <- data.frame(scale(Housing[,2:9]), scale(Housing[,12:15]))
Housing_model1_everything<-lm(price~ bedrooms+garagesize+elem+status,data=Housing)
summary(Housing_model1_everything)
Housing_model1_everything<-lm(price~ bedrooms+garagesize+elem+status,data=scaled_data)
summary(Housing_model1_everything)
vif(Housing_model1_everything)
plot(Housing_model1_everything)











