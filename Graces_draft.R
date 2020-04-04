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
Housing$elemfactor <- as.numeric(as.factor(Housing$elem))
Housing$statusfactor <- as.numeric(as.factor(Housing$status))
Housing$edison <- ifelse(Housing$elem == 'edison', 1, 0)
Housing$parker <- ifelse(Housing$elem == 'parker', 1, 0)
#size: pr = .081 R^2 = .04058
#Lot: pr = .0335 R^2 = .05965
#bath:pr = .131 R^2 = .03051
#bedrooms: pr = .0122 R^2 = .08191 #
#yearbuilt: pr = .184 R^2 = .02375
#garagesize: pr = .00148 R^2 = 0.1284 #
#status: pr = .0361 R^2 = .07674, .051 #
#elem: pr = edison:.0192, parker: .0308, R^2= .2219 #
#statusfactor: pr = 0.0286, R^2 = 0.06312 
#elemfactor: pr = .589, R^2 = .0039
#edison: pr = .0088, R^2 = .089
#parker: pr = .0414 R^2 = .05501

scaled_data <- data.frame(scale(Housing[,2:9]), scale(Housing[,12:15]))
#R^2 : .4116
Housing_model1_everything<-lm(price~ bedrooms+garagesize+elem+status,data=Housing)
summary(Housing_model1_everything)

#R^2 : 0.5209
Housing_model1_everything<-lm(price~ bedrooms+garagesize+elem:status,data=Housing)
summary(Housing_model1_everything)

#R^2 : 0.6127, adjR^2:.4813
Housing_model1_everything<-lm(price ~ poly(lot,2)+ bedrooms + garagesize+ elem:status,data=Housing)
summary(Housing_model1_everything)
vif(Housing_model1_everything)

#R^2 : 0.6124, adjR^2=.49
Housing_model1_everything<-lm(log(price) ~ poly(lot,2)+ garagesize + elem:status, data=Housing)
summary(Housing_model1_everything)
plot(Housing_model1_everything)

Housing$loggarage <- ifelse(Housing$garagesize == 0, 0, log(Housing$garagesize))

Housing_model1_everything<-lm(price ~ poly(lot,2)+ garagesize + elem:status, data=Housing_no74)
summary(Housing_model1_everything)
plot(Housing_model1_everything)

Housing_no74 <- Housing[-c(74,4,20,50), ]

cooksd <- cooks.distance(Housing_model1_everything)
sample_size <- nrow(Housing_no74)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4/sample_size, names(cooksd),""), col="red") 

# Removing Outliers
# influential row numbers
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])

# Alternatively, you can try to remove the top x outliers to have a look
top_x_outlier <- 2
influential <- as.numeric(names(sort(cooksd, decreasing = TRUE)[1:top_x_outlier]))

Housing_2 <- Housing[-influential,]

Housing_model1_everything<-lm(price~ bedrooms:garagesize+elem:status,data=Housing)
summary(Housing_model1_everything)

Housing_model1_everything<-lm(price~ bedrooms+elem:garagesize +status,data=Housing)
summary(Housing_model1_everything)

vif(Housing_model1_everything)
plot(Housing_model1_everything)











