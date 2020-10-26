library(dplyr)
library(ggplot2) 
library(gcookbook)
library(readr)

setwd("C:/Users/victo/OneDrive/Desktop/MSDA/Data Viz Class/PJ2")

Alcohol <- read.csv("StudentAlcConsumption_Por.csv")

Alcohol_1 <- mutate(Alcohol, workdayAlc = recode(AlcComsumptionWorkday , "Very Low" = "1", "Low"="2" , "Medium" = "3",
                                                         "High" = "4", "Very High" = "5"))
Alcohol_2 <- mutate(Alcohol_1, weekendAlc = recode(AlcComsumptionWeekend , "Very Low" = "1", "Low"="2" , "Medium" = "3",
                                                 "High" = "4", "Very High" = "5"))

View(Alcohol_2)

#Q1) Correlation between alchohol consumption (Workday) and school absences?

#Absences and alcohol consumption on work day

ggplot(Alcohol_2, aes(y=Absences, x =factor(workdayAlc), fill= factor(workdayAlc))) +
  geom_violin() +
  xlab("Alcohol Consumption on Workday") +
  ylab("Absences")+
  scale_fill_discrete(name = "Levels", labels = c("1 -Very Low", "2 -Low","3 -Medium","4 -High","5 -Very High", "C"))+
  ggtitle("Absences and Alcohol Consumption on Workday")

#Q2) Correlation between alchohol consumption (Weekend) and school absences?

#Absences and alcohol consumption on week day

ggplot(Alcohol_2, aes(y=Absences, x =factor(weekendAlc), fill= factor(weekendAlc))) +
  geom_violin() +
  xlab("Alcohol Consumption on Weekend") +
  ylab("Absences")+
  scale_fill_discrete(name = "Levels", labels = c("1 -Very Low", "2 -Low","3 -Medium","4 -High","5 -Very High", "C"))+
  ggtitle("Absences and Alcohol Consumption on Weekend")

# Density Plot for Absences during Workdays
plot(density(Alcohol_2$Absences[Alcohol_2$workdayAlc=="1"]),col="blue",main = "Density of Absences during Workdays", xlab = "Absences", ylab = "Alc Consumption on Workdays") 
lines(density(Alcohol_2$Absences[Alcohol_2$workdayAlc=="2"]),col="red")
lines(density(Alcohol_2$Absences[Alcohol_2$workdayAlc=="3"]),col="purple")
lines(density(Alcohol_2$Absences[Alcohol_2$workdayAlc=="4"]),col="Yellow")
lines(density(Alcohol_2$Absences[Alcohol_2$workdayAlc=="5"]),col="Green")
legend("topright", col=c("blue","red","purple","Yellow","Green"), c("Very Low", "Low","Medium","High","Very High"),pch = c(1,1))

# Density Plot for Absences during Weekends
plot(density(Alcohol_2$Absences[Alcohol_2$weekendAlc=="1"]),col="blue",main = "Density of Absences during Weekends", xlab = "Absences", ylab = "Alc Consumption on Weekends") 
lines(density(Alcohol_2$Absences[Alcohol_2$weekendAlc=="2"]),col="red")
lines(density(Alcohol_2$Absences[Alcohol_2$weekendAlc=="3"]),col="purple")
lines(density(Alcohol_2$Absences[Alcohol_2$weekendAlc=="4"]),col="Yellow")
lines(density(Alcohol_2$Absences[Alcohol_2$weekendAlc=="5"]),col="Green")
legend("topright", col=c("blue","red","purple","Yellow","Green"), c("Very Low", "Low","Medium","High","Very High"),pch = c(1,1))



  