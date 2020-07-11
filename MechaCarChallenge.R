MechaCar_mpg <- read.csv('MechaCar_mpg.csv',stringsAsFactors = F) #read in MechaCar_mpg dataset)
head(MechaCar_mpg)
names(MechaCar_mpg) <- c("len","wt","ang","cl","AWD","mpg")
Mecha_matrix <- as.matrix(MechaCar_mpg[,c("len","wt","ang","cl","mpg")]) #convert data frame into numeric matrix
cor(Mecha_matrix)
lm(mpg ~ len + wt + ang + cl, data=MechaCar_mpg)#generate multiple linear regression model
summary(lm(mpg ~ len + wt + ang + cl, data=MechaCar_mpg))#generate summary statistics
Coil <- read.csv("Suspension_Coil.csv",stringsAsFactors = F) #read in Suspension_Coil dataset)
library(dplyr)
summarise(Coil,Min.=min(Coil$PSI),Max.=max(Coil$PSI),Mean=mean(Coil$PSI),Median=median(Coil$PSI),Variance=var(Coil$PSI),SD=sd(Coil$PSI,))#create the summary statistics table
t.test(Coil$PSI,mu=1500)#perform t.test with the population mean of 1500.

