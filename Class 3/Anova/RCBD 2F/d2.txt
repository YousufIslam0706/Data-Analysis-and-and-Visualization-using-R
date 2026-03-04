## ANOVA ##

library (doebioresearch)

#Read Table #
data<-read.csv(file.choose())

head(data)

#it allows only 6 column at a time

d1<- rcbd(data[3:8],data$Gen,data$Rep,1) # 0 for no test, 1 for LSD, 2 for DMRT
d2<- rcbd(data[9:11],data$Genotype,data$Replication,1)
d3<- rcbd(data[15:18],data$Gen,data$Rep,1)

d1

d2

d3
