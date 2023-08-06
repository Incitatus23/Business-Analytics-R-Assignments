#load libraries
library(tidyverse)
library(cluster)
library(fpc)

setwd("C:/Users/adaml/Downloads/")

employdf<-read.csv("Employees.csv")
View(employdf)

quantdf<- employdf[c(1,16, 20, 25)]
view(quantdf)
quantdfn<-scale(quantdf)
View(quantdfn)
set.seed(42)
wss<-function(k){kmeans(quantdfn, k, nstart=10)} $tot.withinss
k_values<- 1:10
wss_values<-map_dbl(k_values, wss)
elbowdf<- data.frame(k_values, wss_values)
ggplot(elbowdf, mapping = aes(x = k_values, y = wss_values)) +
  geom_line() + geom_point()

k4<-kmeans(quantdfn, 4, nstart=1000)
str(k4)
k4
cluster.stats(dist(quantdfn, method="euclidean"), k4$cluster)

#combining each observation's cluster assignment with unscaled data frame
quantdfk4<-cbind(quantdf, clusterID=k4$cluster)
View(quantdfk4)

#write data frame to CSV file to analyze in Excel
write.csv(quantdfk4, "magazine_kmeans_4clusters.csv")

#calculate variable averages for all non-normalized observations
summarize_all(quantdf, mean)

#Calculate variable averages for each cluster
quantdfk4 %>%
  group_by(clusterID) %>%
  summarize_all(mean)