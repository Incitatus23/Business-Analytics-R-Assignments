setwd("C:/Users/adaml/Downloads/")

employdf<-read.csv("Employees.csv")
View(employdf)

quantdf<- employdf[c(1,16, 20, 25)]
view(quantdf)
quantdfn<-scale(quantdf)
View(quantdfn)
set.seed(42)

match_dist<-dist(quantdfn, method="euclidean")

#run hierarchical clustering with the hclust function and group average linkage
cl_match_avg<-hclust(match_dist, method="ward.D")

#plot the dendrogram
plot(cl_match_avg)

#Create 4 clusters using the cutree function
cl_match_avg_4<-cutree(cl_match_avg, k=4)

#display vector of cluster assignments for each observation
cl_match_avg_4

#visualize clusters on the dendrogram
rect.hclust(cl_match_avg, k=4, border=2:4)

#link cluster assignments to original categorical data frame
hcl4df<-cbind(quantdfn, clusterID=cl_match_avg_4)


#display number of observations in each cluster
hcl4df %>%
  group_by(clusterID) %>%
  summarize(n())

hcl4df2 <- as.data.frame(hcl4df)

hcl4df2 %>%
  group_by(clusterID) %>%
  summarize(n())


#Create frequency tables for each variable overall
tabyl(hcl4df2$Age)
tabyl(hcl4df2$MonthlyIncome)
tabyl(hcl4df2$PercentSalaryHike)
tabyl(hcl4df2$YearsAtCompany)

#Create frequency tables for each variable by cluster
tabyl(hcl4df2,Age,clusterID) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits=2) %>%
  adorn_ns()

tabyl(hcl4df2,MonthlyIncome,clusterID) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits=2) %>%
  adorn_ns()

tabyl(hcl4df2,PercentSalaryHike,clusterID) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits=2) %>%
  adorn_ns()


hcl4df2 %>%
  group_by(clusterID) %>%
  summarize_all(mean)