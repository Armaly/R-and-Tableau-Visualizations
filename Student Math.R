# studentMAT
#?

#install.packages("dplyr")
#install.packages("ggplot2")


setwd("~/")



library(class)
library(dplyr)
library(ggplot2)

fullset <- read.csv("student-mat.csv") # read the data into a dataframe
# Convert target variable gTOT to a factor
fullset$gTOT <- as.factor(fullset$gTOT)

# split the full dataset into randomly chosen training,and test sets


set.seed(2000) # random number?

splitSample <- sample(1:2, size=nrow(fullset),replace=TRUE, prob=c(0.7,0.3))
train <- fullset[splitSample==1,]
test <- fullset[splitSample==2,]


train <- select(train, Medu, Fedu,traveltime, studytime, absences, gTOT) #%>%
str(train)
  testassign <- select(test, Medu, Fedu, traveltime, studytime, absences, gTOT) #%>%

  str(testassign)
  
 train2 <- select(test, Medu, Fedu, traveltime, studytime, absences, gTOT)
 
 str(train2)
 
 ##GGPLOT STUFF
 
 #Test ggplot
 p <- ggplot(testassign, aes(x=traveltime, y=studytime, color=studytime))+
   geom_point(size=3)
 p
 
 #Train 2 ggplot
 p <- ggplot(train2, aes(x=traveltime, y=studytime, color=studytime))+
   geom_point(size=3)
 p
 
 ##GGPLOT ENDS

## normalize the data?

#trainscale <- scale(train2)
 str(train2)#test to see what train2 consists of
 colnames(train2)<-NULL#sets the column names   to null
 str(train2)#prints train2 
 class(train2)#checks train2's class type
 train2 <- as.matrix(sapply(train2, as.numeric))#converts train 2 to matrix and gets rid of uneeded strings
 class(train2)#checks class again
 train2 <- scale(train2)#scales train 2
#testscale <- scale(test$gTOT,traveltime, studytime, schoolsup, famsup, absences, gTOT )

# Now use k-means method to do the same job.
predkm2 <- kmeans(train2, 5) # identify the data table and value for k
# get cluster means
aggregate(train2,by=list(predkm2$cluster), FUN=mean)
pred2 <- predkm2$cluster

table(pred2)


#Experimental

#TestAssign

n <- nrow(testassign)-1
wss <- (n)*sum(apply(testassign,2,var))
for (i in 2:n) wss[i] <- sum(kmeans(testassign,
                                    centers=i)$withinss)
plot(1:n, wss, type="b", xlab="Number of Clusters",
     ylab="Travel Time",
     main="Study Time")

#Train 2
n <- nrow(train2)-1
wss <- (n)*sum(apply(train2,2,var))
for (i in 2:n) wss[i] <- sum(kmeans(train2,
                                    centers=i)$withinss)
plot(1:n, wss, type="b", xlab="Number of Clusters",
     ylab="Travel Time",
     main="Study Time")

# Now compute and graph k=means clusters

#TestAssign
options(digits=2)  # round numbers to 2 decimals
kc <- kmeans(testassign,4)   # initially us k=4 cluster solution
kc

# Hierarchical clustering with Dendograms

testassign <-as.data.frame(testassign)  # convert to dataframe
str(testassign)
testassign.dist <-dist(testassign)
testassign.hclust <- hclust(testassign.dist)

plot(testassign.hclust,labels=testassign$Name,main='Dendogram of Students for TestAssign', hang=-1)
#Note, grouped by unique key


#Create a vector showing the cluster membership 3 groups
groups.3 <- cutree(testassign.hclust,3)
table(groups.3)  # show number of items in each cluster
# draw dendogram with red borders around the 3 clusters
rect.hclust(testassign.hclust, k=3, border="red") 

groups.4 <- cutree(testassign.hclust,4)  # repeat for 4 groups
table (groups.4)
rect.hclust(testassign.hclust, k=4, border="blue") 

# show group means (3 clusters) with normalized and raw data
aggregate(testassign, list(groups.3),mean)
aggregate(testassign[,-c(1,1)], list(groups.3),mean)





#Train
# Now compute and graph k=means clusters
options(digits=2)  # round numbers to 2 decimals
kc <- kmeans(train2,4)   # initially us k=4 cluster solution
kc

train2 <-as.data.frame(train2)  # convert to dataframe
str(train2)
train2.dist <-dist(train2)
train2.hclust <- hclust(train2.dist)

plot(testassign.hclust,labels=testassign$Name,main='Dendogram of Students for Train2', hang=-1)
#Note, grouped by unique key

#Create a vector showing the cluster membership 3 groups
groups.3 <- cutree(train2.hclust,3)
table(groups.3)  # show number of items in each cluster
# draw dendogram with red borders around the 3 clusters
rect.hclust(train2.hclust, k=3, border="red") 

groups.4 <- cutree(train2.hclust,4)  # repeat for 4 groups
table (groups.4)
rect.hclust(train2.hclust, k=4, border="blue") 

# show group means (3 clusters) with normalized and raw data
aggregate(train2, list(groups.3),mean)
aggregate(train2[,-c(1,1)], list(groups.3),mean)

