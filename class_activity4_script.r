
library(tidyverse)

#####################################
##### Part 1: for loops         #####
#####################################

#Using only data for iris versicolor
#write a for loop
#that produces a regression table
#for each of the following relationships

vers<-filter(iris,iris$Species=="versicolor") # This command subsets into versicolor only

#1. iris  sepal length x width
#2. iris  petal length x width
#3. iris sepal length x petal length

#I made a list of three function relationships, made an empty object, and then filled it with the summary of the three regressions
vers.list<-list(vers$Sepal.Length~vers$Sepal.Width,
                vers$Petal.Length~vers$Petal.Width,
                vers$Sepal.Length~vers$Petal.Length)

regress<-numeric(0) #empty object

for(i in 1:3) {regress<-summary(lm(vers.list[[i]])) #run the three vers.list functions inside of summary(lm(x)) and fill the object
  print(regress)
  }


#####################################
##### Part 2: data in dplyr     #####
#####################################

#use dplyr to join data of maximum height
#to a new iris data frame
height <- data.frame(Species = c("virginica","setosa","versicolor"),
					Height.cm = c(60,100,11.8))

#Left join adds height to the iris dataset by finding species in commmon
iris_new<-left_join(iris,height)


#####################################
##### Part 3: plots in ggplot2  #####
#####################################

#look at base R scatter plot
plot(iris$Sepal.Length,iris$Sepal.Width)

#3a. now make the same plot in ggplot 
ggplot(data=iris)+
  geom_point(mapping = aes(Sepal.Length,Sepal.Width))

#3b. make a scatter plot with ggplot and get rid of  busy grid lines
ggplot(data=iris,mapping = aes(Sepal.Length,Sepal.Width))+
  geom_point()+
  theme(panel.grid=element_blank(),panel.background = element_rect(fill = "white",colour = "black"))

#3c.make a scatter plot with ggplot and get rid of grid lines,
#show species by color, and increase the point size
ggplot(data=iris,mapping = aes(Sepal.Length,Sepal.Width,color=Species))+
  geom_point(size=3)+
  theme(panel.grid=element_blank(),panel.background = element_rect(fill = "white",colour = "black"))

#####################################
##### Question: how did         #####
##### arguments differ between  #####
##### plot and ggplot?          #####
#####################################		

#Answer on moodle page

