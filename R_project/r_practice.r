library("ggplot2")

plantData <- read.table(file = "plantData.csv", header = T, sep =",", dec = "." )
summary(plantdata)

#x[which(!is.na(x))]     
mean(x[which(!is.na(x))])
is.na(plantdata$Sepal.Length)


meanSL= mean(plantData$Sepal.Length, na.rm = TRUE)

meanSW= mean(plantData$Sepal.Width, na.rm = TRUE)

plantData$Sepal.Length[is.na(plantData$Sepal.Length)]=  meanSL

plantData$Sepal.Width[is.na(plantData$Sepal.Width)] =  meanSW


#### replacing na in the columns

ggplot(data = plantData, aes(x = Sepal.Length)) + geom_histogram()

ggplot(data = plantData, aes(x = Sepal.Length)) +
  geom_histogram() +
  geom_histogram(binwidth = 0.2)


bplot <- ggplot(data = plantData, aes(x = Sepal.Length)) +
  geom_histogram(binwidth = 1, aes(fill = Species))
bplot


ggplot(data = plantData, aes(x = Sepal.Length)) +
  geom_density(aes(fill = Species, alpha = 0.5))


ggplot(plantData, aes(x = Sepal.Width)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.2, fill = "lightblue") +
  geom_freqpoly(aes(y = ..density..), binwidth = 0.2, color = "red")


ggplot(data = plantData, aes(x = Species, y = Petal.Length)) +
  geom_col(aes(fill = Species))

ggplot(data = plantData, aes(x = "", fill = Species)) +
  geom_bar() + coord_polar(theta = "y")

#plotting in the highliting of outliers

boxplot <- ggplot(data = plantData, aes(x = Species, y = Sepal.Length)) +
  geom_boxplot(aes(fill = Species)) 
outliers <- geom_boxplot(aes(fill = Species), outlier.colour = "red",
                         outlier.shape = 8, outlier.size = 2)

boxplot + outliers

#this can also be viewed togther 

ggplot(data = plantData, aes(x = Species, y = Sepal.Length)) +
  geom_boxplot(aes(fill = Species), outlier.colour = "red",
               outlier.shape = 8, outlier.size = 2) 

#flipping box blox or plots using + coord_flip()

ggplot(data = plantData, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point(aes(colour = Species, shape = Species)) + coord_flip()


###################################################################
mydata <- read.table(file = "plantData-Full.csv", header = T, sep = ",", dec = ".")
summary(mydata)
sum(is.null(mydata))
View(mydata)
mydata[,"Species"]
summary(mydata)
df1 <- mydata[, "Species"]
table(df1)
df1 <- as.factor(df1)
class(df1)
typeof(df1) 
mode(df1)
str(df1)
str(mydata)
df2 <- mydata[, c(1,2,3,4)]
str(df2)
length(df2)
dim(df2)
df1 <- as.factor(df1)
class(df1)
str(df1)
df1
install.packages("animation")
library("animation")

kmeans.ani (x = df2, centers = 3,
            hints = c("cluster centre moving", "finding cluster"),
            pch = 2:4, col = 2:4)

#data normalization
normalize <- function(x){
  normalized_data <- (x - min(x)) / max(x) - min(x)
  return(normalized_data)
}

for (i in 1:ncol(df2)){
  df2[,i] <- normalize(df2[, i]) 
}

cluster_result <- kmeans(df2, centers = 3)

cluster_result$size

cluster_result$cluster

#### Plot clustered observationsfor Sepal.Length and Sepal.Width
par(mfrow=c(1,1))

plot(df2[c(1,2)], col= cluster_result$cluster) #this shows how SL and SW are distributed

plot(df2[c(1,2)], col=df1)
class(cluster_result$cluster)
cluster_result$

######################################################
#plotting the cluster analysis using GGplot

ggplot(data = df2, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point(aes(colour = as.factor(cluster_result$cluster)))
