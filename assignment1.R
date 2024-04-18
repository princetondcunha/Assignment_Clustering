#Libraries
library(ggplot2)
library(GGally)
library(DMwR)

#SetSeed
set.seed(4509)

#Function for SSEs
withinSSrange <- function(data,low,high,maxIter)
{
  withinss = array(0,dim=c(high-low+1));
  for(i in low:high)
  {
    withinss[i-low+1] <- kmeans(data,i,maxIter)$tot.withinss
  }
  withinss
}

--------------------------------------------------------------------------------------------------------
#CUSTOMER
--------------------------------------------------------------------------------------------------------
#Import Customer Data
cust <- read.csv("C:/Users/princ/Downloads/DataMining_Assignment/Data/Customer.csv", sep=";")

#Plot GGPairs
ggpairs(cust[,which(names(cust)!="CUSTOMER_SK")],upper = list(continuous = ggally_points), lower = list(continuous = "points"), title="Customers before outlier removal")

#Removing the outlier (CUSTOMER_SK = 1)
cust.clean <- cust[cust$CUSTOMER_SK != 1,]

#Plot GGPairs on cleaned data
ggpairs(cust.clean[,which(names(cust.clean)!="CUSTOMER_SK")],upper = list(continuous = ggally_points), lower = list(continuous = "points"), title="Customers in process of outlier removal")

#Removing the outliers
values_to_remove <- c(64593270, 52236223, 61864816, 26240377, 60279660, 34364924, 58892550)
cust.clean <- cust.clean[!(cust.clean$CUSTOMER_SK %in% values_to_remove), ]

#Plot GGPairs on cleaned data again
ggpairs(cust.clean[,which(names(cust.clean)!="CUSTOMER_SK")],upper = list(continuous = ggally_points), lower = list(continuous = "points"), title="Customers after outlier removal")

#Normalization
cust.scale <- scale(cust.clean[-1])

#Get SSE
plot(withinSSrange(cust.scale,1,50,150))

ckm = kmeans(cust.scale, 6, 150)

cust.realCenters = unscale(ckm$centers, cust.scale)

clusteredCust = cbind(cust.clean,ckm$cluster)

plot(clusteredCust[,2:6],col=ckm$cluster)

write.csv(clusteredCust, file = "C:/Users/princ/Downloads/DataMining_Assignment/Results/Customer_Cluster.csv",col.names = FALSE)

--------------------------------------------------------------------------------------------------------
#PRODUCTS
--------------------------------------------------------------------------------------------------------
  
#Import Product Data from SMU Server
prod <- read.csv("C:/Users/princ/Downloads/DataMining_Assignment/Data/Products.csv", sep=";")

#Plot GGPairs
ggpairs(prod[,which(names(prod)!="ITEM_SK")],upper = list(continuous = ggally_points), lower = list(continuous = "points"), title="Products before outlier removal")

#Removing the outliers
values_to_remove <- c(11740941, 11740923, 11680016, 13890560, 12518530, 12518502, 12518541, 12518601, 12518587)
prod.clean <- prod[!(prod$ITEM_SK %in% values_to_remove), ]

#Plot GGPairs on cleaned data
ggpairs(prod.clean[,which(names(prod.clean)!="ITEM_SK")],upper = list(continuous = ggally_points), lower = list(continuous = "points"), title="Products after outlier removal")

#Normalization
prod.scale <- scale(prod.clean[-1])

#Get SSE
plot(withinSSrange(prod.scale,1,50,150))

pkm = kmeans(prod.scale, 7, 150)

prod.realCenters = unscale(pkm$centers, prod.scale)

clusteredProd = cbind(prod.clean,pkm$cluster)

plot(clusteredProd[,2:5],col=pkm$cluster)

write.csv(clusteredProd, file = "C:/Users/princ/Downloads/DataMining_Assignment/Results/Products_Cluster.csv",col.names = FALSE)
