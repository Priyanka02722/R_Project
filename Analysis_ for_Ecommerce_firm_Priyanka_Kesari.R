#High Value Customers Identification for an Ecommerce firm

# Installing pacakges required for project
install.packages("plyr")
install.packages("ggplot2")
install.packages("scales")
install.packages("NbClust")

#Adding the pacakges to library
library(dplyr)
library(ggplot2)
library(NbClust)
library(scales)

getwd()
file.choose()
setwd("/Users/priyankakesari/Desktop/Learning R/R")
getwd()
my_data <- read.csv("Ecommerce.csv",stringsAsFactors = TRUE)
View(my_data)

#Understand the Ecommerce data
class(my_data)
str(my_data)
head(my_data)

#Summary of data
summary(my_data)
#dimensions as stated in Global Environment
dim(my_data) #541909, 9

#Column X has a NA values - Removing the column
my_data_subset <- subset(my_data,select = -X)
View(my_data_subset)
str(my_data_subset)

# Data Manipulation
length(unique(my_data_subset$CustomerID)) # 4373
sum(is.na(my_data_subset$CustomerID))  #135080

length(unique(my_data_subset$InvoiceNo)) # 25900
length(unique(my_data_subset$StockCode)) #4070
length(unique(my_data_subset$Description)) # 4224
length(unique(my_data_subset$Quantity)) #722
length(unique(my_data_subset$InvoiceDate)) # 305
length(unique(my_data_subset$UnitPrice)) #1630
length(unique(my_data_subset$Country)) # 38



mean(is.na(my_data_subset)) # 3.1 % data has missing values so we can ignore it

pMiss <- function(x){sum(is.na(x) / length(x)*100)}
apply(my_data_subset,2,pMiss)
#Total number of missing value in each column
Miss <- function(x){sum(is.na(x))}
apply(my_data_subset,2,Miss) # Customer ID states 135080
#25% of Customer ID Data is missing which is high in number

# In order to understand the missing data's pattern
install.packages("mice")
library(mice)

md.pattern(my_data_subset)

# Data Visualization for missing values

install.packages("VIM", dependencies = T)
library(VIM)

aggr_plot <- aggr(my_data_subset, col=c('red','green'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(my_data_subset), cex.axis=.8, gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))

my_data_subset_new <- subset(my_data_subset,is.na(my_data_subset$CustomerID))
View(my_data_subset_new)
my_data_subset_new <- subset(my_data_subset, Country == "United Kingdom")
View(my_data_subset_new)

length(unique(my_data_subset_new$InvoiceNo)) # 23494
length(unique(my_data_subset_new$CustomerID))   # 3951

# Data filtered to my_data_subset_new where null value is ommitted Invoice no. 23494 and Customer ID os 3951

#Data Manipulation removing value which are negative in quantity column
new_quant <- my_data_subset_new[my_data_subset_new$Quantity > 0,]
nrow(new_quant)  # 486286

# Date format change in Invoice Date Column
my_data_subset_new$InvoiceDate <- as.Date(my_data_subset_new$InvoiceDate, format = "%d-%b-%y")
str(my_data_subset_new$InvoiceDate)

my_data_subset_new$InvoiceNo <- as.integer(my_data_subset_new$InvoiceNo) # converting Invoice no as integer
str(my_data_subset_new$InvoiceNo)

# Adding a column to analyze customer purchasing behavior i.e. amount spent by the customer
my_data_subset_new['amount_spent'] = my_data_subset_new['Quantity'] * my_data_subset_new['UnitPrice']

View(my_data_subset_new) # Check data

#Clustering by geography
table(my_data_subset_new$Country)

# Distinguishing between invoices with purchases from invoices with returns to understand behavorial pattern of customer
# identify how many returns 
my_data_subset_new$item.return <- grepl("C",my_data_subset_new$InvoiceNo, fixed = TRUE)
my_data_subset_new$purchase.invoice <- ifelse(my_data_subset_new$item.return=="TRUE",0,1)
View(my_data_subset_new)

#Creating dataframe for customers

Customers <- as.data.frame(unique(my_data_subset_new$CustomerID))
names(Customers) <- "CustomerID"

#Recent purchase subtracting the Invoice date from last invoice date
my_data_subset_new$recent <- as.Date("2017-12-08") - as.Date(my_data_subset_new$InvoiceDate)

# Remove the column returns to consider most recent purchase
temp <- subset(my_data_subset_new, purchase.invoice == 1)

  #obtain no. of days of most recent purchase
recent <- aggregate(recent ~ CustomerID, data = temp, FUN = min, na.rm=TRUE)
remove(temp)

View(my_data_subset_new)
View(Customers)

#Add recent data to customers
Customers <- merge(Customers, recent, by="CustomerID", all=TRUE, sort=TRUE)
remove(recent)
View(Customers)
Customers$recent <- as.numeric(Customers$recent)
View(Customers)

#Check Frequency
Customers.invoices <- subset(my_data_subset_new, select = c("CustomerID", "InvoiceNo","purchase.invoice"))
Customers.invoices <- Customers.invoices[!duplicated(Customers.invoices), ]
Customers.invoices <- Customers.invoices[order(Customers.invoices$CustomerID),]
row.names(Customers.invoices) <- NULL

# No. of invoices per year
Yearly.invoices <- aggregate(purchase.invoice ~ CustomerID, data = Customers.invoices, FUN = sum, na.rm=TRUE)
names(Yearly.invoices)[names(Yearly.invoices)=="purchase.invoice"] <-"Frequency"

# Yearly invoices to Customer data Table
Customers <- merge(Customers,Yearly.invoices, by="CustomerID",all=TRUE, sort=TRUE)
remove(Customers.invoices, Yearly.invoices)

range(Customers$Frequency)
table(Customers$Frequency)
View(Customers)

#Delete customers who have not made any puchases in past years
Customers <- subset(Customers, Frequency > 0)
View(Customers)

#lets check total amount spent on each item in invoice
View(my_data_subset_new)
#Total sales to customer

Total.sales <- aggregate(amount_spent ~ CustomerID, data = my_data_subset_new, FUN = sum, na.rm=TRUE)
names(Total.sales)[names(Total.sales)=="amount_spent"] <- "Monetary_value"

#Add new table to Customers data set
Customers <- merge(Customers, Total.sales, by="CustomerID", all.x = TRUE, sort = TRUE)
remove(Total.sales)
View(Customers)

# Identify negative monetary value in customers dataset
hist(Customers$Monetary_value)
Customers$Monetary_value <- ifelse(Customers$Monetary_value < 0 , 0 , Customers$Monetary_value)
hist(Customers$Monetary_value)

#Data Testing Principle

Customers <- Customers[order(-Customers$Monetary_value),]
high.cutoff <- 0.8 * sum(Customers$Monetary_value)
Customers$high <- ifelse(cumsum(Customers$Monetary_value)<= high.cutoff, "Top 20%", "Bottom 80%")
Customers$high <- factor(Customers$high, levels= c("Top 20%", "bottom 80%"),ordered = TRUE)
levels(Customers$high)
round(prop.table(table(Customers$high)),2)

Customers <- Customers[order(Customers$CustomerID),]

# Transform positively skewd variables
Customers$recent.log <- log(Customers$recent)
Customers$Frequency.log <- log(Customers$Frequency)
Customers$Monetary_value.log <- Customers$Monetary_value+0.1
Customers$Monetary_value.log <- log(Customers$Monetary_value.log)

# Z Test

Customers$recent.z <- scale(Customers$recent.log, center = TRUE, scale= TRUE)
Customers$Frequency.z <- scale(Customers$Frequency.log, center = TRUE, scale= TRUE)
Customers$Monetary_value.z <- scale(Customers$Monetary_value.log, center = TRUE, scale = TRUE)

View(Customers)

# Data Visualization for better understanding

# Original scale

scatter_plot.1 <- ggplot(Customers, aes(x= Frequency, y = Monetary_value))
scatter_plot.1 <- scatter_plot.1 + geom_point(mapping = aes(color = recent, shape = "." ))
scatter_plot.1 <- scatter_plot.1 + scale_shape_manual(name = "80/20 Rule", values = c(17, 16))
scatter_plot.1 <- scatter_plot.1 + scale_color_gradient(name ="Recent\n(Days since last Purchases))")
scatter_plot.1 <- scatter_plot.1 + scale_y_continuous(labels = dollar)
scatter_plot.1 <- scatter_plot.1 + xlab ("Frequency ( No.of Purchases)")
scatter_plot.1 <- scatter_plot.1 + ylab ("Monetary value (Yearly Sales)")
scatter_plot.1

# as per the graph there is clump of data points in left side corner of the plot and few outliers
# lets try log transformed values

scatter_plot.2 <- ggplot(Customers, aes(x= Frequency.log, y = Monetary_value.log))
scatter_plot.2 <- scatter_plot.2 + geom_point(mapping = aes(color = recent.log, shape = "." ))
scatter_plot.2 <- scatter_plot.2 + scale_shape_manual(name = "80/20 Rule", values = c(17, 16))
scatter_plot.2 <- scatter_plot.2 + scale_color_gradient(name ="Recent Log transformed values")
scatter_plot.2 <- scatter_plot.2 + scale_y_continuous(labels = dollar)
scatter_plot.2 <- scatter_plot.2 + xlab ("Frequency log transformed")
scatter_plot.2 <- scatter_plot.2 + ylab ("Monetary_value log transformed")
scatter_plot.2

# Z test Scale variable
scatter_plot.3 <- ggplot(Customers, aes(x= Frequency.z, y = Monetary_value.z))
scatter_plot.3 <- scatter_plot.3 + geom_point(mapping = aes(color = recent.z, shape = "." ))
scatter_plot.3 <- scatter_plot.3 + scale_shape_manual(name = "80/20 Rule", values = c(17, 16))
scatter_plot.3 <- scatter_plot.3 + scale_color_gradient(name ="Recent Z Score")
scatter_plot.3 <- scatter_plot.3 + scale_y_continuous(labels = dollar)
scatter_plot.3 <- scatter_plot.3 + xlab ("Frequency Z score")
scatter_plot.3 <- scatter_plot.3 + ylab ("Monetary_value Z score")
scatter_plot.3

# K means clustering

data_preprocessing <- Customers[,9:11]
clustmax <- 10 # Required no. of clusters

models <- data.frame(k=integer(),
                     tot.withinss=numeric(),
                     betweenss=numeric(),
                     totss=numeric(),
                     rssquared=numeric())
for (k in 1:clustmax )  
  {
  print(k)
  


# iter the retun to use in final model
output <- kmeans(data_preprocessing, centers = k, nstart = 20)

  # Add clusters to customers dataset
var.name <- paste("clusters",k,sep = "_")
Customers[,(var.name)] <- output$cluster
Customers[,(var.name)] <- factor(Customers[,(var.name)], levels= c(1:k))

#Graph clusters
graph_clusters <- ggplot(Customers, aes(x= Frequency.log, y= Monetary_value.log))
graph_clusters <- graph_clusters + geom_point(aes(colour = Customers[,(var.name)]))                                           
colors <- c('orange', 'blue', 'green3','deepskyblue','red','darkorchid4','violet','pink1','tan3','black')
graph_clusters <- graph_clusters + scale_color_manual(name = "Group of clusters", values = colors)
graph_clusters <- graph_clusters + xlab("Frequency log transformed")
graph_clusters <- graph_clusters + ylab(" Monetary_value log transformed")
title <- paste("k-means Solution with", k, sep=" ")
title <- paste(title, "Clusters", sep=" ")
graph_clusters <- graph_clusters + ggtitle(title)
print(graph_clusters)

# Lets check clusters in original metrics
library(dplyr)

library(plyr)
library(reshape2)

print(title)
center_clusters <- ddply(Customers, .(Customers[,(var.name)]),summarize,
                         Monetary_value = round(median(Monetary_value),2),
                         Frequency = round(median(Frequency),1),
                         recent = round(median(recent),0))
names(center_clusters)[names(center_clusters)=="Customers[,(var.names)]"] <- "Clusters"
print(center_clusters)
cat("\n")
cat("\n")

#model information
models[k,("k")] <- k
models[k,("tot.withiness")] <- output$tot.withinss
models[k,("betweeness")] <- output$betweenss
models[k,("totss")] <- output$totss
models[k,('rsquared')] <- round(output$betweenss/output$totss,3)
assign("models", models, envir = .GlobalEnv)

remove(output, var.name, graph_clusters,center_clusters, title, colors)
}

any(is.na(models))
models = models[-which(is.na(models))]

# Variance graph
r_graph <- ggplot(models, aes(x = k, y = rsquared, group = 1)) 
r_graph <- r_graph + geom_point() + geom_line()
r_graph <- r_graph + scale_y_continuous(labels = scales::percent)
r_graph <- r_graph + scale_x_continuous(breaks = 1:clustmax)
r_graph <- r_graph + xlab("k (Number of Clusters)")
r_graph <- r_graph + ylab("Variance")
r_graph

#Graph for no. of clusters and withiness
graph1 <- ggplot(models, aes(x = k, y = tot.withiness))
graph1 <- graph1 + geom_point() + geom_line()
graph1 <- graph1 + scale_x_continuous(breaks = 1:clustmax)
graph1 <- graph1 + scale_y_continuous(labels = scales::comma)
graph1 <- graph1 + xlab("k (Number of Clusters)")
graph1 <- graph1 + ylab("Total Withiness")
graph1 # bend in the graph at point 6 shows winthiness

# determine no of clusters using Nbclust Library

library(NbClust)
set.seed(1)
nc <- NbClust(data_preprocessing,min.nc =2 , max.nc = 8, method = "kmeans")
table(nc$Best.n[1,])

nc$All.index
#Graph

barplot(table(nc$Best.n[1,]),xlab="No. of Clusters",ylab= "No. of Criteria ",main="No. of Clusters chosen as per Criteria")

# Hierrachical Clustering
my_data2 <- read.csv("Ecommerce.csv",header = T)
# Data Cleaning
my_data2 <- na.omit(my_data2)
my_data2 <- my_data2[!apply(is.na(my_data2[,-1]), 1, all),]
ncol(m)y_data2
cluster_h <- as.data.frame.matrix(my_data2)
cluster_h <- t(cluster_h)

H_clustering <- dist(my_data2_subset, method ="euclidean")
fit <- H_clustering(my_data2_subset, method = "ward")
?H_clustering

groups <- cutree(fit,k= 3)
groups

my_data2_subset <- cbind(my_data2_subset, ClusterNum = groups)

plot(fit)
rect.hclust(fit,k=3, border = 'blue')
