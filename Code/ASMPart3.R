library("jsonlite")
library("ggplot2")
library("readr")
library("mclust")

df_bus_TO <- stream_in(file("https://www.scss.tcd.ie/~arwhite/Teaching/CS7DS3/Yelp%20data/Business_Toronto_Restaurant.json"),flatten=TRUE)

df_lat_long <-df_bus_TO[,c("latitude","longitude")]
head(df_lat_long)

plot(df_lat_long)

jpeg("Test.jpg")
fit <- Mclust(df_lat_long)
plot(fit, what = "BIC")
dev.off()
max(fit$BIC)

#Took groups from 1 to 9. Fitting it showed that 9 had the highest BICriteria and hence, we need to futher model it. An intuitive explanation for this  can be that there were more clusters in the data but since we had taken only 9 clusters the remaining datapoints that did not fit untill the  8 cluster fit in the 9 clusters. Hence, we need to futher perform clustering to understand further splitting
#Taking group 9 to 19 in order to also consider the group 9 against  which the Bic had turned out to be highest. This can help to compare between the 2 groups
jpeg("Test9to19.jpg")
fit <- Mclust(df_lat_long, G = 9:19)
plot(fit, what = "BIC")
dev.off()
max(fit$BIC)


table(fit$classification, fit2$classification)


fit2 <- Mclust(df_lat_long, G = 16, modelNames = "VVV")
plot(fit2, what="classification")


plot(fit2, what = "uncertainty",col=c(68:105))
fit2$uncertainty

##Due to the high  no. of cluster , we   are not able to differentiate between the neighbourhoods and hence we are taking a upper bound of 19

fit2 <- Mclust(df_lat_long, G = 32, modelNames = "VVV")
plot(fit2, what="classification")


df<- data.frame(fit2$classification, df_bus_TO$neighborhood)
colnames(df)[1] <- "new_cluster"
colnames(df)[2] <- "neighborhood" 

df$new_cluster = as.factor(df$new_cluster)  
df$neighborhood  = as.factor(df$neighborhood)


require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)
model = multinom(neighborhood~new_cluster,data = df,MaxNWts = 1500)
predict(model)

tab = table(predict(model,df),df$neighborhood)

cm = as.matrix(tab)
n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes


precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 

data.frame(precision, recall, f1) 

#Overall accuracy
sum(diag) / n 

df<- data.frame(fit2$classification, df_bus_TO$is_open)
colnames(df)[1] <- "new_cluster"
colnames(df)[2] <- "is_open" 

df$new_cluster = as.factor(df$new_cluster)  
df$neighborhood  = as.factor(df$is_open)

model = multinom(is_open~new_cluster,data = df,MaxNWts = 1500)
predict(model)

tab = table(predict(model,df),df$is_open)


cm = as.matrix(tab)
n = sum(cm) 
nc = nrow(cm)
diag = diag(cm)
rowsums = apply(cm, 1, sum) 
colsums = apply(cm, 2, sum) 
p = rowsums / n 
q = colsums / n 


precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 

data.frame(precision, recall, f1) 

#Overall accuracy
sum(diag) / n 

df_bus_TO$stars

