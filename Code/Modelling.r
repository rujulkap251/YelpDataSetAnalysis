MyData <- read.csv(file="C:/Users/rujulk/Downloads/Final_dataset.csv", header=TRUE, sep=",")
MyData[,"X"] <- NULL
names(MyData)
dim(MyData)
names(MyData)
require(caTools)
set.seed(101)
sample = sample.split(MyData$is_open, SplitRatio = .70)
train_d = subset(MyData, sample == TRUE)
test_d  = subset(MyData, sample == FALSE)




sort(sapply(MyData, function(y) sum(length(which(is.na(y))))))

logitMod <- glm(is_open~neighborhood+longitude+latitude+stars+review_count+Chinese
                +Pizza+American..Traditional.+df_Thursday_closed+Friday_open+Friday_closed
                +df_Saturday_open+RestaurantsPriceRange2+Alcohol+RestaurantsTakeOut+Ambience_casual+BusinessAcceptsCreditCards
                ,data=train_d)
plot(logitMod)

logitMod$xlevels

library(caret)
# Use your model to make predictions, in this example newdata = training set, but replace with your test set
logitMod$xlevels$neighborhood <- union(logitMod$xlevels$description, levels(train_d$neighborhood))
logitMod$xlevels$df_Thursday_closed <- union(logitMod$xlevels$description, levels(test_d$df_Thursday_closed))
logitMod$xlevels$Friday_open <- union(logitMod$xlevels$description, levels(test_d$Friday_open))
logitMod$xlevels$df_Saturday_open <- union(logitMod$xlevels$description, levels(test_d$df_Saturday_open))
logitMod$xlevels$Friday_closed <- union(logitMod$xlevels$description, levels(test_d$Friday_closed))

pdata <- predict(logitMod, newdata = test_d, type = "response",se.fit=FALSE)


# use caret and compute a confusion matrix
library(caret)
confusionMatrix(data = factor(as.numeric(pdata>0.5)), reference = factor(test_d$is_open))


library('glmnet')
library(glmnetUtils)


regularized <- glmnet(is_open~neighborhood+longitude+latitude+stars+review_count+Chinese
                      +Pizza+American..Traditional.+df_Thursday_closed+Friday_open+Friday_closed
                      +df_Saturday_open+RestaurantsPriceRange2+Alcohol+RestaurantsTakeOut+Ambience_casual+BusinessAcceptsCreditCards
                      ,data=train_d,family = "binomial",lambda=0.004)
regularized

library(caret)
# Use your model to make predictions, in this example newdata = training set, but replace with your test set   
regularized$xlevels$neighborhood <- union(regularized$xlevels$description, levels(MyData$neighborhood))
pdata <- predict(regularized, newdata = test_d, type = "response",se.fit=FALSE)

coef(regularized$xlevels$neighborhood)
# use caret and compute a confusion matrix
confusionMatrix(data = factor(as.numeric(pdata>0.5)), reference = factor(test_d$is_open))


coef(regularized)
barchart(coef(regularized)[,"s0"])


regularized_cv <- cv.glmnet(is_open~neighborhood+longitude+latitude+stars+review_count+Chinese
                            +Pizza+American..Traditional.+df_Thursday_closed+Friday_open+Friday_closed
                            +df_Saturday_open+RestaurantsPriceRange2+Alcohol+RestaurantsTakeOut+Ambience_casual+BusinessAcceptsCreditCards
                            ,data=train_d,family = "binomial")
plot(regularized_cv)
coef(regularized_cv)
pdata <- predict(regularized_cv, newdata = test_d, type = "response",se.fit=FALSE)


require(randomForest)
require(MASS)
train=sample(1:nrow(MyData),1500)
names(MyData)
MyData[,"neighborhood"]<-NULL
MyData.rf=randomForest(is_open ~ . , data = MyData , subset = train)
plot(MyData)
MyData$is_open=as.factor(MyData$is_open)
