library(DMwR)
library(cvTools)
library(tree)
library(ggplot2)

#================== Data of cmc =======================

cmc.dat <- read.table("/Users/tracy/msan-ml/hw1/cmc.txt", sep=',')
percentage <- c(0,0.1,0.2,0.3,0.4,0.5,0.6)
folds <- cvFolds(nrow(cmc.dat), K = 10, R = 1)
df <- cmc.dat
attach(df)

class.column <- 10
attribute.column <- 4
df[,class.column] <- as.factor(df[,class.column])

folds <- cvFolds(nrow(df), K = 10, R = 1)
error.rate <- data.frame()
for (i in percentage){
    SE.c <- 0
    SE.knn <- 0
    SE.mean <- 0
    for (k in 1:10){
        df.test <- df[which(folds$which==k),]
        df.train <- df[which(folds$which!=k),]
        missing.index <- sample(1:nrow(df.train), round(i*nrow(df.train)))
        df.train[missing.index, 4] <- NA
        
        c.tree <- tree(V10~., df.train)
        pred.c <- predict(c.tree, df.test, type = "class")
        t.c <- table(pred.c, df.test$V10)
        SE.c <- SE.c + (sum(t.c)-t.c[1,1]-t.c[2,2]-t.c[3,3])/sum(t.c)

        knn.imp.train <- df.train
        knn.imp.test <- df.test
        
        knn.imp.train[1:9] <- scale(as.matrix(knn.imp.train[1:9]))
        knn.imp.test[1:9] <- scale(as.matrix(knn.imp.test[1:9]))
        
        if (sum(is.na(knn.imp.train[,4])) > 0) {
            # use knn to impute the missing value
            knn.imp.train <- knnImputation(knn.imp.train, k=10)
            # naturally have some missing values
        }

        knn.tree <- tree(V10~., knn.imp.train)
        pred.knn.tree <- predict(knn.tree, knn.imp.test, type = "class")
        t.knn <- table(pred.knn.tree, knn.imp.test$V10)
        SE.knn <- SE.knn + (sum(t.knn)-t.knn[1,1]-t.knn[2,2]-t.knn[3,3])/sum(t.knn)
        
        mean.imp.train <- df.train
        mean.imp.test <- df.test
        mean.imp.train[missing.index, 4] <- mean(mean.imp.train[,4],na.rm = TRUE) 
        
        mean.tree <- tree(V10~ ., mean.imp.train)
        pred.mean.tree <- predict(mean.tree, mean.imp.test, type = "class")
        t.mean <- table(pred.mean.tree, mean.imp.test$V10)
        SE.mean <- SE.mean + (sum(t.mean)-t.mean[1,1]-t.mean[2,2]-t.mean[3,3])/sum(t.mean)

    }
    MSE.c <- SE.c/10
    MSE.knn <- SE.knn/10
    MSE.mean <- SE.mean/10
    error.rate <- rbind(error.rate, c(MSE.c, MSE.knn, MSE.mean))
}
result <- cbind(percentage, error.rate)
names(result)[2:4] <- c("noTreat", "knn", "mean")
percentage.rate <- as.numeric(rep(percentage, 3))
noTreat <- result$noTreat
knn <- result$knn
mean <- result$mean
error <- as.numeric(c(noTreat, knn, mean))
treatment <- c(rep("noTreat",7), rep("knn",7), rep("mean",7))
plot.df <- as.data.frame(cbind(percentage.rate, error, treatment))
plot.df[,1] <- as.numeric(percentage)
plot.df[,2] <- as.numeric(error)

p1 <- ggplot(plot.df, aes(x = percentage.rate, y = error, colour = treatment)) +
    geom_line() +
    ggtitle("Missing data inserted into attribute 3 for cmc dataset")


#==================== Data of Breaset ===================

breast.dat <- read.table("/Users/tracy/msan-ml/hw1/breast.txt", sep=',',
                         stringsAsFactors = F, na.strings = "?")

percentage <- c(0,0.1,0.2,0.3,0.4,0.5,0.6)

attach(df)
df <- breast.dat
class.column <- 11
attribute.column <- 2
df[,class.column] <- as.factor(df[,class.column])

folds <- cvFolds(nrow(df), K = 10, R = 1)
error.rate <- data.frame()
for (i in percentage){
    SE.c <- 0
    SE.knn <- 0
    SE.mean <- 0
    
    for (k in 1:10){
        df.test <- df[which(folds$which==k),]
        df.train <- df[which(folds$which!=k),]
        missing.index <- sample(1:nrow(df.train), round(i*nrow(df.train)))
        df.train[missing.index, 2] <- NA
        
        c.tree <- tree(V11~., df.train)
        pred.c <- predict(c.tree, df.test, type = "class")
        t.c <- table(pred.c, df.test$V11)
        SE.c <- SE.c + (sum(t.c)-t.c[1,1]-t.c[2,2])/sum(t.c)

        knn.imp.train <- df.train
        knn.imp.test <- df.test
        
        knn.imp.train[1:10] <- scale(as.matrix(knn.imp.train[1:10]))
        knn.imp.test[1:10] <- scale(as.matrix(knn.imp.test[1:10]))
        
        if (sum(is.na(knn.imp.train[,4]) > 0)) {
            # use knn to impute the missing value
            knn.imp.train <- knnImputation(knn.imp.train, k=10)
            # naturally have some missing values
            knn.imp.test <- knnImputation(knn.imp.test, k=10)
        }

        knn.tree <- tree(V11~ ., knn.imp.train)
        pred.knn.tree <- predict(knn.tree, knn.imp.test, type = "class")
        t.knn <- table(pred.knn.tree, knn.imp.test$V11)
        SE.knn <- SE.knn + (sum(t.knn)-t.knn[1,1]-t.knn[2,2])/sum(t.knn)
        
        mean.imp.train <- df.train
        mean.imp.test <- df.test
        mean.imp.train[missing.index, 2] <- mean(mean.imp.train[,2],na.rm = TRUE) 

        mean.tree <- tree(V11~ ., mean.imp.train)
        pred.mean.tree <- predict(mean.tree, mean.imp.test, type = "class")
        t.mean <- table(pred.mean.tree, mean.imp.test$V11)
        SE.mean <- SE.mean + (sum(t.mean)-t.mean[1,1]-t.mean[2,2])/sum(t.mean)

    }
    MSE.c <- SE.c/10
    MSE.knn <- SE.knn/10
    MSE.mean <- SE.mean/10
    error.rate <- rbind(error.rate, c(MSE.c, MSE.knn, MSE.mean))

}
result <- cbind(percentage, error.rate)
names(result)[2:4] <- c("noTreat", "knn", "mean")
percentage.rate <- as.numeric(rep(percentage, 3))
noTreat <- result$noTreat
knn <- result$knn
mean <- result$mean
error <- as.numeric(c(noTreat, knn, mean))
treatment <- c(rep("noTreat",7), rep("knn",7), rep("mean",7))
plot.df <- as.data.frame(cbind(percentage.rate, error, treatment))
plot.df[,1] <- as.numeric(percentage.rate)
plot.df[,2] <- as.numeric(error)

p2 <- ggplot(plot.df, aes(x = percentage.rate, y = error, colour = treatment)) +
    geom_line() +
    ggtitle("Missing data inserted into attribute 1 for breast dataset")
    

## Useful variables
Explore relationship of predictive variables and response variables in order to choose reasonable predivtive variables to use. For the purpose of exploring, some print out may be messy, so here I only show code and analysis for clean format. Except signup\_date and last\_trip\_date, the other variables all have some relationship with active. So we will include these in the model.

```{r, echo=FALSE}
tb_rating_of_ac <- table(data$avg_rating_of_driver, data$active, useNA = c("always"))
prop.table(tb_rating_of_ac, margin=1)
# we can see as the rating increase, basically the propotion in the active column increases. It's reasonable.

tbl = rbind(na.f, na.t)
colnames(tbl) = col.levs
rownames(tbl) = c("isNA.false", "isNA.true")
prop.table(tbl, margin=1)

tb_surge_ac <- table(data$surge_pct, data$active)
prop.table(tb_surge_ac, 1)
# roughly go through all the result we can see as the percentage increase the active column decrease

tb_avgsurge_ac <- table(data$avg_surge, data$active)
prop.table(tb_avgsurge_ac, 1)
# observation and conclusion are same as the above

tb_trips_ac <- table(data$trips_in_first_30_days, data$active)
prop.table(tb_trips_ac, 1)
# not very obvious

tb_black_active <- table(data$uber_black_user, data$active)
ptb_black_ac <- prop.table(tb_black_active, 1)
# black rider tend to be active

tb_week_ac <- table(data$weekday_pct, data$active)
prop.table(tb_week_ac, 1) 
# don't have too much relationship here 

tb_city_ac <- table(data$city, data$active)
prop.table(tb_city_ac, 1)
# King's landing have much higher proportion than other two cities
```
