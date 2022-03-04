library(ggplot2)

library(corrplot)
library(dplyr)
library(glmnet)
#install.packages('tree')
library(tree)


#loading in data
data = read.csv("E:/DMML_Datasets/king_county/KC_house_data.csv", na.string = ".")
set.seed(7)
data_new<- data


########## initial observations of dataset dimension #########
# 21613 observations and 21 variable
str(data_new)


### checking null values ##########

NA_values=data.frame(no_of_na_values=colSums(is.na(data_new)))
NA_values
head(NA_values,21)


##### removing id and date ########
data_new =subset(data_new,select = -c(id,date))


str(data_new)


####corrplot #####

data1 <- sample_frac(data_new, .1)
M<-cor(data1)
corrplot(M,method="number")


########## scatter plots ##############

library(psych)
pairs.panels(data1, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs(data1,panel=panel.smooth)

#checking linear relashionship with price for continuous variables

library(ggplot2)
library(gridExtra)

p1=ggplot(data = data_new, aes(x = bedrooms, y = price)) +
  geom_jitter() +  geom_smooth(method = "lm", se = FALSE)+labs(title="Scatter plot of Bedrooms and Price", x="bedrooms",y="Price")
p1
p2=ggplot(data = data_new, aes(x = bathrooms, y = price)) +
  geom_jitter() +  geom_smooth(method = "lm", se = FALSE)+labs(title="Scatter plot of Bathrooms and Price", x="bathrooms",y="Price")
p3=ggplot(data = data_new, aes(x = sqft_living, y = price)) +
  geom_jitter() +  geom_smooth(method = "lm", se = FALSE)+labs(title="Scatter plot of Sqft_living and Price", x="Sqft_living",y="Price")
p4=ggplot(data = data_new, aes(x = sqft_above, y = price)) +
  geom_jitter() +  geom_smooth(method = "lm", se = FALSE)+labs(title="Scatter plot of Sqft_above and Price", x="Sqft_above",y="Price")
p5=ggplot(data = data_new, aes(x = sqft_basement, y = price)) +
  geom_jitter() +  geom_smooth(method = "lm", se = FALSE)+labs(title="Scatter plot of Sqft_basement and Price", x="Sqft_basement",y="Price")
p6=ggplot(data = data_new, aes(x = lat, y = price)) +
  geom_jitter() +  geom_smooth(method = "lm", se = FALSE)+labs(title="Scatter plot of Latitude and Price", x="Latitude",y="Price")
p7=ggplot(data = data_new, aes(x = sqft_living15, y = price)) +
  geom_jitter() +  geom_smooth(method = "lm", se = FALSE)+labs(title="Scatter plot of Sqft_living15 and Price", x="Sqft_living15",y="Price")
grid.arrange(p1,p2,p3,p4,p5,p6,p7,nrow=4)



########## boxplot for categorical #############

par(mfrow=c(2, 2))
# bedrooms against price on a box plot we observe the price range to increase with bedrooms count.
boxplot(price~bedrooms,data=data_new,main="Different boxplots", xlab="bedrooms",ylab="price",col="orange",border="brown")
# grade increases  price increases
boxplot(price~grade,data=data_new,main="Different boxplots", xlab="grade",ylab="price",col="orange",border="brown")
# zipcode also effects price as seen few zipcodes showing high price
boxplot(price~zipcode,data=data_new,main="Different boxplots", xlab="zipcode",ylab="price",col="orange",border="brown")
# price also increases with increase in bathrooms
boxplot(price~bathrooms,data=data_new,main="Different boxplots", xlab="bathrooms",ylab="price",col="orange",border="brown")
# price also increases with increase in view
boxplot(price~view,data=data_new,main="Different boxplots", xlab="view",ylab="price",col="orange",border="brown")
###############observation ################################

###by observing above boxplot 33 bedroom with single foor seems to be anamoly removing it #############

anamoly_row<-data_new[data_new$bedrooms == 33, ]
anamoly_row
#found 33 bedroom with 1.5bathroom and 1floor not possible removing that row based on index value found 15871
data_new <- data_new[-c(15871), ]




#########split train and test ########

set.seed(7)
train = data_new %>% sample_frac(.7)
test = data_new %>% setdiff(train)




###### linear regression #######



lm.fit1=lm(price~.-zipcode-sqft_basement, data = train)
summary(lm.fit1)#Adjusted R-squared:  0.698 


#removing few variables to increase R2 values
lm.fit2=lm(price~.-zipcode-sqft_lot-floors-sqft_basement, data = train)
summary(lm.fit2)#Adjusted R-squared:  0.6978



lm.fit3=lm(price~.-zipcode-sqft_lot-floors-sqft_basement-sqft_above-yr_renovated-sqft_living15-sqft_lot15-long, data = train)
summary(lm.fit3)#Adjusted R-squared:  0.6945 




lm.fit4=lm(price~bedrooms+bathrooms+sqft_living+waterfront+view+grade+condition, data = train)
summary(lm.fit4)#Adjusted R-squared:0.6059 

library(performance)
library(see)
library(patchwork)
check_model(lm.fit4)


## checked graph it should some pattern so applied log on dependent variable
lm.fit5=lm(log(price)~.-zipcode-sqft_basement-floors, data = train)


summary(lm.fit5)#Adjusted R-squared:  0.7665 - good R2 value


############################# gauss-markovas assumptions testing #########################


library(performance)
library(see)
library(patchwork)
check_model(lm.fit5)

library(car)
vif(lm.fit5)# - all variables values are less than 10 no multicollinearity
ncvTest(lm.fit5)#ncv failed p<0.05
durbinWatsonTest(lm.fit5)#D-W Statistic value is 1.99283 which is close to 2 accepted




####prediction on Test data ####

pred1 <- predict(lm.fit5, newdata = test)

###mean square value is 0.06427
lm.mse=round(mean((pred1 - log(test$price))^2),5)

lm.mse

### using inbuilt library to calculate mse ######
library(Metrics)
d=mse(log(test$price),predict(lm.fit5,test))
d


#############################################plot pred and test######################################################
#########linear graph is displayed with predicted and actual graph#####

###taking log for dependent variable  because i took log in training  dataset

plot(log(test$price), pred1) 



########### ridge regression ############

######## applying ridge regression to add some bias(penalty) and decrease variance hence increases prediction rate ##################

x_train = model.matrix(log(price)~.,train)[,-1]
x_test = model.matrix(log(price)~., test)[,-1]
y_train = log(train$price)
y_test = log(test$price)


#### cross validation to choose smallest lambda value is for lowest MSE
set.seed(7)
cv.out = cv.glmnet(x_train, y_train, alpha = 0) 
bestlam = cv.out$lambda.1se
print(bestlam)# lambda= 0.06530788 is chosen


# the cross validation as lambda increases MSE also increases

plot(cv.out)


# coefficients got closer to zero as they approced desired lambda value
R.out = glmnet(x_train, y_train, alpha = 0)
plot(R.out, xvar = "lambda")
title("Plot of coefficients for different lambda \n for Ridge Model", line=-2.5)


x = model.matrix(price~., data_new)[,-1]
y=data_new$price
out=glmnet(x,y,alpha=0)
predict(out, type = "coefficients", s = bestlam)[1:19,]

ridge_pred = predict(cv.out, s = bestlam, newx = x_test)
ridgemse = mean((ridge_pred-y_test)^2)
ridgemse ####### mse - 0.0635359





##########  regression tree  ############



tree = tree(log(train$price)~., train)
plot(tree)
text(tree, pretty = 0)



tree_pred=predict(tree, test)
mse_tree = mean((tree_pred-log(test$price))^2)
mse_tree # mean square error- 0.0928848 0.09155184


#### cross validation to check if we need to prune a tree
set.seed(7)
pruned = cv.tree(tree)


######### tree with 8 terminal nodes has lowest deviance error rate
plot(pruned, type = "b")

plot(tree)
text(tree, pretty = 0)
title("Predicted Log Price of a House in King County")

############### ensemble technique ###########
####### bagging(bootstrap aggregation) #############

library(MASS)
library(randomForest)

#maxnode 8 is chosen from above tree

bagging = randomForest(log(price) ~., data=train,mtry=ncol(train)-1, maxnodes=8,importance=TRUE)
bagging

bag.mse= mean(bagging$mse)

bag.mse # 0.08615783

plot(bagging, main=("Number of trees vs Error Rate"))



bag.pre=predict(bagging, newdata = test)

ggplot() + 
  geom_point(aes(x = log(test$price), y = bag.pre)) +
  geom_abline(col="red")+
  labs(title = "Plot of Price vs. Bagging Predicted Price") +
  labs(x="Actual Price", y="Bagging Price")

importance(bagging)



hyper_grid<-expand.grid(
  shrinkage = c(.01, .1, .3),
  interaction.depth = c(2, 4, 6),
  n.minobsinnode = c(5, 10, 15), 
  optimal_trees = 0,               
  min_RMSE = 0                     
)



###################### Random forest ####################################
######### it took 30mins to run the model ##########

library(randomForest)
set.seed(9)
oob.err<-double(18)
test.err<-double(18)
for(mtry in 1:18) 
{
  rf=randomForest(log(price) ~ . , data = train, mtry=mtry, ntree=200) 
  oob.err[mtry] = rf$mse[200] #Error of all Trees fitted on training
  pred<-predict(rf,test) #Predictions on Test Set for each Tree
  test.err[mtry]= with(test, mean( (log(price) - pred)^2)) 
}

############# out of bag error rate ############
#proportion of OutOfBag samples that were incorrectly predicted

matplot(1:mtry , cbind(oob.err), pch=20 , main="Out-of-bag Error Rate Per Predictors",
        col=c("red"),type="b",ylab="Mean Squared Error",
        xlab="Number of Predictors Considered at each Split")
legend("topright",legend=c("Out of Bag Error"),pch=19, col=c("red"))


#########by above graph observation it seems mse is low from 8th split so taking that
rf.best=randomForest(log(price) ~ . , data = bag.train, mtry=8, ntree=400)

rf.pre =predict(rf.best,test)
ggplot() + 
  geom_point(aes(x = log(test$price), y = rf.pre)) +
  geom_abline(col = "red")+
  labs(title="Price vs. Random Forest Prediction", x="Price", y="Random Forest estimation of Price")

rf.mse = mean(rf.best$mse)
rf.mse #0.03840182



################### gradient boosting #################################

############# it took 15 mins ################

#install.packages('gbm')
library(gbm)



for(i in 1:nrow(hyper_grid)) {
  
  # reproducibility
  set.seed(7)
  
  # train model
  gbm.tune <- gbm(
    formula = log(price) ~ .,
    distribution = "gaussian",
    data = train,
    n.trees = 2000,
    interaction.depth = hyper_grid$interaction.depth[i],
    shrinkage = hyper_grid$shrinkage[i],
    n.minobsinnode = hyper_grid$n.minobsinnode[i],
    train.fraction = .75, # train only on 0.75% of train subset and evaluate on remaining 0.25%
    n.cores = NULL # will use all cores by default
    #verbose = FALSE
  )
  
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(gbm.tune$valid.error)
  hyper_grid$min_RMSE[i] <- sqrt(min(gbm.tune$valid.error)) 
}

hyper_grid %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)

hyper_grid <- expand.grid(
  shrinkage = c(.01, .05, .1),
  interaction.depth = c(6, 8, 10),
  n.minobsinnode = c(5, 7, 10),
  optimal_trees = 0,               
  min_RMSE = 0                     
)


for(i in 1:nrow(hyper_grid)) {
  
  # reproducibility
  set.seed(7)
  
  # train model
  gbm.tune <- gbm(
    formula = log(price) ~ .,
    distribution = "gaussian",
    data = train,
    n.trees = 2000,
    interaction.depth = hyper_grid$interaction.depth[i],
    shrinkage = hyper_grid$shrinkage[i],
    n.minobsinnode = hyper_grid$n.minobsinnode[i],
    train.fraction = .75, # train only on 0.75% of train subset and evaluate on remaining 0.25%
    n.cores = NULL # will use all cores by default
    #verbose = FALSE
  )
  
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(gbm.tune$valid.error)
  hyper_grid$min_RMSE[i] <- sqrt(min(gbm.tune$valid.error)) 
}

hyper_grid %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)


bestTreeForPrediction = 1216
best.boost <- gbm(log(price)~., data = train,
                  distribution = "gaussian",
                  n.trees=bestTreeForPrediction,
                  interaction.depth=6,
                  shrinkage = 0.05,
                  n.minobsinnode = 5,
                  verbose=F)

yhat.boost <- predict(best.boost, test, n.trees = bestTreeForPrediction)
boost.mse =mean((yhat.boost-log(test$price))^2)
boost.mse #  0.02683182



ggplot() + 
  geom_point(aes(x = log(test$price), y = yhat.boost)) +
  geom_abline(col = "red")+
  labs(title= "Price vs. Boosting Predicted Price", x="Actual Price", y="Boosting Price")

#summary(best.boost)


####################################################  conclusion   ###################################################################

#comparing MSE of Multiple regression,Ridge regression,Tree,Bagging,boosting

#Multiple regression(OLS logit) - 0.06530788
#Ridgeregression -  0.0635359
#Tree- 0.09155184
#Bagging - 0.08647061
#Boosting -  0.02683182

#### Ridge regression performed slighlty better than OLS(logit), Boosting 0.02683182 comes out as best with least MSE 

################################################ future  Enhancenments ##################################################################

"more interaction can be added between feature variables and ncv test was failed so some form of trnasformations can be applied to 
 feature variables and also i believe it will be interesting to see,price categories can be done and apply k-means with zipcode
"
 

