rm(list = ls())
   
##### PART A #####

# read in College.csv and analyze structure
college <- read.csv("College.csv", header = T, stringsAsFactors = T)
str(college)

# remove name of college
college <- college[, -1]

# split data into train and test (80% train, 20% test)
set.seed(1693)
trainIndices <- sample(1:nrow(college), size = 0.8*nrow(college))

train <- college[trainIndices, ]
train.X <- college[trainIndices, -9]
train.Y <- college[trainIndices, 9]

test <- college[-trainIndices, ]
test.X <- college[-trainIndices, -9]
test.Y <- college[-trainIndices, 9]

#forward stepwise feature selection
library(leaps)
models <- regsubsets(Outstate ~ ., train, method = "forward", nvmax = 17)
sumModels <- summary(models)

#pick out model ID with highest adjusted R^2
bestModelID <- which.max(sumModels$adjr2)
bestModelID
sumModels$adjr2[bestModelID] #0.756

#display features and coefficients of best model
coef(models, bestModelID)

##### PART B #####

library(mgcv)

#run gam model
gam.mod <- gam(Outstate ~ Private + s(Apps) + s(Accept) + s(Top10perc) + s(F.Undergrad) + s(Enroll)
           + s(Room.Board) + s(Personal) + s(PhD) + s(Terminal) + s(S.F.Ratio) + s(perc.alumni) + s(Expend) + s(Grad.Rate),
           data = train)
summary(gam.mod) #adjR2 = 0.823

## Top10perc, F.Undergrad, PhD, are very insignificant
## Terminal and S.F.Ratio are almost significant < 0.01 away from being under 0.05

#plot partial features
plot(gam.mod, shade = T, residuals = T, pages = 13)

## Top10perc, F.Undergrad, Room.Board, Terminal, perc.alumni, Personal, and PhD
## look relatively linear

#run new gam model
gam.new <- gam(Outstate ~ Private + s(Apps) + s(Accept) + Top10perc + F.Undergrad + s(Enroll)
               + Room.Board + s(Personal) + PhD + Terminal + S.F.Ratio + perc.alumni + s(Expend) + s(Grad.Rate),
               data = train)
summary(gam.new) #adjR2 = 0.82

## All smoothing terms are significant or almost significant

#plot partial features
plot(gam.new, shade = T, residuals = T, pages = 7, all.terms = T)

## All smoothed features look non-linear

#check model
par(mfrow = c(2, 2))
gam.check(gam.new)

## s(S.F.Ratio) seems to have a degree of significance to its residuals, indicating a pattern
## WHICH WE DON'T WANT

##### PART C #####

#fit test data to original fully-smoothed model 
set.seed(1693)
pred <- predict(gam.mod, se.fit = T, newdata = test.X)

#calculate MSE
mse <- mean((test.Y - pred$fit)^2)
mse #2671467

#calcualte RMSE
rmse <- sqrt(mse)
rmse #1634.46

#calculate MAE
mae <- mean(abs(test.Y - pred$fit))
mae #1253.74

#fit test data to new partially-smoothed model 
set.seed(1693)
pred <- predict(gam.new, se.fit = T, newdata = test.X)

#calculate MSE
mse <- mean((test.Y - pred$fit)^2)
mse #2613465

#calculate RMSE
rmse <- sqrt(mse)
rmse #1616.62

#calculate MAE
mae <- mean(abs(test.Y - pred$fit))
mae #1258.80
