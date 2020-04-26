
# Train Models
#install.packages("doParallel")
#library(doParallel)

# Parallel Processing
#cores <- detectCores()
#cl <- makeCluster(cores[1]-4)
#registerDoParallel(cl)

# Test Set and Training Set
#library(e1071)


train.df.tidy <- read.csv("census_income_learn_tidy.csv")
#train.df.tidy <- train.df.scale.c.d2.a

# Run sample because of limited computing power 
# Check distribution of sample against orginal using KS tests 

sample_size = 10000
set.seed(02)
idxs = sample(1:nrow(train.df.tidy),sample_size,replace=F)
subsample = train.df.tidy[idxs,]


check_dist <- function(df1, subsample1) {
  pvalues = list()
  for (col in names(df1)) {
    if (class(df1[,col]) %in% c("numeric","integer")) {
      # Numeric variable. Using Kolmogorov-Smirnov test
      
      pvalues[[col]] = ks.test(subsample1[[col]],df1[[col]])$p.value
      
    } else {
      # Categorical variable. Using Pearson's Chi-square test
      
      probs = table(df1[[col]])/nrow(df1)
      pvalues[[col]] = chisq.test(table(subsample1[[col]]),p=probs)$p.value
      return(pvalues)
    }
  }
}

check_dist(train.df.tidy, subsample)

train.df.tidy.sample <- subsample

#

set.seed(013)
folds <- caret::createDataPartition(train.df.tidy.sample$AGI, p = .75, list = FALSE)

mod.train.df <- train.df.tidy.sample[folds, ]
mod.test.df <- train.df.tidy.sample[-folds, ]

mod.train.df.lab <- mod.train.df$AGI
mod.test.df.lab <- mod.test.df$AGI


remove(c2, d_training, dummies, folds, grid_radial, 
       log_010, subsample, train.df, train.df.c, 
       train.df.scale, train.df.scale.c, train.df.scale.c.d, train.df.scale.c.d1, 
       train.df.scale.c.d2, train.df.scale.c.d2.a, train.df.tidy, 
       train.df.tidy.sample, valid.df)

# 5 Fold Cross Validation
trctrl <- trainControl(method = "cv", number = 3)


# Random Forest
grid_rf <- expand.grid(.mtry = c(7, 13, 26, 33, 76))

rf_01 <- train(AGI~., 
               data=mod.train.df, 
               method='rf', 
               metric='Kappa', 
               trControl=trctrl,
               tuneGrid = grid_rf)

importance_rf <- varImp(rf_01)
plot(importance_rf, 20)

test_pred_rf <- predict(rf_01, newdata = mod.test.df)
confusionMatrix(test_pred_rf, mod.test.df$AGI)

remove(rf_01, test_pred_rf, importance_rf)

# Support Vector
grid_radial <- expand.grid(sigma = c(.01, .05, .1), C = c(0.5, 1, 2)) 

svm_01 <- train(AGI ~., data = mod.train.df, method = "svmRadial",
                trControl=trctrl,
                tuneGrid = grid_radial)

importance_svm <- varImp(svm_01)
plot(importance_svm, 20)

test_pred_svm <- predict(svm_01, newdata = mod.test.df)
confusionMatrix(test_pred_svm, mod.test.df$AGI)

# Log Regression
log_010 <- train(AGI ~., data = mod.train.df, method = "glm",
                 family = "binomial", trControl=trctrl)

importance<- varImp(log_010)
plot(importance, 20)

test_pred_log <- predict(log_010, newdata = mod.test.df)
confusionMatrix(test_pred_log, mod.test.df$AGI)