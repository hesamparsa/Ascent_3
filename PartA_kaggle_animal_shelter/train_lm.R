#install.packages("doParallel")  #I could not install this
train_lm <- function(DF, error_est = "none", model = "rpart", 
                     y_ind, x_ind, par = FALSE, form.f = " "){
  library(caret)
  
  if(par){
  ######### Parallel processing
    require(foreach); require(doParallel)
    cl <- makeCluster(3) #use 3 cores
    registerDoParallel(cl)
  }
  
  names(DF)[y_ind] <- "outcome"
  outcome <- DF[, y_ind]
  x_data <- DF[, x_ind]
  
  set.seed(3456)
  
  
  #### Before creating dummy variables scale and center numeric variables
  #### Otherwise, if done in train function of caret,
  ##dummy var. also will be scaled and centered..
  # Caret preProcess is smart enough to ignore Factors here
  
  preProcValues <- preProcess(x_data, method = c("center", "scale"))
  
  x_data <- predict(preProcValues, x_data)
  
  ############### Create dummmy variables before splitting the data.
  # Otherwise train function will create dummies on the fly. But, in this case, training set will not
  # have the same number of levels for each factor. This will create different number of predictros.
  # The same is true for cross-validation. 
  
  if(form.f != " "){
    dv <- dummyVars( form.f , data = x_data)
  }else{
    dv <- dummyVars( ~ . , data = x_data)  
  }
  
  #dv
  x_data <- data.frame(predict(dv, x_data))
  
  x_data <- x_data %>%
    select(-contains("FALSE"))
  
  DF_total <- data.frame(x_data, outcome)

  ################ remove NA
  
  DF_total <- na.omit(DF_total)

  cat("New DF dimensions \n", dim(DF_total), "\n")
  cat("n/p ratio is \n", dim(DF_total)[1]/dim(DF_total)[2], "\n")
  
    
  ############### Separate data
  
  trainIndex <- createDataPartition(DF_total$outcome, p = .8, list = F)
  df_train <- DF_total[ trainIndex,]
  df_test  <- DF_total[-trainIndex,]

  fitControl <- trainControl(method = error_est,
                             number = 10,
  #                           classProbs = TRUE, # TRUE for ROC
  #                           summaryFunction = twoClassSummary, ## twoClassSummary this for ROC metric
                             verbose = TRUE)

  lmFit <- train(outcome ~ . , data = df_train,
                 method = model,
  #               metric= "ROC", # "ROC" for AUC
  #               preProc = c("scale"),
                 trControl = fitControl)
  if(par){
    stopCluster(cl)
  }
  
  return(list(model = lmFit, train = df_train, test = df_test))
}

