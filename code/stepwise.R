stepwise = function(train,test,name,y,crit) {
  train_temp <- train[,..name]
  if(y == 'single_price') {
    model <- lm(data.frame(train$single_price,train_temp))
    colinear <- names(model$coefficients[which(is.na(model$coefficients))])
    #print(typeof(colinear))
    #train_temp <- data.frame(train_temp)
    train_temp <- train_temp[, (colinear) := NULL]
    #print(train_temp)
    regfit.bwd <- regsubsets(y=train$single_price,x=train_temp, method="backward")
  }
  if(y == 'single_sellerprice') {
    model <- lm(data.frame(train$single_sellerprice,train_temp))
    colinear <- names(model$coefficients[which(is.na(model$coefficients))])
    #print(typeof(colinear))
    #train_temp <- data.frame(train_temp)
    train_temp <- train_temp[, (colinear) := NULL]
    #print(train_temp)
    regfit.bwd <- regsubsets(y=train$single_sellerprice,x=train_temp, method="backward")
  }
  if(y == 'single_buyerprice') {
    model <- lm(data.frame(train$single_buyerprice,train_temp))
    colinear <- names(model$coefficients[which(is.na(model$coefficients))])
    #print(typeof(colinear))
    #train_temp <- data.frame(train_temp)
    train_temp <- train_temp[, (colinear) := NULL]
    #print(train_temp)
    regfit.bwd <- regsubsets(y=train$single_buyerprice,x=train_temp, method="backward")
  }
  if(y == 'att') {
    model <- lm(lm(data.frame(train$expatt,train_temp)))
    colinear <- names(model$coefficients[which(is.na(model$coefficients))])
    #print(typeof(colinear))
    #train_temp <- data.frame(train_temp)
    train_temp <- train_temp[, (colinear) := NULL]
    #print(train_temp)
    regfit.bwd <- regsubsets(y=train$expatt,x=train_temp, method="backward")
  }
  summary_bwd <- summary(regfit.bwd)
  if(crit == 'bic'){
    #which.min(summary_bwd$bic)
    print(round(coef(regfit.bwd,which.min(summary_bwd$bic)),10))
    #print(names(coef(regfit.bwd,which.min(summary_bwd$bic))))
    name1 <- names(coef(regfit.bwd,which.min(summary_bwd$bic)))
    name1 <- name1[-1]
    print(name1)
  }
  if(crit == 'adjr2'){
    #which.min(summary_bwd$adjr2)
    print(round(coef(regfit.bwd,which.max(summary_bwd$adjr2)),10))
    #print(names(coef(regfit.bwd,which.min(summary_bwd$adjr2))))
    name1 <- names(coef(regfit.bwd,which.max(summary_bwd$adjr2)))
    name1 <- name1[-1]
    print(name1)
    print(max(summary_bwd$adjr2))
  }
  if(y == 'single_price') {
    best <- lm(data.frame(train$single_price,train[,..name1]))
  }
  if(y == 'single_sellerprice') {
    best <- lm(data.frame(train$single_sellerprice,train[,..name1]))
  }
  if(y == 'single_buyerprice') {
    best <- lm(data.frame(train$single_buyerprice,train[,..name1]))
  }
  if(y == 'att') {
    best <- lm(data.frame(train$expatt,train[,..name1]))
  }
  prediction <- predict(best, data.frame(test[,..name1]))
  #print(test[,..name1])
  
  return(prediction)
}


