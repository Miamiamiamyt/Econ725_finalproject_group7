cv_loop = function(y,train,test,name,foldsnum,a){
  if(y == 'single_price'){
    cv.out <- cv.glmnet(as.matrix(train[,..name]),train$single_price, type.measure = "mse",nfolds = foldsnum,alpha=a)
    bestlam <- cv.out$lambda.min
    best <- glmnet(as.matrix(train[,..name]),train$single_price,alpha=a,lambda=bestlam)
  }
  if(y == 'single_sellerprice'){
    cv.out <- cv.glmnet(as.matrix(train[,..name]),train$single_sellerprice, type.measure = "mse",nfolds = foldsnum,alpha=a)
    bestlam <- cv.out$lambda.min
    best <- glmnet(as.matrix(train[,..name]),train$single_sellerprice,alpha=a,lambda=bestlam)
  }
  if(y == 'single_buyerprice'){
    cv.out <- cv.glmnet(as.matrix(train[,..name]),train$single_buyerprice, type.measure = "mse",nfolds = foldsnum,alpha=a)
    bestlam <- cv.out$lambda.min
    best <- glmnet(as.matrix(train[,..name]),train$single_buyerprice,alpha=a,lambda=bestlam)
  }
  if(y == 'att'){
    #print("aaa")
    cv.out <- cv.glmnet(as.matrix(train[,..name]),train$expatt, type.measure = "mse",nfolds = foldsnum,alpha=a)
    bestlam <- cv.out$lambda.min
    best <- glmnet(as.matrix(train[,..name]),train$expatt,alpha=a,lambda=bestlam)
  }
  prediction <- predict(best,as.matrix(test[,..name]))
  return(prediction)
}