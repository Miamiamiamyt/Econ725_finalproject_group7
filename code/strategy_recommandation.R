#get the data for our favarite team: Milwaukee Brewers
remove_covar <- c("Ari","Atl","Bal","Bos","ChC","Cin","Cle","Col","CWS","Det","Flo","Hou",
                  "Kan","LAA","LAD","Mil","Min","NYM","NYY","Oak","Phi","Pit","SDP","Sea","SFG","StL","Tam","Tex","Tor","Was")
home_mil <- panel11[home=='Mil',-..remove_covar]
away_mil <- panel11[away=='Mil',-..remove_covar]
#save(home_mil,file = "home_mil.RData")
#save(away_mil,file = "away_mil.RData")

train_home <- data.table()
test_home <- data.table()
for (i in unique(home_mil$ticketid)){
  n <- nrow(home_mil[ticketid == i,])
  print(n)
  sample <- sample.int(n, size = 0.75*n, replace = F)
  train_home <- rbind(train_home,home_mil[ticketid == i,][sample, ])
  test_home  <- rbind(test_home,home_mil[ticketid == i,][-sample, ])
}

train_away <- data.table()
test_away <- data.table()
for (i in unique(away_mil$ticketid)){
  n <- nrow(away_mil[ticketid == i,])
  print(n)
  sample <- sample.int(n, size = 0.75*n, replace = F)
  train_away <- rbind(train_away,away_mil[ticketid == i,][sample, ])
  test_away  <- rbind(test_away,away_mil[ticketid == i,][-sample, ])
}

#save(test_home,file = "test_home.RData")
#save(train_home,file = "train_home.RData")

#save(test_away,file = "test_away.RData")
#save(train_away,file = "train_away.RData")
home_mil$revenue <- home_mil$single_buyerprice - home_mil$single_sellerprice
away_mil$revenue <- away_mil$single_buyerprice - away_mil$single_sellerprice

train_home$revenue <- train_home$single_buyerprice - train_home$single_sellerprice
test_home$revenue <- test_home$single_buyerprice - test_home$single_sellerprice

train_away$revenue <- train_away$single_buyerprice - train_away$single_sellerprice
test_away$revenue <- test_away$single_buyerprice - test_away$single_sellerprice

#use random forest to select key variables to determine the strategy
###################################################################
random_forest <- function(train_set){
  order <- colnames(train_set)
  order <- order[!order %in% c("ticketid","newgameid","home","away","section","expatt","single_price","single_sellerprice","single_buyerprice","revenue")]
  train <- as.h2o(train_set)
  #test <- as.h2o(test_set)
  rf <- h2o.randomForest(
    x = order,
    #y = "revenue",
    #y = "single_price",
    y = "expatt",
    training_frame = train,
    ntrees = 1000,
    seed = 1234
    #nfolds = 5,
    #max_depth = 5
  )
  return(rf)
}
h2o.init()
#home
rf <- random_forest(train_home)
score <- h2o.varimp(rf)
#test_home <- as.h2o(test_home)
predict_rf <- h2o.predict(rf,test_home)
mse_rf <- mean((predict_rf-test_home$revenue)^2)
print(mse_rf)
nmse_rf <- mean((predict_rf-test_home$revenue)^2)/mean((test_home$revenue-mean(test_home$revenue))^2)
print(nmse_rf)
#h2o.download_mojo(rf, getwd(), FALSE)
#predictpr_rf <- h2o.predict(rf,test_home)
#msepr_rf <- mean((predictpr_rf-test_home$single_price)^2)
#nmsepr_rf <- mean((predictpr_rf-test_home$single_price)^2)/mean((test_home$single_price-mean(test_home$single_price))^2)

#predictat_rf <- h2o.predict(rf,test_home)
#mseat_rf <- mean((predictat_rf-test_home$expatt)^2)
#nmseat_rf <- mean((predictat_rf-test_home$expatt)^2)/mean((test_home$expatt-mean(test_home$expatt))^2)


#away
rf1 <- random_forest(train_away)
score1 <- h2o.varimp(rf1)
#test_away <- as.h2o(test_away)
predict_rf1 <- h2o.predict(rf1,test_away)
mse_rf1 <- mean((predict_rf1-test_away$revenue)^2)
print(mse_rf1)
nmse_rf1 <- mean((predict_rf1-test_away$revenue)^2)/mean((test_away$revenue-mean(test_away$revenue))^2)
print(nmse_rf1)
#h2o.download_mojo(rf1, getwd(), FALSE)

h2o.shutdown()

#save(score,file = "scorehome.RData")
#save(score1,file = "scoreaway.RData")



###################################################################
keycor <- c("rec", "_Inumb_2", "sumx", "numrec", "ticketidnum", "s9", "s4", "homegametogo", "awaygametogo", "dtg", "s11", "dowgame", "seastickonly", "s3", "day","EBregpnlistdum","homerecord","awayrecord")
home_temp <- home_mil[,..keycor]
home_temp <- scale(home_temp)
set.seed(0)
km4_home <- kmeans(x = home_temp,centers = 4)
#km4_home$cluster
km4_home$centers
km4_home$size
km4_home$totss #total sum of squares
km4_home$withinss #within cluster sum of squares
km4_home$tot.withinss #total within cluster sum of squares
km4_home$betweenss #between cluster sum of squares

keycor1 <- c("rec", "_Inumb_2", "numrec", "sumx", "ticketidnum", "seastickonly", "dtg", "awaygametogo","EBregpnlistdum", "homerecord", "s11", "homegametogo", "homerecordgtg", "s9", "dowgame","day","awayrecord")
away_temp <- away_mil[,..keycor1]
away_temp <- scale(away_temp)
set.seed(0)
km4_away <- kmeans(x = away_temp,centers = 4)
#km4_away$cluster
km4_away$centers
km4_away$size
km4_away$totss #total sum of squares
km4_away$withinss #within cluster sum of squares
km4_away$tot.withinss #total within cluster sum of squares
km4_away$betweenss #between cluster sum of squares
#######################################################################
home_mil <- home_mil[order(-revenue)]
away_mil <- away_mil[order(-revenue)]

print(head(home_mil[,..keycor],20)) #type2
print(head(away_mil[,..keycor1],20)) #type1