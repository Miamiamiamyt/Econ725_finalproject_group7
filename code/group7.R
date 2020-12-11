setwd("/Users/miamiamia/Desktop/Econ725_finalproject_group7")
library(haven)
library(data.table)
library(lubridate)
library(h2o)
library(stringr)
library(leaps)
library(glmnet)
library(e1071)
library(ggplot2)
#install.packages("e1071")
#install.packages("corrplot")
#library(corrplot)


# 1.load the data and change into data.table
section <- read_dta("stubhub_crosssection.dta")
section1 <- data.table(section)

panel <- read_dta("stubhub_panel.dta")
panel1 <- data.table(panel)


#panel[newgameid=='248',.(section,ssection,row,srow)][1:10]
#section[,.(gamedatenum,year,month,day)][200:300]
#panel[newgameid=='248',.(gamedatenum,year,month,day,hournum)][1:100]
#panel[newgameid=='248',.(numrec,rec,homerecord,awayrecord,gamehomerecord,gameawayrecord)][1:100]


#a little test of how to convert the date.

#dmy(17267)
#as_date(15257)
#lubridate::month(as_date(17257, origin = "1960-01-01"))
#lubridate::month(as.Date("2007-01-01"),label = F)



#after comparing the R code and the attendence.do code in supplements, I found the origin date is 1960-01-01

# 2.convert the numerical date gamedatenum into year month and day
#time of year
section1$year <- lubridate::year(as_date(section1$gamedatenum,origin = "1960-01-01"))
#month of year
section1$month <- lubridate::month(as_date(section1$gamedatenum,origin = "1960-01-01"))
#day of month
section1$day <- lubridate::wday(as_date(section1$gamedatenum,origin = "1960-01-01"))

section11 <- section1[ ,.(newgameid,home,away,gamedatenum,year,month,day,section,pricenum,number,aisle,parking,seastickonly,multirow,homediv,homeleague,homerecord,homegameback,homegametogo,homewildgameback,homegameahead,homewildgameahead,homepriortofirstgame,awaydiv,awayleague,awayrecord,awaygameback,awaygametogo,awaywildgameback,awaygameahead,awaywildgameahead,awaypriortofirstgame,att,dtg,buyerpricenum,sellerpricenum)]

#time of year
panel1$year <- lubridate::year(as_date(panel1$gamedatenum,origin = "1960-01-01"))
#month of year
panel1$month <- lubridate::month(as_date(panel1$gamedatenum,origin = "1960-01-01"))
#day of month
panel1$day <- lubridate::wday(as_date(panel1$gamedatenum,origin = "1960-01-01"))
#time of day -> hournum

# 5.get the single price for each observation
panel1$number <- strtoi(panel1$number)
panel1$single_price <- (panel1$pricenum)/(panel1$number)
panel1$single_sellerprice <- (panel1$sellerpricenum)/(panel1$number)
panel1$single_buyerprice <- (panel1$buyerpricenum)/(panel1$number)

section11$number <- strtoi(section11$number)
section11$single_price <- (section11$pricenum)/(section11$number)
section11$single_sellerprice <- (section11$sellerpricenum)/(section11$number)
section11$single_buyerprice <- (section11$buyerpricenum)/(section11$number)


# 3.select variable 
source("select_variable.R")
add <- c("ticketid","newgameid","home","away","section","expatt","single_price","single_sellerprice","single_buyerprice")
#panel11 <- panel1[ ,.(newgameid,home,away,gamedatenum,year,month,day,hournum,section,single_price,number,aisle,parking,seastickonly,multirow,homediv,homeleague,homerecord,homegameback,homegametogo,homewildgameback,homegameahead,homewildgameahead,homepriortofirstgame,awaydiv,awayleague,awayrecord,awaygameback,awaygametogo,awaywildgameback,awaygameahead,awaywildgameahead,awaypriortofirstgame,att,expatt,dtg,single_buyerprice,single_sellerprice)]
name <- select_variable(panel1,'price',50)
#name <- select_variable(panel1,'attendance',50)
fullname <- append(name,add)
print(fullname)
panel11 <- panel1[,..fullname]


# 4.generate dummies for section variable
source("generate_dummy.R")
panel11 <- panel11[!(s1==0 & s2==0 & s3==0 & s4==0 & s5==0 & s6==0 & s7==0 & s8==0 & s9==0 & s10==0 & s11==0 & s12==0),]
panel11 <- na.omit(panel11)
source("generate_team.R")
panel11 <- generate_team(panel11)
#save(panel11,file = "panel11price.RData")
#save(panel11,file = "panel11att.RData")

#sapply(panel11, is.list)
write_dta(data = panel11,path = "panel11price.dta")
#write_dta(data = panel11,path = "panel11att.dta")

source("generate_dummy1.R")
section11 <- section11[!(s1==0 & s2==0 & s3==0 & s4==0 & s5==0 & s6==0 & s7==0 & s8==0 & s9==0 & s10==0 & s11==0 & s12==0),]
section11 <- na.omit(section11)
save(section11,file = "section11.RData")


add1 <- c("s1","s2","s3","s4","s5","s6","s7","s8","s9","s10","s11","s12","Ari","Atl","Bal","Bos","ChC","Cin","Cle","Col","CWS","Det","Flo","Hou",
         "Kan","LAA","LAD","Mil","Min","NYM","NYY","Oak","Phi","Pit","SDP","Sea","SFG","StL","Tam","Tex","Tor","Was")
name <- append(name,add1)

# 6.get train and test set
train <- data.table()
test <- data.table()

for (i in unique(panel11$newgameid)){
  n <- nrow(panel11[newgameid == i,])
  print(n)
  sample <- sample.int(n, size = 0.5*n, replace = F)
  train <- rbind(train,panel11[newgameid == i,][sample, ])
  test  <- rbind(test,panel11[newgameid == i,][-sample, ])
}



#save(test,file = "testatt.RData")
#save(train,file = "trainatt.RData")

#save(test,file = "testpr.RData")
#save(train,file = "trainpr.RData")


train1 <- data.table()
test1 <- data.table()

for (i in unique(section11$newgameid)){
  n <- nrow(section11[newgameid == i,])
  print(n)
  sample <- sample.int(n, size = 0.5*n, replace = F)
  train1 <- rbind(train1,section11[newgameid == i,][sample, ])
  test1  <- rbind(test1,section11[newgameid == i,][-sample, ])
}

save(test1,file = "test1.RData")
save(train1,file = "train1.RData")

# 7.poly,backward,ridge, lasso and random forest
#####################################################
#polynomial model
#covar <- c("month","day","hournum","homerecord","awayrecord","s1","s2","s3","s4","s5","s6","s7","s8","s9","s10","s11","s12")
#polyvar_train <- poly(as.matrix(train[,..covar]),degree = 2,raw = T)
#poly cannot handle this large number of variables.
polyvar_train <- train[,.(month,day,hournum,homerecord,awayrecord,s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,
                          I(month^2),I(day^2),I(hournum^2),I(homerecord^2),I(awayrecord^2),I(s1^2),I(s2^2),I(s3^2),I(s4^2),I(s5^2),I(s6^2),I(s7^2),I(s8^2),I(s9^2),I(s10^2),I(s11^2),I(s12^2),
                          month*day,month*hournum,month*homerecord,month*awayrecord,month*s1,month*s2,month*s3,month*s4,month*s5,month*s6,month*s7,month*s8,month*s9,month*s10,month*s11,month*s12,
                          day*hournum,day*homerecord,day*awayrecord,day*s1,day*s2,day*s3,day*s4,day*s5,day*s6,day*s7,day*s8,day*s9,day*s10,day*s11,day*s12,
                          hournum*homerecord,hournum*awayrecord,hournum*s1,hournum*s2,hournum*s3,hournum*s4,hournum*s5,hournum*s6,hournum*s7,hournum*s8,hournum*s9,hournum*s10,hournum*s11,hournum*s12,
                          homerecord*awayrecord,homerecord*s1,homerecord*s2,homerecord*s3,homerecord*s4,homerecord*s5,homerecord*s6,homerecord*s7,homerecord*s8,homerecord*s9,homerecord*s10,homerecord*s11,homerecord*s12,
                          awayrecord*s1,awayrecord*s2,awayrecord*s3,awayrecord*s4,awayrecord*s5,awayrecord*s6,awayrecord*s7,awayrecord*s8,awayrecord*s9,awayrecord*s10,awayrecord*s11,awayrecord*s12,
                          s1*s2,s1*s3,s1*s4,s1*s5,s1*s6,s1*s7,s1*s8,s1*s9,s1*s10,s1*s11,s1*s12,
                          s2*s3,s2*s4,s2*s5,s2*s6,s2*s7,s2*s8,s2*s9,s2*s10,s2*s11,s2*s12,
                          s3*s4,s3*s5,s3*s6,s3*s7,s3*s8,s3*s9,s3*s10,s3*s11,s3*s12,
                          s4*s5,s4*s6,s4*s7,s4*s8,s4*s9,s4*s10,s4*s11,s4*s12,
                          s5*s6,s5*s7,s5*s8,s5*s9,s5*s10,s5*s11,s5*s12,
                          s6*s7,s6*s8,s6*s9,s6*s10,s6*s11,s6*s12,
                          s7*s8,s7*s9,s7*s10,s7*s11,s7*s12,
                          s8*s9,s8*s10,s8*s11,s8*s12,
                          s9*s10,s9*s11,s9*s12,
                          s10*s11,s10*s12,
                          s11*s12)]

polyvar_test <- test[,.(month,day,hournum,homerecord,awayrecord,s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,
                          I(month^2),I(day^2),I(hournum^2),I(homerecord^2),I(awayrecord^2),I(s1^2),I(s2^2),I(s3^2),I(s4^2),I(s5^2),I(s6^2),I(s7^2),I(s8^2),I(s9^2),I(s10^2),I(s11^2),I(s12^2),
                          month*day,month*hournum,month*homerecord,month*awayrecord,month*s1,month*s2,month*s3,month*s4,month*s5,month*s6,month*s7,month*s8,month*s9,month*s10,month*s11,month*s12,
                          day*hournum,day*homerecord,day*awayrecord,day*s1,day*s2,day*s3,day*s4,day*s5,day*s6,day*s7,day*s8,day*s9,day*s10,day*s11,day*s12,
                          hournum*homerecord,hournum*awayrecord,hournum*s1,hournum*s2,hournum*s3,hournum*s4,hournum*s5,hournum*s6,hournum*s7,hournum*s8,hournum*s9,hournum*s10,hournum*s11,hournum*s12,
                          homerecord*awayrecord,homerecord*s1,homerecord*s2,homerecord*s3,homerecord*s4,homerecord*s5,homerecord*s6,homerecord*s7,homerecord*s8,homerecord*s9,homerecord*s10,homerecord*s11,homerecord*s12,
                          awayrecord*s1,awayrecord*s2,awayrecord*s3,awayrecord*s4,awayrecord*s5,awayrecord*s6,awayrecord*s7,awayrecord*s8,awayrecord*s9,awayrecord*s10,awayrecord*s11,awayrecord*s12,
                          s1*s2,s1*s3,s1*s4,s1*s5,s1*s6,s1*s7,s1*s8,s1*s9,s1*s10,s1*s11,s1*s12,
                          s2*s3,s2*s4,s2*s5,s2*s6,s2*s7,s2*s8,s2*s9,s2*s10,s2*s11,s2*s12,
                          s3*s4,s3*s5,s3*s6,s3*s7,s3*s8,s3*s9,s3*s10,s3*s11,s3*s12,
                          s4*s5,s4*s6,s4*s7,s4*s8,s4*s9,s4*s10,s4*s11,s4*s12,
                          s5*s6,s5*s7,s5*s8,s5*s9,s5*s10,s5*s11,s5*s12,
                          s6*s7,s6*s8,s6*s9,s6*s10,s6*s11,s6*s12,
                          s7*s8,s7*s9,s7*s10,s7*s11,s7*s12,
                          s8*s9,s8*s10,s8*s11,s8*s12,
                          s9*s10,s9*s11,s9*s12,
                          s10*s11,s10*s12,
                          s11*s12)]

polylm_pr <- lm(data.frame(train$single_price,polyvar_train))
polymse_pr <- mean((predict(polylm_pr,polyvar_test)-test$single_price)^2)
polynmse_pr <- mean((predict(polylm_pr,polyvar_test)-test$single_price)^2)/mean((test$single_price-mean(test$single_price))^2)

polylm_at <- lm(data.frame(train$expatt,polyvar_train))
polymse_at <- mean((predict(polylm_at,polyvar_test)-test$expatt)^2)
polynmse_at <- mean((predict(polylm_at,polyvar_test)-test$expatt)^2)/mean((test$expatt-mean(test$expatt))^2)

###########################################################################
#backward stepwise selection
source("stepwise.R")
#prediction1_spbic <- stepwise(train1,test1,'single_price','adjr2')
#bwdmse1_spbic <- mean((test1$single_price-prediction1_spbic)^2)
#bwdnmse1_spbic <- mean((test1$single_price-prediction1_spbic)^2)/mean((test1$single_price-mean(test1$single_price))^2)

#temp_name <- as.character(colnames(train_home))
#temp_name <- temp_name[! temp_name %in% c("ticketid","newgameid","home","away","section","expatt","single_price","single_sellerprice","single_buyerprice")]
#prediction_milpr <- stepwise(train_home,test_home,temp_name,'single_price','adjr2')
#bwdmse_milpr <- mean((test_home$single_price-prediction_milpr)^2)
#bwdnmse_milpr <- mean((test_home$single_price-prediction_milpr)^2)/mean((test_home$single_price-mean(test_home$single_price))^2)

#prediction_milat <- stepwise(train_home,test_home,temp_name,'att','adjr2')
#bwdmse_milat <- mean((test_home$expatt-prediction_milat)^2)
#bwdnmse_milat <- mean((test_home$expatt-prediction_milat)^2)/mean((test_home$expatt-mean(test_home$expatt))^2)


#name <- c("gamedatenum","year","month","day","hournum","number","aisle","parking","seastickonly","multirow","homediv","homeleague","homerecord","homegameback","homegametogo","homewildgameback","homegameahead","homewildgameahead","homepriortofirstgame","awaydiv","awayleague","awayrecord","awaygameback","awaygametogo","awaywildgameback","awaygameahead","awaywildgameahead","awaypriortofirstgame","dtg")

prediction_spbic <- stepwise(train,test,name,'single_price','adjr2')
bwdmse_spbic <- mean((test$single_price-prediction_spbic)^2)
bwdnmse_spbic <- mean((test$single_price-prediction_spbic)^2)/mean((test$single_price-mean(test$single_price))^2)

#prediction1_sepbic <- stepwise(train1,test1,'single_sellerprice','bic')
#bwdmse1_sepbic <- mean((test1$single_sellerprice-prediction1_sepbic)^2)
#bwdnmse1_sepbic <- mean((test1$single_sellerprice-prediction1_sepbic)^2)/mean((test1$single_sellerprice-mean(test1$single_sellerprice))^2)

prediction_sepbic <- stepwise(train,test,name,'single_sellerprice','adjr2')
bwdmse_sepbic <- mean((test$single_sellerprice-prediction_sepbic)^2)
bwdnmse_sepbic <- mean((test$single_sellerprice-prediction_sepbic)^2)/mean((test$single_sellerprice-mean(test$single_sellerprice))^2)

#prediction1_bpbic <- stepwise(train1,test1,'single_buyerprice','bic')
#bwdmse1_bpbic <- mean((test1$single_buyerprice-prediction1_bpbic)^2)
#bwdnmse1_bpbic <- mean((test1$single_buyerprice-prediction1_bpbic)^2)/mean((test1$single_buyerprice-mean(test1$single_buyerprice))^2)

prediction_bpbic <- stepwise(train,test,name,'single_buyerprice','adjr2')
bwdmse_bpbic <- mean((test$single_buyerprice-prediction_bpbic)^2)
bwdnmse_bpbic <- mean((test$single_buyerprice-prediction_bpbic)^2)/mean((test$single_buyerprice-mean(test$single_buyerprice))^2)

#prediction1_attbic <- stepwise(train1,test1,'att','bic')
#bwdmse1_attbic <- mean((test1$expatt-prediction1_attbic)^2)
#bwdnmse1_attbic <- mean((test1$expatt-prediction1_attbic)^2)/mean((test1$expatt-mean(test1$expatt))^2)

prediction_attbic <- stepwise(train,test,name,'att','adjr2')
bwdmse_attbic <- mean((test$expatt-prediction_attbic)^2)
bwdnmse_attbic <- mean((test$expatt-prediction_attbic)^2)/mean((test$expatt-mean(test$expatt))^2)
###########################################################################
#ridge and lasso with 10 folds cv
source("cv_loop.R")

predpr_ridge <- cv_loop('single_price',train,test,name,10,0)
msepr_ridge <- mean((test$single_price-predpr_ridge)^2)
nmsepr_ridge <- mean((test$single_price-predpr_ridge)^2)/mean((test$single_price-mean(test$single_price))^2)

predatt_ridge <- cv_loop('att',train,test,name,10,0)
mseatt_ridge <- mean((test$expatt-predatt_ridge)^2)
nmseatt_ridge <- mean((test$expatt-predatt_ridge)^2)/mean((test$expatt-mean(test$expatt))^2)


predpr_lasso <- cv_loop('single_price',train,test,name,10,1)
msepr_lasso <- mean((test$single_price-predpr_lasso)^2)
nmsepr_lasso <- mean((test$single_price-predpr_lasso)^2)/mean((test$single_price-mean(test$single_price))^2)

predatt_lasso <- cv_loop('att',train,test,name,10,1)
mseatt_lasso <- mean((test$expatt-predatt_lasso)^2)
nmseatt_lasso <- mean((test$expatt-predatt_lasso)^2)/mean((test$expatt-mean(test$expatt))^2)


#predmilpr_ridge <- cv_loop('single_price',train_home,test_home,temp_name,10,0)
#msemilpr_ridge <- mean((test_home$single_price-predmilpr_ridge)^2)
#nmsemilpr_ridge <- mean((test_home$single_price-predmilpr_ridge)^2)/mean((test_home$single_price-mean(test_home$single_price))^2)

#predmilat_ridge <- cv_loop('att',train_home,test_home,temp_name,10,0)
#msemilat_ridge <- mean((test_home$expatt-predmilat_ridge)^2)
#nmsemilat_ridge <- mean((test_home$expatt-predmilat_ridge)^2)/mean((test_home$expatt-mean(test_home$expatt))^2)


#predmilpr_lasso <- cv_loop('single_price',train_home,test_home,temp_name,10,1)
#msemilpr_lasso <- mean((test_home$single_price-predmilpr_lasso)^2)
#nmsemilpr_lasso <- mean((test_home$single_price-predmilpr_lasso)^2)/mean((test_home$single_price-mean(test_home$single_price))^2)

#predmilat_lasso <- cv_loop('att',train_home,test_home,temp_name,10,1)
#msemilat_lasso <- mean((test_home$expatt-predmilat_lasso)^2)
#nmsemilat_lasso <- mean((test_home$expatt-predmilat_lasso)^2)/mean((test_home$expatt-mean(test_home$expatt))^2)

##########################################################################


#8.svm
#########################################################################
source("svm.R")
#see the svm.R file,and choose the characteristics you want to analyse
#########################################################################

#9.strategy recommendation
#########################################################################
source("strategy_recommandation.R")
#since the code here is too long, please see the strategy_recommandation.R file
#########################################################################