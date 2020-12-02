setwd("/Users/miamiamia/Desktop/Econ725_finalproject_group7")
library(haven)
library(data.table)
library(lubridate)

# 1.load the data and change into data.table
#section <- read_dta("stubhub_crosssection.dta")
#section <- data.table(section)

panel <- read_dta("stubhub_panel.dta")
panel1 <- data.table(panel)

#panel[newgameid=='248',.(section,ssection,row,srow)][1:10]
#section[,.(gamedatenum,year,month,day)][200:300]
#panel[newgameid=='248',.(gamedatenum,year,month,day,hournum)][1:100]
#panel[newgameid=='248',.(numrec,rec,homerecord,awayrecord,gamehomerecord,gameawayrecord)][1:100]


#a little test of how to convert the date.

#dmy(17267)
#as_date(15257)
#as_date(17257, origin = "1960-01-01")

#after comparing the R code and the attendence.do code in supplements, I found the origin date is 1960-01-01

# 2.convert the numerical date gamedatenum into year month and day
#section$year <- year(as_date(section$gamedatenum,origin = "1960-01-01"))
#section$month <- month(as_date(section$gamedatenum,origin = "1960-01-01"))
#section$day <- day(as_date(section$gamedatenum,origin = "1960-01-01"))

#time of year
panel1$year <- year(as_date(panel$gamedatenum,origin = "1960-01-01"))
#month of year
panel1$month <- month(as_date(panel$gamedatenum,origin = "1960-01-01"))
#day of month
panel1$day <- day(as_date(panel$gamedatenum,origin = "1960-01-01"))
#time of day -> hournum

panel11 <- panel1[ ,.(att,pricenum,buyerpricenum,sellerpricenum,number,home,away,homerecord,awayrecord,gamedatenum,year,month,day,hournum,section,aisle,parking,seastickonly,multirow,dtg)]

# 3.generate dummies for row variable
generate_dummy <- function(data,type) {
  for(i in unique(data$section)){
    if(TRUE %in% grepl(i,type$s1,fixed = TRUE) ) {
      data[section == i,]$s1 <- 1
      print('s1')
    }else if(TRUE %in% grepl(i,type$s2,fixed = TRUE) ) {
      data[section == i,]$s2 <- 1
      print('s2')
    }else if(TRUE %in% grepl(i,type$s3,fixed = TRUE) ) {
      data[section == i,]$s3 <- 1
      print('s3')
    }else if(TRUE %in% grepl(i,type$s4,fixed = TRUE) ) {
      data[section == i,]$s4 <- 1
      print('s4')
    }else if(TRUE %in% grepl(i,type$s5,fixed = TRUE) ) {
      data[section == i,]$s5 <- 1
      print('s5')
    }else if(TRUE %in% grepl(i,type$s6,fixed = TRUE) ) {
      data[section == i,]$s6 <- 1
      print('s6')
    }else if(TRUE %in% grepl(i,type$s7,fixed = TRUE) ) {
      data[section == i,]$s7 <- 1
      print('s7')
    }else if(TRUE %in% grepl(i,type$s8,fixed = TRUE) ) {
      data[section == i,]$s8 <- 1
      print('s8')
    }else if(TRUE %in% grepl(i,type$s9,fixed = TRUE) ) {
      data[section == i,]$s9 <- 1
      print('s9')
    }else if(TRUE %in% grepl(i,type$s10,fixed = TRUE) ) {
      data[section == i,]$s10 <- 1
      print('s10')
    }else if(TRUE %in% grepl(i,type$s11,fixed = TRUE) ) {
      data[section == i,]$s11 <- 1
      print('s11')
    }else if(TRUE %in% grepl(i,type$s12,fixed = TRUE) ) {
      data[section == i,]$s12 <- 1
      print('s12')
    }
  }
  
  return(data)
}

s1 <- c('CLUBHOUSE BOX G','CLUBHOUSE BOX H','CLUBHOUSE BOX I','CLUBHOUSE BOX J','CLUBHOUSE BOX K')
s2 <- c('DUGOUT BOX C','DUGOUT BOX D','DUGOUT BOX E','DUGOUT BOX F','DUGOUT BOX N','DUGOUT BOX O','DUGOUT BOX P','DUGOUT BOX Q','FIRST BASE BOX A','FIRST BASE BOX B','THIRD BASE BOX R','THIRD BASE BOX S')
s3 <- c('INFIELD BOX 115','INFIELD BOX 116','INFIELD BOX 117','INFIELD BOX 118','INFIELD BOX 119','INFIELD BOX 120','INFIELD BOX 121','INFIELD BOX 122','INFIELD BOX 123','INFIELD BOX 124','INFIELD BOX 125','INFIELD BOX 126','INFIELD BOX 127','INFIELD BOX 128','INFIELD BOX 129')
s4 <- c('BASELINE RESERVE 110','BASELINE RESERVE 111','BASELINE RESERVE 112','BASELINE RESERVE 113','BASELINE RESERVE 114','BASELINE RESERVE 130','BASELINE RESERVE 131','BASELINE RESERVE 132','BASELINE RESERVE 133','BASELINE RESERVE 134')
s5 <- c('BULLPEN RESERVE 106','BULLPEN RESERVE 107','BULLPEN RESERVE 108','BULLPEN RESERVE 109','BULLPEN RESERVE 135','BULLPEN RESERVE 136','BULLPEN RESERVE 137','BULLPEN RESERVE 138')
s6 <- c('BLEACHERS 100W','BLEACHERS 101','BLEACHERS 102','BLEACHERS 103','BLEACHERS 104','BLEACHERS 105','BLEACHERS 139','BLEACHERS 140','BLEACHERS 141','BLEACHERS 142','BLEACHERS 143','BLEACHERS 144','BLEACHERS 145W')
s7 <- c('CLUB BOX 204','CLUB BOX 205','CLUB BOX 206','CLUB BOX 207','CLUB BOX 208','CLUB BOX 209','CLUB BOX 210A','CLUB BOX 210B','CLUB BOX 210C','CLUB BOX 210D','CLUB BOX 210E','CLUB BOX 210F','CLUB BOX 210G','CLUB BOX 210H','CLUB BOX 210I','CLUB BOX 211','CLUB BOX 212','CLUB BOX 213','CLUB BOX 214','CLUB BOX 215','CLUB BOX 216')
s8 <- c('CLUB RESERVE 200','CLUB RESERVE 201','CLUB RESERVE 202','CLUB RESERVE 203','CLUB RESERVE 217','CLUB RESERVE 218','CLUB RESERVE 219','CLUB RESERVE 220','CLUB RESERVE 221','CLUB RESERVE 222','CLUB RESERVE 223','CLUB RESERVE 224')
s9 <- c('INFIELD RESERVE 305','INFIELD RESERVE 306','INFIELD RESERVE 307','INFIELD RESERVE 308','INFIELD RESERVE 309','INFIELD RESERVE 310','INFIELD RESERVE 311','INFIELD RESERVE 312','INFIELD RESERVE 313','INFIELD RESERVE 314','INFIELD RESERVE 315','INFIELD RESERVE 316','INFIELD RESERVE 317','INFIELD RESERVE 318','INFIELD RESERVE 319','INFIELD RESERVE 320','INFIELD RESERVE 321','INFIELD RESERVE 322','INFIELD RESERVE 323','INFIELD RESERVE 324','INFIELD RESERVE 325','INFIELD RESERVE 326','INFIELD RESERVE 327')
s10 <- c('OUTFIELD RESERVE 300','OUTFIELD RESERVE 301','OUTFIELD RESERVE 302','OUTFIELD RESERVE 303','OUTFIELD RESERVE 304','OUTFIELD RESERVE 328','OUTFIELD RESERVE 329','OUTFIELD RESERVE 330','OUTFIELD RESERVE 331','OUTFIELD RESERVE 332')
s11 <- c()
s12 <- c('MVP BOX 310','MVP BOX 311','MVP BOX 314','MVP BOX 315','MVP BOX 317','MVP BOX 318','MVP BOX 321','MVP BOX 322')

panel11$s1 <- 0;panel11$s2 <- 0;panel11$s3 <- 0
panel11$s4 <- 0;panel11$s5 <- 0;panel11$s6 <- 0
panel11$s7 <- 0;panel11$s8 <- 0;panel11$s9 <- 0
panel11$s10 <- 0;panel11$s11 <- 0;panel11$s12 <- 0

type_Ari <- list(s1=s1,s2=s2,s3=s3,s4=s4,s5=s5,s6=s6,s7=s7,s8=s8,s9=s9,s10=s10,s11=s11,s12=s12)
panel11[home == 'Ari',] <- generate_dummy(panel11[home == 'Ari',],type_Ari)

#TRUE %in% grepl('BOX 208',c('CLUB BOX 208W','RESERVED 208'),fixed = TRUE) 

print(panel11[home == 'Ari',])








