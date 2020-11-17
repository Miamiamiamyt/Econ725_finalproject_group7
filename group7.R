setwd("/Users/miamiamia/Desktop/725finalproject")
library(haven)
library(data.table)
library(lubridate)

# 1.load the data and change into data.table
section <- read_dta("stubhub_crosssection.dta")
section <- data.table(section)

panel <- read_dta("stubhub_panel.dta")
panel <- data.table(panel)

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
section$year <- year(as_date(section$gamedatenum,origin = "1960-01-01"))
section$month <- month(as_date(section$gamedatenum,origin = "1960-01-01"))
section$day <- day(as_date(section$gamedatenum,origin = "1960-01-01"))

#time of year
panel$year <- year(as_date(panel$gamedatenum,origin = "1960-01-01"))
#month of year
panel$month <- month(as_date(panel$gamedatenum,origin = "1960-01-01"))
#day of month
panel$day <- day(as_date(panel$gamedatenum,origin = "1960-01-01"))
#time of day -> hournum

# 3.generate dummies for row variable
# adjust the format of row
panel$row <- gsub("\\ \\ ", "\\ ", panel$row)

for(i in unique(panel$row)) {
  #print(i)
  rowname <- paste("row_", i, sep = "")
  print(rowname)
  panel$rowname <- 0
  panel <- panel[row == i, ':='(rowname = 1)]
}





