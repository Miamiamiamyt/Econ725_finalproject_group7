#1.generate types of price
generate_types = function(data){
  data$pricetype <- 0
  data[single_price<=10,]$pricetype <- 1
  data[single_price>10 & single_price<=20,]$pricetype <- 2
  data[single_price>20 & single_price<=30,]$pricetype <- 3
  data[single_price>30 & single_price<=50,]$pricetype <- 4
  data[single_price>50]$pricetype <- 5
  return(data)
}
n <- nrow(panel11)
print(n)
sample <- sample.int(n, size = 0.01*n, replace = F)
train_svm <- panel11[sample, ]

train_svm <- generate_types(train_svm)


#characteristics <- c("homerecord","awayrecord","day","hournum","s1","s2","s3","s4","s5","s6","s7","s8","s9","s10","s11","s12"，"Ari","Atl","Bal","Bos","ChC","Cin","Cle","Col","CWS","Det","Flo","Hou",
#"Kan","LAA","LAD","Mil","Min","NYM","NYY","Oak","Phi","Pit","SDP","Sea","SFG","StL","Tam","Tex","Tor","Was")
characteristics <- c("Ari","Atl","Bal","Bos","ChC","Cin","Cle","Col","CWS","Det","Flo","Hou","Kan","LAA","LAD","Mil","Min","NYM","NYY","Oak","Phi","Pit","SDP","Sea","SFG","StL","Tam","Tex","Tor","Was")

model <- svm(y = train_svm$pricetype,x = train_svm[,..characteristics],type = "C-classification",scale = T,kernel = 'radial')
summary(model)

#homerecord
plot(x=train_svm$homerecord,y=train_svm$homerecord,
     col = as.integer(train_svm$pricetype),
     pch = c("o"))

#awayrecord
plot(x=train_svm$awayrecord,y=train_svm$awayrecord,
     col = as.integer(train_svm$pricetype),
     pch = c("o"))

#day
plot(x=train_svm$day,y=train_svm$day,
     col = as.integer(train_svm$pricetype),
     pch = c("o"))

#hournum
plot(x=train_svm$hournum,y=train_svm$hournum,
     col = as.integer(train_svm$pricetype),
     pch = c("o"))

#section
plot(x=train_svm$s1,y=train_svm$s12,
     col = as.integer(train_svm$pricetype),
     pch = c("o"))

#team
plot(x=train_svm$Bos,y=train_svm$NYY,
     col = as.integer(train_svm$pricetype),
     pch = c("o"))

#cmdscale(dist(train_svm[,..characteristics]))