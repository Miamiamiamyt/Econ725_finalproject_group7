select_variable = function(data,y,numb){
  num_cols <- unlist(lapply(data, is.numeric))
  temp <- data[,..num_cols]
  cor_var <- round(cor(temp),5)
  remove <- c("lnsellerpricenum","lnbuyerpricenum","pricenum","sellerpricenum","buyerpricenum","expatt90un","expatt","single_price","single_sellerprice","single_buyerprice")
  if(y == 'price'){
    name <- names(tail(sort(cor_var["single_price",]),numb))
    #print(as.list(name))
  }
  if(y == 'attendance'){
    name <- names(tail(sort(cor_var["expatt",]),numb))
    #print(as.list(name))
  }
  name <- name[!name %in% remove]
  return(name)
}