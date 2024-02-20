getNumberOfUniqueElements = function(x){
  counter = 1
  v = {}
  for(i in 2:length(x)){
    if(x[i] == x[i-1]){
      counter = counter + 1
    }
    else{
      v = append(v,counter)
      counter = 1
    }
  }
  v = append(v,counter)
  return(v)
}


quan = function(qr,data){
  #(length(data[data[,1] == y,3]) == 0)
  if (data[data[,1] == qr,3] > 5) {
    data[data[,1] == qr,3] = data[data[,1] == qr,3] - 1
    return(list(result = 1, updated_data = data))
  } else if (data[data[,1] == qr,3] <= 5 & data[data[,1] == qr,3] > 0) {
    print(paste("Warning there is ", data[data[,1] == qr,3] ," from item ",data[data[,1] == qr,1], " only in the Warhouse"))
    data[data[,1] == qr,3]= data[data[,1] == qr,3] - 1
    return(list(result = 1, updated_data = data))
  } else if (data[data[,1] == qr,3] <= 0) {
    print(paste("Warhouse is empty from item ",data[data[,1] == qr,1]))
    return(list(result = 0, updated_data = data))
  }
}





# Start of the program
print("Please select the dataframe on which you wish to work.")
name = file.choose()
data = read.csv(name)

datavector = {}
print(nrow(data))
while(TRUE){
  cat("Enter item code from 1 to ", nrow(data),"\n")
  print("If you finished print 0")
  result <- tryCatch(
    {
      qr = opencv::qr_scanner()
    },
    error = function(e) {
      return(NULL)
    }
  )
  if(is.null(qr) | is.null(result)){
    y = as.integer(readline())
    if(is.na(y))
      print("Please enter a number")
    else if(y == 0)
      break
    else if((length(data[data[,1] == y,3]) == 0))
      print("This item is not found within the database items")
    else{
      rr = quan(y,data)
      if(rr$result != 0){
        datavector = append(datavector,y)
        data = rr$updated_data 
      }
    }
  }
  else if(qr == 0)
    break
  else if((length(data[data[,1] == as.integer(qr),3]) == 0))
    print("This item is not found within the database items")
  else{
    rr = quan(qr,data)
    if(rr$result != 0){
      datavector = append(datavector,qr)
      data = rr$updated_data 
    }
  }
}


#another way to make the same idea is using table instead of making a method
# t = table(datavector)
# tdf = as.data.frame(t)
# datavector = tdf[,1]
# quantity = tdf[,2]

if(length(datavector) == 0)
  stop()

if(length(datavector) == 1)
  quantity = 1

if(length(datavector) > 1){
  datavector = datavector[order(datavector)]
  quantity = getNumberOfUniqueElements(datavector)
  datavector = unique(datavector)
}





items = {}
pricevector = {}
equalvector = {}
for(i in 1:length(datavector)){
  # data[data[,1] == qr,3]
  x = data[data[,1] == datavector[i],2] * quantity[i]
  items = append(items,paste(quantity[i] , " X item ", data[data[,1] == datavector[i],1]))
  pricevector = c(pricevector ,x)
}

equalvector = rep(" = ",length(items))
m = matrix( c(items,equalvector,pricevector), nrow = length(datavector))
colnames(m)=c("Items"," ","Price")

rownames(m) = rep(" ",length(items))

numberOfItems = sum(quantity)
meanPrice = mean(pricevector)
# In R, the == operator is used for element-wise equality comparison
# while the %in% operator is used to test whether a set of values is contained in another vector or set.
mini = min(data[data[,1] %in% datavector,2])
maxi = max(data[data[,1] %in% datavector,2])
requiredToPay = sum(pricevector)

m = rbind(m,c("Number of items "," = " , numberOfItems))
m = rbind(m,c("Mean price of all items ", " = ",meanPrice))
m = rbind(m,c("Min item price "," = ",mini))
m = rbind(m,c("Max item price "," = " , maxi))
m = rbind(m,c("Required to pay "," = ",requiredToPay))


write.csv(data, name, row.names = FALSE)

print(m ,quote = FALSE)



