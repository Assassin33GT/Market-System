
print("Please select the database on which you wish to work.")
data = read.csv(file.choose())

item = c()
inp1 = c()

plot(qr_code(toString(0)))
plot(qr_code(toString(1)))
plot(qr_code(toString(2)))
plot(qr_code(toString(3)))
plot(qr_code(toString(4)))
plot(qr_code(toString(5)))
plot(qr_code(toString(6)))

while(TRUE)
{
  cat("Enter the item code from 1 to",dim(data)[1],"\n")
  cat("If you finished Press 0")
    inp1 = opencv::qr_scanner()
    #inp1 = readline()
    inp1 = as.numeric(inp1)
    if(inp1>0 && inp1<=dim(data)[1] || inp1 == 6 && inp1<=dim(data)[1]|| inp1 == 7&& inp1<=dim(data)[1]|| inp1 == 8&& inp1<=dim(data)[1]|| inp1 == 9&& inp1<=dim(data)[1])
    {
      if(data[inp1,3] > 0){
      item = append(item,inp1)
      data[inp1,3] = data[inp1,3]-1
      }else{
        print("Out Of Stock \n")
      }
    }else if(inp1 == 0){
      break
    }else if(inp1 > dim(data)[1]){
      cat("Incorrect number \n")
    }
}
if(is.null(item))
{
  cat("No items found")
}else{
item = sort(item)
count = c()
c = 0
ind = c(0)
for(i in 1:length(item))
{
  for(j in i:length(item))
  {
    if(item[i] == item[j])
    {
      if(i==1)
        c = c + 1
      else if(i >= 1 & item[i]!=item[i-1]){
         c = c + 1}
      else{
      break}
         
    }
  }
  if(c != 0)
  count = append(count,c)
  
  c = 0
}

item = unique(item)
write.csv(data,file.choose())
sum = 0
price = c()
cat("                Price   Supply \n")
for(i in 1:length(item))
{
  cat(count[i],"x item ",item[i],"     ",(data[item[i],2]*count[i]),"   ",data[item[i],3],"\n")
  sum = sum + (data[item[i],2]*count[i])
  price = append(price,data[item[i],2])
}
cat("Number of items            ",sum(count),"\n")
cat("Mean price of all items    ",sum/sum(count),"\n")
cat("Min item price             ",min(price),"\n")
cat("Max item price             ",max(price),"\n")
cat("Required to pay            ",sum,"\n")
}
