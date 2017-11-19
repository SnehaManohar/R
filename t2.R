#2.Store all dept faculty names with designation and salary details.
#a. extract each dept faculty details separately
#b. extract Professors of each dept seperately
#c. extract people who earn more than 1.5 lakh in each dept where their designation is prof, associate or assistant
#d.Find out the cost of professors in each dept .(sum up their salary to get cost of them)
#e. Find the cost of each dept faculty
#f. Find out the average cost of faculty in each dept.
#g.Which dept has highest average cost of faculty
#i. Which dept has lowest cost of faculty

teac<-read.csv("F:/7th sem/Clas/R/fac.csv")
d<-unique(teac$Dept)
for (x in d)
{
  temp<-c()
  print(paste("DEPARTMENT ",x))
  r<-c(1:9)
  for (i in r)
  {
  y<-teac[i,]
  if(y$Dept==x)
  print(y)
  }
}
############
d<-unique(teac$Dept)
for (x in d)
{
  
  print(x)
  temp<-teac$Name[teac$Dept==x]
  print(temp)
}
##############
d<-unique(teac$Dept)
for (x in d)
{
  
  print(x)
  r<-c(1:9)
  for (i in r)
  {
    y<-teac[i,]
    if(y$Sal>=1.5 & (y$Desig=="Prof"||y$Desig=="Assi"||y$Desig=="Asso") & y$Dept==x)
      print(y$Name)
  }
}
########################
d<-unique(teac$Dept)
for (x in d)
{
  sum = 0
  print(x)
  r<-c(1:9)
  for (i in r)
  {
    y<-teac[i,]
    if(y$Dept==x& y$Desig=="Prof")
      sum= sum+y$Sal
  }
  print(sum)
}
####################
d<-unique(teac$Dept)
for (x in d)
{
  sum = 0
  print(x)
  r<-c(1:9)
  for (i in r)
  {
    y<-teac[i,]
    if(y$Dept==x)
      sum= sum+y$Sal
  }
  print(sum)
}
######################
d<-unique(teac$Dept)
salaries<-c()

for (x in d)
{
  sum = 0
  count = 0
  print(x)
  r<-c(1:9)
  for (i in r)
  {
    y<-teac[i,]
    if(y$Dept==x){
      sum= sum+y$Sal
      count=count+1
    }
  }
  val = sum/count
  salaries<-c(salaries,val)
  print(val)
}
####################
min_val = min(salaries)
print(min_val)
max_val = max(salaries)
d<-unique(teac$Dept)


for (x in d)
{
  sum = 0
  count = 0
  r<-c(1:9)
  for (i in r)
  {
    y<-teac[i,]
    if(y$Dept==x){
      sum= sum+y$Sal
      count=count+1
    }
  }
  val = sum/count
  if(val == min_val){
    print("MIN")
    print(x)
  }
  if(val == max_val){
    print("MAX")
    print(x)
  }
}