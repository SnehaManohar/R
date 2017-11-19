#1.Store all dept (9 depts) students names,USN,Dept names, 5 subject grades and SGPA in csv file using ms excel. 
#a. Extract each dept students names separately.
#b. Extract S grade scores in all subjects in each dept seperately.
#c. Extract students who have scored at least S grades in any 2 subjects
#d. Extract students who have scored above 9 SGPA in each dept

stud<-read.csv("F:/7th sem/Clas/R/coll.csv")
d<-unique(stud$Dept)
for (x in d)
{
  temp<-c()
  print(x)
  temp<-stud$Names[stud$Dept==x]
  print(temp)
}

##
for (x in d)
{
  temp<-c()
  print(x)
  temp<-stud$Names[stud$Dept==x & (stud$M1>60 )]
  temp<-c(temp,stud$Names[stud$Dept==x & (stud$M2>60 )])
  temp<-c(temp,stud$Names[stud$Dept==x & (stud$M3>60 )])
  temp<-c(temp,stud$Names[stud$Dept==x & (stud$M4>60 )])
  temp<-c(temp,stud$Names[stud$Dept==x & (stud$M5>60 )])
  print(length(temp))
}

##
r<-c(1:14)
for (i in r)
  {
 
  x <- stud[i,]
  count = 0
  if (x$M1 > 60)
      count = count +1
  if (x$M2 > 60)
    count = count +1
  if (x$M3 > 60)
    count = count +1
  if (x$M4 > 60)
    count = count +1
  if (x$M5 > 60)
    count = count +1
  if (count >=2)
    print(x$Names)
}

r<-c(1:14)
for (i in r)
{
  x <- stud[i,]
  if(x$SGPA > 8)
    print(x$Names)
}