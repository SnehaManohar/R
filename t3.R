#1.Store all dept (9 depts) students names,USN,Dept names, 5 subject grades and SGPA in csv file using ms excel.
#a. Store students marks numerically, transform into grades and store in new dataframe
#b. Check whether students grades are identical or not in each subject
#c. Extract students' marks in each subject separately. If the student has scored greater than 80 map it as "good", if it is between (80 and 60) map it as "moderate", if it is between (40 and 60) map it as "need improvement", else map it as "poor".
#d. Consider dataset given in (1.a) , map the S,A, grades as " GOOD"; map ,B,C grades as "average", D,E grades as "below average"; 'F' grade as "poor".
#e. Tranform dept names to numerical data.
#f. Using factor() and mapvalues() convert dept names to numerical data.
#g. Create table from student data with USN and names only.
#h. Display the typeof each column.
#i. Write separate functions to perform all the above functions separately and call them in R script.
#j. Write a function to perform statistical analysis of students data.
#k. Use sapply to perform 1.c, 1d

s<-read.csv("F:/7th sem/Clas/R/coll.csv")
s$g1 = ""
s$g2 = ""
s$g3 = ""
s$g4 = ""
s$g5 = ""
stud<-s

r<-c(1:14)
col<-c(4:8)
for (i in r)
{
  for (j in col){  
    
    if (stud[i,j] >= 90){
      stud[i,j+6] = 'S'
    }
    else if (stud[i,j] >= 75){
      stud[i,j+6] = 'A'
    }
    else if (stud[i,j] >= 60){
      stud[i,j+6] = 'B'
    }
    else if (stud[i,j] >= 45){
      stud[i,j+6] = 'C'
    }
    else if (stud[i,j] >= 30){
      stud[i,j+6] = 'D'
    }
    else
      stud[i,j+6] = 'E'
  }  
  
}
stud
#################################################
r<-c(1:14)
for (i in r)
{
  x<-stud[i,]
  print(x$Names)
  #print(identical(x$g1,x$g2,x$g3,x$g4,x$g5))
  if(identical(x$g1,x$g2))
    if(identical(x$g2,x$g3))
      if(identical(x$g3,x$g4))
        print(identical(x$g4,x$g5))
          
}
#######################################################

#comments<-mapvalues(stud$M1,from = c(80:100),to = rep("GOOD",21))
#comments<-mapvalues(stud$M1,from = c(60:80),to = rep("MODERATE",21))
#comments<-mapvalues(stud$M1,from = c(40:60),to = rep("GOOD",21))
#comments
r<-c(1:14)
col<-c(4:8)
for (i in r)
{
  x<-c()
  src<-c()
  t1<-c()
  t2<-c()
  t3<-c()
  t4<-c()
  for (j in col){  
    src<-c(src,stud[i,j])
  }
  c1 = 0
  c2 = 0
  c3 = 0
  c4 = 0
  for (j in col){  
    if (stud[i,j] >=80 && stud[i,j]<=100){
      t1<-c(t1,stud[i,j])
      c1 = c1 + 1
    }
    else if (stud[i,j] >=60 && stud[i,j]<80){
      t2<-c(t2,stud[i,j])
      c2 = c2 + 1
    }
    else if (stud[i,j] >=40 && stud[i,j]<60){
      t3<-c(t3,stud[i,j])
      c3 = c3 + 1
    }
    else if (stud[i,j] >=0 && stud[i,j]<40){
      t4<-c(t4,stud[i,j])
      c4 = c4 + 1
    }
  }
  comments<-mapvalues(src,from = t1,to = rep("GOOD",c1))
  print(comments)
  #comments<-mapvalues(src,from = t2,to = rep("MODERATE",c2))
  #print(comments)
  #comments<-mapvalues(src,from = t3,to = rep("NEED IMPR",c3))
  #print(comments)
  #comments<-mapvalues(src,from = t4,to = rep("POOR",c4))
  #print(comments)
}
##########################################################################
library(plyr)
r<-c(1:14)
col<-c(10:14)
for (i in r)
{
  x<-c()
  src<-c()
  t1<-c()
  t2<-c()
  t3<-c()
  t4<-c()
  for (j in col){  
    src<-c(src,stud[i,j])
  }
  c1 = 0
  c2 = 0
  c3 = 0
  c4 = 0
  for (j in col){  
    if (stud[i,j] == "S" || stud[i,j] == "A" ){
      t1<-c(t1,stud[i,j])
      c1 = c1 + 1
    }
    else if (stud[i,j] == "B" || stud[i,j] == "C" ){
      t2<-c(t2,stud[i,j])
      c2 = c2 + 1
    }
    else if (stud[i,j] == "D" || stud[i,j] == "E" ){
      t3<-c(t3,stud[i,j])
      c3 = c3 + 1
    }
    else if (stud[i,j] == "F"){
      t4<-c(t4,stud[i,j])
      c4 = c4 + 1
    }
  }
  comments<-mapvalues(src,from = t1,to = rep("GOOD",c1))
  print(comments)
  #comments<-mapvalues(src,from = t2,to = rep("AVG",c2))
  #print(comments)
  #comments<-mapvalues(src,from = t3,to = rep("B AVG",c3))
  #print(comments)
  #comments<-mapvalues(src,from = t4,to = rep("POOR",c4))
  #print(comments)
}
################################################################
#SAME AS NEXT QUESTION,SIMPLER WITH MAP AND FACTOR
s<-read.csv("F:/7th sem/Clas/R/coll.csv")
s$DNo. = ""
stud<-s

r<-c(1:14)
for (i in r)
{
    if (stud[i,]$Dept == "CS"){
      stud[i,]$DNo. = '1'
    } 
  if (stud[i,]$Dept == "IS"){
    stud[i,]$DNo. = '2'
  } 
  if (stud[i,]$Dept == "EE"){
    stud[i,]$DNo. = '3'
  } 
  if (stud[i,]$Dept == "EC"){
    stud[i,]$DNo. = '4'
  } 
  if (stud[i,]$Dept == "ML"){
    stud[i,]$DNo. = '5'
  } 
  if (stud[i,]$Dept == "BT"){
    stud[i,]$DNo. = '6'
  } 
  if (stud[i,]$Dept == "CV"){
    stud[i,]$DNo. = '7'
  } 
  if (stud[i,]$Dept == "IT"){
    stud[i,]$DNo. = '8'
  } 
  if (stud[i,]$Dept == "SI"){
    stud[i,]$DNo. = '9'
  } 
}
stud
######################################################
s<-read.csv("F:/7th sem/Clas/R/coll.csv")
s <- transform(s, DeptNo = as.factor(mapvalues(Dept,c("CS","IS","EE","EC","ML","BT","CV","IT","SI"),c(1,2,3,4,5,6,7,8,9))))
s
####################################################
cc<-rep('NULL',9)
cc[c(1,2)]<-'character'
df<-read.csv("F:/7th sem/Clas/R/coll.csv",colClasses = cc)
df
####################################################
s<-read.csv("F:/7th sem/Clas/R/coll.csv")
col<-c(1:9)
for (i in col)
{
  print(s[1,i])
  print(typeof(s[1,i]))
}
###for some reason everything is tored as integer, check it out
##see test1q2.rmd
#####################################################
choose.cols<-function(l,h){
  cc<-rep('NULL',9)
  cc[c(l:h)]<-'character'
  df<-read.csv("F:/7th sem/Clas/R/coll.csv",colClasses = cc)
  df
}
choose.cols(1,5)
####################################################################
s<-read.csv("F:/7th sem/Clas/R/coll.csv")
plot(s$Names,s$M2,type="h")
#########################################

