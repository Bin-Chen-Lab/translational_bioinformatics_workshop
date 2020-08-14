###
#Data type (factor, numeric, character, class (common classes in bioinformatics))#
###
#Vectors- linear data of single type
#Types: logical, integer, double (numeric), character [+ complex, raw]
num.var = c(1, 2.5, 4.5)
num.var
# L — whole numbers int.var = c(1L, 6L, 10L)
# Logical vector
log.var = c(T, FALSE, TRUE, FALSE) # logical vector
chr.var = c('hese are', "some strings")
chr.var
#[1] "hese are"     "some strings”
#' and " are equivalent
c(1, c(2, c(3, 4)))
[1] 1 2 3 4
#Vector of specific length can be created via functions:
x = integer(10) #contain zeroes
x = character(10) #contain empty characters
#etc...
#If need to create an object with NA:
x = NA

###
#Type conversion in R
###
#When we create data with c() function, we perform type conversion and type will the the elder type (the right one) in row:
#logical, integer, double, character
c(T,'T')
#[1] TRUE TRUE
typeof(c(T,0L))
#[1] "integer"
typeof(c(T,0L,1))
#[1] "double"
c(T,0L,'a')
#[1] "TRUE" "0" "a"
#Sometimes (if we won’t lose data), conversion will be performed automaticaly:
1 + TRUE
#[1] 2
1 & FALSE
#[1] FALSE
1 + '10'
#Error in 1 + "10" : non-numeric argument to binary operator
#For force conversion use as.* functions:
as.numeric(c('1','10.3'))
#[1] 1.0 10.3
as.character(TRUE)
#[1] "TRUE"


#Matrix vs data.frame

#Can store only single type of data
a = matrix (c(1: 4), ncol = 2, nrow = 2)
a=matrix(c(1:3,'a'),ncol=2)
a
#[,1] [,2]
#[1,]    1    3
#[2,]    2    4
b = array (1:12, c (2, 3, 2))
b

#List of vectors: every column can store its own data type
x=data.frame(a=1:3,b=c(T,T,F),c=c('a','b','c'))
y=data.frame(a=1:2,b=c(1,'a'))
#'a=' allows one to set names to columns
#While matrix will look like:
cbind(a=1:3,b=c(T,T,F),c=c('a','b','c'))



# NB! Do not allow same row names, but not columns!
rownames(x) = c('r1','r2','r3')

rownames(x) = c('r1','r2','r1')


#NB! Sometimes the data.frame can be read as factor.
https://stackoverflow.com/questions/23874293/read-table-reads-numbers-as-factors

####
#LISTS
###
#Can contain data of different type and class. Including other lists.
x = list(1:3, "a", c(T, F, T), c(2.3, 5.9),a)
str(x) #

# if user create list with c(), it still will return list, but will perfrom differently from list()
x = list(list(1, 2), c(3, 4))

#A useful way to visualize data is str()
str(x)


###
#l(s)apply
###
#Works like this:
apply(X, MARGIN, FUN, ...)
#where:
#
#X is an array or a matrix if the dimension of the array is 2;
#MARGIN is a variable defining how the function is applied: when MARGIN=1, it applies over rows, whereas with MARGIN=2, it works over columns.
#Note that when you use the construct MARGIN=c(1,2), it applies to both rows and columns; and
#FUN, which is the function that you want to apply to the data. It can be any R function, including a User Defined Function (UDF).

m = matrix(rnorm(100,10+rep(1:10,each=10)),ncol=10)
apply(m, 1, FUN=mean) #apply to rows
apply(m,2,mean) #apply to colummns
apply(m,2,function(x) {exp(mean(log(x)))}) #apply custom function
#Sweep. Lets normalize mean:
#The sweep() function is probably the closest to the apply() family.
#You use it when you want to replicate different actions on the MARGIN elements that you have chosen (limiting here to the matrix case).
#This function expects the following elements:
#an input array, which in this case is a matrix;
#a MARGIN, 2 to indicate the columns;
#a summary statistics (here mean); and
#a function to be applied. You use the arithmetic operator “-” for subtraction.
sweep(m,2,apply(m,2,mean))

#l(s)apply. Apply for lists
#You want to apply a given function to every element of a list and obtain a list as a result. When you execute ?lapply, you see that the syntax looks like the apply() function.
#The difference is that:
#It can be used for other objects like dataframes, lists or vectors; and
#The output returned is a list (which explains the “l” in the function name), which has the same number of elements as the object passed to it.

l = list(a=1:10,b=-1:3,c=1:100)
lapply(l,summary)
#The sapply() function works like lapply(), but it tries to simplify the output to the most elementary data structure that is possible.
#And indeed, sapply() is a ‘wrapper’ function for lapply().
sapply(l,summary)
sapply(l,length)


#Aggregate data
aggregate()
(aggregate(iris[-5],by=list(iris$Species),FUN=mean))


#Make  it to print a return :)
output=matrix()
for(i in unique(iris$Species)){
  x=subset(iris,Species==i)
  print(c(i,(apply(x[1:4],2,mean))))
}

#Merge
merge(data1,data2,by.x,by.y,all=T)
iris$Plant=paste0('Plant',row.names(iris))
iris1=iris[c(1:2,5:6)]
iris2=iris[c(3:4,6)]
merged_iris=merge(iris1,iris2,by='Plant')
#By function
mydata <- iris
by(mydata$Sepal.Length,list(mydata$Species),mean)


###
#What’s the point of knowing this?
###

x=matrix(1:100000000,ncol=10000)
time=Sys.time()
mean_vector_loop=vector()
for(i in 1:nrow(x)){ mean_vector_loop=
  c(mean_vector_loop,mean(x[i,]))
}
Sys.time()-time
#Time difference of 3.223895 secs


time=Sys.time()
mean_vector_apply=apply(x,1,FUN=mean)
Sys.time()-time
#Time difference of 2.534737 secs


#apply for data conversion
df <- data.frame(lapply(df, as.character), stringsAsFactors=FALSE)

###
#Collapse/Summarize data (dplyr)
###
#dplyr have several useful functions:
#mutate() adds new variables that are functions of existing variables
#select() picks variables based on their names.
#filter() picks cases based on their values.
#summarise() reduces multiple values down to a single summary.
#arrange() changes the ordering of the rows.

dim(starwars)
#> [1] 87 14
starwars


#Filter data!
starwars[starwars$skin_color=='light'&starwars$eye_color=='brown',]
starwars %>% filter(skin_color == "light", eye_color == "brown")
#Arrange data!
starwars %>% arrange(height, mass)
#Select data!
starwars %>% select(hair_color, skin_color, eye_color)
subset(starwars,select=c('hair_color', 'skin_color', 'eye_color'))
#Change data! Besides selecting sets of existing columns, it’s often useful to add new columns that are functions of existing columns. This is the job of mutate():
starwars %>% mutate(height_m = height / 100)
#Summarize the dataset`
starwars %>% summarise(height = mean(height, na.rm = TRUE))
#Order data`
starwars %>% arrange(height, mass)
