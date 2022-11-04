# LAB ASSIGNMENT 1

install.packages("devtools")
devtools::install_github("MansMeg/markmyassignment")
library(markmyassignment)
lab_path<-"https://raw.githubusercontent.com/STIMALiU/AdvRCourse/master/Labs/Tests/lab1.yml"
set_assignment(lab_path)
mark_my_assignment()

name<-"Abhinay Krishna Vellala"
liuid<-"abhve999"


#1.1.1 my numb vector

my_num_vector<-function()
{
  x<-c(log10(11), cos(pi/5), exp(pi/3), (1173%%7)/19)
  return(x)
}
my_num_vector()

#1.1.2 filter my vector

filter_my_vector<-function(x,leq)
{
  x <- ifelse(x>=leq, NA, x)
  
  return(x)
}

filter_my_vector(x=c(1,4,6,8,10), leq=8)

#1.1.3 Creating dot_prod function


dot_prod <- function(a,b)
{
  x <- sum(a*b)
  x
}
dot_prod(a = c(3,1,12,2,4), b = c(1,2,3,4,5))


#1.1.4 Approx 

approx_e <- function(N)
  
{
  e=sum(1/factorial(0:N))
  e
}
approx_e(6)

#1.2.1 My magic matrix

my_magic_matrix <- function()
{
  matrix(c(4,3,8,9,5,1,2,7,6),nrow=3, ncol=3)
}
my_magic_matrix()

#1.2.2 Add Elements

calculate_elements <- function(A)
{
  A = length(A)
  A
}
mat <- my_magic_matrix()
calculate_elements(A=mat)

new_mat <- cbind(mat,mat)
calculate_elements(A=new_mat)

#1.2.3


row_to_zero <- function(A,i)
{
  A[i,] <- 0
  A
}
mat<-my_magic_matrix()
row_to_zero(A=mat,i=3)

# 1.2.4 Add_elements_to_matrix

add_elements_to_matrix <- function(A,x,i,j){
  A[i,j] <- A[i,j]+x
  A
}
mat <- my_magic_matrix()
add_elements_to_matrix(A=mat,x=10,i=2,j=3)

# 1.3.1 my_magic_list

my_magic_list <- function()
{
  p <- "my own list"
  q <- my_num_vector()
  r <- my_magic_matrix()
  l<- list("info"=p,q,r)
  l
}

my_magic_list()


#1.3.2 change_info

change_info<- function(x,text)
{
  x["info"] <- text
  x
}

change_info(x=my_magic_list(), text = "Some new info")


#1.3.3 Add Note

add_note<-function(x,note)
{
  x["note"] <- note
  x
}
a_list=my_magic_list()
add_note(x=a_list, note= "This is a list")

#1.3.4 SUm of numeric parts

sum_numeric_parts<-function(x)
{  n=as.numeric(unlist(x))
n=as.list(n)
for(i in 1:length(n)){
  if(is.na(n[i])){
    n[i]=NULL
  }
}
x_sum = sum(as.numeric(n))
x_sum
}
sum_numeric_parts(x=a_list[2])

#1.4.1 Data Frames

my_data.frame<- function(){
  df <-data.frame("id"=c(1:3),"name"=c("John","Lisa","Azra"),"income"=c(7.30,0.00,15.21),"rich"=c(FALSE,FALSE,TRUE)) 
  return(df)  
}
my_data.frame()

#1.4.2 Sort Head

sort_head <- function(df,var.name,n)
{
  df<-df[order(-df[var.name]),]
  return(head(df,n))
}
data(iris)
sort_head(df=iris,var.name="Petal.Length", n=5)

#1.4.3 Add Median variable

add_median_variable<-function(df,j)
{
  mu<-median(df[,j])
  l<-length(df[,j])
  d<-vector()
  for(i in 1:l)
  {
    if(df[,j][i]< mu){
      d[i]="Smaller"
    }
    else if(df[,j][i]==mu){
      d[i]="Median"
    }
    else{
      d[i]="Greater"
    }
  }
  df$compared_to_median<-d
  return(df)
}


#1.4.4

analyze_columns<-function(df,j)
{
  
  list1<-c("mean"=mean(df[,j[1]]),"median"=median(df[,j[1]]),"sd"=sd(df[,j[1]]))
  list2<-c("mean"=mean(df[,j[2]]),"median"=median(df[,j[2]]),"sd"=sd(df[,j[2]]))
  m<-(cor(df[,j]))
  l<-list(list1,list2,m)
  names(l)<-c(colnames(df[j[1]]),colnames(df[j[2]]),"correlation_matrix")
  return(l)
}
data(faithful)
analyze_columns(df=faithful,1:2)
