#R practice
#Basic data structure ----
x <- 2 #This is a scalar
#You can make notes throughout your code using "#" at the beginning of the text

#You can call a scalar/vector/dataframe by typing it 
#Trying running a line with "x"

v <- c(83, 29, 10, 34)
w <- c(FALSE, TRUE, TRUE, TRUE)
x <- as.factor(c("grp1", "grp1", "grp2", "grp2")) 
#Note we're overwriting our previous "x" value
y <- c(2, 3, 9, 4)
z <- c("cnt", "trt1", "trt2", "trt3")
#These are all vectors, or strings of scalars. 

#Trying running the (class) funct on each

df <- data.frame(v,w,x,y,z)
#We can combine vectors into dataframes. This is the "classic" format that your 
#data will load in as
#Trying running class

#When trying to call a piece of a dataframe use [row,column]

#Pull column 1

#Pull rows 1-3

#Pull row 4, column 2

#Basic functions----

#Find the sum of the values in y
#To see what arguments are needed for a function, type "?function"

#Find the sum of the values in w

#Find the sum of the values in z
#Sum will not work on character strings, should get an error

#Find the sum of 93, 2, 74

#Find the mean of y

#Find the sum of all numbers in vectors v and y

#Let's explore functions a little more by creating a vector of repeated values.
#We'll use the rep() function. Take a look at the help page.
rep(x=1, times=5)
rep(1, 5)

#This will work, but will not result in the same thing...
rep(5, 1)

#Create a vector that repeats 1,2,3 four times

#File organization----

#When you work in R you want to make sure that you are working in the "right"
#place. To figure out where you are working. Run getwd()
getwd()
#The working directory is: 
#"C:/Users/Meghan/OneDrive/Documents/USU_Research/AshlynMonroeUR"
#The working directory is currently tied to the Rproject, AshlynMonroeUR.Rproj
#If you wanted to set a new working directory use setwd()

#Your working directory determines where your code will be saved and how to call
#your data from a file. Your data is saved in ./USU_Research/AshlynMonroeUR/Data
#Let's look at my file directory and where your code/data are saved!

#Load data ----
expdat <- read.csv("./Data/exp_data.csv", header=TRUE)
#testing git
