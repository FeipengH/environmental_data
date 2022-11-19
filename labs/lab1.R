#Lab 1
#Feipeng Huang

c(1, 2, 3)
"c(1, 2, 3)"

#Q1
#c(1, 2, 3) is numeric, "c(1, 2, 3)" is character

c_1 = c(1, 2, 3)
c_2 = "c(1, 2, 3)"

#Q2
#function, c_1 equals to a vector (no quotation marks)

#Q3
#variable, c_2 equals to a character/string (has quotation marks)

#Q4
#c_1 equals to a vector with elements 1, 2, and 3
#c_2 equals to a character called "c(1, 2, 3)"

my_vec = 1:6
mat_1 = matrix(my_vec, nrow = 3)
mat_1

#Q5
#The matrix has 3 rows and 2 columns.

#Q6
mat_1[3,1]

#Q7
mat_2 = matrix(my_vec, nrow = 2, ncol = 3)
mat_2

#Q8
mat_3 = matrix(my_vec, nrow = 3, ncol = 2)
mat_3

#Q9
# R uses columns to recycle/distribute the values.

#Q10
mat_4 = matrix(my_vec, nrow = 2, ncol = 2)
mat_4

#Q11
# R still uses columns to recycle/distribute the values.

first = 5.2
second = "five point two"
third = 0:5
my_list_1 = list(first, second, third)
my_list_1
names(my_list_1) = c("two", "one", "three")
my_list_1

#Q12
my_list_1[[1]] 
#value
#by position
#select the first element of the list
my_list_1[[as.numeric("1")]] 
#value
#by position
#select the first element of the list
my_list_1[["1"]] 
#NULL
#By name
#select the element called "1" in the list
my_list_1[["one"]] 
#value
#by name
#select the element called "one" in the list
my_list_1$one
#value
#by name
#select the element called "one" in the list
my_list_1$"one"
#value
#by name
#select the element called "one" in the list
#my_list_1$1
#error
#by position but missing [[]] so not valid
my_list_1$"1"
#NULL
#by name
#select the element called "1" in the list

#Q13
my_list_1[["one"]] 
my_list_1$one
my_list_1$"one"
#They all select the element called "one", which is "five point two".

#Q14
my_list_1[["1"]] 
my_list_1$"1"
#They both select the element called "1", which does not exist in the list.