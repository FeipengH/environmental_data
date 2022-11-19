#DataCamp: Intro to R
#Feipeng Huang

#Variables
a = "Feipeng"
b1 = 45.6
b2 = "45.6"
c1 = 0:3
#b1 + b2
b1 + c1
#Q1. character
#Q2. numeric
#Q3. character
#Q4. Adding b1 and b2 returns error message "non-numeric argument to binary operator" because b1 and b2 are two types of data.
#Q5. Yes, they are both numerics.
#Q6. b1 and c1 are both numerics. b1 has 1 element and c1 has 4 elements. When adding them together, b1 adds to each element in c1, giving "45.6 46.6 47.6 48.6".

#Vectors
#Q7
v1 = c(-2:2)
v1
#Q8
v2 = c(v1*3)
v2
#Q9
sum(v2)

#Matrices
vec_4 = c(1:12)
#Q10
mat_1 = matrix(vec_4, byrow = TRUE, nrow = 3, ncol = 4)
mat_1
#Q11
mat_2 = matrix(vec_4, byrow = FALSE, nrow = 3, ncol = 4)
mat_2

#Lists
#Q12
third = c(0:5)
my_list_1 = list(5.2, "five point two", third)
names(my_list_1) = c("two", "one", "three")
my_list_1
#Q13
my_list_1[[3]]
#Q14
my_list_1[["one"]]

#Logical Tests and Subsetting
#Q15
my_vec = c(rep(1:3, 5))
my_vec
my_bool_vec = my_vec == 3
my_bool_vec
data.frame(my_vec, my_bool_vec)
#Q16
my_vec[my_bool_vec]
