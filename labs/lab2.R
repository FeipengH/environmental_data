#Run the following code to create a large vector containing randomly generated integers between 1 and 12:
n = 12345
vec_1 = sample(12, n, replace = TRUE)
head(vec_1)
#Use a logical test operator to create a Boolean vector (called vec_2) whose entries are TRUE if the corresponding entry in vec_1 is 3 and FALSE otherwise.
###Q1
vec_2 = c(vec_1 == 3)
#Self test: you can use vec_2 to retrieve all of the 3 elements of vec_1 using the following:
#vec_1[vec_2]
###Q2
# the vector is very large, easy to lose track

n = 12345
vec_1 = sample(12, n, replace = TRUE)
head(vec_1)
length(vec_1)
sum(vec_1 == 3)

n = 10
vec_1 = sample(12, n, replace = TRUE)
paste0("Sum of elements with value 3: ", sum(vec_1 == 3))

###Q3
#The numbers are randomly generated each time.
###Q4
#Number and order of 3 change each time. A logical test always makes the correct selection in a second.
###Q5
#By-hand subsetting is very time-consuming when data sets are large.
#By-hand subsetting has more room for error.
#You do not have any code to reuse or share.

###Q6
for (i in 1:10)
{
  print(paste0("This is loop iteration: ", i))
}

###Q7
n <- 20
for (i in 1:n)
{
  print(i)
}

###Q8
#Create an integer variable, n, that holds the value 17.
n = 17
#Write code to create a vector called vec_1 of length n. vec_1 should contain [pseudo]randomly generated integers between 1 and 10.
vec_1 = sample(10, n, replace = TRUE) ##randomly generated integers between 1 and 10, length n (17)
vec_1
for (i in 1:n) #Iterates n times (once for each element of vec_1)
{
  print(paste0("The element of vec_1 at index ", i, " is ", vec_1[i])) #Prints a message that includes the iteration number as well as the corresponding element of vec_1
}

###Q9
create_and_print_vec = function(n, min = 1, max = 10)
{
  vec = sample(min:max, n, replace = TRUE) 
  for (i in 1:n)  
  {print(paste0("The element at index ", i, " is ", vec[i]))
  }
}
create_and_print_vec(20, min = 1, max = 10)
create_and_print_vec(10, min = 100, max = 2000)