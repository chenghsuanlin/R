# 1.1 R Data Types

# 1. Vectors

# Create a numeric vector
a <- c(1,2,5.3,6,-2,4)
print(a)

a <- 2:9
a

# Refer to elements of a vector using subscripts.
a[c(2,4)]

# Create a character vector
b <- c("one", "two", "three") 
b == "one"

b[b == "one"]


# 2. Matrices

# Create a matrix
M <- matrix(c('a','a','b','c','b','a'), nrow = 2, ncol = 3)
print(M)

# Create a matrix and fill in by row
M <- matrix( c('a','a','b','c','b','a'), nrow = 2, ncol = 3, byrow = TRUE)
print(M)

# Create an array
a <- array(c('green', 'yellow'), dim = c(3, 3, 2))
print(a)

# Matrix Manipulation
A <- matrix(c(1,2,3,4), nrow = 2, ncol = 2)
B <- matrix(c(1,1,2,2), nrow = 2, ncol = 2)
A
B

A * B
A %*% B

t(A)

solve(A)

b <- c(1,1)
solve(A, b)
solve(A) %*% b


# 3. Lists

# Create a list.
list1 <- list(c(2,5,3), 21.3, sin)

# Print the list.
print(list1)

# Create list and asign variable names.
list2  <- list(vector = c(2,5,3),
numeric = 21.3,
func = sin)

# Print names of list and list itself.
names(list2)
print(list2)


# 4. Factros

# Create a vector.
apple_colors <- c('green','green','yellow','red','red','red','green')

# Create a factor object.
factor_apple <- factor(apple_colors)

# Print the factor.
print(factor_apple)
print(nlevels(factor_apple))


# 5. Data Frames

name <- c("David", "Hsi", "Jessie")
age <- c("24", "25", "36")
gender <- c("Male", "Male", "Female")

# Create by variables
data1 <- data.frame(name, age, gender)
data1

data2 <- data.frame(
  name = c("David", "Hsi", "Jessie"),
  age = c("24", "25", "36"),
  gender = c("Male", "Male", "Female")
)
data2

head(data2)
colnames(data2) <- c("Var_1", "Var_2","Var_3")
rownames(data2) <- c("1", "2", "3")
data2
summary(data2)


# HW 1-1
data(iris)
head(iris, 5)

SummarizeData <- function(x){
  output <- data.frame(mean = colMeans(x),
                       var = var(x),
                       max = max(x),
                       min = min(x))
  return(output)
}

SummarizeData(iris[1:4])


# HW 1-2
dist.mat <- matrix(0, nrow = 150, ncol = 150)

for(i in 1:150){
  for(j in 1:150){
    for(k in 1:4){
      dist.mat[i,j] <- sqrt((iris[i, k] - iris[j, k])^2)
    }
  }
}

dist.mat
