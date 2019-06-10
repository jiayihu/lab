A = matrix(1:16, 4, 4)
show(A)
A[c(1, 3), c(2, 4)]
A[1:3, 2:4]
A[1:2,]
A[-c(1, 3),]

library(ISLR)
View(Auto)
dim(Auto)
Auto = na.omit(Auto)
dim(Auto)
names(Auto)
attach(Auto) # Make Auto variables available by name
plot(cylinders, mpg, col="red")
pairs(~ mpg + displacement, Auto)
