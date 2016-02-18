rm(list = ls())
data(mtcars)
write.csv(mtcars, file = "./mtcars.csv", row.names = F)


##read data into R
read.csv("iris.data")



m = matrix(data=c(1, 0, 2, 3, 3, 3), nrow=2, ncol=3, byrow=TRUE)
layout(m)
layout.show(n =3 )

hist(iris$Sepal.Length)
hist(iris$Sepal.Length, freq = FALSE) #density histogram
hist(iris$Sepal.Length)
