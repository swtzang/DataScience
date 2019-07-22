# iris data set 
# https://rpubs.com/pranaugi011089/98288
library(MASS) #Load package 'MASS' to perform LDA
fit.LDA = lda( Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, iris)
fit.LDA

#Perform classification
fit.LDA.C = predict(fit.LDA, newdata=iris[,c(1,2,3,4)])$class
fit.LDA.C

table(iris[,5],fit.LDA.C)
