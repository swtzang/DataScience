# Decision Tree with R | Complete Example
# https://www.youtube.com/watch?v=tU3Adlru1Ng
#Read data file
mydata <- read.csv("../DataScience/Cardiotocographic.csv")
str(mydata)
mydata$NSPF <- as.factor(mydata$NSP)

#Decision tree with party
library(party)
mytree <- ctree(NSPF~LB+AC+FM, mydata, controls=ctree_control(mincriterion=0.9, minsplit=50))
print(mytree)
plot(mytree,type="simple")

#Misclassification error
tab<-table(predict(mytree), mydata$NSPF)
print(tab)
1-sum(diag(tab))/sum(tab)
