library(rpart)
library(MASS)
library(ISLR)
library(caret)
library(rpart.plot)

attach(Carseats)
High=ifelse(Sales<=8, "No", "Yes")
Carseats = data.frame(Carseats, High)
tree.carseats = rpart(High~.-Sales, Carseats)
printcp(tree.carseats)
plotcp(tree.carseats)
summary(tree.carseats)

detach(Carseats)

attach(Boston)
set.seed(1)
idx.train = createDataPartition(y = Boston$medv, p = 0.8,
                                list = FALSE)
train=Boston[idx.train,]
test=Boston[-idx.train,]
cfit=rpart(medv~., train,method="anova",control=rpart.control(cp=0))
printcp(cfit)
summary(cfit)
plotcp(cfit)
a = printcp(cfit)
library(data.table)
a = as.data.frame(a)
a = transpose(a)
smallest = min(a$V4)



cfit_pruned=prune(cfit,cp=0.00281127)
yhat_max=predict(cfit,newdata = test)
yhat_pruned=predict(cfit_pruned,newdata = test)
plot(yhat_max,test$medv,col="blue")
points(yhat_pruned,test$medv,col="red")
c(cor(yhat_max,test$medv)^2, cor(yhat_pruned, test$medv)^2)
rpart.plot(cfit_pruned,fallen.leaves=FALSE,tweak=1.4)

# Uzduotys

#(a)
data = read.csv("music_spotify.csv", strip.white = TRUE, header=TRUE)
data$X <- NULL
data$song_title <- NULL
data$artist <- NULL
idx.train = createDataPartition(y = data$target, p = 0.8,
                                list=FALSE)
train=data[idx.train,]
test=data[-idx.train,]

cfit = rpart(target~., train, method="class", control=rpart.control(cp=0))
plotcp(cfit)
printcp(cfit)
p = cfit$cptable[which.min(cfit$cptable[,"xerror"]),"CP"]

#(b)
cfit_pruned=prune(cfit, cp=p)
rpart.plot(cfit_pruned, fallen.leaves = FALSE, tweak = 1.2,
           extra = 101, type=1, box.palette = "YlGnBl")


#(c)
sum(cfit_pruned$frame$var == "<leaf>")
node = cfit_pruned$frame[1, 'dev']/cfit_pruned$frame[1, 'n']
err = min(cfit_pruned$cptable[,"xerror"]) * node
err
#(d)
#(e)

pred=predict(cfit,newdata=test,type="class")

pred1=predict(cfit_pruned,newdata=test,type="class")

table = table(predicted=pred,true_labels=test$target)
table
mean(test$target==pred)
diag(table)/colSums(table)


table1 = table(predicted=pred1,true_labels=test$target)
table1
mean(test$target==pred1)
diag(table1)/colSums(table1)

#(f)
#(g)

plot(cfit$variable.importance)

#(2)
#(a)

train = read.csv("sign_mnist_train.csv")
test = read.csv("sign_mnist_test.csv")
cfit = rpart(label~., train, method="class", control=rpart.control(cp=0))
printcp(cfit)
plotcp(cfit)
p = cfit$cptable[which.min(cfit$cptable[,"xerror"]),"CP"]

#(b)
cfit_pruned=prune(cfit, cp=p)
sum(cfit_pruned$frame$var == "<leaf>")
fold = cfit$cptable[which.min(cfit$cptable[,"xerror"]),"xerror"]
fold1 = fold * cfit$frame[1, 'dev']/cfit$frame[1, 'n']
fold1
#(c)
pred = predict(cfit, newdata=test, type="class")
pred1 = predict(cfit_pruned, newdata = test, type = "class")

table = table(predicted=pred,true_labels=test$label)
table
mean(test$label==pred)
diag(table)/colSums(table)

table1 = table(predicted=pred1,true_labels=test$label)
table1
mean(test$label==pred1)
diag(table1)/colSums(table1)
#(d)

train = train[,c(1,seq(2,785,2))]
cfit = rpart(label~., train, method="class", control=rpart.control(cp=0))
p = cfit$cptable[which.min(cfit$cptable[,"xerror"]),"CP"]
cfit_pruned=prune(cfit, cp=p)
pred1 = predict(cfit_pruned, newdata = test, type = "class")
table1 = table(predicted=pred1,true_labels=test$label)
table1
mean(test$label==pred1)
diag(table1)/colSums(table1)

