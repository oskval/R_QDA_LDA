library(caret)
library(MASS)
data = read.csv("HTRU_2.csv", header = FALSE)
head(data)
data$V9=as.factor(data$V9)
head(data)
plot(data$V9)

idx.train = createDataPartition(y = data$V9, p = 0.8,
                                list = FALSE)
train = data[idx.train,]
test = data[-idx.train,]
lda_classifier = lda(formula = V9 ~ ., data = train)
predictions = predict(lda_classifier, test)$class
table(predicted=predictions, true_labels = test$V9)

qda_classifier = qda(formula = V9 ~ ., data = train)
predictions = predict(qda_classifier, test)$class
table(predicted=predictions, true_lables = test$V9)

cols=c("#808000", "800080")
plot(train$V2, train$V6, col=cols[train$V9])
grid()

## Asiigment
train = read.csv("sign_mnist_train.csv")
test = read.csv("sign_mnist_test.csv")
par(mfrow=c(4,4),tcl=-0.5, mai=c(0.1,0.1,0.1,0.1),
    xaxt='n',yaxt='n')
id_letter=which(train$label==12)
for(i in id_letter[1:16]){
  hand_sign=as.matrix(train[i,2:785])
  image(matrix(hand_sign,28,28,byrow=F),col=gray.colors(255))
}

## Antra uzduotis
lda_classifier = lda(formula = label ~ ., data = train)
predicted_labels = predict(lda_classifier, test)$class
mean(test$label==predicted_labels)
table1 = table(predicted=predicted_labels,true_labels=test$label)
diag(table1)/colSums(table1)

# trecia uzduotis
train = train[,c(1,seq(2,785,2))]

lda_classifier = lda(formula = label ~ ., data = train)
predicted_labels = predict(lda_classifier, test)$class
mean(test$label==predicted_labels)
table1 = table(predicted=predicted_labels,true_labels=test$label)
diag(table1)/colSums(table1)

# ketvirta
train = read.csv("sign_mnist_train.csv")
test = read.csv("sign_mnist_test.csv")

qda_classifier = qda(formula = label ~ ., data = train)
predicted_labels = predict(qda_classifier, test)$class
mean(test$label==predicted_labels)
table1 = table(predicted=predicted_labels,true_labels=test$label)
diag(table1)/colSums(table1)


# Penkta
train = train[,c(1,seq(2,785,2))]
qda_classifier = qda(formula = label ~ ., data = train)
predicted_labels = predict(qda_classifier, test)$class
mean(test$label==predicted_labels)
table1 = table(predicted=predicted_labels,true_labels=test$label)
diag(table1)/colSums(table1)

