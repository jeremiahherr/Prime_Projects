# load in necessary libraries
library(tree)
library(randomForest)
library(e1071)

# read in data from working directory
list.files()
# read in and prepare the training data
train = read.table("NumberSample1.txt")
colnames(train)
# give each variable an appropriate name
# the first column gives the numbers 1 through 10,000
names(train)[1] = "nums"
# the second column gives the number of primes between the current number and the last power of 10
# and includes the current number if it is prime
names(train)[2] = "nth"
# the third column gives the order of magnitude of each prime (in powers of 10)
names(train)[3] = "order"
# the fourth column gives the proportion of numbers that are prime at each order of magnitude
names(train)[4] = "prop"
# the fifth column gives the gap between the current number and the last prime number
names(train)[5] = "gap"
# the sixth column gives each number modulo 6
names(train)[6] = "mod6"
# the seventh column gives each number modulo 2 (0 means a number is even, 1 means it is odd)
names(train)[7] = "mod2"
# the eighth column gives the approximate average gap between primes less than or equal to the current number
# using ln(current number), by the Prime Number Theorem 
names(train)[8] = "avgGap"
# the ninth column gives the approximate probability that the current number is prime
# using 1/ln(current number), by the Prime Number Theorem
names(train)[9] = "probPrime"
# the tenth column tells us whether the number is prime (Yes or No)
names(train)[10] = "prime"
colnames(train)

# read in and prepare the testing data
test = read.table("NumberSample2.txt")
colnames(test)
# give each variable an appropriate name
# the first column gives the numbers 1 through 10,000
names(test)[1] = "nums"
# the second column gives the number of primes between the current number and the last power of 10
# and includes the current number if it is prime
names(test)[2] = "nth"
# the third column gives the order of magnitude of each prime (in powers of 10)
names(test)[3] = "order"
# the fourth column gives the proportion of numbers that are prime at each order of magnitude
names(test)[4] = "prop"
# the fifth column gives the gap between the current number and the last prime number
names(test)[5] = "gap"
# the sixth column gives each number modulo 6
names(test)[6] = "mod6"
# the seventh column gives each number modulo 2 (0 means a number is even, 1 means it is odd)
names(test)[7] = "mod2"
# the eighth column gives the approximate average gap between primes less than or equal to the current number
# using ln(current number), by the Prime Number Theorem 
names(test)[8] = "avgGap"
# the ninth column gives the approximate probability that the current number is prime
# using 1/ln(current number), by the Prime Number Theorem
names(test)[9] = "probPrime"
# the tenth column tells us whether the number is prime (Yes or No)
names(test)[10] = "prime"
colnames(test)

# begin forming predictive models
# try a tree model
set.seed(1)
tree.fit = tree(prime~.,data=train)
summary(tree.fit)
tree.pred = predict(tree.fit,data=test,type="class")
table(tree.pred,test$prime)
mean(tree.pred!=test$prime)

tree.cv = cv.tree(tree.fit,FUN=prune.misclass)
tree.cv
prune.tree = prune.misclass(tree.fit,best=4)
prune.pred = predict(prune.tree,data=test,type="class")
table(prune.pred,test$prime)
mean(prune.pred!=test$prime)

# try a bagging model
set.seed(2)
bag.fit = randomForest(prime~.,data=train,mtry=9,importance=TRUE)
bag.pred = predict(bag.fit,newdata=test)
table(bag.pred,test$prime)
mean(bag.pred!=test$prime)

# try a random forest model
set.seed(3)
rf.fit = randomForest(prime~.,data=train,importance=TRUE)
rf.pred = predict(rf.fit,newdata=test)
table(rf.pred,test$prime)
mean(rf.pred!=test$prime)

# try a support vector classifier model
set.seed(4)
svc.fit = tune(svm,prime~.,data=train,kernel="linear")
best1 = svc.fit$best.model
svc.pred = predict(best1,test)
table(svc.pred,test$prime)
mean(svc.pred!=test$prime)

# try a support vector machine model
set.seed(5)
svm1.fit = tune(svm,prime~.,data=train,kernel="radial")
best2 = svm1.fit$best.model
svm1.pred = predict(best2,test)
table(svm1.pred,test$prime)
mean(svm1.pred!=test$prime)

svm2.fit = tune(svm,prime~.,data=train,kernel="polynomial")
best3 = svm2.fit$best.model
svm2.pred = predict(best3,test)
table(svm2.pred,test$prime)
mean(svm2.pred!=test$prime)