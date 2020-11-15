# load in necessary libraries
library(boot)
library(MASS)
library(class)
library(glmnet)

# read in data from working directory
list.files()
nums = read.table("NumberSample.txt")
colnames(nums)

# give each variable an appropriate name
# the first column gives the numbers 1 through 10,000
names(nums)[1] = "nums"
# the second column gives the number of primes between the current number and the last power of 10
# and includes the current number if it is prime
names(nums)[2] = "nth"
# the third column gives the order of magnitude of each prime (in powers of 10)
names(nums)[3] = "order"
# the fourth column gives the proportion of numbers that are prime at each order of magnitude
names(nums)[4] = "prop"
# the fifth column gives the gap between the current number and the last prime number
names(nums)[5] = "gap"
# the sixth column gives each number modulo 6
names(nums)[6] = "mod6"
# the seventh column gives each number modulo 2 (0 means a number is even, 1 means it is odd)
names(nums)[7] = "mod2"
# the eightth column tells us whether the number is prime (Yes or No)
names(nums)[8] = "prime"
colnames(nums)

# begin forming predictive models

# try Logistic Regression
kfold.glm = function(k)
{
	errorSum = 0
	kfoldTable = NULL
	for (i in 1:k)
	{
		set.seed(i)
		train = sample(10000,10000-(10000/k))
		glm.fit = glm(prime~.,data=nums,family=binomial,subset=train)
		glm.probs = predict(glm.fit,type="response")
		glm.pred = rep("N",10000)
		glm.pred[glm.probs>.5]="Y"
		if (i == 1) kfoldTable = table(glm.pred[-train],nums$prime[-train])
		else 
		{
			tempTable = table(glm.pred[-train],nums$prime[-train])
			if (!is.na(tempTable[1])) kfoldTable[1] = kfoldTable[1] + tempTable[1]
			if (!is.na(tempTable[2])) kfoldTable[2] = kfoldTable[2] + tempTable[2]
			if (!is.na(tempTable[3])) kfoldTable[3] = kfoldTable[3] + tempTable[3]
			if (!is.na(tempTable[4])) kfoldTable[4] = kfoldTable[4] + tempTable[4]
		}
		errorSum = errorSum + mean((glm.pred!=nums$prime)[-train])
	}
	print(kfoldTable)
	return (errorSum/k)
}
kfold.glm(10)

# try LDA
kfold.lda = function(k)
{
	errorSum = 0
	kfoldTable = NULL
	for (i in 1:k)
	{
		set.seed(i)
		train = sample(10000,10000-(10000/k))
		lda.fit = lda(prime~.,data=nums,subset=train)
		lda.pred = predict(lda.fit,nums[-train,])$class
		if (i == 1) kfoldTable = table(lda.pred,nums$prime[-train])
		else 
		{
			tempTable = table(lda.pred,nums$prime[-train])
			if (!is.na(tempTable[1])) kfoldTable[1] = kfoldTable[1] + tempTable[1]
			if (!is.na(tempTable[2])) kfoldTable[2] = kfoldTable[2] + tempTable[2]
			if (!is.na(tempTable[3])) kfoldTable[3] = kfoldTable[3] + tempTable[3]
			if (!is.na(tempTable[4])) kfoldTable[4] = kfoldTable[4] + tempTable[4]
		}
		errorSum = errorSum + mean(lda.pred!=nums$prime[-train])
	}
	print(kfoldTable)
	return (errorSum/k)
}
kfold.lda(10)

# try QDA
kfold.qda = function(k)
{
	errorSum = 0
	kfoldTable = NULL
	for (i in 1:k)
	{
		set.seed(i)
		train = sample(10000,10000-(10000/k))
		qda.fit = qda(prime~.,data=nums,subset=train)
		qda.pred = predict(qda.fit,nums[-train,])$class
		if (i == 1) kfoldTable = table(qda.pred,nums$prime[-train])
		else 
		{
			tempTable = table(qda.pred,nums$prime[-train])
			if (!is.na(tempTable[1])) kfoldTable[1] = kfoldTable[1] + tempTable[1]
			if (!is.na(tempTable[2])) kfoldTable[2] = kfoldTable[2] + tempTable[2]
			if (!is.na(tempTable[3])) kfoldTable[3] = kfoldTable[3] + tempTable[3]
			if (!is.na(tempTable[4])) kfoldTable[4] = kfoldTable[4] + tempTable[4]
		}
		errorSum = errorSum + mean(qda.pred!=nums$prime[-train])
	}
	print(kfoldTable)
	return (errorSum/k)
}
kfold.qda(23)
# kfold.qda does not work for k < 23
# otherwise I get: Error in qda.default(x, grouping, ...) : rank deficiency in group Y

# try KNN
kfold.knn = function(kf,kn)
{
	errorSum = 0
	kfoldTable = NULL
	for (i in 1:kf)
	{
		set.seed(i)
		train = sample(10000,10000-(10000/kf))
		train.X = cbind(nums[,-8])[train,]
		test.X = cbind(nums[,-8])[-train,]
		train.prime = nums$prime[train]
		knn.pred = knn(train.X,test.X,train.prime,k=kn)
		if (i == 1) kfoldTable = table(knn.pred,nums$prime[-train])
		else 
		{
			tempTable = table(knn.pred,nums$prime[-train])
			if (!is.na(tempTable[1])) kfoldTable[1] = kfoldTable[1] + tempTable[1]
			if (!is.na(tempTable[2])) kfoldTable[2] = kfoldTable[2] + tempTable[2]
			if (!is.na(tempTable[3])) kfoldTable[3] = kfoldTable[3] + tempTable[3]
			if (!is.na(tempTable[4])) kfoldTable[4] = kfoldTable[4] + tempTable[4]
		}
		errorSum = errorSum + mean(knn.pred!=nums$prime[-train])
	}
	print(kfoldTable)
	return (errorSum/kf)
}
kfold.knn(10,1)
kfold.knn(10,3)
kfold.knn(10,5)
kfold.knn(10,10)

# try Ridge Regression
kfold.ridge = function(k)
{
	errorSum = 0
	kfoldTable = NULL
	for (i in 1:k)
	{
		set.seed(i)
		train = sample(10000,10000-(10000/k))
		x = model.matrix(prime~.,nums)[,-8]
		y = nums$prime
		x.train = x[train,]
		x.test = x[-train,]
		y.train = y[train]
		y.test = y[-train]
		ridge.cv = cv.glmnet(x.train,y.train,alpha=0,family="binomial")
		bestlam = ridge.cv$lambda.min
		ridge.mod = glmnet(x.train,y.train,alpha=0,family="binomial",lambda=bestlam)
		ridge.prob = predict(ridge.mod,type="response",s=bestlam,newx=x.test,newy=y.test)
		ridge.pred = rep("N",10000)
		ridge.pred[ridge.prob>.5]="Y"
		if (i == 1) kfoldTable = table(ridge.pred[-train],nums$prime[-train])
		else 
		{
			tempTable = table(ridge.pred[-train],nums$prime[-train])
			if (!is.na(tempTable[1])) kfoldTable[1] = kfoldTable[1] + tempTable[1]
			if (!is.na(tempTable[2])) kfoldTable[2] = kfoldTable[2] + tempTable[2]
			if (!is.na(tempTable[3])) kfoldTable[3] = kfoldTable[3] + tempTable[3]
			if (!is.na(tempTable[4])) kfoldTable[4] = kfoldTable[4] + tempTable[4]
		}
		errorSum = errorSum + mean(ridge.pred[-train]!=nums$prime[-train])
	}
	print(kfoldTable)
	return (errorSum/k)
}
kfold.ridge(10)

# try the Lasso
kfold.lasso = function(k)
{
	errorSum = 0
	kfoldTable = NULL
	for (i in 1:k)
	{
		set.seed(i)
		train = sample(10000,10000-(10000/k))
		x = model.matrix(prime~.,nums)[,-8]
		y = nums$prime
		x.train = x[train,]
		x.test = x[-train,]
		y.train = y[train]
		y.test = y[-train]
		lasso.cv = cv.glmnet(x.train,y.train,alpha=1,family="binomial")
		bestlam = lasso.cv$lambda.min
		lasso.mod = glmnet(x.train,y.train,alpha=1,family="binomial",lambda=bestlam)
		lasso.prob = predict(lasso.mod,type="response",s=bestlam,newx=x.test,newy=y.test)
		lasso.pred = rep("N",10000)
		lasso.pred[lasso.prob>.5]="Y"
		if (i == 1) kfoldTable = table(lasso.pred[-train],nums$prime[-train])
		else 
		{
			tempTable = table(lasso.pred[-train],nums$prime[-train])
			if (!is.na(tempTable[1])) kfoldTable[1] = kfoldTable[1] + tempTable[1]
			if (!is.na(tempTable[2])) kfoldTable[2] = kfoldTable[2] + tempTable[2]
			if (!is.na(tempTable[3])) kfoldTable[3] = kfoldTable[3] + tempTable[3]
			if (!is.na(tempTable[4])) kfoldTable[4] = kfoldTable[4] + tempTable[4]
		}
		errorSum = errorSum + mean(lasso.pred[-train]!=nums$prime[-train])
	}
	print(kfoldTable)
	return (errorSum/k)
}
kfold.lasso(10)
