# read in data from working directory
list.files()
primes = read.table("SamplePrimes.txt")
colnames(primes)
# give each variable an appropriate name
# the first column gives the collected prime numbers
names(primes)[1] = "primes"
# the second column tells us that each prime is the nth prime at its order of magnitude
names(primes)[2] = "nth"
# the third column gives the order of magnitude of each prime (in powers of 10)
names(primes)[3] = "order"
# the fourth column gives the proportion of numbers that are prime at each order of magnitude
names(primes)[4] = "prop"
colnames(primes)

# begin exploring the data
plot(primes)
dev.new()
lm.fit = lm(primes)
summary(lm.fit)

# examine relationship between primes and order
# and between primes and prop
par(mfrow=c(1,2))
hist(primes$order,col="red",ylab="Number of Primes",xlab="Order of Magnitude")
hist(primes$prop,col="blue",ylab="Number of Primes",xlab="Proportion of Primes")
dev.new()

# examine relationship between prop and order
lm.fit1 = lm(prop~order,data=primes)
summary(lm.fit1)
plot(prop~order,data=primes,col="blue")
abline(lm.fit1,col="red")

lm.fit2 = lm(prop~poly(order,5),data=primes)
summary(lm.fit2)
lines(fitted(lm.fit2)~order,data=primes,col="green")
anova(lm.fit1,lm.fit2)

# examine relationship between primes and nth
# and between primes and order and prop
lm.fit3 = lm(primes~nth,data=primes)
summary(lm.fit3)

dev.new()
plot(primes~nth,data=primes,col="yellow")
abline(lm.fit3,col="red")

lm.fit4 = lm(primes~order+prop,data=primes)
summary(lm.fit4)

anova(lm.fit3,lm.fit4)

lm.fit6 = lm(primes~order*prop,data=primes)
summary(lm.fit6)

anova(lm.fit3,lm.fit6)

lm.fit7 = lm(primes~poly(order*prop,5),data=primes)
summary(lm.fit7)

anova(lm.fit6,lm.fit7)
anova(lm.fit3,lm.fit7)

# examine relationship between nth and order and prop
lm.fit8 = lm(nth~order*prop,data=primes)
summary(lm.fit8)

lm.fit9 = lm(nth~poly(order*prop,5),data=primes)
summary(lm.fit9)

anova(lm.fit8,lm.fit9)