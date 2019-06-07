#Bootstrap
#Q1
library(dslabs)
library(caret)
data("mnist_27")

set.seed(1995)
indexes <- createResample(mnist_27$train$y, 10)
a1 <- table(indexes[1])
a1[names(a1) ==3]
a1[names(a1) ==4]
a1[names(a1) ==7]

#answers are 1, 4, 0
#or alternatively
as.data.frame(table(indexes[1]))
#or alternatively
sum(indexes$Resample01==3)
sum(indexes$Resample01==4)
sum(indexes$Resample01==7)

#Q2

b <- table(indexes)
              
a1 <- table(indexes[1])
a<- a1[names(a1) ==3]

a1 <- table(indexes[2])
a<- a+ a1[names(a1) ==3]

a1 <- table(indexes[3])
a<- a+ a1[names(a1) ==3]

a1 <- table(indexes[6])
a<- a+ a1[names(a1) ==3]

a1 <- table(indexes[8])
a<- a+ a1[names(a1) ==3]

a1 <- table(indexes[9])
a<- a+ a1[names(a1) ==3]
a
######################################
#book ansswer 11
x<- sapply(indexes, function(ind){
  sum(ind == 3)
})
sum(x)
######################################

#Q3
B<- 10^5
N<-100
seed <- 1
Ms<- replicate(B,{
  y <- rnorm(N, 0, 1)
  M<- quantile(y, 0.75)
})
mean(Ms)
sd(Ms)
#answers are expected value =0.6643748 standard error = 0.1351783
######################################
#Book answer
set.seed(1)
B <- 10000
q_75 <- replicate(B, {
  y <- rnorm(100, 0, 1)
  quantile(y, 0.75)
})

mean(q_75)
sd(q_75)
######################################

#Q4
set.seed(1)
y <- rnorm(100, 0, 1)
set.seed(1)
#ind10 <- createResample(y, 10, list = FALSE) 
#df10 <- as.data.frame(ind10)
#df10 <- df10 %>% mutate(y = [ind10]y)

#mstar<- sapply(ind10, function(ind){
#  quantile(ind, 0.75)
#})
#mean(mstar)
#sd(mstar)

B <- 10
M_star <- replicate(B, {
  X_star <- sample(y, 100, replace = TRUE)
  quantile(X_star, 0.75)
})
mean(M_star)
sd(M_star)
######################################
#Solution from Book
set.seed(1)
y <- rnorm(100, 0, 1)
set.seed(1)
indexes <- createResample(y, 10)
q_75_star <- sapply(indexes, function(ind){
  y_star <- y[ind]
  quantile(y_star, 0.75)
})
mean(q_75_star)
sd(q_75_star)
######################################
#Q5
set.seed(1)
y <- rnorm(100, 0, 1)
B <- 10000
M_star <- replicate(B, {
  X_star <- sample(y, 100, replace = TRUE)
  quantile(X_star, 0.75)
})
mean(M_star)
sd(M_star)
######################################
# book answer
set.seed(1)
y <- rnorm(100, 0, 1)
indexes <- createResample(y, 10000)
q_75_star <- sapply(indexes, function(ind){
  y_star <- y[ind]
  quantile(y_star, 0.75)
})
mean(q_75_star)
sd(q_75_star)
######################################
#Q6
#SDs roughly the same
