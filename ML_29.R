#ML Caret package
#library(caret)
#library(dslabs)
#library(dplyr)
#library(purrr)
#library(e1071)
#data(heights)
#y <- heights$sex
#x <- heights$height

#Check confussion matrix
library(dslabs)
library(dplyr)
library(lubridate)
library(caret)

data("reported_heights")

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

w <- table(dat$sex,dat$type)
w
prop.table(w)
prop.table(w,1)
prop.table(w,2)

#Q2 prediction accuracy based only on the type
y_hat <- ifelse(x=="online", "Male", "Female") %>% factor(c("Female", "Male"))
mean(y_hat == dat$sex)

set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_set <- dat[test_index, ]
train_set <- dat[-test_index, ]
head(test_set)
head(train_set)

#Q3 confussion matrix
table(predicted = y_hat, actual = y)

#Q4 sensitivity
confusionMatrix(data = y_hat, reference = y)

#Comprehension Check: Practice with Machine Learning
library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

train %>% group_by(Species) %>% summarize(mean(Sepal.Length), sd(Sepal.Length))
train %>% group_by(Species) %>% summarize(mean(Sepal.Width), sd(Sepal.Width))

train %>% group_by(Species) %>% summarize(mean(Petal.Length), sd(Petal.Length))
train %>% group_by(Species) %>% summarize(mean(Petal.Width), sd(Petal.Width))

cutoff <- seq(1.3, 7, by = 0.01)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") 
  mean(y_hat == train$Species)
})

plot(cutoff,accuracy)
max(accuracy)
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

# from answer
foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1) #range(x) gives min max, range(x)[1] gives min, range(x)[2] max
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==train$Species)
  })
}
predictions <- apply(train[,-5],2,foo) #train[,-5] means excluding column 5 in table, 2 means manipulate on columns
sapply(predictions,max)	
#end of code from answer
range(train$Petal.Length)[1]
range(train$Petal.Length)[2]

# Q3 Using the smart cutoff value calculated on the training data, what is the overall accuracy in the test data?
y_hat <- ifelse(test$Petal.Length > best_cutoff, "virginica", "versicolor")  
mean(y_hat == test$Species)
# from answer
foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1) #range(x) gives min max, range(x)[1] gives min, range(x)[2] max
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==test$Species)
  })
}
predictions <- apply(test[,-5],2,foo) #train[,-5] means excluding column 5 in table, 2 means manipulate on columns
sapply(predictions,max)	
#end of code from answer

#my code & test$Petal.Length > x
cutoff <- seq(1.3, 7, by = 0.01)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(test$Petal.Width > x , "virginica", "versicolor") 
  mean(y_hat == test$Species)
})
plot(cutoff,accuracy)
max(accuracy)
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff


#Q5 What is the overall accuracy for the test data now?
#########################################################################
cutoff <- seq(1.3, 7, by = 0.01)
accuracy <- map_dbl(cutoff, function(x){
  y_hat_l <- ifelse(train$Petal.Length > x , "virginica", "versicolor") 
  mean(y_hat_l == train$Species)
})
plot(cutoff,accuracy)
max(accuracy)
best_cutoff_Petal_L <- cutoff[which.max(accuracy)]
best_cutoff_Petal_L

cutoff <- seq(1.3, 7, by = 0.01)
accuracy <- map_dbl(cutoff, function(x){
  y_hat_w <- ifelse(train$Petal.Width > x , "virginica", "versicolor") 
  mean(y_hat_w == train$Species)
})
plot(cutoff,accuracy)
max(accuracy)
best_cutoff_Petal_W <- cutoff[which.max(accuracy)]
best_cutoff_Petal_W
#######################################################################
y_hat_w <- ifelse(train$Petal.Width > 1.5 , "virginica", "versicolor") 
mean(y_hat_w == train$Species)
y_hat_l <- ifelse(train$Petal.Length > 4.7 , "virginica", "versicolor") 
mean(y_hat_l == train$Species)

y_hat_lw <- ifelse((train$Petal.Width > 1.5 
                    | train$Petal.Length > 4.7) , "virginica", "versicolor")
mean(y_hat_lw == train$Species)
w<- data.frame(y_hat_w, y_hat_l, y_hat_lw,train$Species)
w


y_hat_lw <- ifelse((test$Petal.Width > 1.5 
                    & test$Petal.Length > 4.7) , "virginica", "versicolor")
mean(y_hat_lw == test$Species)

#Code from Answer
library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

plot(iris,pch=21,bg=iris$Species)

set.seed(2)
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

petalLengthRange <- seq(range(train[,3])[1],range(train[,3])[2],by=0.1)
petalWidthRange <- seq(range(train[,4])[1],range(train[,4])[2],by=0.1)
cutoffs <- expand.grid(petalLengthRange,petalWidthRange)

id <- sapply(seq(nrow(cutoffs)),function(i){
  y_hat <- ifelse(train[,3]>cutoffs[i,1] | train[,4]>cutoffs[i,2],'virginica','versicolor')
  mean(y_hat==train$Species)
}) %>% which.max

optimalCutoff <- cutoffs[id,] %>% as.numeric
y_hat <- ifelse(test[,3]>optimalCutoff[1] & test[,4]>optimalCutoff[2],'virginica','versicolor')
mean(y_hat==test$Species)
# end of code from answer

##Conditional probabilities
#Check Review
set.seed(1)
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))
sum(test[disease==0])+sum(test[disease==1])
#Q2 What is the probability that a test is positive?
length(test)
length(test[disease==0])
length(test[disease==1])
plus_test <- sum(test)/length(test)

#Q3 What is the probability that an individual has the disease if the test is negative?
# P(Di|Neg) = P(Neg|Di)*P(Di)/P(Neg)
# P(Neg|Di) = 1 - P(Pos|Di) = 1 - sum(test[disease==1])/sum(disease)

PNegDi <- 1 - sum(test[disease==1])/sum(disease)
PNegDi

PDi <- sum(disease==1)/length(disease) # oR
PDi <- length(test[disease==1])/(length(test[disease==1])+ length(test[disease==0]))
PHe <- 1 - PDi

PNegHe <- (sum(disease==0) -sum(test[disease==0]))/sum(disease==0) # ~90% of healthy patients tested negative
PNegHe

PPosDi <- sum(test[disease==1])/sum(disease==1) # ~85% of diease patients tested positive
PPosDi

PNeg <- PNegHe*(PHe)+(1-PPosDi)*PDi
PNeg

PDiNeg <- (1-PPosDi)*PDi/PNeg
PDiNeg

#Q4 What is the probability that you have the disease if the test is positive?
# P(Di|Pos) = P(Pos|Dis)*P(Dis)/P(Pos)
PPosDi
PDi
PPos <- 1-PNeg
PPos

PDiPos <- PPosDi*PDi/PPos
PDiPos

#5 If the test is positive, what is the relative risk of having the disease?
#First calculate the probability of having the disease given a positive test, 
#then normalize it against the disease prevalence (normalize ~ divide by).
PDiPos/PDi

## Conditional Probabilities Practice
#Q1
library(dplyr)
library(ggplot2)
library(dslabs)
data("heights")
heights %>% 
  mutate(height = round(height)) %>%
  group_by(height) %>%
  summarize(p = mean(sex == "Male")) %>% qplot(height, p, data =.)

#Q2 Which of the following lines of code can be used to replace MISSING CODE to make the correct plot
ps <- seq(0, 1, 0.1)
heights %>% 
  mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)

#Q3 ...estimate the conditional expectations and make a plot
library(MASS)
Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
plot(dat)

ps <- seq(0, 1, 0.1)
dat %>% 
  mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(y = mean(y), x = mean(x)) %>%	
  qplot(x, y, data =.)

## Linear regression for prediction
library(dplyr)
library(HistData)

galton_heights <- GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

library(caret)
y <- galton_heights$son
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

train_set <- galton_heights %>% slice(-test_index)
test_set <- galton_heights %>% slice(test_index)

avg <- mean(train_set$son)
avg

mean((avg - test_set$son)^2) #squared loss

#We also introduced least squares as a method for estimating the slope ß0 and intercept ß1:
fit <- lm(son ~ father, data = train_set)
fit$coef
#We can see that this does indeed provide an improvement over our guessing approach.
y_hat <- fit$coef[1] + fit$coef[2]*test_set$father
mean((y_hat - test_set$son)^2)

##Predict function
y_hat <- predict(fit, test_set)
mean((y_hat - test_set$son)^2)
?predict.lm
?predict.glm
?predict.knn3

## Comprehension check - Linear regression
set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
#start adding my code
#library(caret)
y <-dat$y

set.seed(1)
RMSE  <- replicate(100, { 
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y ~ x, data = train_set)
  y_hat <- predict(fit, test_set)
  sqrt(mean((y_hat - test_set$y)^2))
}
)
mean(RMSE)
sd(RMSE)

#Q2 Repeat several times
n <- c(100, 500, 1000, 5000, 10000)
set.seed(1)
resu <- sapply(n, function(k){
  n <- k
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n , c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  y <-dat$y
  
  RMSE  <- replicate(100, { 
    test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    fit <- lm(y ~ x, data = train_set)
    y_hat <- predict(fit, test_set)
    sqrt(mean((y_hat - test_set$y)^2))
  }
  )
  c(k,mean(RMSE), sd(RMSE))
})
resu

##########################
##########################

library(tidyverse)
library(dslabs)
data("polls_2008")
qplot(day, margin, data = polls_2008)

#29.1 Bin smoothing
span <- 7 
fit <- with(polls_2008, 
            ksmooth(day, margin, x.points = day, kernel="box", bandwidth = span))

polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(day, smooth), color="red")

#29.2 Kernel smoothing
span <- 7
fit <- with(polls_2008, 
            ksmooth(day, margin,  x.points = day, kernel="normal", bandwidth = span))

polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(day, smooth), color="red")

#29.3 Local weighted regression (loess)
total_days <- diff(range(polls_2008$day))
span <- 21/total_days

fit <- loess(margin ~ day, degree=1, span = span, data=polls_2008)

polls_2008 %>% mutate(smooth = fit$fitted) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(day, smooth), color="red")


#29.3.2 Beware of default smoothing parameters
polls_2008 %>% ggplot(aes(day, margin)) +
  geom_point() + 
  geom_smooth()

#But be careful with default parameters as they are rarely optimal. 
#However, you can conveniently change them:
  polls_2008 %>% ggplot(aes(day, margin)) +
  geom_point() + 
  geom_smooth(color="red",  span = 0.15,
              method.args = list(degree=1))
  
  #Q1
  library(tidyverse)
  library(purrr)
  library(pdftools)
  
  fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
  dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
    s <- str_trim(s)
    header_index <- str_which(s, "2015")[1]
    tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
    month <- tmp[1]
    header <- tmp[-1]
    tail_index  <- str_which(s, "Total")
    n <- str_count(s, "\\d+")
    out <- c(1:header_index, which(n==1), which(n>=28), tail_index:length(s))
    s[-out] %>%
      str_remove_all("[^\\d\\s]") %>%
      str_trim() %>%
      str_split_fixed("\\s+", n = 6) %>%
      .[,1:5] %>%
      as_data_frame() %>% 
      setNames(c("day", header)) %>%
      mutate(month = month,
             day = as.numeric(day)) %>%
      gather(year, deaths, -c(day, month)) %>%
      mutate(deaths = as.numeric(deaths))
  }) %>%
    mutate(month = recode(month, "JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6, 
                          "JUL" = 7, "AGO" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
    mutate(date = make_date(year, month, day)) %>%
    filter(date <= "2018-05-01" & !is.na(deaths))
  ############
  total_days <- as.numeric(diff(range(dat$date)))
  span <- 60/total_days
  fit <- loess(deaths ~ as.numeric(date), degree=1, span = span, data=dat)
  
  dat %>% mutate(smooth = fit$fitted) %>%
    ggplot(aes(date, deaths)) +
    geom_point(size = 2, alpha = .2, color = "black") +
    geom_line(aes(date, smooth), color="red")
  ############
  #Q2
  dat %>% 
    mutate(smooth = predict(fit), day = yday(date), year = as.character(year(date))) %>%
    ggplot(aes(day, smooth, col = year)) +
    geom_line(lwd = 2)

#Q3
  library(broom)
  library(dslabs)
  data("mnist_27")
  mnist_27$train %>% glm(y ~ x_2, family = "binomial", data = .) %>% tidy()
  
  qplot(x_2, y, data = mnist_27$train)
  
  ############
  total_days <- as.numeric(diff(range(mnist_27$train$x_2)))
  span <- 0.1/total_days
  fit <- loess( as.numeric(y) ~ x_2, degree=1, span = span, data=mnist_27$train)
  
  mnist_27$train %>% mutate(smooth = fit$fitted) %>%
    ggplot(aes(x_2, as.numeric(y))) +
    geom_point(size = 2, alpha = .2, color = "black") +
    geom_line(aes(x_2, smooth), color="red")
  ############book answer
  mnist_27$train %>% 
    mutate(y = ifelse(y=="7", 1, 0)) %>%
    ggplot(aes(x_2, y)) + 
    geom_smooth(method = "loess")
  
  #### Matrices #####

my_vector <- 1:15
  mat <- matrix(my_vector,5,3)
  mat
mat_t <- matrix(my_vector, 3,5,byrow = TRUE)
mat_t
identical(t(mat),mat_t)
##
library(dslabs)
data("mnist_27")
x<- mnist_27$train$x_2[1:784]
mat_2 <- matrix(x,28,28,byrow = TRUE)

grid <- matrix(mat_2[3,],28,28)
image(1:28,1:28,grid)
#Questions
#Q1 second option x <- matrix(rnorm(100*10), 100, 10) correct
x <- matrix(rnorm(100*10), 100, 10)
#Q2 
xdim(x)
nrow(x)
ncol(x)
#Q3
x1 <- x + seq(nrow(x))
x1 <- sweep(x, 1, 1:nrow(x),"+")
#Q4
x1 <- sweep(x, 2, 1:ncol(x), FUN = "+")
#Q5
rowMeans(x)
colMeans(x)
#Q6
library(tidyverse)
library(dslabs)
mnist <- read_mnist()
#boxplot(rowMeans(mnist$train$images)~mnist$train$labels) 
#pixle_vector <- as.vector(mnist$train$images)
qplot(as.vector(mnist$train$images), bins=30,color=I("black"))
bin_x <- mnist$train$images
class(bin_x)
bin_vector <- as.vector(bin_x)
mean(bin_vector >50 & bin_vector < 205)

#Generative models - nearest neighbor - Distance
library(tidyverse)
library(dslabs)

set.seed(0)
if(!exists("mnist")) mnist <- read_mnist()
ind <- which(mnist$train$labels %in% c(2,7)) %>% sample(500) #sample of 500 labels
x <- mnist$train$images[ind,] # 500 predictors x 784 = 392000 elements 
y <- mnist$train$labels[ind] #500 labels = 500 elements (7 or 2)
y[1:3]
sqrt(sum((x_1-x_2)^2))
sqrt(sum((x_1-x_3)^2))
sqrt(sum((x_3-x_2)^2))
sqrt(crossprod(x_1-x_2)) #the same as above
sqrt(crossprod(x_1-x_3))
sqrt(crossprod(x_3-x_2))

d <- dist(x)
class(d)
as.matrix(d)[1:3,1:3]
image(as.matrix(d))
image(as.matrix(d)[order(y),order(y)])
d<- dist(t(x))
dim(as.matrix(d))

#Q1
library(dslabs)
data("tissue_gene_expression")
#This dataset includes a matrix x:
dim(tissue_gene_expression$x)
#This matrix has the gene expression levels of 500 genes from 189 biological 
#samples representing seven different tissues. The tissue type is stored in y:
table(tissue_gene_expression$y)
d <- dist(tissue_gene_expression$x)
class(d)
#Q2
image(as.matrix(d))
image(as.matrix(d)[1:2,])
image(as.matrix(d)[1:28,])
image(as.matrix(d)[39:40,])
image(as.matrix(d)[73:74,])
#Yes, the samples from the same tissue type are closest to each other. correct
#Q3
image(as.matrix(d))

#30 knn
library(tidyverse)
library(dslabs)
data("mnist_27")
mnist_27$test%>% ggplot(aes(x_1, x_2, color = y)) +  geom_point()

library(caret)
knn_fit <- knn3(y ~ ., data = mnist_27$train)
knn_fit <- knn3(y ~ ., data = mnist_27$train, k = 5)

y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]

#linera regression version
fit_lm <- mnist_27$train %>% mutate(y = ifelse(y == 7, 1, 0)) %>% lm(y ~ x_1 + x_2, data = .)
p_hat_lm <- predict(fit_lm, mnist_27$test)
y_hat_lm <- factor(ifelse(p_hat_lm > 0.5, 7, 2))
confusionMatrix(data = y_hat_lm, reference = mnist_27$test$y)$overall["Accuracy"]

#We will choose better colors and use the stat_contour function to draw a curve that separates pairs  
#(for which x1 x2 pair)
mnist_27$true_p %>% ggplot(aes(x_1, x_2, z = p, fill = p)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D", "white", "#00BFC4")) +
  stat_contour(breaks=c(0.5), color="black")

#Over-training is the reason that we have higher accuracy in the train set compared to the test set
y_hat_knn <- predict(knn_fit, mnist_27$train, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$train$y)$overall["Accuracy"]
y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]

#30.1.1 Overtraining
#Here we fit a kNN model with k=1:
knn_fit_1 <- knn3(y ~ ., data = mnist_27$train, k = 1)
y_hat_knn_1 <- predict(knn_fit_1, mnist_27$train, type = "class")
confusionMatrix(data=y_hat_knn_1, reference=mnist_27$train$y)$overall[["Accuracy"]]
#However, the test set accuracy is actually worse than logistic regression:
y_hat_knn_1 <- predict(knn_fit_1, mnist_27$test, type = "class")
confusionMatrix(data=y_hat_knn_1, reference=mnist_27$test$y)$overall["Accuracy"]

#30.1.3 Picking the k in kNN
ks <- seq(3, 251, 2)

library(purrr)
accuracy <- map_df(ks, function(k){
  fit <- knn3(y ~ ., data = mnist_27$train, k = k)
  
  y_hat <- predict(fit, mnist_27$train, type = "class")
  cm_train <- confusionMatrix(data = y_hat, reference = mnist_27$train$y)
  train_error <- cm_train$overall["Accuracy"]
  
  y_hat <- predict(fit, mnist_27$test, type = "class")
  cm_test <- confusionMatrix(data = y_hat, reference = mnist_27$test$y)
  test_error <- cm_test$overall["Accuracy"]
  
  tibble(train = train_error, test = test_error)
})

ks[which.max(accuracy$test)]
max(accuracy$test)
