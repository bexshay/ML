#Generative models
library(tidyverse)
library(caret)

library(dslabs)
data("heights")

y <- heights$height
set.seed(1995)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

#In this case, the Naive Bayes approach is particularly appropriate because we know that the normal 
#distribution is a good approximation for the conditional distributions of height given sex for both classes  
# Y=1 (female) and  Y=0 (Male).
#This implies that we can approximate the conditional distributions  
#fX|Y=1 and  fX|Y=0 by simply estimating averages and standard deviations from the data:
params <- train_set %>% group_by(sex) %>% 
summarize(avg = mean(height), sd = sd(height))
params
#he prevalence, which we will denote with  π=Pr(Y=1)
pi <- train_set %>% summarize(pi=mean(sex=="Female")) %>% pull(pi)
pi

#now we can use our estimates of average and standard deviation to get an actual rule
x <- test_set$height

f0 <- dnorm(x, params$avg[2], params$sd[2])
f1 <- dnorm(x, params$avg[1], params$sd[1])

p_hat_bayes <- f1*pi / (f1*pi + f0*(1 - pi))
#32.7.2 Controlling prevalence
y_hat_bayes <- ifelse(p_hat_bayes > 0.5, "Female", "Male")
sensitivity(data = factor(y_hat_bayes), reference = factor(test_set$sex))
specificity(data = factor(y_hat_bayes), reference = factor(test_set$sex))

p_hat_bayes_unbiased <- f1 * 0.5 / (f1 * 0.5 + f0 * (1 - 0.5)) 
y_hat_bayes_unbiased <- ifelse(p_hat_bayes_unbiased> 0.5, "Female", "Male")

sensitivity(data = factor(y_hat_bayes_unbiased), reference = factor(test_set$sex))
specificity(data = factor(y_hat_bayes_unbiased), reference = factor(test_set$sex))

qplot(x, p_hat_bayes_unbiased, geom = "line") + 
  geom_hline(yintercept = 0.5, lty = 5) + 
  geom_vline(xintercept = 67, lty = 2)

# 32.7.3 Quadratic Discriminant Analysis (qda) and 32.7.4 Linear discriminant analysis (lda)
# the case: the 2 or 7 example
data("mnist_27")
params <- mnist_27$train %>% 
  group_by(y) %>% 
  summarize(avg_1 = mean(x_1), avg_2 = mean(x_2), 
            sd_1= sd(x_1), sd_2 = sd(x_2), 
            r = cor(x_1, x_2))
params

mnist_27$train %>% mutate(y = factor(y)) %>% 
  ggplot(aes(x_1, x_2, fill = y, color=y)) + 
  geom_point(show.legend = FALSE) + 
  stat_ellipse(type="norm", lwd = 1.5)

library(caret)
train_qda <- train(y ~ ., method = "qda", data = mnist_27$train)

y_hat <- predict(train_qda, mnist_27$test)
confusionMatrix(data = y_hat, reference = mnist_27$test$y)$overall["Accuracy"]

mnist_27$train %>% mutate(y = factor(y)) %>% 
  ggplot(aes(x_1, x_2, fill = y, color=y)) + 
  geom_point(show.legend = FALSE) + 
  stat_ellipse(type="norm") +
  facet_wrap(~y)

#32.7.4 Linear discriminant analysis
params <- mnist_27$train %>% 
  group_by(y) %>% 
  summarize(avg_1 = mean(x_1), avg_2 = mean(x_2), sd_1= sd(x_1), sd_2 = sd(x_2), r = cor(x_1,x_2))

params <- params %>% mutate(sd_1 = mean(sd_1), sd_2=mean(sd_2), r=mean(r))
params 

train_lda <- train(y ~ .,
                   method = "lda",
                   data = mnist_27$train)
y_hat <- predict(train_lda, mnist_27$test)
confusionMatrix(data = y_hat, reference = mnist_27$test$y)$overall["Accuracy"]

#32.7.6 Case study: more than three classes
if(!exists("mnist")) mnist <- read_mnist()

set.seed(3456)
index_127 <- sample(which(mnist$train$labels %in% c(1,2,7)), 2000)
y <- mnist$train$labels[index_127] 
x <- mnist$train$images[index_127,]
index_train <- createDataPartition(y, p=0.8, list = FALSE)
class(mnist$train$images)

# get the quadrants
# temporary object to help figure out the quadrants
row_column <- expand.grid(row=1:28, col=1:28) 
upper_left_ind <- which(row_column$col <= 14 & row_column$row <= 14)
lower_right_ind <- which(row_column$col > 14 & row_column$row > 14)

# binarize the values. Above 200 is ink, below is no ink
x <- x > 200 

# cbind proportion of pixels in upper right quadrant and
## proportion of pixels in lower right quadrant
x <- cbind(rowSums(x[ ,upper_left_ind])/rowSums(x), 
           rowSums(x[ ,lower_right_ind])/rowSums(x)) 

train_set <- data.frame(y = factor(y[index_train]),
                        x_1 = x[index_train,1],
                        x_2 = x[index_train,2])
test_set <- data.frame(y = factor(y[-index_train]),
                       x_1 = x[-index_train,1],
                       x_2 = x[-index_train,2])

train_set %>% 
  ggplot(aes(x_1, x_2, color=y)) + 
  geom_point()

train_qda <- train(y ~ ., method = "qda", data = train_set)
predict(train_qda, test_set, type = "prob") %>% head()
predict(train_qda, test_set) %>% head()
confusionMatrix(predict(train_qda, test_set), test_set$y)$table
confusionMatrix(predict(train_qda, test_set), test_set$y)$overall["Accuracy"]

#lda - The accuracy is much worse because the model is more rigid. 
train_lda <- train(y ~ ., method = "lda", data = train_set)
confusionMatrix(predict(train_lda, test_set), test_set$y)$overall["Accuracy"]


#The results for kNN are much better:
train_knn <- train(y ~ ., method = "knn", tuneGrid = data.frame(k = seq(15, 51, 2)), 
                   data = train_set)
confusionMatrix(predict(train_knn, test_set), test_set$y)$overall["Accuracy"]

train_set %>% mutate(y = factor(y)) %>% 
  ggplot(aes(x_1, x_2, fill = y, color=y)) + 
  geom_point(show.legend = FALSE) + 
  stat_ellipse(type="norm") 

#Q1 
library(dslabs)
library(caret)
data("tissue_gene_expression")

set.seed(1993, sample.kind = "Rounding") # use this line of code if you are using R 3.6 or later
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

train_lda <- train(x, y, method="lda") 
y_hat <- predict(train_lda, x) 
train_lda$results["Accuracy"] 
########################################
#book answer
fit_lda <- train(x, y, method = "lda")
fit_lda$results["Accuracy"]
########################################
#Q2
tm <- t(fit_lda$finalModel$means)
x <- as.vector(tm[,1])
y <- as.vector(tm[,2])
x_n <- colnames(tm)

plot(x, y, xlab=x_n[1], ylab=x_n[2], pch=18, col="blue")
text(x, y, row.names(tm), cex=0.6, pos=4, col="red")

#################################################################
#book answer
t(fit_lda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()
#################################################################
#Q3
library(dslabs)
library(caret)
data("tissue_gene_expression")

set.seed(1993, sample.kind = "Rounding") # use this line of code if you are using R 3.6 or later
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

train_qda <- train(x, y, method="qda") 
y_hat <- predict(train_qda, x) 
train_qda$results["Accuracy"] 
########################################
#book answer
fit_qda <- train(x, y, method = "qda")
fit_qda$results["Accuracy"]
########################################
#Q4
tm <- t(fit_qda$finalModel$means)
x <- as.vector(tm[,1])
y <- as.vector(tm[,2])

plot(x, y, xlab=colnames(tm)[1], ylab=colnames(tm)[2], pch=18, col="blue")
text(x, y, row.names(tm), cex=0.6, pos=4, col="red")
#################################################################
#book answer
t(fit_qda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()
#################################################################
#Q5 re-run lda with  preProcess = "center"
library(dslabs)
library(caret)
data("tissue_gene_expression")

set.seed(1993, sample.kind = "Rounding") # use this line of code if you are using R 3.6 or later
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

fit_lda <- train(x, y, method = "lda", preProcess = "center")
fit_lda$results["Accuracy"]
########################################
tm <- t(fit_lda$finalModel$means)
x <- as.vector(tm[,1])
y <- as.vector(tm[,2])
x_n <- colnames(tm)

plot(x, y, xlab=x_n[1], ylab=x_n[2], pch=18, col="blue")
text(x, y, row.names(tm), cex=0.6, pos=4, col="red")

#################################################################################################################
#book answer
#Explanation
#The following code can be used to make the plot to evaluate which genes are driving the algorithm after scaling:
fit_lda <- train(x, y, method = "lda", preProcess = "center")
fit_lda$results["Accuracy"]
t(fit_lda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(predictor_name, hippocampus)) +
  geom_point() +
  coord_flip()
#You can see that it is different genes driving the algorithm now. This is because the predictor means change.
#In the previous exercises we saw that both LDA and QDA approaches worked well. 
#For further exploration of the data, you can plot the predictor values for the two genes with the largest 
#differences between the two groups in a scatter plot to see how they appear to follow a bivariate distribution 
#as assumed by the LDA and QDA approaches, coloring the points by the outcome, using the following code:
d <- apply(fit_lda$finalModel$means, 2, diff)
ind <- order(abs(d), decreasing = TRUE)[1:2]
plot(x[, ind], col = y)
#################################################################################################################
#Q6
library(dslabs)      
library(caret)
data("tissue_gene_expression")

set.seed(1993, sample.kind = "Rounding") # use this line of code if you are using R 3.6 or later
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]

fit_lda <- train(x, y, method = "lda", preProcess = c("center"))
fit_lda$results["Accuracy"]
confusionMatrix(fit_lda)
