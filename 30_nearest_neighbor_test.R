# Nearest neighbor
library(tidyverse)
library(caret)
library(dslabs)
library(purrr)

data(heights)
y <- heights$sex
x <- heights$height

#The caret package includes the function createDataPartition 
#that helps us generates indexes for randomly splitting the data into training and test sets:
#The argument times is used to define how many random samples of indexes to return
#the argument p is used to define what proportion of the data is represented by the index
#the argument list is used to decide if we want the indexes returned as a list or not
set.seed(1,sample.kind="Rounding") 
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]

heights %>% group_by(sex) %>% summarize(mean(height), sd(height))

#Like in 30.1.3 Picking the k in kNN
set.seed(1,sample.kind="Rounding") 
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]
ks <- seq(1, 101, 3) 
F_1 <- sapply(ks, function(k){
  fit <- knn3(sex ~ height, data = train_set, k = k)
  y_hat <- predict(fit, test_set, type = "class") #%>% factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(test_set$sex))
})

max(F_1)
best_k <- ks[which.max(F_1)]
best_k

#EDX answer code ks  shows 40nbut answer is 46 (may be RStudio version matters, added additional sample.kind)
set.seed(1,sample.kind="Rounding") 
test_index <- createDataPartition(heights$sex, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]     

ks <- seq(1, 101, 3)
F_1 <- sapply(ks, function(k){
  fit <- knn3(sex ~ height, data = train_set, k = k)
  y_hat <- predict(fit, test_set, type = "class") %>% 
    factor(levels = levels(train_set$sex))
  F_meas(data = y_hat, reference = test_set$sex)
})
plot(ks, F_1)
max(F_1)
ks[which.max(F_1)]


#Q2 
library(tidyverse)
library(caret)
library(dslabs)
library(purrr)

data("tissue_gene_expression")
#This matrix has the gene expression levels of 500 genes from 189 biological samples representing seven different tissues
dim(tissue_gene_expression$x)
#The tissue type is stored in y
table(tissue_gene_expression$y)

set.seed(1,sample.kind="Rounding") 
ks<-seq(1,11,2)
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x

test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

train_set_x <- x[test_index,]
test_set_x <- x[-test_index,]
train_set_y <- y[test_index]
test_set_y <- y[-test_index]

accuracy <- sapply(ks, function(k){
  fit <- knn3(train_set_x, train_set_y, k = k)
  y_hat <- predict(fit, test_set_x, type = "class")
  cm_test <- confusionMatrix(data = y_hat, reference = test_set_y)
  test_error <- cm_test$overall["Accuracy"]
  tibble(k = k, test = test_error)
})
class(accuracy)
accuracy
###################################################################
#EDX Q2 answer
set.seed(1)
library(caret)
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
train_index <- createDataPartition(y, list = FALSE)
sapply(seq(1, 11, 2), function(k){
  fit <- knn3(x[train_index,], y[train_index], k = k)
  y_hat <- predict(fit, newdata = data.frame(x=x[-train_index,]),
                   type = "class")
  mean(y_hat == y[-train_index])
})
###################################################################