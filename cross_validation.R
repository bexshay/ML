#cross validsation
#Q1
library(dplyr)
library(caret)

set.seed(1996)
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p) #If mean or sd are not specified they assume the default values of 0 and 1, respectively.
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()
x_subset <- x[ ,sample(p, 100)]
#answer
fit <- train(x_subset, y, method = "glm")
fit$results

#Q2
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("genefilter")

library(genefilter)
tt <- colttests(x, y)
#answer
pvals <- tt$p.value 

#Q3
ind <- subset(tt,p.value<=0.01)
ind2 <-subset(pvals,pvals<=0.01)
length(ind2)
#answer =108

#Q4
#https://stackoverflow.com/questions/45532058/subsetting-a-matrix-on-the-basis-of-a-list-of-some-of-the-column-names
res_mat <- matrix(row.names(ind)) #creating matrix from row names
class(res_mat)
x_subset2 <- x[,res_mat[,1]] #using them in subsetting

fit2 <- train(x_subset2, y, method = "glm") #refitting it to model
fit2$results

#answer is 0.7605277

#Q5
#fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
#ggplot(fit)

#Q6
#answer We used the entire dataset to select the columns used in the model.

#Q7
library(dslabs)
data(tissue_gene_expression)
dim(tissue_gene_expression$x)
#This matrix has the gene expression levels of 500 genes from 189 biological 
#samples representing seven different tissues. The tissue type is stored in y:
w<- tissue_gene_expression$y
d <- dist(tissue_gene_expression$x)
class(d)

fit <- train(tissue_gene_expression$x, w, method = "knn", tuneGrid = data.frame(k = seq(1, 7, 2)))
fit$results
#answer k=1
