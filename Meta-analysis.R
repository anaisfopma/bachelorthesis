#Read in data, all studies
data <- read.table("data.txt", sep="\t", header=TRUE)
data

#Random-effects model
library(metafor)
res <- rma(yi=yi, vi=vi, data=data, slab=author)
res
predict(res)
forest(res)
funnel(res)

#Outliers
inf <- influence(res)
plot(inf)

#Read in data2, without 7 8 10 12
data2 <- read.table("data2.txt", sep="\t", header=TRUE)
data2

#Random-effects model without 7 8 10 12, sensitivity analysis
res2 <- rma(yi=yi, vi=vi, data=data2, slab=author)
res2
forest(res2)
funnel(res2)

#####################################################

#Read in data3, only stats studies
data3 <- read.table("data3.txt", sep="\t", header=TRUE)
data3

#Random-effects model
library(metafor)
res3 <- rma(yi=yi, vi=vi, data=data3, slab=author)
res3
forest(res3)

#Outliers
inf3 <- influence(res3)
plot(inf3)

#Read in data4, without 3 5
data4 <- read.table("data4.txt", sep="\t", header=TRUE)
data4

#Random-effects model without 3 5, sensitivity analysis
res4 <- rma(yi=yi, vi=vi, data=data4, slab=author)
res4
forest(res4)

######################################################

#Trim and fill
tf.res <- trimfill(res)
tf.res
funnel(tf.res)

######################################################

#Subgroup analyses

dat1 <- data[c(1,3,5,8,9,11,13,14,15),] #EM
re1 <- rma(yi=yi, vi=vi, data=dat1, slab=author)
re1 
forest(re1)

dat2 <- data[c(2,6,7),] #SO
re2 <- rma(yi=yi, vi=vi, data=dat2, slab=author)
re2  
forest(re2)

dat3 <- data[c(4,10,12),] #EO
re3 <- rma(yi=yi, vi=vi, data=dat3, slab=author)
re3 
forest(re3)

#Best model
databest <- data[c(-12,-10,-4,-13,-7,-6,-1),] 
resbest <- rma(yi=yi, vi=vi, data=databest, slab=author)
resbest 
forest(resbest)

#Funnel plots
funnel(resbest)














