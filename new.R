require(OpenImageR)

## for multinomial logit mode
require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)
#

mnist.dat <- read.csv("/Users/andrea/Desktop/PTRC/mnist.csv")
#attach(mnist.dat)
#imageShow(matrix(as.numeric(mnist.dat[848,-1]),nrow=28,ncol=28,byrow=T))

#SHOW THE IMAGE OF THE MEAN OF THE NUMBER, BUT CALCULATED FOR ROW, BECAUSE EVERY ROW IS A NUMBER

number <- c(mnist.dat[,1])

x <- c(0,0,0,0,0,0,0,0,0,0)

for (val in number){
  if (val == 0){
    x[1] <- x[1] + 1 
  }
  if (val == 1){
    x[2] <- x[2] + 1 
  }
  if (val == 2){
    x[3] <- x[3] + 1 
  }
  if (val == 3){
    x[4] <- x[4] + 1 
  }
  if (val == 4){
    x[5] <- x[5] + 1 
  }
  if (val == 5){
    x[6] <- x[6] + 1 
  }
  if (val == 6){
    x[7] <- x[7] + 1 
  }
  if (val == 7){
    x[8] <- x[8] + 1 
  }
  if (val == 8){
    x[9] <- x[9] + 1 
  }
  if (val == 9){
    x[10] <- x[10] + 1 
  }
}

vector <- c()
print(x)
print(sum(x)) # number 2 is the majority class !!remember the array position start at 1 !!
wrong_prediction <- sum(x) - max(x)
majority_percentage <- (max(x) * 100 )/sum(x)




vector <- c(apply(mnist.dat != 0, 1, sum)) #number of zeroes grouped by 1, which means row

mean_number <- tapply(vector,mnist.dat[,1],mean) #mean
standard_dev <- tapply(vector,mnist.dat[,1],sd) #standard deviation

scaled_vector <- scale(vector, center = TRUE, scale = TRUE)
print(scaled_vector)

x <- data.frame("label" = mnist.dat[,1], "vectorWeight" = scaled_vector) # x is our dataframe
model <- multinom(x)
summary(model)

######## Mean of the number

mean_image <- data.frame(mnist.dat[,2:ncol(mnist.dat)])
mean_image <- apply(mean_image,2,mean)

#DISPLAY IMAGE OF THE MEAN
imageShow(matrix(as.numeric(mean_image[]),nrow=28,ncol=28,byrow=T))
############ end mean image

#prediction <- predict(model, type = 'probs', x["vectorWeight"])
prediction <- predict(model, x["vectorWeight"])
summary(prediction)

#y <- data.frame("label" = mnist.dat[,1], "vectorWeight" = vector) # not scaled vector
#result2 <- multinom(y)
#summary(result2)




