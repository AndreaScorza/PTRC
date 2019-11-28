require(OpenImageR)

## for multinomial logit mode
require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)
#require(caret)
#require(SDMTools)
#require(crossval)
require(MLmetrics)
require(base)
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

######## Mean of the number

mean_image <- data.frame(mnist.dat[,2:ncol(mnist.dat)])
mean_image <- apply(mean_image,2,mean)

#DISPLAY IMAGE OF THE MEAN
imageShow(matrix(as.numeric(mean_image[]),nrow=28,ncol=28,byrow=T))
############ end mean image


vector <- c(apply(mnist.dat != 0, 1, sum)) #number of zeroes grouped by 1, which means row

mean_number <- tapply(vector,mnist.dat[,1],mean) #mean
standard_dev <- tapply(vector,mnist.dat[,1],sd) #standard deviation

scaled_vector <- scale(vector, center = TRUE, scale = TRUE)
print(scaled_vector)

x <- data.frame("label" = mnist.dat[,1], "vectorWeight" = scaled_vector) # x is our dataframe
model <- multinom(x)
summary(model)

#prediction <- predict(model, type = 'probs', x["vectorWeight"])
prediction <- predict(model, x["vectorWeight"])
summary(prediction)

#y <- data.frame("label" = mnist.dat[,1], "vectorWeight" = vector) # not scaled vector
#result2 <- multinom(y)
#summary(result2)

trueValues <- as.factor(mnist.dat[,1]) #label of true values as factor
trueValues2 <- mnist.dat[,1]


#making the ground truth correct label vector 
#we create a vector where if we predicted correcly is going to print 0 otherwise 1
truth_vect <- c()
i <- 1
zeroCount <- 0
while (i <= 42000){
  if (prediction[i] == trueValues2[i]){ #we use the vector instead of the factor
    truth_vect <- c(truth_vect, 0)
  }
  else{
    truth_vect <- c(truth_vect, 1)
    zeroCount <- zeroCount + 1
  }
  i <- i + 1
}
print(zeroCount)
incorrectPredPerc <- (zeroCount * 100) / length(truth_vect)
correctPredPerc <- ((length(truth_vect) - zeroCount) * 100) / length(truth_vect)
print(correctPredPerc)
# or
# you could have just used this :
P <- Precision(truth_vect, prediction)
R <- Recall(truth_vect, prediction)
A <- Accuracy(truth_vect, prediction)
F1 <- (2*P*R/P+R)
print(F1)
print(P)
print(R)
print(A)


# we print the confusion matrix just with 1 and 0 wich means predicted wrongly and correctly
confMat <- ConfusionMatrix(prediction, truth_vect)
print(confMat)

#here we use the confision matrix with all the values
# on y_true ti dice i numeri che ha predetto erroneamente, ad esempio il 3, è stato predetto correttamente
# 1122 volte, ed è stato scambiato con lo 0 864 volte
confMat2 <- ConfusionMatrix(prediction, trueValues)
print(confMat2)


###############finding the next feature

#width
#trying with the width, every number has the same ....
width_vect <- c()
j <- 1
while (j <= 42000){
firstRow <- as.numeric(mnist.dat[j,])
firstRow <- matrix(firstRow[-c(1)],nrow = 28,ncol = 28)
#print(firstRow) #we have the first row as a Matrix

result <- colSums (firstRow, na.rm = FALSE, dims = 1)
#print(result) #sum by column of the 28 column for a digit

width <- 0
count <- 1
while(count <= 28){
  if (result[count] != 0){
    width <- width + 1
  }
  count <- count + 1
}

width_vect <- c(width_vect, width)
j <- j + 1
}
print(length(width_vect))



#trying the biggest row value and biggest column value

colMax <- c()
rowMax <- c() #all the maximum value
j <- 1
while (j <= 42000){
  
  firstNumber <- as.numeric(mnist.dat[j,])
  fnm <- matrix(firstNumber[-c(1)],nrow = 28,ncol = 28) # FNM = first number matrix
  #print(fnm) #we have the first row as a Matrix
  #print(max(colSums(fnm, na.rm = FALSE, dims = 1)))
  #print(max(rowSums(fnm, na.rm = FALSE, dims = 1)))
  
  colMax <- c(colMax, max(colSums (fnm, na.rm = FALSE, dims = 1)))
  rowMax  <- c(rowMax, max(rowSums (fnm, na.rm = FALSE, dims = 1)))
  print(j)
  j <- j + 1
}

print(colMax)

y <- data.frame("label" = mnist.dat[,1], "Row_Max" = rowMax, "Col_Max" = colMax) 
model2 <- multinom(y)

prediction2 <- predict(model2, y[,2:3])
summary(prediction2)

print(ConfusionMatrix(prediction2, trueValues))

#doing again the truth vector
truth_vect2 <- c()
i <- 1
zeroCount2 <- 0
while (i <= 42000){
  if (prediction2[i] == trueValues2[i]){ #we use the vector instead of the factor
    truth_vect2 <- c(truth_vect2, 0)
  }
  else{
    truth_vect2 <- c(truth_vect2, 1)
    zeroCount2 <- zeroCount2 + 1
  }
  i <- i + 1
}

print(zeroCount2)
incorrectPredPerc2 <- (zeroCount2 * 100) / length(truth_vect2)
correctPredPerc2 <- ((length(truth_vect2) - zeroCount2) * 100) / length(truth_vect2)
print(correctPredPerc2)
# or
# you could have just used this :
P2 <- Precision(truth_vect2, prediction2)
R2 <- Recall(truth_vect2, prediction2)
A2 <- Accuracy(truth_vect2, prediction2)
F12 <- (2*P2*R2/P2+R2)
print(F12)
print(P2)
print(R2)
print(A2)

# it actually works worse 



