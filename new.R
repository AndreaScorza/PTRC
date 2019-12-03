require(OpenImageR)
require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)
require(MLmetrics)
require(base)
require(glmnet)
require(e1071)


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
#we take the longest white row and longest white column

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

#trying the third approach
imageShow(matrix(as.numeric(mnist.dat[6,-1]),nrow=28,ncol=28,byrow=T))

firstNumber <- as.numeric(mnist.dat[6,])
fnm <- matrix(firstNumber[-c(1)],nrow = 28,ncol = 28) # FNM = first number matrix
imageShow(fnm)

print(ncol(fnm))

#we did this to center the image even more, and remove the noise of black useless row /columns
z <- 1
my_list <- list()
while (z <= 42000){
    firstNumber <- as.numeric(mnist.dat[z,])
    fnm <- matrix(firstNumber[-c(1)],nrow = 28,ncol = 28)
    

    
    for (i in 1: ncol(fnm)){
    if (sum(fnm[i]) == 0){
      fnm <- fnm[, -i]
      }
    }
    
    for (i in 1: nrow(fnm)){
    if (sum(fnm[i]) == 0){
      fnm <- fnm[-i,]
      }
    }
    
    
    vec <- c()
    vec <- c (vec, mean(fnm[1:4,1:4]))
    vec <- c (vec, mean(fnm[1:4,5:8]))
    vec <- c (vec, mean(fnm[1:4,9:12]))
    vec <- c (vec, mean(fnm[1:4,13:ncol(fnm)]))
    
    vec <- c (vec, mean(fnm[5:8,1:4]))
    vec <- c (vec, mean(fnm[5:8,5:8]))
    vec <- c (vec, mean(fnm[5:8,9:12]))
    vec <- c (vec, mean(fnm[5:8,13:ncol(fnm)]))
    
    vec <- c (vec, mean(fnm[9:12,1:4]))
    vec <- c (vec, mean(fnm[9:12,5:8]))
    vec <- c (vec, mean(fnm[9:12,9:12]))
    vec <- c (vec, mean(fnm[9:12,13:ncol(fnm)]))
    
    
    vec <- c (vec, mean(fnm[13:nrow(fnm),1:4]))
    vec <- c (vec, mean(fnm[13:nrow(fnm),5:8]))
    vec <- c (vec, mean(fnm[13:nrow(fnm),9:12]))
    vec <- c (vec, mean(fnm[13:nrow(fnm),13:ncol(fnm)]))
    my_list <- list(my_list, vec)
    
    z <- z + 1
}

print(my_list[1])
print(vec)


training_data2 <- data.frame("label" = mnist.dat[,1], "weights" = vec)
model2 <- multinom(training_data2)
summary(model2)



# NEW PART POINT 5

label <- mnist.dat[,1]
features <- mnist.dat[,-1]
#dafare <- mnist.dat[5001:42000, -1]
print(dafare)

x <- data.frame("label" = label, "features" = features)
print(x[5,1])

model <- multinom(x[1:100,] ,MaxNWts = 10000)
      
prediction <- predict(model, x[101:200, ])
summary(prediction)

print (x$label[i])
i <- 1
truth_vect <- c()
while (i < 201){
  if(label[i] == prediction[i]) {
    truth_vect <- c(truth_vect, 0)
  }
  else{
    truth_vect <- c(truth_vect, 1)
  }
  i <- i + 1
}
###### Trying cv.glmnet

labels <- mnist.dat[1:5000,1]
features <- mnist.dat[1:5000,-1]

test_labels <- mnist.dat[5001:42000,1]
test_features <- mnist.dat[5001:42000,-1]

model  <- cv.glmnet(as.matrix(features), labels, family="multinomial", type.measure="class")
plot(model) # to find the best lambda

prediction <- predict(model, as.matrix(test_features),type="class")
summary(prediction)
prediction.confmat <- table(test_labels, prediction)
print(prediction.confmat)
print(sum(diag(prediction.confmat))/sum(prediction.confmat)) #that's the accuracy

####### svm

## doing it again but with the function on the whole dataset

mnist.dat.cleaned <- rem.zero(mnist.dat)
i <- 1
vect <- c()
for (i in 1:ncol(mnist.dat.cleaned[1:5000,])){
  if (sum(mnist.dat.cleaned[1:5000,i]) == 0){
    vect <- c(vect, i)
  }
}
i <- 1
for (i in 1:ncol(mnist.dat.cleaned[5001:nrow(mnist.dat.cleaned),])){
  if (sum(mnist.dat.cleaned[5001:nrow(mnist.dat.cleaned),i]) == 0){
    vect <- c(vect, i)
  }
}
print(vect)
mnist.dat.cleaned <- mnist.dat.cleaned[, -vect]

labels <- mnist.dat.cleaned[1:5000, 1]
features <- mnist.dat.cleaned[1:5000, -1]

test_labels <- mnist.dat.cleaned[5001:nrow(mnist.dat.cleaned), 1]
test_features <- mnist.dat.cleaned[5001:nrow(mnist.dat.cleaned), -1]

model.svm <- svm(features, labels)
pred.svm <- predict(model.svm, test_features)

summary(pred.svm)
print(test_labels)
pred.svm.confmat <- table(test_labels, pred.svm)
print(pred.svm.confmat)
print(sum(diag(pred.svm.confmat))/sum(pred.svm.confmat)) #that's the accuracy

#### neural network

labels <- mnist.dat[1:5000,1]
features <- mnist.dat[1:5000,-1]

test_labels <- mnist.dat[5001:42000,1]
test_features <- mnist.dat[5001:42000,-1]


model  <- nnet(as.matrix(features), labels, size = 3, MaxNWts = 10000)

prediction <- predict(model, as.matrix(test_features))
summary(prediction)
prediction.confmat <- table(test_labels, prediction)
print(prediction.confmat)
print(sum(diag(prediction.confmat))/sum(prediction.confmat)) #that's the accuracy


