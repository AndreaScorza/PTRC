mnist.dat <- read.csv("C:/Users/User/Desktop/PTRC/mnist.csv")

require(OpenImageR)
require(nnet)
install.packages("SDMTools")
require(SDMTools)
install.packages("crossval")
require(crossval)
install.packages("MLmetrics")
require(MLmetrics)

source("http://bioconductor.org/biocLite.R")
biocLite("EBImage")

library("EBImage")

firstcol <- data.frame()
one <- data.frame()

label <- c(mnist.dat[,1])

#firstcol <- rbind(label)


x <- c(0,0,0,0,0,0,0,0,0,0)

for (i in label){
  if (i==0){
    x[1] <- x[1]+1
}


if (i==1){
    x[2] <- x[2]+1
  }

  if (i==2){
    x[3] <- x[3]+1
  }

  if (i==3){
    x[4] <- x[4]+1
  }

  if (i==4){
    x[5] <- x[5]+1
  }

  if (i==5){
    x[6] <- x[6]+1
  }

  if (i==6){
    x[7] <- x[7]+1
  }

  if (i==7){
    x[8] <- x[8]+1
  }

  if (i==8){
    x[9] <- x[9]+1
  }

  if (i==9){
    x[10] <- x[10]+1
  }

} 

x
sum(x)
max(x)

percent <- round((max(x, na.rm=FALSE)/sum(x))*100,2)

percent



Ink


data_mean <- tapply(label, mnist.dat[,1], mean)
data_sd <- tapply(label, mnist.dat[,1], sd)

data_mean
data_sd

#Creating a new matrix, replacing all positive pixels by 1
new_set <- mnist.dat[,2:ncol(mnist.dat)]
new_dataset <- replace(new_set, new_set!=0, 1)

#Max Dataset
{
max_dataset <- mnist.dat[,2:ncol(mnist.dat)]
for (i in max_dataset[1:ncol(max_dataset)]){
max_dataset[i] <- replace(new_set, new_set!= max(new_set), 0)
}
max_dataset <- cbind(mnist.dat[,1], max_dataset)
}


imageShow(matrix(as.numeric(max_dataset[366,-1]),nrow=28,ncol=28,byrow=T))
imageShow(matrix(as.numeric(set1[80,-1]),nrow=28,ncol=28,byrow=T))


#MODEL WITH DENSISTIES FOR 4 ZONES (+2300 correct predictions compared to other model)
{

Density_1 <- cbind(new_set[, 1:196])
Density_2 <- cbind(new_set[, 197:392])
Density_3 <- cbind(new_set[, 393:588])
Density_4 <- cbind(new_set[, 589: 784])
Density_5 <- cbind(new_set[])
Density_6 <- cbind(new_set)

Density_1 <- c(apply(Density_1 != 0,1,sum))
Density_2 <- c(apply(Density_2 != 0,1,sum))
Density_3 <- c(apply(Density_3 != 0,1,sum))
Density_4 <- c(apply(Density_4 != 0,1,sum))

Scaled_D1 <- scale(Density_1, center = TRUE, scale = TRUE)
Scaled_D2 <- scale(Density_2, center = TRUE, scale = TRUE)
Scaled_D3 <- scale(Density_3, center = TRUE, scale = TRUE)
Scaled_D4 <- scale(Density_4, center = TRUE, scale = TRUE)

Denstity_D <- c()
Density_D <- cbind(Density_1, Density_2, Density_3, Density_4)

Scaled_D <- c()
Scaled_D <- cbind(Scaled_D1, Scaled_D2, Scaled_D3, Scaled_D4)

training_data3 <- data.frame("label" = mnist.dat[,1], "weights" = Density_D)
model3 <- multinom(training_data3)
summary(model3)
prediction3 <- predict(model3, training_data3)
summary(prediction3)
}

# TRIAL WITH 8 ZONES (-1100 correct predictions compared to 4 ZONES)
{
Density1 <- c()
Density2 <- c()
Density3 <- c()
Density4 <- c()
Density5 <- c()
Density6 <- c()
Density7 <- c()
Density8 <- c()
Density1 <- cbind(new_set[, 1:98])
Density2 <- cbind(new_set[, 99:196])
Density3 <- cbind(new_set[, 197:294])
Density4 <- cbind(new_set[, 295: 392])
Density5 <- cbind(new_set[, 393:490])
Density6 <- cbind(new_set[, 491:588])
Density7 <- cbind(new_set[, 589:686])
Density8 <- cbind(new_set[, 687:784])
Density1 <- c(apply(Density1 != 0,1,sum))
Density2 <- c(apply(Density2 != 0,1,sum))
Density3 <- c(apply(Density3 != 0,1,sum))
Density4 <- c(apply(Density4 != 0,1,sum))
Density5 <- c(apply(Density5 != 0,1,sum))
Density6 <- c(apply(Density6 != 0,1,sum))
Density7 <- c(apply(Density7 != 0,1,sum))
Density8 <- c(apply(Density8 != 0,1,sum))
ScaledD1 <- scale(Density1, center = TRUE, scale = TRUE)
ScaledD2 <- scale(Density2, center = TRUE, scale = TRUE)
ScaledD3 <- scale(Density3, center = TRUE, scale = TRUE)
ScaledD4 <- scale(Density4, center = TRUE, scale = TRUE)
ScaledD5 <- scale(Density5, center = TRUE, scale = TRUE)
ScaledD6 <- scale(Density6, center = TRUE, scale = TRUE)
ScaledD7 <- scale(Density7, center = TRUE, scale = TRUE)
ScaledD8 <- scale(Density8, center = TRUE, scale = TRUE)

ScaledD <- c()
ScaledD <- cbind(ScaledD1, ScaledD2, ScaledD3, ScaledD4, ScaledD5, ScaledD6, ScaledD7, ScaledD8)

training_data4 <- data.frame("label" = mnist.dat[,1], "weights" = ScaledD)
model4 <- multinom(training_data4)
summary(model4)
prediction4 <- predict(model4, training_data4)
summary(prediction4)

summary(prediction)
summary(prediction3)
}


trueValue <- mnist.dat[,1]
truth_vect3 <- c()
i <- 1
while (i <= 42000){
  if (prediction3[i] == trueValue[i]){
    truth_vect3 <- c(truth_vect3, 0)
  }
  else{
    truth_vect3 <- c(truth_vect3, 1)
  }
  i <- i + 1
}
print(truth_vect3)

CM <- ConfusionMatrix(prediction3, truth_vect3)
CM2 <- ConfusionMatrix(prediction3, trueValue)
print(CM)
print(CM2)


{
set1 <- data.frame()
set2 <-data.frame()
set3 <-data.frame()
set4 <-data.frame()
set5 <-data.frame()
set6 <-data.frame()
set7 <-data.frame()
set8 <-data.frame()
set9 <- data.frame()
set10 <-data.frame()
set11 <-data.frame()
set12 <-data.frame()
set13 <-data.frame()
set14 <-data.frame()
set15 <-data.frame()
set16 <-data.frame()

set1 <- cbind(as.matrix(new_set[1:196, 1:196]))
set2 <- cbind(as.matrix(new_set[1:196, 197:392]))
set3 <- cbind(as.matrix(new_set[1:196, 393:588]))
set4 <- cbind(as.matrix(new_set[1:196, 589:784]))
set5 <- cbind(as.matrix(new_set[197:392, 1:196]))
set6 <- cbind(as.matrix(new_set[197:392, 197:392]))
set7 <- cbind(as.matrix(new_set[197:392, 393:588]))
set8 <- cbind(as.matrix(new_set[197:392, 589:784]))
set9 <- cbind(as.matrix(new_set[393:588, 1:196]))
set10 <- cbind(as.matrix(new_set[393:588, 197:392]))
set11 <- cbind(as.matrix(new_set[393:588, 393:588]))
set12 <- cbind(as.matrix(new_set[393:588, 589:784]))
set13 <- cbind(as.matrix(new_set[589:784, 1:196]))
set14 <- cbind(as.matrix(new_set[589:784, 197:392 ]))
set15 <- cbind(as.matrix(new_set[589:784, 393:588]))
set16 <- cbind(as.matrix(new_set[589:784, 589:784]))

Ink1 <- scale(c(apply(set1 != 0,1,sum),center=TRUE, scale= TRUE))
Ink2 <- scale(c(apply(set2 != 0,1,sum),center=TRUE, scale= TRUE))
Ink3 <- scale(c(apply(set3 != 0,1,sum),center=TRUE, scale= TRUE))
Ink4 <- scale(c(apply(set4 != 0,1,sum),center=TRUE, scale= TRUE))
Ink5 <- scale(c(apply(set5 != 0,1,sum),center=TRUE, scale= TRUE))
Ink6 <- scale(c(apply(set6 != 0,1,sum),center=TRUE, scale= TRUE))
Ink7 <- scale(c(apply(set7 != 0,1,sum),center=TRUE, scale= TRUE))
Ink8 <- scale(c(apply(set8 != 0,1,sum),center=TRUE, scale= TRUE))
Ink9 <- scale(c(apply(set9 != 0,1,sum),center=TRUE, scale= TRUE))
Ink10 <- scale(c(apply(set10 != 0,1,sum),center=TRUE, scale= TRUE))
Ink11 <- scale(c(apply(set11 != 0,1,sum),center=TRUE, scale= TRUE))
Ink12 <- scale(c(apply(set12 !=0,1,sum),center=TRUE, scale= TRUE))
Ink13 <- scale(c(apply(set13 != 0,1,sum),center=TRUE, scale= TRUE))
Ink14 <- scale(c(apply(set14 != 0,1,sum),center=TRUE, scale= TRUE))
Ink15 <- scale(c(apply(set15 != 0,1,sum),center=TRUE, scale= TRUE))
Ink16 <- scale(c(apply(set16 != 0,1,sum),center=TRUE, scale= TRUE))

{
Ink1 <- scale(c(apply(set1 != 0,1,sum),center=TRUE, scale= TRUE))
Ink2 <- scale(c(apply(set2 != 0,1,sum),center=TRUE, scale= TRUE))
Ink3 <- scale(c(apply(set3 != 0,1,sum),center=TRUE, scale= TRUE))
Ink4 <- scale(c(apply(set4 != 0,1,sum),center=TRUE, scale= TRUE))
Ink5 <- scale(c(apply(set5 != 0,1,sum),center=TRUE, scale= TRUE))
Ink6 <- scale(c(apply(set6 != 0,1,sum),center=TRUE, scale= TRUE))
Ink7 <- scale(c(apply(set7 != 0,1,sum),center=TRUE, scale= TRUE))
Ink8 <- scale(c(apply(set8 != 0,1,sum),center=TRUE, scale= TRUE))
Ink9 <- scale(c(apply(set9 != 0,1,sum),center=TRUE, scale= TRUE))
Ink10 <- scale(c(apply(set10 != 0,1,sum),center=TRUE, scale= TRUE))
Ink11 <- scale(c(apply(set11 != 0,1,sum),center=TRUE, scale= TRUE))
Ink12 <- scale(c(apply(set12 !=0,1,sum),center=TRUE, scale= TRUE))
Ink13 <- scale(c(apply(set13 != 0,1,sum),center=TRUE, scale= TRUE))
Ink14 <- scale(c(apply(set14 != 0,1,sum),center=TRUE, scale= TRUE))
Ink15 <- scale(c(apply(set15 != 0,1,sum),center=TRUE, scale= TRUE))
Ink16 <- scale(c(apply(set16 != 0,1,sum),center=TRUE, scale= TRUE))
}
{
 
vec2 <- c() 
vec2 <- rbind(vec2, mean(Ink1))
vec2 <- rbind(vec2, mean(Ink2))
vec2 <- rbind(vec2, mean(Ink3))
vec2 <- rbind(vec2, mean(Ink4))
vec2 <- rbind(vec2, mean(Ink5))
vec2 <- rbind(vec2, mean(Ink6))
vec2 <- rbind(vec2, mean(Ink7))
vec2 <- rbind(vec2, mean(Ink8))
vec2 <- rbind(vec2, mean(Ink9))
vec2 <- rbind(vec2, mean(Ink10))
vec2 <- rbind(vec2, mean(Ink11))
vec2 <- rbind(vec2, mean(Ink12))
vec2 <- rbind(vec2, mean(Ink13))
vec2 <- rbind(vec2, mean(Ink14))
vec2 <- rbind(vec2, mean(Ink15))
vec2 <- rbind(vec2, mean(Ink16))


down_dataset <- down_sample_image(new_dataset, )

down_dataset <- data.frame()
for (i in 1:nrow(new_set)){
  down_dataset <- rbind(down_dataset, down_sample_image(as.matrix(new_set[i, 1:ncol(new_set)]),2))
}




sum(mnist.dat)
sum(new_dataset)

#Adding the 1st label column (unmodified by previous function)
new_dataset <- cbind(mnist.dat[,1], new_dataset)

sum(new_dataset)
mean_Ink_per_number <- round(tapply(Ink, mnist.dat[,1], mean),2)
sd_Ink_per_number <-round(tapply(Ink, mnist.dat[,1], sd),2)

tapply(Ink, mnist.dat[,1])

mean_number <- Ink

EBI <- readImage(system.file("images", "sample-color.png", package="EBImage"))


# CREATE SUB-SETS OF IMAGE (MULTI-ZONING)

subset1 <- c()
subset1 <- subset(new_set, new_set==1,  select = as.matrix(new_set[1:3, 1:3]))
subset1 <- new_set[1:nrow(new_set), 1:ncol(new_set)]
subset2 <- split.data.frame(new_set[], as.matrix(new_set))
subset2 <- split.data.frame(new_set, 28, as.matrix(new_set))
subset2 <- new_set[seq_len(28), seq_len(28)]
{
# DIVIDE EVERY PIXEL OF A LABEL PER MEAN PIXEL VALUE PER NUMBER

mean_image <- data.frame(mnist.dat[,2:ncol(mnist.dat)])
mean_image <- apply(mean_image,2,mean)

#DISPLAY IMAGE OF THE MEAN

imageShow(matrix(as.numeric(mean_image[]),nrow=28,ncol=28,byrow=T))

#DISPLAY IMAGE OF BINARY WEIGHTS
imageShow(matrix(as.numeric(new_dataset[367,-1]),nrow=28,ncol=28,byrow=T))

#DISPLAY IMAGE OF DATA
imageShow(matrix(as.numeric(mnist.dat[367, -1]),nrow=28,ncol=28,byrow=T))

confusion.matrix()

Ink<- c()

Ink <- c(apply(mnist.dat != 0,1,sum))
#Ink_version2 <- c(apply(new_dataset == 1 ,1,sum))

Ink
#Ink_version2

scaled_Ink <- scale(Ink, center=TRUE, scale= TRUE)
#scaled_Ink_version2 <- scale(Ink_version2, center=TRUE, scale= TRUE)


training_data <- data.frame("label" = mnist.dat[,1], "weights" = scaled_Ink)
#training_data_version2 <- data.frame("label" = mnist.dat[,1], "weights" = scaled_Ink_version2)

model <- multinom(training_data)
#result <- multinom(training_data_version2)
summary(model)

prediction <- predict(model, training_data)
summary(prediction)

trueValue <- mnist.dat[,1]
print(trueValue)
        
#Transform results in a TRUE/FALSE output

truth_vect <- c()
i <- 1
while (i <= 42000){
  if (prediction[i] == trueValue[i]){
    truth_vect <- c(truth_vect, 0)
  }
  else{
    truth_vect <- c(truth_vect, 1)
  }
  i <- i + 1
}
print(truth_vect)

truth_vect_two_features <- c()
i <- 1
while (i <= 42000){
  if (prediction_two_features[i] == trueValue[i]){
    truth_vect_two_features <- c(truth_vect_two_features, 0)
  }
  else{
    truth_vect_two_features <- c(truth_vect_two_features, 1)
  }
  i <- i + 1
}
print(truth_vect_two_features)

CM <- ConfusionMatrix(prediction, truth_vect)
CM2 <- ConfusionMatrix(prediction, trueValue)
print(CM)
print(CM2)

P <- Precision(truth_vect, prediction)
R <- Recall(truth_vect, prediction)
A <- Accuracy(truth_vect, prediction)
F1 <- (2*P*R/P+R)
print(F1)
print(P)
print(R)
print(A)

#WITH TWO FEATURES (Density + Zones Density --> Precision of 99.6%)
P3 <- Precision(truth_vect_two_features, prediction_two_features)
R3 <- Recall(truth_vect_two_features, prediction_two_features)
A3 <- Accuracy(truth_vect_two_features, prediction_two_features)
F3 <- (2*P3*R3/P3+R3)
print(F3)
print(P3)
print(R3)
print(A3)

summary(prediction_two_features)

firstRow <- mnist.dat[1,]
firstRow <- matrix(firstRow[-c(1)],nrow = 28,ncol = 28)
print(firstRow) #we have the first row as a Matrix

}


matrix <- mnist.dat[1:12, 1:12]
matrix2 <- split.data.frame(matrix, as.factor(matrix[seq_len(6), seq_len(6)]))

}


#ZONING ANDREA

firstNumber <- as.numeric(mnist.dat[6,])
fnm <- matrix(firstNumber[-c(1)],nrow = 28,ncol = 28) # FNM = first number matrix
imageShow(fnm)

print(ncol(fnm))

#we did this to center the image even more, and remove the noise of black useless row /columns

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

print(vec)

training_data2 <- data.frame("label" = mnist.dat[,1], "weights" = vec2)
model2 <- multinom(training_data2)
summary(model2)
prediction2 <- predict(model2, training_data2)
summary(prediction2)
}

training_data_two_features <- data.frame("label" = mnist.dat[,1], "weights" = training_data, training_data3)
model_two_features <- multinom(training_data_two_features)
prediction_two_features <- predict(model_two_features, training_data_two_features)
summary(prediction_two_features)
summary(prediction)
summary(prediction3)

# Difference between feature models
summary(prediction3) - summary(prediction)
summary(prediction_two_features) - summary(prediction3)
summary(prediction_two_features) - summary(prediction)


