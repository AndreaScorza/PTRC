  mnist.dat <- read.csv('C:/Users/User/Desktop/PTRC/mnist.csv')

require(OpenImageR)
require(nnet)


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

sum(mnist.dat)
sum(new_dataset)

#Adding the 1st label column (unmodified by previous function)
new_dataset <- cbind(mnist.dat[,1], new_dataset)

sum(new_dataset)
mean_Ink_per_number <- round(tapply(Ink, mnist.dat[,1], mean),2)
sd_Ink_per_number <-round(tapply(Ink, mnist.dat[,1], sd),2)

tapply(Ink, mnist.dat[,1])

mean_number <- Ink






# DIVIDE EVERY PIXEL OF A LABEL PER MEAN PIXEL VALUE PER NUMBER

mean_image <- data.frame(mnist.dat[,2:ncol(mnist.dat)])
mean_image <- apply(mean_image,2,mean)

#DISPLAY IMAGE OF THE MEAN

imageShow(matrix(as.numeric(mean_image[]),nrow=28,ncol=28,byrow=T))

#DISPLAY IMAGE OF BINARY WEIGHTS
imageShow(matrix(as.numeric(new_dataset[367,-1]),nrow=28,ncol=28,byrow=T))

#DISPLAY IMAGE OF DATA
imageShow(matrix(as.numeric(mnist.dat[367, -1]),nrow=28,ncol=28,byrow=T))



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
