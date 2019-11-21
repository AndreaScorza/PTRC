mnist.dat <- read.csv('C:/Users/User/Desktop/PTRC/mnist.csv')

require(OpenImageR)

#imageShow(matrix(as.numeric(mnist.dat[360,-1]),nrow=28,ncol=28,byrow=T))

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

percent <- (max(x, na.rm=FALSE)/sum(x))*100



