require(OpenImageR)
mnist.dat <- read.csv("/Users/andrea/Desktop/PTRC/mnist.csv")

#imageShow(matrix(as.numeric(mnist.dat[848,-1]),nrow=28,ncol=28,byrow=T))

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

print(x)
print(sum(x)) # number 2 is the majority class !!remember the array position start at 1 !!

