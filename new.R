require(OpenImageR)
mnist.dat <- read.csv("/Users/andrea/Desktop/PTRC/mnist.csv")
imageShow(matrix(as.numeric(mnist.dat[380,-1]),nrow=28,ncol=28,byrow=T))