# load MINIST dataset
# training set image file
f = file("~/Desktop/STATS503/HW/HW5/Data/train-images-idx3-ubyte 2", "rb")
# Read Magic Number(A constant numerical or text value used to identify a file format)
readBin(f, integer(), n=1, endian="big")
# Read Number of Images
readBin(f, integer(), n=1, endian="big")
# Read Number of Rows
readBin(f, integer(), n=1, endian="big")
# Read Number of Columns
readBin(f, integer(), n=1, endian="big")
# Read pixels of every image, each image has nrow x ncol pixels
# Store them in a matrix form for easy visulization
m = (matrix(readBin(f,integer(), size=1, n=28*28, endian="big"),28,28))
image(m)
# Let's flip the image (Sinec we know the first letter is a "5")
df = as.data.frame(m)
df1 = df[,c(28:1)]
m=as.matrix(df1)
image(m)

# Do the same for first 25 images
par(mfrow=c(5,5))
par(mar=c(0.1,0.1,0.1,0.1))
for(i in 1:25){m = matrix(readBin(f,integer(), size=1, n=28*28, endian="big"),28,28);image(m[,28:1])}

# training set label
f = file("~/Desktop/STATS503/HW/HW5/Data//train-labels-idx1-ubyte", "rb")
# Read Magic Number
readBin(f, integer(), n=1, endian="big")

# Read Number of Labels
n = readBin(f,'integer',n=1,size=4,endian='big')
# Read All the Labels
y = readBin(f,'integer',n=n,size=1,signed=F)
close(f)
# See if the first letter is "5"
y[1]

# Display first 25 labels
mlabel=t(matrix(y[2:26],5,5))
mlabel


# summary to load train and test dataset: and swap training and testing set
load_image_file <- function(filename) {
  ret = list()
  f = file(filename,'rb')
  readBin(f,'integer',n=1,size=4,endian='big')
  ret$n = readBin(f,'integer',n=1,size=4,endian='big')
  nrow = readBin(f,'integer',n=1,size=4,endian='big')
  ncol = readBin(f,'integer',n=1,size=4,endian='big')
  x = readBin(f,'integer',n=ret$n*nrow*ncol,size=1,signed=F)
  ret$x = matrix(x, ncol=nrow*ncol, byrow=T)
  close(f)
  ret
}

load_label_file <- function(filename) {
  f = file(filename,'rb')
  readBin(f,'integer',n=1,size=4,endian='big')
  n = readBin(f,'integer',n=1,size=4,endian='big')
  y = readBin(f,'integer',n=n,size=1,signed=F)
  close(f)
  y
}
"~/Desktop/STATS503/HW/HW5/Data//t10k-labels-idx1-ubyte"

train <- load_image_file("~/Desktop/STATS503/HW/HW5/Data//t10k-images-idx3-ubyte")
test <- load_image_file("~/Desktop/STATS503/HW/HW5/Data/train-images-idx3-ubyte 2")

train$y <- load_label_file("~/Desktop/STATS503/HW/HW5/Data//t10k-labels-idx1-ubyte")
test$y <- load_label_file("~/Desktop/STATS503/HW/HW5/Data//train-labels-idx1-ubyte")  

class(train)
##########################################################################
# SVM
library(e1071)
train


svmfit=svm(labels ~ X + Y, data=TRA, kernel="linear", cost=1, scale=FALSE)
plot(svmfit,filter(sim_dat, type == "Circle"), Y ~ X)

