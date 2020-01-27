# Visualization and Data Manipulation in R


# graphics in R
# basic graphs
data("iris")
pairs(iris)

hist(iris$Sepal.Length)

boxplot(iris$Sepal.Length)

boxplot(iris)

set.seed(100)
x <- runif(10)
y <- runif(10)
plot(x,y,type="p",main="Test plot",col="blue",
     xlim=c(0,1),ylim=c(0,1),xlab="X-axis",
     ylab="Y-axis",pch=20)
points(y/2,x/2,col="red",pch=1:10)
lines(y[1:2],x[1:2],col="purple")
abline(v=0.5,col="green")
abline(h=0.5,col="green",lty=2) # lty: the line type
abline(a=0,b=1,col="green",lwd=5) # a:intercept, b:slope, lwd: the line width

# gray scale plot: image function

library(tensorflow)
#install_tensorflow()
library(keras)
mnist <- dataset_mnist()
x_train <- mnist$train$x
y_train <- mnist$train$y


# visualize the digits
par(mfcol=c(6,6))
par(mar=c(0, 0, 3, 0), xaxs='i', yaxs='i')
for (idx in 1:36) { 
  im <- x_train[idx,,]
  im <- t(apply(im, 2, rev)) 
  image(1:28, 1:28, im, col=gray((0:255)/255), 
        xaxt='n', main=paste(y_train[idx]))
}


# ggplot2 package
library(ggplot2)
data_crime = read.table("citycrime.txt", header = TRUE)
head(data_crime)
plot(x = data_crime$Robbery, y = data_crime$Assault, xlab = 'Robbery', ylab = 'Assault', main = 'Crime')
qplot(x = Robbery, y = Assault, data = data_crime, xlab = 'Robbery', ylab = 'Assault', main = 'Crime')

crime_plot <- ggplot(data = data_crime,aes(x = Robbery, y = Assault))

# scatterplot by using geom_point()
crime_plot + geom_point()

# lines
crime_plot + geom_line()

#color and size
crime_plot + geom_point(aes(color = Murder, size = Larceny))

crime_plot + geom_point(aes(color = Murder)) + 
  scale_colour_gradient(high = "red", low = "blue") + theme_bw()

#text
crime_plot + geom_text(aes(label=rownames(data_crime)), check_overlap = TRUE)


crime_plot <- ggplot(data = data_crime) + geom_point(aes(x = Robbery, y = Assault, color = Murder))
crime_plot <- crime_plot + labs(title = "Crime", x = 'Robbery', y = 'Assualt') 
crime_plot + theme(text = element_text(size=20))

# just change size of title
crime_plot + theme(plot.title = element_text(size = 20))


# annotate our graph
crime_plot + geom_abline(intercept = 1500, slope = -0.5) + 
  geom_text(aes(x = c(1500), y = c(800), label = c('separating hyperplane')), size = 5)


#install.packages('GGally')
library(GGally)
crime_pairs = ggpairs(data_crime,axisLabels = "none",
                      upper = list(continuous = "points", combo = "dot"),
                      lower = list(continuous = "cor", combo = "dot"),
                      diag = list(continuous = "densityDiag")) + 
  theme_bw()
crime_pairs

#3d plot
library(plotly)
p = plot_ly(data_crime, x = ~Robbery, y = ~Assault, z = ~Murder,color=~Larceny)
p


# Data Manipulation
data(iris)
dim(iris)
nrow(iris)
ncol(iris)
names(iris)
str(iris)
summary(iris)
table(iris$Species)
unique(iris$Species)
View(iris)
head(iris)
tail(iris)

id <- which(iris$Species=='setosa')
iris_setosa <- iris[id,]
iris_setosa <- iris_setosa[,-5] # drop the factor 

iris[3,4]
iris$Petal.Length[4]

max(iris$Sepal.Length)

min(iris$Sepal.Length)

which.max(iris$Sepal.Length)

mean(iris$Sepal.Length)

median(iris$Sepal.Length)

sd(iris$Sepal.Length)

# Reshaping a dataset
library(reshape2)
data_crime_city = data.frame(data_crime, City =
                               rownames(data_crime))
head(data_crime_city)

long_data_crime = melt(data_crime_city, id.vars = "City")
head(long_data_crime)

wide_data_crime = dcast(long_data_crime, City ~ variable)
head(wide_data_crime)

trigon_data = data.frame(lapply(c(sin, cos), function(f) f(seq(0, 2*pi,0.05))), x= seq(0, 2*pi,0.05))
colnames(trigon_data) = c("sin", "cos", "x")
trigon_data_long = melt(trigon_data, id.vars = "x")
ggplot(trigon_data_long, aes(x=x, y=value,colour=variable, group = variable)) + geom_line()


# dplyr package
library(dplyr)
head(select(iris,Sepal.Length,Sepal.Width),3)

# select all columns that start with the character string "Sepal"
head(select(iris,starts_with("Sepal")),3)

# filter()
head(filter(iris,Species=="setosa"),3)
head(filter(iris,Species %in% c("setosa","versicolor")),3)

#tail(filter(iris,Species %in% c("setosa","versicolor")),3)
filter(iris,Species=="setosa",Sepal.Length>5.5)
filter(iris,Species=="setosa",Sepal.Length>5.5,Sepal.Width<4)

# pipe operation
iris %>%
  select(Sepal.Length) %>%
  head(3)

# order from small to large
iris %>% arrange(Sepal.Length) %>% head(3)
iris %>% arrange(Species,Sepal.Length) %>% head(3)

# View(iris %>% arrange(Species,Sepal.Length))
iris %>% arrange(Species,Sepal.Length) %>% filter(Sepal.Width>=3.6) %>% tail

# mutate()
iris %>% mutate(sl_mean= mean(Sepal.Length)) %>% head(3)
iris %>% mutate(sl_mean= mean(Sepal.Length),sl_sd=sd(Sepal.Length)) %>% 
  head(3)

