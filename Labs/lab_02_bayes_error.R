# a. plot the 20 means using the corresponding class color
load("mixture.Rdata")
library(ggplot2)

# means: 20 by 2 table, ordered by \mu_1, ..., \mu_20
means = as.data.frame(means)
color = c(rep("blue", 10), rep("orange", 10))
ggplot(means, aes(x =V1, y = V2)) + geom_point(colour = color)

# b. for each grid point, compute conditional probability p(x| Y = "blue") 
# and p(x| Y = "organge"), and p(x)
library(mvtnorm)

# mixture Gaussian for blue class
density_blue = function(x){
  den = 0
  for(i in 1:10){
    den = den + dmvnorm(x, as.numeric(means[i, ], 0.2*diag(2)))
  }
  return(den/10)
}

# mixture Gaussian for orange class
density_orange = function(x){
  den = 0
  for(i in 11:20){
    den = den + dmvnorm(x, as.numeric(means[i, ], 0.2*diag(2)))
  }
  return(den/10)
}
x_marginal_blue = apply(xnew, 1, density_blue)
x_marginal_orange = apply(xnew, 1, density_orange)
x_marginal = 0.5*x_marginal_blue + 0.5*x_marginal_orange 

# c. based on b, for each grid point, compute p(Y = blue |x) and decide a color 
# for each grid point
conditional_blue = x_marginal_blue*0.5/x_marginal
color = rep("orange", nrow(xnew))
color[conditional_blue>0.5] = "blue"

# d. plot the test points using the decided color in c, and also plot the decision
# boundary
xnew = data.frame(xnew)
ggplot(xnew, aes(x = V1, y =V2, z = as.numeric(conditional_blue>0.5))) +
  geom_point(color = color) +geom_contour(color = "black")

# e. compute the Bayes error rate
bayes_error = sum(x_marginal*(conditional_blue * as.numeric(conditional_blue<0.5) +
                                (1 - conditional_blue) *as.numeric(conditional_blue>0.5))) / sum(x_marginal)

