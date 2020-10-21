library(Rcpp)
library(tidyverse)



# First Try - Bedhead -----------------------------------------------------

# The original formula:

# xnew=sin(x*y/b)*y+cos(a*x-y)
# ynew=x+sin(y)/b

#The funtion that will create the trajectory of millions of points (n)
#starting at origin points x0 and y0
#using the attractor functions + the previous data point for each new point 
# + parameters to be defined - a, b
#Will result in a data frame 'dat', to be plotted in ggplot
cppFunction('DataFrame createTrajectory(int n, double x0, double y0, 
            double a, double b) {
            // create the columns
            NumericVector x(n);
            NumericVector y(n);
            x[0]=x0;
            y[0]=y0;
            for(int i = 1; i < n; ++i) {
            x[i] = sin(x[i-1]*y[i-1]/b)*y[i-1]+cos(a*x[i-1]-y[i-1]);
            y[i] = x[i-1]+sin(y[i-1])/b; 
            }
            // return a new data frame
            return DataFrame::create(_["x"]= x, _["y"]= y);
            }
            ')


#Loops through a set of 10 random images to be exported.

for(i in 1:10) {
  
#Set parameters - random values between -1 and 1
a <- runif(1, -1, 1)
b <- runif(1, -1, 1)

#Create the data frame that will be plotted with ggplot
# Number of points, x and y points start at 1, a and b set above 
#createTrajectory(n, x0, y0, a, b)
dat = createTrajectory(4000000, 1, 1, a, b)

#plot
p <- ggplot(dat, aes(x, y)) + 
  geom_point(shape=46, alpha=.01) + 
  theme(legend.position  = "none",
        panel.background = element_rect(fill="#f3f3e6"), 
        axis.ticks       = element_blank(),
        panel.grid       = element_blank(),
        axis.title       = element_blank(),
        axis.text        = element_blank())

ggsave(filename = paste0("bedhead"," a ",round(a,3)," b ",round(b,3),".png"), 
       p, 
       device = "png", 
       width = 10,
       height = 10,
       units = c("in"),)

#"ratchet" parameter
#Try adding color

}