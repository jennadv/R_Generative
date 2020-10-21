library(Rcpp)
library(tidyverse)


#  Clifford - softology ---------------------------------------------------------------


# The original formula:

#xnew=sin(a*y)+c*cos(a*x)
#ynew=sin(b*x)+d*cos(b*y)

#The funtion that will create the trajectory of millions of points (n)
#starting at origin points x0 and y0
#using the attractor functions + the previous data point for each new point 
# + parameters to be defined - a, b, c and d
#Will result in a data frame 'dat', to be plotted in ggplot
cppFunction('DataFrame createTrajectory(int n, double x0, double y0, 
            double a, double b, double c, double d) {
            // create the columns
            NumericVector x(n);
            NumericVector y(n);
            x[0]=x0;
            y[0]=y0;
            for(int i = 1; i < n; ++i) {
            x[i] = sin(a*y[i-1]) + c*cos(a*x[i-1]);
            y[i] = sin(b*x[i-1]) + d*cos(b*y[i-1]);
            }
            // return a new data frame
            return DataFrame::create(_["x"]= x, _["y"]= y);
            }
            ')


#Loops through a set of 10 random images to be exported.

for(i in 1:10) {

#Variables a, b, c and d are floating point values between -3 and +3

a <- runif(1, -3, 3)
b <- runif(1, -3, 3)
c <- runif(1, -3, 3)
d <- runif(1, -3, 3)

#Create the data frame that will be plotted with ggplot
# Number of points, x and y points start at .1, a and b set above 
#createTrajectory(n, x0, y0, a, b)
dat = createTrajectory(4000000, .1, .1, a, b, c, d)

#find previous clipping code if interested in cropping the image

#plot
p <- ggplot(dat, aes(x, y)) + 
  geom_point(shape=46, alpha=.01) + 
  theme(legend.position  = "none",
        panel.background = element_rect(fill="#f3f3e6"), 
        axis.ticks       = element_blank(),
        panel.grid       = element_blank(),
        axis.title       = element_blank(),
        axis.text        = element_blank())

ggsave(filename = paste0("clifford"," a ",round(a,3)," b ",round(b, 3)," c ",round(c, 3)," d ",round(d, 3),".png"), 
       p, 
       device = "png", 
       width = 10,
       height = 10,
       units = c("in"),)

}

