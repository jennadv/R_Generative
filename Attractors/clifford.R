library(Rcpp)
library(tidyverse)


#  Clifford - softology ---------------------------------------------------------------


# The formula looks like this originally:

#xnew=sin(a*y)+c*cos(a*x)
#ynew=sin(b*x)+d*cos(b*y)

#Compare to the OG formula and you'll see that 
#the code uses the previous point as a reference for each new point
#Here we define the start points and constants, then use them as input for an Rcpp loop 
#where we have our attractor functions. This makes an output dataframe with the 
#x and y coordinates of our points, which we then plot using ggplot.
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


#Variables a and b are floating point values between -3 and +3
#Clifford 1 - two circles
# a = -1.7
# b = 1.8
# c = -1.9
# d = -0.4

#Clifford 2 - cleaner, more clear lines
a = -1.7
b = 1.3
c = -0.1
d = -1.21

#Create the data frame that will be plotted with ggplot
# Number of points, x and y points start at .1, a and b set above 
#createTrajectory(n, x0, y0, a, b)
dat = createTrajectory(4000000, .1, .1, a, b, c, d)

#find previous clipping code if interested in cropping the image

#plot
ggplot(dat, aes(x, y)) + 
  geom_point(shape=46, alpha=.01) + 
  theme(legend.position  = "none",
        panel.background = element_rect(fill="#f3f3e6"), 
        axis.ticks       = element_blank(),
        panel.grid       = element_blank(),
        axis.title       = element_blank(),
        axis.text        = element_blank())

ggsave("clifford2.png", device = "png", 
       width = 10,
       height = 10,
       units = c("in"),)
