library(Rcpp)
library(tidyverse)



# First Try - Bedhead -----------------------------------------------------


#BEDHEAD - from Will Chase originally, also used Softology blog 
# The formula looks like this originally:
# xnew=sin(x*y/b)*y+cos(a*x-y)
# ynew=x+sin(y)/b
#Compare to the OG formula and you'll see that 
#the code uses the previous point as a reference for each new point
#Here we define the start points and constants, then use them as input for an Rcpp loop 
#where we have our attractor functions. This makes an output dataframe with the 
#x and y coordinates of our points, which we then plot using ggplot.
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

#Variables a and b are floating point values between -1 and +1

#Bedhead 1, 2
a = -.8
b = -.2

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

ggsave(filename = paste0("bedhead"," a ",a," b ",b,".png"), p, device = "png", 
       width = 10,
       height = 10,
       units = c("in"),)

#Try adding "ratchet" parameter
#Try adding color


