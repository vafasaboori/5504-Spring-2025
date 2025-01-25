# Code Folding Shortcuts----
# Collapse — Option+Command+L or Alt+L for PC
# Expand — Shift+Option+Command+L or Shift+Alt+L for PC
# Collapse All — Option+Command+O or Alt+O for PC
# Expand All — Shift+Option+Command+O or Shift+Alt+O for PC

# Clear the Environment ----
rm(list = ls())

# ggplot2 is one of the core members of the tidyverse ---- 
install.packages("tidyverse")
library(tidyverse)

# mpg data frame found in ggplot2 ----
library(ggplot2)
ggplot2::mpg
str(mpg)
?mpg # or 
help(mpg)

# To plot mpg, we put displ on the x-axis and hwy on the y-axis ----
ggplot(mpg, aes(x = displ, y = hwy)) + # Canvas (Coordinates)
    geom_point()

# Alternate method
mpg %>% ggplot(aes(x = displ, y = hwy)) + #shortcut for pipe shift + command + m 
  geom_point()

# Question! ----
# A group of points fall outside of the linear trend with higher mileage
# How can you explain these cars?

# Answer ----
## map the colors of points to the class variable to reveal the class of each car
ggplot(mpg, aes(x = displ, y = hwy, color = class)) + 
  geom_point()
# 2 seater cars are small despite large engine which results in high MPG

# alpha aesthetic controls the transparency of the points
ggplot(mpg, aes(x = displ, y = hwy, alpha = class)) + 
  geom_point()

# shape aesthetic controls the shape of the points
ggplot(mpg, aes(x = displ, y = hwy, shape = class)) + 
  geom_point()
# What happened to the SUVs? 
# ggplot2 will only use six shapes at a time. 
# By default, additional groups will go unplotted when you use the shape

# set the aesthetic properties of your geom manually. all points blue
ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point(color = "blue")
# color doesn’t convey info about a variable, only changes the appearance

# Class Exercise! ----
# What’s gone wrong with this code? Why are the points not blue?
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = "blue"))

# Answer ----
# "blue" within aes() is treated as a categorical variable (factor)
# ggplot2 assigns different colors (1st red) to levels of categorical variable

# split your plot into facets (subplots) ----
# each facet display one subset of the data
ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point() + 
  facet_wrap(~ class, nrow = 2)

# facet plot on the combination of two variables add facet_grid()
ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point() + 
  facet_grid(drv ~ cyl)
    
# Class Exercise! ----
# What do the empty cells in plot with facet_grid(drv ~ cyl) mean?

# Answer -----
# combinations of drv and cyl that have no observations

# Geometric Objects: smooth geom, a smooth line fitted to the data ----
ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_smooth()

# different line type for each unique value of the mapped variable
ggplot(mpg, aes(x = displ, y = hwy, linetype = drv)) + 
  geom_smooth()

ggplot(mpg, aes(x = displ, y = hwy, linetype = class)) + 
  geom_smooth()

# or color
ggplot(mpg, aes(x = displ, y = hwy, color = drv)) + 
  geom_smooth()

# display multiple geoms in the same plot
ggplot(data = mpg, aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth()

# add color manually
ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point(color ="blue") + 
  geom_smooth(color = "red")

# different color and a separate trend-line for each class
ggplot(data = mpg, aes(x = displ, y = hwy, color = class)) + 
  geom_point() + 
  geom_smooth()

# same graph without standard error 
ggplot(data = mpg, aes(x = displ, y = hwy, color = class)) + 
  geom_point() + 
  geom_smooth(se = FALSE)

# different color for each class with a single trend-line for entire data set
ggplot(data = mpg, aes(x = displ, y = hwy)) + 
  geom_point(aes(color = class)) + 
  geom_smooth(se = FALSE)

# Geometric Objects: Scales, let's start with a simple graph ----
ggplot(mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_point()

# let's reverse scales
ggplot(mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_point() +
  scale_x_reverse() +
  scale_y_reverse()

# Reversing the scales essentially flips the plot:
#	The x-axis runs from high to low instead of low to high.
#	The y-axis also runs from high to low.

# Why Reverse Scales?
# Reversing scales can be useful in certain scenarios:
# Example: Plotting a ranking system, such as countries ranked by GDP per capita
## The top-ranked country should appear at the top of the y-axis

# Color Sclase
# We can use palette from the RColorBrewer package
ggplot(mpg, mapping = aes(x = displ, y = hwy, color = class)) + 
  geom_point() +
  scale_color_brewer() # Applies a Brewer color palette to the color scale. 
# distinguishable color scheme for the levels of the "class" variable.

# Color Palettes
ggplot(mpg, mapping = aes(x = displ, y = hwy, color = class)) + 
  geom_point() +
  scale_color_brewer(palette = "Set1") 
  
# Bar Graphs and histograms
# bar graph based on a categorical variable (e.g. class) ----
ggplot(mpg, aes(x = class)) + 
  geom_bar()
# count is a new value obtained through statistical transformation

# Bar graph with flipped bars
ggplot(mpg, aes(x=class)) +
  geom_bar() +
  coord_flip()

# create a bar graph based on a continuous variable (e.g. hwy)
ggplot(mpg, aes(x = hwy)) + 
  geom_histogram()

# create a stacked bar graph for class var. separated by drv var. (categorical)
ggplot(mpg, aes(x = class, fill = drv)) + 
  geom_bar()

# Limit X or Y Axis
ggplot(mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point() + 
  geom_smooth() +
  coord_cartesian(xlim=c(0,4.5)) # Cartesian Coordinate System
  
# Labels and Annotations ----
ggplot(mpg, aes(x = displ, y = hwy, color = class)) +
  geom_point() +
  labs(title = "Fuel Efficiency by Engine Volume",
       subtitle = "Fuel Economy Data from 1998 to 2008 for 38 Popular Cae Models",
       x = "Engine Volume (Liters)",
       y = "Fuel Efficiency (Miles per Gallon",
       color = "Car Type")

# Facet Wrap creates subplots based on one categorical variable
ggplot(mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point() + 
  facet_wrap(~ cyl)

# Facet Grid creates subplots based on two categorical variable
ggplot(mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point() + 
  facet_grid(cyl ~ fl)

# ctrl + shift + 6 (Maximize Plot Window)

# Class Exercise! ----
# Create a faceted plot to compare the relationship between (displ) and (cty) 
# across different vehicle types (class). 
# Use facets to create subplots for each vehicle type.

# Answer ----
ggplot(mpg, aes(x = displ, y = cty)) +
  geom_point() +
  facet_wrap(~ class, scales = "free") +
  labs(title = "Relationship Between Engine Displacement and City MPG by Vehicle Type",
       x = "Engine Displacement",
       y = "City MPG") +
  theme_minimal()
# Other themes include theme_bw(), theme_classic(), theme_void()