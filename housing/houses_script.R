# Load ggplot2 package and data set
library(ggplot2)
library(dplyr)
houses <- read.delim("/Users/anhvu/Documents/Master/Block 2/R/input/houses.txt", 
  stringsAsFactors = FALSE)
summary(houses)
options(scipen=999)

# Let's dive in!
# Plotting housing price distribution
ggplot() +
  geom_point(data = houses, aes(x = Lot.Area, y = SalePrice), color = "blue")

# Plotting selling price on lot area
ggplot(data = houses, aes(x = Lot.Area, y = SalePrice)) + 
  geom_point(color = 'blue') +
  geom_smooth() + ylab("Selling price") + xlab("Size")

# Plotting on garage type with garage area
ggplot(data = houses, aes(x = Garage.Type, y = Garage.Area)) +
  geom_boxplot(color = 'blue') + xlab("Garage Type") + ylab("Garage Area")

# Plotting on which month of the year were the household sold
ggplot(data = houses, aes(x = Mo.Sold, group = Yr.Sold, color = Yr.Sold)) +
  geom_line(stat = "count") + scale_x_continuous(limits = c(1, 12)) +
  xlab("Month sold") + ylab("Number of houses")

# Plotting selling price with lot area on different building types
ggplot(data = houses, aes(x = Lot.Area, y = SalePrice, color = Bldg.Type, 
  size = Gr.Liv.Area)) + geom_point() +
  theme(legend.position = "bottom", legend.box.background = 
  element_rect(fill = "darkgrey",color = "darkgrey")) 

# Plotting selling price with garagae area and type
ggplot(data = houses, aes(x = Garage.Area, y = SalePrice)) +
  geom_point(position = "jitter") +
  facet_grid(~Garage.Type) +
  theme(panel.spacing=unit(0.03, "lines")) 
