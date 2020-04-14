install.packages('ggplot2')
install.packages('dplyr')
library('dplyr')
library('ggplot2')
library('tidyverse')

options(scipen=999)

data <- read.delim('/Users/anhvu/Downloads/beer_reviews_cleaned.csv', 
                   stringsAsFactors = FALSE, header=TRUE, sep = ',')
data <- na.omit(data)
data[0:5,]

sd(data$beer_quality)

mean(data$beer_quality)

# distribution of beer ratings in general
ggplot(data, aes(x = beer_quality)) + 
    geom_histogram(aes(y = ..density..), binwidth =  0.5, 
    fill = 'light pink', color = 'white') + 
    xlab('Beer quality rating') + ylab('Density') +
    theme(axis.title.y = element_text(margin = unit (c(0, 0.4,0,0), "cm"), size = 12),
        axis.title.x = element_text(margin = unit (c(0.2, 0.5,0.2,0.2), "cm"), size = 12), 
        plot.title = element_text(hjust = 0.5, lineheight = 1.5),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)) +
    geom_density(adjust = 5) + labs(title = 
        "Beer quality ratings distribution (2002 - 2011)") +
    scale_x_continuous(limits=c(1,10), breaks = (1:10))

# boxplot of beer quality ratings over the years

ggplot(data = data, aes(x = factor(year), y = beer_quality)) +
  geom_boxplot(fill = 'light pink', outlier.shape = NA) + 
  xlab('Year') + ylab('') +
  theme(axis.title.y = element_text(margin = unit (c(0, 0.4,0,0), "cm")),
        axis.title.x = element_text(margin = unit (c(0.1, 0.4,0.1,0.1), "cm"), size = 12)) +
  labs(title = "Beer quality range over the years") +
  theme(axis.title.x = element_text(margin = unit (c(0.2, 0.5,0.2,0.2), "cm")),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, lineheight = 1.5)) +
  scale_y_continuous(limits = c(1,10), breaks = c(1:10))

# Number of reviewers vs beer quality ratings
agg <- data %>%
  group_by(review_profilename) %>%
  summarise(average.rating = mean(beer_quality), number.of.reviews = n(),
            avg_per_review = mean(beer_quality)/n())  %>%
  arrange(desc(avg_per_review))
agg <- data.frame(agg)
summary(agg$number.of.reviews)/10
# Number of reviwers vs number of beers
agg_2 <- data %>%
  group_by(review_profilename) %>%
  mutate(avg.beer = n_distinct(beer_name))
agg_2 <- data.frame(agg_2)
  
avg.beer <- mutate()

# What is the beer style that receives most reviews/highest ratings? 

average <- data %>%
  group_by(beer_name) %>%
  summarise(Mean = mean(review_aroma))
average

# Pie chart of top 5 beer styles vs rest
agg_4 <- data %>%
  group_by(beer_style) %>%
  summarise(num_review = n()) %>%
  arrange(desc(num_review))
agg_4 <- data.frame(agg_4)

newrow <- data.frame(beer_style = 'Top 5 beer styles', num_review = sum(agg_4[11:104, 2]))

agg_4[nrow(agg_4) + 1, ] <- newrow
agg_4[105, 1] <- 'Other types of beer'
rows_to_keep <- c(1,2,3,4,5,6, 7, 8 , 9, 10,105)

agg_4 <- agg_4[rows_to_keep,]

agg_4 <- agg_4 %>%
  mutate(perc = num_review/sum(agg_4$num_review))
  
ggplot(data = agg_4, aes(x = "", y = perc, fill = factor(beer_style))) +
  geom_bar(stat ='identity') + 
  xlab ('Beer style') + ylab ('') + 
  labs(title = 'Most-reviewed beer styles in percentage') +
  scale_fill_discrete(name = 'Beer style') + 
  theme(axis.title.x=element_blank(),
        axis.title.y = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.text = element_text(size = 10),
        plot.title = element_text(hjust = 0.5, lineheight = 1.5, vjust = 0.8)) +
  coord_polar('y', start = 0) +
  geom_text(aes(label = paste0(round(perc * 100), "%")), 
            position = position_stack(vjust = 0.3), size = 3)

#breaks = c('American Indian Pale Ale (IPA)', 'American Double / Imperial IPA'
#, 'American Pale Ale (APA)', 'Russian Imperial Stout', 
#'American Double / Imperial Stout', 'Other types of beer')


#What is the trend of consumer tastes over the years?
#What is the top 5 beer for each criterion? x
#What is the top 5 beer with highest quality? x
#What is the brewery with top rated beer? x

agg_5 <- data %>%
  group_by(beer_name) %>%
  summarise(Quality = mean(beer_quality),
            NumberOfReviews = n()) %>%
  filter(Quality > mean(Quality) + 1.5 * sd(Quality)) %>%
  arrange(desc(NumberOfReviews)) 

agg_5_2 <- data %>%
  group_by(beer_name) %>%
  summarise(Taste = mean(review_taste),
            NumberOfReviews = n()) %>%
  filter(Taste > mean(Taste) + 1.5 * sd(Taste)) %>%
  arrange(desc(NumberOfReviews))

agg_5 <- agg_5[1:5,1:3]

agg_5[2,]

#Are popular beers perceived as higher quality beers?
#Are reviewers trustworthy? Do they tend to review popular beers only or do they also reach out to less popular ones?
#What are finalists for our team?

# Some graphs about the reviewers

agg_6 <- data %>%
  group_by(review_profilename) %>%
  summarise(Quality = mean(beer_quality), Number_of_reviews = n(),
            Aroma = mean(review_aroma), Appearance = mean(review_appearance),
            Taste = mean(review_taste), Palate = mean(review_palate)) %>%
  arrange(desc(Number_of_reviews)) %>%
  filter(Number_of_reviews > 10.00)

agg_6 <- data.frame(agg_6)

agg_6[1:3,1]

# a chart that shows the number of breweries and quality over the year
agg_7 <- data %>%
  group_by(year) %>%
  summarise(Alcohol = mean(beer_abv), 
            Quality = mean(beer_quality),
            NumberOfBeers = n_distinct(beer_name), 
            NumberOfBeersStyle = n_distinct(beer_style)) 
agg_7 <- data.frame(agg_7)

colors <- c("Beer names" = "blue", "Beer styles" = "orange", "Alcohol" = 'green')

ggplot(agg_7, aes(x = year)) +
    geom_line(aes(y = NumberOfBeers, color = "Beer names")) +
    geom_line(aes(y=NumberOfBeersStyle, color = "Beer styles")) + 
    labs(x = "Year", y = "", color = "Legend",
         title = "Number of beer names versus number of beer styles (2002 - 2011)") +
    scale_color_manual(values = colors)  +
    theme(axis.title.y = element_text(margin = unit (c(0, 0.4,0,0), "cm"), size = 12),
        axis.title.x = element_text(margin = unit (c(0.2, 0.5,0.2,0.2), "cm"), 
        size = 12),
    plot.title= element_text(size = 12),
    axis.ticks.x) +
    scale_y_continuous(limits = c(0,15000))

# histogram of alcohol %
ggplot(data, aes(x = beer_abv)) + 
  geom_histogram(aes(y = ..density..), binwidth = 1.5, fill = 'light pink'
                 , color = 'white') + ylim(0,0.25) +
  labs(x = "Alcohol percentage", y = "Fraction", 
       title = "Alcohol percentage of the beers") +
  theme(axis.title.y = element_text(margin = unit (c(0, 0.4,0,0), "cm"), size = 12),
        axis.title.x = element_text(margin = unit (c(0.2, 0.5,0.2,0.2), "cm"), size = 12),
        plot.title = element_text(size = 12, lineheight =  1.5, hjust = 0.5)) + 
  geom_density(adjust = 5) + 
  scale_x_continuous(breaks = c(0:15), labels = (0:15), limit = c(0,15))

ggplot(data, aes(x = beer_abv)) + 
  geom_histogram(binwidth = 1.5, fill = 'light pink', color = 'white') +
  labs(x = "Alcohol percentage", y = "Frequency", title = "Alcohol percentage of the beers") +
  theme(axis.title.y = element_text(margin = unit (c(0, 0.4,0,0), "cm"), size = 12),
        axis.title.x = element_text(margin = unit (c(0.2, 0.5,0.2,0.2), "cm"), size = 12),
        plot.title = element_text(size = 12, lineheight =  1.5, hjust = 0.5)) + 
  scale_x_continuous(breaks = c(0:20), labels = (0:20), limit = c(0,20))


agg_8 <- data %>%
  select(beer_name, review_profilename, beer_quality, review_aroma, review_taste
         , review_palate, review_appearance) %>%
  group_by(beer_name) %>%
  summarise(count = n(), Quality = mean(beer_quality), 
      Aroma = mean(review_aroma), Taste = mean(review_taste), 
      Palate =mean(review_palate), Apperance = mean(review_appearance))

agg_8 <- data.frame(agg_8)

ggplot(data = agg_8, aes(x = count, y = Aroma)) +
  geom_point(position = "jitter", color = 'orange') +
  labs(x = 'Number of reviewers', y = 'Beer quality')
source("http://www.sthda.com/upload/rquery_cormat.r")

res <- cor(agg_8[1:104, 3:7], method = 'pearson')

cormat <- round(res, 2)
library(reshape2)
melted_cormat <- melt(cormat)
head(melted_cormat)

get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cormat)
upper_tri

library(reshape2)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Heatmap
library(ggplot2)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  labs(title = 'Correlation between different metrics') +
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 12, lineheight = 1.5, hjust = 1.5))+
  coord_fixed()
# Print the heatmap
print(ggheatmap)


ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

agg_9 <- data %>%
  group_by(beer_style) %>%
  summarise(Quality = mean(beer_quality), num_review = n(), alcohol = mean(beer_abv)) %>%
  arrange(desc(Quality))
agg_9 <- data.frame(agg_9)

newrow_2 <- data.frame(beer_style = 'Top 10 beer styles', Quality = 'NA'
  , num_review = sum(agg_9[11:104, 3]))

agg_9[nrow(agg_9) + 1, ] <- newrow_2
agg_9[105, 1] <- 'Other types of beer'
rows_to_keep <- c(1,2,3,4,5,6, 7, 8 , 9, 10,105)

agg_9 <- agg_9[rows_to_keep,]

agg_9 <- agg_9 %>%
  mutate(perc = num_review/sum(agg_9$num_review) * 100)

sum(agg_9$perc[1:10])
ggplot(data = agg_9, aes(x = "", y = perc, fill = factor(beer_style))) +
  geom_bar(stat ='identity') + 
  xlab ('Beer style') + ylab ('') + 
  labs(title = 'Highest quality beer styles in percentage') +
  scale_fill_discrete(name = 'Beer style') + 
  theme(axis.title.x=element_blank(),
        axis.title.y = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.text = element_text(size = 10),
        plot.title = element_text(hjust = 0.5, lineheight = 1.5, vjust = 0.8)) +
  coord_polar('y', start = 0) +
  geom_text(aes(label= round(perc)*100), 
            position = position_stack(vjust = 0.3), size = 3)

agg_10 <- data[data$beer_quality == 10,]
agg_10 <- agg_10 %>%
  group_by(beer_name) %>%
  summarise(count=n())

agg_11 <- data %>%
  group_by(beer_style) %>%
  summarise(mean = mean(beer_abv))

agg_12 <- data %>%
  group_by(year) %>%
  summarise(Quality = mean(beer_quality), Aroma = mean(review_aroma),
            Apperance = mean(review_appearance), Taste = mean(review_taste),
            Palate = mean(review_palate))

ggplot(data = data, aes(x = review_appearance, y = beer_quality)) +
  geom_point(position = "jitter") +
  facet_grid(~year) +
  theme(panel.spacing=unit(0.03, "lines"))

agg_13 <- data %>%
  group_by(beer_name, beer_abv) %>%
  summarise(Quality = mean(beer_quality), Aroma = mean(review_aroma),
            Apperance = mean(review_appearance), Taste = mean(review_taste),
            Palate = mean(review_palate), count = n(), year = min(year)) %>%
  filter (count < 500, beer_abv > 9.0)

