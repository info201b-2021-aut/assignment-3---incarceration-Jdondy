library("tidyverse")
library("usmap")
library("maps")
library("mapdata")
library("mapproj")
library("usdata")
jurisdiction_level <- read.csv("https://github.com/vera-institute/incarceration-trends/blob/master/incarceration_trends.csv?raw=true")

#Which state has the highest black prison admission?
state_highest_adm <- na.omit(jurisdiction_level[which.max(jurisdiction_level$black_prison_adm),4])

#Which county has the highest black prison admission? 
county_highest_adm <- na.omit(jurisdiction_level[which.max(jurisdiction_level$black_prison_adm),5])

#Which state has the lowest black prison admission?
state_lowest_adm <- na.omit(jurisdiction_level[which.min(jurisdiction_level$black_prison_adm),4])

#In what year was the black prison admission the highest?
year_highest_prison_adm <- na.omit(jurisdiction_level[which.max(jurisdiction_level$black_prison_adm),2])

#In what year was the black prison admission the lowest?
year_lowest_prison_adm <- na.omit(jurisdiction_level[which.min(jurisdiction_level$black_prison_adm),2])

d1 <- jurisdiction_level %>%
  group_by(year) %>%
  summarise(bmean = mean(na.omit(black_prison_adm)))

d2 <- jurisdiction_level %>%
  group_by(year) %>%
  summarise(wmean = mean(na.omit(white_prison_adm)))

d3 <- jurisdiction_level %>%
  group_by(year) %>%
  summarise(tmean = mean(na.omit(total_prison_adm))) 

joindata <- na.omit(left_join(d3,d1))

ggplot (d1, aes(x= year, y= bmean)) + 
  geom_point(size = 2, color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  geom_smooth(color = "red") +
  ggtitle("Average Black Prison Admission Over Time") +
  xlab("Time Progressing (Yearly)") +
  ylab("Average Black Prison Admission")

ggplot (d2, aes(x= year, y= wmean)) + 
  geom_point(size = 2, color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  geom_smooth(color = "red") +
  ggtitle("Average White Prison Admission Over Time") +
  xlab("Time Progressing (Yearly)") +
  ylab("Average White Prison Admission")

ggplot (joindata, aes(x= tmean, y= bmean)) + 
  geom_point(size = 2, color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  geom_smooth(color = "red") +
  ggtitle("Average Black Prison Admission per Year vs. Average Total Prison Admission per Year") +
  xlab("Average Total Yearly Prison Admission") +
  ylab("Average Yearly Black Prison Admission")

jurisdiction_level$black_prison_adm[is.na(jurisdiction_level$black_prison_adm)] = 0
burger <- jurisdiction_level %>%
  filter (year == 2016) %>%
  group_by(state) %>%
  summarise(bmean = mean(black_prison_adm))

hot_dog <- jurisdiction_level %>%
  filter (year == 1990) %>%
  group_by(state) %>%
  summarise(bmean = mean(black_prison_adm))

state_shape <- map_data("state")
state_shape$region <- state2abbr(state_shape$region)
state_shape <- state_shape %>%
  rename(state = region) %>%
  left_join(burger, by="state") 
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),       
    axis.text = element_blank(),        
    axis.ticks = element_blank(),     
    axis.title = element_blank(),       
    plot.background = element_blank(),  
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    panel.border = element_blank()      
  )

ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = bmean),
    color = "white", 
    size = .1        
  ) +
  coord_quickmap() +
  scale_fill_continuous(low = "#132B43", high = "Red") +
  labs(fill = "Black Prison Admission") +
  ggtitle("Black Prison Admission Across the US in 2016") +
  blank_theme 


state_shape2 <- map_data("state")
state_shape2$region <- state2abbr(state_shape2$region)
state_shape2 <- state_shape2 %>%
  rename(state = region) %>%
  left_join(hot_dog, by="state") 

ggplot(state_shape2) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = bmean),
    color = "white", 
    size = .1        
  ) +
  coord_quickmap() +
  scale_fill_continuous(low = "#132B43", high = "Red") +
  labs(fill = "Black Prison Admission") +
  ggtitle("Black Prison Admission Across the US in 1990") +
  blank_theme 
