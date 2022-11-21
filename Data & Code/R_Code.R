
#Installing Packages
install.packages("tidyverse")

install.packages("lubridate")


#Loading Packages
library(tidyverse)

library(lubridate)


#Importing the dataset
car_details_cleaned <- read_csv("car_details_cleaned.csv")


#Exploring the dataset
View(car_details_cleaned)

head(car_details_cleaned)

glimpse(car_details_cleaned) #There are 8 columns and 4340 rows in the dataset

is_null(car_details_cleaned) # No null values


# Seperating the first word in column 'name' to another column 'brand'
car_details_cleaned$brand <- word(car_details_cleaned$name, 1)

View(car_details_cleaned)


# Data Visualization
table(car_details_cleaned$brand)

ggplot(car_details_cleaned) +
  geom_bar(mapping = aes(x=brand)) +
  theme(axis.text.x = element_text(angle = 90))


install.packages("plotly") 

library(plotly) # Installing 'plotly' package to visualize data in pie chart


no_of_cars <- table(car_details_cleaned$brand)


plot_ly(data = car_details_cleaned, labels = ~brand, values = ~no_of_cars,
        type = 'pie',
        marker = list(colors=colors, line = list(brand = "white", width = 2))) %>% 
  layout(title = "No. of Car by Brands")




table(car_details_cleaned$year)

ggplot(car_details_cleaned) +
  geom_bar(mapping = aes(x=year), fill = "black")


no_of_cars_year_wise <- table(car_details_cleaned$year)

plot_ly(data = car_details_cleaned, labels = ~year, values = ~no_of_cars_year_wise,
        type = 'pie',
        marker = list(colors=colors, line = list(brand = "white", width = 2))) %>% 
  layout(title = "Year Wise Distribution")




table(car_details_cleaned$fuel)

fuel_type <- table(car_details_cleaned$fuel)

plot_ly(data = car_details_cleaned, labels = ~fuel, values = ~fuel_type,
        type = 'pie',
        marker = list(colors=colors, line = list(brand = "white", width = 2))) %>% 
  layout(title = "Fuel Type")





table(car_details_cleaned$owner)

ggplot(car_details_cleaned) +
  geom_bar(mapping = aes(x=owner), fill = "dark blue") +
  theme(axis.text.x = element_text(angle = 90))


ownership_type <- table(car_details_cleaned$owner)

plot_ly(data = car_details_cleaned, labels = ~owner, values = ~ownership_type,
        type = 'pie',
        marker = list(colors=colors, line = list(brand = "white", width = 2))) %>% 
  layout(title = "Ownership Type")





table(car_details_cleaned$seller_type)

ggplot(car_details_cleaned) +
  geom_bar(mapping = aes(x=seller_type), fill = "maroon")


seller_type_count <- table(car_details_cleaned$seller_type)

plot_ly(data = car_details_cleaned, labels = ~seller_type, values = ~seller_type_count,
        type = 'pie',
        marker = list(colors=colors, line = list(brand = "white", width = 2))) %>% 
  layout(title = "Seller Type")






table(car_details_cleaned$transmission)

ggplot(car_details_cleaned) +
  geom_bar(mapping = aes(x=transmission), fill = "wheat3")


transmission_type <- table(car_details_cleaned$transmission)

plot_ly(data = car_details_cleaned, labels = ~transmission, values = ~transmission_type,
        type = 'pie',
        marker = list(colors=colors, line = list(brand = "white", width = 2))) %>% 
  layout(title = "Transmission Type")





ggplot(car_details_cleaned, 
       aes(x = year, fill = transmission)) + 
  geom_bar(position = "dodge") +
  labs(title = "Transmission Type Yearly")



ggplot(car_details_cleaned, 
       aes(x = year, y = fuel, fill = transmission)) +
  geom_violin() +
  labs(title = "Transmission Type by Fuel Category")



ggplot(car_details_cleaned) + 
  aes(x = brand, fill = transmission) + 
  geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Transmission Type by Brand")



ggplot(car_details_cleaned) + 
  aes(x = brand, fill = owner) + 
  geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Transmission Type by Onwership")




library(dplyr)

ggplot(car_details_cleaned) +
  aes(x = year, y = brand) +
  geom_point(size = 1)




ggplot(car_details_cleaned) +
  aes(x = brand, y = km_driven) +
  geom_boxplot(fill = "003300") +
  theme(axis.text.x = element_text(angle = 90))


