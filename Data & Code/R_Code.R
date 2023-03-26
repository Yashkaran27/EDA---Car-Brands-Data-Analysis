
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
glimpse(car_details_cleaned) # There are 8 columns and 4340 rows in the dataset
is_null(car_details_cleaned) # No null values


# Separating the first word in column 'name' to another column 'brand'
car_details_cleaned$brand <- word(car_details_cleaned$name, 1)
View(car_details_cleaned)



# Data Visualization
table(car_details_cleaned$brand)

# car count by brand
ggplot(car_details_cleaned) +
  geom_bar(mapping = aes(x=brand)) +
  theme(axis.text.x = element_text(angle = 90))

install.packages("plotly") 

library(plotly) # Installing 'plotly' package to visualize data in pie chart

no_of_cars <- table(car_details_cleaned$brand)

# % of cars by brand
plot_ly(data = car_details_cleaned, labels = ~brand, values = ~no_of_cars,
        type = 'pie',
        marker = list(colors=colors, line = list(brand = "white", width = 2))) %>% 
  layout(title = "No. of Car by Brands")



table(car_details_cleaned$year)

# car count by year
ggplot(car_details_cleaned) +
  geom_bar(mapping = aes(x=year), fill = "black")

no_of_cars_year_wise <- table(car_details_cleaned$year)

# % of cars by year
plot_ly(data = car_details_cleaned, labels = ~year, values = ~no_of_cars_year_wise,
        type = 'pie',
        marker = list(colors=colors, line = list(brand = "white", width = 2))) %>% 
  layout(title = "Year Wise Distribution")



table(car_details_cleaned$fuel)

fuel_type <- table(car_details_cleaned$fuel)

# % of cars by fuel type
plot_ly(data = car_details_cleaned, labels = ~fuel, values = ~fuel_type,
        type = 'pie',
        marker = list(colors=colors, line = list(brand = "white", width = 2))) %>% 
  layout(title = "Fuel Type")



table(car_details_cleaned$owner)

# car count by owner
ggplot(car_details_cleaned) +
  geom_bar(mapping = aes(x=owner), fill = "dark blue") +
  theme(axis.text.x = element_text(angle = 90))

ownership_type <- table(car_details_cleaned$owner)

# % of cars by owner
plot_ly(data = car_details_cleaned, labels = ~owner, values = ~ownership_type,
        type = 'pie',
        marker = list(colors=colors, line = list(brand = "white", width = 2))) %>% 
  layout(title = "Ownership Type")



table(car_details_cleaned$seller_type)

# car count by seller type
ggplot(car_details_cleaned) +
  geom_bar(mapping = aes(x=seller_type), fill = "maroon")

seller_type_count <- table(car_details_cleaned$seller_type)

# % of cars by seller type
plot_ly(data = car_details_cleaned, labels = ~seller_type, values = ~seller_type_count,
        type = 'pie',
        marker = list(colors=colors, line = list(brand = "white", width = 2))) %>% 
  layout(title = "Seller Type")



table(car_details_cleaned$transmission)

# car count by transmission type
ggplot(car_details_cleaned) +
  geom_bar(mapping = aes(x=transmission), fill = "wheat3")

transmission_type <- table(car_details_cleaned$transmission)

# % of cars by transmission type
plot_ly(data = car_details_cleaned, labels = ~transmission, values = ~transmission_type,
        type = 'pie',
        marker = list(colors=colors, line = list(brand = "white", width = 2))) %>% 
  layout(title = "Transmission Type")



# transmission type by year
ggplot(car_details_cleaned, 
       aes(x = year, fill = transmission)) + 
  geom_bar(position = "dodge") +
  labs(title = "Transmission Type Yearly")



# transmission type by fuel category and year
ggplot(car_details_cleaned, 
       aes(x = year, y = fuel, fill = transmission)) +
  geom_violin() +
  labs(title = "Transmission Type by Fuel Category")



# transmission type by brand
ggplot(car_details_cleaned) + 
  aes(x = brand, fill = transmission) + 
  geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Transmission Type by Brand")



# ownership type by brand
ggplot(car_details_cleaned) + 
  aes(x = brand, fill = owner) + 
  geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Ownership type by Brand")


library(dplyr)

# brand sold in each year
ggplot(car_details_cleaned) +
  aes(x = year, y = brand, color = year) +
  geom_point(size = 1) +
  xlim(2000, 2020) +
  labs(title = "Brand sold in each year")



# KM driven by brand
ggplot(car_details_cleaned) +
  aes(x = brand, y = km_driven) +
  ylim(0,300000) +
  geom_boxplot(fill = "#009999") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "KMs driven by brand")



# Selling price range by brands
ggplot(car_details_cleaned) +
  geom_line(mapping = aes(x=brand, y=selling_price), col = "#660000") +
  ylim(0, 2500000) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Selling price range by brands")



# Selling price range by brands and transmission type
ggplot(car_details_cleaned) +
  geom_line(mapping = aes(x=selling_price, y=brand, color = transmission)) +
  xlim(0, 2500000) +
  labs(title = "Selling price range by brand & transmission type")



# Selling price by Km driven
ggplot(car_details_cleaned) +
  geom_line(mapping = aes(x=km_driven, y=selling_price), size = .3) +
  geom_smooth(mapping = aes(x=km_driven, y=selling_price)) +
  xlim(0, 200000) +
  ylim(0, 2000000) +
  labs(title = "Selling price by KMs driven")



# Selling price by Km driven and transmission type
ggplot(car_details_cleaned) +
  geom_line(mapping = aes(x=km_driven, y=selling_price), 
            col = "#663300",size = 0.3) +
  geom_smooth(mapping = aes(
    x=km_driven, y=selling_price, color = transmission)) +
  xlim(0, 200000) +
  ylim(0, 2000000) +
  labs(title = "Selling price by KMs driven & transmission type")

# End of code
