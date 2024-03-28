# Importing required libraries ###########################
library(readr)
library(readxl)
library(dplyr)
library(ggplot2)

# Import data frames ######################################
google_search_data <- read_csv("Data/google_search_data.csv")
product_manufacturer_list <- read_csv("Data/product_manufacturer_list.csv")
sales_data <- read_csv("Data/sales_data.csv")
social_media_data <- read_excel("Data/social_media_data.xlsx")
Theme_list <- read_csv("Data/Theme_list.csv")
Theme_product_list <- read_csv("Data/Theme_product_list.csv")


# Analyzing data frames ######################################

## Column names of data ################
## To find out relations between datasets
colnames(google_search_data)
colnames(social_media_data)
colnames(sales_data)

# observed few unnamed columns in product_manufacturer_list
colnames(product_manufacturer_list)
colnames(Theme_list)
colnames(Theme_product_list)

## Dimensions of all data frames ######################
dim(google_search_data)
dim(social_media_data)
dim(sales_data)

dim(product_manufacturer_list)
dim(Theme_list)
dim(Theme_product_list)


#### QUESTION 3 ###################################################################
## Provide a report for data sufficiency, sparsity and anomalies in each data source

## Further analysis on sales data ##############

## Renaming required columns ############
colnames(sales_data)[2] = "PRODUCT_ID"

#NA values
colSums(is.na(sales_data))

# observed min values of sales_dollars_value and sales_lbs_value as "0"
summary(sales_data)

### finding how many rows have sales_dollar_value as 0
nrow(sales_data[sales_data$sales_dollars_value == 0, ])
# 17662 number of rows are having sales_dollar_value as 0

### finding how many rows have sales_lbs_value as 0
nrow(sales_data[sales_data$sales_lbs_value == 0, ])
# 53720 number of rows are having sales_dollar_value as 0

### finding how many rows have both sales_dollar_value and sales_lbs_value as 0
nrow(sales_data[sales_data$sales_dollars_value == 0 & sales_data$sales_lbs_value == 0, ])
# 8165 number of rows are having sales_dollar_value and sales_lbs_value as 0

### removing rows with sales_dollars_value equals to 0
sales_data = sales_data[!(sales_data$sales_dollars_value == 0), ]

### removing rows with sales_lbs_value equals to 0
sales_data = sales_data[!(sales_data$sales_lbs_value == 0), ]

### finding variance and standard deviation for sales_dollar_value
var(sales_data$sales_dollars_value) # 6192082938
sd(sales_data$sales_dollars_value) # 78689.79

### finding variance and standard deviation for sales_lbs_value
var(sales_data$sales_lbs_value) # 16440973968
sd(sales_data$sales_lbs_value) # 128222.4

### finding variance and standard deviation for sales_units_value
var(sales_data$sales_units_value) # 730937798
sd(sales_data$sales_units_value) # 27035.86

# Observed high variance and standard deviation for sales_dollars_value, sales_lbs_value and sales_units_value :
# -> data points are dispersed widely from the mean.
# -> There is a higher degree of uncertainty or unpredictability in the dataset.
# -> High chances of outliers.
# -> possibility of skewness.

### Plots to get a better view on data
ggplot(sales_data, aes(y = sales_dollars_value))+
  geom_boxplot(fill = "skyblue", color = "darkblue") +
  labs(title = "Boxplot of Column sales_dollar_value", y = "Values")

ggplot(sales_data, aes(y = sales_lbs_value))+
  geom_boxplot(fill = "skyblue", color = "darkblue") +
  labs(title = "Boxplot of Column sales_lbs_value", y = "Values")

ggplot(sales_data, aes(y = sales_units_value))+
  geom_boxplot(fill = "skyblue", color = "darkblue") +
  labs(title = "Boxplot of Column sales_units_value", y = "Values")

# Unable to Interpret from histogram properly So normalizing them
ggplot(sales_data, aes(y = log(sales_dollars_value)))+
  geom_boxplot(fill = "skyblue", color = "darkblue") +
  labs(title = "Boxplot of normalized Column sales_dollar_value", y = "Values")

ggplot(sales_data, aes(y = log(sales_lbs_value)))+
  geom_boxplot(fill = "skyblue", color = "darkblue") +
  labs(title = "Boxplot of normalized Column sales_lbs_value", y = "Values")

ggplot(sales_data, aes(y = log(sales_units_value)))+
  geom_boxplot(fill = "skyblue", color = "darkblue") +
  labs(title = "Boxplot of normalized Column sales_units_value", y = "Values")




## Further analysis on google_search_data #############

## Renaming required columns ############
colnames(google_search_data)[4] = "CLAIM_ID"

summary(google_search_data)

# observed 218511 NA values in Theme Id column of social_media_data
colSums(is.na(google_search_data))

nrow(google_search_data[is.na(google_search_data$CLAIM_ID), ]) #0
table(google_search_data$platform)
# Frequency google > amazon > chewy > walmart

# checking year
table(google_search_data$year_new)
table(google_search_data$year_new,google_search_data$week_number)
# 2014 = upto 52
# 2015 = upto 52
# 2016 = except 52
# 2017 = upto 52
# 2018 = upto 52
# 2019 = upto 40

# Finding variance and standard deviation
# SearchVolume
var(google_search_data$searchVolume) # 126586693
sd(google_search_data$searchVolume) # 11251.08

### Plots to get a better view on data
ggplot(google_search_data, aes(y = searchVolume))+
  geom_boxplot(fill = "skyblue", color = "darkblue") +
  labs(title = "Boxplot of Column searchVolume", y = "Values")

# Unable to Interpret from histogram properly So normalizing them
ggplot(google_search_data, aes(y = log(searchVolume)))+
  geom_boxplot(fill = "skyblue", color = "darkblue") +
  labs(title = "Boxplot of Column searchVolume", y = "Values")




## Further analysis on product_manufacturer_list ############
summary(product_manufacturer_list)

# observed no values in c(...3 ,...4 , ...5 , ...6 , ...7) columns in product_manufacturer_list
colSums(is.na(product_manufacturer_list))

# removing unnamed columns
product_manufacturer_list$...3 <- NULL
product_manufacturer_list$...4 <- NULL
product_manufacturer_list$...5 <- NULL
product_manufacturer_list$...6 <- NULL
product_manufacturer_list$...7 <- NULL

# Frequency distribution of vendors
vendors_freq <- as.data.frame(table(product_manufacturer_list$Vendor))
names(vendors_freq) <- c("vendor", "Frequency")

ggplot(vendors_freq, aes(x = vendor, y = Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Histogram of vendors", x = "vendor", y = "Frequency")




## Further analysis on social_media_data ###################

# observed min value of total_post as "0"
summary(social_media_data)

## Renaming required columns ############
colnames(social_media_data)[1] = "THEME_ID"

# observed 218511 NA values in Theme Id column of social_media_data
colSums(is.na(social_media_data))

nrow(social_media_data[is.na(social_media_data$`THEME_ID`), ]) #218511

# around 41% of rows are having NA in Theme Id field.
# Since we cannot remove around 41% of whole data and
# As we have separate claim category as no claim we can add these NA values as no claim.
social_media_data$`THEME_ID`[is.na(social_media_data$`THEME_ID`)] = 0

no_posts = social_media_data[social_media_data$total_post == 0, ]
nrow(no_posts) # 87364
length(unique(no_posts$`THEME_ID`)) # 154

# calculate variance and standard deviation for total_post
var(social_media_data$total_post) # 19886049
sd(social_media_data$total_post) # 4459.378

### Plots to get a better view on data
ggplot(social_media_data, aes(y = total_post))+
  geom_boxplot(fill = "skyblue", color = "darkblue") +
  labs(title = "Boxplot of Column total_post", y = "Values")

# Unable to Interpret from histogram properly So normalizing them
# normalizing values of total_post with log is not possible
# Since there are many 0 values in total_post
# Hence we are using min-max scaling for normalizing the total_post column
c = 1e-10
ggplot(social_media_data, aes(y = log(total_post + c)))+
  geom_boxplot(fill = "skyblue", color = "darkblue") +
  labs(title = "Boxplot of Column total_post", y = "Values")



## Further analysis on Theme_list ##############
summary(Theme_list)
colSums(is.na(Theme_list))
length(unique(Theme_list$CLAIM_ID)) # 208
length(unique(Theme_list$`Claim Name`)) # 208
nrow(Theme_list) # 208
# we can conclude claim_id and claim_name are mapped one on one.



## Further analysis on Theme_product_list ###################
summary(Theme_product_list)
colSums(is.na(Theme_product_list))
length(unique(Theme_product_list$PRODUCT_ID)) #57317
length(unique(Theme_product_list$CLAIM_ID)) # 51
nrow(Theme_product_list) #91485
# we can conclude mapping between product_id and claim_id is not one on one. Since there are no NA values also.


