# Importing required libraries ###########################
library(dplyr)
library(lubridate)
library(plotly)



#### QUESTION 1 ###################################################################
##	Provide the list of themes available across all data sources

## prove that claim id and theme id are same######################
# vector of all the themes
unique_themes_key = unique(Theme_list$CLAIM_ID)

#unique themes in social media data
unique_themes_smd = unique(social_media_data$THEME_ID)

# unique themes in google search data
unique_themes_gsd = unique(google_search_data$CLAIM_ID)

# unique themes in theme product list
unique_themes_tpl = unique(Theme_product_list$CLAIM_ID)

# check whether all themes of individual sources are present in list of themes or not
check1 = sapply(unique_themes_smd, function(x){
  x %in% unique_themes_key
})
check2 = sapply(unique_themes_gsd, function(x){
  x %in% unique_themes_key
})
check3 = sapply(unique_themes_tpl, function(x){
  x %in% unique_themes_key
})

sum(check1) == length(unique_themes_smd) # TRUE
sum(check2) == length(unique_themes_gsd) # TRUE
sum(check3) == length(unique_themes_tpl) # TRUE

# we can observe that all the Theme_id's present in social_media_data are present in Theme_List
# And it is same for google_search_data and theme_product_list with Claim_id's
# Hence we can conclude both Theme_id and Claim_id columns are same.

## Merging required data frames ################
# Merging Theme_list and Theme_product_list on CLAIM_ID
Theme_product_list_merged = merge(Theme_product_list, Theme_list, by = "CLAIM_ID", all.x = TRUE)

# Merging product_manufacturer_list and Theme_product_list_merged on PRODUCT_ID
product_manufacturer_theme_list = merge(product_manufacturer_list, Theme_product_list_merged, by = 'PRODUCT_ID',all.x = TRUE)

# Merging product manufacturer list to sales data on PRODUCT_ID
sales_data_merged = merge(product_manufacturer_theme_list, sales_data, by = "PRODUCT_ID", all.x = TRUE)

nrow(sales_data[sales_data$PRODUCT_ID == 543, ]) # 132
nrow(sales_data_merged[sales_data_merged$PRODUCT_ID == 543, ]) # 924
# Reason for increase in number of rows in merged data is :
# -> for product_id : 543 we have 7 different CLAIM_ID'S
# -> So in our result we got 132 * 7 = 924 rows.

summary(sales_data_merged)
# we can observe there are same number of NA(s) in all columns

t = sales_data_merged[is.na(sales_data_merged$sales_dollars_value), ]
View(t)
# we can see 31803 rows do not have any values.

# So removing those rows.
sales_data_merged = sales_data_merged[!is.na(sales_data_merged$sales_dollars_value), ]

## unique themes across sales_data ######################
unique_themes_sd = unique(sales_data_merged$CLAIM_ID)

## Unique themes across all data sources #############
unique_themes_all = data.frame(
  CLAIM_ID = unique(c(unique_themes_smd, unique_themes_gsd, unique_themes_sd))
)

unique_themes_all = merge(Theme_list,unique_themes_all, by = "CLAIM_ID", all.y = TRUE)




#### QUESTION 2 ###################################################################
##	Understands consumer preference(themes) available in each data source

#social media data
consumer_pref_smd = head(arrange(social_media_data, desc(total_post)), 50)
consumer_pref_smd = unique(consumer_pref_smd$THEME_ID)

#google search data
consumer_pref_gsd = head(arrange(google_search_data, desc(searchVolume)), 50)
consumer_pref_gsd = unique(consumer_pref_gsd$CLAIM_ID)

#sales data
consumer_pref_sd = head(arrange(sales_data_merged, desc(sales_units_value)), 50)
consumer_pref_sd = unique(consumer_pref_sd$CLAIM_ID)

consumer_pref_themes_all = data.frame(
  CLAIM_ID = unique(c(consumer_pref_smd, consumer_pref_gsd, consumer_pref_sd))
)

consumer_pref_themes_all = merge(Theme_list, consumer_pref_themes_all, by = "CLAIM_ID", all.y = TRUE)





#### QUESTION 4 ###################################################################
##	Recommend the time granularity (Daily/Weekly/Monthly/Quarterly/Yearly) for the analysis


# modifying all source data (sales_data, social_media_data, google_search_data) ###########
# Sales_data :
sales_data_merged$system_calendar_key_N = as.character(sales_data_merged$system_calendar_key_N)
sales_data_merged$system_calendar_key_N = as.Date(sales_data_merged$system_calendar_key_N, format='%Y%m%d')

# dividing date into year, month and week for easy analysis
sales_data_merged$year = year(sales_data_merged$system_calendar_key_N)
sales_data_merged$month = month(sales_data_merged$system_calendar_key_N)
sales_data_merged$week = isoweek(sales_data_merged$system_calendar_key_N)

# No need of system_calendar_key_N since we have year, month, week values with us.
# So removing that column.
sales_data_merged$system_calendar_key_N = NULL

# Since we are having lots of rows which will not give appropriate results for us.
# We are going to group sales_data based on product_id , week, month, year
# And replace remaining values with its mean.Because it will not cause more deviation in values.
# since we are getting average of a week data.

sales_data_merged_weekly <- sales_data_merged %>%
  group_by(PRODUCT_ID, year, month, week) %>%
  summarise(across(everything(), mean))


# social_media_data:
social_media_data$published_date = as.character(social_media_data$published_date)
social_media_data$published_date = as.Date(social_media_data$published_date, format = '%Y%m%d')

# dividing date into year, month and week for easy analysis
social_media_data$year = year(social_media_data$published_date)
social_media_data$month = month(social_media_data$published_date)
social_media_data$week = isoweek(social_media_data$published_date)

# No need of published_date since we have year, month, week values with us.
# So removing that column.
social_media_data$published_date = NULL

# Since we are having lots of rows which will not give appropriate results for us.
# We are going to group social_media_data based on product_id , week, month, year
# And replace remaining values with its mean.Because it will not cause more deviation in values.
# since we are getting average of a week data.

social_media_data_weekly <- social_media_data %>%
  group_by(PRODUCT_ID, year, month, week) %>%
  summarise_all(mean)

# google_search_data:





