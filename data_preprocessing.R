library(lubridate)
library(plotly)

####################################### Data preprocessing ########################################
## Renaming required columns ############
colnames(sales_data)[2] = "PRODUCT_ID"
colnames(google_search_data)[4] = "CLAIM_ID"
colnames(social_media_data)[1] = "Theme_Id"

## prove that claim id and theme id are same######################

# vector of all the themes
unique_themes_key = unique(Theme_list$CLAIM_ID)

# Note : NA in claim_id/Theme_id are considered as no claim with claim_id/Theme_id  = 0.
# since these NaN value belongs to No claims which we have under Claim names category

#unique themes in social media data
unique_themes_smd = unique(social_media_data$Theme_Id)

# unique themes in google search data
unique_themes_gsd = unique(google_search_data$CLAIM_ID)

# check whether all themes of individual sources are present in list of themes or not
check1 = sapply(unique_themes_smd, function(x){
  x %in% unique_themes_key
})
check2 = sapply(unique_themes_gsd, function(x){
  x %in% unique_themes_key
})

sum(check1) == length(unique_themes_smd) # TRUE
sum(check2) == length(unique_themes_gsd) # TRUE

# we can observe that all the Theme_id's present in social_media_data are present in Themes of Theme_List
# And it is same in the case of google_search_data with Claim_id's
# Hence we can conclude both Theme_id and Claim_id columns are same and we can consider it as same.

## Merging required data frames ################

# Merging Theme_list and Theme_product_list on CLAIM_ID
Theme_product_list_merged = merge(Theme_product_list,Theme_list, by = "CLAIM_ID", all.x = TRUE)

# Merging product_manufacturer_list and Theme_product_list_merged on PRODUCT_ID
product_manufacturer_theme_list = merge(product_manufacturer_list, Theme_product_list_merged, by = 'PRODUCT_ID',all.x = TRUE)

## unique themes across sales_data ######################

# We are merging product manufacturer list to yearly sales data for only those product id's
# for which we have vendor information and theme/claim information, that's why LEFT JOIN
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

unique_themes_sd = unique(sales_data_merged$CLAIM_ID)

## Unique themes across all data sources #############
unique_themes_all = data.frame(
  CLAIM_ID = unique(c(unique_themes_smd, unique_themes_gsd, unique_themes_sd))
)

unique_themes_all = merge(Theme_list,unique_themes_all, by = "CLAIM_ID", all.y = TRUE)


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
  summarise_all(mean)

# observing sales data for a year after grouping data
# 2016 data :
# temp_sales_data = sales_data[1:52, ]
#
# # Create a ggplot object
# p <- ggplot(temp_sales_data, aes(x = week, y = sales_dollars_value, text = paste("Week :",week))) +
#   geom_line(aes(group = 1)) +
#   geom_point(color = "blue") +
#   labs(x = "Month", y = "Sales", title = "Weekly Sales Trend") +
#   theme_minimal()
#
# p <- ggplotly(p, tooltip = "text")
# # Print the plot
# print(p)

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





