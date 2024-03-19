# Data preprocessing ###############
# prove that claim id and theme id are same######################

# vector of all the themes
unique_themes_key = unique(Theme_list$CLAIM_ID)

# Note : NA in claim_id/Theme_id are considered as no claim with claim_id/Theme_id  = 0.
# since these NaN value belongs to No claims which we have under Claim names category

#unique themes in social media data
colnames(social_media_data)[1] = "Theme_Id"
unique_themes_smd = unique(social_media_data$Theme_Id)
unique_themes_smd = replace(unique_themes_smd,is.na(unique_themes_smd),0)

# unique themes in google search data
unique_themes_gsd = unique(google_search_data$Claim_ID)
unique_themes_gsd = replace(unique_themes_gsd,is.na(unique_themes_gsd),0)

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

# List of Themes across data sources ##################
# We have unique themes in social_media_data and google_search_data already found out.
# Unique themes in sales_data :
# Merging Theme_list and Theme_product_list on CLAIM_ID
Theme_product_list_merged = merge(Theme_product_list,Theme_list, by = "CLAIM_ID", all.x = TRUE)



# Merging data frames with common columns #################


# Merging Theme_product_list_merged and product_manufacturer_list on PRODUCT_ID
product_manufacturer_theme_list = merge(product_manufacturer_list, Theme_product_list_merged, by = "PRODUCT_ID", all.x = TRUE )
# number of rows has been increased because one PRODUCT_ID can have multiple CLAIM_ID(s)


