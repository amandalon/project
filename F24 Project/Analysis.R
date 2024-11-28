# Merge data function.
combine_data_by_year <- function(years_data) {
  # Combine a single dataset.
  combined_data <- do.call(rbind, years_data)
  return(combined_data)
}

# Merge single-family data.
single_family_data <- list(single_2001, single_2002, single_2003, single_2004, single_2005, single_2006, single_2007,
                           single_2008, single_2009, single_2010, single_2011, single_2012, single_2013, single_2014,
                           single_2015, single_2016, single_2017, single_2018, single_2019, single_2020, single_2021, single_2022)

single_family_combined <- combine_data_by_year(single_family_data)

# Merge two-family data.
two_family_data <- list(two_fam_2001, two_fam_2002, two_fam_2003, two_fam_2004, two_fam_2005, two_fam_2006, two_fam_2007,
                        two_fam_2008, two_fam_2009, two_fam_2010, two_fam_2011, two_fam_2012, two_fam_2013, two_fam_2014,
                        two_fam_2015, two_fam_2016, two_fam_2017, two_fam_2018, two_fam_2019, two_fam_2020, two_fam_2021, two_fam_2022)

two_family_combined <- combine_data_by_year(two_family_data)

# Merge three-family data.
three_family_data <- list(three_fam_2001, three_fam_2002, three_fam_2003, three_fam_2004, three_fam_2005, three_fam_2006,
                          three_fam_2007, three_fam_2008, three_fam_2009, three_fam_2010, three_fam_2011, three_fam_2012,
                          three_fam_2013, three_fam_2014, three_fam_2015, three_fam_2016, three_fam_2017, three_fam_2018,
                          three_fam_2019, three_fam_2020, three_fam_2021, three_fam_2022)

three_family_combined <- combine_data_by_year(three_family_data)

# Merge four-family data.
four_family_data <- list(four_fam_2001, four_fam_2002, four_fam_2003, four_fam_2004, four_fam_2005, four_fam_2006,
                         four_fam_2007, four_fam_2008, four_fam_2009, four_fam_2010, four_fam_2011, four_fam_2012,
                         four_fam_2013, four_fam_2014, four_fam_2015, four_fam_2016, four_fam_2017, four_fam_2018,
                         four_fam_2019, four_fam_2020, four_fam_2021, four_fam_2022)

four_family_combined <- combine_data_by_year(four_family_data)

# Standardize column name formatting.
adjust_colnames <- function(df) {
  colnames(df) <- gsub(" ", "_", colnames(df))  
  colnames(df) <- gsub("List_Year", "List_Year", colnames(df))  
  return(df)
}

# Adjust the column names for all four datasets.
single_family_combined <- adjust_colnames(single_family_combined)
two_family_combined <- adjust_colnames(two_family_combined)
three_family_combined <- adjust_colnames(three_family_combined)
four_family_combined <- adjust_colnames(four_family_combined)

###############################################################################
# 1Analyze the factors affecting property sale prices
# 
# 2 Identify the features influencing price (e.g., area, age of the property, renovation status, etc.).
# 
# 
# 3 Calculate the average sale price of each property type per year and plot the trend over time.
# ：
# 
# 4 Compare the differences between sold and unsold properties in terms of price, area, property age, renovation status, etc.Compare the differences between sold and unsold properties in terms of price, area, property age, renovation status, etc.
# 
# 
# 5 Calculate the property turnover rate for each town (percentage of sold properties out of the total number of properties).
# 
# 
# 6 Analyze the listing duration of properties and compare sold and unsold properties to assess market liquidity.
# 
# 
# 
# 
library(kableExtra)

# (1) 
combined_data <- rbind(single_family_combined, two_family_combined, three_family_combined, four_family_combined)

combined_data$Date_Recorded <- as.Date(combined_data$Date_Recorded, format = "%m/%d/%Y")


head(combined_data$Date_Recorded)

correlation_analysis <- cor(combined_data[, c("Sale_Amount", "Sales_Ratio")], use = "complete.obs")
print(correlation_analysis)
lm_result <- lm(Sale_Amount ~ + Sales_Ratio  + Residential_Type, data = combined_data)
summary(lm_result)

# : 2.
library(ggplot2)
avg_sale_price_per_year <- aggregate(Sale_Amount ~ List_Year, data = combined_data, FUN = mean)
ggplot(avg_sale_price_per_year, aes(x = List_Year, y = Sale_Amount)) +
  geom_line() +
  labs(title = "Average Sale Price Per Year", x = "Year", y = "Average Sale Price")


sold_properties <- subset(combined_data, Sales_Ratio == 1)
unsold_properties <- subset(combined_data, Sales_Ratio < 1)


summary_sold <- summary(sold_properties$Sale_Amount)
summary_sold
summary_unsold <- summary(unsold_properties$Sale_Amount)
summary_unsold

# Create a DataFrame to display the price comparison between sold and unsold properties.
sale_comparison <- data.frame(
  Category = c("Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max"),
  Sold = as.numeric(summary_sold),
  Unsold = as.numeric(summary_unsold)
)

sale_comparison %>%
  kable("html", col.names = c("Statistic", "Sold Properties", "Unsold Properties")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

# Calculate the property turnover rate.
town_turnover_rate <- aggregate(Sales_Ratio ~ Town, data = combined_data, FUN = function(x) sum(x == 1) / length(x))
print(town_turnover_rate)
town_turnover_rate %>%
  kable("html", col.names = c("Town", "Turnover Rate")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

# Analyze property market liquidity.
sold_properties$Date_Recorded <- as.Date(sold_properties$Date_Recorded, format = "%Y-%m-%d")
unsold_properties$Date_Recorded <- as.Date(unsold_properties$Date_Recorded, format = "%Y-%m-%d")

sold_properties$listing_duration <- as.numeric(difftime(Sys.Date(), sold_properties$Date_Recorded, units = "days"))
unsold_properties$listing_duration <- as.numeric(difftime(Sys.Date(), unsold_properties$Date_Recorded, units = "days"))

avg_listing_duration_sold <- mean(sold_properties$listing_duration, na.rm = TRUE)
avg_listing_duration_unsold <- mean(unsold_properties$listing_duration, na.rm = TRUE)

# Use inflation-adjusted sale prices.
inflation_data <- data.frame(
  List_Year = 2006:2023,
  Inflation_Rate = c(3.2, 2.5, 1.9, 3.1, 2.4, 1.7, 1.9, 2.0, 2.3, 1.8, 2.0, 2.5, 1.9, 2.2, 2.6, 3.1, 2.8, 2.7)  
)

combined_data <- merge(combined_data, inflation_data, by = "List_Year", all.x = TRUE)
avg_inflation_adjusted_price_per_year <- aggregate(Inflation_Adjusted_Sale_Amount ~ List_Year, data = combined_data, FUN = mean)