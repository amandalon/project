# omits any data that we won't be working with, along with any properties with a $0 sale price.AA
library(readr)
raw_data <- read_csv("Real_Estate_Sales_2001-2022_GL.csv")
clean_data <- raw_data[raw_data$`Sale Amount` != 0, c('List Year', 'Date Recorded', 'Town', 'Sale Amount', 'Sales Ratio', 'Residential Type')]

# sort data based on year.AA
timeframe <- 2001:2022
years <- function(year){
  na.omit(clean_data[clean_data$'List Year' == year, ])
} 

# data sets by year.AA
data_2001 <- years(2001)
data_2002 <- years(2002)
data_2003 <- years(2003)
data_2004 <- years(2004)
data_2005 <- years(2005)
data_2006 <- years(2006)
data_2007 <- years(2007)
data_2008 <- years(2008)
data_2009 <- years(2009)
data_2010 <- years(2010)
data_2011 <- years(2011)
data_2012 <- years(2012)
data_2013 <- years(2013)
data_2014 <- years(2014)
data_2015 <- years(2015)
data_2016 <- years(2016)
data_2017 <- years(2017)
data_2018 <- years(2018)
data_2019 <- years(2019)
data_2020 <- years(2020)
data_2021 <- years(2021)
data_2022 <- years(2022)


# residential types.AA
residential_types <- unique(clean_data$`Residential Type`)
residential_types

# sort data based on residential type.AA
sort_by_residential_type <- function(df, type){
  df[df$`Residential Type` == type, ]
} 

data_by_year_residential <- list()

# loop over each year and residential type, storing data frames in the nested list.AA
for (year in timeframe) {
  year_data <- years(year)
  data_by_year_residential[[as.character(year)]] <- list()
  
  for (residential in residential_types) {
    data_by_year_residential[[as.character(year)]][[residential]] <- sort_by_residential_type(year_data, residential)
  }
}

# single family.AA
single_2001 <- data_by_year_residential[["2001"]][["Single Family"]]
single_2002 <- data_by_year_residential[["2002"]][["Single Family"]]
single_2003 <- data_by_year_residential[["2003"]][["Single Family"]]
single_2004 <- data_by_year_residential[["2004"]][["Single Family"]]
single_2005 <- data_by_year_residential[["2005"]][["Single Family"]]
single_2006 <- data_by_year_residential[["2006"]][["Single Family"]]
single_2007 <- data_by_year_residential[["2007"]][["Single Family"]]
single_2008 <- data_by_year_residential[["2008"]][["Single Family"]]
single_2009 <- data_by_year_residential[["2009"]][["Single Family"]]
single_2010 <- data_by_year_residential[["2010"]][["Single Family"]]
single_2011 <- data_by_year_residential[["2011"]][["Single Family"]]
single_2012 <- data_by_year_residential[["2012"]][["Single Family"]]
single_2013 <- data_by_year_residential[["2013"]][["Single Family"]]
single_2014 <- data_by_year_residential[["2014"]][["Single Family"]]
single_2015 <- data_by_year_residential[["2015"]][["Single Family"]]
single_2016 <- data_by_year_residential[["2016"]][["Single Family"]]
single_2017 <- data_by_year_residential[["2017"]][["Single Family"]]
single_2018 <- data_by_year_residential[["2018"]][["Single Family"]]
single_2019 <- data_by_year_residential[["2019"]][["Single Family"]]
single_2020 <- data_by_year_residential[["2020"]][["Single Family"]]
single_2021 <- data_by_year_residential[["2021"]][["Single Family"]]
single_2022 <- data_by_year_residential[["2022"]][["Single Family"]]

# unknown.AA 
unknown_2001 <- data_by_year_residential[["2001"]][["NA"]]
unknown_2002 <- data_by_year_residential[["2002"]][["NA"]]
unknown_2003 <- data_by_year_residential[["2003"]][["NA"]]
unknown_2004 <- data_by_year_residential[["2004"]][["NA"]]
unknown_2005 <- data_by_year_residential[["2005"]][["NA"]]
unknown_2006 <- data_by_year_residential[["2006"]][["NA"]]
unknown_2007 <- data_by_year_residential[["2007"]][["NA"]]
unknown_2008 <- data_by_year_residential[["2008"]][["NA"]]
unknown_2009 <- data_by_year_residential[["2009"]][["NA"]]
unknown_2010 <- data_by_year_residential[["2010"]][["NA"]]
unknown_2011 <- data_by_year_residential[["2011"]][["NA"]]
unknown_2012 <- data_by_year_residential[["2012"]][["NA"]]
unknown_2013 <- data_by_year_residential[["2013"]][["NA"]]
unknown_2014 <- data_by_year_residential[["2014"]][["NA"]]
unknown_2015 <- data_by_year_residential[["2015"]][["NA"]]
unknown_2016 <- data_by_year_residential[["2016"]][["NA"]]
unknown_2017 <- data_by_year_residential[["2017"]][["NA"]]
unknown_2018 <- data_by_year_residential[["2018"]][["NA"]]
unknown_2019 <- data_by_year_residential[["2019"]][["NA"]]
unknown_2020 <- data_by_year_residential[["2020"]][["NA"]]
unknown_2021 <- data_by_year_residential[["2021"]][["NA"]]
unknown_2022 <- data_by_year_residential[["2022"]][["NA"]]

# condo.AA
condo_2001 <- data_by_year_residential[["2001"]][["Condo"]]
condo_2002 <- data_by_year_residential[["2002"]][["Condo"]]
condo_2003 <- data_by_year_residential[["2003"]][["Condo"]]
condo_2004 <- data_by_year_residential[["2004"]][["Condo"]]
condo_2005 <- data_by_year_residential[["2005"]][["Condo"]]
condo_2006 <- data_by_year_residential[["2006"]][["Condo"]]
condo_2007 <- data_by_year_residential[["2007"]][["Condo"]]
condo_2008 <- data_by_year_residential[["2008"]][["Condo"]]
condo_2009 <- data_by_year_residential[["2009"]][["Condo"]]
condo_2010 <- data_by_year_residential[["2010"]][["Condo"]]
condo_2011 <- data_by_year_residential[["2011"]][["Condo"]]
condo_2012 <- data_by_year_residential[["2012"]][["Condo"]]
condo_2013 <- data_by_year_residential[["2013"]][["Condo"]]
condo_2014 <- data_by_year_residential[["2014"]][["Condo"]]
condo_2015 <- data_by_year_residential[["2015"]][["Condo"]]
condo_2016 <- data_by_year_residential[["2016"]][["Condo"]]
condo_2017 <- data_by_year_residential[["2017"]][["Condo"]]
condo_2018 <- data_by_year_residential[["2018"]][["Condo"]]
condo_2019 <- data_by_year_residential[["2019"]][["Condo"]]
condo_2020 <- data_by_year_residential[["2020"]][["Condo"]]
condo_2021 <- data_by_year_residential[["2021"]][["Condo"]]
condo_2022 <- data_by_year_residential[["2022"]][["Condo"]]

# two family.AA
two_fam_2001 <- data_by_year_residential[["2001"]][["Two Family"]]
two_fam_2002 <- data_by_year_residential[["2002"]][["Two Family"]]
two_fam_2003 <- data_by_year_residential[["2003"]][["Two Family"]]
two_fam_2004 <- data_by_year_residential[["2004"]][["Two Family"]]
two_fam_2005 <- data_by_year_residential[["2005"]][["Two Family"]]
two_fam_2006 <- data_by_year_residential[["2006"]][["Two Family"]]
two_fam_2007 <- data_by_year_residential[["2007"]][["Two Family"]]
two_fam_2008 <- data_by_year_residential[["2008"]][["Two Family"]]
two_fam_2009 <- data_by_year_residential[["2009"]][["Two Family"]]
two_fam_2010 <- data_by_year_residential[["2010"]][["Two Family"]]
two_fam_2011 <- data_by_year_residential[["2011"]][["Two Family"]]
two_fam_2012 <- data_by_year_residential[["2012"]][["Two Family"]]
two_fam_2013 <- data_by_year_residential[["2013"]][["Two Family"]]
two_fam_2014 <- data_by_year_residential[["2014"]][["Two Family"]]
two_fam_2015 <- data_by_year_residential[["2015"]][["Two Family"]]
two_fam_2016 <- data_by_year_residential[["2016"]][["Two Family"]]
two_fam_2017 <- data_by_year_residential[["2017"]][["Two Family"]]
two_fam_2018 <- data_by_year_residential[["2018"]][["Two Family"]]
two_fam_2019 <- data_by_year_residential[["2019"]][["Two Family"]]
two_fam_2020 <- data_by_year_residential[["2020"]][["Two Family"]]
two_fam_2021 <- data_by_year_residential[["2021"]][["Two Family"]]
two_fam_2022 <- data_by_year_residential[["2022"]][["Two Family"]]

# three family.AA
three_fam_2001 <- data_by_year_residential[["2001"]][["Three Family"]]
three_fam_2002 <- data_by_year_residential[["2002"]][["Three Family"]]
three_fam_2003 <- data_by_year_residential[["2003"]][["Three Family"]]
three_fam_2004 <- data_by_year_residential[["2004"]][["Three Family"]]
three_fam_2005 <- data_by_year_residential[["2005"]][["Three Family"]]
three_fam_2006 <- data_by_year_residential[["2006"]][["Three Family"]]
three_fam_2007 <- data_by_year_residential[["2007"]][["Three Family"]]
three_fam_2008 <- data_by_year_residential[["2008"]][["Three Family"]]
three_fam_2009 <- data_by_year_residential[["2009"]][["Three Family"]]
three_fam_2010 <- data_by_year_residential[["2010"]][["Three Family"]]
three_fam_2011 <- data_by_year_residential[["2011"]][["Three Family"]]
three_fam_2012 <- data_by_year_residential[["2012"]][["Three Family"]]
three_fam_2013 <- data_by_year_residential[["2013"]][["Three Family"]]
three_fam_2014 <- data_by_year_residential[["2014"]][["Three Family"]]
three_fam_2015 <- data_by_year_residential[["2015"]][["Three Family"]]
three_fam_2016 <- data_by_year_residential[["2016"]][["Three Family"]]
three_fam_2017 <- data_by_year_residential[["2017"]][["Three Family"]]
three_fam_2018 <- data_by_year_residential[["2018"]][["Three Family"]]
three_fam_2019 <- data_by_year_residential[["2019"]][["Three Family"]]
three_fam_2020 <- data_by_year_residential[["2020"]][["Three Family"]]
three_fam_2021 <- data_by_year_residential[["2021"]][["Three Family"]]
three_fam_2022 <- data_by_year_residential[["2022"]][["Three Family"]]

#four family.AA
four_fam_2001 <- data_by_year_residential[["2001"]][["Four Family"]]
four_fam_2002 <- data_by_year_residential[["2002"]][["Four Family"]]
four_fam_2003 <- data_by_year_residential[["2003"]][["Four Family"]]
four_fam_2004 <- data_by_year_residential[["2004"]][["Four Family"]]
four_fam_2005 <- data_by_year_residential[["2005"]][["Four Family"]]
four_fam_2006 <- data_by_year_residential[["2006"]][["Four Family"]]
four_fam_2007 <- data_by_year_residential[["2007"]][["Four Family"]]
four_fam_2008 <- data_by_year_residential[["2008"]][["Four Family"]]
four_fam_2009 <- data_by_year_residential[["2009"]][["Four Family"]]
four_fam_2010 <- data_by_year_residential[["2010"]][["Four Family"]]
four_fam_2011 <- data_by_year_residential[["2011"]][["Four Family"]]
four_fam_2012 <- data_by_year_residential[["2012"]][["Four Family"]]
four_fam_2013 <- data_by_year_residential[["2013"]][["Four Family"]]
four_fam_2014 <- data_by_year_residential[["2014"]][["Four Family"]]
four_fam_2015 <- data_by_year_residential[["2015"]][["Four Family"]]
four_fam_2016 <- data_by_year_residential[["2016"]][["Four Family"]]
four_fam_2017 <- data_by_year_residential[["2017"]][["Four Family"]]
four_fam_2018 <- data_by_year_residential[["2018"]][["Four Family"]]
four_fam_2019 <- data_by_year_residential[["2019"]][["Four Family"]]
four_fam_2020 <- data_by_year_residential[["2020"]][["Four Family"]]
four_fam_2021 <- data_by_year_residential[["2021"]][["Four Family"]]
four_fam_2022 <- data_by_year_residential[["2022"]][["Four Family"]]

# Merge data function.
combine_data_by_year <- function(years_data) {
  # Combine a single data set.
  combined_data <- do.call(rbind, years_data)
  return(combined_data)
}

# Merge single-family data.
single_family_data <- list(single_2001, single_2002, single_2003, single_2004, single_2005, single_2006, single_2007,
                           single_2008, single_2009, single_2010, single_2011, single_2012, single_2013, single_2014,
                           single_2015, single_2016, single_2017, single_2018, single_2019, single_2020, single_2021, single_2022)

single_family_combined <- combine_data_by_year(single_family_data)

# Merge two family data.
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
# 1 Analyze the factors affecting property sale prices
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

library(kableExtra)

# (1) Analyze the factors affecting property sale prices
combined_data <- rbind(single_family_combined, two_family_combined, three_family_combined, four_family_combined)

combined_data$Date_Recorded <- as.Date(combined_data$Date_Recorded, format = "%m/%d/%Y")

head(combined_data$Date_Recorded)

correlation_analysis <- cor(combined_data[, c("Sale_Amount", "Sales_Ratio")], use = "complete.obs")
print(correlation_analysis)
lm_result <- lm(Sale_Amount ~ + Sales_Ratio  + Residential_Type, data = combined_data)
summary(lm_result)

# properties with Sales_Ratio < 1 are considered unsold, while those with Sales_Ratio == 1 are considered sold.
sold_properties <- subset(combined_data, Sales_Ratio == 1)
unsold_properties <- subset(combined_data, Sales_Ratio < 1)

# Compare the differences between sold and unsold properties.
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

# Property Turnover Rate
town_turnover_rate <- aggregate(Sales_Ratio ~ Town, data = combined_data, FUN = function(x) sum(x == 1) / length(x))
print(town_turnover_rate)
town_turnover_rate %>%
  kable("html", col.names = c("Town", "Turnover Rate")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

# Analyze the liquidity of the real estate market.
# The listing duration is recorded in the Date_Recorded column.
# Compute the average listing duration for sold and unsold properties.
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


