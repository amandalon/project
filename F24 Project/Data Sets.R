```{r pressure, echo=FALSE}
# omits any data that we won't be working with, along with any properties with a $0 sale price.AA
raw_data <- Real_Estate_Sales_2001.2022_GL
clean_data <- raw_data[raw_data$'Sale.Amount' != 0, c('List.Year', 'Date.Recorded', 'Town', 'Sale.Amount', 'Sales.Ratio', 'Residential.Type')]

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
```

```{r}
library(dplyr)

inflation.scalar = c('2001' = 1, '2002' = 0.99, '2003' = 0.96, '2004' = 0.95, '2005' = 0.92, '2006' = 0.88, '2007' = 0.87, '2008' = 0.83, '2009' = 0.83, '2010' = 0.81, '2011' = 0.80, '2012' = 0.77, '2013' = 0.76, '2014' = 0.75, '2015' = 0.75, '2016' = 0.74, '2017' = 0.72, '2018' = 0.71, '2019' = 0.70, '2020' = 0.68, '2021' = 0.67, '2022' = 0.62)
#Data sourced from the Bureau of Labor Statistics CPI calculator using January to January values

clean_data$Inflation.Adjusted = clean_data$Sale.Amount * inflation.scalar[as.character(clean_data$List.Year)]
```
