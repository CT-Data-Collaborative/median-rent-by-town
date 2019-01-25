library(dplyr)
library(datapkg)
library(acs)
library(stringr)
library(reshape2)
library(data.table)
library(tidyr)
source('./scripts/acsHelpers.R')

##################################################################
#
# Processing Script for Median Rent by Town
# Created by Jenna Daly
# On 03/05/2018
#
##################################################################

#Setup environment
sub_folders <- list.files()
raw_location <- grep("raw", sub_folders, value=T)
path_to_raw <- (paste0(getwd(), "/", raw_location))

options(scipen=999)
acsdata <- getACSData(
    getCTGeos("town"),
    yearList = 2010:2017,
    table = "DP04"
)

med_rent <- data.table()
for (data in acsdata) {
    year <- data@endyear
    if (year %in% c(2015, 2016, 2017)) {
      median_rent <- acsSum(data, 267, "Median Rent") #VC191
    } else {
      median_rent <- acsSum(data, 263, "Median Rent") #VC185, VC189
    } 

    datafips <- data.table(fips = getACSFips(data))
    estimates <- data.table(
        FIPS = datafips$fips,
        Year = year,
        estimate(median_rent)
    )
    names(estimates)[names(estimates) == "HC01_VC185.Estimate; GROSS RENT - Median (dollars)"] <- "Median Rent"
    names(estimates)[names(estimates) == "HC01_VC189.Estimate; GROSS RENT - Occupied units paying rent - Median (dollars)"] <- "Median Rent"
    names(estimates)[names(estimates) == "HC01_VC191.Estimate; GROSS RENT - Occupied units paying rent - Median (dollars)"] <- "Median Rent"

    estimates <- melt(
        estimates,
        id.vars = c("FIPS", "Year"),
        variable.name = "Variable",
        variable.factor = F,
        value.name = "Number",
        value.factor = F
    )

    moes <- data.table(
        FIPS = datafips$fips,
        Year = year,
        standard.error(median_rent) * 1.645      
    )
    names(moes)[names(moes) == "HC01_VC185.Estimate; GROSS RENT - Median (dollars)"] <- "Median Rent"
    names(moes)[names(moes) == "HC01_VC189.Estimate; GROSS RENT - Occupied units paying rent - Median (dollars)"] <- "Median Rent"
    names(moes)[names(moes) == "HC01_VC191.Estimate; GROSS RENT - Occupied units paying rent - Median (dollars)"] <- "Median Rent"
    
    moes <- melt(
        moes,
        id.vars = c("FIPS", "Year"),
        variable.name = "Variable",
        variable.factor = F,
        value.name = "Margins of Error",
        value.factor = F
    )

    setkey(estimates, FIPS, Year, `Variable`)
    setkey(moes, FIPS, Year, `Variable`)

    med_rent <- rbind(med_rent, estimates[moes])
}

#create CT df to merge with original to caluate ratios

ct_value <- med_rent[med_rent$FIPS == "09",]

merge <- merge(med_rent, ct_value, by = "Year")

merge$`Ratio to State Median Num` <- merge$Number.x / merge$Number.y
merge$`Ratio to State Median MOE` <- sqrt((merge$`Margins of Error.x`)^2 + ((merge$Number.x/merge$Number.y)*((merge$`Margins of Error.y`)^2))) / (merge$Number.y)

merge <- merge %>% 
  select(FIPS.x, Year, Variable.x, Number.x, `Margins of Error.x`, `Ratio to State Median Num`, `Ratio to State Median MOE`) 

merge <- merge[merge$FIPS.x != "0900100000",]

data_long <- gather(merge, `Measure Type`, Value, 4:7)

#Clean up columns
data_long$Variable.x[grepl("Margins", data_long$`Measure Type`)] <- "Margins of Error"
data_long$Variable.x[grepl("MOE", data_long$`Measure Type`)] <- "Margins of Error"
data_long$`Measure Type` <- gsub(".x", "", data_long$`Measure Type`)
data_long$`Measure Type`[data_long$`Measure Type` == "Margins of Error"] <- "Number"
data_long$`Measure Type` <- gsub(" Num", "", data_long$`Measure Type`)
data_long$`Measure Type` <- gsub(" MOE", "", data_long$`Measure Type`)

#Merge in Towns by FIPS (filter out county data)
town_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-town-list/master/datapackage.json'
town_fips_dp <- datapkg_read(path = town_fips_dp_URL)
towns <- (town_fips_dp$data[[1]])

data_long_fips <- merge(data_long, towns, by.x = "FIPS.x", by.y = "FIPS")

data_long_fips$Year <- paste(data_long_fips$Year-4, data_long_fips$Year, sep="-")

data_long_final <- data_long_fips %>% 
  select(Town, FIPS.x, Year, `Measure Type`, Variable.x, Value) 
    
names(data_long_final)[names(data_long_final) == "FIPS.x"] <- "FIPS"
names(data_long_final)[names(data_long_final) == "Variable.x"] <- "Variable"

data_long_final$Value <-  round(data_long_final$Value, 2)

data_long_final <- data_long_final %>% 
  arrange(Town, Year, `Measure Type`, desc(Variable))

write.table(
    data_long_final,
    file.path("data", "median-rent-town-2017.csv"),
    sep = ",",
    row.names = F,
    col.names = T,
    na = "-6666" 
)
