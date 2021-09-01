library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggExtra)
library(dplyr)
library(tidyr)
library(usmap)
library(janitor)
library(ggThemeAssist)
install.packages("corrplot")
library(corrplot)
install.packages("usdata")
library(usdata)
source("http://www.sthda.com/upload/rquery_cormat.r")
install.packages("MASS")
library(MASS)


setwd("/Users/danielschneider/Desktop/data/")
df = read.csv("county_pops.csv")
atlas = read.csv("research_atlas_2019.csv")
income = read.csv("personal_income.csv")
real_gdp = read.csv("real_gdp.csv")

#identifying % change in pop from 2010 to 2020

df$pop_pct_change = ((df$POPESTIMATE2020 - df$POPESTIMATE2010)/df$POPESTIMATE2010)
df$pop_pct_change <- round(df$pop_pct_change, 3) * 100

#Removing entries where the city and state names were the same to avoid issues with aggregation
mask = which(!(df$STNAME == df$CTYNAME))
df = df[mask,]
df = arrange(df, desc(pop_pct_change))

#df$CTYNAME <- gsub(' County',"",df$CTYNAME)
df <- rename(df, County = CTYNAME)
df = arrange(df, desc(pop_pct_change))
df
#combining these names to create a unique identifier in the case of duplicate cty names
df$area <- paste(df$STNAME, df$County, sep=" - ")
df <- select(df, area, POPESTIMATE2020, pop_pct_change)

# to get one entry for each county
df <- group_by(df, area) %>% 
  summarise("POPESTIMATE2020" = mean(POPESTIMATE2020), 
            "pop_pct_change" = mean(pop_pct_change))


#Selecting relevant columns from the 2019 food research atlas data:
atlas$area <- paste(atlas$State, atlas$County, sep=" - ")
atlas  <- select(atlas, area,       
                       LowIncomeTracts, 
                       MedianFamilyIncome, 
                       lapophalf, 
                       lawhitehalfshare, 
                       lablackhalfshare, 
                       laasianhalfshare,
                       lanhopihalfshare,
                       laaianhalfshare,
                       lahisphalfshare) %>% 
  drop_na()

# convert data types from character to numeric 
atlas[2:10] <- atlas[2:10] %>% mutate_if(is.character,as.double)

#grouping together the multiple entries per census tract based on averages
atlas <- group_by(atlas, area) %>% 
  summarise("LowIncomeTracts" = sum(LowIncomeTracts),
            "MedianFamilyIncome" = mean(MedianFamilyIncome),
            "lapophalf" = mean(lapophalf),
            "lawhitehalfshare" = mean(lawhitehalfshare), 
            "lablackhalfshare" = mean(lablackhalfshare), 
            "laasianhalfshare" = mean(laasianhalfshare),
            "lanhopihalfshare" = mean(lanhopihalfshare),
            "laaianhalfshare" = mean(laaianhalfshare),
            "lahisphalfshare" = mean(lahisphalfshare)) %>% 
  drop_na()


atlas <- separate(data = atlas, col = area , into = c("State", "County"), sep = " - ")


#cleaning the personal income data set
income = rename(income, personal_income = Per.capita.personal.income..Dollars.)
income$personal_income = as.numeric(income$personal_income)
real_gdp$real_gdp = as.numeric(real_gdp$real_gdp)
real_gdp <- separate(data = real_gdp, col = County , into = c("name", "State"), sep = ", ")
income <- separate(data = income, col = County , into = c("name", "State"), sep = ", ")
#income <- subset(income, select = c(name, personal_income))

income$area <- paste(income$name, "County", sep = " ")
income$State <- abbr2state(income$State)
income$area <- paste(income$State, income$area, sep = " - ")
income <- subset(income, select = c(area, personal_income))

real_gdp$area <- paste(real_gdp$name, "County", sep = " ")
real_gdp$State <- abbr2state(real_gdp$State)
real_gdp$area <- paste(real_gdp$State, real_gdp$area, sep = " - ")
real_gdp <- subset(real_gdp, select = c(area, real_gdp))


atlas$area <- paste(atlas$State, atlas$County, sep = " - ")

atlas <- merge(x = atlas, y = income, by = "area", all.x = FALSE)
atlas <- merge(x = atlas, y = real_gdp, by = "area", all.x = FALSE)
atlas_merge <- subset(atlas, select = -c(State, County))

#population is more important for this analysis so a left join is used
#some demographic data from the atlas was not available, 
#so some NAs were produced which will be dropped
county_data = merge(x = df, y = atlas_merge, by = "area", all.x = TRUE)
county_data <- drop_na(county_data)


# combine state and county names to ensure that counties of the same name are not aggregated together



#cleaning population so we can deal with integers
county_data[3:12] <- round(county_data[3:12], digits = 3)
county_data$POPESTIMATE2020 <- floor(county_data$POPESTIMATE2020)

#separating the identifier back into state and county
county_data <- separate(data = county_data, col = area , into = c("State", "County"), sep = " - ")

#we will analyze the top 200 largest and smallest counties in the US
county_top <- arrange(slice_max(county_data, POPESTIMATE2020, n=300))
county_bottom <- arrange(slice_min(county_data, POPESTIMATE2020, n=200))




# ggplot(data=county_data,mapping=aes(x=LowIncomeTracts, y=lapophalf)) +
#   geom_jitter() +
#   geom_smooth(method=lm, formula=y ~ splines::bs(x, 3))

#exploring some of the strongest correlations

rquery.cormat(county_data[,-c(0,1,2)])

ggplot(data=county_data,mapping=aes(x=real_gdp, y=lapophalf)) +
  geom_jitter() +
  coord_flip() +
  geom_smooth()

