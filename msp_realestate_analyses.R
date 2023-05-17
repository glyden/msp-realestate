options(scipen = 99999)
library(dplyr)
library(lubridate)
library(splines)
#### Zillow's Zestimate is still functional, I will have to make a case that 
#### I can make better predictions than they do.

#### Get Data From All of Minneapolis Parcels:
#### https://opendata.minneapolismn.gov/
#### These should be up to date as of 2022
#### so, variables used from this dataset should
#### be used cautiously as they may not reflect
#### what was actually there during the last sale
all_mpls <- read.csv('Assessors_Parcel_Data_2022.csv',na.strings="",
                     skipNul=T) %>%
  mutate(APN = as.numeric(gsub(pattern = 'p', replacement = '', PIN))) %>%
  mutate(square_feet = ABOVEGROUNDAREA,
         sqft_hundred = square_feet/100,
         built_decade = 10*(floor(YEARBUILT/10)),
         Baths = BATHROOMS,
         neighborhood = tolower(NEIGHBORHOOD),
         Year.Built = YEARBUILT)
  # left_join(msp_sales %>% select(APN, square_feet) %>% mutate(APN = as.character(paste0('0', APN)),in_sales = TRUE))
all_mpls %>% select(ABOVEGROUNDAREA,BELOWGROUNDAREA,square_feet,sqft_hundred,
                    YEARBUILT,built_decade,BATHROOMS,Baths,neighborhood) %>%
  head()

#### Note Here, APN can be duplicated, so be careful when joining on APN,
#### Even though it seems to be the ID variable
sum(duplicated(all_mpls$APN)) # no duplicates


# Additional variables in the Assessors Data
# ZONING, Lot Size (PARCEL_AREA_SQFT), BELOWGROUNDAREA, NUM_GAR_STALLS
# PRIMARYHEATING, CONSTRUCTIONTYPE, EXTERIORTYPE, ROOF, TOTAL_UNITS,
# FIREPLACES, NUM_BLDGS


#### Sales Data from the Neighborhood sales finder app
msp_sales <- read.csv('mpls_sales_2002-2022.csv') %>% 
  bind_rows(read.csv('mpls_sales_OctNov_2022.csv')) %>%
  bind_rows(read.csv('./mpls_sales_Dec_2022.csv')) %>%
  bind_rows(read.csv('./mpls_sales_Jan_2023.csv')) %>%
  bind_rows(read.csv('./mpls_sales_Feb_2023.csv')) %>%
  rename('square_feet' = 'Building.Area..Sq..Ft..') %>% # above ground
  select(-X) %>%
  distinct()
nrow(msp_sales) # 46470
head(msp_sales)
sum(duplicated(msp_sales$X)) # don't need this duplicated row number
sum(duplicated(msp_sales$APN)) # sales of same home
which(duplicated(msp_sales$APN))[1]
msp_sales %>% filter(APN==1202924210097) # seems like an error but maybe duplex
msp_sales = msp_sales %>%
  # select(APN, Address, square_feet, Sales.Date, Sales.Price) %>%
  #mutate(APN = as.character(paste0('0', APN))) %>%
  left_join(all_mpls %>%
              select(APN, ZONING, PARCEL_AREA_SQFT, BELOWGROUNDAREA,
                     NUM_GAR_STALLS, PRIMARYHEATING, CONSTRUCTIONTYPE,
                     EXTERIORTYPE, ROOF, TOTAL_UNITS, FIREPLACES, NUM_BLDGS),
            by = 'APN') %>%
  mutate(Sales.Month = ifelse(substr(Sales.Date, 1, 3) == 'Jan', 1,
           ifelse(substr(Sales.Date, 1, 3) == 'Feb', 2,
           ifelse(substr(Sales.Date, 1, 3) == 'Mar', 3,
           ifelse(substr(Sales.Date, 1, 3) == 'Apr', 4,
           ifelse(substr(Sales.Date, 1, 3) == 'May', 5,
           ifelse(substr(Sales.Date, 1, 3) == 'Jun', 6,
           ifelse(substr(Sales.Date, 1, 3) == 'Jul', 7,
           ifelse(substr(Sales.Date, 1, 3) == 'Aug', 8,
           ifelse(substr(Sales.Date, 1, 3) == 'Sep', 9,
           ifelse(substr(Sales.Date, 1, 3) == 'Oct', 10,
           ifelse(substr(Sales.Date, 1, 3) == 'Nov', 11,
           ifelse(substr(Sales.Date, 1, 3) == 'Dec', 12,NA)))))))))))),
         Sales.Quarter = ifelse(substr(Sales.Date, 1, 3) %in% c('Jan', 'Feb', 'Mar'), 1,
           ifelse(substr(Sales.Date, 1, 3) %in% c('Apr', 'May', 'Jun'), 2,
           ifelse(substr(Sales.Date, 1, 3) %in% c('Jul', 'Aug', 'Sep'), 3,
           ifelse(substr(Sales.Date, 1, 3) %in% c('Oct', 'Nov', 'Dec'), 4, NA)))),
         Sales.Year = substr(Sales.Date, (nchar(Sales.Date)+1)-4,nchar(Sales.Date)),
         Sales.Day = gsub(pattern = ',', replacement = '', as.character(substr(Sales.Date, 5,6))),
         sale_date = as.Date(paste0(Sales.Year,'-',Sales.Month,'-',Sales.Day)),
         Sale.Weekday = weekdays(sale_date),
         Sales.Price = as.integer(gsub(pattern = "\\$|,", replacement = '', Sales.Price)),
         sale_quarter = paste0(Sales.Year, Sales.Quarter),
         sqft_hundred = 100*floor(square_feet/100),
         built_decade = 10*(floor(Year.Built/10)),
         neighborhood = tolower(neighborhood))


#### Again, APN will need to be used carefully.  For example, the square
#### footage between the Assessors Data and the Neighborhood sales data
#### don't always match, often in a way that would indicate multiple units.
#### Probably best to use Neighborhood sales data unless data doesn't exist
#### in neighborhood sales data.  Because for example, a property that was
#### sold in 2005 may have more bathrooms in 2022, in which case the 
#### predictor is after the outcome (temporal problem).  I think this would 
#### be negligible enough misclassification to ignore, but best to be safe 
#### where possible.
sum(duplicated(msp_sales$APN))

#### As suspected, several parcels have multiple units.  It would probably
#### be best if this is treated as a separate category.  May be possible
#### to put it in with duplexes and condos?
table(msp_sales$TOTAL_UNITS)


#### Question: How many properties re-sold (duplicates) during the 20 years?
#### What I actually found was that this db seems to only be most recent
#### sales.
resales <- msp_sales %>% 
  group_by(Address) %>%
  summarize(n_sales = n()) %>%
  ungroup %>%
  filter(n_sales > 1)
head(resales)
resales %>% nrow()

# In the 20 years from Jan 1, 2002 to Oct 31 2022, there were 44139 single family
# homes that were sold only once
nrow(msp_sales) - sum(resales$n_sales)

# 989 Single family homes were sold more than once, representing 2129 sales
nrow(resales)
sum(resales$n_sales)

# The most sold propery was sold 8 times
max(resales$n_sales)

mult_sales <- msp_sales[ which(msp_sales$Address %in% resales$Address),] %>%
  arrange(Address)

# Data was pulled on Nov 8, what is the most recent sale on that day?
# Answer: Sept 30 - Probably a 2 month lag such that the backend on 
# their side gets updated once a month.  Check first business day after
# Dec 1 to see if Oct gets dumped.  If this is the case, then the pull
# probably just needs to happen once a month.
max(msp_sales$sale_date)

# What is the frequeny of Sales Day of Week (Presumably Closing,
# or recording of sale?)
table(msp_sales$Sale.Weekday)

# I am assuming that the pull that I have represents most recent
# sales of all properties, not all sales (ie if a property was sold
# 3 times in the last 20 years, only the most recent will show up).
# Given that, what is the distribution of sales month?
table(msp_sales$Sales.Month)

# Starting some modeling: Variables I have are
# Sales Date, Building Area, Beds, Baths, Stories, 
# Year Built, Neighborhood

# First question, how to represent time
# Going to start with a categorical of quarter.  Since I am doing
# prediction, might end up with random forest.  Could also do
# a LASSO with categoricals/splines to get best knots
date_catmod <- lm(Sales.Price ~ -1 + factor(sale_quarter),
   data = msp_sales)

plot(x = 1:length(date_catmod$coefficients), y = unname(date_catmod$coefficients))

# as a causal kind of guy, I'll end up down a lot of rabbit holes.
# Here is a first: there seems to be some cyclicality of prices
# Prices look to be highest in Q2 and lower in other quarters. The
# first question is whether this reflects a difference in the type
# of houses sold.  I'll first try a simple regression of house size
# on quarter.
size_quartermod <- lm(square_feet ~ -1 + factor(Sales.Quarter),
                      data = msp_sales)

summary(size_quartermod)

# If I adjust for house size, does cyclicality go away?
date_catmod2 <- lm(Sales.Price ~ -1 + factor(sale_quarter) + square_feet,
                  data = msp_sales)

plot(x = 1:length(date_catmod2$coefficients)-1, y = unname(date_catmod2$coefficients))
summary(date_catmod2)

date_catmod3 <- lm(Sales.Price ~ -1 + factor(Sales.Quarter),
                   data = msp_sales)
summary(date_catmod3)

date_catmod4 <- lm(Sales.Price ~ -1 + factor(Sales.Quarter) + square_feet,
                   data = msp_sales)
summary(date_catmod4)

date_catmod5 <- lm(Sales.Price ~ -1 + factor(Sales.Quarter) + square_feet + factor(Baths),
                   data = msp_sales)
summary(date_catmod5)

# Even after adjusting for size and number of baths, there is still a quarterly
# trend, though the significance goes away Q2 has highest prices, followed by 
# Q3 which is not much different than Q2, Q4 is next, but not substantially
# different than Q1.

# Let's try an expansion of square feet, starting categorical:
size_catmod1 <- lm(Sales.Price ~ -1 + factor(sqft_hundred),
                   data = msp_sales)

plot(x = 1:length(size_catmod1$coefficients), y = unname(size_catmod1$coefficients))
# Almost perfectly linear from about 300 to 1700, then still linear but with
# a slightly steeper slope from 1700 to 4000.  After 4000 the variance gets big,
# especially after 5000.

# I am going to ignore bedrooms as square footage has always seemed more 
# important to me, butmay allow it into the LASSO.  Looking at baths now
bath_catmod1 <- lm(Sales.Price ~ -1 + factor(Baths),
                   data = msp_sales)

plot(x = 1:length(bath_catmod1$coefficients), y = unname(bath_catmod1$coefficients))

# 0 Baths throws a wrench in things.  I suspect it is new constructions, because
# they are high price.  May also be apartments.  
table(msp_sales$Baths)
# It is only 32 out of 46268 records, might be best to exclude?


# Year Built
built_catmod <- lm(Sales.Price ~ -1 + factor(built_decade),
                   data = msp_sales)

plot(x = 1:length(built_catmod$coefficients), y = (built_catmod$coefficients))


# Beds
beds_catmod1 <- lm(Sales.Price ~ -1 + factor(Beds),
                   data = msp_sales)

plot(x = 1:length(beds_catmod1$coefficients), y = unname(beds_catmod1$coefficients))


#### Preparing Data for LASSO:
# Continuous, Use Spline:
# Sales Date, Building Area, Beds, Baths, Stories, 
# Year Built, Lot Size (PARCEL_AREA_SQFT), BELOWGROUNDAREA, 

# Categorical
# Neighborhood, ZONING, 
# PRIMARYHEATING, CONSTRUCTIONTYPE, EXTERIORTYPE, ROOF, 
# FIREPLACES

# Could go either way:
# TOTAL_UNITS, NUM_BLDGS, NUM_GAR_STALLS


#### Spline Expansions:
# Size:
for(sf in sort(unique(msp_sales$sqft_hundred))){
  msp_sales[[paste0("sqft_", sf)]] <- {msp_sales$square_feet > sf} * (msp_sales$square_feet - sf)
}
# Baths:
bth_knots <- c(0,1,4,8) # knots based on plot from categorical exploration
for(sf in bth_knots){
  msp_sales[[paste0("baths_", sf)]] <- {msp_sales$Baths > sf} * (msp_sales$Baths - sf)
}
# Beds, same knots as Baths:
for(sf in bth_knots){
  msp_sales[[paste0("beds_", sf)]] <- {msp_sales$Beds > sf} * (msp_sales$Beds - sf)
}
# Year Built:
for(sf in sort(unique(msp_sales$built_decade))){
  msp_sales[[paste0("yrbuilt_", sf)]] <- {msp_sales$Year.Built > sf} * (msp_sales$Year.Built - sf)
}
# Sales Date:
yrs <- c(2002:year(Sys.Date()))

quart_cuts <- as.integer(
  sort(
  as.Date(paste0(c(rep(yrs,4)),
                             "-",
                             c(rep(1,length(yrs)), 
                               rep(4,length(yrs)), 
                               rep(7,length(yrs)), 
                               rep(10,length(yrs))),
                             "-01"))))
q_names <- unique(msp_sales$sale_quarter)
for(i in 1:length(q_names)){
  msp_sales[[paste0("q_", q_names[i])]] <- {as.integer(msp_sales$sale_date) > quart_cuts[i]} * (as.integer(msp_sales$sale_date) - quart_cuts[i])
  
}

colnames(msp_sales)

## fit model
# note: above-ground square feet (building area) from neighborhood sales app
# from assessor:
# PRIMARYHEATING, CONSTRUCTIONTYPE, EXTERIORTYPE, ROOF, TOTAL_UNITS, FIREPLACES, NUM_BLDGS
mod1 <- lm(Sales.Price ~ ns(square_feet, df = 4) + factor(Baths) + 
             ns(Year.Built, df = 5) + 
             factor(sale_quarter) + factor(neighborhood) +
             factor(Beds) + ns(BELOWGROUNDAREA, df=3) +
             factor(NUM_GAR_STALLS) + factor(PRIMARYHEATING) +
             I(FIREPLACES > 0) + #factor(Sales.Month) +
             factor(EXTERIORTYPE),
           data = msp_sales %>% filter(sale_date >= mdy('03/13/2020'),
                                       TOTAL_UNITS == 1,
                                       Baths <= 4,
                                       Beds %in% 1:5))
summary(mod1) # good R^2
nobs(mod1)
plot(x=1:12,y=coef(mod1)[grepl("sale_quarter",names(coef(mod1)))])
termplot(mod1)

msp_sales %>% 
  filter(Address=="4323 GARFIELD AVE") 
pred1 = msp_sales %>% 
  filter(Address=="4323 GARFIELD AVE") %>%
  select(square_feet,Year.Built,neighborhood,Beds,BELOWGROUNDAREA,
         NUM_GAR_STALLS,PRIMARYHEATING,FIREPLACES,EXTERIORTYPE) %>%
  mutate(Baths = 3,sale_quarter="20223",#"20231",
         Sales.Month=5)

predict(mod1, newdata = pred1, type = "terms")
predict(mod1, newdata = pred1, interval = "prediction")
