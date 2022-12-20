#Walmart would like to predict the sales and demand accurately
#There are certain events and holidays which impact sales on each day. 
#There are sales data available for 45 stores of Walmart. 
#facing a challenge due to unforeseen demands and runs out of stock some times,
#An ideal ML algorithm will predict demand accurately and ingest factors like 
# economic conditions including CPI, Unemployment Index, etc.

#Walmart runs several promotional markdown events throughout the year.
#These markdowns precede prominent holidays, the four largest of all, which are 
#the Super Bowl, Labour Day, Thanksgiving, and Christmas.
#The weeks including these holidays are weighted five times higher in the evaluation than non-holiday weeks


rm(list = ls())
#setwd("~/Workspaces/Data Science_2022/R")
getwd()

# load library
library(dplyr)
library(lubridate)
library(corrplot)


# function to transform dates into days in sequence
transform_Date_DaySequence <- function(dates) {
  next_date = min(dates)
  day_count = 1
  day_count_vector = NULL
  for(i in dates) {
    if(i > next_date) {
      next_date = i
      day_count <- day_count+1
    }
    day_count_vector = c(day_count_vector, day_count)
  }
  day_count_vector
}


#Import the csv walmart data
WalmartStoreSales <- read.csv("Walmart_Store_sales.csv")


#check the structure of data
# ----- Dataset Description -----
# This is the historical data which covers sales from 2010-02-05 to 2012-11-01, 
#in the file Walmart_Store_sales. Within this file you will find the following fields:
# Store - the store number
# Date - the week of sales
# Weekly_Sales -  sales for the given store
# Holiday_Flag - whether the week is a special holiday week 
#1 – Holiday week 0 – Non-holiday week
# Temperature - Temperature on the day of sale
# Fuel_Price - Cost of fuel in the region
# CPI – Prevailing consumer price index
# Unemployment - Prevailing unemployment rate

str(WalmartStoreSales)

# Format columns
WalmartStoreSales$Date <- as.Date(WalmartStoreSales$Date, "%d-%m-%Y")

# Hygiene check
str(WalmartStoreSales)
head(WalmartStoreSales)
summary(WalmartStoreSales)




################################################################

# Analysis Tasks

# Basic Statistics tasks

###### 1.##### 
#Which store has maximum sales

# Find max sales of each store
store_with_max_sales <- WalmartStoreSales %>% 
  group_by(Store) %>% 
  summarise(sum_sales = sum(Weekly_Sales)) %>% 
  arrange(desc(sum_sales))
store_with_max_sales[1,]

ans1 <- paste("Store", store_with_max_sales[1,1], "has maximum sales : ", store_with_max_sales[1,2])
print(ans1)
#Store 20 has maximum sales :  301397792.46"

plot(store_with_max_sales,
     main="Store vs Sales Plot")
pie(store_with_max_sales$sum_sales, 
    store_with_max_sales$Store,
    main="Sales according to Store Pie Chart",
    radius=1.0)





##### 2. ##### 
# Which store has maximum standard deviation i.e., the sales vary a lot. 
# Also, find out the coefficient of mean to standard deviation

stores_max_sd <- WalmartStoreSales %>% 
  group_by(Store) %>% 
  summarise(mean_sales = mean(Weekly_Sales), 
            sd_sales = sd(Weekly_Sales),
            cov = sd_sales/mean_sales) %>% 
  arrange(desc(sd_sales))
stores_max_sd[1,]

ans2 <- paste("Store", stores_max_sd[1,1], "has the maximum standard deviation:", 
              stores_max_sd[1,3], "\n Coefficient of variation:", stores_max_sd[1,4])
print(ans2)
# Store mean_sales sd_sales   cov
# <fct>      <dbl>    <dbl> <dbl>
#   1 14      2020978.  317570. 0.157
# [1] "Store 14 has the maximum standard deviation: 317569.949475508 \n Coefficient of variation: 0.157136736009483"
plot(stores_max_sd$Store, stores_max_sd$sd_sales,
     main="Store vs Standard Deviation Plot",
     type="h",
     col="red")


#Find store with max coefficient
print("Store with max coefficient of mean to standard deviation")
arrange(stores_max_sd, desc(cov))[1,]
# [1] "Store with max coefficient of mean to standard deviation"
# Store mean_sales sd_sales   cov
# <fct>      <dbl>    <dbl> <dbl>
#   1 35       919725.  211243. 0.230
plot(stores_max_sd$Store, stores_max_sd$cov,
     main="Store vs Coefficient of Mean to Standard Deviation Plot",
     type="h",
     col="blue")




##### 3. #####
#Which store/s has good quarterly growth rate in Q3’2012
head(WalmartStoreSales, 2)

#Add column quarter and find out sum of each quarter
quarter_details <- WalmartStoreSales %>% 
  mutate(quarter = quarters(Date), Year = as.numeric(format(Date, "%Y"))) %>% 
  group_by(Store, quarter, Year) %>% 
  summarise(quarter_sum = sum(Weekly_Sales)) %>% 
  filter(quarter %in% c("Q2", "Q3") & Year == "2012")

#calculate growth rate
quarterly_growth_rate <- quarter_details %>% 
  group_by(Store) %>% 
  mutate(Q2 = lag(quarter_sum),
         Q3 = quarter_sum,
         growth_rate = ((Q3 - Q2)/Q2)) %>% 
  na.omit() %>% 
  arrange(desc(growth_rate))
quarterly_growth_rate[1,]

ans3 <- paste("Store", quarterly_growth_rate[1, 1], "has the best growth rate in Quarter 3 : ", quarterly_growth_rate[1, 7])
print(ans3)
#[1] "Store 7 has the best growth rate in Quarter 3 :  0.13330776030738"
# Store quarter  Year quarter_sum       Q2       Q3 growth_rate
# <fct> <chr>               <dbl>       <dbl>    <dbl>    <dbl>       <dbl>
#  7     Q3                   2012    8262787. 7290859. 8262787.       0.133

plot(quarterly_growth_rate$Store, 
     quarterly_growth_rate$growth_rate, 
     type="p",
     main="Store vs Growth Rate",
     col="forestgreen")





##### 4. #####
#Some holidays have a negative impact on sales. Find out holidays which have 
#higher sales than the mean sales in non-holiday season for all stores together

#Find mean sales in non holiday season
non_holiday_sales <- filter(WalmartStoreSales, Holiday_Flag == 0)
non_holiday_mean_sales <- mean(WalmartStoreSales$Weekly_Sales)

#Find stores with sale > non holiday mean sales
holidays_higher_sales <- filter(WalmartStoreSales, 
                                Holiday_Flag == 1, 
                                Weekly_Sales > non_holiday_mean_sales)
holidays_higher_sales

print("Holidays which have higher sales than the mean sales in non-holiday 
      season for all stores together: ")

print(unique(holidays_higher_sales$Date))
# [1] "2010-02-12" "2010-09-10" "2010-11-26" "2010-12-31" "2011-02-11"
# [6] "2011-09-09" "2011-11-25" "2011-12-30" "2012-02-10" "2012-09-07"

dim(holidays_higher_sales)
summary(as.factor(holidays_higher_sales$Date))




##### 5 #####
##### Provide a monthly and semester view of sales in units and give insights

head(WalmartStoreSales,2)

#monthly view of sales
MonthlyView_Sales <- WalmartStoreSales %>% 
  na.omit() %>% 
  mutate(Month = as.numeric(format(Date, "%m")), Year = as.numeric(format(Date, "%Y"))) %>% 
  group_by( Month, Year) %>% 
  summarise(Total_Weekly_Sales = sum(Weekly_Sales)) %>% 
  arrange(desc(Total_Weekly_Sales))
MonthlyView_Sales
dim(MonthlyView_Sales)

max = max(MonthlyView_Sales$Total_Weekly_Sales)
ans5 <- paste("Highest sale : " , max, 
      " Month : ", MonthlyView_Sales[MonthlyView_Sales$Total_Weekly_Sales == max, 1],
      "\n Year : ", MonthlyView_Sales[MonthlyView_Sales$Total_Weekly_Sales == max, 2])


min = min(MonthlyView_Sales$Total_Weekly_Sales)
ans5_low <- paste("Lowest sale : " , min, 
              " Month : ", MonthlyView_Sales[MonthlyView_Sales$Total_Weekly_Sales == min, 1],
              "\n Year : ", MonthlyView_Sales[MonthlyView_Sales$Total_Weekly_Sales == min, 2])
print(ans5)
print(ans5_low)
summary(MonthlyView_Sales)

plot(MonthlyView_Sales$Month,
     MonthlyView_Sales$Total_Weekly_Sales,
     type="b",
     col=rainbow(12))
# [1] "Highest sale :  288760532.72  Month :  12 \n Year :  2010"
# > print(ans5_low)
# [1] "Lowest sale :  163703966.83  Month :  1 \n Year :  2011"
 
###### Insight ######
# The highest sale was in December, 2010 then Dec, 2011. 
# Hence, Walmart needs to  adopt promotional strategy similar to what was done in these 2 months
# to increase sales in other months.
# Additionally, demand in December peaks due to holiday event, therefore, Walmart should
# keep more in-demand products in stock.


# Semester wise sale analysis
head(WalmartStoreSales)

Semester_View_Sales <- WalmartStoreSales %>% 
  na.omit() %>% 
  mutate(Semester = semester(Date, 2010)) %>% 
  group_by(Semester) %>% 
  summarise(Total_Sem_Sales = sum(Weekly_Sales)) %>% 
  arrange(desc(Total_Sem_Sales))
Semester_View_Sales

dim(Semester_View_Sales)                          
max_sem <- max(Semester_View_Sales)                            

ans6 <- paste("Highest semester sale : " , max_sem, 
              " Semester : ", Semester_View_Sales[Semester_View_Sales$Total_Sem_Sales == max_sem, 1])                            
ans6                            
# [1] "Highest semester sale :  1320860210.04  Semester :  2011.2"

plot(Semester_View_Sales$Semester,
     Semester_View_Sales$Total_Sem_Sales,
     type="h",
     col=rainbow(2))

###### Insight ######
# The highest sale was achieved in second semester of year 2011.
# Hence, Walmart should adopt the marketing strategies used in this semester 
# and apply them all around the year in order to improve their sales throughout the year.






###################### Statistical Model #############################

# Linear Regression

# For Store 1 – Build  prediction models to forecast demand
Store1_data <- WalmartStoreSales[WalmartStoreSales$Store == 1, ]
str(Store1_data)
summary(Store1_data)

# Arranging date is ascending order
Store1_data <- arrange(Store1_data, Store1_data$Date)
# change dates into days
min_date = min(Store1_data$Date)
Store1_data <- mutate(Store1_data, Days = as.numeric((Store1_data$Date - min_date)+1))

dim(Store1_data)

# Coverting dates into days in sequence
Store1_data <- cbind(Store1_data, Day_Count = transform_Date_DaySequence(Store1_data$Date))
dim(Store1_data)
Store1_data

Store1_data <- Store1_data[-c(1,2)]
str(Store1_data)

cor_data <- cor(Store1_data)
corrplot(cor_data, method = c("number"), type="lower")


#### Regression analysis on multiple variables
Store1_result_all <- lm(formula = Weekly_Sales ~ ., 
                    data = Store1_data)
summary(Store1_result_all)  

# Only Temperature has a significant relation with Weekly sales in Store 1 as p-value is less than 0.05 significant value
# The dependent variable Weekly_Sales does not significantly depend on other independent variables 
# This model has 11.2% accuracy in predicting weekly sales for Store 1
# The model p-value: 0.001035 indicates that this model significantly predicts sales



# Hypothesize if CPI, unemployment, and fuel price have any impact on sales.
#Ho - CPI, unemployment, and fuel price have no impact on sales
#H1 - CPI, unemployment, and fuel price have impact on sales
Store1_result1 <- lm(formula = Weekly_Sales ~ Fuel_Price + CPI + Unemployment, 
                    data = Store1_data)
summary(Store1_result1) 

# Reject Ho for Fuel_Price [p-value: 0.16851]  - No impact on sales
# Do not reject Ho for CPI[0.00164] and Unemployment[0.03659] - indicates that CPI and Unemployment have impact on sales
# This model has 6.5% accuracy in predicting impact


Store1_result6 <- lm(formula = Weekly_Sales ~ CPI, 
                     data = Store1_data)
summary(Store1_result6)
# CPI has a significant impact on Weekly sales in Store 1 as p-value[0.00679] is less than 0.05 significant value
# This model has 5% accuracy in predicting weekly sales for Store 1

plot(Store1_data$CPI, Store1_data$Weekly_Sales)
abline(Store1_result6)







########### LM for WalmartStoreSales
str(WalmartStoreSales)

# Change dates into days by creating new variable.
WalmartStoreSales <- arrange(WalmartStoreSales, WalmartStoreSales$Date)
#WalmartStoreSales$Holiday_Flag <- as.numeric(WalmartStoreSales$Holiday_Flag)
WalmartStoreSales <- cbind(WalmartStoreSales, Day_Count = transform_Date_DaySequence(WalmartStoreSales$Date))
WalmartStoreSales <- mutate(WalmartStoreSales, Days = as.numeric((WalmartStoreSales$Date - min_date)+1))
head(WalmartStoreSales)
str(WalmartStoreSales)
Walmart_data <- WalmartStoreSales[-2]

cor_data <- cor(Walmart_data)
corrplot(cor_data, method = c("number"), type="lower")


# Regression Analysis using multiple variables
Walmart_result <- lm(Weekly_Sales ~ ., data = Walmart_data)
summary(Walmart_result)
# This model [p-value: < 2.2e-16] indicates relationship between independent and dependent variables
# This model indicates that Store, Holiday_Flag, Temperature, CPI, Unemployment have significant impact on sales
# Fuel_Price and Days do not significantly impact sales
# This model has 14.06 % accuracy in predicting Weekly_Sales



# Hypothesize if CPI, unemployment, and fuel price have any impact on sales.
#Ho - CPI, unemployment, and fuel price have no impact on sales
#H1 - CPI, unemployment, and fuel price have impact on sales
Walmart_result1 <- lm(Weekly_Sales ~ CPI + Unemployment + Fuel_Price,
                     data = Walmart_data)
summary(Walmart_result1)
# Reject Ho for Fuel_Price [p-value: 0.212]  - No impact on sales
# Do not reject Ho for CPI[<2e-16] and Unemployment[<2e-16] - indicates that CPI and Unemployment have significant impact on sales
# This model has 2.3% accuracy in predicting impact - the significant independent variables are able to explain 2.3% of variation in Weekly sales




Walmart_result2 <- lm(Weekly_Sales ~ Day_Count,
                      data = Walmart_data)
summary(Walmart_result2)
# Day_Count does not impact Weekly sales
# This model has very poor accuracy

plot(Walmart_result1$model)

# Accuracy of models is seen to be quite low.
# Walmart_result is the model with best accuracy with 14.06 % accuracy in predicting Weekly_Sales



















