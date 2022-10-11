
# Loading Libraries
library(fixest)
library(readxl)
library(RCurl)
library(tidyverse)

#Import csv file from github repository
myfile <- getURL("https://raw.githubusercontent.com/fgomespereira/P1---Unconventional-Monetary-Policy-in-the-EA/main/Panel%201.csv")
panel1 <- read.csv(text = myfile)
tmp2 <- panel1

# APP_DUMMY setting
tmp2$app_dummy <- ifelse(tmp2$Year==2015 |
                           tmp2$Year==2016 |
                           tmp2$Year==2017 |
                           tmp2$Year==2018, 1, 0)

# Remove from 2012 to 2018
tmp2 <- subset(tmp2, Year>=2012 & Year<=2018)

# Make Year as factor
tmp2$Year <- as.factor(tmp2$Year)

# Regression
reg_1 <- feols(Loans_G ~
                 log(Assets)  +
                 Equity_over_A +
                 ROA +
                 Deposits_over_A +
                 Exposure_app * app_dummy |
                 Bank + Year,
                 data = tmp2)

reg_2 <- feols(Loans_G ~
                 log(Assets)  +
                 Equity_over_A +
                 ROA +
                 Deposits_over_A +
                 treat_50 * app_dummy |
                 Bank + Year,
                 data = tmp2)

reg_3 <- feols(Loans_G ~
                 log(Assets)  +
                 Equity_over_A +
                 ROA +
                 Deposits_over_A +
                 treat_75 * app_dummy |
                 Bank + Year,
                 data = tmp2)

reg_4 <- feols(Loans_G ~
                 log(Assets)  +
                 Equity_over_A +
                 ROA +
                 Deposits_over_A +
                 Exposure_app * app_dummy +
                 Country * Year |
                 Bank + Year,
                 data = tmp2)

reg_5 <- feols(Loans_G ~
                 log(Assets)  +
                 Equity_over_A +
                 ROA +
                 Deposits_over_A +
                 treat_50 * app_dummy +
                 Country * Year |
                 Bank + Year,
                 data = tmp2)


reg_6 <- feols(Loans_G ~
                 log(Assets) +
                 Equity_over_A +
                 ROA  +
                 Deposits_over_A +
                 treat_75 * app_dummy +
                 Country * Year |
                 Bank + Year,
                 data = tmp2)


reg_7 <- feols(L_over_A_G ~
                 log(Assets)  +
                 Equity_over_A +
                 ROA +
                 Deposits_over_A +
                 Exposure_app * app_dummy |
                 Bank + Year,
                 data = tmp2)

reg_8 <- feols(L_over_A_G ~
                 log(Assets)  +
                 Equity_over_A +
                 ROA +
                 Deposits_over_A +
                 treat_50 * app_dummy |
                 Bank + Year,
                 data = tmp2)

reg_9 <- feols(L_over_A_G ~
                 log(Assets)  +
                 Equity_over_A +
                 ROA +
                 Deposits_over_A +
                 treat_75 * app_dummy |
                 Bank + Year,
                 data = tmp2)

reg_10 <- feols(L_over_A_G ~
                  log(Assets) +
                  Equity_over_A +
                  ROA +
                  Deposits_over_A +
                  Exposure_app * app_dummy +
                  Country * Year |
                  Bank + Year,
                  data = tmp2)

reg_11 <- feols(L_over_A_G ~
                  log(Assets) +
                  Equity_over_A +
                  ROA +
                  Deposits_over_A +
                  treat_50 * app_dummy +
                  Country * Year |
                  Bank + Year,
                  data = tmp2)


reg_12 <- feols(L_over_A_G ~
                  log(Assets) +
                  Equity_over_A +
                  ROA +
                  Deposits_over_A +
                  treat_75 * app_dummy +
                  Country * Year |
                  Bank + Year,
                  data = tmp2)

### REGRESSION TABLES

# Regression tables
etable(reg_1, reg_2, reg_3, reg_4, reg_5, reg_6, reg_7, reg_8, reg_9, reg_10, reg_11, reg_12,
       drop = c("Country"), # suppress Country*Year coefficients 
       postprocess.df = pandoc.table.return, style = "rmarkdown")




