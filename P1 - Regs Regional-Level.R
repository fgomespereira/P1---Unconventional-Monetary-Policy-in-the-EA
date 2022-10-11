# Loading Libraries
library(fixest)
library(readxl)
library(RCurl)
library(tidyverse)

#Import csv file from github repository
myfile <- getURL("https://raw.githubusercontent.com/fgomespereira/P1---Unconventional-Monetary-Policy-in-the-EA/main/Panel%202.csv")
panel2 <- read.csv(text = myfile)
tmp2 <- panel2

# APP_DUMMY setting
tmp2$app_dummy <- ifelse(tmp2$Year==2015 |
                           tmp2$Year==2016 |
                           tmp2$Year==2017 |
                           tmp2$Year==2018, 1, 0)


reg_1 <- feols(NUTS2_GDP_G ~ log(NUTS2_GDP) + 
                 log(NUTS2_POP) + 
                 NUTS2_UNE + 
                 log(NUTS2_COMP) +
                 log(NUTS2_GPC) +
                 Exposure * app_dummy |
                 NUTS2 + Year + Country,
                 data = tmp2)

reg_2 <- feols(NUTS2_GDP_G ~ log(NUTS2_GDP) + 
                 log(NUTS2_POP) + 
                 NUTS2_UNE + 
                 log(NUTS2_COMP) +
                 log(NUTS2_GPC) +
                 treat_50 * app_dummy |
                 NUTS2 + Year + Country,
                 data = tmp2)

reg_3 <- feols(NUTS2_GDP_G ~ log(NUTS2_GDP) + 
                 log(NUTS2_POP) + 
                 NUTS2_UNE + 
                 log(NUTS2_COMP) +
                 log(NUTS2_GPC) +
                 treat_75 * app_dummy |
                 NUTS2 + Year + Country,
                 data = tmp2)

reg_4 <- feols(NUTS2_UNE_G ~ log(NUTS2_GDP) + 
                 log(NUTS2_POP) + 
                 NUTS2_UNE + 
                 log(NUTS2_COMP) +
                 log(NUTS2_GPC) +
                 Exposure * app_dummy |
                 NUTS2 + Year + Country,
                 data = tmp2)

reg_5 <- feols(NUTS2_UNE_G ~ log(NUTS2_GDP) + 
                 log(NUTS2_POP) + 
                 NUTS2_UNE + 
                 log(NUTS2_COMP) +
                 log(NUTS2_GPC) +
                 treat_50 * app_dummy |
                 NUTS2 + Year + Country,
                 data = tmp2)

reg_6 <- feols(NUTS2_UNE_G ~ log(NUTS2_GDP) + 
                 log(NUTS2_POP) + 
                 NUTS2_UNE + 
                 log(NUTS2_COMP) +
                 log(NUTS2_GPC) +
                 treat_75 * app_dummy |
                 NUTS2 + Year + Country,
                 data = tmp2)

reg_7 <- feols(NUTS2_GFCF_G ~ log(NUTS2_GDP) + 
                 log(NUTS2_POP) + 
                 NUTS2_UNE + 
                 log(NUTS2_COMP) +
                 log(NUTS2_GPC) +
                 Exposure * app_dummy |
                 NUTS2 + Year + Country,
                 data = tmp2)

reg_8 <- feols(NUTS2_GFCF_G ~ log(NUTS2_GDP) + 
                 log(NUTS2_POP) + 
                 NUTS2_UNE + 
                 log(NUTS2_COMP) +
                 log(NUTS2_GPC) +
                 treat_50 * app_dummy |
                 NUTS2 + Year + Country,
                 data = tmp2)

reg_9 <- feols(NUTS2_GFCF_G ~ log(NUTS2_GDP) + 
                 log(NUTS2_POP) + 
                 NUTS2_UNE + 
                 log(NUTS2_COMP) +
                 log(NUTS2_GPC) +
                 treat_75 * app_dummy |
                 NUTS2 + Year + Country,
                 data = tmp2)

reg_10 <- feols(NUTS2_COMP_G ~ log(NUTS2_GDP) + 
                  log(NUTS2_POP) + 
                  NUTS2_UNE + 
                  log(NUTS2_COMP) +
                  log(NUTS2_GPC) +
                  Exposure * app_dummy |
                  NUTS2 + Year + Country,
                  data = tmp2)

reg_11 <- feols(NUTS2_COMP_G ~ log(NUTS2_GDP) + 
                  log(NUTS2_POP) + 
                  NUTS2_UNE + 
                  log(NUTS2_COMP) +
                  log(NUTS2_GPC) +
                  treat_50 * app_dummy |
                  NUTS2 + Year + Country,
                  data = tmp2)

reg_12 <- feols(NUTS2_COMP_G ~ log(NUTS2_GDP) + 
                  log(NUTS2_POP) + 
                  NUTS2_UNE + 
                  log(NUTS2_COMP) +
                  log(NUTS2_GPC) +
                  treat_75 * app_dummy |
                  NUTS2 + Year + Country,
                  data = tmp2)



### REGRESSION TABLES

# Regression tables
etable(reg_1, reg_2, reg_3, reg_4, reg_5, reg_6, reg_7, reg_8, reg_9, reg_10, reg_11, reg_12,
       drop = c("NUTS2_GDP", "NUTS2_POP","NUTS2_COMP", "NUTS2_GPC", "NUTS2_UNE"), # suppress control coefficients 
       postprocess.df = pandoc.table.return, style = "rmarkdown")
