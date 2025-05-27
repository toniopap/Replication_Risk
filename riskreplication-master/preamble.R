#### Load all relevant packages and read self-written functions


load("data/allcountrydata.Rdata")


library(evd)
library(sjPlot)
library(ggplot2)
library(ggdist)
library(ggpubr)
library(sjmisc)
library(gt)
library(gtsummary)
library(dplyr)
library(tidyr)
library(tidylog)
library(kableExtra)
library(texreg)
library(maxLik)
library(sandwich)
library(lmtest)
library(haven)
library(eurostat)
library(rvest)
library(knitr)
library(forcats)




source("functions.R")                                              ## functions used in the code
 
source("ml_functions.R")                                           ## functions to estimate maximum likelihood models


