

# Maj Beldring Henningsen, majbh@sund.ku.dk

# Paper vaccince
# Script V
# nlme with nls starthelp


# Packages and settings ----------------------------------------

library(tidyverse)
library(ggpubr) # p values in boxplot
library(gridExtra) # gridarrange
ggplot2::theme_set(ggplot2::theme_bw())  # globally sets ggplot2 theme to theme_bw
library(GGally) # for ggpairs
library(nls.multstart)
library(nlme) # for nlslist

#Sys.setlocale("LC_ALL","English") # date formatting
#memory.size()            # Checking your memory size
#memory.limit()           # Checking the set limit
#memory.limit(size=56000) # suggest for 64 bit
#options(stringsAsFactors = FALSE) # prevent factorizing caracters


# Loading data and preparing data -------------------------------------

load("K:/paper_vaccine/IV_filter_vaccine.RData")
rm(df_vacc); gc()


# wilmink function -----------------------------------------

f_wilmink <- function(DIM, a,b,k,d){
  a + b * DIM + exp(-(exp(k)) * DIM)*d
}


# nls multistart (not grouped herd level) ------------------


# repeat for all 8 diff. datasets
nls_no1 <- nls.multstart::nls_multstart(logSCC ~ f_wilmink(DIM, a, b, k, d),
                                         data = df1_no,
                                         lower=c(a=0, b=0, k=-5, d=0),
                                         upper=c(a=9, b=1.5, k=0, d=5),
                                         start_lower = c(a=0, b=0, k=-5, d=0),
                                         start_upper = c(a=8, b=1, k=-0.01, d=4),
                                         iter = 500,
                                         supp_errors = "Y")

nls_out_no1 <- coef(nls_no1) %>% 
  as_tibble() 

# Temp file with all NLS output
save.image("K:/paper_vaccine/V_modeloutput_vaccine_TEMP_NLS.RData")

# nlme wilmink ----------------------------------------------


# repeat for all 6 different data sets
nlme_vacc3 <- nlme(logSCC ~ a + b * DIM + exp(-(exp(k)) * DIM)*d,
                  data=df3_vacc,
                  fixed=a+b+k+d~1,
                  random=a+b+k+d~1,
                  groups=~BES_ID,
                  start = nls_out_vacc3$value,
                  # start=c(a = 3.6, b = 0.0016, k = -2.5, d = 2.2), # df1_neg -check median for better start
                  # start=c(a = 3.6, b = 0.0032, k = -2.4, d = 1.9), # df2_neg - ok start
                  # start=c(a = 3.9, b = 0.0035, k = -1.8, d = 4.0), # df3_neg -check median for better start
                  # start=c(a = 4.0, b = 0.0036, k = -1.9, d = 2.9), # df4_neg -check median for better start
                  na.action=na.exclude,
                  control = list(maxIter = 1200, msMaxIter = 1200))

nlme_out_vacc3 <- coef(nlme_vacc3) %>% 
  as_tibble() 


# temp saved vacc 1, vacc2
save.image("K:/paper_vaccine/V_modeloutput_vaccine_TEMP3.RData")




# Saving -----------------------------

save.image("K:/paper_vaccine/V_modeloutput_vaccine.RData")
