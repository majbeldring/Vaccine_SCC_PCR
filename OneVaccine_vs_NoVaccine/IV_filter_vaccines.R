#' 
#' 
#' Maj Beldring, majbh@sund.ku.dk
#' UCPH, November 2021
#' 
#' Paper Vaccines 
#' Filter data for SCC modelling
#' Script #4
#'
#'
# Packages and settings: ----------------------------------------


library(tidyverse)
library(gridExtra)
library(data.table)

Sys.setlocale("LC_ALL","English") # date formatting
memory.size()            # Checking your memory size
memory.limit()           # Checking the set limit
memory.limit(size=56000) # suggest for 64 bit
options(stringsAsFactors = FALSE) # prevent factorizing caracters


# Loading data and preparing data ---------------------------

load("K:/paper_vaccine/III_merge_vaccine.RData") 

dplyr::n_distinct(df_vacc$DYR_ID) # 2377918
dplyr::n_distinct(df_vacc$BES_ID) # 3913

# df_model for paper I --------------------------------------

# filtering milk and SCC values
# can't do all in one. Have to count data loss for the prisma
df_vacc <- df_vacc %>%
  filter(SCC > 0) %>%
  filter(SCC < 9999) %>%
  filter(MILK > 0) %>%   # not mentioned in prisma
  filter(MILK < 100) %>%  # not mentioned in prisma
  filter(BREED == 1) %>%
  filter(HERDTYPE == 1) %>% 
  filter(DIM < 306) %>%
  filter(DIM > 5) %>%
  ungroup() %>%
  dplyr::select(BES_ID, DYR_ID, PARITY, DIM, SCC, VACC) %>%
  mutate(logSCC = log(SCC)) %>% 
  ungroup()
#'
#'better filter:   filter(between(SCC, 1, 9998))
#'
dplyr::n_distinct(df_vacc$DYR_ID)  # 1526149, pre-above: 2377918
dplyr::n_distinct(df_vacc$BES_ID)  # 3064, pre-above: 3913


save.image("K:/paper_vaccine/IV_filter_vaccine_TEMP.RData") 


# Didiving into parity groups ---------------------------

df1_vacc <- df_vacc %>%
  filter(PARITY == 1) %>%
  filter(VACC == 1) %>%
  group_by(BES_ID) %>%
  mutate(count = n()) %>%
  filter(count > 200) %>% # not sure if this should be added
  ungroup() %>%
  mutate(BES_ID = factor(BES_ID)) %>%
  dplyr::select(BES_ID, DYR_ID, DIM, logSCC)

df1_no <- df_vacc %>%
  filter(PARITY == 1) %>%
  filter(VACC == 0) %>%
  group_by(BES_ID) %>%
  mutate(count = n()) %>%
  filter(count > 200) %>% # not sure if this should be added
  ungroup() %>%
  mutate(BES_ID = factor(BES_ID)) %>%
  dplyr::select(BES_ID, DYR_ID, DIM, logSCC)

df2_vacc <- df_vacc %>%
  filter(PARITY == 2) %>%
  filter(VACC == 1) %>%
  group_by(BES_ID) %>%
  mutate(count = n()) %>%
  filter(count > 200) %>% # not sure if this should be added
  ungroup() %>%
  mutate(BES_ID = factor(BES_ID)) %>%
  dplyr::select(BES_ID, DYR_ID, DIM, logSCC)

df2_no <- df_vacc %>%
  filter(PARITY == 2) %>%
  filter(VACC == 0) %>%
  group_by(BES_ID) %>%
  mutate(count = n()) %>%
  filter(count > 200) %>% # not sure if this should be added
  ungroup() %>%
  mutate(BES_ID = factor(BES_ID)) %>%
  dplyr::select(BES_ID, DYR_ID, DIM, logSCC)

df3_vacc <- df_vacc %>%
  filter(PARITY == 3) %>%
  filter(VACC == 1) %>%
  group_by(BES_ID) %>%
  mutate(count = n()) %>%
  filter(count > 200) %>% # not sure if this should be added
  ungroup() %>%
  mutate(BES_ID = factor(BES_ID)) %>%
  dplyr::select(BES_ID, DYR_ID, DIM, logSCC)

df3_no <- df_vacc %>%
  filter(PARITY == 3) %>%
  filter(VACC == 0) %>%
  group_by(BES_ID) %>%
  mutate(count = n()) %>%
  filter(count > 200) %>% # not sure if this should be added
  ungroup() %>%
  mutate(BES_ID = factor(BES_ID)) %>%
  dplyr::select(BES_ID, DYR_ID, DIM, logSCC)

df4_vacc <- df_vacc %>%
  filter(PARITY > 3) %>%
  filter(VACC == 1) %>%
  group_by(BES_ID) %>%
  mutate(count = n()) %>%
  filter(count > 200) %>% # not sure if this should be added
  ungroup() %>%
  mutate(BES_ID = factor(BES_ID)) %>%
  dplyr::select(BES_ID, DYR_ID, DIM, logSCC)

df4_no <- df_vacc %>%
  filter(PARITY > 3) %>%
  filter(VACC == 0) %>%
  group_by(BES_ID) %>%
  mutate(count = n()) %>%
  filter(count > 200) %>% # not sure if this should be added
  ungroup() %>%
  mutate(BES_ID = factor(BES_ID)) %>%
  dplyr::select(BES_ID, DYR_ID, DIM, logSCC)



# Save Data for modelling -----------------------------------

save.image("K:/paper_vaccine/IV_filter_vaccine.RData") 







# Data loss - for Prisma in article -------------------

df0 <- df_all %>%
  filter(SCC > 0) %>%
  filter(SCC < 9999) %>% 
  filter(MILK > 0) %>%   # not mentioned in prisma, but not needed as we dont have any NEG milk
  filter(MILK < 100)     # not mentioned in prisma, but not needed as we dont have any milk >100

df1 <- df0 %>% 
  filter(HERDTYPE == 1)
df2 <- df1 %>% 
  filter(BREED == 1)
df3 <- df2 %>% 
  filter(PCR_TEST == 1)
df4 <- df3 %>% 
  filter(PARITY > 1)

df4 <- df_all %>% 
  filter(SCC > 0) %>%
  filter(SCC < 9999)
df5 <- df_all %>% 
  filter(MILK > 0) %>%
  filter(MILK < 100)
df6 <- df_all %>% 
  filter(DIM < 306) %>%
  filter(DIM > 5) 

# basis
dplyr::n_distinct(df_all$DYR_ID) # 1551899
dplyr::n_distinct(df_all$BES_ID) # 3843

# after screening:
dplyr::n_distinct(df0$DYR_ID) # 1551847
dplyr::n_distinct(df0$BES_ID) # 3843

# herd type
dplyr::n_distinct(df1$DYR_ID)   # 1392592 (1551847-1392592 = 159,255)
dplyr::n_distinct(df1$BES_ID)   # 3492 (loosing 351 eco herds)

# breed
dplyr::n_distinct(df2$DYR_ID)   # 997713 (loosing 394879 animals)
dplyr::n_distinct(df2$BES_ID)   # 2970 (loosing 552 herds)

# pcr tested
dplyr::n_distinct(df3$DYR_ID)   # 214992 (loosing 782721)
dplyr::n_distinct(df3$BES_ID)   # 1492 (loosing 1478 herds)

# data in each model ready group
dplyr::n_distinct(df2_neg$DYR_ID)   # 
dplyr::n_distinct(df2_neg$BES_ID)   # 

dplyr::n_distinct(df2_pos$DYR_ID)   # 
dplyr::n_distinct(df2_pos$BES_ID)   # 

dplyr::n_distinct(df3_neg$DYR_ID)   # 
dplyr::n_distinct(df3_neg$BES_ID)   # 

dplyr::n_distinct(df3_pos$DYR_ID)   # 
dplyr::n_distinct(df3_pos$BES_ID)   # 

dplyr::n_distinct(df4_neg$DYR_ID)   # 
dplyr::n_distinct(df4_neg$BES_ID)   # 

dplyr::n_distinct(df4_pos$DYR_ID)   # 
dplyr::n_distinct(df4_pos$BES_ID)   # 


df_included <- df_all %>%
  filter(SCC > 0) %>%
  filter(SCC < 9999) %>%
  filter(MILK > 0) %>%   # not mentioned in prisma
  filter(MILK < 100) %>%  # not mentioned in prisma
  filter(PCR_TEST == 1) %>%
  filter(BREED == 1) %>%
  filter(HERDTYPE == 1) %>% 
  filter(DIM < 306) %>%
  filter(DIM > 5) %>%
  filter(PARITY > 1) %>%
  ungroup() %>%
  dplyr::select(BES_ID, DYR_ID, PARITY, DIM, SCC, MILK, RES_MAJOR, RES_MINOR) %>%
  mutate(logSCC = log(SCC)) %>% 
  ungroup()

dplyr::n_distinct(df_included$DYR_ID)   # 
dplyr::n_distinct(df_included$BES_ID)   # 


rm(df1, df2, df3, df4, df5, df6); gc()


# distributions - Visualisations -----------------------------


# # removing SCC=1 (to avoid logSCC=0)
# ggplot(df4, aes(x=SCC)) + 
#   geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
#                  colour="black", fill="white") +
#   scale_x_continuous(trans="log10") +
#   geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot

# ggplot(df_all, aes(x=MILK)) +
#   geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
#                  colour="black", fill="white") +
#   #scale_x_continuous(trans="log10") +
#   geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot

## save data with SCC=1 (logSCC=0), before removing:
#df_with_SCC1 <- df4

#dplyr::n_distinct(df5$DYR_ID)   # 214916 , loosing none
#dplyr::n_distinct(df5$BES_ID)   # 1486, loosing non!
# however loosing: df4_obs - df5_obs = 2753641 - 2752698 = 943 observations






# MISC - not applied ------------------------------------

# # remove logSCC= 0. Do not impact model. Keep logSCC=0 (SCC=1)
# df5 <- df4 %>%
#   filter(logSCC > 0)


# Parity grouping
# not needed. Do this directly when creting the 6 different dataframes
# df5 <- df4 %>%
#   filter(PARITY > 1) %>%
#   mutate(
#     PARITY = as.numeric(PARITY),
#     PARITY = replace(PARITY, PARITY > 3, 4),
#     PARITY = as.factor(PARITY))


# # create with herds with min 50 animals and 200 obs.
# This is instead done when creating the small data 
# df6 <- df5 %>%
#   group_by(BES_ID) %>%
#   filter(n() > 200)
# df6 <- df5 %>%
#   group_by(BES_ID, DYR_ID) %>%
#   filter(n() > 200)
# create coloumn counting herd occurences
# df <- df %>%
#   group_by(BES_ID, PARITY) %>%
#   mutate(count = n())


# factorize BES_ID. # don't do it with DYR_ID. Not needed for now. Data must be smaller for this










# Grouping data for modelling -------------------------------------------


# PARITY 2 - with SCC=1 (logSCC = 0) included ---------------------------

# # creting a Parity 2 test data with SCC=1 (logSCC=0)
# df_SCC_zero_P2_pos <- df_all %>%
#   filter(PCR_TEST == 1) %>%
#   filter(BREED == 1) %>%
#   filter(HERDTYPE == 1) %>%
#   filter(SCC > 0) %>%
#   filter(SCC < 9999) %>%
#   filter(MILK > 0) %>%
#   filter(MILK < 100) %>%
#   mutate(logSCC = log(SCC)) %>% 
#   filter(PARITY == 2) %>%
#   filter(RES_MAJOR == 1) %>%
#   group_by(BES_ID) %>%
#   mutate(count = n()) %>%
#   filter(count > 200) %>% # not sure if this should be added
#   ungroup() %>%
#   mutate(BES_ID = factor(BES_ID)) %>%
#   dplyr::select(BES_ID, DYR_ID, DIM, logSCC, SCC)
# 
# df_SCC_zero_P2_neg <- df_all %>%
#   filter(PCR_TEST == 1) %>%
#   filter(BREED == 1) %>%
#   filter(HERDTYPE == 1) %>%
#   filter(SCC > 0) %>%
#   filter(SCC < 9999) %>%
#   filter(MILK > 0) %>%
#   filter(MILK < 100) %>%
#   mutate(logSCC = log(SCC)) %>% 
#   filter(PARITY == 2) %>%
#   filter(RES_MAJOR == 0) %>%
#   group_by(BES_ID) %>%
#   mutate(count = n()) %>%
#   filter(count > 200) %>% # not sure if this should be added
#   ungroup() %>%
#   mutate(BES_ID = factor(BES_ID)) %>%
#   dplyr::select(BES_ID, DYR_ID, DIM, logSCC, SCC)
# 
# 
# 
# # PARITY 1 -------------------------------------------
# 
# # RES MAJOR POS ; min 200 obs
# df1_pos <- df_model %>%
#   ungroup() %>%
#   filter(PARITY == 1) %>%
#   filter(RES_MAJOR == 1) %>%
#   group_by(BES_ID) %>%
#   mutate(count = n()) %>%
#   filter(count > 200) %>%
#   ungroup() %>%
#   #dplyr::select(BES_ID, DIM, logSCC)
#   dplyr::select(BES_ID, DYR_ID, DIM, logSCC, SCC)
# df2_pos$BES_ID <- factor(df2_pos$BES_ID) # keep only used levels by resetting the variable
# 
# dplyr::n_distinct(df1_pos$BES_ID) # 557
# dplyr::n_distinct(df1_pos$DYR_ID) # 49107
# summary(df1_pos)
# 
# 
# # RES MAJOR POS ; min 200 obs
# df1_neg <- df_model %>%
#   ungroup() %>%
#   filter(PARITY == 1) %>%
#   filter(RES_MAJOR == 0) %>%
#   group_by(BES_ID) %>%
#   mutate(count = n()) %>%
#   filter(count > 200) %>%
#   ungroup() %>%
#   #dplyr::select(BES_ID, DIM, logSCC)
#   dplyr::select(BES_ID, DYR_ID, DIM, logSCC, SCC)
# df1_neg$BES_ID <- factor(df1_neg$BES_ID) # keep only used levels by resetting the variable
# 
# dplyr::n_distinct(df1_neg$BES_ID) # 643
# dplyr::n_distinct(df1_neg$DYR_ID) # 77811
# summary(df1_neg)
# 
# 
# 
# 
# # all negative - only for Parity 2 --------------------------
# 
# # df2_ALL_NEG
# df2_ALL_NEG <- df_model %>%
#   ungroup() %>%
#   filter(PARITY == 1) %>%
#   filter(RES_MAJOR == 0) %>%
#   filter(RES_MINOR == 0) %>%
#   group_by(BES_ID) %>%
#   mutate(count = n()) %>%
#   filter(count > 200) %>% # not sure if this should be added
#   ungroup() %>%
#   dplyr::select(BES_ID, DYR_ID, DIM, logSCC, SCC)
# #dplyr::select(BES_ID, DYR_ID, DIM, logSCC)
# df2_ALL_NEG$BES_ID <- factor(df2_ALL_NEG$BES_ID) # keep only used levels by resetting the variable
# 
# dplyr::n_distinct(df2_ALL_NEG$BES_ID) # 41
# dplyr::n_distinct(df2_ALL_NEG$DYR_ID) # 1731
# 
# 
# 
# # PARITY 2 ----------------------------------------------
# 
# # RES MAJOR POS ; min 200 obs
# df2_pos <- df_model %>%
#   ungroup() %>%
#   filter(PARITY == 2) %>%
#   filter(RES_MAJOR == 1) %>%
#   group_by(BES_ID) %>%
#   mutate(count = n()) %>%
#   filter(count > 200) %>%
#   ungroup() %>%
#   #dplyr::select(BES_ID, DIM, logSCC)
#   dplyr::select(BES_ID, DYR_ID, DIM, logSCC, SCC)
# df2_pos$BES_ID <- factor(df2_pos$BES_ID) # keep only used levels by resetting the variable
# 
# dplyr::n_distinct(df2_pos$BES_ID) # 528
# dplyr::n_distinct(df2_pos$DYR_ID) # 41136
# summary(df2_pos)
# 
# # RES MAJOR NEG ; min 200 obs
# df2_neg <- df_model %>%
#   ungroup() %>%
#   filter(PARITY == 2) %>%
#   filter(RES_MAJOR == 0) %>%
#   group_by(BES_ID) %>%
#   mutate(count = n()) %>%
#   filter(count > 200) %>%
#   ungroup() %>%
#   #dplyr::select(BES_ID, DIM, logSCC)
#   dplyr::select(BES_ID, DYR_ID, DIM, logSCC, SCC)
# df2_neg$BES_ID <- factor(df2_neg$BES_ID) # keep only used levels by resetting the variable
# 
# dplyr::n_distinct(df2_neg$BES_ID) # 554
# dplyr::n_distinct(df2_neg$DYR_ID) # 44210
# summary(df2_neg)
# 
# 
# # PARITY 3-------------------------------------------
# # RES MAJOR POS ; min 200 obs
# df3_pos <- df_model %>%
#   ungroup() %>%
#   filter(PARITY == 3) %>%
#   filter(RES_MAJOR == 1) %>%
#   group_by(BES_ID) %>%
#   mutate(count = n()) %>%
#   filter(count > 200) %>%
#   ungroup() %>%
#   #dplyr::select(BES_ID, DIM, logSCC)
#   dplyr::select(BES_ID, DYR_ID, DIM, logSCC, SCC)
# df3_pos$BES_ID <- factor(df3_pos$BES_ID) # keep only used levels by resetting the variable
# 
# dplyr::n_distinct(df3_pos$BES_ID) # 414
# dplyr::n_distinct(df3_pos$DYR_ID) # 22722
# summary(df3_pos)
# 
# # RES MAJOR NEG ; min 200 obs
# df3_neg <- df_model %>%
#   ungroup() %>%
#   filter(PARITY == 3) %>%
#   filter(RES_MAJOR == 0) %>%
#   group_by(BES_ID) %>%
#   mutate(count = n()) %>%
#   filter(count > 200) %>%
#   ungroup() %>%
#   #dplyr::select(BES_ID, DIM, logSCC)
#   dplyr::select(BES_ID, DYR_ID, DIM, logSCC, SCC)
# df3_neg$BES_ID <- factor(df3_neg$BES_ID) # keep only used levels by resetting the variable
# 
# dplyr::n_distinct(df3_neg$BES_ID) # 373
# dplyr::n_distinct(df3_neg$DYR_ID) # 20063
# summary(df3_neg)
# 
# # PARITY 4 ------------------------------------------
# # RES MAJOR POS ; min 200 obs
# df4_pos <- df_model %>%
#   ungroup() %>%
#   filter(PARITY > 3) %>%
#   filter(RES_MAJOR == 1) %>%
#   group_by(BES_ID) %>%
#   mutate(count = n()) %>%
#   filter(count > 200) %>%
#   ungroup() %>%
#   #dplyr::select(BES_ID, DIM, logSCC)
#   dplyr::select(BES_ID, DYR_ID, DIM, logSCC, SCC, PARITY)
# df4_pos$BES_ID <- factor(df4_pos$BES_ID) # keep only used levels by resetting the variable
# 
# dplyr::n_distinct(df4_pos$BES_ID) # 300
# dplyr::n_distinct(df4_pos$DYR_ID) # 12027
# summary(df4_pos)
# 
# # RES MAJOR NEG ; min 200 obs
# df4_neg <- df_model %>%
#   ungroup() %>%
#   filter(PARITY > 3) %>%
#   filter(RES_MAJOR == 0) %>%
#   group_by(BES_ID) %>%
#   mutate(count = n()) %>%
#   filter(count > 200) %>%
#   ungroup() %>%
#   #dplyr::select(BES_ID, DIM, logSCC, PARITY)
#   dplyr::select(BES_ID, DYR_ID, DIM, logSCC, SCC, PARITY)
# df4_neg$BES_ID <- factor(df4_neg$BES_ID) # keep only used levels by resetting the variable
# 
# dplyr::n_distinct(df4_neg$BES_ID) # 265
# dplyr::n_distinct(df4_neg$DYR_ID) # 10351
# summary(df4_neg)
# 
# 
