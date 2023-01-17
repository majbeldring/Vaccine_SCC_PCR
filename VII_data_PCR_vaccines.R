

# Maj Beldring Henningsen, majbh@sund.ku.dk

# PCR test vs Vaccinations
# prepare data

# Prepare data with E. coli and Staph A.
# Vaccines
# not other treatments or pathogens
# include production data to get last and first SCC (to compare...)



# Packages and settings -------------------------------------

library(tidyverse)
library(ggpubr) # p values in boxplot
library(gridExtra) # gridarrange
library(lubridate) # for date wrangling
ggplot2::theme_set(ggplot2::theme_bw())  # globally sets ggplot2 theme to theme_bw
library(GGally) # for ggpairs
options(stringsAsFactors = FALSE) # prevent factorizing caracters


# Loading data and preparing data -------------------------------------

# treatments
sundhed       <- read_csv("M:/data/sundhed.csv") 
lksygdomskode <- read.csv("M:/data/lksygdomskode_fixed.csv") #debugged prior loading
AB_treatments <- read_csv("M:/AB_treatments.csv") # AB treated diseases identified by Søren & Jeanette
# vetpcr
vetrespcr     <- read_csv("M:/data/vetrespcrNY.csv") # problems with kirtel_id
vetresdyr     <- read_csv("M:/data/vetresdyrNY.csv") # problems with virus data
vetpcrkode    <- read_csv("M:/data/vetpcrkode_fixed.csv")
# calvings
kaelvninger   <- read_csv("M:/data/kaelvninger.csv") 
# production - needed to make connection from DYR_ID to BES_ID
yktr          <- read_csv("M:/data/yktr.csv")
# breed
dyrinfo       <- read_csv("M:/data/dyrinfo.csv") 
racekode      <- read_csv("M:/data/racekode.csv")
# herd
brugsart      <- read_csv("M:/data/brugsart.csv")
brugsartkode  <- read_csv("M:/data/brugsartkode.csv")


 # vetpcr ------------------------------------------
# vetrespcr, vetresdyr, vetpcrkode, create:vetpcr

str(vetrespcr); str(vetresdyr); str(vetpcrkode) # udtagdato format: Date

# vetrespcr:
vetrespcr <- vetrespcr %>%
  dplyr::select(VETPCRKD_ID, VETRESDYR_ID, ANALYSEDATAEJAFR) %>%
  filter(!is.na(VETRESDYR_ID)) %>%
  rename(ID = VETPCRKD_ID, PCR_VALUE = ANALYSEDATAEJAFR )

# merge with vetpcrkode to include kode ID
vetrespcr <- left_join(vetrespcr, vetpcrkode , by = "ID") %>%
  dplyr::select(VETRESDYR_ID, PCR_VALUE, NAVNKORT) %>%
  rename(PATHOGEN = NAVNKORT)

# vetresdyr; PCR test = vetprtype 10 according to vetprovetype:
vetresdyr <- vetresdyr %>%
  filter(UDTAGDATO > as.Date("2009-12-31")) %>%
  filter(VETPRTYPE_ID == 10) %>%
  dplyr::select(ID, DYR_ID, UDTAGDATO) %>%
  rename(VETRESDYR_ID = ID, PCR_DATE = UDTAGDATO)

# create vetpcr: joining vetresdyr and vetrespcr
vetpcr <- full_join(vetrespcr, vetresdyr, by = "VETRESDYR_ID", sort="TRUE",allow.cartesian=TRUE) %>%
  dplyr::select(-VETRESDYR_ID) %>%
  drop_na()

# vetpcr now replace vetresdyr, vetrespcr and vetpcrkode
rm(vetresdyr, vetrespcr, vetpcrkode); gc()


# breed ----------------------------------------------------------
# create dataset with only animal ID and breed (NA when merging: rename to `other`)

str(dyrinfo); str(racekode) # FOEDSELSDATO is date format

dyrinfo <- dyrinfo %>%
  dplyr::select(ID, RACE_ID, FOEDSELSDATO) %>%
  rename(BIRTH = FOEDSELSDATO, DYR_ID = ID)

racekode <- racekode %>%
  dplyr::select(ID, RACENAVN) %>%
  rename(RACE_ID = ID, RACE = RACENAVN )

# join racekode and dyrinfo to create breed:
breed <- left_join(dyrinfo, racekode , by = "RACE_ID") %>%
  dplyr::select(-RACE_ID) %>%
  drop_na()

rm(dyrinfo, racekode); gc()



# yktr -> production ------------------------------------------------------

str(yktr) # kontroldato format: Date

production <- yktr %>%
  dplyr::select(DYR_ID, BES_ID, KONTROLDATO, CELLETAL, KGMAELK) %>%
  drop_na() %>%
  filter(KONTROLDATO > as.Date("2009-12-31")) %>%
  rename(SCC = CELLETAL, MILK = KGMAELK)

rm(yktr); gc()





# calvings ------------------------------------------------------

str(kaelvninger) # kaelvedato format: POSIXc

calvings <- kaelvninger %>%
  mutate_if(~'POSIXt' %in% class(.x), as.Date) %>% # change date format
  dplyr::select(DYR_ID, KAELVEDATO, KAELVNINGSNR) %>%
  filter(KAELVEDATO > as.Date("2009-12-31")) %>%
  drop_na() %>%
  rename(CALVING_DATE = KAELVEDATO, PARITY = KAELVNINGSNR)

rm(kaelvninger); gc()



# vaccinations -------------------------------------------

str(sundhed); str(lksygdomskode) # sygdomsdato format: Date


treatments <- sundhed %>%
  filter(SYGDOMSDATO > as.Date("2009-12-31")) %>%
  dplyr::select(DYR_ID, SYGDOMSDATO, LKSK_ID) %>%
  drop_na() %>%
  rename(TREATMENT_DATE = SYGDOMSDATO, ID = LKSK_ID)

treatments <- left_join(treatments, lksygdomskode , by = "ID") %>%
  dplyr::select(DYR_ID, TREATMENT_DATE, LKSYGTEKST) %>%
  rename(DISEASE = LKSYGTEKST)


rm(sundhed, lksygdomskode, AB_treatments); gc()



# brugsart -> herd, eco vs con --------------------------------------------

str(brugsart); str(brugsartkode)

brugsart <- brugsart %>%
  rename(ID = BRUGSART_ID) %>%
  group_by(DATO_TIL) %>% 
  replace_na(list(DATO_TIL = as.Date("2020-06-01")))

brugsartkode <- dplyr::filter(brugsartkode, grepl('lk', BRUGSARTTEKST)) # keep only milk herds
brugsartkode <- brugsartkode %>%
  dplyr::select(ID, BRUGSARTTEKST) %>%
  rename(HERD_TYPE = BRUGSARTTEKST) %>%
  mutate(HERD_TYPE = if_else(str_detect(HERD_TYPE, pattern = "logisk$"), "eco", HERD_TYPE)) %>%
  mutate(HERD_TYPE = if_else(str_detect(HERD_TYPE, pattern = "lk"), "con", HERD_TYPE))


# create herd dataset by joining code and brugsart:
herd <- left_join(brugsart, brugsartkode , by = "ID") %>%
  dplyr::select(-ID) %>%
  drop_na()

rm(brugsart, brugsartkode); gc()



# check classes for each variable ------------------------------------
# (note: better to wait changing to factors after joining)

str(breed)
str(calvings)
str(production)
str(herd)
str(treatments)
str(vetpcr)


# Temp saving
save.image("K:/paper_vaccine/VII_data_PCR_vaccine_TEMP.RData")





# Filtering -------------------------------------------

# Breed
breed <- breed %>% 
  mutate(RACE = if_else(str_detect(RACE, pattern = "Holstein"), "holstein", RACE)) %>%
  mutate(RACE = if_else(str_detect(RACE, pattern = "Jersey"), "jersey", RACE)) %>%
  mutate(RACE = if_else(str_detect(RACE, pattern = "broget"), "other", RACE)) %>%
  mutate(RACE = if_else(str_detect(RACE, pattern = "alkerace$"), "other", RACE)) %>%
  mutate(RACE = if_else(str_detect(RACE, pattern = "Krydsning"), "other", RACE))

breed <- dplyr::filter(breed, grepl('holstein|jersey|other', RACE)) #keep only 3

# add coloumn to breed wit numerious values:
breed <- breed %>% 
  mutate(BREED = case_when(RACE == "holstein" ~ 1, 
                           RACE == "jersey" ~ 2, 
                           RACE == "other" ~ 3)) 



# vetpcr 
str(vetpcr)

# keeping only pcr pathogens. Recall: agalactiae = B.strep
pcr <- dplyr::filter(vetpcr, grepl('aureus|oli', PATHOGEN)) 

pcr <- pcr %>% 
  mutate(PATHOGEN = if_else(str_detect(PATHOGEN, pattern = "oli$"), "e.coli", PATHOGEN)) %>%
  mutate(PATHOGEN = if_else(str_detect(PATHOGEN, pattern = "aureus$"), "s.aureus", PATHOGEN)) %>%
  relocate(DYR_ID, PCR_DATE, PCR_VALUE, PATHOGEN)

pcr <- pcr %>%
  add_column(PCR_TEST = 1) 

rm(vetpcr); gc()



# Vaccinations
# 206, 120340, 340, Mastitis Staf/E.coli, vaccination, 1299

# mastitis vaccine, with 1 for vaccine:
vaccination <- dplyr::filter(treatments, 
                             grepl('Mastitis Staf/E.coli, vaccination', DISEASE))
# Binary vaccination status
vaccination <- vaccination %>% 
  mutate(VACC = case_when(DISEASE == "Mastitis Staf/E.coli, vaccination" ~ 1)) %>% 
  rename(VACCINATION_DATE = TREATMENT_DATE) %>%
  dplyr::select(-DISEASE)

rm(treatments); gc()


# Temp saving
save.image("K:/paper_vaccine/VII_data_PCR_vaccine_TEMP1.RData")





# MERGE ---------------------------------------------------------

## df1: Production + herd type
df1 <- left_join(production, herd, by = "BES_ID", sort="TRUE",allow.cartesian=TRUE)
df1 <- df1 %>% 
  filter(DATO_FRA <= KONTROLDATO) %>% 
  filter(DATO_TIL >= KONTROLDATO) %>%
  dplyr::select(-DATO_FRA, -DATO_TIL) %>%
  mutate(HERDTYPE = case_when(HERD_TYPE == 'con' ~ 1, HERD_TYPE == 'eco' ~ 0)) %>%
  dplyr::select(-HERD_TYPE)

glimpse(df1) # HERDTYPE: 1=con, 0=eco
rm(herd, production); gc()



## df2: + breed
df2 <- full_join(df1, breed, by = "DYR_ID", sort="TRUE",allow.cartesian=TRUE)
df2 <- df2 %>% 
  drop_na() %>%
  dplyr::select(-RACE) # 1=holstein, 2=jersey, 3=other dairy breeds

glimpse(df2) # BREED: 1=Holstein, 2=Jersey, 3= other
rm(df1, breed); gc()


## + calvings (create DIM + parity)
df3 <- full_join(df2, calvings, by = "DYR_ID", sort="TRUE",allow.cartesian=TRUE)

# df4: calving date will be start for each lactation
df4 <- df3 %>% 
  drop_na() %>%
  filter(CALVING_DATE < KONTROLDATO) %>%
  filter(KONTROLDATO - 350 < CALVING_DATE) %>% # setting max lact phase to 350 days
  arrange(DYR_ID, KONTROLDATO, desc(CALVING_DATE)) %>%
  distinct(DYR_ID, KONTROLDATO, .keep_all = TRUE)

glimpse(df4)
rm(df2, df3, calvings); gc()



# add DIM, we don't need DIM, but whatever
df4$DIM <- as.Date(as.character(df4$KONTROLDATO), format="%Y-%m-%d")-
  as.Date(as.character(df4$CALVING_DATE), format="%Y-%m-%d")

df4 <- df4 %>% 
  mutate(DIM = as.numeric(DIM))

df5 <- df4 # since we skipped the dryoff step and want the same number of steps as in merge1

# temp saving
rm(df4); gc()
save.image("K:/paper_vaccine/VII_data_PCR_vaccine_TEMP2.RData") 


# The last control hereby functions as the an alternative dry off date
df6 <- df5
rm(df5); gc()
branch <- df6 %>% 
  arrange(DYR_ID, PARITY, desc(KONTROLDATO)) %>%
  distinct(DYR_ID, PARITY, .keep_all = TRUE)
branch <- branch %>%
  dplyr::select(DYR_ID, PARITY, KONTROLDATO, CALVING_DATE)


# We only look at those with 3 vaccines
# 60 days before calving, 75 days after calving.
vaccination <- left_join(branch, vaccination, by = "DYR_ID", sort="TRUE",allow.cartesian=TRUE)
vaccination <- vaccination %>%
  group_by(DYR_ID, PARITY) %>%
  filter(VACCINATION_DATE + 60 > CALVING_DATE |is.na(VACCINATION_DATE)) %>%
  filter(VACCINATION_DATE - 75 < CALVING_DATE |is.na(VACCINATION_DATE)) %>%
  dplyr::select(DYR_ID, PARITY, VACC, VACCINATION_DATE)
vaccination <- vaccination %>%
  drop_na()

# Count Vaccination before in each lac_phase 
vaccination <- rename(count(vaccination, DYR_ID, PARITY), VACC = n)
table(vaccination$VACC) 

# convert vaccinations to factors:
vaccination <- vaccination %>%
  mutate(VACC = factor(VACC))


# temp saving (saving df6 + vaccination)
save.image("K:/paper_vaccine/VII_data_PCR_vaccine_TEMP3.RData") 


# df6: remove all but first KONTROLDATO. No need to keep all SCC and milk throughout the lac phase..
df7 <- df6 %>%
  dplyr::select(BES_ID, DYR_ID, CALVING_DATE, PARITY, HERDTYPE, BREED) %>%
  distinct()


## + vaccination
df8 <- left_join(df7, vaccination, by = c("DYR_ID", "PARITY"), sort="TRUE",allow.cartesian=TRUE)
#convert all NA's in vaccination to 0 (0= NOT vaccinated ever)
df8$VACC = factor(df8$VACC, levels=c(levels(df8$VACC), 0))
df8$VACC[is.na(df8$VACC)] = 0

glimpse(df8) # 
table(df8$VACC) 


##  +PCR
pcr <- left_join(branch, pcr, by = "DYR_ID", sort="TRUE",allow.cartesian=TRUE)
pcr <- pcr %>%
  drop_na()
pcr <- pcr %>%
  filter(PCR_DATE > CALVING_DATE |is.na(PCR_DATE)) %>%
  filter(PCR_DATE - 400 < CALVING_DATE |is.na(PCR_DATE)) %>%
  filter(PCR_DATE - 50 < KONTROLDATO |is.na(PCR_DATE)) %>%
  filter(PCR_DATE + 36 > KONTROLDATO |is.na(PCR_DATE)) %>%
  dplyr::select(DYR_ID, PARITY, PCR_DATE, PATHOGEN, PCR_VALUE, PCR_TEST)

df12 <- df8 # to avoid renaiming following df's
df13 <- left_join(df12, pcr, by = c("DYR_ID", "PARITY"), sort="TRUE",allow.cartesian=TRUE) %>%
  drop_na()


df_all <- df13
rm(pcr, branch, vaccination, df6, df7, df8, df12, df13, dryoff); gc()

# ONLY holstein and conventionel..
df_less <- df_all %>%
  filter(BREED == 1) %>%
  filter(HERDTYPE == 1) %>%
  dplyr::select(-PCR_DATE, -HERDTYPE, -BREED, -CALVING_DATE, -PCR_TEST)

# FIX:
# APply POS and NEG colomn with cutoff 37



# save cleaned data: --------------------------------------------

save.image("K:/paper_vaccine/VII_data_PCR_vaccine.RData")




