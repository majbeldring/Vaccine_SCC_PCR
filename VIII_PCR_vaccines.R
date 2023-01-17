

# Maj Beldring Henningsen, majbh@sund.ku.dk

# PCR test vs Vaccinations
# Analyse data

# To Do:
## 2x2 table vaccinated vs non-vaccinated
## PCR_value distribution
## e.coli vs staph aureus
## intercorrelation, pathogen, vaccination status vs PCR_value ?

### FIX NOTE::: CREATE DATA NO! Choose only herds, that does not vaccinate at all
# so we only compare with abosolutely NO Vaccinations (and not animals previous vaccinated, but not in this parity)


# Packages and settings -------------------------------------

library(tidyverse)
library(ggpubr) # p values in boxplot
library(gridExtra) # gridarrange
ggplot2::theme_set(ggplot2::theme_bw())  # globally sets ggplot2 theme to theme_bw
library(GGally) # for ggpairs
options(stringsAsFactors = FALSE) # prevent factorizing caracters


# Loading data -----------------------------

load("K:/paper_vaccine/VII_data_PCR_vaccine.RData")
# df_less only holstein and conventionel!


# and preparing data ---------------------------------

# adding PCR result colomn
df_even_less <- df_less %>% 
  mutate(FULL_VACC = case_when(VACC == 3 ~ 1, VACC == 0 ~ 0)) %>%
  mutate(PCR_RESULT = case_when(PCR_VALUE < 37 ~ 1, PCR_VALUE >= 37 ~ 0)) %>%
  select(-VACC, -BES_ID)



# vaccination count -----------------------------------

# from: 2010-01-01 to 2020-03-11
dplyr::n_distinct(vaccination$DYR_ID)  # 33.270 unique animals
nrow(vaccination)                      # 107.383 vaccinations performed


# load("K:/paper_vaccine/VII_data_PCR_vaccine_TEMP1.RData")
# rm(calvings, herd, pcr, production, breed); gc()
# to find total numbers of :
## vaccinations
## unique animals



# barplot --------------------------------------------- 

# split data in pathogens...

# barplot:
# Y-axis: count positive tests
# x-axis, Vacc vs non-vacc. Parity and Pathogen dependend


df_even_less %>%
  filter(PATHOGEN == "s.aureus") %>%
  count(FULL_VACC, PCR_RESULT) %>%
  group_by(FULL_VACC) %>%
  mutate(n = n/sum(n) * 100) %>%
  ggplot() + aes(FULL_VACC, n, fill = PCR_RESULT, label = paste0(round(n, 2), "%")) + 
  geom_col() +
  geom_text(position=position_stack(0.5))


ggplot(df_even_less, aes(fill=FULL_VACC, y=PCR_VALUE, x=PATHOGEN)) + 
  geom_bar(position="fill", stat="identity")



# 1 is fully vaccinated, 0 is non-vaccinated
plot_coli <- df_even_less %>%
  filter(PATHOGEN == "e.coli") %>%
  filter(PARITY == 3) %>%
  ggplot(aes(PCR_RESULT, group = FULL_VACC)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  scale_y_continuous(labels=scales::percent)



ggplot(data, aes(fill=condition, y=value, x=specie)) + 
  geom_bar(position="fill", stat="identity")



ggplot(df_all, aes(x=VACC, y=PCR_VALUE, fill=PATHOGEN)) +
  geom_boxplot()


ggplot(data = df_less, aes(x = VACC, fill = PATHOGEN)) + 
  geom_bar(stat = "identity", position = "dodge", colour = "black") + 
  scale_fill_brewer(palette = "Pastel1") +
  labs(x = "Vaccinations", y = "Counts \n", 
       title = "PCR positive vs Vaccinations",
       fill = "Pathogen") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(face="bold", colour="blue", size = 12),
        axis.title.y = element_text(face="bold", colour="blue", size = 12),
        legend.position = "bottom")




# intercorrelation parameters with PCR-------------------
# Vaccination vs PCR Ct value for E. coli and Staph A

ggpairs(nlme_out_neg2,
        upper = list(continuous = wrap("cor", size = 8)))


df_all %>% 
  ggpairs()
ggsave("gg_neg2.tiff", width = 40, height = 20, units = "cm", dpi=300)


# saving ---------------

# for saving plot:
# ggsave("C:/Users/zjt234/PhD/PaperI_PCR_Wilmink/final_figures/gg_neg2.tiff", width = 40, height = 20, units = "cm", dpi=300)






