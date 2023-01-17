

# Maj Beldring Henningsen, majbh@sund.ku.dk

# confidence interval


# Packages and settings ----------------------------------------

library(tidyverse)
library(ggpubr) # p values in boxplot
library(gridExtra) # gridarrange
ggplot2::theme_set(ggplot2::theme_bw())  # globally sets ggplot2 theme to theme_bw
library(GGally) # for ggpairs
library(nls.multstart)
library(nlme) # for nlslist

Sys.setlocale("LC_ALL","English") # date formatting
memory.size()            # Checking your memory size
memory.limit()           # Checking the set limit
memory.limit(size=56000) # suggest for 64 bit
options(stringsAsFactors = FALSE) # prevent factorizing caracters


# Loading data and preparing data -------------------------------------

#load("K:/paper_vaccine/VI_parameters_vaccine.RData")





# diagnostic plot ----------------------------------------------------

# QQ plot:
qqnorm(nlme_neg2, ~ ranef(., standard = TRUE))
qqnorm(nlme_neg2, ~ resid(., type = "p")) # shows each parameter

#Q-Q plots take your sample data, sort it in ascending order, 
#and then plot them versus quantiles calculated from a theoretical distribution. 
#The number of quantiles is selected to match the size of your sample data

plot(nlme_neg1) # not useful. Data is 




# output ---------------------------------------------------------------

# output stats
coef(nlme_neg1) %>% 
  as_tibble() %>% 
  #summarise(across(everything(), mean))
  summarise(across(everything(), median))
  #summarise(across(everything(), 
  #                 list(
  #                   "percentile" = ~ quantile(.x, probs = seq(0.05, .9, 0.05)))))

# neg1:
# a: 3.29 - 3.81, 
# b: 0.00103 - 0.00254
# c: -2.68 - -2.42
# d: 1.89 - 2.41


sample.mean <- mean(nlme_out_neg1$a)
print(sample.mean)

sample.n <- length(nlme_out_neg1$a)
sample.sd <- sd(nlme_out_neg1$a)
sample.se <- sample.sd/sqrt(sample.n)
print(sample.se)

alpha = 0.05
degrees.freedom = sample.n - 1
t.score = qt(p=alpha/2, df=degrees.freedom,lower.tail=F)
print(t.score)

margin.error <- t.score * sample.se
lower.bound <- sample.mean - margin.error
upper.bound <- sample.mean + margin.error
print(c(lower.bound,upper.bound))

# CI neg1: 3.554937 3.590440




# percentile curves based on wilmink:----------------------------------------------------

# not working.... unused arguments (pctile, everything())
pos3_pct <- nlme_out_pos3 %>% 
  as_tibble() %>% 
  summarise(across(everything(), 
                   list(
                     "percentile" = ~ quantile(.x, probs = seq(0.1, .9, 0.1))))) %>% 
  dplyr::rename_with(~ str_remove(.x, "_percentile"), ends_with("_percentile")) %>% 
  mutate(pctile = names(a)) %>% select(pctile, everything())


curve_pct_pos4 <- pos4_pct %>% 
  #' join parameters with x-axis (`DIM`)
  crossing(DIM = seq_len(305)) %>% 
  
  #' calculate the proper `logSCC`
  mutate(logSCC = pmap_dbl(select(., DIM, a,b,k,d), f_wilmink)) %>% {
    ggplot(., aes(DIM, logSCC)) + 
      # aes(group = BES_ID) +
      aes(group = pctile) +
      aes(color = pctile) +
      geom_line() +
      
      #labs(caption = "PCR negative, parity 2") +
      ggtitle("SCC curve, pctile of parametres Wilmink: PCR POS parity 4") +
      #ylim(3.0, 11.0) +
      ggpubr::theme_classic2() +
      NULL
  } %>% 
  plotly::ggplotly()


# CI and SD manual ----------------------------------------

# data: all_out_nlme
# confint function on the nlme out can not be used, as it is not implemented yet
# library(MASS)
# confint(nlme_neg1, level = 0.95) # Error: not (yet) implemented.
# confint.default(nlme_neg1) # runs put gives all the parameters

all_out_nlme <-
  dplyr::select(-BES_ID) %>%
  filter(PARITY == 2) %>%
  filter(PCR == NEG) %>%
  filter

## from example(glm)
counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3, 1, 9); treatment <- gl(3, 3)
glm.D93 <- glm(counts ~ outcome + treatment, family = poisson())
library(MASS)
confint(nlme_neg1, level = 0.95) # needs MASS to be installed
confint.default(glm.D93)  # based on asymptotic normality


# SD of the mean --------------------------------------------


data("mtcars")

sample.mean <- mean(mtcars$mpg)
print(sample.mean)

sample.n <- length(mtcars$mpg)
sample.sd <- sd(mtcars$mpg)
sample.se <- sample.sd/sqrt(sample.n)
print(sample.se)

alpha = 0.05
degrees.freedom = sample.n - 1
t.score = qt(p=alpha/2, df=degrees.freedom,lower.tail=F)
print(t.score)

margin.error <- t.score * sample.se
lower.bound <- sample.mean - margin.error
upper.bound <- sample.mean + margin.error
print(c(lower.bound,upper.bound))




