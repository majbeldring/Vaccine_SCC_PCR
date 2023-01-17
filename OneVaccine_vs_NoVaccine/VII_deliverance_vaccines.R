

# Maj Beldring Henningsen, majbh@sund.ku.dk

# plot and table values for paper vaccine

# note: nlme_wilmink4.RData should be remaked without 9999 values

# missing: 2x2 tabel...


# Packages and settings -------------------------------------

library(tidyverse)
library(ggpubr) # p values in boxplot
library(gridExtra) # gridarrange
ggplot2::theme_set(ggplot2::theme_bw())  # globally sets ggplot2 theme to theme_bw
library(GGally) # for ggpairs
options(stringsAsFactors = FALSE) # prevent factorizing caracters


# Loading data and preparing data -------------------------------------

load("K:/paper_vaccine/VI_parameters_vaccine.RData")




# SCC distribution  -----------------------

summary(df_model)

# Plot SCC distribution 
ggplot(df_model, aes(x=SCC)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 colour="black", fill="white") +
  scale_x_continuous(trans="log10") +
  geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot

df_all %>%
  filter(SCC < 50) %>%
ggplot(aes(x=SCC)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot



# Summary statistics of input variables -----------------------

# repeat for all parity groups
df2_neg %>%
  summarize(min = exp(min(logSCC)),
            q1 = exp(quantile(logSCC, 0.25)),
            median = exp(median(logSCC)),
            mean = exp(mean(logSCC)),
            q3 = exp(quantile(logSCC, 0.75)),
            max = exp(max(logSCC)))




# Table parameters mean and median -----------------------

# use df: all_out_nlme, repeat for all parameters
all_out_nlme %>%
  group_by(PARITY, PCR) %>%
  summarize(median = median(a),
            mean = mean(a))




# PLOTTING 2-IN-1 -------------------------------------------

# dashed and solid: Repeat for Parity 2,3,4
curve2 <- list(pos2_mean = pos2_mean, neg2_mean = neg2_mean) %>% 
  enframe() %>% 
  unnest(value) %>% 
  crossing(DIM = seq_len(305)) %>% 
  #' calculate the proper `logSCC`
  mutate(logSCC = pmap_dbl(select(., DIM, a, b, k, d), f_wilmink)) %>% {
    ggplot(., aes(DIM, logSCC, group = name)) + 
      geom_line(aes(linetype = name), size = rel(1.0)) + # Dotted for NEG
      ylim(3.5, 6.0) +
      ggpubr::theme_classic2() +
      scale_linetype_manual(values = c(
        "pos2_mean" = "solid", "neg2_mean" = "dotted"
      )) + 
      labs(linetype = 'PCR') +
      theme(axis.text = element_text(size = 10),
            axis.title = element_text(size = 10)
            #,legend.position = "top"
            ) +
      NULL
  } 

curves_all <- ggarrange(curve2, curve3, curve4, 
                      ncol=1, nrow=3, 
                      common.legend = TRUE, legend="right")



# PLOTTING 6-IN-1 -------------------------------------------

# in colour
curve2 <- list(pos2_mean = pos2_mean, neg2_mean = neg2_mean) %>% 
  enframe() %>% 
  unnest(value) %>% 
  crossing(DIM = seq_len(305)) %>% 
  #' calculate the proper `logSCC`
  mutate(logSCC = pmap_dbl(select(., DIM, a, b, k, d), f_wilmink)) %>% {
    ggplot(., aes(DIM, logSCC, group = name)) + 
      geom_line(aes(linetype = name), size = rel(1.0)) + # Dotted for NEG
      ylim(3.5, 6.0) +
      ggpubr::theme_classic2() +
      scale_linetype_manual(values = c(
        "pos2_mean" = "solid", "neg2_mean" = "dotted"
      )) + 
      labs(linetype = 'PCR') +
      theme(axis.text = element_text(size = 10),
            axis.title = element_text(size = 10)
            #,legend.position = "top"
      ) +
      NULL
  } 




# intercorrelation parameters --------------------------------------

nlme_out_neg2 %>% 
  ggpairs()
ggsave("gg_neg2.tiff", width = 40, height = 20, units = "cm", dpi=300)




# Retrieve logSCC values at MIN, day 100 and day 150 -----------------

# redo for all groups
# re-transform logSCC to SCC
# calculate manuel dΔlogSCC from DIM 100-150 (/50)

min_pos3 <-
nlme_out_pos3 %>% 
  summarise(across(everything(), mean)) %>% 
  #' join parameters with x-axis (`DIM`)
  crossing(DIM = seq_len(305)) %>% 
  #' calculate the proper `logSCC`
  mutate(logSCC = pmap_dbl(select(., DIM, a,b,k,d), f_wilmink)) %>% 
  
  # identity{} : min SCC, DIM at 100, DIM at 150
  identity() %>% {
    bind_rows(
      slice_min(., logSCC, n = 1),
      filter(., DIM == 100),
      filter(., DIM == 150)
    )
  } %>%
  mutate(SCC = exp(logSCC)) 

# minSCC, minlogSCC and DIM at minimum retrieved from table
# delta logSCC calculated using th 100 and 150 DIM logSCC value




# ecdf plot --------------------------------------

# here for Parity 2
ecdf_Parity2 <-
  ggarrange( 
    all_out_nlme %>%
      select(a, PARITY, PCR) %>%
      group_by(PARITY, PCR) %>%
      filter(PARITY == 2) %>%
      ggplot(aes(a, colour = PCR)) +
      stat_ecdf(),
    all_out_nlme %>%
      select(b, PARITY, PCR) %>%
      group_by(PARITY, PCR) %>%
      filter(PARITY == 2) %>%
      ggplot(aes(b, colour = PCR)) +
      stat_ecdf(),
    all_out_nlme %>%
      select(k, PARITY, PCR) %>%
      group_by(PARITY, PCR) %>%
      filter(PARITY == 2) %>%
      ggplot(aes(k, colour = PCR)) +
      stat_ecdf(),
    all_out_nlme %>%
      select(d, PARITY, PCR) %>%
      group_by(PARITY, PCR) %>%
      filter(PARITY == 2) %>%
      ggplot(aes(d, colour = PCR)) +
      stat_ecdf(), 
    ncol=2, nrow=2, common.legend = TRUE, legend="right")




# Confidence interval ---------------------------------------

# See script VIII



# histogram parameters --------------------------------------

# repeat for all Parity groups
h_d4 <- all_out_nlme %>%
  filter(PARITY == 4) %>%
  ggplot(aes(x=d, fill=PCR, color=PCR)) +
  geom_histogram(position="identity", alpha=0.5)+
  labs(x="Parity 4, parameter d")

hist_all <- ggarrange(h_a2, h_b2, h_k2, h_d2, 
                      h_a3, h_b3, h_k3, h_d3, 
                      h_a4, h_b4, h_k4, h_d4, 
                      ncol=4, nrow=3, common.legend = TRUE, legend="right")









