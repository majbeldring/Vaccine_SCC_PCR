

# Maj Beldring Henningsen, majbh@sund.ku.dk

# Parameters handling from nlme and nls
# fitting SCC curves and extract the values

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

Sys.setenv(LANG = "en")



# Loading data and preparing data -------------------------------------

load("K:/paper_vaccine/V_modeloutput_vaccine_NLME.RData")





# 1. Summary statistics of input variables -----------------------
# data: the data fitted to the models (df2_vacc etc)

# repeat for all parity groups, vacc and no vacc
df4_no %>%
  summarize(min = exp(min(logSCC)),
            q1 = exp(quantile(logSCC, 0.25)),
            median = exp(median(logSCC)),
            mean = exp(mean(logSCC)),
            q3 = exp(quantile(logSCC, 0.75)),
            max = exp(max(logSCC)))




# 2. curves in colours --------------------
# data: pos4_mean

# repeat for all 3 parities, vacc and no 
vacc2_mean <- nlme_out_vacc2 %>% 
  dplyr::summarise(across(everything(), mean))

# Colour: Repeat for all parities. 
curve2 <- list(vacc2_mean = vacc2_mean, no2_mean = no2_mean) %>% 
  enframe() %>% 
  unnest(value) %>% 
  crossing(DIM = seq_len(305)) %>% 
  #' calculate the proper `logSCC`
  mutate(logSCC = pmap_dbl(select(., DIM, a, b, k, d), f_wilmink)) %>% {
    ggplot(., aes(DIM, logSCC, group = name, color=name)) + 
      geom_line(size = rel(1.5)) +
      ylim(3.5, 6.0) +
      ggpubr::theme_classic2() + 
      labs(title = "Parity 2", color= "VACC") + 
      theme(axis.text = element_text(size = 14),
            axis.title = element_text(size = 14),
            plot.title = element_text(hjust = 0.5),
            #legend.position="none"
            #,legend.position = "top"
      ) +
      scale_color_discrete(breaks=c("vacc2_mean", "no2_mean"),
                           labels=c("YES", "NO")) +
      #geom_hline(yintercept=5.3, linetype="dashed") + #200.000 threshold
      NULL
  } 

ggarrange(curve2, curve3, curve4, 
          ncol=1, nrow=3, 
          common.legend = TRUE, legend="right")

# save in paper folder
ggsave("C:/Users/zjt234/PhD/Paper_Vaccines_wilmink/final_figures/curves_all.tiff", width = 40, height = 40, units = "cm", dpi=300)







# 3. ecdf plot --------------------------------------
# data: all_out_nlme
# save only VACC 0 or VACC 3

all_out_nlme_no4 <- nlme_out_no4 %>%
  add_column(PARITY = "4", VACC = "NO")

all_out_nlme4 <- rbind(all_out_nlme_vacc4, all_out_nlme_no4) 
rm(all_out_nlme_vacc4, all_out_nlme_no4); gc()

all_out_nlme4$PARITY <- factor(all_out_nlme4$PARITY)
all_out_nlme4$VACC <- factor(all_out_nlme4$VACC)

all_out_nlme4 <- all_out_nlme4 %>% rownames_to_column("BES_ID") # nyt BES_ID

# here for Parity 4
ecdf_Parity4 <-
  ggarrange( 
    all_out_nlme4 %>%
      select(a, PARITY, VACC) %>%
      group_by(PARITY, VACC) %>%
      ggplot(aes(a, colour = VACC)) +
      stat_ecdf(size = rel(1.5)) +
      ggpubr::theme_classic2() + 
      theme(axis.text = element_text(size = 14),
            axis.title = element_text(size = 14)),
    all_out_nlme4 %>%
      select(b, PARITY, VACC) %>%
      group_by(PARITY, VACC) %>%
      ggplot(aes(b, colour = VACC)) +
      stat_ecdf(size = rel(1.5))+
      ggpubr::theme_classic2() + 
      theme(axis.text = element_text(size = 14),
            axis.title = element_text(size = 14)),
    all_out_nlme4 %>%
      select(k, PARITY, VACC) %>%
      group_by(PARITY, VACC) %>%
      ggplot(aes(k, colour = VACC)) +
      stat_ecdf(size = rel(1.5))+
      ggpubr::theme_classic2() + 
      theme(axis.text = element_text(size = 14),
            axis.title = element_text(size = 14)),
    all_out_nlme4 %>%
      select(d, PARITY, VACC) %>%
      group_by(PARITY, VACC) %>%
      ggplot(aes(d, colour = VACC)) +
      stat_ecdf(size = rel(1.5))+
      ggpubr::theme_classic2() + 
      theme(axis.text = element_text(size = 14),
            axis.title = element_text(size = 14)), 
    ncol=2, nrow=2, common.legend = TRUE, legend="right")

ecdf_Parity4
ggsave("C:/Users/zjt234/PhD/Paper_Vaccines_wilmink/final_figures/ecdf_Parity4.tiff", width = 40, height = 40, units = "cm", dpi=300)












# 4. mean, median, sd, se, CI of wilmink parameters  -------------------

# output stats - for this NLME out must be loaded 
coef(nlme_vacc4) %>% 
  as_tibble() %>% 
  #summarise(across(everything(), mean))
  summarise(across(everything(), median))
#summarise(across(everything(), 
#                 list(
#                   "percentile" = ~ quantile(.x, probs = seq(0.05, .9, 0.05)))))

# following doesn't require all NLME output
sample.mean <- mean(nlme_out_vacc4$d)
sample.median <- median(nlme_out_vacc4$d)

sample.n <- length(nlme_out_vacc4$d)
sample.sd <- sd(nlme_out_vacc4$d)
sample.se <- sample.sd/sqrt(sample.n)

alpha = 0.05
degrees.freedom = sample.n - 1
t.score = qt(p=alpha/2, df=degrees.freedom,lower.tail=F)

margin.error <- t.score * sample.se
lower.bound <- sample.mean - margin.error
upper.bound <- sample.mean + margin.error

print(sample.mean) # mean
print(sample.median) # median
#print(t.score) # t.score
print(sample.sd) # SD
print(sample.se) # SE
print(c(lower.bound,upper.bound)) #CI


# this method: first change PCR to VACC, and keep only VACC == 0 ^ 3 (|| ?)
# use df: all_out_nlme, repeat for all parameters
all_out_nlme4 %>%
  summarize(median = median(a),
            mean = mean(a),
            sd = sd(a))






# 5. logSCC values at MIN, day 100 and day 150 ---------------------

# redo for all groups
# re-transform logSCC to SCC
# calculate manuel dΔlogSCC from DIM 100-150 (/50)
min_vacc4 <-
  nlme_out_vacc4 %>% 
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








# 6. Q-Q plot ----------------------------------------------------

# data: raw nlme output (need to reload _NLME)
load("K:/paper_vaccine/V_modeloutput_vaccine_NLME.RData")

# QQ plot:
# qq1, qq2, qq3: vacc2, vacc3, vacc4 #00BFC4 
# qq4, qq5, qq6: no2, no3, no4 #F8766D
qq6 <- ggqqplot(residuals(nlme_no4), shape=1, 
                xlab = "Theoretical Quantiles", 
                ylab = "Sample Quantiles", title = "Parity > 3, non-vaccinated", 
                font.x = c(14), font.y = c(14), 
                color = "#F8766D")
qq6$layers[[2]]$aes_params$colour <- "black" 
qq6

qq_all <- ggarrange(qq1, qq4, qq2, qq5, qq3, qq6, 
                    ncol=2, nrow=3, 
                    common.legend = TRUE, legend="right")
qq_all

ggsave("C:/Users/zjt234/PhD/Paper_Vaccines_wilmink/final_figures/new_qq_all.tiff", width = 40, height = 40, units = "cm", dpi=300)




# 7. residuals plot ---------------------------------

library(broom.mixed)
broom::augment(nlme_no4, data = df4_no) -> 
  aug_nlme_no4

# Residuals plot, all data.
resid6 <- ggplot(aug_nlme_no4, aes(.fitted, .resid)) + 
  ggpubr::theme_classic2() + 
  #geom_point(size=1) +
  # geom_point(alpha = 1/10) +
  geom_point(shape=1, size=1, fill='white') +
  geom_hline(yintercept = 0, color = "red", size=1) +
  # geom_smooth(se=FALSE, color = "red") +
  labs(x = "Fitted", y = "Residuals", title = "Parity > 3, non-vaccinated") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14)
        #,legend.position = "top"
  ) +
  NULL

resid_all <- ggarrange(resid1, resid4, resid2, resid5, resid3, resid6, 
                       ncol=2, nrow=3, 
                       common.legend = TRUE, legend="right")

resid_all
ggsave("C:/Users/zjt234/PhD/Paper_Vaccines_wilmink/final_figures/resid_all.tiff", width = 40, height = 40, units = "cm", dpi=300)











