# The Impact of Merger on Airline Financial Performance
# This script tests Hypothesis 1 and Hypothesis 2
# Figure 1, Figure 3, Table 7, Table 9

# Install packages of Instantaneous effect from GitHub
# remotes::install_github("shuo-zhang-ucsb/twowayfeweights") 
# remotes::install_github("shuo-zhang-ucsb/did_multiplegt") 

# load required packages
pkgs = c(
  'tidyverse','patchwork','fastDummies','ggthemes','did','bacondecomp',
  'kableExtra','fixest','ggplot2','readxl','readr','tidyr',
  'dplyr','stringr','lme4','RColorBrewer','broom.mixed', 'TwoWayFEWeights', 
  'DIDmultiplegt', 'here')

kwy = lapply(pkgs, library, character.only=TRUE)

# set plot theme
theme_set(theme_clean() + theme(plot.background = element_blank(),
                                legend.background = element_blank()))
options(knitr.kable.NA = '')

# read in data
Essay3_R <- readxl::read_excel(here('Data',"Essay_Quarterly.xlsx"))

# create variables
Essay3_R = Essay3_R %>% mutate(pOntime = ONTIME/TOTAL.RECORDS,
                               OPOR = OP_PROFIT_LOSS/OP_REVENUES,
                               lTotalDelay = log(totaldelay),
                               pDelay = totaldelay/TOTAL.RECORDS,
                               lTotalComplaint = log(total),
                               pComp = total/enplaned.passengers *1000,
                               lCarrierDelay = log(AIR.CARRIER.DELAY),
                               ROA = NET_INCOME/ASSETS,
                               lBagComplaint = log(baggage),
                               lfuel = log(TDOMT_COST),
                               lEnPax = log(RevPaxEnplaned),
                               lEMPFTE = log(EMPFTE),
                               lFare = log(avg_fare),
                               lYield = log(yieldPAX),
                               lfleetutil = log(fleetutil),
                               lpcpGDP = log(percpita_gdp),
                               lseatmile = log(AvlSeatMiles)
                               )


# write.csv(Essay3_R, file = "Essay3_cor.csv")

#Essay3_R$Carriercode = as.factor(Essay3_R$Carriercode)
#Essay3_R$year = as.factor(Essay3_R$year)
#Essay3_R$quarter = as.factor(Essay3_R$quarter)
#Essay3_R$rel_occasion = Essay3_R$Occasion - Essay3_R$FirstTreat
#Essay3_R$recession = as.factor(Essay3_R$recession)
#Essay3_R$LCC = as.factor(Essay3_R$LCC)

#### SECTION 1 STACKED REGRESSION ####

# function to get treat-year specific cohorts for 20 quarters

make_dt <- function(tyr) {
  Essay3_R %>% 
    filter(Occasion <= 52) %>% # drop observation after everyone is treated
    filter(FirstTreat == tyr | FirstTreat > tyr + 20) %>% 
    filter(Occasion %>% between(tyr - 4, tyr + 20)) %>% 
    mutate(FirstTreat = if_else(FirstTreat == tyr, FirstTreat, NA_real_),
           rel_occasion = Occasion - FirstTreat) %>% 
    select(Carriercode, Occasion, FirstTreat, Treated, 
           rel_occasion, 
           OPOR,pOntime,
           LFP, 
           lfleetutil,
           lYield, lEnPax, lEMPFTE,
           lfuel,lpcpGDP,lFare, 
           percent_chg_fare, 
           percent_gdp, 
           LCC, recession, 
           lTotalDelay,
           lseatmile,
           lCarrierDelay,
           year, quarter) %>% 
    mutate(dt = as.character(tyr))
}

# treats
treats <- Essay3_R %>% 
  filter(FirstTreat < max(FirstTreat)) %>% 
  pull(FirstTreat) %>% 
  unique() %>% 
  sort()

# stack the data 
stacked_data <- map_dfr(treats, make_dt) %>% 
  dummy_cols(select_columns = "rel_occasion", 
             remove_selected_columns = FALSE,
             ignore_na = TRUE) %>% 
  mutate(across(starts_with("rel_occasion_"), ~replace_na(., 0))) %>% 
  mutate(cluster = paste0(Carriercode, "_", dt))

# make formula
indicatorStacked <- c(paste0("`", "rel_occasion_", c(-4:-1, 1:20), "`"))

# estimate the model and plot
# without controls
stack1 <- feols(OPOR ~ .[indicatorStacked] | Occasion^dt + Carriercode^dt,
                cluster = "Carriercode", data = stacked_data)
summary(stack1)


# with controls
stack1_C <- feols(OPOR ~ .[indicatorStacked] + 
                  lYield + LFP + lfleetutil +
                  lEnPax + lEMPFTE + 
                  lCarrierDelay + lseatmile
                  |Occasion^dt + Carriercode^dt,
                  cluster = "Carriercode",
                  data = stacked_data)

summary(stack1_C)
# unpack
coef_all = broom::tidy(stack1_C, conf.int = TRUE)
write.csv(coef_all, file= "coef_all.csv")
write.csv(stacked_data, file = "stacked_data_all.csv")

ES_stack1_C

ES_stack1_C <- broom::tidy(stack1_C, conf.int = TRUE)[1:24,] %>%
  # add in the relative time variable
  mutate(t = c(-4:-1, 1:20)) %>% 
  select(t, estimate, conf.low, conf.high) %>% 
  bind_rows(tibble(t = 0, estimate = 0, conf.low = 0, conf.high = 0)) %>% 
  mutate(group = as.factor(case_when(
    t < 0 ~ 1,
    t >= 0 ~ 2
  ))) %>% 
  # plot
  ggplot(aes(x = t, y = estimate)) + 
  geom_point(aes(fill = factor(group)), shape = 21) + 
  scale_fill_manual(values = c("#993441", "#0029a5"))  + 
  geom_line() + 
  ggtitle("The Dynamic Effect of Airline Mergers on OPOR") + 
  geom_errorbar(aes(ymin = conf.low + 0.03, ymax = conf.high - 0.03, color= factor(group)), 
                linetype = "longdash", show.legend = FALSE) + 
  scale_color_manual(values = c("#993441", "#0029a5")) + 
  geom_hline(yintercept = 0,  linetype = "longdash", color = "gray") + 
  geom_vline(xintercept = 0,  linetype = "longdash", color = "gray") + 
  labs(y = "Change in OPOR", x = "Quarters Relative to Merger")+
       #subtitle = "Stagged Regression"#) + 
  scale_x_continuous(breaks = seq(-4, 20, by = 1)) + 
  scale_y_continuous(breaks = seq(-2, 2, by = 0.05)) + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 90),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), 
        legend.position = "none")

ggsave(ES_stack1_C, filename = here::here("Figs_Tables", "All Carriers_ES.png"), 
       dpi = 500, width = 6, height = 4)


#### SECTION 2 INSTANTANEOUS EFFECT ####

# create first treat identifier for Instantaneous effect
Essay3_R$FirstTreat_IE = ifelse(Essay3_R$FirstTreat == Essay3_R$Occasion, 1, 0)

Y = "OPOR"
G = "Carriercode"
T = "Occasion"
D = "FirstTreat_IE" # This variable is used to estimate using deChaiseMartin (change of treatment status from t-1 to t)
D_TWFE = "Treated" # This variable is used to demonstrate TWFE pitfalls. Refer to Lang and Bliese 2016 Coding

controls = c("lYield", "LFP","lfleetutil", "lEnPax","lEMPFTE", "lfuel", 
             "lCarrierDelay")

# get rid of NA and Inf for model
Essay3_R$lYield[is.na(Essay3_R$lYield)] = 0
Essay3_R$LFP[is.na(Essay3_R$LFP)] = 0
Essay3_R$lfleetutil[is.na(Essay3_R$lfleetutil)] = 0
Essay3_R$lEnPax[is.na(Essay3_R$lEnPax)] = 0
Essay3_R$lEMPFTE[is.na(Essay3_R$lEMPFTE)] = 0
Essay3_R$lfuel[is.na(Essay3_R$lfuel)] = 0
Essay3_R$lCarrierDelay[is.na(Essay3_R$lCarrierDelay)] = 0


Essay3_R$lYield[is.infinite(Essay3_R$lYield)] = 0
Essay3_R$LFP[is.infinite(Essay3_R$LFP)] = 0
Essay3_R$lfleetutil[is.infinite(Essay3_R$lfleetutil)] = 0
Essay3_R$lEnPax[is.infinite(Essay3_R$lEnPax)] = 0
Essay3_R$lEMPFTE[is.infinite(Essay3_R$lEMPFTE)] = 0
Essay3_R$lfuel[is.infinite(Essay3_R$lfuel)] = 0
Essay3_R$lCarrierDelay[is.infinite(Essay3_R$lCarrierDelay)] = 0


# decompose weights
twowayfeweights(Essay3_R, Y, G, T, D_TWFE, cmd_type = "feTR")
twowayfeweights(Essay3_R, Y, G, T, D_TWFE, cmd_type = "feTR", controls = controls)

# estimate instantaneous effect
set.seed(7735) # set seed for exact model outputs 
# model without control for baselining. Not reported in manuscript
model_IE_nc = did_multiplegt(Essay3_R, Y, G, T, D, placebo = 4, 
                             brep = 10, 
                             parallel = TRUE,
                             cluster = "Carriercode")

model_IE = did_multiplegt(Essay3_R, Y, G, T, D, controls = controls, 
                          dynamic = 0, 
                          placebo = 4,
                          brep = 20, 
                          parallel = TRUE, 
                          cluster = 'Carriercode') # brep (bootstrap) to get CI



#### Some prep work to plot Instantaneous Effect 

# unlist and combine model outputs for plotting
total = list(model_IE_nc, model_IE)
test = as.data.frame(do.call(cbind, total))
test$Names = rownames(test)

# extract estimates and se for plot
estimates =rbind(test[grepl('^placebo_', test$Names), ], test[grepl('^effect', test$Names), ])
se = rbind(test[grepl('^se_placebo_', test$Names), ], test[grepl('^se_effect', test$Names), ])

estimates$Names = NULL
se$Names = NULL

# convert coeffients to numeric
for (i in 1:ncol(estimates)){
  estimates[[i]] <- as.numeric(estimates[[i]])}

for (j in 1:ncol(se)){
  se[[j]] <- as.numeric(se[[j]])}

# Instantaneous Effect Plot (This is Figure 1 in the manuscript)
df <- data.frame(x =-4:0,
                 F =estimates$V2,
                 L =estimates$V2 - se$V2*1.96, # 5% confidence level
                 U =estimates$V2 + se$V2*1.96)
# IE_plot

IE_plot =  df %>% 
  mutate(group = as.factor(case_when(
    x < 0~1,
    x >= 0~2 ))) %>% 
  # plot
  ggplot(aes(x = x, y = F)) + 
  geom_point(aes(fill= factor(group)), shape = 21) + geom_line() + 
  scale_fill_manual(values = c("#993441", "#0029a5")) + 
  ggtitle("Instantaneous Effect of Airline Mergers on OPOR") + 
  geom_errorbar(aes(ymin = L, ymax = U, 
                    color = factor(group)), 
                linetype = "longdash", show.legend = FALSE) + 
  scale_color_manual(values = c("#993441", "#0029a5"))+
  geom_hline(yintercept = 0,  linetype = "longdash", color = "gray") + 
  geom_vline(xintercept = 0,  linetype = "longdash", color = "gray") + 
  labs(y = "Chang in OPOR", x = "Quarters before Merger",
       subtitle = "de Chaisemartin and D’Haultfoeuille (2020) Estimator") + 
  scale_x_continuous(breaks = seq(-4, 0, by = 1)) + 
  scale_y_continuous(breaks = seq(-0.5, 0.5, by = 0.1)) + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 90),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")

IE_plot

ggsave(IE_plot, filename = here::here("Figs_Tables", "IE_Plot_All.png"), 
       dpi = 500, width = 6, height = 4)


# Another tidy function to unpack models
tidy_IE = function(x, level = 0.95) {
  ests = x[grepl("^placebo_|^effect|^dynamic_", names(x))]
  ret = data.frame(
    term      = names(ests),
    estimate  = as.numeric(ests),
    std.error = as.numeric(x[grepl("^se_placebo|^se_effect|^se_dynamic", names(x))]),
    N         = as.numeric(x[grepl("^N_placebo|^N_effect|^N_dynamic", names(x))])
  ) |>
    # For CIs we'll assume standard normal distribution
    within({
      conf.low  = estimate - std.error*(qnorm(1-(1-level)/2))
      conf.high = estimate + std.error*(qnorm(1-(1-level)/2))
    })
  return(ret)
}

coef_IE = tidy_IE(model_IE)
write.csv(coef_IE, file = "coef_IE_All.csv")


