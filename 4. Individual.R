# This script tests individual merger effect at carrier level
# Figure 7

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
                               lseatmile = log(AvlSeatMiles))


# Excluding 7 acquirers
# without frontier
Essay3_R = Essay3_R[!(Essay3_R$airline %in% 
                             c ("SOUTHWEST",  "ALASKA" ,  
                                "AMERICAN","DELTA",
                                "EXPRESSJET","US",
                                 "UNITED")),]

# function to get treat-year specific cohorts
make_dt <- function(tyr) {
  Essay3_R %>% 
    #filter(Occasion <= 52) %>% 
    filter(FirstTreat == tyr | FirstTreat > tyr + 20) %>% 
    filter(Occasion %>% between(tyr - 4, tyr + 20)) %>% 
    mutate(FirstTreat = if_else(FirstTreat == tyr, FirstTreat, NA_real_),
           rel_occasion = Occasion - FirstTreat) %>% 
    select(Carriercode, Occasion, FirstTreat, rel_occasion, 
           OPOR,pOntime,
           LFP, 
           lfleetutil,
           lYield, lEnPax, lEMPFTE,
           lfuel,lpcpGDP,lFare, 
           percent_chg_fare, 
           percent_gdp, 
           LCC, recession, 
           lTotalDelay,
           lCarrierDelay,
           lseatmile,
           year, quarter) %>% 
    mutate(dt = as.character(tyr))
}

# treats
treats <- Essay3_R %>% 
  filter(FirstTreat <= max(FirstTreat)) %>% 
  pull(FirstTreat) %>% 
  unique() %>% 
  sort()


# stack the datasets
stacked_data <- map_dfr(treats, make_dt) %>% 
  dummy_cols(select_columns = "rel_occasion", remove_selected_columns = FALSE,
             ignore_na = TRUE) %>% 
  mutate(across(starts_with("rel_occasion_"), ~replace_na(., 0))) %>% 
  mutate(cluster = paste0(Carriercode, "_", dt))

# make formula
indicatorStacked <- c(paste0("rel_occasion_", c(-4:-1, 1:20)))

# estimate the model and plot
# with controls
stack_model <- feols(OPOR ~ .[indicatorStacked] + 
                       lYield + LFP + lfleetutil +
                       lEnPax + lEMPFTE + lfuel +
                       lCarrierDelay 
                     | Carriercode^dt,
                     cluster = "cluster",
                     data = stacked_data)
coef_ES_United= broom::tidy(stack_model, conf.int = TRUE)



# coef_ES_Express= broom::tidy(stack_model, conf.int = TRUE)
# coef_ES_Delta= broom::tidy(stack_model, conf.int = TRUE)
# coef_ES_American= broom::tidy(stack_model, conf.int = TRUE)
# coef_ES_Alaska= broom::tidy(stack_model, conf.int = TRUE)
# coef_ES_Frontier= broom::tidy(stack_model, conf.int = TRUE)
# coef_ES_US= broom::tidy(stack_model, conf.int = TRUE)
# coef_ES_Southwest= broom::tidy(stack_model, conf.int = TRUE)


coef_ES_Alaska$carrier = "Alaska"
coef_ES_American$carrier = "American"
coef_ES_Delta$carrier = "Delta"
coef_ES_Express$carrier = "ExpressJet"
coef_ES_Frontier$carrier = "Frontier"
coef_ES_Southwest$carrier = "Southwest"
coef_ES_United$carrier = "United"
coef_ES_US$carrier = "US"

# combine data

coef_all = rbind(coef_ES_Alaska, coef_ES_American, coef_ES_Delta,
                 coef_ES_Express, coef_ES_Frontier, coef_ES_Southwest, 
                 coef_ES_United, coef_ES_US)

write.csv(coef_all, file = "coef_8_carrier.csv")

#### Read in clean file ####
library(readxl)
library(tidyr)

# read in and reshape
df_8 = read_excel("coef_8_carrier.xlsx")
df_long = pivot_longer(df_8,-c(Time), values_to = "OPOR", names_to = "Carrier")

df_long = read_excel("coef_8_carrier.xlsx", sheet = "Long")

# plot
ggplot(df_long, aes(x=Time, y=estimate, group=Carrier)) +
  geom_line(aes(color=Carrier))+
  geom_point(aes(color=Carrier))
  #geom_errorbar(aes(ymin = conflow + 0.5, ymax = confhigh -0.5, color= factor(Carrier)), 
                #linetype = "longdash", show.legend = FALSE)



ES_8 <- coef_new %>%
  ggplot(aes(x = term, y = estimate, group = carrier)) + 
  geom_line(aes(color = carrier)) + 
  geom_point(aes(color = carrier)) ####


  scale_fill_manual(values = c("#993441", "#0029a5"))  + 
  geom_line() + 
  ggtitle("The Dynamic Effect of Airline Mergers on OPOR") + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color= factor(group)), 
                linetype = "longdash", show.legend = FALSE) + 
  scale_color_manual(values = c("#993441", "#0029a5")) + 
  geom_hline(yintercept = 0,  linetype = "longdash", color = "gray") + 
  geom_vline(xintercept = 0,  linetype = "longdash", color = "gray") + 
  labs(y = "Change in OPOR", x = "Quarters Relative to Merger",
       subtitle = "High Performing Acquirers") + 
  scale_x_continuous(breaks = seq(-4, 20, by = 1)) + 
  scale_y_continuous(breaks = seq(-2, 2, by = 0.05)) + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 90),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), 
        legend.position = "none")

plot(ES_HighP)
ggsave(ES_HighP, filename = here::here("Figs_Tables", "ES_HighP.png"), 
       dpi = 500, width = 6, height = 4)


#### 2. Instantaneous Effect - High Performer ####

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
set.seed(9999) # set seed for exact model outputs 
# model without control for baselining. Not reported in manuscript
model_IE_nc = did_multiplegt(Essay3_R, Y, G, T, D, placebo = 4, 
                             brep = 5, 
                             parallel = TRUE,
                             cluster = "Carriercode")

model_IE = did_multiplegt(Essay3_R, Y, G, T, D, controls = controls, 
                          dynamic = 0, 
                          placebo = 4,
                          brep = 2, # brep (bootstrap) to get CI
                          parallel = TRUE, 
                          cluster = 'Carriercode') 
list(model_IE)


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

IE_plot_HighP =  df %>% 
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
       subtitle = "High Performing Acquirers") + 
  scale_x_continuous(breaks = seq(-4, 0, by = 1)) + 
  scale_y_continuous(breaks = seq(-0.5, 0.5, by = 0.1)) + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 90),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")

IE_plot_HighP

ggsave(IE_plot_HighP, filename = here::here("Figs_Tables", "IE_Plot_HighP.png"),
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

coef_IE_HighP = tidy_IE(model_IE)
write.csv(coef_IE_HighP, file = "coef_IE_HighP.csv")













