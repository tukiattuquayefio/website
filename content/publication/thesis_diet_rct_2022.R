# re-analysis of thesis diet RCT study

# load packages
library(pacman)
pacman::p_load(tidyverse, easystats, performance, 
               parameters, ggstatsplot, 
               gtsummary, flextable, ggrdiges)

# load data from PLOS1
# r read csv from url
# allows you to directly download csv file from website
data_raw <- read.csv("https://doi.org/10.1371/journal.pone.0172645.s001")

# Select variables
data <- data_raw %>% 
  dplyr::select(PID:Age, DFS, K10_SUM, IPAQ_Total, 
                HVLT_ret_1_1:DS_backward_4_2, WC_1, BMI_1, Energy_kJ) %>% 
  dplyr::mutate(DS_total_1_1 = DS_forward_1_1 + DS_backward_1_1,
                DS_total_1_2 = DS_forward_1_2 + DS_backward_1_2,
                DS_total_4_1 = DS_forward_4_1 + DS_backward_4_1,
                DS_total_4_2 = DS_forward_4_2 + DS_backward_4_2) %>% 
  dplyr::select(-c(DS_forward_1_1:DS_backward_4_2)) %>% 
  dplyr::mutate(GROUP = factor(GROUP, levels = c(1,2), labels = c("Control", "Diet")),
                Sex   = factor(Sex,   levels = c(1,2), labels = c("Male"   , "Female")),
                PID   = as.factor(PID))

# summary of data
names(data)
summary(data)


data %>% 
  dplyr::select(-c(PID, HVLT_RET_DAY1:HVLT_changeacrossdays,LM_RET_DAY1,LM_RET_DAY4)) %>% 
  gtsummary::tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd}) [{min} - {max}]", # stats and format for continuous columns
                                          all_categorical() ~ "{n} ({p}%)"), # stats and format for categorical columns
                         digits = all_continuous() ~ 1, # rounding for continuous columns
                         type = where(is.numeric) ~ "continuous",
                         label = list( # display labels for column names
                           GROUP ~ "Group",
                           Sex ~ "Sex",
                           Age ~ "Age",
                           DFS ~ "Total DFS Score",
                           K10_SUM ~ "K-10 Score",
                           IPAQ_Total ~ "Total Physical Activity (IPAQ)",
                           BMI_1 ~ "BMI",
                           WC_1 ~ "Waist Circumference",
                           Energy_kJ ~ "Total Energy (kJ)",
                           HVLT_ret_1_1 ~ "HVLT Retention % Day 1 Time 1",
                           HVLT_ret_1_2 ~ "HVLT Retention % Day 1 Time 2",
                           HVLT_ret_4_1 ~ "HVLT Retention % Day 4 Time 1",
                           HVLT_ret_4_2 ~ "HVLT Retention % Day 4 Time 2",
                           LM_RET_1_1 ~ "Logical Memory Retention % Day 1 Time 1",
                           LM_RET_1_2 ~ "Logical Memory Retention % Day 1 Time 2",
                           LM_RET_4_1 ~ "Logical Memory Retention % Day 4 Time 1",
                           LM_RET_4_2 ~ "Logical Memory Retention % Day 4 Time 2",
                           DS_total_1_1 ~ "Digit Span Total Day 1 Time 1",
                           DS_total_1_2 ~ "Digit Span Total Day 1 Time 2",
                           DS_total_4_1 ~ "Digit Span Total Day 4 Time 1",
                           DS_total_4_2 ~ "Digit Span Total Day 4 Time 2"),
                         missing = "no", # how missing values should display
                         sort = list(all_categorical() ~ "frequency") # sort categorical variables by frequency
  ) %>%
  gtsummary::modify_caption(caption  = "Sample Characteristics") %>% 
  gtsummary::bold_labels()


# pivot data longer
cog_vars <- c("HVLT_ret_1_1","HVLT_ret_1_2","HVLT_ret_4_1","HVLT_ret_4_2",
              "LM_RET_1_1","LM_RET_1_2","LM_RET_4_1","LM_RET_4_2",
              "DS_total_1_1","DS_total_1_2","DS_total_4_1","DS_total_4_2")

data_long <- stats::reshape(data = as.data.frame(data), 
                            idvar = "PID", 
                            varying = list(HVLT=c(8:11),
                                           LM=c(15:18),
                                           DS=c(24:27)), 
                            direction="long", 
                            v.names = c("HVLT","LM", "DS"),
                            sep="_") %>% 
  dplyr::mutate(Day = dplyr::case_when(time == 1 | time == 2 ~ 1,
                                       time == 3 | time == 4 ~ 2)) %>% 
  dplyr::mutate(Time = dplyr::case_when(time == 1 | time == 3 ~ 1,
                                       time == 2 | time == 4 ~ 2)) %>%
  dplyr::mutate(Day   = factor(Day,   levels = c(1,2), labels = c("Day 1"  , "Day 2")),
                Time  = factor(Time,  levels = c(1,2), labels = c("Pre"    , "Post"))) %>% 
  dplyr::select(-c(time,HVLT_RET_DAY1:LM_RET_DAY4))

summary(data_long)

# correlations
corr <- ggstatsplot::ggcorrmat(data %>% select(where(is.numeric)))

# descriptives

# energy by group
psych::describeBy(x = data$Energy_kJ, group = data$GROUP, data = data,digits = 2)
stats::t.test(Energy_kJ~GROUP, data = data)
(energy_group_ridges <- data %>% 
    ggplot( 
      aes(x = Energy_kJ, 
          y = GROUP, 
          fill = GROUP)) +
    geom_density_ridges() + 
    theme_ridges() +
    labs("Energy consumed by Group") +
    xlab("Energy (kJ)") +
    ylab("Group") +
    scale_fill_viridis_d() + 
    theme(legend.position = "none"))

energy_group <- ggstatsplot::ggbetweenstats(data = data,
                            x = GROUP, 
                            y = Energy_kJ,
                            title = "Energy (kJ) by Group",
                            ylab = "Energy (kJ)",
                            outlier.tagging = TRUE,
                            outlier.label = PID)


# bmi by group
psych::describeBy(x = data$BMI_1, group = data$GROUP, data = data,digits = 2)
stats::t.test(BMI_1~GROUP, data = data)
(bmi_group_ridges <- data %>% 
    ggplot( 
      aes(x = BMI_1, 
          y = GROUP, 
          fill = GROUP)) +
    geom_density_ridges() + 
    theme_ridges() +
    labs("BMI consumed by Group") +
    xlab("BMI") +
    ylab("Group") +
    scale_fill_viridis_d() + 
    theme(legend.position = "none"))

bmi_group <- ggstatsplot::ggbetweenstats(data = data,
                            x = GROUP, 
                            y = BMI_1,
                            title = "BMI by Group",
                            ylab = "BMI",
                            outlier.tagging = TRUE,
                            outlier.label = PID)

see::plots(energy_group, bmi_group, tags = paste("Fig. ", 1:2))

# DATA ANALYSIS -----------------------------------------------------------


# ANCOVA HVLT

lm.hvlt <- lm(HVLT ~ Day*Time*GROUP + BMI_1 + Sex + Age + DFS, data = data_long)
summary(lm.hvlt)
parameters::parameters(lm.hvlt)
plot(parameters::parameters(lm.hvlt))
ggstatsplot::ggcoefstats(lm.hvlt, exclude.intercept = TRUE)

library(lme4)
lmeModel = lmer(HVLT ~ GROUP*Day*Time + (1|PID), data=data_long)
anova(lmeModel)
lmeModel = lmer(LM ~ GROUP*Day*Time + (1|PID), data=data_long)
anova(lmeModel)
lmeModel = lmer(DS ~ GROUP*Day*Time + (1|PID), data=data_long)
anova(lmeModel)
parameters::parameters(lmeModel)
plot(parameters::parameters(lmeModel))
ggstatsplot::ggcoefstats(lmeModel, exclude.intercept = TRUE,output = "tidy")




set.seed(123)

grouped_ggwithinstats(
  data            = dplyr::filter(data_long, GROUP == "Control"),
  x               = Time,
  y               = HVLT,
  type            = "parametric",
  xlab            = "Day",
  ylab            = "HVLT Retention",
  grouping.var    = Day,
  outlier.tagging = TRUE,
  outlier.label   = PID,
  output = "plot"
)

