library(tidyr)
library(dplyr)
library(lme4)
library(lmerTest)
library(emmeans)
library(ggplot2)

DataOutDir <- "/Users/patricia.naomi/Desktop/Visual_Plasticty_Pipeline/Pipeline Processed Outputs/Spreadsheets"
setwd(DataOutDir)
print(getwd())
files <- list.files(pattern = "\\.csv$",full.names = TRUE)
print(files)
dataf <- read.csv("data.csv")

headers <- colnames(dataf)
print(headers)

data1 <- data.frame( 
  subj         = as.factor(dataf$Sub_ID),
  NL_ID        = as.factor(dataf$NL_ID),
  SessionDay   = as.factor(dataf$Sess_Day),
  AttentionDep = as.factor(dataf$Attn_Dep),
  Hemifield    = as.factor(dataf$Hemifield),
  Contrast     = as.factor(dataf$Sweep),
  Pre          = as.factor(dataf$Pre),
  Post         = as.factor(dataf$Post)
)


data1$Pre <- as.numeric(as.character(data1$Pre))
data1$Post <- as.numeric(as.character(data1$Post))


data_long <- data1 %>%
  pivot_longer(
    cols = c(Pre, Post),
    names_to = "isPrePost",
    values_to = "Value"
  )

exclude_ids <- c(8,10,20, 22, 26, 27, 28, 29, 30, 32, 33, 34) # single session subjs
BadSubjs <- c(-10)
filtered_data <- data_long %>%
  filter(!(as.numeric(as.character(subj)) %in% exclude_ids |
           as.numeric(as.character(NL_ID)) %in% BadSubjs))


SSRP_Mod <- lmer(Value ~ isPrePost  *AttentionDep*SessionDay*Hemifield * Contrast + (1|subj),data = filtered_data)

anova(SSRP_Mod)
# summary(SSRP_Mod)


data1_subset <- subset(data_long, SessionDay == 1)
data2_subset <- subset(filtered_data, SessionDay == 2)

SSRP_Mod_DayX <- lmer(Value ~ isPrePost  *AttentionDep*Hemifield * Contrast + (1|subj),data = data1_subset)
anova(SSRP_Mod_DayX)


