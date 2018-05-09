library(tidyverse)
library(here)
library(knitr)
library(broom)

lundata <- read_csv(here("data", 'lunden_data (francisco).csv'))
head(lundata)

lundatafran <- lundata %>% 
  select(., -`Pitch = contrastive`)
   
lang_tidy_dur <- lundata %>%
  select(., -`Language name`, -`Language family`, -Pitch, -Intensity, -`Pitch = contrastive`) %>%
  rename(., lang = X1, stress = `Stress system type`, duration = `Duration`, contrast = `Duration = contrastive`)

lang_all <- lundata %>%
  select(., -`Language name`, -`Language family`, -`Pitch = contrastive`) %>%
  rename(., lang = X1, stress = `Stress system type`, duration = `Duration`, intensity = `Intensity`, pitch = `Pitch`, contrast = `Duration = contrastive`)

lang_all

lang_tidy_dur %>%
  ggplot(., aes(x = lang, y = contrast, color = stress)) +
  geom_jitter(height = 0.1, width = 0.3) +
  geom_smooth(method = glm, method.args = binomial)

durcont <- lang_tidy_dur %>%
  ggplot(., aes(x = duration, y = contrast, color = stress)) +
  geom_jitter(height = 0.1, width = 0.3) +
  geom_smooth(method = glm, method.args = binomial)


glm_null <- glm(contrast ~ 1, data = lang_all, family = binomial(link = "logit"))
glm_dur <- glm(contrast ~ duration, data = lang_all, family = binomial(link = "logit"))
glm_int <-glm(contrast ~ intensity, data = lang_all, family = binomial(link = "logit"))
glm_all <- glm(contrast ~ duration + intensity + pitch, data = lang_all, family = binomial(link = "logit"))

summary(glm_null)
summary(glm_dur)
summary(glm_int)
summary(glm_all)

#main effect of duration
anova_dur <- anova(glm_null, glm_dur, test = 'Chisq')
