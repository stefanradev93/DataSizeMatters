library(tidyverse)
data <- read.csv("C:/Users/User1/Desktop/data.csv")
# cor sd mean correct rt vs age
data %>% select(Mn_RT__correct_incongruent, agex) %>%
  group_by(agex) %>% summarise_all(sd) %>%
  correlation::correlation(bayesian = T)

data %>% select(Mn_RT_correct_congruent, agex) %>%
  group_by(agex) %>% summarise_all(sd) %>%
  correlation::correlation(bayesian = T)

# cor sd ndt correct vs age
data %>% select(tplus, agex) %>%
  group_by(agex) %>% summarise_all(sd) %>%
  correlation::correlation(bayesian = T)

# cor sd ndt false vs age
data %>% select(tminus, agex) %>%
  group_by(agex) %>% summarise_all(sd) %>%
  correlation::correlation(bayesian = T)

# cor sd boundary incongruent vs age
data %>% select(a2, agex) %>%
  group_by(agex) %>% summarise_all(sd) %>%
  correlation::correlation(bayesian = T)

# cor sd boundary congruent vs age
data %>% select(a1, agex) %>%
  group_by(agex) %>% summarise_all(sd) %>%
  correlation::correlation(bayesian = T)

# cor sd drift congruent vs age
data %>% select(v1, agex) %>%
  group_by(agex) %>% summarise_all(sd) %>%
  correlation::correlation(bayesian = T)

# cor sd drift incongruent vs age
data %>% select(v2, agex) %>%
  group_by(agex) %>% summarise_all(sd) %>%
  correlation::correlation(bayesian = T)
