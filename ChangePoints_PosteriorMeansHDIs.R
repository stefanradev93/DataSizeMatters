library(tidyverse)
library(HDInterval)
change_point_chains <- read_csv("C:/Users/User1/Seafile/Revision/change_point_chains.csv")


# rts --------------

# rt congruent cp1, posterior mean and 95% hdi
change_point_chains %>% select(contains("rt_congruent_cp1")) %>%
  gather(.) %>% select(value) %>%
  summarise_all(list(mean, HDInterval::hdi))

# rt congruent cp2, posterior mean and 95% hdi
change_point_chains %>% select(contains("rt_congruent_cp2")) %>%
  gather(.) %>% select(value) %>%
  summarise_all(list(mean, HDInterval::hdi))

# rt incongruent cp1, posterior mean and 95% hdi
change_point_chains %>% select(contains("rt_incongruent_cp1")) %>%
  gather(.) %>% select(value) %>%
  summarise_all(list(mean, HDInterval::hdi))

# rt incongruent cp2, posterior mean and 95% hdi
change_point_chains %>% select(contains("rt_incongruent_cp2")) %>%
  gather(.) %>% select(value) %>%
  summarise_all(list(mean, HDInterval::hdi))

# drift rates --------------

# drift congruent cp1, posterior mean and 95% hdi
change_point_chains %>% select(contains("v2_cp1")) %>%
  gather(.) %>% select(value) %>%
  summarise_all(list(mean, HDInterval::hdi))

# drift congruent cp2, posterior mean and 95% hdi
change_point_chains %>% select(contains("v2_cp2")) %>%
  gather(.) %>% select(value) %>%
  summarise_all(list(mean, HDInterval::hdi))

# drift incongruent cp1, posterior mean and 95% hdi
change_point_chains %>% select(contains("v1_cp1")) %>%
  gather(.) %>% select(value) %>%
  summarise_all(list(mean, HDInterval::hdi))

# drift incongruent cp2, posterior mean and 95% hdi
change_point_chains %>% select(contains("v1_cp2")) %>%
  gather(.) %>% select(value) %>%
  summarise_all(list(mean, HDInterval::hdi))

# boundary separations --------------

# boundary congruent cp1, posterior mean and 95% hdi
change_point_chains %>% select(contains("a2_cp1")) %>%
  gather(.) %>% select(value) %>%
  summarise_all(list(mean, HDInterval::hdi))

# boundary congruent cp2, posterior mean and 95% hdi
change_point_chains %>% select(contains("a2_cp2")) %>%
  gather(.) %>% select(value) %>%
  summarise_all(list(mean, HDInterval::hdi))

# boundary incongruent cp1, posterior mean and 95% hdi
change_point_chains %>% select(contains("a1_cp1")) %>%
  gather(.) %>% select(value) %>%
  summarise_all(list(mean, HDInterval::hdi))

# boundary incongruent cp2, posterior mean and 95% hdi
change_point_chains %>% select(contains("a1_cp2")) %>%
  gather(.) %>% select(value) %>%
  summarise_all(list(mean, HDInterval::hdi))

# non-decision times --------------

# correct non-decision times cp1, posterior mean and 95% hdi
change_point_chains %>% select(starts_with("t_cp1")) %>%
  gather(.) %>% select(value) %>%
  summarise_all(list(mean, HDInterval::hdi))

# error non-decision times cp1, posterior mean and 95% hdi
change_point_chains %>% select(contains("t_minus_cp1")) %>%
  gather(.) %>% select(value) %>%
  summarise_all(list(mean, HDInterval::hdi))
                                            