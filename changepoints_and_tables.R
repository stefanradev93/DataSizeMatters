library(mcp)
library(loo)
library(tidyverse)

#get estimates directly from python
df <- read_csv("D:/iat/final_iat.csv", guess_max=10000)


# get parameter means per year of age

#rt incongruent
agemeansrt <- tapply(df$Mn_RT__correct_incongruent, df$age, mean)%>% as_tibble(rownames="age")
agemeansrt$age <- as.numeric(agemeansrt$age)
names(agemeansrt) <- c("age", "meanrt")

#rt congruent
agemeansrt_c <- tapply(df$Mn_RT_correct_congruent, df$age, mean)%>% as_tibble(rownames="age")
agemeansrt_c$age <- as.numeric(agemeansrt_c$age)
names(agemeansrt_c) <- c("age", "meanrt")

#decision caution incongruent
agemeansa <- tapply(df$a1, df$age, mean) %>% as_tibble(rownames="age")
agemeansa$age <- as.numeric(agemeansa$age)
names(agemeansa) <- c("age", "meana")

#decision caution congruent
agemeansa_c <- tapply(df$a2, df$age, mean) %>% as_tibble(rownames="age")
agemeansa_c$age <- as.numeric(agemeansa_c$age)
names(agemeansa_c) <- c("age", "meana")

# non-decision time correct responses
agemeanst <- tapply(df$tplus, df$age, mean) %>% as_tibble(rownames="age")
agemeanst$age <- as.numeric(agemeanst$age)
names(agemeanst) <- c("age", "meant")

# non-decision time error response
agemeanst_minus <- tapply(df$tminus, df$age, mean) %>% as_tibble(rownames="age")
agemeanst_minus$age <- as.numeric(agemeanst_minus$age)
names(agemeanst_minus) <- c("age", "meant")

# processing speed incongruent
agemeansv <- tapply(df$v1, df$age, mean) %>% as_tibble(rownames="age")
agemeansv$age <- as.numeric(agemeansv$age)
names(agemeansv) <- c("age", "meanv")

# processing speed congruent
agemeansv_c <- tapply(df$v2, df$age, mean) %>% as_tibble(rownames="age")
agemeansv_c$age <- as.numeric(agemeansv_c$age)
names(agemeansv_c) <- c("age", "meanv")

# processing speed change point model: two change points, linear

model_lll = list(
  meanv ~ 1 +age, 
  ~ 0 +age,
  ~ 0 + age)

# decision caution change point model: two change points, linear
model_ll_a = list(
  meana ~ 1 +age, 
  ~ 0 + age,
  ~ 0 + age)

# non-decision time change point model: one change point, linear

model_ll_t = list(
  meant ~ 1 +age, 
  ~ 0 + age, 
  ~ 0 + age)

# rt change point model: two change points, linear

model_lll_rt = list(
  meanrt ~ 1 +age, 
  ~ 0 + age,
  ~ 0 + age)


### fit  models, with custom priors

fit_lll = mcp(model_lll, data = agemeansv,iter = 200000, adapt=10000,
              prior = list(cp_1 = "dunif(20, 40)",cp_2 = "dunif(50, 70)"))

fit_lll_c = mcp(model_lll, data = agemeansv_c,iter = 200000, adapt=10000,
              prior = list(cp_1 = "dunif(20, 40)",cp_2 = "dunif(50, 70)"))

fit_lll_a = mcp(model_ll_a, data = agemeansa,iter = 200000, adapt=10000,
               prior = list(cp_1 = "dunif(15, 25)",cp_2 = "dunif(50, 70)"))

fit_lll_a_c = mcp(model_ll_a, data = agemeansa_c,iter = 200000, adapt=10000,
                prior = list(cp_1 = "dunif(15, 25)",cp_2 = "dunif(50, 70)"))

fit_ll_t = mcp(model_ll_t, data = agemeanst,iter = 200000, adapt=10000,
               prior = list(cp_1 = "dunif(12, 18)",cp_2 = "dunif(50, 70)"))

fit_ll_t_minus = mcp(model_ll_t, data = agemeanst_minus,iter = 200000, adapt=10000,
               prior = list(cp_1 = "dunif(12, 18)"))

fit_lll_rt = mcp(model_lll_rt, data = agemeansrt,iter = 200000, adapt=10000,
                prior = list(cp_1 = "dunif(15, 25)",cp_2 = "dunif(50, 70)"))

fit_lll_rt_c = mcp(model_lll_rt, data = agemeansrt_c,iter = 200000, adapt=10000,
                 prior = list(cp_1 = "dunif(15, 25)",cp_2 = "dunif(50, 70)"))

# get posterior chains

chains_v <- data.frame(v1_cp1_1 = data.frame(fit_lll[["mcmc_post"]][[1]])$cp_1,
                       v1_cp1_2 = data.frame(fit_lll[["mcmc_post"]][[2]])$cp_1,
                       v1_cp1_3 = data.frame(fit_lll[["mcmc_post"]][[3]])$cp_1,
                       v1_cp2_1 = data.frame(fit_lll[["mcmc_post"]][[1]])$cp_2,
                       v1_cp2_2 = data.frame(fit_lll[["mcmc_post"]][[2]])$cp_2,
                       v1_cp2_3 = data.frame(fit_lll[["mcmc_post"]][[3]])$cp_2)

chains_v_c <- data.frame(v2_cp1_1 = data.frame(fit_lll_c[["mcmc_post"]][[1]])$cp_1,
                       v2_cp1_2 = data.frame(fit_lll_c[["mcmc_post"]][[2]])$cp_1,
                       v2_cp1_3 = data.frame(fit_lll_c[["mcmc_post"]][[3]])$cp_1,
                       v2_cp2_1 = data.frame(fit_lll_c[["mcmc_post"]][[1]])$cp_2,
                       v2_cp2_2 = data.frame(fit_lll_c[["mcmc_post"]][[2]])$cp_2,
                       v2_cp2_3 = data.frame(fit_lll_c[["mcmc_post"]][[3]])$cp_2)

chains_a <- data.frame( a1_cp1_1 = data.frame(fit_lll_a[["mcmc_post"]][[1]])$cp_1,
                        a1_cp1_2 = data.frame(fit_lll_a[["mcmc_post"]][[2]])$cp_1,
                        a1_cp1_3 = data.frame(fit_lll_a[["mcmc_post"]][[3]])$cp_1,
                        a1_cp2_1 = data.frame(fit_lll_a[["mcmc_post"]][[1]])$cp_2,
                        a1_cp2_2 = data.frame(fit_lll_a[["mcmc_post"]][[2]])$cp_2,
                        a1_cp2_3 = data.frame(fit_lll_a[["mcmc_post"]][[3]])$cp_2)

chains_a_c <- data.frame( a2_cp1_1 = data.frame(fit_lll_a_c[["mcmc_post"]][[1]])$cp_1,
                        a2_cp1_2 = data.frame(fit_lll_a_c[["mcmc_post"]][[2]])$cp_1,
                        a2_cp1_3 = data.frame(fit_lll_a_c[["mcmc_post"]][[3]])$cp_1,
                        a2_cp2_1 = data.frame(fit_lll_a_c[["mcmc_post"]][[1]])$cp_2,
                        a2_cp2_2 = data.frame(fit_lll_a_c[["mcmc_post"]][[2]])$cp_2,
                        a2_cp2_3 = data.frame(fit_lll_a_c[["mcmc_post"]][[3]])$cp_2)
                       
chains_tplus <- data.frame(t_cp1_1 = data.frame(fit_ll_t[["mcmc_post"]][[1]])$cp_1,
                       t_cp1_2 = data.frame(fit_ll_t[["mcmc_post"]][[2]])$cp_1,
                       t_cp1_3 = data.frame(fit_ll_t[["mcmc_post"]][[3]])$cp_1)

chains_tminus<- data.frame(t_minus_cp1_1 = data.frame(fit_ll_t_minus[["mcmc_post"]][[1]])$cp_1,
                            t_minus_cp1_2 = data.frame(fit_ll_t_minus[["mcmc_post"]][[2]])$cp_1,
                            t_minus_cp1_3 = data.frame(fit_ll_t_minus[["mcmc_post"]][[3]])$cp_1)

chains_rt <- data.frame(rt_incongruent_cp1_1 = data.frame(fit_lll_rt[["mcmc_post"]][[1]])$cp_1,
                        rt_incongruent_cp1_2 = data.frame(fit_lll_rt[["mcmc_post"]][[2]])$cp_1,
                        rt_incongruent_cp1_3 = data.frame(fit_lll_rt[["mcmc_post"]][[3]])$cp_1,
                        rt_incongruent_cp2_1 = data.frame(fit_lll_rt[["mcmc_post"]][[1]])$cp_2,
                        rt_incongruent_cp2_2 = data.frame(fit_lll_rt[["mcmc_post"]][[2]])$cp_2,
                        rt_incongruent_cp2_3 = data.frame(fit_lll_rt[["mcmc_post"]][[3]])$cp_2)

chains_rt_c <- data.frame(rt_congruent_cp1_1 = data.frame(fit_lll_rt_c[["mcmc_post"]][[1]])$cp_1,
                        rt_congruent_cp1_2 = data.frame(fit_lll_rt_c[["mcmc_post"]][[2]])$cp_1,
                        rt_congruent_cp1_3 = data.frame(fit_lll_rt_c[["mcmc_post"]][[3]])$cp_1,
                        rt_congruent_cp2_1 = data.frame(fit_lll_rt_c[["mcmc_post"]][[1]])$cp_2,
                        rt_congruent_cp2_2 = data.frame(fit_lll_rt_c[["mcmc_post"]][[2]])$cp_2,
                        rt_congruent_cp2_3 = data.frame(fit_lll_rt_c[["mcmc_post"]][[3]])$cp_2)

all_chains <- cbind(chains_rt, chains_rt_c, chains_v, chains_v_c, 
                    chains_a, chains_a_c, chains_tplus, chains_tminus)
write_csv(all_chains,"change_point_chains.csv")


# Additional analyses for Appendix:

# Across person correlations
xtable::xtable(psych::corr.test(df[,1:6])[[1]], type = "latex")

# Within person correlations: fisher z transformation, means
z_cors <- df[,31:45] %>% mutate_all(~correlation:: z_fisher(r= .)) %>%
  summarise_all(mean) %>% mutate_all(~correlation:: z_fisher(z=.))

z_cor_table <- data.frame(Parameters= c("v2", "a1", "a2", "tplus", "tminus"),
                          v1 =as.double(c(z_cors[1,1], z_cors[1,2], z_cors[1,3], z_cors[1,4],z_cors[1,5])),
                          v2 =as.double(c("",z_cors[1,6], z_cors[1,7], z_cors[1,8], z_cors[1,9])), 
                          a1 = as.double(c("", "", z_cors[1,10], z_cors[1,11], z_cors[1,12] )),
                          a2 =as.double(c("", "","", z_cors[1,13], z_cors[1,14])),
                          tplus = as.double(c("", "", "", "", z_cors[1,15])))

xtable::xtable(z_cor_table, type = "latex")

# Descriptives

descriptives <- df %>% select(agex,
                              Mn_RT__correct_incongruent, Mn_RT_correct_congruent,
                              v1, v2, a1, a2, tplus, tminus)
xtable::xtable((psych::describe(descriptives) %>% select(mean:max)),  type = "latex")
