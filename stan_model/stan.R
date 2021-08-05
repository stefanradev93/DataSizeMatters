model_string = "

data {
    int<lower=0> C;         // Number of conditions
    int<lower=0> N_c;      // Number of congruent trials (for this person)
    int<lower=0> N_i;      // Number of incongruent trials (for this person)

    real X_c[N_c];  // RTs congruent
    int B_c[N_c];   // Responses congruent, for boundary in {0, 1}
    
    real X_i[N_i];  // RTs incongruent
    int B_i[N_i];   // Responses incongruent, for boundary in {0, 1}
}

parameters {
    //parameters
    vector[C] v;           // drift rate per condition 
    vector[C] a;           // boundary per condition 
    vector[C] t;           // ndt correct/error
}


model {
    // Diffusion model
    t[1] ~ uniform(0.1, 3.0); //correct ndt
    t[2] ~ uniform(0.1, 7.0); //error ndt
    v[1] ~ uniform(0, 7.0); // congruent v
    a[1] ~ uniform(0, 7.0); // congruent a
    v[2] ~ uniform(0, 4.0); // incongruent v
    a[2] ~ uniform(0, 4.0); // incongruent a
    
                // congruent trial loop
    for (nc in 1:N_c) {
                    if (B_c[nc] == 1) {
                        X_c[nc] ~ wiener(a[1], t[1], 0.5, v[1]);
                    } else {
                        X_c[nc] ~ wiener(a[1], t[2], 1 - 0.5, -v[1]);
                    }
        }
                // incongruent trial loop
    for (ni in 1:N_i) {
                    if (B_i[ni] == 1) {
                        X_i[ni] ~ wiener(a[2], t[1], 0.5, v[2]);
                    } else {
                        X_i[ni] ~ wiener(a[2], t[2], 1 - 0.5, -v[2]);
                    }
                }
            }
"

library(rstan)
library(tidyverse)
library(tictoc)

# compile model
sm = stan_model(model_code=model_string)

# get starting values
initf1 <- function() {
  list(v = c(2,2), a = c(2,2), t=c(0.3,0.3))} 

# get data
data <- read_csv("stan/stan_data.csv") %>% rename(subject = id)
ids <- unique(data$subject)
ids <- ids[330:6400]

# loop for sampling
fits <- list()
tic("total_sampling")
counter <- 0
for (id in ids) {
  tryCatch({
  data_1p <- data %>% filter(subject==id) %>% filter(rt>0)
  data_c <- data_1p %>% filter(block==1)
  data_i <- data_1p %>% filter(block==0)
  
  stan_data = list(C=2, N_c=nrow(data_c), N_i=nrow(data_i), 
                   X_c = data_c$rt, B_c = data_c$response,
                   X_i = data_i$rt, B_i = data_i$response)
  
  # sample 
  fit = rstan::sampling(sm, data=stan_data, iter=2000, warmup=1000, chains=4, init=initf1,
                        control=list(adapt_delta=.9999999, stepsize =.1), cores=4)
  # save results in list
  fit_summary <- as.data.frame(summary(fit)$summary)
  sampler_stats  <-get_sampler_params(fit, inc_warmup = FALSE)
  divergent <- sum(sampler_stats[[1]][,5])+sum(sampler_stats[[2]][,5])+
    sum(sampler_stats[[3]][,5])+sum(sampler_stats[[4]][,5])
  fit_summary <- cbind(fit_summary,divergent)
  save(fit_summary, file=paste0("stan/samples/",id, ".RData"))
  fits[[as.character(id)]] = divergent
  }, error=function(e){})
  counter = counter + 1 
  cat(counter)
}
toc()


#time: 201268.837sec for 6071
# 460.45 days for 1,200,000
