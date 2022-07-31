library(tidyverse)
library(ggplot2)
library(janitor)
library(rstanarm)

data <- read.csv('alzheimer_data.csv')

# Linear model

lm_mod <- stan_glm(lhippo ~ age + educ, 
                 family = gaussian, 
                 prior_intercept = normal(0, 100),
                 prior = normal(0, 10),
                 data = data)


plot(lm_mod, pars = c('age', 'educ'))

coef(lm_mod)
posterior_interval(lm_mod, prob = 0.95)


bayesplot::color_scheme_set("brightblue")
plot(lm_mod, "areas", regex_pars = c('age', 'educ'), 
     prob = 0.5, prob_outer = 0.9)

plot_title <- ggplot2::ggtitle("Posterior Distributions")
plot(lm_mod, "hist", regex_pars = c('age', 'educ')) + plot_title

bayesplot::color_scheme_set("blue")
trace <- plot(lm_mod, "trace")
trace + ggplot2::scale_color_discrete()

yrep <- posterior_predict(lm_mod)
y_avg <- apply(yrep, 1, mean)

hist(y_avg)
abline(v=mean(data$lhippo), lwd=4)

launch_shinystan(lm_mod)




# Logistic regression

data <- data %>% 
  mutate(disease_status = ifelse(diagnosis == 0, yes = 0, no = 1)) %>% 
  mutate(across(c(disease_status, diagnosis, female), as.factor))

is.factor(data$disease_status)  

table(data$disease_status,data$diagnosis)

tabyl(data$disease_status)

bglm <- stan_glm(disease_status ~ age + lhippo + naccmmse, 
                 family = binomial, 
                 prior_intercept = normal(0, 1),
                 prior = normal(0, 1),
                 data = data)

prior_summary(bglm)

plot(bglm, pars = c('age', 'lhippo', 'naccmmse'))

coef(bglm)
exp(coef(bglm))
posterior_interval(bglm, prob = 0.95)


bayesplot::color_scheme_set("brightblue")
plot(bglm, "areas", regex_pars = c('lhippo', 'naccmmse'), 
     prob = 0.5, prob_outer = 0.9)

plot_title <- ggplot2::ggtitle("Posterior Distributions")
plot(bglm, "hist", regex_pars = c('age', 'lhippo', 'naccmmse')) + plot_title

bayesplot::color_scheme_set("blue")
trace <- plot(bglm, "trace")
trace + ggplot2::scale_color_discrete()


yrep <- posterior_predict(bglm)

# obtaining the posterior samples
draws <- as.matrix(bglm)

launch_shinystan(bglm)
