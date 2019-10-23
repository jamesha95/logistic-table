# Here we run a multilevel logistic regression.

# We start with a simple logistic regression (ignoring any nesting)

### ------- Set up -------------------------------------------------------------

library(tidyverse)
library(stats)
library(absmapsdata)
library(lme4)
library(grattantheme)

# Most of the time, we read strings as characters, but since we're doing stats,
# it is helpful to treat them as factors. 

unemployment_data <- read_csv('unemployment_data_by_sa4.csv', 
                              col_types = "fffffffidid") %>%
  # We filter out the NA states (which correspond to "No usual address")
  # It could be interesting to re-do the analysis with a separate indicator variable for 'No usual address'
  filter(!is.na(state))

# Here's the trick that allows us to perform logistic regression easily - converting from long to wide format

wide_form <- unemployment_data %>%
  spread(key = lfs,
         value = count) %>%
  rename(emp = `Employed, any`,
         unemp = `Unemployed, any`) %>%
  mutate(logdensity = log10(density))


### ------- Simple logistic regression --------------------------------------------------------

logit_model <- glm(data = wide_form, 
                   formula = cbind(unemp, emp) ~ state + age + sex + atsi + education + age:sex + atsi:education, 
                   family = binomial())

summary(logit_model)

# Almost everything is significant, because of the size of the dataset. 
# We effectively have 9 million unit records
unemployment_data %>% summarise(sum(count))

# The exception is sex - it doesn't appear to have a direct effect, but rather 
# moderates the relationship between age and unemployment, and education and unemployment

# Let's check how each variable affects the odds of unemployment.
# By taking the exponent of the coefficients, we can see what effect each variable has on the odds-ratio
# That is, how does each variable multiply a person's odds of unemployment?
logit_model$coefficients %>% exp()

# Shockingly, the effect of having sub-Bachelor degree education is a 7-fold increase in your odds of unemployment if you are Indigenous
# For the population in general, it's only a 60% increase in odds.

# What is the baseline probability of unemployment? That is, the probability of unemployment for a reference?
# Because of the order of TableBuilder outputs, the reference is: 
# an ATSI male from NSW aged 25-29, with a Bachelor degree or higher
# The intercept is -3.447
# That means the odds are exp(-3.447) = 0.032
# That means the probability of unemployment is 3.1%, using the formula prob = odds/(1+odds)

cameo <- data.frame(#State name in full
                    state = "Queensland", 
                    
                    # "25-29 years" up to "60-64 years"
                    age = "35-39 years",
                    
                    # "Female" or "Male"
                    sex = "Female", 
                    
                    # "Non-Indigenous" or "ATSI (any)" or "Not stated"
                    atsi = "Non-Indigenous",
                    
                    # "Lower than degree-level or NA/NS/SC" or "Degree-level or higher"
                    education = "Lower than degree-level or NA/NS/SC")

predict(logit_model, newdata = cameo, type = "response")

##------ Plotting interaction effects -----------------------------------------

# Interaction terms are fascinating - how does sex moderate age, and how does ATSI status moderate education?

## a) age by sex
# i) get predictions
# We choose our reference to be a non-Indigenous Victorian with higher education
new_data = expand.grid(age = levels(unemployment_data$age),  # all combinations of
                       sex = levels(unemployment_data$sex), 
                       atsi = levels(unemployment_data$atsi)[2], # reference level
                       education = levels(unemployment_data$education)[1],
                       state = levels(unemployment_data$state)[2])

new_data$pred = predict.glm(logit_model, newdata = new_data, type = 'response') # predicted probabilities

# Now we just tidy the 'age' variable by dropping the word 'years'
new_data = mutate(new_data, 
                  age = factor(x = age, 
                               levels = levels(new_data$age), 
                               labels = str_remove(string = levels(new_data$age), pattern=' years')))
# ii) plot results
ggplot(new_data, 
       aes(x = age, y = pred, group = factor(sex), col = factor(sex))) +
  geom_point(size = 5) +
  geom_line(size = 1.05) +
  xlab('Age, years') +
  ylab('Probability unemployed') +
  scale_color_manual('', values = c('#A02226', '#F68B33')) +
  theme_grattan() +
  scale_y_continuous(limits = c(0, NA), 
                     expand = expand_scale(add = c(0, 0.005)),
                     labels = scales::percent_format(accuracy = 1)) +
  theme(legend.position = "right",
        legend.direction = "vertical")


grattan_save("interaction_sex_age.png", type = "normal")


## b) education by ATSI status
# i) get predictions
# We choose our reference to be a 25-29 year old Victorian woman
new_data2 = expand.grid(age = levels(unemployment_data$age)[1],  # all combinations of
                       sex = levels(unemployment_data$sex)[2], 
                       atsi = levels(unemployment_data$atsi), # reference level
                       education = levels(unemployment_data$education),
                       state = levels(unemployment_data$state)[2]) 
new_data2$pred = predict.glm(logit_model, newdata = new_data2, type = 'response') # predicted probabilities

# ii) plot results
ggplot(new_data2, 
       aes(x = atsi, y = pred, group = factor(atsi), col = factor(education))) +
  geom_line(size = 1.05, 
            colour = grattan_grey2) +
  geom_point(size = 5) +
  xlab('ATSI status') +
  ylab('Probability unemployed') +
  scale_color_manual('', values = c('#A02226', '#F68B33', '#FFC35A')) +
  theme_grattan() +
  scale_y_continuous(limits = c(0, NA), 
                     expand = expand_scale(add = c(0, 0.005)),
                     labels = scales::percent_format(accuracy = 1)) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal")


grattan_save("interaction_atsi_education.png", type = "normal")



####------- Multilevel logistic regression -----------------------

# We specific a model with interactions between ATSI and education, 
# and age and sex, with a random effect for SA4 (a random-intercept)

MLM <- glmer(formula = cbind(unemp, emp) ~ age + sex + atsi + education + age:sex + atsi:education + (1|sa4), 
             family = binomial(), 
             data = wide_form)

# This should take less than one minute

summary(MLM)

## examine SA4 level random intercepts
ranef = ranef(MLM)$sa4
ranef = mutate(ranef, sa4 = row.names(ranef))
hist(ranef$`(Intercept)`, breaks = 10)
# look at high areas (one's in Qld, one in WA)
filter(ranef, `(Intercept)` > 0.5)

# look at the lower (both are in NSW)
filter(ranef, `(Intercept)` < -0.5)

ranef %>%
  mutate(odds_ratio = exp(`(Intercept)`)) %>%
  left_join(sa42016, by = c("sa4" = "sa4_name_2016")) %>%
  filter(gcc_name_2016 %in% c("Greater Melbourne")) %>%
  ggplot() + 
  geom_sf(aes(geometry = geometry, fill = odds_ratio)) +
  scale_fill_gradientn(colours = grattan_pal(4)) +
  facet_grid(rows = 'gcc_name_2016')

grattan_save("unemployment_heat_map_controlled.png", type = "normal")

# Interesting that employment outcomes still appear to vary, possibly with SES?
# But using SEIFA might be a poor choice because unemployment is likely to feed into SEIFA.

# It looks like there is underlying similarity within each SA4 (which is why the random effects vary quite widely)

# What happens if we just leave sa4 as a fixed effect?

sa4_logit_model <- glm(data = wide_form, 
                       formula = cbind(unemp, emp) ~  sa4 + age + sex + atsi + education + age:sex + atsi:education, 
                       family = binomial())

summary(sa4_logit_model)

# Since we've sampled every SA4, this is probably an okay thing to do. 
# Normally you'd use a random intercept model when you haven't sampled from every cluster



# Here's a reason you might actually want to use a mixed effect model. 
# Let's investigate the effect of 'state' on unemployment, controlling for SA4

fixed_effects <- glm(data = wide_form, 
                   formula = cbind(unemp, emp) ~ state + sa4 + age + sex + atsi + education + age:sex + atsi:education, 
                   family = binomial())

summary(fixed_effects)

# So it looks like every state has a significant effect, and most SA4s are significant

# Now what happens if we include a random effect for SA4? This is like saying, individuals are 
# grouped by SA4, and a group-level predictor is 'state'. Now we can simultaneously measure the 
# effect of state and the group effect of SA4 (the bit due to unobservable factors)

mixed_effects <- glmer(data = wide_form, 
                       formula = cbind(unemp, emp) ~ state + age + sex + atsi + education + age:sex + atsi:education + (1|sa4), 
                       family = binomial())

summary(mixed_effects)

# And we see that only Qld, SA and WA are significantly different from NSW.
# While Victoria used to look different, this suggests that the difference is due to unobserved, underlying sa4
# characteristics, rather than due to the state itself. 

# As for the ACT, it's unclear (and actually irrelevant) to ask whether there's a state/territory level effect
# or just an SA4 level effect, because the ACT only consists of one sa4. 
# Interestingly, in the NT, there's no longer a significant effect of state/territory.
# The NT has two sa4s with very different intercepts (Darwin vs the Outback)
# Previously, the fixed effect model tried to estimate an effect of the NT (worsening unemployment), and
# a separate effect for Darwin (better unemployment), with no estimate for the Outback (because that would mean
# three estimates for only two actual groups).
# The mixed effect answer seems more sensible -- no discernable difference between the NT and NSW, but a definite 
# difference between the Outback and Darwin, due to unobserved factors.

sa4_effects = ranef(mixed_effects)$sa4
sa4_effects = mutate(sa4_effects, sa4 = row.names(sa4_effects))
sa4_effects %>% 
  arrange(desc(`(Intercept)`))




# ------- Diagnostics -- using SA4 as a predictor vs not using any geography -----------------------

# ROC (Receiver Operating Characteristic) is a measure of how well the model
# can discriminate between two people, one employed and one unemployed.
# Does the model do better than a coin toss? (50:50)

# First we build a model without geography

no_geo_logit_model <- glm(data = wide_form, 
                          formula = cbind(unemp, emp) ~  age + sex + atsi + education + age:sex + atsi:education, 
                          family = binomial())

summary(no_geo_logit_model)

# Now we create 'unit-record data' to run our ROC test on

unit_record_data <- splitstackshape::expandRows(dataset = unemployment_data, count = "count") %>%
  mutate(unemp = ifelse(lfs == "Employed, any", 0, 1), 
         emp = 1- unemp)

# Here's how the geograhpy-free model does

prob_no_geo = predict.glm(newdata = unit_record_data, no_geo_logit_model, type = c("response"))
unit_record_data$prob_no_geo = prob_no_geo
roc <- pROC::roc(unemp ~ prob_no_geo, data = unit_record_data)
plot(roc)
pROC::auc(roc)

# And here's how the model with sa4 covariates performs

prob = predict.glm(newdata = unit_record_data, sa4_logit_model, type = c("response"))
unit_record_data$prob = prob
roc <- pROC::roc(unemp ~ prob, data = unit_record_data)
plot(roc)
pROC::auc(roc)

# So including geographical information is an improvement, but still only gets us from a 58% to 61% chance of 
# correctly discriminating between an employed and an unemployed person. 





