# Here we run a multi-level logistic regression using previously-wrangled TableBuilder data

### ------- Set up -------------------------------------------------------------

source('logistic_regression.R')



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





# What happens if we just leave sa4 as a fixed effect? --------------------------------------------------------------

sa4_logit_model <- glm(data = wide_form, 
                       formula = cbind(unemp, emp) ~  sa4 + age + sex + atsi + education + age:sex + atsi:education, 
                       family = binomial())

summary(sa4_logit_model)

# Since we've sampled every SA4, this is probably an okay thing to do. 
# Normally you'd use a random intercept model when you haven't sampled from every cluster




# Why would you actually bother with a multi-level model, in this example? -----------------------------------

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





#------- A second example, using population density ---------------------------------------------------------
fe_2 <- glm(data = wide_form, 
            formula = cbind(unemp, emp) ~ logdensity + sa4 + age + sex + atsi + education + age:sex + atsi:education, 
            family = binomial())

summary(fe_2)

exp(fe_2$coefficients)[["logdensity"]]
# This suggests that residents of higher-density sa4s are less likely to experience unemployment
# A resident of an sa4 with 10x the density has 8% lower odds of being unemployed

me_2 <- glmer(data = wide_form, 
                       formula = cbind(unemp, emp) ~ logdensity + age + sex + atsi + education + age:sex + atsi:education + (1|sa4), 
                       family = binomial())

summary(me_2)

exp(me_2@beta)[[2]]
# Now higher-density areas are associated with high unemployment (but it's non-signficant)
# Allowing random intercepts for sa4s has allowed us to separate the effects due to density from
# the effects due to unobserved group characteristics.
# Previously, the sa4 level estimated effects were confounded by density.
# And this suggests that there's probably unobserved sa4 factors other than density that are driving 
# the different unemployment rates between regions.

# ------- Diagnostics - using SA4 as a predictor vs not using any geography -----------------------

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
         emp = 1 - unemp)

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




