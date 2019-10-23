# Here we run a logistic regression using previously-wrangled TableBuilder data

# We start with a simple logistic regression (ignoring any nesting)




### ------- Set up -------------------------------------------------------------

library(tidyverse)
library(stats)
library(absmapsdata)
library(lme4)
library(grattantheme)

# Most of the time, we read strings as characters, but since we're doing stats,
# it is helpful to treat them as factors. 

unemployment_data <- read_csv('clean_data/unemployment_data_by_sa4.csv', 
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

simple_logit_model <- glm(data = wide_form, 
                          formula = cbind(unemp, emp) ~ state + age + sex + atsi + education, 
                          family = binomial())

summary(simple_logit_model)

# Everything is significant. Women are more likely to be unemployed than men,  
# non-ATSI Australians are much less likely to be unemployed than ATSI Australians, 
# every age category is less likely to be unemployed than 25-29 year olds, and
# non-degree holders are quite a bit more likely to be unemployed.

# But let's dig a little deeper





# We add interaction terms now ------------------------------------------------------------

logit_model <- glm(data = wide_form, 
                   formula = cbind(unemp, emp) ~ state + age + sex + atsi + education + age:sex + atsi:education, 
                   family = binomial())

summary(logit_model)

# Again, almost everything is significant, because of the size of the dataset. 
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
  state = "Victoria", 
  
  # "25-29 years" up to "60-64 years"
  age = "25-29 years",
  
  # "Female" or "Male"
  sex = "Male", 
  
  # "Non-Indigenous" or "ATSI (any)" or "Not stated"
  atsi = "ATSI (any)",
  
  # "Lower than degree-level or NA/NS/SC" or "Degree-level or higher"
  education = "Lower than degree-level or NA/NS/SC")

predict(logit_model, newdata = cameo, type = "response")
# Using type = "response" means we get probabilities instead of log-odds as our output




##------ Plotting interaction effects -----------------------------------------

# Interaction terms are fascinating - how does sex moderate age, and how does ATSI status moderate education?

## a) age by sex --------------------------------------------------------------
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




## b) education by ATSI status --------------------------------------------------------------
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



