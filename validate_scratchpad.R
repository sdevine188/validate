library(tidyverse)
library(validate)

# https://cran.r-project.org/web/packages/validate/vignettes/introduction.html

# load women data
data(women)
women %>% glimpse()

# use check_that to validate simple rules
women %>% check_that(height > 0, weight > 0, height/weight > 0.5) %>% summary()

# use confront to app pre-specified validation rules created with validator()
women1 <- women
validation_rules <- validator(height == women_reference$height)
women %>% confront(x = validation_rules, ref = list(women_reference = women1)) %>% summary()

# use validator() to list multiple rules
validation_rules <- validator(height > 0, weight > 0, height/weight > 0)
women %>% confront(x = validation_rules) %>% summary()

# use validator to create intermediate variables then used in validation rules
validation_rules <- validator(BMI := (weight*0.45359)/(height*0.0254)^2, rule_1 = height > 0, rule_2 = weight > 0,
        rule_3 = BMI < 23, rule_4 = mean(BMI) > 22 & mean(BMI) < 22.5)
validation_rules
women %>% confront(validation_rules) %>% summary()

# create validation rules from data.frame
# must have a variable called "rule"
df <- data.frame(rule = c("height>0","weight>0","height/weight>0.5"),
        label = c("height positive","weight positive","ratio limit"))
validation_rules <- validator(.data = df)
validation_rules
women %>% confront(validation_rules) %>% summary()

# aggregate validation test output from confront()
output <- women %>% confront(validation_rules)
output
output %>% summary()
# aggregate by rule
output %>% aggregate()
# aggregate by record - note that each record has its own row, presumably in the order of appearance in data
output %>% aggregate(by = "record")
output %>% aggregate(by = "record") %>% rownames()
women %>% dim()
