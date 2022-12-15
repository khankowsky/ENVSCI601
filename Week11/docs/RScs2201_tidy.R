# RScs2201_tidy
# taken from Adam Loy's R markdown file.
# By Eugene.Gallagher@umb.edu
options(width = 90)
library(dplyr)
library(ggformula)
library(Sleuth3)
library(broom)
library(car)
library(dplyr)     # data manipulation
library(ggformula) # graphics
library(Sleuth3)   # Sleuth data sets
# display four significant digits by default and no significance stars
options(digits = 4, show.signif.stars = FALSE)
summary(case2201)

# Plot the jittered data on a log scale
gf_jitter(log(Matings + 0.5) ~ Age, data = case2201, height = 0.25, width = 0.25, pch = 1) %>%
  gf_labs(x = "Age (years) -- Slightly Jittered", y = "Number of Matings (log scale)")

glm1 <- glm(Matings ~ Age + I(Age^2), data = case2201, family = poisson)
summary(glm1)

summary(fitted(glm1))
tidy(glm1)

# Eliminating the quadratic term
reduced <- update(glm1, . ~ . - I(Age^2))

# Wald CI
beta1 <- coef(reduced)[2]
se <- sqrt(vcov(reduced)[2,2])
beta1 + c(-1, 1) * qnorm(.975) * se
exp(beta1 + c(-1, 1) * qnorm(.975) * se)

# Profile-likelihood CIs
confint(glm1)

# Compare the full and reduced models.

anova(reduced, glm1, test = "Chisq")


