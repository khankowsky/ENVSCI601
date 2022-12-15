# RScs2202_tidy
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
summary(case2202)
summary(case2202)

scatterplotMatrix(~ PctCover + ForestAge + log(Salamanders + 0.5), data = case2202, 
                  smooth = FALSE, var.labels = c("PctCover", "ForestAge", "Salamanders"))

# Using 70% as the dividing line
case2202$Closed <- ifelse(case2202$PctCover > 70,"closed", "open")

ssom <- glm(Salamanders ~ PctCover * ForestAge * Closed + I(PctCover^2) + I(ForestAge^2) + 
              I(PctCover^2):Closed + I(ForestAge^2):Closed, 
            data = case2202, family = poisson)
summary(ssom)

1 - pchisq(89.178, df = 35)

gf_point(residuals(ssom, type = "deviance") ~ fitted(ssom)) %>%
  gf_hline(yintercept = 0, color = "blue") %>%
  gf_hline(yintercept = 2, color = "gray60", linetype = 2)  %>%
  gf_hline(yintercept = -2, color = "gray60", linetype = 2) %>%
  gf_labs(x = "Fitted means", y = "Deviance residual")

# The full model via quasi-likelihood
ssom <- update(ssom, . ~ ., family = quasipoisson)

# The reduced (inferential) model
inferential_model <- glm(Salamanders ~ PctCover * Closed + I(PctCover^2) + 
                           I(PctCover^2):Closed, data = case2202, family = quasipoisson)

# drop-in-deviance F test
anova(inferential_model, ssom, test = "F")

aug <- augment(inferential_model)
gf_point(Salamanders ~ PctCover, data = aug) %>%
  gf_line(exp(.fitted) ~ PctCover, color = ~Closed) %>%
  gf_labs(x = "Percentage of Canopy Cover",
          y = "Salamander Count")
