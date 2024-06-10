library(skimr)
library(tidyverse)
library(rstatix) # statistical test
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(car)
options(pillar.sigfig = 3)

# read the file
babies_df <- read.csv(file = 'babies.csv', sep = ",")
data <- babies_df[, c("wt", "smoke")]

df <- na.omit(data)
df


summary(df)

str(df)





attach(df)
df$smoke <- factor(smoke)

# quality of data
skim(as_tibble(df))


# descriptive statistics
t1 <- df %>% get_summary_stats(wt, show = c("n", "min", "max", "mean", "median", "iqr", "sd"))
t2 <- df %>% group_by(smoke) %>% get_summary_stats(wt, show = c("n", "min", "max", "mean", "median", "iqr", "sd")) # to appendix

t1
t2



# ---Histogram--- to show distribution between categories


# Set margin size for plots
par(mfrow = c(3, 2), mar = c(4, 4, 2, 1))

# Plot histograms for each category
first <- df$wt[df$smoke == 0]
hist(first, col = 'gray', main = "Histogram of category 0", ylab = "Frequency",
     freq = TRUE, xlab = "Weight in ounces", breaks = 10)

second <- df$wt[df$smoke == 1]
hist(second, col = 'gray', main = "Histogram of category 1", ylab = "Frequency",
     freq = TRUE, xlab = "Weight in ounces", breaks = 10)

third <- df$wt[df$smoke == 2]
hist(third, col = 'gray', main = "Histogram of category 2", ylab = "Frequency",
     freq = TRUE, xlab = "Weight in ounces", breaks = 10)

fourth <- df$wt[df$smoke == 3]
hist(fourth, col = 'gray', main = "Histogram of category 3", ylab = "Frequency",
     freq = TRUE, xlab = "Weight in ounces", breaks = 10)

fifth <- df$wt[df$smoke == 9]
hist(fifth, col = 'gray', main = "Histogram of category 9", ylab = "Frequency",
     freq = TRUE, xlab = "Weight in ounces", breaks = 10)






# ---Assumptions check---



# check normality assumption


gr3 <- ggqqplot(df, "wt", facet.by = "smoke",color = 'blue')
gr3



# variance homogeneity checking 


l_test <- leveneTest(wt ~ factor(smoke), data = df)

# Print test results
summary(l_test)



# ---Global test---

t3 <- df %>% anova_test(wt ~ smoke)
t3
#summary(model)
#anova(model)
# As the p-value is less than the significance level 0.05,
# we can conclude that there are significant differences
# between the groups 


# ---Two-sample t-tests---
# ---Two-sample t-tests---
# ---Two-sample t-tests---


t4 <- pairwise.t.test(wt,smoke, pool.sd = TRUE, p.adjust.method="none")
t4



# Adjusted with Bonferroni method


t6 <- pairwise.t.test(df$wt, df$smoke, pool.sd = TRUE, p.adjust.method = "bonferroni")
t6


# Adjusted with Tukey's HSD and confidence interval

t5 <- TukeyHSD(aov(wt ~ smoke,
                          data = df))

t5




