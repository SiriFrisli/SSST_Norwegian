
packages <- c("tidyverse", "lubridate", "ggpubr", "ggthemes", "Polychrome", 
              "car", "lmtest", "changepoint")

for (x in packages) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x,
                     dependencies = TRUE,
                     repos='http://cran.us.r-project.org')
  }
}

for (x in packages) {
  library(x, character.only = TRUE)
}

theme_set(
  theme_bw() +
    theme(legend.position = "top")
)

covid <- readRDS("D:/Data/Datasets/Classification_datasets/misinformation_class_FINISHED_99.RDS")

# Want to look at the posting of misinfo over time
covid$month <- format(as.Date(covid$created_at), format = "%Y-%m")
covid$month <- ym(covid$month)

covid$label <- case_when(
  covid$label == "non.misinfo" ~ "non misinformation",
  covid$label == "misinfo" ~ "misinformation")

covid_date <- covid |>
  count(month, label, sort = TRUE)

wide_covid <- covid_date |>
  spread(label, n)

shapiro_misinfo <- shapiro.test(wide_covid$misinformation)
shapiro_non_misinfo <- shapiro.test(wide_covid$'non misinformation')

print(shapiro_misinfo)
print(shapiro_non_misinfo)
# The p-values from both tests are smaller than 0.05, implying that the data are significantly different from normal distribution. 

levene_test <- leveneTest(n ~ label, data = covid_date)

print(levene_test)
# F-value: 31.622
# p-value: 3.57e-07
# Heteroscedasticity is present: The difference in variances between the misinfo and non-misinfo groups is statistically significant. 
# The null hypothesis (that the variance is equal) is thus rejected

# Using the Mann Whitney u test instead of a standard t-test, since my data does not meet the assumptions of normality and homoscedasticity. 
mann_whitney_test <- wilcox.test(n ~ label, data = covid_date)
print(mann_whitney_test)

# p-value is smaller than 0.05, the null hypothesis is rejected. 
# This indicates that the temporal patterns of misinformation and non-misinformation instances are significantly different from each other.

# Is the different sizes in the two groups affecting the result?
# Bootstrapping example
set.seed(123)
n_iterations <- 1000
results <- replicate(n_iterations, {
  sample_non_misinfo <- sample(covid_date$n[covid_date$label == "non misinformation"], 
                               size = sum(covid_date$label == "misinformation"), 
                               replace = TRUE)
  sample_misinfo <- covid_date$n[covid_date$label == "misinformation"]
  wilcox.test(sample_non_misinfo, sample_misinfo, exact = FALSE)$p.value
  })

# Summary of bootstrap results
mean(results < 0.05)  # Proportion of significant results
# Returns 1: The difference between the misinformation and non-misinformation groups 
# is consistently significant across all the resampled datasets. I.e. the observed 
# difference in temporal patterns is robust and not just a result of the imbalance in group sizes.

ggplot(covid_date, aes(x = label, y = log(n+1), fill = label)) +
  geom_boxplot() +
  labs(title = "Distribution of Tweet Counts",
       x = "Label",
       y = "Tweet counts, log transformed") +
  theme_minimal()

# ################################################################################
# install.packages("forecast")
# library(forecast)
# 
# # Decomposing the time series
# ts_data <- ts(misinfo_data$n, frequency = 12)
# decomp <- stl(ts_data, s.window = "periodic")
# 
# # Plotting the decomposition
# plot(decomp)

################################################################################
library(pracma)

misinfo_data <- covid_date |>
  filter(label == "misinformation") |>
  arrange(month)

misinfo_data$n_smooth <- movavg(misinfo_data$n, n = 2, type = "s")

peaks <- findpeaks(misinfo_data$n_smooth, threshold = 0.01 * max(misinfo_data$n_smooth))

# Adding peak information to the data
misinfo_data$peak <- ifelse(misinfo_data$n_smooth %in% peaks[, 1], "Peak", "Non-Peak")

# Plotting the data with peaks highlighted
ggplot(misinfo_data, aes(x = month, y = n_smooth)) +
  geom_line() +
  geom_point(aes(color = peak), size = 3) +
  labs(title = "Peaks in Misinformation Dissemination",
       x = "Month",
       y = "Number of Instances",
       color = "Peak:") +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "2 months", minor_breaks  = "1 month") +
  theme(axis.text.x = element_text(angle=30, vjust = 0.5, hjust = 0.5))

