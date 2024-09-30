packages <- c("tidyverse", "lubridate", "ggpubr", "ggthemes", "Polychrome", 
              "car", "lmtest", "changepoint")

# for (x in packages) {
#   if (!require(x, character.only = TRUE)) {
#     install.packages(x,
#                      dependencies = TRUE,
#                      repos='http://cran.us.r-project.org')
#   }
# }

for (x in packages) {
  library(x, character.only = TRUE)
}

theme_set(
  theme_bw() +
    theme(legend.position = "top")
)

covid <- readRDS("D:/Data/Datasets/covid_99_class_urls.RDS")

# Formatting the date column
covid$created_at <- ymd_hms(covid$created_at)
covid$date <- as.Date(covid$created_at)

agg_covid <- covid |>
  group_by(date, label) |>
  summarise(tweet_count = n())

agg_covid_wide <- agg_covid |>
  pivot_wider(names_from = label, values_from = tweet_count)

agg_covid_wide[is.na(agg_covid_wide)] <- 0

mann_whitney_test <- wilcox.test(agg_covid_wide$"non.misinfo", agg_covid_wide$misinfo, 
                                 exact = FALSE, correct = FALSE)
print(mann_whitney_test)

n_iterations <- 1000
set.seed(123)
results <- replicate(n_iterations, {
  sample_non_misinfo <- sample(agg_covid$tweet_count[agg_covid$label == "non.misinfo"], 
                               size = sum(agg_covid$label == "misinfo"), 
                               replace = TRUE)
  sample_misinfo <- agg_covid$tweet_count[agg_covid$label == "misinfo"]
  wilcox.test(sample_non_misinfo, sample_misinfo, exact = FALSE)$p.value
})
mean(results < 0.05)
# 1


ks.test <- ks.test(agg_covid_wide$misinfo, agg_covid_wide$"non.misinfo")
print(ks.test)


ggplot(agg_covid, aes(x = date, y = tweet_count, color = label)) +
  geom_line() +
  labs(title = "Peaks in Misinformation Dissemination",
       x = "Month",
       y = "Number of Instances",
       color = "Peak:") +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "2 months", minor_breaks  = "1 month") +
  theme(axis.text.x = element_text(angle=30, vjust = 0.5, hjust = 0.5))


acf(agg_covid_wide$misinfo, main = "Autocorrelation - Misinformation")
acf(agg_covid_wide$"non.misinfo", main = "Autocorrelation - Non-misinformation")


agg_covid_wide$time <- seq_along(agg_covid_wide$date)

# Fit a Poisson regression model
glm_model <- glm(misinfo ~ time, family = poisson(link = "log"), data = agg_covid_wide)
summary(glm_model)


# Create a contingency table
orig_table <- table(covid$date, covid$label)
# Perform the Chi-Square test
orig_chi <- chisq.test(orig_table)
orig_stat <- orig_chi$statistic
# p-value < 2.2e-16

covid$month <- format(as.Date(covid$created_at), format = "%Y-%m")
covid$month <- ym(covid$month)

ks.test(covid$month[covid$label == "misinformation"], 
        covid$month[covid$label == "non.misinformation"])


library(mgcv)
covid$label_dic <- case_when(
  covid$label == "non.misinfo" ~ 0,
  covid$label == "misinfo" ~ 1)

covid$date <- as.POSIXct(covid$date) # if it's a date-time variable
covid$label_dic <- as.factor(covid$label_dic)

model <- gam(label_dic ~ s(as.numeric(date)), family = binomial, data = covid)
summary(model)
plot(model)

new_data <- data.frame(date = seq(from = min(covid$date), to = max(covid$date), by = "days"))
new_data$predicted <- predict(model, new_data, type = "response")



# covid$month <- format(as.Date(covid$created_at), format = "%Y-%m")
# covid$month <- ym(covid$month)
# 
# covid$label_dic <- case_when(
#   covid$label == "non.misinfo" ~ 0,
#   covid$label == "misinfo" ~ 1)
# 
# covid$label <- case_when(
#   covid$label == "non.misinfo" ~ "non misinformation",
#   covid$label == "misinfo" ~ "misinformation")
# 
# covid_date <- covid |>
#   ungroup() |>
#   count(month, label, sort = TRUE)
# 
# wide_covid <- covid_date |>
#   spread(label, n)

covid |>
  ungroup() |>
  count(label)
# misinformation = 34 581
# non-misinformation = 739 362
# 0.045

################################################################################
# H1: Tweets classified as misinformation will exhibit distinct temporal patterns 
# compared to non-misinformation tweets during the COVID-19 pandemic.

# Checking the distribution of the residuals
model <- lm(label_dic~month, data = covid)
res <- resid(model)
qqnorm(res)
qqline(res)
# The plot shows that the residuals stray from the line quite a bit at the tails -> not normally distributed

plot(density(res))
# The density plot confirms that the residuals does not follow a normal distribution

# Checking for heteroscedasticity 
covid$res <- model$residuals
ggplot(covid, aes(y=res, x=month)) +
  geom_point()+
  geom_abline(slope=0)
# Clear signs of heteroscedasticity

levene_test <- leveneTest(n ~ label, data = covid_date)

print(levene_test)
# F-value: 31.622
# p-value: 3.57e-07
# Heteroscedasticity is present: The difference in variances between the misinfo and non-misinfo groups is statistically significant. 
# The null hypothesis (that the variance is equal) is thus rejected

# Using the Mann Whitney u test instead of a standard t-test, since my data does not meet the assumptions of normality and homoscedasticity. 
mann_whitney_test <- wilcox.test(n ~ label, data = covid_date, exact = FALSE)
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
  labs(title = "Comparison of Monthly Tweet Counts (Log-Transformed)",
       x = "",
       y = "Tweet counts per month") +
  theme(axis.text=element_text(size=12))

# Create a contingency table
contingency_table <- table(covid$month, covid$label)
# Perform the Chi-Square test
chisq.test(contingency_table)
# p-value < 2.2e-16

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

################################################################################
# H2: The engagement metrics (likes, retweets, replies) 
# for misinformation tweets are higher than those for non-misinformation tweets.

# retweet_count # Number of retweets
# reply_count # number of replies
# like_count # number of likes 
# quote_count # number of quotes
# impression_count # number of times a tweet has been seen, implemented in mid-december 2022, not relevant for this data

covid_retweet <- covid |>
  count(retweet_count, label)

covid_reply <- covid |>
  count(reply_count, label)

covid_like <- covid |>
  count(like_count, label)

covid_quote <- covid |>
  count(quote_count, label)

retweet_test <- wilcox.test(n ~ label, data = covid_retweet)
print(retweet_test) # p = 2.565e-06

reply_test <- wilcox.test(n ~ label, data = covid_reply)
print(reply_test) # p = 1

like_test <- wilcox.test(n ~ label, data = covid_like)
print(like_test) # p = 0.9171

quote_test <- wilcox.test(n ~ label, data = covid_quote)
print(quote_test) # p = 0.7967


library(mvabund)
# Multivariate Generalized Linear Models (GLMs) 
# Convert the engagement variables to a matrix for mvabund
engagement_data <- mvabund(covid[, c("retweet_count", "like_count", "reply_count", "quote_count")])
covid_small <- covid |>
  select(c(retweet_count, like_count, reply_count, quote_count, label, month))

# Fit the model
model_mvabund <- manyglm(engagement_data ~ label, family = "poisson", data = covid_small)
summary(model_mvabund)
