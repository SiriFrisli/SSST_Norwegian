packages <- c("tidyverse", "lubridate", "ggpubr", "ggthemes", "Polychrome", 
              "car", "lmtest", "changepoint", "ISOweek")

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

covid <- readRDS("E:/Data/Datasets/Classification_data_filtered/covid_classified_without_rt_98_FINAL.RDS")

# Formatting the date columns
covid$created_at <- ymd_hms(covid$created_at)
covid$date <- as.Date(covid$created_at)
covid$month <- format(as.Date(covid$created_at), format = "%Y-%m")
covid$month <- ym(covid$month)
covid$week_num <- strftime(covid$date, format = "%Y-%V")

glimpse(covid)

covid |>
  ungroup() |>
  count(label)
# 21809 misinfo
# 404453 non misinfo

# How many are retweets?
covid_retweets <- covid |>
  filter(!is.na(unnest_referenced_tweets_id)) # 89709

###############################################################################
# H1: Tweets classified as misinformation exhibit distinct temporal patterns 
# compared to non-misinformation tweets during the COVID-19 pandemic, 
# with peaks in activity coinciding with significant events. 

###
### TIMELINE
###

covid_week <- covid |>
  group_by(week_num, label) |>
  summarise(tweet_count = n())

covid_month <- covid |>
  group_by(month, label) |>
  summarise(tweet_count = n())

# Function to scale the values
scale_values <- function(x){(x-min(x))/(max(x)-min(x))}

covid_mis <- covid_month |>
  filter(label == "misinfo")
covid_mis <- as.data.frame(covid_mis)

covid_non_mis <- covid_month |>
  filter(label == "non.misinfo")
covid_non_mis <- as.data.frame(covid_non_mis)

covid_mis$n_scaled <- scale_values(covid_mis$tweet_count)
covid_non_mis$n_scaled <- scale_values(covid_non_mis$tweet_count)

# covid_non_mis$week_start <- ISOweek2date(paste0(gsub("-", "-W", covid_non_mis$week_num), "-1"))
# covid_mis$week_start <- ISOweek2date(paste0(gsub("-", "-W", covid_mis$week_num), "-1"))

legend <- c("non-misinformation observations" = "#dfc27d", "misinformation observations" = "#a6611a")

covid_non_mis_plot <- covid_non_mis |>
  ggplot(aes(x = month, y = n_scaled, fill = "label", group = 1)) +
  geom_line(linewidth = 1.1, aes(color = "non-misinformation observations")) +
  geom_line(data = covid_mis, aes(y = n_scaled, color = "misinformation observations"), linewidth = 1.1) +
  scale_y_continuous(limits = c(0,1)) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +
  theme(axis.text.x = element_text(angle=30, vjust = 0.5, hjust = 0.5)) +
  labs(title = "Timeline of Monthly Tweet Counts", 
       subtitle = "Counts scaled using min-max normalization",
       y = "", x = "", color = "") +
  scale_color_manual(values = legend)

ggsave(filename = "FIGURE_1.png", plot = covid_non_mis_plot, device = "png", units = "px", dpi="print")

###
### BOXPLOT, by monthly tweet count
###

covid_month_norm <- covid_month |>
  group_by(label) |>
  mutate(value_norm = scale_values(tweet_count))

covid_month_norm$label <- case_when(
  covid_month_norm$label == "non.misinfo" ~ "non-misinformation",
  covid_month_norm$label == "misinfo" ~ "misinformation")
  
ggplot(covid_month_norm, aes(x = label, y = log(tweet_count + 1), fill = label)) +
  geom_boxplot() +
  labs(title = "Boxplots of Monthly Tweet Counts",
       subtitle = "Log-transformed",
       x = "",
       y = "")  +
  theme(axis.text=element_text(size=12), 
        legend.position = "none")

covid_month_norm_plot <- covid_month_norm |>
  ggplot(aes(x = label, y = value_norm)) + 
  geom_boxplot(aes(fill = label)) + 
  scale_fill_manual(values = c("#a6611a", "#dfc27d")) +
  labs(
    title = "Tweets per month",
    y = "Min-max normalized scale",
    x = "",
  ) +
  facet_wrap(~label, scales = "free") + 
  theme(
    strip.text = element_text(size = 12),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none"
  )
ggsave(filename = "FIGURE_2.png", plot = covid_month_norm_plot, device = "png", units = "px", dpi="print")

################################################################################
# H2: The engagement metrics (likes, retweets, replies) 
# for misinformation tweets are higher than those for non-misinformation tweets.

# retweet_count # Number of retweets
# reply_count # number of replies
# like_count # number of likes 

covid |>
  group_by(label) |>
  summarise(
    retweet_mean = mean(retweet_count, na.rm = TRUE),
    retweet_median = median(retweet_count, na.rm = TRUE),
    retweet_sd = sd(retweet_count, na.rm = TRUE),
    like_mean = mean(like_count, na.rm = TRUE),
    like_median = median(like_count, na.rm = TRUE),
    like_sd = sd(like_count, na.rm = TRUE),
    reply_mean = mean(reply_count, na.rm = TRUE),
    reply_median = median(reply_count, na.rm = TRUE),
    reply_sd = sd(reply_count, na.rm = TRUE)
  )


# Fit Poisson regression model for retweets
retweet_model <- glm(retweet_count ~ label, family = poisson, data = covid)
summary(retweet_model)

# Check for overdispersion: compare residual deviance with degrees of freedom
retweet_dispersion <- with(retweet_model, sum(residuals^2) / df.residual)
retweet_dispersion  # 21.79, indicates overdispersion

like_model <- glm(like_count ~ label, family = poisson, data = covid)
summary(like_model)
like_dispersion <- with(like_model, sum(residuals^2) / df.residual)
like_dispersion  # 59, indicates overdispersion

reply_model <- glm(reply_count ~ label, family = poisson, data = covid)
summary(reply_model)
reply_dispersion <- with(reply_model, sum(residuals^2) / df.residual)
reply_dispersion # 13.48

# There's overdispersion in all of the models, so switchin to negative binomial regression
nb_retweet_model <- glm.nb(retweet_count ~ label, data = covid)
nb_like_model <- glm.nb(like_count ~ label, data = covid)
nb_reply_model <- glm.nb(reply_count ~ label, data = covid)
summary(nb_retweet_model)
summary(nb_like_model)
summary(nb_reply_model)

exp(coef(nb_retweet_model)[2]) 
exp(confint.default(nb_retweet_model))

exp(coef(nb_like_model)[2]) 
exp(confint.default(nb_like_model))

exp(coef(nb_reply_model)[2]) 
exp(confint.default(nb_reply_model))


################################################################################
# H3: The majority of identified COVID-19 misinformation originates from a small minority of users. 

misinfo_by_user <- covid |>
  filter(label == "misinfo") |>
  group_by(author_hash) |>
  summarise(misinfo_count = n()) |>
  arrange(desc(misinfo_count))

total_misinfo <- sum(misinfo_by_user$misinfo_count)
misinfo_by_user <- misinfo_by_user |>
  mutate(misinfo_proportion = misinfo_count / total_misinfo)

misinfo_by_user |>
  summarise(max = max(misinfo_count), min = min(misinfo_count), 
            mean = mean(misinfo_count), median = median(misinfo_count), 
            sd = sd(misinfo_count))

# Lorenz curve
library(ineq)
plot(Lc(misinfo_by_user$misinfo_count), col = "#F8766D", main = "Lorenz Curve for Misinformation Tweets")

misinfo_by_user <- misinfo_by_user |>
  arrange(desc(misinfo_count)) |>
  mutate(cum_tweets = cumsum(misinfo_count) / sum(misinfo_count),
         cum_users = (row_number()) / n())

# Gini coefficient
gini_coef <- Gini(misinfo_by_user$misinfo_count)
print(gini_coef)

lor_curve <- ggplot(misinfo_by_user, aes(x = cum_users, y = cum_tweets)) +
  geom_line(size = 1.2, color = "#F8766D") +       # Lorenz curve
  geom_abline(intercept = 0, size = 1, slope = 1, linetype = "dashed") +  # Line of equality
  labs(title = paste("Lorenz Curve of Misinformation Tweets by User (Gini:", round(gini_coef, 2), ")"),
       x = "Cumulative Proportion of Users",
       y = "Cumulative Proportion of Misinformation Tweets") +
  theme_minimal()

ggsave(filename = "FIGURE_3.png", plot = lor_curve, device = "png", units = "px", dpi="print")
