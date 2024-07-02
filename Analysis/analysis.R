## Analysis of the classified dataset

packages <- c("tidyverse", "lubridate", "ggpubr", "ggthemes", "Polychrome")

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

covid <- readRDS("D:/Data/misinformation_class_FINISHED_99.RDS")

# Want to look at the posting of misinfo over time
covid$month <- format(as.Date(covid$created_at), format = "%Y-%m")
covid$month <- ym(covid$month)

covid$label <- case_when(
  covid$label == "non.misinfo" ~ "non misinformation",
  covid$label == "misinfo" ~ "misinformation")

covid_date <- covid |>
  count(month, label, sort = TRUE)

covid_date_grouped <- covid |>
  count(month, label, sort = TRUE) |>
  group_by(label)

theme_set(
  theme_bw() +
    theme(legend.position = "top")
)

# timeline_plot <- covid_date |>
#   ggplot(aes(month, n, group=1)) +
#   geom_line(linewidth=1.2, colour="#00bfc4") +
#   scale_y_continuous(limits = c(0,65000), breaks = seq(0,65000,5000)) +
#   scale_x_date(date_labels = "%m-%Y", date_breaks = "2 months", minor_breaks  = "1 month") +
#   labs(title = "Number of posts per month in total", y = "", x="") +
#   theme(axis.text.x = element_text(angle=30, vjust = 0.5, hjust = 0.5))
# timeline_plot
# 
# timeline_nonmisinfo <- covid_date |>
#   filter(label == "non misinformation") |>
#   ggplot(aes(x = month, y = n, group = 1)) +
#   geom_line(linewidth = 1.2, colour="#00bfc4") +
#   scale_y_continuous(limits = c(0,60000), breaks = seq(0,60000,5000)) +
#   scale_x_date(date_labels = "%m-%Y", date_breaks = "2 months", minor_breaks  = "1 month") +
#   labs(title = "Number of posts per month not containing misinformation", y = "", x="") +
#   theme(axis.text.x = element_text(angle=20, vjust = 0.5, hjust = 0.5))
# timeline_nonmisinfo 
# 
# timeline_misinfo <- covid_date |>
#   filter(label == "misinformation") |>
#   ggplot(aes(x = month, y = n, group = 1)) +
#   geom_line(linewidth = 1.2, colour="#F8766D") +
#   scale_y_continuous(limits = c(0,4000), breaks = seq(0,4000,500)) +
#   scale_x_date(date_labels = "%m-%Y", date_breaks = "2 months", minor_breaks  = "1 month") +
#   labs(title = "Number of posts per month containing misinformation", y = "", x="") +
#   theme(axis.text.x = element_text(angle=20, vjust = 0.5, hjust = 0.5))
# timeline_misinfo 
# 
# plots <- ggarrange(timeline_nonmisinfo, timeline_misinfo,
#                    ncol = 1, nrow = 2)
# plots

timeline_plot_group <- covid_date |>
  ggplot(aes(x = month, y = n, color = label, group = label, fill = label)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(limits = c(0,60000), breaks = seq(0,60000,5000)) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "2 months", minor_breaks  = "1 month") +
  labs(title = "Posts per month, grouped by label", y = "", x="") +
  theme(axis.text.x = element_text(angle=30, vjust = 0.5, hjust = 0.5))
timeline_plot_group

hist_plot_group <- covid |>
  ggplot(aes(x = month, fill = label)) +
  geom_histogram(color="#e9ecef", binwidth = 30) +
  scale_y_continuous(limits = c(0,65000), breaks = seq(0,65000,5000)) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "2 months", minor_breaks  = "1 month") +
  scale_fill_manual(values=c("#F8766D", "#00BFC4")) +
  labs(title = "Posts per month, grouped by label", y = "n", fill = "Label") +
  theme(axis.text.x = element_text(angle=30, vjust = 0.5, hjust = 0.5))
hist_plot_group

bar_plot_group <- covid |>
  ggplot(aes(x = month, fill = as.factor(label))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels=scales::percent) +
  coord_cartesian(ylim=c(0.85, 1)) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "2 months", minor_breaks  = "1 month") +
  scale_fill_manual(values=c("#F8766D", "#00BFC4")) +
  labs(title = "Percentage of misinformation/non-misinformation tweets per month", fill = "Label", x = "", y = "") +
  theme(axis.text.x = element_text(angle=30, vjust = 0.5, hjust = 0.5))
bar_plot_group

################################################################################
scale_values <- function(x){(x-min(x))/(max(x)-min(x))}

covid_mis <- covid_date |>
  filter(label == "misinformation")

covid_non_mis <- covid_date |>
  filter(label == "non misinformation")

covid_mis$n_scaled <- scale_values(covid_mis$n)
covid_non_mis$n_scaled <- scale_values(covid_non_mis$n)

legend <- c("non-misinformation observations" = "#00bfc4", "misinformation observations" = "#F8766D")

mis_non_mis <- covid_non_mis |>
  ggplot(aes(x = month, y = n_scaled, fill = "label")) +
  geom_line(linewidth = 1.2, aes(color = "non-misinformation observations")) +
  geom_line(data = covid_mis, aes(y = n_scaled, color = "misinformation observations"), linewidth = 1.2) +
  scale_y_continuous(limits = c(0,1)) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "2 months", minor_breaks  = "1 month") +
  theme(axis.text.x = element_text(angle=30, vjust = 0.5, hjust = 0.5)) +
  labs(y = "Normalized monthly incidence", x = "", color = "") +
  scale_color_manual(values = legend)
mis_non_mis

################################################################################
# The external sources

# Loading the data with info about external sites, and combining the two
covid_ext <- readRDS("D:/Data/covid_relevant_nort_domain.RDS")

covid <- covid |>
  merge(covid_ext, by = "id") |>
  select(tweet, month, id, created_at = created_at.x, author_hash = author_hash.x, label, dom_url)

covid_sites <- covid |>
  count(label, dom_url, sort = TRUE)

# want to remove URL shorteners 
covid_sites <- covid_sites[!grepl("dlvr.it", covid_sites$dom_url),]
covid_sites <- covid_sites[!grepl("bit.ly", covid_sites$dom_url),]

# Selecting the top 20 misinfo sites
covid_mis <- covid_sites |>
  filter(label == "misinformation") |>
  head(n = 21)
covid_mis <- covid_mis[-1,]

covid_mis$n_scaled <- scale_values(covid_mis$n)
# misinfo_sites <- covid_mis |>
#   mutate(dom_url = reorder(dom_url, n, decreasing = TRUE)) |>
#   ggplot(aes(x = dom_url, y = n)) +
#   geom_bar(fill="#F8766D", stat = "identity") +
#   labs(title = "Misinformation posts with external links", 
#        subtitle = "The most linked to external sites among tweets labeled as containing misinformation, top 20 sites", 
#        y = "Number of times linked to by a tweet", x = "") +
#   theme(axis.text.x = element_text(angle=30, vjust = 0.5, hjust = 0.5))
# misinfo_sites 

misinfo_sites_bar <- covid_mis |>
  head(10) |>
  ggplot(aes(x = fct_reorder(dom_url, n_scaled), y = n_scaled)) +
  geom_col(fill="#F8766D") +
  coord_flip() + 
  labs(title = "Top 10 external sites among misinformation tweets", y = "Normalized scale", x = "") +
  theme(legend.position = "none")
misinfo_sites_bar

# Selecting the top 20 non-misinfo sites
covid_nonmis <- covid_sites |>
  filter(label == "non misinformation") |>
  head(n = 21)
covid_nonmis <- covid_nonmis[-1,]

covid_nonmis$n_scaled <- scale_values(covid_nonmis$n)

non_misinfo_sites_bar <- covid_nonmis |>
  head(10) |>
  ggplot(aes(x = fct_reorder(dom_url, n_scaled), y = n_scaled)) +
  geom_col(fill="#00BFC4") +
  coord_flip() + 
  labs(title = "Top 10 external sites among non-misinformation tweets", y = "Normalized scale", x = "") +
  theme(legend.position = "none")
non_misinfo_sites_bar

# non_misinfo_sites <- covid_nonmis |>
#   mutate(dom_url = reorder(dom_url, n, decreasing = TRUE)) |>
#   ggplot(aes(x = dom_url, y = n)) +
#   geom_bar(fill="#00BFC4", stat = "identity") +
#   labs(title = "Non-misinformation posts with external links", 
#        subtitle = "The most linked to external sites among tweets labeled as not containing misinformation, top 20 sites", 
#        y = "Number of times linked to by a tweet", x = "") +
#   theme(axis.text.x = element_text(angle=30, vjust = 0.5, hjust = 0.5))
# non_misinfo_sites 

plots <- ggarrange(misinfo_sites_bar, non_misinfo_sites_bar,
                   ncol = 1, nrow = 2)
plots

###############################################################################
## NOT INCLUDED

# top sites through pandemic
covid_grouped <- covid |>
  count(month, label, dom_url, sort = TRUE)

# want to remove URL shorteners 
covid_grouped <- covid_grouped[!grepl("dlvr.it", covid_grouped$dom_url),]
covid_grouped <- covid_grouped[!grepl("bit.ly", covid_grouped$dom_url),]

covid_mis_grouped <- covid_grouped |>
  filter(label == "misinformation") |>
  drop_na(dom_url)

covid_mis_grouped_cut <- covid_mis_grouped |>
  arrange(desc(n)) |>
  group_by(month) |>
  filter(row_number(month) == 1)

misinfo_sites_timeline <- covid_mis_grouped_cut |>
  ggplot(aes(x = month, y = n, fill = dom_url)) +
  geom_col(position = "stack") +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "2 months", minor_breaks  = "1 month") +
  theme(axis.text.x = element_text(angle=30, vjust = 0.5, hjust = 0.5),
        legend.title = element_blank()) +
  labs(title = "External sites most often linked to in misinformation tweets", 
       subtitle = "Top site per month", 
       y = "Number of times linked to by a tweet", x = "")
# scale_fill_manual(values = unname(P16))
misinfo_sites_timeline

covid_grouped <- covid_grouped |>
  filter(label == "non misinformation") |>
  drop_na(dom_url)

covid_grouped_cut <- covid_grouped |>
  arrange(desc(n)) |>
  group_by(month) |>
  filter(row_number(month) == 1)

covid_grouped_cut_timeline <- covid_grouped_cut |>
  ggplot(aes(x = month, y = n, fill = dom_url)) +
  geom_col(position = "stack") +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "2 months", minor_breaks  = "1 month") +
  theme(axis.text.x = element_text(angle=30, vjust = 0.5, hjust = 0.5),
        legend.title = element_blank()) +
  labs(title = "External sites most often linked to in non-misinformation tweets", 
       subtitle = "Top site per month", 
       y = "Number of times linked to by a tweet", x = "")
# scale_fill_manual(values = unname(P16))
covid_grouped_cut_timeline

plots <- ggarrange(covid_grouped_cut_timeline, misinfo_sites_timeline,
                   ncol = 1, nrow = 2)
plots

################################################################################
## Percentage of links being to alternative media

# loading a dataset with list of alternative media sites
am <- read.csv2("~/INORK_R/Processing/misinfo_sites.csv")
am <- am |>
  select(c("dom_url" = "url", "source")) |>
  tail(13)

covid_sites <- covid |>
  count(label, dom_url, sort = TRUE) |>
  drop_na(dom_url)

am_sites <- covid_sites |>
  left_join(am, by = "dom_url")

sites_percent <- am_sites |>
  group_by(label) |>
  mutate(percentage = n / sum(n) * 100) |>
  drop_na(source)

sites_percent$percentage <- round(sites_percent$percentage, digits = 2)

sites_percent_mis <- sites_percent |>
  filter(label == "misinformation")

sites_percent_nonmis <- sites_percent |>
  filter(label == "non misinformation")

# legend <- c("non-misinformation" = "#00bfc4", "misinformation" = "#F8766D")
# 
# am_sites <- sites_percent_mis |>
#   ggplot(aes(x = fct_reorder(dom_url, percentage), y = percentage, fill = "label")) +
#   geom_col(aes(color = "misinformation"), position = "dodge") +
#   geom_col(data = sites_percent_nonmis, position = "dodge",
#            aes(x = dom_url, y = percentage, colour = "non-misinformation")) +
#   coord_flip() +
#   labs(title = "", y = "", x = "")
# am_sites

am_sites_plot <- sites_percent |>
  ggplot(aes(x = fct_reorder(dom_url, percentage), y = percentage, fill = label, color = label)) +
  geom_col(position = "dodge") + 
 # geom_text(aes(label = percentage), position = position_dodge(width = 1), size = 3.5, hjust = -0.1, color = "black") +
  coord_flip() + 
  scale_y_continuous(limits = c(0,2.6), breaks = seq(0,2.6, 0.2)) +
  labs(title = "", x = "", y = "Percent") +
  theme(legend.title=element_blank())
am_sites_plot
