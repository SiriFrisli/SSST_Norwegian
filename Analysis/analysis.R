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

covid <- readRDS("D:/Data/misinformation_class_FINISHED_95.RDS")

covid |>
  count(label)
# misinfo = 46602 non misinfo = 611654

# Want to look at the posting of misinfo over time
covid$month <- format(as.Date(covid$created_at), format = "%Y-%m")
covid$month <- ym(covid$month)

glimpse(covid)

covid_date <- covid |>
  count(month, sort = TRUE)

covid_date_grouped <- covid |>
  count(month, label, sort = TRUE)

theme_set(
  theme_bw() +
    theme(legend.position = "top")
)

timeline_plot <- covid_date |>
  ggplot(aes(month, n, group=1)) +
  geom_line(linewidth=1.2, colour="#00bfc4") +
  scale_y_continuous(limits = c(0,65000), breaks = seq(0,65000,5000)) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "2 months", minor_breaks  = "1 month") +
  labs(title = "Number of posts per month in total", y = "", x="") +
  theme(axis.text.x = element_text(angle=30, vjust = 0.5, hjust = 0.5))
timeline_plot

timeline_nonmisinfo <- covid_date_grouped |>
  filter(label == "non.misinfo") |>
  ggplot(aes(x = month, y = n, group = 1)) +
  geom_line(linewidth = 1.2, colour="#00bfc4") +
  scale_y_continuous(limits = c(0,60000), breaks = seq(0,60000,5000)) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "2 months", minor_breaks  = "1 month") +
  labs(title = "Number of posts per month not containing misinformation", y = "", x="") +
  theme(axis.text.x = element_text(angle=20, vjust = 0.5, hjust = 0.5))
timeline_nonmisinfo 

timeline_misinfo <- covid_date_grouped |>
  filter(label == "misinfo") |>
  ggplot(aes(x = month, y = n, group = 1)) +
  geom_line(linewidth = 1.2, colour="#F8766D") +
  scale_y_continuous(limits = c(0,4000), breaks = seq(0,4000,500)) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "2 months", minor_breaks  = "1 month") +
  labs(title = "Number of posts per month containing misinformation", y = "", x="") +
  theme(axis.text.x = element_text(angle=20, vjust = 0.5, hjust = 0.5))
timeline_misinfo 

plots <- ggarrange(timeline_nonmisinfo, timeline_misinfo,
                   ncol = 1, nrow = 2)
plots


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
  geom_histogram(color="#e9ecef") +
  scale_y_continuous(limits = c(0,65000), breaks = seq(0,65000,5000)) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "2 months", minor_breaks  = "1 month") +
  scale_fill_manual(values=c("#404080", "#00BFC4")) +
  labs(title = "Posts per month", y = "n", fill = "Label") +
  theme(axis.text.x = element_text(angle=30, vjust = 0.5, hjust = 0.5))
hist_plot_group

################################################################################
# The external sources

# Loading the data with info about external sites, and combining the two
covid_ext <- readRDS("D:/Data/covid_nort_domain.RDS")

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
  filter(label == "misinfo") |>
  head(n = 21)
covid_mis <- covid_mis[-1,]

misinfo_sites <- covid_mis |>
  mutate(dom_url = reorder(dom_url, n, decreasing = TRUE)) |>
  ggplot(aes(x = dom_url, y = n)) +
  geom_bar(fill="#F8766D", stat = "identity") +
  labs(title = "Misinformation posts with external links", 
       subtitle = "The most linked to external sites among tweets labeled as containing misinformation, top 20 sites", 
       y = "Number of times linked to by a tweet", x = "") +
  theme(axis.text.x = element_text(angle=30, vjust = 0.5, hjust = 0.5))
misinfo_sites 


# Selecting the top 20 non-misinfo sites
covid_nonmis <- covid_sites |>
  filter(label == "non.misinfo") |>
  head(n = 21)
covid_nonmis <- covid_nonmis[-1,]

non_misinfo_sites <- covid_nonmis |>
  mutate(dom_url = reorder(dom_url, n, decreasing = TRUE)) |>
  ggplot(aes(x = dom_url, y = n)) +
  geom_bar(fill="#00BFC4", stat = "identity") +
  labs(title = "Non-misinformation posts with external links", 
       subtitle = "The most linked to external sites among tweets labeled as not containing misinformation, top 20 sites", 
       y = "Number of times linked to by a tweet", x = "") +
  theme(axis.text.x = element_text(angle=30, vjust = 0.5, hjust = 0.5))
non_misinfo_sites 

plots <- ggarrange(non_misinfo_sites, misinfo_sites,
                   ncol = 1, nrow = 2)
plots

# top sites through pandemic
covid_grouped <- covid |>
  count(month, label, dom_url, sort = TRUE)

# want to remove URL shorteners 
covid_grouped <- covid_grouped[!grepl("dlvr.it", covid_grouped$dom_url),]
covid_grouped <- covid_grouped[!grepl("bit.ly", covid_grouped$dom_url),]

covid_mis_grouped <- covid_grouped |>
  filter(label == "misinfo") |>
  drop_na(dom_url)

covid_mis_grouped_cut <- covid_mis_grouped |>
  arrange(desc(n)) |>
  group_by(month) |>
  filter(row_number(month) == 1)

P16 = createPalette(16,  c("#ff0000", "#00ff00", "#0000ff"), M = 10000)
swatch(P16)

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
  filter(label == "non.misinfo") |>
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

# Counting the number of unique sites per group

covid_grouped_mis <- covid_sites |>
  filter(label == "misinfo")
covid_grouped_nonmis <- covid_sites |>
  filter(label == "non.misinfo")