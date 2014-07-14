source("Rstart.R")

date_threshold = "2014-07-03"
color_before = "#2980b9"
color_after = "#c0392b"

# Not run: code for parsing and saving Shown HN submissions out of all submissions
#
# data <- tbl_df(read.csv("hacker_news_stories.csv", header=T))
# data <- data %>% filter(grepl("Show HN:", title))
# write.csv(data,"show_hn.csv",row.names=F)

data <- tbl_df(read.csv("show_hn.csv", header=T))
data <- data %>% mutate(created_at = as.Date(created_at))

### Daily aggregation

data_daily_all <- data %>%
  filter(created_at >= "2011-01-01") %>%
  group_by(created_at) %>%
  summarize(num_submissions = n(),
            avg_points = mean(points),
            avg_comments = mean(num_comments),
            perc_front_page = sum(points >= 10) / num_submissions) %>%
  mutate(feature = ifelse(created_at < date_threshold,"Before Show HN","After Show HN"),
         moving_avg_submissions = filter(num_submissions,rep(1/30,30), sides=1),
         moving_avg_points = filter(avg_points,rep(1/30,30), sides=1),
         moving_avg_comments = filter(avg_comments,rep(1/30,30), sides=1),
         moving_avg_perc_front = filter(perc_front_page,rep(1/30,30), sides=1)
  )

data_daily_all$feature <- factor(data_daily_all$feature, levels=c("Before Show HN", "After Show HN"))

data_daily_end <- data_daily_all %>%
  filter(created_at > (as.Date(date_threshold) - 53))

data_agg <- data_daily_end %>%
  select(feature, num_submissions, avg_points, avg_comments, perc_front_page) %>%
  group_by(feature) %>%
  summarize(mean_submissions = mean(num_submissions),
            mean_points = mean(avg_points),
            mean_comments = mean(avg_comments),
            mean_front = mean(perc_front_page)
  )

###
### Summary time series charts
###

second_color = colors[1]

ggplot(aes(x=as.POSIXct(created_at), y=moving_avg_submissions), data=data_daily_all) +
  geom_line(size=0.5, color=second_color) +
  geom_area(alpha=0.5, fill=second_color) +
  geom_smooth(method="loess", color="#1a1a1a", alpha=0) + 
  theme_custom() +
  scale_x_datetime(labels = date_format("%b %Y")) +
  theme(axis.title.y = element_text(color=second_color),
        axis.title.x = element_text(color=second_color)) +
  labs(title="# of Daily Show HN Submissions to Hacker News", x="Date", y="# Show HN Submissions (30-Day Moving Avg)")

ggsave("show-hn-submissions.png", dpi=300, width=4, height=3)

second_color = colors[2]

ggplot(aes(x=as.POSIXct(created_at), y=moving_avg_points), data=data_daily_all) +
  geom_line(size=0.5, color=second_color) +
  geom_area(alpha=0.5, fill=second_color) +
  geom_smooth(method="loess", color="#1a1a1a", alpha=0) + 
  theme_custom() +
  scale_x_datetime(labels = date_format("%b %Y")) +
  theme(axis.title.y = element_text(color=second_color),
        axis.title.x = element_text(color=second_color)) +
  labs(title="Avg # Points by Day For Show HN Submissions to Hacker News", x="Date", y="Avg # Pts. for Show HN Submissions (30-Day Moving Avg)")

ggsave("show-hn-points.png", dpi=300, width=4, height=3)

second_color = colors[5]

ggplot(aes(x=as.POSIXct(created_at), y=moving_avg_comments), data=data_daily_all) +
  geom_line(size=0.5, color=second_color) +
  geom_area(alpha=0.5, fill=second_color) +
  geom_smooth(method="loess", color="#1a1a1a", alpha=0) + 
  theme_custom() +
  scale_x_datetime(labels = date_format("%b %Y")) +
  theme(axis.title.y = element_text(color=second_color),
        axis.title.x = element_text(color=second_color)) +
  labs(title="Average # Comments by Day For Show HN Submissions to Hacker News", x="Date", y="Avg # Cmnts. for Show HN Submissions (30-Day Moving Avg)")

ggsave("show-hn-comments.png", dpi=300, width=4, height=3)

second_color = colors[4]

ggplot(aes(x=as.POSIXct(created_at), y=moving_avg_perc_front), data=data_daily_all) +
  geom_line(size=0.5, color=second_color) +
  geom_area(alpha=0.5, fill=second_color) +
  geom_smooth(method="loess", color="#1a1a1a", alpha=0) + 
  theme_custom() +
  scale_x_datetime(labels = date_format("%b %Y")) +
  scale_y_continuous(labels = percent) +
  theme(axis.title.y = element_text(color=second_color),
        axis.title.x = element_text(color=second_color)) +
  labs(title="Front Page % by Day For Show HN Submissions to Hacker News", x="Date", y="% Show HN Submissions w/ >=10 pts. (30-Day Moving Avg)")

ggsave("show-hn-perc-front.png", dpi=300, width=4, height=3)

###
### Before/After Show HN
###

ggplot(aes(x=created_at, y=num_submissions, fill=feature, color=feature), data=data_daily_end) +
  geom_line(size=0.5) +
  geom_area(alpha=0.5, color="transparent") +
  geom_hline(yint=data_agg[2,2], color = color_before, alpha=0.25, linetype="dashed") +
  geom_hline(yint=data_agg[1,2], color = color_after, alpha=0.25, linetype="dashed") +
  theme_custom() +
  theme(legend.position="top", legend.direction="horizontal", legend.key.size=unit(.25, "cm"), legend.margin=unit(-0.5,"cm")) +
  scale_fill_manual(values=rev(c(color_before, color_after))) + 
  scale_color_manual(values=rev(c(color_before, color_after))) + 
  labs(title="# Daily Submissions Before/After Addition of Show HN Feature", x="Date", y="# of Show HN Submissions", fill="", color="")

ggsave("show-hn-end-submissions.png", dpi=300, width=4, height=3)

ggplot(aes(x=created_at, y=avg_points, fill=feature, color=feature), data=data_daily_end) +
  geom_line(size=0.5) +
  geom_area(alpha=0.5, color="transparent") +
  geom_hline(yint=data_agg[2,3], color = color_before, alpha=0.25, linetype="dashed") +
  geom_hline(yint=data_agg[1,3], color = color_after, alpha=0.25, linetype="dashed") +
  theme_custom() +
  theme(legend.position="top", legend.direction="horizontal", legend.key.size=unit(.25, "cm"), legend.margin=unit(-0.5,"cm")) +
  scale_fill_manual(values=rev(c(color_before, color_after))) + 
  scale_color_manual(values=rev(c(color_before, color_after))) + 
  labs(title="Avg Points per Day Before/After Addition of Show HN Feature", x="Date", y="Avg # Points for Show HN Submissions", fill="", color="")

ggsave("show-hn-end-points.png", dpi=300, width=4, height=3)

ggplot(aes(x=created_at, y=avg_comments, fill=feature, color=feature), data=data_daily_end) +
  geom_line(size=0.5) +
  geom_area(alpha=0.5, color="transparent") +
  geom_hline(yint=data_agg[2,4], color = color_before, alpha=0.25, linetype="dashed") +
  geom_hline(yint=data_agg[1,4], color = color_after, alpha=0.25, linetype="dashed") +
  theme_custom() +
  theme(legend.position="top", legend.direction="horizontal", legend.key.size=unit(.25, "cm"), legend.margin=unit(-0.5,"cm")) +
  scale_fill_manual(values=rev(c(color_before, color_after))) + 
  scale_color_manual(values=rev(c(color_before, color_after))) + 
  labs(title="Avg Comments per Day Before/After Addition of Show HN Feature", x="Date", y="Avg # Comments for Show HN Submissions", fill="", color="")

ggsave("show-hn-end-comments.png", dpi=300, width=4, height=3)

ggplot(aes(x=created_at, y=perc_front_page, fill=feature, color=feature), data=data_daily_end) +
  geom_line(size=0.5) +
  geom_area(alpha=0.5, color="transparent") +
  geom_hline(yint=data_agg[2,5], color = color_before, alpha=0.25, linetype="dashed") +
  geom_hline(yint=data_agg[1,5], color = color_after, alpha=0.25, linetype="dashed") +
  theme_custom() +
  theme(legend.position="top", legend.direction="horizontal", legend.key.size=unit(.25, "cm"), legend.margin=unit(-0.5,"cm")) +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values=rev(c(color_before, color_after))) + 
  scale_color_manual(values=rev(c(color_before, color_after))) + 
  labs(title="Front Page % of Show HN Before/After Addition of Show HN Feature", x="Date", y="% Show HN Submissions w/ >=10 pts.", fill="", color="")

ggsave("show-hn-end-perc-front.png", dpi=300, width=4, height=3)

