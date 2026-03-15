library(RedditExtractoR)
library(dplyr)
library(purrr)
library(tibble)
library(readr)
library(stringr)
library(lubridate)
library(janitor)
library(tidyr)
library(broom)
library(ggplot2)
library(forcats)
library(vader)
library(MASS)


options(stringsAsFactors = FALSE)
-
# 1================Settings and functinos

subreddits <- c("depression", "ptsd", "anxiety","SuicideWatch","relationship_advice")

# If this doesnt work im gonna scream
n_threads_per_sub <- 200

sort_mode <- "top"
sort_period <- "year"
sleep_sec <- 3


`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

empty_url_df <- function() {
  tibble(
    url = character(),
    query_subreddit = character(),
    query_sort = character(),
    query_period = character()
  )
}

safe_find_thread_urls <- function(subreddit, sort_by = "top", period = "year", max_tries = 4) {
  for (i in seq_len(max_tries)) {
    
    out <- tryCatch(
      {
        find_thread_urls(
          subreddit = subreddit,
          sort_by   = sort_by,
          period    = period
        )
      },
      error = function(e) NULL,
      warning = function(w) {
        message(sprintf("Warning for r/%s: %s", subreddit, conditionMessage(w)))
        invokeRestart("muffleWarning")
      }
    )
    
    if (!is.null(out) && nrow(out) > 0) {
      out <- janitor::clean_names(out)
      
      if ("url" %in% names(out)) {
        return(
          out %>%
            mutate(
              query_subreddit = subreddit,
              query_sort      = sort_by,
              query_period    = period
            )
        )
      }
    }
    
    Sys.sleep(sleep_sec * i)
  }
  
  empty_url_df()
}

pick_col <- function(df, candidates, default = NA) {
  hit <- candidates[candidates %in% names(df)][1]
  if (length(hit) == 0 || is.na(hit)) {
    rep(default, nrow(df))
  } else {
    df[[hit]]
  }
}


# 3============= Collect thread

thread_urls <- map_dfr(subreddits, function(sr) {
  Sys.sleep(sleep_sec)
  
  safe_find_thread_urls(
    subreddit = sr,
    sort_by   = sort_mode,
    period    = sort_period,
    max_tries = 4
  ) %>%
    slice_head(n = n_threads_per_sub)
})

thread_urls <- thread_urls %>%
  distinct(url, .keep_all = TRUE)

write_csv(thread_urls, "01_thread_urls.csv")

# -------------------------
analysis_threads <- tibble(
  url           = as.character(pick_col(thread_urls, c("url"), NA_character_)),
  subreddit     = as.character(pick_col(thread_urls, c("subreddit", "query_subreddit"), NA_character_)),
  title         = as.character(pick_col(thread_urls, c("title"), NA_character_)),
  date_raw      = pick_col(thread_urls, c("date", "timestamp", "created_utc", "created"), NA),
  comments_n    = suppressWarnings(as.numeric(pick_col(thread_urls, c("comments", "num_comments"), NA_real_))),
  query_sort    = as.character(pick_col(thread_urls, c("query_sort"), sort_mode)),
  text = as.character(pick_col(thread_urls, c("text"), NA_character_)),
  query_period  = as.character(pick_col(thread_urls, c("query_period"), sort_period))
) %>%
  mutate(
    created_datetime = suppressWarnings(as_datetime(date_raw, tz = "UTC")),
    weekday_utc = wday(created_datetime, label = TRUE, week_start = 1),
    hour_utc = hour(created_datetime),
    time_bin = case_when(
      hour_utc >= 0  & hour_utc < 6  ~ "night",
      hour_utc >= 6  & hour_utc < 12 ~ "morning",
      hour_utc >= 12 & hour_utc < 18 ~ "afternoon",
      hour_utc >= 18 & hour_utc <= 23 ~ "evening",
      TRUE ~ NA_character_
    )
  )

write_csv(analysis_threads, "02_analysis_threads.csv")

analysis_threads <- analysis_threads[,-1] 



dep_df <- analysis_threads %>%
  filter(subreddit == "depression") %>%
  filter(!is.na(comments_n), !is.na(weekday_utc), !is.na(time_bin)) %>%
  mutate(
    weekday_utc = factor(
      weekday_utc,
      levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
    ),
    time_bin = factor(
      time_bin,
      levels = c("night", "morning", "afternoon", "evening")
    )
  )


weekday_summary_dep <- dep_df %>%
  group_by(weekday_utc) %>%
  summarise(
    mean_comments = mean(comments_n, na.rm = TRUE),
    median_comments = median(comments_n, na.rm = TRUE),
    n_posts = n(),
    .groups = "drop"
  )


ggplot(weekday_summary_dep, aes(x = weekday_utc, y = mean_comments)) +
  geom_col(fill = "steelblue", width = 0.75) +
  geom_text(
    aes(label = round(mean_comments, 1)),
    vjust = -0.4,
    size = 4.2
  ) +
  scale_y_continuous(
    breaks = seq(0, 70, by = 5),
    limits = c(0, max(weekday_summary_dep$mean_comments) + 5)
  ) +
  labs(
    title = "Comments by day in r/depression",
    x = "day",
    y = "Mean number of comments"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_line(color = "grey90"),
    panel.grid.major.x = element_blank()
  )


timebin_summary_dep <- dep_df %>%
  group_by(time_bin) %>%
  summarise(
    mean_comments = mean(comments_n, na.rm = TRUE),
    median_comments = median(comments_n, na.rm = TRUE),
    n_posts = n(),
    .groups = "drop"
  )


ggplot(timebin_summary_dep, aes(x = time_bin, y = mean_comments)) +
  geom_col(fill = "tomato3", width = 0.75) +
  geom_text(
    aes(label = round(mean_comments, 1)),
    vjust = -0.4,
    size = 4.2,
    fontface = "bold"
  ) +
  scale_y_continuous(
    breaks = seq(0, 70, by = 5),
    limits = c(0, max(timebin_summary_dep$mean_comments) + 5)
  ) +
  labs(
    title = "Comments by Time Interval in r/depression",
    x = "Time interval",
    y = "Mean number of comments"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.8),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_line(color = "grey90"),
    panel.grid.major.x = element_blank()
  )

postcount_timebin_dep <- dep_df %>%
  count(time_bin)

hour_summary_dep <- dep_df %>%
  group_by(hour_utc) %>%
  summarise(
    mean_comments = mean(comments_n, na.rm = TRUE),
    median_comments = median(comments_n, na.rm = TRUE),
    n_posts = n(),
    .groups = "drop"
  )


ggplot(hour_summary_dep, aes(x = hour_utc, y = mean_comments)) +
  geom_line(linewidth = 1.2, color = "darkorange2") +
  geom_point(size = 3, color = "darkorange3") +
  scale_x_continuous(
    breaks = seq(0, 23, by = 2),
    limits = c(0, 23)
  ) +
  scale_y_continuous(
    breaks = seq(0, max(hour_summary_dep$mean_comments, na.rm = TRUE) + 5, by = 5)
  ) +
  labs(
    title = "Comment Count by Posting Hour in r/depression",
    x = "Hour of day (UTC)",
    y = "Mean number of comments"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text( hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0),
    panel.grid.minor = element_line(color = "grey90")
  )


#===============ANOVA


anova_raw_results <- analysis_threads %>%
  filter(!is.na(comments_n), !is.na(weekday_utc), !is.na(subreddit)) %>%
  group_by(subreddit) %>%
  group_split() %>%
  setNames(unique(analysis_threads$subreddit[!is.na(analysis_threads$subreddit)])) %>%
  map(~ aov(comments_n ~ weekday_utc, data = .x))

# View ANOVA summaries
map(anova_raw_results, summary)


#=====================SHAPIRO
shapiro_results <- analysis_threads %>%
  filter(!is.na(comments_n), !is.na(weekday_utc), !is.na(subreddit)) %>%
  group_by(subreddit) %>%
  group_split() %>%
  setNames(unique(analysis_threads$subreddit[!is.na(analysis_threads$subreddit)])) %>%
  map(~ shapiro.test(residuals(aov(comments_n ~ weekday_utc, data = .x))))

shapiro_results



bartlett_results <- analysis_threads %>%
  filter(!is.na(comments_n), !is.na(weekday_utc), !is.na(subreddit)) %>%
  group_by(subreddit) %>%
  group_split() %>%
  setNames(unique(analysis_threads$subreddit[!is.na(analysis_threads$subreddit)])) %>%
  map(~ bartlett.test(comments_n ~ weekday_utc, data = .x))

bartlett_results


#============================KRUSKAL

kruskal_results <- analysis_threads %>%
  filter(!is.na(comments_n), !is.na(weekday_utc), !is.na(subreddit)) %>%
  group_by(subreddit) %>%
  group_modify(~ {
    test <- kruskal.test(comments_n ~ weekday_utc, data = .x)
    broom::tidy(test)
  }) %>%
  ungroup()

kruskal_results

#------------- time bin

kruskal_timebin_results <- analysis_threads %>%
  filter(!is.na(comments_n), !is.na(time_bin), !is.na(subreddit)) %>%
  group_by(subreddit) %>%
  group_modify(~ broom::tidy(kruskal.test(comments_n ~ time_bin, data = .x))) %>%
  ungroup()

kruskal_timebin_results

plot_timebin_all <- analysis_threads %>%
  filter(!is.na(comments_n), !is.na(time_bin), !is.na(subreddit)) %>%
  mutate(
    time_bin = factor(
      time_bin,
      levels = c("night", "morning", "afternoon", "evening")
    )
  )

ggplot(plot_timebin_all, aes(x = time_bin, y = comments_n, fill = time_bin)) +
  geom_boxplot(alpha = 0.85, outlier.alpha = 0.3) +
  facet_wrap(~ subreddit) +
  scale_y_log10(
    breaks = c(1, 2, 5, 10, 20, 50, 100, 200, 500)
  ) +
  labs(
    title = "Comment Count Distribution by Time Interval",
    x = "Time interval",
    y = "Number of comments (log scale)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 1),
    axis.title = element_text(face = "bold"),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_line(color = "grey90"),
    strip.text = element_text(face = "bold")
  )


ptsd_df <- analysis_threads %>%
  filter(subreddit == "ptsd", !is.na(comments_n), !is.na(time_bin))

pairwise.wilcox.test(
  x = ptsd_df$comments_n,
  g = ptsd_df$time_bin,
  p.adjust.method = "BH",
  exact = FALSE
)
#=========================================HOUR


kruskal_hour_results <- analysis_threads %>%
  filter(!is.na(comments_n), !is.na(hour_utc), !is.na(subreddit)) %>%
  group_by(subreddit) %>%
  group_modify(~ broom::tidy(kruskal.test(comments_n ~ factor(hour_utc), data = .x))) %>%
  ungroup()

kruskal_hour_results


#=============Machine Learning=================


sent_df <- vader_df(analysis_threads$text)

analysis_sent <- bind_cols(
  analysis_threads,
  sent_df %>% select(compound, pos, neu, neg)
) %>%
  rename(
    sentiment = compound,
    sentiment_pos = pos,
    sentiment_neu = neu,
    sentiment_neg = neg
  )



analysis_sent <- bind_cols(
  analysis_threads,
  sent_df %>% select(compound, pos, neu, neg)
) %>%
  rename(
    sentiment = compound,
    sentiment_pos = pos,
    sentiment_neu = neu,
    sentiment_neg = neg
  )

analysis_sent %>%
  group_by(weekday_utc, time_bin) %>%
  summarise(
    mean_comments = mean(comments_n, na.rm = TRUE),
    mean_sentiment = mean(sentiment, na.rm = TRUE),
    n_posts = n(),
    .groups = "drop"
  )


ggplot(analysis_sent, aes(x = sentiment, y = comments_n)) +
  geom_point(alpha = 0.5) +
  scale_y_log10() +
  labs(
    title = "Comment Count and Sentiment",
    x = "Sentiment score",
    y = "Comment count (log scale)"
  ) +
  theme_minimal()


analysis_sent$weekday_utc <- factor(
  as.character(analysis_sent$weekday_utc),
  levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
  ordered = FALSE
)
contrasts(analysis_sent$weekday_utc) <- contr.treatment(7, base = 1)

analysis_sent$time_bin <- factor(
  as.character(analysis_sent$time_bin),
  levels = c("afternoon", "evening", "morning", "night"),
  ordered = FALSE
)

m_nb_sent2 <- glm.nb(
  comments_n ~ weekday_utc + time_bin + sentiment,
  data = analysis_sent
)

summary(m_nb_sent2)
exp(coef(m_nb_sent2))






