library(readr)
library(dplyr)
library(ggplot2)
library(magrittr)
library(ggcorrplot)

# data exploration and summary statistics

deepseek_vs_chatgpt <- read_csv("deepseek_vs_chatgpt.csv")
View(deepseek_vs_chatgpt)

# Check missing values
missing_values <- colSums(is.na(deepseek_vs_chatgpt))
print(missing_values)

# convert to dataframe:
dvc <- as.data.frame(deepseek_vs_chatgpt)

# impute with mean and mode
dvc <- dvc %>%
    mutate(
        across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))
    ) %>%
    mutate(
        across(where(is.character), ~ ifelse(is.na(.), mode(.[!is.na(.)]), .))
    )


# Create one-hot encoding (alternative to integer encoding)
dvc$query_type_general <- as.integer(dvc$Query_Type == "General")
dvc$query_type_technical <- as.integer(dvc$Query_Type == "Technical")

# encode labels in Query Type
# "General"   "Technical"
dvc$Query_Type <- ifelse(dvc$Query_Type == "General", 0,
    ifelse(dvc$Query_Type == "Technical", 1, NA)
)


dvc$Device_Type <- ifelse(dvc$Device_Type == "Mobile", 0,
    ifelse(dvc$Device_Type == "Laptop/Desktop", 1,
        ifelse(dvc$Device_Type == "Tablet", 2,
            ifelse(dvc$Device_Type == "Smart Speaker", 3, NA)
        )
    )
)

dvc$Response_Time_Category <- ifelse(dvc$Response_Time_Category == "Standard", 0,
    ifelse(dvc$Response_Time_Category == "Fast", 1,
        ifelse(dvc$Response_Time_Category == "Instant", 2, NA)
    )
)

for (col in names(dvc)[sapply(dvc, is.numeric)]) {
    if (!grepl("query_type_|device_type_|response_time_", col) &&
        !col %in% c("Query_Type", "Device_Type", "Response_Time_Category")) {
        dvc[[paste0(col, "_scaled")]] <- scale(dvc[[col]])
    }
}

colnames(dvc) <- gsub(" ", "_", tolower(colnames(dvc)))

na_after <- colSums(is.na(dvc))
print(na_after)

# Export cleaned dataframe to CSV
write_csv(dvc, "cleaned_deepseek_vs_chatgpt.csv")

summary(dvc)

# Histogram of user ratings
ggplot(dvc, aes(x = user_rating)) +
    geom_histogram(binwidth = 0.5, fill = "blue", alpha = 0.7) +
    theme_minimal()

# Density plot of session durations
ggplot(dvc, aes(x = session_duration_sec)) +
    geom_density(fill = "red", alpha = 0.5) +
    theme_minimal()

# Group by AI platform and summarize key metrics
dvc %>%
    group_by(ai_platform) %>%
    summarise(
        avg_rating = mean(user_rating, na.rm = TRUE),
        avg_response_accuracy = mean(response_accuracy, na.rm = TRUE),
        avg_session_duration = mean(session_duration_sec, na.rm = TRUE)
    )

# AI Platform Comparison (DeepSeek vs. ChatGPT)

# Boxplot of user ratings by platform
ggplot(dvc, aes(x = ai_platform, y = user_rating, fill = ai_platform)) +
    geom_boxplot() +
    theme_minimal() +
    labs(title = "User Ratings by AI Platform")+
  scale_fill_manual(values = c("DeepSeek" = "#ffb31c", "ChatGPT" = "#ff1c5c"))

# Boxplot of response accuracy
ggplot(dvc, aes(x = ai_platform, y = response_accuracy, fill = ai_platform)) +
    geom_boxplot() +
    theme_minimal() +
    labs(title = "Response Accuracy by AI Platform") +
  scale_fill_manual(values = c("DeepSeek" = "#ffb31c", "ChatGPT" = "#ff1c5c"))

ggplot(dvc, aes(x = ai_platform, y = session_duration_sec, fill = ai_platform)) +
    geom_boxplot() +
    theme_minimal() +
    labs(title = "Session Duration by AI Platform")+
  scale_fill_manual(values = c("DeepSeek" = "#ffb31c", "ChatGPT" = "#ff1c5c"))

# Check normality (Shapiro-Wilk Test)
set.seed(0)

# For DeepSeek

deepseek_data <- dvc %>% filter(ai_platform == "DeepSeek")
deepseek_sample <- sample(deepseek_data$user_rating, min(5000, nrow(deepseek_data)))
shapiro_deepseek <- shapiro.test(deepseek_sample)
print(shapiro_deepseek)

# For ChatGPT
chatgpt_data <- dvc %>% filter(ai_platform == "ChatGPT")
chatgpt_sample <- sample(chatgpt_data$user_rating, min(5000, nrow(chatgpt_data)))
shapiro_chatgpt <- shapiro.test(chatgpt_sample)
print(shapiro_chatgpt)

ggplot(dvc, aes(sample = user_rating)) +
    stat_qq() +
    stat_qq_line() +
    facet_wrap(~ai_platform)

# If normal <U+2192> Use t-test, else <U+2192> Use Wilcoxon test
wilcox.test(user_rating ~ ai_platform, data = dvc)
# difference in user ratings across platforms is signficant

wilcox.test(session_duration_sec ~ ai_platform, data = dvc)

wilcox.test(response_accuracy ~ ai_platform, data = dvc)

cor_test <- cor(dvc %>% select(user_rating, response_accuracy, response_speed_sec, correction_needed), use = "complete.obs")
cor_test
ggcorrplot(cor_test, method = "circle")

# Churn Analysis

dvc$date <- as.Date(dvc$date)

# Weekly aggregation
weekly_churn <- dvc %>%
    mutate(week = lubridate::floor_date(date, "week")) %>%
    group_by(week, ai_platform) %>%
    summarise(avg_churn_rate = mean(daily_churn_rate, na.rm = TRUE)) %>%
    ungroup()

# Monthly aggregation
monthly_churn <- dvc %>%
    mutate(month = lubridate::floor_date(date, "month")) %>%
    group_by(month, ai_platform) %>%
    summarise(avg_churn_rate = mean(daily_churn_rate, na.rm = TRUE)) %>%
    ungroup()

# Plot weekly churn
ggplot(weekly_churn, aes(x = week, y = avg_churn_rate, color = ai_platform)) +
    geom_line() +
    geom_point() +
    theme_minimal() +
    labs(title = "Weekly Average Churn Rate", x = "Week", y = "Churn Rate")

# Plot monthly churn
# Go with monthly
ggplot(monthly_churn, aes(x = month, y = avg_churn_rate, color = ai_platform)) +
    geom_line() +
    geom_point() +
    theme_minimal() +
    labs(title = "Monthly Average Churn Rate", x = "Month", y = "Churn Rate")+
  scale_color_manual(values = c("DeepSeek" = "#ffb31c", "ChatGPT" = "#ff1c5c"))


ggplot(dvc, aes(x = weekday, y = daily_churn_rate, fill = weekday)) +
    geom_boxplot() +
    theme_minimal() +
    labs(title = "Churn Rate by Weekday")

# Factors Influencing Churn (Correlation & Group Analysis)
cor_test <- cor(dvc %>% select(daily_churn_rate, retention_rate, session_duration_sec, user_experience_score), use = "complete.obs")
cor_test
ggcorrplot(cor_test, method = "circle")

# Bin churned users into 3 categories
dvc <- dvc %>%
    mutate(churn_bin = cut(churned_users,
        breaks = quantile(churned_users, probs = seq(0, 1, length.out = 4), na.rm = TRUE),
        labels = c("Low", "Medium", "High"),
        include.lowest = TRUE
    ))

# Calculate averages by churn bin
churn_summary <- dvc %>%
    group_by(churn_bin) %>%
    summarise(
        avg_session_duration = mean(session_duration_sec_scaled, na.rm = TRUE),
        avg_user_experience = mean(user_experience_score_scaled, na.rm = TRUE),
        avg_correction_needed = mean(correction_needed_scaled, na.rm = TRUE)
    )

# Print the summary table
print(churn_summary, width=Inf)

# Convert to long format for plotting
library(tidyr)
churn_summary_long <- churn_summary %>%
    pivot_longer(
        cols = starts_with("avg_"),
        names_to = "metric",
        values_to = "value"
    )

# Create line chart
ggplot(churn_summary_long, aes(x = churn_bin, y = value, group = metric, color = metric)) +
    geom_line(linewidth = 2) +
    geom_point(size = 3) +
    theme_minimal() +
    labs(
        title = "Metrics by Churn Level",
        x = "Churn Level",
        y = "Average Value"
    ) +
    scale_color_manual(
        values = c("#120582", "#db3c07", "#dbb807"),
        labels = c("Session Duration", "User Experience", "Corrections Needed")
    )

# Churn Prediction (Logistic Regression)
dvc <- dvc %>%
    mutate(churn_flag = ifelse(churned_users > 0, 1, 0))

# Fit logistic regression
churn_model <- glm(churn_flag ~ session_duration_sec + user_experience_score + correction_needed,
    data = dvc, family = binomial
)

summary(churn_model)

# Time Series
library(lubridate)

ggplot(dvc, aes(x = date, y = response_accuracy, color = ai_platform)) +
    geom_line() +
    theme_minimal() +
    labs(title = "Active Users Over Time")

# Weekly aggregation
weekly_response_accuracy <- dvc %>%
    mutate(week = lubridate::floor_date(date, "week")) %>%
    group_by(week, ai_platform) %>%
    summarise(avg_response_accuracy = mean(response_accuracy, na.rm = TRUE)) %>%
    ungroup()

ggplot(weekly_response_accuracy, aes(x = week, y = avg_response_accuracy, color = ai_platform)) +
    geom_line() +
    theme_minimal() +
    labs(title = "Weekly Response Accuracy Over Time", x = "Week", y = "Average Response Accuracy")

# Monthly aggregation
monthly_response_accuracy <- dvc %>%
    mutate(month = lubridate::floor_date(date, "month")) %>%
    group_by(month, ai_platform) %>%
    summarise(avg_response_accuracy = mean(response_accuracy, na.rm = TRUE)) %>%
    ungroup()

ggplot(monthly_response_accuracy, aes(x = month, y = avg_response_accuracy, color = ai_platform)) +
    geom_line(linewidth=0.8) +
    theme_minimal() +
    labs(
        title = "Monthly Response Accuracy Over Time",
        x = "Month",
        y = "Average Response Accuracy"
    )+
  scale_color_manual(values = c("DeepSeek" = "#ffb31c", "ChatGPT" = "#ff1c5c"))

accuracy_ts_c <- monthly_response_accuracy %>%
    filter(ai_platform == "ChatGPT") %>%
    select(month, avg_response_accuracy) %>%
    arrange(month)

# Forecasting ChatGPT's response accuracy

accuracy_ts_c <- ts(
    accuracy_ts_c$avg_response_accuracy,
    start = c(
        year(min(accuracy_ts_c$month)),
        month(min(accuracy_ts_c$month))
    ),
    frequency = 12
)

library(tseries)
adf_test_c <- adf.test(accuracy_ts_c)
print(adf_test_c)

accuracy_ts_c_diff <- diff(accuracy_ts_c, differences = 4)

adf_test_c_diff <- adf.test(accuracy_ts_c_diff)
print(adf_test_c_diff)

library(prophet)

accuracy_c_prophet <- monthly_response_accuracy %>%
    filter(ai_platform == "ChatGPT") %>%
    select(month, avg_response_accuracy) %>%
    rename(ds = month, y = avg_response_accuracy)

c_model <- prophet()
c_model <- fit.prophet(c_model, accuracy_c_prophet)

c_future <- make_future_dataframe(c_model, periods = 12, freq = "month")

c_forecast <- predict(c_model, c_future)

plot(c_model, c_forecast) +
    labs(title = "Forecasted Response Accuracy (ChatGPT)")


prophet_plot_components(c_model, c_forecast)

# Forecasting Deepseek's response accuracy

accuracy_ts_d <- monthly_response_accuracy %>%
    filter(ai_platform == "DeepSeek") %>%
    select(month, avg_response_accuracy) %>%
    arrange(month)

accuracy_ts_d <- ts(
    accuracy_ts_d$avg_response_accuracy,
    start = c(
        year(min(accuracy_ts_d$month)),
        month(min(accuracy_ts_d$month))
    ),
    frequency = 12
)

library(tseries)
adf_test_d <- adf.test(accuracy_ts_d)
print(adf_test_d)

accuracy_ts_d_diff <- diff(accuracy_ts_d, differences = 3)

adf_test_d_diff <- adf.test(accuracy_ts_d_diff)
print(adf_test_d_diff)

library(prophet)

accuracy_d_prophet <- monthly_response_accuracy %>%
    filter(ai_platform == "DeepSeek") %>%
    select(month, avg_response_accuracy) %>%
    rename(ds = month, y = avg_response_accuracy)

d_model <- prophet()
d_model <- fit.prophet(d_model, accuracy_d_prophet)

d_future <- make_future_dataframe(d_model, periods = 12, freq = "month")

d_forecast <- predict(d_model, d_future)

plot(d_model, d_forecast) +
    labs(title = "Forecasted Response Accuracy (DeepSeek)")

prophet_plot_components(d_model, d_forecast)
