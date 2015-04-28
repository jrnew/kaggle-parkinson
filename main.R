#======================================================================
# main.R
# Jin Rou New, 2015
#======================================================================
# Aims:
# Binary classification to PD vs non-PD classes
# Identification of daily habits via GPS data

rm(list = ls())
setwd("~/Copy/Berkeley/ph290-spring-2015/parkinson")
library(dplyr)
library(ggplot2)
library(scales) # to access breaks/formatting function for dates
library(grid) # required for arrow
library(GGally)
library(ggmap)
data_dir <- "data"
procdata_dir <- "procdata"
fig_dir <- "fig"
users <- c("APPLE", "CHERRY", "CROCUS", "DAFODIL", 
           "DAISY", "FLOX", "IRIS", "LILY",
           "MAPLE", "ORANGE", "ORCHID", "PEONY", "ROSE",
           "SUNFLOWER", "SWEETPEA", "VIOLET")
get_limits <- function(x, factor = 1.05) {
  limits <- quantile(x, probs = c(0.025, 0.975))
  x <- x[x >= limits[1] & x <= limits[2]]
  minx <- min(x, na.rm = TRUE)
  maxx <- max(x, na.rm = TRUE)
  meanx <- mean(x, na.rm = TRUE)
  return(c(meanx - (meanx - minx) * factor,
           meanx + (maxx - meanx) * factor))
}
rms <- function(x, y, z) {
  sqrt((x^2 + y^2 + z^2)/3)
}
percaccuracy <- function(predicted, actual) {
  return(mean(predicted == actual)*100)
}
#----------------------------------------------------------------------
users_file <- file.path(data_dir, "users.csv")
df_users <- read.csv(users_file, stringsAsFactors = FALSE)
#----------------------------------------------------------------------
load(file = file.path(procdata_dir, paste0("df_accel_combinedxyz_hour_all.rda")))
# df_users <- df_users %>% rename(user = Code.Name,
#                     pd_score1 = PD.Score.1,
#                     pd_score2 = PD.Score.2)
# df_accel_combinedxyz_hour_all <- df_accel_combinedxyz_hour_all %>% 
#   left_join(df_users)
# df_accel_combinedxyz_hour_all$user_type <- 
#   ifelse(df_accel_combinedxyz_hour_all$age_diagnosed == "Control",
#          "Control", "PD")

# Data availability by user
table(df_accel_combinedxyz_hour_all$user)
#----------------------------------------------------------------------
# Aggregate by user
df_accel_combinedxyz_hour_all_by_user <- df_accel_combinedxyz_hour_all %>%
  group_by(user) %>%
  summarise(gender = head(gender, 1),
            user_type = head(user_type, 1),
            age = head(age, 1),
            pd_score1 = head(pd_score1, 1),
            pd_score2 = head(pd_score2, 1),
            xyz.mean.sd = sd(xyz.mean),
            xyz.absolute.deviation.sd = sd(xyz.absolute.deviation),
            xyz.standard.deviation.sd = sd(xyz.standard.deviation),
            xyz.max.deviation.sd = sd(xyz.max.deviation),
            xyz.PSD.1.sd = sd(xyz.PSD.1),
            xyz.PSD.3.sd = sd(xyz.PSD.3),
            xyz.PSD.6.sd = sd(xyz.PSD.6),
            xyz.PSD.10.sd = sd(xyz.PSD.10),
            xyz.mean = mean(xyz.mean),
            xyz.absolute.deviation = mean(xyz.absolute.deviation),
            xyz.standard.deviation = mean(xyz.standard.deviation),
            xyz.max.deviation = mean(xyz.max.deviation),
            xyz.PSD.1 = mean(xyz.PSD.1),
            xyz.PSD.3 = mean(xyz.PSD.3),
            xyz.PSD.6 = mean(xyz.PSD.6),
            xyz.PSD.10 = mean(xyz.PSD.10))
# save(df_accel_combinedxyz_hour_all_by_user, 
#      file = file.path(procdata_dir, "df_accel_combinedxyz_hour_all_by_user.rda"))
vars_user <- names(df_accel_combinedxyz_hour_all_by_user)
vars_user <- vars_user[grepl("xyz", vars_user)]
df_accel_combinedxyz_hour_all_by_user$pd_score1nonNA <- 
  ifelse(is.na(df_accel_combinedxyz_hour_all_by_user$pd_score1), 1, 
         df_accel_combinedxyz_hour_all_by_user$pd_score1)
save(df_accel_combinedxyz_hour_all_by_user, 
     file = file.path(procdata_dir, "df_accel_combinedxyz_hour_all_by_user.rda"))
pdf(file.path(fig_dir, "accel-summary-by-user.pdf"))
for (var_user in vars_user) {
  print(var_user)
  p <- ggplot(data = df_accel_combinedxyz_hour_all_by_user,
              aes_string(x = "user_type", y = var_user)) +
    geom_boxplot() + geom_point(aes_string(color = "pd_score1nonNA"))
  print(p)
}
dev.off()
#----------------------------------------------------------------------
# vars <- c("xyz.mean", "xyz.PSD.1", "xyz.PSD.3", "xyz.PSD.6", "xyz.PSD.10")
vars <- names(df_accel_combinedxyz_hour_all)[grepl("xyz", names(df_accel_combinedxyz_hour_all))]
select_cols <- names(df_accel_combinedxyz_hour_all) %in% vars
pdf(file.path(fig_dir, "pairs.pdf"), width = 14)
par(mar = c(1, 1, 1, 1))
pairs(df_accel_combinedxyz_hour_all[, select_cols],
      lower.panel = NULL,
      col = ifelse(df_accel_combinedxyz_hour_all$user_type == "Control", "#0000FF30", "#FF000030"),
      pch = 19)
dev.off()
ggpairs(data = df_accel_combinedxyz_hour_all,
        columns = which(select_cols)[1:2],
        lower = list(continuous = "smooth"),
        colour = "user_type",
        alpha = 50)
#----------------------------------------------------------------------
accel_data <- data.frame(df_accel_combinedxyz_hour_all[, names(df_accel_combinedxyz_hour_all) %in% vars],
                         class = as.factor(df_accel_combinedxyz_hour_all$user_type))

library(MASS)
users_all <- unique(df_accel_combinedxyz_hour_all$user)
prob_qda <- pred_qda <- actual <- rep(NA, length(users_all))
for (i in seq_along(users_all)) {
  indices_user_select <- df_accel_combinedxyz_hour_all$user == users_all[i]
  print(sum(indices_user_select))
  mod <- lda(class ~ ., data = accel_data[-indices_user_select, ])
  prob <- mean(predict(mod, accel_data[indices_user_select, ])$class == "PD")
  pred <- ifelse(prob > 0.5, "PD", "Control")
  prob_qda[i] <- prob
  pred_qda[i] <- pred
  actual[i] <- unique(as.character(accel_data$class[indices_user_select]))
}
percaccuracy(pred_qda, actual)
data.frame(prob_qda, pred_qda, actual)

library(kernlab)
users_all <- unique(df_accel_combinedxyz_hour_all$user)
prob_svm <- pred_svm <- actual <- rep(NA, length(users_all))
for (i in seq_along(users_all)) {
  indices_user_select <- df_accel_combinedxyz_hour_all$user == users_all[i]
  mod <- ksvm(class ~ ., data = accel_data[-indices_user_select, ],
              type = "C-bsvc", kernel = "rbfdot",
              kpar = list(sigma = 0.1), C = 10, prob.model = TRUE)
  prob <- mean(predict(mod, accel_data[indices_user_select, ], 
                       type = "response") == "PD")
  pred <- ifelse(prob > 0.5, "PD", "Control")
  prob_svm[i] <- prob
  pred_svm[i] <- pred
  actual[i] <- unique(as.character(accel_data$class[indices_user_select]))
}
percaccuracy(pred_svm, actual)
data.frame(prob_svm, pred_svm, actual)

library(randomForest)
users_all <- unique(df_accel_combinedxyz_hour_all$user)
prob_rf <- pred_rf <- actual <- rep(NA, length(users_all))
for (i in seq_along(users_all)) {
  indices_user_select <- df_accel_combinedxyz_hour_all$user == users_all[i]
  mod <- randomForest(class ~ ., data = accel_data[-indices_user_select, ], ntree = 1000,
                      importance = TRUE, do.trace = 10)
  prob <- mean(predict(mod, accel_data[indices_user_select, ], 
                       type = "response") == "PD")
  pred <- ifelse(prob > 0.5, "PD", "Control")
  prob_rf[i] <- prob
  pred_rf[i] <- pred
  actual[i] <- unique(as.character(accel_data$class[indices_user_select]))
}
percaccuracy(pred_rf, actual)
data.frame(prob_rf, pred_rf, actual)

# Relative importance (RF)
mod_rf <- randomForest(class ~ ., data = accel_data, ntree = 1000,
                    importance = TRUE, do.trace = 10)
mod_rf_impt <- importance(mod_rf)
# varImpPlot(mod_rf)
mod_rf_impt_df <- data.frame(Variable = dimnames(mod_rf_impt)[[1]], 
                             MeanDecreaseMSE = mod_rf_impt[, 1],
                             MeanDecreaseNodeImpurity = mod_rf_impt[, 2])
mod_rf_impt_df <- mod_rf_impt_df[order(mod_rf_impt_df$MeanDecreaseMSE, decreasing = TRUE), ]
mod_rf_impt_df$Variable <- factor(mod_rf_impt_df$Variable, 
                                  levels = rev(mod_rf_impt_df$Variable))
num_plot <- 20
p <- ggplot(data = head(mod_rf_impt_df, num_plot),
            aes(x = MeanDecreaseMSE, y = Variable)) +
  geom_point(shape = 19, size = 3, col = "blue") +
  xlab("Relative importance") + ylab("") + 
  xlim(range(head(mod_rf_impt_df$MeanDecreaseMSE, num_plot))) +
  theme(plot.background = element_rect(fill = "transparent", colour = NA))
pdf(file.path(fig_dir, "rf-importance.pdf"), height = 7)
print(p)
dev.off()

# pdf(file.path(fig_dir, paste0("accelcombinedxyz_", user, ".pdf")), width = 20, height = 10)
# for (var in vars) {
#   for (i in 1:length(days_list)) {
#     select <- df_accel_combinedxyz$day %in% days_list[[i]]
#     p <- ggplot(data = df_accel_combinedxyz[select, ], 
#                 aes_string(x = "hour", y = var)) +
#       # scale_x_datetime(labels = date_format("%H-%M-%S")
#       #                  , breaks = date_breaks(width = "3 hour")) +
#       scale_y_continuous(limits = c(0, max(quantile(df_accel_combinedxyz[[var]], prob = 0.975), 
#                                            quantile(df_accel_combinedxyz[[var2]], prob = 0.975)))) +
#       geom_line() + 
#       geom_line(data = df_accel_combinedxyz[select, ], 
#                 aes_string(x = "hour", y = var2), color = "blue") +
#       facet_wrap(~date, nrow = 4, ncol = 4)
#     print(p)
#   }
# }
# dev.off()
#----------------------------------------------------------------------
# GPS data: plot GPS coordinates
for (user in users) {
  print(user)
  if (FALSE) {
    gps_file <- paste0(user, "-hdl_gps_*.csv")  
    df_gps <- read.csv(file.path(data_dir, gps_file), stringsAsFactors = FALSE)
    df_gps <- df_gps %>%
      filter(time != "time") %>%
      mutate(diffSecs = as.numeric(diffSecs),
             latitude = as.numeric(latitude),
             longitude = as.numeric(longitude),
             altitude = as.numeric(altitude),
             daytime_orig = as.character(time),
             daytime = as.POSIXct(daytime_orig, format = "%Y-%m-%d %X"),
             day = sapply(daytime_orig, function(x) strsplit(x, " ")[[1]][1]),
             time = sapply(daytime_orig, function(x) strsplit(x, " ")[[1]][2]),
             hour = sapply(time, function(x) strsplit(x, ":")[[1]][1])) %>%
      select(-daytime_orig) %>%
      group_by(daytime, day, time, hour) %>%
      summarise(latitude = mean(latitude),
                longitude = mean(longitude),
                altitude = mean(altitude)) %>%
      arrange(daytime)
    df_gps_start <- df_gps %>%
      group_by(day) %>%
      summarise(latitude = head(latitude, 1),
                longitude = head(longitude, 1))
    df_gps_end <- df_gps %>%
      group_by(day) %>%
      summarise(latitude = tail(latitude, 1),
                longitude = tail(longitude, 1))
    save(df_gps, file = file.path(procdata_dir, paste0("df_gps_", user, ".rda")))
    save(df_gps_start, file = file.path(procdata_dir, paste0("df_gps_start_", user, ".rda")))
    save(df_gps_end, file = file.path(procdata_dir, paste0("df_gps_end_", user, ".rda")))
    # Metadata
    print(user)
    print("dim(data)")
    print(dim(df_gps))
    print("Number of days")
    print(length(unique(df_gps$day)))
    print("----------------------------------------------------------------------")
  }
  
  load(file = file.path(procdata_dir, paste0("df_gps_", user, ".rda")))
  load(file = file.path(procdata_dir, paste0("df_gps_start_", user, ".rda")))
  load(file = file.path(procdata_dir, paste0("df_gps_end_", user, ".rda")))
  
  nrow <- 1
  ncol <- 3
#   days <- unique(df_gps$day)
  days <- c("2012-02-07", "2012-02-08", "2012-02-09")
  ndays <- length(days)
  days_list <- split(days, rep(1:ceiling(ndays/(nrow*ncol)), each = nrow*ncol)[1:ndays])
#   pdf(file.path(fig_dir, paste0("gps_", user, ".pdf")), width = 20, height = 20)
  pdf(file.path(fig_dir, paste0("gps_", user, "1.pdf")), width = 14, height = 3.5)
  for (i in 1:length(days_list)) {
    select <- df_gps$day %in% days_list[[i]]
    select_start <- df_gps_start$day %in% days_list[[i]]
    select_end <- df_gps_end$day %in% days_list[[i]]
    
    p <- ggmap(get_map(location = c(lon = mean(df_gps$longitude),
                                    lat = mean(df_gps$latitude)),
                       maptype = "roadmap", source = "google", zoom = 9)) + 
      scale_x_continuous(limits = get_limits(df_gps$longitude)) +
      scale_y_continuous(limits = get_limits(df_gps$latitude)) + 
      xlab("") + ylab("") + 
      geom_path(data = df_gps[select, ], aes(x = longitude, y = latitude)) + 
      geom_point(data = df_gps_start[select_start, ], 
                 aes(x = longitude, y = latitude), colour = "blue", size = 4) +
      geom_point(data = df_gps_end[select_end, ], 
                 aes(x = longitude, y = latitude), colour = "red") +
      facet_wrap(~day, nrow = 4, ncol = 4)
    print(p)
  }
  dev.off()
  rm(df_gps)
  rm(df_gps_start)
  rm(df_gps_end)
}
# p2 <- ggplot(data = df_gps, aes(x = longitude, y = latitude)) + 
#   geom_path() + 
#   geom_point(data = df_gps_start, colour = "blue", size = 3) +
#   geom_point(data = df_gps_end, colour = "red") +
#   facet_wrap(~day)
# print(p2)
