library(scales)
library(ggplot2)
library(eurostat)
library(readxl)
library(dplyr)
library(readr)
library(tibble)
library(tidyr)
library(stringr)
library(purrr)
library(lubridate)
library(writexl)
library(glmnet) 
library(Metrics)
library(patchwork)
library(viridis)



# Funkcija ketvirčiams
generate_quarters <- function(start_quarter, n) {
  year <- as.integer(substr(start_quarter, 1, 4))
  quarter <- as.integer(substr(start_quarter, 6, 6))
  quarters <- character(n)
  for (i in 1:n) {
    quarter <- quarter + 1
    if (quarter > 4) {
      quarter <- 1
      year <- year + 1
    }
    quarters[i] <- paste0(year, "K", quarter)
  }
  return(quarters)
}


# Funkcija klaidoms skaičiuoti
errors_by_h <- function(df) {
  lapply(1:forecast_horizon, function(h) {
    subset(df, Horizon == h)$Forecast - subset(df, Horizon == h)$Real
  })
}

#------------------------Darbas su jau stacionariu final table-------------------------

stationary_table <- read_excel("C:/Users/Admin/Desktop/Kursinis 2025/stationary_table_long.xlsx")
colnames(stationary_table)<-make.names(colnames(stationary_table))

###########################################################################################################
###########################################################################################################

#------------------------Mutual information-------------------------


library(infotheo)

calculate_mutual_information_for_all_columns <- function(data) {
  mi_results <- data.frame(Variable = character(), MI = numeric(), stringsAsFactors = FALSE)
  
  for (col_name in colnames(data)[-c(1, 2)]) {
    column <- data[[col_name]]
    
    mi_value <- mutinformation(discretize(as.factor(data$BVP)), discretize(as.factor(column)))
    
    mi_results <- rbind(mi_results, data.frame(Variable = col_name, MI = mi_value))
  }
  
  return(mi_results)
}

mi_results <- calculate_mutual_information_for_all_columns(stationary_table)
length(mi_results)
#------------------------TOP 100 Mutual information-------------------------

top_100_variables <- mi_results[order(mi_results$MI, decreasing = TRUE), ][1:100, ]
View(top_100_variables)

write_xlsx(top_100_variables, "C:\\Users\\Admin\\Desktop\\Kursinis 2025\\top_100_filtered_mutual_information.xlsx")

###########################################################################################################
##########################################################################################################
###########################################################################################################
###########################################################################################################
###########################################################################################################
###########################################################################################################
      'lambda_n' <- function(model, N = 5){
        require(magrittr)
        non_optimal_lambda <- 
          sapply(
            model$lambda, 
            function(x){
              predict(model, type = 'nonzero', s = x) %>% nrow %>% {.[. > 1]} %>% {abs(.-N)}
            }) %>% which.min
        model$lambda[non_optimal_lambda]
      }
      #lambda_n(cvob1, N = 5)
################################################################################################################################
######################################################################LASSO, LM ir Post-LASSO 2021-2027###################################
library(tidyfit)
library(pbapply)
library(forecast)
library(zoo)
library(glmnet)

first_two_columns<-stationary_table[,1:2]
#lango dydis
window_size <- 78
forecast_horizon <- 9 #prognozes

target_time <- "2024K4"
max_index <- which(stationary_table$Time == target_time)

#visa pradine lentele
X <- as.matrix(stationary_table[, -c(1, 2)])
#X <- X[, !colnames(X) %in% 'Assets_Financial.Transactions_Net.financial.transactions...worth._General.Government..Non.Consolidated.']

#colnames(X) <- make.names(colnames(X))
ncol(X)
#X
#bvp
y <- as.numeric(unlist(stationary_table[, 2]))


set.seed(123)

final_forecasts_lm_min <- list()
final_forecasts_post_lasso_min <- list()
final_forecasts_lm_1se <- list()
final_forecasts_post_lasso_1se <- list()
final_support_vars_min <- list()
final_arima_forecasts_min <- list()
final_support_vars_1se <- list()
final_arima_forecasts_1se <- list()

for (i in 1:(max_index - window_size  + 1)) {
  #print(i)
  X_window <- X[i:(i + window_size - 1), ]  #lango dydzio x'ai
  y_window <- y[i:(i + window_size - 1)]   #lango dydzio y'ai
  #print(y_window)
  lasso_model <- cv.glmnet(X_window, y_window, family = "gaussian", alpha = 1)
  #plot(lasso_model)
  best_lambda <- lasso_model$lambda.min
  #best_lambda=lasso_model$lambda[30]
  #best_lambda
  se_lambda <- lasso_model$lambda.1se
  
  #lambda min
  coef_lasso_min <- coef(lasso_model, s = "lambda.min")
  support_vars_min <- which(coef_lasso_min[-1,1] != 0) # be intercept
  
  #print(support_vars_min)
  #cor(y_window, X_window[, support_vars_min])
  
  #lambda_1se
  coef_lasso_1se <- coef(lasso_model, s = "lambda.1se")
  support_vars_1se <- which(coef_lasso_1se[-1, 1] != 0) #visi 0??
  forecasts <- list()
  #su lambda min
  if (length(support_vars_min) > 0) {
    #kintamieji su lambda min
    X_window_selected_min <- X_window[, support_vars_min, drop = FALSE]
    #print(X_window_selected_min)
    
    #apjungiam kintamuosius su y
    data_window <- as.data.frame(cbind(y = y_window, X_window_selected_min))
    #apmokom lm
    lm_model<-lm(y ~ ., data = data_window)
    selected_vars <- colnames(X_window_selected_min)
    #apmokom lasso
    if (ncol(X_window_selected_min) >= 2) {
      model_lasso <- cv.glmnet(X_window_selected_min, y_window, alpha = 1)
      #plot(model_lasso)
    }
    else{
      coef_lasso_min <- coef(lasso_model, s =  lambda_n(lasso_model, N = 10))
      support_vars_min <- which(coef_lasso_min[-1,1] != 0) # be intercept
      X_window_selected_min <- X_window[, support_vars_min, drop = FALSE]
      model_lasso <- cv.glmnet(X_window_selected_min, y_window, alpha = 1)
      #plot(model_lasso)
    }
    selected_var_names <- colnames(X_window_selected_min) #su lambda min
    #print(selected_var_names)
    #arima
    arima_forecasts <- list()
    for (j in 1:ncol(X_window_selected_min)) {
      model <- auto.arima(X_window_selected_min[, j])
      arima_forecasts[[j]] <- as.numeric(forecast(model, h = forecast_horizon)$mean)
    }
    if (length(arima_forecasts) > 0) {
      arima_forecasts_matrix <- do.call(rbind, arima_forecasts)
      arima_forecasts_df <- as.data.frame(t(arima_forecasts_matrix))
      colnames(arima_forecasts_df) <- selected_var_names
      
      #prognoze lm
      arima_forecasts_df_selected <- arima_forecasts_df[, selected_var_names, drop = FALSE]
      #print(arima_forecasts_df_selected)
      predictions_lm <- predict(lm_model, newdata = arima_forecasts_df_selected)
      #prognoze post lasso
      predictions_post_lasso <- predict(model_lasso, newx = as.matrix(arima_forecasts_df_selected), s="lambda.min")
      #print(predictions_post_lasso)
      #surenkame rezultatus
      if (exists("predictions_lm")) {
        final_forecasts_lm_min[[i]] <- predictions_lm
      }
      if (exists("predictions_post_lasso")) {
        final_forecasts_post_lasso_min[[i]] <- as.numeric(predictions_post_lasso)
      }
      if (exists("support_vars_min")) {
        final_support_vars_min[[i]] <- support_vars_min
      }
      if (exists("arima_forecasts_df")) {
        final_arima_forecasts_min[[i]] <- arima_forecasts_df
      }
      
    }
  }
  #su lambda 1se
  # if (length(support_vars_1se) > 0) {
  #   #kintamieji su lambda 1se
  #   X_window_selected_1se <- X_window[, support_vars_1se, drop = FALSE]
  #   #apjungiam kintamuosius su y
  #   data_window <- as.data.frame(cbind(y = y_window, X_window_selected_1se))
  #   #apmokom lm
  #   lm_model<-lm(y ~ ., data = data_window)
  #   selected_vars <- colnames(X_window_selected_1se)
  # 
  #   #apmokom lasso
  #   if (ncol(X_window_selected_1se) >= 2) {
  #   model_lasso <- cv.glmnet(X_window_selected_1se, y_window, alpha = 1)}
  #   selected_var_names <- colnames(X_window_selected_1se) #su lambda min
  #   #arima
  #   arima_forecasts <- list()
  # 
  #   for (j in 1:ncol(X_window_selected_1se)) {
  #     model <- auto.arima(X_window_selected_1se[, j])
  #     arima_forecasts[[j]] <- as.numeric(forecast(model, h = forecast_horizon)$mean)
  #   }
  #   if (length(arima_forecasts) > 0) {
  #     arima_forecasts_matrix <- do.call(rbind, arima_forecasts)
  #     arima_forecasts_df <- as.data.frame(t(arima_forecasts_matrix))
  #     colnames(arima_forecasts_df) <- selected_var_names
  # 
  #     #prognoze lm
  #     arima_forecasts_df_selected <- arima_forecasts_df[, selected_vars, drop = FALSE]
  #     predictions_lm <- predict(lm_model, newdata = arima_forecasts_df_selected)
  #     #prognoze post lasso
  #     predictions_post_lasso <- predict(model_lasso, newx = as.matrix(arima_forecasts_df_selected))
  #     #surenkame rezultatus
  #     if (exists("predictions_lm")) {
  #       final_forecasts_lm_1se[[i]] <- predictions_lm
  #     }
  #     if (exists("predictions_post_lasso")) {
  #       final_forecasts_post_lasso_1se[[i]] <- as.numeric(predictions_post_lasso)
  #     }
  #     if (exists("support_vars_1se")) {
  #       final_support_vars_1se[[i]] <- support_vars_1se
  #     }
  #     if (exists("arima_forecasts_df")) {
  #       final_arima_forecasts_1se[[i]] <- arima_forecasts_df
  #     }
  # 
  #   }
  # }
}

print(final_forecasts_lm_min)
print(final_forecasts_post_lasso_min)
print(final_forecasts_lm_1se)
print(final_forecasts_post_lasso_1se)
print(final_support_vars_min)
print(final_arima_forecasts_min)
print(final_support_vars_1se)
print(final_arima_forecasts_1se)


#final_forecasts_lm_min[1]
#names(final_arima_forecasts_min)


valid_lm <- which((window_size + seq_along(final_forecasts_lm_min) + forecast_horizon - 1) <= 300)
valid_post <- which((window_size + seq_along(final_forecasts_post_lasso_min) + forecast_horizon - 1) <= 300)

max_time_index <- max(
  window_size + valid_lm + forecast_horizon - 1,
  window_size + valid_post + forecast_horizon - 1
)

#kiek eilučių reikia
n_needed <- max_time_index - nrow(first_two_columns)

#jei reikia pridėti eilutes
if (n_needed > 0) {
  last_time <- first_two_columns$Time[nrow(first_two_columns)]
  new_times <- generate_quarters(last_time, n_needed)
  new_rows <- as.data.frame(matrix(NA, nrow = n_needed, ncol = ncol(first_two_columns)))
  colnames(new_rows) <- colnames(first_two_columns)
  new_rows$Time <- new_times
  first_two_columns <- rbind(first_two_columns, new_rows)
}

# LM. kiekvienam langui kuriam df, kuriame yra laikas, prognoze, lango numeris
all_lm_forecasts_shifted <- do.call(rbind, lapply(valid_lm, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_lm_min[[i]],
    Type = "LM",
    Window = i
  )
}))
# Post LASSO. kiekvienam langui kuriam df, kuriame yra laikas, prognoze, lango numeris
all_post_lasso_forecasts_shifted <- do.call(rbind, lapply(valid_post, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_post_lasso_min[[i]],
    Type = "Post-LASSO",
    Window = i
  )
}))

#apjungiam
forecast_shifted_df_lm <-all_lm_forecasts_shifted
forecast_shifted_df_postl<-all_post_lasso_forecasts_shifted
#realus bvp su df
real_bvp_df <- data.frame(
  Time = first_two_columns$Time,
  Real = first_two_columns$BVP
)

#apkerpam laika
real_bvp_df_trimmed <- real_bvp_df[(window_size + 1):nrow(real_bvp_df), ]

#LM grafikas
ggplot() +
  geom_line(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), linewidth = 1.2) +
  geom_point(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), size = 2) +
  
  geom_line(data = subset(forecast_shifted_df_lm, Type == "LM"),
            aes(x = Time, y = Forecast, group = Window, color = factor(Window)),
            alpha = 0.8, linewidth = 1.1) +
  
  labs(
    title = "BVP tiesinio modelio prognozių palyginimas",
    x = "Ketvirtis", y = "BVP", color = "Langas arba realybė"
  ) +
  scale_color_manual(
    values = c("Reali reikšmė" = "darkgreen")
  ) +
  scale_color_viridis_d(option = "plasma", begin = 0, end = 0.85, direction = -1, name = "Langas") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )

library(Metrics)

lm_merged <- merge(forecast_shifted_df_lm, real_bvp_df_trimmed, by = "Time")
lm_merged <- lm_merged[!is.na(lm_merged$Real),]
rmse_lm_total <- rmse(lm_merged$Real, lm_merged$Forecast)
print(paste("RMSE (LM, total):", round(rmse_lm_total, 4)))


#post LASSO grafikas
ggplot() +
  geom_line(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), linewidth = 1.2) +
  geom_point(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), size = 2) +
  
  geom_line(data = subset(forecast_shifted_df_postl, Type == "Post-LASSO"),
            aes(x = Time, y = Forecast, group = Window, color = factor(Window)),
            alpha = 0.8, linewidth = 1.1) +
  
  labs(
    title = "BVP prognozių palyginimas",
    x = "Ketvirtis", y = "BVP", color = "Langas arba realybė"
  ) +
  scale_color_manual(
    values = c("Reali reikšmė" = "darkgreen"),
    guide = "none"
  ) +
  scale_color_viridis_d(option = "plasma", begin = 0, end = 0.85, direction = -1, name = "Langas") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

post_merged <- merge(forecast_shifted_df_postl, real_bvp_df_trimmed, by = "Time")
post_merged <- post_merged[!is.na(post_merged$Real),]
rmse_post_total <- rmse(post_merged$Real, post_merged$Forecast)
print(paste("RMSE (Post-LASSO, total):", round(rmse_post_total, 4)))


forecast_shifted_df_lm$Horizon <- rep(1:forecast_horizon, times = length(unique(forecast_shifted_df_lm$Window)))
forecast_shifted_df_lm
forecast_shifted_df_lm <- do.call(rbind, lapply(valid_lm, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_lm_min[[i]],
    Horizon = 1:forecast_horizon,
    Window = i
  )
}))

library(viridis)

num_windows <- length(unique(forecast_shifted_df_lm$Horizon))

blue_shades <- viridis(num_windows, option = "plasma", begin = 0, end = 0.9, direction = -1)

k4 <- ggplot() +
  geom_line(data = real_bvp_df_trimmed,
            aes(x = Time, y = Real, color = "Realybė", group = 1),
            size = 1.2) +
  geom_point(data = real_bvp_df_trimmed,
             aes(x = Time, y = Real, color = "Realybė")) +
  
  geom_line(data = forecast_shifted_df_lm,
            aes(x = Time, y = Forecast, group = Horizon, color = as.factor(Horizon)),
            size = 1.2, linetype = "dashed") +
  geom_point(data = forecast_shifted_df_lm,
             aes(x = Time, y = Forecast, color = as.factor(Horizon))) +
  
  scale_color_manual(
    values = c(setNames(blue_shades, as.character(unique(forecast_shifted_df_lm$Horizon))),
               "Realybė" = "red"),
    guide = guide_legend(override.aes = list(linetype = c(rep("dashed", num_windows), "solid"),
                                             size = c(rep(1.2, num_windows), 1.2)), title = "Horizontai")
  ) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )+
  labs(title = "BVP prognozės (Tiesinė regresija) ir realybė pagal horizontus",
       x = "Kvartalas",
       y = "BVP",
       color = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold")) 

ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs27.png", plot = k4, width = 10, height = 6, dpi = 300, bg = "white")


forecast_shifted_df_lm <- merge(forecast_shifted_df_lm, real_bvp_df_trimmed, by = "Time")
forecast_shifted_df_lm <- forecast_shifted_df_lm[!is.na(forecast_shifted_df_lm$Real),]
rmse_lm_by_horizon <- aggregate(cbind(Forecast, Real) ~ Horizon, data = forecast_shifted_df_lm, FUN = function(x) x)
rmse_lm_by_horizon$RMSE <- sapply(unique(forecast_shifted_df_lm$Horizon), function(h) {
  rmse_vals <- forecast_shifted_df_lm[forecast_shifted_df_lm$Horizon == h, ]
  rmse(rmse_vals$Real, rmse_vals$Forecast)
})
print(rmse_lm_by_horizon[, c("Horizon", "RMSE")])


err_lm_pl_21_27 <- errors_by_h(forecast_shifted_df_lm)




forecast_shifted_df_post <- do.call(rbind, lapply(valid_post, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_post_lasso_min[[i]],
    Horizon = 1:forecast_horizon,
    Window = i
  )
}))
forecast_shifted_df_post


k5 <- ggplot() +
  geom_line(data = real_bvp_df_trimmed,
            aes(x = Time, y = Real, color = "Realybė", group = 1),
            size = 1.2) +
  geom_point(data = real_bvp_df_trimmed,
             aes(x = Time, y = Real, color = "Realybė")) +
  
  geom_line(data = forecast_shifted_df_post,
            aes(x = Time, y = Forecast, group = Horizon, color = as.factor(Horizon)),
            size = 1.2, linetype = "dashed") +
  geom_point(data = forecast_shifted_df_post,
             aes(x = Time, y = Forecast, color = as.factor(Horizon))) +
  
  scale_color_manual(
    values = c(setNames(blue_shades, as.character(unique(forecast_shifted_df_post$Horizon))),
               "Realybė" = "red"),
    guide = guide_legend(override.aes = list(linetype = c(rep("dashed", num_windows), "solid"),
                                             size = c(rep(1.2, num_windows), 1.2)), title = "Horizontai")
  ) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )+
  labs(title = "BVP prognozės (Post-LASSO) ir realybė pagal horizontus",
       x = "Kvartalas",
       y = "BVP",
       color = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold")) 

ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs28.png", plot = k5, width = 10, height = 6, dpi = 300, bg = "white")




forecast_shifted_df_post <- merge(forecast_shifted_df_post, real_bvp_df_trimmed, by = "Time")
forecast_shifted_df_post <- forecast_shifted_df_post[!is.na(forecast_shifted_df_post$Real),]
rmse_post_by_horizon <- aggregate(cbind(Forecast, Real) ~ Horizon, data = forecast_shifted_df_post, FUN = function(x) x)
rmse_post_by_horizon$RMSE <- sapply(unique(forecast_shifted_df_post$Horizon), function(h) {
  rmse_vals <- forecast_shifted_df_post[forecast_shifted_df_post$Horizon == h, ]
  rmse(rmse_vals$Real, rmse_vals$Forecast)
})
print(rmse_post_by_horizon[, c("Horizon", "RMSE")])


err_las_pl_21_27 <- errors_by_h(forecast_shifted_df_post)



rmse_plot_df <- data.frame(
  Horizon = rep(1:forecast_horizon, 2),
  RMSE = c(rmse_lm_by_horizon$RMSE, rmse_post_by_horizon$RMSE),
  Model = rep(c("LM", "Post-LASSO"), each = forecast_horizon)
)



lm3 <- ggplot(rmse_plot_df, aes(x = Horizon, y = RMSE, colour = Model)) +
  geom_line(size = 1.3, alpha = 0.9) +
  geom_point(size = 3) +
  scale_color_manual(values = c("LM" = "red","Post-LASSO" = "blue"), name = "")+
  scale_x_continuous(labels = number_format(accuracy = 1)) + 
  labs(
    title = "2021K1-2024K4",
    x = "Horizontas",
    y = "RMSE",
    color = "Modelis"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )


####################################################2019k2-2021k2############################################################
library(tidyfit)
library(pbapply)
library(forecast)
library(zoo)
library(glmnet)
#lango dydis
window_size <- 79
forecast_horizon <- 8 #prognozes

target_time <- "2021K2"
max_index <- which(stationary_table$Time == target_time)

#visa pradine lentele
X <- as.matrix(stationary_table[, -c(1, 2)])
#X <- X[, !colnames(X) %in% 'Assets_Financial.Transactions_Net.financial.transactions...worth._General.Government..Non.Consolidated.']

#colnames(X) <- make.names(colnames(X))
ncol(X)
#X
#bvp
y <- as.numeric(unlist(stationary_table[, 2]))


set.seed(123)

final_forecasts_lm_min <- list()
final_forecasts_post_lasso_min <- list()
final_forecasts_lm_1se <- list()
final_forecasts_post_lasso_1se <- list()
final_support_vars_min <- list()
final_arima_forecasts_min <- list()
final_support_vars_1se <- list()
final_arima_forecasts_1se <- list()

for (i in 1:(max_index - window_size  + 1)) {
  #print(i)
  X_window <- X[i:(i + window_size - 1), ]  #lango dydzio x'ai
  y_window <- y[i:(i + window_size - 1)]   #lango dydzio y'ai
  #print(y_window)
  lasso_model <- cv.glmnet(X_window, y_window, family = "gaussian", alpha = 1)
  #plot(lasso_model)
  best_lambda <- lasso_model$lambda.min
  #best_lambda=lasso_model$lambda[30]
  #best_lambda
  se_lambda <- lasso_model$lambda.1se
  
  #lambda min
  coef_lasso_min <- coef(lasso_model, s = "lambda.min")
  support_vars_min <- which(coef_lasso_min[-1,1] != 0) # be intercept
  
  #print(support_vars_min)
  #cor(y_window, X_window[, support_vars_min])
  
  #lambda_1se
  coef_lasso_1se <- coef(lasso_model, s = "lambda.1se")
  support_vars_1se <- which(coef_lasso_1se[-1, 1] != 0) #visi 0??
  forecasts <- list()
  #su lambda min
  if (length(support_vars_min) > 0) {
    #kintamieji su lambda min
    X_window_selected_min <- X_window[, support_vars_min, drop = FALSE]
    #print(X_window_selected_min)
    
    #apjungiam kintamuosius su y
    data_window <- as.data.frame(cbind(y = y_window, X_window_selected_min))
    #apmokom lm
    lm_model<-lm(y ~ ., data = data_window)
    selected_vars <- colnames(X_window_selected_min)
    #apmokom lasso
    if (ncol(X_window_selected_min) >= 2) {
      model_lasso <- cv.glmnet(X_window_selected_min, y_window, alpha = 1)
      #plot(model_lasso)
    }
    else{
      coef_lasso_min <- coef(lasso_model, s =  lambda_n(lasso_model, N = 20))
      support_vars_min <- which(coef_lasso_min[-1,1] != 0) # be intercept
      X_window_selected_min <- X_window[, support_vars_min, drop = FALSE]
      model_lasso <- cv.glmnet(X_window_selected_min, y_window, alpha = 1)
      #plot(model_lasso)
    }
    selected_var_names <- colnames(X_window_selected_min) #su lambda min
    #print(selected_var_names)
    #arima
    arima_forecasts <- list()
    for (j in 1:ncol(X_window_selected_min)) {
      model <- auto.arima(X_window_selected_min[, j])
      arima_forecasts[[j]] <- as.numeric(forecast(model, h = forecast_horizon)$mean)
    }
    if (length(arima_forecasts) > 0) {
      arima_forecasts_matrix <- do.call(rbind, arima_forecasts)
      arima_forecasts_df <- as.data.frame(t(arima_forecasts_matrix))
      colnames(arima_forecasts_df) <- selected_var_names
      
      #prognoze lm
      arima_forecasts_df_selected <- arima_forecasts_df[, selected_var_names, drop = FALSE]
      #print(arima_forecasts_df_selected)
      predictions_lm <- predict(lm_model, newdata = arima_forecasts_df_selected)
      #prognoze post lasso
      predictions_post_lasso <- predict(model_lasso, newx = as.matrix(arima_forecasts_df_selected), s="lambda.min")
      #print(predictions_post_lasso)
      #surenkame rezultatus
      if (exists("predictions_lm")) {
        final_forecasts_lm_min[[i]] <- predictions_lm
      }
      if (exists("predictions_post_lasso")) {
        final_forecasts_post_lasso_min[[i]] <- as.numeric(predictions_post_lasso)
      }
      if (exists("support_vars_min")) {
        final_support_vars_min[[i]] <- support_vars_min
      }
      if (exists("arima_forecasts_df")) {
        final_arima_forecasts_min[[i]] <- arima_forecasts_df
      }
      
    }
  }
}

print(final_forecasts_lm_min)
print(final_forecasts_post_lasso_min)
print(final_support_vars_min)
print(final_arima_forecasts_min)


#final_forecasts_lm_min[1]
#names(final_arima_forecasts_min)


valid_lm <- which((window_size + seq_along(final_forecasts_lm_min) + forecast_horizon - 1) <= nrow(first_two_columns))
valid_post <- which((window_size + seq_along(final_forecasts_post_lasso_min) + forecast_horizon - 1) <= nrow(first_two_columns))

max_time_index <- max(
  window_size + valid_lm + forecast_horizon - 1,
  window_size + valid_post + forecast_horizon - 1
)

#kiek eilučių reikia
n_needed <- max_time_index - nrow(first_two_columns)

#jei reikia pridėti eilutes
if (n_needed > 0) {
  last_time <- first_two_columns$Time[nrow(first_two_columns)]
  new_times <- generate_quarters(last_time, n_needed)
  new_rows <- as.data.frame(matrix(NA, nrow = n_needed, ncol = ncol(first_two_columns)))
  colnames(new_rows) <- colnames(first_two_columns)
  new_rows$Time <- new_times
  first_two_columns <- rbind(first_two_columns, new_rows)
}

# LM. kiekvienam langui kuriam df, kuriame yra laikas, prognoze, lango numeris
all_lm_forecasts_shifted <- do.call(rbind, lapply(valid_lm, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_lm_min[[i]],
    Type = "LM",
    Window = i
  )
}))
# Post LASSO. kiekvienam langui kuriam df, kuriame yra laikas, prognoze, lango numeris
all_post_lasso_forecasts_shifted <- do.call(rbind, lapply(valid_post, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_post_lasso_min[[i]],
    Type = "Post-LASSO",
    Window = i
  )
}))

#apjungiam
forecast_shifted_df_lm <-all_lm_forecasts_shifted
View(forecast_shifted_df_lm)
forecast_shifted_df_postl<-all_post_lasso_forecasts_shifted
View(forecast_shifted_df_postl)

#realus bvp su df
# real_bvp_df <- data.frame(
#   Time = first_two_columns$Time,
#   Real = y
# )
#apkerpam laika
start_period <- "2019K2"
end_period <- "2023K2"

real_bvp_df_trimmed <- real_bvp_df[real_bvp_df$Time >= start_period & real_bvp_df$Time <= end_period, ]
View(real_bvp_df_trimmed)


#LM grafikas
ggplot() +
  geom_line(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), linewidth = 1.2) +
  geom_point(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), size = 2) +
  
  geom_line(data = subset(forecast_shifted_df_lm, Type == "LM"),
            aes(x = Time, y = Forecast, group = Window, color = factor(Window)),
            alpha = 0.8, linewidth = 1.1) +
  
  labs(
    title = "BVP tiesinio modelio prognozių palyginimas",
    x = "Ketvirtis", y = "BVP", color = "Langas arba realybė"
  ) +
  scale_color_manual(
    values = c("Reali reikšmė" = "darkgreen")
  ) +
  scale_color_viridis_d(option = "plasma", begin = 0, end = 0.85, direction = -1, name = "Langas") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )

library(Metrics)  # для rmse()

lm_merged <- merge(forecast_shifted_df_lm, real_bvp_df_trimmed, by = "Time")
rmse_lm_total <- rmse(lm_merged$Real, lm_merged$Forecast)
print(paste("RMSE (LM, total):", round(rmse_lm_total, 4)))


#post LASSO grafikas
ggplot() +
  geom_line(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), linewidth = 1.2) +
  geom_point(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), size = 2) +
  
  geom_line(data = subset(forecast_shifted_df_postl, Type == "Post-LASSO"),
            aes(x = Time, y = Forecast, group = Window, color = factor(Window)),
            alpha = 0.8, linewidth = 1.1) +
  
  labs(
    title = "BVP prognozių palyginimas",
    x = "Ketvirtis", y = "BVP", color = "Langas arba realybė"
  ) +
  scale_color_manual(
    values = c("Reali reikšmė" = "darkgreen"),
    guide = "none"
  ) +
  scale_color_viridis_d(option = "plasma", begin = 0, end = 0.85, direction = -1, name = "Langas") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

post_merged <- merge(forecast_shifted_df_postl, real_bvp_df_trimmed, by = "Time")

rmse_post_total <- rmse(post_merged$Real, post_merged$Forecast)
print(paste("RMSE (Post-LASSO, total):", round(rmse_post_total, 4)))


forecast_shifted_df_lm$Horizon <- rep(1:forecast_horizon, times = length(unique(forecast_shifted_df_lm$Window)))
forecast_shifted_df_lm
forecast_shifted_df_lm <- do.call(rbind, lapply(valid_lm, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_lm_min[[i]],
    Horizon = 1:forecast_horizon,
    Window = i
  )
}))

library(viridis)

num_windows <- length(unique(forecast_shifted_df_lm$Horizon))

blue_shades <- viridis(num_windows, option = "plasma", begin = 0, end = 0.9, direction = -1)

k6 <- ggplot() +
  geom_line(data = real_bvp_df_trimmed,
            aes(x = Time, y = Real, color = "Realybė", group = 1),
            size = 1.2) +
  geom_point(data = real_bvp_df_trimmed,
             aes(x = Time, y = Real, color = "Realybė")) +
  
  geom_line(data = forecast_shifted_df_lm,
            aes(x = Time, y = Forecast, group = Horizon, color = as.factor(Horizon)),
            size = 1.2, linetype = "dashed") +
  geom_point(data = forecast_shifted_df_lm,
             aes(x = Time, y = Forecast, color = as.factor(Horizon))) +
  
  scale_color_manual(
    values = c(setNames(blue_shades, as.character(unique(forecast_shifted_df_lm$Horizon))),
               "Realybė" = "red"),
    guide = guide_legend(override.aes = list(linetype = c(rep("dashed", num_windows), "solid"),
                                             size = c(rep(1.2, num_windows), 1.2)), title = "Horizontai")
  ) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )+
  labs(title = "BVP prognozės (Tiesinė regresija) ir realybė pagal horizontus",
       x = "Kvartalas",
       y = "BVP",
       color = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold")) 

ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs24.png", plot = k6, width = 10, height = 6, dpi = 300, bg = "white")


forecast_shifted_df_lm <- merge(forecast_shifted_df_lm, real_bvp_df_trimmed, by = "Time")
rmse_lm_by_horizon <- aggregate(cbind(Forecast, Real) ~ Horizon, data = forecast_shifted_df_lm, FUN = function(x) x)
rmse_lm_by_horizon$RMSE <- sapply(unique(forecast_shifted_df_lm$Horizon), function(h) {
  rmse_vals <- forecast_shifted_df_lm[forecast_shifted_df_lm$Horizon == h, ]
  rmse(rmse_vals$Real, rmse_vals$Forecast)
})
print(rmse_lm_by_horizon[, c("Horizon", "RMSE")])

err_lm_pl_19_23 <- errors_by_h(forecast_shifted_df_lm)


forecast_shifted_df_post <- do.call(rbind, lapply(valid_post, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_post_lasso_min[[i]],
    Horizon = 1:forecast_horizon,
    Window = i
  )
}))
forecast_shifted_df_post


k7 <- ggplot() +
  geom_line(data = real_bvp_df_trimmed,
            aes(x = Time, y = Real, color = "Realybė", group = 1),
            size = 1.2) +
  geom_point(data = real_bvp_df_trimmed,
             aes(x = Time, y = Real, color = "Realybė")) +
  
  geom_line(data = forecast_shifted_df_post,
            aes(x = Time, y = Forecast, group = Horizon, color = as.factor(Horizon)),
            size = 1.2, linetype = "dashed") +
  geom_point(data = forecast_shifted_df_post,
             aes(x = Time, y = Forecast, color = as.factor(Horizon))) +
  
  scale_color_manual(
    values = c(setNames(blue_shades, as.character(unique(forecast_shifted_df_post$Horizon))),
               "Realybė" = "red"),
    guide = guide_legend(override.aes = list(linetype = c(rep("dashed", num_windows), "solid"),
                                             size = c(rep(1.2, num_windows), 1.2)), title = "Horizontai")
  ) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )+
  labs(title = "BVP prognozės (Post-LASSO) ir realybė pagal horizontus",
       x = "Kvartalas",
       y = "BVP",
       color = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold")) 

ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs25.png", plot = k7, width = 10, height = 6, dpi = 300, bg = "white")



forecast_shifted_df_post <- merge(forecast_shifted_df_post, real_bvp_df_trimmed, by = "Time")
rmse_post_by_horizon <- aggregate(cbind(Forecast, Real) ~ Horizon, data = forecast_shifted_df_post, FUN = function(x) x)
rmse_post_by_horizon$RMSE <- sapply(unique(forecast_shifted_df_post$Horizon), function(h) {
  rmse_vals <- forecast_shifted_df_post[forecast_shifted_df_post$Horizon == h, ]
  rmse(rmse_vals$Real, rmse_vals$Forecast)
})
print(rmse_post_by_horizon[, c("Horizon", "RMSE")])

err_las_pl_19_23 <- errors_by_h(forecast_shifted_df_post)


rmse_plot_df <- data.frame(
  Horizon = rep(1:forecast_horizon, 2),
  RMSE = c(rmse_lm_by_horizon$RMSE, rmse_post_by_horizon$RMSE),
  Model = rep(c("LM", "Post-LASSO"), each = forecast_horizon)
)



lm2 <- ggplot(rmse_plot_df, aes(x = Horizon, y = RMSE, colour = Model)) +
  geom_line(size = 1.3, alpha = 0.9) +
  geom_point(size = 3) +
  scale_color_manual(values = c("LM" = "red","Post-LASSO" = "blue"), name = "")+
  scale_x_continuous(labels = number_format(accuracy = 1)) + 
  labs(
    title = "2019K2-2023K2",
    x = "Horizontas",
    y = "RMSE",
    color = "Modelis"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )


#################################################################################################################
#################################################################2017-2019########################################
#lango dydis
window_size <- 71
forecast_horizon <- 8 #prognozes

target_time <- "2019K2"
max_index <- which(stationary_table$Time == target_time)

#visa pradine lentele
X <- as.matrix(stationary_table[, -c(1, 2)])
#X <- X[, !colnames(X) %in% 'Assets_Financial.Transactions_Net.financial.transactions...worth._General.Government..Non.Consolidated.']

#colnames(X) <- make.names(colnames(X))
ncol(X)
#X
#bvp
y <- as.numeric(unlist(stationary_table[, 2]))


set.seed(123)

final_forecasts_lm_min <- list()
final_forecasts_post_lasso_min <- list()
final_forecasts_lm_1se <- list()
final_forecasts_post_lasso_1se <- list()
final_support_vars_min <- list()
final_arima_forecasts_min <- list()
final_support_vars_1se <- list()
final_arima_forecasts_1se <- list()

for (i in 1:(max_index - window_size  + 1)) {
  #print(i)
  X_window <- X[i:(i + window_size - 1), ]  #lango dydzio x'ai
  y_window <- y[i:(i + window_size - 1)]   #lango dydzio y'ai
  #print(y_window)
  lasso_model <- cv.glmnet(X_window, y_window, family = "gaussian", alpha = 1)
  #plot(lasso_model)
  best_lambda <- lasso_model$lambda.min
  #best_lambda=lasso_model$lambda[30]
  #best_lambda
  se_lambda <- lasso_model$lambda.1se
  
  #lambda min
  coef_lasso_min <- coef(lasso_model, s = "lambda.min")
  support_vars_min <- which(coef_lasso_min[-1,1] != 0) # be intercept
  
  #print(support_vars_min)
  #cor(y_window, X_window[, support_vars_min])
  
  #lambda_1se
  coef_lasso_1se <- coef(lasso_model, s = "lambda.1se")
  support_vars_1se <- which(coef_lasso_1se[-1, 1] != 0) #visi 0??
  forecasts <- list()
  #su lambda min
  if (length(support_vars_min) > 0) {
    #kintamieji su lambda min
    X_window_selected_min <- X_window[, support_vars_min, drop = FALSE]
    #print(X_window_selected_min)
    
    #apjungiam kintamuosius su y
    data_window <- as.data.frame(cbind(y = y_window, X_window_selected_min))
    #apmokom lm
    lm_model<-lm(y ~ ., data = data_window)
    selected_vars <- colnames(X_window_selected_min)
    #apmokom lasso
    if (ncol(X_window_selected_min) >= 2) {
      model_lasso <- cv.glmnet(X_window_selected_min, y_window, alpha = 1)
      #plot(model_lasso)
    }
    else{
      coef_lasso_min <- coef(lasso_model, s =  lambda_n(lasso_model, N = 10))
      support_vars_min <- which(coef_lasso_min[-1,1] != 0) # be intercept
      X_window_selected_min <- X_window[, support_vars_min, drop = FALSE]
      model_lasso <- cv.glmnet(X_window_selected_min, y_window, alpha = 1)
      #plot(model_lasso)
    }
    selected_var_names <- colnames(X_window_selected_min) #su lambda min
    #print(selected_var_names)
    #arima
    arima_forecasts <- list()
    for (j in 1:ncol(X_window_selected_min)) {
      model <- auto.arima(X_window_selected_min[, j])
      arima_forecasts[[j]] <- as.numeric(forecast(model, h = forecast_horizon)$mean)
    }
    if (length(arima_forecasts) > 0) {
      arima_forecasts_matrix <- do.call(rbind, arima_forecasts)
      arima_forecasts_df <- as.data.frame(t(arima_forecasts_matrix))
      colnames(arima_forecasts_df) <- selected_var_names
      
      #prognoze lm
      arima_forecasts_df_selected <- arima_forecasts_df[, selected_var_names, drop = FALSE]
      #print(arima_forecasts_df_selected)
      predictions_lm <- predict(lm_model, newdata = arima_forecasts_df_selected)
      #prognoze post lasso
      predictions_post_lasso <- predict(model_lasso, newx = as.matrix(arima_forecasts_df_selected), s="lambda.min")
      #print(predictions_post_lasso)
      #surenkame rezultatus
      if (exists("predictions_lm")) {
        final_forecasts_lm_min[[i]] <- predictions_lm
      }
      if (exists("predictions_post_lasso")) {
        final_forecasts_post_lasso_min[[i]] <- as.numeric(predictions_post_lasso)
      }
      if (exists("support_vars_min")) {
        final_support_vars_min[[i]] <- support_vars_min
      }
      if (exists("arima_forecasts_df")) {
        final_arima_forecasts_min[[i]] <- arima_forecasts_df
      }
      
    }
  }
}

print(final_forecasts_lm_min)
print(final_forecasts_post_lasso_min)
print(final_support_vars_min)
print(final_arima_forecasts_min)



#final_forecasts_lm_min[1]
#names(final_arima_forecasts_min)


valid_lm <- which((window_size + seq_along(final_forecasts_lm_min) + forecast_horizon - 1) <= nrow(first_two_columns))
valid_post <- which((window_size + seq_along(final_forecasts_post_lasso_min) + forecast_horizon - 1) <= nrow(first_two_columns))

max_time_index <- max(
  window_size + valid_lm + forecast_horizon - 1,
  window_size + valid_post + forecast_horizon - 1
)

#kiek eilučių reikia
n_needed <- max_time_index - nrow(first_two_columns)

#jei reikia pridėti eilutes
if (n_needed > 0) {
  last_time <- first_two_columns$Time[nrow(first_two_columns)]
  new_times <- generate_quarters(last_time, n_needed)
  new_rows <- as.data.frame(matrix(NA, nrow = n_needed, ncol = ncol(first_two_columns)))
  colnames(new_rows) <- colnames(first_two_columns)
  new_rows$Time <- new_times
  first_two_columns <- rbind(first_two_columns, new_rows)
}

# LM. kiekvienam langui kuriam df, kuriame yra laikas, prognoze, lango numeris
all_lm_forecasts_shifted <- do.call(rbind, lapply(valid_lm, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_lm_min[[i]],
    Type = "LM",
    Window = i
  )
}))
# Post LASSO. kiekvienam langui kuriam df, kuriame yra laikas, prognoze, lango numeris
all_post_lasso_forecasts_shifted <- do.call(rbind, lapply(valid_post, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_post_lasso_min[[i]],
    Type = "Post-LASSO",
    Window = i
  )
}))

#apjungiam
forecast_shifted_df_lm <-all_lm_forecasts_shifted
forecast_shifted_df_postl<-all_post_lasso_forecasts_shifted
#realus bvp su df
# real_bvp_df <- data.frame(
#   Time = first_two_columns$Time,
#   Real = y
# )
#apkerpam laika
start_period <- "2017K2"
end_period <- "2021K2"

real_bvp_df_trimmed <- real_bvp_df[real_bvp_df$Time >= start_period & real_bvp_df$Time <= end_period, ]

#LM grafikas
ggplot() +
  geom_line(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), linewidth = 1.2) +
  geom_point(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), size = 2) +
  
  geom_line(data = subset(forecast_shifted_df_lm, Type == "LM"),
            aes(x = Time, y = Forecast, group = Window, color = factor(Window)),
            alpha = 0.8, linewidth = 1.1) +
  
  labs(
    title = "BVP tiesinio modelio prognozių palyginimas",
    x = "Ketvirtis", y = "BVP", color = "Langas arba realybė"
  ) +
  scale_color_manual(
    values = c("Reali reikšmė" = "darkgreen")
  ) +
  scale_color_viridis_d(option = "plasma", begin = 0, end = 0.85, direction = -1, name = "Langas") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )
lm_merged <- merge(forecast_shifted_df_lm, real_bvp_df_trimmed, by = "Time")

rmse_lm_total <- rmse(lm_merged$Real, lm_merged$Forecast)
print(paste("RMSE (LM), total):", round(rmse_lm_total, 4)))


#post LASSO grafikas
ggplot() +
  geom_line(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), linewidth = 1.2) +
  geom_point(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), size = 2) +
  
  geom_line(data = subset(forecast_shifted_df_postl, Type == "Post-LASSO"),
            aes(x = Time, y = Forecast, group = Window, color = factor(Window)),
            alpha = 0.8, linewidth = 1.1) +
  
  labs(
    title = "BVP prognozių palyginimas",
    x = "Ketvirtis", y = "BVP", color = "Langas arba realybė"
  ) +
  scale_color_manual(
    values = c("Reali reikšmė" = "darkgreen"),
    guide = "none"
  ) +
  scale_color_viridis_d(option = "plasma", begin = 0, end = 0.85, direction = -1, name = "Langas") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

post_merged <- merge(forecast_shifted_df_postl, real_bvp_df_trimmed, by = "Time")

rmse_post_total <- rmse(post_merged$Real, post_merged$Forecast)
print(paste("RMSE (Post-LASSO, total):", round(rmse_post_total, 4)))


forecast_shifted_df_lm$Horizon <- rep(1:forecast_horizon, times = length(unique(forecast_shifted_df_lm$Window)))
forecast_shifted_df_lm
forecast_shifted_df_lm <- do.call(rbind, lapply(valid_lm, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_lm_min[[i]],
    Horizon = 1:forecast_horizon,
    Window = i
  )
}))


library(viridis)

num_windows <- length(unique(forecast_shifted_df_lm$Horizon))

blue_shades <- viridis(num_windows, option = "plasma", begin = 0, end = 0.9, direction = -1)

k8 <- ggplot() +
  geom_line(data = real_bvp_df_trimmed,
            aes(x = Time, y = Real, color = "Realybė", group = 1),
            size = 1.2) +
  geom_point(data = real_bvp_df_trimmed,
             aes(x = Time, y = Real, color = "Realybė")) +
  
  geom_line(data = forecast_shifted_df_lm,
            aes(x = Time, y = Forecast, group = Horizon, color = as.factor(Horizon)),
            size = 1.2, linetype = "dashed") +
  geom_point(data = forecast_shifted_df_lm,
             aes(x = Time, y = Forecast, color = as.factor(Horizon))) +
  
  scale_color_manual(
    values = c(setNames(blue_shades, as.character(unique(forecast_shifted_df_lm$Horizon))),
               "Realybė" = "red"),
    guide = guide_legend(override.aes = list(linetype = c(rep("dashed", num_windows), "solid"),
                                             size = c(rep(1.2, num_windows), 1.2)), title = "Horizontai")
  ) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )+
  labs(title = "BVP prognozės (Tiesinė regresija) ir realybė pagal horizontus",
       x = "Kvartalas",
       y = "BVP",
       color = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold")) 

ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs21.png", plot = k8, width = 10, height = 6, dpi = 300, bg = "white")


forecast_shifted_df_lm <- merge(forecast_shifted_df_lm, real_bvp_df_trimmed, by = "Time")
rmse_lm_by_horizon <- aggregate(cbind(Forecast, Real) ~ Horizon, data = forecast_shifted_df_lm, FUN = function(x) x)
rmse_lm_by_horizon$RMSE <- sapply(unique(forecast_shifted_df_lm$Horizon), function(h) {
  rmse_vals <- forecast_shifted_df_lm[forecast_shifted_df_lm$Horizon == h, ]
  rmse(rmse_vals$Real, rmse_vals$Forecast)
})
print(rmse_lm_by_horizon[, c("Horizon", "RMSE")])

err_lm_pl_17_21 <- errors_by_h(forecast_shifted_df_lm)


forecast_shifted_df_post <- do.call(rbind, lapply(valid_post, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_post_lasso_min[[i]],
    Horizon = 1:forecast_horizon,
    Window = i
  )
}))
forecast_shifted_df_post

k10 <- ggplot() +
  geom_line(data = real_bvp_df_trimmed,
            aes(x = Time, y = Real, color = "Realybė", group = 1),
            size = 1.2) +
  geom_point(data = real_bvp_df_trimmed,
             aes(x = Time, y = Real, color = "Realybė")) +
  
  geom_line(data = forecast_shifted_df_post,
            aes(x = Time, y = Forecast, group = Horizon, color = as.factor(Horizon)),
            size = 1.2, linetype = "dashed") +
  geom_point(data = forecast_shifted_df_post,
             aes(x = Time, y = Forecast, color = as.factor(Horizon))) +
  
  scale_color_manual(
    values = c(setNames(blue_shades, as.character(unique(forecast_shifted_df_post$Horizon))),
               "Realybė" = "red"),
    guide = guide_legend(override.aes = list(linetype = c(rep("dashed", num_windows), "solid"),
                                             size = c(rep(1.2, num_windows), 1.2)), title = "Horizontai")
  ) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )+
  labs(title = "BVP prognozės (Post-LASSO) ir realybė pagal horizontus",
       x = "Kvartalas",
       y = "BVP",
       color = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold")) 

ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs22.png", plot = k10, width = 10, height = 6, dpi = 300, bg = "white")



forecast_shifted_df_post <- merge(forecast_shifted_df_post, real_bvp_df_trimmed, by = "Time")
rmse_post_by_horizon <- aggregate(cbind(Forecast, Real) ~ Horizon, data = forecast_shifted_df_post, FUN = function(x) x)
rmse_post_by_horizon$RMSE <- sapply(unique(forecast_shifted_df_post$Horizon), function(h) {
  rmse_vals <- forecast_shifted_df_post[forecast_shifted_df_post$Horizon == h, ]
  rmse(rmse_vals$Real, rmse_vals$Forecast)
})
print(rmse_post_by_horizon[, c("Horizon", "RMSE")])

err_las_pl_17_21 <- errors_by_h(forecast_shifted_df_post)


rmse_plot_df <- data.frame(
  Horizon = rep(1:forecast_horizon, 2),
  RMSE = c(rmse_lm_by_horizon$RMSE, rmse_post_by_horizon$RMSE),
  Model = rep(c("LM", "Post-LASSO"), each = forecast_horizon)
)


lm1 <- ggplot(rmse_plot_df, aes(x = Horizon, y = RMSE, colour = Model)) +
  geom_line(size = 1.3, alpha = 0.9) +
  geom_point(size = 3) +
  scale_color_manual(values = c("LM" = "red","Post-LASSO" = "blue"), name = "")+
  scale_x_continuous(labels = number_format(accuracy = 1)) + 
  labs(
    title = "2017K2-2021K2",
    x = "Horizontas",
    y = "RMSE",
    color = "Modelis"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )


library(patchwork)

top_row <- lm1 + lm2

# Центруем нижний график между пустыми местами
bottom_row <- plot_spacer() + lm3 + plot_spacer()
bottom_row <- bottom_row + plot_layout(widths = c(1, 2, 1))  # Центрируем RMSE

# Объединяем всё и добавляем заголовок и общую легенду
final_plot <- (top_row / bottom_row) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

final_plot <- final_plot +
  plot_layout(guides = "collect") +   
  plot_annotation(
    title = "RMSE pagal horizontus",
    theme = theme(
      plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),legend.position = "bottom"  
    )
  )

# Визуализация
x11()
final_plot
ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs35.png", plot = final_plot, width = 10, height = 6, dpi = 300, bg = "white")


#################################################################################################################
#####################################################################ARIMA#######################################
#####################################################################2021k1-2027k1###############################
library(tidyfit)
library(pbapply)
library(forecast)
library(zoo)
library(glmnet)

window_size <- 78
forecast_horizon <- 9
target_time <- "2024K4"
max_index <- which(stationary_table$Time == target_time)

X <- as.matrix(stationary_table[, -c(1, 2)])
y <- as.numeric(unlist(stationary_table[, 2]))

set.seed(123)

final_support_vars_min <- list()
final_arima_forecasts_min <- list()
final_arima_y_forecasts <- list()

for (i in 1:(max_index - window_size + 1)) {
  X_window <- X[i:(i + window_size - 1), ]
  y_window <- y[i:(i + window_size - 1)]
  
  lasso_model <- cv.glmnet(X_window, y_window, family = "gaussian", alpha = 1)
  coef_lasso_min <- coef(lasso_model, s = "lambda.min")
  support_vars_min <- which(coef_lasso_min[-1, 1] != 0)
  
  if (length(support_vars_min) > 0) {
    X_window_selected_min <- X_window[, support_vars_min, drop = FALSE]
    selected_var_names <- colnames(X_window_selected_min)
    
    arima_forecasts <- list()
    for (j in 1:ncol(X_window_selected_min)) {
      model <- auto.arima(X_window_selected_min[, j])
      arima_forecasts[[j]] <- as.numeric(forecast(model, h = forecast_horizon)$mean)
    }
    
    if (length(arima_forecasts) > 0) {
      arima_forecasts_matrix <- do.call(rbind, arima_forecasts)
      arima_forecasts_df <- as.data.frame(t(arima_forecasts_matrix))
      colnames(arima_forecasts_df) <- selected_var_names
      
      final_arima_forecasts_min[[i]] <- arima_forecasts_df
      final_support_vars_min[[i]] <- support_vars_min
    }
  }
  
  arima_model_y <- auto.arima(y_window)
  forecast_y <- forecast(arima_model_y, h = forecast_horizon)$mean
  final_arima_y_forecasts[[i]] <- as.numeric(forecast_y)
}

print(final_support_vars_min)
print(final_arima_forecasts_min) 
print(final_arima_y_forecasts)
valid_arima <- which((window_size + seq_along(final_arima_y_forecasts) + forecast_horizon - 1) <= nrow(first_two_columns))

max_time_index <- max(
  window_size + valid_arima + forecast_horizon - 1
)
first_two_columns<-stationary_table[, 1:2]
#kiek eilučių reikia
n_needed <- max_time_index - nrow(first_two_columns)

#jei reikia pridėti eilutes
if (n_needed > 0) {
  last_time <- first_two_columns$Time[nrow(first_two_columns)]
  new_times <- generate_quarters(last_time, n_needed)
  new_rows <- as.data.frame(matrix(NA, nrow = n_needed, ncol = ncol(first_two_columns)))
  colnames(new_rows) <- colnames(first_two_columns)
  new_rows$Time <- new_times
  first_two_columns <- rbind(first_two_columns, new_rows)
}
# LM. kiekvienam langui kuriam df, kuriame yra laikas, prognoze, lango numeris
all_arima_forecasts_shifted <- do.call(rbind, lapply(valid_arima, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_arima_y_forecasts[[i]],
    Type = "ARIMA",
    Window = i
  )
}))
#all_arima_forecasts_shifted
#apjungiam
forecast_arima_df <- all_arima_forecasts_shifted
#real_bvp_df
#first_two_columns
#realus bvp su df
real_bvp_df <- data.frame(
  Time = first_two_columns$Time,
  Real = first_two_columns$BVP
)
#apkerpam laika
real_bvp_df_trimmed <- real_bvp_df[(window_size + 1):nrow(real_bvp_df), ]

#ARIMA grafikas
ggplot() +
  geom_line(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), linewidth = 1.2) +
  geom_point(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), size = 2) +
  
  geom_line(data = subset(forecast_arima_df, Type == "ARIMA"),
            aes(x = Time, y = Forecast, group = Window, color = factor(Window)),
            alpha = 0.8, linewidth = 1.1) +
  
  labs(
    title = "BVP prognozių palyginimas",
    x = "Ketvirtis", y = "BVP", color = "Langas arba realybė"
  ) +
  scale_color_manual(
    values = c("Reali reikšmė" = "darkgreen"),
    guide = "none"
  ) +
  scale_color_viridis_d(option = "plasma", begin = 0, end = 0.85, direction = -1, name = "Langas") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

forecast_arima_df$Horizon <- rep(1:forecast_horizon, times = length(unique(forecast_arima_df$Window)))
forecast_arima_df
forecast_arima_df_kt <- do.call(rbind, lapply(valid_arima, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_arima_y_forecasts[[i]],
    Horizon = 1:forecast_horizon,
    Window = i
  )
}))


num_windows <- length(unique(forecast_arima_df_kt$Horizon))

blue_shades <- viridis(num_windows, option = "plasma", begin = 0, end = 0.9, direction = -1)


k1 <- ggplot() +
  geom_line(data = real_bvp_df_trimmed,
            aes(x = Time, y = Real, color = "Realybė", group = 1),
            size = 1.2) +
  geom_point(data = real_bvp_df_trimmed,
             aes(x = Time, y = Real, color = "Realybė")) +
  
  geom_line(data = forecast_arima_df_kt,
            aes(x = Time, y = Forecast, group = Horizon, color = as.factor(Horizon)),
            size = 1.2, linetype = "dashed") +
  geom_point(data = forecast_arima_df_kt,
             aes(x = Time, y = Forecast, color = as.factor(Horizon))) +
  
  scale_color_manual(
    values = c(setNames(blue_shades, as.character(unique(forecast_arima_df_kt$Horizon))),
               "Realybė" = "red"),
    guide = guide_legend(override.aes = list(linetype = c(rep("dashed", num_windows), "solid"),
                                             size = c(rep(1.2, num_windows), 1.2)), title = "Horizontai")
  ) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )+
  labs(title = "BVP prognozės (ARIMA) ir realybė pagal horizontus",
       x = "Kvartalas",
       y = "BVP",
       color = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold")) 

ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs34.png", plot = k1, width = 10, height = 6, dpi = 300, bg = "white")


forecast_arima_df_kt <- merge(forecast_arima_df_kt, real_bvp_df_trimmed, by = "Time")
forecast_arima_df_kt <- forecast_arima_df_kt[!is.na(forecast_arima_df_kt$Real),]
rmse_post_by_horizon <- aggregate(cbind(Forecast, Real) ~ Horizon, data = forecast_arima_df_kt, FUN = function(x) x)
rmse_post_by_horizon$RMSE <- sapply(unique(forecast_arima_df_kt$Horizon), function(h) {
  rmse_vals <- forecast_arima_df_kt[forecast_arima_df_kt$Horizon == h, ]
  rmse(rmse_vals$Real, rmse_vals$Forecast)
})
print(rmse_post_by_horizon[, c("Horizon", "RMSE")])

err_arima_21_27 <- errors_by_h(forecast_arima_df_kt)


rmse_plot_df <- data.frame(
  Horizon = rep(1:forecast_horizon, 2),
  RMSE = rmse_post_by_horizon$RMSE,
  Model = rep("ARIMA", each = forecast_horizon)
)

x11()
ar3 <- ggplot(rmse_plot_df, aes(x = Horizon, y = RMSE, colour = Model)) +
  geom_line(size = 1.3, alpha = 0.9) +
  geom_point(size = 3) +
  scale_color_manual(values = c("ARIMA" = "red"), name = "")+
  scale_x_continuous(labels = number_format(accuracy = 1)) + 
  labs(
    title = "2021K1-2024K4",
    x = "Horizontas",
    y = "RMSE",
    color = "Modelis"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )


#####################################################################ARIMA#######################################
#####################################################################2017k2-2019k2###############################

window_size <- 71
forecast_horizon <- 8
target_time <- "2019K2"
max_index <- which(stationary_table$Time == target_time)

X <- as.matrix(stationary_table[, -c(1, 2)])
y <- as.numeric(unlist(stationary_table[, 2]))

set.seed(123)

final_support_vars_min <- list()
final_arima_forecasts_min <- list()
final_arima_y_forecasts <- list()

for (i in 1:(max_index - window_size + 1)) {
  X_window <- X[i:(i + window_size - 1), ]
  y_window <- y[i:(i + window_size - 1)]
  
  lasso_model <- cv.glmnet(X_window, y_window, family = "gaussian", alpha = 1)
  coef_lasso_min <- coef(lasso_model, s = "lambda.min")
  support_vars_min <- which(coef_lasso_min[-1, 1] != 0)
  
  if (length(support_vars_min) > 0) {
    X_window_selected_min <- X_window[, support_vars_min, drop = FALSE]
    selected_var_names <- colnames(X_window_selected_min)
    
    arima_forecasts <- list()
    for (j in 1:ncol(X_window_selected_min)) {
      model <- auto.arima(X_window_selected_min[, j])
      arima_forecasts[[j]] <- as.numeric(forecast(model, h = forecast_horizon)$mean)
    }
    
    if (length(arima_forecasts) > 0) {
      arima_forecasts_matrix <- do.call(rbind, arima_forecasts)
      arima_forecasts_df <- as.data.frame(t(arima_forecasts_matrix))
      colnames(arima_forecasts_df) <- selected_var_names
      
      final_arima_forecasts_min[[i]] <- arima_forecasts_df
      final_support_vars_min[[i]] <- support_vars_min
    }
  }
  
  arima_model_y <- auto.arima(y_window)
  forecast_y <- forecast(arima_model_y, h = forecast_horizon)$mean
  final_arima_y_forecasts[[i]] <- as.numeric(forecast_y)
}

print(final_support_vars_min)
print(final_arima_forecasts_min) 
print(final_arima_y_forecasts)
valid_arima <- which((window_size + seq_along(final_arima_y_forecasts) + forecast_horizon - 1) <= nrow(first_two_columns))

max_time_index <- max(
  window_size + valid_arima + forecast_horizon - 1
)
first_two_columns<-stationary_table[, 1:2]
#kiek eilučių reikia
n_needed <- max_time_index - nrow(first_two_columns)

#jei reikia pridėti eilutes
if (n_needed > 0) {
  last_time <- first_two_columns$Time[nrow(first_two_columns)]
  new_times <- generate_quarters(last_time, n_needed)
  new_rows <- as.data.frame(matrix(NA, nrow = n_needed, ncol = ncol(first_two_columns)))
  colnames(new_rows) <- colnames(first_two_columns)
  new_rows$Time <- new_times
  first_two_columns <- rbind(first_two_columns, new_rows)
}
# LM. kiekvienam langui kuriam df, kuriame yra laikas, prognoze, lango numeris
all_arima_forecasts_shifted <- do.call(rbind, lapply(valid_arima, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_arima_y_forecasts[[i]],
    Type = "ARIMA",
    Window = i
  )
}))
#all_arima_forecasts_shifted
#apjungiam
forecast_arima_df <- all_arima_forecasts_shifted
#real_bvp_df
#first_two_columns
#realus bvp su df
real_bvp_df <- data.frame(
  Time = first_two_columns$Time,
  Real = first_two_columns$BVP
)
start_period <- "2017K2"
end_period <- "2021K2"

real_bvp_df_trimmed <- real_bvp_df[real_bvp_df$Time >= start_period & real_bvp_df$Time <= end_period, ]

#ARIMA grafikas
ggplot() +
  geom_line(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), linewidth = 1.2) +
  geom_point(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), size = 2) +
  
  geom_line(data = subset(forecast_arima_df, Type == "ARIMA"),
            aes(x = Time, y = Forecast, group = Window, color = factor(Window)),
            alpha = 0.8, linewidth = 1.1) +
  
  labs(
    title = "BVP prognozių palyginimas",
    x = "Ketvirtis", y = "BVP", color = "Langas arba realybė"
  ) +
  scale_color_manual(
    values = c("Reali reikšmė" = "darkgreen"),
    guide = "none"
  ) +
  scale_color_viridis_d(option = "plasma", begin = 0, end = 0.85, direction = -1, name = "Langas") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

forecast_arima_df$Horizon <- rep(1:forecast_horizon, times = length(unique(forecast_arima_df$Window)))
forecast_arima_df
forecast_arima_df_kt <- do.call(rbind, lapply(valid_arima, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_arima_y_forecasts[[i]],
    Horizon = 1:forecast_horizon,
    Window = i
  )
}))

ggplot() +
  geom_line(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), linewidth = 1.2) +
  geom_point(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), size = 2) +
  
  geom_line(data = forecast_arima_df_kt,
            aes(x = Time, y = Forecast, color = as.factor(Horizon), group = Horizon),
            linewidth = 1.1) +
  
  labs(
    title = "Prognozės pagal horizontus",
    x = "Ketvirtis", y = "BVP", color = "Prognozės horizontas"
  ) +
  scale_color_viridis_d(option = "plasma", begin = 0, end = 0.9) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

num_windows <- length(unique(forecast_arima_df_kt$Horizon))

blue_shades <- viridis(num_windows, option = "plasma", begin = 0, end = 0.9, direction = -1)

k2 <- ggplot() +
  geom_line(data = real_bvp_df_trimmed,
            aes(x = Time, y = Real, color = "Realybė", group = 1),
            size = 1.2) +
  geom_point(data = real_bvp_df_trimmed,
             aes(x = Time, y = Real, color = "Realybė")) +
  
  geom_line(data = forecast_arima_df_kt,
            aes(x = Time, y = Forecast, group = Horizon, color = as.factor(Horizon)),
            size = 1.2, linetype = "dashed") +
  geom_point(data = forecast_arima_df_kt,
             aes(x = Time, y = Forecast, color = as.factor(Horizon))) +
  
  scale_color_manual(
    values = c(setNames(blue_shades, as.character(unique(forecast_arima_df_kt$Horizon))),
               "Realybė" = "red"),
    guide = guide_legend(override.aes = list(linetype = c(rep("dashed", num_windows), "solid"),
                                             size = c(rep(1.2, num_windows), 1.2)), title = "Horizontai")
  ) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )+
  labs(title = "BVP prognozės (ARIMA) ir realybė pagal horizontus",
       x = "Kvartalas",
       y = "BVP",
       color = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, , face = "bold")) 

ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs30.png", plot = k2, width = 10, height = 6, dpi = 300, bg = "white")


forecast_arima_df_kt <- merge(forecast_arima_df_kt, real_bvp_df_trimmed, by = "Time")
rmse_post_by_horizon <- aggregate(cbind(Forecast, Real) ~ Horizon, data = forecast_arima_df_kt, FUN = function(x) x)
rmse_post_by_horizon$RMSE <- sapply(unique(forecast_arima_df_kt$Horizon), function(h) {
  rmse_vals <- forecast_arima_df_kt[forecast_arima_df_kt$Horizon == h, ]
  rmse(rmse_vals$Real, rmse_vals$Forecast)
})
print(rmse_post_by_horizon[, c("Horizon", "RMSE")])

err_arima_17_21 <- errors_by_h(forecast_arima_df_kt)


rmse_plot_df <- data.frame(
  Horizon = rep(1:forecast_horizon, 2),
  RMSE = rmse_post_by_horizon$RMSE,
  Model = rep("ARIMA", each = forecast_horizon)
)

x11()
ar1 <- ggplot(rmse_plot_df, aes(x = Horizon, y = RMSE, colour = Model)) +
  geom_line(size = 1.3, alpha = 0.9) +
  geom_point(size = 3) +
  scale_color_manual(values = c("ARIMA" = "red"), name = "")+
  labs(
    title = "2017K2-2021K2",
    x = "Horizontas",
    y = "RMSE",
    color = "Modelis"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )


#####################################################################ARIMA#######################################
#####################################################################2019k2-2021k2###############################

window_size <- 79
forecast_horizon <- 8
target_time <- "2021K2"
max_index <- which(stationary_table$Time == target_time)

X <- as.matrix(stationary_table[, -c(1, 2)])
y <- as.numeric(unlist(stationary_table[, 2]))

set.seed(123)

final_support_vars_min <- list()
final_arima_forecasts_min <- list()
final_arima_y_forecasts <- list()

for (i in 1:(max_index - window_size + 1)) {
  X_window <- X[i:(i + window_size - 1), ]
  y_window <- y[i:(i + window_size - 1)]
  
  lasso_model <- cv.glmnet(X_window, y_window, family = "gaussian", alpha = 1)
  coef_lasso_min <- coef(lasso_model, s = "lambda.min")
  support_vars_min <- which(coef_lasso_min[-1, 1] != 0)
  
  if (length(support_vars_min) > 0) {
    X_window_selected_min <- X_window[, support_vars_min, drop = FALSE]
    selected_var_names <- colnames(X_window_selected_min)
    
    arima_forecasts <- list()
    for (j in 1:ncol(X_window_selected_min)) {
      model <- auto.arima(X_window_selected_min[, j])
      arima_forecasts[[j]] <- as.numeric(forecast(model, h = forecast_horizon)$mean)
    }
    
    if (length(arima_forecasts) > 0) {
      arima_forecasts_matrix <- do.call(rbind, arima_forecasts)
      arima_forecasts_df <- as.data.frame(t(arima_forecasts_matrix))
      colnames(arima_forecasts_df) <- selected_var_names
      
      final_arima_forecasts_min[[i]] <- arima_forecasts_df
      final_support_vars_min[[i]] <- support_vars_min
    }
  }
  
  arima_model_y <- auto.arima(y_window)
  forecast_y <- forecast(arima_model_y, h = forecast_horizon)$mean
  final_arima_y_forecasts[[i]] <- as.numeric(forecast_y)
}

print(final_support_vars_min)
print(final_arima_forecasts_min) 
print(final_arima_y_forecasts)
valid_arima <- which((window_size + seq_along(final_arima_y_forecasts) + forecast_horizon - 1) <= nrow(first_two_columns))

max_time_index <- max(
  window_size + valid_arima + forecast_horizon - 1
)
first_two_columns<-stationary_table[, 1:2]
#kiek eilučių reikia
n_needed <- max_time_index - nrow(first_two_columns)

#jei reikia pridėti eilutes
if (n_needed > 0) {
  last_time <- first_two_columns$Time[nrow(first_two_columns)]
  new_times <- generate_quarters(last_time, n_needed)
  new_rows <- as.data.frame(matrix(NA, nrow = n_needed, ncol = ncol(first_two_columns)))
  colnames(new_rows) <- colnames(first_two_columns)
  new_rows$Time <- new_times
  first_two_columns <- rbind(first_two_columns, new_rows)
}
# LM. kiekvienam langui kuriam df, kuriame yra laikas, prognoze, lango numeris
all_arima_forecasts_shifted <- do.call(rbind, lapply(valid_arima, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_arima_y_forecasts[[i]],
    Type = "ARIMA",
    Window = i
  )
}))
#all_arima_forecasts_shifted
#apjungiam
forecast_arima_df <- all_arima_forecasts_shifted
#real_bvp_df
#first_two_columns
#realus bvp su df
real_bvp_df <- data.frame(
  Time = first_two_columns$Time,
  Real = first_two_columns$BVP
)
start_period <- "2019K2"
end_period <- "2023K2"

real_bvp_df_trimmed <- real_bvp_df[real_bvp_df$Time >= start_period & real_bvp_df$Time <= end_period, ]

#ARIMA grafikas
ggplot() +
  geom_line(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), linewidth = 1.2) +
  geom_point(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), size = 2) +
  
  geom_line(data = subset(forecast_arima_df, Type == "ARIMA"),
            aes(x = Time, y = Forecast, group = Window, color = factor(Window)),
            alpha = 0.8, linewidth = 1.1) +
  
  labs(
    title = "BVP prognozių palyginimas",
    x = "Ketvirtis", y = "BVP", color = "Langas arba realybė"
  ) +
  scale_color_manual(
    values = c("Reali reikšmė" = "darkgreen"),
    guide = "none"
  ) +
  scale_color_viridis_d(option = "plasma", begin = 0, end = 0.85, direction = -1, name = "Langas") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

forecast_arima_df$Horizon <- rep(1:forecast_horizon, times = length(unique(forecast_arima_df$Window)))
forecast_arima_df
forecast_arima_df_kt <- do.call(rbind, lapply(valid_arima, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_arima_y_forecasts[[i]],
    Horizon = 1:forecast_horizon,
    Window = i
  )
}))

num_windows <- length(unique(forecast_arima_df_kt$Horizon))

blue_shades <- viridis(num_windows, option = "plasma", begin = 0, end = 0.9, direction = -1)

k3 <- ggplot() +
  geom_line(data = real_bvp_df_trimmed,
            aes(x = Time, y = Real, color = "Realybė", group = 1),
            size = 1.2) +
  geom_point(data = real_bvp_df_trimmed,
             aes(x = Time, y = Real, color = "Realybė")) +
  
  geom_line(data = forecast_arima_df_kt,
            aes(x = Time, y = Forecast, group = Horizon, color = as.factor(Horizon)),
            size = 1.2, linetype = "dashed") +
  geom_point(data = forecast_arima_df_kt,
             aes(x = Time, y = Forecast, color = as.factor(Horizon))) +
  
  scale_color_manual(
    values = c(setNames(blue_shades, as.character(unique(forecast_arima_df_kt$Horizon))),
               "Realybė" = "red"),
    guide = guide_legend(override.aes = list(linetype = c(rep("dashed", num_windows), "solid"),
                                             size = c(rep(1.2, num_windows), 1.2)), title = "Horizontai")
  ) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )+
  labs(title = "BVP prognozės (ARIMA) ir realybė pagal horizontus",
       x = "Kvartalas",
       y = "BVP",
       color = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold")) 

ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs32.png", plot = k3, width = 10, height = 6, dpi = 300, bg = "white")


forecast_arima_df_kt <- merge(forecast_arima_df_kt, real_bvp_df_trimmed, by = "Time")
rmse_post_by_horizon <- aggregate(cbind(Forecast, Real) ~ Horizon, data = forecast_arima_df_kt, FUN = function(x) x)
rmse_post_by_horizon$RMSE <- sapply(unique(forecast_arima_df_kt$Horizon), function(h) {
  rmse_vals <- forecast_arima_df_kt[forecast_arima_df_kt$Horizon == h, ]
  rmse(rmse_vals$Real, rmse_vals$Forecast)
})
print(rmse_post_by_horizon[, c("Horizon", "RMSE")])

err_arima_19_23 <- errors_by_h(forecast_arima_df_kt)


rmse_plot_df <- data.frame(
  Horizon = rep(1:forecast_horizon, 2),
  RMSE = rmse_post_by_horizon$RMSE,
  Model = rep("ARIMA", each = forecast_horizon)
)

x11()
ar2 <- ggplot(rmse_plot_df, aes(x = Horizon, y = RMSE, colour = Model)) +
  geom_line(size = 1.3, alpha = 0.9) +
  geom_point(,size = 3) +
  scale_color_manual(values = c("ARIMA" = "red"), name = "")+
labs(
    title = "2019K2-2023K2",
    x = "Horizontas",
    y = "RMSE",
    color = "Modelis"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )



library(patchwork)

top_row <- ar1 + ar2

# Центруем нижний график между пустыми местами
bottom_row <- plot_spacer() + ar3 + plot_spacer()
bottom_row <- bottom_row + plot_layout(widths = c(1, 2, 1))  # Центрируем RMSE

# Объединяем всё и добавляем заголовок и общую легенду
final_plot <- (top_row / bottom_row) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

final_plot <- final_plot +
  plot_layout(guides = "collect") +   
  plot_annotation(
  title = "RMSE pagal horizontus",
  theme = theme(
    plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),legend.position = "bottom"  
  )
)

# Визуализация
x11()
final_plot

ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs33.png", plot = final_plot, width = 10, height = 6, dpi = 300, bg = "white")


######################################################################################################################
######################################################################################################################
######################################################################random forest su MI 2017k2-2019k2###############
library(tidyfit)
library(pbapply)
library(forecast)
library(zoo)
library(glmnet)
library(randomForest)
#lango dydis
window_size <- 71
forecast_horizon <- 8 #prognozes

#visa pradine lentele
X <- as.matrix(stationary_table[, -c(1, 2)])
#bvp
y <- as.numeric(unlist(stationary_table[, 2]))
set.seed(123)
target_time <- "2019K2"

max_index <- which(stationary_table$Time == target_time)

final_forecasts_randfor <- list()
final_arima_forecasts <- list()
for (i in 1:(max_index - window_size + 1)) {
  data_window <- stationary_table[i:(i + window_size - 1), ]
  y_window <- y[i:(i + window_size - 1)]
  X_window <- X[i:(i + window_size - 1), ]
  
  mi_results <- calculate_mutual_information_for_all_columns(data_window)
  top_100_variables <- mi_results[order(mi_results$MI, decreasing = TRUE), ][1:100, ]
  selected_var_names <- top_100_variables$Variable
  
  #su lambda min
  if (length(selected_var_names) > 0) {
    X_window_selected <- X_window[, selected_var_names, drop = FALSE]
    rf_model <- randomForest(X_window_selected, y_window, ntree = 10000)
    #arima
    arima_forecasts <- list()
    for (j in 1:ncol(X_window_selected)) {
      model <- auto.arima(X_window_selected[, j])
      arima_forecasts[[j]] <- as.numeric(forecast(model, h = forecast_horizon)$mean)
    }
    if (length(arima_forecasts) > 0) {
      arima_forecasts_matrix <- do.call(rbind, arima_forecasts)
      arima_forecasts_df <- as.data.frame(t(arima_forecasts_matrix))
      colnames(arima_forecasts_df) <- selected_var_names
      
      y_pred_rf <- predict(rf_model, arima_forecasts_df)
      #surenkame rezultatus
      final_forecasts_randfor[[i]] <- y_pred_rf
      final_arima_forecasts[[i]] <- arima_forecasts_df
      
    }
  }
}

start_period <- "2017K2"
end_period <- "2021K2"

valid_rf <- which((window_size + seq_along(final_forecasts_randfor) + forecast_horizon - 1) <= nrow(first_two_columns))

real_bvp_df_trimmed <- real_bvp_df[real_bvp_df$Time >= start_period & real_bvp_df$Time <= end_period, ]
print(final_forecasts_randfor)
print(final_arima_forecasts)


# Random Forest — prognozės kiekvienam langui
all_rf_forecasts_shifted <- do.call(rbind, lapply(valid_rf, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_randfor[[i]],
    Type = "Random Forest su MI",
    Window = i
  )
}))

all_rf_forecasts_shifted_filtered <- all_rf_forecasts_shifted[
  all_rf_forecasts_shifted$Time >= start_period & all_rf_forecasts_shifted$Time <= end_period, ]


#jei kirpti, tai all_rf_forecastst_shifted_filtered
gr1<-ggplot() +
  geom_line(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), linewidth = 1.2) +
  geom_point(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), size = 2) +
  geom_line(data = subset(all_rf_forecasts_shifted, Type == "Random Forest su MI"),
            aes(x = Time, y = Forecast, group = Window, color = interaction(Type, Window)),
            alpha = 0.6, linewidth = 0.9) +
  labs(
    title = "BVP prognozių palyginimas su Random Forest",
    x = "Ketvirtis", y = "BVP", color = "Modelis ir langas"
  ) +
  scale_color_manual(
    values = c("Reali reikšmė" = "darkgreen"),
    guide = "none"
  ) +
  scale_color_viridis_d(option = "plasma", begin = 0, end = 0.85, direction = -1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
gr1

forecast_mi_rf_df <- all_rf_forecasts_shifted

forecast_mi_rf_df$Horizon <- rep(1:forecast_horizon, times = length(unique(forecast_mi_rf_df$Window)))
forecast_mi_rf_df
forecast_mi_rf_df <- do.call(rbind, lapply(valid_rf, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_randfor[[i]],
    Horizon = 1:forecast_horizon,
    Window = i
  )
}))



num_windows <- length(unique(forecast_mi_rf_df$Horizon))

blue_shades <- viridis(num_windows, option = "plasma", begin = 0, end = 0.9, direction = -1)

k11 <- ggplot() +
  geom_line(data = real_bvp_df_trimmed,
            aes(x = Time, y = Real, color = "Realybė", group = 1),
            size = 1.2) +
  geom_point(data = real_bvp_df_trimmed,
             aes(x = Time, y = Real, color = "Realybė")) +
  
  geom_line(data = forecast_mi_rf_df,
            aes(x = Time, y = Forecast, group = Horizon, color = as.factor(Horizon)),
            size = 1.2, linetype = "dashed") +
  geom_point(data = forecast_mi_rf_df,
             aes(x = Time, y = Forecast, color = as.factor(Horizon))) +
  
  scale_color_manual(
    values = c(setNames(blue_shades, as.character(unique(forecast_mi_rf_df$Horizon))),
               "Realybė" = "red"),
    guide = guide_legend(override.aes = list(linetype = c(rep("dashed", num_windows), "solid"),
                                             size = c(rep(1.2, num_windows), 1.2)), title = "Horizontai")
  ) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )+
  labs(title = "BVP prognozės (Random Forest ir MI) ir realybė pagal horizontus",
       x = "Kvartalas",
       y = "BVP",
       color = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold")) 

ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs36.png", plot = k11, width = 10, height = 6, dpi = 300, bg = "white")


forecast_mi_rf_df <- merge(forecast_mi_rf_df, real_bvp_df_trimmed, by = "Time")
rmse_post_by_horizon <- aggregate(cbind(Forecast, Real) ~ Horizon, data = forecast_mi_rf_df, FUN = function(x) x)
rmse_post_by_horizon$RMSE <- sapply(unique(forecast_mi_rf_df$Horizon), function(h) {
  rmse_vals <- forecast_mi_rf_df[forecast_mi_rf_df$Horizon == h, ]
  rmse(rmse_vals$Real, rmse_vals$Forecast)
})
print(rmse_post_by_horizon[, c("Horizon", "RMSE")])

err_rf_mi_17_21 <- errors_by_h(forecast_mi_rf_df)


rmse_plot_df <- data.frame(
  Horizon = rep(1:forecast_horizon, 2),
  RMSE = rmse_post_by_horizon$RMSE,
  Model = rep("Random Forest", each = forecast_horizon)
)


rf1 <- ggplot(rmse_plot_df, aes(x = Horizon, y = RMSE, colour = Model)) +
  geom_line(size = 1.3, alpha = 0.9) +
  geom_point(,size = 3) +
  scale_color_manual(values = c("Random Forest" = "red"), name = "")+
  labs(
    title = "2017K2-2021K2",
    x = "Horizontas",
    y = "RMSE",
    color = "Modelis"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )


######################################################################random forest su MI 2019k1-2021k1###############
#lango dydis
library(randomForest)
window_size <- 79
forecast_horizon <- 8 #prognozes

#visa pradine lentele
X <- as.matrix(stationary_table[, -c(1, 2)])
#bvp
y <- as.numeric(unlist(stationary_table[, 2]))
set.seed(123)

target_time <- "2021K2"
max_index <- which(stationary_table$Time == target_time)

final_forecasts_randfor <- list()
final_arima_forecasts <- list()
for (i in 1:(max_index - window_size + 1)) {
  data_window <- stationary_table[i:(i + window_size - 1), ]
  y_window <- y[i:(i + window_size - 1)]
  X_window <- X[i:(i + window_size - 1), ]
  
  mi_results <- calculate_mutual_information_for_all_columns(data_window)
  top_100_variables <- mi_results[order(mi_results$MI, decreasing = TRUE), ][1:100, ]
  selected_var_names <- top_100_variables$Variable
  
  #su lambda min
  if (length(selected_var_names) > 0) {
    X_window_selected <- X_window[, selected_var_names, drop = FALSE]
    rf_model <- randomForest(X_window_selected, y_window, ntree = 10000)
    #arima
    arima_forecasts <- list()
    for (j in 1:ncol(X_window_selected)) {
      model <- auto.arima(X_window_selected[, j])
      arima_forecasts[[j]] <- as.numeric(forecast(model, h = forecast_horizon)$mean)
    }
    if (length(arima_forecasts) > 0) {
      arima_forecasts_matrix <- do.call(rbind, arima_forecasts)
      arima_forecasts_df <- as.data.frame(t(arima_forecasts_matrix))
      colnames(arima_forecasts_df) <- selected_var_names
      
      y_pred_rf <- predict(rf_model, arima_forecasts_df)
      #surenkame rezultatus
      final_forecasts_randfor[[i]] <- y_pred_rf
      final_arima_forecasts[[i]] <- arima_forecasts_df
      
    }
  }
}


start_period <- "2019K2"
end_period <- "2023K2"

valid_rf <- which((window_size + seq_along(final_forecasts_randfor) + forecast_horizon - 1) <= nrow(first_two_columns))
real_bvp_df_trimmed <- real_bvp_df[real_bvp_df$Time >= start_period & real_bvp_df$Time <= end_period, ]
print(final_forecasts_randfor)
print(final_arima_forecasts)



# Random Forest — prognozės kiekvienam langui
all_rf_forecasts_shifted <- do.call(rbind, lapply(valid_rf, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_randfor[[i]],
    Type = "Random Forest su MI",
    Window = i
  )
}))

all_rf_forecasts_shifted_filtered <- all_rf_forecasts_shifted[
  all_rf_forecasts_shifted$Time >= start_period & all_rf_forecasts_shifted$Time <= end_period, ]

gr5<-ggplot() +
  geom_line(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), linewidth = 1.2) +
  geom_point(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), size = 2) +
  geom_line(data = subset(all_rf_forecasts_shifted, Type == "Random Forest su MI"),
            aes(x = Time, y = Forecast, group = Window, color = interaction(Type, Window)),
            alpha = 0.6, linewidth = 0.9) +
  labs(
    title = "BVP prognozių palyginimas su Random Forest",
    x = "Ketvirtis", y = "BVP", color = "Modelis ir langas"
  ) +
  scale_color_manual(
    values = c("Reali reikšmė" = "darkgreen"),
    guide = "none"
  ) +
  scale_color_viridis_d(option = "plasma", begin = 0, end = 0.85, direction = -1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
gr5

forecast_mi_rf_df <- all_rf_forecasts_shifted

forecast_mi_rf_df$Horizon <- rep(1:forecast_horizon, times = length(unique(forecast_mi_rf_df$Window)))
forecast_mi_rf_df
forecast_mi_rf_df <- do.call(rbind, lapply(valid_rf, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_randfor[[i]],
    Horizon = 1:forecast_horizon,
    Window = i
  )
}))

num_windows <- length(unique(forecast_mi_rf_df$Horizon))

blue_shades <- viridis(num_windows, option = "plasma", begin = 0, end = 0.9, direction = -1)

k12 <- ggplot() +
  geom_line(data = real_bvp_df_trimmed,
            aes(x = Time, y = Real, color = "Realybė", group = 1),
            size = 1.2) +
  geom_point(data = real_bvp_df_trimmed,
             aes(x = Time, y = Real, color = "Realybė")) +
  
  geom_line(data = forecast_mi_rf_df,
            aes(x = Time, y = Forecast, group = Horizon, color = as.factor(Horizon)),
            size = 1.2, linetype = "dashed") +
  geom_point(data = forecast_mi_rf_df,
             aes(x = Time, y = Forecast, color = as.factor(Horizon))) +
  
  scale_color_manual(
    values = c(setNames(blue_shades, as.character(unique(forecast_mi_rf_df$Horizon))),
               "Realybė" = "red"),
    guide = guide_legend(override.aes = list(linetype = c(rep("dashed", num_windows), "solid"),
                                             size = c(rep(1.2, num_windows), 1.2)), title = "Horizontai")
  ) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )+
  labs(title = "BVP prognozės (Random Forest ir MI) ir realybė pagal horizontus",
       x = "Kvartalas",
       y = "BVP",
       color = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold")) 

ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs37.png", plot = k12, width = 10, height = 6, dpi = 300, bg = "white")



forecast_mi_rf_df <- merge(forecast_mi_rf_df, real_bvp_df_trimmed, by = "Time")
rmse_post_by_horizon <- aggregate(cbind(Forecast, Real) ~ Horizon, data = forecast_mi_rf_df, FUN = function(x) x)
rmse_post_by_horizon$RMSE <- sapply(unique(forecast_mi_rf_df$Horizon), function(h) {
  rmse_vals <- forecast_mi_rf_df[forecast_mi_rf_df$Horizon == h, ]
  rmse(rmse_vals$Real, rmse_vals$Forecast)
})
print(rmse_post_by_horizon[, c("Horizon", "RMSE")])

err_rf_mi_19_23 <- errors_by_h(forecast_mi_rf_df)


rmse_plot_df <- data.frame(
  Horizon = rep(1:forecast_horizon, 2),
  RMSE = rmse_post_by_horizon$RMSE,
  Model = rep("Random Forest", each = forecast_horizon)
)


rf2 <- ggplot(rmse_plot_df, aes(x = Horizon, y = RMSE, colour = Model)) +
  geom_line(size = 1.3, alpha = 0.9) +
  geom_point(,size = 3) +
  scale_color_manual(values = c("Random Forest" = "red"), name = "")+
  labs(
    title = "2019K2-2023K2",
    x = "Horizontas",
    y = "RMSE",
    color = "Modelis"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )

#########################################################################################################################
##############################################################################random forest su MI 2025k1-2027k1#########
#lango dydis

window_size <- 78
forecast_horizon <- 9 #prognozes

#visa pradine lentele
X <- as.matrix(stationary_table[, -c(1, 2)])
#bvp
y <- as.numeric(unlist(stationary_table[, 2]))
set.seed(123)

target_time <- "2024K4"
max_index <- which(stationary_table$Time == target_time)

final_forecasts_randfor <- list()
final_arima_forecasts <- list()
for (i in 1:(max_index - window_size + 1)) {
  data_window <- stationary_table[i:(i + window_size - 1), ]
  y_window <- y[i:(i + window_size - 1)]
  X_window <- X[i:(i + window_size - 1), ]
  
  mi_results <- calculate_mutual_information_for_all_columns(data_window)
  top_100_variables <- mi_results[order(mi_results$MI, decreasing = TRUE), ][1:100, ]
  selected_var_names <- top_100_variables$Variable
  
  #su lambda min
  if (length(selected_var_names) > 0) {
    X_window_selected <- X_window[, selected_var_names, drop = FALSE]
    rf_model <- randomForest(X_window_selected, y_window, ntree = 10000)
    #arima
    arima_forecasts <- list()
    for (j in 1:ncol(X_window_selected)) {
      model <- auto.arima(X_window_selected[, j])
      arima_forecasts[[j]] <- as.numeric(forecast(model, h = forecast_horizon)$mean)
    }
    if (length(arima_forecasts) > 0) {
      arima_forecasts_matrix <- do.call(rbind, arima_forecasts)
      arima_forecasts_df <- as.data.frame(t(arima_forecasts_matrix))
      colnames(arima_forecasts_df) <- selected_var_names
      
      y_pred_rf <- predict(rf_model, arima_forecasts_df)
      #surenkame rezultatus
      final_forecasts_randfor[[i]] <- y_pred_rf
      final_arima_forecasts[[i]] <- arima_forecasts_df
      
    }
  }
}

#real_bvp_df

valid_rf <- which((window_size + seq_along(final_forecasts_randfor) + forecast_horizon - 1) <= 200)
#valid_rf
max_time_index <- max(window_size + valid_rf + forecast_horizon - 1)

#kiek eilučių reikia
n_needed <- max_time_index - nrow(first_two_columns)

#jei reikia pridėti eilutes
if (n_needed > 0) {
  last_time <- first_two_columns$Time[nrow(first_two_columns)]
  new_times <- generate_quarters(last_time, n_needed)
  new_rows <- as.data.frame(matrix(NA, nrow = n_needed, ncol = ncol(first_two_columns)))
  colnames(new_rows) <- colnames(first_two_columns)
  new_rows$Time <- new_times
  first_two_columns <- rbind(first_two_columns, new_rows)
}

print(final_forecasts_randfor)
print(final_arima_forecasts)



# Random Forest — prognozės kiekvienam langui
all_rf_forecasts_shifted <- do.call(rbind, lapply(valid_rf, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_randfor[[i]],
    Type = "Random Forest su MI",
    Window = i
  )
}))
real_bvp_df_trimmed <- real_bvp_df[(window_size + 1):nrow(real_bvp_df), ]
all_rf_forecasts_shifted_filtered <- all_rf_forecasts_shifted[
  all_rf_forecasts_shifted$Time >= start_period & all_rf_forecasts_shifted$Time <= end_period, ]

ggplot() +
  geom_line(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), linewidth = 1.2) +
  geom_point(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), size = 2) +
  geom_line(data = subset(all_rf_forecasts_shifted, Type == "Random Forest su MI"),
            aes(x = Time, y = Forecast, group = Window, color = interaction(Type, Window)),
            alpha = 0.6, linewidth = 0.9) +
  labs(
    title = "BVP prognozių palyginimas su Random Forest",
    x = "Ketvirtis", y = "BVP", color = "Modelis ir langas"
  ) +
  scale_color_manual(
    values = c("Reali reikšmė" = "darkgreen"),
    guide = "none"
  ) +
  scale_color_viridis_d(option = "plasma", begin = 0, end = 0.85, direction = -1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

forecast_mi_rf_df <- all_rf_forecasts_shifted

forecast_mi_rf_df$Horizon <- rep(1:forecast_horizon, times = length(unique(forecast_mi_rf_df$Window)))
forecast_mi_rf_df
forecast_mi_rf_df <- do.call(rbind, lapply(valid_rf, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_randfor[[i]],
    Horizon = 1:forecast_horizon,
    Window = i
  )
}))

num_windows <- length(unique(forecast_mi_rf_df$Horizon))

blue_shades <- viridis(num_windows, option = "plasma", begin = 0, end = 0.9, direction = -1)

k13 <- ggplot() +
  geom_line(data = real_bvp_df_trimmed,
            aes(x = Time, y = Real, color = "Realybė", group = 1),
            size = 1.2) +
  geom_point(data = real_bvp_df_trimmed,
             aes(x = Time, y = Real, color = "Realybė")) +
  
  geom_line(data = forecast_mi_rf_df,
            aes(x = Time, y = Forecast, group = Horizon, color = as.factor(Horizon)),
            size = 1.2, linetype = "dashed") +
  geom_point(data = forecast_mi_rf_df,
             aes(x = Time, y = Forecast, color = as.factor(Horizon))) +
  
  scale_color_manual(
    values = c(setNames(blue_shades, as.character(unique(forecast_mi_rf_df$Horizon))),
               "Realybė" = "red"),
    guide = guide_legend(override.aes = list(linetype = c(rep("dashed", num_windows), "solid"),
                                             size = c(rep(1.2, num_windows), 1.2)), title = "Horizontai")
  ) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )+
  labs(title = "BVP prognozės (Random Forest ir MI) ir realybė pagal horizontus",
       x = "Kvartalas",
       y = "BVP",
       color = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold")) 

ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs38.png", plot = k13, width = 10, height = 6, dpi = 300, bg = "white")



forecast_mi_rf_df <- merge(forecast_mi_rf_df, real_bvp_df_trimmed, by = "Time")
forecast_mi_rf_df <- forecast_mi_rf_df[!is.na(forecast_mi_rf_df$Real),]
rmse_post_by_horizon <- aggregate(cbind(Forecast, Real) ~ Horizon, data = forecast_mi_rf_df, FUN = function(x) x)
rmse_post_by_horizon$RMSE <- sapply(unique(forecast_mi_rf_df$Horizon), function(h) {
  rmse_vals <- forecast_mi_rf_df[forecast_mi_rf_df$Horizon == h, ]
  rmse(rmse_vals$Real, rmse_vals$Forecast)
})
print(rmse_post_by_horizon[, c("Horizon", "RMSE")])

err_rf_mi_21_27 <- errors_by_h(forecast_mi_rf_df)


rmse_plot_df <- data.frame(
  Horizon = rep(1:forecast_horizon, 2),
  RMSE = rmse_post_by_horizon$RMSE,
  Model = rep("Random Forest", each = forecast_horizon)
)


rf3 <- ggplot(rmse_plot_df, aes(x = Horizon, y = RMSE, colour = Model)) +
  geom_line(size = 1.3, alpha = 0.9) +
  geom_point(,size = 3) +
  scale_color_manual(values = c("Random Forest" = "red"), name = "")+
  scale_x_continuous(labels = number_format(accuracy = 1)) + 
  labs(
    title = "2019K1-2024K4",
    x = "Horizontas",
    y = "RMSE",
    color = "Modelis"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )

top_row <- rf1 + rf2

# Центруем нижний график между пустыми местами
bottom_row <- plot_spacer() + rf3 + plot_spacer()
bottom_row <- bottom_row + plot_layout(widths = c(1, 2, 1))  # Центрируем RMSE

# Объединяем всё и добавляем заголовок и общую легенду
final_plot <- (top_row / bottom_row) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

final_plot <- final_plot +
  plot_layout(guides = "collect") +   
  plot_annotation(
    title = "RMSE pagal horizontus",
    theme = theme(
      plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),legend.position = "bottom"  
    )
  )

# Визуализация
x11()
final_plot
ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs39.png", plot = final_plot, width = 10, height = 6, dpi = 300, bg = "white")


###########################################################################################################################
############################################################################################################################
##################################################################Random forest su LASSO 2017k2-2019k2############################
library(tidyfit)
library(pbapply)
library(forecast)
library(zoo)
library(glmnet)
library(randomForest)
#lango dydis
window_size <- 71
forecast_horizon <- 8 #prognozes

target_time <- "2019K2"
max_index <- which(stationary_table$Time == target_time)

#visa pradine lentele
X <- as.matrix(stationary_table[, -c(1, 2)])
#bvp
y <- as.numeric(unlist(stationary_table[, 2]))


set.seed(123)

final_forecasts_rf_min <- list()

for (i in 1:(max_index - window_size + 1)) {
  X_window <- X[i:(i + window_size - 1), ]  #lango dydzio x'ai
  y_window <- y[i:(i + window_size - 1)]   #lango dydzio y'ai
  
  lasso_model <- cv.glmnet(X_window, y_window, family = "gaussian", alpha = 1)
  
  support_vars_min <- which(coef(lasso_model, s = "lambda.min")[-1,1] != 0)

  #su lambda min
  if (length(support_vars_min) > 0) {
    #kintamieji su lambda min
    X_window_selected_min <- X_window[, support_vars_min, drop = FALSE]
    selected_var_names <- colnames(X_window_selected_min) #su lambda min
    rf_model <- randomForest(X_window_selected_min, y_window, ntree = 10000)
    #arima
    arima_forecasts <- list()
    for (j in 1:ncol(X_window_selected_min)) {
      model <- auto.arima(X_window_selected_min[, j])
      arima_forecasts[[j]] <- as.numeric(forecast(model, h = forecast_horizon)$mean)
    }
    if (length(arima_forecasts) > 0) {
      arima_forecasts_matrix <- do.call(rbind, arima_forecasts)
      arima_forecasts_df <- as.data.frame(t(arima_forecasts_matrix))
      colnames(arima_forecasts_df) <- selected_var_names
      
      predictions_rf <- predict(rf_model, newdata = arima_forecasts_df)
      
      final_forecasts_rf_min[[i]] <- predictions_rf
    }
  }
}
print(final_forecasts_rf_min)
valid_rf_lasso <- which((window_size + seq_along(final_forecasts_rf_min) + forecast_horizon - 1) <= nrow(first_two_columns))
valid_rf_lasso

# Random Forest — prognozės kiekvienam langui
all_rf_min_forecasts_shifted <- do.call(rbind, lapply(valid_rf_lasso, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_rf_min[[i]],
    Type = "Random Forest su Lasso",
    Window = i
  )
}))


start_period <- "2017K2"
end_period <- "2021K2"
real_bvp_df_trimmed <- real_bvp_df[real_bvp_df$Time >= start_period & real_bvp_df$Time <= end_period, ]
all_rf_min_forecasts_shifted_filtered <- all_rf_min_forecasts_shifted[
  all_rf_min_forecasts_shifted$Time >= start_period & all_rf_min_forecasts_shifted$Time <= end_period, ]

gr7<-ggplot() +
  geom_line(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), linewidth = 1.2) +
  geom_point(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), size = 2) +
  geom_line(data = subset(all_rf_min_forecasts_shifted, Type == "Random Forest su Lasso"),
            aes(x = Time, y = Forecast, group = Window, color = interaction(Type, Window)),
            alpha = 0.6, linewidth = 0.9) +
  labs(
    title = "BVP prognozių palyginimas su Random Forest",
    x = "Ketvirtis", y = "BVP", color = "Modelis ir langas"
  ) +
  scale_color_manual(
    values = c("Reali reikšmė" = "darkgreen"),
    guide = "none"
  ) +
  scale_color_viridis_d(option = "plasma", begin = 0, end = 0.85, direction = -1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
gr7
forecast_shifted_df_lasso<-all_rf_min_forecasts_shifted
forecast_shifted_df_lasso$Horizon <- rep(1:forecast_horizon, times = length(unique(forecast_shifted_df_lasso$Window)))
forecast_shifted_df_lasso <- do.call(rbind, lapply(valid_rf_lasso, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_rf_min[[i]],
    Horizon = 1:forecast_horizon,
    Window = i
  )
}))
forecast_shifted_df_lasso
num_windows <- length(unique(forecast_shifted_df_lasso$Horizon))

blue_shades <- viridis(num_windows, option = "plasma", begin = 0, end = 0.9, direction = -1)

k14 <- ggplot() +
  geom_line(data = real_bvp_df_trimmed,
            aes(x = Time, y = Real, color = "Realybė", group = 1),
            size = 1.2) +
  geom_point(data = real_bvp_df_trimmed,
             aes(x = Time, y = Real, color = "Realybė")) +
  
  geom_line(data = forecast_shifted_df_lasso,
            aes(x = Time, y = Forecast, group = Horizon, color = as.factor(Horizon)),
            size = 1.2, linetype = "dashed") +
  geom_point(data = forecast_shifted_df_lasso,
             aes(x = Time, y = Forecast, color = as.factor(Horizon))) +
  
  scale_color_manual(
    values = c(setNames(blue_shades, as.character(unique(forecast_shifted_df_lasso$Horizon))),
               "Realybė" = "red"),
    guide = guide_legend(override.aes = list(linetype = c(rep("dashed", num_windows), "solid"),
                                             size = c(rep(1.2, num_windows), 1.2)), title = "Horizontai")
  ) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )+
  labs(title = "BVP prognozės (Random Forest ir LASSO) ir realybė pagal horizontus",
       x = "Kvartalas",
       y = "BVP",
       color = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold")) 

ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs40.png", plot = k14, width = 10, height = 6, dpi = 300, bg = "white")



forecast_shifted_df_lasso <- merge(forecast_shifted_df_lasso, real_bvp_df_trimmed, by = "Time")
rmse_post_by_horizon <- aggregate(cbind(Forecast, Real) ~ Horizon, data = forecast_shifted_df_lasso, FUN = function(x) x)
rmse_post_by_horizon$RMSE <- sapply(unique(forecast_shifted_df_lasso$Horizon), function(h) {
  rmse_vals <- forecast_shifted_df_lasso[forecast_shifted_df_lasso$Horizon == h, ]
  rmse(rmse_vals$Real, rmse_vals$Forecast)
})
print(rmse_post_by_horizon[, c("Horizon", "RMSE")])

err_rf_las_17_21 <- errors_by_h(forecast_shifted_df_lasso)


rmse_plot_df <- data.frame(
  Horizon = rep(1:forecast_horizon, 2),
  RMSE = rmse_post_by_horizon$RMSE,
  Model = rep("Random Forest", each = forecast_horizon)
)


rfl1 <- ggplot(rmse_plot_df, aes(x = Horizon, y = RMSE, colour = Model)) +
  geom_line(size = 1.3, alpha = 0.9) +
  geom_point(,size = 3) +
  scale_color_manual(values = c("Random Forest" = "red"), name = "")+
  labs(
    title = "2017K2-2021K2",
    x = "Horizontas",
    y = "RMSE",
    color = "Modelis"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )

##################################################################Random forest su LASSO 2019k1-2021k1############################
library(tidyfit)
library(pbapply)
library(forecast)
library(zoo)
library(glmnet)
library(randomForest)
#lango dydis
window_size <- 79
forecast_horizon <- 8 #prognozes

target_time <- "2021K2"
max_index <- which(stationary_table$Time == target_time)
max_index

#visa pradine lentele
X <- as.matrix(stationary_table[, -c(1, 2)])
#bvp
y <- as.numeric(unlist(stationary_table[, 2]))


set.seed(123)

final_forecasts_rf_min <- list()

for (i in 1:(max_index - window_size + 1)) {
  X_window <- X[i:(i + window_size - 1), ]  #lango dydzio x'ai
  y_window <- y[i:(i + window_size - 1)]   #lango dydzio y'ai
  
  lasso_model <- cv.glmnet(X_window, y_window, family = "gaussian", alpha = 1)
  #plot(lasso_model)
  support_vars_min <- which(coef(lasso_model, s = "lambda.min")[-1,1] != 0)
  #names(support_vars_min)
  #su lambda min
  if (length(support_vars_min) > 0) {
    #kintamieji su lambda min
    X_window_selected_min <- X_window[, support_vars_min, drop = FALSE]
    colnames(X_window_selected_min)
    selected_var_names <- colnames(X_window_selected_min) #su lambda min
    rf_model <- randomForest(X_window_selected_min, y_window, ntree = 10000)
    #sort(importance(rf_model))
    #arima
    arima_forecasts <- list()
    for (j in 1:ncol(X_window_selected_min)) {
      model <- auto.arima(X_window_selected_min[, j])
      arima_forecasts[[j]] <- as.numeric(forecast(model, h = forecast_horizon)$mean)
      #print(arima_forecasts)
    }
    if (length(arima_forecasts) > 0) {
      arima_forecasts_matrix <- do.call(cbind, arima_forecasts)
      #arima_forecasts_matrix
      arima_forecasts_df <- as.data.frame(arima_forecasts_matrix)
      colnames(arima_forecasts_df) <- selected_var_names
      
      predictions_rf <- predict(rf_model, newdata = arima_forecasts_df)
      #predictions_rf
      
      final_forecasts_rf_min[[i]] <- predictions_rf
    }
  }
  else{
    coef_lasso_min <- coef(lasso_model, s = lambda_n(lasso_model, N = 10))
    support_vars_min <- which(coef_lasso_min[-1, 1] != 0)
    X_window_selected_min <- X_window[, support_vars_min, drop = FALSE]
    colnames(X_window_selected_min)
    selected_var_names <- colnames(X_window_selected_min) #su lambda min
    rf_model <- randomForest(X_window_selected_min, y_window, ntree = 10000)
    arima_forecasts <- list()
    for (j in 1:ncol(X_window_selected_min)) {
      model <- auto.arima(X_window_selected_min[, j])
      arima_forecasts[[j]] <- as.numeric(forecast(model, h = forecast_horizon)$mean)
      #print(arima_forecasts)
    }
    if (length(arima_forecasts) > 0) {
      arima_forecasts_matrix <- do.call(cbind, arima_forecasts)
      #arima_forecasts_matrix
      arima_forecasts_df <- as.data.frame(arima_forecasts_matrix)
      colnames(arima_forecasts_df) <- selected_var_names
      
      predictions_rf <- predict(rf_model, newdata = arima_forecasts_df)
      #predictions_rf
      
      final_forecasts_rf_min[[i]] <- predictions_rf
    }
  }
}
print(final_forecasts_rf_min)
final_forecasts_rf_min

valid_rf_lasso <- which((window_size + seq_along(final_forecasts_rf_min) + forecast_horizon - 1) <= nrow(first_two_columns))
valid_rf_lasso

# Random Forest — prognozės kiekvienam langui
all_rf_min_forecasts_shifted <- do.call(rbind, lapply(valid_rf_lasso, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_rf_min[[i]],
    Type = "Random Forest su Lasso",
    Window = i
  )
}))

start_period <- "2019K2"
end_period <- "2023K2"

real_bvp_df_trimmed <- real_bvp_df[real_bvp_df$Time >= start_period & real_bvp_df$Time <= end_period, ]
all_rf_min_forecasts_shifted_filtered <- all_rf_min_forecasts_shifted[
  all_rf_min_forecasts_shifted$Time >= start_period & all_rf_min_forecasts_shifted$Time <= end_period, ]


gr11<-ggplot() +
  geom_line(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), linewidth = 1.2) +
  geom_point(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), size = 2) +
  geom_line(data = subset(all_rf_min_forecasts_shifted, Type == "Random Forest su Lasso"),
            aes(x = Time, y = Forecast, group = Window, color = interaction(Type, Window)),
            alpha = 0.6, linewidth = 0.9) +
  labs(
    title = "BVP prognozių palyginimas su Random Forest",
    x = "Ketvirtis", y = "BVP", color = "Modelis ir langas"
  ) +
  scale_color_manual(
    values = c("Reali reikšmė" = "darkgreen"),
    guide = "none"
  ) +
  scale_color_viridis_d(option = "plasma", begin = 0, end = 0.85, direction = -1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
gr11

forecast_shifted_df_lasso<-all_rf_min_forecasts_shifted
forecast_shifted_df_lasso$Horizon <- rep(1:forecast_horizon, times = length(unique(forecast_shifted_df_lasso$Window)))
forecast_shifted_df_lasso <- do.call(rbind, lapply(valid_rf_lasso, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_rf_min[[i]],
    Horizon = 1:forecast_horizon,
    Window = i
  )
}))
forecast_shifted_df_lasso
num_windows <- length(unique(forecast_shifted_df_lasso$Horizon))

blue_shades <- viridis(num_windows, option = "plasma", begin = 0, end = 0.9, direction = -1)

k15 <- ggplot() +
  geom_line(data = real_bvp_df_trimmed,
            aes(x = Time, y = Real, color = "Realybė", group = 1),
            size = 1.2) +
  geom_point(data = real_bvp_df_trimmed,
             aes(x = Time, y = Real, color = "Realybė")) +
  
  geom_line(data = forecast_shifted_df_lasso,
            aes(x = Time, y = Forecast, group = Horizon, color = as.factor(Horizon)),
            size = 1.2, linetype = "dashed") +
  geom_point(data = forecast_shifted_df_lasso,
             aes(x = Time, y = Forecast, color = as.factor(Horizon))) +
  
  scale_color_manual(
    values = c(setNames(blue_shades, as.character(unique(forecast_shifted_df_lasso$Horizon))),
               "Realybė" = "red"),
    guide = guide_legend(override.aes = list(linetype = c(rep("dashed", num_windows), "solid"),
                                             size = c(rep(1.2, num_windows), 1.2)), title = "Horizontai")
  ) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )+
  labs(title = "BVP prognozės (Random Forest ir LASSO) ir realybė pagal horizontus",
       x = "Kvartalas",
       y = "BVP",
       color = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold")) 

ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs41.png", plot = k15, width = 10, height = 6, dpi = 300, bg = "white")



forecast_shifted_df_lasso <- merge(forecast_shifted_df_lasso, real_bvp_df_trimmed, by = "Time")
rmse_post_by_horizon <- aggregate(cbind(Forecast, Real) ~ Horizon, data = forecast_shifted_df_lasso, FUN = function(x) x)
rmse_post_by_horizon$RMSE <- sapply(unique(forecast_shifted_df_lasso$Horizon), function(h) {
  rmse_vals <- forecast_shifted_df_lasso[forecast_shifted_df_lasso$Horizon == h, ]
  rmse(rmse_vals$Real, rmse_vals$Forecast)
})
print(rmse_post_by_horizon[, c("Horizon", "RMSE")])

err_rf_las_19_23 <- errors_by_h(forecast_shifted_df_lasso)


rmse_plot_df <- data.frame(
  Horizon = rep(1:forecast_horizon, 2),
  RMSE = rmse_post_by_horizon$RMSE,
  Model = rep("Random Forest", each = forecast_horizon)
)


rfl2 <- ggplot(rmse_plot_df, aes(x = Horizon, y = RMSE, colour = Model)) +
  geom_line(size = 1.3, alpha = 0.9) +
  geom_point(,size = 3) +
  scale_color_manual(values = c("Random Forest" = "red"), name = "")+
  labs(
    title = "2019K2-2023K2",
    x = "Horizontas",
    y = "RMSE",
    color = "Modelis"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )

###########################################################################################################################
##################################################################Random forest su LASSO 2019k1-2024k4############################
library(tidyfit)
library(pbapply)
library(forecast)
library(zoo)
library(glmnet)
library(randomForest)
#lango dydis
window_size <- 78
forecast_horizon <- 9 #prognozes

target_time <- "2024K4"
max_index <- which(stationary_table$Time == target_time)
max_index

#visa pradine lentele
X <- as.matrix(stationary_table[, -c(1, 2)])
#bvp
y <- as.numeric(unlist(stationary_table[, 2]))


set.seed(123)

final_forecasts_rf_min <- list()

for (i in 1:(max_index - window_size + 1)) {
  X_window <- X[i:(i + window_size - 1), ]  #lango dydzio x'ai
  y_window <- y[i:(i + window_size - 1)]   #lango dydzio y'ai
  
  lasso_model <- cv.glmnet(X_window, y_window, family = "gaussian", alpha = 1)
  #plot(lasso_model)
  support_vars_min <- which(coef(lasso_model, s = "lambda.min")[-1,1] != 0)
  #names(support_vars_min)
  #su lambda min
  if (length(support_vars_min) > 0) {
    #kintamieji su lambda min
    X_window_selected_min <- X_window[, support_vars_min, drop = FALSE]
    colnames(X_window_selected_min)
    selected_var_names <- colnames(X_window_selected_min) #su lambda min
    rf_model <- randomForest(X_window_selected_min, y_window, ntree = 10000)
    #sort(importance(rf_model))
    #arima
    arima_forecasts <- list()
    for (j in 1:ncol(X_window_selected_min)) {
      model <- auto.arima(X_window_selected_min[, j])
      arima_forecasts[[j]] <- as.numeric(forecast(model, h = forecast_horizon)$mean)
      #print(arima_forecasts)
    }
    if (length(arima_forecasts) > 0) {
      arima_forecasts_matrix <- do.call(cbind, arima_forecasts)
      #arima_forecasts_matrix
      arima_forecasts_df <- as.data.frame(arima_forecasts_matrix)
      colnames(arima_forecasts_df) <- selected_var_names
      
      predictions_rf <- predict(rf_model, newdata = arima_forecasts_df)
      #predictions_rf
      
      final_forecasts_rf_min[[i]] <- predictions_rf
    }
  }
  else{
    coef_lasso_min <- coef(lasso_model, s = lambda_n(lasso_model, N = 10))
    support_vars_min <- which(coef_lasso_min[-1, 1] != 0)
    X_window_selected_min <- X_window[, support_vars_min, drop = FALSE]
    colnames(X_window_selected_min)
    selected_var_names <- colnames(X_window_selected_min) #su lambda min
    rf_model <- randomForest(X_window_selected_min, y_window, ntree = 10000)
    arima_forecasts <- list()
    for (j in 1:ncol(X_window_selected_min)) {
      model <- auto.arima(X_window_selected_min[, j])
      arima_forecasts[[j]] <- as.numeric(forecast(model, h = forecast_horizon)$mean)
      #print(arima_forecasts)
    }
    if (length(arima_forecasts) > 0) {
      arima_forecasts_matrix <- do.call(cbind, arima_forecasts)
      #arima_forecasts_matrix
      arima_forecasts_df <- as.data.frame(arima_forecasts_matrix)
      colnames(arima_forecasts_df) <- selected_var_names
      
      predictions_rf <- predict(rf_model, newdata = arima_forecasts_df)
      #predictions_rf
      
      final_forecasts_rf_min[[i]] <- predictions_rf
    }
  }
}
print(final_forecasts_rf_min)

first_two_columns<-stationary_table[,1:2]
first_two_columns

valid_rf_lasso <- which((window_size + seq_along(final_forecasts_rf_min) + forecast_horizon - 1) <= 300)
valid_rf_lasso

max_time_index <- max(window_size + valid_rf_lasso + forecast_horizon - 1)

#kiek eilučių reikia
n_needed <- max_time_index - nrow(first_two_columns)

#jei reikia pridėti eilutes
if (n_needed > 0) {
  last_time <- first_two_columns$Time[nrow(first_two_columns)]
  new_times <- generate_quarters(last_time, n_needed)
  new_rows <- as.data.frame(matrix(NA, nrow = n_needed, ncol = ncol(first_two_columns)))
  colnames(new_rows) <- colnames(first_two_columns)
  new_rows$Time <- new_times
  first_two_columns <- rbind(first_two_columns, new_rows)
}

# Random Forest — prognozės kiekvienam langui
all_rf_min_forecasts_shifted <- do.call(rbind, lapply(valid_rf_lasso, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_rf_min[[i]],
    Type = "Random Forest su Lasso",
    Window = i
  )
}))


#realus bvp su df
real_bvp_df <- data.frame(
  Time = first_two_columns$Time,
  Real = first_two_columns$BVP
)

#apkerpam laika
real_bvp_df_trimmed <- real_bvp_df[(window_size + 1):nrow(real_bvp_df), ]

gr11<-ggplot() +
  geom_line(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), linewidth = 1.2) +
  geom_point(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), size = 2) +
  geom_line(data = subset(all_rf_min_forecasts_shifted, Type == "Random Forest su Lasso"),
            aes(x = Time, y = Forecast, group = Window, color = interaction(Type, Window)),
            alpha = 0.6, linewidth = 0.9) +
  labs(
    title = "BVP prognozių palyginimas su Random Forest",
    x = "Ketvirtis", y = "BVP", color = "Modelis ir langas"
  ) +
  scale_color_manual(
    values = c("Reali reikšmė" = "darkgreen"),
    guide = "none"
  ) +
  scale_color_viridis_d(option = "plasma", begin = 0, end = 0.85, direction = -1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
gr11

forecast_shifted_df_lasso<-all_rf_min_forecasts_shifted
forecast_shifted_df_lasso$Horizon <- rep(1:forecast_horizon, times = length(unique(forecast_shifted_df_lasso$Window)))
forecast_shifted_df_lasso <- do.call(rbind, lapply(valid_rf_lasso, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_rf_min[[i]],
    Horizon = 1:forecast_horizon,
    Window = i
  )
}))
forecast_shifted_df_lasso
num_windows <- length(unique(forecast_shifted_df_lasso$Horizon))

blue_shades <- viridis(num_windows, option = "plasma", begin = 0, end = 0.9, direction = -1)

k17 <- ggplot() +
  geom_line(data = real_bvp_df_trimmed,
            aes(x = Time, y = Real, color = "Realybė", group = 1),
            size = 1.2) +
  geom_point(data = real_bvp_df_trimmed,
             aes(x = Time, y = Real, color = "Realybė")) +
  
  geom_line(data = forecast_shifted_df_lasso,
            aes(x = Time, y = Forecast, group = Horizon, color = as.factor(Horizon)),
            size = 1.2, linetype = "dashed") +
  geom_point(data = forecast_shifted_df_lasso,
             aes(x = Time, y = Forecast, color = as.factor(Horizon))) +
  
  scale_color_manual(
    values = c(setNames(blue_shades, as.character(unique(forecast_shifted_df_lasso$Horizon))),
               "Realybė" = "red"),
    guide = guide_legend(override.aes = list(linetype = c(rep("dashed", num_windows), "solid"),
                                             size = c(rep(1.2, num_windows), 1.2)), title = "Horizontai")
  ) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )+
  labs(title = "BVP prognozės (Random Forest ir LASSO) ir realybė pagal horizontus",
       x = "Kvartalas",
       y = "BVP",
       color = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold")) 

ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs42.png", plot = k17, width = 10, height = 6, dpi = 300, bg = "white")



forecast_shifted_df_lasso <- merge(forecast_shifted_df_lasso, real_bvp_df_trimmed, by = "Time")
forecast_shifted_df_lasso <- forecast_shifted_df_lasso[!is.na(forecast_shifted_df_lasso$Real),]
rmse_post_by_horizon <- aggregate(cbind(Forecast, Real) ~ Horizon, data = forecast_shifted_df_lasso, FUN = function(x) x)
rmse_post_by_horizon$RMSE <- sapply(unique(forecast_shifted_df_lasso$Horizon), function(h) {
  rmse_vals <- forecast_shifted_df_lasso[forecast_shifted_df_lasso$Horizon == h, ]
  rmse(rmse_vals$Real, rmse_vals$Forecast)
})
print(rmse_post_by_horizon[, c("Horizon", "RMSE")])

err_rf_las_21_27 <- errors_by_h(forecast_shifted_df_lasso)


rmse_plot_df <- data.frame(
  Horizon = rep(1:forecast_horizon, 2),
  RMSE = rmse_post_by_horizon$RMSE,
  Model = rep("Random Forest", each = forecast_horizon)
)





rfl3 <- ggplot(rmse_plot_df, aes(x = Horizon, y = RMSE, colour = Model)) +
  geom_line(size = 1.3, alpha = 0.9) +
  geom_point(,size = 3) +
  scale_color_manual(values = c("Random Forest" = "red"), name = "")+
  scale_x_continuous(labels = number_format(accuracy = 1)) + 
  labs(
    title = "2019K1-2024K4",
    x = "Horizontas",
    y = "RMSE",
    color = "Modelis"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )



top_row <- rfl1 + rfl2

# Центруем нижний график между пустыми местами
bottom_row <- plot_spacer() + rfl3 + plot_spacer()
bottom_row <- bottom_row + plot_layout(widths = c(1, 2, 1))  # Центрируем RMSE

# Объединяем всё и добавляем заголовок и общую легенду
final_plot <- (top_row / bottom_row) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

final_plot <- final_plot +
  plot_layout(guides = "collect") +   
  plot_annotation(
    title = "RMSE pagal horizontus",
    theme = theme(
      plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),legend.position = "bottom"  
    )
  )

# Визуализация
x11()
final_plot
ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs43.png", plot = final_plot, width = 10, height = 6, dpi = 300, bg = "white")


##########################################################################################################################
#############################################################################################################################
#########################################################RANDOM FOREST SU RANDOM FOREST 2017k2-2019k2###########################
library(tidyfit)
library(pbapply)
library(forecast)
library(zoo)
library(glmnet)
library(randomForest)
#lango dydis
window_size <- 71
forecast_horizon <- 8 #prognozes

target_time <- "2019K2"
max_index <- which(stationary_table$Time == target_time)


#visa pradine lentele
X <- as.matrix(stationary_table[, -c(1, 2)])
#bvp
y <- as.numeric(unlist(stationary_table[, 2]))

str(X)

set.seed(123)

final_forecasts_rf_rf <- list()

for (i in 1:(max_index - window_size + 1)) {
  X_window <- X[i:(i + window_size - 1), ]  #lango dydzio x'ai
  y_window <- y[i:(i + window_size - 1)]   #lango dydzio y'ai
  
  rf_importance_model <- randomForest(X_window, y_window, ntree = 1000, importance = TRUE)
  importance_values <- importance(rf_importance_model, type = 1)[, 1]  # %IncMSE
  
  top_vars <- names(sort(importance_values, decreasing = TRUE))[1:min(20, length(importance_values))]
  X_window_selected <- X_window[, top_vars, drop = FALSE]
  
  rf_model <- randomForest(X_window_selected, y_window, ntree = 10000)
  #arima
  arima_forecasts <- list()
  for (j in 1:ncol(X_window_selected)) {
    model <- auto.arima(X_window_selected[, j])
    arima_forecasts[[j]] <- as.numeric(forecast(model, h = forecast_horizon)$mean)
  }
  if (length(arima_forecasts) > 0) {
    arima_forecasts_matrix <- do.call(rbind, arima_forecasts)
    arima_forecasts_df <- as.data.frame(t(arima_forecasts_matrix))
    colnames(arima_forecasts_df) <- top_vars
    
    predictions_rf <- predict(rf_model, newdata = arima_forecasts_df)
    final_forecasts_rf_rf[[i]] <- predictions_rf
  }
}
print(final_forecasts_rf_rf)

valid_rf_rf <- which((window_size + seq_along(final_forecasts_rf_rf) + forecast_horizon - 1) <= nrow(first_two_columns))

# Random Forest — prognozės kiekvienam langui
all_rf_rf_forecasts_shifted <- do.call(rbind, lapply(valid_rf_rf, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_rf_rf[[i]],
    Type = "Random Forest su RF",
    Window = i
  )
}))

start_period <- "2017K2"
end_period <- "2021K2"

real_bvp_df_trimmed <- real_bvp_df[real_bvp_df$Time >= start_period & real_bvp_df$Time <= end_period, ]


gr13<-ggplot() +
  geom_line(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), linewidth = 1.2) +
  geom_point(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), size = 2) +
  geom_line(data = subset(all_rf_rf_forecasts_shifted, Type == "Random Forest su RF"),
            aes(x = Time, y = Forecast, group = Window, color = interaction(Type, Window)),
            alpha = 0.6, linewidth = 0.9) +
  labs(
    title = "BVP prognozių palyginimas su Random Forest",
    x = "Ketvirtis", y = "BVP", color = "Modelis ir langas"
  ) +
  scale_color_manual(
    values = c("Reali reikšmė" = "darkgreen"),
    guide = "none"
  ) +
  scale_color_viridis_d(option = "plasma", begin = 0, end = 0.85, direction = -1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
gr13
forecast_shifted_df_rf_rf<-all_rf_rf_forecasts_shifted
forecast_shifted_df_rf_rf$Horizon <- rep(1:forecast_horizon, times = length(unique(forecast_shifted_df_rf_rf$Window)))
forecast_shifted_df_rf_rf <- do.call(rbind, lapply(valid_rf_rf, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_rf_rf[[i]],
    Horizon = 1:forecast_horizon,
    Window = i
  )
}))
forecast_shifted_df_rf_rf
num_windows <- length(unique(forecast_shifted_df_rf_rf$Horizon))

blue_shades <- viridis(num_windows, option = "plasma", begin = 0, end = 0.9, direction = -1)

k18 <- ggplot() +
  geom_line(data = real_bvp_df_trimmed,
            aes(x = Time, y = Real, color = "Realybė", group = 1),
            size = 1.2) +
  geom_point(data = real_bvp_df_trimmed,
             aes(x = Time, y = Real, color = "Realybė")) +
  
  geom_line(data = forecast_shifted_df_rf_rf,
            aes(x = Time, y = Forecast, group = Horizon, color = as.factor(Horizon)),
            size = 1.2, linetype = "dashed") +
  geom_point(data = forecast_shifted_df_rf_rf,
             aes(x = Time, y = Forecast, color = as.factor(Horizon))) +
  
  scale_color_manual(
    values = c(setNames(blue_shades, as.character(unique(forecast_shifted_df_rf_rf$Horizon))),
               "Realybė" = "red"),
    guide = guide_legend(override.aes = list(linetype = c(rep("dashed", num_windows), "solid"),
                                             size = c(rep(1.2, num_windows), 1.2)), title = "Horizontai")
  ) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )+
  labs(title = "BVP prognozės (Random Forest su vidine svarbos metrika) ir realybė pagal horizontus",
       x = "Kvartalas",
       y = "BVP",
       color = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold")) 

ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs44.png", plot = k18, width = 10, height = 6, dpi = 300, bg = "white")



forecast_shifted_df_rf_rf <- merge(forecast_shifted_df_rf_rf, real_bvp_df_trimmed, by = "Time")
rmse_post_by_horizon <- aggregate(cbind(Forecast, Real) ~ Horizon, data = forecast_shifted_df_rf_rf, FUN = function(x) x)
rmse_post_by_horizon$RMSE <- sapply(unique(forecast_shifted_df_rf_rf$Horizon), function(h) {
  rmse_vals <- forecast_shifted_df_rf_rf[forecast_shifted_df_rf_rf$Horizon == h, ]
  rmse(rmse_vals$Real, rmse_vals$Forecast)
})
print(rmse_post_by_horizon[, c("Horizon", "RMSE")])

err_rf_rf_17_21 <- errors_by_h(forecast_shifted_df_rf_rf)


rmse_plot_df <- data.frame(
  Horizon = rep(1:forecast_horizon, 2),
  RMSE = rmse_post_by_horizon$RMSE,
  Model = rep("Random Forest", each = forecast_horizon)
)


rfr1 <- ggplot(rmse_plot_df, aes(x = Horizon, y = RMSE, colour = Model)) +
  geom_line(size = 1.3, alpha = 0.9) +
  geom_point(,size = 3) +
  scale_color_manual(values = c("Random Forest" = "red"), name = "")+
  scale_x_continuous(labels = number_format(accuracy = 1)) + 
  labs(
    title = "2017K2-2021K2",
    x = "Horizontas",
    y = "RMSE",
    color = "Modelis"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )


#########################################################RANDOM FOREST SU RANDOM FOREST 2019k1-2021k1###########################
library(tidyfit)
library(pbapply)
library(forecast)
library(zoo)
library(glmnet)
library(randomForest)
#lango dydis
window_size <- 79
forecast_horizon <- 8 #prognozes

target_time <- "2021K2"
max_index <- which(stationary_table$Time == target_time)


#visa pradine lentele
X <- as.matrix(stationary_table[, -c(1, 2)])
#bvp
y <- as.numeric(unlist(stationary_table[, 2]))

str(X)

set.seed(123)

final_forecasts_rf_rf <- list()

for (i in 1:(max_index - window_size + 1)) {
  X_window <- X[i:(i + window_size - 1), ]  #lango dydzio x'ai
  y_window <- y[i:(i + window_size - 1)]   #lango dydzio y'ai
  
  rf_importance_model <- randomForest(X_window, y_window, ntree = 1000, importance = TRUE)
  importance_values <- importance(rf_importance_model, type = 1)[, 1]  # %IncMSE
  
  top_vars <- names(sort(importance_values, decreasing = TRUE))[1:min(20, length(importance_values))]
  X_window_selected <- X_window[, top_vars, drop = FALSE]
  
  rf_model <- randomForest(X_window_selected, y_window, ntree = 10000)
  #arima
  arima_forecasts <- list()
  for (j in 1:ncol(X_window_selected)) {
    model <- auto.arima(X_window_selected[, j])
    arima_forecasts[[j]] <- as.numeric(forecast(model, h = forecast_horizon)$mean)
  }
  if (length(arima_forecasts) > 0) {
    arima_forecasts_matrix <- do.call(rbind, arima_forecasts)
    arima_forecasts_df <- as.data.frame(t(arima_forecasts_matrix))
    colnames(arima_forecasts_df) <- top_vars
    
    predictions_rf <- predict(rf_model, newdata = arima_forecasts_df)
    final_forecasts_rf_rf[[i]] <- predictions_rf
  }
}
print(final_forecasts_rf_rf)

valid_rf_rf <- which((window_size + seq_along(final_forecasts_rf_rf) + forecast_horizon - 1) <= nrow(first_two_columns))

# Random Forest — prognozės kiekvienam langui
all_rf_rf_forecasts_shifted <- do.call(rbind, lapply(valid_rf_rf, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_rf_rf[[i]],
    Type = "Random Forest su RF",
    Window = i
  )
}))

start_period <- "2019K2"
end_period <- "2023K2"

real_bvp_df_trimmed <- real_bvp_df[real_bvp_df$Time >= start_period & real_bvp_df$Time <= end_period, ]


gr17<-ggplot() +
  geom_line(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), linewidth = 1.2) +
  geom_point(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), size = 2) +
  geom_line(data = subset(all_rf_rf_forecasts_shifted, Type == "Random Forest su RF"),
            aes(x = Time, y = Forecast, group = Window, color = interaction(Type, Window)),
            alpha = 0.6, linewidth = 0.9) +
  labs(
    title = "BVP prognozių palyginimas su Random Forest",
    x = "Ketvirtis", y = "BVP", color = "Modelis ir langas"
  ) +
  scale_color_manual(
    values = c("Reali reikšmė" = "darkgreen"),
    guide = "none"
  ) +
  scale_color_viridis_d(option = "plasma", begin = 0, end = 0.85, direction = -1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
gr17
forecast_shifted_df_rf_rf<-all_rf_rf_forecasts_shifted
forecast_shifted_df_rf_rf$Horizon <- rep(1:forecast_horizon, times = length(unique(forecast_shifted_df_rf_rf$Window)))
forecast_shifted_df_rf_rf <- do.call(rbind, lapply(valid_rf_rf, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_rf_rf[[i]],
    Horizon = 1:forecast_horizon,
    Window = i
  )
}))
forecast_shifted_df_rf_rf
num_windows <- length(unique(forecast_shifted_df_rf_rf$Horizon))

blue_shades <- viridis(num_windows, option = "plasma", begin = 0, end = 0.9, direction = -1)

k19 <- ggplot() +
  geom_line(data = real_bvp_df_trimmed,
            aes(x = Time, y = Real, color = "Realybė", group = 1),
            size = 1.2) +
  geom_point(data = real_bvp_df_trimmed,
             aes(x = Time, y = Real, color = "Realybė")) +
  
  geom_line(data = forecast_shifted_df_rf_rf,
            aes(x = Time, y = Forecast, group = Horizon, color = as.factor(Horizon)),
            size = 1.2, linetype = "dashed") +
  geom_point(data = forecast_shifted_df_rf_rf,
             aes(x = Time, y = Forecast, color = as.factor(Horizon))) +
  
  scale_color_manual(
    values = c(setNames(blue_shades, as.character(unique(forecast_shifted_df_rf_rf$Horizon))),
               "Realybė" = "red"),
    guide = guide_legend(override.aes = list(linetype = c(rep("dashed", num_windows), "solid"),
                                             size = c(rep(1.2, num_windows), 1.2)), title = "Horizontai")
  ) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )+
  labs(title = "BVP prognozės (Random Forest su vidine svarbos metrika) ir realybė pagal horizontus",
       x = "Kvartalas",
       y = "BVP",
       color = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold")) 

ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs45.png", plot = k19, width = 10, height = 6, dpi = 300, bg = "white")



forecast_shifted_df_rf_rf <- merge(forecast_shifted_df_rf_rf, real_bvp_df_trimmed, by = "Time")
rmse_post_by_horizon <- aggregate(cbind(Forecast, Real) ~ Horizon, data = forecast_shifted_df_rf_rf, FUN = function(x) x)
rmse_post_by_horizon$RMSE <- sapply(unique(forecast_shifted_df_rf_rf$Horizon), function(h) {
  rmse_vals <- forecast_shifted_df_rf_rf[forecast_shifted_df_rf_rf$Horizon == h, ]
  rmse(rmse_vals$Real, rmse_vals$Forecast)
})
print(rmse_post_by_horizon[, c("Horizon", "RMSE")])

err_rf_rf_19_23 <- errors_by_h(forecast_shifted_df_rf_rf)


rmse_plot_df <- data.frame(
  Horizon = rep(1:forecast_horizon, 2),
  RMSE = rmse_post_by_horizon$RMSE,
  Model = rep("Random Forest", each = forecast_horizon)
)


rfr2 <- ggplot(rmse_plot_df, aes(x = Horizon, y = RMSE, colour = Model)) +
  geom_line(size = 1.3, alpha = 0.9) +
  geom_point(,size = 3) +
  scale_color_manual(values = c("Random Forest" = "red"), name = "")+
  scale_x_continuous(labels = number_format(accuracy = 1)) + 
  labs(
    title = "2019K2-2023K2",
    x = "Horizontas",
    y = "RMSE",
    color = "Modelis"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )

#####################################################################################################################
######################################################################################################################
######################################################################Random Forest su RF 2025-2027##################
library(tidyfit)
library(pbapply)
library(forecast)
library(zoo)
library(glmnet)
library(randomForest)
#lango dydis
window_size <- 78
forecast_horizon <- 9 #prognozes

target_time <- "2024K4"
max_index <- which(stationary_table$Time == target_time)


#visa pradine lentele
X <- as.matrix(stationary_table[, -c(1, 2)])
#bvp
y <- as.numeric(unlist(stationary_table[, 2]))

str(X)

set.seed(123)

final_forecasts_rf_rf <- list()

for (i in 1:(max_index - window_size + 1)) {
  X_window <- X[i:(i + window_size - 1), ]  #lango dydzio x'ai
  y_window <- y[i:(i + window_size - 1)]   #lango dydzio y'ai
  
  rf_importance_model <- randomForest(X_window, y_window, ntree = 1000, importance = TRUE)
  importance_values <- importance(rf_importance_model, type = 1)[, 1]  # %IncMSE
  
  top_vars <- names(sort(importance_values, decreasing = TRUE))[1:min(20, length(importance_values))]
  X_window_selected <- X_window[, top_vars, drop = FALSE]
  
  rf_model <- randomForest(X_window_selected, y_window, ntree = 10000)
  #arima
  arima_forecasts <- list()
  for (j in 1:ncol(X_window_selected)) {
    model <- auto.arima(X_window_selected[, j])
    arima_forecasts[[j]] <- as.numeric(forecast(model, h = forecast_horizon)$mean)
  }
  if (length(arima_forecasts) > 0) {
    arima_forecasts_matrix <- do.call(rbind, arima_forecasts)
    arima_forecasts_df <- as.data.frame(t(arima_forecasts_matrix))
    colnames(arima_forecasts_df) <- top_vars
    
    predictions_rf <- predict(rf_model, newdata = arima_forecasts_df)
    final_forecasts_rf_rf[[i]] <- predictions_rf
  }
}
print(final_forecasts_rf_rf)
first_two_columns<-stationary_table[,1:2]
first_two_columns
valid_rf_rf <- which((window_size + seq_along(final_forecasts_rf_rf) + forecast_horizon - 1) <= 200)
max_time_index <- max(
  window_size + valid_rf_rf + forecast_horizon - 1)

#kiek eilučių reikia
n_needed <- max_time_index - nrow(first_two_columns)

#jei reikia pridėti eilutes
if (n_needed > 0) {
  last_time <- first_two_columns$Time[nrow(first_two_columns)]
  new_times <- generate_quarters(last_time, n_needed)
  new_rows <- as.data.frame(matrix(NA, nrow = n_needed, ncol = ncol(first_two_columns)))
  colnames(new_rows) <- colnames(first_two_columns)
  new_rows$Time <- new_times
  first_two_columns <- rbind(first_two_columns, new_rows)
}
# Random Forest — prognozės kiekvienam langui
all_rf_rf_forecasts_shifted <- do.call(rbind, lapply(valid_rf_rf, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_rf_rf[[i]],
    Type = "Random Forest su RF",
    Window = i
  )
}))

real_bvp_df <- data.frame(
  Time = first_two_columns$Time,
  Real = first_two_columns$BVP
)

#apkerpam laika
real_bvp_df_trimmed <- real_bvp_df[(window_size + 1):nrow(real_bvp_df), ]


gr17<-ggplot() +
  geom_line(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), linewidth = 1.2) +
  geom_point(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), size = 2) +
  geom_line(data = subset(all_rf_rf_forecasts_shifted, Type == "Random Forest su RF"),
            aes(x = Time, y = Forecast, group = Window, color = interaction(Type, Window)),
            alpha = 0.6, linewidth = 0.9) +
  labs(
    title = "BVP prognozių palyginimas su Random Forest",
    x = "Ketvirtis", y = "BVP", color = "Modelis ir langas"
  ) +
  scale_color_manual(
    values = c("Reali reikšmė" = "darkgreen"),
    guide = "none"
  ) +
  scale_color_viridis_d(option = "plasma", begin = 0, end = 0.85, direction = -1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
gr17
forecast_shifted_df_rf_rf<-all_rf_rf_forecasts_shifted
forecast_shifted_df_rf_rf$Horizon <- rep(1:forecast_horizon, times = length(unique(forecast_shifted_df_rf_rf$Window)))
forecast_shifted_df_rf_rf <- do.call(rbind, lapply(valid_rf_rf, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_rf_rf[[i]],
    Horizon = 1:forecast_horizon,
    Window = i
  )
}))
forecast_shifted_df_rf_rf
num_windows <- length(unique(forecast_shifted_df_rf_rf$Horizon))

blue_shades <- viridis(num_windows, option = "plasma", begin = 0, end = 0.9, direction = -1)

k20 <- ggplot() +
  geom_line(data = real_bvp_df_trimmed,
            aes(x = Time, y = Real, color = "Realybė", group = 1),
            size = 1.2) +
  geom_point(data = real_bvp_df_trimmed,
             aes(x = Time, y = Real, color = "Realybė")) +
  
  geom_line(data = forecast_shifted_df_rf_rf,
            aes(x = Time, y = Forecast, group = Horizon, color = as.factor(Horizon)),
            size = 1.2, linetype = "dashed") +
  geom_point(data = forecast_shifted_df_rf_rf,
             aes(x = Time, y = Forecast, color = as.factor(Horizon))) +
  
  scale_color_manual(
    values = c(setNames(blue_shades, as.character(unique(forecast_shifted_df_rf_rf$Horizon))),
               "Realybė" = "red"),
    guide = guide_legend(override.aes = list(linetype = c(rep("dashed", num_windows), "solid"),
                                             size = c(rep(1.2, num_windows), 1.2)), title = "Horizontai")
  ) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )+
  labs(title = "BVP prognozės (Random Forest su vidine svarbos metrika) ir realybė pagal horizontus",
       x = "Kvartalas",
       y = "BVP",
       color = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold")) 

ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs46.png", plot = k20, width = 10, height = 6, dpi = 300, bg = "white")



forecast_shifted_df_rf_rf <- merge(forecast_shifted_df_rf_rf, real_bvp_df_trimmed, by = "Time")
forecast_shifted_df_rf_rf <- forecast_shifted_df_rf_rf[!is.na(forecast_shifted_df_rf_rf$Real),]

rmse_post_by_horizon <- aggregate(cbind(Forecast, Real) ~ Horizon, data = forecast_shifted_df_rf_rf, FUN = function(x) x)
rmse_post_by_horizon$RMSE <- sapply(unique(forecast_shifted_df_rf_rf$Horizon), function(h) {
  rmse_vals <- forecast_shifted_df_rf_rf[forecast_shifted_df_rf_rf$Horizon == h, ]
  rmse(rmse_vals$Real, rmse_vals$Forecast)
})
print(rmse_post_by_horizon[, c("Horizon", "RMSE")])

err_rf_rf_21_27 <- errors_by_h(forecast_shifted_df_rf_rf)


rmse_plot_df <- data.frame(
  Horizon = rep(1:forecast_horizon, 2),
  RMSE = rmse_post_by_horizon$RMSE,
  Model = rep("Random Forest", each = forecast_horizon)
)


rfr3 <- ggplot(rmse_plot_df, aes(x = Horizon, y = RMSE, colour = Model)) +
  geom_line(size = 1.3, alpha = 0.9) +
  geom_point(,size = 3) +
  scale_color_manual(values = c("Random Forest" = "red"), name = "")+
  scale_x_continuous(labels = number_format(accuracy = 1)) + 
  labs(
    title = "2019K1-2024K4",
    x = "Horizontas",
    y = "RMSE",
    color = "Modelis"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )


top_row <- rfr1 + rfr2

bottom_row <- plot_spacer() + rfr3 + plot_spacer()
bottom_row <- bottom_row + plot_layout(widths = c(1, 2, 1)) 

final_plot <- (top_row / bottom_row) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

final_plot <- final_plot +
  plot_layout(guides = "collect") +   
  plot_annotation(
    title = "RMSE pagal horizontus",
    theme = theme(
      plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),legend.position = "bottom"  
    )
  )

x11()
final_plot
ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs47.png", plot = final_plot, width = 10, height = 6, dpi = 300, bg = "white")


  

#########################################################################################################################
########################################################Random forest su XGBOOST#####################################################
#####################################################2021-2027#####################################################
library(tidyfit)
library(pbapply)
library(forecast)
library(zoo)
library(glmnet)
library(randomForest)
library(xgboost)
library(caret)

window_size <- 78
forecast_horizon <- 9
target_time <- "2024K4"
max_index <- which(stationary_table$Time == target_time)

X <- as.matrix(stationary_table[, -c(1, 2)])
y <- as.numeric(unlist(stationary_table[, 2]))

set.seed(123)
final_forecasts_rf_xgb <- list()
final_forecasts_rf_xgb_param <- list()
for (i in 1:(max_index - window_size + 1)) {
  X_window <- X[i:(i + window_size - 1), ]
  y_window <- y[i:(i + window_size - 1)]
  
  rf_importance_model <- randomForest(X_window, y_window, ntree = 1000, importance = TRUE)
  importance_values <- importance(rf_importance_model, type = 1)[, 1]
  
  top_vars <- names(sort(importance_values, decreasing = TRUE))[1:min(20, length(importance_values))]
  X_window_selected <- X_window[, top_vars, drop = FALSE]
  
  xgb_grid <- expand.grid(
    nrounds = c(100, 150, 200, 300, 500, 1000),
    max_depth = c(3, 5, 8, 10, 15, 20, 50),
    eta = c(0.05, 0.1, 0.15, 0.2, 0.3),
    gamma = 0,
    colsample_bytree = 0.8,
    min_child_weight = 1,
    subsample = 0.8
  )
  
  train_control <- trainControl(
    method = "timeslice",
    initialWindow = 60,
    horizon = 9,
    fixedWindow = TRUE,
    verboseIter = FALSE
  )
  
  xgb_model <- train(
    x = as.data.frame(X_window_selected),
    y = y_window,
    method = "xgbTree",
    trControl = train_control,
    tuneGrid = xgb_grid,
    verbose = FALSE
  )
  
  arima_forecasts <- list()
  for (j in 1:ncol(X_window_selected)) {
    model <- auto.arima(X_window_selected[, j])
    arima_forecasts[[j]] <- as.numeric(forecast(model, h = forecast_horizon)$mean)
  }
  
  if (length(arima_forecasts) > 0) {
    arima_forecasts_matrix <- do.call(rbind, arima_forecasts)
    arima_forecasts_df <- as.data.frame(t(arima_forecasts_matrix))
    colnames(arima_forecasts_df) <- top_vars
    
    predictions_xgb <- predict(xgb_model, newdata = arima_forecasts_df)
    final_forecasts_rf_xgb[[i]] <- predictions_xgb
    final_forecasts_rf_xgb_param[[i]] <- list(
      forecast = predictions_xgb,
      best_params = xgb_model$bestTune
    )
  }
}
best_params <- xgb_model$bestTune
print(best_params)
xgb_model$results
ggplot(xgb_model$results, aes(x = nrounds, y = RMSE, color = factor(max_depth))) +
  geom_line() +
  facet_wrap(~ eta, scales = "free") +
  theme_minimal()


print(final_forecasts_rf_xgb)

first_two_columns <- stationary_table[, 1:2]
valid_rf_xgb <- which((window_size + seq_along(final_forecasts_rf_xgb) + forecast_horizon - 1) <= 200)
max_time_index <- max(window_size + valid_rf_xgb + forecast_horizon - 1)
n_needed <- max_time_index - nrow(first_two_columns)

#jei reikia pridėti eilutes
if (n_needed > 0) {
  last_time <- first_two_columns$Time[nrow(first_two_columns)]
  new_times <- generate_quarters(last_time, n_needed)
  new_rows <- as.data.frame(matrix(NA, nrow = n_needed, ncol = ncol(first_two_columns)))
  colnames(new_rows) <- colnames(first_two_columns)
  new_rows$Time <- new_times
  first_two_columns <- rbind(first_two_columns, new_rows)
}
# Random Forest — prognozės kiekvienam langui
all_rf_xgb_forecasts_shifted <- do.call(rbind, lapply(valid_rf_xgb, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_rf_xgb[[i]],
    Type = "Random Forest su Xgb",
    Window = i
  )
}))

real_bvp_df <- data.frame(
  Time = first_two_columns$Time,
  Real = first_two_columns$BVP
)

#apkerpam laika
real_bvp_df_trimmed <- real_bvp_df[(window_size + 1):nrow(real_bvp_df), ]


ggplot() +
  geom_line(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), linewidth = 1.2) +
  geom_point(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), size = 2) +
  geom_line(data = subset(all_rf_xgb_forecasts_shifted, Type == "Random Forest su Xgb"),
            aes(x = Time, y = Forecast, group = Window, color = interaction(Type, Window)),
            alpha = 0.6, linewidth = 0.9) +
  labs(
    title = "BVP prognozių palyginimas su Xgboost",
    x = "Ketvirtis", y = "BVP", color = "Modelis ir langas"
  ) +
  scale_color_manual(
    values = c("Reali reikšmė" = "darkgreen"),
    guide = "none"
  ) +
  scale_color_viridis_d(option = "plasma", begin = 0, end = 0.85, direction = -1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
forecast_shifted_df_rf_xgb<-all_rf_xgb_forecasts_shifted
forecast_shifted_df_rf_xgb$Horizon <- rep(1:forecast_horizon, times = length(unique(forecast_shifted_df_rf_xgb$Window)))
forecast_shifted_df_rf_xgb <- do.call(rbind, lapply(valid_rf_xgb, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_rf_xgb[[i]],
    Horizon = 1:forecast_horizon,
    Window = i
  )
}))
forecast_shifted_df_rf_xgb
num_windows <- length(unique(forecast_shifted_df_rf_xgb$Horizon))

blue_shades <- viridis(num_windows, option = "plasma", begin = 0, end = 0.9, direction = -1)

x1 <- ggplot() +
  geom_line(data = real_bvp_df_trimmed,
            aes(x = Time, y = Real, color = "Realybė", group = 1),
            size = 1.2) +
  geom_point(data = real_bvp_df_trimmed,
             aes(x = Time, y = Real, color = "Realybė")) +
  
  geom_line(data = forecast_shifted_df_rf_xgb,
            aes(x = Time, y = Forecast, group = Horizon, color = as.factor(Horizon)),
            size = 1.2, linetype = "dashed") +
  geom_point(data = forecast_shifted_df_rf_xgb,
             aes(x = Time, y = Forecast, color = as.factor(Horizon))) +
  
  scale_color_manual(
    values = c(setNames(blue_shades, as.character(unique(forecast_shifted_df_rf_xgb$Horizon))),
               "Realybė" = "red"),
    guide = guide_legend(override.aes = list(linetype = c(rep("dashed", num_windows), "solid"),
                                             size = c(rep(1.2, num_windows), 1.2)), title = "Horizontai")
  ) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )+
  labs(title = "BVP prognozės (XGBOOST su Random Forest) ir realybė pagal horizontus",
       x = "Kvartalas",
       y = "BVP",
       color = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold")) 

ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs50.png", plot = x1, width = 10, height = 6, dpi = 300, bg = "white")



forecast_shifted_df_rf_xgb <- merge(forecast_shifted_df_rf_xgb, real_bvp_df_trimmed, by = "Time")
forecast_shifted_df_rf_xgb <- forecast_shifted_df_rf_xgb[!is.na(forecast_shifted_df_rf_xgb$Real),]

rmse_post_by_horizon <- aggregate(cbind(Forecast, Real) ~ Horizon, data = forecast_shifted_df_rf_xgb, FUN = function(x) x)
rmse_post_by_horizon$RMSE <- sapply(unique(forecast_shifted_df_rf_xgb$Horizon), function(h) {
  rmse_vals <- forecast_shifted_df_rf_xgb[forecast_shifted_df_rf_xgb$Horizon == h, ]
  rmse(rmse_vals$Real, rmse_vals$Forecast)
})
print(rmse_post_by_horizon[, c("Horizon", "RMSE")])



rmse_plot_df <- data.frame(
  Horizon = rep(1:forecast_horizon, 2),
  RMSE = rmse_post_by_horizon$RMSE,
  Model = rep("XGBOOST", each = forecast_horizon)
)

err_Rf_Xb <- errors_by_h(forecast_shifted_df_rf_xgb)


rfr3 <- ggplot(rmse_plot_df, aes(x = Horizon, y = RMSE, colour = Model)) +
  geom_line(size = 1.3, alpha = 0.9) +
  geom_point(,size = 3) +
  scale_color_manual(values = c("Random Forest" = "red"), name = "")+
  scale_x_continuous(labels = number_format(accuracy = 1)) + 
  labs(
    title = "2019K1-2024K4",
    x = "Horizontas",
    y = "RMSE",
    color = "Modelis"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )
########################################################Random forest su XGBOOST#####################################################
#####################################################2019-2023#####################################################
library(tidyfit)
library(pbapply)
library(forecast)
library(zoo)
library(glmnet)
library(randomForest)
library(xgboost)
library(caret)

window_size <- 79
forecast_horizon <- 8
target_time <- "2021K2"
max_index <- which(stationary_table$Time == target_time)

X <- as.matrix(stationary_table[, -c(1, 2)])
y <- as.numeric(unlist(stationary_table[, 2]))

set.seed(123)
final_forecasts_rf_xgb <- list()
final_forecasts_rf_xgb_param <- list()
for (i in 1:(max_index - window_size + 1)) {
  X_window <- X[i:(i + window_size - 1), ]
  y_window <- y[i:(i + window_size - 1)]
  
  rf_importance_model <- randomForest(X_window, y_window, ntree = 1000, importance = TRUE)
  importance_values <- importance(rf_importance_model, type = 1)[, 1]
  
  top_vars <- names(sort(importance_values, decreasing = TRUE))[1:min(20, length(importance_values))]
  X_window_selected <- X_window[, top_vars, drop = FALSE]
  
  xgb_grid <- expand.grid(
    nrounds = c(100, 150, 200, 300, 500, 1000),
    max_depth = c(3, 5, 8, 10, 15, 20, 50),
    eta = c(0.05, 0.1, 0.15, 0.2, 0.3),
    gamma = 0,
    colsample_bytree = 0.8,
    min_child_weight = 1,
    subsample = 0.8
  )
  
  train_control <- trainControl(
    method = "timeslice",
    initialWindow = 60,
    horizon = 9,
    fixedWindow = TRUE,
    verboseIter = FALSE
  )
  
  xgb_model <- train(
    x = as.data.frame(X_window_selected),
    y = y_window,
    method = "xgbTree",
    trControl = train_control,
    tuneGrid = xgb_grid,
    verbose = FALSE
  )
  
  arima_forecasts <- list()
  for (j in 1:ncol(X_window_selected)) {
    model <- auto.arima(X_window_selected[, j])
    arima_forecasts[[j]] <- as.numeric(forecast(model, h = forecast_horizon)$mean)
  }
  
  if (length(arima_forecasts) > 0) {
    arima_forecasts_matrix <- do.call(rbind, arima_forecasts)
    arima_forecasts_df <- as.data.frame(t(arima_forecasts_matrix))
    colnames(arima_forecasts_df) <- top_vars
    
    predictions_xgb <- predict(xgb_model, newdata = arima_forecasts_df)
    final_forecasts_rf_xgb[[i]] <- predictions_xgb
    final_forecasts_rf_xgb_param[[i]] <- list(
      forecast = predictions_xgb,
      best_params = xgb_model$bestTune
    )
  }
}
best_params <- xgb_model$bestTune
print(best_params)
xgb_model$results
ggplot(xgb_model$results, aes(x = nrounds, y = RMSE, color = factor(max_depth))) +
  geom_line() +
  facet_wrap(~ eta, scales = "free") +
  theme_minimal()


print(final_forecasts_rf_xgb)

first_two_columns <- stationary_table[, 1:2]
valid_rf_xgb <- which((window_size + seq_along(final_forecasts_rf_xgb) + forecast_horizon - 1) <= 200)
max_time_index <- max(window_size + valid_rf_xgb + forecast_horizon - 1)
n_needed <- max_time_index - nrow(first_two_columns)

#jei reikia pridėti eilutes
if (n_needed > 0) {
  last_time <- first_two_columns$Time[nrow(first_two_columns)]
  new_times <- generate_quarters(last_time, n_needed)
  new_rows <- as.data.frame(matrix(NA, nrow = n_needed, ncol = ncol(first_two_columns)))
  colnames(new_rows) <- colnames(first_two_columns)
  new_rows$Time <- new_times
  first_two_columns <- rbind(first_two_columns, new_rows)
}
# Random Forest — prognozės kiekvienam langui
all_rf_xgb_forecasts_shifted <- do.call(rbind, lapply(valid_rf_xgb, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_rf_xgb[[i]],
    Type = "Random Forest su Xgb",
    Window = i
  )
}))

real_bvp_df <- data.frame(
  Time = first_two_columns$Time,
  Real = first_two_columns$BVP
)

#apkerpam laika
start_period <- "2019K2"
end_period <- "2023K2"

real_bvp_df_trimmed <- real_bvp_df[real_bvp_df$Time >= start_period & real_bvp_df$Time <= end_period, ]
all_rf_xgb_forecasts_shifted_filtered <- all_rf_xgb_forecasts_shifted[
  all_rf_xgb_forecasts_shifted$Time >= start_period & all_rf_xgb_forecasts_shifted$Time <= end_period, ]


ggplot() +
  geom_line(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), linewidth = 1.2) +
  geom_point(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), size = 2) +
  geom_line(data = subset(all_rf_xgb_forecasts_shifted_filtered, Type == "Random Forest su Xgb"),
            aes(x = Time, y = Forecast, group = Window, color = interaction(Type, Window)),
            alpha = 0.6, linewidth = 0.9) +
  labs(
    title = "BVP prognozių palyginimas su Xgboost",
    x = "Ketvirtis", y = "BVP", color = "Modelis ir langas"
  ) +
  scale_color_manual(
    values = c("Reali reikšmė" = "darkgreen"),
    guide = "none"
  ) +
  scale_color_viridis_d(option = "plasma", begin = 0, end = 0.85, direction = -1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
forecast_shifted_df_rf_xgb<-all_rf_xgb_forecasts_shifted
forecast_shifted_df_rf_xgb$Horizon <- rep(1:forecast_horizon, times = length(unique(forecast_shifted_df_rf_xgb$Window)))
forecast_shifted_df_rf_xgb <- do.call(rbind, lapply(valid_rf_xgb, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_rf_xgb[[i]],
    Horizon = 1:forecast_horizon,
    Window = i
  )
}))
forecast_shifted_df_rf_xgb

num_windows <- length(unique(forecast_shifted_df_rf_xgb$Horizon))

blue_shades <- viridis(num_windows, option = "plasma", begin = 0, end = 0.9, direction = -1)

x2 <- ggplot() +
  geom_line(data = real_bvp_df_trimmed,
            aes(x = Time, y = Real, color = "Realybė", group = 1),
            size = 1.2) +
  geom_point(data = real_bvp_df_trimmed,
             aes(x = Time, y = Real, color = "Realybė")) +
  
  geom_line(data = forecast_shifted_df_rf_xgb,
            aes(x = Time, y = Forecast, group = Horizon, color = as.factor(Horizon)),
            size = 1.2, linetype = "dashed") +
  geom_point(data = forecast_shifted_df_rf_xgb,
             aes(x = Time, y = Forecast, color = as.factor(Horizon))) +
  
  scale_color_manual(
    values = c(setNames(blue_shades, as.character(unique(forecast_shifted_df_rf_xgb$Horizon))),
               "Realybė" = "red"),
    guide = guide_legend(override.aes = list(linetype = c(rep("dashed", num_windows), "solid"),
                                             size = c(rep(1.2, num_windows), 1.2)), title = "Horizontai")
  ) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )+
  labs(title = "BVP prognozės (XGBOOST su Random Forest) ir realybė pagal horizontus",
       x = "Kvartalas",
       y = "BVP",
       color = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold")) 

ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs51.png", plot = x2, width = 10, height = 6, dpi = 300, bg = "white")



forecast_shifted_df_rf_xgb <- merge(forecast_shifted_df_rf_xgb, real_bvp_df_trimmed, by = "Time")
forecast_shifted_df_rf_xgb <- forecast_shifted_df_rf_xgb[!is.na(forecast_shifted_df_rf_xgb$Real),]
rmse_post_by_horizon <- aggregate(cbind(Forecast, Real) ~ Horizon, data = forecast_shifted_df_rf_xgb, FUN = function(x) x)
rmse_post_by_horizon$RMSE <- sapply(unique(forecast_shifted_df_rf_xgb$Horizon), function(h) {
  rmse_vals <- forecast_shifted_df_rf_xgb[forecast_shifted_df_rf_xgb$Horizon == h, ]
  rmse(rmse_vals$Real, rmse_vals$Forecast)
})
print(rmse_post_by_horizon[, c("Horizon", "RMSE")])



rmse_plot_df <- data.frame(
  Horizon = rep(1:forecast_horizon, 2),
  RMSE = rmse_post_by_horizon$RMSE,
  Model = rep("XGBOOST", each = forecast_horizon)
)

err_Rf_Xb_19_23 <- errors_by_h(forecast_shifted_df_rf_xgb)


rfr2 <- ggplot(rmse_plot_df, aes(x = Horizon, y = RMSE, colour = Model)) +
  geom_line(size = 1.3, alpha = 0.9) +
  geom_point(,size = 3) +
  scale_color_manual(values = c("XGBOOST" = "red"), name = "")+
  scale_x_continuous(labels = number_format(accuracy = 1)) + 
  labs(
    title = "2019K2-2023K2",
    x = "Horizontas",
    y = "RMSE",
    color = "Modelis"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )

########################################################Random forest su XGBOOST#####################################################
#####################################################2017-2021#####################################################
library(tidyfit)
library(pbapply)
library(forecast)
library(zoo)
library(glmnet)
library(randomForest)
library(xgboost)
library(caret)

window_size <- 71
forecast_horizon <- 8
target_time <- "2019K2"
max_index <- which(stationary_table$Time == target_time)

X <- as.matrix(stationary_table[, -c(1, 2)])
y <- as.numeric(unlist(stationary_table[, 2]))

set.seed(123)
final_forecasts_rf_xgb <- list()
final_forecasts_rf_xgb_param <- list()
for (i in 1:(max_index - window_size + 1)) {
  X_window <- X[i:(i + window_size - 1), ]
  y_window <- y[i:(i + window_size - 1)]
  
  rf_importance_model <- randomForest(X_window, y_window, ntree = 1000, importance = TRUE)
  importance_values <- importance(rf_importance_model, type = 1)[, 1]
  
  top_vars <- names(sort(importance_values, decreasing = TRUE))[1:min(20, length(importance_values))]
  X_window_selected <- X_window[, top_vars, drop = FALSE]
  
  xgb_grid <- expand.grid(
    nrounds = c(100, 150, 200, 300, 500, 1000),
    max_depth = c(3, 5, 8, 10, 15, 20, 50),
    eta = c(0.05, 0.1, 0.15, 0.2, 0.3),
    gamma = 0,
    colsample_bytree = 0.8,
    min_child_weight = 1,
    subsample = 0.8
  )
  
  train_control <- trainControl(
    method = "timeslice",
    initialWindow = 60,
    horizon = 9,
    fixedWindow = TRUE,
    verboseIter = FALSE
  )
  
  xgb_model <- train(
    x = as.data.frame(X_window_selected),
    y = y_window,
    method = "xgbTree",
    trControl = train_control,
    tuneGrid = xgb_grid,
    verbose = FALSE
  )
  
  arima_forecasts <- list()
  for (j in 1:ncol(X_window_selected)) {
    model <- auto.arima(X_window_selected[, j])
    arima_forecasts[[j]] <- as.numeric(forecast(model, h = forecast_horizon)$mean)
  }
  
  if (length(arima_forecasts) > 0) {
    arima_forecasts_matrix <- do.call(rbind, arima_forecasts)
    arima_forecasts_df <- as.data.frame(t(arima_forecasts_matrix))
    colnames(arima_forecasts_df) <- top_vars
    
    predictions_xgb <- predict(xgb_model, newdata = arima_forecasts_df)
    final_forecasts_rf_xgb[[i]] <- predictions_xgb
    final_forecasts_rf_xgb_param[[i]] <- list(
      forecast = predictions_xgb,
      best_params = xgb_model$bestTune
    )
  }
}
best_params <- xgb_model$bestTune
print(best_params)
xgb_model$results
ggplot(xgb_model$results, aes(x = nrounds, y = RMSE, color = factor(max_depth))) +
  geom_line() +
  facet_wrap(~ eta, scales = "free") +
  theme_minimal()


print(final_forecasts_rf_xgb)

first_two_columns <- stationary_table[, 1:2]
valid_rf_xgb <- which((window_size + seq_along(final_forecasts_rf_xgb) + forecast_horizon - 1) <= 200)
max_time_index <- max(window_size + valid_rf_xgb + forecast_horizon - 1)
n_needed <- max_time_index - nrow(first_two_columns)

#jei reikia pridėti eilutes
if (n_needed > 0) {
  last_time <- first_two_columns$Time[nrow(first_two_columns)]
  new_times <- generate_quarters(last_time, n_needed)
  new_rows <- as.data.frame(matrix(NA, nrow = n_needed, ncol = ncol(first_two_columns)))
  colnames(new_rows) <- colnames(first_two_columns)
  new_rows$Time <- new_times
  first_two_columns <- rbind(first_two_columns, new_rows)
}
# Random Forest — prognozės kiekvienam langui
all_rf_xgb_forecasts_shifted <- do.call(rbind, lapply(valid_rf_xgb, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_rf_xgb[[i]],
    Type = "Random Forest su Xgb",
    Window = i
  )
}))

real_bvp_df <- data.frame(
  Time = first_two_columns$Time,
  Real = first_two_columns$BVP
)

#apkerpam laika
start_period <- "2017K2"
end_period <- "2021K2"

real_bvp_df_trimmed <- real_bvp_df[real_bvp_df$Time >= start_period & real_bvp_df$Time <= end_period, ]
all_rf_xgb_forecasts_shifted_filtered <- all_rf_xgb_forecasts_shifted[
  all_rf_xgb_forecasts_shifted$Time >= start_period & all_rf_xgb_forecasts_shifted$Time <= end_period, ]


ggplot() +
  geom_line(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), linewidth = 1.2) +
  geom_point(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), size = 2) +
  geom_line(data = subset(all_rf_xgb_forecasts_shifted_filtered, Type == "Random Forest su Xgb"),
            aes(x = Time, y = Forecast, group = Window, color = interaction(Type, Window)),
            alpha = 0.6, linewidth = 0.9) +
  labs(
    title = "BVP prognozių palyginimas su Xgboost",
    x = "Ketvirtis", y = "BVP", color = "Modelis ir langas"
  ) +
  scale_color_manual(
    values = c("Reali reikšmė" = "darkgreen"),
    guide = "none"
  ) +
  scale_color_viridis_d(option = "plasma", begin = 0, end = 0.85, direction = -1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
forecast_shifted_df_rf_xgb<-all_rf_xgb_forecasts_shifted
forecast_shifted_df_rf_xgb$Horizon <- rep(1:forecast_horizon, times = length(unique(forecast_shifted_df_rf_xgb$Window)))
forecast_shifted_df_rf_xgb <- do.call(rbind, lapply(valid_rf_xgb, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_rf_xgb[[i]],
    Horizon = 1:forecast_horizon,
    Window = i
  )
}))
forecast_shifted_df_rf_xgb

num_windows <- length(unique(forecast_shifted_df_rf_xgb$Horizon))

blue_shades <- viridis(num_windows, option = "plasma", begin = 0, end = 0.9, direction = -1)

x3 <- ggplot() +
  geom_line(data = real_bvp_df_trimmed,
            aes(x = Time, y = Real, color = "Realybė", group = 1),
            size = 1.2) +
  geom_point(data = real_bvp_df_trimmed,
             aes(x = Time, y = Real, color = "Realybė")) +
  
  geom_line(data = forecast_shifted_df_rf_xgb,
            aes(x = Time, y = Forecast, group = Horizon, color = as.factor(Horizon)),
            size = 1.2, linetype = "dashed") +
  geom_point(data = forecast_shifted_df_rf_xgb,
             aes(x = Time, y = Forecast, color = as.factor(Horizon))) +
  
  scale_color_manual(
    values = c(setNames(blue_shades, as.character(unique(forecast_shifted_df_rf_xgb$Horizon))),
               "Realybė" = "red"),
    guide = guide_legend(override.aes = list(linetype = c(rep("dashed", num_windows), "solid"),
                                             size = c(rep(1.2, num_windows), 1.2)), title = "Horizontai")
  ) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )+
  labs(title = "BVP prognozės (XGBOOST su Random Forest) ir realybė pagal horizontus",
       x = "Kvartalas",
       y = "BVP",
       color = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold")) 

ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs52.png", plot = x3, width = 10, height = 6, dpi = 300, bg = "white")



forecast_shifted_df_rf_xgb <- merge(forecast_shifted_df_rf_xgb, real_bvp_df_trimmed, by = "Time")
forecast_shifted_df_rf_xgb <- forecast_shifted_df_rf_xgb[!is.na(forecast_shifted_df_rf_xgb$Real),]
rmse_post_by_horizon <- aggregate(cbind(Forecast, Real) ~ Horizon, data = forecast_shifted_df_rf_xgb, FUN = function(x) x)
rmse_post_by_horizon$RMSE <- sapply(unique(forecast_shifted_df_rf_xgb$Horizon), function(h) {
  rmse_vals <- forecast_shifted_df_rf_xgb[forecast_shifted_df_rf_xgb$Horizon == h, ]
  rmse(rmse_vals$Real, rmse_vals$Forecast)
})
print(rmse_post_by_horizon[, c("Horizon", "RMSE")])



rmse_plot_df <- data.frame(
  Horizon = rep(1:forecast_horizon, 2),
  RMSE = rmse_post_by_horizon$RMSE,
  Model = rep("XGBOOST", each = forecast_horizon)
)

err_Rf_Xb_17_21 <- errors_by_h(forecast_shifted_df_rf_xgb)


rfr1 <- ggplot(rmse_plot_df, aes(x = Horizon, y = RMSE, colour = Model)) +
  geom_line(size = 1.3, alpha = 0.9) +
  geom_point(,size = 3) +
  scale_color_manual(values = c("XGBOOST" = "red"), name = "")+
  scale_x_continuous(labels = number_format(accuracy = 1)) + 
  labs(
    title = "2017K2-2021K2",
    x = "Horizontas",
    y = "RMSE",
    color = "Modelis"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )

top_row <- rfr1 + rfr2

bottom_row <- plot_spacer() + rfr3 + plot_spacer()
bottom_row <- bottom_row + plot_layout(widths = c(1, 2, 1))

final_plot <- (top_row / bottom_row) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

final_plot <- final_plot +
  plot_layout(guides = "collect") +   
  plot_annotation(
    title = "RMSE pagal horizontus",
    theme = theme(
      plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),legend.position = "bottom"  
    )
  )

x11()
final_plot
ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs53.png", plot = final_plot, width = 10, height = 6, dpi = 300, bg = "white")


###########################################################################################################################
#####################################################################################Random Forest+XGB linear 2021-2027####
library(tidyfit)
library(pbapply)
library(forecast)
library(zoo)
library(glmnet)
library(randomForest)
library(xgboost)
library(caret)

window_size <- 78
forecast_horizon <- 9
target_time <- "2024K4"
max_index <- which(stationary_table$Time == target_time)

X <- as.matrix(stationary_table[, -c(1, 2)])
y <- as.numeric(unlist(stationary_table[, 2]))

set.seed(123)
final_forecasts_rf_xgb <- list()
final_forecasts_rf_xgb_param <- list()
for (i in 1:(max_index - window_size + 1)) {
  X_window <- X[i:(i + window_size - 1), ]
  y_window <- y[i:(i + window_size - 1)]
  
  rf_importance_model <- randomForest(X_window, y_window, ntree = 1000, importance = TRUE)
  importance_values <- importance(rf_importance_model, type = 1)[, 1]
  
  top_vars <- names(sort(importance_values, decreasing = TRUE))[1:min(20, length(importance_values))]
  X_window_selected <- X_window[, top_vars, drop = FALSE]
  
  xgb_grid <- expand.grid(
    nrounds = c(100, 150, 200),
    eta = c(0.05, 0.1, 0.15),
    lambda = c(0, 0.1, 1),
    alpha = c(0, 0.1, 1)
  )
  
  train_control <- trainControl(
    method = "timeslice",
    initialWindow = 60,
    horizon = 9,
    fixedWindow = TRUE,
    verboseIter = FALSE
  )
  
  xgb_model <- train(
    x = as.data.frame(X_window_selected),
    y = y_window,
    method = "xgbLinear",
    trControl = train_control,
    tuneGrid = xgb_grid,
    verbose = FALSE
  )
  
  arima_forecasts <- list()
  for (j in 1:ncol(X_window_selected)) {
    model <- auto.arima(X_window_selected[, j])
    arima_forecasts[[j]] <- as.numeric(forecast(model, h = forecast_horizon)$mean)
  }
  
  if (length(arima_forecasts) > 0) {
    arima_forecasts_matrix <- do.call(rbind, arima_forecasts)
    arima_forecasts_df <- as.data.frame(t(arima_forecasts_matrix))
    colnames(arima_forecasts_df) <- top_vars
    
    predictions_xgb <- predict(xgb_model, newdata = arima_forecasts_df)
    final_forecasts_rf_xgb[[i]] <- predictions_xgb
    final_forecasts_rf_xgb_param[[i]] <- list(
      forecast = predictions_xgb,
      best_params = xgb_model$bestTune
    )
  }
}
print(final_forecasts_rf_xgb_param)
best_params <- xgb_model$bestTune
print(best_params)
xgb_model$results
ggplot(xgb_model$results, aes(x = nrounds, y = RMSE, color = factor(eta))) +
  geom_line() +
  facet_wrap(~ eta, scales = "free") +
  theme_minimal()


print(final_forecasts_rf_xgb)

first_two_columns <- stationary_table[, 1:2]
valid_rf_xgb <- which((window_size + seq_along(final_forecasts_rf_xgb) + forecast_horizon - 1) <= 200)
max_time_index <- max(window_size + valid_rf_xgb + forecast_horizon - 1)
n_needed <- max_time_index - nrow(first_two_columns)

#jei reikia pridėti eilutes
if (n_needed > 0) {
  last_time <- first_two_columns$Time[nrow(first_two_columns)]
  new_times <- generate_quarters(last_time, n_needed)
  new_rows <- as.data.frame(matrix(NA, nrow = n_needed, ncol = ncol(first_two_columns)))
  colnames(new_rows) <- colnames(first_two_columns)
  new_rows$Time <- new_times
  first_two_columns <- rbind(first_two_columns, new_rows)
}
# Random Forest — prognozės kiekvienam langui
all_rf_xgb_forecasts_shifted <- do.call(rbind, lapply(valid_rf_xgb, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_rf_xgb[[i]],
    Type = "Random Forest su Xgb(linear)",
    Window = i
  )
}))

real_bvp_df <- data.frame(
  Time = first_two_columns$Time,
  Real = first_two_columns$BVP
)

#apkerpam laika
real_bvp_df_trimmed <- real_bvp_df[(window_size + 1):nrow(real_bvp_df), ]


ggplot() +
  geom_line(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), linewidth = 1.2) +
  geom_point(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), size = 2) +
  geom_line(data = subset(all_rf_xgb_forecasts_shifted, Type == "Random Forest su Xgb(linear)"),
            aes(x = Time, y = Forecast, group = Window, color = interaction(Type, Window)),
            alpha = 0.6, linewidth = 0.9) +
  labs(
    title = "BVP prognozių palyginimas su Xgboost",
    x = "Ketvirtis", y = "BVP", color = "Modelis ir langas"
  ) +
  scale_color_manual(
    values = c("Reali reikšmė" = "darkgreen"),
    guide = "none"
  ) +
  scale_color_viridis_d(option = "plasma", begin = 0, end = 0.85, direction = -1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
forecast_shifted_df_rf_xgb<-all_rf_xgb_forecasts_shifted
forecast_shifted_df_rf_xgb$Horizon <- rep(1:forecast_horizon, times = length(unique(forecast_shifted_df_rf_xgb$Window)))
forecast_shifted_df_rf_xgb <- do.call(rbind, lapply(valid_rf_xgb, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_rf_xgb[[i]],
    Horizon = 1:forecast_horizon,
    Window = i
  )
}))
forecast_shifted_df_rf_xgb
num_windows <- length(unique(forecast_shifted_df_rf_xgb$Horizon))

blue_shades <- viridis(num_windows, option = "plasma", begin = 0, end = 0.9, direction = -1)

x4 <- ggplot() +
  geom_line(data = real_bvp_df_trimmed,
            aes(x = Time, y = Real, color = "Realybė", group = 1),
            size = 1.2) +
  geom_point(data = real_bvp_df_trimmed,
             aes(x = Time, y = Real, color = "Realybė")) +
  
  geom_line(data = forecast_shifted_df_rf_xgb,
            aes(x = Time, y = Forecast, group = Horizon, color = as.factor(Horizon)),
            size = 1.2, linetype = "dashed") +
  geom_point(data = forecast_shifted_df_rf_xgb,
             aes(x = Time, y = Forecast, color = as.factor(Horizon))) +
  
  scale_color_manual(
    values = c(setNames(blue_shades, as.character(unique(forecast_shifted_df_rf_xgb$Horizon))),
               "Realybė" = "red"),
    guide = guide_legend(override.aes = list(linetype = c(rep("dashed", num_windows), "solid"),
                                             size = c(rep(1.2, num_windows), 1.2)), title = "Horizontai")
  ) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )+
  labs(title = "BVP prognozės (XGBOOST su Random Forest) ir realybė pagal horizontus",
       x = "Kvartalas",
       y = "BVP",
       color = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold")) 

ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs54.png", plot = x4, width = 10, height = 6, dpi = 300, bg = "white")



forecast_shifted_df_rf_xgb <- merge(forecast_shifted_df_rf_xgb, real_bvp_df_trimmed, by = "Time")
forecast_shifted_df_rf_xgb <- forecast_shifted_df_rf_xgb[!is.na(forecast_shifted_df_rf_xgb$Real),]
rmse_post_by_horizon <- aggregate(cbind(Forecast, Real) ~ Horizon, data = forecast_shifted_df_rf_xgb, FUN = function(x) x)
rmse_post_by_horizon$RMSE <- sapply(unique(forecast_shifted_df_rf_xgb$Horizon), function(h) {
  rmse_vals <- forecast_shifted_df_rf_xgb[forecast_shifted_df_rf_xgb$Horizon == h, ]
  rmse(rmse_vals$Real, rmse_vals$Forecast)
})
print(rmse_post_by_horizon[, c("Horizon", "RMSE")])



rmse_plot_df <- data.frame(
  Horizon = rep(1:forecast_horizon, 2),
  RMSE = rmse_post_by_horizon$RMSE,
  Model = rep("XGBOOST", each = forecast_horizon)
)

err_Rf_Xb_ln_19_27 <- errors_by_h(forecast_shifted_df_rf_xgb)


rfr3 <- ggplot(rmse_plot_df, aes(x = Horizon, y = RMSE, colour = Model)) +
  geom_line(size = 1.3, alpha = 0.9) +
  geom_point(,size = 3) +
  scale_color_manual(values = c("XGBOOST" = "red"), name = "")+
  scale_x_continuous(labels = number_format(accuracy = 1)) + 
  labs(
    title = "2019K1-2024K1",
    x = "Horizontas",
    y = "RMSE",
    color = "Modelis"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )
#########################################################################################################################
####################################################################2019-2023########################################

window_size <- 79
forecast_horizon <- 8
target_time <- "2021K2"
max_index <- which(stationary_table$Time == target_time)

X <- as.matrix(stationary_table[, -c(1, 2)])
y <- as.numeric(unlist(stationary_table[, 2]))

set.seed(123)
final_forecasts_rf_xgb <- list()
# 
for (i in 1:(max_index - window_size + 1)) {
  X_window <- X[i:(i + window_size - 1), ]
  y_window <- y[i:(i + window_size - 1)]
  
  rf_importance_model <- randomForest(X_window, y_window, ntree = 1000, importance = TRUE)
  importance_values <- importance(rf_importance_model, type = 1)[, 1]
  
  top_vars <- names(sort(importance_values, decreasing = TRUE))[1:min(20, length(importance_values))]
  X_window_selected <- X_window[, top_vars, drop = FALSE]
  
  xgb_grid <- expand.grid(
    nrounds = c(100, 150, 200),
    eta = c(0.05, 0.1, 0.15),
    lambda = c(0, 0.1, 1),
    alpha = c(0, 0.1, 1)
  )
  
  train_control <- trainControl(
    method = "timeslice",
    initialWindow = 60,
    horizon = 9,
    fixedWindow = TRUE,
    verboseIter = FALSE
  )
  
  xgb_model <- train(
    x = as.data.frame(X_window_selected),
    y = y_window,
    method = "xgbLinear",
    trControl = train_control,
    tuneGrid = xgb_grid,
    verbose = FALSE
  )
  
  arima_forecasts <- list()
  for (j in 1:ncol(X_window_selected)) {
    model <- auto.arima(X_window_selected[, j])
    arima_forecasts[[j]] <- as.numeric(forecast(model, h = forecast_horizon)$mean)
  }
  
  if (length(arima_forecasts) > 0) {
    arima_forecasts_matrix <- do.call(rbind, arima_forecasts)
    arima_forecasts_df <- as.data.frame(t(arima_forecasts_matrix))
    colnames(arima_forecasts_df) <- top_vars
    
    predictions_xgb <- predict(xgb_model, newdata = arima_forecasts_df)
    final_forecasts_rf_xgb[[i]] <- predictions_xgb
    final_forecasts_rf_xgb_param[[i]] <- list(
      forecast = predictions_xgb,
      best_params = xgb_model$bestTune
    )
  }
}

print(final_forecasts_rf_xgb)
start_period <- "2019K2"
end_period <- "2023K2"

best_params <- xgb_model$bestTune
print(best_params)


first_two_columns <- stationary_table[, 1:2]
valid_rf_xgb <- which((window_size + seq_along(final_forecasts_rf_xgb) + forecast_horizon - 1) <= nrow(first_two_columns))
real_bvp_df_trimmed <- real_bvp_df[real_bvp_df$Time >= start_period & real_bvp_df$Time <= end_period, ]
max_time_index <- max(window_size + valid_rf_xgb + forecast_horizon - 1)
n_needed <- max_time_index - nrow(first_two_columns)

#jei reikia pridėti eilutes
if (n_needed > 0) {
  last_time <- first_two_columns$Time[nrow(first_two_columns)]
  new_times <- generate_quarters(last_time, n_needed)
  new_rows <- as.data.frame(matrix(NA, nrow = n_needed, ncol = ncol(first_two_columns)))
  colnames(new_rows) <- colnames(first_two_columns)
  new_rows$Time <- new_times
  first_two_columns <- rbind(first_two_columns, new_rows)
}
# Random Forest — prognozės kiekvienam langui
all_rf_xgb_forecasts_shifted <- do.call(rbind, lapply(valid_rf_xgb, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_rf_xgb[[i]],
    Type = "Random Forest su Xgb",
    Window = i
  )
}))

all_rf_xgb_forecasts_shifted_filtered <- all_rf_xgb_forecasts_shifted[
  all_rf_xgb_forecasts_shifted$Time >= start_period & all_rf_xgb_forecasts_shifted$Time <= end_period, ]


ggplot() +
  geom_line(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), linewidth = 1.2) +
  geom_point(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), size = 2) +
  geom_line(data = subset(all_rf_xgb_forecasts_shifted_filtered, Type == "Random Forest su Xgb"),
            aes(x = Time, y = Forecast, group = Window, color = interaction(Type, Window)),
            alpha = 0.6, linewidth = 0.9) +
  labs(
    title = "BVP prognozių palyginimas su Xgboost",
    x = "Ketvirtis", y = "BVP", color = "Modelis ir langas"
  ) +
  scale_color_manual(
    values = c("Reali reikšmė" = "darkgreen"),
    guide = "none"
  ) +
  scale_color_viridis_d(option = "plasma", begin = 0, end = 0.85, direction = -1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
forecast_shifted_df_rf_xgb<-all_rf_xgb_forecasts_shifted
forecast_shifted_df_rf_xgb$Horizon <- rep(1:forecast_horizon, times = length(unique(forecast_shifted_df_rf_xgb$Window)))
forecast_shifted_df_rf_xgb <- do.call(rbind, lapply(valid_rf_xgb, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_rf_xgb[[i]],
    Horizon = 1:forecast_horizon,
    Window = i
  )
}))
forecast_shifted_df_rf_xgb
num_windows <- length(unique(forecast_shifted_df_rf_xgb$Horizon))

blue_shades <- viridis(num_windows, option = "plasma", begin = 0, end = 0.9, direction = -1)
x5 <- ggplot() +
  geom_line(data = real_bvp_df_trimmed,
            aes(x = Time, y = Real, color = "Realybė", group = 1),
            size = 1.2) +
  geom_point(data = real_bvp_df_trimmed,
             aes(x = Time, y = Real, color = "Realybė")) +
  
  geom_line(data = forecast_shifted_df_rf_xgb,
            aes(x = Time, y = Forecast, group = Horizon, color = as.factor(Horizon)),
            size = 1.2, linetype = "dashed") +
  geom_point(data = forecast_shifted_df_rf_xgb,
             aes(x = Time, y = Forecast, color = as.factor(Horizon))) +
  
  scale_color_manual(
    values = c(setNames(blue_shades, as.character(unique(forecast_shifted_df_rf_xgb$Horizon))),
               "Realybė" = "red"),
    guide = guide_legend(override.aes = list(linetype = c(rep("dashed", num_windows), "solid"),
                                             size = c(rep(1.2, num_windows), 1.2)), title = "Horizontai")
  ) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )+
  labs(title = "BVP prognozės (XGBOOST su Random Forest) ir realybė pagal horizontus",
       x = "Kvartalas",
       y = "BVP",
       color = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold")) 


ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs56.png", plot = x5, width = 10, height = 6, dpi = 300, bg = "white")



forecast_shifted_df_rf_xgb <- merge(forecast_shifted_df_rf_xgb, real_bvp_df_trimmed, by = "Time")
forecast_shifted_df_rf_xgb <- forecast_shifted_df_rf_xgb[!is.na(forecast_shifted_df_rf_xgb$Real),]
rmse_post_by_horizon <- aggregate(cbind(Forecast, Real) ~ Horizon, data = forecast_shifted_df_rf_xgb, FUN = function(x) x)
rmse_post_by_horizon$RMSE <- sapply(unique(forecast_shifted_df_rf_xgb$Horizon), function(h) {
  rmse_vals <- forecast_shifted_df_rf_xgb[forecast_shifted_df_rf_xgb$Horizon == h, ]
  rmse(rmse_vals$Real, rmse_vals$Forecast)
})
print(rmse_post_by_horizon[, c("Horizon", "RMSE")])



rmse_plot_df <- data.frame(
  Horizon = rep(1:forecast_horizon, 2),
  RMSE = rmse_post_by_horizon$RMSE,
  Model = rep("XGBOOST", each = forecast_horizon)
)

err_Rf_Xb_ln_19_23 <- errors_by_h(forecast_shifted_df_rf_xgb)


rfr2 <- ggplot(rmse_plot_df, aes(x = Horizon, y = RMSE, colour = Model)) +
  geom_line(size = 1.3, alpha = 0.9) +
  geom_point(,size = 3) +
  scale_color_manual(values = c("XGBOOST" = "red"), name = "")+
  scale_x_continuous(labels = number_format(accuracy = 1)) + 
  labs(
    title = "2019K2-2023K2",
    x = "Horizontas",
    y = "RMSE",
    color = "Modelis"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )

#########################################################################################################################
####################################################################2017k2-2021k2########################################

window_size <- 71
forecast_horizon <- 8
target_time <- "2019K2"
max_index <- which(stationary_table$Time == target_time)

X <- as.matrix(stationary_table[, -c(1, 2)])
y <- as.numeric(unlist(stationary_table[, 2]))

set.seed(123)
final_forecasts_rf_xgb <- list()
# 
for (i in 1:(max_index - window_size + 1)) {
  X_window <- X[i:(i + window_size - 1), ]
  y_window <- y[i:(i + window_size - 1)]
  
  rf_importance_model <- randomForest(X_window, y_window, ntree = 1000, importance = TRUE)
  importance_values <- importance(rf_importance_model, type = 1)[, 1]
  
  top_vars <- names(sort(importance_values, decreasing = TRUE))[1:min(20, length(importance_values))]
  X_window_selected <- X_window[, top_vars, drop = FALSE]
  
  xgb_grid <- expand.grid(
    nrounds = c(100, 150, 200),
    eta = c(0.05, 0.1, 0.15),
    lambda = c(0, 0.1, 1),
    alpha = c(0, 0.1, 1)
  )
  
  train_control <- trainControl(
    method = "timeslice",
    initialWindow = 60,
    horizon = 9,
    fixedWindow = TRUE,
    verboseIter = FALSE
  )
  
  xgb_model <- train(
    x = as.data.frame(X_window_selected),
    y = y_window,
    method = "xgbLinear",
    trControl = train_control,
    tuneGrid = xgb_grid,
    verbose = FALSE
  )
  
  arima_forecasts <- list()
  for (j in 1:ncol(X_window_selected)) {
    model <- auto.arima(X_window_selected[, j])
    arima_forecasts[[j]] <- as.numeric(forecast(model, h = forecast_horizon)$mean)
  }
  
  if (length(arima_forecasts) > 0) {
    arima_forecasts_matrix <- do.call(rbind, arima_forecasts)
    arima_forecasts_df <- as.data.frame(t(arima_forecasts_matrix))
    colnames(arima_forecasts_df) <- top_vars
    
    predictions_xgb <- predict(xgb_model, newdata = arima_forecasts_df)
    final_forecasts_rf_xgb[[i]] <- predictions_xgb
    final_forecasts_rf_xgb_param[[i]] <- list(
      forecast = predictions_xgb,
      best_params = xgb_model$bestTune
    )
  }
}

print(final_forecasts_rf_xgb)
start_period <- "2017K2"
end_period <- "2021K2"

best_params <- xgb_model$bestTune
print(best_params)


first_two_columns <- stationary_table[, 1:2]
valid_rf_xgb <- which((window_size + seq_along(final_forecasts_rf_xgb) + forecast_horizon - 1) <= nrow(first_two_columns))
real_bvp_df_trimmed <- real_bvp_df[real_bvp_df$Time >= start_period & real_bvp_df$Time <= end_period, ]
max_time_index <- max(window_size + valid_rf_xgb + forecast_horizon - 1)
n_needed <- max_time_index - nrow(first_two_columns)

#jei reikia pridėti eilutes
if (n_needed > 0) {
  last_time <- first_two_columns$Time[nrow(first_two_columns)]
  new_times <- generate_quarters(last_time, n_needed)
  new_rows <- as.data.frame(matrix(NA, nrow = n_needed, ncol = ncol(first_two_columns)))
  colnames(new_rows) <- colnames(first_two_columns)
  new_rows$Time <- new_times
  first_two_columns <- rbind(first_two_columns, new_rows)
}
# Random Forest — prognozės kiekvienam langui
all_rf_xgb_forecasts_shifted <- do.call(rbind, lapply(valid_rf_xgb, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_rf_xgb[[i]],
    Type = "Random Forest su Xgb",
    Window = i
  )
}))

all_rf_xgb_forecasts_shifted_filtered <- all_rf_xgb_forecasts_shifted[
  all_rf_xgb_forecasts_shifted$Time >= start_period & all_rf_xgb_forecasts_shifted$Time <= end_period, ]



ggplot() +
  geom_line(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), linewidth = 1.2) +
  geom_point(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), size = 2) +
  geom_line(data = subset(all_rf_xgb_forecasts_shifted_filtered, Type == "Random Forest su Xgb"),
            aes(x = Time, y = Forecast, group = Window, color = interaction(Type, Window)),
            alpha = 0.6, linewidth = 0.9) +
  labs(
    title = "BVP prognozių palyginimas su Xgboost",
    x = "Ketvirtis", y = "BVP", color = "Modelis ir langas"
  ) +
  scale_color_manual(
    values = c("Reali reikšmė" = "darkgreen"),
    guide = "none"
  ) +
  scale_color_viridis_d(option = "plasma", begin = 0, end = 0.85, direction = -1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
forecast_shifted_df_rf_xgb<-all_rf_xgb_forecasts_shifted
forecast_shifted_df_rf_xgb$Horizon <- rep(1:forecast_horizon, times = length(unique(forecast_shifted_df_rf_xgb$Window)))
forecast_shifted_df_rf_xgb <- do.call(rbind, lapply(valid_rf_xgb, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_rf_xgb[[i]],
    Horizon = 1:forecast_horizon,
    Window = i
  )
}))
forecast_shifted_df_rf_xgb
num_windows <- length(unique(forecast_shifted_df_rf_xgb$Horizon))

blue_shades <- viridis(num_windows, option = "plasma", begin = 0, end = 0.9, direction = -1)
x6 <- ggplot() +
  geom_line(data = real_bvp_df_trimmed,
            aes(x = Time, y = Real, color = "Realybė", group = 1),
            size = 1.2) +
  geom_point(data = real_bvp_df_trimmed,
             aes(x = Time, y = Real, color = "Realybė")) +
  
  geom_line(data = forecast_shifted_df_rf_xgb,
            aes(x = Time, y = Forecast, group = Horizon, color = as.factor(Horizon)),
            size = 1.2, linetype = "dashed") +
  geom_point(data = forecast_shifted_df_rf_xgb,
             aes(x = Time, y = Forecast, color = as.factor(Horizon))) +
  
  scale_color_manual(
    values = c(setNames(blue_shades, as.character(unique(forecast_shifted_df_rf_xgb$Horizon))),
               "Realybė" = "red"),
    guide = guide_legend(override.aes = list(linetype = c(rep("dashed", num_windows), "solid"),
                                             size = c(rep(1.2, num_windows), 1.2)), title = "Horizontai")
  ) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )+
  labs(title = "BVP prognozės (XGBOOST su Random Forest) ir realybė pagal horizontus",
       x = "Kvartalas",
       y = "BVP",
       color = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold")) 


ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs57.png", plot = x6, width = 10, height = 6, dpi = 300, bg = "white")



forecast_shifted_df_rf_xgb <- merge(forecast_shifted_df_rf_xgb, real_bvp_df_trimmed, by = "Time")
forecast_shifted_df_rf_xgb <- forecast_shifted_df_rf_xgb[!is.na(forecast_shifted_df_rf_xgb$Real),]
rmse_post_by_horizon <- aggregate(cbind(Forecast, Real) ~ Horizon, data = forecast_shifted_df_rf_xgb, FUN = function(x) x)
rmse_post_by_horizon$RMSE <- sapply(unique(forecast_shifted_df_rf_xgb$Horizon), function(h) {
  rmse_vals <- forecast_shifted_df_rf_xgb[forecast_shifted_df_rf_xgb$Horizon == h, ]
  rmse(rmse_vals$Real, rmse_vals$Forecast)
})
print(rmse_post_by_horizon[, c("Horizon", "RMSE")])



rmse_plot_df <- data.frame(
  Horizon = rep(1:forecast_horizon, 2),
  RMSE = rmse_post_by_horizon$RMSE,
  Model = rep("XGBOOST", each = forecast_horizon)
)

err_Rf_Xb_ln_17_21 <- errors_by_h(forecast_shifted_df_rf_xgb)


rfr1 <- ggplot(rmse_plot_df, aes(x = Horizon, y = RMSE, colour = Model)) +
  geom_line(size = 1.3, alpha = 0.9) +
  geom_point(,size = 3) +
  scale_color_manual(values = c("XGBOOST" = "red"), name = "")+
  scale_x_continuous(labels = number_format(accuracy = 1)) + 
  labs(
    title = "2017K2-2021K2",
    x = "Horizontas",
    y = "RMSE",
    color = "Modelis"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )

top_row <- rfr1 + rfr2

bottom_row <- plot_spacer() + rfr3 + plot_spacer()
bottom_row <- bottom_row + plot_layout(widths = c(1, 2, 1))

final_plot <- (top_row / bottom_row) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

final_plot <- final_plot +
  plot_layout(guides = "collect") +   
  plot_annotation(
    title = "RMSE pagal horizontus",
    theme = theme(
      plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),legend.position = "bottom"  
    )
  )

x11()
final_plot
ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs58.png", plot = final_plot, width = 10, height = 6, dpi = 300, bg = "white")

#####################################################################################################################
########################################################XGBOOST+MI#####################################################
#####################################################2021-2027#####################################################
library(tidyfit)
library(pbapply)
library(forecast)
library(zoo)
library(glmnet)
library(randomForest)
library(xgboost)
library(caret)

window_size <- 78
forecast_horizon <- 9
target_time <- "2024K4"
max_index <- which(stationary_table$Time == target_time)

X <- as.matrix(stationary_table[, -c(1, 2)])
y <- as.numeric(unlist(stationary_table[, 2]))

set.seed(123)
final_forecasts_mi_xgb <- list()
final_arima_forecasts <- list()


for (i in 1:(max_index - window_size + 1)) {
  X_window <- X[i:(i + window_size - 1), ]
  y_window <- y[i:(i + window_size - 1)]
  
  mi_results <- calculate_mutual_information_for_all_columns(data_window)
  top_100_variables <- mi_results[order(mi_results$MI, decreasing = TRUE), ][1:100, ]
  selected_var_names <- top_100_variables$Variable
  
  if (length(selected_var_names) > 0) {
    X_window_selected <- X_window[, selected_var_names, drop = FALSE]
    
    xgb_grid <- expand.grid(
      nrounds = c(100, 150, 200, 300, 500),
      max_depth = c(3, 5, 8, 10, 15),
      eta = c(0.05, 0.1, 0.15, 0.2),
      gamma = 0,
      colsample_bytree = 0.8,
      min_child_weight = 1,
      subsample = 0.8
    )
    
    train_control <- trainControl(
      method = "timeslice",
      initialWindow = 60,
      horizon = 9,
      fixedWindow = TRUE,
      verboseIter = FALSE
    )
    
    xgb_model <- train(
      x = as.data.frame(X_window_selected),
      y = y_window,
      method = "xgbTree",
      trControl = train_control,
      tuneGrid = xgb_grid,
      verbose = FALSE
    )
    
    arima_forecasts <- list()
    for (j in 1:ncol(X_window_selected)) {
      model <- auto.arima(X_window_selected[, j])
      arima_forecasts[[j]] <- as.numeric(forecast(model, h = forecast_horizon)$mean)
    }
    
    if (length(arima_forecasts) > 0) {
      arima_forecasts_matrix <- do.call(rbind, arima_forecasts)
      arima_forecasts_df <- as.data.frame(t(arima_forecasts_matrix))
      colnames(arima_forecasts_df) <- selected_var_names
      
      predictions_xgb <- predict(xgb_model, newdata = arima_forecasts_df)
      final_forecasts_mi_xgb[[i]] <- predictions_xgb
    }
  }
}
best_params <- xgb_model$bestTune
print(best_params)
xgb_model$results
ggplot(xgb_model$results, aes(x = nrounds, y = RMSE, color = factor(max_depth))) +
  geom_line() +
  facet_wrap(~ eta, scales = "free") +
  theme_minimal()

print(final_forecasts_mi_xgb)

first_two_columns <- stationary_table[, 1:2]
valid_mi_xgb <- which((window_size + seq_along(final_forecasts_mi_xgb) + forecast_horizon - 1) <= 200)
max_time_index <- max(window_size + valid_mi_xgb + forecast_horizon - 1)
n_needed <- max_time_index - nrow(first_two_columns)

#jei reikia pridėti eilutes
if (n_needed > 0) {
  last_time <- first_two_columns$Time[nrow(first_two_columns)]
  new_times <- generate_quarters(last_time, n_needed)
  new_rows <- as.data.frame(matrix(NA, nrow = n_needed, ncol = ncol(first_two_columns)))
  colnames(new_rows) <- colnames(first_two_columns)
  new_rows$Time <- new_times
  first_two_columns <- rbind(first_two_columns, new_rows)
}
# Random Forest — prognozės kiekvienam langui
all_mi_xgb_forecasts_shifted <- do.call(rbind, lapply(valid_mi_xgb, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_mi_xgb[[i]],
    Type = "MI su Xgb",
    Window = i
  )
}))

real_bvp_df <- data.frame(
  Time = first_two_columns$Time,
  Real = first_two_columns$BVP
)

#apkerpam laika
real_bvp_df_trimmed <- real_bvp_df[(window_size + 1):nrow(real_bvp_df), ]


ggplot() +
  geom_line(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), linewidth = 1.2) +
  geom_point(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), size = 2) +
  geom_line(data = subset(all_mi_xgb_forecasts_shifted, Type == "MI su Xgb"),
            aes(x = Time, y = Forecast, group = Window, color = interaction(Type, Window)),
            alpha = 0.6, linewidth = 0.9) +
  labs(
    title = "BVP prognozių palyginimas su Xgboost",
    x = "Ketvirtis", y = "BVP", color = "Modelis ir langas"
  ) +
  scale_color_manual(
    values = c("Reali reikšmė" = "darkgreen"),
    guide = "none"
  ) +
  scale_color_viridis_d(option = "plasma", begin = 0, end = 0.85, direction = -1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
forecast_shifted_df_mi_xgb<-all_mi_xgb_forecasts_shifted
forecast_shifted_df_mi_xgb$Horizon <- rep(1:forecast_horizon, times = length(unique(forecast_shifted_df_mi_xgb$Window)))
forecast_shifted_df_mi_xgb <- do.call(rbind, lapply(valid_mi_xgb, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_mi_xgb[[i]],
    Horizon = 1:forecast_horizon,
    Window = i
  )
}))
forecast_shifted_df_mi_xgb

num_windows <- length(unique(forecast_shifted_df_mi_xgb$Horizon))

blue_shades <- viridis(num_windows, option = "plasma", begin = 0, end = 0.9, direction = -1)
x7 <- ggplot() +
  geom_line(data = real_bvp_df_trimmed,
            aes(x = Time, y = Real, color = "Realybė", group = 1),
            size = 1.2) +
  geom_point(data = real_bvp_df_trimmed,
             aes(x = Time, y = Real, color = "Realybė")) +
  
  geom_line(data = forecast_shifted_df_mi_xgb,
            aes(x = Time, y = Forecast, group = Horizon, color = as.factor(Horizon)),
            size = 1.2, linetype = "dashed") +
  geom_point(data = forecast_shifted_df_mi_xgb,
             aes(x = Time, y = Forecast, color = as.factor(Horizon))) +
  
  scale_color_manual(
    values = c(setNames(blue_shades, as.character(unique(forecast_shifted_df_mi_xgb$Horizon))),
               "Realybė" = "red"),
    guide = guide_legend(override.aes = list(linetype = c(rep("dashed", num_windows), "solid"),
                                             size = c(rep(1.2, num_windows), 1.2)), title = "Horizontai")
  ) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )+
  labs(title = "BVP prognozės (XGBOOST su MI) ir realybė pagal horizontus",
       x = "Kvartalas",
       y = "BVP",
       color = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold")) 


ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs60.png", plot = x7, width = 10, height = 6, dpi = 300, bg = "white")



forecast_shifted_df_mi_xgb <- merge(forecast_shifted_df_mi_xgb, real_bvp_df_trimmed, by = "Time")
forecast_shifted_df_mi_xgb <- forecast_shifted_df_mi_xgb[!is.na(forecast_shifted_df_mi_xgb$Real),]
rmse_post_by_horizon <- aggregate(cbind(Forecast, Real) ~ Horizon, data = forecast_shifted_df_mi_xgb, FUN = function(x) x)
rmse_post_by_horizon$RMSE <- sapply(unique(forecast_shifted_df_mi_xgb$Horizon), function(h) {
  rmse_vals <- forecast_shifted_df_mi_xgb[forecast_shifted_df_mi_xgb$Horizon == h, ]
  rmse(rmse_vals$Real, rmse_vals$Forecast)
})
print(rmse_post_by_horizon[, c("Horizon", "RMSE")])



rmse_plot_df <- data.frame(
  Horizon = rep(1:forecast_horizon, 2),
  RMSE = rmse_post_by_horizon$RMSE,
  Model = rep("XGBOOST", each = forecast_horizon)
)

err_Rf_Xb_mi_21_27 <- errors_by_h(forecast_shifted_df_mi_xgb)


rfr3 <- ggplot(rmse_plot_df, aes(x = Horizon, y = RMSE, colour = Model)) +
  geom_line(size = 1.3, alpha = 0.9) +
  geom_point(,size = 3) +
  scale_color_manual(values = c("XGBOOST" = "red"), name = "")+
  scale_x_continuous(labels = number_format(accuracy = 1)) + 
  labs(
    title = "2021K1-2024K4",
    x = "Horizontas",
    y = "RMSE",
    color = "Modelis"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )


#####################################################################################################################
########################################################XGBOOST+MI#####################################################
#####################################################2019-2023#####################################################
library(tidyfit)
library(pbapply)
library(forecast)
library(zoo)
library(glmnet)
library(randomForest)
library(xgboost)
library(caret)

window_size <- 79
forecast_horizon <- 8
target_time <- "2021K2"
max_index <- which(stationary_table$Time == target_time)

X <- as.matrix(stationary_table[, -c(1, 2)])
y <- as.numeric(unlist(stationary_table[, 2]))

set.seed(123)
final_forecasts_mi_xgb <- list()
final_arima_forecasts <- list()


for (i in 1:(max_index - window_size + 1)) {
  X_window <- X[i:(i + window_size - 1), ]
  y_window <- y[i:(i + window_size - 1)]
  
  mi_results <- calculate_mutual_information_for_all_columns(data_window)
  top_100_variables <- mi_results[order(mi_results$MI, decreasing = TRUE), ][1:100, ]
  selected_var_names <- top_100_variables$Variable
  
  if (length(selected_var_names) > 0) {
    X_window_selected <- X_window[, selected_var_names, drop = FALSE]
    
    xgb_grid <- expand.grid(
      nrounds = c(100, 150),
      max_depth = c(3, 5),
      eta = c(0.05, 0.1),
      gamma = 0,
      colsample_bytree = 0.8,
      min_child_weight = 1,
      subsample = 0.8
    )
    
    train_control <- trainControl(
      method = "timeslice",
      initialWindow = 60,
      horizon = 9,
      fixedWindow = TRUE,
      verboseIter = FALSE
    )
    
    xgb_model <- train(
      x = as.data.frame(X_window_selected),
      y = y_window,
      method = "xgbTree",
      trControl = train_control,
      tuneGrid = xgb_grid,
      verbose = FALSE
    )
    
    arima_forecasts <- list()
    for (j in 1:ncol(X_window_selected)) {
      model <- auto.arima(X_window_selected[, j])
      arima_forecasts[[j]] <- as.numeric(forecast(model, h = forecast_horizon)$mean)
    }
    
    if (length(arima_forecasts) > 0) {
      arima_forecasts_matrix <- do.call(rbind, arima_forecasts)
      arima_forecasts_df <- as.data.frame(t(arima_forecasts_matrix))
      colnames(arima_forecasts_df) <- selected_var_names
      
      predictions_xgb <- predict(xgb_model, newdata = arima_forecasts_df)
      final_forecasts_mi_xgb[[i]] <- predictions_xgb
    }
  }
}
start_period <- "2019K2"
end_period <- "2023K2"
best_params <- xgb_model$bestTune
print(best_params)
xgb_model$results
ggplot(xgb_model$results, aes(x = nrounds, y = RMSE, color = factor(max_depth))) +
  geom_line() +
  facet_wrap(~ eta, scales = "free") +
  theme_minimal()

print(final_forecasts_mi_xgb)

first_two_columns <- stationary_table[, 1:2]
valid_mi_xgb <- which((window_size + seq_along(final_forecasts_mi_xgb) + forecast_horizon - 1) <= 200)
max_time_index <- max(window_size + valid_mi_xgb + forecast_horizon - 1)
n_needed <- max_time_index - nrow(first_two_columns)

#jei reikia pridėti eilutes
if (n_needed > 0) {
  last_time <- first_two_columns$Time[nrow(first_two_columns)]
  new_times <- generate_quarters(last_time, n_needed)
  new_rows <- as.data.frame(matrix(NA, nrow = n_needed, ncol = ncol(first_two_columns)))
  colnames(new_rows) <- colnames(first_two_columns)
  new_rows$Time <- new_times
  first_two_columns <- rbind(first_two_columns, new_rows)
}
# Random Forest — prognozės kiekvienam langui
all_mi_xgb_forecasts_shifted <- do.call(rbind, lapply(valid_mi_xgb, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_mi_xgb[[i]],
    Type = "MI su Xgb",
    Window = i
  )
}))

real_bvp_df <- data.frame(
  Time = first_two_columns$Time,
  Real = first_two_columns$BVP
)

#apkerpam laika
real_bvp_df_trimmed <- real_bvp_df[real_bvp_df$Time >= start_period & real_bvp_df$Time <= end_period, ]


ggplot() +
  geom_line(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), linewidth = 1.2) +
  geom_point(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), size = 2) +
  geom_line(data = subset(all_mi_xgb_forecasts_shifted, Type == "MI su Xgb"),
            aes(x = Time, y = Forecast, group = Window, color = interaction(Type, Window)),
            alpha = 0.6, linewidth = 0.9) +
  labs(
    title = "BVP prognozių palyginimas su Xgboost",
    x = "Ketvirtis", y = "BVP", color = "Modelis ir langas"
  ) +
  scale_color_manual(
    values = c("Reali reikšmė" = "darkgreen"),
    guide = "none"
  ) +
  scale_color_viridis_d(option = "plasma", begin = 0, end = 0.85, direction = -1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
forecast_shifted_df_mi_xgb<-all_mi_xgb_forecasts_shifted
forecast_shifted_df_mi_xgb$Horizon <- rep(1:forecast_horizon, times = length(unique(forecast_shifted_df_mi_xgb$Window)))
forecast_shifted_df_mi_xgb <- do.call(rbind, lapply(valid_mi_xgb, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_mi_xgb[[i]],
    Horizon = 1:forecast_horizon,
    Window = i
  )
}))
forecast_shifted_df_mi_xgb
ggplot() +
  geom_line(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), linewidth = 1.2) +
  geom_point(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), size = 2) +
  
  geom_line(data = forecast_shifted_df_mi_xgb,
            aes(x = Time, y = Forecast, color = as.factor(Horizon), group = Horizon),
            linewidth = 1.1) +
  
  labs(
    title = "Prognozės pagal horizontus",
    x = "Ketvirtis", y = "BVP", color = "Prognozės horizontas"
  ) +
  scale_color_viridis_d(option = "plasma", begin = 0, end = 0.9) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#####################################################################################################################
########################################################XGBOOST+MI#####################################################
#####################################################2017-2021#####################################################
library(tidyfit)
library(pbapply)
library(forecast)
library(zoo)
library(glmnet)
library(randomForest)
library(xgboost)
library(caret)

window_size <- 71
forecast_horizon <- 9
target_time <- "2019K2"
max_index <- which(stationary_table$Time == target_time)

X <- as.matrix(stationary_table[, -c(1, 2)])
y <- as.numeric(unlist(stationary_table[, 2]))

set.seed(123)
final_forecasts_mi_xgb <- list()
final_arima_forecasts <- list()


for (i in 1:(max_index - window_size + 1)) {
  X_window <- X[i:(i + window_size - 1), ]
  y_window <- y[i:(i + window_size - 1)]
  
  mi_results <- calculate_mutual_information_for_all_columns(data_window)
  top_100_variables <- mi_results[order(mi_results$MI, decreasing = TRUE), ][1:100, ]
  selected_var_names <- top_100_variables$Variable
  
  if (length(selected_var_names) > 0) {
    X_window_selected <- X_window[, selected_var_names, drop = FALSE]
    
    xgb_grid <- expand.grid(
      nrounds = c(100, 150, 200, 300, 500),
      max_depth = c(3, 5, 8, 10, 15),
      eta = c(0.05, 0.1, 0.15, 0.2),
      gamma = 0,
      colsample_bytree = 0.8,
      min_child_weight = 1,
      subsample = 0.8
    )
    
    train_control <- trainControl(
      method = "timeslice",
      initialWindow = 60,
      horizon = 9,
      fixedWindow = TRUE,
      verboseIter = FALSE
    )
    
    xgb_model <- train(
      x = as.data.frame(X_window_selected),
      y = y_window,
      method = "xgbTree",
      trControl = train_control,
      tuneGrid = xgb_grid,
      verbose = FALSE
    )
    
    arima_forecasts <- list()
    for (j in 1:ncol(X_window_selected)) {
      model <- auto.arima(X_window_selected[, j])
      arima_forecasts[[j]] <- as.numeric(forecast(model, h = forecast_horizon)$mean)
    }
    
    if (length(arima_forecasts) > 0) {
      arima_forecasts_matrix <- do.call(rbind, arima_forecasts)
      arima_forecasts_df <- as.data.frame(t(arima_forecasts_matrix))
      colnames(arima_forecasts_df) <- selected_var_names
      
      predictions_xgb <- predict(xgb_model, newdata = arima_forecasts_df)
      final_forecasts_mi_xgb[[i]] <- predictions_xgb
    }
  }
}
start_period <- "2017K2"
end_period <- "2021K2"
best_params <- xgb_model$bestTune
print(best_params)
xgb_model$results
ggplot(xgb_model$results, aes(x = nrounds, y = RMSE, color = factor(max_depth))) +
  geom_line() +
  facet_wrap(~ eta, scales = "free") +
  theme_minimal()

print(final_forecasts_mi_xgb)

first_two_columns <- stationary_table[, 1:2]
valid_mi_xgb <- which((window_size + seq_along(final_forecasts_mi_xgb) + forecast_horizon - 1) <= 200)
max_time_index <- max(window_size + valid_mi_xgb + forecast_horizon - 1)
n_needed <- max_time_index - nrow(first_two_columns)

#jei reikia pridėti eilutes
if (n_needed > 0) {
  last_time <- first_two_columns$Time[nrow(first_two_columns)]
  new_times <- generate_quarters(last_time, n_needed)
  new_rows <- as.data.frame(matrix(NA, nrow = n_needed, ncol = ncol(first_two_columns)))
  colnames(new_rows) <- colnames(first_two_columns)
  new_rows$Time <- new_times
  first_two_columns <- rbind(first_two_columns, new_rows)
}
# Random Forest — prognozės kiekvienam langui
all_mi_xgb_forecasts_shifted <- do.call(rbind, lapply(valid_mi_xgb, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_mi_xgb[[i]],
    Type = "MI su Xgb",
    Window = i
  )
}))

real_bvp_df <- data.frame(
  Time = first_two_columns$Time,
  Real = first_two_columns$BVP
)

#apkerpam laika
real_bvp_df_trimmed <- real_bvp_df[real_bvp_df$Time >= start_period & real_bvp_df$Time <= end_period, ]


ggplot() +
  geom_line(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), linewidth = 1.2) +
  geom_point(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), size = 2) +
  geom_line(data = subset(all_mi_xgb_forecasts_shifted, Type == "MI su Xgb"),
            aes(x = Time, y = Forecast, group = Window, color = interaction(Type, Window)),
            alpha = 0.6, linewidth = 0.9) +
  labs(
    title = "BVP prognozių palyginimas su Xgboost",
    x = "Ketvirtis", y = "BVP", color = "Modelis ir langas"
  ) +
  scale_color_manual(
    values = c("Reali reikšmė" = "darkgreen"),
    guide = "none"
  ) +
  scale_color_viridis_d(option = "plasma", begin = 0, end = 0.85, direction = -1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
forecast_shifted_df_mi_xgb<-all_mi_xgb_forecasts_shifted
forecast_shifted_df_mi_xgb$Horizon <- rep(1:forecast_horizon, times = length(unique(forecast_shifted_df_mi_xgb$Window)))
forecast_shifted_df_mi_xgb <- do.call(rbind, lapply(valid_mi_xgb, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_mi_xgb[[i]],
    Horizon = 1:forecast_horizon,
    Window = i
  )
}))
forecast_shifted_df_mi_xgb
ggplot() +
  geom_line(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), linewidth = 1.2) +
  geom_point(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), size = 2) +
  
  geom_line(data = forecast_shifted_df_mi_xgb,
            aes(x = Time, y = Forecast, color = as.factor(Horizon), group = Horizon),
            linewidth = 1.1) +
  
  labs(
    title = "Prognozės pagal horizontus",
    x = "Ketvirtis", y = "BVP", color = "Prognozės horizontas"
  ) +
  scale_color_viridis_d(option = "plasma", begin = 0, end = 0.9) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



######################################################################################################################
######################################################################################################################
################################################################XGBOOST+XGBOOST 2021-2027#######################################
library(tidyfit)
library(pbapply)
library(forecast)
library(zoo)
library(glmnet)
library(randomForest)
library(xgboost)
library(caret)

window_size <- 78
forecast_horizon <- 9
target_time <- "2024K4"
max_index <- which(stationary_table$Time == target_time)

X <- as.matrix(stationary_table[, -c(1, 2)])
y <- as.numeric(unlist(stationary_table[, 2]))

set.seed(123)
final_forecasts_xgb_xgb <- list()

for (i in 1:(max_index - window_size + 1)) {
  X_window <- X[i:(i + window_size - 1), ]
  y_window <- y[i:(i + window_size - 1)]
  
  dtrain <- xgb.DMatrix(data = X_window, label = y_window)
  model_importance <- xgboost(
    data = dtrain,
    max.depth = 5, eta = 0.1,
    nrounds = 100,
    objective = "reg:squarederror",
    verbose = 0
  )
  
  importance <- xgb.importance(model = model_importance)
  
  if (nrow(importance) == 0) next 
  
  top_n <- min(20, nrow(importance))
  top_20_vars <- importance$Feature[1:top_n]
  
  X_window_selected <- X_window[, top_20_vars, drop = FALSE]
  
  # Grid search
  xgb_grid <- expand.grid(
    nrounds = c(100, 150, 200, 300, 500),
    max_depth = c(3, 5, 8, 10, 15),
    eta = c(0.05, 0.1, 0.15, 0.2),
    gamma = 0,
    colsample_bytree = 0.8,
    min_child_weight = 1,
    subsample = 0.8
  )
  
  train_control <- trainControl(
    method = "timeslice",
    initialWindow = 60,
    horizon = 9,
    fixedWindow = TRUE,
    verboseIter = FALSE
  )
  
  xgb_model <- train(
    x = as.data.frame(X_window_selected),
    y = y_window,
    method = "xgbTree",
    trControl = train_control,
    tuneGrid = xgb_grid,
    verbose = FALSE
  )
  
  arima_forecasts <- list()
  for (j in 1:ncol(X_window_selected)) {
    model <- auto.arima(X_window_selected[, j])
    arima_forecasts[[j]] <- as.numeric(forecast(model, h = forecast_horizon)$mean)
  }
  
  if (length(arima_forecasts) > 0) {
    arima_forecasts_matrix <- do.call(rbind, arima_forecasts)
    arima_forecasts_df <- as.data.frame(t(arima_forecasts_matrix))
    colnames(arima_forecasts_df) <- colnames(X_window_selected)
    
    predictions_xgb <- predict(xgb_model, newdata = arima_forecasts_df)
    final_forecasts_xgb_xgb[[i]] <- predictions_xgb
  }
}

best_params <- xgb_model$bestTune
print(best_params)
xgb_model$results
ggplot(xgb_model$results, aes(x = nrounds, y = RMSE, color = factor(max_depth))) +
  geom_line() +
  facet_wrap(~ eta, scales = "free") +
  theme_minimal()

print(final_forecasts_xgb_xgb)

first_two_columns <- stationary_table[, 1:2]
valid_xgb_xgb <- which((window_size + seq_along(final_forecasts_xgb_xgb) + forecast_horizon - 1) <= 200)
max_time_index <- max(window_size + valid_xgb_xgb + forecast_horizon - 1)
n_needed <- max_time_index - nrow(first_two_columns)

#jei reikia pridėti eilutes
if (n_needed > 0) {
  last_time <- first_two_columns$Time[nrow(first_two_columns)]
  new_times <- generate_quarters(last_time, n_needed)
  new_rows <- as.data.frame(matrix(NA, nrow = n_needed, ncol = ncol(first_two_columns)))
  colnames(new_rows) <- colnames(first_two_columns)
  new_rows$Time <- new_times
  first_two_columns <- rbind(first_two_columns, new_rows)
}
# Random Forest — prognozės kiekvienam langui
all_xgb_xgb_forecasts_shifted <- do.call(rbind, lapply(valid_xgb_xgb, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_xgb_xgb[[i]],
    Type = "Xgb su Xgb",
    Window = i
  )
}))

real_bvp_df <- data.frame(
  Time = first_two_columns$Time,
  Real = first_two_columns$BVP
)

#apkerpam laika
real_bvp_df_trimmed <- real_bvp_df[(window_size + 1):nrow(real_bvp_df), ]


ggplot() +
  geom_line(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), linewidth = 1.2) +
  geom_point(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), size = 2) +
  geom_line(data = subset(all_xgb_xgb_forecasts_shifted, Type == "Xgb su Xgb"),
            aes(x = Time, y = Forecast, group = Window, color = interaction(Type, Window)),
            alpha = 0.6, linewidth = 0.9) +
  labs(
    title = "BVP prognozių palyginimas su Xgboost",
    x = "Ketvirtis", y = "BVP", color = "Modelis ir langas"
  ) +
  scale_color_manual(
    values = c("Reali reikšmė" = "darkgreen"),
    guide = "none"
  ) +
  scale_color_viridis_d(option = "plasma", begin = 0, end = 0.85, direction = -1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
forecast_shifted_df_xgb_xgb<-all_xgb_xgb_forecasts_shifted
forecast_shifted_df_xgb_xgb$Horizon <- rep(1:forecast_horizon, times = length(unique(forecast_shifted_df_xgb_xgb$Window)))
forecast_shifted_df_xgb_xgb <- do.call(rbind, lapply(valid_xgb_xgb, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_xgb_xgb[[i]],
    Horizon = 1:forecast_horizon,
    Window = i
  )
}))
forecast_shifted_df_xgb_xgb
num_windows <- length(unique(forecast_shifted_df_xgb_xgb$Horizon))

blue_shades <- viridis(num_windows, option = "plasma", begin = 0, end = 0.9, direction = -1)
x14 <- ggplot() +
  geom_line(data = real_bvp_df_trimmed,
            aes(x = Time, y = Real, color = "Realybė", group = 1),
            size = 1.2) +
  geom_point(data = real_bvp_df_trimmed,
             aes(x = Time, y = Real, color = "Realybė")) +
  
  geom_line(data = forecast_shifted_df_xgb_xgb,
            aes(x = Time, y = Forecast, group = Horizon, color = as.factor(Horizon)),
            size = 1.2, linetype = "dashed") +
  geom_point(data = forecast_shifted_df_xgb_xgb,
             aes(x = Time, y = Forecast, color = as.factor(Horizon))) +
  
  scale_color_manual(
    values = c(setNames(blue_shades, as.character(unique(forecast_shifted_df_xgb_xgb$Horizon))),
               "Realybė" = "red"),
    guide = guide_legend(override.aes = list(linetype = c(rep("dashed", num_windows), "solid"),
                                             size = c(rep(1.2, num_windows), 1.2)), title = "Horizontai")
  ) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )+
  labs(title = "BVP prognozės (XGBOOST su XGBOOST) ir realybė pagal horizontus",
       x = "Kvartalas",
       y = "BVP",
       color = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold")) 


ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs68.png", plot = x14, width = 10, height = 6, dpi = 300, bg = "white")



forecast_shifted_df_xgb_xgb <- merge(forecast_shifted_df_xgb_xgb, real_bvp_df_trimmed, by = "Time")
forecast_shifted_df_xgb_xgb <- forecast_shifted_df_xgb_xgb[!is.na(forecast_shifted_df_xgb_xgb$Real),]
rmse_post_by_horizon <- aggregate(cbind(Forecast, Real) ~ Horizon, data = forecast_shifted_df_xgb_xgb, FUN = function(x) x)
rmse_post_by_horizon$RMSE <- sapply(unique(forecast_shifted_df_xgb_xgb$Horizon), function(h) {
  rmse_vals <- forecast_shifted_df_xgb_xgb[forecast_shifted_df_xgb_xgb$Horizon == h, ]
  rmse(rmse_vals$Real, rmse_vals$Forecast)
})
print(rmse_post_by_horizon[, c("Horizon", "RMSE")])



rmse_plot_df <- data.frame(
  Horizon = rep(1:forecast_horizon, 2),
  RMSE = rmse_post_by_horizon$RMSE,
  Model = rep("XGBOOST", each = forecast_horizon)
)

err_Xb_Xb_21_27 <- errors_by_h(forecast_shifted_df_xgb_xgb)


rfr3 <- ggplot(rmse_plot_df, aes(x = Horizon, y = RMSE, colour = Model)) +
  geom_line(size = 1.3, alpha = 0.9) +
  geom_point(,size = 3) +
  scale_color_manual(values = c("XGBOOST" = "red"), name = "")+
  scale_x_continuous(labels = number_format(accuracy = 1)) + 
  labs(
    title = "2021K1-2024K4",
    x = "Horizontas",
    y = "RMSE",
    color = "Modelis"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )

######################################################################################################################
######################################################################################################################
################################################################XGBOOST+XGBOOST 2019-2023#######################################
library(tidyfit)
library(pbapply)
library(forecast)
library(zoo)
library(glmnet)
library(randomForest)
library(xgboost)
library(caret)

window_size <- 79
forecast_horizon <- 8
target_time <- "2021K2"
max_index <- which(stationary_table$Time == target_time)

X <- as.matrix(stationary_table[, -c(1, 2)])
y <- as.numeric(unlist(stationary_table[, 2]))

set.seed(123)
final_forecasts_xgb_xgb <- list()

for (i in 1:(max_index - window_size + 1)) {
  X_window <- X[i:(i + window_size - 1), ]
  y_window <- y[i:(i + window_size - 1)]
  
  # Первичная модель для отбора важных переменных
  dtrain <- xgb.DMatrix(data = X_window, label = y_window)
  model_importance <- xgboost(
    data = dtrain,
    max.depth = 5, eta = 0.1,
    nrounds = 100,
    objective = "reg:squarederror",
    verbose = 0
  )
  
  importance <- xgb.importance(model = model_importance)
  
  if (nrow(importance) == 0) next 
  
  top_n <- min(20, nrow(importance))
  top_20_vars <- importance$Feature[1:top_n]
  
  X_window_selected <- X_window[, top_20_vars, drop = FALSE]
  
  # Grid search
  xgb_grid <- expand.grid(
    nrounds = c(100, 150, 200, 300, 500),
    max_depth = c(3, 5, 8, 10, 15),
    eta = c(0.05, 0.1, 0.15, 0.2),
    gamma = 0,
    colsample_bytree = 0.8,
    min_child_weight = 1,
    subsample = 0.8
  )
  
  train_control <- trainControl(
    method = "timeslice",
    initialWindow = 60,
    horizon = 9,
    fixedWindow = TRUE,
    verboseIter = FALSE
  )
  
  # Финальный XGBoost
  xgb_model <- train(
    x = as.data.frame(X_window_selected),
    y = y_window,
    method = "xgbTree",
    trControl = train_control,
    tuneGrid = xgb_grid,
    verbose = FALSE
  )
  
  arima_forecasts <- list()
  for (j in 1:ncol(X_window_selected)) {
    model <- auto.arima(X_window_selected[, j])
    arima_forecasts[[j]] <- as.numeric(forecast(model, h = forecast_horizon)$mean)
  }
  
  if (length(arima_forecasts) > 0) {
    arima_forecasts_matrix <- do.call(rbind, arima_forecasts)
    arima_forecasts_df <- as.data.frame(t(arima_forecasts_matrix))
    colnames(arima_forecasts_df) <- colnames(X_window_selected)
    
    predictions_xgb <- predict(xgb_model, newdata = arima_forecasts_df)
    final_forecasts_xgb_xgb[[i]] <- predictions_xgb
  }
}

start_period <- "2019K2"
end_period <- "2023K2"
best_params <- xgb_model$bestTune
print(best_params)
xgb_model$results
ggplot(xgb_model$results, aes(x = nrounds, y = RMSE, color = factor(max_depth))) +
  geom_line() +
  facet_wrap(~ eta, scales = "free") +
  theme_minimal()

print(final_forecasts_xgb_xgb)

first_two_columns <- stationary_table[, 1:2]
valid_xgb_xgb <- which((window_size + seq_along(final_forecasts_xgb_xgb) + forecast_horizon - 1) <= 200)
max_time_index <- max(window_size + valid_xgb_xgb + forecast_horizon - 1)
n_needed <- max_time_index - nrow(first_two_columns)

#jei reikia pridėti eilutes
if (n_needed > 0) {
  last_time <- first_two_columns$Time[nrow(first_two_columns)]
  new_times <- generate_quarters(last_time, n_needed)
  new_rows <- as.data.frame(matrix(NA, nrow = n_needed, ncol = ncol(first_two_columns)))
  colnames(new_rows) <- colnames(first_two_columns)
  new_rows$Time <- new_times
  first_two_columns <- rbind(first_two_columns, new_rows)
}
# Random Forest — prognozės kiekvienam langui
all_xgb_xgb_forecasts_shifted <- do.call(rbind, lapply(valid_xgb_xgb, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_xgb_xgb[[i]],
    Type = "Xgb su Xgb",
    Window = i
  )
}))

real_bvp_df <- data.frame(
  Time = first_two_columns$Time,
  Real = first_two_columns$BVP
)

#apkerpam laika
real_bvp_df_trimmed <- real_bvp_df[real_bvp_df$Time >= start_period & real_bvp_df$Time <= end_period, ]


ggplot() +
  geom_line(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), linewidth = 1.2) +
  geom_point(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), size = 2) +
  geom_line(data = subset(all_xgb_xgb_forecasts_shifted, Type == "Xgb su Xgb"),
            aes(x = Time, y = Forecast, group = Window, color = interaction(Type, Window)),
            alpha = 0.6, linewidth = 0.9) +
  labs(
    title = "BVP prognozių palyginimas su Xgboost",
    x = "Ketvirtis", y = "BVP", color = "Modelis ir langas"
  ) +
  scale_color_manual(
    values = c("Reali reikšmė" = "darkgreen"),
    guide = "none"
  ) +
  scale_color_viridis_d(option = "plasma", begin = 0, end = 0.85, direction = -1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
forecast_shifted_df_xgb_xgb<-all_xgb_xgb_forecasts_shifted
forecast_shifted_df_xgb_xgb$Horizon <- rep(1:forecast_horizon, times = length(unique(forecast_shifted_df_xgb_xgb$Window)))
forecast_shifted_df_xgb_xgb <- do.call(rbind, lapply(valid_xgb_xgb, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_xgb_xgb[[i]],
    Horizon = 1:forecast_horizon,
    Window = i
  )
}))
forecast_shifted_df_xgb_xgb
num_windows <- length(unique(forecast_shifted_df_xgb_xgb$Horizon))

blue_shades <- viridis(num_windows, option = "plasma", begin = 0, end = 0.9, direction = -1)
x15 <- ggplot() +
  geom_line(data = real_bvp_df_trimmed,
            aes(x = Time, y = Real, color = "Realybė", group = 1),
            size = 1.2) +
  geom_point(data = real_bvp_df_trimmed,
             aes(x = Time, y = Real, color = "Realybė")) +
  
  geom_line(data = forecast_shifted_df_xgb_xgb,
            aes(x = Time, y = Forecast, group = Horizon, color = as.factor(Horizon)),
            size = 1.2, linetype = "dashed") +
  geom_point(data = forecast_shifted_df_xgb_xgb,
             aes(x = Time, y = Forecast, color = as.factor(Horizon))) +
  
  scale_color_manual(
    values = c(setNames(blue_shades, as.character(unique(forecast_shifted_df_xgb_xgb$Horizon))),
               "Realybė" = "red"),
    guide = guide_legend(override.aes = list(linetype = c(rep("dashed", num_windows), "solid"),
                                             size = c(rep(1.2, num_windows), 1.2)), title = "Horizontai")
  ) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )+
  labs(title = "BVP prognozės (XGBOOST su XGBOOST) ir realybė pagal horizontus",
       x = "Kvartalas",
       y = "BVP",
       color = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold")) 


ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs69.png", plot = x15, width = 10, height = 6, dpi = 300, bg = "white")



forecast_shifted_df_xgb_xgb <- merge(forecast_shifted_df_xgb_xgb, real_bvp_df_trimmed, by = "Time")
forecast_shifted_df_xgb_xgb <- forecast_shifted_df_xgb_xgb[!is.na(forecast_shifted_df_xgb_xgb$Real),]
rmse_post_by_horizon <- aggregate(cbind(Forecast, Real) ~ Horizon, data = forecast_shifted_df_xgb_xgb, FUN = function(x) x)
rmse_post_by_horizon$RMSE <- sapply(unique(forecast_shifted_df_xgb_xgb$Horizon), function(h) {
  rmse_vals <- forecast_shifted_df_xgb_xgb[forecast_shifted_df_xgb_xgb$Horizon == h, ]
  rmse(rmse_vals$Real, rmse_vals$Forecast)
})
print(rmse_post_by_horizon[, c("Horizon", "RMSE")])



rmse_plot_df <- data.frame(
  Horizon = rep(1:forecast_horizon, 2),
  RMSE = rmse_post_by_horizon$RMSE,
  Model = rep("XGBOOST", each = forecast_horizon)
)

err_Xb_Xb_19_23 <- errors_by_h(forecast_shifted_df_xgb_xgb)


rfr2 <- ggplot(rmse_plot_df, aes(x = Horizon, y = RMSE, colour = Model)) +
  geom_line(size = 1.3, alpha = 0.9) +
  geom_point(,size = 3) +
  scale_color_manual(values = c("XGBOOST" = "red"), name = "")+
  scale_x_continuous(labels = number_format(accuracy = 1)) + 
  labs(
    title = "2019K2-2023K2",
    x = "Horizontas",
    y = "RMSE",
    color = "Modelis"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )

######################################################################################################################
######################################################################################################################
################################################################XGBOOST+XGBOOST 2017-2021#######################################
library(tidyfit)
library(pbapply)
library(forecast)
library(zoo)
library(glmnet)
library(randomForest)
library(xgboost)
library(caret)

window_size <- 71
forecast_horizon <- 8
target_time <- "2019K2"
max_index <- which(stationary_table$Time == target_time)

X <- as.matrix(stationary_table[, -c(1, 2)])
y <- as.numeric(unlist(stationary_table[, 2]))

set.seed(123)
final_forecasts_xgb_xgb <- list()

for (i in 1:(max_index - window_size + 1)) {
  X_window <- X[i:(i + window_size - 1), ]
  y_window <- y[i:(i + window_size - 1)]
  
  # Первичная модель для отбора важных переменных
  dtrain <- xgb.DMatrix(data = X_window, label = y_window)
  model_importance <- xgboost(
    data = dtrain,
    max.depth = 5, eta = 0.1,
    nrounds = 100,
    objective = "reg:squarederror",
    verbose = 0
  )
  
  importance <- xgb.importance(model = model_importance)
  
  if (nrow(importance) == 0) next  # пропускаем, если не удалось определить важности
  
  top_n <- min(20, nrow(importance))
  top_20_vars <- importance$Feature[1:top_n]
  
  X_window_selected <- X_window[, top_20_vars, drop = FALSE]
  
  # Grid search
  xgb_grid <- expand.grid(
    nrounds = c(100, 150, 200, 300, 500),
    max_depth = c(3, 5, 8, 10, 15),
    eta = c(0.05, 0.1, 0.15, 0.2),
    gamma = 0,
    colsample_bytree = 0.8,
    min_child_weight = 1,
    subsample = 0.8
  )
  
  train_control <- trainControl(
    method = "timeslice",
    initialWindow = 60,
    horizon = 9,
    fixedWindow = TRUE,
    verboseIter = FALSE
  )
  
  # Финальный XGBoost
  xgb_model <- train(
    x = as.data.frame(X_window_selected),
    y = y_window,
    method = "xgbTree",
    trControl = train_control,
    tuneGrid = xgb_grid,
    verbose = FALSE
  )
  
  # ARIMA прогнозы по выбранным признакам
  arima_forecasts <- list()
  for (j in 1:ncol(X_window_selected)) {
    model <- auto.arima(X_window_selected[, j])
    arima_forecasts[[j]] <- as.numeric(forecast(model, h = forecast_horizon)$mean)
  }
  
  if (length(arima_forecasts) > 0) {
    arima_forecasts_matrix <- do.call(rbind, arima_forecasts)
    arima_forecasts_df <- as.data.frame(t(arima_forecasts_matrix))
    colnames(arima_forecasts_df) <- colnames(X_window_selected)
    
    predictions_xgb <- predict(xgb_model, newdata = arima_forecasts_df)
    final_forecasts_xgb_xgb[[i]] <- predictions_xgb
  }
}
start_period <- "2017K2"
end_period <- "2021K2"
best_params <- xgb_model$bestTune
print(best_params)
xgb_model$results
ggplot(xgb_model$results, aes(x = nrounds, y = RMSE, color = factor(max_depth))) +
  geom_line() +
  facet_wrap(~ eta, scales = "free") +
  theme_minimal()

print(final_forecasts_xgb_xgb)

first_two_columns <- stationary_table[, 1:2]
valid_xgb_xgb <- which((window_size + seq_along(final_forecasts_xgb_xgb) + forecast_horizon - 1) <= 200)
max_time_index <- max(window_size + valid_xgb_xgb + forecast_horizon - 1)
n_needed <- max_time_index - nrow(first_two_columns)

#jei reikia pridėti eilutes
if (n_needed > 0) {
  last_time <- first_two_columns$Time[nrow(first_two_columns)]
  new_times <- generate_quarters(last_time, n_needed)
  new_rows <- as.data.frame(matrix(NA, nrow = n_needed, ncol = ncol(first_two_columns)))
  colnames(new_rows) <- colnames(first_two_columns)
  new_rows$Time <- new_times
  first_two_columns <- rbind(first_two_columns, new_rows)
}
# Random Forest — prognozės kiekvienam langui
all_xgb_xgb_forecasts_shifted <- do.call(rbind, lapply(valid_xgb_xgb, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_xgb_xgb[[i]],
    Type = "Xgb su Xgb",
    Window = i
  )
}))

real_bvp_df <- data.frame(
  Time = first_two_columns$Time,
  Real = first_two_columns$BVP
)

#apkerpam laika
real_bvp_df_trimmed <- real_bvp_df[real_bvp_df$Time >= start_period & real_bvp_df$Time <= end_period, ]


ggplot() +
  geom_line(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), linewidth = 1.2) +
  geom_point(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), size = 2) +
  geom_line(data = subset(all_xgb_xgb_forecasts_shifted, Type == "Xgb su Xgb"),
            aes(x = Time, y = Forecast, group = Window, color = interaction(Type, Window)),
            alpha = 0.6, linewidth = 0.9) +
  labs(
    title = "BVP prognozių palyginimas su Xgboost",
    x = "Ketvirtis", y = "BVP", color = "Modelis ir langas"
  ) +
  scale_color_manual(
    values = c("Reali reikšmė" = "darkgreen"),
    guide = "none"
  ) +
  scale_color_viridis_d(option = "plasma", begin = 0, end = 0.85, direction = -1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
forecast_shifted_df_xgb_xgb<-all_xgb_xgb_forecasts_shifted
forecast_shifted_df_xgb_xgb$Horizon <- rep(1:forecast_horizon, times = length(unique(forecast_shifted_df_xgb_xgb$Window)))
forecast_shifted_df_xgb_xgb <- do.call(rbind, lapply(valid_xgb_xgb, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_xgb_xgb[[i]],
    Horizon = 1:forecast_horizon,
    Window = i
  )
}))
forecast_shifted_df_xgb_xgb
forecast_shifted_df_xgb_xgb
num_windows <- length(unique(forecast_shifted_df_xgb_xgb$Horizon))

blue_shades <- viridis(num_windows, option = "plasma", begin = 0, end = 0.9, direction = -1)
x16 <- ggplot() +
  geom_line(data = real_bvp_df_trimmed,
            aes(x = Time, y = Real, color = "Realybė", group = 1),
            size = 1.2) +
  geom_point(data = real_bvp_df_trimmed,
             aes(x = Time, y = Real, color = "Realybė")) +
  
  geom_line(data = forecast_shifted_df_xgb_xgb,
            aes(x = Time, y = Forecast, group = Horizon, color = as.factor(Horizon)),
            size = 1.2, linetype = "dashed") +
  geom_point(data = forecast_shifted_df_xgb_xgb,
             aes(x = Time, y = Forecast, color = as.factor(Horizon))) +
  
  scale_color_manual(
    values = c(setNames(blue_shades, as.character(unique(forecast_shifted_df_xgb_xgb$Horizon))),
               "Realybė" = "red"),
    guide = guide_legend(override.aes = list(linetype = c(rep("dashed", num_windows), "solid"),
                                             size = c(rep(1.2, num_windows), 1.2)), title = "Horizontai")
  ) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )+
  labs(title = "BVP prognozės (XGBOOST su XGBOOST) ir realybė pagal horizontus",
       x = "Kvartalas",
       y = "BVP",
       color = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold")) 


ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs70.png", plot = x16, width = 10, height = 6, dpi = 300, bg = "white")



forecast_shifted_df_xgb_xgb <- merge(forecast_shifted_df_xgb_xgb, real_bvp_df_trimmed, by = "Time")
forecast_shifted_df_xgb_xgb <- forecast_shifted_df_xgb_xgb[!is.na(forecast_shifted_df_xgb_xgb$Real),]
rmse_post_by_horizon <- aggregate(cbind(Forecast, Real) ~ Horizon, data = forecast_shifted_df_xgb_xgb, FUN = function(x) x)
rmse_post_by_horizon$RMSE <- sapply(unique(forecast_shifted_df_xgb_xgb$Horizon), function(h) {
  rmse_vals <- forecast_shifted_df_xgb_xgb[forecast_shifted_df_xgb_xgb$Horizon == h, ]
  rmse(rmse_vals$Real, rmse_vals$Forecast)
})
print(rmse_post_by_horizon[, c("Horizon", "RMSE")])



rmse_plot_df <- data.frame(
  Horizon = rep(1:forecast_horizon, 2),
  RMSE = rmse_post_by_horizon$RMSE,
  Model = rep("XGBOOST", each = forecast_horizon)
)

err_Xb_Xb_17_21 <- errors_by_h(forecast_shifted_df_xgb_xgb)


rfr1 <- ggplot(rmse_plot_df, aes(x = Horizon, y = RMSE, colour = Model)) +
  geom_line(size = 1.3, alpha = 0.9) +
  geom_point(,size = 3) +
  scale_color_manual(values = c("XGBOOST" = "red"), name = "")+
  scale_x_continuous(labels = number_format(accuracy = 1)) + 
  labs(
    title = "2017K2-2021K2",
    x = "Horizontas",
    y = "RMSE",
    color = "Modelis"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )


top_row <- rfr1 + rfr2

bottom_row <- plot_spacer() + rfr3 + plot_spacer()
bottom_row <- bottom_row + plot_layout(widths = c(1, 2, 1))

final_plot <- (top_row / bottom_row) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

final_plot <- final_plot +
  plot_layout(guides = "collect") +   
  plot_annotation(
    title = "RMSE pagal horizontus",
    theme = theme(
      plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),legend.position = "bottom"  
    )
  )

x11()
final_plot
ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs71.png", plot = final_plot, width = 10, height = 6, dpi = 300, bg = "white")




################################################################################---------linear------
################################################################XGBOOST+XGBOOST 2021-2027#######################################
library(tidyfit)
library(pbapply)
library(forecast)
library(zoo)
library(glmnet)
library(randomForest)
library(xgboost)
library(caret)

window_size <- 78
forecast_horizon <- 9
target_time <- "2024K4"
max_index <- which(stationary_table$Time == target_time)

X <- as.matrix(stationary_table[, -c(1, 2)])
y <- as.numeric(unlist(stationary_table[, 2]))

set.seed(123)
final_forecasts_xgb_xgb <- list()

for (i in 1:(max_index - window_size + 1)) {
  
  # 1) Langas
  X_window <- X[i:(i + window_size - 1), ]
  y_window <- y[i:(i + window_size - 1)]
  
  # 2) Pirminis XGBoost kintamųjų svarbai įvertinti
  dtrain <- xgb.DMatrix(data = X_window, label = y_window)
  model_importance <- xgboost(
    data        = dtrain,
    max.depth   = 5,
    eta         = 0.1,
    nrounds     = 100,
    objective   = "reg:squarederror",
    verbose     = 0
  )
  
  importance <- xgb.importance(model = model_importance)
  if (nrow(importance) == 0) next                # jei svarbų nėra – praleidžiam
  
  top_n <- min(20, nrow(importance))             # ne daugiau kaip 20
  top_vars <- importance$Feature[1:top_n]
  
  X_sel <- X_window[, top_vars, drop = FALSE]
  
  # 3) Grid-search XGB (linearinis booster)
  xgb_grid <- expand.grid(
    nrounds = c(100, 150, 200),
    eta     = c(0.05, 0.1, 0.15),
    lambda  = c(0, 0.1, 1),
    alpha   = c(0, 0.1, 1)
  )
  
  train_control <- trainControl(
    method        = "timeslice",
    initialWindow = 60,
    horizon       = 9,
    fixedWindow   = TRUE,
    verboseIter   = FALSE
  )
  
  xgb_model_final <- train(
    x         = as.data.frame(X_sel),
    y         = y_window,
    method    = "xgbLinear",
    tuneGrid  = xgb_grid,
    trControl = train_control,
    verbose   = FALSE
  )
  
  # 4) ARIMA prognozės atrinktiems kintamiesiems
  arima_fc <- lapply(seq_len(ncol(X_sel)), function(j) {
    forecast(auto.arima(X_sel[, j]), h = forecast_horizon)$mean
  })
  
  # 5) Prognozės su XGBoost
  arima_df <- setNames(as.data.frame(do.call(cbind, arima_fc)), colnames(X_sel))
  preds    <- predict(xgb_model_final, newdata = arima_df)
  
  final_forecasts_xgb_xgb[[i]] <- preds
}

first_two_columns <- stationary_table[, 1:2]
valid_xgb_xgb <- which((window_size + seq_along(final_forecasts_xgb_xgb) + forecast_horizon - 1) <= 200)
max_time_index <- max(window_size + valid_xgb_xgb + forecast_horizon - 1)
n_needed <- max_time_index - nrow(first_two_columns)

#jei reikia pridėti eilutes
if (n_needed > 0) {
  last_time <- first_two_columns$Time[nrow(first_two_columns)]
  new_times <- generate_quarters(last_time, n_needed)
  new_rows <- as.data.frame(matrix(NA, nrow = n_needed, ncol = ncol(first_two_columns)))
  colnames(new_rows) <- colnames(first_two_columns)
  new_rows$Time <- new_times
  first_two_columns <- rbind(first_two_columns, new_rows)
}
# Random Forest — prognozės kiekvienam langui
all_xgb_xgb_forecasts_shifted <- do.call(rbind, lapply(valid_xgb_xgb, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_xgb_xgb[[i]],
    Type = "Xgb su Xgb",
    Window = i
  )
}))

real_bvp_df <- data.frame(
  Time = first_two_columns$Time,
  Real = first_two_columns$BVP
)

#apkerpam laika
real_bvp_df_trimmed <- real_bvp_df[(window_size + 1):nrow(real_bvp_df), ]


ggplot() +
  geom_line(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), linewidth = 1.2) +
  geom_point(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), size = 2) +
  geom_line(data = subset(all_xgb_xgb_forecasts_shifted, Type == "Xgb su Xgb"),
            aes(x = Time, y = Forecast, group = Window, color = interaction(Type, Window)),
            alpha = 0.6, linewidth = 0.9) +
  labs(
    title = "BVP prognozių palyginimas su Xgboost",
    x = "Ketvirtis", y = "BVP", color = "Modelis ir langas"
  ) +
  scale_color_manual(
    values = c("Reali reikšmė" = "darkgreen"),
    guide = "none"
  ) +
  scale_color_viridis_d(option = "plasma", begin = 0, end = 0.85, direction = -1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
forecast_shifted_df_xgb_xgb<-all_xgb_xgb_forecasts_shifted
forecast_shifted_df_xgb_xgb$Horizon <- rep(1:forecast_horizon, times = length(unique(forecast_shifted_df_xgb_xgb$Window)))
forecast_shifted_df_xgb_xgb <- do.call(rbind, lapply(valid_xgb_xgb, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_xgb_xgb[[i]],
    Horizon = 1:forecast_horizon,
    Window = i
  )
}))
forecast_shifted_df_xgb_xgb
num_windows <- length(unique(forecast_shifted_df_xgb_xgb$Horizon))

blue_shades <- viridis(num_windows, option = "plasma", begin = 0, end = 0.9, direction = -1)
x17 <- ggplot() +
  geom_line(data = real_bvp_df_trimmed,
            aes(x = Time, y = Real, color = "Realybė", group = 1),
            size = 1.2) +
  geom_point(data = real_bvp_df_trimmed,
             aes(x = Time, y = Real, color = "Realybė")) +
  
  geom_line(data = forecast_shifted_df_xgb_xgb,
            aes(x = Time, y = Forecast, group = Horizon, color = as.factor(Horizon)),
            size = 1.2, linetype = "dashed") +
  geom_point(data = forecast_shifted_df_xgb_xgb,
             aes(x = Time, y = Forecast, color = as.factor(Horizon))) +
  
  scale_color_manual(
    values = c(setNames(blue_shades, as.character(unique(forecast_shifted_df_xgb_xgb$Horizon))),
               "Realybė" = "red"),
    guide = guide_legend(override.aes = list(linetype = c(rep("dashed", num_windows), "solid"),
                                             size = c(rep(1.2, num_windows), 1.2)), title = "Horizontai")
  ) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )+
  labs(title = "BVP prognozės (XGBOOST su XGBOOST) ir realybė pagal horizontus",
       x = "Kvartalas",
       y = "BVP",
       color = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold")) 


ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs72.png", plot = x17, width = 10, height = 6, dpi = 300, bg = "white")



forecast_shifted_df_xgb_xgb <- merge(forecast_shifted_df_xgb_xgb, real_bvp_df_trimmed, by = "Time")
forecast_shifted_df_xgb_xgb <- forecast_shifted_df_xgb_xgb[!is.na(forecast_shifted_df_xgb_xgb$Real),]
rmse_post_by_horizon <- aggregate(cbind(Forecast, Real) ~ Horizon, data = forecast_shifted_df_xgb_xgb, FUN = function(x) x)
rmse_post_by_horizon$RMSE <- sapply(unique(forecast_shifted_df_xgb_xgb$Horizon), function(h) {
  rmse_vals <- forecast_shifted_df_xgb_xgb[forecast_shifted_df_xgb_xgb$Horizon == h, ]
  rmse(rmse_vals$Real, rmse_vals$Forecast)
})
print(rmse_post_by_horizon[, c("Horizon", "RMSE")])



rmse_plot_df <- data.frame(
  Horizon = rep(1:forecast_horizon, 2),
  RMSE = rmse_post_by_horizon$RMSE,
  Model = rep("XGBOOST", each = forecast_horizon)
)

err_Xb_Xb_21_27_linear <- errors_by_h(forecast_shifted_df_xgb_xgb)


rfr3 <- ggplot(rmse_plot_df, aes(x = Horizon, y = RMSE, colour = Model)) +
  geom_line(size = 1.3, alpha = 0.9) +
  geom_point(,size = 3) +
  scale_color_manual(values = c("XGBOOST" = "red"), name = "")+
  scale_x_continuous(labels = number_format(accuracy = 1)) + 
  labs(
    title = "2021K1-2024K4",
    x = "Horizontas",
    y = "RMSE",
    color = "Modelis"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )


######################################################################################################################
################################################################XGBOOST+XGBOOST 2019-2023#######################################
library(tidyfit)
library(pbapply)
library(forecast)
library(zoo)
library(glmnet)
library(randomForest)
library(xgboost)
library(caret)

window_size <- 79
forecast_horizon <- 8
target_time <- "2021K2"
max_index <- which(stationary_table$Time == target_time)

X <- as.matrix(stationary_table[, -c(1, 2)])
y <- as.numeric(unlist(stationary_table[, 2]))

set.seed(123)
final_forecasts_xgb_xgb <- list()

for (i in 1:(max_index - window_size + 1)) {
  
  # 1) Langas
  X_window <- X[i:(i + window_size - 1), ]
  y_window <- y[i:(i + window_size - 1)]
  
  # 2) Pirminis XGBoost kintamųjų svarbai įvertinti
  dtrain <- xgb.DMatrix(data = X_window, label = y_window)
  model_importance <- xgboost(
    data        = dtrain,
    max.depth   = 5,
    eta         = 0.1,
    nrounds     = 100,
    objective   = "reg:squarederror",
    verbose     = 0
  )
  
  importance <- xgb.importance(model = model_importance)
  if (nrow(importance) == 0) next                # jei svarbų nėra – praleidžiam
  
  top_n <- min(20, nrow(importance))             # ne daugiau kaip 20
  top_vars <- importance$Feature[1:top_n]
  
  X_sel <- X_window[, top_vars, drop = FALSE]
  
  # 3) Grid-search XGB (linearinis booster)
  xgb_grid <- expand.grid(
    nrounds = c(100, 150, 200),
    eta     = c(0.05, 0.1, 0.15),
    lambda  = c(0, 0.1, 1),
    alpha   = c(0, 0.1, 1)
  )
  
  train_control <- trainControl(
    method        = "timeslice",
    initialWindow = 60,
    horizon       = 9,
    fixedWindow   = TRUE,
    verboseIter   = FALSE
  )
  
  xgb_model_final <- train(
    x         = as.data.frame(X_sel),
    y         = y_window,
    method    = "xgbLinear",
    tuneGrid  = xgb_grid,
    trControl = train_control,
    verbose   = FALSE
  )
  
  # 4) ARIMA prognozės atrinktiems kintamiesiems
  arima_fc <- lapply(seq_len(ncol(X_sel)), function(j) {
    forecast(auto.arima(X_sel[, j]), h = forecast_horizon)$mean
  })
  
  # 5) Prognozės su XGBoost
  arima_df <- setNames(as.data.frame(do.call(cbind, arima_fc)), colnames(X_sel))
  preds    <- predict(xgb_model_final, newdata = arima_df)
  
  final_forecasts_xgb_xgb[[i]] <- preds
}
start_period <- "2019K2"
end_period <- "2023K2"

first_two_columns <- stationary_table[, 1:2]
valid_xgb_xgb <- which((window_size + seq_along(final_forecasts_xgb_xgb) + forecast_horizon - 1) <= 200)
max_time_index <- max(window_size + valid_xgb_xgb + forecast_horizon - 1)
n_needed <- max_time_index - nrow(first_two_columns)

#jei reikia pridėti eilutes
if (n_needed > 0) {
  last_time <- first_two_columns$Time[nrow(first_two_columns)]
  new_times <- generate_quarters(last_time, n_needed)
  new_rows <- as.data.frame(matrix(NA, nrow = n_needed, ncol = ncol(first_two_columns)))
  colnames(new_rows) <- colnames(first_two_columns)
  new_rows$Time <- new_times
  first_two_columns <- rbind(first_two_columns, new_rows)
}
# Random Forest — prognozės kiekvienam langui
all_xgb_xgb_forecasts_shifted <- do.call(rbind, lapply(valid_xgb_xgb, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_xgb_xgb[[i]],
    Type = "Xgb su Xgb",
    Window = i
  )
}))

real_bvp_df <- data.frame(
  Time = first_two_columns$Time,
  Real = first_two_columns$BVP
)

#apkerpam laika
real_bvp_df_trimmed <- real_bvp_df[real_bvp_df$Time >= start_period & real_bvp_df$Time <= end_period, ]


ggplot() +
  geom_line(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), linewidth = 1.2) +
  geom_point(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), size = 2) +
  geom_line(data = subset(all_xgb_xgb_forecasts_shifted, Type == "Xgb su Xgb"),
            aes(x = Time, y = Forecast, group = Window, color = interaction(Type, Window)),
            alpha = 0.6, linewidth = 0.9) +
  labs(
    title = "BVP prognozių palyginimas su Xgboost",
    x = "Ketvirtis", y = "BVP", color = "Modelis ir langas"
  ) +
  scale_color_manual(
    values = c("Reali reikšmė" = "darkgreen"),
    guide = "none"
  ) +
  scale_color_viridis_d(option = "plasma", begin = 0, end = 0.85, direction = -1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
forecast_shifted_df_xgb_xgb<-all_xgb_xgb_forecasts_shifted
forecast_shifted_df_xgb_xgb$Horizon <- rep(1:forecast_horizon, times = length(unique(forecast_shifted_df_xgb_xgb$Window)))
forecast_shifted_df_xgb_xgb <- do.call(rbind, lapply(valid_xgb_xgb, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_xgb_xgb[[i]],
    Horizon = 1:forecast_horizon,
    Window = i
  )
}))
forecast_shifted_df_xgb_xgb
num_windows <- length(unique(forecast_shifted_df_xgb_xgb$Horizon))

blue_shades <- viridis(num_windows, option = "plasma", begin = 0, end = 0.9, direction = -1)
x18 <- ggplot() +
  geom_line(data = real_bvp_df_trimmed,
            aes(x = Time, y = Real, color = "Realybė", group = 1),
            size = 1.2) +
  geom_point(data = real_bvp_df_trimmed,
             aes(x = Time, y = Real, color = "Realybė")) +
  
  geom_line(data = forecast_shifted_df_xgb_xgb,
            aes(x = Time, y = Forecast, group = Horizon, color = as.factor(Horizon)),
            size = 1.2, linetype = "dashed") +
  geom_point(data = forecast_shifted_df_xgb_xgb,
             aes(x = Time, y = Forecast, color = as.factor(Horizon))) +
  
  scale_color_manual(
    values = c(setNames(blue_shades, as.character(unique(forecast_shifted_df_xgb_xgb$Horizon))),
               "Realybė" = "red"),
    guide = guide_legend(override.aes = list(linetype = c(rep("dashed", num_windows), "solid"),
                                             size = c(rep(1.2, num_windows), 1.2)), title = "Horizontai")
  ) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )+
  labs(title = "BVP prognozės (XGBOOST su XGBOOST) ir realybė pagal horizontus",
       x = "Kvartalas",
       y = "BVP",
       color = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold")) 


ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs73.png", plot = x18, width = 10, height = 6, dpi = 300, bg = "white")



forecast_shifted_df_xgb_xgb <- merge(forecast_shifted_df_xgb_xgb, real_bvp_df_trimmed, by = "Time")
forecast_shifted_df_xgb_xgb <- forecast_shifted_df_xgb_xgb[!is.na(forecast_shifted_df_xgb_xgb$Real),]
rmse_post_by_horizon <- aggregate(cbind(Forecast, Real) ~ Horizon, data = forecast_shifted_df_xgb_xgb, FUN = function(x) x)
rmse_post_by_horizon$RMSE <- sapply(unique(forecast_shifted_df_xgb_xgb$Horizon), function(h) {
  rmse_vals <- forecast_shifted_df_xgb_xgb[forecast_shifted_df_xgb_xgb$Horizon == h, ]
  rmse(rmse_vals$Real, rmse_vals$Forecast)
})
print(rmse_post_by_horizon[, c("Horizon", "RMSE")])



rmse_plot_df <- data.frame(
  Horizon = rep(1:forecast_horizon, 2),
  RMSE = rmse_post_by_horizon$RMSE,
  Model = rep("XGBOOST", each = forecast_horizon)
)

err_Xb_Xb_19_23_linear <- errors_by_h(forecast_shifted_df_xgb_xgb)


rfr2 <- ggplot(rmse_plot_df, aes(x = Horizon, y = RMSE, colour = Model)) +
  geom_line(size = 1.3, alpha = 0.9) +
  geom_point(,size = 3) +
  scale_color_manual(values = c("XGBOOST" = "red"), name = "")+
  scale_x_continuous(labels = number_format(accuracy = 1)) + 
  labs(
    title = "2019K2-2023K2",
    x = "Horizontas",
    y = "RMSE",
    color = "Modelis"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )
######################################################################################################################
######################################################################################################################
################################################################XGBOOST+XGBOOST 2017-2021#######################################
library(tidyfit)
library(pbapply)
library(forecast)
library(zoo)
library(glmnet)
library(randomForest)
library(xgboost)
library(caret)

window_size <- 71
forecast_horizon <- 8
target_time <- "2019K2"
max_index <- which(stationary_table$Time == target_time)

X <- as.matrix(stationary_table[, -c(1, 2)])
y <- as.numeric(unlist(stationary_table[, 2]))

set.seed(123)
final_forecasts_xgb_xgb <- list()

for (i in 1:(max_index - window_size + 1)) {
  
  # 1) Langas
  X_window <- X[i:(i + window_size - 1), ]
  y_window <- y[i:(i + window_size - 1)]
  
  # 2) Pirminis XGBoost kintamųjų svarbai įvertinti
  dtrain <- xgb.DMatrix(data = X_window, label = y_window)
  model_importance <- xgboost(
    data        = dtrain,
    max.depth   = 5,
    eta         = 0.1,
    nrounds     = 100,
    objective   = "reg:squarederror",
    verbose     = 0
  )
  
  importance <- xgb.importance(model = model_importance)
  if (nrow(importance) == 0) next                # jei svarbų nėra – praleidžiam
  
  top_n <- min(20, nrow(importance))             # ne daugiau kaip 20
  top_vars <- importance$Feature[1:top_n]
  
  X_sel <- X_window[, top_vars, drop = FALSE]
  
  # 3) Grid-search XGB (linearinis booster)
  xgb_grid <- expand.grid(
    nrounds = c(100, 150, 200),
    eta     = c(0.05, 0.1, 0.15),
    lambda  = c(0, 0.1, 1),
    alpha   = c(0, 0.1, 1)
  )
  
  train_control <- trainControl(
    method        = "timeslice",
    initialWindow = 60,
    horizon       = 9,
    fixedWindow   = TRUE,
    verboseIter   = FALSE
  )
  
  xgb_model_final <- train(
    x         = as.data.frame(X_sel),
    y         = y_window,
    method    = "xgbLinear",
    tuneGrid  = xgb_grid,
    trControl = train_control,
    verbose   = FALSE
  )
  
  # 4) ARIMA prognozės atrinktiems kintamiesiems
  arima_fc <- lapply(seq_len(ncol(X_sel)), function(j) {
    forecast(auto.arima(X_sel[, j]), h = forecast_horizon)$mean
  })
  
  # 5) Prognozės su XGBoost
  arima_df <- setNames(as.data.frame(do.call(cbind, arima_fc)), colnames(X_sel))
  preds    <- predict(xgb_model_final, newdata = arima_df)
  
  final_forecasts_xgb_xgb[[i]] <- preds
}
start_period <- "2017K2"
end_period <- "2021K2"


first_two_columns <- stationary_table[, 1:2]
valid_xgb_xgb <- which((window_size + seq_along(final_forecasts_xgb_xgb) + forecast_horizon - 1) <= 200)
max_time_index <- max(window_size + valid_xgb_xgb + forecast_horizon - 1)
n_needed <- max_time_index - nrow(first_two_columns)

#jei reikia pridėti eilutes
if (n_needed > 0) {
  last_time <- first_two_columns$Time[nrow(first_two_columns)]
  new_times <- generate_quarters(last_time, n_needed)
  new_rows <- as.data.frame(matrix(NA, nrow = n_needed, ncol = ncol(first_two_columns)))
  colnames(new_rows) <- colnames(first_two_columns)
  new_rows$Time <- new_times
  first_two_columns <- rbind(first_two_columns, new_rows)
}
# Random Forest — prognozės kiekvienam langui
all_xgb_xgb_forecasts_shifted <- do.call(rbind, lapply(valid_xgb_xgb, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_xgb_xgb[[i]],
    Type = "Xgb su Xgb",
    Window = i
  )
}))

real_bvp_df <- data.frame(
  Time = first_two_columns$Time,
  Real = first_two_columns$BVP
)

#apkerpam laika
real_bvp_df_trimmed <- real_bvp_df[real_bvp_df$Time >= start_period & real_bvp_df$Time <= end_period, ]


ggplot() +
  geom_line(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), linewidth = 1.2) +
  geom_point(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), size = 2) +
  geom_line(data = subset(all_xgb_xgb_forecasts_shifted, Type == "Xgb su Xgb"),
            aes(x = Time, y = Forecast, group = Window, color = interaction(Type, Window)),
            alpha = 0.6, linewidth = 0.9) +
  labs(
    title = "BVP prognozių palyginimas su Xgboost",
    x = "Ketvirtis", y = "BVP", color = "Modelis ir langas"
  ) +
  scale_color_manual(
    values = c("Reali reikšmė" = "darkgreen"),
    guide = "none"
  ) +
  scale_color_viridis_d(option = "plasma", begin = 0, end = 0.85, direction = -1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
forecast_shifted_df_xgb_xgb<-all_xgb_xgb_forecasts_shifted
forecast_shifted_df_xgb_xgb$Horizon <- rep(1:forecast_horizon, times = length(unique(forecast_shifted_df_xgb_xgb$Window)))
forecast_shifted_df_xgb_xgb <- do.call(rbind, lapply(valid_xgb_xgb, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_xgb_xgb[[i]],
    Horizon = 1:forecast_horizon,
    Window = i
  )
}))
forecast_shifted_df_xgb_xgb
num_windows <- length(unique(forecast_shifted_df_xgb_xgb$Horizon))

blue_shades <- viridis(num_windows, option = "plasma", begin = 0, end = 0.9, direction = -1)
x19 <- ggplot() +
  geom_line(data = real_bvp_df_trimmed,
            aes(x = Time, y = Real, color = "Realybė", group = 1),
            size = 1.2) +
  geom_point(data = real_bvp_df_trimmed,
             aes(x = Time, y = Real, color = "Realybė")) +
  
  geom_line(data = forecast_shifted_df_xgb_xgb,
            aes(x = Time, y = Forecast, group = Horizon, color = as.factor(Horizon)),
            size = 1.2, linetype = "dashed") +
  geom_point(data = forecast_shifted_df_xgb_xgb,
             aes(x = Time, y = Forecast, color = as.factor(Horizon))) +
  
  scale_color_manual(
    values = c(setNames(blue_shades, as.character(unique(forecast_shifted_df_xgb_xgb$Horizon))),
               "Realybė" = "red"),
    guide = guide_legend(override.aes = list(linetype = c(rep("dashed", num_windows), "solid"),
                                             size = c(rep(1.2, num_windows), 1.2)), title = "Horizontai")
  ) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )+
  labs(title = "BVP prognozės (XGBOOST su XGBOOST) ir realybė pagal horizontus",
       x = "Kvartalas",
       y = "BVP",
       color = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold")) 


ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs74.png", plot = x19, width = 10, height = 6, dpi = 300, bg = "white")



forecast_shifted_df_xgb_xgb <- merge(forecast_shifted_df_xgb_xgb, real_bvp_df_trimmed, by = "Time")
forecast_shifted_df_xgb_xgb <- forecast_shifted_df_xgb_xgb[!is.na(forecast_shifted_df_xgb_xgb$Real),]
rmse_post_by_horizon <- aggregate(cbind(Forecast, Real) ~ Horizon, data = forecast_shifted_df_xgb_xgb, FUN = function(x) x)
rmse_post_by_horizon$RMSE <- sapply(unique(forecast_shifted_df_xgb_xgb$Horizon), function(h) {
  rmse_vals <- forecast_shifted_df_xgb_xgb[forecast_shifted_df_xgb_xgb$Horizon == h, ]
  rmse(rmse_vals$Real, rmse_vals$Forecast)
})
print(rmse_post_by_horizon[, c("Horizon", "RMSE")])



rmse_plot_df <- data.frame(
  Horizon = rep(1:forecast_horizon, 2),
  RMSE = rmse_post_by_horizon$RMSE,
  Model = rep("XGBOOST", each = forecast_horizon)
)

err_Xb_Xb_17_21_linear <- errors_by_h(forecast_shifted_df_xgb_xgb)


rfr1 <- ggplot(rmse_plot_df, aes(x = Horizon, y = RMSE, colour = Model)) +
  geom_line(size = 1.3, alpha = 0.9) +
  geom_point(,size = 3) +
  scale_color_manual(values = c("XGBOOST" = "red"), name = "")+
  scale_x_continuous(labels = number_format(accuracy = 1)) + 
  labs(
    title = "2017K2-2021K2",
    x = "Horizontas",
    y = "RMSE",
    color = "Modelis"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )

top_row <- rfr1 + rfr2

bottom_row <- plot_spacer() + rfr3 + plot_spacer()
bottom_row <- bottom_row + plot_layout(widths = c(1, 2, 1))

final_plot <- (top_row / bottom_row) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

final_plot <- final_plot +
  plot_layout(guides = "collect") +   
  plot_annotation(
    title = "RMSE pagal horizontus",
    theme = theme(
      plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),legend.position = "bottom"  
    )
  )

x11()
final_plot
ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs75.png", plot = final_plot, width = 10, height = 6, dpi = 300, bg = "white")


#############################################################################################################################
########################################################XGBOOST+Lasso#####################################################
#####################################################2021-2027#####################################################
library(tidyfit)
library(pbapply)
library(forecast)
library(zoo)
library(glmnet)
library(randomForest)
library(xgboost)
library(caret)

window_size <- 78
forecast_horizon <- 9
target_time <- "2024K4"
max_index <- which(stationary_table$Time == target_time)

X <- as.matrix(stationary_table[, -c(1, 2)])
y <- as.numeric(unlist(stationary_table[, 2]))

set.seed(123)
final_forecasts_lasso_xgb <- list()
final_arima_forecasts <- list()


for (i in 1:(max_index - window_size + 1)) {
  X_window <- X[i:(i + window_size - 1), ]
  y_window <- y[i:(i + window_size - 1)]
  
  lasso_model <- cv.glmnet(X_window, y_window, family = "gaussian", alpha = 1)
  support_vars_min <- which(coef(lasso_model, s = "lambda.min")[-1, 1] != 0)
  
  
  if (length(support_vars_min) > 0) {
    X_window_selected <- X_window[, support_vars_min, drop = FALSE]
    selected_var_names <- colnames(X_window_selected)
    
    xgb_grid <- expand.grid(
      nrounds = c(100, 150, 200, 300, 500),
      max_depth = c(3, 5, 8, 10, 15),
      eta = c(0.05, 0.1, 0.15, 0.2),
      gamma = 0,
      colsample_bytree = 0.8,
      min_child_weight = 1,
      subsample = 0.8
    )
    
    train_control <- trainControl(
      method = "timeslice",
      initialWindow = 60,
      horizon = 9,
      fixedWindow = TRUE,
      verboseIter = FALSE
    )
    
    xgb_model <- train(
      x = as.data.frame(X_window_selected),
      y = y_window,
      method = "xgbTree",
      trControl = train_control,
      tuneGrid = xgb_grid,
      verbose = FALSE
    )
    
    arima_forecasts <- list()
    for (j in 1:ncol(X_window_selected)) {
      model <- auto.arima(X_window_selected[, j])
      arima_forecasts[[j]] <- as.numeric(forecast(model, h = forecast_horizon)$mean)
    }
    
    if (length(arima_forecasts) > 0) {
      arima_forecasts_matrix <- do.call(cbind, arima_forecasts)
      arima_forecasts_df <- as.data.frame(arima_forecasts_matrix)
      colnames(arima_forecasts_df) <- selected_var_names
      
      predictions_xgb <- predict(xgb_model, newdata = arima_forecasts_df)
      final_forecasts_lasso_xgb[[i]] <- predictions_xgb
    }
  }
}
best_params <- xgb_model$bestTune
print(best_params)
xgb_model$results
ggplot(xgb_model$results, aes(x = nrounds, y = RMSE, color = factor(max_depth))) +
  geom_line() +
  facet_wrap(~ eta, scales = "free") +
  theme_minimal()
print(final_forecasts_lasso_xgb)

first_two_columns <- stationary_table[, 1:2]
valid_lasso_xgb <- which((window_size + seq_along(final_forecasts_lasso_xgb) + forecast_horizon - 1) <= 300)
max_time_index <- max(window_size + valid_lasso_xgb + forecast_horizon - 1)
n_needed <- max_time_index - nrow(first_two_columns)

#jei reikia pridėti eilutes
if (n_needed > 0) {
  last_time <- first_two_columns$Time[nrow(first_two_columns)]
  new_times <- generate_quarters(last_time, n_needed)
  new_rows <- as.data.frame(matrix(NA, nrow = n_needed, ncol = ncol(first_two_columns)))
  colnames(new_rows) <- colnames(first_two_columns)
  new_rows$Time <- new_times
  first_two_columns <- rbind(first_two_columns, new_rows)
}
# Random Forest — prognozės kiekvienam langui
all_lasso_xgb_forecasts_shifted <- do.call(rbind, lapply(valid_lasso_xgb, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_lasso_xgb[[i]],
    Type = "Lasso su Xgb",
    Window = i
  )
}))

real_bvp_df <- data.frame(
  Time = first_two_columns$Time,
  Real = first_two_columns$BVP
)

#apkerpam laika
real_bvp_df_trimmed <- real_bvp_df[(window_size + 1):nrow(real_bvp_df), ]


ggplot() +
  geom_line(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), linewidth = 1.2) +
  geom_point(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), size = 2) +
  geom_line(data = subset(all_lasso_xgb_forecasts_shifted, Type == "Lasso su Xgb"),
            aes(x = Time, y = Forecast, group = Window, color = interaction(Type, Window)),
            alpha = 0.6, linewidth = 0.9) +
  labs(
    title = "BVP prognozių palyginimas su Xgboost",
    x = "Ketvirtis", y = "BVP", color = "Modelis ir langas"
  ) +
  scale_color_manual(
    values = c("Reali reikšmė" = "darkgreen"),
    guide = "none"
  ) +
  scale_color_viridis_d(option = "plasma", begin = 0, end = 0.85, direction = -1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
forecast_shifted_df_lasso_xgb<-all_lasso_xgb_forecasts_shifted
forecast_shifted_df_lasso_xgb$Horizon <- rep(1:forecast_horizon, times = length(unique(forecast_shifted_df_lasso_xgb$Window)))
forecast_shifted_df_lasso_xgb <- do.call(rbind, lapply(valid_lasso_xgb, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_lasso_xgb[[i]],
    Horizon = 1:forecast_horizon,
    Window = i
  )
}))
forecast_shifted_df_lasso_xgb
num_windows <- length(unique(forecast_shifted_df_lasso_xgb$Horizon))

blue_shades <- viridis(num_windows, option = "plasma", begin = 0, end = 0.9, direction = -1)
x7 <- ggplot() +
  geom_line(data = real_bvp_df_trimmed,
            aes(x = Time, y = Real, color = "Realybė", group = 1),
            size = 1.2) +
  geom_point(data = real_bvp_df_trimmed,
             aes(x = Time, y = Real, color = "Realybė")) +
  
  geom_line(data = forecast_shifted_df_lasso_xgb,
            aes(x = Time, y = Forecast, group = Horizon, color = as.factor(Horizon)),
            size = 1.2, linetype = "dashed") +
  geom_point(data = forecast_shifted_df_lasso_xgb,
             aes(x = Time, y = Forecast, color = as.factor(Horizon))) +
  
  scale_color_manual(
    values = c(setNames(blue_shades, as.character(unique(forecast_shifted_df_lasso_xgb$Horizon))),
               "Realybė" = "red"),
    guide = guide_legend(override.aes = list(linetype = c(rep("dashed", num_windows), "solid"),
                                             size = c(rep(1.2, num_windows), 1.2)), title = "Horizontai")
  ) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )+
  labs(title = "BVP prognozės (XGBOOST su LASSO) ir realybė pagal horizontus",
       x = "Kvartalas",
       y = "BVP",
       color = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold")) 


ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs60.png", plot = x7, width = 10, height = 6, dpi = 300, bg = "white")



forecast_shifted_df_lasso_xgb <- merge(forecast_shifted_df_lasso_xgb, real_bvp_df_trimmed, by = "Time")
forecast_shifted_df_lasso_xgb <- forecast_shifted_df_lasso_xgb[!is.na(forecast_shifted_df_lasso_xgb$Real),]
rmse_post_by_horizon <- aggregate(cbind(Forecast, Real) ~ Horizon, data = forecast_shifted_df_lasso_xgb, FUN = function(x) x)
rmse_post_by_horizon$RMSE <- sapply(unique(forecast_shifted_df_lasso_xgb$Horizon), function(h) {
  rmse_vals <- forecast_shifted_df_lasso_xgb[forecast_shifted_df_lasso_xgb$Horizon == h, ]
  rmse(rmse_vals$Real, rmse_vals$Forecast)
})
print(rmse_post_by_horizon[, c("Horizon", "RMSE")])



rmse_plot_df <- data.frame(
  Horizon = rep(1:forecast_horizon, 2),
  RMSE = rmse_post_by_horizon$RMSE,
  Model = rep("XGBOOST", each = forecast_horizon)
)

err_Xb_lasso_21_27 <- errors_by_h(forecast_shifted_df_lasso_xgb)


rfr3 <- ggplot(rmse_plot_df, aes(x = Horizon, y = RMSE, colour = Model)) +
  geom_line(size = 1.3, alpha = 0.9) +
  geom_point(,size = 3) +
  scale_color_manual(values = c("XGBOOST" = "red"), name = "")+
  scale_x_continuous(labels = number_format(accuracy = 1)) + 
  labs(
    title = "2021K1-2024K4",
    x = "Horizontas",
    y = "RMSE",
    color = "Modelis"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )


########################################################XGBOOST+Lasso#####################################################
#####################################################2017-2021#####################################################
library(tidyfit)
library(pbapply)
library(forecast)
library(zoo)
library(glmnet)
library(randomForest)
library(xgboost)
library(caret)

window_size <- 71
forecast_horizon <- 8
target_time <- "2019K2"
max_index <- which(stationary_table$Time == target_time)

X <- as.matrix(stationary_table[, -c(1, 2)])
y <- as.numeric(unlist(stationary_table[, 2]))

set.seed(123)
final_forecasts_lasso_xgb <- list()
final_arima_forecasts <- list()


for (i in 1:(max_index - window_size + 1)) {
  X_window <- X[i:(i + window_size - 1), ]
  y_window <- y[i:(i + window_size - 1)]
  
  lasso_model <- cv.glmnet(X_window, y_window, family = "gaussian", alpha = 1)
  support_vars_min <- which(coef(lasso_model, s = "lambda.min")[-1, 1] != 0)
  
  
  if (length(support_vars_min) > 0) {
    X_window_selected <- X_window[, support_vars_min, drop = FALSE]
    selected_var_names <- colnames(X_window_selected)
    
    xgb_grid <- expand.grid(
      nrounds = c(100, 150, 200, 300, 500),
      max_depth = c(3, 5, 8, 10, 15),
      eta = c(0.05, 0.1, 0.15, 0.2),
      gamma = 0,
      colsample_bytree = 0.8,
      min_child_weight = 1,
      subsample = 0.8
    )
    
    train_control <- trainControl(
      method = "timeslice",
      initialWindow = 60,
      horizon = 9,
      fixedWindow = TRUE,
      verboseIter = FALSE
    )
    
    xgb_model <- train(
      x = as.data.frame(X_window_selected),
      y = y_window,
      method = "xgbTree",
      trControl = train_control,
      tuneGrid = xgb_grid,
      verbose = FALSE
    )
    
    arima_forecasts <- list()
    for (j in 1:ncol(X_window_selected)) {
      model <- auto.arima(X_window_selected[, j])
      arima_forecasts[[j]] <- as.numeric(forecast(model, h = forecast_horizon)$mean)
    }
    
    if (length(arima_forecasts) > 0) {
      arima_forecasts_matrix <- do.call(cbind, arima_forecasts)
      arima_forecasts_df <- as.data.frame(arima_forecasts_matrix)
      colnames(arima_forecasts_df) <- selected_var_names
      
      predictions_xgb <- predict(xgb_model, newdata = arima_forecasts_df)
      final_forecasts_lasso_xgb[[i]] <- predictions_xgb
    }
  }
}
best_params <- xgb_model$bestTune
print(best_params)
xgb_model$results
ggplot(xgb_model$results, aes(x = nrounds, y = RMSE, color = factor(max_depth))) +
  geom_line() +
  facet_wrap(~ eta, scales = "free") +
  theme_minimal()
print(final_forecasts_lasso_xgb)

first_two_columns <- stationary_table[, 1:2]
valid_lasso_xgb <- which((window_size + seq_along(final_forecasts_lasso_xgb) + forecast_horizon - 1) <= 300)
max_time_index <- max(window_size + valid_lasso_xgb + forecast_horizon - 1)
n_needed <- max_time_index - nrow(first_two_columns)

#jei reikia pridėti eilutes
if (n_needed > 0) {
  last_time <- first_two_columns$Time[nrow(first_two_columns)]
  new_times <- generate_quarters(last_time, n_needed)
  new_rows <- as.data.frame(matrix(NA, nrow = n_needed, ncol = ncol(first_two_columns)))
  colnames(new_rows) <- colnames(first_two_columns)
  new_rows$Time <- new_times
  first_two_columns <- rbind(first_two_columns, new_rows)
}
# Random Forest — prognozės kiekvienam langui
all_lasso_xgb_forecasts_shifted <- do.call(rbind, lapply(valid_lasso_xgb, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_lasso_xgb[[i]],
    Type = "Lasso su Xgb",
    Window = i
  )
}))

real_bvp_df <- data.frame(
  Time = first_two_columns$Time,
  Real = first_two_columns$BVP
)
start_period<-"2017K2"
end_period<-"2021K2"
#apkerpam laika
real_bvp_df_trimmed <- real_bvp_df[real_bvp_df$Time >= start_period & real_bvp_df$Time <= end_period, ]


ggplot() +
  geom_line(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), linewidth = 1.2) +
  geom_point(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), size = 2) +
  geom_line(data = subset(all_lasso_xgb_forecasts_shifted, Type == "Lasso su Xgb"),
            aes(x = Time, y = Forecast, group = Window, color = interaction(Type, Window)),
            alpha = 0.6, linewidth = 0.9) +
  labs(
    title = "BVP prognozių palyginimas su Xgboost",
    x = "Ketvirtis", y = "BVP", color = "Modelis ir langas"
  ) +
  scale_color_manual(
    values = c("Reali reikšmė" = "darkgreen"),
    guide = "none"
  ) +
  scale_color_viridis_d(option = "plasma", begin = 0, end = 0.85, direction = -1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
forecast_shifted_df_lasso_xgb<-all_lasso_xgb_forecasts_shifted
forecast_shifted_df_lasso_xgb$Horizon <- rep(1:forecast_horizon, times = length(unique(forecast_shifted_df_lasso_xgb$Window)))
forecast_shifted_df_lasso_xgb <- do.call(rbind, lapply(valid_lasso_xgb, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_lasso_xgb[[i]],
    Horizon = 1:forecast_horizon,
    Window = i
  )
}))
num_windows <- length(unique(forecast_shifted_df_lasso_xgb$Horizon))

blue_shades <- viridis(num_windows, option = "plasma", begin = 0, end = 0.9, direction = -1)
x8 <- ggplot() +
  geom_line(data = real_bvp_df_trimmed,
            aes(x = Time, y = Real, color = "Realybė", group = 1),
            size = 1.2) +
  geom_point(data = real_bvp_df_trimmed,
             aes(x = Time, y = Real, color = "Realybė")) +
  
  geom_line(data = forecast_shifted_df_lasso_xgb,
            aes(x = Time, y = Forecast, group = Horizon, color = as.factor(Horizon)),
            size = 1.2, linetype = "dashed") +
  geom_point(data = forecast_shifted_df_lasso_xgb,
             aes(x = Time, y = Forecast, color = as.factor(Horizon))) +
  
  scale_color_manual(
    values = c(setNames(blue_shades, as.character(unique(forecast_shifted_df_lasso_xgb$Horizon))),
               "Realybė" = "red"),
    guide = guide_legend(override.aes = list(linetype = c(rep("dashed", num_windows), "solid"),
                                             size = c(rep(1.2, num_windows), 1.2)), title = "Horizontai")
  ) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )+
  labs(title = "BVP prognozės (XGBOOST su LASSO) ir realybė pagal horizontus",
       x = "Kvartalas",
       y = "BVP",
       color = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold")) 


ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs61.png", plot = x8, width = 10, height = 6, dpi = 300, bg = "white")



forecast_shifted_df_lasso_xgb <- merge(forecast_shifted_df_lasso_xgb, real_bvp_df_trimmed, by = "Time")
forecast_shifted_df_lasso_xgb <- forecast_shifted_df_lasso_xgb[!is.na(forecast_shifted_df_lasso_xgb$Real),]
rmse_post_by_horizon <- aggregate(cbind(Forecast, Real) ~ Horizon, data = forecast_shifted_df_lasso_xgb, FUN = function(x) x)
rmse_post_by_horizon$RMSE <- sapply(unique(forecast_shifted_df_lasso_xgb$Horizon), function(h) {
  rmse_vals <- forecast_shifted_df_lasso_xgb[forecast_shifted_df_lasso_xgb$Horizon == h, ]
  rmse(rmse_vals$Real, rmse_vals$Forecast)
})
print(rmse_post_by_horizon[, c("Horizon", "RMSE")])



rmse_plot_df <- data.frame(
  Horizon = rep(1:forecast_horizon, 2),
  RMSE = rmse_post_by_horizon$RMSE,
  Model = rep("XGBOOST", each = forecast_horizon)
)

err_Xb_lasso_17_21 <- errors_by_h(forecast_shifted_df_lasso_xgb)


rfr1 <- ggplot(rmse_plot_df, aes(x = Horizon, y = RMSE, colour = Model)) +
  geom_line(size = 1.3, alpha = 0.9) +
  geom_point(,size = 3) +
  scale_color_manual(values = c("XGBOOST" = "red"), name = "")+
  scale_x_continuous(labels = number_format(accuracy = 1)) + 
  labs(
    title = "2017K2-2021K2",
    x = "Horizontas",
    y = "RMSE",
    color = "Modelis"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )

########################################################XGBOOST+Lasso#####################################################
#####################################################2019-2023#####################################################
library(tidyfit)
library(pbapply)
library(forecast)
library(zoo)
library(glmnet)
library(randomForest)
library(xgboost)
library(caret)

window_size <- 79
forecast_horizon <- 8
target_time <- "2021K2"
max_index <- which(stationary_table$Time == target_time)

X <- as.matrix(stationary_table[, -c(1, 2)])
y <- as.numeric(unlist(stationary_table[, 2]))

set.seed(123)
final_forecasts_lasso_xgb <- list()
final_arima_forecasts <- list()


for (i in 1:(max_index - window_size + 1)) {
  X_window <- X[i:(i + window_size - 1), ]
  y_window <- y[i:(i + window_size - 1)]
  
  lasso_model <- cv.glmnet(X_window, y_window, family = "gaussian", alpha = 1)
  support_vars_min <- which(coef(lasso_model, s = "lambda.min")[-1, 1] != 0)
  
  
  if (length(support_vars_min) > 0) {
    X_window_selected <- X_window[, support_vars_min, drop = FALSE]
    selected_var_names <- colnames(X_window_selected)
    
    xgb_grid <- expand.grid(
      nrounds = c(100, 150, 200, 300, 500),
      max_depth = c(3, 5, 8, 10, 15),
      eta = c(0.05, 0.1, 0.15, 0.2),
      gamma = 0,
      colsample_bytree = 0.8,
      min_child_weight = 1,
      subsample = 0.8
    )
    
    train_control <- trainControl(
      method = "timeslice",
      initialWindow = 60,
      horizon = 9,
      fixedWindow = TRUE,
      verboseIter = FALSE
    )
    
    xgb_model <- train(
      x = as.data.frame(X_window_selected),
      y = y_window,
      method = "xgbTree",
      trControl = train_control,
      tuneGrid = xgb_grid,
      verbose = FALSE
    )
    
    arima_forecasts <- list()
    for (j in 1:ncol(X_window_selected)) {
      model <- auto.arima(X_window_selected[, j])
      arima_forecasts[[j]] <- as.numeric(forecast(model, h = forecast_horizon)$mean)
    }
    
    if (length(arima_forecasts) > 0) {
      arima_forecasts_matrix <- do.call(cbind, arima_forecasts)
      arima_forecasts_df <- as.data.frame(arima_forecasts_matrix)
      colnames(arima_forecasts_df) <- selected_var_names
      
      predictions_xgb <- predict(xgb_model, newdata = arima_forecasts_df)
      final_forecasts_lasso_xgb[[i]] <- predictions_xgb
    }
  }
}
best_params <- xgb_model$bestTune
print(best_params)
xgb_model$results
ggplot(xgb_model$results, aes(x = nrounds, y = RMSE, color = factor(max_depth))) +
  geom_line() +
  facet_wrap(~ eta, scales = "free") +
  theme_minimal()
print(final_forecasts_lasso_xgb)

first_two_columns <- stationary_table[, 1:2]
valid_lasso_xgb <- which((window_size + seq_along(final_forecasts_lasso_xgb) + forecast_horizon - 1) <= 300)
max_time_index <- max(window_size + valid_lasso_xgb + forecast_horizon - 1)
n_needed <- max_time_index - nrow(first_two_columns)

#jei reikia pridėti eilutes
if (n_needed > 0) {
  last_time <- first_two_columns$Time[nrow(first_two_columns)]
  new_times <- generate_quarters(last_time, n_needed)
  new_rows <- as.data.frame(matrix(NA, nrow = n_needed, ncol = ncol(first_two_columns)))
  colnames(new_rows) <- colnames(first_two_columns)
  new_rows$Time <- new_times
  first_two_columns <- rbind(first_two_columns, new_rows)
}
# Random Forest — prognozės kiekvienam langui
all_lasso_xgb_forecasts_shifted <- do.call(rbind, lapply(valid_lasso_xgb, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_lasso_xgb[[i]],
    Type = "Lasso su Xgb",
    Window = i
  )
}))

real_bvp_df <- data.frame(
  Time = first_two_columns$Time,
  Real = first_two_columns$BVP
)
start_period<-"2019K2"
end_period<-"2023K2"
#apkerpam laika
real_bvp_df_trimmed <- real_bvp_df[real_bvp_df$Time >= start_period & real_bvp_df$Time <= end_period, ]


ggplot() +
  geom_line(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), linewidth = 1.2) +
  geom_point(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), size = 2) +
  geom_line(data = subset(all_lasso_xgb_forecasts_shifted, Type == "Lasso su Xgb"),
            aes(x = Time, y = Forecast, group = Window, color = interaction(Type, Window)),
            alpha = 0.6, linewidth = 0.9) +
  labs(
    title = "BVP prognozių palyginimas su Xgboost",
    x = "Ketvirtis", y = "BVP", color = "Modelis ir langas"
  ) +
  scale_color_manual(
    values = c("Reali reikšmė" = "darkgreen"),
    guide = "none"
  ) +
  scale_color_viridis_d(option = "plasma", begin = 0, end = 0.85, direction = -1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
forecast_shifted_df_lasso_xgb<-all_lasso_xgb_forecasts_shifted
forecast_shifted_df_lasso_xgb$Horizon <- rep(1:forecast_horizon, times = length(unique(forecast_shifted_df_lasso_xgb$Window)))
forecast_shifted_df_lasso_xgb <- do.call(rbind, lapply(valid_lasso_xgb, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_lasso_xgb[[i]],
    Horizon = 1:forecast_horizon,
    Window = i
  )
}))
forecast_shifted_df_lasso_xgb
num_windows <- length(unique(forecast_shifted_df_lasso_xgb$Horizon))

blue_shades <- viridis(num_windows, option = "plasma", begin = 0, end = 0.9, direction = -1)
x9 <- ggplot() +
  geom_line(data = real_bvp_df_trimmed,
            aes(x = Time, y = Real, color = "Realybė", group = 1),
            size = 1.2) +
  geom_point(data = real_bvp_df_trimmed,
             aes(x = Time, y = Real, color = "Realybė")) +
  
  geom_line(data = forecast_shifted_df_lasso_xgb,
            aes(x = Time, y = Forecast, group = Horizon, color = as.factor(Horizon)),
            size = 1.2, linetype = "dashed") +
  geom_point(data = forecast_shifted_df_lasso_xgb,
             aes(x = Time, y = Forecast, color = as.factor(Horizon))) +
  
  scale_color_manual(
    values = c(setNames(blue_shades, as.character(unique(forecast_shifted_df_lasso_xgb$Horizon))),
               "Realybė" = "red"),
    guide = guide_legend(override.aes = list(linetype = c(rep("dashed", num_windows), "solid"),
                                             size = c(rep(1.2, num_windows), 1.2)), title = "Horizontai")
  ) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )+
  labs(title = "BVP prognozės (XGBOOST su LASSO) ir realybė pagal horizontus",
       x = "Kvartalas",
       y = "BVP",
       color = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold")) 


ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs62.png", plot = x9, width = 10, height = 6, dpi = 300, bg = "white")



forecast_shifted_df_lasso_xgb <- merge(forecast_shifted_df_lasso_xgb, real_bvp_df_trimmed, by = "Time")
forecast_shifted_df_lasso_xgb <- forecast_shifted_df_lasso_xgb[!is.na(forecast_shifted_df_lasso_xgb$Real),]
rmse_post_by_horizon <- aggregate(cbind(Forecast, Real) ~ Horizon, data = forecast_shifted_df_lasso_xgb, FUN = function(x) x)
rmse_post_by_horizon$RMSE <- sapply(unique(forecast_shifted_df_lasso_xgb$Horizon), function(h) {
  rmse_vals <- forecast_shifted_df_lasso_xgb[forecast_shifted_df_lasso_xgb$Horizon == h, ]
  rmse(rmse_vals$Real, rmse_vals$Forecast)
})
print(rmse_post_by_horizon[, c("Horizon", "RMSE")])



rmse_plot_df <- data.frame(
  Horizon = rep(1:forecast_horizon, 2),
  RMSE = rmse_post_by_horizon$RMSE,
  Model = rep("XGBOOST", each = forecast_horizon)
)

err_Xb_lasso_19_23 <- errors_by_h(forecast_shifted_df_lasso_xgb)


rfr2 <- ggplot(rmse_plot_df, aes(x = Horizon, y = RMSE, colour = Model)) +
  geom_line(size = 1.3, alpha = 0.9) +
  geom_point(,size = 3) +
  scale_color_manual(values = c("XGBOOST" = "red"), name = "")+
  scale_x_continuous(labels = number_format(accuracy = 1)) + 
  labs(
    title = "2019K2-2023K2",
    x = "Horizontas",
    y = "RMSE",
    color = "Modelis"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )


top_row <- rfr1 + rfr2

bottom_row <- plot_spacer() + rfr3 + plot_spacer()
bottom_row <- bottom_row + plot_layout(widths = c(1, 2, 1))

final_plot <- (top_row / bottom_row) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

final_plot <- final_plot +
  plot_layout(guides = "collect") +   
  plot_annotation(
    title = "RMSE pagal horizontus",
    theme = theme(
      plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),legend.position = "bottom"  
    )
  )

x11()
final_plot
ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs63.png", plot = final_plot, width = 10, height = 6, dpi = 300, bg = "white")



###############################################################################################------------------2021-2027----------
###################################################################################################----------linear--------------
library(tidyfit)
library(pbapply)
library(forecast)
library(zoo)
library(glmnet)
library(randomForest)
library(xgboost)
library(caret)

# --- funkcija lambda_n, jei reikia platesnio LASSO rėžimo ---------------
lambda_n <- function(model, N = 5) {
  require(magrittr)
  non_optimal <- sapply(
    model$lambda,
    function(x) {
      predict(model, type = "nonzero", s = x) %>%
        nrow() %>% {abs(. - N)}
    }
  ) %>% which.min()
  model$lambda[non_optimal]
}

# ------------------------------------------------------------------------
window_size <- 78
forecast_horizon <- 9
target_time <- "2024K4"
max_index <- which(stationary_table$Time == target_time)

X <- as.matrix(stationary_table[, -c(1, 2)])
y <- as.numeric(unlist(stationary_table[, 2]))

set.seed(123)
final_forecasts_lasso_xgb <- list()

for (i in 1:(max_index - window_size + 1)) {
  
  X_window <- X[i:(i + window_size - 1), ]
  y_window <- y[i:(i + window_size - 1)]
  
  # --- LASSO atranka ----------------------------------------------------
  lasso_model <- cv.glmnet(X_window, y_window, family = "gaussian", alpha = 1)
  support_vars <- which(coef(lasso_model, s = "lambda.min")[-1, 1] != 0)
  
  # jei atrenkama < 2 kintamųjų, parenkame kitą lambda
  if (length(support_vars) < 2) {
    alt_lambda <- lambda_n(lasso_model, N = 5)
    support_vars <- which(coef(lasso_model, s = alt_lambda)[-1, 1] != 0)
  }
  
  if (length(support_vars) > 0) {
    
    X_sel <- X_window[, support_vars, drop = FALSE]
    sel_names <- colnames(X_sel)
    
    # --- XGBoost parametrai --------------------------------------------
    xgb_grid <- expand.grid(
      nrounds = c(100, 150, 200),
      eta     = c(0.05, 0.1, 0.15),
      lambda  = c(0, 0.1, 1),
      alpha   = c(0, 0.1, 1)
    )
    
    train_control <- trainControl(
      method        = "timeslice",
      initialWindow = 60,
      horizon       = 9,
      fixedWindow   = TRUE,
      verboseIter   = FALSE
    )
    
    xgb_model_final <- train(
      x = as.data.frame(X_sel),
      y = y_window,
      method   = "xgbLinear",
      trControl = train_control,
      tuneGrid  = xgb_grid,
      verbose   = FALSE
    )
    
    # --- ARIMA prognozės atrinktiems kintamiesiems ---------------------
    arima_forecasts <- lapply(seq_len(ncol(X_sel)), function(j) {
      forecast(auto.arima(X_sel[, j]), h = forecast_horizon)$mean
    })
    
    # suformuojame prognozių duomenų rėmelį tokia pačia tvarka kaip X_sel
    arima_df <- setNames(as.data.frame(do.call(cbind, arima_forecasts)),
                         sel_names)
    
    # --- prognozuojame BVP su XGBoost ----------------------------------
    preds <- predict(xgb_model_final, newdata = arima_df)
    final_forecasts_lasso_xgb[[i]] <- preds
  }
}

print(final_forecasts_rf_xgb_param)
best_params <- xgb_model$bestTune
print(best_params)
xgb_model$results
ggplot(xgb_model$results, aes(x = nrounds, y = RMSE, color = factor(eta))) +
  geom_line() +
  facet_wrap(~ eta, scales = "free") +
  theme_minimal()

print(final_forecasts_lasso_xgb)

first_two_columns <- stationary_table[, 1:2]
valid_lasso_xgb <- which((window_size + seq_along(final_forecasts_lasso_xgb) + forecast_horizon - 1) <= 300)
max_time_index <- max(window_size + valid_lasso_xgb + forecast_horizon - 1)
n_needed <- max_time_index - nrow(first_two_columns)

#jei reikia pridėti eilutes
if (n_needed > 0) {
  last_time <- first_two_columns$Time[nrow(first_two_columns)]
  new_times <- generate_quarters(last_time, n_needed)
  new_rows <- as.data.frame(matrix(NA, nrow = n_needed, ncol = ncol(first_two_columns)))
  colnames(new_rows) <- colnames(first_two_columns)
  new_rows$Time <- new_times
  first_two_columns <- rbind(first_two_columns, new_rows)
}
# Random Forest — prognozės kiekvienam langui
all_lasso_xgb_forecasts_shifted <- do.call(rbind, lapply(valid_lasso_xgb, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_lasso_xgb[[i]],
    Type = "Lasso su Xgb",
    Window = i
  )
}))

real_bvp_df <- data.frame(
  Time = first_two_columns$Time,
  Real = first_two_columns$BVP
)

#apkerpam laika
real_bvp_df_trimmed <- real_bvp_df[(window_size + 1):nrow(real_bvp_df), ]


ggplot() +
  geom_line(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), linewidth = 1.2) +
  geom_point(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), size = 2) +
  geom_line(data = subset(all_lasso_xgb_forecasts_shifted, Type == "Lasso su Xgb"),
            aes(x = Time, y = Forecast, group = Window, color = interaction(Type, Window)),
            alpha = 0.6, linewidth = 0.9) +
  labs(
    title = "BVP prognozių palyginimas su Xgboost",
    x = "Ketvirtis", y = "BVP", color = "Modelis ir langas"
  ) +
  scale_color_manual(
    values = c("Reali reikšmė" = "darkgreen"),
    guide = "none"
  ) +
  scale_color_viridis_d(option = "plasma", begin = 0, end = 0.85, direction = -1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
forecast_shifted_df_lasso_xgb<-all_lasso_xgb_forecasts_shifted
forecast_shifted_df_lasso_xgb$Horizon <- rep(1:forecast_horizon, times = length(unique(forecast_shifted_df_lasso_xgb$Window)))
forecast_shifted_df_lasso_xgb <- do.call(rbind, lapply(valid_lasso_xgb, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_lasso_xgb[[i]],
    Horizon = 1:forecast_horizon,
    Window = i
  )
}))
forecast_shifted_df_lasso_xgb
num_windows <- length(unique(forecast_shifted_df_lasso_xgb$Horizon))

blue_shades <- viridis(num_windows, option = "plasma", begin = 0, end = 0.9, direction = -1)
x8 <- ggplot() +
  geom_line(data = real_bvp_df_trimmed,
            aes(x = Time, y = Real, color = "Realybė", group = 1),
            size = 1.2) +
  geom_point(data = real_bvp_df_trimmed,
             aes(x = Time, y = Real, color = "Realybė")) +
  
  geom_line(data = forecast_shifted_df_lasso_xgb,
            aes(x = Time, y = Forecast, group = Horizon, color = as.factor(Horizon)),
            size = 1.2, linetype = "dashed") +
  geom_point(data = forecast_shifted_df_lasso_xgb,
             aes(x = Time, y = Forecast, color = as.factor(Horizon))) +
  
  scale_color_manual(
    values = c(setNames(blue_shades, as.character(unique(forecast_shifted_df_lasso_xgb$Horizon))),
               "Realybė" = "red"),
    guide = guide_legend(override.aes = list(linetype = c(rep("dashed", num_windows), "solid"),
                                             size = c(rep(1.2, num_windows), 1.2)), title = "Horizontai")
  ) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )+
  labs(title = "BVP prognozės (XGBOOST su LASSO) ir realybė pagal horizontus",
       x = "Kvartalas",
       y = "BVP",
       color = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold")) 


ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs64.png", plot = x8, width = 10, height = 6, dpi = 300, bg = "white")



forecast_shifted_df_lasso_xgb <- merge(forecast_shifted_df_lasso_xgb, real_bvp_df_trimmed, by = "Time")
forecast_shifted_df_lasso_xgb <- forecast_shifted_df_lasso_xgb[!is.na(forecast_shifted_df_lasso_xgb$Real),]
rmse_post_by_horizon <- aggregate(cbind(Forecast, Real) ~ Horizon, data = forecast_shifted_df_lasso_xgb, FUN = function(x) x)
rmse_post_by_horizon$RMSE <- sapply(unique(forecast_shifted_df_lasso_xgb$Horizon), function(h) {
  rmse_vals <- forecast_shifted_df_lasso_xgb[forecast_shifted_df_lasso_xgb$Horizon == h, ]
  rmse(rmse_vals$Real, rmse_vals$Forecast)
})
print(rmse_post_by_horizon[, c("Horizon", "RMSE")])



rmse_plot_df <- data.frame(
  Horizon = rep(1:forecast_horizon, 2),
  RMSE = rmse_post_by_horizon$RMSE,
  Model = rep("XGBOOST", each = forecast_horizon)
)

err_Xb_lasso_21_27_linear <- errors_by_h(forecast_shifted_df_lasso_xgb)


rfr3 <- ggplot(rmse_plot_df, aes(x = Horizon, y = RMSE, colour = Model)) +
  geom_line(size = 1.3, alpha = 0.9) +
  geom_point(,size = 3) +
  scale_color_manual(values = c("XGBOOST" = "red"), name = "")+
  scale_x_continuous(labels = number_format(accuracy = 1)) + 
  labs(
    title = "2021K1-2024K4",
    x = "Horizontas",
    y = "RMSE",
    color = "Modelis"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )


########################################################XGBOOST+Lasso#####################################################
#####################################################2017-2021#####################################################
library(tidyfit)
library(pbapply)
library(forecast)
library(zoo)
library(glmnet)
library(randomForest)
library(xgboost)
library(caret)

window_size <- 71
forecast_horizon <- 8
target_time <- "2019K2"
max_index <- which(stationary_table$Time == target_time)

X <- as.matrix(stationary_table[, -c(1, 2)])
y <- as.numeric(unlist(stationary_table[, 2]))

set.seed(123)
final_arima_forecasts <- list()

final_forecasts_lasso_xgb <- list()

for (i in 1:(max_index - window_size + 1)) {
  
  X_window <- X[i:(i + window_size - 1), ]
  y_window <- y[i:(i + window_size - 1)]
  
  # --- LASSO atranka ----------------------------------------------------
  lasso_model <- cv.glmnet(X_window, y_window, family = "gaussian", alpha = 1)
  support_vars <- which(coef(lasso_model, s = "lambda.min")[-1, 1] != 0)
  
  # jei atrenkama < 2 kintamųjų, parenkame kitą lambda
  if (length(support_vars) < 2) {
    alt_lambda <- lambda_n(lasso_model, N = 5)
    support_vars <- which(coef(lasso_model, s = alt_lambda)[-1, 1] != 0)
  }
  
  if (length(support_vars) > 0) {
    
    X_sel <- X_window[, support_vars, drop = FALSE]
    sel_names <- colnames(X_sel)
    
    # --- XGBoost parametrai --------------------------------------------
    xgb_grid <- expand.grid(
      nrounds = c(100, 150, 200),
      eta     = c(0.05, 0.1, 0.15),
      lambda  = c(0, 0.1, 1),
      alpha   = c(0, 0.1, 1)
    )
    
    train_control <- trainControl(
      method        = "timeslice",
      initialWindow = 60,
      horizon       = 9,
      fixedWindow   = TRUE,
      verboseIter   = FALSE
    )
    
    xgb_model_final <- train(
      x = as.data.frame(X_sel),
      y = y_window,
      method   = "xgbLinear",
      trControl = train_control,
      tuneGrid  = xgb_grid,
      verbose   = FALSE
    )
    
    # --- ARIMA prognozės atrinktiems kintamiesiems ---------------------
    arima_forecasts <- lapply(seq_len(ncol(X_sel)), function(j) {
      forecast(auto.arima(X_sel[, j]), h = forecast_horizon)$mean
    })
    
    # suformuojame prognozių duomenų rėmelį tokia pačia tvarka kaip X_sel
    arima_df <- setNames(as.data.frame(do.call(cbind, arima_forecasts)),
                         sel_names)
    
    # --- prognozuojame BVP su XGBoost ----------------------------------
    preds <- predict(xgb_model_final, newdata = arima_df)
    final_forecasts_lasso_xgb[[i]] <- preds
  }
}


first_two_columns <- stationary_table[, 1:2]
valid_lasso_xgb <- which((window_size + seq_along(final_forecasts_lasso_xgb) + forecast_horizon - 1) <= 300)
max_time_index <- max(window_size + valid_lasso_xgb + forecast_horizon - 1)
n_needed <- max_time_index - nrow(first_two_columns)

#jei reikia pridėti eilutes
if (n_needed > 0) {
  last_time <- first_two_columns$Time[nrow(first_two_columns)]
  new_times <- generate_quarters(last_time, n_needed)
  new_rows <- as.data.frame(matrix(NA, nrow = n_needed, ncol = ncol(first_two_columns)))
  colnames(new_rows) <- colnames(first_two_columns)
  new_rows$Time <- new_times
  first_two_columns <- rbind(first_two_columns, new_rows)
}
# Random Forest — prognozės kiekvienam langui
all_lasso_xgb_forecasts_shifted <- do.call(rbind, lapply(valid_lasso_xgb, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_lasso_xgb[[i]],
    Type = "Lasso su Xgb",
    Window = i
  )
}))

real_bvp_df <- data.frame(
  Time = first_two_columns$Time,
  Real = first_two_columns$BVP
)
start_period<-"2017K2"
end_period<-"2021K2"
#apkerpam laika
real_bvp_df_trimmed <- real_bvp_df[real_bvp_df$Time >= start_period & real_bvp_df$Time <= end_period, ]


ggplot() +
  geom_line(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), linewidth = 1.2) +
  geom_point(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), size = 2) +
  geom_line(data = subset(all_lasso_xgb_forecasts_shifted, Type == "Lasso su Xgb"),
            aes(x = Time, y = Forecast, group = Window, color = interaction(Type, Window)),
            alpha = 0.6, linewidth = 0.9) +
  labs(
    title = "BVP prognozių palyginimas su Xgboost",
    x = "Ketvirtis", y = "BVP", color = "Modelis ir langas"
  ) +
  scale_color_manual(
    values = c("Reali reikšmė" = "darkgreen"),
    guide = "none"
  ) +
  scale_color_viridis_d(option = "plasma", begin = 0, end = 0.85, direction = -1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
forecast_shifted_df_lasso_xgb<-all_lasso_xgb_forecasts_shifted
forecast_shifted_df_lasso_xgb$Horizon <- rep(1:forecast_horizon, times = length(unique(forecast_shifted_df_lasso_xgb$Window)))
forecast_shifted_df_lasso_xgb <- do.call(rbind, lapply(valid_lasso_xgb, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_lasso_xgb[[i]],
    Horizon = 1:forecast_horizon,
    Window = i
  )
}))
forecast_shifted_df_lasso_xgb
num_windows <- length(unique(forecast_shifted_df_lasso_xgb$Horizon))

blue_shades <- viridis(num_windows, option = "plasma", begin = 0, end = 0.9, direction = -1)
x10 <- ggplot() +
  geom_line(data = real_bvp_df_trimmed,
            aes(x = Time, y = Real, color = "Realybė", group = 1),
            size = 1.2) +
  geom_point(data = real_bvp_df_trimmed,
             aes(x = Time, y = Real, color = "Realybė")) +
  
  geom_line(data = forecast_shifted_df_lasso_xgb,
            aes(x = Time, y = Forecast, group = Horizon, color = as.factor(Horizon)),
            size = 1.2, linetype = "dashed") +
  geom_point(data = forecast_shifted_df_lasso_xgb,
             aes(x = Time, y = Forecast, color = as.factor(Horizon))) +
  
  scale_color_manual(
    values = c(setNames(blue_shades, as.character(unique(forecast_shifted_df_lasso_xgb$Horizon))),
               "Realybė" = "red"),
    guide = guide_legend(override.aes = list(linetype = c(rep("dashed", num_windows), "solid"),
                                             size = c(rep(1.2, num_windows), 1.2)), title = "Horizontai")
  ) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )+
  labs(title = "BVP prognozės (XGBOOST su LASSO) ir realybė pagal horizontus",
       x = "Kvartalas",
       y = "BVP",
       color = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold")) 


ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs65.png", plot = x10, width = 10, height = 6, dpi = 300, bg = "white")



forecast_shifted_df_lasso_xgb <- merge(forecast_shifted_df_lasso_xgb, real_bvp_df_trimmed, by = "Time")
forecast_shifted_df_lasso_xgb <- forecast_shifted_df_lasso_xgb[!is.na(forecast_shifted_df_lasso_xgb$Real),]
rmse_post_by_horizon <- aggregate(cbind(Forecast, Real) ~ Horizon, data = forecast_shifted_df_lasso_xgb, FUN = function(x) x)
rmse_post_by_horizon$RMSE <- sapply(unique(forecast_shifted_df_lasso_xgb$Horizon), function(h) {
  rmse_vals <- forecast_shifted_df_lasso_xgb[forecast_shifted_df_lasso_xgb$Horizon == h, ]
  rmse(rmse_vals$Real, rmse_vals$Forecast)
})
print(rmse_post_by_horizon[, c("Horizon", "RMSE")])



rmse_plot_df <- data.frame(
  Horizon = rep(1:forecast_horizon, 2),
  RMSE = rmse_post_by_horizon$RMSE,
  Model = rep("XGBOOST", each = forecast_horizon)
)

err_Xb_lasso_17_21_linear <- errors_by_h(forecast_shifted_df_lasso_xgb)


rfr1 <- ggplot(rmse_plot_df, aes(x = Horizon, y = RMSE, colour = Model)) +
  geom_line(size = 1.3, alpha = 0.9) +
  geom_point(,size = 3) +
  scale_color_manual(values = c("XGBOOST" = "red"), name = "")+
  scale_x_continuous(labels = number_format(accuracy = 1)) + 
  labs(
    title = "2017K2-2021K2",
    x = "Horizontas",
    y = "RMSE",
    color = "Modelis"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )
########################################################XGBOOST+Lasso#####################################################
#####################################################2019-2023#####################################################
library(tidyfit)
library(pbapply)
library(forecast)
library(zoo)
library(glmnet)
library(randomForest)
library(xgboost)
library(caret)

window_size <- 79
forecast_horizon <- 8
target_time <- "2021K2"
max_index <- which(stationary_table$Time == target_time)

X <- as.matrix(stationary_table[, -c(1, 2)])
y <- as.numeric(unlist(stationary_table[, 2]))

set.seed(123)
final_forecasts_lasso_xgb <- list()
final_arima_forecasts <- list()


for (i in 1:(max_index - window_size + 1)) {
  
  X_window <- X[i:(i + window_size - 1), ]
  y_window <- y[i:(i + window_size - 1)]
  
  # --- LASSO atranka ----------------------------------------------------
  lasso_model <- cv.glmnet(X_window, y_window, family = "gaussian", alpha = 1)
  support_vars <- which(coef(lasso_model, s = "lambda.min")[-1, 1] != 0)
  
  # jei atrenkama < 2 kintamųjų, parenkame kitą lambda
  if (length(support_vars) < 2) {
    alt_lambda <- lambda_n(lasso_model, N = 5)
    support_vars <- which(coef(lasso_model, s = alt_lambda)[-1, 1] != 0)
  }
  
  if (length(support_vars) > 0) {
    
    X_sel <- X_window[, support_vars, drop = FALSE]
    sel_names <- colnames(X_sel)
    
    # --- XGBoost parametrai --------------------------------------------
    xgb_grid <- expand.grid(
      nrounds = c(100, 150, 200),
      eta     = c(0.05, 0.1, 0.15),
      lambda  = c(0, 0.1, 1),
      alpha   = c(0, 0.1, 1)
    )
    
    train_control <- trainControl(
      method        = "timeslice",
      initialWindow = 60,
      horizon       = 9,
      fixedWindow   = TRUE,
      verboseIter   = FALSE
    )
    
    xgb_model_final <- train(
      x = as.data.frame(X_sel),
      y = y_window,
      method   = "xgbLinear",
      trControl = train_control,
      tuneGrid  = xgb_grid,
      verbose   = FALSE
    )
    
    # --- ARIMA prognozės atrinktiems kintamiesiems ---------------------
    arima_forecasts <- lapply(seq_len(ncol(X_sel)), function(j) {
      forecast(auto.arima(X_sel[, j]), h = forecast_horizon)$mean
    })
    
    # suformuojame prognozių duomenų rėmelį tokia pačia tvarka kaip X_sel
    arima_df <- setNames(as.data.frame(do.call(cbind, arima_forecasts)),
                         sel_names)
    
    # --- prognozuojame BVP su XGBoost ----------------------------------
    preds <- predict(xgb_model_final, newdata = arima_df)
    final_forecasts_lasso_xgb[[i]] <- preds
  }
}
best_params <- xgb_model$bestTune
print(best_params)
xgb_model$results
ggplot(xgb_model$results, aes(x = nrounds, y = RMSE, color = factor(max_depth))) +
  geom_line() +
  facet_wrap(~ eta, scales = "free") +
  theme_minimal()
print(final_forecasts_lasso_xgb)

first_two_columns <- stationary_table[, 1:2]
valid_lasso_xgb <- which((window_size + seq_along(final_forecasts_lasso_xgb) + forecast_horizon - 1) <= 300)
max_time_index <- max(window_size + valid_lasso_xgb + forecast_horizon - 1)
n_needed <- max_time_index - nrow(first_two_columns)

#jei reikia pridėti eilutes
if (n_needed > 0) {
  last_time <- first_two_columns$Time[nrow(first_two_columns)]
  new_times <- generate_quarters(last_time, n_needed)
  new_rows <- as.data.frame(matrix(NA, nrow = n_needed, ncol = ncol(first_two_columns)))
  colnames(new_rows) <- colnames(first_two_columns)
  new_rows$Time <- new_times
  first_two_columns <- rbind(first_two_columns, new_rows)
}
# Random Forest — prognozės kiekvienam langui
all_lasso_xgb_forecasts_shifted <- do.call(rbind, lapply(valid_lasso_xgb, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_lasso_xgb[[i]],
    Type = "Lasso su Xgb",
    Window = i
  )
}))

real_bvp_df <- data.frame(
  Time = first_two_columns$Time,
  Real = first_two_columns$BVP
)
start_period<-"2019K2"
end_period<-"2023K2"
#apkerpam laika
real_bvp_df_trimmed <- real_bvp_df[real_bvp_df$Time >= start_period & real_bvp_df$Time <= end_period, ]


ggplot() +
  geom_line(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), linewidth = 1.2) +
  geom_point(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), size = 2) +
  geom_line(data = subset(all_lasso_xgb_forecasts_shifted, Type == "Lasso su Xgb"),
            aes(x = Time, y = Forecast, group = Window, color = interaction(Type, Window)),
            alpha = 0.6, linewidth = 0.9) +
  labs(
    title = "BVP prognozių palyginimas su Xgboost",
    x = "Ketvirtis", y = "BVP", color = "Modelis ir langas"
  ) +
  scale_color_manual(
    values = c("Reali reikšmė" = "darkgreen"),
    guide = "none"
  ) +
  scale_color_viridis_d(option = "plasma", begin = 0, end = 0.85, direction = -1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
forecast_shifted_df_lasso_xgb<-all_lasso_xgb_forecasts_shifted
forecast_shifted_df_lasso_xgb$Horizon <- rep(1:forecast_horizon, times = length(unique(forecast_shifted_df_lasso_xgb$Window)))
forecast_shifted_df_lasso_xgb <- do.call(rbind, lapply(valid_lasso_xgb, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_lasso_xgb[[i]],
    Horizon = 1:forecast_horizon,
    Window = i
  )
}))
forecast_shifted_df_lasso_xgb
num_windows <- length(unique(forecast_shifted_df_lasso_xgb$Horizon))

blue_shades <- viridis(num_windows, option = "plasma", begin = 0, end = 0.9, direction = -1)
x12 <- ggplot() +
  geom_line(data = real_bvp_df_trimmed,
            aes(x = Time, y = Real, color = "Realybė", group = 1),
            size = 1.2) +
  geom_point(data = real_bvp_df_trimmed,
             aes(x = Time, y = Real, color = "Realybė")) +
  
  geom_line(data = forecast_shifted_df_lasso_xgb,
            aes(x = Time, y = Forecast, group = Horizon, color = as.factor(Horizon)),
            size = 1.2, linetype = "dashed") +
  geom_point(data = forecast_shifted_df_lasso_xgb,
             aes(x = Time, y = Forecast, color = as.factor(Horizon))) +
  
  scale_color_manual(
    values = c(setNames(blue_shades, as.character(unique(forecast_shifted_df_lasso_xgb$Horizon))),
               "Realybė" = "red"),
    guide = guide_legend(override.aes = list(linetype = c(rep("dashed", num_windows), "solid"),
                                             size = c(rep(1.2, num_windows), 1.2)), title = "Horizontai")
  ) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )+
  labs(title = "BVP prognozės (XGBOOST su LASSO) ir realybė pagal horizontus",
       x = "Kvartalas",
       y = "BVP",
       color = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold")) 


ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs66.png", plot = x12, width = 10, height = 6, dpi = 300, bg = "white")



forecast_shifted_df_lasso_xgb <- merge(forecast_shifted_df_lasso_xgb, real_bvp_df_trimmed, by = "Time")
forecast_shifted_df_lasso_xgb <- forecast_shifted_df_lasso_xgb[!is.na(forecast_shifted_df_lasso_xgb$Real),]
rmse_post_by_horizon <- aggregate(cbind(Forecast, Real) ~ Horizon, data = forecast_shifted_df_lasso_xgb, FUN = function(x) x)
rmse_post_by_horizon$RMSE <- sapply(unique(forecast_shifted_df_lasso_xgb$Horizon), function(h) {
  rmse_vals <- forecast_shifted_df_lasso_xgb[forecast_shifted_df_lasso_xgb$Horizon == h, ]
  rmse(rmse_vals$Real, rmse_vals$Forecast)
})
print(rmse_post_by_horizon[, c("Horizon", "RMSE")])



rmse_plot_df <- data.frame(
  Horizon = rep(1:forecast_horizon, 2),
  RMSE = rmse_post_by_horizon$RMSE,
  Model = rep("XGBOOST", each = forecast_horizon)
)

err_Xb_lasso_19_23_linear <- errors_by_h(forecast_shifted_df_lasso_xgb)


rfr2 <- ggplot(rmse_plot_df, aes(x = Horizon, y = RMSE, colour = Model)) +
  geom_line(size = 1.3, alpha = 0.9) +
  geom_point(,size = 3) +
  scale_color_manual(values = c("XGBOOST" = "red"), name = "")+
  scale_x_continuous(labels = number_format(accuracy = 1)) + 
  labs(
    title = "2019K2-2023K2",
    x = "Horizontas",
    y = "RMSE",
    color = "Modelis"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )


top_row <- rfr1 + rfr2

bottom_row <- plot_spacer() + rfr3 + plot_spacer()
bottom_row <- bottom_row + plot_layout(widths = c(1, 2, 1))

final_plot <- (top_row / bottom_row) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

final_plot <- final_plot +
  plot_layout(guides = "collect") +   
  plot_annotation(
    title = "RMSE pagal horizontus",
    theme = theme(
      plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),legend.position = "bottom"  
    )
  )

x11()
final_plot
ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs67.png", plot = final_plot, width = 10, height = 6, dpi = 300, bg = "white")


#########################################################################################################################
#########################################################################################################################
#################################################################XGBOOST+RF 2021-2027##############################################
library(tidyfit)
library(pbapply)
library(forecast)
library(zoo)
library(glmnet)
library(randomForest)
library(xgboost)
library(caret)

window_size <- 78
forecast_horizon <- 9
target_time <- "2024K4"
max_index <- which(stationary_table$Time == target_time)

X <- as.matrix(stationary_table[, -c(1, 2)])
y <- as.numeric(unlist(stationary_table[, 2]))

set.seed(123)
final_forecasts_xgb_rf <- list()

for (i in 1:(max_index - window_size + 1)) {
  X_window <- X[i:(i + window_size - 1), ]
  y_window <- y[i:(i + window_size - 1)]
  
  dtrain <- xgb.DMatrix(data = X_window, label = y_window)
  model_importance <- xgboost(
    data = dtrain,
    max.depth = 5,
    eta = 0.1,
    nrounds = 100,
    objective = "reg:squarederror",
    verbose = 0
  )
  
  importance <- xgb.importance(model = model_importance)
  top_20_vars <- importance$Feature[1:min(20, nrow(importance))]
  
  X_window_selected <- X_window[, top_20_vars, drop = FALSE]
  
  arima_forecasts <- list()
  for (j in 1:ncol(X_window_selected)) {
    model <- auto.arima(X_window_selected[, j])
    arima_forecasts[[j]] <- as.numeric(forecast(model, h = forecast_horizon)$mean)
  }
  
  if (length(arima_forecasts) > 0) {
    arima_forecasts_matrix <- do.call(rbind, arima_forecasts)
    arima_forecasts_df <- as.data.frame(t(arima_forecasts_matrix))
    colnames(arima_forecasts_df) <- top_20_vars
    
    rf_model <- randomForest(
      x = as.data.frame(X_window_selected),
      y = y_window,
      ntree = 1000
    )
    
    predictions_rf <- predict(rf_model, newdata = arima_forecasts_df)
    final_forecasts_xgb_rf[[i]] <- predictions_rf
  }
}

print(final_forecasts_xgb_rf)

first_two_columns <- stationary_table[, 1:2]
valid_xgb_rf <- which((window_size + seq_along(final_forecasts_xgb_rf) + forecast_horizon - 1) <= 300)
max_time_index <- max(window_size + valid_xgb_rf + forecast_horizon - 1)
n_needed <- max_time_index - nrow(first_two_columns)

#jei reikia pridėti eilutes
if (n_needed > 0) {
  last_time <- first_two_columns$Time[nrow(first_two_columns)]
  new_times <- generate_quarters(last_time, n_needed)
  new_rows <- as.data.frame(matrix(NA, nrow = n_needed, ncol = ncol(first_two_columns)))
  colnames(new_rows) <- colnames(first_two_columns)
  new_rows$Time <- new_times
  first_two_columns <- rbind(first_two_columns, new_rows)
}
# Random Forest — prognozės kiekvienam langui
all_xgb_rf_forecasts_shifted <- do.call(rbind, lapply(valid_xgb_rf, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_xgb_rf[[i]],
    Type = "Xgb su Rf",
    Window = i
  )
}))

real_bvp_df <- data.frame(
  Time = first_two_columns$Time,
  Real = first_two_columns$BVP
)

#apkerpam laika
real_bvp_df_trimmed <- real_bvp_df[(window_size + 1):nrow(real_bvp_df), ]


ggplot() +
  geom_line(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), linewidth = 1.2) +
  geom_point(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), size = 2) +
  geom_line(data = subset(all_xgb_rf_forecasts_shifted, Type == "Xgb su Rf"),
            aes(x = Time, y = Forecast, group = Window, color = interaction(Type, Window)),
            alpha = 0.6, linewidth = 0.9) +
  labs(
    title = "BVP prognozių palyginimas su Xgboost",
    x = "Ketvirtis", y = "BVP", color = "Modelis ir langas"
  ) +
  scale_color_manual(
    values = c("Reali reikšmė" = "darkgreen"),
    guide = "none"
  ) +
  scale_color_viridis_d(option = "plasma", begin = 0, end = 0.85, direction = -1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


forecast_shifted_df_xgb_rf<-all_xgb_rf_forecasts_shifted
forecast_shifted_df_xgb_rf <- do.call(rbind, lapply(valid_xgb_rf, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time     = first_two_columns$Time[time_range],
    Forecast = final_forecasts_xgb_rf[[i]],
    Horizon  = 1:forecast_horizon,
    Window   = i
  )
}))


num_windows <- length(unique(forecast_shifted_df_xgb_rf$Horizon))

blue_shades <- viridis(num_windows, option = "plasma", begin = 0, end = 0.9, direction = -1)
x12 <- ggplot() +
  geom_line(data = real_bvp_df_trimmed,
            aes(x = Time, y = Real, color = "Realybė", group = 1),
            size = 1.2) +
  geom_point(data = real_bvp_df_trimmed,
             aes(x = Time, y = Real, color = "Realybė")) +
  
  geom_line(data = forecast_shifted_df_xgb_rf,
            aes(x = Time, y = Forecast, group = Horizon, color = as.factor(Horizon)),
            size = 1.2, linetype = "dashed") +
  geom_point(data = forecast_shifted_df_xgb_rf,
             aes(x = Time, y = Forecast, color = as.factor(Horizon))) +
  
  scale_color_manual(
    values = c(setNames(blue_shades, as.character(unique(forecast_shifted_df_xgb_rf$Horizon))),
               "Realybė" = "red"),
    guide = guide_legend(override.aes = list(linetype = c(rep("dashed", num_windows), "solid"),
                                             size = c(rep(1.2, num_windows), 1.2)), title = "Horizontai")
  ) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )+
  labs(title = "BVP prognozės (Random Forest su XGBOOST) ir realybė pagal horizontus",
       x = "Kvartalas",
       y = "BVP",
       color = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold")) 


ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs76.png", plot = x12, width = 10, height = 6, dpi = 300, bg = "white")



forecast_shifted_df_xgb_rf <- merge(forecast_shifted_df_xgb_rf, real_bvp_df_trimmed, by = "Time")
forecast_shifted_df_xgb_rf <- forecast_shifted_df_xgb_rf[!is.na(forecast_shifted_df_xgb_rf$Real),]
rmse_post_by_horizon <- aggregate(cbind(Forecast, Real) ~ Horizon, data = forecast_shifted_df_xgb_rf, FUN = function(x) x)
rmse_post_by_horizon$RMSE <- sapply(unique(forecast_shifted_df_xgb_rf$Horizon), function(h) {
  rmse_vals <- forecast_shifted_df_xgb_rf[forecast_shifted_df_xgb_rf$Horizon == h, ]
  rmse(rmse_vals$Real, rmse_vals$Forecast)
})
print(rmse_post_by_horizon[, c("Horizon", "RMSE")])



rmse_plot_df <- data.frame(
  Horizon = rep(1:forecast_horizon, 2),
  RMSE = rmse_post_by_horizon$RMSE,
  Model = rep("Random Forest", each = forecast_horizon)
)

err_Xb_RF_21_27_last <- errors_by_h(forecast_shifted_df_xgb_rf)


rfr3 <- ggplot(rmse_plot_df, aes(x = Horizon, y = RMSE, colour = Model)) +
  geom_line(size = 1.3, alpha = 0.9) +
  geom_point(,size = 3) +
  scale_color_manual(values = c("Random Forest" = "red"), name = "")+
  scale_x_continuous(labels = number_format(accuracy = 1)) + 
  labs(
    title = "2021K1-2024K4",
    x = "Horizontas",
    y = "RMSE",
    color = "Modelis"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )
#########################################################################################################################
#########################################################################################################################
#################################################################XGBOOST+RF 2017-2021##############################################
library(tidyfit)
library(pbapply)
library(forecast)
library(zoo)
library(glmnet)
library(randomForest)
library(xgboost)
library(caret)

window_size <- 71
forecast_horizon <- 8
target_time <- "2019K2"
max_index <- which(stationary_table$Time == target_time)

X <- as.matrix(stationary_table[, -c(1, 2)])
y <- as.numeric(unlist(stationary_table[, 2]))

set.seed(123)
final_forecasts_xgb_rf <- list()

for (i in 1:(max_index - window_size + 1)) {
  X_window <- X[i:(i + window_size - 1), ]
  y_window <- y[i:(i + window_size - 1)]
  
  dtrain <- xgb.DMatrix(data = X_window, label = y_window)
  model_importance <- xgboost(
    data = dtrain,
    max.depth = 5,
    eta = 0.1,
    nrounds = 100,
    objective = "reg:squarederror",
    verbose = 0
  )
  
  importance <- xgb.importance(model = model_importance)
  top_20_vars <- importance$Feature[1:min(20, nrow(importance))]
  
  X_window_selected <- X_window[, top_20_vars, drop = FALSE]
  
  arima_forecasts <- list()
  for (j in 1:ncol(X_window_selected)) {
    model <- auto.arima(X_window_selected[, j])
    arima_forecasts[[j]] <- as.numeric(forecast(model, h = forecast_horizon)$mean)
  }
  
  if (length(arima_forecasts) > 0) {
    arima_forecasts_matrix <- do.call(rbind, arima_forecasts)
    arima_forecasts_df <- as.data.frame(t(arima_forecasts_matrix))
    colnames(arima_forecasts_df) <- top_20_vars
    
    rf_model <- randomForest(
      x = as.data.frame(X_window_selected),
      y = y_window,
      ntree = 1000
    )
    
    predictions_rf <- predict(rf_model, newdata = arima_forecasts_df)
    final_forecasts_xgb_rf[[i]] <- predictions_rf
  }
}

print(final_forecasts_xgb_rf)

first_two_columns <- stationary_table[, 1:2]
valid_xgb_rf <- which((window_size + seq_along(final_forecasts_xgb_rf) + forecast_horizon - 1) <= 300)
max_time_index <- max(window_size + valid_xgb_rf + forecast_horizon - 1)
n_needed <- max_time_index - nrow(first_two_columns)

#jei reikia pridėti eilutes
if (n_needed > 0) {
  last_time <- first_two_columns$Time[nrow(first_two_columns)]
  new_times <- generate_quarters(last_time, n_needed)
  new_rows <- as.data.frame(matrix(NA, nrow = n_needed, ncol = ncol(first_two_columns)))
  colnames(new_rows) <- colnames(first_two_columns)
  new_rows$Time <- new_times
  first_two_columns <- rbind(first_two_columns, new_rows)
}
# Random Forest — prognozės kiekvienam langui
all_xgb_rf_forecasts_shifted <- do.call(rbind, lapply(valid_xgb_rf, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_xgb_rf[[i]],
    Type = "Xgb su Rf",
    Window = i
  )
}))

real_bvp_df <- data.frame(
  Time = first_two_columns$Time,
  Real = first_two_columns$BVP
)

start_period<-"2017K2"
end_period<-"2021K2"
#apkerpam laika
real_bvp_df_trimmed <- real_bvp_df[real_bvp_df$Time >= start_period & real_bvp_df$Time <= end_period, ]


ggplot() +
  geom_line(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), linewidth = 1.2) +
  geom_point(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), size = 2) +
  geom_line(data = subset(all_xgb_rf_forecasts_shifted, Type == "Xgb su Rf"),
            aes(x = Time, y = Forecast, group = Window, color = interaction(Type, Window)),
            alpha = 0.6, linewidth = 0.9) +
  labs(
    title = "BVP prognozių palyginimas su Xgboost",
    x = "Ketvirtis", y = "BVP", color = "Modelis ir langas"
  ) +
  scale_color_manual(
    values = c("Reali reikšmė" = "darkgreen"),
    guide = "none"
  ) +
  scale_color_viridis_d(option = "plasma", begin = 0, end = 0.85, direction = -1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
forecast_shifted_df_xgb_rf<-all_xgb_rf_forecasts_shifted
forecast_shifted_df_xgb_rf$Horizon <- rep(1:forecast_horizon, times = length(unique(forecast_shifted_df_xgb_rf$Window)))
forecast_shifted_df_xgb_rf <- do.call(rbind, lapply(valid_lasso_xgb, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_xgb_rf[[i]],
    Horizon = 1:forecast_horizon,
    Window = i
  )
}))
forecast_shifted_df_xgb_rf
num_windows <- length(unique(forecast_shifted_df_xgb_rf$Horizon))

blue_shades <- viridis(num_windows, option = "plasma", begin = 0, end = 0.9, direction = -1)
x12 <- ggplot() +
  geom_line(data = real_bvp_df_trimmed,
            aes(x = Time, y = Real, color = "Realybė", group = 1),
            size = 1.2) +
  geom_point(data = real_bvp_df_trimmed,
             aes(x = Time, y = Real, color = "Realybė")) +
  
  geom_line(data = forecast_shifted_df_xgb_rf,
            aes(x = Time, y = Forecast, group = Horizon, color = as.factor(Horizon)),
            size = 1.2, linetype = "dashed") +
  geom_point(data = forecast_shifted_df_xgb_rf,
             aes(x = Time, y = Forecast, color = as.factor(Horizon))) +
  
  scale_color_manual(
    values = c(setNames(blue_shades, as.character(unique(forecast_shifted_df_xgb_rf$Horizon))),
               "Realybė" = "red"),
    guide = guide_legend(override.aes = list(linetype = c(rep("dashed", num_windows), "solid"),
                                             size = c(rep(1.2, num_windows), 1.2)), title = "Horizontai")
  ) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )+
  labs(title = "BVP prognozės (Random Forest su XGBOOST) ir realybė pagal horizontus",
       x = "Kvartalas",
       y = "BVP",
       color = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold")) 


ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs77.png", plot = x12, width = 10, height = 6, dpi = 300, bg = "white")



forecast_shifted_df_xgb_rf <- merge(forecast_shifted_df_xgb_rf, real_bvp_df_trimmed, by = "Time")
forecast_shifted_df_xgb_rf <- forecast_shifted_df_xgb_rf[!is.na(forecast_shifted_df_xgb_rf$Real),]
rmse_post_by_horizon <- aggregate(cbind(Forecast, Real) ~ Horizon, data = forecast_shifted_df_xgb_rf, FUN = function(x) x)
rmse_post_by_horizon$RMSE <- sapply(unique(forecast_shifted_df_xgb_rf$Horizon), function(h) {
  rmse_vals <- forecast_shifted_df_xgb_rf[forecast_shifted_df_xgb_rf$Horizon == h, ]
  rmse(rmse_vals$Real, rmse_vals$Forecast)
})
print(rmse_post_by_horizon[, c("Horizon", "RMSE")])



rmse_plot_df <- data.frame(
  Horizon = rep(1:forecast_horizon, 2),
  RMSE = rmse_post_by_horizon$RMSE,
  Model = rep("Random Forest", each = forecast_horizon)
)

err_Xb_RF_17_21_last <- errors_by_h(forecast_shifted_df_xgb_rf)


rfr1 <- ggplot(rmse_plot_df, aes(x = Horizon, y = RMSE, colour = Model)) +
  geom_line(size = 1.3, alpha = 0.9) +
  geom_point(,size = 3) +
  scale_color_manual(values = c("Random Forest" = "red"), name = "")+
  scale_x_continuous(labels = number_format(accuracy = 1)) + 
  labs(
    title = "2017K2-2021K2",
    x = "Horizontas",
    y = "RMSE",
    color = "Modelis"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )
#########################################################################################################################
#########################################################################################################################
#################################################################XGBOOST+RF 2019-2023##############################################
library(tidyfit)
library(pbapply)
library(forecast)
library(zoo)
library(glmnet)
library(randomForest)
library(xgboost)
library(caret)

window_size <- 79
forecast_horizon <- 8
target_time <- "2021K2"
max_index <- which(stationary_table$Time == target_time)

X <- as.matrix(stationary_table[, -c(1, 2)])
y <- as.numeric(unlist(stationary_table[, 2]))

set.seed(123)
final_forecasts_xgb_rf <- list()

for (i in 1:(max_index - window_size + 1)) {
  X_window <- X[i:(i + window_size - 1), ]
  y_window <- y[i:(i + window_size - 1)]
  
  dtrain <- xgb.DMatrix(data = X_window, label = y_window)
  model_importance <- xgboost(
    data = dtrain,
    max.depth = 5,
    eta = 0.1,
    nrounds = 100,
    objective = "reg:squarederror",
    verbose = 0
  )
  
  importance <- xgb.importance(model = model_importance)
  top_20_vars <- importance$Feature[1:min(20, nrow(importance))]
  
  X_window_selected <- X_window[, top_20_vars, drop = FALSE]
  
  arima_forecasts <- list()
  for (j in 1:ncol(X_window_selected)) {
    model <- auto.arima(X_window_selected[, j])
    arima_forecasts[[j]] <- as.numeric(forecast(model, h = forecast_horizon)$mean)
  }
  
  if (length(arima_forecasts) > 0) {
    arima_forecasts_matrix <- do.call(rbind, arima_forecasts)
    arima_forecasts_df <- as.data.frame(t(arima_forecasts_matrix))
    colnames(arima_forecasts_df) <- top_20_vars
    
    rf_model <- randomForest(
      x = as.data.frame(X_window_selected),
      y = y_window,
      ntree = 1000
    )
    
    predictions_rf <- predict(rf_model, newdata = arima_forecasts_df)
    final_forecasts_xgb_rf[[i]] <- predictions_rf
  }
}

print(final_forecasts_xgb_rf)

first_two_columns <- stationary_table[, 1:2]
valid_xgb_rf <- which((window_size + seq_along(final_forecasts_xgb_rf) + forecast_horizon - 1) <= 300)
max_time_index <- max(window_size + valid_xgb_rf + forecast_horizon - 1)
n_needed <- max_time_index - nrow(first_two_columns)

#jei reikia pridėti eilutes
if (n_needed > 0) {
  last_time <- first_two_columns$Time[nrow(first_two_columns)]
  new_times <- generate_quarters(last_time, n_needed)
  new_rows <- as.data.frame(matrix(NA, nrow = n_needed, ncol = ncol(first_two_columns)))
  colnames(new_rows) <- colnames(first_two_columns)
  new_rows$Time <- new_times
  first_two_columns <- rbind(first_two_columns, new_rows)
}
# Random Forest — prognozės kiekvienam langui
all_xgb_rf_forecasts_shifted <- do.call(rbind, lapply(valid_xgb_rf, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_xgb_rf[[i]],
    Type = "Xgb su Rf",
    Window = i
  )
}))

real_bvp_df <- data.frame(
  Time = first_two_columns$Time,
  Real = first_two_columns$BVP
)

#apkerpam laika
start_period<-"2019K2"
end_period<-"2023K2"
#apkerpam laika
real_bvp_df_trimmed <- real_bvp_df[real_bvp_df$Time >= start_period & real_bvp_df$Time <= end_period, ]


ggplot() +
  geom_line(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), linewidth = 1.2) +
  geom_point(data = real_bvp_df_trimmed, aes(x = Time, y = Real, color = "Reali reikšmė"), size = 2) +
  geom_line(data = subset(all_xgb_rf_forecasts_shifted, Type == "Xgb su Rf"),
            aes(x = Time, y = Forecast, group = Window, color = interaction(Type, Window)),
            alpha = 0.6, linewidth = 0.9) +
  labs(
    title = "BVP prognozių palyginimas su Xgboost",
    x = "Ketvirtis", y = "BVP", color = "Modelis ir langas"
  ) +
  scale_color_manual(
    values = c("Reali reikšmė" = "darkgreen"),
    guide = "none"
  ) +
  scale_color_viridis_d(option = "plasma", begin = 0, end = 0.85, direction = -1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
forecast_shifted_df_xgb_rf<-all_xgb_rf_forecasts_shifted
forecast_shifted_df_xgb_rf$Horizon <- rep(1:forecast_horizon, times = length(unique(forecast_shifted_df_xgb_rf$Window)))
forecast_shifted_df_xgb_rf <- do.call(rbind, lapply(valid_lasso_xgb, function(i) {
  start_time_index <- window_size + i
  time_range <- start_time_index:(start_time_index + forecast_horizon - 1)
  data.frame(
    Time = first_two_columns$Time[time_range],
    Forecast = final_forecasts_xgb_rf[[i]],
    Horizon = 1:forecast_horizon,
    Window = i
  )
}))
forecast_shifted_df_xgb_rf
num_windows <- length(unique(forecast_shifted_df_xgb_rf$Horizon))

blue_shades <- viridis(num_windows, option = "plasma", begin = 0, end = 0.9, direction = -1)
x13 <- ggplot() +
  geom_line(data = real_bvp_df_trimmed,
            aes(x = Time, y = Real, color = "Realybė", group = 1),
            size = 1.2) +
  geom_point(data = real_bvp_df_trimmed,
             aes(x = Time, y = Real, color = "Realybė")) +
  
  geom_line(data = forecast_shifted_df_xgb_rf,
            aes(x = Time, y = Forecast, group = Horizon, color = as.factor(Horizon)),
            size = 1.2, linetype = "dashed") +
  geom_point(data = forecast_shifted_df_xgb_rf,
             aes(x = Time, y = Forecast, color = as.factor(Horizon))) +
  
  scale_color_manual(
    values = c(setNames(blue_shades, as.character(unique(forecast_shifted_df_xgb_rf$Horizon))),
               "Realybė" = "red"),
    guide = guide_legend(override.aes = list(linetype = c(rep("dashed", num_windows), "solid"),
                                             size = c(rep(1.2, num_windows), 1.2)), title = "Horizontai")
  ) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )+
  labs(title = "BVP prognozės (Random Forest su XGBOOST) ir realybė pagal horizontus",
       x = "Kvartalas",
       y = "BVP",
       color = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold")) 


ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs78.png", plot = x13, width = 10, height = 6, dpi = 300, bg = "white")



forecast_shifted_df_xgb_rf <- merge(forecast_shifted_df_xgb_rf, real_bvp_df_trimmed, by = "Time")
forecast_shifted_df_xgb_rf <- forecast_shifted_df_xgb_rf[!is.na(forecast_shifted_df_xgb_rf$Real),]
rmse_post_by_horizon <- aggregate(cbind(Forecast, Real) ~ Horizon, data = forecast_shifted_df_xgb_rf, FUN = function(x) x)
rmse_post_by_horizon$RMSE <- sapply(unique(forecast_shifted_df_xgb_rf$Horizon), function(h) {
  rmse_vals <- forecast_shifted_df_xgb_rf[forecast_shifted_df_xgb_rf$Horizon == h, ]
  rmse(rmse_vals$Real, rmse_vals$Forecast)
})
print(rmse_post_by_horizon[, c("Horizon", "RMSE")])



rmse_plot_df <- data.frame(
  Horizon = rep(1:forecast_horizon, 2),
  RMSE = rmse_post_by_horizon$RMSE,
  Model = rep("Random Forest", each = forecast_horizon)
)

err_Xb_RF_19_23_last <- errors_by_h(forecast_shifted_df_xgb_rf)


rfr2 <- ggplot(rmse_plot_df, aes(x = Horizon, y = RMSE, colour = Model)) +
  geom_line(size = 1.3, alpha = 0.9) +
  geom_point(,size = 3) +
  scale_color_manual(values = c("Random Forest" = "red"), name = "")+
  scale_x_continuous(labels = number_format(accuracy = 1)) + 
  labs(
    title = "2019K2-2023K2",
    x = "Horizontas",
    y = "RMSE",
    color = "Modelis"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )

top_row <- rfr1 + rfr2

bottom_row <- plot_spacer() + rfr3 + plot_spacer()
bottom_row <- bottom_row + plot_layout(widths = c(1, 2, 1))

final_plot <- (top_row / bottom_row) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

final_plot <- final_plot +
  plot_layout(guides = "collect") +   
  plot_annotation(
    title = "RMSE pagal horizontus",
    theme = theme(
      plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),legend.position = "bottom"  
    )
  )

x11()
final_plot
ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs79.png", plot = final_plot, width = 10, height = 6, dpi = 300, bg = "white")




#########################################################################################################################
#########################################################################################################################
#-------Testavimas-------------------


#DM testas kiekvienam horizontui ---------------------

# Dvipuse alternatyva
min_len <- min(length(err_arima_17_21), length(err_Rf_Xb_17_21))

dm_results <- lapply(1:min_len, function(h) {
  cat(paste0("Horizontas ", h, ":\n"))
  dm.test(err_arima_17_21[[h]], err_Rf_Xb_17_21[[h]],
          alternative = "two.sided",
          h = h,
          power = 1,
          varestimator = "bartlett")
})

p_values <- sapply(dm_results, function(res) res$p.value)
names(p_values) <- paste0("h", 1:min_len)
print(p_values)

dm_summary <- data.frame(
  Horizon = 1:min_len,
  Statistic = sapply(dm_results, function(res) res$statistic),
  P_Value = p_values
)
print(dm_summary)




e1_all <- unlist(err_arima_17_21)
e2_all <- unlist(err_Rf_Xb_17_21)
dm.test(e1_all, e2_all, alternative = "two.sided", h = 1, power = 1)




# Vienpuse alternatyva

min_len <- min(length(err_arima_21_27), length(err_Rf_Xb_ln_19_27))
dm_results <- lapply(1:min_len, function(h) {
  cat(paste0("Horizontas ", h, ":\n"))
  dm.test(
    err_arima_21_27[[h]],       
    err_Rf_Xb_ln_19_27[[h]],    
    alternative = "less",     
    h           = h,
    power       = 1,
    varestimator= "bartlett"
  )
})

p_values <- sapply(dm_results, function(res) res$p.value)
names(p_values) <- paste0("h", 1:min_len)
print(p_values)

dm_summary <- data.frame(
  Horizon = 1:min_len,
  Statistic = sapply(dm_results, function(res) res$statistic),
  P_Value = p_values
)
print(dm_summary)


e1_all <- unlist(err_arima_21_27)
e2_all <- unlist(err_Rf_Xb_ln_19_27)
dm.test(e1_all, e2_all,
        alternative = "less",  
        h = 1,
        power = 1)





