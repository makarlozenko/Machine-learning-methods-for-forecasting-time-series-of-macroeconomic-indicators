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



#------------------------Darbas su jau stacionariu final table-------------------------

stationary_table <- read_excel("C:/Users/Admin/Desktop/Kursinis 2025/stationary_table_long.xlsx")


###########################################################################################################
###########################################################################################################

#------------------------Koreliacija-------------------------


calculate_correlation_with_gdp <- function(data, output_file = "Correlation_results.xlsx") {
  gdp_column <- data[[2]]
  results <- data.frame(Variable = character(), Correlation = numeric(), stringsAsFactors = FALSE)
  
  for (col_name in colnames(data)[-c(1, 2)]) { 
    column <- data[[col_name]]
    
    if (is.numeric(column)) {
      corr_value <- cor(gdp_column, column, use = "complete.obs")
      results <- rbind(results, data.frame(Variable = col_name, Correlation = corr_value))
    }
  }
  
  write_xlsx(results, output_file)
  
  print(paste("Saved in", output_file))
  return(results)
}

correlation_results <- calculate_correlation_with_gdp(stationary_table, "C:/Users/Admin/Desktop/Kursinis 2025/Correlation_results.xlsx")
head(correlation_results)


#------------------------TOP 100 Koreliacija-------------------------
absolute_correlations <- abs(correlation_results$Correlation)
top_100_indices <- order(absolute_correlations, decreasing = TRUE)[1:100]
top_100_table_cor <- correlation_results[top_100_indices, ]

write_xlsx(top_100_table_cor, "C:\\Users\\Admin\\Desktop\\Kursinis 2025\\top_100_coreliation.xlsx")


###########################################################################################################
###########################################################################################################

#------------------------Lasso-------------------------

set.seed(3)
X <- as.matrix(stationary_table[, -c(1, 2)]) 
y <- as.numeric(unlist(stationary_table[, 2]))


cv_model <- cv.glmnet(X, y, family = "gaussian", alpha = 1)

best_lambda <- cv_model$lambda.min
print(best_lambda)

plot(cv_model)

predictions <- predict(cv_model, newx = X, s = best_lambda)
best_lambda <- cv_model$lambda.min

best_coef <- coef(cv_model, s = "lambda.min")

best_coef_vector <- as.vector(best_coef)
names(best_coef_vector) <- rownames(best_coef)



#------------------------TOP 100 Lasso-------------------------

sorted_coef <- sort(abs(best_coef_vector), decreasing = TRUE)

top_100_coef <- sorted_coef[1:100]

top_100_names <- names(top_100_coef)

top_100_table <- data.frame(
  Variable = top_100_names,
  Coefficient = top_100_coef
)


View(top_100_table)
write_xlsx(top_100_table, "C:\\Users\\Admin\\Desktop\\Kursinis 2025\\top_100_coefficients_Lasso.xlsx")



nonzero_coef <- best_coef_vector[best_coef_vector != 0]
nonzero_coef <- nonzero_coef[names(nonzero_coef) != "(Intercept)"]
nonzero_names <- names(nonzero_coef)
length(nonzero_names)

# nenuliniu 41


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

#------------------------TOP 100 Mutual information-------------------------

top_100_variables <- mi_results[order(mi_results$MI, decreasing = TRUE), ][1:100, ]
View(top_100_variables)

write_xlsx(top_100_variables, "C:\\Users\\Admin\\Desktop\\Kursinis 2025\\top_100_filtered_mutual_information.xlsx")

###########################################################################################################
###########################################################################################################

#Kintamuju atrinkimas

lasso_vars <- nonzero_names

top_cor_vars <- head(top_100_table_cor$Variable, 15)
top_mi_vars  <- head(top_100_variables$Variable, 15)

all_candidate_vars <- unique(c(top_cor_vars, top_mi_vars))

vars_to_add <- setdiff(all_candidate_vars, lasso_vars)

final_vars <- unique(c(lasso_vars, vars_to_add))
final_vars <- final_vars[final_vars != "(Intercept)"]


write_xlsx(
  data.frame(Selected_Variables = final_vars),
  "C:\\Users\\Admin\\Desktop\\Kursinis 2025\\final_selected_variables.xlsx"
)


# Atrinkome 62 kintamuosius -- final_vars

###########################################################################################################
###########################################################################################################


#------------------------Regression------------------------- 

library(car)


X <- stationary_table[, final_vars]
y <- as.numeric(unlist(stationary_table[, 2]))

regression_data <- data.frame(y = y, X)

null_model <- lm(y ~ 1, data = regression_data)
summary(null_model)
AIC(null_model)


full_model <- lm(y ~ ., data = regression_data)
model_summary<-summary(full_model)
summary(full_model)


step_model <- step(null_model, 
                   scope = list(lower = null_model, upper = full_model), 
                   direction = "both", 
                   trace = TRUE)

# Summary of the final stepwise model
model_summary<-summary(step_model)

selected_vars <- names(coef(step_model))
selected_vars <- selected_vars[selected_vars != "(Intercept)"]
selected_vars

#stepaic atrinko 40 kint


p_values <- model_summary$coefficients[, 4]

important_vars <- names(p_values)[p_values < 0.1]

important_vars
important_vars <- important_vars[important_vars != "(Intercept)"]

#Tokiu radome 33 ---  important_vars

AIC(step_model)

regression_data1 <- data.frame(y = y, X=regression_data[, important_vars])


full_model1 <- lm(y ~ ., data = regression_data1)
model_summary1 <- summary(full_model1)

p_values1 <- model_summary1$coefficients[, 4]

important_vars1 <- names(p_values1)[p_values1 < 0.05]

important_vars1
AIC(full_model1)
vif(full_model1)


# important_vars1 - visi reiksmingi - ---- 23 tokiu
important_vars1 <- gsub("^X\\.", "", important_vars1)
important_vars1 <- important_vars1[important_vars1 != "(Intercept)"]

regression_data2 <- data.frame(y = y, X=regression_data[, important_vars1])
full_model2 <- lm(y ~ ., data = regression_data2)
model_summary2 <- summary(full_model2)
AIC(full_model2)
vif(full_model2)


#Pasalinam viena multikolinearu is dvieju
important_vars2 <- important_vars1[important_vars1 != "Males_Urban.and.rural.areas_15.64_Unemployed"]
regression_data3 <- data.frame(y = y, X=regression_data[, important_vars2])
full_model3 <- lm(y ~ ., data = regression_data3)
model_summary3 <- summary(full_model3)
AIC(full_model3)
vif(full_model3)
p_values3 <- model_summary3$coefficients[, 4]
important_vars3 <- names(p_values3)[p_values3 < 0.05]

important_vars3 <- gsub("^X\\.", "", important_vars3)
important_vars3 <- important_vars3[important_vars3 != "(Intercept)"]

# Gerai tinkantis modelis
regression_data4 <- data.frame(y = y, X=regression_data[, important_vars3])
full_model4 <- lm(y ~ ., data = regression_data4)
model_summary4 <- summary(full_model4)
AIC(full_model4)
vif(full_model4)

x11()
plot(full_model4$fitted.values, resid(full_model4), 
     xlab = "Fitted values", ylab = "Residuals", main = "Residuals vs Fitted")

x11()
hist(resid(full_model4), breaks = 30, main = "Liekanų histograma")

x11()
qqnorm(resid(full_model4))
qqline(resid(full_model4), col = "red")

shapiro.test(resid(full_model4))
# p-value > 0.05 - gerai

library(lmtest)
bptest(full_model4)
# p-value > 0.05 - gerai

dwtest(full_model4)
# p-value > 0.05 - gerai

# important_vars3 - tokiu 21

###########################################################################################################
###########################################################################################################

#-----------------ARIMA---------------------
                                       
library(pbapply)
library(forecast)
library(zoo)


first_two_columns <- stationary_table[, 1:2]
final_columns <- stationary_table[, final_vars]
table_for_forecast_0 <- cbind(first_two_columns, final_columns)
View(table_for_forecast_0)


#---------important_vars3 -- tokiu 21   #############################################################

#----ramybes periodas  2017K2 - 2019K2-----

table_for_forecast<-data.frame(table_for_forecast_0)

columns_to_select <- c(names(table_for_forecast)[1:2], important_vars3)
table_for_forecast <- table_for_forecast[, columns_to_select]
names(table_for_forecast)


train_data_21_2017 <- table_for_forecast[1:71, ]
test_data_21_2017 <- table_for_forecast[72:80, ]

View(train_data_21_2017)
View(test_data_21_2017)


X <- train_data_21_2017[, -c(1, 2)]

X_forecast_21_2017 <- pblapply(1:ncol(X), function(col) {
  eilute <- X[, col]
  
  ts_eilute <- ts(eilute, start = c(1999, 3), freq = 4)
  
  modelis <- auto.arima(ts_eilute)
  
  prognoze <- forecast(modelis, h = nrow(test_data_21_2017))
  
  prognoze$mean
}) %>% do.call(cbind, .)

colnames(X_forecast_21_2017) <- colnames(X)


#############
#############
#----BVP su ARIMA----

bvp_ts <- ts(train_data_21_2017$BVP, start = c(1999, 3), freq = 4)

bvp_modelis <- auto.arima(bvp_ts)
bvp_forecast <- forecast(bvp_modelis, h = nrow(test_data_21_2017))
bvp_forecast_df <- data.frame(Time = test_data_21_2017$Time,
                              BVP_forecast_ARIMA = as.numeric(bvp_forecast$mean))

combined_bvp_arima <- data.frame(
  Time = test_data_21_2017$Time,
  BVP_forecast = as.numeric(bvp_forecast$mean),
  BVP_real = test_data_21_2017$BVP
)
m1 <- ggplot(combined_bvp_arima, aes(x = Time)) +
  geom_line(aes(y = BVP_forecast, color = "Prognozė", group = 1), size = 1.2, linetype = "dashed") +
  geom_line(aes(y = BVP_real, color = "Realybė", group = 2), size = 1.2) +
  geom_point(aes(y = BVP_forecast, color = "Prognozė")) +
  geom_point(aes(y = BVP_real, color = "Realybė")) +
  scale_color_manual(values = c("Prognozė" = "blue", "Realybė" = "red")) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )+
  labs(
    title = "BVP prognozė (ARIMA) ir realūs duomenys: ramybės periodas",
    x = "Kvartalas",
    y = "BVP",
    color = ""
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs6.png", plot = m1, width = 10, height = 6, dpi = 300, bg = "white")


rmse_bvp_arima <- sqrt(mean((combined_bvp_arima$BVP_forecast - combined_bvp_arima$BVP_real)^2, na.rm = TRUE))
print(rmse_bvp_arima)


#----Shokas 2007K1-2009K1----

table_for_forecast <- data.frame(table_for_forecast_0)

columns_to_select <- c(names(table_for_forecast)[1:2], important_vars3)
table_for_forecast <- table_for_forecast[, columns_to_select]
names(table_for_forecast)


train_data_21_2007 <- table_for_forecast[1:30, ]
test_data_21_2007 <- table_for_forecast[31:39, ]


View(train_data_21_2007)
View(test_data_21_2007)

X_train_21_2007 <- train_data_21_2007[, -c(1, 2)]

X_forecast_21_2007 <- pblapply(1:ncol(X_train_21_2007), function(col) {
  eilute <- X_train_21_2007[, col]
  
  ts_eilute <- ts(eilute, start = c(1999, 3), freq = 4)
  
  modelis <- auto.arima(ts_eilute)
  
  prognoze <- forecast(modelis, h = nrow(test_data_21_2007))
  
  prognoze$mean
}) %>% do.call(cbind, .)

colnames(X_forecast_21_2007) <- colnames(X_train_21_2007)


#############
#############
#----BVP su ARIMA----
bvp_ts <- ts(train_data_21_2007$BVP, start = c(1999, 3), freq = 4)

bvp_modelis <- auto.arima(bvp_ts)

bvp_forecast <- forecast(bvp_modelis, h = nrow(test_data_21_2007))

bvp_forecast_df <- data.frame(Time = test_data_21_2007$Time,
                              BVP_forecast_ARIMA = as.numeric(bvp_forecast$mean))

combined_bvp_arima <- data.frame(
  Time = test_data_21_2007$Time,
  BVP_forecast = as.numeric(bvp_forecast$mean),
  BVP_real = test_data_21_2007$BVP
)
m2 <- ggplot(combined_bvp_arima, aes(x = Time)) +
  geom_line(aes(y = BVP_forecast, color = "Prognozė", group = 1), size = 1.2, linetype = "dashed") +
  geom_line(aes(y = BVP_real, color = "Realybė", group = 2), size = 1.2) +
  geom_point(aes(y = BVP_forecast, color = "Prognozė")) +
  geom_point(aes(y = BVP_real, color = "Realybė")) +
  scale_color_manual(values = c("Prognozė" = "blue", "Realybė" = "red")) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )+
  labs(
    title = "BVP prognozė (ARIMA) ir realūs duomenys: 2008 metų krizė",
    x = "Kvartalas",
    y = "BVP",
    color = ""
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs7.png", plot = m2, width = 10, height = 6, dpi = 300, bg = "white")

rmse_bvp_arima <- sqrt(mean((combined_bvp_arima$BVP_forecast - combined_bvp_arima$BVP_real)^2, na.rm = TRUE))
print(rmse_bvp_arima)


#----Shokas 2019K1-2021K1 ------------

table_for_forecast <- data.frame(table_for_forecast_0)

columns_to_select <- c(names(table_for_forecast)[1:2], important_vars3)
table_for_forecast <- table_for_forecast[, columns_to_select]
names(table_for_forecast)


train_data_21_2019 <- table_for_forecast[1:78, ]
test_data_21_2019 <- table_for_forecast[79:90, ]


View(train_data_21_2019)
View(test_data_21_2019)

X_train_21_2019 <- train_data_21_2019[, -c(1, 2)]

X_forecast_21_2019 <- pblapply(1:ncol(X_train_21_2019), function(col) {
  eilute <- X_train_21_2019[, col]
  
  ts_eilute <- ts(eilute, start = c(1999, 3), freq = 4)
  
  modelis <- auto.arima(ts_eilute)
  
  prognoze <- forecast(modelis, h = nrow(test_data_21_2019))
  
  prognoze$mean
}) %>% do.call(cbind, .)

colnames(X_forecast_21_2019) <- colnames(X_train_21_2019)


#############
#############
#-----BVP su ARIMA----
bvp_ts <- ts(train_data_21_2019$BVP, start = c(1999, 3), freq = 4)

bvp_modelis <- auto.arima(bvp_ts)

bvp_forecast <- forecast(bvp_modelis, h = nrow(test_data_21_2019))

bvp_forecast_df <- data.frame(Time = test_data_21_2019$Time,
                              BVP_forecast_ARIMA = as.numeric(bvp_forecast$mean))

combined_bvp_arima <- data.frame(
  Time = test_data_21_2019$Time,
  BVP_forecast = as.numeric(bvp_forecast$mean),
  BVP_real = test_data_21_2019$BVP
)
m3 <- ggplot(combined_bvp_arima, aes(x = Time)) +
  geom_line(aes(y = BVP_forecast, color = "Prognozė", group = 1), size = 1.2, linetype = "dashed") +
  geom_line(aes(y = BVP_real, color = "Realybė", group = 2), size = 1.2) +
  geom_point(aes(y = BVP_forecast, color = "Prognozė")) +
  geom_point(aes(y = BVP_real, color = "Realybė")) +
  scale_color_manual(values = c("Prognozė" = "blue", "Realybė" = "red")) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )+
  labs(
    title = "BVP prognozė (ARIMA) ir realūs duomenys: COVID-19 šokas",
    x = "Kvartalas",
    y = "BVP",
    color = ""
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs8.png", plot = m3, width = 10, height = 6, dpi = 300, bg = "white")

rmse_bvp_arima <- sqrt(mean((combined_bvp_arima$BVP_forecast - combined_bvp_arima$BVP_real)^2, na.rm = TRUE))
print(rmse_bvp_arima)


#######################################################################################
                          #----final_vars -- 62 -----

# ------Ramybes periodas  2017K2 - 2019K2-------

columns_to_select <- c(names(table_for_forecast_0)[1:2], final_vars)
table_for_forecast <- table_for_forecast_0[, columns_to_select]
names(table_for_forecast)


train_data_62_2017 <- table_for_forecast[1:71, ]
test_data_62_2017 <- table_for_forecast[72:80, ]

View(train_data_62_2017)
View(test_data_62_2017)


X <- train_data_62_2017[, -c(1, 2)]

X_forecast_62_2017 <- pblapply(1:ncol(X), function(col) {
  eilute <- X[, col]
  
  ts_eilute <- ts(eilute, start = c(1999, 3), freq = 4)
  
  modelis <- auto.arima(ts_eilute)
  
  prognoze <- forecast(modelis, h = nrow(test_data_62_2017))
  
  prognoze$mean
}) %>% do.call(cbind, .)

colnames(X_forecast_62_2017) <- colnames(X)


#----shokas 2007K1-2009K1 -------

columns_to_select <- c(names(table_for_forecast)[1:2], final_vars)
table_for_forecast <- table_for_forecast[, columns_to_select]
names(table_for_forecast)


train_data_62_2007 <- table_for_forecast[1:30, ]
test_data_62_2007 <- table_for_forecast[31:39, ]


View(train_data_62_2007)
View(test_data_62_2007)

X_train_62_2007 <- train_data_62_2007[, -c(1, 2)]

X_forecast_62_2007 <- pblapply(1:ncol(X_train_62_2007), function(col) {
  eilute <- X_train_62_2007[, col]
  
  ts_eilute <- ts(eilute, start = c(1999, 3), freq = 4)
  
  modelis <- auto.arima(ts_eilute)
  
  prognoze <- forecast(modelis, h = nrow(test_data_62_2007))
  
  prognoze$mean
}) %>% do.call(cbind, .)

colnames(X_forecast_62_2007) <- colnames(X_train_62_2007)


#----Shokas 2019K1-2021K1 ------

columns_to_select <- c(names(table_for_forecast)[1:2], final_vars)
table_for_forecast <- table_for_forecast[, columns_to_select]
names(table_for_forecast)


train_data_62_2019 <- table_for_forecast[1:78, ]
test_data_62_2019 <- table_for_forecast[79:90, ]


View(train_data_62_2019)
View(test_data_62_2019)

X_train_62_2019 <- train_data_62_2019[, -c(1, 2)]

X_forecast_62_2019 <- pblapply(1:ncol(X_train_62_2019), function(col) {
  eilute <- X_train_62_2019[, col]
  
  ts_eilute <- ts(eilute, start = c(1999, 3), freq = 4)
  
  modelis <- auto.arima(ts_eilute)
  
  prognoze <- forecast(modelis, h = nrow(test_data_62_2019))
  
  prognoze$mean
}) %>% do.call(cbind, .)

colnames(X_forecast_62_2019) <- colnames(X_train_62_2019)



#########################################################################################################################
#########################################################################################################################

#----------------------Tiesine regresija----------------------------

#---------ramybes periodas 2017---------

#Apmokome modeli
X <- train_data_21_2017
y <- as.numeric(train_data_21_2017$BVP)

train_x1 <- data.frame(y = y, X)

model_lm <- lm(y ~ ., data = train_x1[, important_vars3])
model_summary<-summary(model_lm)

#Paimam dataseta gauta su ARIMA
X_test <- data.frame(X_forecast_21_2017)

forecast_bvp <- predict(model_lm, newdata = X_test[, important_vars3])
forecast_bvp_df <- data.frame(Time = test_data_21_2017$Time, BVP_forecast = forecast_bvp)

combined_data_bvp <- merge(forecast_bvp_df, test_data_21_2017[, c("Time", "BVP")], by = "Time")



m4 <- ggplot(combined_data_bvp, aes(x = Time)) +
  geom_line(aes(y = BVP_forecast, color = "Prognozė", group = 1), size = 1.2, linetype = "dashed") +
  geom_line(aes(y = BVP, color = "Realybė", group = 2), size = 1.2) +
  geom_point(aes(y = BVP_forecast, color = "Prognozė")) +
  geom_point(aes(y = BVP, color = "Realybė")) +
  scale_color_manual(values = c("Prognozė" = "blue", "Realybė" = "red")) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )+
  labs(
    title = "BVP prognozė (Tiesinė regresija) ir realūs duomenys: ramybės periodas",
    x = "Kvartalas",
    y = "BVP",
    color = ""
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs9.png", plot = m4, width = 10, height = 6, dpi = 300, bg = "white")



#RMSE
rmse_bvp <- sqrt(mean((forecast_bvp - test_data_21_2017$BVP)^2))
# 0.004762875



#-------Shoko periodas 2008--------------

#Apmokome modeli
X <- train_data_21_2007
y <- as.numeric(train_data_21_2007$BVP)

train_x1 <- data.frame(y = y, X)

model_lm <- lm(y ~ ., data = train_x1[, important_vars3])
model_summary3<-summary(model_lm)
#vif(model_lm)

#Paimam dataseta gauta su ARIMA
X_test <- data.frame(X_forecast_21_2007)

forecast_bvp <- predict(model_lm, newdata = X_test[, important_vars3])
forecast_bvp_df <- data.frame(Time = test_data_21_2007$Time, BVP_forecast = forecast_bvp)

combined_data_bvp <- merge(forecast_bvp_df, test_data_21_2007[, c("Time", "BVP")], by = "Time")
View(combined_data_bvp)

m5 <- ggplot(combined_data_bvp, aes(x = Time)) +
  geom_line(aes(y = BVP_forecast, color = "Prognozė", group = 1), size = 1.2, linetype = "dashed") +
  geom_line(aes(y = BVP, color = "Realybė", group = 2), size = 1.2) +
  geom_point(aes(y = BVP_forecast, color = "Prognozė")) +
  geom_point(aes(y = BVP, color = "Realybė")) +
  scale_color_manual(values = c("Prognozė" = "blue", "Realybė" = "red")) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )+
  labs(
    title = "BVP prognozė (Tiesinė regresija) ir realūs duomenys: 2008 krizė",
    x = "Kvartalas",
    y = "BVP",
    color = ""
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs10.png", plot = m5, width = 10, height = 6, dpi = 300, bg = "white")



#RMSE
rmse_bvp <- sqrt(mean((forecast_bvp - test_data_21_2007$BVP)^2))
# 0.05016105


#----------------Shoko periodas 2019-------------

#Apmokome modeli
X <- train_data_21_2019
y <- as.numeric(train_data_21_2019$BVP)

train_x1 <- data.frame(y = y, X)

model_lm <- lm(y ~ ., data = train_x1[, important_vars3])
summary(model_lm)
#vif(model_lm)

#Paimam dataseta gauta su ARIMA
X_test <- data.frame(X_forecast_21_2019)

forecast_bvp <- predict(model_lm, newdata = X_test[, important_vars3])
forecast_bvp_df <- data.frame(Time = test_data_21_2019$Time, BVP_forecast = forecast_bvp)

combined_data_bvp <- merge(forecast_bvp_df, test_data_21_2019[, c("Time", "BVP")], by = "Time")

m6 <- ggplot(combined_data_bvp, aes(x = Time)) +
  geom_line(aes(y = BVP_forecast, color = "Prognozė", group = 1), size = 1.2, linetype = "dashed") +
  geom_line(aes(y = BVP, color = "Realybė", group = 2), size = 1.2) +
  geom_point(aes(y = BVP_forecast, color = "Prognozė")) +
  geom_point(aes(y = BVP, color = "Realybė")) +
  scale_color_manual(values = c("Prognozė" = "blue", "Realybė" = "red")) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )+
  labs(
    title = "BVP prognozė (Tiesinė regresija) ir realūs duomenys: COVID-19 šokas",
    x = "Kvartalas",
    y = "BVP",
    color = ""
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs11.png", plot = m6, width = 10, height = 6, dpi = 300, bg = "white")



#RMSE
rmse_bvp <- sqrt(mean((forecast_bvp - test_data_21_2019$BVP)^2))
#0.02504528



#########################################################################################################################
#########################################################################################################################

#----Bandymas su LASSO------


####################----ramybes periodas 2017---#################

###------62 kint----- 

X_matrix <- as.matrix(train_data_62_2017[, final_vars])
y_vector <- as.numeric(train_data_62_2017$BVP)

# SU alpha = 1
set.seed(123)

cv_lasso <- cv.glmnet(X_matrix, y_vector, alpha = 1)

plot(cv_lasso)


best_lambda <- cv_lasso$lambda.1se

model_lasso <- glmnet(X_matrix, y_vector, alpha = 1, lambda = best_lambda)


X_test_matrix <- as.matrix(X_forecast_62_2017[, final_vars])
forecast_bvp_lasso <- predict(model_lasso, newx = X_test_matrix)

forecast_bvp_df_lasso <- data.frame(
  Time = test_data_62_2017$Time,
  BVP_forecast = as.numeric(forecast_bvp_lasso)
)

combined_data_lasso <- merge(forecast_bvp_df_lasso, test_data_62_2017[, c("Time", "BVP")], by = "Time")

ggplot(combined_data_lasso, aes(x = Time)) +
  geom_line(aes(y = BVP_forecast, color = "Prognozė (LASSO)", group = 1), size = 1.2, linetype = "dashed") +
  geom_line(aes(y = BVP, color = "Realybė", group = 2), size = 1.2) +
  geom_point(aes(y = BVP_forecast, color = "Prognozė (LASSO)")) +
  geom_point(aes(y = BVP, color = "Realybė")) +
  scale_color_manual(values = c("Prognozė (LASSO)" = "blue", "Realybė" = "red")) +
  theme_minimal() +
  labs(title = "BVP prognozė su LASSO ir realybė",
       x = "Kvartalas",
       y = "BVP",
       color = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ylim(0, 0.1)

rmse_lasso <- sqrt(mean((forecast_bvp_lasso - test_data_62_2017$BVP)^2))
# 0.005455197
# 0.005126316su 1se

#--------------RIDGE--------------------
# SU alpha = 0
set.seed(123)

cv_lasso <- cv.glmnet(X_matrix, y_vector, alpha = 0)

plot(cv_lasso)


best_lambda <- cv_lasso$lambda.1se

model_lasso <- glmnet(X_matrix, y_vector, alpha = 0, lambda = best_lambda)


X_test_matrix <- as.matrix(X_forecast_62_2017[, final_vars])
forecast_bvp_lasso <- predict(model_lasso, newx = X_test_matrix)

forecast_bvp_df_lasso <- data.frame(
  Time = test_data_62_2017$Time,
  BVP_forecast = as.numeric(forecast_bvp_lasso)
)

combined_data_lasso <- merge(forecast_bvp_df_lasso, test_data_62_2017[, c("Time", "BVP")], by = "Time")


m7 <- ggplot(combined_data_lasso, aes(x = Time)) +
  geom_line(aes(y = BVP_forecast, color = "Prognozė", group = 1), size = 1.2, linetype = "dashed") +
  geom_line(aes(y = BVP, color = "Realybė", group = 2), size = 1.2) +
  geom_point(aes(y = BVP_forecast, color = "Prognozė")) +
  geom_point(aes(y = BVP, color = "Realybė")) +
  scale_color_manual(values = c("Prognozė" = "blue", "Realybė" = "red")) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )+
  labs(
    title = "BVP prognozė su Ridge ir realybė",
    x = "Kvartalas",
    y = "BVP",
    color = ""
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs12.png", plot = m7, width = 10, height = 6, dpi = 300, bg = "white")


rmse_lasso <- sqrt(mean((forecast_bvp_lasso - test_data_62_2017$BVP)^2))
# 0.005124707
# 0.00512493 su 1se

#--------------ELASIC NET--------------------
# SU alpha = 0.5
set.seed(123)

cv_lasso <- cv.glmnet(X_matrix, y_vector, alpha = 0.5)

plot(cv_lasso)


best_lambda <- cv_lasso$lambda.1se

model_lasso <- glmnet(X_matrix, y_vector, alpha = 0.5, lambda = best_lambda)


X_test_matrix <- as.matrix(X_forecast_62_2017[, final_vars])
forecast_bvp_lasso <- predict(model_lasso, newx = X_test_matrix)

forecast_bvp_df_lasso <- data.frame(
  Time = test_data_62_2017$Time,
  BVP_forecast = as.numeric(forecast_bvp_lasso)
)

combined_data_lasso <- merge(forecast_bvp_df_lasso, test_data_62_2017[, c("Time", "BVP")], by = "Time")

ggplot(combined_data_lasso, aes(x = Time)) +
  geom_line(aes(y = BVP_forecast, color = "Prognozė (LASSO)", group = 1), size = 1.2, linetype = "dashed") +
  geom_line(aes(y = BVP, color = "Realybė", group = 2), size = 1.2) +
  geom_point(aes(y = BVP_forecast, color = "Prognozė (LASSO)")) +
  geom_point(aes(y = BVP, color = "Realybė")) +
  scale_color_manual(values = c("Prognozė (LASSO)" = "blue", "Realybė" = "red")) +
  theme_minimal() +
  labs(title = "BVP prognozė su LASSO ir realybė",
       x = "Kvartalas",
       y = "BVP",
       color = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ylim(0,1)

rmse_lasso <- sqrt(mean((forecast_bvp_lasso - test_data_62_2017$BVP)^2))
# 0.005455371
# 0.005126316 su 1se


# SU alpha = 0.1
set.seed(123)

cv_lasso <- cv.glmnet(X_matrix, y_vector, alpha = 0.1)

plot(cv_lasso)


best_lambda <- cv_lasso$lambda.min

model_lasso <- glmnet(X_matrix, y_vector, alpha = 0.1, lambda = best_lambda)


X_test_matrix <- as.matrix(X_forecast_62_2017[, final_vars])
forecast_bvp_lasso <- predict(model_lasso, newx = X_test_matrix)

forecast_bvp_df_lasso <- data.frame(
  Time = test_data_62_2017$Time,
  BVP_forecast = as.numeric(forecast_bvp_lasso)
)

combined_data_lasso <- merge(forecast_bvp_df_lasso, test_data_62_2017[, c("Time", "BVP")], by = "Time")

ggplot(combined_data_lasso, aes(x = Time)) +
  geom_line(aes(y = BVP_forecast, color = "Prognozė (LASSO)", group = 1), size = 1.2, linetype = "dashed") +
  geom_line(aes(y = BVP, color = "Realybė", group = 2), size = 1.2) +
  geom_point(aes(y = BVP_forecast, color = "Prognozė (LASSO)")) +
  geom_point(aes(y = BVP, color = "Realybė")) +
  scale_color_manual(values = c("Prognozė (LASSO)" = "blue", "Realybė" = "red")) +
  theme_minimal() +
  labs(title = "BVP prognozė su LASSO ir realybė",
       x = "Kvartalas",
       y = "BVP",
       color = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ylim(0,1)

rmse_lasso <- sqrt(mean((forecast_bvp_lasso - test_data_62_2017$BVP)^2))
# 0.02893639
# 0.0051

# SU alpha = 0.9
set.seed(123)

cv_lasso <- cv.glmnet(X_matrix, y_vector, alpha = 0.9)

plot(cv_lasso)


best_lambda <- cv_lasso$lambda.1se

model_lasso <- glmnet(X_matrix, y_vector, alpha = 0.9, lambda = best_lambda)


X_test_matrix <- as.matrix(X_forecast_62_2017[, final_vars])
forecast_bvp_lasso <- predict(model_lasso, newx = X_test_matrix)

forecast_bvp_df_lasso <- data.frame(
  Time = test_data_62_2017$Time,
  BVP_forecast = as.numeric(forecast_bvp_lasso)
)

combined_data_lasso <- merge(forecast_bvp_df_lasso, test_data_62_2017[, c("Time", "BVP")], by = "Time")

ggplot(combined_data_lasso, aes(x = Time)) +
  geom_line(aes(y = BVP_forecast, color = "Prognozė (LASSO)", group = 1), size = 1.2, linetype = "dashed") +
  geom_line(aes(y = BVP, color = "Realybė", group = 2), size = 1.2) +
  geom_point(aes(y = BVP_forecast, color = "Prognozė (LASSO)")) +
  geom_point(aes(y = BVP, color = "Realybė")) +
  scale_color_manual(values = c("Prognozė (LASSO)" = "blue", "Realybė" = "red")) +
  theme_minimal() +
  labs(title = "BVP prognozė su LASSO ir realybė",
       x = "Kvartalas",
       y = "BVP",
       color = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ylim(0,1)

rmse_lasso <- sqrt(mean((forecast_bvp_lasso - test_data_62_2017$BVP)^2))
# 0.02901734
# 0.005126316



####################----shoko periodas  2008 ---#################

### 62 kint ###

X_matrix <- as.matrix(train_data_62_2007[, final_vars])
y_vector <- as.numeric(train_data_62_2007$BVP)


# SU alpha = 1
set.seed(123)

cv_lasso <- cv.glmnet(X_matrix, y_vector, alpha = 1)

plot(cv_lasso)


best_lambda <- cv_lasso$lambda.1se

model_lasso <- glmnet(X_matrix, y_vector, alpha = 1, lambda = best_lambda)


X_test_matrix <- as.matrix(X_forecast_62_2007[, final_vars])
forecast_bvp_lasso <- predict(model_lasso, newx = X_test_matrix)

forecast_bvp_df_lasso <- data.frame(
  Time = test_data_62_2007$Time,
  BVP_forecast = as.numeric(forecast_bvp_lasso)
)

combined_data_lasso <- merge(forecast_bvp_df_lasso, test_data_62_2007[, c("Time", "BVP")], by = "Time")


m8 <- ggplot(combined_data_lasso, aes(x = Time)) +
  geom_line(aes(y = BVP_forecast, color = "Prognozė", group = 1), size = 1.2, linetype = "dashed") +
  geom_line(aes(y = BVP, color = "Realybė", group = 2), size = 1.2) +
  geom_point(aes(y = BVP_forecast, color = "Prognozė")) +
  geom_point(aes(y = BVP, color = "Realybė")) +
  scale_color_manual(values = c("Prognozė" = "blue", "Realybė" = "red")) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )+
  labs(
    title = "BVP prognozė su LASSO ir realybė",
    x = "Kvartalas",
    y = "BVP",
    color = ""
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs13.png", plot = m8, width = 10, height = 6, dpi = 300, bg = "white")





rmse_lasso <- sqrt(mean((forecast_bvp_lasso - test_data_62_2007$BVP)^2))
# 0.05398879
# 0.05185871 su 1se

#--------------RIDGE--------------------
# SU alpha = 0
set.seed(123)

cv_lasso <- cv.glmnet(X_matrix, y_vector, alpha = 0)

plot(cv_lasso)


best_lambda <- cv_lasso$lambda.min

model_lasso <- glmnet(X_matrix, y_vector, alpha = 0, lambda = best_lambda)


X_test_matrix <- as.matrix(X_forecast_62_2007[, final_vars])
forecast_bvp_lasso <- predict(model_lasso, newx = X_test_matrix)

forecast_bvp_df_lasso <- data.frame(
  Time = test_data_62_2007$Time,
  BVP_forecast = as.numeric(forecast_bvp_lasso)
)

combined_data_lasso <- merge(forecast_bvp_df_lasso, test_data_62_2007[, c("Time", "BVP")], by = "Time")

ggplot(combined_data_lasso, aes(x = Time)) +
  geom_line(aes(y = BVP_forecast, color = "Prognozė (LASSO)", group = 1), size = 1.2, linetype = "dashed") +
  geom_line(aes(y = BVP, color = "Realybė", group = 2), size = 1.2) +
  geom_point(aes(y = BVP_forecast, color = "Prognozė (LASSO)")) +
  geom_point(aes(y = BVP, color = "Realybė")) +
  scale_color_manual(values = c("Prognozė (LASSO)" = "blue", "Realybė" = "red")) +
  theme_minimal() +
  labs(title = "BVP prognozė su LASSO ir realybė",
       x = "Kvartalas",
       y = "BVP",
       color = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ylim(0,1)

rmse_lasso <- sqrt(mean((forecast_bvp_lasso - test_data_62_2007$BVP)^2))
# 0.05316668
# 0.05188459 su 1se

#--------------ELASIC NET--------------------
# SU alpha = 0.5
set.seed(123)

cv_lasso <- cv.glmnet(X_matrix, y_vector, alpha = 0.5)

plot(cv_lasso)


best_lambda <- cv_lasso$lambda.min

model_lasso <- glmnet(X_matrix, y_vector, alpha = 0.5, lambda = best_lambda)


X_test_matrix <- as.matrix(X_forecast_62_2007[, final_vars])
forecast_bvp_lasso <- predict(model_lasso, newx = X_test_matrix)

forecast_bvp_df_lasso <- data.frame(
  Time = test_data_62_2007$Time,
  BVP_forecast = as.numeric(forecast_bvp_lasso)
)

combined_data_lasso <- merge(forecast_bvp_df_lasso, test_data_62_2007[, c("Time", "BVP")], by = "Time")

ggplot(combined_data_lasso, aes(x = Time)) +
  geom_line(aes(y = BVP_forecast, color = "Prognozė (LASSO)", group = 1), size = 1.2, linetype = "dashed") +
  geom_line(aes(y = BVP, color = "Realybė", group = 2), size = 1.2) +
  geom_point(aes(y = BVP_forecast, color = "Prognozė (LASSO)")) +
  geom_point(aes(y = BVP, color = "Realybė")) +
  scale_color_manual(values = c("Prognozė (LASSO)" = "blue", "Realybė" = "red")) +
  theme_minimal() +
  labs(title = "BVP prognozė su LASSO ir realybė",
       x = "Kvartalas",
       y = "BVP",
       color = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ylim(0,1)

rmse_lasso <- sqrt(mean((forecast_bvp_lasso - test_data_62_2007$BVP)^2))
# 0.0539008
# 0.05185871  1se


# SU alpha = 0.1
set.seed(123)

cv_lasso <- cv.glmnet(X_matrix, y_vector, alpha = 0.1)

plot(cv_lasso)


best_lambda <- cv_lasso$lambda.min

model_lasso <- glmnet(X_matrix, y_vector, alpha = 0.1, lambda = best_lambda)


X_test_matrix <- as.matrix(X_forecast_62_2007[, final_vars])
forecast_bvp_lasso <- predict(model_lasso, newx = X_test_matrix)

forecast_bvp_df_lasso <- data.frame(
  Time = test_data_62_2007$Time,
  BVP_forecast = as.numeric(forecast_bvp_lasso)
)

combined_data_lasso <- merge(forecast_bvp_df_lasso, test_data_62_2007[, c("Time", "BVP")], by = "Time")

ggplot(combined_data_lasso, aes(x = Time)) +
  geom_line(aes(y = BVP_forecast, color = "Prognozė (LASSO)", group = 1), size = 1.2, linetype = "dashed") +
  geom_line(aes(y = BVP, color = "Realybė", group = 2), size = 1.2) +
  geom_point(aes(y = BVP_forecast, color = "Prognozė (LASSO)")) +
  geom_point(aes(y = BVP, color = "Realybė")) +
  scale_color_manual(values = c("Prognozė (LASSO)" = "blue", "Realybė" = "red")) +
  theme_minimal() +
  labs(title = "BVP prognozė su LASSO ir realybė",
       x = "Kvartalas",
       y = "BVP",
       color = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ylim(0,1)

rmse_lasso <- sqrt(mean((forecast_bvp_lasso - test_data_62_2007$BVP)^2))
# 0.05383091
# 0.05185871 su 1se

# SU alpha = 0.9
set.seed(123)

cv_lasso <- cv.glmnet(X_matrix, y_vector, alpha = 0.9)

plot(cv_lasso)


best_lambda <- cv_lasso$lambda.min

model_lasso <- glmnet(X_matrix, y_vector, alpha = 0.9, lambda = best_lambda)


X_test_matrix <- as.matrix(X_forecast_62_2007[, final_vars])
forecast_bvp_lasso <- predict(model_lasso, newx = X_test_matrix)

forecast_bvp_df_lasso <- data.frame(
  Time = test_data_62_2007$Time,
  BVP_forecast = as.numeric(forecast_bvp_lasso)
)

combined_data_lasso <- merge(forecast_bvp_df_lasso, test_data_62_2007[, c("Time", "BVP")], by = "Time")

ggplot(combined_data_lasso, aes(x = Time)) +
  geom_line(aes(y = BVP_forecast, color = "Prognozė (LASSO)", group = 1), size = 1.2, linetype = "dashed") +
  geom_line(aes(y = BVP, color = "Realybė", group = 2), size = 1.2) +
  geom_point(aes(y = BVP_forecast, color = "Prognozė (LASSO)")) +
  geom_point(aes(y = BVP, color = "Realybė")) +
  scale_color_manual(values = c("Prognozė (LASSO)" = "blue", "Realybė" = "red")) +
  theme_minimal() +
  labs(title = "BVP prognozė su LASSO ir realybė",
       x = "Kvartalas",
       y = "BVP",
       color = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

rmse_lasso <- sqrt(mean((forecast_bvp_lasso - test_data_62_2007$BVP)^2))
# 0.05406769
# 0.05185871 su 1se



####################----shoko periodas 2019---#################

### 62 kint ###

X_matrix <- as.matrix(train_data_62_2019[,-c(1,2)])
y_vector <- as.numeric(train_data_62_2019$BVP)

View(X_matrix)


# SU alpha = 1
set.seed(123)

cv_lasso <- cv.glmnet(X_matrix, y_vector, alpha = 1)

plot(cv_lasso)


best_lambda <- cv_lasso$lambda.min
model_lasso <- glmnet(X_matrix, y_vector, alpha = 1, lambda = best_lambda)


X_test_matrix <- as.matrix(X_forecast_62_2019)
forecast_bvp_lasso <- predict(model_lasso, newx = X_test_matrix)

forecast_bvp_df_lasso <- data.frame(
  Time = test_data_62_2019$Time,
  BVP_forecast = as.numeric(forecast_bvp_lasso)
)

combined_data_lasso <- merge(forecast_bvp_df_lasso, test_data_62_2019[, c("Time", "BVP")], by = "Time")


m9 <- ggplot(combined_data_lasso, aes(x = Time)) +
  geom_line(aes(y = BVP_forecast, color = "Prognozė", group = 1), size = 1.2, linetype = "dashed") +
  geom_line(aes(y = BVP, color = "Realybė", group = 2), size = 1.2) +
  geom_point(aes(y = BVP_forecast, color = "Prognozė")) +
  geom_point(aes(y = BVP, color = "Realybė")) +
  scale_color_manual(values = c("Prognozė" = "blue", "Realybė" = "red")) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )+
  labs(
    title = "BVP prognozė su LASSO ir realybė",
    x = "Kvartalas",
    y = "BVP",
    color = ""
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs14.png", plot = m9, width = 10, height = 6, dpi = 300, bg = "white")


rmse_lasso <- sqrt(mean((forecast_bvp_lasso - test_data_62_2019$BVP)^2))
# 0.02337607
# 0.02365128 su 1se

#--------------RIDGE--------------------
# SU alpha = 0
set.seed(123)

cv_lasso <- cv.glmnet(X_matrix, y_vector, alpha = 0)

plot(cv_lasso)


best_lambda <- cv_lasso$lambda.min

model_lasso <- glmnet(X_matrix, y_vector, alpha = 0, lambda = best_lambda)


X_test_matrix <- as.matrix(X_forecast_62_2019)
forecast_bvp_lasso <- predict(model_lasso, newx = X_test_matrix)

forecast_bvp_df_lasso <- data.frame(
  Time = test_data_62_2019$Time,
  BVP_forecast = as.numeric(forecast_bvp_lasso)
)

combined_data_lasso <- merge(forecast_bvp_df_lasso, test_data_62_2019[, c("Time", "BVP")], by = "Time")

ggplot(combined_data_lasso, aes(x = Time)) +
  geom_line(aes(y = BVP_forecast, color = "Prognozė (LASSO)", group = 1), size = 1.2, linetype = "dashed") +
  geom_line(aes(y = BVP, color = "Realybė", group = 2), size = 1.2) +
  geom_point(aes(y = BVP_forecast, color = "Prognozė (LASSO)")) +
  geom_point(aes(y = BVP, color = "Realybė")) +
  scale_color_manual(values = c("Prognozė (LASSO)" = "blue", "Realybė" = "red")) +
  theme_minimal() +
  labs(title = "BVP prognozė su LASSO ir realybė",
       x = "Kvartalas",
       y = "BVP",
       color = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ylim(0,1)

rmse_lasso <- sqrt(mean((forecast_bvp_lasso - test_data_62_2019$BVP)^2))
# 0.02338122
# 0.02376572 su 1se

#--------------ELASIC NET--------------------
# SU alpha = 0.5
set.seed(123)

cv_lasso <- cv.glmnet(X_matrix, y_vector, alpha = 0.5)

plot(cv_lasso)


best_lambda <- cv_lasso$lambda.min

model_lasso <- glmnet(X_matrix, y_vector, alpha = 0.5, lambda = best_lambda)


X_test_matrix <- as.matrix(X_forecast_62_2019)
forecast_bvp_lasso <- predict(model_lasso, newx = X_test_matrix)

forecast_bvp_df_lasso <- data.frame(
  Time = test_data_62_2019$Time,
  BVP_forecast = as.numeric(forecast_bvp_lasso)
)

combined_data_lasso <- merge(forecast_bvp_df_lasso, test_data_62_2019[, c("Time", "BVP")], by = "Time")

ggplot(combined_data_lasso, aes(x = Time)) +
  geom_line(aes(y = BVP_forecast, color = "Prognozė (LASSO)", group = 1), size = 1.2, linetype = "dashed") +
  geom_line(aes(y = BVP, color = "Realybė", group = 2), size = 1.2) +
  geom_point(aes(y = BVP_forecast, color = "Prognozė (LASSO)")) +
  geom_point(aes(y = BVP, color = "Realybė")) +
  scale_color_manual(values = c("Prognozė (LASSO)" = "blue", "Realybė" = "red")) +
  theme_minimal() +
  labs(title = "BVP prognozė su LASSO ir realybė",
       x = "Kvartalas",
       y = "BVP",
       color = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ylim(0,1)

rmse_lasso <- sqrt(mean((forecast_bvp_lasso - test_data_62_2019$BVP)^2))
# 0.02351547
# 0.02365591 su 1se


# SU alpha = 0.1
set.seed(123)

cv_lasso <- cv.glmnet(X_matrix, y_vector, alpha = 0.1)

plot(cv_lasso)


best_lambda <- cv_lasso$lambda.min

model_lasso <- glmnet(X_matrix, y_vector, alpha = 0.1, lambda = best_lambda)


X_test_matrix <- as.matrix(X_forecast_62_2019)
forecast_bvp_lasso <- predict(model_lasso, newx = X_test_matrix)

forecast_bvp_df_lasso <- data.frame(
  Time = test_data_62_2019$Time,
  BVP_forecast = as.numeric(forecast_bvp_lasso)
)

combined_data_lasso <- merge(forecast_bvp_df_lasso, test_data_62_2019[, c("Time", "BVP")], by = "Time")

ggplot(combined_data_lasso, aes(x = Time)) +
  geom_line(aes(y = BVP_forecast, color = "Prognozė (LASSO)", group = 1), size = 1.2, linetype = "dashed") +
  geom_line(aes(y = BVP, color = "Realybė", group = 2), size = 1.2) +
  geom_point(aes(y = BVP_forecast, color = "Prognozė (LASSO)")) +
  geom_point(aes(y = BVP, color = "Realybė")) +
  scale_color_manual(values = c("Prognozė (LASSO)" = "blue", "Realybė" = "red")) +
  theme_minimal() +
  labs(title = "BVP prognozė su LASSO ir realybė",
       x = "Kvartalas",
       y = "BVP",
       color = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ylim(0,1)

rmse_lasso <- sqrt(mean((forecast_bvp_lasso - test_data_62_2019$BVP)^2))
# 0.02333845
# 0.02371145  su 1se



# SU alpha = 0.9
set.seed(123)

cv_lasso <- cv.glmnet(X_matrix, y_vector, alpha = 0.9)

plot(cv_lasso)


best_lambda <- cv_lasso$lambda.min

model_lasso <- glmnet(X_matrix, y_vector, alpha = 0.9, lambda = best_lambda)


X_test_matrix <- as.matrix(X_forecast_62_2019)
forecast_bvp_lasso <- predict(model_lasso, newx = X_test_matrix)

forecast_bvp_df_lasso <- data.frame(
  Time = test_data_62_2019$Time,
  BVP_forecast = as.numeric(forecast_bvp_lasso)
)

combined_data_lasso <- merge(forecast_bvp_df_lasso, test_data_62_2019[, c("Time", "BVP")], by = "Time")

ggplot(combined_data_lasso, aes(x = Time)) +
  geom_line(aes(y = BVP_forecast, color = "Prognozė (LASSO)", group = 1), size = 1.2, linetype = "dashed") +
  geom_line(aes(y = BVP, color = "Realybė", group = 2), size = 1.2) +
  geom_point(aes(y = BVP_forecast, color = "Prognozė (LASSO)")) +
  geom_point(aes(y = BVP, color = "Realybė")) +
  scale_color_manual(values = c("Prognozė (LASSO)" = "blue", "Realybė" = "red")) +
  theme_minimal() +
  labs(title = "BVP prognozė su LASSO ir realybė",
       x = "Kvartalas",
       y = "BVP",
       color = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

rmse_lasso <- sqrt(mean((forecast_bvp_lasso - test_data_62_2019$BVP)^2))
# 0.02337547
# 0.02365051 su 1 se



####################################################################################################################################
####################################################################################################################################
#------------ADAPTIVE LASSO------------------------------------------


#-----------2017-------------

X_matrix <- as.matrix(train_data_62_2017[, final_vars])
y_vector <- as.numeric(train_data_62_2017$BVP)
X_test_matrix <- as.matrix(X_forecast_62_2017[, final_vars])


library(glmnet)
set.seed(123)
ridge_cv <- cv.glmnet(x = X_matrix, y = y_vector, alpha = 0, type.measure = "mse")
best_ridge_coef <- as.numeric(coef(ridge_cv$glmnet.fit, s = ridge_cv$lambda.min))[-1]

epsilon <- 1e-4
penalty_factors <- 1 / (abs(best_ridge_coef) + epsilon)

alasso_cv <- cv.glmnet(x = X_matrix, y = y_vector,
                       alpha = 1,                  # LASSO
                       type.measure = "mse",
                       nfolds = 10,
                       penalty.factor = penalty_factors,
                       keep = TRUE)

plot(alasso_cv)

lambda_min <- alasso_cv$lambda.1se
best_alasso_coef <- coef(alasso_cv, s = lambda_min)

forecast_bvp <- predict(alasso_cv, newx = X_test_matrix, s = lambda_min)

forecast_df <- data.frame(
  Time = test_data_62_2017$Time,
  BVP_forecast = as.numeric(forecast_bvp),
  BVP = test_data_62_2017$BVP
)


m10 <- ggplot(forecast_df, aes(x = Time)) +
  geom_line(aes(y = BVP_forecast, color = "Prognozė", group = 1), size = 1.2, linetype = "dashed") +
  geom_line(aes(y = BVP, color = "Realybė", group = 2), size = 1.2) +
  geom_point(aes(y = BVP_forecast, color = "Prognozė")) +
  geom_point(aes(y = BVP, color = "Realybė")) +
  scale_color_manual(values = c("Prognozė" = "blue", "Realybė" = "red")) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )+
  labs(
    title = "BVP prognozė su Adaptive LASSO ir realybė",
    x = "Kvartalas",
    y = "BVP",
    color = ""
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs15.png", plot = m10, width = 10, height = 6, dpi = 300, bg = "white")


rmse_adaptive <- sqrt(mean((forecast_df$BVP_forecast - forecast_df$BVP)^2))
print(rmse_adaptive)
#0.005900389
#0.00542151 1 se


#-----------2008-------------

X_matrix <- as.matrix(train_data_62_2007[, final_vars])
y_vector <- as.numeric(train_data_62_2007$BVP)
X_test_matrix <- as.matrix(X_forecast_62_2007[, final_vars])


library(glmnet)
set.seed(123)
ridge_cv <- cv.glmnet(x = X_matrix, y = y_vector, alpha = 0, type.measure = "mse")
best_ridge_coef <- as.numeric(coef(ridge_cv$glmnet.fit, s = ridge_cv$lambda.min))[-1]

epsilon <- 1e-4
penalty_factors <- 1 / (abs(best_ridge_coef) + epsilon)

alasso_cv <- cv.glmnet(x = X_matrix, y = y_vector,
                       alpha = 1,                  # LASSO
                       type.measure = "mse",
                       nfolds = 10,
                       penalty.factor = penalty_factors,
                       keep = TRUE)

plot(alasso_cv)

lambda_min <- alasso_cv$lambda.1se
best_alasso_coef <- coef(alasso_cv, s = lambda_min)

forecast_bvp <- predict(alasso_cv, newx = X_test_matrix, s = lambda_min)

forecast_df <- data.frame(
  Time = test_data_62_2007$Time,
  BVP_forecast = as.numeric(forecast_bvp),
  BVP = test_data_62_2007$BVP
)
m11 <- ggplot(forecast_df, aes(x = Time)) +
  geom_line(aes(y = BVP_forecast, color = "Prognozė", group = 1), size = 1.2, linetype = "dashed") +
  geom_line(aes(y = BVP, color = "Realybė", group = 2), size = 1.2) +
  geom_point(aes(y = BVP_forecast, color = "Prognozė")) +
  geom_point(aes(y = BVP, color = "Realybė")) +
  scale_color_manual(values = c("Prognozė" = "blue", "Realybė" = "red")) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )+
  labs(
    title = "BVP prognozė su Adaptive LASSO ir realybė",
    x = "Kvartalas",
    y = "BVP",
    color = ""
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs16.png", plot = m11, width = 10, height = 6, dpi = 300, bg = "white")


rmse_adaptive <- sqrt(mean((forecast_df$BVP_forecast - forecast_df$BVP)^2))
print(rmse_adaptive)
#0.05185871
#0.05185871 se

#-----------2019-------------

X_matrix <- as.matrix(train_data_62_2019[, final_vars])
y_vector <- as.numeric(train_data_62_2019$BVP)
X_test_matrix <- as.matrix(X_forecast_62_2019[, final_vars])


library(glmnet)
set.seed(123)
ridge_cv <- cv.glmnet(x = X_matrix, y = y_vector, alpha = 0, type.measure = "mse")
best_ridge_coef <- as.numeric(coef(ridge_cv$glmnet.fit, s = ridge_cv$lambda.min))[-1]

epsilon <- 1e-4
penalty_factors <- 1 / (abs(best_ridge_coef) + epsilon)

alasso_cv <- cv.glmnet(x = X_matrix, y = y_vector,
                       alpha = 1,                  # LASSO
                       type.measure = "mse",
                       nfolds = 10,
                       penalty.factor = penalty_factors,
                       keep = TRUE)

plot(alasso_cv)

lambda_min <- alasso_cv$lambda.1se
best_alasso_coef <- coef(alasso_cv, s = lambda_min)

forecast_bvp <- predict(alasso_cv, newx = X_test_matrix, s = lambda_min)

forecast_df <- data.frame(
  Time = test_data_62_2019$Time,
  BVP_forecast = as.numeric(forecast_bvp),
  BVP = test_data_62_2019$BVP
)

m12 <- ggplot(forecast_df, aes(x = Time)) +
  geom_line(aes(y = BVP_forecast, color = "Prognozė", group = 1), size = 1.2, linetype = "dashed") +
  geom_line(aes(y = BVP, color = "Realybė", group = 2), size = 1.2) +
  geom_point(aes(y = BVP_forecast, color = "Prognozė")) +
  geom_point(aes(y = BVP, color = "Realybė")) +
  scale_color_manual(values = c("Prognozė" = "blue", "Realybė" = "red")) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )+
  labs(
    title = "BVP prognozė su Adaptive LASSO ir realybė",
    x = "Kvartalas",
    y = "BVP",
    color = ""
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs17.png", plot = m12, width = 10, height = 6, dpi = 300, bg = "white")


rmse_adaptive <- sqrt(mean((forecast_df$BVP_forecast - forecast_df$BVP)^2))
print(rmse_adaptive)
#0.02372056
#0.02349299 se




##################################################################################################################################
#----------RANDOM FOREST----------------------------------------------------------

library(randomForest)
# 2017
X <- train_data_21_2017
X_test <- X_forecast_21_2017

set.seed(123)
rf_model <- randomForest(BVP ~ ., data = X[, -1], ntree = 1000, importance = TRUE)

forecast_bvp_rf <- predict(rf_model, newdata = X_test)

forecast_df_rf <- data.frame(
  Time = test_data_21_2017$Time,
  BVP_forecast = as.numeric(forecast_bvp_rf),
  BVP = test_data_21_2017$BVP
)


m13 <- ggplot(forecast_df_rf, aes(x = Time)) +
  geom_line(aes(y = BVP_forecast, color = "Prognozė", group = 1), size = 1.2, linetype = "dashed") +
  geom_line(aes(y = BVP, color = "Realybė", group = 2), size = 1.2) +
  geom_point(aes(y = BVP_forecast, color = "Prognozė")) +
  geom_point(aes(y = BVP, color = "Realybė")) +
  scale_color_manual(values = c("Prognozė" = "blue", "Realybė" = "red")) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )+
  labs(
    title = "BVP prognozė su Random Forest ir realybė",
    x = "Kvartalas",
    y = "BVP",
    color = ""
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs18.png", plot = m13, width = 10, height = 6, dpi = 300, bg = "white")


rmse_rf <- sqrt(mean((forecast_df_rf$BVP_forecast - forecast_df_rf$BVP)^2))
print(rmse_rf)
#0.004604448


# 2008
X <- train_data_21_2007
X_test <- X_forecast_21_2007

set.seed(123)
rf_model <- randomForest(BVP ~ ., data = X[, -1], ntree = 1000, importance = TRUE)

forecast_bvp_rf <- predict(rf_model, newdata = X_test)

forecast_df_rf <- data.frame(
  Time = test_data_21_2007$Time,
  BVP_forecast = as.numeric(forecast_bvp_rf),
  BVP = test_data_21_2007$BVP
)

m14 <- ggplot(forecast_df_rf, aes(x = Time)) +
  geom_line(aes(y = BVP_forecast, color = "Prognozė", group = 1), size = 1.2, linetype = "dashed") +
  geom_line(aes(y = BVP, color = "Realybė", group = 2), size = 1.2) +
  geom_point(aes(y = BVP_forecast, color = "Prognozė")) +
  geom_point(aes(y = BVP, color = "Realybė")) +
  scale_color_manual(values = c("Prognozė" = "blue", "Realybė" = "red")) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )+
  labs(
    title = "BVP prognozė su Random Forest ir realybė",
    x = "Kvartalas",
    y = "BVP",
    color = ""
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs19.png", plot = m14, width = 10, height = 6, dpi = 300, bg = "white")


rmse_rf <- sqrt(mean((forecast_df_rf$BVP_forecast - forecast_df_rf$BVP)^2))
print(rmse_rf)
#0.05291053


# 2019
X <- train_data_21_2019
X_test <- X_forecast_21_2019

set.seed(123)
rf_model <- randomForest(BVP ~ ., data = X[, -1], ntree = 1000, importance = TRUE)

forecast_bvp_rf <- predict(rf_model, newdata = X_test)

forecast_df_rf <- data.frame(
  Time = test_data_21_2019$Time,
  BVP_forecast = as.numeric(forecast_bvp_rf),
  BVP = test_data_21_2019$BVP
)

m15 <- ggplot(forecast_df_rf, aes(x = Time)) +
  geom_line(aes(y = BVP_forecast, color = "Prognozė", group = 1), size = 1.2, linetype = "dashed") +
  geom_line(aes(y = BVP, color = "Realybė", group = 2), size = 1.2) +
  geom_point(aes(y = BVP_forecast, color = "Prognozė")) +
  geom_point(aes(y = BVP, color = "Realybė")) +
  scale_color_manual(values = c("Prognozė" = "blue", "Realybė" = "red")) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )+
  labs(
    title = "BVP prognozė su Random Forest ir realybė",
    x = "Kvartalas",
    y = "BVP",
    color = ""
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("C:/Users/Admin/Desktop/Kursinis 2025/grafikai/kurs20.png", plot = m15, width = 10, height = 6, dpi = 300, bg = "white")


rmse_rf <- sqrt(mean((forecast_df_rf$BVP_forecast - forecast_df_rf$BVP)^2))
print(rmse_rf)
#0.02434663
