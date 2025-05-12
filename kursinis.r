#install.packages("httr2", dependencies=TRUE)
#install.packages("purrr")
#install.packages("eurostat")

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
library(ggplot2)


#------------------------BVP-------------------------

toc <- get_eurostat_toc()

results1 <- search_eurostat("GDP")
print(results1)
gdp_data <- get_eurostat("namq_10_gdp", time_format = "num")
gdp_lt4<- subset(gdp_data, geo == "LT" & s_adj == "SCA" &  unit == "CLV20_MEUR" & na_item=="B1GQ")
gdp_lt4<-gdp_lt4%>%select(c(-1,-2,-3,-4,-5))%>%rename("Time"="TIME_PERIOD", "BVP"="values")
convert_to_quarter <- function(x) {
  year <- floor(x) 
  quarter <- ceiling((x - year) * 4 + 1) 
  paste(year, "K", quarter, sep = "")
  
}

gdp_lt4$Time <- sapply(gdp_lt4$Time, convert_to_quarter)

gdp_lt4 <- gdp_lt4 %>%
  filter(as.numeric(sub("K.*", "", Time)) >= 1999 & 
           as.numeric(sub("K.*", "", Time)) <= 2024)

#------------------------Price indexes-------------------------

# Dataset - https://data.imf.org/regular.aspx?key=61015892
# International monetary found IMF

Country_Indexes_And_Weights <- read_excel("C:/Users/Admin/Desktop/Kursinis 2025/Country_Indexes_And_Weights.xlsx", skip = 2)

first_column <- Country_Indexes_And_Weights[, 1, drop = FALSE]
columns_from_51 <- Country_Indexes_And_Weights[, 51:ncol(Country_Indexes_And_Weights)]
result <- cbind(first_column, columns_from_51)
result <- result[-c(1, 16), ]
result_transposed <- t(result)
result_transposed <- as.data.frame(result_transposed)
colnames(result_transposed) <- result_transposed[1, ]
result_transposed <- result_transposed[-1, ]
result_transposed$Date <- rownames(result_transposed)
result_transposed <- result_transposed[, c(ncol(result_transposed), 1:(ncol(result_transposed)-1))]
rownames(result_transposed) <- NULL

price_indx_lt <- result_transposed

price_indx_lt$Date <- parse_date_time(price_indx_lt$Date, orders = c("ym", "ymd"))

price_indx_lt <- price_indx_lt %>%
  filter(year(Date) >= 1999 & year(Date) <= 2024)

price_indx_lt <- price_indx_lt %>%
  mutate(Time = paste(year(Date), "K", quarter(Date), sep = ""))

price_indx_lt <- price_indx_lt %>%
  mutate(across(3:27, as.numeric))

price_indx_lt <- price_indx_lt %>%
  group_by(Time) %>%
  summarise(across(3:26, mean))



#-----------------------------Industrial Production index, original, 2010=100-----------------------------

# Dataset - https://w3.unece.org/PXWeb2015/pxweb/en/STAT/STAT__20-ME__5-MEPW/3_en_MECCIndProdM_r.px/table/tableViewLayout1/
# UNECE Statistical Database

Industrial_Production <- read_excel("C:/Users/Admin/Desktop/Kursinis 2025/Industrial_Production.xlsx")
Industrial_Production <- Industrial_Production[-c(2,4,5:nrow(Industrial_Production)), ]  
Industrial_Production <- Industrial_Production[, -c(1,3:100)]  
Industrial_Production <- as.data.frame(t(Industrial_Production))
Industrial_Production <- Industrial_Production %>% rownames_to_column(var = "Metai")
colnames(Industrial_Production) <- Industrial_Production[1, ] 
Industrial_Production <- Industrial_Production[-1, ]
colnames(Industrial_Production)[1] <- "Metai"

Industrial_Production$Metai <- parse_date_time(Industrial_Production$Metai, orders = c("ym", "ymd"))

Industrial_Production <- Industrial_Production %>%
  filter(year(Metai) >= 1999 & year(Metai) <= 2024)

Industrial_Production <- Industrial_Production %>%
  mutate(Time = paste(year(Metai), "K", quarter(Metai), sep = ""))

Industrial_Production <- Industrial_Production %>%
  mutate(across(2:3, as.numeric))

Industrial_Production <- Industrial_Production %>%
  group_by(Time) %>%
  summarise(across(2:3, mean))%>%select(c(-2))


#----------------------------------------------------------------------------------------

#Construction input price indices (2021 – 100)

construction_input_price_ind <- read_csv("C:/Users/Admin/Desktop/Kursinis 2025/data-table (2).csv")

construction_input_price_ind <- construction_input_price_ind %>%
  select(`Time`, `Main construction inputs grouping`, `Value`) %>%
  pivot_wider(names_from = `Main construction inputs grouping`, values_from = `Value`)

data <- construction_input_price_ind %>%
  filter(str_sub(Time, 6, 7) %in% c("03", "06", "09", "12"))

convert_to_quarters <- function(time_str) {
  year <- str_sub(time_str, 1, 4)
  month <- str_sub(time_str, 6, 7)
  quarter <- case_when(
    month == "03" ~ "K1",
    month == "06" ~ "K2",
    month == "09" ~ "K3",
    month == "12" ~ "K4"
  )
  return(paste0(year, quarter))
}

construction_input_price_ind <- data %>%
  mutate(Time = sapply(Time, convert_to_quarters))

construction_input_price_ind <- construction_input_price_ind %>%
  filter(as.integer(substr(Time, 1, 4)) >= 1999 & as.integer(substr(Time, 1, 4)) <= 2024)

construction_input_price_ind <- construction_input_price_ind %>%
  rename_with(~paste0(.x, "_price_ind"), .cols = 2:ncol(construction_input_price_ind))
#----------------------------------------------------------------------------------------

# Employment (ESA 2010) seasonally and working day adjusted thousands

Employment <- read_csv("C:/Users/Admin/Desktop/Kursinis 2025/data-table (6).csv")
Employment <- filter(Employment, Employment[[4]] == "All NACE branches" & Unit == "thousand persons")
Employment <- select(Employment, 1,5,8)

Employment <- Employment %>%
  select(1, 2, 3) %>%
  pivot_wider(names_from = 2, values_from = 3)

Employment <- Employment %>%
  filter(as.integer(substr(Time, 1, 4)) >= 1999 & as.integer(substr(Time, 1, 4)) <= 2024)


#----------------------------------------------------------------------------------------

#Foreign direct investment at the end of the period to EU              EUR million

foreign_direct_investment <-read_delim("C:/Users/Admin/Desktop/Kursinis 2025/data-table (7_1).csv", 
                                           delim = "\t", escape_double = FALSE, 
                                           trim_ws = TRUE)
View(foreign_direct_investment)

foreign_direct_investment <- filter(foreign_direct_investment, foreign_direct_investment[[6]] == "Total")
foreign_direct_investment <- select(foreign_direct_investment, 1,8)
foreign_direct_investment <- foreign_direct_investment %>%
  filter(as.integer(substr(Time, 1, 4)) >= 1999 & as.integer(substr(Time, 1, 4)) <= 2024)
foreign_direct_investment <- rename(foreign_direct_investment, "foreign_direct_investment_mil_eur"=2)

#----------------------------------------------------------------------------------------

# Financial accounts of general government


data_table_8_1_ <- read_delim("C:/Users/Admin/Desktop/Kursinis 2025/data-table (8_1).csv", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE)
data_table_8_2_ <- read_delim("C:/Users/Admin/Desktop/Kursinis 2025/data-table (8_2).csv", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE)
data_table_8_3_ <- read_delim("C:/Users/Admin/Desktop/Kursinis 2025/data-table (8_3).csv", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE)

financial_accounts_of_general_government <- bind_rows(data_table_8_1_, data_table_8_2_, data_table_8_3_)
View(financial_accounts_of_general_government)

# Извлекаем уникальные комбинации значений для столбцов 3, 4, 5 и 6
unique_combinations <- financial_accounts_of_general_government %>%
  distinct(`Assets/Liabilities`, `Type of Financial indicator`, Indicators, `Sector / Subsector`)

# Просмотр уникальных комбинаций
print(unique_combinations)

# Создание отдельных таблиц для каждой уникальной комбинации
# Получаем все уникальные строки для столбцов 3, 4, 5 и 6
unique_combinations_list <- unique_combinations %>%
  mutate(
    table_name = paste(`Assets/Liabilities`, `Type of Financial indicator`, Indicators, `Sector / Subsector`, sep = "_")
  )

# Функция для фильтрации и создания таблиц
for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  # Создаем отдельную таблицу для текущей комбинации
  assign(table_name, filter(financial_accounts_of_general_government, 
                            `Assets/Liabilities` == current_combination$`Assets/Liabilities` &
                              `Type of Financial indicator` == current_combination$`Type of Financial indicator` &
                              Indicators == current_combination$Indicators &
                              `Sector / Subsector` == current_combination$`Sector / Subsector`))
  
  # Для удобства: выводим имя текущей таблицы
  print(paste("Создана таблица:", table_name))
}


# Создание пустого списка для хранения всех таблиц
combined_tables <- list()

# Перебор всех таблиц, созданных ранее
for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  # Получаем соответствующую таблицу
  current_table <- get(table_name)
  
  # Оставляем только столбцы Time и Value, а Value переименовываем на имя таблицы
  current_table_transformed <- current_table %>%
    select(Time, Value) %>%
    rename(!!table_name := Value)  # Даем новому столбцу имя таблицы
  
  # Добавляем эту таблицу в список
  combined_tables[[table_name]] <- current_table_transformed
}

# Объединяем все таблицы по дате (Time)
financial_accounts_of_general_government <- reduce(combined_tables, full_join, by = "Time")
financial_accounts_of_general_government <- financial_accounts_of_general_government %>%
  filter(as.integer(substr(Time, 1, 4)) >= 1999 & as.integer(substr(Time, 1, 4)) <= 2024)
View(financial_accounts_of_general_government)

financial_accounts_of_general_government <- financial_accounts_of_general_government %>%
  mutate(
    year = as.numeric(str_sub(Time, 1, 4)),          # Извлекаем год
    quarter = as.numeric(str_sub(Time, 6, 6)),       # Извлекаем квартал
    time_order = year + (quarter - 1) / 4            # Преобразуем в дробное число для сортировки
  ) %>%
  arrange(desc(time_order))  
financial_accounts_of_general_government <- financial_accounts_of_general_government %>%
  select(-year, -quarter, -time_order)

#----------------------------------------------------------------------------------------
#General government revenue and expenditure MIL EUR

data_table_9_1_ <- read_delim("C:/Users/Admin/Desktop/Kursinis 2025/data-table (9_1).csv", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE)
data_table_9_2_ <- read_delim("C:/Users/Admin/Desktop/Kursinis 2025/data-table (9_2).csv", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE)
data_table_9_3_ <- read_delim("C:/Users/Admin/Desktop/Kursinis 2025/data-table (9_3).csv", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE)
general_government_revenue_and_expenditure <- bind_rows(data_table_9_1_, data_table_9_2_, data_table_9_3_)


general_government_revenue_and_expenditure <- general_government_revenue_and_expenditure %>% select(c(-2,-3,-6))

unique_combinations <- general_government_revenue_and_expenditure %>%
  distinct(`Transaction:Name`, `Institutional sector`)

print(unique_combinations)


unique_combinations_list <- unique_combinations %>%
  mutate(
    table_name = paste(`Transaction:Name`, `Institutional sector`, sep = "_")
  )

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  assign(table_name, filter(general_government_revenue_and_expenditure, 
                            `Transaction:Name` == current_combination$`Transaction:Name` &
                              `Institutional sector` == current_combination$`Institutional sector`))
  
  print(paste("Создана таблица:", table_name))
}

combined_tables <- list()

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
    current_table <- get(table_name)
  
  # Оставляем только столбцы Time и Value, а Value переименовываем на имя таблицы
  current_table_transformed <- current_table %>%
    select(Time, Value) %>%
    rename(!!table_name := Value)
  
  # Добавляем эту таблицу в список
  combined_tables[[table_name]] <- current_table_transformed
}

# Объединяем все таблицы по дате (Time)
general_government_revenue_and_expenditure <- reduce(combined_tables, full_join, by = "Time")

general_government_revenue_and_expenditure <- general_government_revenue_and_expenditure %>%
  filter(as.integer(substr(Time, 1, 4)) >= 1999 & as.integer(substr(Time, 1, 4)) <= 2024)

general_government_revenue_and_expenditure <- general_government_revenue_and_expenditure %>%
  mutate(
    year = as.numeric(str_sub(Time, 1, 4)),          # Извлекаем год
    quarter = as.numeric(str_sub(Time, 6, 6)),       # Извлекаем квартал
    time_order = year + (quarter - 1) / 4            # Преобразуем в дробное число для сортировки
  ) %>%
  arrange(desc(time_order))  
general_government_revenue_and_expenditure <- general_government_revenue_and_expenditure %>%
  select(-year, -quarter, -time_order)
View(general_government_revenue_and_expenditure)


#----------------------------------------------------------------------------------------
#General government deficit/surplus (SNA 2008) MIL EUR



general_government_deficit_surplus <- read_delim("C:/Users/Admin/Desktop/Kursinis 2025/data-table (10_1).csv", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE)


general_government_deficit_surplus <- general_government_deficit_surplus %>% select(c(-2,-5))

unique_combinations <- general_government_deficit_surplus %>%
  distinct(`Indicators`, `Institutional sector`)

print(unique_combinations)


unique_combinations_list <- unique_combinations %>%
  mutate(
    table_name = paste(`Indicators`, `Institutional sector`, sep = "_")
  )

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  assign(table_name, filter(general_government_deficit_surplus, 
                            `Indicators` == current_combination$`Indicators` &
                              `Institutional sector` == current_combination$`Institutional sector`))
  
  print(paste("Создана таблица:", table_name))
}

combined_tables <- list()

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  current_table <- get(table_name)
  
  # Оставляем только столбцы Time и Value, а Value переименовываем на имя таблицы
  current_table_transformed <- current_table %>%
    select(Time, Value) %>%
    rename(!!table_name := Value)
  
  # Добавляем эту таблицу в список
  combined_tables[[table_name]] <- current_table_transformed
}

# Объединяем все таблицы по дате (Time)
general_government_deficit_surplus <- reduce(combined_tables, full_join, by = "Time")

general_government_deficit_surplus <- general_government_deficit_surplus %>%
  filter(as.integer(substr(Time, 1, 4)) >= 1999 & as.integer(substr(Time, 1, 4)) <= 2024)
general_government_deficit_surplus <- general_government_deficit_surplus %>%distinct(Time, .keep_all = TRUE)

#----------------------------------------------------------------------------------------
#Investment in tangible fixed assets at current prices  (I-IV qua) THOUS EUR

investment_in_tangible_fixed_assets <- read_csv("C:/Users/Admin/Desktop/Kursinis 2025/data-table (11).csv")
investment_in_tangible_fixed_assets <- investment_in_tangible_fixed_assets %>% select(c(-2,-5))

unique_combinations <- investment_in_tangible_fixed_assets %>%
  distinct(`Purpose`, `Sector`)

print(unique_combinations)


unique_combinations_list <- unique_combinations %>%
  mutate(
    table_name = paste(`Purpose`, `Sector`, sep = "_")
  )

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  assign(table_name, filter(investment_in_tangible_fixed_assets, 
                            `Purpose` == current_combination$`Purpose` &
                              `Sector` == current_combination$`Sector`))
  
  print(paste("Создана таблица:", table_name))
}

combined_tables <- list()

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  current_table <- get(table_name)
  
  # Оставляем только столбцы Time и Value, а Value переименовываем на имя таблицы
  current_table_transformed <- current_table %>%
    select(Time, Value) %>%
    rename(!!table_name := Value)
  
  # Добавляем эту таблицу в список
  combined_tables[[table_name]] <- current_table_transformed
}

# Объединяем все таблицы по дате (Time)
investment_in_tangible_fixed_assets <- reduce(combined_tables, full_join, by = "Time")

investment_in_tangible_fixed_assets <- investment_in_tangible_fixed_assets %>%
  filter(as.integer(substr(Time, 1, 4)) >= 1999 & as.integer(substr(Time, 1, 4)) <= 2024)


#----------------------------------------------------------------------------------------
#Sales of service enterprises (VAT excluded) THOUS EUR

sales_of_service_enterprises <- read_csv("C:/Users/Admin/Desktop/Kursinis 2025/data-table (13).csv")
colnames(sales_of_service_enterprises)

sales_of_service_enterprises <- sales_of_service_enterprises %>% select(c(-2,-5))

unique_combinations <- sales_of_service_enterprises %>%
  distinct(`Economic activity (NACE Rev. 2, 2-4 digit level):Code`, `Economic activity (NACE Rev. 2, 2-4 digit level):Name`)

print(unique_combinations)


unique_combinations_list <- unique_combinations %>%
  mutate(
    table_name = paste(`Economic activity (NACE Rev. 2, 2-4 digit level):Code`, `Economic activity (NACE Rev. 2, 2-4 digit level):Name`, sep = "_")
  )

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  assign(table_name, filter(sales_of_service_enterprises, 
                            `Economic activity (NACE Rev. 2, 2-4 digit level):Code` == current_combination$`Economic activity (NACE Rev. 2, 2-4 digit level):Code` &
                              `Economic activity (NACE Rev. 2, 2-4 digit level):Name` == current_combination$`Economic activity (NACE Rev. 2, 2-4 digit level):Name`))
  
  print(paste("Создана таблица:", table_name))
}

combined_tables <- list()

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  current_table <- get(table_name)
  
  # Оставляем только столбцы Time и Value, а Value переименовываем на имя таблицы
  current_table_transformed <- current_table %>%
    select(Time, Value) %>%
    rename(!!table_name := Value)
  
  # Добавляем эту таблицу в список
  combined_tables[[table_name]] <- current_table_transformed
}

# Объединяем все таблицы по дате (Time)
sales_of_service_enterprises <- reduce(combined_tables, full_join, by = "Time")

sales_of_service_enterprises <- sales_of_service_enterprises %>%
  filter(as.integer(substr(Time, 1, 4)) >= 1999 & as.integer(substr(Time, 1, 4)) <= 2024)

#----------------------------------------------------------------------------------------
#Indices of the number of persons employed in service enterprises (2021 – 100) per cent


indices_of_the_number_of_persons_employed_in_service_enterprises <- read_csv("C:/Users/Admin/Desktop/Kursinis 2025/data-table (14).csv")
colnames(indices_of_the_number_of_persons_employed_in_service_enterprises)

indices_of_the_number_of_persons_employed_in_service_enterprises <- indices_of_the_number_of_persons_employed_in_service_enterprises %>% select(c(-2,-5))

unique_combinations <- indices_of_the_number_of_persons_employed_in_service_enterprises %>%
  distinct(`Economic activity (NACE Rev. 2):Code`, `Economic activity (NACE Rev. 2):Name`)

print(unique_combinations)


unique_combinations_list <- unique_combinations %>%
  mutate(
    table_name = paste(`Economic activity (NACE Rev. 2):Code`, `Economic activity (NACE Rev. 2):Name`, sep = "_")
  )

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  assign(table_name, filter(indices_of_the_number_of_persons_employed_in_service_enterprises, 
                            `Economic activity (NACE Rev. 2):Code` == current_combination$`Economic activity (NACE Rev. 2):Code` &
                              `Economic activity (NACE Rev. 2):Name` == current_combination$`Economic activity (NACE Rev. 2):Name`))
  
  print(paste("Создана таблица:", table_name))
}

combined_tables <- list()

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  current_table <- get(table_name)
  
  # Оставляем только столбцы Time и Value, а Value переименовываем на имя таблицы
  current_table_transformed <- current_table %>%
    select(Time, Value) %>%
    rename(!!table_name := Value)
  
  # Добавляем эту таблицу в список
  combined_tables[[table_name]] <- current_table_transformed
}

# Объединяем все таблицы по дате (Time)
indices_of_the_number_of_persons_employed_in_service_enterprises <- reduce(combined_tables, full_join, by = "Time")

indices_of_the_number_of_persons_employed_in_service_enterprises <- indices_of_the_number_of_persons_employed_in_service_enterprises %>%
  filter(as.integer(substr(Time, 1, 4)) >= 1999 & as.integer(substr(Time, 1, 4)) <= 2024)


#----------------------------------------------------------------------------------------
#Industrial production (VAT and excises excluded) eur thous


industrial_production <- read_csv("C:/Users/Admin/Desktop/Kursinis 2025/data-table (15).csv")
colnames(industrial_production)
industrial_production <- filter(industrial_production, Estimation == "at current prices")

industrial_production <- industrial_production %>% select(c(-2,-3,-6))

unique_combinations <- industrial_production %>%
  distinct(`Economic activity (NACE Rev. 2):Code`, `Economic activity (NACE Rev. 2):Name`)

print(unique_combinations)


unique_combinations_list <- unique_combinations %>%
  mutate(
    table_name = paste(`Economic activity (NACE Rev. 2):Code`, `Economic activity (NACE Rev. 2):Name`, sep = "_")
  )

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  assign(table_name, filter(industrial_production, 
                            `Economic activity (NACE Rev. 2):Code` == current_combination$`Economic activity (NACE Rev. 2):Code` &
                              `Economic activity (NACE Rev. 2):Name` == current_combination$`Economic activity (NACE Rev. 2):Name`))
  
  print(paste("Создана таблица:", table_name))
}

combined_tables <- list()

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  current_table <- get(table_name)
  
  # Оставляем только столбцы Time и Value, а Value переименовываем на имя таблицы
  current_table_transformed <- current_table %>%
    select(Time, Value) %>%
    rename(!!table_name := Value)
  
  # Добавляем эту таблицу в список
  combined_tables[[table_name]] <- current_table_transformed
}

# Объединяем все таблицы по дате (Time)
industrial_production <- reduce(combined_tables, full_join, by = "Time")

industrial_production <- industrial_production %>%
  filter(as.integer(substr(Time, 1, 4)) >= 1999 & as.integer(substr(Time, 1, 4)) <= 2024)


#----------------------------------------------------------------------------------------
#Useful floor area of dwellings completed thous m^2


floor_area_of_dwellings_completed <- read_csv("C:/Users/Admin/Desktop/Kursinis 2025/data-table (16).csv")
colnames(floor_area_of_dwellings_completed)
floor_area_of_dwellings_completed <- filter(floor_area_of_dwellings_completed, floor_area_of_dwellings_completed[[3]] != "Residences for communities")

floor_area_of_dwellings_completed <- floor_area_of_dwellings_completed %>% select(c(-2,-4))

unique_combinations <- floor_area_of_dwellings_completed %>%
  distinct(`Classification of Types of Construction (CC)(types of residential buildings)`)

print(unique_combinations)


unique_combinations_list <- unique_combinations %>%
  mutate(
    table_name = paste(`Classification of Types of Construction (CC)(types of residential buildings)`, sep = "_")
  )

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  assign(table_name, filter(floor_area_of_dwellings_completed, 
                            `Classification of Types of Construction (CC)(types of residential buildings)` == current_combination$`Classification of Types of Construction (CC)(types of residential buildings)`))
  
  print(paste("Создана таблица:", table_name))
}

combined_tables <- list()

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  current_table <- get(table_name)
  
  # Оставляем только столбцы Time и Value, а Value переименовываем на имя таблицы
  current_table_transformed <- current_table %>%
    select(Time, Value) %>%
    rename(!!table_name := Value)
  
  # Добавляем эту таблицу в список
  combined_tables[[table_name]] <- current_table_transformed
}

# Объединяем все таблицы по дате (Time)
floor_area_of_dwellings_completed <- reduce(combined_tables, full_join, by = "Time")

floor_area_of_dwellings_completed <- floor_area_of_dwellings_completed %>%
  filter(as.integer(substr(Time, 1, 4)) >= 1999 & as.integer(substr(Time, 1, 4)) <= 2024)




#----------------------------------------------------------------------------------------
#Floor area of new non-residential building completed thous m^2


floor_area_of_nonresidential_completed <- read_csv("C:/Users/Admin/Desktop/Kursinis 2025/data-table (18).csv")
colnames(floor_area_of_nonresidential_completed)

floor_area_of_nonresidential_completed <- floor_area_of_nonresidential_completed %>% select(c(-2,-4))

unique_combinations <- floor_area_of_nonresidential_completed %>%
  distinct(`Classification of Types of Construction (CC) (types of non-residential buildings)`)

print(unique_combinations)


unique_combinations_list <- unique_combinations %>%
  mutate(
    table_name = paste(`Classification of Types of Construction (CC) (types of non-residential buildings)`, sep = "_")
  )

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  assign(table_name, filter(floor_area_of_nonresidential_completed, 
                            `Classification of Types of Construction (CC) (types of non-residential buildings)` == current_combination$`Classification of Types of Construction (CC) (types of non-residential buildings)`))
  
  print(paste("Создана таблица:", table_name))
}

combined_tables <- list()

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  current_table <- get(table_name)
  
  # Оставляем только столбцы Time и Value, а Value переименовываем на имя таблицы
  current_table_transformed <- current_table %>%
    select(Time, Value) %>%
    rename(!!table_name := Value)
  
  # Добавляем эту таблицу в список
  combined_tables[[table_name]] <- current_table_transformed
}

# Объединяем все таблицы по дате (Time)
floor_area_of_nonresidential_completed <- reduce(combined_tables, full_join, by = "Time")

floor_area_of_nonresidential_completed <- floor_area_of_nonresidential_completed %>%
  filter(as.integer(substr(Time, 1, 4)) >= 1999 & as.integer(substr(Time, 1, 4)) <= 2024)


#----------------------------------------------------------------------------------------
#Turnover of retail trade, motor vehicles, catering enterprises (VAT excluded) at current prices eur thous


turnover_of_enterprises <- read_csv("C:/Users/Admin/Desktop/Kursinis 2025/data-table (19).csv")
colnames(turnover_of_enterprises)

turnover_of_enterprises <- turnover_of_enterprises %>% select(c(-2,-3,-5))

unique_combinations <- turnover_of_enterprises %>%
  distinct(`Economic activity (NACE Rev. 2):Name`)

print(unique_combinations)


unique_combinations_list <- unique_combinations %>%
  mutate(
    table_name = paste(`Economic activity (NACE Rev. 2):Name`, sep = "_")
  )

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  assign(table_name, filter(turnover_of_enterprises, 
                              `Economic activity (NACE Rev. 2):Name` == current_combination$`Economic activity (NACE Rev. 2):Name`))
  
  print(paste("Создана таблица:", table_name))
}

combined_tables <- list()

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  current_table <- get(table_name)
  
  # Оставляем только столбцы Time и Value, а Value переименовываем на имя таблицы
  current_table_transformed <- current_table %>%
    select(Time, Value) %>%
    rename(!!table_name := Value)
  
  # Добавляем эту таблицу в список
  combined_tables[[table_name]] <- current_table_transformed
}

# Объединяем все таблицы по дате (Time)
turnover_of_enterprises <- reduce(combined_tables, full_join, by = "Time")

turnover_of_enterprises <- turnover_of_enterprises %>%
  filter(as.integer(substr(Time, 1, 4)) >= 1999 & as.integer(substr(Time, 1, 4)) <= 2024)



#----------------------------------------------------------------------------------------
#Turnover of wholesale trade enterprises (VAT excluded) at current price  eur thous


turnover_of_wholesale_enterprises <- read_csv("C:/Users/Admin/Desktop/Kursinis 2025/data-table (20).csv")
colnames(turnover_of_wholesale_enterprises)

turnover_of_wholesale_enterprises <- turnover_of_wholesale_enterprises %>% select(c(-2,-3,-5))

unique_combinations <- turnover_of_wholesale_enterprises %>%
  distinct(`Economic activity (NACE Rev. 2):Name`)

print(unique_combinations)


unique_combinations_list <- unique_combinations %>%
  mutate(
    table_name = paste(`Economic activity (NACE Rev. 2):Name`, sep = "_")
  )

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  assign(table_name, filter(turnover_of_wholesale_enterprises, 
                            `Economic activity (NACE Rev. 2):Name` == current_combination$`Economic activity (NACE Rev. 2):Name`))
  
  print(paste("Создана таблица:", table_name))
}

combined_tables <- list()

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  current_table <- get(table_name)
  
  # Оставляем только столбцы Time и Value, а Value переименовываем на имя таблицы
  current_table_transformed <- current_table %>%
    select(Time, Value) %>%
    rename(!!table_name := Value)
  
  # Добавляем эту таблицу в список
  combined_tables[[table_name]] <- current_table_transformed
}

# Объединяем все таблицы по дате (Time)
turnover_of_wholesale_enterprises <- reduce(combined_tables, full_join, by = "Time")

turnover_of_wholesale_enterprises <- turnover_of_wholesale_enterprises %>%
  filter(as.integer(substr(Time, 1, 4)) >= 1999 & as.integer(substr(Time, 1, 4)) <= 2024)


#----------------------------------------------------------------------------------------
#Unemployed thous


Unemployed <- read_csv("C:/Users/Admin/Desktop/Kursinis 2025/data-table (22).csv")
colnames(Unemployed)

Unemployed <- Unemployed %>% select(c(-2,-6))
Unemployed <- Unemployed %>% filter(Sex != "Males and females" & Age != "Total by age"& Age != "65+")


unique_combinations <- Unemployed %>%
  distinct(`Sex`,`Place of residence`,`Age`)

print(unique_combinations)


unique_combinations_list <- unique_combinations %>%
  mutate(
    table_name = paste(`Sex`,`Place of residence`,`Age`, sep = "_")
  )

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  assign(table_name, filter(Unemployed, 
                            `Sex` == current_combination$`Sex` &
                              `Place of residence` == current_combination$`Place of residence` &
                              `Age` == current_combination$`Age`))
  
  print(paste("Создана таблица:", table_name))
}

combined_tables <- list()

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  current_table <- get(table_name)
  
  # Оставляем только столбцы Time и Value, а Value переименовываем на имя таблицы
  current_table_transformed <- current_table %>%
    select(Time, Value) %>%
    rename(!!paste0(table_name, "_Unemployed") := Value)
  
  # Добавляем эту таблицу в список
  combined_tables[[table_name]] <- current_table_transformed
}

# Объединяем все таблицы по дате (Time)
Unemployed <- reduce(combined_tables, full_join, by = "Time")

Unemployed <- Unemployed %>%
  filter(as.integer(substr(Time, 1, 4)) >= 1999 & as.integer(substr(Time, 1, 4)) <= 2024)







#----------------------------------------------------------------------------------------
#Labour force thous


Labour_force <- read_csv("C:/Users/Admin/Desktop/Kursinis 2025/data-table (23).csv")
colnames(Labour_force)

Labour_force <- Labour_force %>% select(c(-2,-6))
Labour_force <- Labour_force %>% filter(Sex != "Males and females" & Age != "Total by age")


unique_combinations <- Labour_force %>%
  distinct(`Sex`,`Place of residence`,`Age`)

print(unique_combinations)


unique_combinations_list <- unique_combinations %>%
  mutate(
    table_name = paste(`Sex`,`Place of residence`,`Age`, sep = "_")
  )

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  assign(table_name, filter(Labour_force, 
                            `Sex` == current_combination$`Sex` &
                            `Place of residence` == current_combination$`Place of residence` &
                            `Age` == current_combination$`Age`))
  
  print(paste("Создана таблица:", table_name))
}

combined_tables <- list()

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  current_table <- get(table_name)
  
  # Оставляем только столбцы Time и Value, а Value переименовываем на имя таблицы
  current_table_transformed <- current_table %>%
    select(Time, Value) %>%
    rename(!!paste0(table_name, "_Labour_force") := Value)
  
  # Добавляем эту таблицу в список
  combined_tables[[table_name]] <- current_table_transformed
}

# Объединяем все таблицы по дате (Time)
Labour_force <- reduce(combined_tables, full_join, by = "Time")

Labour_force <- Labour_force %>%
  filter(as.integer(substr(Time, 1, 4)) >= 1999 & as.integer(substr(Time, 1, 4)) <= 2024)



#----------------------------------------------------------------------------------------
#Activity rate per cent


Activity_rate <- read_csv("C:/Users/Admin/Desktop/Kursinis 2025/data-table (24).csv")
colnames(Activity_rate)

Activity_rate <- Activity_rate %>% select(c(-2,-6))
Activity_rate <- Activity_rate %>% filter(Sex != "Males and females" & Age != "Total by age")


unique_combinations <- Activity_rate %>%
  distinct(`Sex`,`Place of residence`,`Age`)

print(unique_combinations)


unique_combinations_list <- unique_combinations %>%
  mutate(
    table_name = paste(`Sex`,`Place of residence`,`Age`, sep = "_")
  )

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  assign(table_name, filter(Activity_rate, 
                            `Sex` == current_combination$`Sex` &
                              `Place of residence` == current_combination$`Place of residence` &
                              `Age` == current_combination$`Age`))
  
  print(paste("Создана таблица:", table_name))
}

combined_tables <- list()

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  current_table <- get(table_name)
  
  # Оставляем только столбцы Time и Value, а Value переименовываем на имя таблицы
  current_table_transformed <- current_table %>%
    select(Time, Value) %>%
    rename(!!paste0(table_name, "_Activity_rate") := Value)
  
  # Добавляем эту таблицу в список
  combined_tables[[table_name]] <- current_table_transformed
}

# Объединяем все таблицы по дате (Time)
Activity_rate <- reduce(combined_tables, full_join, by = "Time")

Activity_rate <- Activity_rate %>%
  filter(as.integer(substr(Time, 1, 4)) >= 1999 & as.integer(substr(Time, 1, 4)) <= 2024)



#----------------------------------------------------------------------------------------
#Long-term unemployment rate per cent


Longterm_unemployment<- read_csv("C:/Users/Admin/Desktop/Kursinis 2025/data-table (25).csv")
colnames(Longterm_unemployment)

Longterm_unemployment <- Longterm_unemployment %>% select(c(-2,-5))
Longterm_unemployment <- Longterm_unemployment %>% filter(Sex != "Males and females")


unique_combinations <- Longterm_unemployment %>%
  distinct(`Sex`,`Place of residence`)

print(unique_combinations)


unique_combinations_list <- unique_combinations %>%
  mutate(
    table_name = paste(`Sex`,`Place of residence`, sep = "_")
  )

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  assign(table_name, filter(Longterm_unemployment, 
                            `Sex` == current_combination$`Sex` &
                              `Place of residence` == current_combination$`Place of residence`))
  
  print(paste("Создана таблица:", table_name))
}

combined_tables <- list()

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  current_table <- get(table_name)
  
  # Оставляем только столбцы Time и Value, а Value переименовываем на имя таблицы
  current_table_transformed <- current_table %>%
    select(Time, Value) %>%
    rename(!!paste0(table_name, "_Longterm_unemployment") := Value)
  
  # Добавляем эту таблицу в список
  combined_tables[[table_name]] <- current_table_transformed
}

# Объединяем все таблицы по дате (Time)
Longterm_unemployment <- reduce(combined_tables, full_join, by = "Time")

Longterm_unemployment <- Longterm_unemployment %>%
  filter(as.integer(substr(Time, 1, 4)) >= 1999 & as.integer(substr(Time, 1, 4)) <= 2024)


#----------------------------------------------------------------------------------------
#Employed persons thous


Employed_persons <- read_csv("C:/Users/Admin/Desktop/Kursinis 2025/data-table (28).csv")

Employed_persons <- Employed_persons %>% select(c(-2,-6))
Employed_persons <- Employed_persons %>% filter(Sex != "Males and females" & Age != "Total by age")


unique_combinations <- Employed_persons %>%
  distinct(`Sex`,`Place of residence`,`Age`)

print(unique_combinations)


unique_combinations_list <- unique_combinations %>%
  mutate(
    table_name = paste(`Sex`,`Place of residence`,`Age`, sep = "_")
  )

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  assign(table_name, filter(Employed_persons, 
                            `Sex` == current_combination$`Sex` &
                              `Place of residence` == current_combination$`Place of residence` &
                              `Age` == current_combination$`Age`))
  
  print(paste("Создана таблица:", table_name))
}

combined_tables <- list()

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  current_table <- get(table_name)
  
  # Оставляем только столбцы Time и Value, а Value переименовываем на имя таблицы
  current_table_transformed <- current_table %>%
    select(Time, Value) %>%
    rename(!!paste0(table_name, "_Employed_persons") := Value)
  
  # Добавляем эту таблицу в список
  combined_tables[[table_name]] <- current_table_transformed
}

# Объединяем все таблицы по дате (Time)
Employed_persons <- reduce(combined_tables, full_join, by = "Time")

Employed_persons <- Employed_persons %>%
  filter(as.integer(substr(Time, 1, 4)) >= 1999 & as.integer(substr(Time, 1, 4)) <= 2024)


#----------------------------------------------------------------------------------------
#Inactive_persons thous


Inactive_persons <- read_csv("C:/Users/Admin/Desktop/Kursinis 2025/data-table (26).csv")
colnames(Inactive_persons)

Inactive_persons <- Inactive_persons %>% select(c(-2,-6))
Inactive_persons <- Inactive_persons %>% filter(Sex != "Males and females" & Age != "Total by age")


unique_combinations <- Inactive_persons %>%
  distinct(`Sex`,`Place of residence`,`Age`)

print(unique_combinations)


unique_combinations_list <- unique_combinations %>%
  mutate(
    table_name = paste(`Sex`,`Place of residence`,`Age`, sep = "_")
  )

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  assign(table_name, filter(Inactive_persons, 
                            `Sex` == current_combination$`Sex` &
                              `Place of residence` == current_combination$`Place of residence` &
                              `Age` == current_combination$`Age`))
  
  print(paste("Создана таблица:", table_name))
}

combined_tables <- list()

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  current_table <- get(table_name)
  
  # Оставляем только столбцы Time и Value, а Value переименовываем на имя таблицы
  current_table_transformed <- current_table %>%
    select(Time, Value) %>%
    rename(!!paste0(table_name, "_Inactive_persons") := Value)
  
  # Добавляем эту таблицу в список
  combined_tables[[table_name]] <- current_table_transformed
}

# Объединяем все таблицы по дате (Time)
Inactive_persons <- reduce(combined_tables, full_join, by = "Time")

Inactive_persons <- Inactive_persons %>%
  filter(as.integer(substr(Time, 1, 4)) >= 1999 & as.integer(substr(Time, 1, 4)) <= 2024)



#----------------------------------------------------------------------------------------
#	Labour costs per hour worked EUR


Labour_costs_per_hour <- read_csv("C:/Users/Admin/Desktop/Kursinis 2025/data-table (29).csv")
colnames(Labour_costs_per_hour)

Labour_costs_per_hour <- Labour_costs_per_hour %>% select(c(-2,-3,-5))

unique_combinations <- Labour_costs_per_hour %>%
  distinct(`Economic activity (NACE Rev. 2):Name`)

print(unique_combinations)


unique_combinations_list <- unique_combinations %>%
  mutate(
    table_name = paste(`Economic activity (NACE Rev. 2):Name`, sep = "_")
  )

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  assign(table_name, filter(Labour_costs_per_hour, 
                            `Economic activity (NACE Rev. 2):Name` == current_combination$`Economic activity (NACE Rev. 2):Name`))
  
  print(paste("Создана таблица:", table_name))
}

combined_tables <- list()

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  current_table <- get(table_name)
  
  # Оставляем только столбцы Time и Value, а Value переименовываем на имя таблицы
  current_table_transformed <- current_table %>%
    select(Time, Value) %>%
    rename(!!paste0(table_name, "_Labour_costs_per_hour") := Value)
  
  # Добавляем эту таблицу в список
  combined_tables[[table_name]] <- current_table_transformed
}

# Объединяем все таблицы по дате (Time)
Labour_costs_per_hour <- reduce(combined_tables, full_join, by = "Time")

Labour_costs_per_hour <- Labour_costs_per_hour %>%
  filter(as.integer(substr(Time, 1, 4)) >= 1999 & as.integer(substr(Time, 1, 4)) <= 2024)



#----------------------------------------------------------------------------------------
#	Basic social benefit EUR


Basic_social_benefit <- read_csv("C:/Users/Admin/Desktop/Kursinis 2025/data-table (30).csv")

Basic_social_benefit <- Basic_social_benefit %>% select(c(-2,-3)) %>% rename("Basic_social_benefit"=Value)


Basic_social_benefit <- Basic_social_benefit %>%
  filter(as.integer(substr(Time, 1, 4)) >= 1999 & as.integer(substr(Time, 1, 4)) <= 2024)


#----------------------------------------------------------------------------------------
#	Minimum monthly wage EUR


Minimum_monthly_wage <- read_csv("C:/Users/Admin/Desktop/Kursinis 2025/data-table (31).csv")

Minimum_monthly_wage <- Minimum_monthly_wage %>% select(c(-2,-3)) %>% rename("Minimum_monthly_wage"=Value)


Minimum_monthly_wage <- Minimum_monthly_wage %>%
  filter(as.integer(substr(Time, 1, 4)) >= 1999 & as.integer(substr(Time, 1, 4)) <= 2024)


#----------------------------------------------------------------------------------------
#	Minimum hourly pay EUR


Minimum_hourly_pay <- read_csv("C:/Users/Admin/Desktop/Kursinis 2025/data-table (32).csv")

Minimum_hourly_pay <- Minimum_hourly_pay %>% select(c(-2,-3)) %>% rename("Minimum_hourly_pay"=Value)


Minimum_hourly_pay <- Minimum_hourly_pay %>%
  filter(as.integer(substr(Time, 1, 4)) >= 1999 & as.integer(substr(Time, 1, 4)) <= 2024)



#----------------------------------------------------------------------------------------
#	Indice of average monthly state social insurance old-age pension per cent


Indice_average_monthly_social_insurance_oldage_pension <- read_csv("C:/Users/Admin/Desktop/Kursinis 2025/data-table (34).csv")

Indice_average_monthly_social_insurance_oldage_pension <- Indice_average_monthly_social_insurance_oldage_pension %>% select(c(-2,-3)) %>% rename("Indice_average_monthly_social_insurance_oldage_pension"=Value)


Indice_average_monthly_social_insurance_oldage_pension <- Indice_average_monthly_social_insurance_oldage_pension %>%
  filter(as.integer(substr(Time, 1, 4)) >= 1999 & as.integer(substr(Time, 1, 4)) <= 2024)


#----------------------------------------------------------------------------------------
#	Passenger transport by all modes of transport thous


Passenger_transport <- read_csv("C:/Users/Admin/Desktop/Kursinis 2025/data-table (36).csv")
colnames(Passenger_transport)

Passenger_transport <- Passenger_transport %>% select(c(-2,-4))

unique_combinations <- Passenger_transport %>%
  distinct(`Modes of transport`)

print(unique_combinations)


unique_combinations_list <- unique_combinations %>%
  mutate(
    table_name = paste(`Modes of transport`, sep = "_")
  )

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  assign(table_name, filter(Passenger_transport, 
                            `Modes of transport` == current_combination$`Modes of transport`))
  
  print(paste("Создана таблица:", table_name))
}

combined_tables <- list()

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  current_table <- get(table_name)
  
  # Оставляем только столбцы Time и Value, а Value переименовываем на имя таблицы
  current_table_transformed <- current_table %>%
    select(Time, Value) %>%
    rename(!!paste0(table_name, "_Passenger_transport") := Value)
  
  # Добавляем эту таблицу в список
  combined_tables[[table_name]] <- current_table_transformed
}

# Объединяем все таблицы по дате (Time)
Passenger_transport <- reduce(combined_tables, full_join, by = "Time")

Passenger_transport <- Passenger_transport %>%
  filter(as.integer(substr(Time, 1, 4)) >= 1999 & as.integer(substr(Time, 1, 4)) <= 2024)

#----------------------------------------------------------------------------------------
#	Tonne-kilometres by all modes of transport thousand tonne-kilometres


Tonne_kilometres <- read_csv("C:/Users/Admin/Desktop/Kursinis 2025/data-table (37).csv")
colnames(Tonne_kilometres)

Tonne_kilometres <- Tonne_kilometres %>% select(c(-2,-4))

unique_combinations <- Tonne_kilometres %>%
  distinct(`Modes of transport`)

print(unique_combinations)


unique_combinations_list <- unique_combinations %>%
  mutate(
    table_name = paste(`Modes of transport`, sep = "_")
  )

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  assign(table_name, filter(Tonne_kilometres, 
                            `Modes of transport` == current_combination$`Modes of transport`))
  
  print(paste("Создана таблица:", table_name))
}

combined_tables <- list()

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  current_table <- get(table_name)
  
  # Оставляем только столбцы Time и Value, а Value переименовываем на имя таблицы
  current_table_transformed <- current_table %>%
    select(Time, Value) %>%
    rename(!!paste0(table_name, "_Tonne_kilometres") := Value)
  
  # Добавляем эту таблицу в список
  combined_tables[[table_name]] <- current_table_transformed
}

# Объединяем все таблицы по дате (Time)
Tonne_kilometres <- reduce(combined_tables, full_join, by = "Time")

Tonne_kilometres <- Tonne_kilometres %>%
  filter(as.integer(substr(Time, 1, 4)) >= 1999 & as.integer(substr(Time, 1, 4)) <= 2024)

#----------------------------------------------------------------------------------------
#	Goods carried by all modes of transport thousand tonnes


Goods_carried <- read_csv("C:/Users/Admin/Desktop/Kursinis 2025/data-table (38).csv")
colnames(Goods_carried)

Goods_carried <- Goods_carried %>% select(c(-2,-4))

unique_combinations <- Goods_carried %>%
  distinct(`Modes of transport`)

print(unique_combinations)


unique_combinations_list <- unique_combinations %>%
  mutate(
    table_name = paste(`Modes of transport`, sep = "_")
  )

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  assign(table_name, filter(Goods_carried, 
                            `Modes of transport` == current_combination$`Modes of transport`))
  
  print(paste("Создана таблица:", table_name))
}

combined_tables <- list()

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  current_table <- get(table_name)
  
  # Оставляем только столбцы Time и Value, а Value переименовываем на имя таблицы
  current_table_transformed <- current_table %>%
    select(Time, Value) %>%
    rename(!!paste0(table_name, "_Goods_carried") := Value)
  
  # Добавляем эту таблицу в список
  combined_tables[[table_name]] <- current_table_transformed
}

# Объединяем все таблицы по дате (Time)
Goods_carried <- reduce(combined_tables, full_join, by = "Time")

Goods_carried <- Goods_carried %>%
  filter(as.integer(substr(Time, 1, 4)) >= 1999 & as.integer(substr(Time, 1, 4)) <= 2024)



#----------------------------------------------------------------------------------------
#	Passenger-kilometres by rail thousand passenger-kilometres


Passenger_km_by_rail <- read_csv("C:/Users/Admin/Desktop/Kursinis 2025/data-table (39).csv")
colnames(Passenger_km_by_rail)

Passenger_km_by_rail <- Passenger_km_by_rail %>% select(c(-2,-4))

unique_combinations <- Passenger_km_by_rail %>%
  distinct(`Type of traffic`)

print(unique_combinations)


unique_combinations_list <- unique_combinations %>%
  mutate(
    table_name = paste(`Type of traffic`, sep = "_")
  )

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  assign(table_name, filter(Passenger_km_by_rail, 
                            `Type of traffic` == current_combination$`Type of traffic`))
  
  print(paste("Создана таблица:", table_name))
}

combined_tables <- list()

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  current_table <- get(table_name)
  
  # Оставляем только столбцы Time и Value, а Value переименовываем на имя таблицы
  current_table_transformed <- current_table %>%
    select(Time, Value) %>%
    rename(!!paste0(table_name, "_Passenger_km_by_rail") := Value)
  
  # Добавляем эту таблицу в список
  combined_tables[[table_name]] <- current_table_transformed
}

# Объединяем все таблицы по дате (Time)
Passenger_km_by_rail <- reduce(combined_tables, full_join, by = "Time")

Passenger_km_by_rail <- Passenger_km_by_rail %>%
  filter(as.integer(substr(Time, 1, 4)) >= 1999 & as.integer(substr(Time, 1, 4)) <= 2024)

#----------------------------------------------------------------------------------------
#	Containers_in_Seaports


Containers_in_Seaports <- read_csv("C:/Users/Admin/Desktop/Kursinis 2025/data-table (41).csv")
colnames(Containers_in_Seaports)

Containers_in_Seaports <- Containers_in_Seaports %>% select(c(-2,-6))

unique_combinations <- Containers_in_Seaports %>%
  distinct(`Number of containers`,`Container`, `Type of loading`)

print(unique_combinations)


unique_combinations_list <- unique_combinations %>%
  mutate(
    table_name = paste(`Number of containers`,`Container`, `Type of loading`, sep = "_")
  )

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  assign(table_name, filter(Containers_in_Seaports, 
                            `Number of containers` == current_combination$`Number of containers`&
                              `Container` == current_combination$`Container`&
                              `Type of loading` == current_combination$`Type of loading`))
  
  print(paste("Создана таблица:", table_name))
}

combined_tables <- list()

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  current_table <- get(table_name)
  
  # Оставляем только столбцы Time и Value, а Value переименовываем на имя таблицы
  current_table_transformed <- current_table %>%
    select(Time, Value) %>%
    rename(!!paste0(table_name, "_Units") := Value)
  
  # Добавляем эту таблицу в список
  combined_tables[[table_name]] <- current_table_transformed
}

# Объединяем все таблицы по дате (Time)
Containers_in_Seaports <- reduce(combined_tables, full_join, by = "Time")

Containers_in_Seaports <- Containers_in_Seaports %>%
  filter(as.integer(substr(Time, 1, 4)) >= 1999 & as.integer(substr(Time, 1, 4)) <= 2024)


#----------------------------------------------------------------------------------------
#	Road vehicles in seaports


Road_vehicles_in_seaports <- read_csv("C:/Users/Admin/Desktop/Kursinis 2025/data-table (42).csv")
colnames(Road_vehicles_in_seaports)

Road_vehicles_in_seaports <- Road_vehicles_in_seaports %>% select(c(-2,-5))

unique_combinations <- Road_vehicles_in_seaports %>%
  distinct(`Type of transport`,`Overload type`)

print(unique_combinations)


unique_combinations_list <- unique_combinations %>%
  mutate(
    table_name = paste(`Type of transport`,`Overload type`, sep = "_")
  )

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  assign(table_name, filter(Road_vehicles_in_seaports, 
                            `Type of transport` == current_combination$`Type of transport`&
                              `Overload type` == current_combination$`Overload type`))
  
  print(paste("Создана таблица:", table_name))
}

combined_tables <- list()

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  current_table <- get(table_name)
  
  # Оставляем только столбцы Time и Value, а Value переименовываем на имя таблицы
  current_table_transformed <- current_table %>%
    select(Time, Value) %>%
    rename(!!paste0(table_name, "_Units_in_seaports") := Value)
  
  # Добавляем эту таблицу в список
  combined_tables[[table_name]] <- current_table_transformed
}

# Объединяем все таблицы по дате (Time)
Road_vehicles_in_seaports <- reduce(combined_tables, full_join, by = "Time")

Road_vehicles_in_seaports <- Road_vehicles_in_seaports %>%
  filter(as.integer(substr(Time, 1, 4)) >= 1999 & as.integer(substr(Time, 1, 4)) <= 2024)

#----------------------------------------------------------------------------------------
#	Passengers in seaports


Passengers_in_seaports <- read_csv("C:/Users/Admin/Desktop/Kursinis 2025/data-table (43).csv")
colnames(Passengers_in_seaports)

Passengers_in_seaports <- Passengers_in_seaports %>% select(c(-2,-4))

unique_combinations <- Passengers_in_seaports %>%
  distinct(`Passengers in ports`)

print(unique_combinations)


unique_combinations_list <- unique_combinations %>%
  mutate(
    table_name = paste(`Passengers in ports`, sep = "_")
  )

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  assign(table_name, filter(Passengers_in_seaports, 
                            `Passengers in ports` == current_combination$`Passengers in ports`))
  
  print(paste("Создана таблица:", table_name))
}

combined_tables <- list()

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  current_table <- get(table_name)
  
  # Оставляем только столбцы Time и Value, а Value переименовываем на имя таблицы
  current_table_transformed <- current_table %>%
    select(Time, Value) %>%
    rename(!!paste0(table_name, "_in_seaports") := Value)
  
  # Добавляем эту таблицу в список
  combined_tables[[table_name]] <- current_table_transformed
}

# Объединяем все таблицы по дате (Time)
Passengers_in_seaports <- reduce(combined_tables, full_join, by = "Time")

Passengers_in_seaports <- Passengers_in_seaports %>%
  filter(as.integer(substr(Time, 1, 4)) >= 1999 & as.integer(substr(Time, 1, 4)) <= 2024)

#----------------------------------------------------------------------------------------
#	Goods in seaports thousand tonnes


Goods_in_seaports <- read_csv("C:/Users/Admin/Desktop/Kursinis 2025/data-table (44).csv")
colnames(Goods_in_seaports)

Goods_in_seaports <- Goods_in_seaports %>% select(c(-2,-5))

unique_combinations <- Goods_in_seaports %>%
  distinct(`Group of goods`,`Overload type`)

print(unique_combinations)


unique_combinations_list <- unique_combinations %>%
  mutate(
    table_name = paste(`Group of goods`, `Overload type`,sep = "_")
  )

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  assign(table_name, filter(Goods_in_seaports, 
                            `Group of goods` == current_combination$`Group of goods`&
                             `Overload type` == current_combination$`Overload type`))
  
  print(paste("Создана таблица:", table_name))
}

combined_tables <- list()

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  current_table <- get(table_name)
  
  # Оставляем только столбцы Time и Value, а Value переименовываем на имя таблицы
  current_table_transformed <- current_table %>%
    select(Time, Value) %>%
    rename(!!paste0(table_name, "_in_seaports") := Value)
  
  # Добавляем эту таблицу в список
  combined_tables[[table_name]] <- current_table_transformed
}

# Объединяем все таблицы по дате (Time)
Goods_in_seaports <- reduce(combined_tables, full_join, by = "Time")

Goods_in_seaports <- Goods_in_seaports %>%
  filter(as.integer(substr(Time, 1, 4)) >= 1999 & as.integer(substr(Time, 1, 4)) <= 2024)



#----------------------------------------------------------------------------------------
#	Passenger-kilometres by sea thousand passenger-kilometres



Passenger_km_by_sea <- read_csv("C:/Users/Admin/Desktop/Kursinis 2025/data-table (45).csv")

Passenger_km_by_sea <- Passenger_km_by_sea %>% select(c(-2,-3)) %>% rename("Passenger_km_by_sea"=Value)


Passenger_km_by_sea <- Passenger_km_by_sea %>%
  filter(as.integer(substr(Time, 1, 4)) >= 1999 & as.integer(substr(Time, 1, 4)) <= 2024)



#----------------------------------------------------------------------------------------
#	Passenger transport by sea thousand 



Passenger_transport_by_sea <- read_csv("C:/Users/Admin/Desktop/Kursinis 2025/data-table (46).csv")

Passenger_transport_by_sea <- Passenger_transport_by_sea %>% select(c(-2,-3)) %>% rename("Passenger_transport_by_sea"=Value)


Passenger_transport_by_sea <- Passenger_transport_by_sea %>%
  filter(as.integer(substr(Time, 1, 4)) >= 1999 & as.integer(substr(Time, 1, 4)) <= 2024)




#----------------------------------------------------------------------------------------
#	Goods transport by sea thousand tonnes


Goods_transport_by_sea <- read_csv("C:/Users/Admin/Desktop/Kursinis 2025/data-table (47).csv")
colnames(Goods_transport_by_sea)

Goods_transport_by_sea <- Goods_transport_by_sea %>% select(c(-2,-4))

unique_combinations <- Goods_transport_by_sea %>%
  distinct(`Type of traffic`)

print(unique_combinations)


unique_combinations_list <- unique_combinations %>%
  mutate(
    table_name = paste(`Type of traffic`,sep = "_")
  )

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  assign(table_name, filter(Goods_transport_by_sea, 
                            `Type of traffic` == current_combination$`Type of traffic`))
  
  print(paste("Создана таблица:", table_name))
}

combined_tables <- list()

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  current_table <- get(table_name)
  
  # Оставляем только столбцы Time и Value, а Value переименовываем на имя таблицы
  current_table_transformed <- current_table %>%
    select(Time, Value) %>%
    rename(!!paste0(table_name, "_transport_by_sea") := Value)
  
  # Добавляем эту таблицу в список
  combined_tables[[table_name]] <- current_table_transformed
}

# Объединяем все таблицы по дате (Time)
Goods_transport_by_sea <- reduce(combined_tables, full_join, by = "Time")

Goods_transport_by_sea <- Goods_transport_by_sea %>%
  filter(as.integer(substr(Time, 1, 4)) >= 1999 & as.integer(substr(Time, 1, 4)) <= 2024)


#----------------------------------------------------------------------------------------
#	Vehicle-kilometres of buses thousand km


Vehicle_km_of_buses <- read_csv("C:/Users/Admin/Desktop/Kursinis 2025/data-table (48).csv")
colnames(Vehicle_km_of_buses)

Vehicle_km_of_buses <- Vehicle_km_of_buses %>% select(c(-2,-4))

unique_combinations <- Vehicle_km_of_buses %>%
  distinct(`Type of route`)

print(unique_combinations)


unique_combinations_list <- unique_combinations %>%
  mutate(
    table_name = paste(`Type of route`,sep = "_")
  )

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  assign(table_name, filter(Vehicle_km_of_buses, 
                            `Type of route` == current_combination$`Type of route`))
  
  print(paste("Создана таблица:", table_name))
}

combined_tables <- list()

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  current_table <- get(table_name)
  
  # Оставляем только столбцы Time и Value, а Value переименовываем на имя таблицы
  current_table_transformed <- current_table %>%
    select(Time, Value) %>%
    rename(!!paste0(table_name, "_Vehicle_km_of_buses") := Value)
  
  # Добавляем эту таблицу в список
  combined_tables[[table_name]] <- current_table_transformed
}

# Объединяем все таблицы по дате (Time)
Vehicle_km_of_buses <- reduce(combined_tables, full_join, by = "Time")

Vehicle_km_of_buses <- Vehicle_km_of_buses %>%
  filter(as.integer(substr(Time, 1, 4)) >= 1999 & as.integer(substr(Time, 1, 4)) <= 2024)


#----------------------------------------------------------------------------------------
#	Passenger transport by road thousand 


Passenger_transport_by_road <- read_csv("C:/Users/Admin/Desktop/Kursinis 2025/data-table (49).csv")
colnames(Passenger_transport_by_road)

Passenger_transport_by_road <- Passenger_transport_by_road %>% select(c(-2,-5))

unique_combinations <- Passenger_transport_by_road %>%
  distinct(`Type of transport`,`Type of traffic`)

print(unique_combinations)


unique_combinations_list <- unique_combinations %>%
  mutate(
    table_name = paste(`Type of transport`,`Type of traffic`,sep = "_")
  )

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  assign(table_name, filter(Passenger_transport_by_road, 
                            `Type of transport` == current_combination$`Type of transport`&
                              `Type of traffic` == current_combination$`Type of traffic`))
  
  print(paste("Создана таблица:", table_name))
}

combined_tables <- list()

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  current_table <- get(table_name)
  
  # Оставляем только столбцы Time и Value, а Value переименовываем на имя таблицы
  current_table_transformed <- current_table %>%
    select(Time, Value) %>%
    rename(!!paste0(table_name, "_Passenger_transport_by_road") := Value)
  
  # Добавляем эту таблицу в список
  combined_tables[[table_name]] <- current_table_transformed
}

# Объединяем все таблицы по дате (Time)
Passenger_transport_by_road <- reduce(combined_tables, full_join, by = "Time")

Passenger_transport_by_road <- Passenger_transport_by_road %>%
  filter(as.integer(substr(Time, 1, 4)) >= 1999 & as.integer(substr(Time, 1, 4)) <= 2024)


#----------------------------------------------------------------------------------------
#	Number of road accidents



Number_of_road_accidents <- read_csv("C:/Users/Admin/Desktop/Kursinis 2025/data-table (50).csv")

Number_of_road_accidents <- Number_of_road_accidents %>% select(c(-2,-3)) %>% rename("Number_of_road_accidents"=Value)


Number_of_road_accidents <- Number_of_road_accidents %>%
  filter(as.integer(substr(Time, 1, 4)) >= 1999 & as.integer(substr(Time, 1, 4)) <= 2024)



#----------------------------------------------------------------------------------------
#	Number of persons injured and killed in road accidents


Number_persons_inj_kil_in_road <- read_csv("C:/Users/Admin/Desktop/Kursinis 2025/data-table (51).csv")
colnames(Number_persons_inj_kil_in_road)

Number_persons_inj_kil_in_road <- Number_persons_inj_kil_in_road %>% select(c(-2,-4))

unique_combinations <- Number_persons_inj_kil_in_road %>%
  distinct(`Type of victim`)

print(unique_combinations)


unique_combinations_list <- unique_combinations %>%
  mutate(
    table_name = paste(`Type of victim`,sep = "_")
  )

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  assign(table_name, filter(Number_persons_inj_kil_in_road, 
                            `Type of victim` == current_combination$`Type of victim`))
  
  print(paste("Создана таблица:", table_name))
}

combined_tables <- list()

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  current_table <- get(table_name)
  
  # Оставляем только столбцы Time и Value, а Value переименовываем на имя таблицы
  current_table_transformed <- current_table %>%
    select(Time, Value) %>%
    rename(!!paste0(table_name, "_in_road_accidents") := Value)
  
  # Добавляем эту таблицу в список
  combined_tables[[table_name]] <- current_table_transformed
}

# Объединяем все таблицы по дате (Time)
Number_persons_inj_kil_in_road <- reduce(combined_tables, full_join, by = "Time")

Number_persons_inj_kil_in_road <- Number_persons_inj_kil_in_road %>%
  filter(as.integer(substr(Time, 1, 4)) >= 1999 & as.integer(substr(Time, 1, 4)) <= 2024)

#----------------------------------------------------------------------------------------
#	Tonne-kilometres by road


Tonne_km_by_road<- read_csv("C:/Users/Admin/Desktop/Kursinis 2025/data-table (52).csv")
colnames(Tonne_km_by_road)

Tonne_km_by_road <- Tonne_km_by_road %>% select(c(-2,-5))

unique_combinations <- Tonne_km_by_road %>%
  distinct(`Type of carriage`,`Payoff`)

print(unique_combinations)


unique_combinations_list <- unique_combinations %>%
  mutate(
    table_name = paste(`Type of carriage`,`Payoff`,sep = "_")
  )

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  assign(table_name, filter(Tonne_km_by_road, 
                            `Type of carriage` == current_combination$`Type of carriage` &
                              `Payoff` == current_combination$`Payoff`))
  
  print(paste("Создана таблица:", table_name))
}

combined_tables <- list()

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  current_table <- get(table_name)
  
  # Оставляем только столбцы Time и Value, а Value переименовываем на имя таблицы
  current_table_transformed <- current_table %>%
    select(Time, Value) %>%
    rename(!!paste0(table_name, "_by_road") := Value)
  
  # Добавляем эту таблицу в список
  combined_tables[[table_name]] <- current_table_transformed
}

# Объединяем все таблицы по дате (Time)
Tonne_km_by_road <- reduce(combined_tables, full_join, by = "Time")

Tonne_km_by_road <- Tonne_km_by_road %>%
  filter(as.integer(substr(Time, 1, 4)) >= 1999 & as.integer(substr(Time, 1, 4)) <= 2024)

#----------------------------------------------------------------------------------------
#	Tonne-kilometres of crude oil and petroleum products


Tonne_km_of_crude_oil_and_petroleum <- read_csv("C:/Users/Admin/Desktop/Kursinis 2025/data-table (54).csv")
colnames(Tonne_km_of_crude_oil_and_petroleum)

Tonne_km_of_crude_oil_and_petroleum <- Tonne_km_of_crude_oil_and_petroleum %>% select(c(-2,-5))

unique_combinations <- Tonne_km_of_crude_oil_and_petroleum %>%
  distinct(`Group of goods`,`Type of traffic`)

print(unique_combinations)


unique_combinations_list <- unique_combinations %>%
  mutate(
    table_name = paste(`Group of goods`,`Type of traffic`,sep = "_")
  )

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  assign(table_name, filter(Tonne_km_of_crude_oil_and_petroleum, 
                            `Group of goods` == current_combination$`Group of goods` &
                              `Type of traffic` == current_combination$`Type of traffic`))
  
  print(paste("Создана таблица:", table_name))
}

combined_tables <- list()

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  current_table <- get(table_name)
  
  # Оставляем только столбцы Time и Value, а Value переименовываем на имя таблицы
  current_table_transformed <- current_table %>%
    select(Time, Value) %>%
    rename(!!paste0(table_name, "_Tonne_kilometres") := Value)
  
  # Добавляем эту таблицу в список
  combined_tables[[table_name]] <- current_table_transformed
}

# Объединяем все таблицы по дате (Time)
Tonne_km_of_crude_oil_and_petroleum <- reduce(combined_tables, full_join, by = "Time")

Tonne_km_of_crude_oil_and_petroleum <- Tonne_km_of_crude_oil_and_petroleum %>%
  filter(as.integer(substr(Time, 1, 4)) >= 1999 & as.integer(substr(Time, 1, 4)) <= 2024)

#----------------------------------------------------------------------------------------
#	Passengers at airports


Passengers_at_airports <- read_csv("C:/Users/Admin/Desktop/Kursinis 2025/data-table (56).csv")
colnames(Passengers_at_airports)

Passengers_at_airports <- Passengers_at_airports %>% select(c(-2,-5))

unique_combinations <- Passengers_at_airports %>%
  distinct(`Passengers in airports`,`Type of traffic`)

print(unique_combinations)


unique_combinations_list <- unique_combinations %>%
  mutate(
    table_name = paste(`Passengers in airports`,`Type of traffic`,sep = "_")
  )

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  assign(table_name, filter(Passengers_at_airports, 
                            `Passengers in airports` == current_combination$`Passengers in airports` &
                              `Type of traffic` == current_combination$`Type of traffic`))
  
  print(paste("Создана таблица:", table_name))
}

combined_tables <- list()

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  current_table <- get(table_name)
  
  # Оставляем только столбцы Time и Value, а Value переименовываем на имя таблицы
  current_table_transformed <- current_table %>%
    select(Time, Value) %>%
    rename(!!paste0(table_name, "_Passengers_at_airports") := Value)
  
  # Добавляем эту таблицу в список
  combined_tables[[table_name]] <- current_table_transformed
}

# Объединяем все таблицы по дате (Time)
Passengers_at_airports <- reduce(combined_tables, full_join, by = "Time")

Passengers_at_airports <- Passengers_at_airports %>%
  filter(as.integer(substr(Time, 1, 4)) >= 1999 & as.integer(substr(Time, 1, 4)) <= 2024)


#----------------------------------------------------------------------------------------
#	Passenger transport by air


Passenger_transport_by_air <- read_csv("C:/Users/Admin/Desktop/Kursinis 2025/data-table (58).csv")
colnames(Passenger_transport_by_air)

Passenger_transport_by_air <- Passenger_transport_by_air %>% select(c(-2,-4))

unique_combinations <- Passenger_transport_by_air %>%
  distinct(`Kind of transport`)

print(unique_combinations)


unique_combinations_list <- unique_combinations %>%
  mutate(
    table_name = paste(`Kind of transport`,sep = "_")
  )

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  assign(table_name, filter(Passenger_transport_by_air, 
                            `Kind of transport` == current_combination$`Kind of transport`))
  
  print(paste("Создана таблица:", table_name))
}

combined_tables <- list()

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  current_table <- get(table_name)
  
  # Оставляем только столбцы Time и Value, а Value переименовываем на имя таблицы
  current_table_transformed <- current_table %>%
    select(Time, Value) %>%
    rename(!!paste0(table_name, "_Passenger_transport_by_air") := Value)
  
  # Добавляем эту таблицу в список
  combined_tables[[table_name]] <- current_table_transformed
}

# Объединяем все таблицы по дате (Time)
Passenger_transport_by_air <- reduce(combined_tables, full_join, by = "Time")

Passenger_transport_by_air <- Passenger_transport_by_air %>%
  filter(as.integer(substr(Time, 1, 4)) >= 1999 & as.integer(substr(Time, 1, 4)) <= 2024)


#----------------------------------------------------------------------------------------
#	Number of flights performed


Number_of_flights <- read_csv("C:/Users/Admin/Desktop/Kursinis 2025/data-table (59).csv")
colnames(Passenger_transport_by_air)

Number_of_flights <- Number_of_flights %>% select(c(-2,-4))

unique_combinations <- Number_of_flights %>%
  distinct(`Type of flight`)

print(unique_combinations)


unique_combinations_list <- unique_combinations %>%
  mutate(
    table_name = paste(`Type of flight`,sep = "_")
  )

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  assign(table_name, filter(Number_of_flights, 
                            `Type of flight` == current_combination$`Type of flight`))
  
  print(paste("Создана таблица:", table_name))
}

combined_tables <- list()

for (i in 1:nrow(unique_combinations_list)) {
  current_combination <- unique_combinations_list[i, ]
  table_name <- current_combination$table_name
  
  current_table <- get(table_name)
  
  # Оставляем только столбцы Time и Value, а Value переименовываем на имя таблицы
  current_table_transformed <- current_table %>%
    select(Time, Value) %>%
    rename(!!paste0(table_name, "_number") := Value)
  
  # Добавляем эту таблицу в список
  combined_tables[[table_name]] <- current_table_transformed
}

# Объединяем все таблицы по дате (Time)
Number_of_flights <- reduce(combined_tables, full_join, by = "Time")

Number_of_flights <- Number_of_flights %>%
  filter(as.integer(substr(Time, 1, 4)) >= 1999 & as.integer(substr(Time, 1, 4)) <= 2024)





#----------------------------------------------------------------------------------------
#	Passenger-kilometres by inland waterways



Passenger_km_by_inland_waterways <- read_csv("C:/Users/Admin/Desktop/Kursinis 2025/data-table (61).csv")

Passenger_km_by_inland_waterways <- Passenger_km_by_inland_waterways %>% select(c(-2,-3)) %>% rename("Passenger_km_by_inland_waterways"=Value)


Passenger_km_by_inland_waterways <- Passenger_km_by_inland_waterways %>%
  filter(as.integer(substr(Time, 1, 4)) >= 1999 & as.integer(substr(Time, 1, 4)) <= 2024)


#----------------------------------------------------------------------------------------
#	Passenger transport by inland waterways



Passenger_transport_by_inland_waterways <- read_csv("C:/Users/Admin/Desktop/Kursinis 2025/data-table (62).csv")

Passenger_transport_by_inland_waterways <- Passenger_transport_by_inland_waterways %>% select(c(-2,-3)) %>% rename("Passenger_transport_by_inland_waterways"=Value)


Passenger_transport_by_inland_waterways <- Passenger_transport_by_inland_waterways %>%
  filter(as.integer(substr(Time, 1, 4)) >= 1999 & as.integer(substr(Time, 1, 4)) <= 2024)



#----------------------------------------------------------------------------------------
#	Tonne-kilometres by inland waterways



Tonne_km_by_inland_waterways <- read_csv("C:/Users/Admin/Desktop/Kursinis 2025/data-table (63).csv")

Tonne_km_by_inland_waterways <- Tonne_km_by_inland_waterways %>% select(c(-2,-3)) %>% rename("Tonne_km_by_inland_waterways"=Value)


Tonne_km_by_inland_waterways <- Tonne_km_by_inland_waterways %>%
  filter(as.integer(substr(Time, 1, 4)) >= 1999 & as.integer(substr(Time, 1, 4)) <= 2024)


#----------------------------------------------------------------------------------------
#	Goods transport by inland waterways



Goods_transport_by_inland_waterways <- read_csv("C:/Users/Admin/Desktop/Kursinis 2025/data-table (64).csv")

Goods_transport_by_inland_waterways <- Goods_transport_by_inland_waterways %>% select(c(-2,-3)) %>% rename("Goods_transport_by_inland_waterways"=Value)


Goods_transport_by_inland_waterways <- Goods_transport_by_inland_waterways %>%
  filter(as.integer(substr(Time, 1, 4)) >= 1999 & as.integer(substr(Time, 1, 4)) <= 2024)


#----------------------------------------------------------------------------------------
#	Exports, seasonally adjusted EUR thousand




Exports <- read_delim("C:/Users/Admin/Desktop/Kursinis 2025/data-table (65_1).csv", 
                               delim = "\t", escape_double = FALSE, 
                               trim_ws = TRUE)

Exports <- Exports %>% select(c(-2,-3)) %>% rename("Exports"=Value)


Exports <- Exports %>%
  filter(grepl("^(1999|200[0-9]|201[0-9]|202[0-4])K[1-4]$", Time))




#----------------------------------------------------------------------------------------
#	Imports, seasonally adjusted EUR thousand



Imports <- read_delim("C:/Users/Admin/Desktop/Kursinis 2025/data-table (66_1).csv", 
                      delim = "\t", escape_double = FALSE, 
                      trim_ws = TRUE)


Imports <- Imports %>% select(c(-2,-3)) %>% rename("Imports"=Value)


Imports <- Imports %>%
  filter(grepl("^(1999|200[0-9]|201[0-9]|202[0-4])K[1-4]$", Time))




##############################################################################################
#FINAL TABLE

tables_list <- list(gdp_lt4,price_indx_lt, Industrial_Production, construction_input_price_ind,Employment,foreign_direct_investment,financial_accounts_of_general_government,general_government_revenue_and_expenditure,general_government_deficit_surplus,investment_in_tangible_fixed_assets,sales_of_service_enterprises,indices_of_the_number_of_persons_employed_in_service_enterprises,industrial_production,floor_area_of_dwellings_completed,floor_area_of_nonresidential_completed,turnover_of_enterprises,turnover_of_wholesale_enterprises,Unemployed,Labour_force,Activity_rate,Longterm_unemployment,Employed_persons,Inactive_persons,Labour_costs_per_hour,Basic_social_benefit,Minimum_monthly_wage,Minimum_hourly_pay,Indice_average_monthly_social_insurance_oldage_pension,Passenger_transport,Tonne_kilometres,Goods_carried,Passenger_km_by_rail,Containers_in_Seaports,Road_vehicles_in_seaports,Passengers_in_seaports,Goods_in_seaports,Passenger_km_by_sea,Passenger_transport_by_sea,Goods_transport_by_sea,Vehicle_km_of_buses,Passenger_transport_by_road,Number_of_road_accidents,Number_persons_inj_kil_in_road,Tonne_km_by_road,Tonne_km_of_crude_oil_and_petroleum,Passengers_at_airports,Passenger_transport_by_air,Number_of_flights,Passenger_km_by_inland_waterways,Passenger_transport_by_inland_waterways,Tonne_km_by_inland_waterways,Goods_transport_by_inland_waterways,Exports,Imports)




final_table <- reduce(tables_list, full_join, by = "Time")
View(final_table)

glimpse(final_table)



has_consecutive_nas <- function(column) {
  any(rle(is.na(column))$lengths[rle(is.na(column))$values] >= 2)
}

replace_single_na <- function(column) {
  na_indices <- which(is.na(column))
  
  for (i in na_indices) {
    if (i > 1 & i < length(column)) {
      column[i] <- mean(c(column[i - 1], column[i + 1]), na.rm = TRUE)
    } else if (i == 1) {
      column[i] <- column[i + 1]
    } else if (i == length(column)) {
      column[i] <- column[i - 1]
    }
  }
  
  return(column)
}

final_table <- final_table %>%
  select(where(~ !has_consecutive_nas(.))) %>% 
  mutate(across(everything(), replace_single_na)) %>% 
  select(where(~ !all(. == 0)))


write_xlsx(final_table, "C:/Users/Admin/Desktop/Kursinis 2025/final_table_long.xlsx")


###########################################################################################################
###########################################################################################################

#------------------------Darbas su jau sukurtu final table-------------------------

final_table <- read_excel("C:/Users/Admin/Desktop/Kursinis 2025/final_table_long.xlsx")


###########################################################################################################
###########################################################################################################

library(tseries)


#------------------------KPSS Test for Trend Stationarity-------------------------


bvp <- final_table$BVP
kpss.test(bvp, null="Trend")


test_stationarity_all <- function(data, output_file = "KPSS_results.xlsx") {
  results <- data.frame(Variable = character(), KPSS_p_value = numeric(), Stationary = character(), stringsAsFactors = FALSE)
  
  for (col_name in colnames(data)[-1]) {
    column <- data[[col_name]]
    
    if (is.numeric(column)) {
      test <- kpss.test(column, null = "Trend")
      p_value <- test$p.value
      
      stationary_status <- ifelse(p_value > 0.05, "Stationary", "Non-Stationary")
      
      results <- rbind(results, data.frame(Variable = col_name, KPSS_p_value = p_value, Stationary = stationary_status))
    }
  }
  
  write_xlsx(results, output_file)
  
  print(paste("Saved in", output_file))
  return(results)
}

results <- test_stationarity_all(final_table, "C:/Users/Admin/Desktop/Kursinis 2025/KPSS_results.xlsx")

head(results)


#------------------------Gauname nestacionariu stulpeliu pavadinimus-------------------------

get_non_stationary_columns <- function(results) {
  non_stationary_columns <- results$Variable[results$Stationary == "Non-Stationary"]
  return(non_stationary_columns)
}

non_stationary_columns <- get_non_stationary_columns(results)
length(non_stationary_columns)
print(non_stationary_columns)

# 591 - non-stationary


#------------------------Taisome stacionaruma-------------------------

transform_non_stationary_columns <- function(data, non_stationary_columns) {
  transformed_data <- data
  
  for (col_name in non_stationary_columns) {
    column <- data[[col_name]]
    
    if (all(column > 0, na.rm = TRUE)) {
      transformed_data[[col_name]] <- c(NA, diff(log(column)))
      message(paste("Logarithm applied to:", col_name))
    } else {
      transformed_data[[col_name]] <- c(NA, diff(column))
      message(paste("Differentiation applied to:", col_name))
    }
  }
  
  return(transformed_data)
}

transformed_table <- transform_non_stationary_columns(final_table, non_stationary_columns)
view(transformed_table)

# Tikriname kpss antra karta
results_after_repair <- test_stationarity_all(transformed_table, "C:/Users/Admin/Desktop/Kursinis 2025/KPSS_results2.xlsx")

# Gauname nestacionariu stulpeliu pavadinimus po pataisos
non_stationary_columns2 <- get_non_stationary_columns(results_after_repair)
length(non_stationary_columns2)
print(non_stationary_columns2)

# 38     o buvo 591

transformed_table <- transformed_table[-1, ] #nes del def yra na reiksmiu
View(transformed_table)



###########################################################################################################
###########################################################################################################

# URDF testas

library(urca)

test_urdf_all <- function(data, output_file = "URDF_results.xlsx") {
  results <- data.frame(Variable = character(), 
                        Test_Statistic = numeric(), 
                        Critical_Value_5pct = numeric(), 
                        Stationary = character(), 
                        stringsAsFactors = FALSE)
  
  for (col_name in colnames(data)[-1]) {
    column <- data[[col_name]]
    
    if (is.numeric(column)) {
      ur_test <- ur.df(y = column, type = 'drift', selectlags = 'AIC')
      
      test_stat <- summary(ur_test)@teststat[1]
      crit_value <- summary(ur_test)@cval[1, 2]
      
      stationary_status <- ifelse(test_stat > crit_value, 'Non-Stationary', 'Stationary')
      
      results <- rbind(results, data.frame(Variable = col_name, 
                                           Test_Statistic = test_stat, 
                                           Critical_Value_5pct = crit_value, 
                                           Stationary = stationary_status))
    }
  }
  
  write_xlsx(results, output_file)
  print(paste("Saved in", output_file))
  return(results)
}

set.seed(123)
urdf_results <- test_urdf_all(transformed_table, "C:/Users/Admin/Desktop/Kursinis 2025/URDF_results.xlsx")
non_stationary_columns_urdf <- get_non_stationary_columns(urdf_results)
length(non_stationary_columns_urdf)
print(non_stationary_columns_urdf)

# Liko 78 nestacionarus


# Vel taisome
transform_non_stationary_columns2 <- function(data, non_stationary_columns) {
  transformed_data <- data
  
  for (col_name in non_stationary_columns) {
    column <- data[[col_name]]
    
    transformed_data[[col_name]] <- c(NA, diff(column))
    message(paste("Differentiation applied to:", col_name))
  }
  
  return(transformed_data)
}

transformed_table_urdf <- transform_non_stationary_columns2(transformed_table, non_stationary_columns_urdf)

transformed_table_urdf <- transformed_table_urdf[-1, ] #nes del def yra na reiksmiu



#Tikriname dar karta
urdf_results2 <- test_urdf_all(transformed_table_urdf, "C:/Users/Admin/Desktop/Kursinis 2025/URDF_results2.xlsx")
non_stationary_columns_urdf2 <- get_non_stationary_columns(urdf_results2)
length(non_stationary_columns_urdf2)
print(non_stationary_columns_urdf2)

#Liko tik 0 non stacionary pagal udf

#Dar karta su kpss

results_after_repair_urdf_kpss <- test_stationarity_all(transformed_table_urdf, "C:/Users/Admin/Desktop/Kursinis 2025/KPSS_results_after_urdf.xlsx")

non_stationary_columns5 <- get_non_stationary_columns(results_after_repair_urdf_kpss)
length(non_stationary_columns5)
print(non_stationary_columns5)

#Liko 40 pagal kpss

# Naikinam nestacionarus stulpelius is abieju testu
columns_to_remove <- c(non_stationary_columns_urdf2, non_stationary_columns5)
stationary_table <- transformed_table_urdf %>% select(-one_of(columns_to_remove))
view(stationary_table)

#galutine patikra
urdf_results_final <- test_urdf_all(stationary_table, "C:/Users/Admin/Desktop/Kursinis 2025/URDF_results_final.xlsx")
non_stationary_columns_urdf_final <- get_non_stationary_columns(urdf_results_final)
length(non_stationary_columns_urdf_final)


kpss_results_final <- test_stationarity_all(stationary_table, "C:/Users/Admin/Desktop/Kursinis 2025/KPSS_results_final.xlsx")
non_stationary_columns_final <- get_non_stationary_columns(kpss_results_final)
length(non_stationary_columns_final)

#Gauname 0

View(stationary_table)


###########################################################################################################
###########################################################################################################

#------------------------Normalizuojam-------------------------
library(dplyr)
library(scales)

stationary_table <- stationary_table %>%
  mutate(across(3:ncol(stationary_table), ~rescale(.)))


write_xlsx(stationary_table, "C:/Users/Admin/Desktop/Kursinis 2025/stationary_table_long.xlsx")

###########################################################################################################
###########################################################################################################



library(ggplot2)


crisis_start1 <- "2007K1"
crisis_end1 <- "2008K4"

crisis_start2 <- "2020K1"
crisis_end2 <- "2020K4"

ggplot(transformed_table, aes(x = factor(Time, levels = unique(Time)), y = BVP)) +
  # 2007–2008 krizė
  annotate("rect", xmin = crisis_start1, xmax = crisis_end1, 
           ymin = -Inf, ymax = Inf, fill = "gray80", alpha = 0.3) +
  # 2020 m. COVID-19 krizė
  annotate("rect", xmin = crisis_start2, xmax = crisis_end2, 
           ymin = -Inf, ymax = Inf, fill = "gray60", alpha = 0.3) +
  
  geom_line(group = 1, color = "#1E88E5", size = 1.5, alpha = 0.8) +
  geom_point(color = "#D81B60", size = 2, alpha = 0.8) +  # sumažintas taškų dydis
  
  theme_minimal(base_family = "Arial") +
  labs(
    title = "BVP pokyčiai laikui bėgant",
    subtitle = "Pažymėti 2007–2008 m. ir 2020 m. ekonominiai nuosmukiai",
    x = "Laikas (metai ir ketvirčiai)",
    y = "BVP (logaritmuoti skirtumai)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "#333333"),
    plot.subtitle = element_text(hjust = 0.5, size = 14, color = "#666666"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 12),
    panel.grid.major = element_line(color = "#DDDDDD", linetype = "dotted"),
    panel.grid.minor = element_blank()
  ) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 4)])  # Rodo tik kas 4-tą laikotarpį



