library(readxl)

# 1. Загрузка и подготовка данных
gen_results <- read_excel("turk_athletics_genders.xlsx")
colnames(gen_results) <- c("Year", "Gold", "Silver", "Bronze", "Place4", "Place5", "Place6", "Place7", "Place8", "Gender")

# 2. Объединение всех призовых мест (1–8)
gen_results$Total <- rowSums(gen_results[, c("Gold", "Silver", "Bronze", "Place4", "Place5", "Place6", "Place7", "Place8")])

# 3. Общая статистика по всем местам 1–8
gen_summary <- aggregate(. ~ Year, data = gen_results[, c("Year", "Gold", "Silver", "Bronze", "Place4", "Place5", "Place6", "Place7", "Place8")], sum)
rownames(gen_summary) <- gen_summary$Year
gen_summary$Year <- NULL

# Дискриптивный анализ
gen_summary

boxplot(
  gen_summary, 
  main = "Boxplot для достижений Турции в тяжелой атлетике", 
  ylab = "Кол-во медалей", 
  col = "lightgreen"
)

calculate_mode <- function(x) {
  unique_x <- unique(x)
  unique_x[which.max(tabulate(match(x, unique_x)))]
}

mean_values <- colMeans(gen_summary)
median_values <- apply(gen_summary, 2, median)
min_values <- apply(gen_summary, 2, min)
max_values <- apply(gen_summary, 2, max)
sd_values <- apply(gen_summary, 2, sd)
mode_values <- apply(gen_summary, 2, calculate_mode)

analisys = data.frame(
  MEAN = mean_values,
  MEDIAN = median_values,
  MIN = min_values,
  MAX = max_values,
  SD = sd_values,
  MODE = mode_values
)
analisys


# 4. Столбчатая диаграмма
data_matrix <- t(as.matrix(gen_summary))
barplot(
  data_matrix,
  beside = TRUE,
  col = rainbow(nrow(data_matrix)),
  names.arg = rownames(gen_summary),
  main = "Количество мест 1–8 на Олимпиадах (всего)",
  xlab = "Год",
  ylab = "Количество",
  legend.text = rownames(data_matrix),
  args.legend = list(x = "topright", bty = "n")
)

# 5. Круговая диаграмма по золотым медалям
gold_data <- gen_summary[gen_summary$Gold > 0, ]
gold_values <- gold_data$Gold
gold_years <- rownames(gold_data)

pie(
  x = gold_values,
  labels = gold_years,
  main = "Количество золотых медалей по Олимпиадам",
  col = rainbow(length(gold_values))
)
text(
  x = 0, y = -1,
  labels = "Распределение золотых медалей по годам",
  col = "black"
)

# 6. Тренд по призовым местам 1–8 для мужчин и женщин
library(dplyr)

trend_data <- gen_results %>%
  group_by(Year, Gender) %>%
  summarise(TotalMedals = sum(Total), .groups = 'drop') %>%
  tidyr::pivot_wider(names_from = Gender, values_from = TotalMedals, values_fill = 0) %>%
  arrange(Year)

# Построение графика трендов
plot(
  trend_data$Year, trend_data$М, type = "o", col = "blue",
  ylim = c(0, max(trend_data$М, trend_data$Ж)),
  xlab = "Год", ylab = "Количество мест 1–8",
  main = "Динамика призовых мест у мужчин и женщин"
)
lines(trend_data$Year, trend_data$Ж, type = "o", col = "red")
legend("topright", legend = c("Мужчины", "Женщины"), col = c("blue", "red"), lty = 1, pch = 1)

# Подпись к пикам
text_x <- 2015
text_y <- max(trend_data$М) - 1
text(
  x = text_x,
  y = text_y,
  labels = paste0("Пик у мужчин: ", max(trend_data$М), " мест\nПик у женщин: ", max(trend_data$Ж), " мест"),
  col = "black"
)

#Спортивные достижения разных стран ЗОЛОТО
all_gold <- read.csv("all_athletics_gold.csv", row.names = 1, check.names = FALSE)
years <- as.numeric(colnames(all_gold))
medal_data <- as.matrix(all_gold)
mode(medal_data) <- "numeric"

plot(NA, 
     xlim = range(years), 
     ylim = c(0, max(medal_data, na.rm = TRUE)), 
     xlab = "Год", 
     ylab = "Золотые медали",
     main = "Динамика золотых медалей по странам",
     xaxt = "n")
axis(1, at = years)

colors <- rainbow(nrow(medal_data))
for(i in 1:nrow(medal_data)) {
  lines(years, medal_data[i,], type = "b", col = colors[i], pch = 19, lwd = 2)
}

legend("topright", 
       legend = rownames(medal_data), col = colors, pch = 19, lwd = 2)

#Спортивные достижения разных стран Золото+Серебро+Бронза
all_prize <- read.csv("all_athletics.csv", row.names = 1, check.names = FALSE)
years <- as.numeric(colnames(all_prize))
medal_data <- as.matrix(all_prize)
mode(medal_data) <- "numeric"

plot(NA, 
     xlim = range(years), 
     ylim = c(0, max(medal_data, na.rm = TRUE)), 
     xlab = "Год", 
     ylab = "Количество золотых, серебряных и бронзовых медатей",
     main = "Динамика 1-3 мест по странам",
     xaxt = "n")
axis(1, at = years)

colors <- rainbow(nrow(medal_data))
for(i in 1:nrow(medal_data)) {
  lines(years, medal_data[i,], type = "b", col = colors[i], pch = 19, lwd = 2)
}

legend("topright", 
       legend = rownames(medal_data), col = colors, pch = 19, lwd = 2)

#Динамика медалей у мужчин и женщин в тяжелой атлетике Турции
data <- read.csv("turk_athletics_genders.csv", na.strings = c("", " "))
data <- data[complete.cases(data), ]

men <- data[data$Пол == "М", ]
women <- data[data$Пол == "Ж", ]
total_men <- sum(men$Золото + men$Серебро + men$Бронза)
total_women <- sum(women$Золото + women$Серебро + women$Бронза)

par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

### 1. Линейный график (динамика по годам)
men_total <- men$Золото + men$Серебро + men$Бронза
women_total <- women$Золото + women$Серебро + women$Бронза

plot(NA, xlim = range(data$Год), ylim = c(0, max(c(men_total, women_total)) + 1),
     main = "Динамика медалей по годам", xlab = "Год", ylab = "Медали")
lines(men$Год, men_total, type = "o", col = "blue", pch = 19)
lines(women$Год, women_total, type = "o", col = "red", pch = 19)
legend("topright", legend = c("Мужчины", "Женщины"), 
       col = c("blue", "red"), pch = 19)

### 3. Круговая диаграмма (доля медалей)
pie(c(total_men, total_women), labels = c("Мужчины", "Женщины"), 
    col = c("blue", "red"), main = "Доля медалей по полу")

### 4. Столбчатая диаграмма по типам медалей
medal_types <- c(sum(data$Золото), sum(data$Серебро), sum(data$Бронза))
barplot(medal_types, names.arg = c("Золото", "Серебро", "Бронза"),
        col = c("gold", "grey", "brown"), main = "Медали по типам")

par(mfrow = c(1, 1))

men <- data[data$Пол == "М", ]
women <- data[data$Пол == "Ж", ]

create_medal_matrix <- function(gender_data) (
  matrix(
    c(gender_data$Золото, 
      gender_data$Серебро, 
      gender_data$Бронза,
      gender_data$X4,
      gender_data$X5,
      gender_data$X6,
      gender_data$X7,
      gender_data$X8),
    nrow = 8,
    byrow = TRUE,
    dimnames = list(c("Золото", "Серебро", "Бронза", "4-е", "5-е", "6-е", "7-е", "8-е"), 
                    unique(data$Год))
  )
)

men_matrix <- create_medal_matrix(men)
women_matrix <- create_medal_matrix(women)

par(mfrow = c(2, 2), mar = c(4, 4, 3, 1), oma = c(0, 0, 2, 0))
### 1. Общее количество мест (1-8) по полу
total_men <- rowSums(men_matrix)
total_women <- rowSums(women_matrix)

barplot(rbind(total_men, total_women),
        beside = TRUE,
        col = c("blue", "pink"),
        main = "Общее количество мест (1-8)",
        xlab = "Тип места",
        ylab = "Количество",
        names.arg = rownames(men_matrix),
        legend.text = c("Мужчины", "Женщины"),
        args.legend = list(x = "topright"))

### 2. Сравнение по годам (мужчины)
barplot(men_matrix,
        beside = TRUE,
        col = rainbow(8),
        main = "Мужчины: распределение мест",
        legend.text = c(1,2,3,4,5,6,7,8),
        xlab = "Год",
        ylab = "Количество")
        

### 3. Сравнение по годам (женщины)
barplot(women_matrix,
        beside = TRUE,
        col = rainbow(8),
        main = "Женщины: распределение мест",
        legend.text = c(1,2,3,4,5,6,7,8),
        xlab = "Год",
        ylab = "Количество")

### 4. Сравнение золотых медалей
gold_comparison <- rbind(men_matrix["Золото",], women_matrix["Золото",])
barplot(gold_comparison,
        beside = TRUE,
        col = c("blue", "pink"),
        main = "Золотые медали по годам",
        xlab = "Год",
        ylab = "Количество",
        legend.text = c("Мужчины", "Женщины"),
        args.legend = list(x = "topright"))

title("Анализ выступлений Турции по тяжелой атлетике (2004-2024)", outer = TRUE)

par(mfrow = c(1, 1))
