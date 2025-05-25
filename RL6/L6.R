library(car)

# Чтение данных из CSV-файла
data <- read.csv("athlete_events.csv", 
                 header = TRUE, 
                 stringsAsFactors = FALSE,
                 na.strings = c("NA", ""),
                 quote = "\"")

data <- data[!duplicated(data$ID), ]

# Фильтрация по виду спорта
weightlifting <- data[data$Sport == "Weightlifting", ]

weightlifting_men <- weightlifting[weightlifting$Sex == "M", ]
weightlifting_women <- weightlifting[weightlifting$Sex == "F", ]

men <- data[data$Sex == "M", ]
women <- data[data$Sex == "F", ]

# Общая функция для получения границ
get_limits <- function(var) {
  range(na.omit(c(data[[var]], weightlifting[[var]])))
}

age_lim <- get_limits("Age")
height_lim <- get_limits("Height")
weight_lim <- get_limits("Weight")

plot_pair <- function(var, title, unit, x_lim, group, group_name, sex) {
  if (var == "Age") {
    x_lim <- c(5, 60)
  }
  
  data_group <- data[data$Sex == sex, ]
  
  group_var <- na.omit(group[[var]])
  data_var <- na.omit(data_group[[var]])

  par(mfrow = c(2, 2))
  hist(group_var, main = paste(title, "(", group_name, ")"), 
       xlab = unit, col = "lightblue", xlim = x_lim)
  hist(data_var, main = paste(title, "(все атлеты)"), 
       xlab = unit, col = "lightgreen", xlim = x_lim)
  
  boxplot(group_var, main = paste(title, "(", group_name, ")"), 
          ylab = unit, ylim = x_lim)
  boxplot(data_var, main = paste(title, "(все атлеты)"), 
          ylab = unit, ylim = x_lim)
  
  par(mfrow = c(1, 1))
}

# Дескриптивный анализ для мужчин
plot_pair("Age", "Возраст", "лет", age_lim, weightlifting_men, "тяжёлая атлетика (мужчины)", "M")
plot_pair("Height", "Рост", "см", height_lim, weightlifting_men, "тяжёлая атлетика (мужчины)", "M")
plot_pair("Weight", "Вес", "кг", weight_lim, weightlifting_men, "тяжёлая атлетика (мужчины)", "M")

cat("Мужчины:\n")
print(summary(men[, c("Age", "Height", "Weight")]))

# Дескриптивный анализ для женщин
plot_pair("Age", "Возраст", "лет", age_lim, weightlifting_women, "тяжёлая атлетика (женщины)", "F")
plot_pair("Height", "Рост", "см", height_lim, weightlifting_women, "тяжёлая атлетика (женщины)", "F")
plot_pair("Weight", "Вес", "кг", weight_lim, weightlifting_women, "тяжёлая атлетика (женщины)", "F")

cat("\nЖенщины:\n")
print(summary(women[, c("Age", "Height", "Weight")]))

# Проверка нормальности распределения и дисперсии веса (мужчины)
cat("\nShapiro-Wilk Test (вес мужчин - тяжелая атлетика):\n")
shapiro.test(na.omit(weightlifting_men$Weight))
qqPlot(na.omit(weightlifting_men$Weight), main="QQ Plot: вес мужчин")

cat("Стандартное отклонение и дисперсия (вес мужчин - тяжелая атлетика)")
sd(na.omit(weightlifting_men$Weight))
var(na.omit(weightlifting_men$Weight))

# Проверка нормальности распределения и дисперсии веса (женщины)
cat("\nShapiro-Wilk Test (вес женщин - тяжелая атлетика):\n")
shapiro.test(na.omit(weightlifting_women$Weight))
qqPlot(na.omit(weightlifting_women$Weight), main="QQ Plot: вес женщин")

cat("Стандартное отклонение и дисперсия (вес женщин - тяжелая атлетика)")
sd(na.omit(weightlifting_women$Weight))
var(na.omit(weightlifting_women$Weight))

# Проверка гипотезы о среднем весе тяжелоатлетов
# Мужчины
weights_men <- na.omit(weightlifting_men$Weight)
wilcox.test(weights_men, mu = 78, conf.int = TRUE)

# Женщины
weights_women <- na.omit(weightlifting_women$Weight)
wilcox.test(weights_women, mu = 65, conf.int = TRUE)

# Все атлеты
weights_all <- na.omit(weightlifting$Weight)
wilcox.test(weights_all, mu = 80, conf.int = TRUE)

# Проверка гипотезы о равенстве суреднего веса в двух разных видах спорта
judo_men <- data[data$Sport == "Judo" & data$Sex == "M" & !is.na(data$Weight), ]
judo_weights <- judo_men$Weight

# Проверка нормальности распределения веса (дзюдоисты)
cat("\nShapiro-Wilk Test (вес дзюдоистов):\n")
shapiro.test(judo_weights)
qqPlot(judo_weights, main="QQ Plot: вес дзюдоистов")

# Сравнение дисперсий (проверка на гомоскедастичность)
bartlett.test(list(weights_men, judo_weights))
wilcox.test(weights_men, judo_weights)

# Тест Стьюдента
t.test(weights_men, rugby_weights, var.equal = FALSE)
