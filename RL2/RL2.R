data <- read.csv("Языки.csv", header = TRUE, sep = ",")
data <- data[, -1]
head(data)
ratings <- data[, -1]

summary_stats <- data.frame(
  Language = colnames(ratings),
  Max = apply(ratings, 2, max),
  Min = apply(ratings, 2, min),
  Mean = apply(ratings, 2, mean)
)

summary_stats

normalized <- ratings / 10

high_pref <- colSums(normalized > 0.7)
low_pref <- colSums(normalized < 0.3)

preferences <- data.frame(
  Language = colnames(ratings),
  High_Preference = high_pref,
  Low_Preference = low_pref
)

preferences

avg_scores <- colMeans(ratings)

ranking <- sort(avg_scores, decreasing = TRUE)
round(ranking, 2)

barplot(
  avg_scores, 
  main = "Средние оценки языков программирования", 
  col = rainbow(length(avg_scores)), 
  las = 3, 
  ylab = "Средняя оценка", 
  ylim = c(0, 10)
)

text(x = length(avg_scores) / 2, y = 9, labels = "Python получил самые высокие оценки,\nPascal — самые низкие", cex = 1, col = "black", font = 2)


##part 2
csv_data <- read.csv("Языки.csv")
csv_data
timename <- subset(csv_data, select = c(Отметка.времени, ФИО))
csv_data <- csv_data[,-1]
num_data <- csv_data[,-1]
csv_data
num_data
timename

calculate_mode <- function(x) {
  unique_x <- unique(x)
  unique_x[which.max(tabulate(match(x, unique_x)))]
}

mean_values <- colMeans(num_data)
median_values <- apply(num_data, 2, median)
min_values <- apply(num_data, 2, min)
max_values <- apply(num_data, 2, max)
sd_values <- apply(num_data, 2, sd)
mode_values <- apply(num_data, 2, calculate_mode)

analisys = data.frame(
  MEAN = mean_values,
  MEDIAN = median_values,
  MIN = min_values,
  MAX = max_values,
  SD = sd_values,
  MODE = mode_values
)
analisys

sorted_data <- csv_data[order(-csv_data[,"Python"]), ]
sorted_data

high_python <- subset(csv_data, Python > 7)
print(dim(high_python))

hist(
  csv_data[, "Python"], 
  col = "blue", 
  main = "Рис.1. Распределение оценок Python", 
  xlab = "Оценка", 
  ylab = "Количество студентов", 
  ylim = c(0, 10) 
)

text(
  x = 6, y = 9, 
  labels = "Большинство студентов поставили\nPython высокие оценки", 
  col = "black", font = 2, cex = 1
)


boxplot(
  num_data, 
  main = "Рис.3. Boxplot для оценок всех языков", 
  ylab = "Оценка языков", 
  col = "lightgreen", 
  ylim = c(0, 10)
)

hist(
  num_data,
  
)

merged_data <- merge(csv_data, timename, by = "ФИО", all.x = TRUE)
merged_data

new_row <- data.frame(Timestamp = Sys.time(), Name = "Новый студент", Python = 9, R = 8, Java = 7)
csv_data <- rbind(csv_data, new_row)


