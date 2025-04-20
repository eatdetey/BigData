library(tidyverse)
library(ggplot2)
library(psych)
library(gridExtra)
library(factoextra)
library(cluster)
library(parameters)
library(dendextend)
library(scatterplot3d)

students <- read.csv("student-mat.csv", sep = ",")


#==========================================
#==========================================
#==========================================

str(students)
head(students)

describe(students %>% select(age, Medu, Fedu, traveltime, studytime, failures, 
                             famrel, freetime, goout, Dalc, Walc, health, 
                             absences, G1, G2, G3))

table(students$school)
table(students$sex)
table(students$address)
table(students$famsize)
table(students$Pstatus)
table(students$Mjob)
table(students$Fjob)
table(students$reason)
table(students$guardian)
table(students$schoolsup)
table(students$famsup)
table(students$paid)
table(students$activities)
table(students$nursery)
table(students$higher)
table(students$internet)
table(students$romantic)

# Боксплот оценок
grades_boxplot <- students %>%
  select(G1, G2, G3) %>%
  gather(key = "Period", value = "Grade") %>%
  ggplot(aes(x = Period, y = Grade, fill = Period)) +
  geom_boxplot() +
  labs(title = "Распределение оценок по периодам", 
       x = "Период", 
       y = "Оценка") +
  theme_minimal()

# Анализ корреляции между оценками
cor(students %>% select(G1, G2, G3), method = "pearson")

# Боксплот временных характеристик
demo_boxplot <- students %>%
  select(traveltime, studytime, freetime, goout) %>%
  gather(key = "Variable", value = "Value") %>%
  ggplot(aes(x = Variable, y = Value, fill = Variable)) +
  geom_boxplot() +
  labs(title = "Распределение временных характеристик", 
       x = "Переменная", 
       y = "Значение") +
  theme_minimal()

# Боксплот социальных характеристик
social_boxplot <- students %>%
  select(famrel,Dalc, Walc) %>%
  gather(key = "Variable", value = "Value") %>%
  ggplot(aes(x = Variable, y = Value, fill = Variable)) +
  geom_boxplot() +
  labs(title = "Распределение социальных характеристик", 
       x = "Переменная", 
       y = "Значение") +
  theme_minimal()

# Дополнительный анализ: распределение оценок по полу
grades_by_sex <- students %>%
  select(sex, G1, G2, G3) %>%
  gather(key = "Period", value = "Grade", -sex) %>%
  ggplot(aes(x = Period, y = Grade, fill = sex)) +
  geom_boxplot() +
  labs(title = "Распределение оценок по полу", 
       x = "Период", 
       y = "Оценка") +
  theme_minimal()

grid.arrange(grades_boxplot, grades_by_sex, social_boxplot, demo_boxplot, ncol = 2)

#==========================================

#==========================================

#==========================================

numeric_data_NO_SCALE <- students %>% 
  select(age, Medu, Fedu, traveltime, studytime, failures, 
         famrel, freetime, goout, Dalc, Walc, health, 
         absences, G1, G2, G3)

numeric_data <- students %>% 
  select(age, Medu, Fedu, traveltime, studytime, failures, 
         famrel, freetime, goout, Dalc, Walc, health, 
         absences, G1, G2, G3) %>%
  scale()

head(numeric_data)

set.seed(123)

wss <- function(k) {
  kmeans(numeric_data, k, nstart = 10)$tot.withinss
}

# WSS для k от 1 до 10
k.values <- 1:10
wss_values <- map_dbl(k.values, wss)

#МЕТОД ЛОКТЯ

elbow_plot <- fviz_nbclust(numeric_data, kmeans, method = "wss", k.max = 15) + theme_minimal()

print(elbow_plot)

#МЕТОД СИЛУЭТА

silhouette_plot <- fviz_nbclust(numeric_data, kmeans, method = "silhouette", k.max = 15) + theme_minimal()

print(silhouette_plot)

#СТАТИСТИКА РАЗРЫВА

gap_stat <- clusGap(numeric_data, FUN = kmeans, nstart = 25, K.max = 10, B = 100)

print(gap_stat)

gap_plot <- fviz_gap_stat(gap_stat) + theme_minimal()

print(gap_plot)

#АЛГОРИТМ НА ОСНОВЕ КОНСЕНСУСА

n_clust <- n_clusters(numeric_data_NO_SCALE, package = c("easystats", "NbClust", "mclust"), fast = TRUE)

plot(n_clust)

#ПОСТРОЕНИЕ ДЕНДРОГРАММЫ

dist_matrix <- dist(numeric_data_NO_SCALE, method = "euclidean")

hc <- hclust(dist_matrix, method = "ward.D2")

dend <- as.dendrogram(hc)

par(mfrow = c(2,1))

# Визуализация с 2 кластерами
dend_2 <- color_branches(dend, k = 2, groupLabels = TRUE)
plot(dend_2, main = "Дендрограмма с 2 кластерами\n(Метод Ward.D2)", 
     xlab = "Объекты", ylab = "Высота")
rect.hclust(hc, k = 2, border = "red")

# Визуализация с 3 кластерами
dend_3 <- color_branches(dend, k = 3, groupLabels = TRUE)
plot(dend_3, main = "Дендрограмма с 3 кластерами\n(Метод Ward.D2)", 
     xlab = "Объекты", ylab = "Высота")
rect.hclust(hc, k = 3, border = c("red", "blue", "green"))

par(mfrow = c(1,1))

#КЛАСТЕРИЗАЦИЯ K-MEANS
km_res <- kmeans(numeric_data, centers = 3, nstart = 25)

fviz_cluster(km_res, data = numeric_data,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_minimal())

students_clustered <- students %>%
    mutate(cluster = as.factor(km_res$cluster))

cluster_means <- students_clustered %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean))

# Визуализация средних значений
cluster_means_long <- cluster_means %>%
  pivot_longer(-cluster, names_to = "variable", values_to = "mean_value")

ggplot(cluster_means_long, aes(x = variable, y = mean_value, fill = cluster)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(title = "Средние значения переменных по кластерам",
       x = "Переменная",
       y = "Среднее значение") +
  theme_minimal()

# БОКСПЛОТЫ ПО КЛАСТЕРАМ
p1 <- ggplot(students_clustered, aes(x = cluster, y = G3, fill = cluster)) +
  geom_boxplot() +
  labs(title = "Итоговые оценки (G3) по кластерам")

p2 <- ggplot(students_clustered, aes(x = cluster, y = absences, fill = cluster)) +
  geom_boxplot() +
  labs(title = "Пропуски по кластерам")

p3 <- ggplot(students_clustered, aes(x = cluster, y = studytime, fill = cluster)) +
  geom_boxplot() +
  labs(title = "Время учебы по кластерам")

p4 <- ggplot(students_clustered, aes(x = cluster, y = goout, fill = cluster)) +
  geom_boxplot() +
  labs(title = "Время с друзьями по кластерам")

p5 <- ggplot(students_clustered, aes(x = cluster, y = Walc, fill = cluster)) +
  geom_boxplot() +
  labs(title = "Употребление алкоголя в выходные по кластерам")

p6 <- ggplot(students_clustered, aes(x = cluster, y = Dalc, fill = cluster)) +
  geom_boxplot() +
  labs(title = "Употребление алкоголя в будние дни по кластерам")

grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 3)

# ДИАГРАММЫ РАССЕЯНИЯ
key_vars <- students_clustered %>%
  select(G1, G2, G3, absences, studytime, cluster)

pairs(key_vars[,1:5], col = key_vars$cluster, 
      pch = 19, cex = 0.6,
      main = "Матрица диаграмм рассеяния по кластерам")

#3D ВИЗУАЛИЗАЦИЯ
scatterplot3d(students_clustered$G1, 
              students_clustered$G2, 
              students_clustered$G3,
              color = as.numeric(students_clustered$cluster),
              pch = 19,
              main = "3D визуализация кластеров по оценкам",
              xlab = "G1",
              ylab = "G2",
              zlab = "G3")
legend("topright", legend = levels(students_clustered$cluster),
       col = 1:3, pch = 19)
