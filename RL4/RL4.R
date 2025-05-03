library(rvest)
library(dplyr)
library(purrr)
library(stringr)

get_numbeo_data <- function(year) {
  url <- paste0("https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=", year)
  
  tryCatch({
    page <- read_html(url)
    
    table_node <- page %>% html_node("table#t2")
    
    if (is.na(table_node)) {
      message(paste("Таблица не найдена на странице за", year))
      return(NULL)
    }
    
    table_data <- table_node %>% 
      html_table(header = TRUE, fill = TRUE) %>%
      as_tibble() %>%
      mutate(across(-c(Rank, Country), as.numeric)) %>%
      rename_all(~str_remove_all(., "\\s+")) %>%
      mutate(Year = year)
    
    return(table_data)
    
  }, error = function(e) {
    message(paste("Ошибка при обработке данных за", year, ":", e$message))
    return(NULL)
  })
}

test_data <- get_numbeo_data(2021)
if (!is.null(test_data)) {
  print("Тестовый запрос успешен:")
  print(head(test_data))
  
  years <- 2014:2021
  all_data <- map_dfr(years, ~{
    Sys.sleep(3)
    get_numbeo_data(.x)
  })
  
  if (!is.null(all_data) && nrow(all_data) > 0) {
    all_data <- all_data %>% select(where(~!all(is.na(.x))))

    write.csv(all_data, "numbeo_quality_of_life_2014_2021.csv", row.names = FALSE)
    message("\nДанные успешно сохранены в файл 'numbeo_quality_of_life_2014_2021.csv'")
    message("Всего записей: ", nrow(all_data))
    message("Столбцы: ", paste(names(all_data), collapse = ", "))

    str(all_data)
  } else {
    message("Не удалось собрать данные.")
  }
} else {
  message("Тестовый запрос не удался.")
}

all_data

# ДЕСКРИПТИВНЫЙ АНАЛИЗ
cat("=== ОБЩАЯ ИНФОРМАЦИЯ ===\n")
str(all_data)
cat("\nКоличество стран:", length(unique(all_data$Country)))
cat("\nГоды в данных:", unique(all_data$Year), "\n")

cat("\n=== СВОДНАЯ СТАТИСТИКА ===\n")
numeric_cols <- sapply(all_data, is.numeric)
summary(all_data[, numeric_cols])

last_year <- max(all_data$Year)
cat("\n=== ТОП-5 СТРАН ЗА", last_year, "ГОД ===\n")
head(all_data[all_data$Year == last_year, c("Country", "QualityofLifeIndex")] %>% 
       arrange(desc(QualityofLifeIndex)), 5)

par(mfrow = c(1, 2))

boxplot(QualityofLifeIndex ~ Year, data = all_data,
        main = "Качество жизни по годам",
        xlab = "Год", ylab = "Индекс качества жизни",
        col = "lightblue", las = 2)

boxplot(SafetyIndex ~ Year, data = all_data,
        main = "Безопасность по годам",
        xlab = "Год", ylab = "Индекс безопасности",
        col = "lightgreen", las = 2)

par(mfrow = c(1, 1))

cat("\n=== СРЕДНИЕ ЗНАЧЕНИЯ ПОКАЗАТЕЛЕЙ ПО ГОДАМ ===\n")
aggregate(. ~ Year, data = all_data[, numeric_cols], mean, na.rm = TRUE)

last_year_data <- all_data[all_data$Year == last_year, numeric_cols]
cat("\n=== КОРРЕЛЯЦИЯ ПОКАЗАТЕЛЕЙ ЗА", last_year, "ГОД ===\n")
round(cor(last_year_data, use = "complete.obs"), 2)


# ВАРИАНТ 7

selected_countries <- c("Turkey", "Greece", "Egypt", "Australia", "New Zealand")
filtered_data <- all_data[all_data$Country %in% selected_countries, ]

if (nrow(filtered_data) == 0) {
  stop("Не найдены данные для указанных стран. Проверьте названия.")
} else {
  cat("Найдены данные для стран:", toString(unique(filtered_data$Country)), "\n")
}

metrics <- setdiff(names(filtered_data), c("Year", "Country", "Rank"))

par(mfrow = c(3, 3), mar = c(4, 4, 2, 1))

for (metric in metrics) {
  plot(1, type = "n", 
       xlim = range(filtered_data$Year), 
       ylim = range(filtered_data[[metric]], na.rm = TRUE),
       main = metric,
       xlab = "Year", ylab = "Index Value")
  
  colors <- c("red", "blue", "green", "orange", "purple")
  for (i in seq_along(selected_countries)) {
    country_data <- filtered_data[filtered_data$Country == selected_countries[i], ]
    lines(country_data$Year, country_data[[metric]], 
          col = colors[i], lwd = 2, type = "b")
  }
  
  if (metric == metrics[1]) {
    legend("bottomright", legend = selected_countries,
           col = colors, lwd = 2, cex = 1.2)
  }
}

#4 ЗАДАНИЕ
library(rvest)
library(dplyr)

url <- "https://ru.wikipedia.org/wiki/Список_музеев_Ростовской_области"

page <- read_html(url)

table_node <- html_node(page, "table.wikitable")

rows <- html_nodes(table_node, "tr")[-1]

museum_data <- lapply(rows, function(row) {
  columns <- html_nodes(row, "td")
  if (length(columns) >= 3) {
    name_node <- html_node(columns[2], "a")
    name <- if (!is.na(name_node)) html_text(name_node, trim = TRUE) else html_text(columns[2], trim = TRUE)
    link <- html_attr(name_node, "href")
    full_link <- if (!is.na(link)) paste0("https://ru.wikipedia.org", link) else NA
    location <- html_text(columns[3], trim = TRUE)
    
    data.frame(
      Название = name,
      Местоположение = location,
      Ссылка = full_link,
      stringsAsFactors = FALSE
    )
  } else {
    NULL
  }
})

museum_df <- bind_rows(museum_data)

write.csv(museum_df, file = "музеи_ростовской_области.csv", row.names = FALSE, fileEncoding = "Windows-1251")
