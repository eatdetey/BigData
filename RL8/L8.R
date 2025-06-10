install.packages("igraph")
install.packages("network")
install.packages("sna")
install.packages("ndtv")

library(igraph)

N <- 10

G_size_min <- N + 10
G_size_max <- (N/10 + 5)*2 + 5*N
G_size <- sample(G_size_min:G_size_max, 1)

g_ring <- make_ring(G_size, directed = FALSE, circular = TRUE)

extra_edges <- sample(1:G_size, 1)

g <- g_ring + edges(sample(1:G_size, 2 * extra_edges, replace = TRUE))

if (!is_connected(g)) {
  g <- make_connected(g)
}

cat("Число вершин графа:", vcount(g), "\n")
cat("Число рёбер графа:", ecount(g), "\n")

plot(g,
     vertex.size = 15,
     vertex.color = "lightblue",
     vertex.label.cex = 0.8,
     edge.color = "gray",
     layout = layout_in_circle,
     main = paste("Кольцевой граф\n", 
                  vcount(g), "вершин,", ecount(g), "рёбер"))

adj_matrix <- as_adjacency_matrix(g, sparse = FALSE)
print("Матрица смежности:")
print(adj_matrix)
g

############# 
############ Задача №2

library(ggplot2)
library(ggforce)
library(igraph)

build_knight_graph <- function(N) {
  positions <- expand.grid(x = 1:N, y = 1:N)
  
  moves <- matrix(c(2,1, 2,-1, -2,1, -2,-1, 1,2, 1,-2, -1,2, -1,-2), ncol = 2, byrow = TRUE)
  
  g <- make_empty_graph(n = nrow(positions), directed = TRUE)
  
  for (i in 1:nrow(positions)) {
    current_pos <- as.numeric(positions[i, ])
    new_positions <- t(current_pos + t(moves))
    valid_positions <- new_positions[new_positions[, 1] %in% 1:N & 
                                       new_positions[, 2] %in% 1:N, , drop = FALSE]
    
    for (j in 1:nrow(valid_positions)) {
      target_idx <- which(positions$x == valid_positions[j, 1] & 
                            positions$y == valid_positions[j, 2])
      g <- add_edges(g, c(i, target_idx))
    }
  }
  
  V(g)$x <- positions$x
  V(g)$y <- positions$y
  
  return(g)
}

find_knight_path <- function(N, start, end) {
  g <- build_knight_graph(N)
  
  positions <- expand.grid(x = 1:N, y = 1:N)
  start_idx <- which(positions$x == start[1] & positions$y == start[2])
  end_idx <- which(positions$x == end[1] & positions$y == end[2])
  
  shortest_path <- shortest_paths(g, from = start_idx, to = end_idx, 
                                  mode = "out", output = "both")
  
  path_vertices <- shortest_path$vpath[[1]]
  path_coords <- do.call(rbind, lapply(path_vertices, function(v) {
    c(V(g)$x[v], V(g)$y[v])
  }))
  
  return(list(
    moves = length(path_vertices) - 1,
    path = path_coords
  ))
}

visualize_chessboard <- function(N, path) {
  board <- expand.grid(x = 1:N, y = 1:N)
  board$color <- ifelse((board$x + board$y) %% 2 == 0, "white", "gray")
  
  path_df <- as.data.frame(path)
  colnames(path_df) <- c("x", "y")
  path_df$step <- 1:nrow(path_df)
  
  ggplot(board) +
    geom_tile(aes(x, y = N - y + 1, fill = color), color = "black") +
    scale_fill_identity() +
    geom_point(data = path_df, aes(x, y = N - y + 1), size = 8, color = "red") +
    geom_text(data = path_df, aes(x, y = N - y + 1, label = step), color = "black", size = 4) +
    geom_segment(
      data = path_df[-nrow(path_df), ], 
      aes(x = x, y = N - y + 1, xend = dplyr::lead(x), yend = N - dplyr::lead(y) + 1), 
      arrow = arrow(type = "closed", length = unit(0.2, "inches")), 
      color = "blue", size = 1
    ) +
    geom_point(
      aes(x, y = N - y + 1), 
      data = path_df[1, , drop = FALSE], 
      size = 8, shape = 1, color = "blue", stroke = 2
    ) +
    geom_point(
      aes(x, y = N - y + 1), 
      data = path_df[nrow(path_df), , drop = FALSE], 
      size = 8, shape = 1, color = "yellow", stroke = 2
    ) +
    scale_x_continuous(breaks = 1:N) +
    scale_y_continuous(breaks = 1:N, labels = function(y) N - y + 1) + 
    coord_fixed() +
    theme_minimal() +
    labs(
      title = "Кратчайший путь шахматного коня",
      subtitle = paste("Длина пути:", nrow(path_df) - 1, "ходов"),
      x = "", y = ""
    ) +
    theme(panel.grid.minor = element_blank())
}

input <- c(10, 4, 10, 6, 7)  # N, start_x, start_y, end_x, end_y
N <- input[1]
start <- c(input[2], input[3])
end <- c(input[4], input[5])

result <- find_knight_path(N, start, end)

cat("Минимальное количество ходов:", result$moves, "\n")
cat("Путь:\n")
print(result$path)

visualize_chessboard(N, result$path)