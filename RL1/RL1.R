#Задание 1
p <- 7:4
q <- 0:3

p+q

p-q

p*q

p/q

p**q

#Задание 2
x <- as.vector(rbind(rep(0, 10), seq(2, 20, by=2)))
x

y <- 2**(0:19)
y

z <- 10**(0:4)
z

#Задание 3
sum1 <- sum(1 / (1:50 * (2:51)))

sum2 <- sum(1 / 2^(0:20))

n_terms <- 7 
numerators <- seq(4, by=3, length.out=n_terms)
denominators <- 3^(1:n_terms)
seq3 <- c(1, numerators / denominators)

sum3 <- sum(seq3)

count_greater_than_0.5 <- sum(seq3 > 0.5)

sum1
sum2
sum3
count_greater_than_0.5

#Задание 4
vec3 <-seq(3,27, by=3)
vec3

vec3[c(2,5,7)]
vec3[c(length(vec3)-1)]
vec3[-c(length(vec3)-1)]
vec3[-c(6)]
vec3[c(100)]
vec3[-c(1,length(vec3))]
vec3[vec3>4 & vec3<10]
vec3[vec3 < 4 | vec3 > 10]

#Задание по вариантам №1 (вариант 7)
country <- rep(c("France", "Italy", "Spain"), each = 5)
country

year <- rep(c(2000:2004), 3)
year

data <- data.frame(country, year)
data

#Задание по варинтам №2 (вариант 22)
m <- matrix(sample(-5:5, 100, replace = TRUE), nrow = 10, ncol = 10)
m

which(m == 0, arr.ind = TRUE)

transposed_m <- t(m)
transposed_m

transposed_m[seq(2, nrow(transposed_m), by=2), ]

#Задание по приколу
vector <- rnorm(20, mean = 0, s = 1)
vector
hist(vector)

m2 <-matrix(sample(-10:51, 400, replace = TRUE), nrow = 20, ncol = 20)
m2

row_means <- rowMeans(matrix)

row_means <- apply(mat, 1, mean)
print(row_means)
