##Евклидово расстояние
euclideanDistance <- function(u, v) 
{ 
  sqrt(sum((u - v)^2)) 
} 
##Сортируем объекты согласно расстояния до объекта z
sortObjectByDist <- function(xl, z, metricFunction = euclideanDistance) 
{ 
  l <- dim(xl)[1] 
  n <- dim(xl)[2] - 1 
  ##Создаем матрицу расстояний
  distances <- matrix(NA, l, 2)
  
  for (i in 1:l) 
  { 
    distances[i] <- c(metricFunction(xl[i, 1:n], z)) 
  } 
  #Сортируем
  orderedXl <- xl[order(distances), ] 
  return (orderedXl) 
} 
#Применяем метод 1NN
NN<-function(xl, z) 
{ 
  ##Сортируем выборку согласно классифицируемого объекта
  orderedXl<- sortObjectByDist(xl, z) 
  n<-dim(orderedXl)[2] 
  
  classes <- orderedXl[1, n] 
  return (classes) 
} 
##Рисуем выборку
colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue") 
X<-sample(c(1:150), 25, replace=TRUE) 
xl <- iris[X, 3:5] 
plot(iris[X, 3:4], pch = 21, bg = colors[xl$Species], col = colors[xl$Species], asp = 1, 
     xlab = "äëèíà ëèñòà", ylab = "øèðèíà ëèñòà", main = "Êàðòà êëàññèôèêàöèè äëÿ ôóíöèè 1NN") 

x <- 0.6 
y <- 0 
while (x<7) 
{ 
  while (y<3) 
  { 
    z <- c(x, y) 
    class <- NN(xl, z ) 
    points(z[1], z[2], pch = 1, bg = colors[class], col=colors[class] ) 
    y <- y+0.1
  } 
  y <- 0 
  x <- x+0.1 
}

