euclideanDistance <- function(u, v) 
{
  sqrt(sum((u - v) ^ 2))
}

sortObjectsByDist <- function(xl, z, metricFunction = euclideanDistance)
{
  l <- dim(xl)[1] 
  n <- dim(xl)[2] - 1
  distances <- matrix(NA, l, 2) 
  for (i in 1:l) 
  {
    distances[i,] <- c(i, metricFunction(xl[i, 1:n], z))
  }
  orderedXl <- xl[order(distances[, 2]),] 
  return (orderedXl)
}
kNN <- function(xl, z, k) 
{
  orderedXl <- sortObjectsByDist(xl, z, euclideanDistance) 
  n <- dim(orderedXl)[2] - 1 
  classes <-orderedXl[1:k, n + 1] 
  counts <- table(classes)
  class <- names(which.max(counts)) 
  return (class)
}
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], asp = 1,
     xlab = "Длина Листа", ylab = "Ширина Листа", main = "Карта классификации для алгоритма 6NN")
xl <-iris[, 3:5] 
x <- 0.6 
y <- 0 
while (x<7) 
{ 
  while (y<3) 
  { 
    z <- c(x, y) 
    class <-kNN(xl, z, k = 6) 
    points(z[1], z[2], pch = 1, bg = colors[class], col=colors[class] ) 
    y <- y+0.1
  } 
  y <- 0 
  x <- x+0.1 
}