## Содержание

- [Алгоритм ближайших соседей](#Алгоритм-ближайших-соседей)


## Алгоритм ближайших соседей
### 1NN
Алгоритм ближайшего соседа (1NN) является самым простым алгоритмом клссификации.
Для начала подбираем метрику (евклидово пространство), затем считаем расстояние от классифицируемого объекта, до объектов выборки, заносим значения в массив расстояний. На следующем шаге сортируем массив по возрастанию, и далее находим класс первого элемента массива, и относим классифицируемый объект к этому классу.

![Где картинка?](1NN.png?raw=true "Optional Title")

### KNN
Данный алгоритм классификации относит классифицируемый объект z к тому классу, к которому относится большинство из k его ближайших соседей.
Имеется некоторая выборка , состоящая из объектов x(i), i = 1, ..., l (например, выборка ирисов Фишера), и класифицируемый объект, который обозначим z. Чтобы посчитать расстояние от классифицируемого объекта до остальных точек, нужно использовать функцию расстояния(Евклидово расстояние).
Далее отсортируем объекты согласно посчитаного расстояния до объекта *z*: 
```diff
sortObjectsByDist <- function(xl, z, metricFunction = euclideanDistance)#задаем функцию расстояния
{
  l <- dim(xl)[1] 
  n <- dim(xl)[2] - 1
  distances <- matrix(NA, l, 2) #задаем матрицу расстояния
  for (i in 1:l) 
  {
    distances[i,] <- c(i, metricFunction(xl[i, 1:n], z)) ## считаем расстояние от классифицируемой точки до остальных точек выборки
  }
  orderedXl <- xl[order(distances[, 2]),] ##сортируем согласно посчитаного расстояния
  return (orderedXl)
}
```
Применяем сам метод KNN:
```diff
kNN <- function(xl, z, k) 
{
  orderedXl <- sortObjectsByDist(xl, z, euclideanDistance)  ## Сортируем выборку согласно классифицируемого объекта
  n <- dim(orderedXl)[2] - 1 
  classes <-orderedXl[1:k, n + 1] ## Получаем классы первых k соседей
  counts <- table(classes)  ## Составляем таблицу встречаемости каждого класса
  class <- names(which.max(counts)) ## Находим класс, который доминирует среди первых соседей
  return (class) ## возвращаем класс
}
```

![Где картинка?](6NN.png?raw=true "Optional Title")

LeaveOneOut (или LOO) - простая перекрестная проверка, которая необходима, чтобы оценить при каких значениях k алгоритм knn оптимален, и на сколько он ошибается.

![Где картинка?](knn_loo.png?raw=true "Optional Title")

По сравнению с kNN, kwNN принимает во внимание не только количество соседей определенного класса, но и удаленность от классифицируемого обьекта. Выбирается k ближайших соседей. Каждому соседу присваивается вес(мера удаленности соседа от классифицируемого обьекта). Объекту присваивается класс, вес которого больше.

```diff
weightsKWNN = function(i, k)  
{  
  (k + 1 - i) / k  
}  
```

