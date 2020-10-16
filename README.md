## Содержание

- [Алгоритм ближайших соседей](#Алгоритм-ближайших-соседей)
- [Алгоритм взвешенных ближайших соседей](#Алгоритм-взвешенных-ближайших-соседей)
- [Метод парзеновского окна](#Метод-парзеновского-окна)

## Алгоритм ближайших соседей
### 1NN
Алгоритм ближайшего соседа (1NN) является самым простым алгоритмом клссификации.
Для начала подбираем метрику (евклидово пространство), затем считаем расстояние от классифицируемого объекта, до объектов выборки, заносим значения в массив расстояний. На следующем шаге сортируем массив по возрастанию, и далее находим класс первого элемента массива, и относим классифицируемый объект к этому классу.

![Где картинка?](1NN.png?raw=true "Optional Title")

### KNN
Данный алгоритм классификации относит классифицируемый объект z к тому классу, к которому относится большинство из k его ближайших соседей.
Имеется некоторая выборка , состоящая из объектов x(i), i = 1, ..., l (например, выборка ирисов Фишера), и класифицируемый объект, который обозначим z. Чтобы посчитать расстояние от классифицируемого объекта до остальных точек, нужно использовать функцию расстояния(Евклидово расстояние).
Далее отсортируем объекты согласно посчитаного расстояния до объекта *z*: 
```R
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
```R
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

## Алгоритм взвешенных ближайших соседей
### KWNN

Метод K взвешенных ближайших соседей - это метрический алгоритм классификации, основанный на оценивании сходства объектов. Классифицируемый объект относится к тому классу, которому принадлежат ближайшие к нему объекты обучающей выборки.

![Где картинка?](kwnn.png?raw=true "Optional Title")

Весовая функция:
```R
weightsKWNN = function(i, k)
{
  (k + 1 - i) / k
}
```
### Сравнение качества алгоритмов kNN и kwNN
<center>
<table>
  <tbody>
    <tr>
      <th>Метод</th>
      <th>Параметры</th>
      <th>Точность</th>
    </tr>
    <tr>
      <td>KWNN</a></td>
      <td>k=9</td>
      <td>0.0333</td>
    </tr>
    <tr>
      <td>KNN</a></td>
      <td>k=6</td>
      <td>0.0333</td>
    </tr>
	  </tbody>
   </table>
kNN — один из простейших алгоритмов классификации, поэтому на реальных задачах он зачастую оказывается неэффективным. Помимо точности классификации, проблемой этого классификатора является скорость классификации: если в обучающей выборке N объектов, в тестовой выборе M объектов, и размерность пространства K, то количество операций для классификации тестовой выборки может быть оценено как O(KMN).

kwNN отличается от kNN, тем что учитывает порядок соседей классифицируемого объекта, улчшая качество классификации.

## Метод парзеновского окна

Для оценки близости объекта u к классу y алгоритм использует следующую функцию:

![Где картинка?](формула1.svg?raw=true "Optional Title"), где ![Где картинка?](формула2.svg?raw=true "Optional Title") - функция ядра.

Парзеновская оценка плотности имеет вид:
![Где картинка?](формула3.gif?raw=true "Optional Title")

Алгоритм для классифицируемой точки u строит окружность, радиусом h. Все точки, не попавшие в эту окружность, отсеиваются (кроме гауссовского). Для остальных, вычисляется вес, суммируется, и класс с наибольшим весом считается победителем.

Программная реализация алгоритма:
```R
PW = function(distances, u, h) { ##distances – расстояние от точки u до всех точек выборки
    weights = PW.kernel(distances / h)
    classes = unique(names(distances)) ##names(distances) – наименование классов точек выборки.

    weightsByClass = sapply(classes, function(class, arr) { sum(arr[names(arr) == class]) } , weights)

    if (max(weightsByClass) == 0) return("") #ни одна точка не попала в окно

    return(names(which.max(weightsByClass)))
}
```
Параметр ширины h раасчитывается с помощью Loo :
```R
LOOPW = function(points, classes, hValues) {
  n = dim(points)[1]
  loo = rep(0, length(hValues))
  
  for (i in 1:n) {
    u = points[i,]
    sample = points[-i,]
    distances = distances(sample, u)
    names(distances) = classes[-i]
    
    for (j in 1:length(hValues)) {
      h = hValues[j]
      classified = PW(distances, u, h)
      loo[j] = loo[j] + (classified != classes[i])
    }
  }
  
  loo = loo / n
}
```
Чаще всего применяются 5 типов ядер:

- Прямоугольное
![Где картинка?](формула4.svg?raw=true "Optional Title")

![Где картинка?](Прямоугольное.png?raw=true "Optional Title")

- Треугольное
![Где картинка?](формула5.svg?raw=true "Optional Title")

![Где картинка?](Треугольное.png?raw=true "Optional Title")

- Квартическое
![Где картинка?](формула6.svg?raw=true "Optional Title")

![Где картинка?](Квартическое.png?raw=true "Optional Title")

- Епанечниково
![Где картинка?](формула7.svg?raw=true "Optional Title")

![Где картинка?](Епанечниково.png?raw=true "Optional Title")

- Гауссовское

![Где картинка?](Гауссовское?raw=true "Optional Title")



