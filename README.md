## Содержание

- [Метрические алгоритмы классификации](#Метрические-алгоритмы-классификации)
	- [Алгоритм ближайших соседей](#Алгоритм-ближайших-соседей)
	- [Алгоритм взвешенных ближайших соседей](#Алгоритм-взвешенных-ближайших-соседей)
	- [Метод парзеновского окна](#Метод-парзеновского-окна)
	- [Метод потенциальных функций](#Метод-потенциальных-функций)
	- [STOLP](#STOLP)
- [Линейные алгоритмы классификации](#Линейные-алгоритмы-классификации)

# Метрические алгоритмы классификации
## Сравнение алгоритмов классификации

 <table>
  <tr>
    <th>Метод</th>
    <th>Параметры</th>
    <th>Величина ошибок</th>
  </tr>
  <tr>
    <td>KNN</td>
    <td>k=6</td>
    <td>0.033</td>
  </tr>
  <tr>
    <td>KWNN</td>
    <td>k=9</td>
    <td>0.033</td>
  </tr>
  <tr>
    <td>PW,Гауссовское ядро</td>
    <td>h=0.1</td>
    <td>0.04</td>
  </tr>
  <td>PW,ядро Епанечникова</td>
    <td>h=0.32</td>
    <td>0.04</td>
  </tr>
  </tr>
  <td>PW,Треугольное ядро</td>
    <td>h=0.32</td>
    <td>0.04</td>
  </tr>
  </tr>
  <td>PW,Квартическое ядро</td>
    <td>h=0.32</td>
    <td>0.04</td>
  </tr>
  </tr>
  <td>PW,Прямоугольное ядро</td>
    <td>h=0.32</td>
    <td>0.04</td>
  </tr>
</table>
 
## Алгоритм ближайших соседей
### 1NN
Алгоритм ближайшего соседа (1NN) является самым простым алгоритмом клссификации.
Для начала подбираем метрику (евклидово пространство), затем считаем расстояние от классифицируемого объекта, до объектов выборки, заносим значения в массив расстояний. На следующем шаге сортируем массив по возрастанию, и далее находим класс первого элемента массива, и относим классифицируемый объект к этому классу.

![Где картинка?](metric/knn&kwnn/1NN.png?raw=true "Optional Title")

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

![Где картинка?](metric/knn&kwnn/6NN.png?raw=true "Optional Title")

LeaveOneOut (или LOO) - простая перекрестная проверка, которая необходима, чтобы оценить при каких значениях k алгоритм knn оптимален, и на сколько он ошибается.

![Где картинка?](metric/knn&kwnn/knn_loo.png?raw=true "Optional Title")

## Алгоритм взвешенных ближайших соседей
### KWNN

Метод K взвешенных ближайших соседей - это метрический алгоритм классификации, основанный на оценивании сходства объектов. Классифицируемый объект относится к тому классу, которому принадлежат ближайшие к нему объекты обучающей выборки.

![Где картинка?](metric/knn&kwnn/kwnn.png?raw=true "Optional Title")

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

![Где картинка?](images/формула1.svg?raw=true "Optional Title"), где ![Где картинка?](images/формула2.svg?raw=true "Optional Title") - функция ядра.

Парзеновская оценка плотности имеет вид:
![Где картинка?](images/формула3.gif?raw=true "Optional Title")

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
![Где картинка?](images/формула4.svg?raw=true "Optional Title")

![Где картинка?](metric/PW/Прямоугольное.png?raw=true "Optional Title")

- Треугольное
![Где картинка?](images/формула5.svg?raw=true "Optional Title")

![Где картинка?](metric/PW/Треугольное.png?raw=true "Optional Title")

- Квартическое
![Где картинка?](images/формула6.svg?raw=true "Optional Title")

![Где картинка?](metric/PW/Квартическое.png?raw=true "Optional Title")

- Епанечниково
![Где картинка?](images/формула7.svg?raw=true "Optional Title")

![Где картинка?](metric/PW/Епанечниково.png?raw=true "Optional Title")

- Гауссовское

![Где картинка?](metric/PW/Гауссовское.png?raw=true "Optional Title")
Среди всех классифицированных ядер больше всего выделяется гауссовское ядро. Оно однозначно разделило классы на всей плоскости.

## Метод потенциальных функций

Метод потенциальных функций - метрический классификатор, частный случай метода ближайших соседей. Позволяет с помощью простого алгоритма оценивать вес («важность») объектов обучающей выборки при решении задачи классификации.

### Описание алгоритма
Дана обучающая выборка  и объект z, который требуется классифицировать

1. Для каждого элемента из выборки задаётся ширина окна h

2. Для каждого элемента выборки задаётся сила потенциала Y

3. Каждому объекту выборки задаётся вес функции W

4. Суммируем веса объектов одинаковых классов. Класс с наибольшим весом присваиваетс объекту z.

Метод подбора Y

1. Задаём изначально все потенциалы равными 0 и максимальное количество ошибок.

2. Выборки последовательно или случайным образом выьирается объект из выборки Xi.

3. Для Xi запускается алгоритм классификации. Если полученный в результате класс не соответствует исходному, то сила поенциала данного объекта увеличивается на 1. В ином случае повторяем предыдущие шаги.

4. Алгоритм классификации с полученными значениями потенциалов запускается для каждого объекта выборки. Подсчитываем количество ошибок.

5. Если число ошибок меньше заданного (eps), то алгоритм завершает работу. В ином случае повторяем шаги 2-6.

Алгоритм метода потенциальных функций:
```R
potentialFunction <- function(distances, potentials, h, xl, type_core) {
  l <- nrow(xl)
  n <- ncol(xl)
  classes <- xl[, n]
  weights <- table(classes) # Таблица для весов классов
  weights[1:length(weights)] <- 0 # По умолчанию все веса равны нулю
  for (i in 1:l) { # Для каждого объекта выборки
    class <- xl[i, n] # Берется его класс
    r <- distances[i] / h[i]
    weights[class] <- weights[class] + potentials[i] * type_core(r) # Считается его вес, и прибавляется к общему ввесу его класса
  }
  if (max(weights) != 0) return (names(which.max(weights))) # Если есть вес больше нуля, то вернуть класс с наибольшим весом
  return (0) # Если точка не проклассифицировалась вернуть 0
}
```
Алгоритм поиска потенциала:
```R
getPotentials <- function(xl, h, eps, type_core)
{
  l <- nrow(xl) # строки
  n <- ncol(xl) # столбцы (размерность)
  potentials <- rep(0, l)
  distances_to_points <- matrix(0, l, l)
  err <- eps + 1 # будем считать количество ошибок на выборке 
  # err = (eps + 1) для предотвращения раннего выхода из цикла
  # матрица дистанций до других точек выборки
  for (i in 1:l)
    distances_to_points[i,] <- getDistances(xl, c(xl[i, 1], xl[i, 2])) 
  # xl - выборка, xl[i] - текущая точка и её координата
  while(err > eps){
    while (TRUE) {
      # Продолжаем, пока не встретим ошибочное определение к классу
      cur <- sample(1:l, 1) # выбираем случайную точку из выборки
      class <- potentialFunction(distances_to_points[cur, ], potentials, h, xl, type_core)

      if (class != xl[cur, n]) { # если не соответсвует классу
	potentials[cur] = potentials[cur] + 1 # увеличиваем потенциал
	break
      } #if
    } #while(true)

    # считаем количество ошибок
    err <- 0
    for (i in 1:l) {
      class <- potentialFunction(distances_to_points[i, ], potentials, h, xl, type_core)
      err <- err + (class != xl[i, n])
    }
  }
  return (potentials)
}
```
Ядро Епанечникова

![Где картинка?](metric/PF/ЕпанечниковоPF.png?raw=true "Optional Title")

Ядро Гауссовское

![Где картинка?](metric/PF/ГауссовскоеPF.png?raw=true "Optional Title")

Ядро Квартическое

![Где картинка?](metric/PF/КвартическоеPF.png?raw=true "Optional Title")

Ядро Треугольное

![Где картинка?](metric/PF/ТреугольноеPF.png?raw=true "Optional Title")

Достоинства:

1.Простота реализации

2.Хранит лишь часть выборки, следовательно, экономит память

Недостатки:

1.Порождаемый алгоритм медленно сходится

2.Параметры настраиваются слишком грубо

3.При случайном подборе Y время работы может сильно отличаться

## STOLP

Алгоритм СТОЛП (STOLP) — алгоритм отбора эталонных объектов для метрического классификатора. 

Выделяют несколько выдов объектов обучения:

Эталонные — типичные представители классов. Если классифицируемый объект близок к эталону, то, скорее всего, он принадлежит тому же классу.

Неинформативные — плотно окружены другими объектами того же класса. Если их удалить из выборки, это практически не отразится на качестве классификации.

Выбросы — находятся в окружении объектов чужого класса. Как правило, их удаление только улучшает качество классификации.

Алгорим STOLP исключает из выборки выбросы и неинформативные объекты, оставляя лишь нужное количество эталонных. Таким образом улучшается качество классификации, сокращается объем данных и уменьшается время классификации объектов. Другими словами STOLP — алгоритм сжатия данных.

Вход:

![Где картинка?](images/stolp1.png?raw=true "Optional Title") - обучающая выборка;

![Где картинка?](images/stolp2.png?raw=true "Optional Title") - порог фильтрации выбросов;

![Где картинка?](images/stolp3.png?raw=true "Optional Title") - допустимая доля ошибок;

Выход:

Множество опорных объектов ![Где картинка?](images/stolp4.png?raw=true "Optional Title")

1: для всех ![Где картинка?](images/stolp5.png?raw=true "Optional Title") проверить, является ли ![Где картинка?](images/stolp14.png?raw=true "Optional Title") выбросом:

2: если ![Где картинка?](images/stolp6.png?raw=true "Optional Title") то

3: ![Где картинка?](images/stolp7.png?raw=true "Optional Title")

4: Взять по одному эталону из каждого класса:  ![Где картинка?](images/stolp8.png?raw=true "Optional Title")

5: пока ![Где картинка?](images/stolp9.png?raw=true "Optional Title")

6: Выделить множество объектов, на которых алгоритм ![Где картинка?](images/stolp10.png?aw=true "Optional Title") ошибается: ![Где картинка?](images/stolp11.png?raw=true "Optional Title")

7: если ![Где картинка?](images/stolp12.png?raw=true "Optional Title") то

8: выход

9: Присоединить к ![Где картинка?](images/stolp15.png?raw=true "Optional Title") объект с наименьшим отступом: ![Где картинка?](images/stolp13.png?raw=true "Optional Title")

Для нахождения отступов используется функция:
```R
margin = function(points,classes,point,class){
  
  Myclass = points[which(classes==class), ]
  OtherClass = points[which(classes!=class), ]
  
  MyMargin = Parzen(Myclass,point[1:2],1,FALSE)
  OtherMargin = Parzen(OtherClass,point[1:2],1,FALSE)
  
  return(MyMargin-OtherMargin)
  }
  ```
  Отступы для Парзеновского окна выглядят так:
  
  ![Где картинка?](metric/stolp/Отступы.png?raw=true "Optional Title")

Используя метод STOLP, количество эталонных объектов сократилось до 5, скорость работы метода после алгоритма заметно улучшилась.


  ![Где картинка?](metric/stolp/Stolp.png?raw=true "Optional Title")

# Линейные алгоритмы классификации

## Метод опорных векторов

В настоящее время метод опорных векторов (SVM) считается одним из лучших методов классификации.

### Линейно разделимая выборка

