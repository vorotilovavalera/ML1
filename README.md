## Содержание

- [Метрические алгоритмы классификации](#Метрические-алгоритмы-классификации)
	- [Алгоритм ближайших соседей](#Алгоритм-ближайших-соседей)
	- [Алгоритм взвешенных ближайших соседей](#Алгоритм-взвешенных-ближайших-соседей)
	- [Метод парзеновского окна](#Метод-парзеновского-окна)
	- [Метод потенциальных функций](#Метод-потенциальных-функций)
	- [STOLP](#STOLP)
- [Байесовские алгоритмы классификации](#Байесовские-алгоритмы-классификации)
	- [Линии уровня нормального распределения](#Линии-уровня-нормального-распределения)
	- [Наивный нормальный байесовский классификатор](#Наивный-нормальный-байесовский-классификатор)
	- [Подстановочный алгоритм (plug-in)](#Подстановочный-алгоритм)
	- [Линейный дискриминант Фишера - ЛДФ](#Линейный-дискриминант-Фишера)
- [Линейные алгоритмы классификации](#Линейные-алгоритмы-классификации)
	-[Адаптивный линейный элемент(ADALINE)](#ADALINE)

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

# Байесовские алгоритмы классификации

Байесовский подход является классическим в теории распознавания образов и лежит в основе многих методов. Он опирается на теорему о том, что если плотности распределения классов известны, то алгоритм классификации, имеющий минимальную вероятность ошибок, можно выписать в явном виде.

## Линии уровня нормального распределения

Плотность n-мерного нормального распределения равна:
![Где картинка?](bayes/level/level1.png?raw=true "Optional Title"), где mu - математическое ожидание, а sigma - ковариационная матрица (должна быть симметричной, невырожденной и положительно определенной).

Покажем на графике линии уровня нормального распределения и рассчитаем плотность. Для этого рассчитаем коэффициенты уравнения кривой второго порядка:
![Где картинка?](bayes/level/level2.png?raw=true "Optional Title").

Для этого перемножим векторы и обратную матрицу. Тогда после вычислений получим следующие значения:
```R
A = d/det
  B = (-b-c)/det
  C = a/det
  D = (-2*d*mu1 + b*mu2 + c*mu2)/det
  E = (b*mu1 + c*mu1 - 2*a*mu2)/det
  f = (d*mu1*mu1-b*mu1*mu2-c*mu1*mu2+a*mu2*mu2)/det
```
Воспользуемся полученными формулами для нахождения плотности и построим график.

1. Если признаки некореллированы, то плотности распределения имеют форму эллипсоидов, параллельных осям координат:

![Где картинка?](bayes/level/Некоррелированы1.png?raw=true "Optional Title").

Матрица ковариации:
![Где картинка?](bayes/level/level3.png?raw=true "Optional Title").

![Где картинка?](bayes/level/Некоррелированы2.png?raw=true "Optional Title").

Матрица ковариации:
![Где картинка?](bayes/level/level4.png?raw=true "Optional Title").

2. Если признаки имеют одинаковые дисперсии , линии уровня имеют форму эллипсоидов:
![Где картинка?](bayes/level/Одинаковая_дисперсия.png?raw=true "Optional Title").

Матрица ковариации:
![Где картинка?](bayes/level/level5.png?raw=true "Optional Title").

3. Если матрица не диагональна, то линии уровня – эллипсоиды, повернутые относительно оси координат:
![Где картинка?](bayes/level/коррелированы.png?raw=true "Optional Title").

Матрица ковариации:
![Где картинка?](bayes/level/level6.png?raw=true "Optional Title").

## Наивный нормальный байесовский классификатор

Наивный Байесовский классификатор один из самых простых из алгоритмов классификации. Тем не менее, очень часто он работает не хуже, а то и лучше более сложных алгоритмов. Определить класс объекта мы можем по формуле:
![Где картинка?](bayes/naive/naive1.gif?raw=true "Optional Title"), плотность для которой мы считаем, применяя формулу: ![Где картинка?](bayes/naive/naive2.gif?raw=true "Optional Title").

Алгоритм состоит в том, что мы получаем значения мат. ожидания и дисперсии(мю и сигма) и строим по ним выборку с нормальным распределением, для которой потом восстанавливаем мю и сигма. Затем классифицируем точки по формуле выше и можем строить карту классификации.

Пример работы программы:

![Где картинка?](bayes/naive/Наивный.png?raw=true "Optional Title")

Преимущества:

1. Простота реализации.
	
2. Низкие вычислительные затраты при обучении и классификации.
	
3. В тех редких случаях, когда признаки (почти) независимы, наивный байесовский классификатор (почти) оптимален.

Недостатки:

• Низкое качество классификации. Он используется либо как эталон при экспериментальном сравнении алгоритмов, либо как элементарный «строительный блок» при алгоритмических композициях.

## Подстановочный алгоритм

Нормальный дискриминантный анализ - это специальный случай байесовской классификации, предполагающий, что плотности всех классов являются многомерными нормальными.
Случайная величина x имеет многомерное нормальное распределение, если ее плотность задается выражением:
![Где картинка?](bayes/plug-in/plug1.gif?raw=true "Optional Title")

Алгоритм заключается в том, чтобы найти неизвестные параметры 𝜇 и 𝛴 для каждого класса y и подставить их в формулу оптимального байесовского классификатора. В отличие от линейного дискриминанта Фишера(ЛДФ), в данном алгоритме мы предполагаем, что ковариационные матрицы не равны. Оценка параметров нормального распределения производится на основе параметров функций правдоподобия:
![Где картинка?](bayes/plug-in/plug2.gif?raw=true "Optional Title")
![Где картинка?](bayes/plug-in/plug3.gif?raw=true "Optional Title")

Выбирая различные матрицы ковариации и центры для генерации тестовых данных, будем получать различные виды дискриминантной функции:

1. Дискриминантная функфия - эллипс.

![Где картинка?](bayes/plug-in/эллипс.png?raw=true "Optional Title")

2. Дискриминантная функфия - гипербола.

![Где картинка?](bayes/plug-in/гипербола.png?raw=true "Optional Title")

3. Дискриминантная функфия - парабола.

![Где картинка?](bayes/plug-in/парабола.png?raw=true "Optional Title")

## Линейный дискриминант Фишера 

Алгоритм ЛДФ отличается от подстановочного алгоритма тем, что ковариационые матрицы классов равны, поэтому для их восстановления необходимо использовать все объекты выборки. В этом случае разделяющая кривая вырождается в прямую.
Если оценить неизвестную 𝛴(ковариационная матрица, то есть их равенство), с учетом смещенности, то получим следующую формулу:
![Где картинка?](bayes/LDF/ldf1.gif?raw=true "Optional Title")

Восстановление ковариационных матриц в коде алгоритма:

```R
  for (i in 1:rows1){
        sigma = sigma + (t(points1[i,] - mu1) %*% (points1[i,] - mu1))
	}

    for (i in 1:rows2){
        sigma = sigma + (t(points2[i,] - mu2) %*% (points2[i,] - mu2))
	}
```

Разделяющая плоскость здается формулой: ![Где картинка?](bayes/LDF/ldf2.svg?raw=true "Optional Title")
коэффициенты которой находятся следующим образом: ![Где картинка?](bayes/LDF/ldf3.svg?raw=true "Optional Title")

![Где картинка?](bayes/LDF/ldf4.svg?raw=true "Optional Title")

Программная реализация данной функции нахождения коэффициентов ЛДФ выглядит следующим образом:

```R
inverseSigma <- solve(Sigma)
alpha <- inverseSigma %*% t(mu1 - mu2)
beta <- (mu1 %*% inverseSigma %*% t(mu1) - mu2 %*% inverseSigma %*% t(mu2)) / 2
```
Результат работы алгоритма выглядит следующим образом:

![Где картинка?](bayes/LDF/ldf.png?raw=true "Optional Title")

Можно сравнить результаты работы алгоритмов с одинаковыми параметрами:

```R
Sigma1 <- matrix(c(2, 0, 0, 2), 2, 2)
Sigma2 <- matrix(c(2, 0, 0, 2), 2, 2) 
```

Количество элементов в каждом классе: 50.
1.Подстановочный алгоритм.

![Где картинка?](bayes/LDF/plug_ldf.png?raw=true "Optional Title")

2. ЛДФ алгоритм

![Где картинка?](bayes/LDF/ldf50.png?raw=true "Optional Title")

Видим, что превосходство ЛДФ очевидно. При малом количестве элементов в каждом классе ЛДФ превосходит подстановочный алгоритм. Чем меньше элементов, тем хуже работает подстановочный алгоритм.

# Линейные алгоритмы классификации

Пусть ![Где картинка?](linear/l1.gif?raw=true "Optional Title"), тогда алгоритм ![Где картинка?](linear/l2.gif?raw=true "Optional Title") называется линейным алгоритмом.
В данном пространстве классы разделяет гиперплоскость, которая задается уравнением: ![Где картинка?](linear/l3.svg?raw=true "Optional Title"). Если x находится по одну сторону гиперплоскости с её направляющим вектором w, то объект x относится к классу +1, в противном случае - к классу -1.

Эмпирический риск представлен следующей формулой: ![Где картинка?](linear/l4.gif?raw=true "Optional Title"). Для того, чтобы минимизировать его и подобрать оптимальный вектор весов w, рекомендуется пользоваться методом стохастического градиента.

Существует величина ![Где картинка?](linear/l5.svg?raw=true "Optional Title"), которая называется отступом объекта относительно алгоритма клссификации. Если данный отступ отрицательный, тогда алгоритм совершает ошибку.

## Метод стохастического градиента для линейных алгоритмов.

Пусть дана обучающая выборка: множество входных значений X и множество выходящих значений Y, такие что каждому входу xj соответствует yj - выход, j = 1..m.
Нужно найти вектор параметром w, при котором достигается минимум эмпирического риска.
Для этого применим метод градиентного спуска:

1.выберем приближенное значение для вектора параметров w;

2.запускаем итерационный процесс, на каждом шаге которого сдвигаемся в сторону, противоположную вектору градиента 𝑄′(𝑤, 𝑋ℓ)) до тех пор, пока вектор весов 𝑤 не перестанет изменяться, причем вычисления градиента производится не на всех объектах обучения, а выбирается случайный объект (отсюда и название метода «стохастический»), на основе которого и происходят вычисления.

В зависимости от функции потерь, которая используется в функционале эмпирического риска, будем получать различные линейные алгоритмы классификации.
Рассмотрим только сам итерационный процесс стохастического градиента.

Для начала нужно инициализировть веса * w_j; j = 0,..., n* и начальную оценку функционала Q, запускаем итерационный процесс:

```R
# стандартная инициализация весов w
  w <- c(1/2, 1/2, 1/2)

  iterCount <- 0
  # определяем Q
  Q <- 0
  for (i in 1:l) {
    wx <- sum(w * xl[i, 1:n])
    margin <- wx * xl[i, n + 1]
    Q <- Q + lossQuad(margin)
  }
  repeat {

  margins <- array(dim = l)
   for (i in 1:l)
    {
     xi <- xl[i, 1:n]
     yi <- xl[i, n + 1]
     margins[i] <- crossprod(w, xi) * yi ##находим отступ
    }

errorIndexes <- which(margins <= 0) ##инициализируем объекты, на которых выполняется ошибка
    
if (length(errorIndexes) > 0)
{
    # случайным образом выбираем ошибочный объект
    i <- sample(1:l, 1)
    iterCount <- iterCount + 1
    xi <- xl[i, 1:n]
    yi <- xl[i, n + 1]
    
    wx <- crossprod(w, xi)
    margin <- wx * yi
    ex <- lossQuad(margin)
    w <- w - eta * (wx - yi) * xi ##расчитываем градиентный шаг для итерации (здесь правило обновления весов представлено для ADALINE)
    
    Qprev <- Q
    Q <- (1 - lambda) * Q + lambda * ex ##обновляем Q
  
    } else
  {
      break
    }
  }
  return(w) 
}
```

Значение функционала Q: ![Где картинка?](linear/l6.gif?raw=true "Optional Title")

Повторять все, что вычисляется после инициализации веса и начального значения Q пока значение Q не стабилизируется и/или веса w не перестанут изменяться.
Ниже я рассмотрела примеры работы метода стохастического градиента для линейных алгоритмов(ADALINE, Персептрон Розенблатта и Логистическая регрессия), изменяя лишь функции потерь и правила обновления весов.

## ADALINE

![Где картинка?](linear/l7.svg?raw=true "Optional Title") - это квадратичная функция потерь.

![Где картинка?](linear/l8.svg?raw=true "Optional Title")- правило обновления весов на каждом шаге итерации метода стохастического градиента. Данное правило получено путем дифференцирования квадратичной функции.

Программная реализация квадратичной функции потерь:
```R
lossQuad <- function(x)
{
return ((x-1)^2)
} 
```

Правило обновления весов:
```R
w <- w - eta * (wx - yi) * xi
```
Работа алгоритма:

![Где картинка?](linear/adaline.png?raw=true "Optional Title")

## Метод опорных векторов
 
В настоящее время метод опорных векторов (SVM) считается одним из лучших методов классификации.

### Линейно разделимая выборка

Пусть ![Где картинка?](images/svm1.png?raw=true "Optional Title"). Рассмотрим линейный классификатор: ![Где картинка?](images/svm2.png?raw=true "Optional Title") с параметрами ![Где картинка?](images/svm3.png?raw=true "Optional Title") и ![Где картинка?](images/svm4.png?raw=true "Optional Title").

Пусть выборка линейно разделима и существуют значения параметров ![Где картинка?](images/svm5.png?raw=true "Optional Title") и ![Где картинка?](images/svm6.png?raw=true "Optional Title"), при которых функционал числа ошибок ![Где картинка?](images/svm8.png?raw=true "Optional Title") принимает нулевое значение. Но тогда разделяющая гиперплоскость не единственна. Будем выбирать ее таким образом, чтобы она отстояла максимально далеко от ближайших к ней точек обоих классов

Заметим, что алгоритм ![Где картинка?](images/svm7.png?raw=true "Optional Title") не изменится, если ![Где картинка?](images/svm5.png?raw=true "Optional Title") и ![Где картинка?](images/svm6.png?raw=true "Optional Title") одновременно умножить на одну и ту же константу. Выберем эту константу так, чтобы выполнялось условие: ![Где картинка?](images/svm9.png?raw=true "Optional Title")

В случае линейно разделимой выборки получим задачу квадратичного программирования: ![Где картинка?](images/svm10.png?raw=true "Optional Title")
