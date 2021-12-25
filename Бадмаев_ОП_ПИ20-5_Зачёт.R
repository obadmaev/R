SUPPLY <- 1
SALE <- 2 #константы для определения продажи или поставки(пригодится например в switch)
FILE_SUPPLY <- 'in'
FILE_SALE <- 'out'#расширешния
SHOP_COUNT = 10 #количество магазинов
#setwd('C:/Users/obadm/Downloads')
getwd()
#название нашего магазина
SHOP_NAME = 'Магазикс'
dir.create(SHOP_NAME)
setwd(SHOP_NAME)

GOODS <- list(
  list(name = 'Молоко, уп.', min = 600, max = 800, p_supply = 5500, p_sale = 8000, p_util = 400),
  list(name = 'Кефир, уп.', min = 200, max = 300, p_supply = 4600, p_sale = 6000, p_util = 300),
  list(name = 'Хлеб, шт.', min = 30, max = 50, p_supply = 200, p_sale = 340, p_util = 15),
  list(name = 'Вода, бут.', min = 400, max = 430, p_supply = 780, p_sale = 1000, p_util = 25),
  list(name = 'Соль, пачка', min = 18, max = 22, p_supply = 431, p_sale = 731, p_util = 32),
  list(name = 'Гречка, пачка', min = 50, max = 60, p_supply = 874, p_sale = 1432, p_util = 12), #список создаваемых товаров
  list(name = 'Торт, шт.', min = 20, max = 30, p_supply = 2572, p_sale = 5432, p_util = 540)
)

#функция генерации данных в текущей директории
generate.supply <-
  function(way = '', 
           file.name = 'Поставка',#имя файла
           days = 7, #кол-во дней
           goods = 'Молоко, уп.',
           dataType = SUPPLY,
           sale.level = NULL){ #sale.level Для обозначения процента проданных от полученных товаров
    
    if (way != ''){                      #проверим на существование папку
      isFoundDir <- dir.exists(way)
      if (isFoundDir == FALSE){
        dir.create(path = way, showWarnings = FALSE) #создаем папку если директория не существует
        isFoundDir <- dir.exists(way) #проверка существует ли директория после создания
        if (isFoundDir == FALSE){ #если после создания она не существует, то завершаем функцию
          print('Папка назначения не существует, создать её нельзя!')
          return(NULL)
        }
      }
    }
    if (dataType == SALE){        # если формируем файл продаж, считываем или генерим поставку
      file.in <- paste0(way, file.name,'.', FILE_SUPPLY) #названию файла поставки присваиваем название+Поставка + .in
      isFoundFile <- file.exists(file.in) #проверка существует ли файл поставок
      if (isFoundFile) { #если файл поставок существует, для генерации файла продаж нам важно знать, что есть файл поставок
        data.in <- read.table(file = file.in, #прочитываем этот файл поставок
                              header = TRUE,
                              encoding = 'UTF-8') #берём данные из только что созданного файла поставки

      } else {#если файл поставок не существует
        data.in <-
          generate.supply(way = way,
                          file.name = file.name,
                          days = days,
                          goods = goods,
                          dataType = SUPPLY) #иначе, если он не найден, то вызываем функцию и генерим заново
      }
    }
    tab1 <- data.frame('День' = 1:days) #создаем и записываем фрейм с датами
    if (dataType == SALE){ #если это продажа, то сравниваем data.in и data.out
      if (!is.null(sale.level)){ #если уровень продаж задан
        for (i in 1:length(goods)){
          tab1[i+1] <- 
            as.integer(data.in[, i+1]*sale.level[i]/100) #записываем в таблицу значение поставки*уровень продаж
        }
      } else {
        for (i in 1:length(goods)){
          tab1[i+1] <-
            sample(x = 1:goods[[i]]$max,
                   size = days,
                   replace = TRUE # иначе задаем уровень продаж случайно
            )
          colnames(x=tab1)[i+1] = goods[[i]]$name #задаем имена столбцам по наименованиям товаров
        }
      }

    } else {
      
      for (i in 1:length(goods)){
        tab1[i+1] <-
          sample(x = goods[[i]]$min:goods[[i]]$max,
                 size = days,
                 replace = TRUE #Для каждого товара генерим данные поставки
          )
        colnames(x=tab1)[i+1] = goods[[i]]$name #присваиваем имена
      }
    }
    if (dataType == SALE){
      for (i in 1:length(goods)){
        tab1[, i+1] <- 
          ifelse(tab1[, i+1] > data.in[, i+1], #ПРОВЕРКА на то что ПРОДАЖИ<ПОСТАВКИ
                 data.in[, i+1], #если вдруг поставки больше, то в продажи пишем поставки
                 tab1[, i+1])
      }
    }
    ext <- switch (dataType, FILE_SUPPLY, FILE_SALE) #определяем расширение в зависимости от dataType
    write.table(
      x = tab1,
      file = paste0(way, file.name, '.', ext), #создаем таблицу с нашими данными
      col.names = TRUE,
      row.names = FALSE,
      fileEncoding = 'UTF-8'
    )
    return(tab1) #возвращаем её
  }

#функция генерации файла с ценами
generate.price <- function(goods){ #функция для генерации файла с ценами
  empty.raw <- rep(NA, length(goods)) #пустая строка созданная за счет повторения товаров
  price.frame <- data.frame('цена поставки' = empty.raw, #обьект датафрейма, там будет цена поставки
                            'цена продажи' = empty.raw,
                            'цена утилизации' = empty.raw)
  for (i in 1:length(goods)){ #проходимся по списку товаров
    price.frame[i,1] <- goods[[i]]$p_supply #записываем цену поставки в первый столбец
    price.frame[i,2] <- goods[[i]]$p_sale #записываем цену продажи во второй столбец
    price.frame[i,3] <- goods[[i]]$p_util #записываем цену утилизации
    row.names(price.frame)[i] <- goods[[i]]$name #записываем имена товаров
  }
  write.table(x = price.frame,                    #записали таблицу с ценами
              file = paste0('Анализ/Цены.txt'),
              col.names = TRUE,
              row.names = TRUE,
              fileEncoding = 'UTF-8') 
}



#функция создания файлов и папок с сгенерированными данными
generate.data <- function(goods = list(name = 'Молоко, уп.', min = 600, max = 800, p_supply = 5500, p_sale = 8000, p_util = 400),
                          shop.count = SHOP_COUNT, #передаем товары, количество магазинов, sale.level для передачи его в generate.supply
                          sale.level = NULL,
                          days = 7){ #количество дней
  dir.create(path = 'Анализ', showWarnings = FALSE) #создаем папку Анализ куда будем перемещать данные
  generate.price(goods = goods) #файл с ценами 
  for (i in 1:shop.count){ #цикл по магазинам
    folder.name <- paste0('Магазин ', i)  #определяем путь /Магазин 1.2.3...
    way <- paste0(folder.name, '/')  #присваиваем к текущему пути
    dir.create(path = way, showWarnings = FALSE) #создаем папку
    isFoundDir <- dir.exists(way)      #если папка не создалась
    if (isFoundDir == FALSE){ #вернуть что не можем создать
      print('Папка назначения не существует, создать её нельзя!')
      return(NULL)
    }
    
    for (dataType in c(SUPPLY, SALE)){ #цикл по расширениям для переноса созданных файлов в папку анализа
      generate.supply(way = way, #передаем путь, имя файла, дни, товары и тд
                      file.name = SHOP_NAME,
                      days = days,
                      goods = goods,
                      dataType = dataType,
                      sale.level = sale.level #создаем файл
      )
      ext <- switch (dataType, FILE_SUPPLY, FILE_SALE) #расширение получаем
      file_name = paste0(SHOP_NAME,'.',ext) #имя файла имя_магазина+.расширение
      from <- paste0(way, file_name) #откуда переносим файлы
      to <- paste0('Анализ/', 'Магазин', i, '_', file_name) #куда переносим файлы
      isFoundFile <- file.exists(to)  #существует ли файл назначения
      if (isFoundFile){ #если он существует, то удалим его
        file.remove(to) 
      }
      file.copy(from, to) #перенос
    }
    
  }
}






generate.data(goods = GOODS) #сгенерировали данные


tab.layout <- function(){ #генерирует дата фрейм
  rev <- rep(NA, SHOP_COUNT+2)
  returned <- data.frame('Выручка, руб.' = rev, #название столбца ему пустой массив
                         'Прибыль, руб' = rev, 
                         'Реализация, шт' = rev,
                         'Списание, шт' = rev,
                         'Равномерность продаж' = rev,
                         'Продажи макс' = rev,
                         'День макс продаж' = rev,
                         'Продажи мин' = rev,
                         'День мин продаж' = rev,
                         'Списание макс' = rev,
                         'День макс списания' = rev
  )
  rows.init <- c()
  for (i in 1:SHOP_COUNT){
    rows.init <- c(rows.init, paste0('Магазин', i)) #переменная с именами строк магазин 1 2
  }
  rows.init <- c(rows.init, 'Итого', 'Среднее') #добавим Итого и Среднее
  row.names(returned) <- rows.init #имена строк задаем
  return(returned) #возвращаем табличку
}

#вспомогательная фукнция для записи в файл
goods.lst.write <- function(goods.lst){ #
  goods.names <- names(goods.lst)
  for (i in 1:length(goods.lst)){
    write.table(x = goods.lst[[i]],
                file = paste0('Анализ/', goods.names[i], '.csv'), #запись в файл формата csv
                col.names = TRUE,
                row.names = TRUE,
                sep = ';',
                dec = ',',
                fileEncoding = 'UTF-8')
  }
}

goods.lst.sum.write <- function(goods.lst){
  goods.names <- names(goods.lst)
  goods.result <- goods.lst[goods.names[1]]
 # for (i in 2:length(goods.lst)){
  #  print('Разделение')
  #  goods.result <- goods.result + goods.lst[goods.names[i]]
  #  print(goods.result)
 # cdt <- read.csv(paste0('Анализ/',goods.names[i], '.csv'))
 # print(cdt)
  
  #write.table(x = goods.result[1],
   #           file = paste0('Анализ/Сумма_по_магазинам.csv'), #запись в файл формата csv
     #         col.names = TRUE,
      #        row.names = TRUE,
      #        sep = ';',
       #       dec = ',',
        #      fileEncoding = 'UTF-8')

  for (i in 1:length(goods.lst)){
    now <-  read.table(file = paste0('Анализ/', goods.names[i], '.csv'),
               header = TRUE,
               encoding = 'UTF-8')
    print(now)
    print('смотрим')
  }
}
  


data.reader <- function(){ #собирает информацию по магазинам
  data.lst <- list()
  for (j in 1:SHOP_COUNT){
    shop.in <-   #файл с поставкой
      read.table(file = paste0('Анализ/Магазин',j,"_",SHOP_NAME,'.in'), #чтение таблицы 
                 header = TRUE,
                 encoding = 'UTF-8'
      )
    
    colnames(shop.in)[colnames(shop.in) == c('День','Молоко..уп.','Кефир..уп.','Хлеб..шт.','Вода..бут.','Соль..пачка','Гречка..пачка', 'Торт..шт.')] <- c('День недели','Молоко, уп.','Кефир, уп.','Хлеб, шт.','Вода, бут.','Соль, пачка','Гречка, пачка','Торт, шт.')
    shop.out <-  #файл с продажой
      read.table(file = paste0('Анализ/Магазин',j,"_",SHOP_NAME,'.out'),                                             #переводим в нужный вид
                 header = TRUE,
                 encoding = 'UTF-8'
      )
    colnames(shop.out)[colnames(shop.out) == c('День','Молоко..уп.','Кефир..уп.','Хлеб..шт.','Вода..бут.','Соль..пачка','Гречка..пачка', 'Торт..шт.')] <- c('День недели','Молоко, уп.','Кефир, уп.','Хлеб, шт.','Вода, бут.','Соль, пачка','Гречка, пачка','Торт, шт.')
    shop.out <-  #файл с продажой
    data.lst[[j]] <- list('in' = shop.in, 'out' = shop.out) #лист по магазинам, в каждом магазине еще лист из двух элементов файл поставок файл продаж
    names(data.lst)[[j]] <- paste0('Магазин ', j) #
    
  }
  return(data.lst) #возвращаем этот лист
}



create.sum.tables.csv <- function(data.lst, price.data){ #параметры в price.data записываем цены, а в data.lst рез=тат функции data reader
  mytable <- tab.layout() #записываем пустой макет строки
  for (j in 1:length(data.lst)){ #обходим по магазину
    shop.in <- data.lst[[j]]$'in' #считываем файлы поставок и продаж
    shop.out <- data.lst[[j]]$'out'
    TR <- 0 
    TC <- 0
    sale.items <- 0
    util.items <- 0
    for (i in 1:(length(shop.in)-1)){
      p.supply <- price.data[names(shop.in)[i+1], 'цена.поставки'] #из shop.in i+1 тк первый столбец - дата
      p.sale <- price.data[names(shop.in)[i+1], 'цена.продажи'] #
      p.util <- price.data[names(shop.in)[i+1], 'цена.утилизации']
      Q_supply <- sum(shop.in[,i+1]) #поставлнные ящики
      Q_sale <- sum(shop.out[,i+1]) #проданные ящики
      Q_util <- Q_supply - Q_sale #утил ящики
      TR <- TR + Q_sale*p.sale 
      TC <- TC + Q_util*p.util + Q_supply*p.supply #считаем сумму для счетчиков
      sale.items <- sale.items + Q_sale 
      util.items <- util.items + Q_util 
    }
    Pr <- TR- TC #расчитываем профит выручка-расходы
    sale.days <- c() #обьем продаж по дням 
    util.days <- c() #обьем утил по  дням
    for (k in 1:length(unlist(shop.in[1]))){ #нужно узнать какой обьем был продан по дням
      sale.day <- 0
      util.day <- sum(shop.in[k, 2:7]) - sum(shop.out[k, 2:7])  #поставки-продажи
      for (i in 1:(length(shop.in)-1)){
        p.sale <- price.data[names(shop.in)[i+1], 'цена.продажи'] #по каждому товару считаем на сколько продано
        sale.day <- sale.day + shop.out[k,i]*p.sale
      }
      sale.days <- c(sale.days, sale.day) #добавляю продажи по дню и утил по дню
      util.days <- c(util.days, util.day)
    }
    sd.sale <- sd(sale.days) #среднее кв отклон
    sale.max <- max(sale.days) #расчитываем макс
    sale.max.day <- which.max(sale.days)  #индекс дня с макс
    sale.min <- min(sale.days) #мин
    sale.min.day <- which.min(sale.days) #индекс дня мин
    util.max <- max(util.days) #макс
    util.max.day <- which.max(util.days) #индекс дня
    
    row.values <- c(TR, Pr, sale.items, util.items, #создаю массив этих показателей
                    sd.sale, sale.max, sale.max.day,
                    sale.min, sale.min.day,
                    util.max, util.max.day)
    mytable[j, ] <- row.values
  }
  for (i in 1:5){ #запускаем с первые по пятый столбец
    mytable[SHOP_COUNT+1, i] <- sum(mytable[1:SHOP_COUNT, i]) #сумма с 1 по 10 строку
    mytable[SHOP_COUNT+2, i] <- mean(mytable[1:SHOP_COUNT, i]) #ср знач в 12-ую строку записали
  }
  write.table( #записываем значение в файл
    x = mytable,
    file = 'Анализ/TableForSum.csv',
    col.names = TRUE,
    row.names = TRUE,
    sep = ';',
    dec = ',',
    fileEncoding = 'UTF-8'
  )
  return(mytable)
}

#создание таблиц с расчетами по тем же показателям, только для каждого товара в отдельности
create.tables.csv <- function(data.lst, price.data){
  goods.lst <- list()
  for (j in 1:length(data.lst)){
    shop.in <- data.lst[[j]]$'in'
    shop.out <- data.lst[[j]]$'out'
    for (i in 1:(length(shop.in)-1)){
      if (j == 1){
        goods.lst[[i]] <- tab.layout()
        names(goods.lst)[i] <- names(shop.in)[i+1]
      }
      p.supply <- price.data[names(shop.in)[i+1], 'цена.поставки']
      p.sale <- price.data[names(shop.in)[i+1], 'цена.продажи']
      p.util <- price.data[names(shop.in)[i+1], 'цена.утилизации']
      Q_supply <- sum(shop.in[,i+1])
      Q_sale <- sum(shop.out[,i+1])
      Q_util <- Q_supply - Q_sale                                #выполнение расчетов нужных показателей
      TR <- Q_sale*p.sale
      TC <- Q_util*p.util + Q_supply*p.supply
      Pr <- TR-TC
      sd.sale <-  sd(shop.out[,i+1])
      max.sale <- max(shop.out[,i+1])
      max.day.sale <- which.max(shop.out[,i+1])
      min.sale <- min(shop.out[,i+1])
      min.day.sale <- which.min(shop.out[,i+1])
      max.util <- max(shop.in[,i+1] - shop.out[,i+1])
      max.day.util <- which.max(shop.in[,i+1] - shop.out[,i+1])
      row.values <- c(TR, Pr, Q_sale, Q_util, sd.sale, max.sale, max.day.sale, 
                      min.sale, min.day.sale, max.util, max.day.util)  #записываем в строку значения
      goods.lst[[i]][j,] <- row.values #присиваиваем массиву эту строку в его строке
    
      if (j == SHOP_COUNT){
        for (k in 1:5){
          goods.lst[[i]][SHOP_COUNT+1,k] <-  #доходим до конца и считаем среднее значение с суммой
            sum(goods.lst[[i]][1:SHOP_COUNT,k])
          
          goods.lst[[i]][SHOP_COUNT+2, k] <- 
            mean(goods.lst[[i]][1:SHOP_COUNT,k])
        }
      }
    }
  }
  goods.lst.write(goods.lst) #вызываем метод для записи всхех данных в отдельные файлы
  return(goods.lst)
}


#получение данных и их обработка

price.data <- 
  read.table(file = 'Анализ/цены.txt',
             header = TRUE,
             row.names = 1,
             encoding = 'UTF-8'
  )
data.lst <- data.reader()

create.tables.csv(data.lst = data.lst,
                  price.data = price.data) #создаем таблицы csv


create.sum.tables.csv(data.lst = data.lst,
                  price.data = price.data) #создаем таблицы csv










if (FALSE){
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
# +++++++++++++++++++++++++=ИТОГОВЫЕ ГРАФИКИ+++++++++++++++++++++++++++++++++++
  
  
  
  ###############################1#################################
#отрицательные значения возможны, если магазин работает в убыток(отрицательная прибыль и рентабельность)
{
num.shop <- 5 #задаем значение магазина
num.product <- 4 #задаем номер продукта

sale.volume <- data.lst[[num.shop]]$'out'[[num.product+1]] #значение продаж с Магазина num.shop и товара num.product
supply.volume <- data.lst[[num.shop]]$'in'[[num.product+1]] #значение поставок с магазина num.shop и товара num.product
util.volume <- supply.volume - sale.volume #утилизация
util.price <- util.volume*price.data[num.product,3] #затраты на утилизацию
sale.revenue <- sale.volume*price.data[num.product,2] #выручка с продажи
sale.profit <- sale.revenue -
  supply.volume*price.data[num.product,1] - 
  util.price  #чистая прибыль
profitability <- sale.profit/sale.revenue*100 #рентабельность
all.data <-
  list('объем продаж' = sale.volume,
       'Выручка' = sale.revenue,
       'Прибыль' = sale.profit,
       'Списание' = util.price,
       'Рентабельность' = profitability)
cls.lst = colors()[c(45,55,75, 85, 95)] #цвета
par(mfrow = c(2,3)) #определяем количество графиков
for (j in 1:length(all.data)){
  plot(all.data[[j]],
       type = "l",
       col = cls.lst[j],
       sub = names(data.lst[[num.shop]]$'out')[num.product+1], #определеяем параметры графика
       xlab = "День",
       ylab = "Показатель",
       main = names(all.data)[j]
  )
}
}

###############################                  2                  #################################

x <- 3
all.lines <-
  list('прибыль' = list(),
       'списание' = list(),
       'рентабельность' = list())


cls.lst = colors()[sample(1:length(colors()),(length(data.lst[[x]]$'in')-1) , replace = FALSE)] #определяем цвета

for (i in 1:(length(data.lst[[x]]$'in')-1)){
  
  sale.volume <- data.lst[[x]]$'out'[[i+1]] #объем продаж
  supply.volume <- data.lst[[x]]$'in'[[i+1]]#обьем поставок
  util.volume <- supply.volume - sale.volume#обьем утилизации
  util.price <- util.volume*price.data[i,3]#затраты на утилизацию
  sale.revenue <- sale.volume*price.data[i,2]#общая выручка
  sale.profit <- sale.revenue -
    supply.volume*price.data[i,1] - 
    util.price
  profitability <- sale.profit/sale.revenue*100 #рентабельность
  all.lines[['прибыль']][[names(data.lst[[x]]$'out')[i+1]]] <- sale.profit
  all.lines[['списание']][[names(data.lst[[x]]$'out')[i+1]]] <- util.volume
  all.lines[['рентабельность']][[names(data.lst[[x]]$'out')[i+1]]] <- profitability
}




par(mfrow = c(1,3))
for (j in 1:length(all.lines)){
  max_ <- 0
  min_ <- 1000000
  for (el in all.lines[[j]]){
    if (max(el)>max_){
      max_ <- max(el)
    }
    if(min(el)<min_){
      min_ <- min(el)
    }
  }
  plot(0,0,
       type = "n",
       ylim = c(min_, max_),
       xlim = c(1,7),
       sub = names(data.lst[[x]]$'out')[i+1],
       xlab = "день",
       ylab = "Показатель",
       main = names(all.lines)[j]
  )
  
  for (i in 1:length(all.lines[[j]])){
    
    lines(all.lines[[j]][[i]],
          type = "l",
          ylim = c(min(unlist(all.lines[[j]])),max(unlist(all.lines[[j]]))),
          sub = names(data.lst[[x]]$'out')[i+1],
          col = cls.lst[i],
          xlab = "день",
          ylab = "Показатель",
          main = names(all.lines)[j]
    )
  }
}

legend(x = "bottomright",
       legend = names(all.lines[[1]]),
       lty = c(1),
       col = cls.lst,
       lwd = 4)



###############################          3            #################################


par(mfrow = c(1,1))
num.product <- 1 #определяем продукт
volume.lst <- c()
cls.lst = colors()[sample(1:length(colors()),length(data.lst) , replace = FALSE)] #определяем цвета

for (x in 1:length(data.lst)){
  volume.lst <- c(volume.lst, sum(data.lst[[x]]$'out'[[num.product+1]])) #присваиваем сумму по продажам по магазинам
}

pie(volume.lst,
    labels = volume.lst,
    main = "Объем продаж по магазинам", #вызываем круговую диаграмму
    col = cls.lst)

legend("topright",
       legend = names(data.lst), #легенда
       lty = c(1),
       col = cls.lst,
       lwd = 3)










###############################                4                    #################################
num.products <- c(1,2) #номера продуктов
max_ <- 0
min_ <- 100000
lns.lst <- list()
cls.lst = colors()[sample(1:length(colors()),length(data.lst) , replace = FALSE)]

for (x in 1:length(data.lst)){
  lns.lst[[names(data.lst)[x]]] <- list(
    data.lst[[x]]$'out'[[num.products[1]+1]],
    data.lst[[x]]$'out'[[num.products[2]+1]] #обьем продаж по двум товарам
  )
  names(lns.lst[[x]])[1] <- names(data.lst[[x]]$'out')[num.products[1]+1] #присваиваем имена столбцам по товарам
  names(lns.lst[[x]])[2] <- names(data.lst[[x]]$'out')[num.products[2]+1]
  max.temp <- max(max(lns.lst[[x]][[1]]), max(lns.lst[[x]][[2]]))
  min.temp <- min(min(lns.lst[[x]][[1]]), min(lns.lst[[x]][[2]]))
}
plot(0,0,
     type = "l",
     ylim = c(min.temp, max.temp),
     xlim = c(1,10),
     xlab = "День",
     ylab = "Шт.",
     main = 'Объемы продаж по двум товарам по всем магазинам'
)

for (i in 1:length(lns.lst)){
  
  lines(lns.lst[[i]][[1]],
        type = "l",
        col = cls.lst[i],
        pch = 1 #добавляем линии по двум товарам
  )
  lines(lns.lst[[i]][[2]],
        type = "l",
        col = cls.lst[i],
        pch = 2
  )
}

legend("bottomright",
       legend = c(names(data.lst),names(lns.lst[[1]])),
       lty = c(1),
       pch = c(rep(NA, length(data.lst)),1,2), #легенда
       col = c(cls.lst, 'black', 'black'),
       lwd = 2)

















###############################            5                #################################
#номер товара это i

{
num.product <- 4

numbers <- readline(prompt="Введите количество магазинов: ") #определяем количество магазинов

Numbers<-c() #массив для введенных чисел
for (i in 1:numbers){
  num <- readline(prompt=paste0("Введите номер магазина",i,': ')) #выбираем магазины
  Numbers[i]<-as.numeric(num)
}
magaz <- Numbers
all.magaz <- list()
max_ <- rep(0,5)
min_ <- max_
for (x in magaz){
  sale.volume <- data.lst[[x]]$'out'[[num.product+1]] #считываем данные как в пункте 1
  supply.volume <- data.lst[[x]]$'in'[[num.product+1]]
  util.volume <- supply.volume - sale.volume
  util.price <- util.volume*price.data[num.product,3]
  sale.revenue <- sale.volume*price.data[num.product,2]
  sale.profit <- sale.revenue -
    supply.volume*price.data[num.product,1] - 
    util.price
  profitability <- sale.profit/sale.revenue*100
  all.lines <-
    list('объем продаж' = sale.volume,
         'выручка' = sale.revenue,
         'прибыль' = sale.profit,
         'списание' = util.price,
         'рентабельность' = profitability)
  for (k in 1:5){
    mx <- max(unlist(all.lines[k]))
    mn <- min(unlist(all.lines[k]))
    if(mx > max_[k]) {
      max_[k] <- mx
    }
    if(mn < min_[k]){
      min_[k] <- mn
    }
  }
  all.magaz[[names(data.lst)[x]]] <- all.lines
}
cls.lst = colors()[sample(1:length(colors()),7, replace = FALSE)]
par(mfrow = c(2,3))
for (j in 1:length(all.magaz[[1]])){
  plot(0,0,
       xlim = c(1,7),
       ylim = c(min_[j], max_[j]),
       type = "n",
       sub = names(data.lst[[x]]$'out')[i+1],
       xlab = "день",
       main = names(all.magaz[[1]])[j]
  )
  for (i in 1:length(all.magaz)){
    lines(all.magaz[[i]][[j]],
          type = "l",
          col = cls.lst[i]
    )
  }
}
plot.new()

legend("bottomright",
       legend = names(data.lst)[magaz],
       lty = c(1),
       col = cls.lst,
       lwd = 2,
       cex = 1.6)
}




###############################           6           #################################
volume.lst <- c()
good.lengt <- length(data.lst[[1]]$'out')-1
cls.lst = colors()[sample(1:length(colors()),good.lengt , replace = FALSE)] #генерим цвета
for (x in 1:length(data.lst)){
  for (i in 1:good.lengt){
    volume.lst <-  c(volume.lst, sum(data.lst[[x]]$'out'[[i+1]])) #сумму по продажах
  }
  
}
volume.matrix <-
  matrix(volume.lst,
         ncol = length(data.lst),
         nrow = (length(data.lst[[x]]$'out')-1)
  )

layout(mat = matrix(c(1,2), nrow = 1, ncol = 2),
       heights = c(1), # Heights of the two rows
       widths = c(3,2)) # Widths of the two columns

barplot(volume.matrix,
        xlab = 'Магазины',
        names.arg = names(data.lst),
        ylab = 'Объемы продаж по товарам',
        beside = FALSE,
        col = cls.lst)

plot.new()
legend("center",
       legend = names(data.lst[[1]]$'out')[1:good.lengt+1],
       lty = c(1),
       col = cls.lst,
       lwd = 7,
       cex = 0.8)



}