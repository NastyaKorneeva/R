#----------фунции----------
#наличие директории 

dir_check <- function(way){
  way_lst <- unlist(strsplit(x = way, split = "/"))
  if (length(way_lst) == 1){
    return () 
  }
  if (dir.exists(way)){
    dir_check(do.call(paste0,as.list(paste0(way_lst[1:length(way_lst) - 1], "/"))))
    return()
  }
  else{
    dir_check(do.call(paste0,as.list(paste0(way_lst[1:length(way_lst) - 1], "/"))))
    setwd(do.call(paste0,as.list(paste0(way_lst[1:length(way_lst) - 1], "/"))))
    dir.create(tail(way_lst, 1))
  }
}
#генерация поставки
generate.supply <- function(goods, name, way, days){
  dir_check(way)
  tabl <- data.frame("День" = 1:days)
  for (i in 1:length(goods)){
    tabl[i+1] <- sample(x = goods[[i]]$min:goods[[i]]$max, size = days, replace = TRUE)
    colnames(x = tabl)[i+1] = goods[[i]]$name
  }
  write.table(tabl, file = paste0(way, "/", name, ".in"), sep = "\t", row.names = FALSE)
  return(tabl)
}
#генерация продаж
generate.sale <- function(goods, name, way, days){
  dir_check(way)
  tabl <- data.frame("День" = 1:days)
  if (file.exists(paste0(way, "/", name, ".in")) == FALSE){
    generate.supply(goods, name, way, days)
  }
  
  data.in <- read.table(paste0(way, "/", name, ".in"), head = TRUE)
  for (i in 1:length(goods)){
    tabl[i+1] <- sample(x = goods[[i]]$min:goods[[i]]$max, size = days, replace = TRUE)
    nm <- goods[[i]]$name
    colnames(x = tabl)[i+1] = nm
    for (j in 1:nrow(tabl)){
      if (tabl[j, i+1] > data.in[j, i+1]){
        tabl[j, i+1] <- data.in[j, i+1]
      }
    }
    
  }
  
  
  write.table(tabl, file = paste0(way, "/", name, ".out"), sep = "\t", row.names = FALSE)
  return(tabl)
}
#в таблицу записывает цены 
generate.price <- function(goods, way){
  dir_check(way)
  tabl <- data.frame("Действие" = c("Покупка", "Продажа", "Утилизация"))
  for (i in 1:length(goods)){
    tabl[1, i+1] <- goods[[i]]$sup
    tabl[2, i+1] <- goods[[i]]$sal
    tabl[3, i+1] <- goods[[i]]$utl
    colnames(x = tabl)[i+1] = goods[[i]]$name
  }
  write.table(tabl, file = paste0(way, "/", "price", ".txt"), sep = "\t", row.names = FALSE)
  return(tabl)
}
#--------------------------------------------листы с заданием товаров и их количетсва и цен---------------------------------------------------------

{
  #фикс цены
  goods_prices <- list(
    list(name = "Акулы", sup = 100, sal = 1000, utl = 20), 
    list(name = "Селезни", sup = 110, sal = 1100, utl = 30), 
    list(name = "Собачки", sup = 120, sal = 1200, utl = 40), 
    list(name = "Гуси", sup = 130, sal = 1300, utl = 50) 
  )
  #мин и макс цены товаров
  h = list(
    list(name = "Акулы", min = 100, max = 200), 
    list(name = "Селезни", min = 200, max = 300), 
    list(name = "Собачки", min = 300, max = 400), 
    list(name = "Гуси", min = 400, max = 500)
  )
  #кол-во дней
  d <- 20
  
  konkret <- list(shop = 4, good = "Акулы")
  
  good_stats <- "Акулы"
  
  shop <- 4
  
  akulsup <- 100
}

#-------------------------------------------------выполнять один раз и потом не выполнять больше----------------------------------------------
{
  setwd('/Users/nastya/Desktop/Корнеева Настя/Лизунец/Анализ')

  #генерация таблиц
  in1 <- generate.supply(goods = h, name = "Магазин1_Лизунец", way = '/Users/nastya/Desktop/Корнеева Настя/Лизунец/Анализ', days = d)
  in2 <- generate.supply(goods = h, name = "Магазин2_Лизунец", way = '/Users/nastya/Desktop/Корнеева Настя/Лизунец/Анализ', days = d)
  in3 <- generate.supply(goods = h, name = "Магазин3_Лизунец", way = '/Users/nastya/Desktop/Корнеева Настя/Лизунец/Анализ', days = d)
  in4 <- generate.supply(goods = h, name = "Магазин4_Лизунец", way = '/Users/nastya/Desktop/Корнеева Настя/Лизунец/Анализ', days = d)
  in5 <- generate.supply(goods = h, name = "Магазин5_Лизунец", way = '/Users/nastya/Desktop/Корнеева Настя/Лизунец/Анализ', days = d)
  in6 <- generate.supply(goods = h, name = "Магазин6_Лизунец", way = '/Users/nastya/Desktop/Корнеева Настя/Лизунец/Анализ', days = d)
  in7 <- generate.supply(goods = h, name = "Магазин7_Лизунец", way = '/Users/nastya/Desktop/Корнеева Настя/Лизунец/Анализ', days = d)
  in8 <- generate.supply(goods = h, name = "Магазин8_Лизунец", way = '/Users/nastya/Desktop/Корнеева Настя/Лизунец/Анализ', days = d)
  in9 <- generate.supply(goods = h, name = "Магазин9_Лизунец", way = '/Users/nastya/Desktop/Корнеева Настя/Лизунец/Анализ', days = d)
  in10 <- generate.supply(goods = h, name = "Магазин10_Лизунец", way = '/Users/nastya/Desktop/Корнеева Настя/Лизунец/Анализ', days = d)
  out1 <- generate.sale(goods = h, name = "Магазин1_Лизунец", way = '/Users/nastya/Desktop/Корнеева Настя/Лизунец/Анализ', days = d)
  out2 <- generate.sale(goods = h, name = "Магазин2_Лизунец", way = '/Users/nastya/Desktop/Корнеева Настя/Лизунец/Анализ', days = d)
  out3 <- generate.sale(goods = h, name = "Магазин3_Лизунец", way = '/Users/nastya/Desktop/Корнеева Настя/Лизунец/Анализ', days = d)
  out4 <- generate.sale(goods = h, name = "Магазин4_Лизунец", way = '/Users/nastya/Desktop/Корнеева Настя/Лизунец/Анализ', days = d)
  out5 <- generate.sale(goods = h, name = "Магазин5_Лизунец", way = '/Users/nastya/Desktop/Корнеева Настя/Лизунец/Анализ', days = d)
  out6 <- generate.sale(goods = h, name = "Магазин6_Лизунец", way = '/Users/nastya/Desktop/Корнеева Настя/Лизунец/Анализ', days = d)
  out7 <- generate.sale(goods = h, name = "Магазин7_Лизунец", way = '/Users/nastya/Desktop/Корнеева Настя/Лизунец/Анализ', days = d)
  out8 <- generate.sale(goods = h, name = "Магазин8_Лизунец", way = '/Users/nastya/Desktop/Корнеева Настя/Лизунец/Анализ', days = d)
  out9 <- generate.sale(goods = h, name = "Магазин9_Лизунец", way = '/Users/nastya/Desktop/Корнеева Настя/Лизунец/Анализ', days = d)
  out10 <- generate.sale(goods = h, name = "Магазин10_Лизунец", way = '/Users/nastya/Desktop/Корнеева Настя/Лизунец/Анализ', days = d)
  #цены
  Price <- generate.price(goods = goods_prices, way = '/Users/nastya/Desktop/Корнеева Настя/Лизунец/Анализ')

#---------------------------------------------выполнять отсюда после первого  раза--------------------------------------------------  
{
  
  
  #создаем таблицу
  rev <- rep(0, 12)
  profit <- rep(0, length(rev))
  res.tab <- data.frame(" "=c("Магазин 1","Магазин 2","Магазин 3","Магазин 4","Магазин 5","Магазин 6","Магазин 7","Магазин 8","Магазин 9","Магазин 10","Итого","Среднее"),"Выручка руб"=rev, "Прибыль руб"=profit)             
  salevir <-rep(0, nrow(res.tab))
  res.tab$'Реализация, конт.' <- salevir
  salepr <-rep(0, nrow(res.tab))
  res.tab$'Списание, конт.' <- salepr
  
  saleravn <-rep(0, nrow(res.tab))
  res.tab$'sd' <- saleravn
  
  saleprmax <-rep(0, nrow(res.tab))
  res.tab$'Продажи макс' <- saleprmax
  res.tab$'Продажи макс'[11:12] <- ''
  
  saleden1 <-rep(0, nrow(res.tab))
  res.tab$День <- saleden1
  res.tab$День[11:12] <- ''
  
  saleprmin <-rep(0, nrow(res.tab))
  res.tab$'Продажи мин' <- saleprmin
  res.tab$'Продажи мин'[11:12] <- ''
  
  saleden2 <-rep(0, nrow(res.tab))
  res.tab$' День' <- saleden2
  res.tab$' День'[11:12] <- ''
  
  salespmax <-rep(0, nrow(res.tab))
  res.tab$'Списание макс' <- salespmax
  res.tab$'Списание макс'[11:12] <- ''
  
  saleden3 <-rep(0, nrow(res.tab))
  res.tab$'  День' <- saleden3
  res.tab$'  День'[11:12] <- ''
  
  
  
  #продажи максимум
  for (day in (1:nrow(out1))) {
    if ( res.tab[1,7] < sum(out1[day,(2:ncol(out1))])) {
      res.tab[1,7] <- sum(out1[day,(2:ncol(out2))])
      res.tab[1,8] <- day
    }
    if ( res.tab[2,7] < sum(out2[day,(2:ncol(out2))])) {
      res.tab[2,7] <- sum(out2[day,(2:ncol(out2))])
      res.tab[2,8] <- day
    }
    if ( res.tab[3,7] < sum(out3[day,(2:ncol(out3))])) {
      res.tab[3,7] <- sum(out3[day,(2:ncol(out3))])
      res.tab[3,8] <- day
    }
    if ( res.tab[4,7] < sum(out4[day,(2:ncol(out4))])) {
      res.tab[4,7] <- sum(out4[day,(2:ncol(out4))])
      res.tab[4,8] <- day
    }
    if ( res.tab[4,7] < sum(out4[day,(2:ncol(out4))])) {
      res.tab[4,7] <- sum(out4[day,(2:ncol(out4))])
      res.tab[4,8] <- day
    }
    if ( res.tab[5,7] < sum(out5[day,(2:ncol(out5))])) {
      res.tab[5,7] <- sum(out5[day,(2:ncol(out5))])
      res.tab[5,8] <- day
    }
    if ( res.tab[6,7] < sum(out6[day,(2:ncol(out6))])) {
      res.tab[6,7] <- sum(out6[day,(2:ncol(out6))])
      res.tab[6,8] <- day
    }
    if ( res.tab[7,7] < sum(out7[day,(2:ncol(out7))])) {
      res.tab[7,7] <- sum(out7[day,(2:ncol(out7))])
      res.tab[7,8] <- day
    }
    if ( res.tab[8,7] < sum(out8[day,(2:ncol(out8))])) {
      res.tab[8,7] <- sum(out8[day,(2:ncol(out8))])
      res.tab[8,8] <- day
    }
    if ( res.tab[9,7] < sum(out9[day,(2:ncol(out9))])) {
      res.tab[9,7] <- sum(out9[day,(2:ncol(out9))])
      res.tab[9,8] <- day
    }
    if ( res.tab[10,7] < sum(out10[day,(2:ncol(out10))])) {
      res.tab[10,7] <- sum(out10[day,(2:ncol(out10))])
      res.tab[10,8] <- day
    }
  }
  
  #продажи минимум
  res.tab[1,9] <- sum(out1[1,(2:ncol(out1))])
  res.tab[2,9] <- sum(out2[1,(2:ncol(out1))])
  res.tab[3,9] <- sum(out3[1,(2:ncol(out1))]) 
  res.tab[4,9] <- sum(out4[1,(2:ncol(out1))]) 
  res.tab[5,9] <- sum(out5[1,(2:ncol(out1))]) 
  res.tab[6,9] <- sum(out6[1,(2:ncol(out1))]) 
  res.tab[7,9] <- sum(out7[1,(2:ncol(out1))]) 
  res.tab[8,9] <- sum(out8[1,(2:ncol(out1))]) 
  res.tab[9,9] <- sum(out9[1,(2:ncol(out1))]) 
  res.tab[10,9] <- sum(out10[1,(2:ncol(out1))]) 
  
  for (day in (2:nrow(out1))) {
    
    if ( res.tab[1,9] > sum(out1[day,(2:ncol(out1))])) {
      res.tab[1,9] <- sum(out1[day,(2:ncol(out1))])
      res.tab[1,10] <- day
    }
    if ( res.tab[2,9] > sum(out2[day,(2:ncol(out2))])) {
      res.tab[2,9] <- sum(out2[day,(2:ncol(out2))])
      res.tab[2,10] <- day
    }
    if ( res.tab[3,9] > sum(out3[day,(2:ncol(out3))])) {
      res.tab[3,9] <- sum(out3[day,(2:ncol(out3))])
      res.tab[3,10] <- day
    }
    if ( res.tab[4,9] > sum(out4[day,(2:ncol(out4))])) {
      res.tab[4,9] <- sum(out4[day,(2:ncol(out4))])
      res.tab[4,10] <- day
    }
    if ( res.tab[4,9] > sum(out4[day,(2:ncol(out4))])) {
      res.tab[4,9] <- sum(out4[day,(2:ncol(out4))])
      res.tab[4,10] <- day
    }
    if ( res.tab[5,9] > sum(out5[day,(2:ncol(out5))])) {
      res.tab[5,9] <- sum(out5[day,(2:ncol(out5))])
      res.tab[5,10] <- day
    }
    if ( res.tab[6,9] > sum(out6[day,(2:ncol(out6))])) {
      res.tab[6,9] <- sum(out6[day,(2:ncol(out6))])
      res.tab[6,10] <- day
    }
    if ( res.tab[7,9] > sum(out7[day,(2:ncol(out7))])) {
      res.tab[7,9] <- sum(out7[day,(2:ncol(out7))])
      res.tab[7,10] <- day
    }
    if ( res.tab[8,9] > sum(out8[day,(2:ncol(out8))])) {
      res.tab[8,9] <- sum(out8[day,(2:ncol(out8))])
      res.tab[8,10] <- day
    }
    if ( res.tab[9,9] > sum(out9[day,(2:ncol(out9))])) {
      res.tab[9,9] <- sum(out9[day,(2:ncol(out9))])
      res.tab[9,10] <- day
    }
    if ( res.tab[10,9] > sum(out10[day,(2:ncol(out10))])) {
      res.tab[10,9] <- sum(out10[day,(2:ncol(out10))])
      res.tab[10,10] <- day
    }
  }
  for (day in (1:nrow(out1))) {
    if ( as.integer(res.tab[1,11]) < (sum(in1[day,(2:ncol(in1))]) - sum(out1[day,(2:ncol(out1))]))) {
      res.tab[1,11] <- (sum(in1[day,(2:ncol(in1))]) - sum(out1[day,(2:ncol(out1))]))
      res.tab[1,12] <- day
    }
    if ( as.integer(res.tab[2,11]) < sum(in2[day,(2:ncol(in2))]) - sum(out2[day,(2:ncol(out2))])) {
      res.tab[2,11] <- sum(in2[day,(2:ncol(in2))]) - sum(out2[day,(2:ncol(out2))])
      res.tab[2,12] <- day
    }
    if ( as.integer(res.tab[3,11]) < sum(in3[day,(2:ncol(in3))]) - sum(out3[day,(2:ncol(out3))])) {
      res.tab[3,11] <- sum(in3[day,(2:ncol(in3))]) - sum(out3[day,(2:ncol(out3))])
      res.tab[3,12] <- day
    }
    if ( as.integer(res.tab[4,11]) < sum(in4[day,(2:ncol(in4))]) - sum(out4[day,(2:ncol(out4))])) {
      res.tab[4,11] <- sum(in4[day,(2:ncol(in4))]) - sum(out4[day,(2:ncol(out4))])
      res.tab[4,12] <- day
    }
    if ( as.integer(res.tab[5,11]) < sum(in5[day,(2:ncol(in5))]) - sum(out5[day,(2:ncol(out4))])) {
      res.tab[5,11] <- sum(in5[day,(2:ncol(in5))]) - sum(out5[day,(2:ncol(out5))])
      res.tab[5,12] <- day
    }
    if ( as.integer(res.tab[6,11]) < sum(in6[day,(2:ncol(in6))]) - sum(out6[day,(2:ncol(out6))])) {
      res.tab[6,11] <- sum(in6[day,(2:ncol(in6))]) - sum(out6[day,(2:ncol(out6))])
      res.tab[6,12] <- day
    }
    if ( as.integer(res.tab[7,11]) < sum(in7[day,(2:ncol(in7))]) - sum(out7[day,(2:ncol(out7))])) {
      res.tab[7,11] <- sum(in7[day,(2:ncol(in7))]) - sum(out7[day,(2:ncol(out7))])
      res.tab[7,12] <- day
    }
    if ( as.integer(res.tab[8,11]) < sum(in8[day,(2:ncol(in8))]) - sum(out8[day,(2:ncol(out8))])) {
      res.tab[8,11] <- sum(in8[day,(2:ncol(in8))]) - sum(out8[day,(2:ncol(out8))])
      res.tab[8,12] <- day
    }
    if ( as.integer(res.tab[9,11]) < sum(in9[day,(2:ncol(in9))]) - sum(out9[day,(2:ncol(out9))])) {
      res.tab[9,11] <- sum(in9[day,(2:ncol(in9))]) - sum(out9[day,(2:ncol(out9))])
      res.tab[9,12] <- day
    }
    if ( as.integer(res.tab[10,11]) < sum(in10[day,(2:ncol(in10))]) - sum(out10[day,(2:ncol(out10))])) {
      res.tab[10,11] <- sum(in10[day,(2:ncol(in10))]) - sum(out10[day,(2:ncol(out10))])
      res.tab[10,12] <- day
    }
  }
  
  res.tab[1,4] <- res.tab[1,4] + sum(out1[,2:ncol(out1)])
  res.tab[2,4] <- res.tab[2,4] + sum(out2[,2:ncol(out2)])
  res.tab[3,4] <- res.tab[3,4] + sum(out3[,2:ncol(out3)])
  res.tab[4,4] <- res.tab[4,4] + sum(out4[,2:ncol(out4)])
  res.tab[5,4] <- res.tab[5,4] + sum(out5[,2:ncol(out5)])
  res.tab[6,4] <- res.tab[6,4] + sum(out6[,2:ncol(out6)])
  res.tab[7,4] <- res.tab[7,4] + sum(out7[,2:ncol(out7)])
  res.tab[8,4] <- res.tab[8,4] + sum(out8[,2:ncol(out8)])
  res.tab[9,4] <- res.tab[9,4] + sum(out9[,2:ncol(out9)])
  res.tab[10,4] <- res.tab[10,4] + sum(out10[,2:ncol(out10)])
  res.tab[nrow(res.tab)-1,4] <- sum(res.tab[1:10,4])
  res.tab[nrow(res.tab),4] <- mean(res.tab[1:10,4])
  
  
  for (i in 2:ncol(out1)){
    nam <- colnames(x = in1)[i]
    res.tab[1,2] <- res.tab[1,2] + sum(out1[,i]) * Price[2, ][[nam]]
    res.tab[2,2] <- res.tab[2,2] + sum(out2[,i]) * Price[2, ][[nam]]
    res.tab[3,2] <- res.tab[3,2] + sum(out3[,i]) * Price[2, ][[nam]]
    res.tab[4,2] <- res.tab[4,2] + sum(out4[,i]) * Price[2, ][[nam]]
    res.tab[5,2] <- res.tab[5,2] + sum(out5[,i]) * Price[2, ][[nam]]
    res.tab[6,2] <- res.tab[6,2] + sum(out6[,i]) * Price[2, ][[nam]]
    res.tab[7,2] <- res.tab[7,2] + sum(out7[,i]) * Price[2, ][[nam]]
    res.tab[8,2] <- res.tab[8,2] + sum(out8[,i]) * Price[2, ][[nam]]
    res.tab[9,2] <- res.tab[9,2] + sum(out9[,i]) * Price[2, ][[nam]]
    res.tab[10,2] <- res.tab[10,2] + sum(out10[,i]) * Price[2, ][[nam]]
  }
  res.tab[nrow(res.tab)-1,2] <- sum(res.tab[1:10,2])
  res.tab[nrow(res.tab),2] <- mean(res.tab[1:10,2])
  
  
  for (i in 2:ncol(out1)){
    nam <- colnames(x = in1)[i]
    res.tab[1,3] <- res.tab[1,3] + sum(out1[,i]) * Price[2, ][[nam]] - sum(in1[,i]) * Price[1, ][[nam]] - (sum(in1[,i]) - sum(out1[,i])) * Price[3, ][[nam]]
    res.tab[2,3] <- res.tab[2,3] + sum(out2[,i]) * Price[2, ][[nam]] - sum(in2[,i]) * Price[1, ][[nam]] - (sum(in2[,i]) - sum(out2[,i])) * Price[3, ][[nam]]
    res.tab[3,3] <- res.tab[3,3] + sum(out3[,i]) * Price[2, ][[nam]] - sum(in3[,i]) * Price[1, ][[nam]] - (sum(in3[,i]) - sum(out3[,i])) * Price[3, ][[nam]]
    res.tab[4,3] <- res.tab[4,3] + sum(out4[,i]) * Price[2, ][[nam]] - sum(in4[,i]) * Price[1, ][[nam]] - (sum(in4[,i]) - sum(out4[,i])) * Price[3, ][[nam]]
    res.tab[5,3] <- res.tab[5,3] + sum(out5[,i]) * Price[2, ][[nam]] - sum(in5[,i]) * Price[1, ][[nam]] - (sum(in5[,i]) - sum(out5[,i])) * Price[3, ][[nam]]
    res.tab[6,3] <- res.tab[6,3] + sum(out6[,i]) * Price[2, ][[nam]] - sum(in6[,i]) * Price[1, ][[nam]] - (sum(in6[,i]) - sum(out6[,i])) * Price[3, ][[nam]]
    res.tab[7,3] <- res.tab[7,3] + sum(out7[,i]) * Price[2, ][[nam]] - sum(in7[,i]) * Price[1, ][[nam]] - (sum(in7[,i]) - sum(out7[,i])) * Price[3, ][[nam]]
    res.tab[8,3] <- res.tab[8,3] + sum(out8[,i]) * Price[2, ][[nam]] - sum(in8[,i]) * Price[1, ][[nam]] - (sum(in8[,i]) - sum(out8[,i])) * Price[3, ][[nam]]
    res.tab[9,3] <- res.tab[9,3] + sum(out9[,i]) * Price[2, ][[nam]] - sum(in9[,i]) * Price[1, ][[nam]] - (sum(in9[,i]) - sum(out9[,i])) * Price[3, ][[nam]]
    res.tab[10,3] <- res.tab[10,3] + sum(out10[,i]) * Price[2, ][[nam]] - sum(in10[,i]) * Price[1, ][[nam]] - (sum(in10[,i]) - sum(out10[,i])) * Price[3, ][[nam]]
  }
  
  res.tab[nrow(res.tab)-1,3] <- sum(res.tab[1:10,3])
  res.tab[nrow(res.tab),3] <- mean(res.tab[1:10,3])
  
  res.tab[1,5] <- res.tab[1,5] - sum(out1[,2:ncol(out1)]) + sum(in1[,2:ncol(in1)])
  res.tab[2,5] <- res.tab[2,5] - sum(out2[,2:ncol(out2)]) + sum(in2[,2:ncol(in2)])
  res.tab[3,5] <- res.tab[3,5] - sum(out3[,2:ncol(out3)]) + sum(in3[,2:ncol(in3)])
  res.tab[4,5] <- res.tab[4,5] - sum(out4[,2:ncol(out4)]) + sum(in4[,2:ncol(in4)])
  res.tab[5,5] <- res.tab[5,5] - sum(out5[,2:ncol(out5)]) + sum(in5[,2:ncol(in5)])
  res.tab[6,5] <- res.tab[6,5] - sum(out6[,2:ncol(out6)]) + sum(in6[,2:ncol(in6)])
  res.tab[7,5] <- res.tab[7,5] - sum(out7[,2:ncol(out7)]) + sum(in7[,2:ncol(in7)])
  res.tab[8,5] <- res.tab[8,5] - sum(out8[,2:ncol(out8)]) + sum(in8[,2:ncol(in8)])
  res.tab[9,5] <- res.tab[9,5] - sum(out9[,2:ncol(out9)]) + sum(in9[,2:ncol(in9)])
  res.tab[10,5] <- res.tab[10,5] - sum(out10[,2:ncol(out10)]) + sum(in10[,2:ncol(in10)])
  res.tab[nrow(res.tab)-1,5] <- sum(res.tab[1:10,5])
  res.tab[nrow(res.tab),5] <- mean(res.tab[1:10,5])
  
  
  for (i in 2:ncol(out1)){
    res.tab[1,6] <- res.tab[1,6] + round(sd(out1[,i]), 3)
    res.tab[2,6] <- res.tab[2,6] + round(sd(out2[,i]), 3)
    res.tab[3,6] <- res.tab[3,6] + round(sd(out3[,i]), 3)
    res.tab[4,6] <- res.tab[4,6] + round(sd(out4[,i]), 3)
    res.tab[5,6] <- res.tab[5,6] + round(sd(out5[,i]), 3)
    res.tab[6,6] <- res.tab[6,6] + round(sd(out6[,i]), 3)
    res.tab[7,6] <- res.tab[7,6] + round(sd(out7[,i]), 3)
    res.tab[8,6] <- res.tab[8,6] + round(sd(out8[,i]), 3)
    res.tab[9,6] <- res.tab[9,6] + round(sd(out9[,i]), 3)
    res.tab[10,6] <- res.tab[10,6] + round(sd(out10[,i]), 3)
  }
  res.tab[nrow(res.tab)-1,6] <- sum(res.tab[1:10,6])
  res.tab[nrow(res.tab),6] <- mean(res.tab[1:10,6])
  
  #write.table(res.tab, file = "/Users/nastya/Desktop/Корнеева Настя/Лизунец/Анализ/Таблица.csv", sep = ";", row.names = FALSE)
  #таблица сделана
  
  #---------------------------один магазин один товар---------------------------
  
  #Акулы. 4 Магазин
    tables <- list(in1, out1, in2, out2, in3, out3, in4, out4,in5, out5, in6, out6, in7, out7, in8, out8, in9, out9, in10, out10)
    
    rev1 <- sum(tables[[konkret$shop * 2]][,konkret$good]) * Price[2, ][[konkret$good]]
    profit1 <- sum(tables[[konkret$shop * 2]][,konkret$good]) * Price[2, ][[konkret$good]] - sum(tables[[konkret$shop * 2 - 1]][,konkret$good]) * Price[1, ][[konkret$good]] - (sum(tables[[konkret$shop * 2 - 1]][,konkret$good]) - sum(tables[[konkret$shop * 2]][,konkret$good])) * Price[3, ][[konkret$good]]
    konk.tab <- data.frame("Выручка руб"=rev1, "Прибыль руб"=profit1)             
    konk.tab$'Реализация, конт.' <- sum(tables[[konkret$shop * 2]][[konkret$good]])
    konk.tab$'Списание, конт.' <- sum(tables[[konkret$shop * 2 - 1]][[konkret$good]]) - sum(tables[[konkret$shop * 2]][[konkret$good]])
    konk.tab$'sd' <- round(sd(tables[[konkret$shop * 2]][[konkret$good]]), 3)
    konk.tab$'Продажи макс' <- max(tables[[konkret$shop * 2]][[konkret$good]])
    konk.tab$День <- which.max(tables[[konkret$shop * 2]][[konkret$good]])
    konk.tab$'Продажи мин' <- min(tables[[konkret$shop * 2]][[konkret$good]])
    konk.tab$' День' <- which.min(tables[[konkret$shop * 2]][[konkret$good]])
    konk.tab$'Списание макс' <- max(tables[[konkret$shop * 2 - 1]][,konkret$good] - tables[[konkret$shop * 2]][,konkret$good])
    konk.tab$'  День' <- which.max(tables[[konkret$shop * 2 - 1]][,konkret$good] - tables[[konkret$shop * 2]][,konkret$good])
    
    
  #------------------один магазин все товары--------------------------------
    #4 магазин
    
    #создаем таблицу
    rev2 <- 0
    coll <- rep(0, ncol(in1) - 1)
    shop.tab <- data.frame(" " = colnames(in1)[2:ncol(in1)], "Выручка руб"=rev2, "Прибыль руб"=coll)             
    shop.tab$'Реализация, конт.' <- coll
    shop.tab$'Списание, конт.' <- coll
    shop.tab$'sd' <- coll
    shop.tab$'Продажи макс' <- coll
    shop.tab$День <- coll
    shop.tab$'Продажи мин' <- coll
    shop.tab$' День' <- coll
    shop.tab$'Списание макс' <- coll
    shop.tab$'  День' <- coll
    
    
    #продажи максимум
    for (j in 2:ncol(tables[[shop * 2 - 1]])){
      for (day in (1:nrow(tables[[shop * 2]]))) {
        if ( shop.tab[j-1,7] < sum(tables[[shop * 2]][day,j])) {
          shop.tab[j-1,7] <- sum(tables[[shop * 2]][day,j])
          shop.tab[j-1,8] <- day
        }
        
      }
      
      #продажи минимум
      shop.tab[j-1,9] <- sum(tables[[shop * 2]][1,j])
      
      
      for (day in (2:nrow(tables[[shop * 2]]))) {
        if ( shop.tab[j-1,9] > sum(tables[[shop * 2]][day,j])) {
          shop.tab[j-1,9] <- sum(tables[[shop * 2]][day,j])
          shop.tab[j-1,10] <- day
        }
      }
      for (day in (1:nrow(tables[[shop * 2]]))) {
        if ( as.integer(shop.tab[j-1,11]) < (sum(tables[[shop * 2 - 1]][day,(2:ncol(tables[[shop * 2 - 1]]))]) - sum(tables[[shop * 2]][day,j]))) {
          shop.tab[j-1,11] <- (sum(tables[[shop * 2 - 1]][day,(2:ncol(tables[[shop * 2 - 1]]))]) - sum(tables[[shop * 2]][day,j]))
          shop.tab[j-1,12] <- day
        }
      }
      
      shop.tab[j-1,4] <- shop.tab[j-1,4] + sum(tables[[shop * 2]][,j])
      
      
    
      nam <- colnames(x = tables[[shop * 2 - 1]])[j]
      shop.tab[j-1,2] <- shop.tab[j-1,2] + sum(tables[[shop * 2]][,j]) * Price[2, ][[nam]]
      
    
      
      
      
      shop.tab[j-1,3] <- shop.tab[j-1,3] + sum(tables[[shop * 2]][,j]) * Price[2, ][[nam]] - sum(tables[[shop * 2 - 1]][,j]) * Price[1, ][[nam]] - (sum(tables[[shop * 2 - 1]][,j]) - sum(tables[[shop * 2]][,j])) * Price[3, ][[nam]]
      
      
      
      shop.tab[j-1,5] <- shop.tab[j-1,5] - sum(tables[[shop * 2]][,2:ncol(tables[[shop * 2]])]) + sum(tables[[shop * 2 - 1]][,2:ncol(tables[[shop * 2 - 1]])])
      
      
      shop.tab[j-1,6] <- shop.tab[j-1,6] + round(sd(tables[[shop * 2]][,j]), 3)
      
    
    }
   
    
    
  #-----------один товар все магазины------
    #Акулы
    
    #создаем таблицу
    rev2 <- rep(0, 12)
    coll <- rep(0, length(rev))
    good.tab <- data.frame(" "=c("Магазин 1","Магазин 2","Магазин 3","Магазин 4","Магазин 5","Магазин 6","Магазин 7","Магазин 8","Магазин 9","Магазин 10","Итого","Среднее"),"Выручка руб"=rev2, "Прибыль руб"=coll)             
    good.tab$'Реализация, конт.' <- coll
    good.tab$'Списание, конт.' <- coll
    good.tab$'sd' <- coll
    good.tab$'Продажи макс' <- coll
    good.tab$'Продажи макс'[11:12] <- ''
    good.tab$День <- coll
    good.tab$День[11:12] <- ''
    good.tab$'Продажи мин' <- coll
    good.tab$'Продажи мин'[11:12] <- ''
    good.tab$' День' <- coll
    good.tab$' День'[11:12] <- ''
    good.tab$'Списание макс' <- coll
    good.tab$'Списание макс'[11:12] <- ''
    good.tab$'  День' <- coll
    good.tab$'  День'[11:12] <- ''
    
    
    
    #продажи максимум
    for (day in (1:nrow(out1))) {
      if ( good.tab[1,7] < sum(out1[day,good_stats])) {
        good.tab[1,7] <- sum(out1[day,good_stats])
        good.tab[1,8] <- day
      }
      if ( good.tab[2,7] < sum(out2[day,good_stats])) {
        good.tab[2,7] <- sum(out2[day,good_stats])
        good.tab[2,8] <- day
      }
      if ( good.tab[3,7] < sum(out3[day,good_stats])) {
        good.tab[3,7] <- sum(out3[day,good_stats])
        good.tab[3,8] <- day
      }
      if ( good.tab[4,7] < sum(out4[day,good_stats])) {
        good.tab[4,7] <- sum(out4[day,good_stats])
        good.tab[4,8] <- day
      }
      if ( good.tab[4,7] < sum(out4[day,good_stats])) {
        good.tab[4,7] <- sum(out4[day,good_stats])
        good.tab[4,8] <- day
      }
      if ( good.tab[5,7] < sum(out5[day,good_stats])) {
        good.tab[5,7] <- sum(out5[day,good_stats])
        good.tab[5,8] <- day
      }
      if ( good.tab[6,7] < sum(out6[day,good_stats])) {
        good.tab[6,7] <- sum(out6[day,good_stats])
        good.tab[6,8] <- day
      }
      if ( good.tab[7,7] < sum(out7[day,good_stats])) {
        good.tab[7,7] <- sum(out7[day,good_stats])
        good.tab[7,8] <- day
      }
      if ( good.tab[8,7] < sum(out8[day,good_stats])) {
        good.tab[8,7] <- sum(out8[day,good_stats])
        good.tab[8,8] <- day
      }
      if ( good.tab[9,7] < sum(out9[day,good_stats])) {
        good.tab[9,7] <- sum(out9[day,good_stats])
        good.tab[9,8] <- day
      }
      if ( good.tab[10,7] < sum(out10[day,good_stats])) {
        good.tab[10,7] <- sum(out10[day,good_stats])
        good.tab[10,8] <- day
      }
    }
    
    #продажи минимум
    good.tab[1,9] <- sum(out1[1,(good_stats)])
    good.tab[2,9] <- sum(out2[1,(good_stats)])
    good.tab[3,9] <- sum(out3[1,(good_stats)]) 
    good.tab[4,9] <- sum(out4[1,(good_stats)]) 
    good.tab[5,9] <- sum(out5[1,(good_stats)]) 
    good.tab[6,9] <- sum(out6[1,(good_stats)]) 
    good.tab[7,9] <- sum(out7[1,(good_stats)]) 
    good.tab[8,9] <- sum(out8[1,(good_stats)]) 
    good.tab[9,9] <- sum(out9[1,(good_stats)]) 
    good.tab[10,9] <- sum(out10[1,(good_stats)]) 
    
    for (day in (2:nrow(out1))) {
      
      if ( good.tab[1,9] > sum(out1[day,good_stats])) {
        good.tab[1,9] <- sum(out1[day,good_stats])
        good.tab[1,10] <- day
      }
      if ( good.tab[2,9] > sum(out2[day,good_stats])) {
        good.tab[2,9] <- sum(out2[day,good_stats])
        good.tab[2,10] <- day
      }
      if ( good.tab[3,9] > sum(out3[day,good_stats])) {
        good.tab[3,9] <- sum(out3[day,good_stats])
        good.tab[3,10] <- day
      }
      if ( good.tab[4,9] > sum(out4[day,good_stats])) {
        good.tab[4,9] <- sum(out4[day,good_stats])
        good.tab[4,10] <- day
      }
      if ( good.tab[4,9] > sum(out4[day,good_stats])) {
        good.tab[4,9] <- sum(out4[day,good_stats])
        good.tab[4,10] <- day
      }
      if ( good.tab[5,9] > sum(out5[day,good_stats])) {
        good.tab[5,9] <- sum(out5[day,good_stats])
        good.tab[5,10] <- day
      }
      if ( good.tab[6,9] > sum(out6[day,good_stats])) {
        good.tab[6,9] <- sum(out6[day,good_stats])
        good.tab[6,10] <- day
      }
      if ( good.tab[7,9] > sum(out7[day,good_stats])) {
        good.tab[7,9] <- sum(out7[day,good_stats])
        good.tab[7,10] <- day
      }
      if ( good.tab[8,9] > sum(out8[day,good_stats])) {
        good.tab[8,9] <- sum(out8[day,good_stats])
        good.tab[8,10] <- day
      }
      if ( good.tab[9,9] > sum(out9[day,good_stats])) {
        good.tab[9,9] <- sum(out9[day,good_stats])
        good.tab[9,10] <- day
      }
      if ( good.tab[10,9] > sum(out10[day,good_stats])) {
        good.tab[10,9] <- sum(out10[day,good_stats])
        good.tab[10,10] <- day
      }
    }
    for (day in (1:nrow(out1))) {
      if ( as.integer(good.tab[1,11]) < (sum(in1[day,good_stats]) - sum(out1[day,good_stats]))) {
        good.tab[1,11] <- (sum(in1[day,good_stats]) - sum(out1[day,good_stats]))
        good.tab[1,12] <- day
      }
      if ( as.integer(good.tab[2,11]) < sum(in2[day,good_stats]) - sum(out2[day,good_stats])) {
        good.tab[2,11] <- sum(in2[day,good_stats]) - sum(out2[day,good_stats])
        good.tab[2,12] <- day
      }
      if ( as.integer(good.tab[3,11]) < sum(in3[day,good_stats]) - sum(out3[[day,good_stats]])) {
        good.tab[3,11] <- sum(in3[day,good_stats]) - sum(out3[day,good_stats])
        good.tab[3,12] <- day
      }
      if ( as.integer(good.tab[4,11]) < sum(in4[day,good_stats]) - sum(out4[day,good_stats])) {
        good.tab[4,11] <- sum(in4[day,good_stats]) - sum(out4[day,good_stats])
        good.tab[4,12] <- day
      }
      if ( as.integer(good.tab[5,11]) < sum(in5[day,good_stats]) - sum(out5[day,good_stats])) {
        good.tab[5,11] <- sum(in5[day,good_stats]) - sum(out5[day,good_stats])
        good.tab[5,12] <- day
      }
      if ( as.integer(good.tab[6,11]) < sum(in6[day,good_stats]) - sum(out6[day,good_stats])) {
        good.tab[6,11] <- sum(in6[day,good_stats]) - sum(out6[day,good_stats])
        good.tab[6,12] <- day
      }
      if ( as.integer(good.tab[7,11]) < sum(in7[day,good_stats]) - sum(out7[day,good_stats])) {
        good.tab[7,11] <- sum(in7[day,good_stats]) - sum(out7[day,good_stats])
        good.tab[7,12] <- day
      }
      if ( as.integer(good.tab[8,11]) < sum(in8[day,good_stats]) - sum(out8[day,good_stats])) {
        good.tab[8,11] <- sum(in8[day,good_stats]) - sum(out8[day,good_stats])
        good.tab[8,12] <- day
      }
      if ( as.integer(good.tab[9,11]) < sum(in9[day,good_stats]) - sum(out9[day,good_stats])) {
        good.tab[9,11] <- sum(in9[day,good_stats]) - sum(out9[day,good_stats])
        good.tab[9,12] <- day
      }
      if ( as.integer(good.tab[10,11]) < sum(in10[day,good_stats]) - sum(out10[day,good_stats])) {
        good.tab[10,11] <- sum(in10[day,good_stats]) - sum(out10[day,good_stats])
        good.tab[10,12] <- day
      }
    }
    
    good.tab[1,4] <- good.tab[1,4] + sum(out1[[good_stats]])
    good.tab[2,4] <- good.tab[2,4] + sum(out2[[good_stats]])
    good.tab[3,4] <- good.tab[3,4] + sum(out3[[good_stats]])
    good.tab[4,4] <- good.tab[4,4] + sum(out4[[good_stats]])
    good.tab[5,4] <- good.tab[5,4] + sum(out5[[good_stats]])
    good.tab[6,4] <- good.tab[6,4] + sum(out6[[good_stats]])
    good.tab[7,4] <- good.tab[7,4] + sum(out7[[good_stats]])
    good.tab[8,4] <- good.tab[8,4] + sum(out8[[good_stats]])
    good.tab[9,4] <- good.tab[9,4] + sum(out9[[good_stats]])
    good.tab[10,4] <- good.tab[10,4] + sum(out10[[good_stats]])
    good.tab[nrow(good.tab)-1,4] <- sum(good.tab[1:10,4])
    good.tab[nrow(good.tab),4] <- mean(good.tab[1:10,4])
    
    
    good.tab[1,2] <- good.tab[1,2] + sum(out1[[good_stats]]) * Price[2, ][[good_stats]]
    good.tab[2,2] <- good.tab[2,2] + sum(out2[[good_stats]]) * Price[2, ][[good_stats]]
    good.tab[3,2] <- good.tab[3,2] + sum(out3[[good_stats]]) * Price[2, ][[good_stats]]
    good.tab[4,2] <- good.tab[4,2] + sum(out4[[good_stats]]) * Price[2, ][[good_stats]]
    good.tab[5,2] <- good.tab[5,2] + sum(out5[[good_stats]]) * Price[2, ][[good_stats]]
    good.tab[6,2] <- good.tab[6,2] + sum(out6[[good_stats]]) * Price[2, ][[good_stats]]
    good.tab[7,2] <- good.tab[7,2] + sum(out7[[good_stats]]) * Price[2, ][[good_stats]]
    good.tab[8,2] <- good.tab[8,2] + sum(out8[[good_stats]]) * Price[2, ][[good_stats]]
    good.tab[9,2] <- good.tab[9,2] + sum(out9[[good_stats]]) * Price[2, ][[good_stats]]
    good.tab[10,2] <- good.tab[10,2] + sum(out10[[good_stats]]) * Price[2, ][[good_stats]]
    
    good.tab[nrow(good.tab)-1,2] <- sum(good.tab[1:10,2])
    good.tab[nrow(good.tab),2] <- mean(good.tab[1:10,2])
    
    
  
    good.tab[1,3] <- good.tab[1,3] + sum(out1[[good_stats]]) * Price[2, ][[good_stats]] - sum(in1[[good_stats]]) * Price[1, ][[good_stats]] - (sum(in1[[good_stats]]) - sum(out1[[good_stats]])) * Price[3, ][[good_stats]]
    good.tab[2,3] <- good.tab[2,3] + sum(out2[[good_stats]]) * Price[2, ][[good_stats]] - sum(in2[[good_stats]]) * Price[1, ][[good_stats]] - (sum(in2[[good_stats]]) - sum(out2[[good_stats]])) * Price[3, ][[good_stats]]
    good.tab[3,3] <- good.tab[3,3] + sum(out3[[good_stats]]) * Price[2, ][[good_stats]] - sum(in3[[good_stats]]) * Price[1, ][[good_stats]] - (sum(in3[[good_stats]]) - sum(out3[[good_stats]])) * Price[3, ][[good_stats]]
    good.tab[4,3] <- good.tab[4,3] + sum(out4[[good_stats]]) * Price[2, ][[good_stats]] - sum(in4[[good_stats]]) * Price[1, ][[good_stats]] - (sum(in4[[good_stats]]) - sum(out4[[good_stats]])) * Price[3, ][[good_stats]]
    good.tab[5,3] <- good.tab[5,3] + sum(out5[[good_stats]]) * Price[2, ][[good_stats]] - sum(in5[[good_stats]]) * Price[1, ][[good_stats]] - (sum(in5[[good_stats]]) - sum(out5[[good_stats]])) * Price[3, ][[good_stats]]
    good.tab[6,3] <- good.tab[6,3] + sum(out6[[good_stats]]) * Price[2, ][[good_stats]] - sum(in6[[good_stats]]) * Price[1, ][[good_stats]] - (sum(in6[[good_stats]]) - sum(out6[[good_stats]])) * Price[3, ][[good_stats]]
    good.tab[7,3] <- good.tab[7,3] + sum(out7[[good_stats]]) * Price[2, ][[good_stats]] - sum(in7[[good_stats]]) * Price[1, ][[good_stats]] - (sum(in7[[good_stats]]) - sum(out7[[good_stats]])) * Price[3, ][[good_stats]]
    good.tab[8,3] <- good.tab[8,3] + sum(out8[[good_stats]]) * Price[2, ][[good_stats]] - sum(in8[[good_stats]]) * Price[1, ][[good_stats]] - (sum(in8[[good_stats]]) - sum(out8[[good_stats]])) * Price[3, ][[good_stats]]
    good.tab[9,3] <- good.tab[9,3] + sum(out9[[good_stats]]) * Price[2, ][[good_stats]] - sum(in9[[good_stats]]) * Price[1, ][[good_stats]] - (sum(in9[[good_stats]]) - sum(out9[[good_stats]])) * Price[3, ][[good_stats]]
    good.tab[10,3] <- good.tab[10,3] + sum(out10[[good_stats]]) * Price[2, ][[good_stats]] - sum(in10[[good_stats]]) * Price[1, ][[good_stats]] - (sum(in10[[good_stats]]) - sum(out10[[good_stats]])) * Price[3, ][[good_stats]]
  
    
    good.tab[nrow(good.tab)-1,3] <- sum(good.tab[1:10,3])
    good.tab[nrow(good.tab),3] <- mean(good.tab[1:10,3])
    
    good.tab[1,5] <- good.tab[1,5] - sum(out1[,2:ncol(out1)]) + sum(in1[,2:ncol(in1)])
    good.tab[2,5] <- good.tab[2,5] - sum(out2[,2:ncol(out2)]) + sum(in2[,2:ncol(in2)])
    good.tab[3,5] <- good.tab[3,5] - sum(out3[,2:ncol(out3)]) + sum(in3[,2:ncol(in3)])
    good.tab[4,5] <- good.tab[4,5] - sum(out4[,2:ncol(out4)]) + sum(in4[,2:ncol(in4)])
    good.tab[5,5] <- good.tab[5,5] - sum(out5[,2:ncol(out5)]) + sum(in5[,2:ncol(in5)])
    good.tab[6,5] <- good.tab[6,5] - sum(out6[,2:ncol(out6)]) + sum(in6[,2:ncol(in6)])
    good.tab[7,5] <- good.tab[7,5] - sum(out7[,2:ncol(out7)]) + sum(in7[,2:ncol(in7)])
    good.tab[8,5] <- good.tab[8,5] - sum(out8[,2:ncol(out8)]) + sum(in8[,2:ncol(in8)])
    good.tab[9,5] <- good.tab[9,5] - sum(out9[,2:ncol(out9)]) + sum(in9[,2:ncol(in9)])
    good.tab[10,5] <- good.tab[10,5] - sum(out10[,2:ncol(out10)]) + sum(in10[,2:ncol(in10)])
    good.tab[nrow(good.tab)-1,5] <- sum(good.tab[1:10,5])
    good.tab[nrow(good.tab),5] <- mean(good.tab[1:10,5])
    
    
    
    good.tab[1,6] <- good.tab[1,6] + round(sd(out1[[good_stats]]), 3)
    good.tab[2,6] <- good.tab[2,6] + round(sd(out2[[good_stats]]), 3)
    good.tab[3,6] <- good.tab[3,6] + round(sd(out3[[good_stats]]), 3)
    good.tab[4,6] <- good.tab[4,6] + round(sd(out4[[good_stats]]), 3)
    good.tab[5,6] <- good.tab[5,6] + round(sd(out5[[good_stats]]), 3)
    good.tab[6,6] <- good.tab[6,6] + round(sd(out6[[good_stats]]), 3)
    good.tab[7,6] <- good.tab[7,6] + round(sd(out7[[good_stats]]), 3)
    good.tab[8,6] <- good.tab[8,6] + round(sd(out8[[good_stats]]), 3)
    good.tab[9,6] <- good.tab[9,6] + round(sd(out9[[good_stats]]), 3)
    good.tab[10,6] <- good.tab[10,6] + round(sd(out10[[good_stats]]), 3)
  
    good.tab[nrow(good.tab)-1,6] <- sum(good.tab[1:10,6])
    good.tab[nrow(good.tab),6] <- mean(good.tab[1:10,6])
    
    
      
    
    
    
    rev1 <- sum(tables[[konkret$shop * 2]][,konkret$good]) * Price[2, ][[konkret$good]]
    profit1 <- sum(tables[[konkret$shop * 2]][,konkret$good]) * Price[2, ][[konkret$good]] - sum(tables[[konkret$shop * 2 - 1]][,konkret$good]) * Price[1, ][[konkret$good]] - (sum(tables[[konkret$shop * 2 - 1]][,konkret$good]) - sum(tables[[konkret$shop * 2]][,konkret$good])) * Price[3, ][[konkret$good]]
    konk.tab <- data.frame("Выручка руб"=rev1, "Прибыль руб"=profit1)             
    konk.tab$'Реализация, конт.' <- sum(tables[[konkret$shop * 2]][[konkret$good]])
    konk.tab$'Списание, конт.' <- sum(tables[[konkret$shop * 2 - 1]][[konkret$good]]) - sum(tables[[konkret$shop * 2]][[konkret$good]])
    konk.tab$'sd' <- round(sd(tables[[konkret$shop * 2]][[konkret$good]]), 3)
    konk.tab$'Продажи макс' <- max(tables[[konkret$shop * 2]][[konkret$good]])
    konk.tab$День <- which.max(tables[[konkret$shop * 2]][[konkret$good]])
    konk.tab$'Продажи мин' <- min(tables[[konkret$shop * 2]][[konkret$good]])
    konk.tab$' День' <- which.min(tables[[konkret$shop * 2]][[konkret$good]])
    konk.tab$'Списание макс' <- max(tables[[konkret$shop * 2 - 1]][,konkret$good] - tables[[konkret$shop * 2]][,konkret$good])
    konk.tab$'  День' <- which.max(tables[[konkret$shop * 2 - 1]][,konkret$good] - tables[[konkret$shop * 2]][,konkret$good])
    
    #-------------------------------------------------таблицы----------------------------------------------
    
    #write.table(good.tab, file = "/Users/nastya/Desktop/Корнеева Настя/Лизунец/Анализ/Таблица1.csv", sep = ";", row.names = FALSE)
    #write.table(shop.tab, file = "/Users/nastya/Desktop/Корнеева Настя/Лизунец/Анализ/Таблица2.csv", sep = ";", row.names = FALSE)
    #write.table(konk.tab, file = "/Users/nastya/Desktop/Корнеева Настя/Лизунец/Анализ/Таблица3.csv", sep = ";", row.names = FALSE)
}   
}
    #-------------------------------------------------графики----------------------------------------------
    
  #-------------------------------------------------5.1----------------------------------------------
    xrange = range(out1[,1])
    yrange = range(min(out1[,2]),
                     max(out1[,5]))
    plot(xrange,
         yrange,
         main='Магазин 1. Объем продаж акул', 
         xlab="День", 
         ylab="Количество проданного товара, шт",
         type = "n",
         cex.axis=0.8, 
         cex.lab=0.7, 
         cex.main=0.9)
    
    points(out1[,1], out1[,2], pch=20, col="red3")
    lines(out1[,1], out1[,2], pch=20, col="red3")
    
    points(out1[,1], out1[,3], pch=22, col="forestgreen")
    lines(out1[,1], out1[,3], pch=22, col="forestgreen")
    
    points(out1[,1], out1[,4], pch=24, col="steelblue")
    lines(out1[,1], out1[,4], pch=24, col="steelblue")
    
    points(out1[,1], out1[,5], pch=23, col="orange")
    lines(out1[,1], out1[,5], pch=23, col="orange")
    
    # Легенда
    legend("topright", legend=names(out1)[2:ncol(out1)],col=c("red3", "forestgreen", "steelblue", "orange"), pch=c(20,22,24,23))
    
    #продажа АКУЛ во втором магазине--------------------
    xrange = range(out2[,1])
    yrange = range(min(out2[,2]),
                   max(out2[,5]))
    plot(xrange,
         yrange,
         main='Магазин 2. Объем продаж акул', 
         xlab="День", 
         ylab="Количество проданного товара, шт",
         type = "n",
         cex.axis=0.8, 
         cex.lab=0.7, 
         cex.main=0.9)
    
    points(out2[,1], out2[,2], pch=20, col="red3")
    lines(out2[,1], out2[,2], pch=20, col="red3")
    
    points(out2[,1], out2[,3], pch=22, col="forestgreen")
    lines(out2[,1], out2[,3], pch=22, col="forestgreen")
    
    points(out2[,1], out2[,4], pch=24, col="steelblue")
    lines(out2[,1], out2[,4], pch=24, col="steelblue")
    
    points(out2[,1], out2[,5], pch=23, col="orange")
    lines(out2[,1], out2[,5], pch=23, col="orange")
    
    # Легенда
    legend("topright", legend=names(out2)[2:ncol(out2)],col=c("red3", "forestgreen", "steelblue", "orange"), pch=c(20,22,24,23))
    
  #-------------------------------------------------5.2----------------------------------------------  
    
    xrange = range(out1[,1])
    yrange = range(min(out1[,2] * akulsup),
                   max(out1[,5] * akulsup))
    plot(xrange,
         yrange,
         main='Магазин 1. Объем продаж акул', 
         xlab="День", 
         ylab="Количество проданного товара, шт",
         type = "n",
         cex.axis=0.8, 
         cex.lab=0.7, 
         cex.main=0.9)
    
    points(out1[,1], out1[,2] * akulsup, pch=20, col="red3")
    lines(out1[,1], out1[,2] * akulsup, pch=20, col="red3")
    
    points(out1[,1], out1[,3] * akulsup, pch=22, col="forestgreen")
    lines(out1[,1], out1[,3] * akulsup, pch=22, col="forestgreen")
    
    points(out1[,1], out1[,4] * akulsup, pch=24, col="steelblue")
    lines(out1[,1], out1[,4] * akulsup, pch=24, col="steelblue")
    
    points(out1[,1], out1[,5] * akulsup, pch=23, col="orange")
    lines(out1[,1], out1[,5] * akulsup, pch=23, col="orange")
    
    # Легенда
    legend("topright", legend=names(out1)[2:ncol(out1)],col=c("red3", "forestgreen", "steelblue", "orange"), pch=c(20,22,24,23))
    
    xrange = range(out2[,1])
    yrange = range(min(out2[,2]) * akulsup,
                   max(out2[,5]) * akulsup)
    plot(xrange,
         yrange,
         main='Магазин 2. Объем продаж акул', 
         xlab="День", 
         ylab="Количество проданного товара, шт",
         type = "n",
         cex.axis=0.8, 
         cex.lab=0.7, 
         cex.main=0.9)
    
    points(out2[,1], out2[,2] * akulsup, pch=20, col="red3")
    lines(out2[,1], out2[,2] * akulsup, pch=20, col="red3")
    
    points(out2[,1], out2[,3] * akulsup, pch=22, col="forestgreen")
    lines(out2[,1], out2[,3] * akulsup, pch=22, col="forestgreen")
    
    points(out2[,1], out2[,4] * akulsup, pch=24, col="steelblue")
    lines(out2[,1], out2[,4] * akulsup, pch=24, col="steelblue")
    
    points(out2[,1], out2[,5] * akulsup, pch=23, col="orange")
    lines(out2[,1], out2[,5] * akulsup, pch=23, col="orange")
    
    # Легенда
    legend("topright", legend=names(out2)[2:ncol(out2)],col=c("red3", "forestgreen", "steelblue", "orange"), pch=c(20,22,24,23))
  
  #-------------------------------------------------5.2.5----------------------------------------------
    
    xrange = range(in1[,1])
    yrange = range(min(in1[,2]),
                   max(in1[,5]))
    plot(xrange,
         yrange,
         main='Магазин 1. Объем поставок акул', 
         xlab="День", 
         ylab="Количество проданного товара, шт",
         type = "n",
         cex.axis=0.8, 
         cex.lab=0.7, 
         cex.main=0.9)
    
    points(in1[,1], in1[,2], pch=20, col="red3")
    lines(in1[,1], in1[,2], pch=20, col="red3")
    
    points(in1[,1], in1[,3], pch=22, col="forestgreen")
    lines(in1[,1], in1[,3], pch=22, col="forestgreen")
    
    points(in1[,1], in1[,4], pch=24, col="steelblue")
    lines(in1[,1], in1[,4], pch=24, col="steelblue")
    
    points(in1[,1], in1[,5], pch=23, col="orange")
    lines(in1[,1], in1[,5], pch=23, col="orange")
    
    # Легенда
    legend("topright", legend=names(out1)[2:ncol(in1)],col=c("red3", "forestgreen", "steelblue", "orange"), pch=c(20,22,24,23))
    
    xrange = range(in2[,1])
    yrange = range(min(in2[,2]),
                   max(in2[,5]))
    plot(xrange,
         yrange,
         main='Магазин 2. Объем поставок акул', 
         xlab="День", 
         ylab="Количество проданного товара, шт",
         type = "n",
         cex.axis=0.8, 
         cex.lab=0.7, 
         cex.main=0.9)
    
    points(in2[,1], in2[,2], pch=20, col="red3")
    lines(in2[,1], in2[,2], pch=20, col="red3")
    
    points(in2[,1], in2[,3], pch=22, col="forestgreen")
    lines(in2[,1], in2[,3], pch=22, col="forestgreen")
    
    points(in2[,1], in2[,4], pch=24, col="steelblue")
    lines(in2[,1], in2[,4], pch=24, col="steelblue")
    
    points(in2[,1], in2[,5], pch=23, col="orange")
    lines(in2[,1], in2[,5], pch=23, col="orange")
    
    # Легенда
    legend("topright", legend=names(out1)[2:ncol(in2)],col=c("red3", "forestgreen", "steelblue", "orange"), pch=c(20,22,24,23))
    
  #-------------------------------------------------5.3----------------------------------------------    
    {
    barplot(
        shop.tab[, 2], 
        names.arg = shop.tab[, 1], 
        main = "4 Магазин. Динамика прибыли",
        ylab = "Деньги",
        xlab = "Товары",
        col = c("#0099FF", "pink")
      )
      
    }
  #-------------------------------------------------5.4----------------------------------------------  
    
    barplot(good.tab[1:10, 3],
            names.arg = good.tab[1:10, 1], 
            main = "Прибыль по Акулам",
            ylab = "Деньги",
            xlab = "Магазины",
            col = "pink",
            las = 3)
    
  #-------------------------------------------------5.5----------------------------------------------  

    barplot(res.tab[1:10, 3],
            names.arg = res.tab[1:10, 1], 
            main = "Прибыль по Акулам",
            ylab = "Деньги",
            ylim = c(0, max(res.tab[1:10, 3])),
            col = "pink",
            las = 3)
    
    barplot(res.tab[1:10, 3],
            names.arg = res.tab[1:10, 1], 
            main = "Прибыль по Акулам",
            ylab = "Деньги",
            ylim = range(res.tab[1:10, 3]),
            col = "pink",
            las = 3)


res.tab