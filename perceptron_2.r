itter_counter <<- c(0,0,0)

setwd("D:/IATE/Pattern_Recognition/lab5")
r_file <- read.table( file = "Blinov.txt",header=TRUE)

X_Y_clast <- matrix(data=unlist(r_file, recursive = TRUE, use.names = FALSE), nrow=3, 
                    byrow=TRUE,dimnames=list(c("X","Y","Clast")))
real_index <- c(1:length(X_Y_clast[1,]))
X_Y_clast <- rbind(X_Y_clast, real_index)

N = 40                                 # Количество иттераций для остановки
W_search = function(first, second){
  check_count = 0
  flag = TRUE
  W <- c(0,1,2)                        # Произвольный начальный вектор весов
  W_check <- W
  C = 0
  
  x <- c()
  x <- c(X_Y_clast[,X_Y_clast[3,] == first][1,],
         c(-1) %*% X_Y_clast[,X_Y_clast[3,] == second][1,])     # Берём первые координаты образов из кластеров 1 и 2
  y_tmp <- c(X_Y_clast[,X_Y_clast[3,] == first][2,],
             c(-1) %*% X_Y_clast[,X_Y_clast[3,] == second][2,]) # Берём вторые координаты образов из кластеров 1 и 2
  index_tmp <- c(X_Y_clast[,X_Y_clast[3,] == first][4,],
                 X_Y_clast[,X_Y_clast[3,] == second][4,])       # Берём индексы координат
  x <- rbind(x,y_tmp)
  x <- rbind(x, index_tmp)
  x_last = c(1)

  for(i in 1:length(x[1,])){      # Дописываем 1 в конец каждого x(k)
    if(X_Y_clast[3,][x[3,][i]] == second){
      x_last[i] = -1
    } else {
      x_last[i] = 1
    }
  }
  
  x <- rbind(x[1,], x[2,], x_last)
  # x <- rbind(x,x_last)
  W_check <- W
  k = 1
  lenx = length(x[1,])
  while(flag){                                                  # Перцептрон
    lambda <- 1.6
    C <- 1
    
    if(k %% lenx != 0){
      x_k <- x[,k%%lenx]
    } else {
      x_k <- x[,lenx]
    }

    # if((W %*% x_k) <= 0){
    #   W <- W + (x_k*C)
    # }
    
    if((W %*% x_k) <= 0){
      C <- ceiling(as.numeric(-((W %*% x_k)/(x_k %*% x_k))))    # Коррекция абсолютной величины
      if(C == 0){C <- 1}
      W <- W + (x_k*C)
    }
    
    # if((W %*% x_k) <= 0){
    # 
    #   C <- lambda*(abs(as.numeric( (W %*% x_k)/(x_k %*% x_k) )))
    #   if(C == 0){C = lambda}
    #   W <- W + (x_k*C)
    # }
    
    if(isTRUE(all.equal(W,W_check))){
      check_count <- check_count + 1
    } else {
      W_check <- W
      check_count <- 0
    }
    if(check_count == N){
      flag = FALSE
    }
    k <- k+1
  }
  itter_counter[first] <<- k
  x <- cbind(x,W)
  return(x)
}

W_matrix <- c()

# W_matrix <- cbind(W_matrix,W_search(1,2)[,"W"])
# W_matrix <- cbind(W_matrix,W_search(3,1)[,"W"])
# W_matrix <- cbind(W_matrix,W_search(1,3)[,"W"])

for(l in 1:length(unique(X_Y_clast[3,]))){    # Ищем вектора весов для кластеров 1 и 2; 2 и 3; 3 и 1
  if(l == length(unique(X_Y_clast[3,]))){
    W_matrix <- cbind(W_matrix,W_search(l,1)[,"W"])
  } else {
    W_matrix <- cbind(W_matrix,W_search(l,l+1)[,"W"])
  }
}

print(W_matrix)

plot(X_Y_clast[1,], X_Y_clast[2,], type = "n", xlab = "x", ylab = "y")

xlines = seq(min(-1), max(16), 1)
ylines = seq(ceiling(min(0)),
             floor(max(8)), 1)
abline(h = ylines, v = xlines, col = "lightgray")
t_red = 30
t_green = 60
t_blue = 60
palitra <- data.frame(R = t_red, G = t_green, B = t_blue)

for (i in 1:length(unique(X_Y_clast[3,])))
{
  points(X_Y_clast[1,which(X_Y_clast[3,]==i)],X_Y_clast[2,which(X_Y_clast[3,]==i)], 
         col=rgb(as.numeric(palitra$R[i]),
                 as.numeric(palitra$G[i]),
                 as.numeric(palitra$B[i]), 
                 max = 255),
         type = "p", pch = 16, cex = 1)
  palitraNew <- data.frame(R = palitra$R[i]+t_red*(i%%2), 
                           G = palitra$G[i]+t_green, 
                           B = palitra$B[i]+t_blue*(i%%2))
  palitra <- rbind(palitra, palitraNew)
}

for(i in 1:length(unique(X_Y_clast[3,]))){
  x_1 = 15.99
  tmp_1 <-c (99,99)
  tmp_2 <- c(99,99)
  if(i == 1){
    klkl <- "black"
  }
  if(i == 2){
    klkl <- "blue"
  }
  if(i == 3){
    klkl <- "green"
  }
  while(x_1 > -1){
    y_1 = 0.01
    while(y_1 <= 7.5){
      z <- c(x_1, y_1, 1)
      
      if(abs(z %*% W_matrix[,i]) <= 0.05){
        if(tmp_1[1] == 99){
          tmp_1[1] <- x_1
          tmp_1[2] <- y_1
        }
        tmp_2[1] <- x_1
        tmp_2[2] <- y_1
      }
      y_1 = y_1 + 0.01
    }
    x_1 = x_1 - 0.01
  }
  points(c(tmp_1[1],tmp_2[1]),c(tmp_1[2],tmp_2[2]), col = klkl, type = "o")
  print(tmp_1)
  print(tmp_2)
}

legend("bottom", legend = c("1-2", "2-3", "3-1"),
       lwd = 2, col = c("black",
                        "blue",
                        "green"),
       bg = rgb(red = 0, green = 0, blue = 0, alpha = 0)
)

legend("topright", legend = c("class_1", "class_2", "class_3"), lty = 2,
       lwd = 3, col = c(rgb(as.numeric(palitra$R[1]),as.numeric(palitra$G[1]),as.numeric(palitra$B[1]), max = 255),
                        rgb(as.numeric(palitra$R[2]),as.numeric(palitra$G[2]),as.numeric(palitra$B[2]), max = 255),
                        rgb(as.numeric(palitra$R[3]),as.numeric(palitra$G[3]),as.numeric(palitra$B[3]), max = 255))
)

box()
point_class_1 <- function(x1,x2){
  z <- c(x1,x2,1)
  z_belong <- c(0,0,0)
  
  for(i in 1: length(unique(X_Y_clast[3,]))){
    # print(z %*% W_matrix[,i])
    if(i != length(unique(X_Y_clast[3,]))){
      if(z %*% W_matrix[,i] < 0){
        z_belong[i+1] <- z_belong[i+1] + 1
      } else{
        z_belong[i] <- z_belong[i] + 1
      }
    } else {
      if(z %*% W_matrix[,i] < 0){
        z_belong[1] <- z_belong[1] + 1
      } else{
        z_belong[i] <- z_belong[i] + 1
      }
    }
  }
  
  print(W_matrix)
  for(i in 1:length(unique(X_Y_clast[3,]))){
    if(z_belong[i] >= 2){
      points(x = z[1], y = z[2],
             col = rgb(as.numeric(palitra$R[i]),
                       as.numeric(palitra$G[i]),
                       as.numeric(palitra$B[i]),
                       max = 255),
             pch = 16,
             cex = 1.2)
      print(sprintf("[%f;%f] принадлежит классу %i",z[1],z[2],i))
      break
    }
    if(i == length(unique(X_Y_clast[3,]))){
      print(sprintf("[%f;%f] принадлежит ОНР",z[1],z[2]))
      points(x = z[1], y = z[2],
             col = "red",
             pch = 16,
             cex = 1.2)
    }
  }
}

point_class_1(7,4)
print(itter_counter)

