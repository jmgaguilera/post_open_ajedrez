# this script takes a data.frame of games and generate another data.frame with all movements.
rm(list = ls())

library(rchess)
library(magrittr)
library(parallel)
library(doParallel)
library(plyr)

load("games_anonymized.RData")

registerDoParallel(makeCluster(detectCores()))

df_all_movements <- adply(games, .margins = 1, function(x){
  df_all_movements = data.frame()
  
  for (p in 1:nrow(x)) {
    
    movements <- apply(x[p,], 1, function(x){
      chss <- Chess$new()
      chss$load_pgn(x["pgn"])
      chss$history()
    })
    
    # inicio tablero
    chss <- Chess$new()
    num_mov_next <- c()
    list_mov_next <- c()
    list1_mov_next <- c()
    # posición inicial
    next_movements <- chss$moves()
    num_mov_next %<>% append(length(next_movements))
    list_mov_next %<>% append(paste(next_movements, collapse="; "))
    list1_mov_next %<>% append(I(next_movements))
    
    for (i in movements) {
      chss$move(i)
      next_movements <- chss$moves()
      num_mov_next %<>% append(length(next_movements))
      list_mov_next %<>% append(paste(next_movements, collapse="; "))
      list1_mov_next %<>% append(I(next_movements))
    }
    
    df_movements <- data.frame(c("",movements), num_mov_next, list_mov_next, stringsAsFactors = F)
    colnames(df_movements) <- c("Ply", 'Num_Siguientes', 'Ply_posibles')
    df_movements %<>% mutate(num_ply = seq(nrow(.))-1)
    df_movements %<>% mutate(toca_mover=ifelse(df_movements$num_ply %% 2 == 0, 'w', 'b'))
    df_movements %<>% mutate(id = x[p,"id"])
    
    # añadir la posición desde la que se realizaba el movimiento
    
    df_movements %<>% left_join(chss$history_detail(), by = c("num_ply" = "number_move"))
    
    df_all_movements %<>% rbind.fill(df_movements)
  }
  return(df_all_movements)
}, .parallel = TRUE, .paropts = list(.packages = c("rchess", "magrittr", "dplyr")))

save(df_all_movements, file="all_movements.RData")