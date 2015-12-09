library(rchess)
library(dplyr)
library(ggplot2)

load("all_movements.RData")

dfboard <- rchess:::.chessboarddata() %>%
  select(cell, col, row, x, y, cc)

saveGIF({
  for (i in 1:14) {
    dfpaths <- df_all_movements %>% filter(num_ply <= i) %>%
      left_join(dfboard %>% rename(from = cell, x.from = x, y.from = y),
                by = "from") %>%
      left_join(dfboard %>% rename(to = cell, x.to = x, y.to = y) %>% select(-cc, -col, -row),
                by = "to") %>%
      mutate(x_gt_y = abs(x.to - x.from) > abs(y.to - y.from),
             xy_sign = sign((x.to - x.from)*(y.to - y.from)) == 1,
             colores = ifelse(toca_mover=="w", "white", "black"),
             x_gt_y_equal_xy_sign = x_gt_y == xy_sign)
    
    if ((i %% 2) == 1 ) {
      c_pieza = "white"
    } else {
      c_pieza = "black"
    }
    titulo = paste("jugado el ", i, "º movimiento.", sep="")
    
    g <- ggplot(data = dfpaths) +
      geom_tile(data = dfboard, aes(x, y, fill = cc)) +
      geom_curve(data = dfpaths,
                 aes(x = x.from, y = y.from, xend = x.to, yend = y.to, colour=toca_mover),
                 position = position_jitter(width = 0.2, height = 0.2),
                 curvature = 0.50, angle = -45, alpha = 0.02, size = 1.05) +
      scale_color_manual(values=c("white", "black")) +
      scale_fill_manual(values =  c("gray35", "gray65")) +
      ggtitle(titulo) +
      coord_equal()
    
    print(g)
  }
},
movie.name = "board_movement.gif", ani.width=600, ani.height=600)