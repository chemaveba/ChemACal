#' @title doGrafME
#' @description Grafico para visualizar los efectos matriciales por compuesto
#' @param database Database obtenida con la funcion doEvalME
#' @return Gráfico tipo lollipop con valores
#' @export doGrafME
#' @author JMV
#'
doGrafME=function(database){
  long_df=nrow(database)
  filterlow=database[(database$INTERP=="low" ) ,]
  filtermod=database[(database$INTERP=="moderate" ) ,]
  filterhigh=database[(database$INTERP=="high" ) ,]
  long_filter=nrow(filterlow)
  long_dif=long_df-long_filter
  perc_filter=round(100*(long_filter/long_df),0)
  perc_resto=round(100*(long_dif/long_df),0)
  porcenta=c(perc_filter,perc_resto)
  library(ggplot2)
  library(ggpubr)
  tit_1=paste("Low ME= ",perc_filter)
  tit_2=paste("moderate and high ME = ",perc_resto)
  xxx=filterlow$COMP
  yyy=filterlow$MEDF
  gf1=ggplot(filterlow, aes(x = xxx, y = yyy)) +
    geom_segment(aes(x = xxx, xend = xxx, y = 0, yend = yyy),
                 color = "gray", lwd = 1.5) +
    geom_point(size =7, pch = 21, bg = 4, col = 1) +
    geom_text(aes(label = yyy), color = "white", size = 3) +
    coord_flip() +
    labs(title = paste("Low Matrix effect (ME)" ),
         subtitle = paste(tit_1," // ",tit_2),
         caption = "Data source: doGrafME")
  x=filtermod$COMP
  y=filtermod$MEDF
  gf2=ggplot(filtermod, aes(x = x, y = y)) +
    geom_segment(aes(x = x, xend = x, y = 0, yend = y),
                 color = "gray", lwd = 1.5) +
    geom_point(size =7, pch = 21, bg = 4, col = 1) +
    geom_text(aes(label = y), color = "white", size = 3) +
    coord_flip() +
    labs(title = paste("Moderate Matrix effect (ME)" ),
         subtitle = paste(tit_1," // ",tit_2),
         caption = "Data source: doGrafME")
  xx=filterhigh$COMP
  yy=filterhigh$MEDF
  gf3=ggplot(filterhigh, aes(x = xx, y = yy)) +
    geom_segment(aes(x = xx, xend = xx, y = 0, yend = yy),
                 color = "gray", lwd = 1.5) +
    geom_point(size =7, pch = 21, bg = 4, col = 1) +
    geom_text(aes(label = yy), color = "white", size = 3) +
    coord_flip() +
    labs(title = paste("High Matrix effect (ME)" ),
         subtitle = paste(tit_1," // ",tit_2),
         caption = "Data source: doGrafME")
  ggarrange(gf1, gf2, gf3, ncol = 2, nrow = 2)
}
