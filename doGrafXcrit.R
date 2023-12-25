#' @title doGrafXcrit
#' @description Grafico con los valores críticos para el caso de bucles programados
#' para la evaluacion de muchos compuestos
#' @param database Database obtenida con la funcion doEvalCal
#' @param type_cal valor numérico según el tipo de calibración 1=lineal,
#' 2=lineal ponderada y 3=polinomica
#' @param xcrit valor crítico a evaluar sobre las funciones de calibrado ej. 1 microg/L
#' @return Gráfico tipo lollipop con valores
#' @export doGrafXcrit
#' @author JMV
#'
doGrafXcrit=function(database,type_cal,xcrit){
  long_df=nrow(database)/3
  if(type_cal==1){
    cali='Linear'
  }else if(type_cal==2){
    cali='Linear-Weighted'
  }else if(type_cal==3){
    cali='Second-order'
  }
  filter=database[(database$XCRIT>xcrit & database$MODEL==cali) ,]
  long_filter=nrow(filter)
  long_dif=long_df-long_filter
  perc_filter=round(100*(long_filter/long_df),0)
  perc_resto=round(100*(long_dif/long_df),0)
  porcenta=c(perc_filter,perc_resto)
  tit_1=paste('Comply=', perc_filter, '%')
  tit_2=paste('Non-comply=', perc_resto, '%')
  library(ggplot2)
  x=filter$COMP
  y=round(filter$XCRIT,1)
  ggplot(filter, aes(x = x, y = y)) +
    geom_segment(aes(x = x, xend = x, y = 0, yend = y),
                 color = "gray", lwd = 1.5) +
    geom_point(size =7, pch = 21, bg = 4, col = 1) +
    geom_text(aes(label = y), color = "white", size = 3) +
    coord_flip() +
    labs(title = paste("Compounds with calibration ",cali, "and Xcritical > ",xcrit ),
         subtitle = paste(tit_1," // ",tit_2),
         caption = "Data source: doGrafXcrit")
}
