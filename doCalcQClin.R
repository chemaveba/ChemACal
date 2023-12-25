#' @title doCalcQClin
#' @description Calcula el coeficiente de calidad de una funcion de calibrado
#' para el caso lineal
#' @param x vector con los puntos de calibración
#' @param y vector con los valores de la respuesta instrumental
#' @param a valor de la ordenada para la calibración lineal
#' @param b valor de la pendiente para la calibracion lineal
#' @return Valor del QC en porcentaje
#' @export doCalcQClin
#' @author JMV
#' @examples
#'x=c(0.5,1.2,3,6,12,24)
#'y=c(24072,42164,102078,236920,448992,936145)
#'a=-4877
#'b=38964
#'doCalcQClin(x,y,a,b)
#'
doCalcQClin=function (x,y,a,b){
  mediay=mean(y)
  for(i in y){
    result=((y-(a+(b*x)))/mediay)^2
    #interm=((i-ymodel)/mediay)^2
  }
  result=100*sqrt((sum(result))/(length(y)-2))
  result=round(result,1)
  return (result)
}
