#' @title doCalcQCPol
#' @description Calcula el coeficiente de calidad de una funcion de calibrado
#' para el caso polinomico
#' @param x vector con los puntos de calibración
#' @param y vector con los valores de la respuesta instrumental
#' @param a valor del coeficiente a para la calibración polinómica
#' @param b valor del coeficiente b para la calibración polinómica
#' @param c valor del coeficiente c para la calibracion polinómica
#' @return Valor del QC en porcentaje
#' @export doCalcQCPol
#' @author JMV
#' @examples
#'x=c(0.5,1.2,3,6,12,24)
#'y=c(24072,42164,102078,236920,448992,936145)
#'a=1263.025
#'b=36548.64
#'c=99.08992
#'doCalcQCPol(x,y,a,b,c);
#'
doCalcQCPol=function (x,y,a,b,c){
  mediay=mean(y)
  for(i in y){
    result=((y-(a+(b*x)+(c*x*x)))/mediay)^2
    #interm=((i-ymodel)/mediay)^2
  }
  result=100*sqrt((sum(result))/(length(y)-3))
  result=round(result,1)
  return (result)
}
