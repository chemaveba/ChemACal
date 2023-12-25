#' @title doPuntoCritico
#' @description Calcula si la calibración de segundo orden tiene un punto crítico
#' según lo indicado en la Norma ISO 8466-2
#' @param x vector con los puntos de calibración
#' @param b valor del coeficiente b para la calibración de segundo orden
#' @param c valor del coeficiente c para la calibración de segundo orden
#' @return Si hay o no punto crítico
#' @export doPuntoCritico
#' @author JMV
#' @examples
#'x=c(0.5,1.2,3,6,12,24)
#'b=36548.64
#'c=99.08992
#'doPuntoCritico(x,b,c)
#'
doPuntoCritico=function(x,b,c){
  punto=(-b)/(2*c)
  if(punto>0){
    ptomin=min(x)
    ptomax=max(x)
    if(punto>ptomin && punto<ptomax){
      puntocritico='Yes'
    }else{
      puntocritico='No'
    }
  }else{
    puntocritico='Error (point not detected)'
  }

  return (puntocritico)
}
