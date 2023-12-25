#' @title doEvalSlopeL
#' @description Calcula la pendiente para una calibración lineal
#' @param conc vector con los puntos de calibración
#' @param response vector con los valores de la respuesta instrumental
#' @return Valor de la pendiente
#' @export doEvalSlopeL
#' @author JMV
#' @examples
#'conc=c(0.5,1.2,3,6,12,24)
#'response=c(24072,42164,102078,236920,448992,936145)
#'doEvalSlopeL(conc,response)
#'
doEvalSlopeL=function(conc,response){
  DF<-data.frame(y=response, x=conc)
  model=lm(y~x,data=DF)
  slope=model$coefficients[2]
  return (slope)
}
