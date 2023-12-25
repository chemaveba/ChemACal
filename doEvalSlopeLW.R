#' @title doEvalSlopeLW
#' @description Calcula la pendiente para una calibración lineal ponderada
#' @param conc vector con los puntos de calibración
#' @param response vector con los valores de la respuesta instrumental
#' @return Valor de la pendiente (ponderacion 1/x)
#' @export doEvalSlopeLW
#' @author JMV
#' @examples
#'conc=c(0.5,1.2,3,6,12,24)
#'response=c(24072,42164,102078,236920,448992,936145)
#'doEvalSlopeLW(conc,response)
#'
doEvalSlopeLW=function(conc,response){
  DF<-data.frame(y=response, x=conc)
  modelW=lm(y~x,data=DF,weights=(1/x))
  slope=modelW$coefficients[2]
  return (slope)
}
