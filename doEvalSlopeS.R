#' @title doEvalSlopeS
#' @description Calcula la pendiente para una calibración lineal polinómica
#' @param conc vector con los puntos de calibración
#' @param response vector con los valores de la respuesta instrumental
#' @return Valor de la pendiente
#' @export doEvalSlopeS
#' @author JMV
#' @examples
#'conc=c(0.5,1.2,3,6,12,24)
#'response=c(24072,42164,102078,236920,448992,936145)
#'doEvalSlopeS(conc,response)
#'
doEvalSlopeS=function(conc,response){
  DF<-data.frame(y=response, x=conc)
  modelP=lm(y~x+I(x^2),data=DF)
  coefficient1=modelP$coefficients[2]
  coefficient2=modelP$coefficients[3]
  slope=coefficient1+(2*coefficient2*(mean(conc)))
  return (slope)
}
