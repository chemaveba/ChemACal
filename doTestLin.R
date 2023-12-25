#' @title doTestLin
#' @description Calcula la linealidad según el test de la ISO 8466-1 acorde
#' al test de Mandel (Anexo A.2)
#' @param sylin Desviacion estándar de los residuales para el caso lineal
#' @param sypol Desviación estándar de los residuales para el caso polinómico
#' @param N Número de puntos que forma la funcion de calibracion
#' @return La mejor calibracion en función del valor del test
#' @export doTestLin
#' @author JMV
#'
doTestLin=function(sylin,sypol,N){
  DS2=((N-2)*sylin^2)-((N-3)*sypol^2)
  fcalc=DS2/sypol^2
  fcrit=qf(p=.01, df1=1, df2=(N-3), lower.tail=FALSE)
  if(fcalc<fcrit){
    calibration='Linear'
  }else{
    calibration='Other calibration'
  }
  return (calibration)
}
