#' @title doTestLinT
#' @description Calcula la linealidad según el test de la ISO 8466-1 acorde
#' al test de Mandel (Anexo A.2) al igual que doTestLin pero puntuando entre
#' las tres calibraciones evaluadas (lineal, lineal ponderada y polinomica)
#' @param sylin Desviacion estándar de los residuales para el caso lineal
#' @param sylinW Desviación estándar de los residuales para el caso lineal ponderado
#' @param sypol Desviación estándar de los residuales para el caso polinómico
#' @param N Número de puntos que forma la funcion de calibracion
#' @return Puntuación para cada calibración
#' @export doTestLinT
#' @author JMV
#'
doTestLinT=function(sylin,sylinW,sypol,N){
  DS2=((N-2)*sylin^2)-((N-3)*sypol^2)
  fcalc=DS2/sypol^2
  fcrit=qf(p=.01, df1=1, df2=(N-3), lower.tail=FALSE)
  if(fcalc<fcrit){
    #Comparing between linear models
    Weightcalibration=doTestLin(sylin,sylinW,N)
    if(Weightcalibration=='Linear'){
      scorelineal=40
      scorelinealW=20
      scorePoly=0
    }else{
      scorelineal=20
      scorelinealW=40
      scorePoly=0
    }
  }else{
    calibration='Polynomial'
    polynomialcalibration=doTestLin(sylinW,sypol,N)
    if(polynomialcalibration=='Linear'){
      scorelineal=0
      scorelinealW=40
      scorePoly=20
    }else{
      scorelineal=0
      scorelinealW=20
      scorePoly=40
    }
  }
  Testlist=list("Lineal" = scorelineal, "LinealW" = scorelinealW, "Polynomial" = scorePoly)
  return (Testlist)
}
