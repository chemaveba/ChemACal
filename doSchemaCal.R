#' @title doSchemaCal
#' @description Calcula el coeficiente de calidad de una funcion de calibrado
#' para el caso lineal
#' @param all_qc vector con todos los valores de QC de cada una de las calibraciones
#' @param all_lcrit vector con los valores de valores críticos de cada una de las calibraciones
#' @param sylin valor de la desviación estándar de los residuales caso lineal
#' @param sylinW valor de la desviación estándar de los residuales caso lineal ponderado
#' @param sypol valor de la desviación estándar de los residuales caso polinómico
#' @param N números de puntos que forman la calibración
#' @return Puntuación total sobre 100 de cuál es la mejor calibración
#' @export doSchemaCal
#' @author JMV
#'
doSchemaCal=function(all_qc,all_lcrit,sylin,sylinW,sypol,N){
  min_qc=min(all_qc)
  for(i in all_qc){
    factor_qc=round(((min_qc*30)/all_qc),0)
  }
  min_lcrit=min(all_lcrit)
  for(i in all_lcrit){
    factor_lcrit=round(((min_lcrit*30)/all_lcrit),0)
  }
  factor_linealidad=doTestLinT(sylin,sylinW,sypol,N)
  SchemaLin=factor_linealidad$Lineal+factor_qc[1]+factor_lcrit[1]
  SchemaLinW=factor_linealidad$LinealW+factor_qc[2]+factor_lcrit[2]
  SchemaPol=factor_linealidad$Polynomial+factor_qc[3]+factor_lcrit[3]
  Schemalist=list("Lineal" = SchemaLin, "LinealW" = SchemaLinW, "Polynomial" = SchemaPol)
  return (Schemalist)
}
