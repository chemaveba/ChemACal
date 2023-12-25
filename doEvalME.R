#' @title doEvalME
#' @description Evalúa si existe efecto matricial entre dos calibraciones
#' @param name string con el nombre del compuesto ej. 'Aldrin'
#' @param conc1 vector con los puntos de calibración de la calibración 1
#' @param conc2 vector con los puntos de calibración de la calibración 2
#' @param response1 vector con los valores de la respuesta instrumental de la calibración 1
#' @param response2 vector con los valores de la respuesta instrumental de la calibración 2
#' @param type_cal valor numérico según el tipo de calibración 1=lineal, 2=lineal ponderada y 3=polinomica
#' @return Dataframe con el nombre del compuesto, la calibracion evaluada, el valor del efecto matricial y la interpretacion
#' @export doEvalME
#' @author JMV
#' @examples
#' name="Chlorpropham (ng/ml)"
#' conc1=c(3,5,10,50,100)
#' conc2=c(3,5,10,50,100)
#' response1=c(25064,30982,62871,312296,668448)
#' response2=c(21859,30727,59158,334537,676464)
#' type_cal=1
#' doEvalME=(name,conc1,response1,conc2,response2,type_cal)
doEvalME=function(name,conc1,response1,conc2,response2,type_cal){
  if(type_cal==1){
    slope_ini=doEvalSlopeL(conc1,response1)
    slope_fin=doEvalSlopeL(conc2,response2)
    model='Linear'
  }else if (type_cal==2){
    slope_ini=doEvalSlopeLW(conc1,response1)
    slope_fin=doEvalSlopeLW(conc2,response2)
    model='Linear-Weighted'
  }else{
    slope_ini=doEvalSlopeS(conc1,response1)
    slope_fin=doEvalSlopeS(conc2,response2)
    model='Second-order'
  }
  ME=100*((slope_fin/slope_ini)-1)
  ME=round(ME,1)
  if(ME<=20){
    interp='low'
  }else if(ME>20 && ME<=50){
    interp='moderate'
  }else{
    interp='high'
  }
  #Results in DF
  COMP = c(name)
  MODEL = c(model)
  MEDF = c(ME)
  INTERP = c(interp)
  FINALME = data.frame(COMP, MODEL, MEDF, INTERP)
  return (FINALME)

}
