#' @title doEvalCal
#' @description Diferentes cálculos para las diferentes calibraciones estudiadas (lineal,
#' lineal ponderada y polinómica)
#' @param name string con el nombre del compuesto ej. 'Aldrin'
#' @param conc vector con los puntos de calibración
#' @param response vector con los valores de la respuesta instrumental
#' @return Dataframe con la información del modelo, R2, QC, capacidad de detección,
#' respuesta mínima y comentarios
#' @export doEvalCal
#' @author JMV
#' @examples
#' name="Metamidofos (ng/ml)"
#' conc=c(0.5,1.2,3,6,12,24)
#' y=c(24072,42164,102078,236920,448992,936145)
#' doEvalCal(name,conc,response);
#'
doEvalCal=function(name,conc,response){
  weights=1/conc
  DF<-data.frame(y=response, x=conc)
  #Linear case
  model=lm(y~x,data=DF)
  intercept=model$coefficients[1]
  slope=model$coefficients[2]
  sxy=summary(model)$sigma
  r2=summary(model)$r.squared
  eq=paste('Y=',round(intercept,3),'+',round(slope,3),'* X')
  qcl=doCalcQClin(conc,response,intercept,slope)
  coment_linear=''

  #Linear-Weighted case
  modelW=lm(y~x,data=DF,weights=(1/x))
  interceptW=modelW$coefficients[1]
  sxyW=summary(modelW)$sigma
  slopeW=modelW$coefficients[2]
  r2W=summary(modelW)$r.squared
  eqW=paste('Y=',round(interceptW,3),'+',round(slopeW,3),'* X')
  qclw=doCalcQClin(conc,response,interceptW,slopeW)
  coment_linearW=''

  #Second order case
  modelP=lm(y~x+I(x^2),data=DF)
  interceptP=modelP$coefficients[1]
  coefficient1=modelP$coefficients[2]
  coefficient2=modelP$coefficients[3]
  eqP=paste('Y=',round(interceptP,3),'+',round(coefficient1,3),'* X +',round(coefficient2,3),'*X^2')
  r2P=summary(modelP)$r.squared
  sxyP=summary(modelP)$sigma
  slopeP=coefficient1+(2*coefficient2*(mean(conc)))
  qcP=doCalcQCPol(conc,response,interceptP,coefficient1,coefficient2)
  critical_point=doPuntoCritico(conc,coefficient1,coefficient2)
  if(critical_point=='Yes'){
    coment_second='Danger: Critical point detected'
  }else{
    coment_second=''
  }
  #ISO 11843 linear
  gl=length(conc)-2
  delta=doCalcDelta(gl)
  sxx=sum((conc - mean(conc))^2)
  X=(sum(weights*conc))/(sum(1/conc))
  lod=((delta*sxy)/slope)*sqrt(1+(1/length(conc))+(X^2/sxx))

  #ISO 11843 linear weighted
  sxxW=sum(weights*(conc - mean(conc))^2)
  XW=(sum(weights*conc))/(sum(1/conc))
  lodW=((delta*sxyW)/slopeW)*sqrt(1+(1/(sum(1/conc)))+(XW^2/sxxW))

  #ISO 11843 non linear
  N=length(conc)
  N.hat= 1
  Qxx=sum(conc^2) - sum(conc)^2/N
  Qx3=sum(conc^3) -(sum(conc) * sum(conc^2)/N)
  Qx4=sum(conc^4) - sum(conc^2)^2/N
  x.hat=conc[1]
  lodP=(sxyP*delta)/(coefficient1 + 2*coefficient2*x.hat)*sqrt(
    1/N + 1/N.hat + ((x.hat - mean(conc))^2*Qx4 + (x.hat^2 - sum(conc^2)/N)^2 * Qxx -
                       2*(x.hat - mean(conc))*(x.hat^2 - sum(conc^2)/N)*Qx3)/
      (Qx4 * Qxx - Qx3^2)
  )

  # Final results
  all_qc=c(qcl,qclw,qcP)
  all_lcrit=c(lod,lodW,lodP)
  chemaresult=doSchemaCal(all_qc,all_lcrit,sxy,sxyW,sxyP,N)
  #Results in DF
  COMP = c(name,name,name)
  MODEL = c('Linear','Linear-Weighted','Second-order')
  EQUAT = c(eq,eqW,eqP)
  R2 = c(round(r2,4),round(r2W,4),round(r2P,4))
  XCRIT = c(signif(lod,3),signif(lodW,3),signif(lodP,3))
  QC = c(qcl,qclw,qcP)
  min_response=min(response)
  COMENTS=c(coment_linear,coment_linearW,coment_second)
  MINRESPONSE=c(min_response,min_response,min_response)
  SCHEMA = c(chemaresult$Lineal,chemaresult$LinealW,chemaresult$Polynomial)
  FINAL = data.frame(COMP, MODEL, R2, EQUAT, XCRIT, QC, SCHEMA, MINRESPONSE, COMENTS)
  return (FINAL)
}
