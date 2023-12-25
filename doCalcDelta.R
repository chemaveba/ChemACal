#' @title doCalcDelta
#' @description Calcula el valor de delta segun la Norma ISO 11843-1. Para los
#' casos no tabulados (>50 gl) utiliza una aproximación usando la distribución de probabilidad
#' de student
#' @param gl Grados de libertad atendiendo a la función de calibración
#' @return el valor de delta
#' @export doCalcDelta
#' @author JMV
#' @examples
#' gl=4
#' doCalcDelta(4);
#'
doCalcDelta=function (gl){
  if(gl>=2 && gl<=50){
    delta=switch(gl, "2"=5.516, "3"=4.456, "4"=4.067,"5"=3.870,"6"=3.752,"7"=3.673,"8"=3.617,"9"=3.575,"10"=3.543,
                 "11"=3.517,"12"=3.496, "13"=3.479, "14"=3.464,"15"=3.451,"16"=3.440,"17"=3.431,"18"=3.422,"19"=3.415,
                 "20"=3.408,"21"=3.402,"22"=3.397,"23"=3.392, "24"=3.387,"25"=3.383,"26"=3.380,"27"=3.376,"28"=3.373,
                 "29"=3.370,"30"=3.367,"31"=3.365,"32"=3.362,"33"=3.360,"34"=3.358, "35"=3.356,"36"=3.354,"37"=3.352,
                 "38"=3.350,"39"=3.349,"40"=3.347,"41"=3.346,"42"=3.344,"43"=3.343,"44"=3.342,"45"=3.341, "46"=3.339,
                 "47"=3.338,"48"=3.337,"49"=3.336,"50"=3.335)
  }else{
    delta=2*(qt(.05, df = gl,lower.tail=FALSE))
  }
  return(delta)

}

