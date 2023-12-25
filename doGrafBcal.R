#' @title doGrafBcal
#' @description Grafico para visualizar toda la informacion (QC, capacidad de deteccion
#' y respuesta mínima para cada compuesto)
#' @param database Database obtenida con la funcion doEvalCal
#' @return Gráfico tipo lollipop con valores
#' @export doGrafBcal
#' @author JMV
#'
doGrafBcal=function(database){
  # Libraries
  library(ggplot2)
  library(dplyr)
  library(plotly)
  library(viridis)
  library(hrbrthemes)
  data=database
  p <- data %>%
    # Reorder COD to having big bubbles on top
    arrange(desc(XCRIT)) %>%
    # prepare text for tooltip
    mutate(text = paste("QC: ", QC, "\nCOD (uds): ", XCRIT, "\nScore: ", SCHEMA, "\nResponse min: ",
                        round(MINRESPONSE,0),"\nModel: ", MODEL,"\nCompound: ", COMP, sep="")) %>%
    # Classic ggplot
    ggplot( aes(x=SCHEMA, y=QC, size = XCRIT, color = MODEL, text=text)) +
    geom_point(alpha=0.7) +
    scale_size(range = c(1.4, 19), name="Model") +
    scale_color_viridis(discrete=TRUE, guide=FALSE) +
    theme_ipsum()
  # turn ggplot interactive with plotly
  pp <- ggplotly(p, tooltip="text")
  pp
}
