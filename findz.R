##' Hilfsfunktion - sucht den z-Wert für die Erstellung von Konfidenzintervallen
#'
#' Diese Funktion gibt den zu alpha gehörenden z-Wert zurück

#' @param alpha fFür Konfidenzintervalle und Signifikanztests. Default ist 0.05
#' @export findez
#' @examples findez()

findez <- function(alpha = 0.05){
  z <- qnorm(p = 1- (alpha/2), mean = 0, sd = 1)
  return(z)
}