#' Redondear
#'
#' @description
#' Función que se utiliza para redondear un número. Se utiliza esta función para asegurar que los valores decimales con pequenas diferencias no generen cambios mayores al imputar.
#' Para ello, esta función primero trunca al decimal deseado y después redondea al decimal deseado.
#'
#'
#' @param numero El número a redondear.
#' @param decimales_truncado La cantidad de dígitos a la que se truncará el decimal.
#' @param decimales_redondeo la cantidad de decimales a la que se redondeará el número.
#'
#' @return El mismo número de input redondeado según los parámetros especificados.
#' @export
#'
#' @examples
#' a <- 0.0049999999999
#' redondear(a, decimales_truncado = 6, decimales_redondeo = 0)
redondear <- function(numero, decimales_truncado = 6, decimales_redondeo = 0) {
  factor <- 10 ^ decimales_truncado
  valor_truncado <- ceiling(numero * factor) / factor

  valor_truncado_redondeado <- round(valor_truncado, digits = decimales_redondeo)

  return(valor_truncado_redondeado)
}
