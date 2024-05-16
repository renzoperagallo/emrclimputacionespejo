#' Calcular razones
#'
#' @description
#' Función que calcula una columna con una razon en un data frame. Si  ambos valores son 0, resulta 0, si existe un divisor 0, da 0 (en vez de infinito).
#'
#' @param df Dataframe sobre el que se calculará la razón.
#' @param razon El nombre de la columna con la razon resultante. Puede ser entregado sin comillas.
#' @param dividendo El nombre del dividendo de  la razon a calcular. Puede ser entregado sin comillas.
#' @param divisor El nombre del divisor de  la razon a calcular.. Puede ser entregado sin comillas.
#'
#' @return Mismo dataframe con una nueva columna nueva con el resultado de la razón.
#' @export
#'
#' @examples
#' df <- tibble::tibble(
#' rol = 1,
#' ro = 2,
#' ho = 1
#' )
#' resultado <- calcular_razones(
#'   df = df,
#'   razon = roho,
#'   dividendo = ro,
#'   divisor = ho
#' )
calcular_razones <- function(df, razon, dividendo, divisor){
  # Convertir los argumentos en símbolos
  razon_sym <- dplyr::ensym(razon)
  dividendo_sym <- dplyr::ensym(dividendo)
  divisor_sym <- dplyr::ensym(divisor)

  df <-
    df |>
    dplyr::mutate(
      {{ razon_sym }} := dplyr::case_when(
        {{ dividendo_sym }} == 0 & {{ divisor_sym }} == 0 ~ 0,
        {{ divisor_sym }} == 0 ~ 0,
        TRUE ~ {{ dividendo_sym }} / {{ divisor_sym }}
      )
    )
  return(df)
}

# Ejemplo de uso sin comillas
