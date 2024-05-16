#' Transformar a formato largo
#'
#' @description
#' Transforma un microdato a formato largo.
#'
#' @export
#' @param df Microdato con indicadores básicos calculados.
#'
#' @return Microdato en formato largo con las columnas variables y valor.
#'
#' @examples
#' df <- calculador_indicadores_basicos(ejemplo_microdato_mes_t)
#' df <- transformar_formato_largo(df)
#' df
transformar_formato_largo <- function(df){

  columnas_a_transformar <- c("nt", "ro", "ho", "he",  "og", "re", "roho", "hont", "ront", "ognt", "hent", "rehe")

  if(!all(columnas_a_transformar %in% names(df)) ) stop("No se entregaron todas las columnas necesarias. Recordar calcular los indicadores basicos antes de realizar la transformacion.")

  df <-
    df |>
    tidyr::pivot_longer(
      cols = tidyr::all_of(columnas_a_transformar),
      names_to = "variable",
      values_to = "valor"
    )

  return(df)
}
