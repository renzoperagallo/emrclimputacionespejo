#' Truncar razon
#'
#' @description
#' Pequeña función que trunca un valor según los valores definidos en las columnas min y max, siempre y cuando el valor haya sido sometido a una imptuacion (es decir, el valor original sea NA).
#'
#' @param df Microdato en formato largo con las columnas: ano, mes, rol, grupo, sexo, categoria, tamano, variable, valor, valor original, max y min
#' @param var String con el valor a truncar, por ejemplo "nt", "hont", etc.  Usar esta función para imputar nt y las razones básicas.
#'
#' @return Microdato con el valor truncado para la variable especificada.
#' @export
#'
#' @examples
#' \dontrun{
#' # Asumiendo que microdato_largo es un data frame con microdatos en
#' # formato largo con las columnas requeridas.
#' microdato_largo_imputado <-
#'   truncar_razon(
#'   df = microdato_largo,
#'   var = "nt"
#' )
#' }
truncar_razon <- function(df, var){
  df <-
    df |>
    dplyr::mutate(
      valor = dplyr::case_when(
        is.na(valor_original) & variable == var & valor > max ~ max,
        is.na(valor_original) & variable == var & valor < min ~ min,
        .default = valor
      )
    )

  return(df)
}

