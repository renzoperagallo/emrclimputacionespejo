#' Calculo número de observaciones y promedio para una variable de interés según un nivel de representatividad dado.
#'
#' @description
#' Calculo número de observaciones y promedio para una variable de interés según un nivel de representatividad dado. Esta función se utiliza para establecer el númer de cadenas observadas o esperadas para un nivel de desagregación y una variable de interés dada. En el contexto del cálculo, al usar sobre t1 se obtienen los valores "esperados", mientras que, al calcular para el mes t se obtienen los valores "observados".
#'
#' @param df Dataframe que incluye los microdatos de un mes
#' @param variable_interes Cadena de texto que indica el nombre de la variable de interés (nombre de la columna)
#' @param agregacion Vector de caracteres con los nombres de las columnas que definen el nivel de desagregación deseado (por ejemplo, c("sexo", "grupo", "tamano", "categoria")).
#' @export
#' @return Un dataframe con el número de observaciones validas para una variable de interés y el promedio de esa variable, para un nivel de representatividad dado.
#'
#' @examples
#' # Preparar la base
#' microdato_t <- calculador_indicadores_basicos(ejemplo_microdato_mes_t_na)
#' cadenas_esperadas_tcsg <-
#'   n_mean_por_agregacion(
#'   df = microdato_t,
#'   variable_interes = "nt",
#'   agregacion = c("tamano", "categoria", "sexo", "grupo")
#'   )
n_mean_por_agregacion <- function(df, variable_interes, agregacion){

  # Convertir el nombre de la variable de interés a símbolo
  var_interes_sym <- rlang::sym(variable_interes)

  # Agrupar y calcular usando el nombre de variable dinámico y nivel de representatividad
  calculo_cadenas <-
    df |>
    dplyr::filter(!!var_interes_sym > 0) |> # Filtrar usando el símbolo de la variable de interés
    dplyr::group_by(across(all_of(agregacion))) |> # Agrupar por nivel de representatividad
    dplyr::summarise(
      n_cadena = dplyr::n(),
      mean_cadena = mean(!!var_interes_sym, na.rm = TRUE), # Calcular el promedio usando el símbolo de la variable de interés
      .groups = "drop"
    )

  return(calculo_cadenas)
}
