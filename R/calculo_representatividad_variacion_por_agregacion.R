#' Cálculo del número de observaciones y promedio para una variable de interés según un nivel de representatividad dado.
#'
#' @description
#' Calcula la representatividad y variación para un determinado nivel de representatividad a partir de dos dataframes con el número de observaciones y  promedios para un nivel de representatividad.
#' Además, crea columnas con el número de cadenas observadas y esperadas para el nivel de representatividad dado.
#'
#' @param mes_t Dataframe que incluye los microdatos del mes T.
#' @param mes_t1 Dataframe que incluye los microdatos del Mes T1
#' @param variable_interes Cadena de texto que indica el nombre de la variable de interés (nombre de la columna)
#' @param agregacion Vector de caracteres con los nombres de las columnas que definen el nivel de desagregación deseado (por ejemplo, c("sexo", "grupo", "tamano", "categoria")).
#'
#' @export
#' @return Un dataframe con el número de observaciones validas para una variable de interés y el promedio de esa variable, para un nivel de representatividad dado.
#'
#' @examples
#' # Establecer un nivel de representatividad.
#' tcsg <- c("tamano", "categoria", "sexo", "grupo")
#' # Establecer variable de interes
#' variable_interes = "nt"
#'
#' # Calcular los indicadores básicos para dos microdatos (t y t1)
#' microdato_t <- calculador_indicadores_basicos(ejemplo_microdato_mes_t_na)
#' microdato_t1 <- calculador_indicadores_basicos(ejemplo_microdato_mes_t1_na)
#'
#' # Calcular el número de observaciones y promedios para
#' # el nivel de representatividad dado y una variable "nt".
#' mes_t <-
#'   n_mean_por_agregacion(
#'   df = microdato_t,
#'   variable_interes = variable_interes,
#'   agregacion = tcsg
#'   )
#' mes_t1 <-
#'   n_mean_por_agregacion(
#'   df = microdato_t1,
#'   variable_interes = variable_interes,
#'   agregacion = tcsg
#'   )
#'
#' # Calcular los niveles de repreentatividad
#' representatividad_nt <-
#'   calculo_representatividad_variacion_por_agregacion(
#'   mes_t = mes_t,
#'   mes_t1 = mes_t1,
#'   variable_interes = variable_interes,
#'   agregacion = tcsg
#'   )
#' representatividad_nt
calculo_representatividad_variacion_por_agregacion <- function(mes_t, mes_t1 , variable_interes, agregacion){

  representatividad <-
    mes_t |>
    dplyr::full_join(
      mes_t1,
      by = agregacion,
      suffix = c(".t", ".t1")
    ) |>
    dplyr::mutate(
      representatividad = (n_cadena.t / n_cadena.t1)*100,
      variacion = mean_cadena.t / mean_cadena.t1,
      variable = variable_interes
    ) |>
    dplyr::select(
      dplyr::everything(),
      n_cadenas_observadas = n_cadena.t,
      n_cadenas_esperadas = n_cadena.t1,
      -mean_cadena.t,
      -mean_cadena.t1
    )

  return(representatividad)
}
