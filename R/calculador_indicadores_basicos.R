#' Cálculo de Indicadores Básicos en Microdatos
#'
#' @description
#' Esta función calcula indicadores derivados basados en variables específicas de un dataframe de microdatos. Los indicadores calculados son:
#' - `roho`: Ratio de `ro` sobre `ho`, que indica la relación entre los montos reportados y las horas oficiales trabajadas.
#' - `hont`: Ratio de `ho` sobre `nt`, que muestra la proporción de horas oficiales trabajadas por número total de transacciones o actividades.
#' - `ront`: Ratio de `ro` sobre `nt`, que refleja la relación entre los montos reportados y el número total de transacciones o actividades.
#' - `hent`: Ratio de `he` sobre `nt`, que indica la proporción de horas extra trabajadas por número total de transacciones o actividades.
#' - `ognt`: Ratio de `og` sobre `nt`, que muestra la relación entre otros gastos y el número total de transacciones o actividades.
#' - `rehe`: Ratio de `re` sobre `he`, que refleja la relación entre los montos reportados de horas extra y las horas extra trabajadas.
#'
#' Estos indicadores son fundamentales para comprender la eficiencia, productividad y otros aspectos relacionados con el trabajo y los costos.
#'
#' @param df Dataframe que contiene las variables `ro`, `ho`, `nt`, `re`, `he`, y `og` sobre las cuales se calcularán los indicadores.
#' @export
#' @return Devuelve el mismo dataframe ingresado como parámetro, pero con las columnas adicionales de los indicadores calculados.
#'
#' @examples
#' ejemplo_microdato_mes_t <- data.frame(
#'   ro = c(100, 200, NA),
#'   ho = c(50, 0, 100),
#'   nt = c(2, 3, 1),
#'   re = c(30, 0, NA),
#'   he = c(10, 0, 20),
#'   og = c(200, 300, 100)
#' )
#' df_con_indicadores <- calculador_indicadores_basicos(ejemplo_microdato_mes_t)
#' print(df_con_indicadores)
calculador_indicadores_basicos <- function(df){
  df <-
    df |>
    dplyr::mutate(
      roho = dplyr::case_when(
        ho == 0 & ro == 0 ~ 0,
        ro > 0 & ho > 0 ~ ro/ho,
        is.na(ro) | is.na(ho) ~ NA,
        .default = NA
      ),
      hont = dplyr::case_when(
        ho == 0 & nt == 0 ~ 0,
        ho > 0 & nt > 0 ~ ho/nt,
        is.na(ho) | is.na(nt) ~ NA,
        .default = NA)
      ,
      ront = dplyr::case_when(
        ro == 0 & nt == 0 ~ 0,
        ro > 0 & nt > 0 ~ ro/nt,
        is.na(ro) | is.na(nt) ~ NA,
        .default = NA
      ),
      hent = dplyr::case_when(
        he == 0 & nt == 0 ~ 0,
        he == 0 & nt > 0 ~ 0,
        he > 0 & nt > 0 ~ he/nt,
        is.na(he) | is.na(nt) ~ NA,
        .default = NA
      ),
      ognt = dplyr::case_when(
        og == 0 & nt == 0 ~ 0,
        og == 0 & nt > 0 ~ 0,
        og > 0 & nt > 0 ~ og/nt,
        is.na(og) | is.na(nt) ~ NA,
        .default = NA
      ),
      rehe = dplyr::case_when(
        re == 0 & he == 0 ~ 0,
        re > 0 & he > 0 ~ re/he,
        is.na(re) | is.na(he) ~ NA,
        .default = NA
      )
    )
  return(df)
}
