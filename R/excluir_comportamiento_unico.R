#' Crar variable comportamiento único
#'
#' @description
#' Pequeña función que extrae los roles de comportamiento único de un dataframe.
#'
#'
#' @param microdato Dataframe con  los microdatos, debe tener la columna "rol".
#' @param roles_comportamiento_unico Vector opcional con los roles que deben ser considerados como comportamiento único, implicando que serán imputados directamente sin necesidad de buscar vecinos. Esto es especialmente útil para roles específicos con patrones de datos consistentes que no requieren comparación.
#' @export
#' @return Una lista de dos dataframes, el primero de ellos trae los roles sin comportamiento único, mientras que la segunda es un dataframe con los roles de comportamiento único.
#'
#' @examples
#' roles_comportamiento_unico <- seq(1,4)
#' microdato <-
#'   data.frame(
#'     rol = seq(1:10),
#'     nt = 1
#'   )
#' df <-
#'   excluir_comportamiento_unico(
#'   microdato = microdato,
#'   roles_comportamiento_unico = roles_comportamiento_unico
#'   )
excluir_comportamiento_unico <- function(microdato, roles_comportamiento_unico = NULL){

  # Para el periodo t.
  if (!is.null(roles_comportamiento_unico)){
    microdato <-
      microdato |>
      dplyr::mutate(comportamiento_unico = ifelse(rol %in% roles_comportamiento_unico, 1, 0))
  } else {
    microdato <-
      microdato |>
      dplyr::mutate(comportamiento_unico = 0)
  }

  # Base con roles sin comportamietno único

  microdato_sin_cu <-
    microdato |>
    dplyr::filter(comportamiento_unico == 0) |>
    dplyr::select(-comportamiento_unico)
  # Base con roles  con comportamiento único
  microdato_con_cu <-
    microdato |>
    dplyr::filter(comportamiento_unico == 1) |>
    dplyr::select(-comportamiento_unico)


  microdato <- list(
    microdato_sin_cu = microdato_sin_cu,
    microdato_con_cu = microdato_con_cu
  )

  return(microdato)
}
