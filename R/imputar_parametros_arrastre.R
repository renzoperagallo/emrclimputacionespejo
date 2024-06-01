#' Imputar parámetros por arrastre
#'
#' @description
#' Imputa un dataframe con microdatos en formato largo por arrastre.
#'
#' @param microdato_largo_mes_t Df con microdato en formato largo para el mes t.
#' @param microdato_largo_mes_t1 Df con microdato en formato largo para el mes t1.
#' @param variables_imputacion Vector con el nombre de las variables a  imputar, mantener valor por defecto: c("nt", "roho", "hont", "ognt").
#'
#' @return El microdato del mes t  con valores imputados por arrastre.
#' @export
#'
imputar_parametros_arrastre <-
  function(
    microdato_largo_mes_t,
    microdato_largo_mes_t1,
    variables_imputacion =  c("nt", "roho", "hont", "ognt")
    ){
    # Establece los nombres del df sobre el que hacer el join del mes y t1.
    nombres <- names(microdato_largo_mes_t)[!names(microdato_largo_mes_t) %in% c("valor", "comportamiento_unico", "ano", "mes")]

    # Crear valor t y t1
    microdato_largo <-
      microdato_largo_mes_t |>
      dplyr::left_join(
        microdato_largo_mes_t1,
        by = nombres,
        suffix = c("_t", "_t1")
      )

    # Imputar
    microdato_imputado <-
      microdato_largo |>
      dplyr::mutate(
        valor_t = dplyr::case_when(
          variable %in% variables_imputacion &
            is.na(valor_t) &
            !is.na(valor_t1) ~ valor_t1,
          .default = valor_t
        )
      )

    # Restablecer nombres columnas de df entrada
    microdato_imputado <-
      microdato_imputado |>
      dplyr::select(
        ano = ano_t, mes = mes_t, rol, tamano, categoria, sexo, grupo, variable, valor = valor_t
      )

    return(microdato_imputado)
  }
