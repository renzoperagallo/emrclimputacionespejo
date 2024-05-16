#' Imputación de la Variable "rehe" basada en Datos del Periodo Anterior
#'
#' @description
#' Esta función imputa valores faltantes para la variable rehe (remuneraciones extraordinarias por horas extraordinarias) en un conjunto de datos.
#'
#' @param microdato_t Dataframe con los microdatos del mes t.
#' @param microdato_t1 Dataframe con los microdatos del mes t1.
#'
#' @return Devuelve el DataFrame original con la columna re imputada según los criterios establecidos.
#'
#' @export
#'
#' @examples
#' microdato_imputado_mes_t <-
#'   imputar_rehe(
#'     microdato_t = ejemplo_microdato_mes_t_na,
#'     microdato_t1 = ejemplo_microdato_mes_t1
#'   )
#' microdato_imputado_mes_t
imputar_rehe <-
  function(
    microdato_t,
    microdato_t1
  ){
    # Preparar datos de entrada-----------------------------------------------------

    #### Seleccionar variables ####

    microdato_t <- microdato_t |> dplyr::select(rol, ano, mes, categoria, tamano, grupo, sexo, nt, ho, he, ro, re, og)
    microdato_t1 <- microdato_t1 |> dplyr::select(rol, ano, mes, categoria, tamano, grupo, sexo, nt, ho, he, ro, re, og)

    #### Calcular los indicadores básicos ####

    microdato_t <- calculador_indicadores_basicos(microdato_t)
    microdato_t1 <- calculador_indicadores_basicos(microdato_t1)

    #### Transformar microdato a formato largo  ####
    microdato_t_largo <- transformar_formato_largo(microdato_t)
    microdato_t1_largo <- transformar_formato_largo(microdato_t1)


    # Preparar datos para la imputacion ---------------------------------------

    # Contar filas de input (esto es para hacer una comprobación al final del código)
    filas_df_input <- nrow(microdato_t_largo)

    #### Preparacion de los datos ####

    # Establece los nombres del df sobre el que hacer el join del mes y t1.
    nombres <- names(microdato_t_largo)[!names(microdato_t_largo) %in% c("valor",  "ano", "mes")]

    # Crea columna nt
    columna_nt <-
      microdato_t_largo |>
      dplyr::filter(variable == "nt") |>
      dplyr::select(rol, ano, mes, grupo, sexo, nt=valor)

    # Hacer join con la base principal para añadir la columna nt
    microdato_t_largo <-
      microdato_t_largo |>
      dplyr::left_join(
        columna_nt,
        by =c("rol", "ano", "mes", "grupo", "sexo")
      )

    # Crear valor t y t1
    microdato_largo <-
      microdato_t_largo |>
      dplyr::left_join(
        microdato_t1_largo,
        by = nombres,
        suffix = c("_t", "_t1")
      )


    # Imputar -----------------------------------------------------------------

    microdato_imputado <-
      microdato_largo |>
      dplyr::mutate(
        valor_t = dplyr::case_when(
          variable == "rehe" & nt == 0 ~ 0,
          variable == "rehe" & is.na(valor_t) ~ valor_t1,
          .default = valor_t
        )
      )

    # Restablecer nombres columnas de df entrada
    microdato_imputado <-
      microdato_imputado |>
      dplyr::select(
        ano = ano_t, mes = mes_t, rol, tamano, categoria, sexo, grupo, variable, valor = valor_t
      )

    # Preparar salida ---------------------------------------------------------

    # Asegruarse que no hay inconsistencias
    filas_df_output <- nrow(microdato_imputado)
    if(filas_df_input !=filas_df_output){stop("Advertencia, se observa una inconsistencia en las cadenas del dataframe que produce un problema al contar los trabajadores por rol. Posiblemente hay mas de una seccion o tamano para un rol.")}

    #### Transformar a formato ancho ####
    microdato_imputado <-
      microdato_imputado |>
      tidyr::pivot_wider(
        names_from = variable,
        values_from = valor
      )

    #### Calcular RE ####
    microdato_imputado <-
      microdato_imputado |>
      dplyr::mutate(
        re = ifelse(
          is.na(re) & !is.na(rehe) & !is.na(he),
          rehe*he,
          re)
      )

    #### Seleccionar columnas de salida ####
    microdato_imputado <- microdato_imputado |> dplyr::select(rol, ano, mes, categoria, tamano, grupo, sexo, nt, ho, he, ro, re, og)

    return(microdato_imputado)
  }
