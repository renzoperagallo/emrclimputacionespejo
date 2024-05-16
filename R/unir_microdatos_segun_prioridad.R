#' Une dos microdatos priorizando el valor de un df por sobre el otro.
#'
#' @description
#' Esta función une dos dataframes con microdatos para el mismo mes obtenidos a partir de dos métodos de imputación. Posteriormente, prefiere el valor del dataframe prioritario por sobre el valor del dataframe secundario para cada una de las variables de interés. En caso de que la variable sea NA para ambos periodos, se conserva el NA.
#'
#' @param df_prioritario Dataframme con los valores para el periodo t.
#' @param df_secundario  Dataframme con los valores para el periodo t imputado por un segundo método con menos prioridad.
#'
#' @return Un dataframe con las mismas variables, pero priorizando el valor  existente en el df_prioritario. Si ese dataframe tiene un valor na, entonces utilizará el dato  obtenido del df_secundario.
#' @export
#'
unir_microdatos_segun_prioridad <-
  function(df_prioritario,
           df_secundario){

    # Transformar en formato largo ambos df de entrada
    df_prioritario <-
      df_prioritario |>
      tidyr::pivot_longer(
        cols = c("nt", "ro", "ho",  "he",  "og", "re"),
        names_to = "variable",
        values_to = "valor"
      )

    df_secundario <-
      df_secundario |>
      tidyr::pivot_longer(
        cols = c("nt", "ro", "ho",  "he",  "og", "re"),
        names_to = "variable",
        values_to = "valor"
      )
    # Join a ambos df
    df <-
      df_prioritario |>
      dplyr::full_join(
        df_secundario,
        by = c("ano", "mes", "rol", "tamano", "sexo", "grupo", "categoria", "variable"),
        suffix = c("_pri", "_sec")
      )

    # Priorizacion de los valores del df prioritario por sobre el secundario.
    df <-
      df |>
      dplyr::mutate(
        valor = dplyr::case_when(
          !is.na(valor_pri) ~ valor_pri,
          !is.na(valor_sec) ~ valor_sec,
          .default = NA
        )
      ) |>
      dplyr::select(
        -valor_pri,
        -valor_sec
      )

    # Transformar a formato ancho
    df <-
      df |>
      tidyr::pivot_wider(
        names_from = variable,
        values_from = valor
      )

    return(df)
  }
