#' Imputación de Microdatos para la Encuesta de Remuneraciones Mensual
#'
#' @description
#' Esta función imputa una o varias variables de la base de microdatos por medio de hot-deck. Puede usarse para imputar NT, RO, HO y OG.
#' NOTA: Si bien se pueden imputar varias variables a la vez, se recomienda hacerlo una por una para que los resultados de la imputación de NT sirvan para imputar HO y así sucesivamente.
#'
#' @param microdato_t Dataframe con los microdatos del mes T.
#' @param microdato_t1 Dataframe con los microdatos del mes T-1.
#' @param variables_imputacion Es un vector con los nombres de las variables a imputar. Los valores admitidos son "nt", "roho", "hont" y "ognt".Los valores deben ser entregados como vector, por ejemplo: c("nt") o c("roho", "ognt"). Es importante tener en consideración que si se imputa más de una variable con esta función, los valores se imputaran de forma paralela. Es decir, por ejemplo si se considera c("nt", "hont"), los valores imputados de nt no servirán para estimar e imputar hont. En dicho caso, deben imputarse de forma sucesiva.
#' @param n_cadenas_minimas Número entero que representa el número mínimo de observaciones que deben tener los vecinos para ser considerados válidos en el proceso de imputación. Por defecto, este valor es 4, asegurando que solo se utilicen vecinos con una cantidad mínima de datos para la imputación.
#'
#' @return Un dataframe con los microdatos del mes T imputados, incorporando valores para entradas previamente faltantes basados en los datos históricos y la metodología hot deck. Este dataframe resultante es esencial para análisis subsecuentes, asegurando que la información sobre remuneraciones sea completa y representativa.
#' @export
#'
#' @examples
#' microdato_imputado_mes_t <- imputar_variables(microdato_t = ejemplo_microdato_mes_t_na,
#'                                        microdato_t1 = ejemplo_microdato_mes_t1_na,
#'                                        n_cadenas_minimas = 4,
#'                                        variables_imputacion = c("nt"))
imputar_variables <- function(
    microdato_t,
    microdato_t1,
    n_cadenas_minimas = 4,
    variables_imputacion
){
  # Seleccionar las columnas correspondientes -------------------------------

  microdato_t <- microdato_t |> dplyr::select(rol, ano, mes, categoria, tamano, grupo, sexo, nt, ho, he, ro, re, og)
  microdato_t1 <- microdato_t1 |> dplyr::select(rol, ano, mes, categoria, tamano, grupo, sexo, nt, ho, he, ro, re, og)

  # Calcular los indicadores básicos ----------------------------------------

  microdato_t <- calculador_indicadores_basicos(microdato_t)
  microdato_t1 <- calculador_indicadores_basicos(microdato_t1)

  # Imputacion hot deck -----------------------------------------------------

  #### Establecer Agregaciones ####
  tcsg <- c("sexo", "grupo", "tamano", "categoria")
  tcg <- c("grupo", "tamano", "categoria")
  cg <- c("categoria", "grupo")

  #### Calcular alfa de representatividad y variaciones por nivel de representatividad ####
  representatividad_variables_tcsg <-
    calcular_representatividad_todas_variables(
      mes_t = microdato_t,
      mes_t1 = microdato_t1,
      agregacion = tcsg,
      vector_variables_interes =  variables_imputacion
    )
  representatividad_variables_tcg <-
    calcular_representatividad_todas_variables(
      mes_t = microdato_t,
      mes_t1 = microdato_t1,
      agregacion = tcg,
      vector_variables_interes =  variables_imputacion
    )
  representatividad_variables_cg <-
    calcular_representatividad_todas_variables(
      mes_t = microdato_t,
      mes_t1 = microdato_t1,
      agregacion = cg,
      vector_variables_interes =  variables_imputacion
    )

  #### Transformar microdato a formato largo  ####

  microdato_t_largo <- transformar_formato_largo(microdato_t)
  microdato_t1_largo <- transformar_formato_largo(microdato_t1)

  #### Imputación Variables ####

  # Agregación tcsg
  microdato_imputado_mes_t <-
    imputar_parametros_segun_nivel_representatividad(
      microdato_largo_mes_t = microdato_t_largo,
      microdato_largo_mes_t1 = microdato_t1_largo,
      df_nivel_representatividad = representatividad_variables_tcsg,
      agregacion = tcsg,
      n_cadenas_minimas = n_cadenas_minimas,
      variables_imputacion =  variables_imputacion
    )

  # Agregación tcg
  microdato_imputado_mes_t <-
    imputar_parametros_segun_nivel_representatividad(
      microdato_largo_mes_t = microdato_imputado_mes_t,
      microdato_largo_mes_t1 = microdato_t1_largo,
      df_nivel_representatividad = representatividad_variables_tcg,
      agregacion = tcg,
      n_cadenas_minimas = n_cadenas_minimas,
      variables_imputacion =  variables_imputacion
    )

  # Agregación cg
  microdato_imputado_mes_t <-
    imputar_parametros_segun_nivel_representatividad(
      microdato_largo_mes_t = microdato_imputado_mes_t,
      microdato_largo_mes_t1 = microdato_t1_largo,
      df_nivel_representatividad = representatividad_variables_cg,
      agregacion = cg,
      n_cadenas_minimas = n_cadenas_minimas,
      variables_imputacion =  variables_imputacion
    )

  # Imputar por arrastre
  microdato_imputado_mes_t <-
    imputar_parametros_arrastre(
      microdato_largo_mes_t = microdato_imputado_mes_t,
      microdato_largo_mes_t1 = microdato_t1_largo,
      variables_imputacion = variables_imputacion
    )

      # Cálculo parámetros derivados (HO, HE, RE, HT,  RO y OG)-----------------

      #### Transformar a formato ancho ####
      base_calculo <-
        microdato_imputado_mes_t |>
        tidyr::pivot_wider(
          names_from = variable,
          values_from = valor
        )

      #### Calcular parámetros derivados ####

      if ("hont" %in% variables_imputacion){
        base_calculo <-
          base_calculo |>
          dplyr::mutate(
            ho = as.double(hont * nt)
          )
      }
      if ("roho" %in% variables_imputacion){
        base_calculo <-
          base_calculo |>
          dplyr::mutate(
            ro = as.double(roho * ho)
          )
      }
      if ("ognt" %in% variables_imputacion){
        base_calculo <-
          base_calculo |>
          dplyr::mutate(
            og = as.double(ognt * nt)
          )
      }

      # Return ------------------------------------------------------------------

      # Seleccionar columnas requeridas
      base_calculo <-
        base_calculo |>
        dplyr::select(ano, mes, rol, tamano, categoria, sexo, grupo, nt, ro, re, ho, he, og)

      return(base_calculo)
    }
