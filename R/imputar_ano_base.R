#' Imputación de Microdatos para el Año Base con Cambio de Año Base en la Encuesta de Remuneraciones
#'
#' Realiza la imputación de los microdatos de la Encuesta de Remuneraciones para un año completo, utilizando un enfoque iterativo tanto forward (hacia adelante) como backward (hacia atrás), y luego combinando los resultados para mejorar la coherencia y calidad de los datos imputados. Este proceso es particularmente relevante para el cambio de año base 2023, asegurando que los datos anuales sean completos y representativos.
#'
#' @description
#' El proceso de imputación se lleva a cabo en varias etapas:
#'
#' 1. Iteración Forward: Desde febrero hasta diciembre, utilizando los datos del mes anterior como referencia para cada mes.
#'
#' 2. Iteración Backward: Desde noviembre hasta enero, también utilizando los datos del mes anterior como referencia.
#'
#' 3. Combinación de Resultados: Los valores imputados en ambas iteraciones se combinan, priorizando los de la iteración forward.
#'
#' 4. Iteración Forward con Datos Imputados: Se repite el proceso de febrero a diciembre, pero utilizando los datos ya imputados como referencia.
#'
#' 5. Iteración Backward con Datos Imputados: Similarmente, se repite desde noviembre hasta enero, utilizando los datos imputados como referencia.
#'
#' 6. Iteración forward con imptuacion por arrastre: Se imputa por arrastre desde febrero hasta diciembre en sentido forward. Se va utilizando el nuevo mes imptuado de forma sucesiva para imputar el nuevo mes.
#'
#' 7. Iteración backward con imputación por arrastre: Se imputa por arrastre desde noviembre hasta enero en sentido backward. Se va utilizando el nuevo mes imptuado de forma sucesiva para imputar el nuevo mes.
#'
#'
#' Este enfoque iterativo asegura una mejor adaptación de los datos imputados a las tendencias y variaciones dentro del año, considerando tanto la información más reciente como la precedente para ajustar los valores faltantes.
#'
#' @param df DataFrame que contiene los microdatos para todos los meses del año base, con una columna "mes" indicando el mes correspondiente a cada observación.
#' @param roles_comportamiento_unico Vector con los roles de comportamiento único.
#' @param n_cadenas_minimas Número de cadenas mínimas aceptadas para dar una cadena como corta como valida.
#'
#' @return Un DataFrame completo con los datos imputados para todos los meses del año base. Este resultado final representa un conjunto de datos coherente y listo para análisis, con valores faltantes adecuadamente reemplazados siguiendo la metodología descrita.
#'
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Asumiendo que `df_anual` es tu DataFrame con los datos del año base:
#' df_imputado <- imputar_ano_base(df_anual)
#' }
imputar_ano_base <-
  function(df,
           roles_comportamiento_unico = NULL,
           n_cadenas_minimas = 4){

    # Separa el año en meses
    df_separados <- split(df, df$mes)

    # Primera iteracion (forward) ---------------------------------------------

    # Imputacion forward sin valores imputados
    primera_iteracion_forward <- list()
    for (i in seq(2, 12)){
      mes_imputado <-
        imputar_mes(
          microdato_t = df_separados[[i]],
          microdato_t1 = df_separados[[i-1]],
          roles_comportamiento_unico = roles_comportamiento_unico,
          n_cadenas_minimas = n_cadenas_minimas
          )
      primera_iteracion_forward <-
        append(
          primera_iteracion_forward,
          list(mes_imputado)
        )
    }

    gc()
    # Segunda iteracion (backward) --------------------------------------------

    # Imputación backward sin valores imptuados
    segunda_iteracion_backward <- list()
    for (i in seq(11, 1)){
      mes_imputado <-
        imputar_mes(
          microdato_t = df_separados[[i]],
          microdato_t1 = df_separados[[i+1]],
          roles_comportamiento_unico = roles_comportamiento_unico,
          n_cadenas_minimas = n_cadenas_minimas
          )
      segunda_iteracion_backward <- append(segunda_iteracion_backward, list(mes_imputado))
    }

    gc()

    # Integracion valores dos primeras iteraciones ----------------------------

    # Crea lista para recibir los meses
    valores_integrados_iteracion_1y2 <- list()
    # Enero
    valores_integrados_iteracion_1y2 <- append(valores_integrados_iteracion_1y2, segunda_iteracion_backward[11])
    # Febrero a noviembre
    for (i in seq(1,10)){
      mes_unido <- unir_microdatos_segun_prioridad(
        df_prioritario = primera_iteracion_forward[[i]],
        df_secundario = segunda_iteracion_backward[[11-i]]
      )
      valores_integrados_iteracion_1y2 <- append(valores_integrados_iteracion_1y2, list(mes_unido))
    }
    # Diciembre
    valores_integrados_iteracion_1y2 <- append(valores_integrados_iteracion_1y2, primera_iteracion_forward[11])

    # Tercera iteracion (forward con valores imputados) -----------------------

    # Imputación forward con valores imputados
    for (i in seq(2, 12)){
      mes_imputado <-
        imputar_mes(
          microdato_t = valores_integrados_iteracion_1y2[[i]],
          microdato_t1 = valores_integrados_iteracion_1y2[[i-1]],
          roles_comportamiento_unico = roles_comportamiento_unico,
          n_cadenas_minimas = n_cadenas_minimas
          )
      valores_integrados_iteracion_1y2[[i]] <- mes_imputado
    }

    gc()

    # Cuarta iteracion (backward con valores imputados) ----------------------

    # Imptuación backward con valores imputados
    for (i in seq(11, 1)){
      mes_imputado <-
        imputar_mes(
          microdato_t = valores_integrados_iteracion_1y2[[i]],
          microdato_t1 = valores_integrados_iteracion_1y2[[i+1]],
          roles_comportamiento_unico = roles_comportamiento_unico,
          n_cadenas_minimas = n_cadenas_minimas
        )
      valores_integrados_iteracion_1y2[[i]] <- mes_imputado
    }

    gc()

    # Quinta iteracion(arrastre forward) --------------------------------------

    for (i in seq(2,12) ) {
      mes_imputado <-
        imputar_mes_arrastre(
          microdato_t = valores_integrados_iteracion_1y2[[i]],
          microdato_t1 = valores_integrados_iteracion_1y2[[i-1]]
        )
      valores_integrados_iteracion_1y2[[i]] <- mes_imputado
    }

    gc()

    # Sexta iteracion (arrastre backward --------------------------------------

    for (i in seq(11,1) ) {
      mes_imputado <-
        imputar_mes_arrastre(
          microdato_t = valores_integrados_iteracion_1y2[[i]],
          microdato_t1 = valores_integrados_iteracion_1y2[[i+1]]
        )
      valores_integrados_iteracion_1y2[[i]] <- mes_imputado
    }

    # Combinar los dataframes para generar output -----------------------------

    # Combinar los dataframes  para los distintos meses en un dataframe
    df_imputado <- purrr::reduce(valores_integrados_iteracion_1y2, dplyr::bind_rows)

    # Return de la base
    return(df_imputado)
  }
