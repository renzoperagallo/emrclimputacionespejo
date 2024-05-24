#' Aplicar restricciones
#'
#' @description
#' Función que trunca los valores de una base de microdatos de acuerdo a valores máximos y mínimos para cada variable. Los parámetros de truncamiento aplican sobre nt y las razones (roho, hont, ognt, hent y rehe).
#'
#'
#' @param microdato_imputado Dataframe con microdato después de la imputación.
#' @param microdato_original Dataframe con microdato original con valores NA donde debiese imputarse.
#' @param parametros Dataframe con los valores mínimos y máximos para cada variable por año y mes.
#'
#' @return Microdato con los valores de nt, ro, re, og, ho y he truncados para no exceder los máximos y mínimos de las razones. Solo para cadenas que fueron sometidas a imputación.
#' @export
#'
#' @examples
#' \dontrun{
#' # Asumiendo que df_imputado es el microdato imptuado,
#' #df_original es el microdato previo a la imputación y
#' # ejemplo_parametros son los parámetros definidos para el periodo.
#' # df_truncado <-
#'   aplicar_restricciones(
#'   microdato_imputado = df_imputado,
#'   microdato_original = df_original,
#'   parametros = ejemplo_parametros
#' )
#' }
aplicar_restricciones <-
  function(
    microdato_imputado,
    microdato_original,
    parametros
  ){

    #### Seleccionar variables ####
    microdato_imputado <- microdato_imputado |> dplyr::select(rol, ano, mes, categoria, tamano, grupo, sexo, nt, ho, he, ro, re, og)
    microdato_original <- microdato_original |> dplyr::select(rol, ano, mes, categoria, tamano, grupo, sexo, nt, ho, he, ro, re, og)

    #### Calcular los indicadores básicos ####
    microdato_imputado <- calculador_indicadores_basicos(microdato_imputado)
    microdato_original <- calculador_indicadores_basicos(microdato_original)

    #### Transformar microdato a formato largo  ####
    microdato_largo <- transformar_formato_largo(microdato_imputado)
    microdato_original_largo <- transformar_formato_largo(microdato_original)


    # Preparar datos para la imputacion ---------------------------------------

    microdato_largo <-
      microdato_largo |>
      dplyr::left_join(
        microdato_original_largo,
        by =c("rol", "ano", "mes", "categoria", "tamano", "grupo", "sexo", "variable"),
        suffix = c("", "_original")
      ) |>
      dplyr::left_join( # Agregar  los parámetros
        parametros,
        by =c("ano", "mes", "variable")
      )

    # Truncar -----------------------------------------------------------------

    variables_para_truncar <- c("nt", "hont", "ognt", "roho", "hent", "rehe")

    # Truncar las razones
    for (i in variables_para_truncar){
      microdato_largo <- truncar_razon(
        df = microdato_largo,
        var = i
      )
    }

    #### Transformar a formato ancho ####
    microdato_ancho <-
      microdato_largo |>
      dplyr::select(rol, ano, mes, categoria, tamano, grupo, sexo, variable, valor) |>
      tidyr::pivot_wider(
        names_from = variable,
        values_from = valor
      )

    #### Calcular las razones ####

    microdato_ancho <-
      microdato_ancho |>
      dplyr::mutate(
        ho = hont*nt,
        og = ognt*nt,
        ro = roho*ho,
        he = hent*nt,
        re = rehe*he
      )


    # Preparar salida ---------------------------------------------------------

    #### Seleccionar columnas de salida ####
    microdato_ancho <-
      microdato_ancho |>
      dplyr::select(rol, ano, mes, categoria, tamano, grupo, sexo, nt, ho, he, ro, re, og)

    return(microdato_ancho)
  }
