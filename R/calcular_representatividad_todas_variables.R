#' Calcular representatividad para una lista de variables según el nivel de agregación
#'
#' @description
#' Esta función calcula el alfa de representatividad para una lista de variables y un nivel de agregación dado. También entrega la variación y el  n° de cadenas esperadas y observadas para dicha agregación.
#'
#'
#' @param mes_t Microdato del mes t con las variables calculadas de roho, hont, ront, hent, ognt y rehe  a partir de la función 'calculador_indicadores_basicos()'.
#' @param mes_t1 Microdato del mes t1 con las variables calculadas de roho, hont, ront, hent, ognt y rehe  a partir de la función 'calculador_indicadores_basicos()'.
#' @param vector_variables_interes Vector con el enombre de las variables de interés para imputar. Por defecto, están las variables establecidas en la metodología de imputación: c("nt", "roho", "hont", "ognt").
#' @param agregacion Vector con la agregación requerida. Por ejemplo c("tamano", "categoria", "sexo", "grupo").
#'
#' @return Dataframe con  las columnas especificas al nivel de agregación, el nombre de las variables en la columa "variable" y los indicadores cálculados para cada combinatoria.
#' @export
#'
#' @examples
#' # Establecer las variables de interes y el nivel de agregacion
#'   variables <- c("nt", "roho", "hont", "ognt")
#'   tcsg <- c("tamano", "categoria", "sexo", "grupo")
#'
#' # Calcular los indicadores básicos para dos microdatos (t y t1)
#'   microdato_t <- calculador_indicadores_basicos(ejemplo_microdato_mes_t_na)
#'   microdato_t1 <- calculador_indicadores_basicos(ejemplo_microdato_mes_t1_na)
#'
#'# Aplicar la funcion
#' representatividad_variables <-
#'  calcular_representatividad_todas_variables(
#'   mes_t = microdato_t,
#'   mes_t1 = microdato_t1,
#'   vector_variables_interes = variables,
#'   agregacion = tcsg
#'  )
calcular_representatividad_todas_variables <-
  function(
    mes_t,
    mes_t1,
    agregacion,
    vector_variables_interes = c("nt", "roho", "hont", "ognt")
  ){
    df <-
      purrr::map_dfr(
        vector_variables_interes,
        ~calcular_representatividad_por_variable_y_agregacion(
          mes_t = mes_t,
          mes_t1 = mes_t1,
          variable_interes = .x,
          agregacion = agregacion
        )
      )
    return(df)
  }
