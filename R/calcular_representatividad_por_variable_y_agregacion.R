#' Calcular representatividad por variable y agregación
#'
#' @description
#' Esta función calcula el alfa de representatividad para una variable y agregación dados. También entrega la variación y el  n° de cadenas esperadas y observadas para dicha agregación.
#'
#'
#' @param mes_t Microdato del mes t con las variables calculadas de roho, hont, ront, hent, ognt y rehe  a partir de la función 'calculador_indicadores_basicos()'.
#' @param mes_t1 Microdato del mes t1 con las variables calculadas de roho, hont, ront, hent, ognt y rehe  a partir de la función 'calculador_indicadores_basicos()'.
#' @param variable_interes String con el nombre de la variable de interés para imputar.
#' @param agregacion Vector con la agregación requerida. Por ejemplo c("tamano", "categoria", "sexo", "grupo").
#'
#' @return Dataframe con  las columnas especificas al nivel de agregación, el nombre de la variable y los indicadores cálculados para cada combinatoria.
#' @export
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
#' # Calcular los niveles de repreentatividad
#' representatividad_nt <-
#'   calcular_representatividad_por_variable_y_agregacion(
#'   mes_t = microdato_t,
#'   mes_t1 = microdato_t1,
#'   variable_interes = variable_interes,
#'   agregacion = tcsg
#'   )
#' representatividad_nt
calcular_representatividad_por_variable_y_agregacion <-
  function(
    mes_t,
    mes_t1 ,
    variable_interes,
    agregacion
  ){
    #### calcular cadenas esperadas y observadas, como sus promedios para una variable ####

    # Calcular cadenas para el mes t
    mes_t <-
      n_mean_por_agregacion(
        df = mes_t,
        variable_interes = variable_interes,
        agregacion = agregacion
        )

    # Calcular cadenas para el mes t1
    mes_t1 <-
     n_mean_por_agregacion(
       df = mes_t1,
       variable_interes = variable_interes,
       agregacion = agregacion
       )

    #### Calcular representatividad y variacion según agregacion ####
    representatividad <-
     calculo_representatividad_variacion_por_agregacion(
       mes_t = mes_t,
       mes_t1 = mes_t1,
       variable_interes = variable_interes,
       agregacion = agregacion
       )


    return(representatividad)
  }
