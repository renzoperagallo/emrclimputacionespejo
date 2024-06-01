#' Imputar parámetros según nivel de representatividad
#'
#' @description
#' Imputa un dataframe con microdatos en formato largo a
#'
#'
#' @param microdato_largo_mes_t Df con microdato en formato largo para el mes t.
#' @param microdato_largo_mes_t1 Df con microdato en formato largo para el mes t1.
#' @param df_nivel_representatividad Df con los niveles de representatividad y las variaciones dados para un nivel de agregación
#' @param agregacion Vector con el nombre de las columnas que definen el nivel de agregacion, por ejemplo: c("tamano", "categoria", "sexo", "grupo").
#' @param variables_imputacion Vector con el nombre de las variables a  imputar, mantener valor por defecto: c("nt", "roho", "hont", "ognt").
#' @param n_cadenas_minimas Número de cadenas mínimas aceptadas para dar una cadena como corta como valida.
#'
#' @return El microdato del mes t  con valores imptuados para el nivel de agregación en formato largo.
#' @export
#'
#' @examples
#'
#' # Asumiendo que 'microdato_t' es un microdato para el mes t y
#' # 'microdato_t1' es un microdato para el mes t1.
#'
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
#'
imputar_parametros_segun_nivel_representatividad <-
  function(
    microdato_largo_mes_t,
    microdato_largo_mes_t1,
    df_nivel_representatividad,
    agregacion,
    variables_imputacion =  c("nt", "roho", "hont", "ognt"),
    n_cadenas_minimas = 4
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

    # Hacer un join del microdato con el nivel de representatividad
    microdato_largo_con_representatividad <-
      microdato_largo |>
      dplyr::left_join(
        df_nivel_representatividad,
        by = c(agregacion, "variable")
      )

    # Imputar
    microdato_imputado <-
      microdato_largo_con_representatividad |>
      dplyr::mutate(
        valor_t = dplyr::case_when(
          variable %in% variables_imputacion &
            is.na(valor_t) &
              !is.na(valor_t1) &
                n_cadenas_observadas >= n_cadenas_minimas &
                  n_cadenas_esperadas >= n_cadenas_minimas &
                    representatividad >= 50 ~ valor_t1*variacion,
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
