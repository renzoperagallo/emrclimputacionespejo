#' Imputación de Microdatos para la Encuesta de Remuneraciones Mensual
#'
#' Esta función realiza la imputación de microdatos para la encuesta de remuneraciones que se lleva a cabo cada mes, utilizando un método hot deck. La imputación se basa en los datos de hasta tres meses anteriores (T-1, T-2, T-3) para rellenar los valores faltantes en el mes actual (T), asegurando una mayor precisión y representatividad en los datos imputados.
#'
#' @param microdato_t Dataframe con los microdatos del mes T, es decir, el periodo actual que necesita imputación.
#' @param microdato_t1 Dataframe con los microdatos del mes T-1, utilizado como referencia inmediata para la imputación.
#' @param roles_comportamiento_unico Vector opcional con los roles que deben ser considerados como comportamiento único, implicando que serán imputados directamente sin necesidad de buscar vecinos. Esto es especialmente útil para roles específicos con patrones de datos consistentes que no requieren comparación.
#' @param n_cadenas_minimas Número entero que representa el número mínimo de observaciones que deben tener los vecinos para ser considerados válidos en el proceso de imputación. Por defecto, este valor es 4, asegurando que solo se utilicen vecinos con una cantidad mínima de datos para la imputación.
#'
#' @return Un dataframe con los microdatos del mes T imputados, incorporando valores para entradas previamente faltantes basados en los datos históricos y la metodología hot deck. Este dataframe resultante es esencial para análisis subsecuentes, asegurando que la información sobre remuneraciones sea completa y representativa.
#' @export
#'
#' @examples
#' microdato_imputado_mes_t <-
#'   imputar_mes(
#'   microdato_t = ejemplo_microdato_mes_t_na,
#'   microdato_t1 = ejemplo_microdato_mes_t1_na,
#'   roles_comportamiento_unico = ejemplo_roles_comportamiento_unico,
#'   n_cadenas_minimas = 4
#'   )
imputar_mes <- function(microdato_t,
                        microdato_t1,
                        roles_comportamiento_unico = NULL,
                        n_cadenas_minimas = 4){# Aquí debe ir el nombre que corresponde a que la encuesta se realizó satisfactoriamente.

  # Separar los valores de comportamiento único de la bbdd
  # (se crea una lista con una base con los roles de comportamiento unico y otra sin esos roles)

  microdatos_separados_t <-
    microdato_t |>
    excluir_comportamiento_unico(roles_comportamiento_unico)
  microdatos_separados_t1 <-
    microdato_t1 |>
    excluir_comportamiento_unico(roles_comportamiento_unico)

  # Imputar roles de comportamiento único -----------------------------------

  microdato_t_comportamiento_unico <-
    imputar_mes_arrastre(
      microdato_t = microdatos_separados_t[[2]],
      microdato_t1 = microdatos_separados_t1[[2]]
    )


  # Imputar roles sin comportamiento único ----------------------------------

  # Redefinir las variables

  microdato_t <- microdatos_separados_t[[1]]
  microdato_t1 <- microdatos_separados_t1[[1]]

  #### Imputacion Hot deck ####

  ##### Imputar NT #####
  microdato_t <-
    imputar_variables(
      microdato_t = microdato_t,
      microdato_t1 = microdato_t1,
      n_cadenas_minimas = n_cadenas_minimas,
      variables_imputacion = c("nt")
    )

  ##### Imputar HO (a partir de hont) #####
  microdato_t <-
    imputar_variables(
      microdato_t = microdato_t,
      microdato_t1 = microdato_t1,
      n_cadenas_minimas = n_cadenas_minimas,
      variables_imputacion = c("hont")
    )

  ##### Imputar OGNT y ROHO #####
  microdato_t <-
    imputar_variables(
      microdato_t = microdato_t,
      microdato_t1 = microdato_t1,
      n_cadenas_minimas = n_cadenas_minimas,
      variables_imputacion = c("ognt", "roho")
    )

  #### Imputación de HENT ####
  microdato_t <-
    imputar_hent(
      microdato_t = microdato_t,
      microdato_t1 = microdato_t1
    )

  #### Imputacion de REHE ####
  microdato_t <-
    imputar_rehe(
      microdato_t = microdato_t,
      microdato_t1 = microdato_t1
    )


# Arreglos finales --------------------------------------------------------


  #### Unir bases de roles sin comportamiento único con roles de comportamiento único
 microdato_t <-
    microdato_t |>
    dplyr::bind_rows(
      microdato_t_comportamiento_unico
    )

  #### Asegurarse que si NT es 0, todos los demás parámetros son 0 ####
  microdato_t <-
    microdato_t |>
    dplyr::mutate(
      ro = ifelse(nt == 0, 0, ro),
      re = ifelse(nt == 0, 0, re),
      ho = ifelse(nt == 0, 0, ho),
      he = ifelse(nt == 0, 0, he),
      og = ifelse(nt == 0, 0, og)
    )

  #### Recalcular CT y HT ####
  # microdato_t <-
  #   microdato_t |>
  #   dplyr::mutate(
  #     ht = rowSums(dplyr::select(microdato_t, he, ho), na.rm = TRUE),
  #     ct = rowSums(dplyr::select(microdato_t, og, ro, re), na.rm = TRUE)
  #   )

  return(microdato_t)
}
