#' Imputación de mes t  por arrastre
#'
#' @description
#' Esta función imputa el mes t utilizando el mes t1 por medio de arrastre. Es decir, imputa NT, ROHO, HONT, OGNT, HENT y REHE con los valores de t1, posteriormente calcula  RO, RE, HO, HE OG, HT y CT a partir de los ratios arrastrados.  Si la cadena tiene NT = 0 (ya sea el valor original o el imputado), todos los demás valores se imputarán por 0.
#'
#' @param microdato_t Dataframe con microdatos para el mes T.
#' @param microdato_t1 Dataframe con microdatos para el mes T1.
#'
#' @return Dataframe del mes T imputado por arrastre con el mes T1.
#' @export
#'
#' @examples
#' microdato_imputado_mes_t <- imputar_mes_arrastre(microdato_t = ejemplo_microdato_mes_t_na,
#'                                                  microdato_t1 = ejemplo_microdato_mes_t1_na)
imputar_mes_arrastre <-
  function(
    microdato_t,
    microdato_t1
  ){
    # Seleccionar las columnas correspondientes -------------------------------

    microdato_t <- microdato_t |> dplyr::select(rol, ano, mes, categoria, tamano, grupo, sexo, nt, ho, he, ro, re, og)
    microdato_t1 <- microdato_t1 |> dplyr::select(rol, ano, mes, categoria, tamano, grupo, sexo, nt, ho, he, ro, re, og)

    # Calcular los indicadores básicos ----------------------------------------

    microdato_t <- calculador_indicadores_basicos(microdato_t)
    microdato_t1 <- calculador_indicadores_basicos(microdato_t1)

    # Añadir mes t1 -----------------------------------------------------------

    base_calculo <-
      microdato_t |>
      dplyr::left_join(
        microdato_t1,
        by = c("rol", "grupo", "sexo", "categoria", "tamano"),
        keep = FALSE,
        suffix = c("", "_t1")
      )

    # Imputar valores principales ---------------------------------------------

    base_calculo <-
      base_calculo |>
      dplyr::mutate(
        nt = dplyr::case_when(
          is.na(nt) & !is.na(nt_t1) ~ nt_t1,
          TRUE ~ nt
        ),
        roho = dplyr::case_when(
          is.na(roho) & nt == 0 ~ 0,
          is.na(roho) & !is.na(roho_t1)  ~ roho_t1,
          TRUE ~ roho
        ),
        hont = dplyr::case_when(
          is.na(hont) & nt == 0 ~ 0,
          is.na(hont) & !is.na(hont_t1)  ~ hont_t1,
          TRUE ~ hont
        ),
        ognt = dplyr::case_when(
          is.na(ognt) & nt == 0 ~ 0,
          is.na(ognt) & !is.na(ognt_t1)  ~ ognt_t1,
          TRUE ~ ognt
        ),
        hent = dplyr::case_when(
          is.na(hent) & nt == 0 ~ 0,
          is.na(hent) & !is.na(hent_t1)  ~ hent_t1,
          TRUE ~ hent
        ),
        rehe = dplyr::case_when(
          is.na(rehe) & nt == 0 ~ 0,
          is.na(rehe) & !is.na(rehe_t1)  ~ rehe_t1,
          TRUE ~ rehe
        )
      )

    # Cálculo parámetros derivados (HO, HE, RE, HT,  RO y OG)-----------------

    base_calculo <-
      base_calculo |>
      dplyr::mutate(
        ho = ifelse(is.na(ho),hont * nt, ho),
        he = ifelse(is.na(he),hent * nt, he),
        ro = ifelse(is.na(ro),roho * ho, ro),
        re = ifelse(is.na(re),rehe * he, re),
        og = ifelse(is.na(og),ognt * nt, og)
      )

    # base_calculo <-
    #   base_calculo |>
    #   dplyr::mutate(
    #     ht = rowSums(dplyr::select(base_calculo, he, ho), na.rm = TRUE),
    #     ct = rowSums(dplyr::select(base_calculo, og, ro, re), na.rm = TRUE)
    #   )

    # Asegurarse que si NT es 0, todos los demás parámetros son 0 -------------

    base_calculo <-
      base_calculo |>
      dplyr::mutate(
        ro = ifelse(nt > 0, ro, 0),
        re = ifelse(nt > 0, re, 0),
        ho = ifelse(nt > 0, ho, 0),
        he = ifelse(nt > 0, he, 0),
        og = ifelse(nt > 0, og, 0)
      )

    # Return ------------------------------------------------------------------

    # Seleccionar columnas requeridas
    base_calculo <-
      base_calculo |>
      dplyr::select(ano, mes, rol, tamano, categoria, sexo, grupo, nt, ro, re, ho, he, og)

    return(base_calculo)
  }
