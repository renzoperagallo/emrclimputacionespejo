  test_that("calcular_representatividad_todas_variables() funciona (sin ceros)",{
    tbl <-
      tidyr::expand_grid(
        tamano = 1, categoria = 1, rol = 1:2, grupo = 1, sexo = 1, mes = 1:2,
        nt = 1, og = 1, ho = 1, he = 1, ro = 1, re = 1
      ) |>
      dplyr::mutate(
        nt =
          dplyr::case_when(
            mes == 2 & rol == 2 ~ NA,
            mes == 2 & rol == 1 ~ 2,
            TRUE ~ nt
          ),
      ) |>
      calculador_indicadores_basicos()

    id <- "null"

    microdato_t <- tbl |> dplyr::filter(mes == 2)
    microdato_t1 <- tbl |> dplyr::filter(mes == 1)

    # Aplicar la funcion
     out <-
      calcular_representatividad_todas_variables(
       mes_t = microdato_t,
       mes_t1 = microdato_t1,
       vector_variables_interes = c("nt", "roho", "hont", "ognt"),
       agregacion = c("tamano", "categoria", "sexo", "grupo")
      )

    # Extrae los resultados para nt
    actual <-
      out |>
      dplyr::filter(variable == "nt")

    expected <-
      dplyr::tibble(
        tamano = 1, categoria = 1,
         sexo = 1, grupo = 1
      ) |>
      dplyr::mutate(
        n_cadenas_observadas = 1,
        n_cadenas_esperadas = 2,
        representatividad = 50,
        variacion = 2,
        variable = "nt"
      )

    expect_equal(actual, expected)
  })

"calcular_representatividad_todas_variables() funciona (con ceros)" |>
  test_that({
    tbl <-
      tidyr::expand_grid(
        tamano = 1, categoria = 1, rol = 1:2, grupo = 1, sexo = 1, mes = 1:2,
        nt = 1, og = 1, ho = 1, he = 1, ro = 1, re = 1
      ) |>
      dplyr::mutate(
        nt =
          dplyr::case_when(
            mes == 2 & rol == 2 ~ NA,
            mes == 2 & rol == 1 ~ 2,
            mes == 1 & rol == 1 ~ 0,
            TRUE ~ nt
          ),
      ) |>
      calculador_indicadores_basicos()

    id <- "null"

    microdato_t <- tbl |> dplyr::filter(mes == 2)
    microdato_t1 <- tbl |> dplyr::filter(mes == 1)

    by <- c("sexo", "grupo", "tamano", "categoria")
    id <- "null"

    # Aplicar la funcion
    out <-
      calcular_representatividad_todas_variables(
        mes_t = microdato_t,
        mes_t1 = microdato_t1,
        vector_variables_interes = c("nt", "roho", "hont", "ognt"),
        agregacion = c("tamano", "categoria", "sexo", "grupo")
      )
    # Extrae los resultados para nt
    actual <-
      out |>
      dplyr::filter(variable == "nt")

    expected <-
      dplyr::tibble(
        tamano = 1, categoria = 1,
        sexo = 1, grupo = 1
      ) |>
      dplyr::mutate(
        n_cadenas_observadas = 1,
        n_cadenas_esperadas = 1,
        representatividad = 100,
        variacion = 2,
        variable = "nt"
      )

      expect_equal(actual, expected)
  })

# "calculo_nivel_representatividad() funciona (by == NULL, sin ceros)" |>
#   test_that({
#     tbl <-
#       tidyr::expand_grid(
#         tamano = 1, categoria = 1, rol = 1:2, grupo = 1, sexo = 1, mes = 1:2,
#         nt = 1, og = 1, ho = 1, he = 1, ro = 1, re = 1
#       ) |>
#       dplyr::mutate(
#         lag = dplyr::if_else(mes == 2, "", "_t1"),
#         nt =
#           dplyr::case_when(
#             mes == 2 & rol == 2 ~ NA,
#             mes == 2 & rol == 1 ~ 2,
#             TRUE ~ nt
#           ),
#       ) |>
#       calculador_indicadores_basicos() |>
#       dplyr::relocate(lag, .after = mes) |>
#       tidyr::pivot_longer(nt:rehe) |>
#       dplyr::mutate(name = paste0(name, lag)) |>
#       dplyr::select(-lag, -mes) |>
#       tidyr::pivot_wider()
#
#     by <- c("sexo", "grupo", "tamano", "categoria")
#     id <- "null"
#
#     out <-
#       calculo_nivel_representatividad(tbl, by, id) |>
#       dplyr::select(-c(nt_t1:rehe)) |>
#       tidyr::pivot_longer(null_rep_roho:null_n_cadenas_hont_esperadas) |>
#       dplyr::rowwise() |>
#       dplyr::mutate(
#         name = gsub("null_", "", name),
#         variable = stringr::str_extract(name, "(roho|nt|hont|ognt)", group = 1),
#         stat = stringr::str_extract(name, "(rep|var|n_cadenas)", group = 1),
#         suffix = stringr::str_extract(name, "(observadas|esperadas)", group = 1),
#         suffix = tidyr::replace_na(suffix, ""),
#         stat = paste(stat, suffix, sep = "_"),
#         stat = stringr::str_replace(stat, "(.*)_$", "\\1"),
#       ) |>
#       dplyr::select(-suffix, -name)
#
#     # Extrae los resultados para nt
#     actual <-
#       out |>
#       dplyr::filter(variable == "nt") |>
#       tidyr::pivot_wider(names_from = "stat") |>
#       dplyr::select(-variable)
#
#     expected <-
#       dplyr::tibble(
#         tamano = 1, categoria = 1, rol = 1:2,
#         grupo = 1, sexo = 1
#       ) |>
#       dplyr::mutate(
#         rep = 50,
#         var = 2,
#         n_cadenas_observadas = 1,
#         n_cadenas_esperadas = 2
#       )
#
#     expect_equal(actual, expected)
#   })
#
# "calculo_nivel_representatividad() funciona (by == NULL, con ceros)" |>
#   test_that({
#     tbl <-
#       tidyr::expand_grid(
#         tamano = 1, categoria = 1, rol = 1:2, grupo = 1, sexo = 1, mes = 1:2,
#         nt = 1, og = 1, ho = 1, he = 1, ro = 1, re = 1
#       ) |>
#       dplyr::mutate(
#         lag = dplyr::if_else(mes == 2, "", "_t1"),
#         nt =
#           dplyr::case_when(
#             mes == 2 & rol == 2 ~ NA,
#             mes == 2 & rol == 1 ~ 2,
#             mes == 1 & rol == 1 ~ 0,
#             TRUE ~ nt
#           ),
#       ) |>
#       calculador_indicadores_basicos() |>
#       dplyr::relocate(lag, .after = mes) |>
#       tidyr::pivot_longer(nt:rehe) |>
#       dplyr::mutate(name = paste0(name, lag)) |>
#       dplyr::select(-lag, -mes) |>
#       tidyr::pivot_wider()
#
#     by <- c("sexo", "grupo", "tamano", "categoria")
#     id <- "null"
#
#     out <-
#       calculo_nivel_representatividad(tbl, by, id) |>
#       dplyr::select(-c(nt_t1:rehe)) |>
#       tidyr::pivot_longer(null_rep_roho:null_n_cadenas_hont_esperadas) |>
#       dplyr::rowwise() |>
#       dplyr::mutate(
#         name = gsub("null_", "", name),
#         variable = stringr::str_extract(name, "(roho|nt|hont|ognt)", group = 1),
#         stat = stringr::str_extract(name, "(rep|var|n_cadenas)", group = 1),
#         suffix = stringr::str_extract(name, "(observadas|esperadas)", group = 1),
#         suffix = tidyr::replace_na(suffix, ""),
#         stat = paste(stat, suffix, sep = "_"),
#         stat = stringr::str_replace(stat, "(.*)_$", "\\1"),
#       ) |>
#       dplyr::select(-suffix, -name)
#
#     # Extrae los resultados para nt
#     actual <-
#       out |>
#       dplyr::filter(variable == "nt") |>
#       tidyr::pivot_wider(names_from = "stat") |>
#       dplyr::select(-variable)
#
#     expected <-
#       dplyr::tibble(tamano = 1, categoria = 1, rol = 1:2, grupo = 1, sexo = 1) |>
#       dplyr::mutate(
#         rep = 100,
#         var = 2,
#         n_cadenas_observadas = 1,
#         n_cadenas_esperadas = 1
#       )
#
#     expect_equal(actual, expected)
#   })
#
# "calculo_nivel_representatividad() funciona (by != NULL, con ceros)" |>
#   test_that({
#     tbl <-
#       dplyr::tribble(
#         ~tamano, ~categoria, ~rol, ~grupo, ~sexo, ~mes, ~nt,
#         1, 1, 1, 1, 1, 1, 1,
#         1, 1, 2, 1, 1, 1, 1,
#         1, 1, 1, 1, 2, 1, 1,
#         1, 1, 1, 1, 1, 2, 2,
#         1, 1, 2, 1, 1, 2, NA
#       ) |>
#       dplyr::mutate(
#         og = 1, ho = 1, he = 1, ro = 1, re = 1,
#         lag = dplyr::if_else(mes == 2, "", "_t1"),
#       ) |>
#       calculador_indicadores_basicos() |>
#       dplyr::relocate(lag, .after = mes) |>
#       tidyr::pivot_longer(nt:rehe) |>
#       dplyr::mutate(name = paste0(name, lag)) |>
#       dplyr::select(-lag, -mes) |>
#       tidyr::pivot_wider()
#
#     by <- c("sexo")
#     id <- "null"
#
#     out <-
#       calculo_nivel_representatividad(tbl, by, id) |>
#       dplyr::select(-c(nt_t1:rehe)) |>
#       tidyr::pivot_longer(null_rep_roho:null_n_cadenas_hont_esperadas) |>
#       dplyr::rowwise() |>
#       dplyr::mutate(
#         name = gsub("null_", "", name),
#         variable =
#           stringr::str_extract(name, "(roho|nt|hont|ognt)", group = 1),
#         stat =
#           stringr::str_extract(name, "(rep|var|n_cadenas)", group = 1),
#         suffix =
#           stringr::str_extract(name, "(observadas|esperadas)", group = 1),
#         suffix = tidyr::replace_na(suffix, ""),
#         stat = paste(stat, suffix, sep = "_"),
#         stat = stringr::str_replace(stat, "(.*)_$", "\\1"),
#       ) |>
#       dplyr::select(-suffix, -name)
#
#     # Extrae los resultados para nt
#     actual <-
#       out |>
#       dplyr::filter(variable == "nt") |>
#       tidyr::pivot_wider(names_from = "stat") |>
#       dplyr::select(-variable)
#
#     expected <-
#       dplyr::tribble(
#         ~tamano, ~categoria, ~rol, ~grupo, ~sexo, ~rep, ~var,
#         ~n_cadenas_observadas, ~n_cadenas_esperadas,
#         1, 1, 1, 1, 1, 50, 2, 1, 2,
#         1, 1, 2, 1, 1, 50, 2, 1, 2,
#         1, 1, 1, 1, 2, NA, NA, NA, 1
#       )
#
#     expect_equal(actual, expected)
#   })
