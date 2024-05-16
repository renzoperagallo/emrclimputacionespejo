test_that("calcular_representatividad_por_variable_y_agregacion() funciona (sin ceros)",{
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
    calcular_representatividad_por_variable_y_agregacion(
      mes_t = microdato_t,
      mes_t1 = microdato_t1,
      variable_interes = "nt",
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
      calcular_representatividad_por_variable_y_agregacion(
        mes_t = microdato_t,
        mes_t1 = microdato_t1,
        variable_interes = "nt",
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
