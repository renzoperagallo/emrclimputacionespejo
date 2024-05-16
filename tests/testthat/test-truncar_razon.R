test_that("Se trunca el valor de la variable al mínimo y máximo, siempre  y cuando el valro original sea NA", {
  df <- tibble::tibble(
    ano = 2023,
    mes = 2,
    grupo = 1,
    sexo = 1,
    tamano = 1,
    categoria = "A",
    variable = "nt",
    valor = NA,
    valor_original = NA,
    min = 20,
    max = 80
  ) |>
    tidyr::expand_grid(
      rol = seq(1,2)
    ) |>
    dplyr::mutate(
      valor = c(10, 100)
    )

  df_truncado <- truncar_razon(df, var = "nt")

  actual <- df_truncado$valor[1]
  expected <- df_truncado$min[1]
  expect_equal(actual, expected)

  actual <- df_truncado$valor[2]
  expected <- df_truncado$max[2]
  expect_equal(actual, expected)
})

test_that("No se truncan los valores cuando el valor es el original", {
  df <- tibble::tibble(
    ano = 2023,
    mes = 2,
    grupo = 1,
    sexo = 1,
    tamano = 1,
    categoria = "A",
    variable = "nt",
    valor = NA,
    valor_original = 1,
    min = 20,
    max = 80
  ) |>
    tidyr::expand_grid(
      rol = seq(1,2)
    ) |>
    dplyr::mutate(
      valor = c(10, 100)
    )

  df_truncado <- truncar_razon(df, var = "nt")

  actual <- df_truncado$valor[1]
  expected <- 10

  expect_equal(actual, expected)

  actual <- df_truncado$valor[2]
  expected <- 100
  expect_equal(actual, expected)
})
