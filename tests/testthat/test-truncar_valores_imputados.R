parametros <-
  tibble::tibble(
    ano = 2023,
    mes = seq(1,12)
  ) |>
  tidyr::expand_grid(
    variable = c("nt", "hont", "roho", "hent", "rehe")
  ) |>
  dplyr::mutate(
    max = dplyr::case_when(
      variable == "nt" ~ Inf,
      variable == "hont" ~ 1,
      variable == "roho" ~ 100000,
      variable == "hent" ~ 100,
      variable == "rehe" ~ 120000
    ),
    min = dplyr::case_when(
      variable == "nt" ~ -Inf,
      variable == "hont" ~ 1,
      variable == "roho" ~ 1000,
      variable == "hent" ~ -Inf,
      variable == "rehe" ~ 1000
    )
  )

df_original <- tibble::tibble(
  ano = 2023,
  mes = 2,
  grupo = 1,
  sexo = 1,
  tamano = 1,
  categoria = "A",
  nt = 1,
  ro = 1,
  he = 0,
  ho = NA,
  og = 1,
  re = 0
) |>
  tidyr::expand_grid(
    rol = seq(1,2)
  ) |>
  dplyr::mutate(
    ho = dplyr::case_when(
      rol == 1 ~ NA,
      rol == 2 ~ 2
    )
  )

df_imputado <-
  df_original |>
  dplyr::mutate(
    ho = 2
  )

test_that("Se trunca correctamente un valor cuya razon se encuentra por sobre el máximo", {
  df_truncado <-
    aplicar_restricciones(
      microdato_imputado = df_imputado,
      microdato_original = df_original,
      parametros = parametros
    )

  actual = df_truncado$ho[1]
  expected = 1

  expect_equal(actual, expected)
})


test_that("No se imputan un valor que  esta por sobre los límites, pero que no fue sometido a imputación", {
  df_truncado <-
    aplicar_restricciones(
      microdato_imputado = df_imputado,
      microdato_original = df_original,
      parametros = parametros
    )

  actual = df_truncado$ho[2]
  expected = 2

  expect_equal(actual, expected)
})
