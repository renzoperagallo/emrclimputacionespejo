mes_t <- tibble::tibble(
  ano = 2022,
  mes = 2,
  grupo = 1,
  sexo = 1,
  tamano = 1,
  categoria = "A",
  ro = 1,
  ct = 3,
  he = 0,
  ht = 1,
  ho = NA,
  og = 1,
  re = 0
) |>
  tidyr::expand_grid(
    rol = seq(1,4)
  ) |>
  dplyr::mutate(
    nt = c(1,NA,1, NA),
    ho = c(1,NA,1, NA)
    )

mes_t1 <- tibble::tibble(
  ano = 2022,
  mes = 1,
  grupo = 1,
  sexo = 1,
  tamano = 1,
  categoria = "A",
  ro = 1,
  ct = 3,
  he = 0,
  ht = 1,
  ho = 1,
  og = 1,
  nt = 1,
  re = 0
) |>
  tidyr::expand_grid(
    rol = seq(1,4)
  )


test_that("Solo se imputa la variable requerida ", {

  microdato_imputado_mes_t <-
    imputar_variables(
      microdato_t = mes_t,
      microdato_t1 = mes_t1,
      n_cadenas_minimas = 1,
      variables_imputacion = c("nt")
    )

  expect_false(is.na(microdato_imputado_mes_t$nt[2]))
  expect_true(is.na(microdato_imputado_mes_t$ho[2]))

  microdato_imputado_mes_t <-
    imputar_variables(
      microdato_t = microdato_imputado_mes_t,
      microdato_t1 = mes_t1,
      n_cadenas_minimas = 1,
      variables_imputacion = c("hont")
    )

  expect_false(is.na(microdato_imputado_mes_t$ho[2]))
}
)
