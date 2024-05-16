mes_t <- tibble::tibble(
  ano = 2022,
  mes = 2,
  grupo = 1,
  sexo = 1,
  tamano = 1,
  categoria = "A",
  ro = 1,
  ct = 3,
  he = NA,
  ht = NA,
  ho = 1,
  og = 1,
  re = 0,
  nt = 1
) |>
  tidyr::expand_grid(rol = seq(1,4)) |>
  dplyr::mutate(
    nt = c(0,1,10,1)
  )

mes_t1 <- tibble::tibble(
  ano = 2022,
  mes = 12,
  grupo = 1,
  sexo = 1,
  tamano = 1,
  categoria = "A",
  ro = 1,
  ct = 3,
  he = 1,
  ht = 2,
  ho = 1,
  og = 1,
  re = 0,
  nt = 1
) |>
  tidyr::expand_grid(rol = seq(1,4))

df_imputado <- imputar_hent(microdato_t = mes_t,
                            microdato_t1 = mes_t1)


test_that("Imputa 0 en hent cuando nt es 0", {
  expect_equal(df_imputado$he[1], 0)
})

test_that("Imputa el valor de t1 correctamente cuando nt es igual", {
  expect_equal(df_imputado$he[2], 1)
})

test_that("El valor de HE se ajusta al  nt en el mes t", {
  expect_equal(df_imputado$he[3], 10)
})




