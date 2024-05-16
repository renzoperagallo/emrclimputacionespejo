testthat::test_that("Se imputan valores na con valores del mes anterior", {
  # Crea df
  df <-
    dplyr::tibble(
      ano = 1,
      rol = 1,
      tamano = 1,
      categoria = 1,
      grupo = 1,
      sexo = 1
    ) |>
    tidyr::expand_grid(
      nt = 1, og = 1, ho = 1, he = 1, ro = 1, re = 1, mes = 1:2
    ) |>
    dplyr::mutate(
      nt = ifelse(mes == 1, 1, NA),
      og = ifelse(mes == 1, 1, NA),
      ho = ifelse(mes == 1, 1, NA),
      he = ifelse(mes == 1, 1, NA),
      ro = ifelse(mes == 1, 1, NA),
      re = ifelse(mes == 1, 1, NA)
    )
  df_separados <- split(df, df$mes)

  mes_t1 <- df_separados[[1]]
  mes_t <- df_separados[[2]]

  # Probar la funcion
  df_unido <- imputar_mes_arrastre(microdato_t = mes_t,
                                   microdato_t1 = mes_t1)
  expect_equal(df_unido$nt, 1)
  expect_equal(df_unido$og, 1)
  expect_equal(df_unido$ho, 1)
  expect_equal(df_unido$he, 1)
  expect_equal(df_unido$ro, 1)
  expect_equal(df_unido$re, 1)
}
)
testthat::test_that("Se respetan los ratios al imputar valores", {
  # Crea df
  df <-
    dplyr::tibble(
      ano = 1,
      rol = 1,
      tamano = 1,
      categoria = 1,
      grupo = 1,
      sexo = 1
    ) |>
    tidyr::expand_grid(
      nt = 1, og = 1, ho = 1, he = 1, ro = 1, re = 1, mes = 1:2
    ) |>
    dplyr::mutate(
      nt = ifelse(mes == 1, 1, 2),
      og = ifelse(mes == 1, 1, NA),
      ho = ifelse(mes == 1, 1, NA),
      he = ifelse(mes == 1, 1, NA),
      ro = ifelse(mes == 1, 1, NA),
      re = ifelse(mes == 1, 1, NA)
    )
  df_separados <- split(df, df$mes)

  mes_t1 <- df_separados[[1]]
  mes_t <- df_separados[[2]]

  # Probar la funcion
  df_imputado <- imputar_mes_arrastre(microdato_t = mes_t,
                                   microdato_t1 = mes_t1)
  expect_equal(df_imputado$nt, 2)
  expect_equal(df_imputado$og, 2)
  expect_equal(df_imputado$ho, 2)
  expect_equal(df_imputado$he, 2)
  expect_equal(df_imputado$ro, 2)
  expect_equal(df_imputado$re, 2)
}
)

testthat::test_that("Si el nt imputado es 0, todos los demás valores son 0", {
  # Crea df
  df <-
    dplyr::tibble(
      ano = 1,
      rol = 1,
      tamano = 1,
      categoria = 1,
      grupo = 1,
      sexo = 1
    ) |>
    tidyr::expand_grid(
      nt = 1, og = 1, ho = 1, he = 1, ro = 1, re = 1, mes = 1:2
    ) |>
    dplyr::mutate(
      nt = ifelse(mes == 1, 0, NA),
      og = ifelse(mes == 1, 1, NA),
      ho = ifelse(mes == 1, 1, NA),
      he = ifelse(mes == 1, 1, NA),
      ro = ifelse(mes == 1, 1, NA),
      re = ifelse(mes == 1, 1, NA)
    )
  df_separados <- split(df, df$mes)

  mes_t1 <- df_separados[[1]]
  mes_t <- df_separados[[2]]

  # Probar la funcion
  df_imputado <- imputar_mes_arrastre(microdato_t = mes_t,
                                      microdato_t1 = mes_t1)
  expect_equal(df_imputado$nt, 0)
  expect_equal(df_imputado$og, 0)
  expect_equal(df_imputado$ho, 0)
  expect_equal(df_imputado$he, 0)
  expect_equal(df_imputado$ro, 0)
  expect_equal(df_imputado$re, 0)
}
)
