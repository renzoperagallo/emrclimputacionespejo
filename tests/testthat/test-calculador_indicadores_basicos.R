test_that("run without errors", {
  expect_no_error(
    calculador_indicadores_basicos(ejemplo_microdato_mes_t)
  )
})
test_that("Calculation is working ok", {
  df <- tibble::tribble(
    ~ano, ~mes, ~rol, ~tamano, ~categoria, ~sexo, ~grupo, ~nt, ~ro, ~ho, ~ct, ~he, ~ht, ~og, ~re,
    2020, 2,    123,  1,       "B",        1,    1,      10,  100000, 100, 200000, 50, 150, 50000, 50000
  )
  resultado <- calculador_indicadores_basicos(df)
  expect_equal(
    df$ro / df$ho,
    resultado$roho
  )
  expect_equal(
    df$og / df$nt,
    resultado$ognt
  )
  expect_equal(
    df$re / df$he,
    resultado$rehe
  )
  expect_equal(
    df$ho / df$nt,
    resultado$hont
  )
})
test_that("Function is working ok with NA", {
  df <- tibble::tribble(
    ~ano, ~mes, ~rol, ~tamano, ~categoria, ~sexo, ~grupo, ~nt, ~ro, ~ho, ~ct, ~he, ~ht, ~og, ~re,
    2020, 2,    123,  1,       "B",        1,    1,      10,  NA, NA, 200000, 50, 150, NA, NA
  )
  resultado <- calculador_indicadores_basicos(df)
  expect_true(
    is.na(resultado$roho)
  )
  expect_true(
    is.na(resultado$ognt)
  )
  expect_true(
    is.na(resultado$rehe)
  )
  expect_true(
    is.na(resultado$hont)
  )
})
test_that("calculador_indicadores_basicos calcula correctamente los indicadores", {
  # Creamos un dataframe de prueba
  df <- data.frame(
    ro = c(100, 200, NA, 0),
    ho = c(50, 0, 100, 0),
    nt = c(2, 3, 1, 0),
    re = c(30, 0, NA, 0),
    he = c(10, 0, 20, 0),
    og = c(200, 300, 100, 0)
  )

  # Llamamos a la función
  df_resultado <- calculador_indicadores_basicos(df)

  # Verificamos que los indicadores se calculan correctamente
  expect_equal(df_resultado$roho, c(2, NA, NA, 0))
  expect_equal(df_resultado$hont, c(25, NA, 100, 0))
  expect_equal(df_resultado$ront, c(50, 200/3, NA, 0))
  expect_equal(df_resultado$hent, c(5, 0, 20, 0))
  expect_equal(df_resultado$ognt, c(100, 100, 100, 0))
  expect_equal(df_resultado$rehe, c(3, 0, NA, 0))
})

