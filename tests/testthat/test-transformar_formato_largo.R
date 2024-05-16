df <- calculador_indicadores_basicos(ejemplo_microdato_mes_t)
df <- transformar_formato_largo(df)

test_that("Se crean las columnas valor y variable", {
  expect_true("variable" %in% names(df))
  expect_true("valor" %in% names(df))
}
)

test_that("Se crean las columnas valor y variable", {
  df <- calculador_indicadores_basicos(ejemplo_microdato_mes_t)
  df <- dplyr::select(df, -ho)
  expect_error(transformar_formato_largo(df))
}
)
