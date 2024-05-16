test_that("Se redondea valores mayores que 0 y menores que 1 a 1", {

df <- tibble::tibble(
  ano = 2023,
  mes = 2,
  rol = 1,
  grupo = 1,
  sexo = 1,
  tamano = 1,
  categoria = "A",
  nt = 0.1,
  ro = 1,
  he = 1,
  ho = 1,
  og = 1,
  re = 1
)

df_redondeado <- round_microdatos(df)

df_esperado <- tibble::tibble(
  ano = 2023,
  mes = 2,
  rol = 1,
  categoria = "A",
  tamano = 1,
  grupo = 1,
  sexo = 1,
  nt = 1,
  ho = 10,
  he = 10,
  ro = 10,
  re = 10,
  og = 10
)

  expect_equal(df_redondeado, df_esperado)
})

test_that("Se redondea hacia abajo cuando es menor que .5", {

  df <- tibble::tibble(
    ano = 2023,
    mes = 2,
    rol = 1,
    grupo = 1,
    sexo = 1,
    tamano = 1,
    categoria = "A",
    nt = 1,
    ro = 1.49999,
    he = 1,
    ho = 1,
    og = 1,
    re = 1.49999
  )

  df_redondeado <- round_microdatos(df)

  df_esperado <- tibble::tibble(
    ano = 2023,
    mes = 2,
    rol = 1,
    categoria = "A",
    tamano = 1,
    grupo = 1,
    sexo = 1,
    nt = 1,
    ho = 1,
    he = 1,
    ro = 1,
    re = 1,
    og = 1
  )

  expect_equal(df_redondeado, df_esperado)
})


test_that("Se redondea correctamente HO hacia arriba cuando es 1.5", {

  df <- tibble::tibble(
    ano = 2023,
    mes = 2,
    rol = 1,
    grupo = 1,
    sexo = 1,
    tamano = 1,
    categoria = "A",
    nt = 1,
    ro = 1,
    he = 1,
    ho = 1.5,
    og = 1,
    re = 1
  )

  df_redondeado <- round_microdatos(df)

  df_esperado <- tibble::tibble(
    ano = 2023,
    mes = 2,
    rol = 1,
    categoria = "A",
    tamano = 1,
    grupo = 1,
    sexo = 1,
    nt = 1,
    ho = 2,
    he = 1,
    ro = 1,
    re = 1,
    og = 1
  )

  expect_equal(df_redondeado, df_esperado)
})


# truncar -----------------------------------------------------------------


