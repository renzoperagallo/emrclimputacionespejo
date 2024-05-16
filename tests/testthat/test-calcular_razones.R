test_that("Se crea la columna nueva con el nombre correcto", {

  df <- tibble::tibble(
    rol = 1,
    ro = 2,
    ho = 1
  )

  output <- calcular_razones(
    df = df,
    razon = roho,
    dividendo = ro,
    divisor = ho
  )
  expect_true("roho" %in% names(output) )

})

test_that("Se calcula la razón cuando ambos números son positivos", {

  df <- tibble::tibble(
    rol = 1,
    ro = 2,
    ho = 1
  )

  output <- calcular_razones(
    df = df,
    razon = roho,
    dividendo = ro,
    divisor = ho
  )
  expect_equal(output$roho[1], 2)

})

test_that("Cuando el divisor es 0, resulta 0", {

  df <- tibble::tibble(
    rol = 1,
    ro = 2,
    ho = 0
  )

  output <- calcular_razones(
    df = df,
    razon = roho,
    dividendo = ro,
    divisor = ho
  )
  expect_equal(output$roho[1], 0)
})

test_that("Cuando el divisor es NA, resulta NA", {

  df <- tibble::tibble(
    rol = 1,
    ro = 2,
    ho = NA
  )

  output <- calcular_razones(
    df = df,
    razon = roho,
    dividendo = ro,
    divisor = ho
  )
  expect_true(is.na(output$roho[1]))
})


test_that("Cuando el dividendo es NA, resulta NA", {

  df <- tibble::tibble(
    rol = 1,
    ro = NA,
    ho = 1
  )

  output <- calcular_razones(
    df = df,
    razon = roho,
    dividendo = ro,
    divisor = ho
  )
  expect_true(is.na(output$roho[1]))
})


test_that("Cuando el dividendo es NA y el divisor es NA, resulta NA", {

  df <- tibble::tibble(
    rol = 1,
    ro = NA,
    ho = NA
  )

  output <- calcular_razones(
    df = df,
    razon = roho,
    dividendo = ro,
    divisor = ho
  )
  expect_true(is.na(output$roho[1]))
})

test_that("Cuando el dividendo es 0 y el divisor >0, resulta 0", {

  df <- tibble::tibble(
    rol = 1,
    ro = 0,
    ho = 2
  )

  output <- calcular_razones(
    df = df,
    razon = roho,
    dividendo = ro,
    divisor = ho
  )
  expect_equal(output$roho[1], 0)
})
