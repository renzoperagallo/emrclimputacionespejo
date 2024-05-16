test_that("Redondeo funciona", {

  valor <- 0.0000999999
  actual <- redondear(valor)
  expect_equal(actual, 0)
})


test_that("El número de decimales funciona correctamente", {

  valor <- 0.1111111
  actual <- redondear(valor, decimales_redondeo = 1)
  expect_equal(actual, 0.1)
})

test_that("El redondeo se aplica después del truncado", {

  valor <- 1.4
  actual <- redondear(valor, decimales_truncado = 4, decimales_redondeo = 0)
  expect_equal(actual, 1)
})

