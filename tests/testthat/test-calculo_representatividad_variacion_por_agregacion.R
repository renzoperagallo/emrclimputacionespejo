microdato_t <-
  tibble::tibble(
    ano = 2022,
    mes = 2,
    ro = 1,
    ct = 3,
    he = 1,
    ht = 2,
    ho = 1,
    og = 1,
    re = 0,
    nt = 1,
    rol = 1
  ) |>
  tidyr::expand_grid(
    tamano = c(1,2),
    categoria = c("a", "b"),
    sexo = c(1,2),
    grupo = c(1,2)
  )

microdato_t1 <-
  microdato_t |>
  dplyr::mutate(
    mes = 1
  )

# Establecer un nivel de representatividad.
tcsg <- c("tamano", "categoria", "sexo", "grupo")
# Establecer variable de interes
variable_interes = "nt"

# Calcular los indicadores básicos para dos microdatos (t y t1)
microdato_t <- calculador_indicadores_basicos(microdato_t)
microdato_t1 <- calculador_indicadores_basicos(microdato_t1)

# Calcular el número de observaciones y promedios para el nivel de representatividad dado y una variable "nt".
mes_t <- n_mean_por_agregacion(df = microdato_t, variable_interes = variable_interes, agregacion = tcsg)
mes_t1 <- n_mean_por_agregacion(df = microdato_t1, variable_interes = variable_interes, agregacion = tcsg)

# Calcular los niveles de representatividad
resultado <-
  calculo_representatividad_variacion_por_agregacion(
    mes_t = mes_t,
    mes_t1 = mes_t1,
    variable_interes = variable_interes,
    agregacion = tcsg
  )

test_that("Se ejecuta la función sin error", {

  expect_no_error(
    calculo_representatividad_variacion_por_agregacion(
      mes_t = mes_t,
      mes_t1 = mes_t1,
      variable_interes = variable_interes,
      agregacion = tcsg
      )
  )
})

test_that("Se calculan correctamente el número de cadenas observadas", {
 expect_equal(unique(resultado$n_cadenas_observadas), 1)
})
test_that("Se calculan correctamente el número de cadenas esperadas", {
  expect_equal(unique(resultado$n_cadenas_esperadas), 1)
})

test_that("Se calculan correctamente el nivel de representatividad", {
  expect_equal(unique(resultado$representatividad), 100)
})

test_that("Se calculan correctamente el nivel de representatividad", {
  expect_equal(unique(resultado$variacion), 1)
})
