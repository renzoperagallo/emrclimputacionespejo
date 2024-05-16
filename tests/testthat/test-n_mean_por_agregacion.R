test_that("Se genera la cantidad correcta de combinatorias", {

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

  resultado <-
    n_mean_por_agregacion(
      df = microdato_t,
      variable_interes = "nt",
      agregacion = c("tamano", "categoria", "sexo", "grupo")
    )

  expect_equal(2^4, nrow(resultado))

})

test_that("Calcula correctamente el número de cadenas esperadas", {

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
      tamano = 1,
      categoria = "a",
      grupo = 1
    ) |>
    tidyr::expand_grid(
      sexo = c(1,2),
      rol = seq(1,10)
    )

  resultado <-
    n_mean_por_agregacion(
      df = microdato_t,
      variable_interes = "nt",
      agregacion = c("tamano", "categoria", "sexo", "grupo")
    )

  expect_equal(
    10,
    resultado$n_cadena[1]
    )
  expect_equal(
    10,
    resultado$n_cadena[2]
  )

})


test_that("Calcula correctamente el promedio por combinatoria", {

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
      tamano = 1,
      categoria = "a",
      grupo = 1,
      sexo = 1,
      rol = seq(1,10),
      nt = seq(1,10)
    )

  resultado <-
    n_mean_por_agregacion(
      df = microdato_t,
      variable_interes = "nt",
      agregacion = c("tamano", "categoria", "sexo", "grupo")
    )

  expect_equal(
    mean(seq(1,10)),
    resultado$mean_cadena[1]
  )

})

test_that("Funciona con otras variables ", {

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
  expect_no_error(resultado <-
                    n_mean_por_agregacion(
                      df = microdato_t,
                      variable_interes = "nt",
                      agregacion = c("tamano", "categoria", "sexo", "grupo")
                    ))
  expect_no_error(resultado <-
                    n_mean_por_agregacion(
                      df = microdato_t,
                      variable_interes = "ro",
                      agregacion = c("tamano", "categoria", "sexo", "grupo")
                    ))
  expect_no_error(resultado <-
                    n_mean_por_agregacion(
                      df = microdato_t,
                      variable_interes = "ho",
                      agregacion = c("tamano", "categoria", "sexo", "grupo")
                    ))

})
