test_that("Se separan correctamente los roles de comportamiento único", {

  vector_comportamiento_unico <- seq(1, 10)
  df <-
    tibble::tibble(
      rol = seq(1,20),
      ro = 1000
    )


  resultado <- excluir_comportamiento_unico(
    microdato = df,
    roles_comportamiento_unico = vector_comportamiento_unico
    )

  expect_equal(resultado$microdato_sin_cu$rol, seq(11,20))
  expect_equal(resultado$microdato_con_cu$rol, vector_comportamiento_unico)

})
