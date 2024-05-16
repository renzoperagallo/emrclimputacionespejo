test_that("Cuando existen valores para las variables, prefiere los valores del df prioritario", {
  # Crea df
  df <-
    dplyr::tibble(
      ano = 1,
      mes = 1,
      rol = 1,
      tamano = 1,
      categoria = 1,
      grupo = 1,
      sexo = 1
    ) |>
    tidyr::expand_grid(
      nt = 1, og = 1, ho = 1, he = 1, ro = 1, re = 1, metodo = 1:2
    ) |>
    dplyr::mutate(
      nt = ifelse(metodo == 1, 1, 2),
      og = ifelse(metodo == 1, 1, NA),
      ho = ifelse(metodo == 1, 1, 2),
      he = ifelse(metodo == 1, 1, 2),
      ro = ifelse(metodo == 1, 1, 2),
      re = ifelse(metodo == 1, 1, 2),
      ct = ro + re + og
    )
  df_separados <- split(df, df$metodo)

  df_prioritario <- df_separados[[1]]
  df_secundario <- df_separados[[2]]


  # Probar la funcion
  df_unido <- unir_microdatos_segun_prioridad(df_prioritario = df_prioritario,
                                              df_secundario = df_secundario)
    expect_equal(df_unido$nt, 1)
    expect_equal(df_unido$og, 1)
  }
  )
test_that("Cuando no hay valores para df prioritario, prefiere los valores del df secundario", {
  # Crea df
  df <-
    dplyr::tibble(
      ano = 1,
      mes = 1,
      rol = 1,
      tamano = 1,
      categoria = 1,
      grupo = 1,
      sexo = 1
    ) |>
    tidyr::expand_grid(
      nt = 1, og = 1, ho = 1, he = 1, ro = 1, re = 1, metodo = 1:2
    ) |>
    dplyr::mutate(
      nt = ifelse(metodo == 1, NA, 2),
      og = ifelse(metodo == 1, NA, 2),
      ho = ifelse(metodo == 1, NA, 2),
      he = ifelse(metodo == 1, NA, 2),
      ro = ifelse(metodo == 1, NA, 2),
      re = ifelse(metodo == 1, NA, 2),
      ct = ro + re + og
    )
  df_separados <- split(df, df$metodo)

  df_prioritario <- df_separados[[1]]
  df_secundario <- df_separados[[2]]


  # Probar la funcion
  df_unido <- unir_microdatos_segun_prioridad(df_prioritario = df_prioritario,
                                              df_secundario = df_secundario)
  expect_equal(df_unido$nt, 2)
}
)

test_that("Cuando hay na en ambos df para un valor, el resultado es NA", {
  # Crea df
  df <-
    dplyr::tibble(
      ano = 1,
      mes = 1,
      rol = 1,
      tamano = 1,
      categoria = 1,
      grupo = 1,
      sexo = 1
    ) |>
    tidyr::expand_grid(
      nt = 1, og = 1, ho = 1, he = 1, ro = 1, re = 1, metodo = 1:2
    ) |>
    dplyr::mutate(
      nt = ifelse(metodo == 1, NA, NA),
      og = ifelse(metodo == 1, 0, 0),
      ho = ifelse(metodo == 1, NA, 2),
      he = ifelse(metodo == 1, NA, 2),
      ro = ifelse(metodo == 1, NA, 2),
      re = ifelse(metodo == 1, NA, 2),
      ct = ro + re + og
    )
  df_separados <- split(df, df$metodo)

  df_prioritario <- df_separados[[1]]
  df_secundario <- df_separados[[2]]


  # Probar la funcion
  df_unido <- unir_microdatos_segun_prioridad(df_prioritario = df_prioritario,
                                              df_secundario = df_secundario)
  expect_true(is.na(df_unido$nt))
}
)
