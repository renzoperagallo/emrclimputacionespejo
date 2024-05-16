test_that("Se imputa sin errores ", {

  expect_no_error(
    microdato_imputado_mes_t <- imputar_mes(microdato_t = ejemplo_microdato_mes_t_na,
                                            microdato_t1 = ejemplo_microdato_mes_t1_na,
                                            roles_comportamiento_unico = ejemplo_roles_comportamiento_unico,
                                            n_cadenas_minimas = 4)
  )
}
)

test_that("Se imputan todos los valores ", {
  tbl <-
    dplyr::tibble(ano = 1, rol = 1:8, tamano = 1, categoria = 1) |>
    tidyr::expand_grid(name = c("nt", "og", "ho", "he", "ro", "re")) |>
    tidyr::expand_grid(mes = 1:2, grupo = 1, sexo = 1, value = 1) |>
    tidyr::pivot_wider() |>
    dplyr::mutate(
      dplyr::across(
        og,
        ~ dplyr::case_when(
          rol <= 4 & mes == 2 ~ NA,
          rol >= 5 & mes == 1  ~ 0,
          TRUE ~ .x
        )
      )
    )
  tbl_cur <- dplyr::filter(tbl, mes == 2)
  tbl_lag <- dplyr::filter(tbl, mes == 1)
  microdato_imputado_mes_t <-  imputar_mes(microdato_t = tbl_cur, microdato_t1 = tbl_lag)

  expect_equal(
    sum(is.na(microdato_imputado_mes_t)), 0
  )
}
)
