# Ejemplo microdato con datos NA.
ejemplo_microdato_mes_t_na <- insertar_NA_aleatorios(ejemplo_microdato_mes_t,
                                 n_filas = 1000,
                                 fila_completa = TRUE,
                                 seed = 123)
ejemplo_microdato_mes_t1_na <- insertar_NA_aleatorios(ejemplo_microdato_mes_t,
                                                     n_filas = 1000,
                                                     fila_completa = TRUE,
                                                     seed = 234)
ejemplo_microdato_mes_t2_na <- insertar_NA_aleatorios(ejemplo_microdato_mes_t,
                                                     n_filas = 1000,
                                                     fila_completa = TRUE,
                                                     seed = 345)
ejemplo_microdato_mes_t3_na <- insertar_NA_aleatorios(ejemplo_microdato_mes_t,
                                                     n_filas = 1000,
                                                     fila_completa = TRUE,
                                                     seed = 456)

ejemplo_microdato_mes_t_na <- ejemplo_microdato_mes_t_na |> dplyr::mutate(estado_encuesta = "completa")
ejemplo_microdato_mes_t1_na <- ejemplo_microdato_mes_t1_na |> dplyr::mutate(estado_encuesta = "completa")
ejemplo_microdato_mes_t2_na <- ejemplo_microdato_mes_t2_na |> dplyr::mutate(estado_encuesta = "completa")
ejemplo_microdato_mes_t3_na <- ejemplo_microdato_mes_t3_na |> dplyr::mutate(estado_encuesta = "completa")

usethis::use_data(ejemplo_microdato_mes_t_na, overwrite = TRUE)
usethis::use_data(ejemplo_microdato_mes_t1_na, overwrite = TRUE)
usethis::use_data(ejemplo_microdato_mes_t2_na, overwrite = TRUE)
usethis::use_data(ejemplo_microdato_mes_t3_na, overwrite = TRUE)
