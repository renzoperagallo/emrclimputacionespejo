# Cargar y preparar datos desde los microdatos para  usar como bases de ejemplo.

ejemplo_microdato_mes_t <-
  readRDS("./data-raw/76_Base_rectificada_abr2022.rds") |>
  dplyr::mutate(he = ht_abr2022 - ho_abr2022) |>
  dplyr::select(ano ,
                mes ,
                rol = rol_generico,
                tamano,
                categoria,
                sexo,
                grupo,
                nt = nt_abr2022,
                ro = ro_abr2022,
                ho = ho_abr2022,
                ct = c_abr2022,
                he,
                ht = ht_abr2022) |>
  dplyr::mutate(og = ct-ro,# og se estimo para poder usarse.
                re = 0) |> # En la base del boletin no está HE, así que se dejo en 0 para poder usar este script.
  dplyr::mutate(dplyr::across(dplyr::everything(), ~ifelse(is.na(.), 0, .)))

ejemplo_microdato_mes_t1 <-
  readRDS("./data-raw/75_Base_rectificada_mar2022.rds") |>
  dplyr::mutate(he = ht_mar2022 - ho_mar2022) |>
  dplyr::select(ano ,
                mes ,
                rol = rol_generico,
                tamano,
                categoria,
                sexo,
                grupo,
                nt = nt_mar2022,
                ro = ro_mar2022,
                ho = ho_mar2022,
                ct = c_mar2022,
                he,
                ht = ht_mar2022) |>
  dplyr::mutate(og = ct-ro,# og se estimo para poder usarse.
                re = 0) |> # En la base del boletin no está HE, así que se dejo en 0 para poder usar este script.
  dplyr::mutate(dplyr::across(dplyr::everything(), ~ifelse(is.na(.), 0, .)))

ejemplo_microdato_mes_t2 <-
  readRDS("./data-raw/74_Base_rectificada_feb2022.rds") |>
  dplyr::mutate(he = ht_feb2022 - ho_feb2022) |>
  dplyr::select(ano ,
                mes ,
                rol = rol_generico,
                tamano,
                categoria,
                sexo,
                grupo,
                nt = nt_feb2022,
                ro = ro_feb2022,
                ho = ho_feb2022,
                ct = c_feb2022,
                he,
                ht = ht_feb2022) |>
  dplyr::mutate(og = ct-ro,# og se estimo para poder usarse.
                re = 0) |> # En la base del boletin no está HE, así que se dejo en 0 para poder usar este script.
  dplyr::mutate(dplyr::across(dplyr::everything(), ~ifelse(is.na(.), 0, .)))

ejemplo_microdato_mes_t3 <-
  readRDS("./data-raw/73_Base_rectificada_ene2022.rds") |>
  dplyr::mutate(he = ht_ene2022 - ho_ene2022) |>
  dplyr::select(ano ,
                mes ,
                rol = rol_generico,
                tamano,
                categoria,
                sexo,
                grupo,
                nt = nt_ene2022,
                ro = ro_ene2022,
                ho = ho_ene2022,
                ct = c_ene2022,
                he,
                ht = ht_ene2022) |>
  dplyr::mutate(og = ct-ro,# og se estimo para poder usarse.
                re = 0) |> # En la base del boletin no está HE, así que se dejo en 0 para poder usar este script.
  dplyr::mutate(dplyr::across(dplyr::everything(), ~ifelse(is.na(.), 0, .))) |>
  dplyr::mutate(estado_encuesta = "completa")


usethis::use_data(ejemplo_microdato_mes_t, overwrite = TRUE)
usethis::use_data(ejemplo_microdato_mes_t1, overwrite = TRUE)
usethis::use_data(ejemplo_microdato_mes_t2, overwrite = TRUE)
usethis::use_data(ejemplo_microdato_mes_t3, overwrite = TRUE)


