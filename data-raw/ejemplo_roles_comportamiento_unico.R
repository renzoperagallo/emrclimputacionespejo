# Crear vector con roles únicos
roles <-
  ejemplo_microdato_mes_t |>
  dplyr::bind_rows(ejemplo_microdato_mes_t1, ejemplo_microdato_mes_t2, ejemplo_microdato_mes_t3) |>
  dplyr::select(rol)

roles <- as.vector(unique(roles$rol))
set.seed(123)
ejemplo_roles_comportamiento_unico <- sample(roles, 200)
usethis::use_data(ejemplo_roles_comportamiento_unico, overwrite = TRUE)


