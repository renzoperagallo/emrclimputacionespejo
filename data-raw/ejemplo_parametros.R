ejemplo_parametros <-
  tibble::tibble(
    ano = 2023,
    mes = seq(1,12)
  ) |>
  tidyr::expand_grid(
    variable = c("nt", "hont", "roho", "hent", "rehe", "ognt")
    ) |>
  dplyr::mutate(
    max = dplyr::case_when(
      variable == "nt" ~ 99999,
      variable == "hont" ~ 260,
      variable == "roho" ~ Inf,
      variable == "hent" ~ 100,
      variable == "rehe" ~ Inf,
      variable == "ognt" ~ Inf
    ),
    min = dplyr::case_when(
      variable == "nt" ~ 0,
      variable == "hont" ~ 1,
      variable == "roho" & mes %in% seq(1,5) ~ 2113,
      variable == "roho" & mes %in% seq(6, 8) ~ 2268,
      variable == "roho" & mes %in% seq(9, 12) ~ 2371,
      variable ==  "hent" ~ 0,
      variable == "rehe" ~ 1000,
      variable == "ognt" ~ 0
    )
  )
usethis::use_data(ejemplo_parametros, overwrite = TRUE)
