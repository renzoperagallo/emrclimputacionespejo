lst_globals <-
  paste(
    ":=", "n_cadena.t", "n_cadena.t1" ,"mean_cadena.t" ,"mean_cadena.t1" ,"rol" ,"comportamiento_unico" ,"ano" ,"mes" ,"categoria" ,"tamano" ,"grupo" ,"sexo" ,"nt" ,"ho" ,"he" ,"ro" ,"re" ,"og" ,"variable" ,"valor" ,"valor_t" ,"ano_t" ,"mes_t" ,"hent" ,"ct" ,"hont" ,"roho" ,"rehe" ,"ognt" ,"across" ,"all_of" ,"valor_pri" ,"valor_sec"
    ) |>
  strsplit(" ")

utils::globalVariables(lst_globals[[1]])


