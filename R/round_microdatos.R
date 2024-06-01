#' Redondear microdatos según razones
#'
#' @description
#' Función que redondea los microdatos  para la base final. Todos los valores decimales de NT >0 y <1 se redondean a 1. Posteriormente se redondean OG, HO y HE manteniendo las razones de OGNT, HONT y HENT. Después se redondean RE y RO manteniendo la razón de  REHE y ROHO. Finalmente se recalcula ht y ct.
#'
#' @param df Dataframe con los microdatos  y las columnas correspondientes.
#' @param decimales_truncado A qué número de decimales se realizará el truncamiento (ceiling) de los valores previo al redondeo.
#' @param decimales_redondeo A qué número de decimales se hará el redondeo.
#'
#' @return Mismo dataframe de entrada con los valores redondeados.
#' @export
#'
round_microdatos <- function(df,
                             decimales_truncado = 6,
                             decimales_redondeo = 0){

  # Seleccionar columnas
  df <- df |> dplyr::select(rol, ano, mes, categoria, tamano, grupo, sexo, nt, ho, he, ro, re, og)

  # Calcular indicadores básicos

  df <- calcular_razones(df = df, razon = roho, dividendo = ro, divisor = ho)
  df <- calcular_razones(df = df, razon = hont, dividendo = ho, divisor = nt)
  df <- calcular_razones(df = df, razon = hent, dividendo = he, divisor = nt)
  df <- calcular_razones(df = df, razon = ognt, dividendo = og, divisor = nt)
  df <- calcular_razones(df = df, razon = rehe, dividendo = re, divisor = he)
  df <- df |> dplyr::mutate(dplyr::across(dplyr::all_of(c("roho", "hont", "hent", "ognt","rehe")), ~round(., digits = 10)))

  # Redondear indicadores manteniendo las razones.
  df <-
    df |>
    dplyr::mutate(
      nt =  ifelse(
        nt > 0 & nt < 1,
        1,
        redondear(nt, decimales_truncado = decimales_truncado, decimales_redondeo = decimales_redondeo)
        ),
      ho = redondear(hont*nt, decimales_truncado = decimales_truncado, decimales_redondeo = decimales_redondeo),
      ro = redondear(roho*ho, decimales_truncado = decimales_truncado, decimales_redondeo = decimales_redondeo),
      og = redondear(ognt*nt, decimales_truncado = decimales_truncado, decimales_redondeo = decimales_redondeo),
      he = redondear(hent*nt, decimales_truncado = decimales_truncado, decimales_redondeo = decimales_redondeo),
      re = redondear(rehe*he, decimales_truncado = decimales_truncado, decimales_redondeo = decimales_redondeo)
    )

  # Seleccionar columnas
  df <- df |> dplyr::select(ano, mes, rol, categoria, tamano, grupo, sexo, nt, ho, he, ro, re, og)

  return(df)
}
