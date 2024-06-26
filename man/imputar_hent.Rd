% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/imputar_hent.R
\name{imputar_hent}
\alias{imputar_hent}
\title{Imputación de la Variable "hent" basada en Datos del Periodo Anterior}
\usage{
imputar_hent(microdato_t, microdato_t1)
}
\arguments{
\item{microdato_t}{Dataframe con los microdatos del mes t.}

\item{microdato_t1}{Dataframe con los microdatos del mes t1.}
}
\value{
Devuelve el DataFrame original con la columna he imputada según los criterios establecidos.
}
\description{
Esta función imputa valores faltantes para la variable hent (horas extras reportadas) en un conjunto de datos.
}
\examples{
microdato_imputado_mes_t <-
  imputar_hent(
    microdato_t = ejemplo_microdato_mes_t_na,
    microdato_t1 = ejemplo_microdato_mes_t1
  )
microdato_imputado_mes_t
}
