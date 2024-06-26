% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/redondear.R
\name{redondear}
\alias{redondear}
\title{Redondear}
\usage{
redondear(numero, decimales_truncado = 6, decimales_redondeo = 0)
}
\arguments{
\item{numero}{El número a redondear.}

\item{decimales_truncado}{La cantidad de dígitos a la que se truncará el decimal.}

\item{decimales_redondeo}{la cantidad de decimales a la que se redondeará el número.}
}
\value{
El mismo número de input redondeado según los parámetros especificados.
}
\description{
Función que se utiliza para redondear un número. Se utiliza esta función para asegurar que los valores decimales con pequenas diferencias no generen cambios mayores al imputar.
Para ello, esta función primero trunca al decimal deseado y después redondea al decimal deseado.
}
\examples{
a <- 0.0049999999999
redondear(a, decimales_truncado = 6, decimales_redondeo = 0)
}
