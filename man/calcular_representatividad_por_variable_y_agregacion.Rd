% Generated by roxygen2: do not edit by hand
% Please edit documentation in
%   R/calcular_representatividad_por_variable_y_agregacion.R
\name{calcular_representatividad_por_variable_y_agregacion}
\alias{calcular_representatividad_por_variable_y_agregacion}
\title{Calcular representatividad por variable y agregación}
\usage{
calcular_representatividad_por_variable_y_agregacion(
  mes_t,
  mes_t1,
  variable_interes,
  agregacion
)
}
\arguments{
\item{mes_t}{Microdato del mes t con las variables calculadas de roho, hont, ront, hent, ognt y rehe  a partir de la función 'calculador_indicadores_basicos()'.}

\item{mes_t1}{Microdato del mes t1 con las variables calculadas de roho, hont, ront, hent, ognt y rehe  a partir de la función 'calculador_indicadores_basicos()'.}

\item{variable_interes}{String con el nombre de la variable de interés para imputar.}

\item{agregacion}{Vector con la agregación requerida. Por ejemplo c("tamano", "categoria", "sexo", "grupo").}
}
\value{
Dataframe con  las columnas especificas al nivel de agregación, el nombre de la variable y los indicadores cálculados para cada combinatoria.
}
\description{
Esta función calcula el alfa de representatividad para una variable y agregación dados. También entrega la variación y el  n° de cadenas esperadas y observadas para dicha agregación.
}
\examples{
# Establecer un nivel de representatividad.
tcsg <- c("tamano", "categoria", "sexo", "grupo")
# Establecer variable de interes
variable_interes = "nt"

# Calcular los indicadores básicos para dos microdatos (t y t1)
microdato_t <- calculador_indicadores_basicos(ejemplo_microdato_mes_t_na)
microdato_t1 <- calculador_indicadores_basicos(ejemplo_microdato_mes_t1_na)

# Calcular los niveles de repreentatividad
representatividad_nt <-
  calcular_representatividad_por_variable_y_agregacion(
  mes_t = microdato_t,
  mes_t1 = microdato_t1,
  variable_interes = variable_interes,
  agregacion = tcsg
  )
representatividad_nt
}
