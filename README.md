Librería emrclimputacionesespejo
================

<!-- badges: start -->
<!-- badges: end -->

# Descripción

`emrclimputacionesespejo` es una colección de funciones diseñadas para
imputar microdatos por medio de hotdeck, especialmente enfocado en datos
de encuestas de remuneraciones y similares. Este paquete permite
realizar imputaciones basadas en datos de periodos anteriores y calcular
indicadores básicos relevantes para el análisis de remuneraciones y
horas de trabajo.

## Instalación

Puede instalar `emrclimputacionespejo` desde su fuente como sigue:

``` r
# install.packages("pak")
pak::pak("renzoperagallo/emrclimputacionespejo")
```

Una forma alternativa es:

``` r
# install.packages("devtools")
devtools::install_github("renzoperagallo/emrclimputacionespejo")
```

## Instrucciones

La imputación de la base de microdatos para la EMRCL se basa en tres
funciones principales.

### Imputación de valores continuos

La función imputar mes imputa los microdatos del mes T a partir del mes
T-1. El argumento roles_comportamiento_unico indica los roles que se
imputarán forzosamente por arrastre. El número de cadenas mínimas indica
cuál es el número mínimo de vecinos para que una cadena se impute por la
variación de sus vecinos (método de imputación hot-deck en el manual).

``` r
mes_imputado <-
  emrclimputacionespejo::imputar_mes(
    microdato_t = microdato_t,
    microdato_t1 = microdato_t1,
    roles_comportamiento_unico = vec_roles_cu,
    n_cadenas_minimas = 4
  )
```

### Aplicación de restricciones.

Dado que los datos imputados pueden llegar a estar por encima o por
debajo de los rangos establecidos para los valores imputados, esta
función busca conservar las razones entre las distintas variables para
una cadena, pero ajustando los valores imputados a los límites mínimos y
máximos establecidos.

``` r
mes_imputado_restringido <-
  emrclimputacionespejo::aplcar_restricciones(
    microdato_imputado = mes_imputado,
    microdato_original = microdato_t,
    parametros = df_parametros
  )
```

### Redondeo de los valores imputados

La función de redondeo es un paso siguiente que redondea los valores
imputados a números enteros consistentes, conservando las razones de
cada cadena imputada.

``` r
mes_imputado_round <-
  emrclimputacionespejo::round_microdatos(
    df = mes_imputado_restringido,
    decimales_truncado = 6,
    decimales_redondeo = 0
  )
```

## Contribuciones

Las contribuciones a emrclimputacionesespejo son bienvenidas. Por favor,
consulta las instrucciones de contribución para más detalles.

## Licencia

Este paquete está disponible bajo la licencia MIT. Consulta el archivo
LICENSE para más información.
