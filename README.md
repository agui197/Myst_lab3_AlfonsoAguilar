# Myst_lab3_AlfonsoAguilar
En este repositorio se aplica una estrategia de trading con análisis técnico para comprar o vender  traves del broker OANDA el par de divisas EUR_USD

## Módelo
El análisis técnico usado fue WMA (Promedio Móvil Ponderado), mientras en el promedio móvil simple asignamos igual importancia a todos los datos, para el WMA se toma el supuesto que afirma que algunos de estos datos son más importantes, un ejemplo es que el pasado lejano no sea tan relevante como el pasado próximo.

## Variables 
Ventana <- Al igual que con el promedio móvil simple, debemos definir un périodo de días con los que se calculara un precio promedio
Ponderaciones <- Es necesario definir las importancias que se tomaran tambien para cada uno de estos dias de la ventana deseada
pI <- Porcentaje de inversion inicial, con el fin de inicializar la cartera con un cierto porcentaje (puede o no existir esto)
pO <- El porcentaje por operacion es la cantidad que se compra o se vende cada vez que se presenta una señal
Precios <- El historico de precios del par de divisas con el cual se hara la estrategia.

## Regla de trading
Las Medias Móviles en Análisis Técnico tienen como principal función la se suavizar las gráficas y eliminar el ruido producido por movimientos bruscos en el precio, dándonos una visión mas limpia de la Tendencia del Valor. 

Una Media Móvil nos da una SEÑAL DE COMPRA en el momento que el precio la rompe de abajo hacia arriba.

Una Media Móvil nos da una SEÑAL DE VENTA en el momento que el precio rompe dicha media desde arriba hacia abajo.
