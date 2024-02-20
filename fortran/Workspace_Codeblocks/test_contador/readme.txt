Programita de fortran para desdoblegar loops anidados en uno solo
Esto es util cuando no se sabe cuantos loops anidados va a haber

Ejemplo: quiero calcular todas las posibles combinaciones de parametros, sin saber de antemano cuantos parametros voy a variar
Si son dos parametros necesito dos loops anidados, si son tres parametros, tres. Etc...

Solucion: calculo el numero total de iteraciones a hacer con el producto de la cantidad de elementos de cada dimension. 
Hago un solo bucle con todas esas iteraciones y voy simulando como variaria el contador en loops anidados
Para eso me imagino algo parecido a un odometro. 
Voy aumentando de a uno el primer indice. Cuando supera la longitud, lo reseteo a cero.
Cuando se resetea un indice, el subsiguiente aumenta en 1.
Repito.

Es muy rapido, 15000 millones de itereaciones en 10 segundos. No tengo que preocuparme por la performance.
