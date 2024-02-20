# Example Package

This is a simple example package. You can use
[Github-flavored Markdown](https://guides.github.com/features/mastering-markdown/)
to write your content.

# Notas mias (Dancab) 

## Lo primero: tener una estructura de directorio correcta. 

Para eso hay que saber diferenciar entre lo que es el proyecto y lo que es el paquete (o los paquetes, puede haber varios). El primer directorio es el directorio del proyecto, y dentro de este debe haber un directorio para cada paquete. Esto puede resultar confuso porque en la mayoria de los tutoriales (manga de hdps) usan el mismo nombre para ambas cosas, exceptuando quizas una mayuscula de mas en alguno de los dos.
Ademas, dentro de la carpeta del paquete (o package, claro) puede haber mas carpetas con subpackages. Es importante notar que cada package (directorio) se identifica por tener un archivo `__init__.py` (aunque parece que esto fue asi en una epoca y ya no es mas necesario, pero sigue siendo buena practica? Quien sabe) y puede tener un numero variable de modulos (archivos `.py`).
Un ejemplo de estructura que dan en un [tutorial][1] es:

```
funniest/
    funniest/
        __init__.py
    setup.py
```

En el caso anterior, el primer directorio `funniest` podria tener cualquier nombre, no interesa (creo), y seria el directorio del proyecto. El archivo `setup.py` es donde se encuentra toda la configuracion para la instalacion de los packages. Luego, la segunda carpeta `funniest` constituye el paquete. Esto es un poco confuso por culpa de los nombres de mierda que tienen las carpetas. Es mas facilmente entendible con esta otra estructura:

```
proyecto_ejemplo/
    modulo_ejemplo/
        __init__.py
    setup.py
```

Supongamos ademas que en `setup.py` se setea `name="paquete-ejemplo"`, entonces al instalar el package (despues digo como hacerlo) resulta que se instala un paquete de nombre `paquete-ejemplo`, y puedo hacer desde python `import modulo_ejemplo`. De lo cual infiero que el paquete contiene un modulo (puede contener varios). Creo que el primer ejemplo de estructura de directorio donde hay dos carpetas con el mismo nombre es justamente para confundir (un poco a propositos) el paquete con el modulo, pero no me parece la mejor forma de entenderlo, ademas porque en setup se puede estar dandole otro nombre diferente al paquete.

[1]: https://python-packaging.readthedocs.io/en/latest/minimal.html
