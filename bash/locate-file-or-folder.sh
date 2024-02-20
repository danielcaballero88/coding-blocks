# Encontrar un archivo usando locate y grep
# esto encuentra archivos que incluso contienen la string
locate exampledocs | grep "exampledocs"
# para que sea el archivo completo
locate exampledocs | grep "/exampledocs$" # The $ anchor matches the end of a line
# para carpetas lo anterior tambien sirve
