# Prueba técnica - Analista de datos
El procesamiento de esta prueba se realizó con la siguiente estructura de carpetas:

* doc - documentación adicional como presentaciones o reportes.
* input - carpeta con los datos insumo para procesamiento.
* output - carpeta que contiene las salidas de los análisis.
* src - códigos de programación para el cargue y depuración de datos y los análisis estadísticos.

En la carpeta src se encuentran los siguientes códigos de programación:

1. SQL00_CreaDataBase.txt - En este se encuentran los comandos que se aplicaron en SQLite para la creación de la base de datos y la creación y cargue de datos en las tablas.
Nota: SQLite se puso como variable del sistema y por eso se puede ejecutar directamente con el comando "sqlite3".
  
2. R00_ProcesaDatos.R - En este código se crea la conexión con la base de datos creada en el código 1, se depuran y consolidan los datos y se hacen los respectivos análisis estadísticos.

## Notas finales
Teniendo en cuenta que la estructura usada para el procesamiento de la prueba técnica es importante pero que los datos usados contienen información sensible se toman las siguientes medidas para el cargue de archivos en el repositorio:
* En la carpeta doc únicamente se carga el pdf con la descripción de la prueba.
* En la carpeta input se carga el archivo de municipios que no contiene información sensible.
* En la carpeta output se carga el archivo de resultados omitiendo las tablas con información sensible.
