## Acerca de AppCOVID 19. DV.2020-2021

### Grupo de desarrollo

La aplicación ha sido desarrollada como trabajo teorico práctico de la asignatura de Visualización de datos de la licenciatura en Data Science de la Universidad de Valencia.
Los miembros el grupo de trabajo son:

<ul>
<li> <a href="mailto:dasanhom@alumni.uv.es" target="_blank"> David Sánchez </a> </li>
<li> <a href="mailto:marmuoz3@alumni.uv.es" target="_blank"> Jaime Martínez Muñoz </a> </li>
<li> <a href="mailto:guigadal@alumni.uv.es" target="_blank"> Guillermo Fernando </a> </li>
<li> <a href="mailto:gabibri@alumni.uv.es" target="_blank"> Gabriel Bigorra Brisa </a> </li>
<li> <a href="mailto:jogugil@alumni.uv.es" target="_blank"> José Javier Gutiérrez Gil </a> </li>
</ul>

### Fuentes de datos

Para el desarrollo de la aplicación, el grupo de trabajo ha utilizado diferentes fuentes de datos oficiales para recoger datos históricos y diarios sobre el seguimiento de la pandemia y su implicación en las diferentes comunidades autónomas.
Ver página de guia para saber más sobre los banco de datos utilizados y sus fuentes.

### Tratamiento de datos

Debido a la actual situación de pandemia en la que vivimos, los informes y los datos están sujetos a cambios diarios. En concreto, los datos de seguimiento de la situación de pandemia, vacunación y alarma se actualizarán diariamente según los datos proporcionados por nuestras fuentes de datos.
Esto afectará:

+ Informes de seguimiento de pandemias.
+ Valores totales, de incidencia acumulada y proporciones a novel mundial.
+ Se muestra una foto de la situación actual de estado de la pandemia a nivel nacional para cada una de las comunidades autónomas.
+ Se muestran unas recomendaciones que la OMS ofrece a los ciudadanos de como acctuar ante ciertas situaciones provocadas por el estado critico de salud en el que nos encontramos.

## Preparación y vinculación de datos

Dada la heterogeneidad de las fuentes, hubo que realizar diferentes operaciones de limpieza y adecuación de los datos recogidos y visualizados en la aplicación. En ningún caso se han alterado o modificado los datos, las operaciones han sido cambiar el formato y borrar los datos.
Operaciones básicas que hemos realizado:
	-	Eliminar datos que no necesitamos para la app del conjunto de datos inicial.
	-	Dar formato a los datos para su representación.
 	-  	Hemos eliminado aquellos registros que tienen datos faltantes (NA) en las columnas (variables) que hemos utiliado.En cualquier caso, y teniendo en cuenta una posible mejora o actualización de la aplicación, estos datos eliminados 
		se pueden recuperar e integrar fácilmente en las nuevas actualizaciones, pudiendo aplicar tecnicas de imputación como interpolaciones.
	-	Se han creado nuevas variables necesarias para la representación de la información y darle sentido al estudio.
	-	Se han reconstruido las estructuras donde se almacenan las variables para que su acceso a la hora de representar la información sea más cómoda y rápia.