## Presentación

Este proyecto intenta poder extraer de los distintos repositorios institucionales de Facultad de Psicología información sobre las tareas de enseñanza que realizan sus docentes.


## Objetivos

El producto debería permitir:
* buscar por docente o por grupo de docentes la cantidad de enseñanza que se realiza.
* buscar por curso o grupo de cursos la cantidad de enseñanza que demanda.

## Scrapeo
Existen dos fuentes de información:
* La web de Facultad de Psicología (https://psico.edu.uy/directorio) donde están todos los docentes de Facultad de Psicología y puede conseguirse información sobre características de su cargo, las horas, etc.
* El Sistema de Información de la Facultad de Psicología (http://sifp.psico.edu.uy/), donde se alojan fichas de cada curso, clasificadas según tipo de curso (UCOS, optativas, prácticas y proyectos) y otras informaciones según cada tipo de curso. En las fichas los existen varias maneras de nombrar a cada docente.

## Decisiones

La cantidad de enseñanza se calcula en cantidad de grupos. En las ucos cada grupo representa unas 2.15 horas. Para las optativas y prácticas con los créditos se podría estandarizar un equivalente para los grupos de ucos (5 créditos ~un grupo de uco ~2.15 horas de clase o usar que 5 créditos ~3 horas de clase).

Nos estamos centrando en los datos 2018.

Se está trabajando con los docentes adscriptos a institutos solamente.

## Logros

El archivo _ensenanza_psico.Rproj_ permite trabajar desde R con Git (https://swcarpentry.github.io/git-novice-es/14-supplemental-rstudio/).

Hasta ahora se consiguió:

1. El diccionario que comunica los nombres de los docentes de psico.edu.uy con el SIFP: _diccionario2_ (el reconocimiento anda por un 60-70%, hay una versión completada a mano en git que se llama _diccionario2_mano.csv_)

2. Obtener el listado de docentes: _docentes_cargos_, con información de cada cargo docente.

3. Obtener la información de cada ficha de optativas.

4. Obtener la información de cada ficha de UCO: cantidad de horarios, dispositivo de enseñanza, responsable (si dice, falta un 20%):

   1. _ucos_: el df con cada uco, cantidad de docentes y cantidad de horarios propuestos de cada tipo de Dispositivo y totales.
   2. _ucos_horarios_: el df con los datos de cada horario, docente responsable y sus datos de la web.
   3. _ucos_docentes_: el df con los nombre.SIFP de los docentes declarados que participan en la UCO

