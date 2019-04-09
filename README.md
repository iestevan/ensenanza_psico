Este proyecto intenta poder extraer de los distintos repositorios institucionales de Facultad de Psicología información sobre las tareas de enseñanza que realizan sus docentes.

Existen dos fuentes de información:
*La web de Facultad de Psicología (psico.edu.uy/directorio) donde están todos los docentes de Facultad de Psicología y puede conseguirse información sobre características de su cargo, las horas, etc.
*El Sistema de Información de la Facultad de Psicología (http://sifp.psico.edu.uy/), donde se alojan fichas de cada curso, clasificadas según tipo de curso (UCOS, optativas, prácticas y proyectos) y otras informaciones según cada tipo de curso. En las fichas los existen varias maneras de nombrar a cada docente.

El producto debería permitir
*buscar por docente o por grupo de docentes la cantidad de enseñanza que se realiza.
*buscar por curso o grupo de cursos la cantidad de enseñanza que demanda.

Se necesita un diccionario para enlazar las distintas maneras de llamar a un docente en los distintos sistemas de información.

Nos estanmos centrando en los datos 2018.

El archivo ensenanza_psico.Rproj permite trabajar desde R con git.

Hasta ahora se consiguió:

*El diccionario que comunica los nombres de los docentes de psico.edu.uy con el SIFP: diccionario2

*Obtener la información de cada ficha de optativas.

*Obtener la información de cada ficha de UCO: cantidad de horarios, dispositivo de enseñanza, responsable (si dice, falta un 20%):

**ucos: el archivo con cada uco, docentes y horarios propuestos.

**ucos_horarios: el archivo con los datos de cada horario.

*Obtener el listado de docentes: docentes_cargos, con información de cada cargo docente.
