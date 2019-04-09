
#----
#librerías y directorio
#----

library(rvest)
library(dplyr)
library(tidyr)
library(tibble)
library(reshape2)
library(httr)
library(stringr)

############
############
#DICCIONARIO
############
############

#correspondencia de docentes de SIFP en fichas con el listado de docentes----
#optimicé los "costos" para que el Nº de NO reconocidos y DOBLEMENTE reconocidos sea el más pequeño
# ucos_docentes= ucos_docentes %>%
# mutate (nombre.SIFP = ifelse (sapply(gregexpr("\\W+", nombre.SIFP), length)+1 > 3,
#                            paste(word(ucos_docentes$nombre.SIFP, 1), word(ucos_docentes$nombre.SIFP, 3,-1)),
#                            word(ucos_docentes$nombre.SIFP, 1,-1)
#                            )
#         ) %>%
# mutate (docentes = gsub("PAPARAMBORDA", "", nombre.SIFP))

rm(h, lista2)
for (i in ucos_docentes$nombre.SIFP){
  
  if(exists("h")){
    h <- h + 1
  } else {h <- 1}
  
  j = agrep(i, docentes_cargos$nombre.web, max.distance = list(cost=0.33), ignore.case = TRUE, fixed = TRUE, useBytes= TRUE)
  if (any(j)){
    lista = data.frame(indice.SIFP = h, nombre.SIFP = i, indice.web = j, nombre.web = docentes_cargos$nombre.web[j], enlace.web.docente = docentes_cargos$enlace.web.docente[j])
  } else {
    lista = data.frame(indice.SIFP = h, nombre.SIFP = i, indice.web = NA, nombre.web = NA, enlace.web.docente = NA)
  }
  
  if(exists("lista2")){
    lista2 <- rbind(lista2, lista)
  } else {lista2 <- lista}
}
rm(h,i,j,lista)

repetidos = lista2$indice.SIFP[duplicated(lista2$indice.SIFP)]

correspondencia = data.frame(indice.SIFP = 1:length(ucos_docentes$nombre.SIFP), nombre.SIFP = ucos_docentes$nombre.SIFP)
correspondencia = lista2 %>%
  filter(!indice.SIFP %in% repetidos) %>%
  select(indice.SIFP, indice.web, nombre.web) %>%
  left_join(correspondencia, ., by="indice.SIFP")
rm(lista2, repetidos)

correspondencia = correspondencia %>%
  select (-c(indice.SIFP, indice.web))%>%
  arrange (nombre.SIFP) %>% 
  distinct()

correspondencia = correspondencia %>%
  left_join(., docentes_cargos[,1:2], by="nombre.web")

#write.csv(correspondencia, file = "diccionario.csv", row.names=FALSE)

#diccionario = read.csv("diccionario_mano.csv", header = TRUE)


#esto para completar los nombres de los docentes agregados a mano (solo les puse el índice)
# rm(lista2)
# for (i in correspondencia$indice2) {
#   lista = data.frame (indice2 = i,
#                       nombre.web = docentes_cargos$nombre.web[i],
#                       enlace.web.docente = docentes_cargos$enlace.web.docente[i])
#   if(exists("lista2")){
#     lista2 <- rbind(lista2, lista)
#   } else {lista2 <- lista}
# }
# correspondencia = correspondencia %>%
#   select(docentes) %>%
#   bind_cols(., lista2[2:3])
# rm(i, lista, lista2)

#diccionario = read.csv("diccionario.csv", header = TRUE)

#correspondencia de docentes en cada horario con el listado de docentes----

ucos_horarios_todos$indice = c(1:length(ucos_horarios_todos$Día))
ucos_docentes2 = ucos_docentes %>%
  mutate(indice = c(1:length(ucos_docentes$nombre.SIFP))) %>%
  left_join(., select (diccionario, nombre.SIFP, nombre.web, enlace.web.docente), by="nombre.SIFP")
rm(h, lista2)
for (i in ucos_listado$enlace.SIFP.ucos){
  
  grupos = ucos_horarios_todos %>%
    filter (!is.na(nombre.ficha.uco),
            enlace.SIFP.ucos == i)
  
  SIFP = ucos_docentes2 %>%
    filter (enlace.SIFP.ucos == i)
  
  for (h in grupos$indice){
    j = agrep(grupos$nombre.ficha.uco[grupos$indice == h], SIFP$nombre.web, max.distance = list(cost=0.32), ignore.case = FALSE, fixed = TRUE, useBytes= TRUE)
    
    if (any(j)){
      lista = data.frame(indice_ucos_horarios = h,
                         nombre.ficha.uco = grupos$nombre.ficha.uco[grupos$indice == h],
                         indice_ucos_docentes = SIFP$indice[j],
                         nombre.web = SIFP$nombre.web[j],
                         nombre.SIFP = SIFP$nombre.SIFP[j])
    } else {
      lista = data.frame(indice_ucos_horarios = h,
                         nombre.ficha.uco = grupos$nombre.ficha.uco[grupos$indice == h],
                         indice_ucos_docentes = NA,
                         nombre.web = NA,
                         nombre.SIFP = NA)
    }
    
    if(exists("lista2")){
      lista2 <- rbind(lista2, lista)
    } else {lista2 <- lista}
  }
}
rm(h,i,j,lista, SIFP, grupos)

repetidos = lista2$indice_ucos_horarios[duplicated(lista2$indice_ucos_horarios)]

sum(sum(is.na(lista2$nombre.web)),length(repetidos))

correspondencia2 = data.frame(indice_ucos_horarios = 1:length(ucos_horarios_todos$indice))
correspondencia2 = lista2 %>%
  filter(!indice_ucos_horarios %in% repetidos) %>%
  left_join(correspondencia2, ., by="indice_ucos_horarios") %>% 
  select(nombre.ficha.uco, nombre.SIFP, nombre.web) %>% 
  distinct() %>% 
  arrange (nombre.ficha.uco)
rm(repetidos, lista2)
#write.csv(correspondencia2, file = "diccionario2.csv", row.names=FALSE)

#diccionario2 = read.csv("diccionario2_mano.csv", header = TRUE)

diccionario2 = diccionario2 %>% 
  left_join(., docentes_cargos, by="nombre.web")
