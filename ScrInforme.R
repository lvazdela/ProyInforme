#________________________________________
# 22/06/2023
# Bases de datos que me dio Elías para el informe
# El objetivo de este script es analizar las dos bases de datos y ver si 
# se puede automatizar.
# ___________________________________________________

#PRUEBA DE PUSH DE GIT

library(tidyverse)
library(readxl)
dfmed <- read_excel("bases-originales/Base Medicina.xlsx")
View(dfmed)
# se observa que hay repetidos
sum(duplicated(dfmed))
sum(duplicated(dfmed[,1]))
head(dfmed$ID_Docente)

# quiero ver que diferencias tienen los duplicados
dfduplicates <- dfmed[duplicated(dfmed$ID_Docente),]
ind <- which(dfmed$ID_Docente == dfduplicates$ID_Docente[1])
df <- dfmed[ind,]
#El caso 1 de dfduplicates se repite 3 veces, por lo que se ve, es que da clases en
#otras facultades. Creo df2 que es una base de datos vacía con la estructura de dfmed
#(no hay Nas en ID_docente)
df2 <- dfmed %>%
  filter(is.na(ID_Docente))

for(i in 2:dim(dfduplicates)[1]) {
  ind <- which(dfmed$ID_Docente == dfduplicates$ID_Docente[i])
  df <- dfmed[ind,]
  df2 <- bind_rows(df2, df)
}
# se corrobora, los repetidos son porque dan clases en otras facultades.
# seleccionamos los que en la ua tienen "FACULTAD DE MEDICINA" y que estén
#activos

names(dfmed)
dfmed2 <- dfmed %>%
  filter(`PA UA` == "FACULTAD DE MEDICINA", STATUS == "A-ACTIVO")
dim(dfmed2)

categoricas <- c(4, 6,7,8, 11,12, 20, 21, 22, 23, 24, 42)

dfmed2 <- dfmed2 %>%
  mutate(across(categoricas, as.factor))
str(dfmed2)
table(dfmed2$GDO_ESTUDIOS)

#__________________
save(dfmed2, file = 'dfmed2.Rda')
#_________________

load('dfmed2.Rda')
library(forcats) #para usar fct_infreq()


#Ver "Annotate ggplot bar plot in R" en la carpeta ggplot de Zotero para 
#que salga la cantidad en números arriba de la barra y 
#"Answer to "Order Bars in ggplot2 bar graph" para ordenar las barras
dfmed2 %>%
  ggplot(aes(fct_infreq(GDO_ESTUDIOS))) +
  geom_bar(fill = "olivedrab2", color = "darkgreen") +
  geom_text(stat = "count", aes(label = after_stat(count)), nudge_y = 5) +
  scale_x_discrete(labels = c("0.0" = "Sin información",
                              "DOCTOR" = "Doctorado",
                              "ESPECIALISTA" = "Especialidad",
                              "LICENCIADO" = "Licenciatura",
                              "MAESTRO" = "Maestría",
                              "TECNICO" = "Técnico(a)")) +
  labs(x = "Grado de estudios",
       y = "Cantidad de profesores")

#Histograma de edad:
dfmed2 %>%
  ggplot(aes(EDAD)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 5, fill = "olivedrab3", color = "darkgreen")+
  #geom_area(stat = 'bin')+
  geom_density(color = 'red', linewidth = 1)+
  labs(x = "Edad de los académicos",
       y = "Densidad")

names(dfmed2)
table(dfmed2$DESC_DIST)
table(dfmed2$DESCRIPCION)
# dfmed2 <- dfmed2 %>%
#   mutate(jornada = case_when(str_detect(DESCRIPCION, "HC") & !str_starts(DESCRIPCION, "PI") ~ "Hora clase",
#                            str_detect(DESCRIPCION, "MT") & !str_detect("HC") ~ "Medio tiempo",
#                            str_detect(DESCRIPCION, "MT") & str_starts(DESCRIPCION, "HC") ~ "Medio tiempo y hora clase",
#                            str_detect(DESCRIPCION, "TC") ~ "Tiempo completo"))
# table(dfmed2$jornada)

prueba <- "MT"
#detectar medio tiempo más hora clase
str_detect('PI ASOCIADO B MT PR ASIGNATURA HC 06', "PI.+MT.+HC.+")
str_detect('PI ASOCIADO B MT PR ASIGNATURA HC 06', paste0("PI.+", prueba, ".+HC.+"))
#detectar hora clase sin medio tiempo
str_detect('PI ASOCIADO B MT PR ASIGNATURA HC 06', "HC") & !str_detect('PI ASOCIADO B MT PR ASIGNATURA HC 06', "PI.+MT") 

#detectar medio tiempo sin hora clase
str_detect('PI ASOCIADO B MT PR ASIGNATURA HC 06 ', "PI.+MT") & !str_detect('PI ASOCIADO B MT PR ASIGNATURA HC 06', "HC") 

#detectar PIs de tiempo completo 
str_detect('PI ASOCIADO B TC', "PI.+TC")

dfmed2 <- dfmed2 %>%
  mutate(jornada = case_when(str_detect(DESCRIPCION, "HC") & !str_detect(DESCRIPCION, "PI.+MT") ~ "HC",
                             str_detect(DESCRIPCION, "PI.+MT") & !str_detect(DESCRIPCION,"HC") ~ "MT",
                             str_detect(DESCRIPCION, "PI.+MT.+HC.+") ~ "PI MT y HC",
                             str_detect(DESCRIPCION, "PI.+TC") ~ "PI TC",
                             str_starts(DESCRIPCION, "TA.+ MT") ~ "TA MT",
                             str_starts(DESCRIPCION, "TA.+ TC") ~ "TA TC",
                             str_detect(DESCRIPCION, "CATEDRA") ~ "Cátedra",
                             TRUE ~  "Administrativo")) %>%
  mutate(jornada = as.factor(jornada))
table(dfmed2$jornada)
sum(table(dfmed2$jornada))

dfmed2 %>%
  group_by(jornada) %>%
  summarise(n = n()) %>%
  mutate(prop = round(n/sum(n)*100,1)) %>%
  mutate(jornada = factor(jornada)) %>%
  ggplot(aes(x = reorder(jornada, -n), y = n)) +
  geom_col(fill = "olivedrab2", color = "darkgreen") +
  geom_text(aes(y = n, label = paste(prop, "%")), nudge_y = 5,size = 3) +
  geom_text(aes(y = n, label = n),nudge_y = 12, size = 3)+
  labs(x = "Tipo de jornada y categoría",
       y = "Número de profesores")
 
#EJEMPLO DE LA PÁGINA, SI FUNCIONA, HAY QUE INSTALAR ggstats, funciona para dos variables,
# la de fill es la que se reporta como proporción de la x
# d <- as.data.frame(Titanic)
# ggplot(d) +
#   aes(x = Class, fill = Sex, weight = Freq, by = Sex, y = after_stat(prop)) +
#   geom_bar(stat = "prop", position = "dodge") +
#   scale_y_continuous(labels = scales::percent) +
#   geom_text(
#     mapping = aes(
#       label = scales::percent(after_stat(prop), accuracy = .1),
#       y = after_stat(0.01)
#     ),
#     vjust = "bottom",
#     position = position_dodge(.9),
#     stat = "prop"
#   )
# install.packages('ggstats')
dftc <- dfmed2 %>%
  filter(jornada == "Tiempo Completo", GDO_ESTUDIOS == "ESPECIALISTA") %>%
  select(ID_Docente, NOMBRE, EDAD, `Horas a impartir`, `suma horas`, `Observación`)

table(dfmed2$Grupo...10)
df1 <- dfmed2 %>%
  filter(jornada == "Hora clase") %>%
  select(NOMBRE)
df2 <- dfmed2 %>%
  filter(Grupo...10 == "PHC") %>%
  select(NOMBRE)
setdiff(df1, df2)
setdiff(df2, df1)

dfmed2 %>%
  filter(Grupo...10 == "TA TC") %>%
  select(NOMBRE)

dfmed2 %>%
  filter(Grupo...10 == "TA MT") %>%
  select(NOMBRE)
dfmed2 %>%
  filter(str_detect(NOMBRE, "ELBA"))
which(str_detect(dfmed2$NOMBRE, "ARCEGA"))

which(str_detect(dfmed2$NOMBRE, "GEORGE"))

