#___________________________________________
#          ANALISIS DE DATOS
#___________________________________________

#LIBRERIAS----
library(data.table)
library(tidyverse)
library(bit64)

#LECTURA DE ARCHIVOS----
BaseDatos <- fread("C:\\Users\\nico2\\Documents\\Proyecto_CorrupcionSalud\\SECOP_I_IPS.CSV")

#ESCRITURA DE ARCHIVOS----
write.csv(BaseDatos, "C:\\Users\\nico2\\Documents\\Proyecto_CorrupcionSalud\\SECOP_I_IPS.CSV")

#Indicadores----
{
  CSubCont <- BaseDatos[, c( "id_contratista_2", "valor_inicial","valor_total")]
  CSubCont[valor_total>valor_inicial, contratos_mod:= .N, by= id_contratista_2] 
  CSubCont[,total_contratos:= .N, by= id_contratista_2]
  CSubCont[, valor_promedio_inicial:= mean(valor_inicial), by= id_contratista_2]
  CSubCont[, valor_promedio_final:= mean(valor_total), by= id_contratista_2]
  CSubCont[, P:= contratos_mod/total_contratos]
  CSubCont[, diferencia_promedio:= (valor_promedio_final- valor_promedio_inicial), 
           by = id_contratista_2]

  CSubCont[, P:= round(P, digits = 3)]
  CSubCont[,diferencia_promedio:= round(diferencia_promedio, digits = 3)]
  CSubCont <- CSubCont[order(id_contratista_2, contratos_mod, decreasing = TRUE)]
  CSubCont$valor_inicial <- NULL
  CSubCont$valor_total <- NULL
  CSubCont <- CSubCont[!duplicated(id_contratista_2)]
  CSubCont <- CSubCont[order(diferencia_promedio, P, decreasing = TRUE)]
} # 1 Indicador: Porci?n de contratos que necesitaron m?s recursos de los iniciales
{
  NlugaresCont <- BaseDatos %>% group_by(id_contratista_2) %>% 
    summarise(dist_municipios_ejecucion = n_distinct(municipio_ejecucion)) %>% 
    arrange(desc(dist_municipios_ejecucion))

} # 2 Indicador: N?mero de lugares presentes en los cuales se ejecutan los contratos por contratista
{
  NGruposCont <- BaseDatos %>% group_by(id_contratista_2) %>% summarise(dist_grupos = n_distinct(id_grupo)) %>%
    arrange(desc(dist_grupos))
  
  NFamiliaCont <- BaseDatos %>% group_by(id_contratista_2) %>% summarise(dist_famila = n_distinct(id_familia)) %>%
    arrange(desc(dist_famila))
  
} # 3 Indicador: N?mero de familias presentes en los contratos por contratistas
{
  NEntidadesCont <- BaseDatos %>% group_by(id_contratista_2) %>% summarise(dist_entidades = n_distinct(nit_entidad)) %>%
    arrange(desc(dist_entidades))
} # 4 Indicador: N?mero de entidades con las que ha contratado
{
  FrecTipoContratosCont <- BaseDatos[,table(id_contratista_2,tipo_proceso)]
} # 5 Indicador: Frecuencia de 
{
  Resumen <- merge(NGruposCont,NFamiliaCont, by.x = "id_contratista_2", by.y = "id_contratista_2") 
  Resumen <- merge(Resumen, NlugaresCont, by.x = "id_contratista_2", by.y = "id_contratista_2")
  Resumen <- merge(Resumen, CSubCont , by.x = "id_contratista_2", by.y = "id_contratista_2") 
  Resumen <- merge(Resumen, NEntidadesCont , by.x = "id_contratista_2", by.y = "id_contratista_2") %>%
    arrange(desc(dist_grupos), desc(dist_famila))
} # Resumen de los tres indicadores
#MODIFICACIONES DE LA BASE DE DATOS-----
  
  #Ordenar
  BaseDatos <- BaseDatos[order(id_contratista_2)]
  
#Revisiones de contratistas----
  # 860005114
  BaseDatos[id_contratista_2 == "811022474", table(nom_contratista_2)]
  BaseDatos[id_contratista_2 == "811022474", table(nit_entidad)]
  