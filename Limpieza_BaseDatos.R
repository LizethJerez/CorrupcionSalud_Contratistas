#___________________________________________
#          LIMPIEZA DE BASE DE DATOS
#___________________________________________

#LIBRERIAS----
library(data.table)
library(tidyverse)
library(bit64)
library(lubridate)
library(plotly)
library(DT)
#LECTURA DE ARCHIVOS----
BaseDatos <- fread("C:\\Users\\nico2\\Documents\\Proyecto_Corrupci?nSalud\\SECOP_I_IPS.CSV")
BaseDatos <- fread("C:\\Users\\nico2\\Documents\\Proyecto_Corrupci?nSalud\\LimpiezaDatos\\SECOP_I_IPS.CSV",
                   encoding = "UTF-8")   #GENERAL

#ESCRITURA DE ARCHIVOS----
write.csv(BaseDatos, "C:\\Users\\nico2\\Documents\\Proyecto_Corrupci?nSalud\\SECOP_I_IPS.CSV")

#INFORMACI?N DE LOS DATOS----
{
#busqueda del documento 8000065831
BaseDatos[grep("8000065831",BaseDatos$id_contratista), 
          c("TipoInden_Contratista","Identificacion del Contratista", "id_contratista", "Nom_Contratista")] 

#I. Atributos----
  # Un '*' significa que el atributo fue reemplazado para mejorar su lectura
  
  {
    BaseDatos[c(101:200), "uid"]
    sum(BaseDatos$uid=="No Definido") #No hay valores con "No Definido"
    sum(is.na(BaseDatos$uid)) # No hay valores nulos
  } #1. C?digo uid
  { 
    BaseDatos[c(1:15), "a?o_cargue"]
    sum(BaseDatos$a?o_cargue=="No Definido") #No hay valores con "No Definido"
    sum(is.na(BaseDatos$a?o_cargue)) # No hay valores nulos
    table(BaseDatos$a?o_cargue)
  } #2. A?o de cargue en SECOP
  {
    BaseDatos[c(1:15), "a?o_firma"]
    sum(BaseDatos$a?o_firma=="No Definido") #No hay valores con "No Definido"
    sum(is.na(BaseDatos$a?o_firma)) # No hay valores nulos
    table(BaseDatos$a?o_firma)
  } #3. A?o del firma del contrato
  {
    BaseDatos[c(1:15),"nivel_entidad"]
    sum(BaseDatos$nivel_entidad=="No Definido") #No hay valores con "No Definido"
    sum(is.na(BaseDatos$nivel_entidad)) # No hay valores nulos
    table(BaseDatos$nivel_entidad)
  --#65. nivel_entidad_2
      BaseDatos[c(1:15),"nivel_entidad_2"]
      table(BaseDatos$nivel_entidad_2)
      str(BaseDatos$nivel_entidad_2)
  } #4. nivel_entidad*
  {
    BaseDatos[c(1:15), "orden_entidad"]
    sum(BaseDatos$orden_entidad=="No Definido") #No hay valores con "No Definido"
    sum(is.na(BaseDatos$orden_entidad)) # No hay valores nulos
    table(BaseDatos$orden_entidad, BaseDatos$orden_entidad_2)
    BaseDatos[, orden_entidad_2]
  } #5. orden_entidad
  {
    BaseDatos[c(1:15), "nombre_entidad"]
    sum(BaseDatos$nombre_entidad=="No Definido") #No hay valores con "No Definido"
    sum(is.na(BaseDatos$nombre_entidad)) # No hay valores nulos
    table(BaseDatos$nombre_entidad)
  } #6. Nombre de la entidadd
  {
    BaseDatos[c(1:15), "nit_entidad"]
    sum(BaseDatos$nit_entidad=="No Definido") #No hay valores con "No Definido"
    sum(is.na(BaseDatos$nit_entidad)) # No hay valores nulos
  } #7. nit_entidad
  {
    BaseDatos[c(1:15), "cod_entidad"]
    sum(BaseDatos$cod_entidad=="No Definido") #No hay valores con "No Definido"
    sum(is.na(BaseDatos$cod_entidad)) # No hay valores nulos
  } #8. cod_entidad
  {
    BaseDatos[c(1:15), "id_tipo_proceso"]
    sum(BaseDatos$id_tipo_proceso=="No Definido") #No hay valores con "No Definido"
    sum(is.na(BaseDatos$id_tipo_proceso)) # No hay valores nulos
    table(BaseDatos$id_tipo_proceso)
    str(BaseDatos$id_tipo_proceso)
  } #9. id_tipo_proceso
  {
    BaseDatos[c(1:15), "tipo_proceso"]
    sum(BaseDatos$tipo_proceso=="No Definido") #No hay valores con "No Definido"
    sum(is.na(BaseDatos$tipo_proceso)) # No hay valores nulos
    table(BaseDatos$tipo_proceso)
    str(BaseDatos$tipo_proceso)
  } #10. tipo_proceso
  {
    BaseDatos[c(1:15), "estado_proceso"]
    sum(BaseDatos$estado_proceso=="No definido") #No hay valores con "No Definido"
    sum(is.na(BaseDatos$estado_proceso)) # No hay valores nulos
    table(BaseDatos$estado_proceso)
    str(BaseDatos$estado_proceso)
  } #11. estado_proceso
  {
    BaseDatos[c(1:15), "causal_cont_direct"]
    sum(BaseDatos$causal_cont_direct=="Ninguna") #No hay valores con "Ninguna"
    sum(is.na(BaseDatos$causal_cont_direct)) # No hay valores nulos
  } #12. Causal de Otras Formas de Contratacion Directa
  {
    BaseDatos[c(1:15), "id_reg_contrat"]
    sum(BaseDatos$id_reg_contrat=="No Definido") #No hay valores con "No Definido"
    sum(is.na(BaseDatos$id_reg_contrat)) # No hay valores nulos
    table(BaseDatos$id_reg_contrat)
    str(BaseDatos$id_reg_contrat)
  } #13. ID Regimen de Contratacion
  {
    BaseDatos[c(1:15), "regimen_contratacion"]
    sum(BaseDatos$regimen_contratacion=="No Definido") #No hay valores con "No Definido"
    sum(is.na(BaseDatos$regimen_contratacion)) # No hay valores nulos
    table(BaseDatos$regimen_contratacion)
    str(BaseDatos$regimen_contratacion)
  } #14. Regimen de Contratacion
  {
    BaseDatos[c(1:15), "id_objeto"]
    sum(BaseDatos$id_objeto=="No definido") #No hay valores con "No Definido"
    sum(is.na(BaseDatos$id_objeto)) # No hay valores nulos
    table(BaseDatos$id_objeto)
    str(BaseDatos$id_objeto)
  } #15. ID Objeto a Contratar
  {
    BaseDatos[c(1:15), "objeto_contratar"]
    sum(BaseDatos$objeto_contratar=="No definido") #No hay valores con "No Definido"
    sum(is.na(BaseDatos$objeto_contratar)) # No hay valores nulos
    table(BaseDatos$objeto_contratar)
    str(BaseDatos$objeto_contratar)
  } #16. Objeto a Contratar
  {
    BaseDatos[c(1:50), "detalle_objeto"]
    sum(BaseDatos$detalle_objeto=="No Definido") #No hay valores con "No Definido"
    sum(is.na(BaseDatos$detalle_objeto)) # No hay valores nulos
  } #17. Detalle del Objeto a Contratar
  {
    BaseDatos[c(1:15), "tipo_contrato"]
    sum(BaseDatos$tipo_contrato=="No definido") #No hay valores con "No Definido"
    sum(is.na(BaseDatos$tipo_contrato)) # No hay valores nulos
    table(BaseDatos$tipo_contrato)
    str(BaseDatos$tipo_contrato)
  } #18. Tipo de Contrato
  {
    BaseDatos[c(1:15), "municipio_obtencion"]
    sum(BaseDatos$municipio_obtencion=="No definido") #No hay valores con "No Definido"
    sum(is.na(BaseDatos$municipio_obtencion)) # No hay valores nulos
    table(BaseDatos$municipio_obtencion)
  } #19. Municipio Obtencion
  {
    BaseDatos[c(1:15), "municipio_entrega"]
    sum(BaseDatos$municipio_entrega=="No definido") #No hay valores con "No Definido"
    sum(is.na(BaseDatos$municipio_entrega)) # No hay valores nulos
    table(BaseDatos$municipio_entrega)
  } #20. Municipio Entrega
  {
      BaseDatos[c(1:15), "municipio_ejecucion"]
      sum(BaseDatos$municipio_ejecucion=="No definido") #No hay valores con "No Definido"
      sum(is.na(BaseDatos$municipio_ejecucion)) # No hay valores nulos
      table(BaseDatos$municipio_ejecucion)
  } #21. Municipios Ejecucion
  {
    BaseDatos[c(1:15), "fecha_cargue"]
    sum(BaseDatos$fecha_cargue=="No Definido") #No hay valores con "No Definido"
    sum(is.na(BaseDatos$fecha_cargue)) # No hay valores nulos
  } #22. Fecha de Cargue en el SECOP
  {
    BaseDatos[c(1:15), "numero_constancia"]
    sum(BaseDatos$numero_constancia=="No Definido") #No hay valores con "No Definido"
    sum(is.na(BaseDatos$numero_constancia)) # No hay valores nulos
  } #23. Numero de Constancia
  {
    BaseDatos[c(1:15), "numero_proceso"]
    sum(BaseDatos$numero_proceso=="No Definido") #No hay valores con "No Definido"
    sum(is.na(BaseDatos$numero_proceso)) # No hay valores nulos
   } #24. Numero de Proceso
  {
    BaseDatos[c(1:15), "numero_contrato"]
    sum(BaseDatos$numero_contrato=="No definido") #No hay valores con "No Definido"
    sum(is.na(BaseDatos$numero_contrato)) # No hay valores nulos
   } #25. Numero del Contrato
  {
    BaseDatos[c(1:15), "valor_estimado"]
    sum(BaseDatos$valor_estimado=="No Definido") #No hay valores con "No Definido"
    sum(is.na(BaseDatos$valor_estimado)) # No hay valores nulos
   } #26. Cuantia Proceso o valor_estimado
  {
    BaseDatos[c(1:15), "id_grupo"]
    sum(BaseDatos$id_grupo=="0") #No hay valores con "No Definido"
    sum(is.na(BaseDatos$id_grupo)) # No hay valores nulos
    table(BaseDatos$id_grupo)
   } #27. ID Grupo
  {
    BaseDatos[c(1:15), "nombre_grupo"]
    sum(BaseDatos$nombre_grupo=="No Definido") #No hay valores con "No Definido"
    sum(is.na(BaseDatos$nombre_grupo)) # No hay valores nulos
    table(BaseDatos$nombre_grupo)
   } #28. Nombre Grupo
  {
    BaseDatos[c(1:15), "id_familia"]
    sum(BaseDatos$id_familia=="0") #No hay valores con "No Definido"
    sum(is.na(BaseDatos$id_familia)) # No hay valores nulos
    table(BaseDatos$id_familia)
   } #29. ID Familia
  {
    BaseDatos[c(1:15), "nombre_familia"]
    sum(BaseDatos$nombre_familia=="No Definido") #No hay valores con "No Definido"
    sum(is.na(BaseDatos$nombre_familia)) # No hay valores nulos
    table(BaseDatos$nombre_familia)
   } #30. Nombre Familia
  {
    BaseDatos[c(1:15), "id_clase"]
    sum(BaseDatos$id_clase=="0") #No hay valores con "No Definido"
    sum(is.na(BaseDatos$id_clase)) # No hay valores nulos
    table(BaseDatos$id_clase)
   } #31. ID Clase
  {
    BaseDatos[c(1:15), "nombre_clase"]
    sum(BaseDatos$nombre_clase=="No Definido") #No hay valores con "No Definido"
    sum(is.na(BaseDatos$nombre_clase)) # No hay valores nulos
    table(BaseDatos$nombre_clase)
  } #32. Nombre Clase
  {
    BaseDatos[c(1:15), "id_adjudicacion"]
    sum(BaseDatos$id_adjudicacion=="No definido") #No hay valores con "No Definido"
    sum(is.na(BaseDatos$id_adjudicacion)) # No hay valores nulos
  } #33. ID Ajudicacion
  {
  --#62. TipoInden_Contratista
      BaseDatos[c(1:15), "TipoInden_Contratista"]
      sum(is.na(BaseDatos$TipoInden_Contratista)) # No hay valores nulos
      table(BaseDatos$TipoInden_Contratista)
  } #34. Tipo Identifi del Contratista*
  {
    BaseDatos[c(954686:954750),c("TipoInden_Contratista","Identificacion del Contratista","id_contratista")]
  --#63. Inden_Contratista    
      BaseDatos[c(1:100),id_contratista]
      sum(is.na(BaseDatos$id_contratista)) # No hay valores nulos
  } #35. Identificacion del Contratista*
  {
  --#64. nom_contratista
      BaseDatos[c(1:15), nom_contratista]
      sum(is.na(BaseDatos$nom_contratista)) # No hay valores nulos
      table(BaseDatos$nom_contratista)
   } #36. Nom Raz Social Contratista 
  {
      BaseDatos[c(1:15), "departamento_contratista"]
      sum(BaseDatos$departamento_contratista=="No Definido") #No hay valores con "No Definido"
      sum(is.na(BaseDatos$departamento_contratista)) # No hay valores nulos
  } #37. Dpto y Muni Contratista
  {
      BaseDatos[c(1:15), "tipo_id_rep_legal"]
      sum(BaseDatos$tipo_id_rep_legal=="No Definido") #No hay valores con "No Definido"
      sum(is.na(BaseDatos$tipo_id_rep_legal)) # No hay valores nulos
      table(BaseDatos$tipo_id_rep_legal)
   } #38. Tipo Doc Representante Legal
  {
      BaseDatos[c(1:15), "id_rep_legal"]
      sum(BaseDatos$id_rep_legal=="No Definido") #No hay valores con "No Definido"
      sum(is.na(BaseDatos$id_rep_legal)) # No hay valores nulos
  } #39. Identific del Represen Legal
  {
      BaseDatos[c(1:15), "nombre_rep_legal"]
      sum(BaseDatos$nombre_rep_legal=="No Definido") #No hay valores con "No Definido"
      sum(is.na(BaseDatos$nombre_rep_legal)) # No hay valores nulos
  } #40. Nombre del Represen Legal  
  {
      BaseDatos[c(1:15), "fecha_firma"]
      sum(BaseDatos$fecha_firma=="") #No hay valores con "No Definido"
      sum(is.na(BaseDatos$fecha_firma)) # No hay valores nulos
   } #41. Fecha de Firma del Contrato
  {
      BaseDatos[c(1:15), "fecha_ini_ejec"]
      sum(BaseDatos$fecha_ini_ejec=="") #No hay valores con "No Definido"
      sum(is.na(BaseDatos$fecha_ini_ejec)) # No hay valores nulos
   } #42. Fecha Ini Ejec Contrato 
  {
      BaseDatos[c(1:15), "plazo_ejec"]
      sum(BaseDatos$plazo_ejec=="0") #No hay valores con "No Definido"
      sum(is.na(BaseDatos$plazo_ejec)) # No hay valores nulos
   } #43. Plazo de Ejec del Contrato
  {
      BaseDatos[c(1:15), "rango_ejec"]
      sum(BaseDatos$rango_ejec=="0") #No hay valores con "No Definido"
      sum(is.na(BaseDatos$rango_ejec)) # No hay valores nulos
      BaseDatos[, table(rango_ejec)]
   } #44. Rango de Ejec del Contrato
  {
      BaseDatos[c(1:15), "adiciones_dias"]
      sum(BaseDatos$adiciones_dias=="0") #No hay valores con "No Definido"
      sum(is.na(BaseDatos$adiciones_dias)) # No hay valores nulos
      BaseDatos[, table(adiciones_dias)]
      {
        BaseDatos[adiciones_dias == 6340, plazo_ejec:= 141]
        BaseDatos[adiciones_dias == 6340, adiciones_dias:= 60]
      }#Registro especial
  } #45. Tiempo Adiciones en Dias
  {
      BaseDatos[c(1:15), "adiciones_meses"]
      sum(BaseDatos$adiciones_meses=="0") #No hay valores con "No Definido"
      sum(is.na(BaseDatos$adiciones_meses)) # No hay valores nulos
      BaseDatos[, table(adiciones_meses)]
      {
        BaseDatos[adiciones_meses == 642]
        BaseDatos[adiciones_dias == 6340, adiciones_dias:= 60]
      }#Registro especial
  } #46. Tiempo Adiciones en Meses
  {
      BaseDatos[c(1:15), "fecha_fin_ejec"]
      sum(BaseDatos$fecha_fin_ejec=="") #No hay valores con "No Definido"
      sum(is.na(BaseDatos$fecha_fin_ejec)) # No hay valores nulos
   } #47. Fecha Fin Ejec Contrato
  {
      BaseDatos[c(1:15), "compromiso_presupuestal"]
      sum(BaseDatos$compromiso_presupuestal=="No registra") #No hay valores con "No Definido"
      sum(is.na(BaseDatos$compromiso_presupuestal)) # No hay valores nulos
      BaseDatos[, table(compromiso_presupuestal)]
   } #48. Compromiso Presupuestal
  {
      BaseDatos[c(1:15), "valor_inicial"]
      sum(is.na(BaseDatos$valor_inicial)) # No hay valores nulos
      BaseDatos[moneda=="Pesos (COP)" & valor_inicial<10000, c("valor_inicial", "valor_total")]
   } #49. Cuantia Contrato (valor_inicial)
  {
      BaseDatos[c(1:15), "valor_adiciones"]
      sum(is.na(BaseDatos$valor_adiciones)) # No hay valores nulos
  } #50. Valor Total de Adiciones  
  {
      BaseDatos[c(1:15), "valor_total"]
      sum(is.na(BaseDatos$valor_total)) # No hay valores nulos
   } #51. Valor Contrato con Adiciones
  {
      BaseDatos[c(1:15), "objeto_contrato_firma"]
      sum(BaseDatos$objeto_contrato_firma=="No Definido") #No hay valores con "No Definido"
      sum(is.na(BaseDatos$objeto_contrato_firma)) # No hay valores nulos
   } #52. Objeto del Contrato a la Firma
  {
      BaseDatos[c(1:15), "id_origen_recursos"]
      sum(BaseDatos$id_origen_recursos=="0") #No hay valores con "No Definido"
      sum(is.na(BaseDatos$id_origen_recursos)) # No hay valores nulos
      BaseDatos[, table(id_origen_recursos)]
   } #53. ID Origen de los Recursos
  {
      BaseDatos[c(1:15), "origen_recursos"]
      sum(BaseDatos$origen_recursos=="No definido") #No hay valores con "No Definido"
      sum(is.na(BaseDatos$origen_recursos)) # No hay valores nulos
      BaseDatos[, table(origen_recursos)]
   } #54. Origen de los Recursos
  {
      BaseDatos[c(1:15), "codigo_bpin"]
      sum(BaseDatos$codigo_bpin=="") #No hay valores con "No Definido"
      sum(is.na(BaseDatos$codigo_bpin)) # No hay valores nulos
      BaseDatos[, table(codigo_bpin)]
   } #55. Codigo BPIN
  {
      BaseDatos[c(1:15), "proponentes_selecc"]
      sum(BaseDatos$proponentes_selecc=="No definido") #No hay valores con "No Definido"
      sum(is.na(BaseDatos$proponentes_selecc)) # No hay valores nulos
      BaseDatos[proponentes_selecc!="No definido",]
   } #56. Proponentes Seleccionados
  {
      BaseDatos[c(1:15), "calificacion_definitiva"]
      sum(BaseDatos$calificacion_definitiva=="No definido") #No hay valores con "No Definido"
      sum(is.na(BaseDatos$calificacion_definitiva)) # No hay valores nulos
      BaseDatos[calificacion_definitiva!="No definido",]
   } #57. Calificacion Definitiva
  {
      BaseDatos[c(1:15), "id_sub_unid_ejec"]
      sum(BaseDatos$id_sub_unid_ejec=="0") #No hay valores con "No Definido"
      sum(is.na(BaseDatos$id_sub_unid_ejec)) # No hay valores nulos
   } #58. ID Sub Unidad Ejecutora
  {
      BaseDatos[c(1:15), "nombre_sub_unid_ejec"]
      sum(BaseDatos$nombre_sub_unid_ejec=="No definida") #No hay valores con "No Definido"
      sum(is.na(BaseDatos$nombre_sub_unid_ejec)) # No hay valores nulos
  } #59. Nombre Sub Unidad Ejecutora
  {
      BaseDatos[c(1:15), "moneda"]
      sum(BaseDatos$moneda=="No Definida") #No hay valores con "No Definido"
      BaseDatos[, table(moneda)]
   } #60. Moneda
  {
      BaseDatos[c(1:15), "post_conflicto"]
      sum(BaseDatos$post_conflicto=="No Definido") #No hay valores con "No Definido"
      BaseDatos[, table(post_conflicto)]
   } #61. EsPostConflicto
  {
      BaseDatos[c(1:15), "ruta_web"]
      sum(BaseDatos$ruta_web=="") #No hay valores con "No Definido"
   } #62. Ruta Proceso en SECOP I
  {
    summary(BaseDatos$plazo_ejec_calc)
    sum(BaseDatos$plazo_ejec_calc >= (365*4))
  }#--67. plazo_ejec_calc
  
#II. Elementos generales de la tabla----
  str(BaseDatos)
  BaseDatos[c(955793)]
  tables()
}

#MODIFICACIONES DE LA BASE DE DATOS-----

#Ordenar
  BaseDatos <- BaseDatos[order(id_contratista)]

#General

  BaseDatos <- distinct(BaseDatos)
  nombres = c("uid","a?o_cargue","a?o_firma","nivel_entidad","orden_entidad",
            "nombre_entidad","nit_entidad","cod_entidad","id_tipo_proceso",
            "tipo_proceso","estado_proceso","causal_cont_direct","id_reg_contrat",
            "regimen_contratacion","id_objeto","objeto_contratar","detalle_objeto",
            "tipo_contrato","municipio_obtencion","municipio_entrega",
            "municipio_ejecucion","fecha_cargue","numero_constancia","numero_proceso",
            "numero_contrato","valor_estimado","id_grupo","nombre_grupo","id_familia",
            "nombre_familia","id_clase","nombre_clase","id_adjudicacion",
            "id_contratista","nom_contratista","departamento_contratista",
            "tipo_id_rep_legal","id_rep_legal","nombre_rep_legal","fecha_firma",
            "fecha_ini_ejec","plazo_ejec","rango_ejec","adiciones_dias","adiciones_meses",
            "fecha_fin_ejec","compromiso_presupuestal","valor_inicial","valor_adiciones",
            "valor_total","objeto_contrato_firma","id_origen_recursos","origen_recursos",
            "codigo_bpin","proponentes_selecc","calificacion_definitiva","id_sub_unid_ejec",
            "nombre_sub_unid_ejec","moneda","post_conflicto","ruta_web", "nivel_entidad_2",
            "orden_entidad_2","tipo_id_contratista_2", "id_contratista_2","nom_contratista_2",
            "plazo_ejec_calc", "plazo_ejec_dias")
  names(BaseDatos) <- nombres



#Conversi?n de variables
  
  #2. a?o_cargue
  #Se retiran los registros que presenten que el a?o del cargue es mayor al a?o de firma
  BaseDatos <- BaseDatos %>% filter(!(a?o_cargue > a?o_firma))
  
  #Eliminaci?n de los datos anteriores al a?o 2014 y los que no contienen la mayoria de datos
  BaseDatos <- BaseDatos[ a?o_cargue >= 2014 &
                          a?o_firma >= 2014]
  
  #3. a?o_firma
  #Se retiran los registros que presenten a?o de forma nulo
  BaseDatos <- BaseDatos %>% filter(!(is.na(a?o_firma)))
  
  #4. nivel_entidad
    BaseDatos[, nivel_entidad_2:= ifelse(nivel_entidad== "NACIONAL", "N", "T")]
    BaseDatos[, nivel_entidad_2:= factor(nivel_entidad_2)]
  
  #5. orden_entidad
   for (i in 1:1105606){
     BaseDatos[i, orden_entidad_2:= switch(orden_entidad, 
                                     "NACIONAL CENTRALIZADO" = "NC",
                                     "NACIONAL DESCENTRALIZADO" = "ND",
                                     "TERRITORIAL DEPARTAMENTAL CENTRALIZADO" = "TDC",
                                     "TERRITORIAL DEPARTAMENTAL DESCENTRALIZADO" = "TDD",
                                     "TERRITORIAL DISTRITAL MUNICIPAL NIVEL 1" = "TDMN1",
                                     "TERRITORIAL DISTRITAL MUNICIPAL NIVEL 2" = "TDMN2",
                                     "TERRITORIAL DISTRITAL MUNICIPAL NIVEL 3" = "TDMN3",
                                     "TERRITORIAL DISTRITAL MUNICIPAL NIVEL 4" = "TDMN4",
                                     "TERRITORIAL DISTRITAL MUNICIPAL NIVEL 5" = "TDMN5",
                                     "TERRITORIAL DISTRITAL MUNICIPAL NIVEL 6" = "TDMN6")]}
    BaseDatos[, orden_entidad_2:= factor(orden_entidad_2)]
    
  #9. id Tipo de proceso
    BaseDatos[ , id_tipo_proceso:= factor(id_tipo_proceso)]
    
  #10. Tipo de Proceso
    BaseDatos[ , Tipo de Proceso:= factor(Tipo de Proceso)]
  
  #11. Estado del Proceso
    BaseDatos[ , estado_proceso:= factor(estado_proceso)]
    
  #13. ID Regimen de Contratacion
    BaseDatos[ , id_reg_contrat:= factor(id_reg_contrat)]
  
  #14. Regimen de Contratacion
    BaseDatos[ , regimen_contratacion:= factor(regimen_contratacion)]
  
  #15. ID Objeto a Contratar
    BaseDatos[ , id_objeto:= factor(id_objeto)]
    
  #16. Objeto a Contratar
    BaseDatos[ , objeto_contratar:= factor(objeto_contratar)]
  
  #17. detalle_objeto
    # Se retiran los datos que presenten la observaci?n de anuar esfuerzos o similares
    BaseDatos <-  BaseDatos %>%
      filter(!(str_detect(str_to_lower(detalle_objeto),"aunar esfuerzos") |
                 str_detect(str_to_lower(detalle_objeto),"anuar esfuerzos") |
                 str_detect(str_to_lower(detalle_objeto),"aunar espuerzos")))
    # Se retiran los datos que presenten la observaci?n de empr?stitos
    BaseDatos <-  BaseDatos %>%
      filter(!(str_detect(str_to_lower(detalle_objeto),"empr?stito") |
                 str_detect(str_to_lower(detalle_objeto),"emprestito") |
                 str_detect(str_to_lower(detalle_objeto),"empresito")))
  
  #18. tipo_contrato
  # Se retiran los contratos de tipo 'cr?dito', 'Fiducia', 'Comodato' y 'Arrendamiento'
    BaseDatos$tipo_contrato <- as.factor(BaseDatos$tipo_contrato)
    BaseDatos <- BaseDatos %>% 
      filter(!(tipo_contrato == "Cr?dito" | tipo_contrato == "Fiducia" | 
                 tipo_contrato == "Comodato" | tipo_contrato == "Arrendamiento"))
  
  #26. ID Grupo
    BaseDatos[ , id_grupo:= as.character(id_grupo)]
    
  #27. Nombre Grupo
    BaseDatos[ , nombre_grupo:= factor(nombre_grupo)]
    
  #34. Tipo Identifi del Contratista*
    BaseDatos[,TipoInden_Contratista := ifelse(Tipo Identifi del Contratista == "No Definido",NA,
                                               Tipo Identifi del Contratista)] #NA
    BaseDatos[ , TipoInden_Contratista:= factor(TipoInden_Contratista)]
    
  #35. Identificacion del Contratista
    BaseDatos[,id_contratista:= ifelse(Identificacion del Contratista=="No Definido",NA,
                                          Identificacion del Contratista)]
    BaseDatos[, id_contratista:= gsub(":","", BaseDatos$id_contratista)]
    BaseDatos[, id_contratista:= gsub("\\$","", BaseDatos$id_contratista)]
    BaseDatos[, id_contratista:= gsub("\\.","", BaseDatos$id_contratista)]
    BaseDatos[, id_contratista:= gsub("?","", BaseDatos$id_contratista)]
    BaseDatos[, id_contratista:= gsub("?","", BaseDatos$id_contratista)]
    BaseDatos[, id_contratista:= gsub("?","", BaseDatos$id_contratista)]
    BaseDatos[, id_contratista:= gsub("?","", BaseDatos$id_contratista)]
    BaseDatos[, id_contratista:= gsub("/","", BaseDatos$id_contratista)]
    BaseDatos[, id_contratista:= gsub("|","", BaseDatos$id_contratista)]
    BaseDatos[, id_contratista:= as.numeric(id_contratista)]
    
    #??AVISO!! Primero se necesita ordenar los datos por Iden_Contratista y tranformarlos en caracteres
    BaseDatos <- BaseDatos[order(id_contratista)]
    BaseDatos[, id_contratista:= as.character(id_contratista)]
    BaseDatos[c(955677:955793),id_contratista:= ifelse(Identificacion del Contratista=="No Definido",NA,
                                          Identificacion del Contratista)]
    
    for (j in 835590:955793){
      if(!is.na(BaseDatos[j, id_contratista])){
        
        x <- BaseDatos[j, strsplit(id_contratista, split = '-') ]
        y <- limpiarNumero(x)
      
        if (y == "Invalido"){
          x <- BaseDatos[j, strsplit(id_contratista, split = ' ') ]
          y <- limpiarNumero(x)
        }
        if (y == "Invalido"){
          x <- BaseDatos[j, strsplit(id_contratista, split = 'd') ]
          y <- limpiarNumero(x)
        }
        if (y == "Invalido"){
          x <- BaseDatos[j, strsplit(id_contratista, split = 'D') ]
          y <- limpiarNumero(x)
        }
        BaseDatos[j, id_contratista:= y]
      }
    }
    
    #El ?ltimo grupo que tiene probemas es porque hay espacios entre los n?meros
    BaseDatos[, id_contratista:= gsub(" ","", BaseDatos$id_contratista)]
    BaseDatos[, id_contratista:= gsub("-","", BaseDatos$id_contratista)]
    
    # Resmen de los valores da?ados
    valorresIncorrectos <- BaseDatos[grep("Invalido",BaseDatos$id_contratista), 
              c( "Identificacion del Contratista", "nom_contratista")]
    
    valorresIncorrectos <- BaseDatos[c(955677:955793),
                                     c("TipoInden_Contratista","Identificacion del Contratista", "id_contratista")]
    BaseDatos[is.na(id_contratista), sum(valor_total)]

    
  #36. Nom Raz Social Contratista
    BaseDatos[,nom_contratista:= ifelse(Nom Raz Social Contratista=="No Definido",NA,
                                        Nom Raz Social Contratista)]
    
  #38. Tipo Doc Representante Legal
    BaseDatos[ , tipo_id_rep_legal:= factor(tipo_id_rep_legal)]
  
  #42. fecha_ini_ejec
    BaseDatos$fecha_ini_ejec <- dmy(BaseDatos$fecha_ini_ejec)
  
  #43. plazo_ejec
    BaseDatos$plazo_ejec <- as.integer(BaseDatos$plazo_ejec)
  
  #44. Rango de Ejec del Contrato
    BaseDatos[ , rango_ejec:= factor(rango_ejec)]
    
  #47. fecha_fin_ejec
    BaseDatos$fecha_fin_ejec <- dmy(BaseDatos$fecha_fin_ejec)
    
  #53. ID Origen de los Recursos
    BaseDatos[ , id_origen_recursos:= factor(id_origen_recursos)]
    
  #54. Origen de los Recursos
    BaseDatos[ , origen_recursos:= factor(origen_recursos)]
  
  #60. moneda
    
    #Eliminaci?n de los datos anteriores al a?o 2014 y los que no contienen la mayoria de datos
    BaseDatos <- BaseDatos[moneda %in% c("Pesos (COP)", "D?lares (US)")]
    # Adicionalmente, se eliminan los datos que tienen valores menores a los 10 mil pesos colombianos
    BaseDatos <- BaseDatos[!(moneda == "Pesos (COP)" &
                               valor_inicial <= 10000 | 
                               valor_total < 10000)]
    # Adicionalmente, se eliminan los datos que tienen valores menores a los 10 dolares
    BaseDatos <- BaseDatos[!(moneda == "D?lares (US)" &
                               valor_inicial <= 10 | 
                               valor_total < 10)]
    
  #--67. plazo_ejec_calc
    BaseDatos$plazo_ejec_calc <- BaseDatos$fecha_fin_ejec - BaseDatos$fecha_ini_ejec
    BaseDatos$plazo_ejec_calc <- as.integer(BaseDatos$plazo_ejec_calc)
    
    # Se retiran los datos que no sean coherentes en el plazo de ejecuci?n
    BaseDatos <- BaseDatos %>% filter(!is.na(plazo_ejec_calc)) 
    
  #--68. plazo_ejec_dias
    BaseDatos <- data.table(BaseDatos)

    BaseDatos[,plazo_ejec_dias := ifelse(
        rango_ejec == "D", plazo_ejec, 
        ifelse(rango_ejec == "M", plazo_ejec*30, NA
      ))]
  
    #Eliminaci?n de columnas
    BaseDatos$V1 <- NULL #Variable generada automaticamente
    BaseDatos$Tipo Identifi del Contratista <- NULL
    
#Funciones----
    limpiarNumero <- function(bloques){
      valor <- "Invalido"
      if (nrow(bloques) >= 1){
        for (i in 1:nrow(bloques)){
          bloque <- as.numeric(bloques[i])
          if (is.numeric(bloque) & (bloque/100000 > 1) & (!is.na(bloque))){
            valor <- bloque
          }
        }
      }
      return(valor)
    }
    
# Comentario    
  nom_contratista <- BaseDatos %>% group_by(id_contratista) %>% summarise(Dist = n_distinct(nom_contratista))
  
#github