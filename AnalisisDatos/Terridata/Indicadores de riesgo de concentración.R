
# INDICADORES DE RIESGO DE CORRUPCIÓN DE CONCENTRACIÓN
# En este archivo se encuentran los indicadores de: 1), 2) y 3) 
# En los enfoques de entidades,contratistasy la relación 

# 1.Cargar librerias -----------------------------------------------
library(tidyverse)
library(vroom)
library(lubridate)
library(agricolae) #Biodiversity Index


# 2. Cargar datos --------------------------------------------------
direccion <- "Datasets/secop_i_ips_limpia_muestra.csv.gz"
secop_i_ips <- vroom(direccion)
#summary(secop_i_ips)

# ***Ajustes (pasarlo al otro R) ----------------------------------
# Atributo: municipio de entidad: Unión delmunicipio y el departamento
secop_i_ips <- secop_i_ips %>% 
  mutate(dto_munic_entid = 
           paste(departamento_entidad, municipio_entidad,sep = "-"))

# 3. Indicador IHH  ------------------------------------------------
"El índice *Herfindahl*, también conocido como Índice *Herfindahl-Hirschman* o 
*HHI*, es una medida del tamaño de las empresas en relación con la industria y 
un indicador de la cantidad de competencia entre ellas. Se calcula como la suma 
de los cuadrados de las cuotas de mercado de las empresas dentro de la industria
(a veces limitada a las 50 empresas más grandes)."

# 3.1 Entidades----
indice_IHH_entidades <-  secop_i_ips %>%
  group_by(dto_munic_entid,nit_entidad) %>%
  summarise(val_total = sum(valor_total_con_adiciones,na.rm = TRUE),
            cant_total = n()) %>% 
  group_by(dto_munic_entid) %>% 
  mutate(val_total_munic = sum(val_total, na.rm = TRUE),
         cant_total_munic =sum(cant_total, na.rm = TRUE),
         s2_valor = (val_total/val_total_munic*100)^2,             #s2= % al 2
         s2_cantidad = (cant_total/cant_total_munic*100)^2) %>% 
  group_by(dto_munic_entid) %>% 
  summarise(IHH_val=(sum(s2_valor)/10000)*100, 
            IHH_cant= (sum(s2_cantidad)/10000)*100,
            num_entidades = n_distinct(nit_entidad)) 
  

# 3.2 Contratistas----
indice_IHH_contratistas <-  secop_i_ips %>%
  group_by(municipio_ejecucion,id_contratista) %>%
  summarise(val_total = sum(valor_total_con_adiciones,na.rm = TRUE),
            cant_total = n()) %>% 
  group_by(municipio_ejecucion) %>% 
  mutate(val_total_munic = sum(val_total, na.rm = TRUE),
         cant_total_munic =sum(cant_total, na.rm = TRUE),
         s2_valor = (val_total/val_total_munic*100)^2,
         s2_cantidad = (cant_total/cant_total_munic*100)^2) %>% 
  group_by(municipio_ejecucion) %>% 
  summarise(IHH_val=(sum(s2_valor)/10000)*100, 
            IHH_cant=(sum(s2_cantidad)/10000)*100,
            num_contratistas = n_distinct(id_contratista))


# 3.3 Relación entidad - contratista ----
# Analisado desde la persepectiva de la entidad 
indice_IHH_relacion <-  secop_i_ips %>%
  group_by(dto_munic_entid, nit_entidad,id_contratista) %>%
  summarise(val_total = sum(valor_total_con_adiciones,na.rm = TRUE),
            cant_total = n()) %>% 
  group_by(nit_entidad) %>% 
  mutate(numero_contratistas = n_distinct(id_contratista),
         val_total_entid = sum(val_total, na.rm = TRUE),
         cant_total_entid =sum(cant_total, na.rm = TRUE),
         s2_valor = (val_total/val_total_entid*100)^2,
         s2_cantidad = (cant_total/cant_total_entid*100)^2) %>% 
  group_by(dto_munic_entid, nit_entidad) %>% 
  summarise(IHH_val=(sum(s2_valor)/10000)*100, 
            IHH_cant=(sum(s2_cantidad)/10000)*100) %>% 
  group_by(dto_munic_entid) %>% 
  summarise(IHH_val_m = mean(IHH_val,na.rm = TRUE) ,
            IHH_val_sd = sd(IHH_val,na.rm = TRUE) , 
            IHH_cant_m = mean(IHH_cant,na.rm = TRUE),
            IHH_cant_sd = sd(IHH_cant,na.rm = TRUE))



# 4. Indice de simpson SP-----------------------------------------------
  # Ecosistema: Cada municipio,Especie: objeto a estudiar, Organismos: Q,$
"El *indice de diversidad de Simpson* (también conocido como el índice de la 
diversidad de las especies o índice de dominancia) es uno de los parámetros que
permiten medir la biodiversidad de organismos en un hábitat. Llevado al contexto
de la contratación permite medir la diversidad de contratistas en una entidad.

**INDICE DE DIVERSIDAD** : (1 - Indice dominancia)
Cuando el indice es cercano a 1 existe una menor diversidad y por tanto mayor 
dominancia de algunos contratistas, y cuando es cercano a 0 ocurre lo contrario, 
existe mayor diversidad y por tanto menor dominancia de algunos contratistas."

# 4.1 Entidades----
indice_simpson_entidades <- secop_i_ips %>%
  group_by(dto_munic_entid, nit_entidad) %>%
  summarise(val_total = sum(valor_total_con_adiciones,na.rm = TRUE),
            cant_total = n()) %>%
  group_by(dto_munic_entid) %>%
  mutate(numerador_cant = cant_total*(cant_total-1),
         numerador_val= val_total*(val_total-1),
         total_cant_municip= sum(cant_total),
         total_val_municip= sum(val_total)) %>%
  group_by(dto_munic_entid) %>%
  summarise(IS_dominancia_cant = 
           sum(numerador_cant)/(total_cant_municip*(total_cant_municip-1)),
         IS_dominancia_val =
           sum(numerador_val)/(total_val_municip*(total_val_municip-1)),
         num_entidades_municip= n_distinct(nit_entidad)) 

# 4.2 Contratistas----
indice_simpson_contrataistas <- secop_i_ips %>%
  group_by(municipio_ejecucion, id_contratista) %>%
  summarise(val_total = sum(valor_total_con_adiciones,na.rm = TRUE),
            cant_total = n()) %>%
  group_by(municipio_ejecucion) %>%
  mutate(numerador_cant = cant_total*(cant_total-1),
         numerador_val= val_total*(val_total-1),
         total_cant_municip= sum(cant_total),
         total_valor_municip= sum(val_total))%>% 
  group_by(municipio_ejecucion) %>%
  summarise(IS_dominancia_cant=
           sum(numerador_cant)/(total_cant_municip*(total_cant_municip-1)),
         IS_dominancia_val=
           sum(numerador_val)/(total_valor_municip*(total_valor_municip-1)),
         num_contratistas_municip = n_distinct(id_contratista))

# 4.3 Nodos entidad y contratistas----
# Responde a la pregunta ¿Diversidad del número de contratistas por cada entidad?
indice_simpson_nodos <- secop_i_ips %>%
  group_by(dto_munic_entid, nit_entidad) %>%
  summarise(cant_contratistas = n(),
            val_max_contrato = max(valor_total_con_adiciones,na.rm = TRUE),
            val_min_contrato= min(valor_total_con_adiciones,na.rm = TRUE),
            val_prom_contrato= mean(valor_total_con_adiciones,na.rm = TRUE),
            val_sd_contarato = sd(valor_total_con_adiciones,na.rm = TRUE)) %>%
  group_by(dto_munic_entid) %>%
  mutate(numerador_cant = cant_contratistas*(cant_contratistas-1),
         num_entidades_muni= n_distinct(nit_entidad),
         cant_contratis_munic = sum(cant_contratistas))%>%
  group_by(dto_munic_entid) %>%
  mutate(IS_dominancia_cant = 
           sum(numerador_cant)/(cant_contratis_munic*(cant_contratis_munic-1)))


# 4.4 Relación entidad-contratista ----
indice_simpson_relacion <- secop_i_ips %>%   
  mutate(id_relacion = paste(nit_entidad,id_contratista, sep = "-")) %>%  
  group_by(municipio_ejecucion, id_relacion) %>%
  summarise(cant_vinculos = n()) %>%
  group_by(municipio_ejecucion) %>%
  mutate (total_vinculos_munic = sum(cant_vinculos)) %>%  
  group_by(municipio_ejecucion, id_relacion) %>%
  mutate(porcentaje_relativ= cant_vinculos/ total_vinculos_munic,
         numerador_cant = cant_vinculos*(cant_vinculos-1))%>%
  group_by(municipio_ejecucion) %>%
  mutate(IS_dominancia_cant = 
           sum(numerador_cant)/(total_vinculos_munic*(total_vinculos_munic-1)))

#Anterior metodo
"y <- list(1,1,1)
y <- unlist(y, use.names=FALSE)
bogota<- index.bio(data = y, method = Simpson.Div)[,2]"



# 5.Índice de concentración IC4k de las cuatro empresas con mayor partición-----
"# Mide el número de procesos que la entidad estatal adjudicó a los cuatro 
contratistas con más contratos. Un valor alto indicaría que hay pocos 
contratistas con muchos contratos."

# 5.1 Entidades----
"indice_4K_entidades <- secop_i_ips %>%
 group_by(nit_entidad, id_contratista) %>% 
 summarise(nj_cant= n(),
            nj_val = sum(valor_total_con_adiciones)) %>% 
  group_by(nit_entidad) %>% 
  mutate(Ni_cant= sum(nj_cant),
         Ni_val = sum(nj_val),
         Pi_cant= nj_cant/Ni_cant,
         Pi_val = nj_val/Ni_val) %>% 
  top_n(n = 4) %>% 
  summarise(IC4K_cant = sum(Pi_cant)*100,
            IC4K_val= sum(Pi_val)*100)"

# cantidad de contratos
indice_4K_entidades_cant <- secop_i_ips %>%
  group_by(nit_entidad, id_contratista) %>% 
  summarise(nj = n()) %>% 
  group_by(nit_entidad) %>% 
  mutate(Ni= sum(nj),
         Pi= nj/Ni,
         cant_contratis = n_distinct(id_contratista)) %>% 
  top_n(n = 4, wt = nj) %>% 
  summarise(IC4K_cant = sum(Pi)*100)

# Valor de contratos
indice_4K_entidades_val <- secop_i_ips %>%
  group_by(nit_entidad, id_contratista) %>% 
  summarise(nj_val = sum(valor_total_con_adiciones)) %>% 
  group_by(nit_entidad) %>% 
  mutate(Ni_val = sum(nj_val),
         Pi_val = nj_val/Ni_val,
         cant_contratis = n_distinct(id_contratista)) %>% 
  top_n(n = 4, wt = nj_val) %>% 
  summarise(IC4K_val= sum(Pi_val)*100)

# 5.2 Municipio----

# Cantidad de contratos 
indice_4K_municip_cant <- secop_i_ips %>%
  group_by(municipio_ejecucion, id_contratista) %>% 
  summarise(nj = n()) %>% 
  group_by(municipio_ejecucion) %>% 
  mutate(Ni= sum(nj),
         Pi= nj/Ni,
         cant_contratis = n_distinct(id_contratista)) %>% 
  top_n(n = 4, wt=nj) %>% 
  summarise(IC4K_cant = sum(Pi)*100) 

# Valor de contratos
indice_4K_municip_val <- secop_i_ips %>%
  group_by(municipio_ejecucion, id_contratista) %>% 
  summarise(nj_val = sum(valor_total_con_adiciones)) %>% 
  group_by(municipio_ejecucion) %>% 
  mutate(Ni_val = sum(nj_val),
         Pi_val = nj_val/Ni_val,
         cant_contratis = n_distinct(id_contratista)) %>% 
  top_n(n = 4, wt = nj_val) %>% 
  summarise(IC4K_val= sum(Pi_val)*100)



