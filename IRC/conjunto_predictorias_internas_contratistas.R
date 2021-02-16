# Creación de las variables predictorias del conjunto de datos de secop 1,
# para los contratistas

# 1. Librerias ----
library(tidyverse)
library(vroom)

# 2. Carga de conjunto de datos ----
contratos <- vroom("Datasets/secop_i_ips_limpia.csv.gz")

# 3. Construcción de variables predictorias ----

# 3.1. Variables informativas y descriptivas ----

# Determinar la cantidad de contratos por contratista
contratistas <- contratos %>% 
  group_by(id_contratista, dpto_mcpo_contratista) %>% 
  summarize(total_cont = n()) %>% 
  filter(total_cont >= 10)

# Establecer las variables de interes
temp <- contratos %>% 
  filter(id_contratista %in% contratistas$id_contratista) %>% 
  group_by(id_contratista, dpto_mcpo_contratista) %>% 
  summarise(valor_total_cont = sum(valor_total_con_adiciones, na.rm = T),
      valor_cont_mediana = median(valor_total_con_adiciones, na.rm = T),
      valor_cont_promedio = mean(valor_total_con_adiciones, na.rm = T),
      valor_cont_sd = sd(valor_total_con_adiciones, na.rm = T),
      fam_dif = n_distinct(nombre_familia),
      grupo_dif = n_distinct(nombre_grupo),
      entidades_dif = n_distinct(nit_entidad),
      municipios_dif = n_distinct(municipio_ejecucion),
      tiempo_activacion = n_distinct(anno_firma),
      r.legal_dif = n_distinct(id_representante_legal))

# Unión del conjunto de datos
contratistas <- merge(contratistas, temp, 
                      by = c("id_contratista", "dpto_mcpo_contratista"),
                      all.x = T)

# INDICADORES DE RIESGO DE CORRUPCION
# 3.2. Porcentaje de adiciones de valor y tiempo en frec. y cantidad ----
# Tiempo
temp <- contratos %>%
  mutate(tiempo_ejecucion = if_else(rango_ejec_contrato == "D",
                                       plazo_ejec_contrato, 
                                       plazo_ejec_contrato * 30),
         adicion_tiempo = tiempo_adiciones_dias + 
           tiempo_adiciones_meses * 30,
         tiempo_ejecucion_total = tiempo_ejecucion + adicion_tiempo,
         es_adicion = adicion_tiempo > 0) %>% 
  filter(id_contratista %in% contratistas$id_contratista) %>% 
  group_by(id_contratista, dpto_mcpo_contratista, es_adicion) %>% 
  mutate(cont = n(),
         cantidad = sum(tiempo_ejecucion_total, na.rm = T)) %>% 
  ungroup(es_adicion) %>% 
  mutate(cont_total = n(),
         cantidad_total = sum(tiempo_ejecucion_total, na.rm = T)) %>% 
  filter(cantidad_total > 0) %>% 
  mutate(p.adc_tiempo = cont/cont_total,
         p.adc_tiempo_c = cantidad/cantidad_total) %>% 
  filter(es_adicion) %>% 
  select(id_contratista, dpto_mcpo_contratista,
         p.adc_tiempo, p.adc_tiempo_c) %>% unique()

# Unión del conjunto de datos
contratistas <- merge(contratistas, temp, 
                      by = c("id_contratista", "dpto_mcpo_contratista"),
                      all.x = T)
# valor
temp <- contratos %>%
  mutate(es_adicion = valor_adiciones > 0) %>% 
  filter(id_contratista %in% contratistas$id_contratista) %>% 
  group_by(id_contratista, dpto_mcpo_contratista, es_adicion) %>% 
  mutate(cont = n(),
         valor = sum(valor_total_con_adiciones, na.rm = T)) %>% 
  ungroup(es_adicion) %>% 
  mutate(cont_total = n(),
         valor_total = sum(valor_total_con_adiciones, na.rm = T)) %>% 
  filter(valor_total > 0) %>% 
  mutate(p.adc_valor = cont/cont_total,
         p.adc_valor_v = valor/valor_total) %>% 
  filter(es_adicion) %>% 
  select(id_contratista, dpto_mcpo_contratista,
         p.adc_valor, p.adc_valor_v) %>% unique()

# Unión del conjunto de datos
contratistas <- merge(contratistas, temp, 
                      by = c("id_contratista", "dpto_mcpo_contratista"),
                      all.x = T)

contratistas <- contratistas %>% mutate_all(~replace(., is.na(.), 0))

# 3.3. Porcentaje de contratacion directa en frecuencia y valor ----
impt_v <- "Contratación Directa (Ley 1150 de 2007)"

temp <- contratos %>%
  mutate(es_directo = tipo_proceso == impt_v) %>% 
  filter(id_contratista %in% contratistas$id_contratista) %>% 
  group_by(id_contratista, dpto_mcpo_contratista, es_directo) %>% 
  mutate(cont = n(),
         valor = sum(valor_total_con_adiciones, na.rm = T)) %>% 
  ungroup(es_directo) %>% 
  mutate(cont_total = n(),
         valor_total = sum(valor_total_con_adiciones, na.rm = T)) %>% 
  filter(valor_total > 0) %>% 
  mutate(p.contratacion_dir = cont/cont_total,
         p.contratacion_dir_v = valor/valor_total) %>% 
  filter(es_directo) %>% 
  select(id_contratista, dpto_mcpo_contratista,
         p.contratacion_dir, p.contratacion_dir_v) %>% unique()

# Unión del conjunto de datos
contratistas <- merge(contratistas, temp, 
                      by = c("id_contratista", "dpto_mcpo_contratista"),
                      all.x = T)

contratistas <- contratistas %>% mutate_all(~replace(., is.na(.), 0))

# 3.4. Indicador IHH ----
temp <- contratos %>%
  filter(id_contratista %in% contratistas$id_contratista) %>% 
  group_by(id_contratista, dpto_mcpo_contratista, nit_entidad) %>% 
  mutate(cont = n(),
         valor = sum(valor_total_con_adiciones, na.rm = T)) %>% 
  ungroup(nit_entidad) %>% 
  mutate(cont_total = n(),
         valor_total = sum(valor_total_con_adiciones, na.rm = T),
         p.cont_entidad = (cont / cont_total * 100) ^ 2,
         p.valor_entidad = (valor / valor_total * 100) ^ 2) %>%
  select(id_contratista, dpto_mcpo_contratista, 
         p.cont_entidad, p.valor_entidad) %>% unique() %>% 
  summarise(IHH_cant = sum(p.cont_entidad, na.rm = T) / 10000,
            IHH_val = sum(p.valor_entidad, na.rm = T) / 10000)

# Unión del conjunto de datos
contratistas <- merge(contratistas, temp, 
                      by = c("id_contratista", "dpto_mcpo_contratista"),
                      all.x = T)

# Ajuste de los campos que no poseen valores
contratistas <- contratistas %>% 
  mutate(IHH_cant = if_else(!is.na(IHH_cant), IHH_cant,
                            mean(contratistas$IHH_cant, na.rm =  T)),
         IHH_val = if_else(!is.na(IHH_val), IHH_val,
                           mean(contratistas$IHH_val, na.rm =  T)))

# 3.5. Indicador C4K ----
# Tiempo
temp <- contratos %>%
  filter(id_contratista %in% contratistas$id_contratista) %>% 
  group_by(id_contratista, dpto_mcpo_contratista) %>% 
  mutate(entidades_dif = n_distinct(nit_entidad)) %>% 
  filter(entidades_dif > 4) %>%
  group_by(id_contratista, dpto_mcpo_contratista, nit_entidad) %>% 
  summarise(cont = n()) %>% 
  ungroup(nit_entidad) %>% 
  mutate(Ni = sum(cont),
         p.entidad = cont/Ni) %>% 
  arrange(desc(p.entidad)) %>% 
  slice(1:4) %>%
  summarise(IC4K_cant = sum(p.entidad))

# Unión del conjunto de datos
contratistas <- merge(contratistas, temp, 
                      by = c("id_contratista", "dpto_mcpo_contratista"),
                      all.x = T)

# valor
temp <- contratos %>%
  filter(id_contratista %in% contratistas$id_contratista) %>% 
  group_by(id_contratista, dpto_mcpo_contratista) %>% 
  mutate(entidades_dif = n_distinct(nit_entidad)) %>% 
  filter(entidades_dif > 4) %>%
  group_by(id_contratista, dpto_mcpo_contratista, nit_entidad) %>% 
  summarise(valor = sum(valor_total_con_adiciones, na.rm = T)) %>% 
  ungroup(nit_entidad) %>% 
  mutate(Ni_v = sum(valor),
         p.entidad_v = valor/Ni_v) %>% 
  arrange(desc(p.entidad_v)) %>% 
  slice(1:4) %>%
  summarise(IC4K_val = sum(p.entidad_v))

# Unión del conjunto de datos
contratistas <- merge(contratistas, temp, 
                      by = c("id_contratista", "dpto_mcpo_contratista"),
                      all.x = T)

# Remover objetos
rm(contratos)
rm(temp)

# 4. Escritura de datos
write_csv(x = contratistas, path = "Datasets/pred_internas_contratista.csv")
