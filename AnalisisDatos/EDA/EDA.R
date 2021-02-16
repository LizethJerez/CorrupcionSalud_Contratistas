# ****************************
# Análisis exploratorio de datos - EDA
# Dataset: SECOP I
# Fecha de elaboración: 12/10/2020
# ****************************

# 1. Librerias ----------------------
library(tidyverse)
library(vroom)
library(lubridate)
library(skimr)
library(gridExtra)

# 2. Conjunto de datos
contratos <- vroom("Datasets/secop_i_ips_limpia.csv.gz")

contratos <- vroom("Datasets/secop_i_ips_limpia_muestra.csv.gz")

# 3. Análisis exploratorio
# 3.1. Resumen de los datos
skim(contratos)

# 3.2. Contratación por tiempo
# General
cst_g <- contratos %>% 
  mutate(fecha_firma_contrato = as.character(fecha_firma_contrato),
         fecha_firma_contrato = str_replace_all(string = fecha_firma_contrato,
                                                pattern = "-", 
                                                replacement = "/"),
         fecha_firma_contrato = ymd(fecha_firma_contrato),
    mes_contratacion = make_date(day = "01", 
                                 month = month(fecha_firma_contrato), 
                                 year =  year(fecha_firma_contrato))) %>% 
  group_by(mes_contratacion) %>% 
  summarise(num_contratos = n(), 
            valor_contratos = sum(valor_total_con_adiciones, na.rm = T))

# Grafico de contratacion por mes por cantidad
cst_g %>% ggplot(mapping = aes(x = mes_contratacion, y = num_contratos)) + 
  geom_line(color = "dodgerblue2") + geom_point(color = "dodgerblue2") +
  theme_light() +labs(title = "Cantidad de Contratos Mensual",
                        subtitle = "Contratación pública hospitalaria",
                        x = "Mes de firma",
                        y = "Número de contratos",
                        caption = "Datos del SECOP I, 2014 - 2019")

# Grafico de contratacion por mes por valor
cst_g %>% ggplot(mapping = aes(x = mes_contratacion, 
                               y = valor_contratos/1e9)) + 
  geom_line(color = "seagreen4") + geom_point(color = "seagreen4") +
  theme_light() + labs(title = "Valor de Contratos Mensual",
                      subtitle = "Contratación pública hospitalaria",
                      x = "Mes de firma",
                      y = "Valor de contratos (MM)",
                      caption = "Datos del SECOP I, 2014 - 2019") +
  scale_y_continuous(limits = c(0,3000))

# Distribución del numero de contratos por entidades
cd_ent <- contratos %>% group_by(nit_entidad) %>% 
  summarise(num_contratos = n(),
            val_contratos = sum(valor_total_con_adiciones, na.rm = T))

# Distribución numero de contratos
pltd_ent_c <- cd_ent %>% ggplot(aes(x = log(num_contratos, base = 10))) + 
  geom_density(color = "dodgerblue2") +
  labs(title = "Contratos por Entidad",
       subtitle = "Cantidad de contratos",
       x = expression("Cantidad de contratos ("~log[10]~")"),
       y = "Densidad",
       caption = "") +
  theme_light() + scale_y_continuous(limits = c(0, 0.8))

# Distribución valor de contratos
pltd_ent_v <- cd_ent %>% ggplot(aes(x = log(val_contratos, base = 10))) + 
  geom_density(color = "seagreen4") +
  labs(title = "",
       subtitle = "Valor de contratos",
       x = expression("Valor de contratos ("~log[10]~")"),
       y = "",
       caption = "Datos del SECOP I, 2014 - 2019") +
  theme_light() + scale_y_continuous(limits = c(0, 0.8))

grid.arrange(pltd_ent_c, pltd_ent_v, nrow = 1)

# Distribución del numero de contratos por contratistas
cd_cont <- contratos %>% group_by(id_contratista) %>% 
  summarise(num_contratos = n(),
            val_contratos = sum(valor_total_con_adiciones, na.rm = T))

# Distribución numero de contratos
pltd_con_c <- cd_cont %>% ggplot(aes(x = log(num_contratos, base = 10))) + 
  geom_density(color = "dodgerblue2") +
  labs(title = "Contratos por Contratistas",
       subtitle = "Cantidad de contratos",
       x = expression("Cantidad de contratos ("~log[10]~")"),
       y = "Densidad",
       caption = "") +
  theme_light() + scale_y_continuous(limits = c(0, 1))

# Distribución valor de contratos
pltd_con_v <- cd_cont %>% ggplot(aes(x = log(val_contratos, base = 10))) + 
  geom_density(color = "seagreen4") +
  labs(title = "",
       subtitle = "Valor de contratos",
       x = expression("Valor de contratos ("~log[10]~")"),
       y = "",
       caption = "Datos del SECOP I, 2014 - 2019") +
  theme_light() + scale_y_continuous(limits = c(0, 1))

grid.arrange(pltd_con_c, pltd_con_v, nrow = 1)

# Resumen por region
res_dep <- contratos %>% group_by(departamento_entidad) %>% 
  summarise(num_cont = n(),
            val_cont = sum(valor_total_con_adiciones, na.rm = T)/1e6,
            num_ent = n_distinct(nit_entidad),
            num_con = n_distinct(id_contratista),
            val_cont_min = min(valor_total_con_adiciones, na.rm = T)/1e6,
            val_media = mean(valor_total_con_adiciones, na.rm = T)/1e6,
            val_mediana = median(valor_total_con_adiciones, na.rm = T)/1e6,
            val_cont_max = max(valor_total_con_adiciones, na.rm = T)/1e6,
            val_cont_sd = sd(valor_total_con_adiciones, na.rm = T)/1e6)

res_dep <- res_dep %>% arrange(desc(val_cont))

write_csv(x = res_dep, path = "Datasets/resumen_departamento.csv")

# Resumen de los actores de contratación
resumen_gen <- tibble(
  concepto = c("Contratos", "entidades", "contratistas"),
  cantidad = c(contratos = contratos %>% count(),
                               entidades = contratos %>% 
                                 select(nit_entidad) %>% unique() %>% count(),
                               contratistas = contratos %>% 
                                 select(id_contratista) %>% 
                                 unique() %>% count()))


