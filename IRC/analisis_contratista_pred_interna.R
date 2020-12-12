# Análsis de los contratistas de las variables internas seleccionadas
# como predictorias

# 1. Librerias ----
library(tidyverse)
library(skimr)
library(reshape2)

# 2. lectura de datos ----
contratistas <- read_csv("Datasets/pred_internas_contratista.csv")

# 3. Análisis ----

#Resumen estadístico
skim(contratistas)

# 3.1. Correlación entre las variables descriptivas e informativas
# informativas
contratistas %>% 
  select(total_cont, valor_total_cont, valor_cont_mediana,
         valor_cont_promedio, valor_cont_sd, IHH_cant, IHH_val) %>% 
  cor(use = "complete.obs") %>% melt(.) %>% 
  ggplot(aes(x = Var1, y = Var2, 
             fill = value)) + geom_tile() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 7, hjust = 1),
        axis.text.y = element_text(size = 7)) +
  scale_fill_gradient2(low = "lightgreen", 
                       high = "orangered2", mid = "khaki1", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name ="Correlación\nPearson") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

# Descriptivas
contratistas %>% 
  select(fam_dif, grupo_dif, entidades_dif, municipios_dif, 
         tiempo_activacion, r.legal_dif, IHH_cant, IHH_val) %>% 
  cor(use = "complete.obs") %>% melt(.) %>% 
  ggplot(aes(x = Var1, y = Var2, 
             fill = value)) + geom_tile() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 7, hjust = 1),
        axis.text.y = element_text(size = 7)) +
  scale_fill_gradient2(low = "lightgreen", 
                       high = "orangered2", mid = "khaki1", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name ="Correlación\nPearson") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

# IRC
contratistas %>% 
  select(p.adc_tiempo, p.adc_tiempo_c, p.adc_valor, p.adc_valor_v, 
         p.contratacion_dir, p.contratacion_dir_v, IHH_cant, IHH_val) %>% 
  cor(use = "complete.obs") %>% melt(.) %>% 
  ggplot(aes(x = Var1, y = Var2, 
             fill = value)) + geom_tile() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 7, hjust = 1),
        axis.text.y = element_text(size = 7)) +
  scale_fill_gradient2(low = "lightgreen", 
                       high = "orangered2", mid = "khaki1", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name ="Correlación\nPearson") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

# 3.2. Indice IHH
# distribución por cantidad
contratistas %>% ggplot(aes(x = IHH_cant)) +
  geom_density(color = 'dodgerblue4', size = 0.7)

# distribución por valor
contratistas %>% ggplot(aes(x = IHH_val)) +
  geom_density(color = 'dodgerblue4', size = 0.7)

# ¿Cuales regiones presentan un mayor IHH?
contratistas %>% group_by(dpto_mcpo_contratista) %>% 
  summarise(IHH_prom = mean(IHH_val)) %>%
  ggplot(aes(x = IHH_prom, 
             y = fct_reorder(dpto_mcpo_contratista, IHH_prom))) +
  geom_col(fill = "dodgerblue4") +
  labs(x = "Índice IHH por valor", y = "Departamento",
       caption = paste("Contratación hospitalaria pública\n",
                       "durante 2014 - 2019")) + theme_light()

# Cual es la relación entre la cantidad de contratos y el indice IHH
contratistas %>% 
  ggplot(aes(x = IHH_val, y = log(total_cont, base = 2))) + 
  geom_point() + geom_smooth(method = "gam", se = F)

# Relación entre el tiempo de activación y el indice IHH
contratistas %>% 
  mutate(tiempo_activacion = as.factor(tiempo_activacion)) %>% 
  group_by(tiempo_activacion) %>% 
  summarise(IHH_mean = mean(IHH_val)) %>% 
  ggplot(aes(x = fct_reorder(tiempo_activacion, IHH_mean), 
             y = IHH_mean)) + geom_col()

# Relación entre el tiempo de activación y el indice IHH
contratistas %>% 
  mutate(tiempo_activacion = case_when(tiempo_activacion <= 2 ~ "0 - 2",
                                       tiempo_activacion <= 4 ~ "3 - 4",
                                       TRUE ~ "5 - 6")) %>% 
  group_by(fam_dif, tiempo_activacion) %>% 
  summarise(IHH_mean = mean(IHH_cant)) %>% 
  ggplot(aes(x = fam_dif, IHH_mean,
             y = IHH_mean, color = tiempo_activacion)) + geom_point() +
  geom_smooth(method = "glm", se = F, size = 0.5) +
  labs(x = "Variedad de familias", y = "Índice IHH por valor",
       color = "Periodo de \nactivación \n(años)",
       caption = paste("Contratación hospitalaria pública\n",
                       "durante 2014 - 2019")) + theme_light()

# Relación entre el municipio y el indice IHH
contratistas %>% 
  group_by(municipios_dif, dpto_mcpo_contratista) %>% 
  summarise(IHH_mean = mean(IHH_cant)) %>% 
  ggplot(aes(x = municipios_dif, IHH_mean,
             y = IHH_mean)) + geom_point() +
  geom_smooth(method = "gam", se = F, size = 1,
              color = "dodgerblue4") +
  labs(x = "Presencia en municipios", y = "Índice IHH por valor",
       color = "Periodo de \nactivación \n(años)",
       caption = paste("Contratación hospitalaria pública\n",
                       "durante 2014 - 2019")) + theme_light()
