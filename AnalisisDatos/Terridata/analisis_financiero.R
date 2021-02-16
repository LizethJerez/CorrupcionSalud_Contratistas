# Análisis de las variables financieras disponibles 
# en terridata
# Conjunto de datos: Financiero terridata
# Variables: Todas
# Fecha: 24/06/2020

# 1. Librerias ----
library(tidyverse)
library(skimr)
library(plotly)

# 2. Carga y ajuste del conjunto de datos ----
ind_finan <- read_csv(
  file = "Datasets/terridata_financiero.csv",
  locale = locale(encoding = "UTF-8")) 

names(ind_finan) <- c("codigo_departamento", "departamento",
                       "codigo_entidad", "entidad", 
                       "dimension", "subcategoria", 
                       "indicador", "valor", 
                       "dato_cualitativo", "anno",
                       "mes", "fuente", "und_medida")

# Eliminación de la columna de dimension por
# contener los mismos valores
# La variable dato_cualitativo está vacia
ind_finan <- ind_finan %>% 
  select(-dimension, -dato_cualitativo)

#Visualización Categoria de las variables
categoria <- ind_finan %>% 
  select(subcategoria, indicador) %>% unique() %>% 
  mutate(ids = paste(subcategoria, indicador, sep = " - "))

categoria <- rbind(
  data_frame(subcategoria = rep(x = "", 12),
             indicador ={x <- ind_finan %>% 
               select(subcategoria) %>% unique();
             x[[1]]},
             ids = indicador), categoria)

categoria <- categoria %>% 
  rename(parents = subcategoria, info = indicador) %>% 
  mutate(label = paste0(str_sub(info, 1, 10), "..."))

fig <- plot_ly(categoria,
               type = 'sunburst',
               ids = ~ids, labels = ~label, 
               parents = ~parents,
               hovertext = ~info,
               hovertemplate = "%{hovertext}<extra></extra>",
               domain = list(x = c(0,1),
                             y = c(0,1)))

fig <- fig %>% layout(title = "Subcategorias de indicadores",
               margin = list(l = 0, r = 0, t = 25, b = 0))


# Ajuste unidades
ind_finan <- ind_finan %>% 
  mutate(
    valor = ifelse(und_medida == "Millones de pesos corrientes",
                   valor*1e6, valor))

# Ajuste de las variables SGP para diferenciar los indicadores
ind_finan <- ind_finan %>% 
  mutate(indicador = case_when(
    subcategoria == paste0("SGP - Distribución asignaciones",
          " SGP por sectores") ~ paste0("abs - ", indicador),
    subcategoria == paste0("SGP - Distribución porcentual ",
                      "de asignaciones SGP por sectores") ~ 
      paste0("ppa - ", indicador),
    subcategoria == paste0("SGP - Porcentaje de ejecución ",
    "presupuestal por sector") ~ paste0("ppe - ", indicador),
    TRUE ~ indicador
  ))

# División del conjunto de datos
ind_finan_pais <- ind_finan %>% 
  filter(departamento == "Colombia")

ind_finan_dpto <- ind_finan %>% 
  filter(departamento == entidad,
         departamento != "Colombia")

ind_finan_mup <- rbind(
  ind_finan %>% 
    filter(departamento != entidad),
  ind_finan %>% 
    filter(departamento == "Bogotá")
)

# Despejar del R la variable de ind_finan
ind_finan <- NULL

# 3. Análisis ----
# 3.1 Colombia ----
# Categorias del conjunto de datos

# Ajuste del conjunto de datos
ind_finan_pais <- ind_finan_pais %>% 
  select(subcategoria, indicador, 
         valor, anno, mes) %>% 
  pivot_wider(names_from = "indicador",
              values_from = "valor")

# Resumen
skim(data = ind_finan_pais %>% 
       select(-subcategoria))

# Análisis de los ingresos
ind_finan_pais %>% 
  select("anno","Ingresos tributarios", 
         "Ingresos no tributarios", "Ingresos de capital") %>% 
  pivot_longer(
    cols = c("Ingresos tributarios", "Ingresos no tributarios",
             "Ingresos de capital"),
    names_to = "rubro", values_to = "valor") %>%
  filter(!is.na(valor)) %>%
  ggplot(aes(x = anno, y = valor, fill = rubro)) +
  geom_area() +
  labs(title = "Componente Ingresos",
       subtitle = "País", x = "año", y = "Valor") +
  scale_x_continuous(breaks = seq(2000, 2017, by = 2))

# Análisis de los gastos
ind_finan_pais %>% 
  select("anno", "Gastos de capital (Inversión)", 
         "Funcionamiento", "Intereses de deuda pública") %>% 
  pivot_longer(
    cols = c("Gastos de capital (Inversión)", 
             "Funcionamiento", "Intereses de deuda pública"),
    names_to = "rubro", values_to = "valor") %>%
  filter(!is.na(valor)) %>%
  ggplot(aes(x = anno, y = valor, fill = rubro)) +
  geom_area() +
  labs(title = "Componente Gastos",
       subtitle = "País", x = "año", y = "Valor") +
  scale_x_continuous(breaks = seq(2000, 2017, by = 2))

# Ingresos vs gastos
ind_finan_pais %>% 
  select(anno, `Ingresos totales`, `Gastos totales`) %>% 
  pivot_longer(
    cols = c("Ingresos totales", "Gastos totales"),
    names_to = "rubro", values_to = "valor") %>%
  filter(!is.na(valor)) %>% 
  ggplot(aes(x = anno, y = valor, color = rubro)) +
  geom_line() + geom_point(size = 1) +
  labs(title = "Ingresos vs gastos",
       subtitle = "País", x = "año", y = "Valor") +
  scale_x_continuous(breaks = seq(2000, 2017, by = 2))

# Ingresos per capita
ind_finan_pais %>% 
  select("anno","Ingresos per cápita por impuesto predial", 
          paste0("Ingresos per cápita por impuesto a la",
                 " Industria y al comercio"),
  "Transferencias per cápita de los ingresos corrientes") %>% 
  pivot_longer(
    cols = c("Ingresos per cápita por impuesto predial", 
      paste0("Ingresos per cápita por impuesto a la",
             " Industria y al comercio"),
    "Transferencias per cápita de los ingresos corrientes"),
    names_to = "rubro", values_to = "valor") %>%
  filter(!is.na(valor)) %>%
  ggplot(aes(x = anno, y = valor, fill = rubro)) +
  geom_area() +
  labs(title = "Componente Ingresos per capita",
       subtitle = "País", x = "año", y = "Valor") +
  scale_x_continuous(breaks = seq(2000, 2017, by = 2))

# Ingresos vs gastos per capital
ind_finan_pais %>% 
  select(anno, `Ingresos totales per cápita`, 
         `Gastos totales per cápita`) %>% 
  pivot_longer(
    cols = c("Ingresos totales per cápita", 
             "Gastos totales per cápita"),
    names_to = "rubro", values_to = "valor") %>%
  filter(!is.na(valor)) %>% 
  ggplot(aes(x = anno, y = valor, color = rubro)) +
  geom_line() + geom_point(size = 1) +
  labs(title = "Ingresos vs gastos per capita",
       subtitle = "País")

# Déficit o ahorro corriente
ind_finan_pais %>% 
  select(anno, `Déficit o ahorro corriente`) %>% 
  pivot_longer(
    cols = "Déficit o ahorro corriente",
    names_to = "rubro", values_to = "valor") %>%
  filter(!is.na(valor)) %>% 
  ggplot(aes(x = anno, y = valor)) +
  geom_line() + geom_point(size = 1) +
  labs(title = "Déficit o ahorro corriente",
       subtitle = "País", x = "año", y = "valor") +
  scale_x_continuous(breaks = seq(2000, 2017, by = 2))

# Financiamiento
ind_finan_pais %>% 
  select(anno, "Financiamiento") %>% 
  pivot_longer(
    cols = "Financiamiento",
    names_to = "rubro", values_to = "valor") %>%
  filter(!is.na(valor)) %>% 
  ggplot(aes(x = anno, y = valor)) +
  geom_line() + geom_point(size = 1) +
  labs(title = "Financiamiento",
       subtitle = "País", x = "año", y = "valor") +
  scale_x_continuous(breaks = seq(2000, 2017, by = 2))

# Credito interno y externo
ind_finan_pais %>% 
  select(anno, "Credito interno y externo") %>% 
  pivot_longer(
    cols = "Credito interno y externo",
    names_to = "rubro", values_to = "valor") %>%
  filter(!is.na(valor)) %>% 
  ggplot(aes(x = anno, y = valor)) +
  geom_line() + geom_point(size = 1) +
  labs(title = "Credito interno y externo",
       subtitle = "País", x = "año", y = "valor") +
  scale_x_continuous(breaks = seq(2000, 2017, by = 2))

# Regalías per cápita
ind_finan_pais %>% 
  select(anno, paste0("Regalías per cápita ", 
              "(Valor efectivamente girado al municipio)")) %>% 
  pivot_longer(
    cols = paste0("Regalías per cápita ", 
                  "(Valor efectivamente girado al municipio)"),
    names_to = "rubro", values_to = "valor") %>%
  filter(!is.na(valor)) %>% 
  ggplot(aes(x = anno, y = valor)) +
  geom_line() + geom_point(size = 1) +
  labs(title = "Regalías per cápita",
       subtitle = "País")

# Las demás no se tomaron porque no hay datos suficientes

# 3.2 Departamental ----
# Ajuste del conjunto de datos
ind_finan_dpto <- ind_finan_dpto %>%
  select(-c("und_medida", "fuente", 
            "codigo_departamento", "departamento")) %>% 
  pivot_wider(names_from = "indicador",
              values_from = "valor")

# Resumen
skim(data = ind_finan_dpto)

# Análisis de los ingresos
ind_finan_dpto %>% 
  select("anno","codigo_entidad" ,"Ingresos tributarios", 
         "Ingresos no tributarios", "Ingresos de capital") %>% 
  pivot_longer(
    cols = c("Ingresos tributarios", "Ingresos no tributarios",
             "Ingresos de capital"),
    names_to = "rubro", values_to = "valor") %>%
  filter(!is.na(valor)) %>%
  ggplot(aes(x = log(valor, base = 10), fill = rubro)) +
  facet_wrap(vars(anno)) + geom_histogram() +
  labs(title = "Componente Ingresos",
       subtitle = "Departamento", x = "año", y = "Valor") +
  scale_x_continuous(breaks = seq(2000, 2017, by = 2))

# ingresos vs gastos
ind_finan_dpto %>% 
  select(anno, codigo_entidad, 
         `Ingresos totales`, `Gastos totales`) %>% 
  pivot_longer(
    cols = c("Ingresos totales", "Gastos totales"),
    names_to = "rubro", values_to = "valor") %>%
  filter(!is.na(valor), valor != 0) %>% 
  ggplot(aes(x = log(valor, base = 10), color = rubro)) + 
  facet_wrap(vars(anno)) + geom_density() +
  labs(title = "Ingresos vs gastos",
       subtitle = "Departamento")

## Top
ind_finan_dpto %>% 
  select(anno, entidad, codigo_entidad,
         `Ingresos totales`, `Gastos totales`) %>% 
  pivot_longer(
    cols = c("Ingresos totales", "Gastos totales"),
    names_to = "rubro", values_to = "valor") %>%
  filter(!is.na(valor), valor != 0) %>%
  group_by(entidad, rubro) %>%
  summarise(valor = mean(valor, na.rm = TRUE)) %>% 
  ggplot(aes(x = log(valor, base = 10), 
             y = fct_reorder(entidad, valor),
             fill = rubro)) + geom_col(position = "dodge2") +
  labs(title = " Top Ingresos vs gastos",
       subtitle = "Departamento")


# ingresos vs gastos per capital
ind_finan_dpto %>% 
  select(anno, codigo_entidad, 
         `Ingresos totales per cápita`,
         `Gastos totales per cápita`) %>% 
  pivot_longer(
    cols = c("Ingresos totales per cápita",
             "Gastos totales per cápita"),
    names_to = "rubro", values_to = "valor") %>%
  filter(!is.na(valor), valor != 0) %>% 
  ggplot(aes(x = log(valor, base = 10), color = rubro)) + 
  facet_wrap(vars(anno)) + geom_density() +
  labs(title = "Ingresos vs gastos per capital",
       subtitle = "Departamento")

## Top
ind_finan_dpto %>% 
  select(anno, entidad, codigo_entidad,
         `Ingresos totales per cápita`,
         `Gastos totales per cápita`) %>% 
  pivot_longer(
    cols = c("Ingresos totales per cápita", 
             "Gastos totales per cápita"),
    names_to = "rubro", values_to = "valor") %>%
  filter(!is.na(valor), valor != 0) %>%
  group_by(entidad, rubro) %>%
  summarise(valor = mean(valor, na.rm = TRUE)) %>% 
  ggplot(aes(x = valor, 
             y = fct_reorder(entidad, valor),
             fill = rubro)) + geom_col(position = "dodge2") +
  labs(title = "Top Ingresos vs gastos per capital",
       subtitle = "Departamento")

# Regalías per cápita (Valor efectivamente girado al municipio)
ind_finan_dpto %>% 
  select(anno, codigo_entidad, 
         paste0("Regalías per cápita",
          " (Valor efectivamente girado al municipio)")) %>% 
  pivot_longer(
    cols = c(paste0("Regalías per cápita",
          " (Valor efectivamente girado al municipio)")),
    names_to = "rubro", values_to = "valor") %>%
  filter(!is.na(valor), valor != 0) %>% 
  ggplot(aes(x = log(valor, base = 10))) + 
  facet_wrap(vars(anno)) + geom_density() +
  labs(title = "Regalías per cápita",
       subtitle = "Departamento")

## Top
ind_finan_dpto %>% 
  select(anno, entidad, codigo_entidad,
         paste0("Regalías per cápita",
          " (Valor efectivamente girado al municipio)")) %>% 
  pivot_longer(
    cols = c(paste0("Regalías per cápita",
          " (Valor efectivamente girado al municipio)")),
    names_to = "rubro", values_to = "valor") %>%
  filter(!is.na(valor), valor != 0) %>%
  group_by(entidad, rubro) %>%
  summarise(valor = mean(valor, na.rm = TRUE)) %>% 
  ggplot(aes(x = valor, 
             y = fct_reorder(entidad, valor))) + 
           geom_col(position = "dodge2") +
  labs(title = "Top Regalías per cápita",
       subtitle = "Departamento")

# Financiamiento
ind_finan_dpto %>% 
  select(anno, codigo_entidad, 
         Financiamiento) %>% 
  pivot_longer(
    cols = Financiamiento,
    names_to = "rubro", values_to = "valor") %>%
  filter(!is.na(valor), valor != 0) %>% 
  ggplot(aes(x = log(valor, base = 10))) + 
  facet_wrap(vars(anno)) + geom_density() +
  labs(title = "Financiamiento",
       subtitle = "Departamento")

## Top
ind_finan_dpto %>% 
  select(anno, entidad, codigo_entidad,
         Financiamiento) %>% 
  pivot_longer(
    cols = Financiamiento,
    names_to = "rubro", values_to = "valor") %>%
  filter(!is.na(valor), valor != 0) %>%
  group_by(entidad, rubro) %>%
  summarise(valor = mean(valor, na.rm = TRUE)) %>% 
  ggplot(aes(x = log(valor, base = 10), 
             y = fct_reorder(entidad, valor))) + 
  geom_col(position = "dodge2") +
  labs(title = "Top Financiamiento",
       subtitle = "Departamento")

# Total de recursos asignados per cápita
ind_finan_dpto %>% 
  select(anno, codigo_entidad, 
         paste0("Total de recursos asignados",
                " per cápita - SGR (Bienio)"),
         `Total de recursos asignados per cápita - SGP`) %>% 
  pivot_longer(
    cols = c(paste0("Total de recursos asignados",
                    " per cápita - SGR (Bienio)"),
             `Total de recursos asignados per cápita - SGP`),
    names_to = "rubro", values_to = "valor") %>%
  filter(!is.na(valor), valor != 0) %>% 
  ggplot(aes(x = log(valor, base = 10), color = rubro)) + 
  facet_wrap(vars(anno)) + geom_density() +
  labs(title = "Total de recursos asignados per cápita",
       subtitle = "Departamento")

# Inversion salud vs inversion en educación
ind_finan_dpto %>% 
  select(anno, codigo_entidad, 
         `Inversión - Educación`,
         `Inversión - Salud`) %>% 
  pivot_longer(
    cols = c("Inversión - Educación",
             "Inversión - Salud"),
    names_to = "rubro", values_to = "valor") %>%
  filter(!is.na(valor), valor != 0) %>% 
  ggplot(aes(x = log(valor, base = 10),
             y = rubro,
             color = rubro)) + 
  facet_wrap(vars(anno)) + geom_boxplot() +
  labs(title = "Inversion salud vs inversion en educación",
       subtitle = "Departamento") +
  theme(legend.position = "none")

# Inversion salud vs inversion en educación
ind_finan_dpto %>% 
  select(anno, codigo_entidad, 
         `Índice de gestión de proyectos de regalías (IGPR)`,
         `Componente - Eficacia`, `Componente - Eficiencia`,
         `Componente - Sin medidas del SMSCE`,
         `Componente - Transparencia`) %>% 
  pivot_longer(
    cols = c(`Índice de gestión de proyectos de regalías (IGPR)`,
             `Componente - Eficacia`, `Componente - Eficiencia`,
             `Componente - Sin medidas del SMSCE`,
             `Componente - Transparencia`),
    names_to = "rubro", values_to = "valor") %>%
  filter(!is.na(valor), valor != 0) %>% 
  ggplot(aes(x = valor,
             y = rubro,
             color = rubro)) + 
  facet_wrap(vars(anno)) + geom_boxplot() +
  labs(title = "Indicadores SGR",
       subtitle = "Departamento") +
  theme(legend.position = "none")

# Cantidad de proyectos
ind_finan_dpto %>% 
  select(anno, codigo_entidad, 
         `Número de proyectos terminados`,
         `Número de proyectos contratados`, 
         `Número de proyectos sin contratar`) %>% 
  pivot_longer(
    cols = c(`Número de proyectos terminados`,
             `Número de proyectos contratados`, 
             `Número de proyectos sin contratar`),
    names_to = "rubro", values_to = "valor") %>%
  filter(!is.na(valor), valor != 0,
         anno >= 2014) %>%
  group_by(anno, rubro) %>% 
  summarise(num_prom = mean(valor, na.rm = TRUE),
            num_sd = sd(valor, na.rm = TRUE)) %>% 
  ggplot(aes(x = anno,
             fill = rubro)) + 
  geom_ribbon(aes(ymin = num_prom - num_sd,
                  ymax = num_prom + num_sd),
              alpha = 0.5) +
  geom_line(aes(y = num_prom, color = rubro)) +
  facet_wrap(vars(rubro)) +
  labs(title = "Cantidad de proyectos",
       subtitle = "Departamento") +
  theme(legend.position = "none")

# Monto de proyectos
ind_finan_dpto %>% 
  select(anno, codigo_entidad, 
         `Valor de los proyectos terminados`,
         `Valor de los proyectos contratados`, 
         `Valor de los proyectos sin contratar`) %>% 
  pivot_longer(
    cols = c(`Valor de los proyectos terminados`,
             `Valor de los proyectos contratados`, 
             `Valor de los proyectos sin contratar`),
    names_to = "rubro", values_to = "valor") %>%
  filter(!is.na(valor), valor != 0) %>%
  group_by(anno, rubro) %>% 
  summarise(num_prom = mean(valor, na.rm = TRUE),
            num_sd = sd(valor, na.rm = TRUE)) %>% 
  ggplot(aes(x = anno,
             fill = rubro)) + 
  geom_ribbon(aes(ymin = num_prom - num_sd,
                  ymax = num_prom + num_sd),
              alpha = 0.5) +
  geom_line(aes(y = num_prom, color = rubro)) +
  facet_wrap(vars(rubro)) +
  labs(title = "Cantidad de proyectos",
       subtitle = "Departamento") +
  theme(legend.position = "none")

# Indicador de desempeño fiscal
ind_finan_dpto %>% 
  select(anno, codigo_entidad, 
         "Indicador de desempeño fiscal") %>% 
  pivot_longer(cols = c("Indicador de desempeño fiscal"),
               names_to = "rubro", values_to = "valor") %>%
  filter(!is.na(valor), valor != 0) %>%
  ggplot(aes(x = valor)) + 
  geom_density() +
  facet_wrap(vars(anno)) +
  labs(title = "Dist. Indicador Desempeño fiscal",
       subtitle = "Departamento")

# Resumen estadístico
ind_finan_dpto %>% 
  select(anno, entidad,
         "Indicador de desempeño fiscal") %>%
  filter(anno >= 2014) %>% 
  group_by(entidad) %>% 
  summarise(media = mean(`Indicador de desempeño fiscal`,
                         na.rm = TRUE),
            desvest = sd(`Indicador de desempeño fiscal`,
                         na.rm = TRUE),
            min = min(`Indicador de desempeño fiscal`, 
                      na.rm = TRUE),
            max = max(`Indicador de desempeño fiscal`, 
                      na.rm = TRUE)) %>% 
  filter(media > 0) %>% 
  mutate(cv = desvest/media*100) %>% 
  arrange(desc(cv))

# Índice de gestión de proyectos de regalías (IGPR)
ind_finan_dpto %>% 
  select(anno, codigo_entidad, 
  "Índice de gestión de proyectos de regalías (IGPR)") %>%
  rename(igpr = 
    `Índice de gestión de proyectos de regalías (IGPR)`) %>% 
  pivot_longer(cols = "igpr", names_to = "rubro", 
               values_to = "valor") %>%
  filter(!is.na(valor), valor != 0) %>%
  ggplot(aes(x = valor)) + 
  geom_density() +
  facet_wrap(vars(anno)) +
  labs(title = "Dist. IGPR",
       subtitle = "Departamento")

# Resumen estadístico
x <- ind_finan_dpto %>% 
  select(anno, entidad,
  "Índice de gestión de proyectos de regalías (IGPR)") %>%
  rename(igpr = 
    `Índice de gestión de proyectos de regalías (IGPR)`) %>%
  filter(anno >= 2014) %>% 
  group_by(entidad) %>% 
  summarise(media = mean(igpr, na.rm = TRUE),
            desvest = sd(igpr, na.rm = TRUE),
            min = min(igpr, na.rm = TRUE),
            max = max(igpr, na.rm = TRUE)) %>% 
  filter(media > 0) %>% 
  mutate(cv = desvest/media*100) %>% 
  arrange(desc(cv))

# 3.3 Municipal ----
# Ajuste del conjunto de datos
ind_finan_mup <- ind_finan_mup %>%
  mutate(entidad = paste(departamento, 
                         entidad, sep = " - ")) %>% 
  select(-c("und_medida", "fuente", 
            "codigo_departamento", "departamento")) %>% 
  pivot_wider(names_from = "indicador",
              values_from = "valor")

# Resumen
skim(data = ind_finan_mup)

# ingresos vs gastos
ind_finan_mup %>% 
  select(anno, codigo_entidad, 
         `Ingresos totales`, `Gastos totales`) %>% 
  pivot_longer(
    cols = c("Ingresos totales", "Gastos totales"),
    names_to = "rubro", values_to = "valor") %>%
  filter(!is.na(valor), valor != 0) %>% 
  ggplot(aes(x = log(valor, base = 10), color = rubro)) + 
  facet_wrap(vars(anno)) + geom_density() +
  labs(title = "Ingresos vs gastos",
       subtitle = "Municipio")

## Top
ind_finan_mup %>% 
  select(anno, entidad, codigo_entidad,
         `Ingresos totales`, `Gastos totales`) %>% 
  pivot_longer(
    cols = c("Ingresos totales", "Gastos totales"),
    names_to = "rubro", values_to = "valor") %>%
  filter(!is.na(valor), valor != 0) %>%
  group_by(entidad, rubro) %>%
  summarise(valor = mean(valor, na.rm = TRUE)) %>%
  arrange(desc(valor)) %>% head(x = ., n = 30) %>% 
  ggplot(aes(x = valor, 
             y = fct_reorder(entidad, valor),
             fill = rubro)) + geom_col(position = "dodge2") +
  labs(title = "Top Ingresos vs gastos",
       subtitle = "Municipio")

# ingresos vs gastos per capital
ind_finan_mup %>% 
  select(anno, codigo_entidad, 
         `Ingresos totales per cápita`,
         `Gastos totales per cápita`) %>% 
  pivot_longer(
    cols = c("Ingresos totales per cápita",
             "Gastos totales per cápita"),
    names_to = "rubro", values_to = "valor") %>%
  filter(!is.na(valor), valor != 0) %>% 
  ggplot(aes(x = log(valor, base = 10), color = rubro)) + 
  facet_wrap(vars(anno)) + geom_density() +
  labs(title = "Ingresos vs gastos per capita",
       subtitle = "Municipio")

## Top
ind_finan_mup %>% 
  select(anno, entidad, codigo_entidad,
         `Ingresos totales per cápita`,
         `Gastos totales per cápita`) %>% 
  pivot_longer(
    cols = c("Ingresos totales per cápita", 
             "Gastos totales per cápita"),
    names_to = "rubro", values_to = "valor") %>%
  filter(!is.na(valor), valor != 0) %>%
  group_by(entidad, rubro) %>%
  summarise(valor = mean(valor, na.rm = TRUE)) %>%
  arrange(desc(valor)) %>% head(x = ., n = 30) %>% 
  ggplot(aes(x = valor, 
             y = fct_reorder(entidad, valor),
             fill = rubro)) + geom_col(position = "dodge2") +
  labs(title = "Top Ingresos vs gastos per capita",
       subtitle = "Municipio")

# Regalías per cápita (Valor efectivamente girado al municipio)
ind_finan_mup %>% 
  select(anno, codigo_entidad, 
         paste0("Regalías per cápita",
            " (Valor efectivamente girado al municipio)")) %>% 
  pivot_longer(
    cols = c(paste0("Regalías per cápita",
                " (Valor efectivamente girado al municipio)")),
    names_to = "rubro", values_to = "valor") %>%
  filter(!is.na(valor), valor != 0) %>% 
  ggplot(aes(x = log(valor, base = 10))) + 
  facet_wrap(vars(anno)) + geom_density() +
  labs(title = "Regalías per cápita",
       subtitle = "Municipio")

## Top
ind_finan_mup %>% 
  select(anno, entidad, codigo_entidad,
         paste0("Regalías per cápita",
            " (Valor efectivamente girado al municipio)")) %>% 
  pivot_longer(
    cols = c(paste0("Regalías per cápita",
                " (Valor efectivamente girado al municipio)")),
    names_to = "rubro", values_to = "valor") %>%
  filter(!is.na(valor), valor != 0) %>%
  group_by(entidad, rubro) %>%
  summarise(valor = mean(valor, na.rm = TRUE)) %>%
  arrange(desc(valor)) %>% head(x = ., n = 30) %>% 
  ggplot(aes(x = valor, 
             y = fct_reorder(entidad, valor))) + 
  geom_col(position = "dodge2") +
  labs(title = "Top Regalías per cápita",
       subtitle = "Municipio")

# Financiamiento
ind_finan_mup %>% 
  select(anno, codigo_entidad, 
         Financiamiento) %>% 
  pivot_longer(
    cols = Financiamiento,
    names_to = "rubro", values_to = "valor") %>%
  filter(!is.na(valor), valor != 0) %>% 
  ggplot(aes(x = log(valor, base = 10))) + 
  facet_wrap(vars(anno)) + geom_density() +
  labs(title = "Financiamiento",
       subtitle = "Municipio")

## Top
ind_finan_mup %>% 
  select(anno, entidad, codigo_entidad,
         Financiamiento) %>% 
  pivot_longer(
    cols = Financiamiento,
    names_to = "rubro", values_to = "valor") %>%
  filter(!is.na(valor), valor != 0) %>%
  group_by(entidad, rubro) %>%
  summarise(valor = mean(valor, na.rm = TRUE)) %>%
  arrange(desc(valor)) %>% head(x = ., n = 30) %>% 
  ggplot(aes(x = valor, 
             y = fct_reorder(entidad, valor))) + 
  geom_col(position = "dodge2") +
  labs(title = "Top Financiamiento",
       subtitle = "Municipio")

# Total de recursos asignados per cápita
ind_finan_mup %>% 
  select(anno, codigo_entidad, 
         paste0("Total de recursos asignados",
                " per cápita - SGR (Bienio)"),
         `Total de recursos asignados per cápita - SGP`) %>% 
  pivot_longer(
    cols = c(paste0("Total de recursos asignados",
                    " per cápita - SGR (Bienio)"),
             `Total de recursos asignados per cápita - SGP`),
    names_to = "rubro", values_to = "valor") %>%
  filter(!is.na(valor), valor != 0) %>% 
  ggplot(aes(x = log(valor, base = 10), color = rubro)) + 
  facet_wrap(vars(anno)) + geom_density() +
  labs(title = "Total de recursos asignados per cápita",
       subtitle = "Municipio")

# Inversion salud vs inversion en educación
ind_finan_mup %>% 
  select(anno, codigo_entidad, 
         `Inversión - Educación`,
         `Inversión - Salud`) %>% 
  pivot_longer(
    cols = c("Inversión - Educación",
             "Inversión - Salud"),
    names_to = "rubro", values_to = "valor") %>%
  filter(!is.na(valor), valor != 0) %>% 
  ggplot(aes(x = log(valor, base = 10),
             y = rubro,
             color = rubro)) + 
  facet_wrap(vars(anno)) + geom_boxplot() +
  labs(title = "Inversion salud vs inversion en educación",
       subtitle = "Municipio") +
  theme(legend.position = "none")

# Inversion salud vs inversion en educación
ind_finan_mup %>% 
  select(anno, codigo_entidad, 
         `Índice de gestión de proyectos de regalías (IGPR)`,
         `Componente - Eficacia`, `Componente - Eficiencia`,
         `Componente - Sin medidas del SMSCE`,
         `Componente - Transparencia`) %>% 
  pivot_longer(
    cols = c(`Índice de gestión de proyectos de regalías (IGPR)`,
             `Componente - Eficacia`, `Componente - Eficiencia`,
             `Componente - Sin medidas del SMSCE`,
             `Componente - Transparencia`),
    names_to = "rubro", values_to = "valor") %>%
  filter(!is.na(valor), valor != 0) %>% 
  ggplot(aes(x = valor,
             y = rubro,
             color = rubro)) + 
  facet_wrap(vars(anno)) + geom_boxplot() +
  labs(title = "Indicadores SGR",
       subtitle = "Municipio") +
  theme(legend.position = "none")

# Cantidad de proyectos
ind_finan_mup %>% 
  select(anno, codigo_entidad, 
         `Número de proyectos terminados`,
         `Número de proyectos contratados`, 
         `Número de proyectos sin contratar`) %>% 
  pivot_longer(
    cols = c(`Número de proyectos terminados`,
             `Número de proyectos contratados`, 
             `Número de proyectos sin contratar`),
    names_to = "rubro", values_to = "valor") %>%
  filter(!is.na(valor), valor != 0,
         anno >= 2013) %>%
  group_by(anno, rubro) %>% 
  summarise(num_prom = mean(valor, na.rm = TRUE),
            num_sd = sd(valor, na.rm = TRUE)) %>% 
  ggplot(aes(x = anno,
             fill = rubro)) + 
  geom_ribbon(aes(ymin = num_prom - num_sd,
                  ymax = num_prom + num_sd),
              alpha = 0.5) +
  geom_line(aes(y = num_prom, color = rubro)) +
  facet_wrap(vars(rubro)) +
  labs(title = "Cantidad de proyectos",
       subtitle = "Municipio") +
  theme(legend.position = "none")

# Monto de proyectos
ind_finan_mup %>% 
  select(anno, codigo_entidad, 
         `Valor de los proyectos terminados`,
         `Valor de los proyectos contratados`, 
         `Valor de los proyectos sin contratar`) %>% 
  pivot_longer(
    cols = c(`Valor de los proyectos terminados`,
             `Valor de los proyectos contratados`, 
             `Valor de los proyectos sin contratar`),
    names_to = "rubro", values_to = "valor") %>%
  filter(!is.na(valor), valor != 0,
         anno >= 2013) %>%
  group_by(anno, rubro) %>% 
  summarise(num_prom = mean(valor, na.rm = TRUE),
            num_sd = sd(valor, na.rm = TRUE)) %>% 
  ggplot(aes(x = anno,
             fill = rubro)) + 
  geom_ribbon(aes(ymin = num_prom - num_sd,
                  ymax = num_prom + num_sd),
              alpha = 0.5) +
  geom_line(aes(y = num_prom, color = rubro)) +
  facet_wrap(vars(rubro)) +
  labs(title = "Valor de proyectos",
       subtitle = "Municipio") +
  theme(legend.position = "none")


# Indicador de desempeño fiscal
ind_finan_mup %>% 
  select(anno, codigo_entidad, 
         "Indicador de desempeño fiscal") %>% 
  pivot_longer(cols = c("Indicador de desempeño fiscal"),
           names_to = "rubro", values_to = "valor") %>%
  filter(!is.na(valor), valor != 0) %>%
  ggplot(aes(x = valor)) + 
  geom_density() +
  facet_wrap(vars(anno)) +
  labs(title = "Dist. Indicador Desempeño fiscal",
       subtitle = "Municipio")

# Resumen estadístico
ind_finan_mup %>% 
  select(anno, entidad,
         "Indicador de desempeño fiscal") %>%
  filter(anno >= 2014) %>% 
  group_by(entidad) %>% 
  summarise(media = mean(`Indicador de desempeño fiscal`,
                            na.rm = TRUE),
            desvest = sd(`Indicador de desempeño fiscal`,
                        na.rm = TRUE),
            min = min(`Indicador de desempeño fiscal`, 
                      na.rm = TRUE),
            max = max(`Indicador de desempeño fiscal`, 
                      na.rm = TRUE)) %>% 
  filter(media > 0) %>% 
  mutate(cv = desvest/media*100) %>% 
  arrange(desc(cv))

# Índice de gestión de proyectos de regalías (IGPR)
ind_finan_mup %>% 
  select(anno, codigo_entidad, 
    "Índice de gestión de proyectos de regalías (IGPR)") %>%
  rename(igpr = 
    `Índice de gestión de proyectos de regalías (IGPR)`) %>% 
  pivot_longer(cols = "igpr", names_to = "rubro", 
               values_to = "valor") %>%
  filter(!is.na(valor), valor != 0) %>%
  ggplot(aes(x = valor)) + 
  geom_density() +
  facet_wrap(vars(anno)) +
  labs(title = "Dist. IGPR",
       subtitle = "Municipio")

# Resumen estadístico
ind_finan_mup %>% 
  select(anno, entidad, `Número total de proyectos`,
    "Índice de gestión de proyectos de regalías (IGPR)") %>%
  rename(igpr = 
    `Índice de gestión de proyectos de regalías (IGPR)`) %>%
  filter(anno >= 2014) %>% 
  group_by(entidad) %>% 
  summarise(media_np = mean(`Número total de proyectos`,
                         na.rm = TRUE),
            media = mean(igpr, na.rm = TRUE),
            desvest = sd(igpr, na.rm = TRUE),
            min = min(igpr, na.rm = TRUE),
            max = max(igpr, na.rm = TRUE)) %>% 
  filter(media > 0) %>% 
  mutate(cv = desvest/media*100) %>% 
  arrange(desc(cv))

# Número de proyectos de regalias
ind_finan_mup %>% 
  select("anno","Número total de proyectos") %>%
  filter(!is.na(`Número total de proyectos`)) %>% 
  ggplot(aes(x = `Número total de proyectos`)) +
  facet_wrap(vars(anno)) + geom_density()
  

# Categoria distribución porcentual de asignaciones
ind_finan_mup %>% 
  select(anno, codigo_entidad,"ppa - Salud", 
        "ppa - Agua potable", "ppa - Educación",
        "ppa - Propósito general", "ppa - Alimentación escolar",
        "ppa - Ribereños", "ppa - Resguardos indígenas",
        "ppa - Primera infancia") %>%
  pivot_longer(cols = c("ppa - Salud", 
        "ppa - Agua potable", "ppa - Educación",
        "ppa - Propósito general", "ppa - Alimentación escolar",
        "ppa - Ribereños", "ppa - Resguardos indígenas",
        "ppa - Primera infancia"), 
        names_to = "rubro", values_to = "valor") %>%
  filter(!is.na(valor), valor != 0, anno < 2020) %>%
  ggplot(aes(x = valor, color = rubro)) + 
  geom_density() +
  facet_wrap(vars(anno)) +
  labs(title = "Dist. de asignaciones",
       subtitle = "Municipio")

# Resumen estadístico
skim(ind_finan_mup %>% 
  select(anno, "ppa - Salud", "ppa - Primera infancia", 
         "ppa - Educación", "ppa - Propósito general", 
         "ppa - Alimentación escolar", "ppa - Ribereños", 
         "ppa - Resguardos indígenas", "ppa - Agua potable"))

# 4. Conclusión ----