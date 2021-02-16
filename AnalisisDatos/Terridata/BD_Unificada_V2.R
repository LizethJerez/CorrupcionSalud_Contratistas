# BASE DE DATOS UNIFICADA

# Librerias 
library(tidyverse)

## Leer daatos ----
economiatd<- read.csv2("C:\\Users\\Yerazo\\Documents\\Documents\\SIGMAP\\Proyecto\\TerriData_Economia.txt", 
                       header = TRUE, encoding="UTF-8", dec = ".")
indicadores<- read.csv2("C:\\Users\\Yerazo\\Documents\\Documents\\SIGMAP\\Proyecto\\Indicadores-Corrupcion-Consolidado.csv",
                        header = TRUE, encoding="UTF-8")

## Ajustes ----
## Econom?a ----
### Ajustar los nombres de la base de datos de econom?a
economiatd<- economiatd %>%
  rename(cod_departamento = "C?digo.Departamento", 
         departamento = "Departamento", 
         cod_entidad = "C?digo.Entidad", 
         entidad = "Entidad", 
         dimension = "Dimensi?n", 
         subcategoria = "Subcategor?a", 
         indicador = "Indicador", 
         dato_numerico = "Dato.Num?rico", 
         dato_cualitativo = "Dato.Cualitativo", 
         anno = "A?o", 
         mes = "Mes", 
         fuente = "Fuente", 
         unidad_medida = "Unidad.de.Medida")
### Filtros
economiatd<- economiatd[!is.na(economiatd$dato_numerico),]
economiatd<- economiatd %>% filter(anno >=2014)
economiatd<- economiatd %>% filter(departamento != "Colombia")
### Generar una nueva columna con el nombre del departamento-municipio
economiatd<- unite(economiatd, depto_mun, c("departamento", "entidad"), sep = "-", remove = FALSE)

## Indicadores ----
### Ajustar nombres de algunas entidades territoriales para que coincidad en ambas BDs
indicadores<-as.data.table(indicadores)
indicadores[departamento_entidad=="Bogot? D.C." , departamento_entidad:="Bogot?"]
indicadores[departamento_entidad=="Bogot?" , municipio_entidad:="Bogot?"]
indicadores[departamento_entidad=="Norte De Santander", departamento_entidad:="Norte de Santander"]
### Generar una nueva columna con el nombre del departamento-municipio
indicadores<- unite(indicadores, depto_mun, c("departamento_entidad", "municipio_entidad"), sep = "-", remove = FALSE)

## Ajuste manual de registros cr?ticos
### Reemplazar los datos de indicadores como los nombres de economiatd
### (se hace as? para que no corran algunas l?neas de c?digo y no tengan que crear la tabla municipios_general)
### caso especial: No existe en econom?atd Sucre-Sinc? , Bol?var-Santa Rosa de Lima, 
indicadores[depto_mun=="Cauca-Paez/Belalcazar" , municipio_entidad:="Pa?z"]
indicadores[depto_mun=="Boyac?-Ch?quiza (San Pedro de Iguaque)" , municipio_entidad:="Ch?quiza"]
indicadores[depto_mun=="Cesar-Manaure Balcon del Cesar" , municipio_entidad:="Manaure"]
indicadores[depto_mun=="Cauca-L?pez de Micay" , municipio_entidad:="L?pez"]
indicadores[depto_mun=="Huila-El Pital" , municipio_entidad:="Pital"]
indicadores[depto_mun=="Antioquia-Carolina del Pr?ncipe" , municipio_entidad:="Carolina"]
indicadores[depto_mun=="Nari?o-Cuaspud/Carlosama" , municipio_entidad:="Cuaspud"]
indicadores[depto_mun=="Norte de Santander-San Jos? de C?cuta" , municipio_entidad:="C?cuta"]
indicadores[depto_mun=="Tolima-El Espinal" , municipio_entidad:="Espinal"]
indicadores[depto_mun=="Putumayo-San Miguel (La Dorada)" , municipio_entidad:="San Miguel"]
indicadores[depto_mun=="Antioquia-El Pe?ol" , municipio_entidad:="Pe?ol"]
indicadores[depto_mun=="Putumayo-San Miguel de Mocoa" , municipio_entidad:="Mocoa"]
indicadores[depto_mun=="Sucre-Toluviejo" , municipio_entidad:="Tol? Viejo"]
indicadores[depto_mun=="Boyac?-Gameza" , municipio_entidad:="G?meza"]
indicadores[depto_mun=="Bol?var-San Estanislao de Kostka" , municipio_entidad:="San Estanislao"]
indicadores[depto_mun=="Nari?o-Roberto Pay?n/San Jos?" , municipio_entidad:="Roberto Pay?n"]
indicadores[depto_mun=="Putumayo-Valle del Guamuez/La Hormiga" , municipio_entidad:="Valle del Guamuez"]
indicadores[depto_mun=="Tolima-San Sebastian de Mariquita" , municipio_entidad:="San Sebasti?n de Mariquita"]
indicadores[depto_mun=="Cundinamarca-Caqueza" , municipio_entidad:="C?queza"]
indicadores[depto_mun=="Tolima-El Guamo" , municipio_entidad:="Guamo"]
indicadores[depto_mun=="Tolima-El L?bano" , municipio_entidad:="L?bano"]
indicadores[depto_mun=="Sucre-Coloso" , municipio_entidad:="Colos?"]
indicadores[depto_mun=="Cauca-Toribio" , municipio_entidad:="Torib?o"]
indicadores[depto_mun=="Nari?o-San Andres de Tumaco" , municipio_entidad:="San Andr?s de Tumaco"]
indicadores[depto_mun=="Antioquia-San Pedro de los Milagros" , municipio_entidad:="San Pedro de Los Milagros"]
indicadores[depto_mun=="Antioquia-Anza" , municipio_entidad:="Anz?"]
indicadores[depto_mun=="Nari?o-Consaca" , municipio_entidad:="Consac?"]
indicadores[depto_mun=="Quind?o-Calarca" , municipio_entidad:="Calarc?"]
indicadores[depto_mun=="Antioquia-Yolombo" , municipio_entidad:="Yolomb?"]
indicadores[depto_mun=="Santander-Lebr?ja" , municipio_entidad:="Lebrija"]
indicadores[depto_mun=="Cundinamarca-Fomeque" , municipio_entidad:="F?meque"]
indicadores[depto_mun=="Valle del Cauca-Jamundi" , municipio_entidad:="Jamund?"]
indicadores[depto_mun=="Norte de Santander-Abrego" , municipio_entidad:="?brego"]
indicadores[depto_mun=="Atl?ntico-Campo de la Cruz" , municipio_entidad:="Campo de La Cruz"]
indicadores[depto_mun=="Antioquia-San Pedro de Uraba" , municipio_entidad:="San Pedro de Urab?"]
indicadores[depto_mun=="La Guajira-La Jagua Del Pilar" , municipio_entidad:="La Jagua del Pilar"]
indicadores[depto_mun=="Caquet?-Bel?n de Los Andaquies" , municipio_entidad:="Bel?n de Los Andaqu?es"]
indicadores[depto_mun=="Magdalena-Sabanas de San Angel" , municipio_entidad:="Sabanas de San ?ngel"]
indicadores[depto_mun=="Antioquia-San Andr?s de Cuerquia" , municipio_entidad:="San Andr?s de Cuerqu?a"]
indicadores[depto_mun=="Antioquia-San Jos? de la Monta?a" , municipio_entidad:="San Jos? de La Monta?a"]

### Reemplazr la columna con el nombre del departamento-municipio
indicadores$depto_mun<-NULL
indicadores<- unite(indicadores, depto_mun, c("departamento_entidad", "municipio_entidad"), sep = "-", remove = FALSE)


## Extraer datos ---- 
### Departamentos
### Separar datos por departamentos y pasar los indicadores a columnas
Economia_depto <- economiatd %>% 
  filter( subcategoria=="PIB" | subcategoria=="PIB por grandes ramas de actividad econ?mica" | 
            subcategoria=="Porcentaje del PIB por grandes ramas de actividad econ?mica", 
          departamento == entidad, departamento != "Colombia", departamento!= "Bogot?") %>% 
  group_by(depto_mun, cod_departamento, departamento, cod_entidad, entidad, dimension,indicador) %>%
  summarise(dato=mean(dato_numerico))  %>% pivot_wider(names_from = indicador, values_from=dato)
### Unir indicadores general con los indicadores departamentales (recordar problema con San Andr?s y Bogot?)
indicadores_BD<- merge(indicadores, Economia_depto, by.x= "departamento_entidad", by.y= "departamento", all.x=TRUE)
### Eliminar columnas inncesarias despu?s del merge
indicadores_BD$depto_mun.y<- NULL
indicadores_BD$dimension<- NULL
indicadores_BD$cod_departamento<-NULL
indicadores_BD$cod_entidad<-NULL
indicadores_BD$entidad<-NULL

### Municipios
### Separar datos por municipios y pasar los indicadores a columnas
Economia_municipio <- economiatd %>%
  filter( economiatd$depto_mun %in% indicadores$depto_mun & subcategoria=="Valor agregado - Base 2015") %>% 
  group_by(depto_mun,cod_entidad, indicador) %>% summarise(dato=mean(dato_numerico)) %>% 
  pivot_wider(names_from = indicador, values_from=dato)

### Unir indicadores_BD general con los indicadores municipio (recordar problema con San Andr?s y Bogot?)
indicadores_BD<- merge(indicadores_BD, Economia_municipio, by.x="depto_mun.x", by.y="depto_mun",all.x= FALSE,  all.y=TRUE)
### Se dejaron 746 municipios***

### Cambiar los NAs por la media de su columna
indicadores_BD<-as.data.frame(indicadores_BD)
columnas_numericas <- which(sapply(indicadores_BD, is.numeric))
cols_mean <- rep(NA, ncol(indicadores_BD))
cols_mean[columnas_numericas] <- colMeans(indicadores_BD[, columnas_numericas], na.rm = TRUE)

for (x in columnas_numericas) {
  indicadores_BD[is.na(indicadores_BD[,x]), x] <- cols_mean[x]
}


write.csv(indicadores_BD, file = "indicadores_BD.csv", row.names = TRUE)
