#--------------------------------------
#Librerías
#--------------------------------------

library(rakeR)
library(RPostgres)
library(DBI)
library(dplyr)
library(sf)
library(ggplot2)
library(data.table)
library(factoextra)
library(GGally)
library(cowplot)

#-------------------------------------
## Entradas
#-------------------------------------

ruta_casen = "data/casen_rm.rds"
ruta_censo = "data/cons_censo_df.rds"

casen_raw = readRDS(ruta_casen)
cons_censo_df = readRDS(ruta_censo)


#--------------------------------------
# PRE-PROCESAMIENTO
#--------------------------------------

## datos CENSO
col_cons = sort(setdiff(names(cons_censo_df), c("GEOCODIGO", "COMUNA")))

age_levels  = grep("^edad", col_cons, value = TRUE)
esc_levels  = grep("^esco", col_cons, value = TRUE)
sexo_levels = grep("^sexo", col_cons, value = TRUE)


## datos CASEN 
vars_base = c(
  "estrato",
  "esc",
  "edad",
  "sexo",
  "e6a",
  "s28",   # hipertensión
  "ypc"    # ingreso per cápita agregado
)

casen = casen_raw[, vars_base, drop = FALSE]
rm(casen_raw)

casen$Comuna = substr(as.character(casen$estrato), 1, 5)
casen$estrato = NULL

casen$esc  = as.integer(unclass(casen$esc))
casen$edad = as.integer(unclass(casen$edad))
casen$e6a  = as.numeric(unclass(casen$e6a))
casen$sexo = as.integer(unclass(casen$sexo))
casen$s28  = as.integer(unclass(casen$s28))
casen$ypc  = as.numeric(unclass(casen$ypc))

#--------------------------------------
# Sincronizar hipertensión desde s28
#--------------------------------------

casen <- casen %>%
  mutate(
    hipertension = case_when(
      s28 == 1 ~ 1,
      s28 > 1 ~ 0,
      s28 == -88 ~ NA_real_
    )
  )

# Imputación lineal de esc
idx_na = which(is.na(casen$esc))
fit = lm(esc ~ e6a, data = casen[-idx_na,])
pred = predict(fit, newdata = casen[idx_na, , drop = FALSE])
casen$esc[idx_na] = as.integer(round(pmax(0, pmin(29, pred))))

casen$ID = as.character(seq_len(nrow(casen)))
casen <- casen %>% filter(!is.na(hipertension))


#-----------------------------------------
# Re-codificación
#-----------------------------------------

casen$edad_cat = cut(
  casen$edad,
  breaks = c(0,30,40,50,60,70,80,Inf),
  labels = age_levels,
  right = FALSE,
  include.lowest = TRUE
)

casen$esc_cat = factor(
  with(casen,
       ifelse(esc == 0, esc_levels[1],
              ifelse(esc <= 8,  esc_levels[2],
                     ifelse(esc <= 12, esc_levels[3],
                            esc_levels[4])))),
  levels = esc_levels
)

casen$sexo_cat = factor(
  ifelse(casen$sexo == 2, sexo_levels[1],
         ifelse(casen$sexo == 1, sexo_levels[2], NA)),
  levels = sexo_levels
)


#-----------------------------------------
# MICROSIMULACIÓN
#-----------------------------------------

cons_censo_comunas = split(cons_censo_df, cons_censo_df$COMUNA)
inds_list = split(casen, casen$Comuna)

sim_list = lapply(names(cons_censo_comunas), function(zona) {
  
  cons_i    = cons_censo_comunas[[zona]]
  col_order = sort(setdiff(names(cons_i), c("COMUNA","GEOCODIGO")))
  cons_i    = cons_i[, c("GEOCODIGO", col_order), drop = FALSE]
  
  tmp    = inds_list[[zona]]
  inds_i = tmp[, c("ID","edad_cat","esc_cat","sexo_cat"), drop = FALSE]
  names(inds_i) = c("ID","Edad","Escolaridad","Sexo")
  
  w_frac = weight(
    cons = cons_i,
    inds = inds_i,
    vars = c("Edad","Escolaridad","Sexo")
  )
  
  sim_i = integerise(weights = w_frac, inds = inds_i, seed = 123)
  
  # ⬅︎ AQUÍ agregamos ambas variables: hipertensión + ypc
  merge(sim_i,
        tmp[, c("ID","hipertension","ypc")],
        by = "ID", all.x = TRUE)
})

# DF completo de población microsimulada
sim_df = data.table::rbindlist(sim_list, idcol = "COMUNA")


#-----------------------------------------
# AGREGADOS POR ZONA
#-----------------------------------------

zonas_hipert = sim_df %>%
  group_by(zone) %>%
  summarise(porc_hipertension = mean(hipertension, na.rm = TRUE) * 100) %>%
  rename(geocodigo = zone)

zonas_ypc = sim_df %>%
  group_by(zone) %>%
  summarise(mediana_ingreso = median(ypc, na.rm = TRUE)) %>%
  rename(geocodigo = zone)


edad_mayores_60 = c("edad_60_70", "edad_70_80", "edad_mayor_80")

zonas_edad = sim_df %>%
  mutate(es_mayor60 = ifelse(Edad %in% edad_mayores_60, 1, 0)) %>%
  group_by(zone) %>%
  summarise(porc_mayores_60 = mean(es_mayor60, na.rm = TRUE) * 100) %>%
  rename(geocodigo = zone)


#-----------------------------------------
# Conexión BD
#-----------------------------------------

con = dbConnect(
  Postgres(),
  dbname   = "censo_rm_clase",
  host     = "localhost",
  port     = 5432,
  user     = "postgres",
  password = "postgres"
)

dbWriteTable(
  con,
  name = DBI::SQL("output.zonas_hipert_fi"),
  value = zonas_hipert,
  row.names = FALSE
)

dbWriteTable(
  con,
  name = DBI::SQL("output.zonas_ypc_fi"),
  value = zonas_ypc,
  row.names = FALSE
)

dbWriteTable(
  con, name = DBI::SQL("output.zonas_edad_fi"),
  value = zonas_edad, row.names = FALSE
)
#-----------------------------------------
# Lectura zonas censales + unión
#-----------------------------------------

query_gs = "
SELECT *
FROM dpa.zonas_censales_rm
WHERE urbano = 1 AND (
      nom_provin = 'SANTIAGO' OR
      nom_comuna IN ('PUENTE ALTO', 'SAN BERNARDO')
)"

zonas_gs = st_read(con, query = query_gs)
zonas_gs$geocodigo = as.character(zonas_gs$geocodigo)

zonas_gs <- zonas_gs %>%
  left_join(zonas_hipert, by = "geocodigo") %>%
  left_join(zonas_ypc,    by = "geocodigo") %>%
  left_join(zonas_edad,   by = "geocodigo")     

# Guardar en BD
st_write(
  zonas_gs,
  dsn = con,
  layer = DBI::SQL("output.zc_hipert_ingreso_edad_microsim",
                   driver = "PostgreSQL")
)

#----------------------------------------------
# VISUALIZACIÓN (Hipertensión)
#----------------------------------------------

ggplot(zonas_gs) +
  geom_sf(aes(fill = porc_hipertension), color = NA) +
  scale_fill_viridis_c(option = "plasma", name = "% hipertensión", direction = -1) +
  labs(
    title = "Prevalencia de hipertensión arterial en la RM",
    subtitle = "Microsimulación basada en CASEN y Censo",
    caption = "Fuente: elaboración propia"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )


## -----------------------------------------
## CLUSTERING 
## -----------------------------------------


# Seleccionar variables NUMÉRICAS 
vars_clusters <- zonas_gs %>%
  st_drop_geometry() %>%
  select(porc_hipertension, mediana_ingreso, porc_mayores_60)

# Eliminar filas con NA
complete_rows <- complete.cases(vars_clusters)
vars_clusters <- vars_clusters[complete_rows, ]
zonas_gs_clust <- zonas_gs[complete_rows, ]

# Escalar
vars_scaled <- scale(vars_clusters)

#---------------------------------------------------------
# Elegir número óptimo de clusters (método del codo)
#---------------------------------------------------------
fviz_nbclust(vars_scaled, kmeans, method = "wss") +
  labs(
    title = "Método del codo",
    x = "Número de clusters",
    y = "Inercia intra-cluster (WSS)"
  )

# Ejecutar K-Means
set.seed(123)
km <- kmeans(vars_scaled, centers = 3, nstart = 20)

#------------------------------------------------------------
# Agregar resultado de clusters a la capa espacial 
#------------------------------------------------------------
zonas_gs_clust$cluster <- as.factor(km$cluster)

# Estadísticas por cluster
cluster_stats <- zonas_gs_clust %>%
  st_drop_geometry() %>%   
  group_by(cluster) %>%
  summarise(
    hipert_mean = mean(porc_hipertension),
    ingreso_median = median(mediana_ingreso),
    mayores60_mean = mean(porc_mayores_60)
  )

print(cluster_stats)

#hipertension v/s ingreso
ggplot(zonas_gs_clust, aes(x = mediana_ingreso,
                           y = porc_hipertension,
                           color = cluster)) +
  geom_point(alpha = 0.7, size = 2) +
  scale_color_viridis_d() +
  labs(
    title = "Relación entre hipertensión e ingreso por cluster",
    x = "Ingreso mediano",
    y = "% Hipertensión"
  ) +
  theme_minimal()

#hipertension v/S edad
ggplot(zonas_gs_clust, aes(x = porc_mayores_60,
                           y = porc_hipertension,
                           color = cluster)) +
  geom_point(alpha = 0.7, size = 2) +
  scale_color_viridis_d() +
  labs(
    title = "Relación entre hipertensión y proporción de mayores de 60 años",
    x = "% mayores de 60",
    y = "% Hipertensión"
  ) +
  theme_minimal()

#--------------------------------------------
# Mapa con geometría intacta
#--------------------------------------------
ggplot(zonas_gs_clust) +
  geom_sf(aes(fill = cluster), color = NA) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Clusters de Zonas Censales del Gran Santiago",
    subtitle = "Basados en hipertensión, ingreso y envejecimiento"
  ) +
  theme_void()

#--------------------------------------
# Indice de shannon
#---------------------------------------
# Crear geometría única por comuna usando st_union
comunas_sf <- zonas_gs_clust %>%
  group_by(nom_comuna) %>%
  summarise(geometry = st_union(geom)) %>%
  st_as_sf()

#indice shannon

shannon_comunas <- zonas_gs_clust %>%
  st_drop_geometry() %>% 
  group_by(nom_comuna, cluster) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(p = n / sum(n)) %>%
  group_by(nom_comuna) %>%
  summarise(shannon = -sum(p * log(p)))

#unir indice a la geometria comunal
comunas_shannon_map <- comunas_sf %>%
  left_join(shannon_comunas, by = "nom_comuna")

#mapa final indice shannon

ggplot(comunas_shannon_map) +
  geom_sf(aes(fill = shannon), color = NA) +
  scale_fill_viridis_c(option = "magma", na.value = "grey90") +
  labs(
    title = "Índice de Shannon por Comuna",
    subtitle = "Diversidad de Clusters dentro de cada Comuna",
    fill = "Shannon"
  ) +
  theme_minimal()

