#--------------------------------------
#Librerias
#--------------------------------------

library(rakeR)
library(RPostgres)
library(DBI)
library(dplyr)
library(sf)
library(ggplot2)

#-------------------------------------
## Entradas
#-------------------------------------

# casen en formato .rds

ruta_casen = "data/casen_rm.rds"
ruta_censo = "data/cons_censo_df.rds"

casen_raw = readRDS(ruta_casen)
cons_censo_df = readRDS(ruta_censo)


#--------------------------------------
#pre procesamiento
#--------------------------------------

## datos CENSO
# Extraemos los nombres de las columnas que nos son útiles
col_cons = sort(setdiff(names(cons_censo_df), c("GEOCODIGO", "COMUNA")))


# Generamos los niveles para edad, escolaridad y sexo

age_levels = grep("^edad", col_cons, value = TRUE)
esc_levels = grep("^esco", col_cons, value = TRUE)
sexo_levels = grep("^sexo", col_cons, value = TRUE)

## datos CASEN 
# ingenieria de dagtos para re codificar y obtener la variable de interes: 

# Se seleccionan las variables de interés

vars_base = c("estrato", # Para extraer comuna
              "esc", # Para años de escolaridad 
              "edad",
              "sexo",
              "e6a",
              "s28") # s28= variable a microsimular

# Se filtra la CASEN con las variables de interés
casen = casen_raw[, vars_base, drop = FALSE]


# Limpiar memoria
# Importante para objetos muy grandes
rm(casen_raw)


# Extraemos la comuna para cada registro
casen$Comuna = substr(as.character(casen$estrato), 1, 5)

# Se elimina la columna estrato
casen$estrato = NULL


# Quitar etiquetas haven y cambiar tipo de datos
casen$esc = as.integer(unclass(casen$esc))
casen$edad = as.integer(unclass(casen$edad))
casen$e6a = as.numeric(unclass(casen$e6a))
casen$sexo = as.integer(unclass(casen$sexo))
casen$s28 = as.integer(unclass(casen$s28))


#  Crear variable binaria de hipertensión
casen <- casen %>%
  mutate(
    hipertension = case_when(
      s28 == 1 ~ 1,        # Tratamiento por hipertensión arterial
      s28 > 1 ~ 0,         # Otros tratamientos médicos
      s28 == -88 ~ NA_real_  # No responde / no aplica
    )
  )

# Imputación lineal de esc en base a e6a
idx_na = which(is.na(casen$esc))

# Ajustar modelo con casos en donde no hay na's
fit = lm(esc ~ e6a, data = casen[-idx_na,])

# Predicción para los casos con NA
pred = predict(fit, newdata = casen[idx_na, ,drop = FALSE])

# Imputar acotada
casen$esc[idx_na] = as.integer(round(pmax(0, pmin(29, pred))))


# Añadimos a un ID único
casen$ID = as.character(seq_len(nrow(casen)))

#Filtrar casos válidos
casen <- casen %>% filter(!is.na(hipertension))

## re-codificación

# Categorización de edad
casen$edad_cat = cut(
  casen$edad,
  breaks = c(0,30,40,50,60,70,80,Inf),
  labels = age_levels,
  right = FALSE, include.lowest = TRUE
)

# Categorización de Escolaridad
casen$esc_cat = factor(
  with(casen,
       ifelse(esc == 0, esc_levels[1],
              ifelse(esc <= 8, esc_levels[2],
                     ifelse(esc <= 12, esc_levels[3],
                            esc_levels[4])))),
  levels = esc_levels
)

# Recodificación de sexo
casen$sexo_cat = factor(
  ifelse(casen$sexo == 2, sexo_levels[1],
         ifelse(casen$sexo == 1, sexo_levels[2], NA)),
  levels = sexo_levels
)


#-----------------------------------------
# microsimulación
#-----------------------------------------

# crear la lista de constraints POR COMUNA
cons_censo_comunas = split(cons_censo_df, cons_censo_df$COMUNA)

# Lista de INDS 
inds_list = split(casen, casen$Comuna)

sim_list = lapply(names(cons_censo_comunas), function(zona) {
  cons_i    = cons_censo_comunas[[zona]]
  col_order = sort(setdiff(names(cons_i), c("COMUNA","GEOCODIGO")))
  cons_i    = cons_i[, c("GEOCODIGO", col_order), drop = FALSE]
  
  tmp    = inds_list[[zona]]
  inds_i = tmp[, c("ID","edad_cat","esc_cat","sexo_cat"), drop = FALSE]
  names(inds_i) = c("ID","Edad","Escolaridad","Sexo")
  
  
  
  w_frac  = weight(cons = cons_i, inds = inds_i,
                   vars = c("Edad","Escolaridad","Sexo"))
  sim_i   = integerise(weights = w_frac, inds = inds_i, seed = 123)
  merge(sim_i,
        tmp[, c("ID","hipertension")],
        by = "ID", all.x = TRUE)
})

# Data Frame de toda la población
sim_df = data.table::rbindlist(sim_list, idcol = "COMUNA")


# Se agregan los datos
zonas_hipert = aggregate(
  hipertension ~ zone,
  data = sim_df,
  FUN = function(x) mean(x, na.rm = TRUE) * 100
)
names(zonas_hipert) <- c("geocodigo", "porc_hipertension")

#--------------------------------------------------
# conexion a base de datos
#--------------------------------------------------

db_host = "localhost"
db_port = 5432
db_name = "censo_rm_clase"
db_user = "postgres"
db_password = "postgres"

# Conexión
con = dbConnect(
  Postgres(),
  dbname   = db_name,
  host     = db_host,
  port     = db_port,
  user     = db_user,
  password = db_password
)

# Escribimos la tabla dentro de la BD

dbWriteTable(
  con = ,
  name = DBI::SQL("output.zonas_hipert_tmp"),
  value = zonas_hipert,
  row.names = FALSE
)

# leer zonas censales

query_gs = "SELECT *
FROM dpa.zonas_censales_rm
WHERE urbano = 1 AND (
      nom_provin = 'SANTIAGO' OR
	  nom_comuna IN ('PUENTE ALTO', 'SAN BERNARDO')
) "

zonas_gs = st_read(con, query = query_gs)

# ajuste del tipo de dato a character

zonas_gs$geocodigo = as.character(zonas_gs$geocodigo)

# unimos las zonas gs e hipert por medio de geocódigo

zonas_gs_hipert = left_join(zonas_gs, zonas_hipert, by = "geocodigo")

#----------------------------------------------
# visualización de los resultados obtenidos
#----------------------------------------------

ggplot(zonas_gs_hipert) +
  geom_sf(aes(fill = porc_hipertension), color = NA) +
  scale_fill_viridis_c(
    option = "plasma",
    name = "% hipertensión",
    direction = -1
  ) +
  labs(
    title = "Prevalencia de hipertensión arterial en la RM",
    subtitle = "Microsimulación basada en CASEN y Censo",
    caption = "Fuente: elaboración propia a partir de CASEN y CENSO"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

