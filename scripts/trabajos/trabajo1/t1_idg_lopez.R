#-----------------------------------------------------------
## librerias para llamar librerias
#-----------------------------------------------------------

library(DBI)
library(RPostgres)
library(sf) 
library(ggplot2)
library(viridis)
library(cowplot)
library(biscale)


#----------------------------------------------------------
##establecer conexión usando rpostgres
#----------------------------------------------------------

con = dbConnect(
  Postgres(),
  dbname   =  "censo_v_2017",
  host     = "localhost",
  port     = 5432,
  user     = "postgres",
  password = "postgres"
)

#----------------------------------------------------------
## consulta SQL: vulnerabidiad: hacinamiento y educación
#----------------------------------------------------------

consulta_sql= "WITH agg AS 
(

SELECT 

z.geocodigo::double precision AS geocodigo,
c.nom_comuna,

 -- % de personas con hacinamiento alto o crítico (3 o 4)
        ROUND(
            COUNT(*) FILTER (WHERE v.ind_hacin IN (3,4)) * 100.0 / COUNT(*), 2
        ) AS pct_hacinamiento,
    
-- % escolaridad

ROUND(
    COUNT(*) FILTER (WHERE p.escolaridad <= 8) * 100.0 / COUNT(*), 2
    ) AS ptje_esc_baja

FROM public.personas   AS p
    JOIN public.hogares    AS h ON p.hogar_ref_id    = h.hogar_ref_id
    JOIN public.viviendas  AS v ON h.vivienda_ref_id = v.vivienda_ref_id
    JOIN public.zonas      AS z ON v.zonaloc_ref_id  = z.zonaloc_ref_id
    JOIN public.comunas    AS c ON z.codigo_comuna   = c.codigo_comuna
    JOIN public.provincias AS pr ON c.provincia_ref_id = pr.provincia_ref_id
    WHERE pr.nom_provincia = 'QUILLOTA'
    GROUP BY z.geocodigo, c.nom_comuna

)

SELECT  a.geocodigo,
    shp.geom,
    a.nom_comuna,
    a.pct_hacinamiento,
    a.ptje_esc_baja
FROM agg AS a
JOIN dpa.zonas_censales_v AS shp 
    ON shp.geocodigo = a.geocodigo;
"
df_indicadores= st_read(con, query = consulta_sql)



#----------------------------------------------------------
##seleccion comunas
#----------------------------------------------------------

sql_comunas = "

SELECT nom_comuna, geom
FROM dpa.comunas_v
WHERE nom_provin = 'QUILLOTA';

" 
sf_comunas= st_read(con, query = sql_comunas)


#--------------------------------------------------------
# EDA
#--------------------------------------------------------

ggplot(df_indicadores, aes(x = pct_hacinamiento, y = ptje_esc_baja)) +
  geom_point(color = "#1f78b4", size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
  labs(
    title = "Relación entre Hacinamiento y Baja Escolaridad - Provincia de Quillota",
    x = "% de Población con Hacinamiento Alto o Crítico",
    y = "% de Población con Baja Escolaridad (≤8 años)"
  ) +
  theme_minimal()


#----------------------------------------------------------
##mapa hacinamiento
#----------------------------------------------------------

map_hacinamiento <- ggplot(df_indicadores) +
  geom_sf(aes(fill = pct_hacinamiento), color = "gray80", size = 0.2) +
  scale_fill_viridis_c(
    option = "magma", 
    direction = -1,
    name = "% Hacinamiento alto o crítico") +
  labs(
    title = "Distribución del Hacinamiento Alto o Crítico\nProvincia de Quillota") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13, face = "bold"),
    legend.position = "right"
  )

print(map_hacinamiento)

#----------------------------------------------------------
##mapa escolaridad
#----------------------------------------------------------

map_escolaridad <- ggplot(df_indicadores) +
  geom_sf(aes(fill = ptje_esc_baja), color = "gray80", size = 0.2) +
  scale_fill_viridis_c(
    option = "plasma", 
    direction = -1,
    name = "% Baja escolaridad (≤8 años)") +
  labs(
    title = "Distribución de la Baja Escolaridad\nProvincia de Quillota") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13, face = "bold"),
    legend.position = "right"
  )

print(map_escolaridad)

#----------------------------------------------------------
##mapa bibariado
#----------------------------------------------------------

df_bi <- bi_class(
  df_indicadores,
  x = ptje_esc_baja,      # variable 1: baja escolaridad
  y = pct_hacinamiento,   # variable 2: hacinamiento
  dim = 3,                # número de clases
  style = "jenks"         # método de clasificación
)

mapa_bivariado <- ggplot() +
  geom_sf(data = df_bi, aes(fill = bi_class), color = NA) +
  geom_sf(data = sf_comunas, fill = NA, color = "black", size = 0.3) +
  bi_scale_fill(pal = "DkBlue", dim = 3) +
  labs(
    title = "Mapa Bivariado: Baja Escolaridad vs Hacinamiento\nProvincia de Quillota"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 15)
  )

leyenda_bi <- bi_legend(
  pal = "DkBlue", dim = 3,
  xlab = "← Menor escolaridad | Mayor escolaridad →",
  ylab = "← Menor hacinamiento | Mayor hacinamiento →",
  size = 8
)

mapa_final_bi <- ggdraw() +
  draw_plot(mapa_bivariado, 0, 0, 1, 1) +
  draw_plot(leyenda_bi, 0.7, 0.05, 0.25, 0.25)

print(mapa_final_bi)


