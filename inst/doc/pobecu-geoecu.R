## ----include = FALSE----------------------------------------------------------
disponible <- requireNamespace("geoecu", quietly = TRUE) &&
  utils::packageVersion("geoecu") >= "0.4.1" &&
  requireNamespace("ggplot2", quietly = TRUE) && geoecu::geoecu_datos_disponibles()
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.width = 7,
                      fig.height = 5, dpi = 96, eval = disponible)

## ----eval = !disponible, echo = FALSE, results = "asis"-----------------------
# cat("> Esta viñeta necesita los paquetes geoecu y ggplot2 con las capas de",
#     "geoecu descargadas, así que su código no se ejecutó al construirla.\n")

## -----------------------------------------------------------------------------
library(pobecu)
library(ggplot2)

## -----------------------------------------------------------------------------
esmeraldas <- geoecu::read_cantones(provincia = "Esmeraldas")
nrow(esmeraldas)                    # cantones en geoecu
nrow(agrupar_unidades(esmeraldas))  # cantones del censo

## -----------------------------------------------------------------------------
cant <- pob_mapa("canton", anio = 2025)
cant <- geoecu::reubicar_galapagos(cant)
sf::st_drop_geometry(cant)[, c("canton", "poblacion", "area_km2", "densidad")]

ggplot(cant) +
  geom_sf(aes(fill = densidad), color = "white", linewidth = 0.1) +
  scale_fill_viridis_c(trans = "log10", name = "hab/km²",
                       labels = scales::label_number(big.mark = " ")) +
  labs(title = "Densidad de población por cantón, 2025",
       caption = "Fuente: INEC, Revisión 2024; límites: geoecu (CONALI)") +
  theme_void()

## -----------------------------------------------------------------------------
cuenca <- pob_mapa("parroquia", anio = 2030, canton = "Cuenca")

ggplot(cuenca) +
  geom_sf(aes(fill = poblacion), color = "white") +
  scale_fill_gradient(low = "#FFF4D2", high = "#8C3B12", name = "habitantes",
                      trans = "log10") +
  labs(title = "Cuenca: población parroquial estimada, 2030") +
  theme_void()

## -----------------------------------------------------------------------------
casos <- data.frame(dpa_canton = c("1101", "1102", "1103", "1104", "1110"),
                    anio = 2025, casos = c(130, 12, 25, 9, 6))
tasas <- pob_tasa(casos, "casos")
loja <- geoecu::geoecu_unir(tasas, codigo = "dpa_canton", nivel = "canton",
                            provincia = "Loja")

ggplot(loja) +
  geom_sf(aes(fill = tasa), color = "white") +
  scale_fill_distiller(palette = "OrRd", direction = 1, na.value = "grey90",
                       name = "por 100 000") +
  labs(title = "Tasa por 100 000 habitantes (datos de ejemplo)") +
  theme_void()

## -----------------------------------------------------------------------------
casos_e <- data.frame(dpa_canton = c("0802", "0809", "0801"),
                      anio = 2025, casos = c(7, 5, 30))
casos_e$dpa_canton <- homologar_canton(casos_e$dpa_canton)  # Borbón -> Eloy Alfaro
casos_e <- aggregate(casos ~ dpa_canton + anio, casos_e, sum)
tasas_e <- pob_tasa(casos_e, "casos")

capa <- agrupar_unidades(geoecu::read_cantones(provincia = "Esmeraldas"))
mapa_e <- merge(capa, tasas_e[, c("dpa_canton", "tasa")], all.x = TRUE)

casos_p <- data.frame(dpa_parroq = c("131250", "131251", "131252"),
                      anio = 2025, casos = c(4, 2, 1))
casos_p$dpa_parroq <- homologar_parroquia(casos_p$dpa_parroq)  # Sosote -> Rocafuerte
casos_p <- aggregate(casos ~ dpa_parroq + anio, casos_p, sum)
pob_tasa(casos_p, "casos", nivel = "parroquia")

## ----fig.height = 4-----------------------------------------------------------
mayores <- pob_mapa("provincia", anio = c(2025, 2035), edad = c(0, 65))
mayores <- geoecu::reubicar_galapagos(mayores[mayores$edad == "65+", ])

ggplot(mayores) +
  geom_sf(aes(fill = poblacion / 1000), color = "white", linewidth = 0.2) +
  facet_wrap(~anio) +
  scale_fill_viridis_c(name = "miles", option = "magma", direction = -1) +
  labs(title = "Población de 65 años y más por provincia") +
  theme_void()

