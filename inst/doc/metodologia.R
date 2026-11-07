## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.width = 7,
                      fig.height = 4.2, dpi = 96)
library(pobecu)
fmt <- function(x, d = 0) format(round(x, d), big.mark = " ", decimal.mark = ",",
                                 nsmall = d, scientific = FALSE)

## -----------------------------------------------------------------------------
pob_fuentes()

## -----------------------------------------------------------------------------
nac <- poblacion("nacional")
plot(nac$anio, nac$poblacion / 1e6, type = "l", lwd = 2, col = "#0F2233",
     xlab = NULL, ylab = "Millones de habitantes",
     main = "Ecuador, 1950-2050 (INEC, Revisión 2024)")
abline(v = 2022.5, lty = 2, col = "grey60")
text(2023, 4, "proyección", adj = 0, col = "grey40", cex = 0.85)

## -----------------------------------------------------------------------------
table(dpa_pob$metodo)

## -----------------------------------------------------------------------------
e22 <- suppressMessages(poblacion("parroquia", anio = 2022))
cmp <- merge(e22, dpa_pob[, c("dpa_parroq", "censo_2022")], by = "dpa_parroq")
summary(cmp$poblacion / cmp$censo_2022)
plot(cmp$censo_2022, cmp$poblacion, log = "xy", pch = 16, cex = 0.5,
     col = adjustcolor("#0F2233", 0.5),
     xlab = "Censo 2022 (personas empadronadas)",
     ylab = "Estimación pobecu 2022",
     main = "Parroquias: estimación 2022 frente al censo")
abline(0, 1, col = "#F2B705", lwd = 2)

## -----------------------------------------------------------------------------
p <- suppressMessages(poblacion("parroquia", anio = c(2025, 2035)))
w <- reshape(p[, c("dpa_parroq", "anio", "poblacion")], idvar = "dpa_parroq",
             timevar = "anio", direction = "wide")
w$tasa <- 100 * ((w$poblacion.2035 / w$poblacion.2025)^(1 / 10) - 1)
hist(w$tasa, breaks = 60, col = "#F2B705", border = "white",
     xlab = "Crecimiento medio anual 2025-2035 (%)", ylab = "Parroquias",
     main = NULL)

