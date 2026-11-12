library(dplyr)
library(purrr)

alerta.movil <- function(df, ventana, k.alerta) {
    # Recibe: un (df) con transacciones y los parámetros de v (ventana) y k (k.alerta).
    # Devuelve: un df con las columnas fecha, n.tx, media, desv y alerta
    
    calcular.mu <- function(cantidades.transacciones, posicion) {
        if (posicion <= ventana) {
            return(NA)
        }

        valores.ventana <- cantidades.transacciones[(posicion - 1):(posicion - ventana)]

        mean(valores.ventana, na.rm = TRUE)
    }

    calcular.s <- function(cantidades.transacciones, posicion) {
        if (posicion <= ventana) {
            return(NA)
        }

        valores.ventana <- cantidades.transacciones[(posicion - 1):(posicion - ventana)]

        sd(valores.ventana, na.rm = TRUE)
    }

    df |>
        summarise(n.tx = n(), .by = "fecha") |>
        mutate(
            media = imap_dbl(n.tx, \(t, posicion) calcular.mu(n.tx, posicion)),
            desv = imap_dbl(n.tx, \(t, posicion) calcular.s(n.tx, posicion)),
            alerta = n.tx > media + k.alerta * desv
        )
}

benford.mad <- function(x) {
    # Recibe: un vector de montos (x)
    # Devuelve: la desviación absoluta media

    primeros.digitos <- floor(x / 10^(floor(log10(x))))

    proporcion.benford <- function(digito) {
        log10(1 + 1/digito)
    }

    proporcion.simple <- function(digito) {
        ocurrencias <- sum(primeros.digitos == digito)
        cantidad.total <- length(primeros.digitos)

        ocurrencias / cantidad.total
    }

    1/9 * sum(map_dbl(1:9, \(d) abs(proporcion.simple(d) - proporcion.benford(d))))
}

benford.canton <- function(df, canal, cantones) {
    # Recibe: un (df) de transacciones, un (canal) a consultar y un df de (cantones)
    # Devuelve: un df con el mad de cada cantón para el canal
    # especificado, incluyendo cod_canton, n y clase de Nigrini

    df |>
        filter(canal == canal) |>
        summarise(
            n = n(),
            mad = benford.mad(monto_crc),
            clase = case_when(
                mad <= 0.006 ~ "conformidad cercana",
                (mad > 0.006 && mad <= 0.012) ~ "conformidad aceptable",
                (mad > 0.012 && mad <= 0.015) ~ "conformidad marginal",
                .default = "no conformidad"
            ),
            .by = "cod_canton"
        ) |>
        left_join(cantones) |>
        select(-provincia) |>
        arrange(desc(mad))
}
