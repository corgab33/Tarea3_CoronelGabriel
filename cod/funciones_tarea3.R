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
