# =============================================================================
# 05_analisis_mip.R
# Modelo de Leontief: coeficientes técnicos, inversa de Leontief,
# multiplicadores y encadenamientos.
# =============================================================================

# -----------------------------------------------------------------------------
# Coeficientes directos e inversa de Leontief
# -----------------------------------------------------------------------------

#' Matriz de coeficientes técnicos (requerimientos directos)
#'
#' a_ij = z_ij / x_j: insumo doméstico i por peso de producción del sector j.
#'
#' @param Z Matriz de transacciones intermedias domésticas (n x n).
#' @param x Vector de valor bruto de producción (n).
#' @return Matriz A (n x n).
coeficientes_tecnicos <- function(Z, x) {            # Abre la función; recibe Z y el VBP
  stopifnot(nrow(Z) == ncol(Z),                      # Verifica que Z sea cuadrada...
            ncol(Z) == length(x))                    # ...y que haya un VBP por columna
  sweep(Z, 2, ifelse(x == 0, 1, x), "/")             # Divide cada columna por su VBP: A = Z diag(x)^-1
}                                                    # Cierra la función coeficientes_tecnicos

#' Inversa de Leontief (requerimientos directos e indirectos)
#'
#' L = (I - A)^{-1}. Verifica la condición de Hawkins-Simon (radio espectral
#' de A menor que 1), que garantiza que L existe y es no negativa.
#'
#' @param A Matriz de coeficientes técnicos.
#' @return Matriz L.
inversa_leontief <- function(A) {                    # Abre la función; recibe la matriz A
  rho <- max(Mod(eigen(A, only.values = TRUE)$values)) # Calcula el radio espectral de A
  if (rho >= 1) {                                    # Si no se cumple Hawkins-Simon...
    stop(sprintf("No se cumple Hawkins-Simon: radio espectral = %.4f", rho)) # ...detiene con el valor obtenido
  }                                                  # Cierra el control de Hawkins-Simon
  I <- diag(nrow(A))                                 # Crea la matriz identidad del mismo orden que A
  L <- solve(I - A)                                  # Invierte la matriz de Leontief: L = (I - A)^-1
  dimnames(L) <- dimnames(A)                         # Rotula L con los mismos sectores que A
  if (min(L) < -1e-10) {                             # Si aparece algún elemento negativo (no debería)...
    warning("La inversa de Leontief tiene elementos negativos.") # ...emite una advertencia
  }                                                  # Cierra el control de no negatividad
  stopifnot(max(abs((I - A) %*% L - I)) < 1e-8)      # Verifica numéricamente que (I - A) L = I
  L                                                  # Devuelve la inversa de Leontief
}                                                    # Cierra la función inversa_leontief

# -----------------------------------------------------------------------------
# Multiplicadores
# -----------------------------------------------------------------------------

#' Multiplicadores del modelo abierto (tipo I)
#'
#' - Producción: 1'L (VBP total por peso de demanda final del sector j).
#' - Valor agregado: v'L, con v_j = VAB_j / x_j.
#' - Importaciones: m'L; impuestos sobre productos: t'L.
#'   En una MIP de flujos domésticos, (v + m + t)'L = 1' (salvo redondeo).
#' - Empleo (opcional): e'L, con e_j = empleo_j / x_j; tipo I = e'L / e.
#'
#' @param mip Objeto "mip".
#' @param L Inversa de Leontief.
#' @param empleo Vector opcional de puestos de trabajo por sector.
#' @return data.frame por sector.
multiplicadores <- function(mip, L, empleo = NULL) { # Abre la función; el empleo es opcional
  n <- length(mip$x)                                 # Cantidad de sectores
  pr <- mip$primarios[, seq_len(n), drop = FALSE]    # Insumos primarios de los sectores (sin usos finales)
  coef <- sweep(pr, 2, ifelse(mip$x == 0, 1, mip$x), "/") # Coeficientes de insumos primarios por peso de VBP
  out <- data.frame(                                 # Arma la tabla de multiplicadores:
    sector = names(mip$x),                           #   nombre del sector
    produccion = colSums(L),                         #   multiplicador de producción: suma de cada columna de L
    valor_agregado = as.vector(coef["Valor agregado bruto pb", ] %*% L), # multiplicador de VAB: v'L
    importaciones = as.vector(coef["Importaciones", ] %*% L),            # multiplicador de importaciones: m'L
    impuestos = as.vector(coef["Impuestos netos sobre productos", ] %*% L), # multiplicador de impuestos: t'L
    stringsAsFactors = FALSE)                        #   texto como caracteres
  out$control_suma_1 <- out$valor_agregado +         # Control: VAB + importaciones + impuestos...
    out$importaciones + out$impuestos                # ...debe sumar 1 en cada sector
  if (!is.null(empleo)) {                            # Si se proporcionó el empleo por sector...
    e <- .div0(empleo, mip$x)                        # ...calcula los requerimientos directos de empleo por peso de VBP
    out$empleo <- as.vector(e %*% L)                 # ...multiplicador de empleo: e'L
    out$empleo_tipo_I <- .div0(out$empleo, e)        # ...multiplicador tipo I: empleo total / empleo directo
  }                                                  # Cierra el condicional del empleo
  rownames(out) <- NULL                              # Elimina los nombres de fila heredados
  out                                                # Devuelve la tabla de multiplicadores
}                                                    # Cierra la función multiplicadores

# -----------------------------------------------------------------------------
# Encadenamientos
# -----------------------------------------------------------------------------

#' Encadenamientos de Rasmussen-Hirschman y versión de Ghosh
#'
#' BL_j = (1'L)_j / (1'L1 / n)          hacia atrás (comprador)
#' FL_i = (L1)_i / (1'L1 / n)           hacia adelante con Leontief
#' FLG_i = (G1)_i / (1'G1 / n)          hacia adelante con Ghosh,
#'   G = (I - B)^{-1}, B = x^-1 Z (coeficientes de distribución)
#' CV_j: coeficiente de variación de la columna j de L (dispersión del
#'   arrastre; valores bajos indican un efecto repartido entre sectores).
#'
#' Tipología (Hirschman): clave (BL>1, FL>1), impulsor (BL>1, FL<1),
#' base (BL<1, FL>1), independiente (BL<1, FL<1). Se clasifica con FLG.
#'
#' @param mip Objeto "mip".
#' @param L Inversa de Leontief.
#' @return data.frame por sector.
encadenamientos <- function(mip, L) {                # Abre la función; recibe la MIP y su inversa
  n <- nrow(L)                                       # Cantidad de sectores
  x <- mip$x                                         # Valor bruto de producción
  B <- sweep(mip$Z, 1, ifelse(x == 0, 1, x), "/")    # Coeficientes de distribución: B = diag(x)^-1 Z (divide filas)
  G <- solve(diag(n) - B)                            # Inversa de Ghosh: G = (I - B)^-1
  bl <- colSums(L) / (sum(L) / n)                    # Encadenamiento hacia atrás: suma de columna / promedio
  fl <- rowSums(L) / (sum(L) / n)                    # Hacia adelante con Leontief: suma de fila / promedio
  flg <- rowSums(G) / (sum(G) / n)                   # Hacia adelante con Ghosh: suma de fila de G / promedio
  cv <- apply(L, 2, function(col) {                  # Recorre cada columna de L para medir su dispersión:
    stats::sd(col) / mean(col)                       #   coeficiente de variación = desvío / media
  })                                                 # Cierra el cálculo del coeficiente de variación
  tipo <- ifelse(bl >= 1 & flg >= 1, "clave",        # Ambos encadenamientos altos: sector clave
          ifelse(bl >= 1, "impulsor",                # Solo hacia atrás alto: impulsor de la demanda
          ifelse(flg >= 1, "base",                   # Solo hacia adelante alto: sector base (proveedor)
                 "independiente")))                  # Ambos bajos: sector independiente
  data.frame(sector = rownames(L),                   # Arma la tabla de resultados: nombre del sector
             BL = bl,                                #   encadenamiento hacia atrás
             FL_leontief = fl,                       #   hacia adelante (Leontief)
             FL_ghosh = flg,                         #   hacia adelante (Ghosh)
             CV_columna = cv,                        #   dispersión del arrastre
             tipo = tipo,                            #   tipología de Hirschman
             row.names = NULL,                       #   sin nombres de fila
             stringsAsFactors = FALSE)               #   texto como caracteres
}                                                    # Cierra la función encadenamientos

# -----------------------------------------------------------------------------
# Análisis completo e impactos
# -----------------------------------------------------------------------------

#' Análisis completo de una MIP
#'
#' @param mip Objeto "mip".
#' @param empleo Vector opcional de puestos de trabajo por sector.
#' @return Lista con A, L, multiplicadores y encadenamientos.
analizar_mip <- function(mip, empleo = NULL) {       # Abre la función de análisis integral
  A <- coeficientes_tecnicos(mip$Z, mip$x)           # Calcula los coeficientes directos A
  L <- inversa_leontief(A)                           # Calcula los requerimientos directos e indirectos L
  list(A = A, L = L,                                 # Devuelve A y L...
       multiplicadores = multiplicadores(mip, L, empleo), # ...los multiplicadores...
       encadenamientos = encadenamientos(mip, L))    # ...y los encadenamientos
}                                                    # Cierra la función analizar_mip

#' Impacto de un shock de demanda final
#'
#' @param L Inversa de Leontief.
#' @param df Vector de variación de la demanda final (doméstica) por sector.
#' @return Vector de variación del VBP por sector.
impacto_demanda <- function(L, df) {                 # Abre la función; recibe L y el shock de demanda
  stopifnot(length(df) == ncol(L))                   # Verifica que el shock tenga un valor por sector
  dx <- as.vector(L %*% df)                          # Variación del VBP: dx = L df
  stats::setNames(dx, rownames(L))                   # Rotula el resultado con los sectores y lo devuelve
}                                                    # Cierra la función impacto_demanda
