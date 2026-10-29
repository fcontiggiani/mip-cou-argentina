# =============================================================================
# 04_cou_a_mip.R
# Transformación del COU en una matriz insumo-producto simétrica de flujos
# domésticos a precios básicos.
#
# Pasos:
#   1. Utilización a precios de comprador -> precios básicos: se restan
#      impuestos y márgenes de cada producto y los márgenes se reasignan a los
#      productos que los prestan (comercio y transporte).
#   2. Separación de insumos nacionales e importados (supuesto de
#      proporcionalidad de importaciones).
#   3. Supuesto tecnológico para obtener una matriz cuadrada:
#      - Modelo D (Eurostat): rama x rama, estructura fija de ventas por
#        producto. Es el formato de la MIP argentina de 1997 y el usado por
#        Beyrne (2015) y Michelena (2023).
#      - Modelo B (Eurostat): producto x producto, tecnología de rama.
#   Ambos modelos generan coeficientes no negativos y admiten COU
#   rectangulares (más productos que ramas).
#
# Referencias: United Nations (2018), Handbook on Supply, Use and Input-Output
#   Tables..., caps. 11-12; Eurostat (2008), Manual of Supply, Use and
#   Input-Output Tables, cap. 11; Miller y Blair (2009), cap. 5.
# =============================================================================

# -----------------------------------------------------------------------------
# Función auxiliar
# -----------------------------------------------------------------------------

# División segura elemento a elemento: devuelve 0 cuando el denominador es 0
.div0 <- function(a, b) {                            # Abre la función; recibe numerador y denominador
  r <- a / b                                         # Divide elemento a elemento
  r[!is.finite(r)] <- 0                              # Reemplaza por cero los resultados de dividir por cero
  r                                                  # Devuelve el cociente
}                                                    # Cierra la función .div0

# -----------------------------------------------------------------------------
# Paso 1. Valuación a precios básicos
# -----------------------------------------------------------------------------

#' Paso 1. Valuación a precios básicos
#'
#' Supuesto: dentro de cada producto, la tasa de impuestos y la tasa de margen
#' son iguales para todos los usos (intermedios y finales). Los márgenes
#' retirados de cada columna se asignan a los productos con margen negativo
#' (los que prestan el servicio de distribución), en proporción a ese margen.
#'
#' Simplificaciones a tener presentes: el IVA no deducible recae solo sobre
#' usos finales y los derechos de exportación solo sobre las exportaciones;
#' con información adicional (cuadros de impuestos y márgenes por uso) el
#' supuesto uniforme debe reemplazarse.
#'
#' @param cou Objeto de clase "cou".
#' @return Lista con `u_pb` (productos x todos los usos), `t_mat` (impuestos
#'   sobre productos por uso) y `mg_mat` (márgenes netos por uso).
a_precios_basicos <- function(cou) {                 # Abre la función de valuación a precios básicos
  u_pc <- cbind(cou$u_int, cou$y)                    # Une usos intermedios y finales a precios de comprador
  tot_pc <- rowSums(u_pc)                            # Utilización total de cada producto a precios de comprador

  tasa_t <- .div0(cou$impuestos, tot_pc)             # Tasa de impuestos de cada producto (impuestos / uso total)
  t_mat <- u_pc * tasa_t                             # Impuestos contenidos en cada celda (fila i por su tasa)

  mg_tipos <- as.matrix(cou$margenes)                # Márgenes como matriz: una columna por tipo de margen
  mg_mat <- u_pc * 0                                 # Inicia en cero la matriz de márgenes netos por celda
  for (k in seq_len(ncol(mg_tipos))) {               # Trata cada tipo de margen por separado:
    mg_k <- mg_tipos[, k]                            #   margen de este tipo, por producto
    tasa_mg <- .div0(pmax(mg_k, 0), tot_pc)          #   tasa de margen de cada bien (margen positivo / uso total)
    mg_retirado <- u_pc * tasa_mg                    #   margen contenido en cada celda de los bienes
    mg_col <- colSums(mg_retirado)                   #   margen total que paga cada usuario (por columna)
    mg_asignado <- outer(-pmin(mg_k, 0),             #   asigna el servicio a los productos que lo prestan (margen < 0)...
                         .div0(mg_col, sum(mg_col))) #   ...repartido entre usuarios según el margen que pagan
    mg_mat <- mg_mat + mg_retirado - mg_asignado     #   acumula el margen neto (+ en bienes, - en el servicio)
  }                                                  # Cierra el ciclo sobre los tipos de margen
  u_pb <- u_pc - t_mat - mg_mat                      # Utilización a precios básicos = pc - impuestos - márgenes netos
  dimnames(t_mat) <- dimnames(u_pc)                  # Rotula la matriz de impuestos como la utilización
  dimnames(mg_mat) <- dimnames(u_pc)                 # Rotula la matriz de márgenes como la utilización
  dimnames(u_pb) <- dimnames(u_pc)                   # Rotula la utilización a precios básicos
  list(u_pb = u_pb, t_mat = t_mat, mg_mat = mg_mat)  # Devuelve las tres matrices
}                                                    # Cierra la función a_precios_basicos

# -----------------------------------------------------------------------------
# Paso 2. Separación de flujos nacionales e importados
# -----------------------------------------------------------------------------

#' Paso 2. Separación de flujos nacionales e importados
#'
#' Tratamiento del ajuste CIF/FOB: el ajuste (negativo en la oferta de los
#' servicios de flete y seguro) elimina la doble contabilización de los fletes
#' y seguros ya incluidos en el valor CIF de los bienes importados. Se
#' descuenta primero de las importaciones del mismo servicio (fletes prestados
#' por no residentes); solo el excedente, si lo hay, se atribuye a fletes de
#' residentes y se registra como uso doméstico en una columna adicional
#' "Ajuste CIF/FOB" de la demanda final (criterio de la MIP 1997 del INDEC).
#' En la fila de importaciones, esa columna lleva el excedente con signo
#' negativo, de modo que el total importado queda valuado FOB.
#'
#' Supuestos de reparto:
#'   (i) las exportaciones se abastecen primero con producción nacional; solo
#'       el excedente sobre la producción (reexportaciones) es importado;
#'  (ii) en los usos internos (intermedios y finales), la proporción importada
#'       de cada producto es la misma (proporcionalidad de importaciones).
#'
#' Alternativa preferible si se dispone de información: matriz de
#' importaciones por uso (el INDEC la publicó para 2004 y la retiró en la
#' revisión de 2016) o clasificación BEC de las importaciones (COMTRADE).
#'
#' @param cou Objeto "cou".
#' @param u_pb Utilización a precios básicos (productos x todos los usos).
#' @return Lista con `u_dom` y `u_imp` (incluyen la columna "Ajuste CIF/FOB"),
#'   la proporción importada de los usos internos `s` y la discrepancia de las
#'   importaciones implícitas respecto de las publicadas.
separar_importaciones <- function(cou, u_pb) {       # Abre la función de separación de importaciones
  q <- rowSums(cou$prod)                             # Producción nacional de cada producto
  aj_imp <- pmax(cou$aj_cif_fob, -cou$imp)           # Parte del ajuste que se descuenta de las importaciones del producto
  aj_dom <- -(cou$aj_cif_fob - aj_imp)               # Excedente atribuido a fletes de residentes (positivo o cero)
  u_ext <- cbind(u_pb, "Ajuste CIF/FOB" = aj_dom)    # Agrega la columna de ajuste CIF/FOB como uso doméstico
  m_ef <- rowSums(u_ext) - q                         # Oferta importada implícita (uso total - producción nacional)
  discrepancia <- m_ef - (cou$imp + aj_imp)          # Diferencia con las importaciones netas publicadas (redondeo)

  j_x <- match(cou$col_export, colnames(u_ext))      # Columna de exportaciones
  j_aj <- ncol(u_ext)                                # Columna de ajuste CIF/FOB (la última)
  internos <- setdiff(seq_len(ncol(u_ext)), c(j_x, j_aj)) # Columnas de usos internos (intermedios y finales)
  x_dom <- pmax(pmin(u_ext[, j_x], q - aj_dom), 0)   # Exportaciones abastecidas con producción nacional (hasta lo disponible)
  absorcion <- rowSums(u_ext[, internos, drop = FALSE]) # Uso interno total de cada producto
  dom_int <- q - aj_dom - x_dom                      # Producción nacional disponible para los usos internos
  s <- 1 - .div0(dom_int, absorcion)                 # Proporción importada de los usos internos
  s[absorcion == 0] <- 0                             # Sin uso interno, la proporción se fija en cero
  fuera <- s < -1e-3 | s > 1 + 1e-3                  # Marca proporciones imposibles (más allá del redondeo)
  if (any(fuera)) {                                  # Si alguna proporción es imposible...
    warning("Proporción importada fuera de [0, 1] en: ", # ...advierte qué productos tienen el problema...
            paste(names(s)[fuera], collapse = "; "), #    (lista de productos afectados)
            ". Se recorta; revisar los datos.")      # ...y sugiere revisar los datos
  }                                                  # Cierra el condicional
  s <- pmin(pmax(s, 0), 1)                           # Recorta las proporciones al intervalo [0, 1] (errores de redondeo)
  u_imp <- u_ext * 0                                 # Inicia en cero la matriz de usos importados
  u_imp[, internos] <- u_ext[, internos] * s         # Usos internos importados: cada fila por su proporción importada
  u_imp[, j_x] <- u_ext[, j_x] - x_dom               # Exportaciones importadas: solo las reexportaciones
  u_dom <- u_ext - u_imp                             # Uso doméstico = uso total - uso importado
  list(u_dom = u_dom, u_imp = u_imp,                 # Devuelve los usos doméstico e importado...
       s = s, discrepancia = discrepancia,           # ...la proporción importada y la discrepancia...
       aj_residentes = -aj_dom)                      # ...y el ajuste CIF/FOB atribuido a residentes (negativo)
}                                                    # Cierra la función separar_importaciones

# -----------------------------------------------------------------------------
# Paso 3. Supuesto tecnológico y armado de la MIP
# -----------------------------------------------------------------------------

#' Pasos 1 a 3. Construye la MIP simétrica de flujos domésticos
#'
#' @param cou Objeto "cou".
#' @param modelo "D" (rama x rama, estructura fija de ventas por producto) o
#'   "B" (producto x producto, tecnología de rama).
#' @return Lista con:
#'   `Z` (consumo intermedio doméstico), `F` (demanda final doméstica),
#'   `x` (valor bruto de producción), `primarios` (filas de importaciones,
#'   impuestos netos sobre productos y VAB, para las columnas de Z y F),
#'   `ajuste_territorial`, `modelo`, `unidad` y los pasos intermedios.
cou_a_mip <- function(cou, modelo = c("D", "B")) {   # Abre la función; el modelo por defecto es D
  modelo <- match.arg(modelo)                        # Valida que el modelo sea "D" o "B"
  nr <- ncol(cou$prod)                               # Cantidad de ramas
  int <- seq_len(nr)                                 # Índices de las columnas de uso intermedio
  fin <- nr + seq_len(ncol(cou$y) + 1)               # Índices de las columnas de uso final (incluye el ajuste CIF/FOB)

  pb <- a_precios_basicos(cou)                       # Paso 1: utilización a precios básicos
  imp <- separar_importaciones(cou, pb$u_pb)         # Paso 2: separa usos domésticos e importados

  u_dom_int <- imp$u_dom[, int, drop = FALSE]        # Consumo intermedio doméstico (productos x ramas)
  y_dom <- imp$u_dom[, fin, drop = FALSE]            # Demanda final doméstica (productos x usos finales)
  imp_col <- colSums(imp$u_imp)                      # Importaciones contenidas en cada uso (por columna)
  imp_col["Ajuste CIF/FOB"] <- sum(imp$aj_residentes) # En el ajuste CIF/FOB, las importaciones restan los fletes de residentes
  tax_col <- c(colSums(pb$t_mat), "Ajuste CIF/FOB" = 0) # Impuestos sobre productos por uso (cero en el ajuste)

  V <- t(cou$prod)                                   # Matriz de producción V: ramas x productos
  q <- colSums(V)                                    # Producción por producto
  g <- rowSums(V)                                    # Producción por rama

  if (modelo == "D") {                               # Modelo D: MIP rama x rama
    D <- sweep(V, 2, ifelse(q == 0, 1, q), "/")      # Cuotas de mercado D = V q^-1 (qué parte de cada producto hace cada rama)
    Z <- D %*% u_dom_int                             # Transacciones intermedias rama x rama: Z = D U
    F <- D %*% y_dom                                 # Demanda final por rama: F = D Y
    x <- g                                           # El VBP de cada sector es la producción de la rama
    prim_int <- rbind(imp_col[int],                  # Insumos primarios de cada rama: importaciones...
                      tax_col[int],                  # ...impuestos sobre productos...
                      cou$vab)                       # ...y valor agregado bruto
    nombres <- rownames(V)                           # Los sectores de la MIP son las ramas
  } else {                                           # Modelo B: MIP producto x producto
    W <- sweep(V, 1, ifelse(g == 0, 1, g), "/")      # Mezcla de productos de cada rama: W = g^-1 V
    Z <- u_dom_int %*% W                             # Transacciones producto x producto: Z = U g^-1 V
    F <- y_dom                                       # La demanda final ya está por producto
    x <- q                                           # El VBP de cada sector es la producción del producto
    prim_int <- rbind(imp_col[int],                  # Insumos primarios por rama: importaciones...
                      tax_col[int],                  # ...impuestos...
                      cou$vab) %*% W                 # ...y VAB, reasignados a productos con la misma mezcla
    nombres <- colnames(V)                           # Los sectores de la MIP son los productos
  }                                                  # Cierra la elección del modelo

  primarios <- cbind(prim_int,                       # Une los insumos primarios de los sectores...
                     rbind(imp_col[fin],             # ...con las importaciones de cada uso final...
                           tax_col[fin],             # ...los impuestos sobre productos de cada uso final...
                           0))                       # ...y un VAB nulo (los usos finales no generan VAB)
  if (!is.null(cou$ajuste_territorial) &&            # Si existe un ajuste territorial...
      cou$imp_territorial != 0) {                    # ...con importaciones (compras de residentes en el exterior)...
    j <- which.max(abs(cou$ajuste_territorial[1, ])) # ...ubica el uso final donde se registra (consumo privado)...
    k <- ncol(prim_int) + j                          # ...calcula su columna dentro de la tabla de primarios...
    primarios[1, k] <- primarios[1, k] + cou$imp_territorial # ...y suma esas importaciones a ese uso final
  }                                                  # Cierra el condicional del ajuste territorial
  dimnames(Z) <- list(nombres, nombres)              # Rotula filas y columnas de Z
  usos_finales <- c(colnames(cou$y), "Ajuste CIF/FOB") # Nombres de los usos finales de la MIP
  dimnames(F) <- list(nombres, usos_finales)         # Rotula filas (sectores) y columnas (usos finales) de F
  names(x) <- nombres                                # Rotula el vector de VBP
  rownames(primarios) <- c("Importaciones",          # Rotula la fila de importaciones
                           "Impuestos netos sobre productos", # ...la de impuestos sobre productos
                           "Valor agregado bruto pb")         # ...y la de valor agregado
  colnames(primarios) <- c(nombres, usos_finales)    # Rotula las columnas: sectores y usos finales

  structure(list(                                    # Arma el objeto de salida:
    Z = Z, F = F, x = x, primarios = primarios,      #   cuadrantes de la MIP y VBP
    ajuste_territorial = cou$ajuste_territorial,     #   ajuste territorial original
    modelo = modelo, unidad = cou$unidad,            #   modelo tecnológico y unidad de medida
    pasos = list(precios_basicos = pb,               #   resultados intermedios del paso 1
                 importaciones = imp)                #   resultados intermedios del paso 2
  ), class = "mip")                                  # Asigna la clase "mip"
}                                                    # Cierra la función cou_a_mip

# -----------------------------------------------------------------------------
# Presentación de la MIP como tabla única
# -----------------------------------------------------------------------------

#' Tabla completa de la MIP (cuadrantes I, II y III con totales)
#'
#' @param mip Objeto "mip".
#' @return Matriz con Z | F | VBP y, debajo, insumos primarios y VBP.
tabla_mip <- function(mip) {                         # Abre la función de armado de la tabla
  n <- length(mip$x)                                 # Cantidad de sectores
  sup <- cbind(mip$Z, mip$F,                         # Parte superior: cuadrantes I (Z) y II (F)...
               "Valor bruto de producción" = mip$x)  # ...más la columna de VBP
  inf <- cbind(mip$primarios, NA)                    # Parte inferior: cuadrante III (insumos primarios)
  tot <- c(colSums(mip$Z) +                          # Fila de VBP por columnas: insumos intermedios...
             colSums(mip$primarios[, seq_len(n), drop = FALSE]), # ...más insumos primarios de cada sector
           rep(NA, ncol(mip$F) + 1))                 # Celdas vacías bajo los usos finales y el total
  out <- rbind(sup, inf, "Valor bruto de producción" = tot) # Apila las tres partes
  colnames(out)[ncol(out)] <- "Valor bruto de producción" # Asegura el nombre de la última columna
  out                                                # Devuelve la tabla completa
}                                                    # Cierra la función tabla_mip
