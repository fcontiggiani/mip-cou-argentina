# =============================================================================
# estimar_mip_cou_2018.R
# Estimación de la matriz insumo-producto (MIP) de la Argentina para 2018 a
# partir del cuadro de oferta y utilización (COU) del INDEC.
#
# Script autocontenido y didáctico: recorre, paso a paso, todo el camino desde
# el libro Excel del INDEC hasta la inversa de Leontief, los multiplicadores y
# los encadenamientos. Cada línea está comentada.
#
# Resultado: MIP simétrica rama x rama (107 ramas CIIU Rev. 3), de flujos
# domésticos a precios básicos, en millones de pesos corrientes de 2018.
#
# Datos: INDEC, Cuadro de oferta y utilización 2018 (archivo sh_cou_2018_08_21.xls).
#   Verificar en www.indec.gob.ar el nombre vigente del archivo.
#
# Uso:  Rscript estimar_mip_cou_2018.R "ruta/al/sh_cou_2018_08_21.xls"
#   (sin argumento, se busca en datos/COU/sh_cou_2018_08_21.xls)
#
# Requiere: R >= 4.1; paquetes readxl y writexl.
#
# Notación (vectores en minúscula, matrices en mayúscula, ^ = matriz diagonal):
#   V   producción: ramas x productos (traspuesta del bloque de producción de la oferta)
#   q   producción por producto (q = V'1);  g = producción por rama (g = V1)
#   U   utilización intermedia (productos x ramas); Y = utilización final
#   Z   transacciones intermedias de la MIP; F = demanda final; x = VBP
#   A   coeficientes técnicos  A = Z x^-1;  L = (I - A)^-1  inversa de Leontief
# =============================================================================


# -----------------------------------------------------------------------------
# 0. Configuración
# -----------------------------------------------------------------------------
library(readxl)                                      # Carga readxl, para leer los libros .xls del INDEC
library(writexl)                                     # Carga writexl, para exportar los resultados a Excel

args <- commandArgs(trailingOnly = TRUE)             # Lee los argumentos de la línea de comandos
archivo <- if (length(args) > 0) {                   # Si se indicó la ruta del libro como argumento...
  args[1]                                            #   ...se usa esa ruta
} else {                                             # Si no...
  "datos/COU/sh_cou_2018_08_21.xls"                  #   ...se usa la ruta por defecto del proyecto
}                                                    # Cierra la elección de la ruta
if (!file.exists(archivo)) stop("No se encuentra el archivo: ", archivo) # Detiene si el libro no existe
dir.create("salidas", showWarnings = FALSE)          # Crea la carpeta de salidas si no existe
options(width = 140)                                 # Amplía el ancho de impresión en la consola

# Funciones auxiliares de una línea
a_num <- function(x) {                               # Convierte texto en número:
  x[is.na(x) | trimws(x) %in% c("", "-")] <- "0"     #   celdas vacías o con guion valen cero
  as.numeric(x)                                      #   pasa el texto a número
}                                                    # Cierra la función a_num
div0 <- function(a, b) {                             # División que devuelve cero si el denominador es cero:
  r <- a / b                                         #   divide elemento a elemento
  r[!is.finite(r)] <- 0                              #   reemplaza Inf y NaN (divisiones por cero) por cero
  r                                                  #   devuelve el cociente
}                                                    # Cierra la función div0
leer_hoja <- function(hoja) {                        # Lee una hoja completa como matriz de texto:
  as.matrix(read_excel(archivo, sheet = hoja,        #   lee la hoja indicada del libro...
                       col_names = FALSE,            #   ...sin usar la primera fila como encabezado...
                       col_types = "text",           #   ...con todas las celdas como texto...
                       .name_repair = "minimal"))    #   ...y sin modificar los nombres de columna
}                                                    # Cierra la función leer_hoja


# -----------------------------------------------------------------------------
# 1. Lectura del COU
# -----------------------------------------------------------------------------
# El libro tiene dos hojas: "Mat_Of_pc_2018" (oferta) y "Mat_Ut_pc_2018"
# (utilización). En ambas, la fila cuya primera celda dice "Descripción"
# contiene los códigos de columna; los productos (CPC 1.1) ocupan las filas
# siguientes hasta la fila "Total"; las ramas (CIIU Rev. 3) empiezan en la
# columna 3. Los bloques se ubican por esos rótulos, no por rangos fijos.

hojas <- excel_sheets(archivo)                       # Lista las hojas del libro
mo <- suppressMessages(leer_hoja(hojas[grepl("^Mat_Of", hojas)][1])) # Lee la hoja de oferta
mu <- suppressMessages(leer_hoja(hojas[grepl("^Mat_Ut", hojas)][1])) # Lee la hoja de utilización

unidad <- mo[2, 1]                                   # Rótulo de año y unidad, p. ej. "Año 2018 (miles de pesos)"
factor <- if (grepl("miles", unidad, ignore.case = TRUE)) 1 / 1000 else 1 # Factor para pasar a millones de pesos
cat("Unidad del libro:", unidad, "-> se expresa en millones de pesos\n") # Informa la unidad original

# --- Cuadro de oferta ---
f_enc_o <- which(mo[, 1] %in% "Descripción")[1]     # Fila de encabezado con los códigos de columna
f_tot_o <- which(mo[, 1] %in% "Total")[1]           # Fila del total de productos
filas_p <- (f_enc_o + 1):(f_tot_o - 1)               # Filas de productos
cod_o <- mo[f_enc_o, ]                               # Códigos de columna de la oferta
col_ramas <- 3:(match("OPB", cod_o) - 1)             # Columnas de ramas: de la 3 hasta antes de "OPB"
col_o <- function(cod) match(cod, cod_o)             # Ubica una columna de la oferta por su código
num_o <- function(j) a_num(mo[filas_p, j]) * factor  # Lee una columna numérica de la oferta, en millones

productos <- gsub("\\s+", " ", mo[filas_p, 1])      # Nombres de los productos (código CPC y descripción), sin saltos de línea
ramas <- gsub("\\s+", " ", mo[f_enc_o - 1, col_ramas]) # Nombres de las ramas (fila anterior al encabezado), sin saltos de línea
cod_ramas <- cod_o[col_ramas]                        # Códigos CIIU Rev. 3 de las ramas

prod <- sapply(col_ramas, num_o)                     # Producción a precios básicos: matriz productos x ramas
dimnames(prod) <- list(productos, ramas)             # Rotula filas (productos) y columnas (ramas)
imp <- num_o(col_o("IMPO"))                          # Importaciones CIF por producto
aj_cif <- num_o(col_o("Ajuste CIF/FOB"))             # Ajuste CIF/FOB por producto (negativo en fletes y seguros)
impuestos <- num_o(col_o("DI")) +                    # Impuestos sobre productos: derechos de importación...
  num_o(col_o("IP")) +                               # ...más impuestos netos de subsidios...
  num_o(col_o("IVA"))                                # ...más IVA no deducible
tipos_mg <- intersect(c("Mg D", "CSCD"), cod_o)      # Tipos de margen presentes (CSCD: comisiones por cambio de divisas)
margenes <- sapply(tipos_mg, function(cod) num_o(col_o(cod))) # Márgenes por producto y tipo (una columna por tipo)
margenes <- matrix(margenes, nrow = length(filas_p)) # Asegura que los márgenes sean una matriz
opc <- num_o(col_o("OPC"))                           # Oferta total a precios de comprador (para control)

# --- Cuadro de utilización ---
f_enc_u <- which(mu[, 1] %in% "Descripción")[1]     # Fila de encabezado de la utilización
cod_u <- mu[f_enc_u, ]                               # Códigos de columna de la utilización
filas_u <- f_enc_u + seq_along(filas_p)              # Filas de productos (las mismas que en la oferta)
stopifnot(identical(mo[filas_p, 2], mu[filas_u, 2])) # Verifica que los productos coincidan en ambas hojas
col_ramas_u <- 3:(match("UI", cod_u) - 1)            # Columnas de ramas: de la 3 hasta antes de "UI"
stopifnot(length(col_ramas_u) == length(col_ramas))  # Verifica que ambas hojas tengan las mismas ramas
num_u <- function(f, j) a_num(mu[f, j]) * factor     # Lee celdas numéricas de la utilización, en millones

u_pc <- sapply(col_ramas_u, function(j) num_u(filas_u, j)) # Consumo intermedio a precios de comprador (productos x ramas)
cod_fin <- c("EX", "CH", "CP", "Fbc Fijo", "OV",     # Códigos de los usos finales:
             "Productos terminados", "Trabajos en curso") #   exportaciones, consumo, inversión y existencias
nom_fin <- c("Exportaciones", "Consumo privado", "Consumo público", "FBCF", # Nombres legibles de los usos finales
             "Objetos valiosos", "Var. existencias (terminados)", "Var. existencias (en curso)") # (continuación)
y_pc <- sapply(match(cod_fin, cod_u), function(j) num_u(filas_u, j)) # Demanda final a precios de comprador (productos x usos)
colnames(y_pc) <- nom_fin                            # Rotula las columnas de la demanda final
vab <- num_u(which(mu[, 1] %in% "Valor Agregado Bruto pb")[1], col_ramas_u) # VAB a precios básicos por rama

f_res <- which(mu[, 1] %in% "Compras de residentes en el mercado exterior")[1]     # Fila de compras de residentes afuera
f_nores <- which(mu[, 1] %in% "Compras de no residentes en el mercado doméstico")[1] # Fila de compras de no residentes
aj_terr <- rbind(num_u(f_res, match(cod_fin, cod_u)),  # Ajuste territorial: residentes en el exterior...
                 num_u(f_nores, match(cod_fin, cod_u))) # ...y no residentes en el país, por uso final
imp_terr <- a_num(mo[which(mo[, 1] %in% "Compras de residentes en el mercado exterior")[1], # Importaciones por compras...
                     col_o("IMPO")]) * factor        # ...de residentes en el exterior (fila de la oferta)

n_p <- nrow(prod)                                    # Cantidad de productos
n_r <- ncol(prod)                                    # Cantidad de ramas
cat("Productos:", n_p, "| Ramas:", n_r, "\n")        # Informa las dimensiones del COU


# -----------------------------------------------------------------------------
# 2. Validación del COU
# -----------------------------------------------------------------------------
oferta_pb <- rowSums(prod) + imp + aj_cif            # Oferta a precios básicos: producción + importaciones + ajuste
oferta_pc <- oferta_pb + rowSums(margenes) + impuestos # Oferta a precios de comprador: + márgenes + impuestos
uso_pc <- rowSums(u_pc) + rowSums(y_pc)              # Utilización total a precios de comprador
g <- colSums(prod)                                   # Producción de cada rama (VBP)
pib_prod <- sum(vab) + sum(impuestos)                # PIB por la producción: VAB pb + impuestos sobre productos
pib_gasto <- sum(y_pc) + sum(aj_terr) -              # PIB por el gasto: demanda final + ajuste territorial...
  (sum(imp) + sum(aj_cif) + imp_terr)                # ...menos importaciones FOB (incluidas las de residentes afuera)

controles_cou <- c(                                  # Diferencias máximas de cada identidad contable:
  oferta_igual_uso = max(abs(oferta_pc - uso_pc)),   #   oferta = utilización, por producto
  oferta_pc_publicada = max(abs(oferta_pc - opc)),   #   oferta pc calculada = oferta pc publicada
  produccion_igual_costos = max(abs(g - colSums(u_pc) - vab)), # producción = consumo intermedio + VAB, por rama
  margenes_suman_cero = max(abs(colSums(margenes))), #   cada tipo de margen suma cero
  pib_produccion_gasto = abs(pib_prod - pib_gasto))  #   PIB por la producción = PIB por el gasto
print(round(controles_cou, 4))                       # Muestra los controles (deben ser casi cero)
stopifnot(all(controles_cou < 5))                    # Detiene si alguna diferencia supera 5 millones (redondeo)
cat(sprintf("PIB 2018: %s millones de pesos\n",      # Informa el PIB...
            format(round(pib_prod), big.mark = ".", decimal.mark = ","))) # ...con formato argentino


# -----------------------------------------------------------------------------
# 3. Paso 1: de precios de comprador a precios básicos
# -----------------------------------------------------------------------------
# Supuesto: dentro de cada producto, la tasa de impuestos y la tasa de margen
# son las mismas en todos los usos. Los márgenes retirados de los bienes se
# asignan a los productos que prestan el servicio (margen negativo), en
# proporción a lo que paga cada usuario. Cada tipo de margen se trata aparte.

u_todo <- cbind(u_pc, y_pc)                          # Une usos intermedios y finales: productos x (ramas + usos finales)
tot_pc <- rowSums(u_todo)                            # Utilización total de cada producto a precios de comprador

tasa_t <- div0(impuestos, tot_pc)                    # Tasa de impuestos de cada producto
t_mat <- u_todo * tasa_t                             # Impuestos contenidos en cada celda (cada fila por su tasa)

mg_mat <- u_todo * 0                                 # Inicia en cero la matriz de márgenes netos por celda
for (k in seq_len(ncol(margenes))) {                 # Recorre cada tipo de margen:
  mg_k <- margenes[, k]                              #   margen de este tipo, por producto
  tasa_mg <- div0(pmax(mg_k, 0), tot_pc)             #   tasa de margen de los bienes (solo márgenes positivos)
  mg_retirado <- u_todo * tasa_mg                    #   margen contenido en cada celda de los bienes
  mg_col <- colSums(mg_retirado)                     #   margen total que paga cada usuario
  mg_asignado <- outer(-pmin(mg_k, 0),               #   servicio de distribución que presta cada producto (margen < 0)...
                       div0(mg_col, sum(mg_col)))    #   ...repartido entre usuarios según el margen que pagan
  mg_mat <- mg_mat + mg_retirado - mg_asignado       #   acumula el margen neto (+ en bienes, - en servicios)
}                                                    # Cierra el ciclo sobre los tipos de margen

u_pb <- u_todo - t_mat - mg_mat                      # Utilización a precios básicos = pc - impuestos - márgenes netos
cat("Control paso 1 (uso pb = oferta pb):",          # Informa la mayor diferencia entre...
    round(max(abs(rowSums(u_pb) - oferta_pb)), 4), "\n") # ...la utilización y la oferta a precios básicos


# -----------------------------------------------------------------------------
# 4. Paso 2: separación de flujos nacionales e importados
# -----------------------------------------------------------------------------
# (a) Ajuste CIF/FOB: corrige fletes y seguros ya incluidos en el valor CIF de
#     los bienes importados. Se descuenta primero de las importaciones del
#     mismo servicio; el excedente se atribuye a fletes de residentes y se
#     registra como uso doméstico en una columna "Ajuste CIF/FOB".
# (b) Las exportaciones se abastecen primero con producción nacional.
# (c) En los usos internos, la proporción importada de cada producto es la
#     misma (proporcionalidad de importaciones).

q <- rowSums(prod)                                   # Producción nacional de cada producto
aj_imp <- pmax(aj_cif, -imp)                         # Parte del ajuste que se descuenta de las importaciones
aj_res <- aj_cif - aj_imp                            # Excedente atribuido a fletes de residentes (negativo o cero)
u_ext <- cbind(u_pb, "Ajuste CIF/FOB" = -aj_res)     # Agrega la columna de ajuste CIF/FOB como uso doméstico

j_x <- match("Exportaciones", colnames(u_ext))       # Columna de exportaciones
j_aj <- ncol(u_ext)                                  # Columna de ajuste CIF/FOB (la última)
internos <- setdiff(seq_len(ncol(u_ext)), c(j_x, j_aj)) # Columnas de usos internos (intermedios y finales)

x_dom <- pmax(pmin(u_ext[, j_x], q + aj_res), 0)     # Exportaciones abastecidas con producción nacional
absorcion <- rowSums(u_ext[, internos])              # Uso interno total de cada producto
dom_int <- q + aj_res - x_dom                        # Producción nacional disponible para los usos internos
s <- 1 - div0(dom_int, absorcion)                    # Proporción importada de los usos internos
s[absorcion == 0] <- 0                               # Sin uso interno, la proporción se fija en cero
s <- pmin(pmax(s, 0), 1)                             # Acota la proporción a [0, 1] (diferencias de redondeo)

u_imp <- u_ext * 0                                   # Inicia en cero la matriz de usos importados
u_imp[, internos] <- u_ext[, internos] * s           # Usos internos importados: cada fila por su proporción
u_imp[, j_x] <- u_ext[, j_x] - x_dom                 # Exportaciones importadas: solo las reexportaciones
u_dom <- u_ext - u_imp                               # Usos domésticos = usos totales - usos importados
cat("Control paso 2 (uso doméstico = producción):",  # Informa la mayor diferencia entre...
    round(max(abs(rowSums(u_dom) - q)), 4), "\n")    # ...el uso doméstico y la producción nacional


# -----------------------------------------------------------------------------
# 5. Paso 3: MIP rama x rama (modelo D de Eurostat)
# -----------------------------------------------------------------------------
# Supuesto de estructura fija de ventas por producto: cada producto se reparte
# entre sus usuarios igual, sea cual sea la rama que lo produce.
#   D = V q^-1  (cuotas de mercado: qué parte de cada producto produce cada rama)
#   Z = D U     F = D Y     x = g

int <- seq_len(n_r)                                  # Índices de las columnas de uso intermedio
fin <- n_r + seq_len(ncol(u_dom) - n_r)              # Índices de las columnas de uso final (incluye el ajuste CIF/FOB)
V <- t(prod)                                         # Matriz de producción V: ramas x productos
D <- sweep(V, 2, ifelse(q == 0, 1, q), "/")          # Cuotas de mercado: divide cada columna de V por q

Z <- D %*% u_dom[, int]                              # Transacciones intermedias rama x rama: Z = D U
F <- D %*% u_dom[, fin]                              # Demanda final por rama: F = D Y
x <- g                                               # VBP de cada rama
dimnames(Z) <- list(ramas, ramas)                    # Rotula filas y columnas de Z
dimnames(F) <- list(ramas, colnames(u_dom)[fin])     # Rotula filas y columnas de F

imp_col <- colSums(u_imp)                            # Importaciones contenidas en cada uso (por columna)
imp_col[j_aj] <- sum(aj_res)                         # En el ajuste CIF/FOB, las importaciones restan los fletes de residentes
tax_col <- c(colSums(t_mat), 0)                      # Impuestos sobre productos por uso (cero en el ajuste)
primarios <- rbind(                                  # Insumos primarios (cuadrante III), por columna de la MIP:
  "Importaciones" = imp_col,                         #   importaciones
  "Impuestos netos sobre productos" = tax_col,       #   impuestos sobre productos
  "Valor agregado bruto pb" = c(vab, rep(0, length(fin)))) # valor agregado (solo en las ramas)
colnames(primarios) <- c(ramas, colnames(F))         # Rotula las columnas: ramas y usos finales


# -----------------------------------------------------------------------------
# 6. Validación de la MIP
# -----------------------------------------------------------------------------
dif_filas <- max(abs(rowSums(Z) + rowSums(F) - x))   # Filas: ventas intermedias + finales = VBP
dif_cols <- max(abs(colSums(Z) + colSums(primarios[, int]) - x)) # Columnas: insumos + primarios = VBP
pib_mip <- sum(primarios["Valor agregado bruto pb", ]) + # PIB según la MIP: VAB...
  sum(primarios["Impuestos netos sobre productos", ])  # ...más impuestos sobre productos
cat("Control MIP filas:", round(dif_filas, 4),       # Informa la diferencia por filas...
    "| columnas:", round(dif_cols, 4),               # ...por columnas...
    "| PIB MIP - PIB COU:", round(pib_mip - pib_prod, 4), "\n") # ...y la del PIB
stopifnot(dif_filas < 5, dif_cols < 5, min(Z) >= 0)  # Detiene si las identidades no cierran o hay flujos negativos


# -----------------------------------------------------------------------------
# 7. Coeficientes técnicos (A) e inversa de Leontief (L)
# -----------------------------------------------------------------------------
A <- sweep(Z, 2, x, "/")                             # Coeficientes técnicos: a_ij = z_ij / x_j
rho <- max(Mod(eigen(A, only.values = TRUE)$values)) # Radio espectral de A
cat("Radio espectral de A:", round(rho, 4),          # Informa el radio espectral...
    "(Hawkins-Simon: debe ser < 1)\n")               # ...y la condición que debe cumplir
stopifnot(rho < 1)                                   # Detiene si no se cumple la condición de Hawkins-Simon
I <- diag(n_r)                                       # Matriz identidad de orden igual a la cantidad de ramas
L <- solve(I - A)                                    # Inversa de Leontief: L = (I - A)^-1
dimnames(L) <- dimnames(A)                           # Rotula L con las ramas
stopifnot(min(L) >= -1e-10,                          # Verifica que L sea no negativa...
          max(abs((I - A) %*% L - I)) < 1e-8)        # ...y que (I - A) L = I


# -----------------------------------------------------------------------------
# 8. Multiplicadores y encadenamientos
# -----------------------------------------------------------------------------
coef_va <- vab / x                                   # Coeficientes de valor agregado: v_j = VAB_j / x_j
coef_m <- imp_col[int] / x                           # Coeficientes de importaciones: m_j = M_j / x_j
coef_t <- tax_col[int] / x                           # Coeficientes de impuestos sobre productos: t_j = T_j / x_j

B <- sweep(Z, 1, x, "/")                             # Coeficientes de distribución: b_ij = z_ij / x_i
G <- solve(I - B)                                    # Inversa de Ghosh: G = (I - B)^-1

resultados <- data.frame(                            # Arma la tabla de resultados por rama:
  codigo = cod_ramas,                                #   código CIIU Rev. 3
  rama = ramas,                                      #   nombre de la rama
  vbp = round(x),                                    #   valor bruto de producción (millones de pesos)
  mult_produccion = colSums(L),                      #   multiplicador de producción: 1'L
  mult_valor_agregado = as.vector(coef_va %*% L),    #   multiplicador de valor agregado: v'L
  mult_importaciones = as.vector(coef_m %*% L),      #   multiplicador de importaciones: m'L
  mult_impuestos = as.vector(coef_t %*% L),          #   multiplicador de impuestos: t'L
  BL = colSums(L) / (sum(L) / n_r),                  #   encadenamiento hacia atrás (Rasmussen)
  FL_leontief = rowSums(L) / (sum(L) / n_r),         #   hacia adelante con filas de L (Hirschman-Rasmussen)
  FL_ghosh = rowSums(G) / (sum(G) / n_r),            #   hacia adelante con filas de G (Ghosh)
  row.names = NULL,                                  #   sin nombres de fila
  stringsAsFactors = FALSE)                          #   texto como caracteres
resultados$control_suma_1 <- resultados$mult_valor_agregado + # Control: VAB + importaciones + impuestos...
  resultados$mult_importaciones + resultados$mult_impuestos   # ...debe sumar 1 en cada rama
resultados$tipo_ghosh <- ifelse(resultados$BL >= 1 & resultados$FL_ghosh >= 1, "clave", # Tipología de Hirschman:
                         ifelse(resultados$BL >= 1, "impulsor",  #   solo BL alto: impulsor
                         ifelse(resultados$FL_ghosh >= 1, "base", #   solo FL alto: base
                                "independiente")))               #   ambos bajos: independiente
stopifnot(max(abs(resultados$control_suma_1 - 1)) < 1e-6) # Verifica que (v + m + t)'L = 1'

cat("\nRamas con mayor multiplicador de producción:\n") # Imprime un encabezado
top <- head(resultados[order(-resultados$mult_produccion), ], 10) # Las 10 ramas de mayor multiplicador de producción
top$rama <- substr(top$rama, 1, 45)                  # Acorta los nombres para la consola
print(top[, c("codigo", "rama", "mult_produccion", "mult_valor_agregado", "BL", "FL_ghosh", "tipo_ghosh")], # Muestra las columnas principales...
      digits = 3, row.names = FALSE)                 # ...con 3 cifras significativas y sin nombres de fila
cat("\nCantidad de ramas por tipo (Ghosh):\n")       # Imprime un encabezado
print(table(resultados$tipo_ghosh))                  # Cuenta las ramas de cada tipo


# -----------------------------------------------------------------------------
# 9. Ejemplo de uso: impacto de un aumento de la demanda final
# -----------------------------------------------------------------------------
df <- setNames(rep(0, n_r), ramas)                   # Vector de variación de la demanda final, en cero
df[cod_ramas == "1511"] <- 1000                      # +1.000 millones de demanda final de carne (rama 1511)
dx <- as.vector(L %*% df)                            # Variación del VBP de cada rama: dx = L df
dva <- coef_va * dx                                  # Variación del valor agregado de cada rama
cat("\nImpacto de +1.000 millones de demanda final de la rama 1511 (carne):\n") # Imprime un encabezado
cat("  VBP total:", round(sum(dx), 1),               # Informa el aumento del VBP total...
    "| VAB total:", round(sum(dva), 1),              # ...del valor agregado...
    "| importaciones:", round(sum(coef_m * dx), 1), "\n") # ...y de las importaciones
print(head(sort(setNames(dx, cod_ramas), decreasing = TRUE), 6), digits = 4) # Muestra las 6 ramas más afectadas


# -----------------------------------------------------------------------------
# 10. Exportación de resultados
# -----------------------------------------------------------------------------
a_df <- function(m) {                                # Convierte una matriz en data.frame con una columna de rótulos:
  data.frame(rama = rownames(m),                     #   primera columna: nombres de fila
             as.data.frame(m, check.names = FALSE),  #   resto: los valores de la matriz
             check.names = FALSE)                    #   conserva los nombres de columna originales
}                                                    # Cierra la función a_df
mip_tabla <- rbind(cbind(Z, F, VBP = x),             # Cuadrantes I y II con la columna de VBP...
                   cbind(primarios, VBP = NA),       # ...cuadrante III (insumos primarios)...
                   VBP = c(colSums(Z) + colSums(primarios[, int]), rep(NA, length(fin) + 1))) # ...y la fila de VBP
write_xlsx(list(                                     # Escribe un libro Excel con una hoja por tabla:
  MIP_ramaxrama = a_df(round(mip_tabla, 1)),         #   MIP 107 x 107 con demanda final e insumos primarios
  A_coef_tecnicos = a_df(round(A, 6)),               #   coeficientes técnicos
  L_inversa_Leontief = a_df(round(L, 6)),            #   inversa de Leontief
  Indicadores = resultados,                          #   multiplicadores, encadenamientos y tipología
  Notas = data.frame(nota = c(                       #   notas metodológicas:
    "Fuente: INDEC, Cuadro de oferta y utilización 2018. Elaboración propia.", # fuente
    "MIP rama x rama (modelo D de Eurostat), 107 ramas CIIU Rev. 3, flujos domésticos a precios básicos.", # tipo de MIP
    "Millones de pesos corrientes de 2018.",         #     unidad
    "Supuestos: tasas uniformes de impuestos y márgenes por producto; ajuste CIF/FOB descontado primero de las importaciones; exportaciones de producción nacional; proporcionalidad de importaciones en los usos internos." # supuestos
  ))),                                               #   cierra la hoja de notas
  "salidas/mip_cou_2018_107ramas.xlsx")              # Ruta del libro de salida
cat("\nResultados exportados en salidas/mip_cou_2018_107ramas.xlsx\n") # Informa dónde quedaron los resultados
