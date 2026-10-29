# =============================================================================
# main.R
# Estimación de una MIP simétrica y de la inversa de Leontief a partir de los
# cuadros de oferta y utilización (COU) del INDEC.
#
# Uso:   Rscript main.R           (desde la carpeta del proyecto)
# Requiere: R >= 4.1; paquetes readxl (lectura del COU completo),
#           writexl (exportación, opcional) y testthat (pruebas).
# =============================================================================

# -----------------------------------------------------------------------------
# 0. Carga de funciones y opciones
# -----------------------------------------------------------------------------
archivos <- sort(list.files("R", pattern = "\\.R$", full.names = TRUE)) # Lista los scripts de funciones en orden
for (f in archivos) source(f, encoding = "UTF-8")    # Carga cada script de funciones (con acentos en UTF-8)
options(width = 120)                                 # Amplía el ancho de impresión en la consola

# -----------------------------------------------------------------------------
# 1. Datos: COU 2018 del INDEC, versión reducida (10 productos x 5 ramas)
# -----------------------------------------------------------------------------
cou <- cou_indec_2018_reducido()                     # Carga el COU 2018 reducido publicado por el INDEC
cat("\n== Controles del COU 2018 (versión reducida) ==\n") # Imprime un encabezado
validar_cou(cou)                                     # Verifica las identidades contables del COU

# -----------------------------------------------------------------------------
# 2. MIP rama x rama (modelo D) y producto x producto (modelo B)
# -----------------------------------------------------------------------------
mip_D <- cou_a_mip(cou, modelo = "D")                # Construye la MIP rama x rama (estructura fija de ventas)
cat("\n== Controles de la MIP rama x rama (modelo D) ==\n") # Imprime un encabezado
validar_mip(mip_D)                                   # Verifica identidades y Hawkins-Simon de la MIP D

mip_B <- cou_a_mip(cou, modelo = "B")                # Construye la MIP producto x producto (tecnología de rama)
cat("\n== Controles de la MIP producto x producto (modelo B) ==\n") # Imprime un encabezado
validar_mip(mip_B)                                   # Verifica identidades y Hawkins-Simon de la MIP B

cat("\nProporción importada en los usos internos, por producto (%):\n") # Imprime un encabezado
print(round(100 * mip_D$pasos$importaciones$s, 1))   # Muestra la proporción importada de cada producto, en %
cat("\nDiscrepancia de la oferta importada implícita vs. publicada (redondeo):\n") # Imprime un encabezado
print(round(mip_D$pasos$importaciones$discrepancia, 1)) # Muestra la diferencia con las importaciones publicadas

# -----------------------------------------------------------------------------
# 3. Coeficientes directos (A), directos e indirectos (L) y análisis
# -----------------------------------------------------------------------------
an_D <- analizar_mip(mip_D)                          # Calcula A, L, multiplicadores y encadenamientos (modelo D)
cat("\n== Matriz de coeficientes técnicos A (rama x rama) ==\n") # Imprime un encabezado
print(round(an_D$A, 4))                              # Muestra A redondeada a 4 decimales
cat("\n== Inversa de Leontief L = (I - A)^-1 (rama x rama) ==\n") # Imprime un encabezado
print(round(an_D$L, 4))                              # Muestra L redondeada a 4 decimales
cat("\n== Multiplicadores (tipo I) ==\n")            # Imprime un encabezado
print(an_D$multiplicadores, digits = 4, row.names = FALSE) # Muestra la tabla de multiplicadores
cat("\n== Encadenamientos ==\n")                     # Imprime un encabezado
print(an_D$encadenamientos, digits = 3, row.names = FALSE) # Muestra la tabla de encadenamientos

df <- setNames(rep(0, length(mip_D$x)), names(mip_D$x)) # Crea un shock de demanda final nulo para cada rama
df[1] <- 1000                                        # Asigna +1.000 millones a la demanda final de la rama agro
cat("\n== Impacto de +1.000 millones de demanda final de la rama agro (VBP) ==\n") # Imprime un encabezado
print(round(impacto_demanda(an_D$L, df), 1))         # Calcula y muestra dx = L df por rama

# -----------------------------------------------------------------------------
# 4. Exportación de resultados
# -----------------------------------------------------------------------------
an_B <- analizar_mip(mip_B)                          # Calcula el análisis completo también para el modelo B
exportar_mip(mip_D, an_D, dir = "salidas",           # Exporta la MIP rama x rama...
             prefijo = "mip_2018_reducida_ramaxrama") # ...con este prefijo de archivo
exportar_mip(mip_B, an_B, dir = "salidas",           # Exporta la MIP producto x producto...
             prefijo = "mip_2018_reducida_prodxprod") # ...con este prefijo de archivo
cat("\nResultados exportados en la carpeta 'salidas/'.\n") # Informa dónde quedaron los resultados

# -----------------------------------------------------------------------------
# 5. COU completo (107 ramas x 223 productos): configurar y ejecutar
#    (descomentar las líneas una vez disponible el archivo)
# -----------------------------------------------------------------------------
# archivo <- descargar_cou_indec()                    # Descarga el libro del INDEC a datos/ (o copiarlo a mano)
# ubicaciones <- inspeccionar_cou("datos/sh_cou_08_21.xls") # Lista hojas y celdas con rótulos clave
# View(ubicaciones)                                   # Abre la tabla de ubicaciones para leer los rangos
# cfg <- config_cou_plantilla()                       # Crea la plantilla de configuración
# cfg$hoja_oferta <- "..."                            # Completa la hoja del cuadro de oferta
# cfg$prod <- "..."                                   # Completa el rango del bloque de producción (y los demás campos)
# cou_full <- leer_cou_indec(cfg)                     # Lee el COU completo con esa configuración
# validar_cou(cou_full)                               # Verifica sus identidades contables
# mip_full <- cou_a_mip(cou_full, "D")                # Construye la MIP rama x rama de 107 ramas
# validar_mip(mip_full)                               # Verifica la MIP
# an_full <- analizar_mip(mip_full)                   # Calcula A, L, multiplicadores y encadenamientos
# exportar_mip(mip_full, an_full, prefijo = "mip_2018_107ramas") # Exporta los resultados
