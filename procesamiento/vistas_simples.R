# ------------------------------------------------------------------------------
# Versiones simplificadas de las tablas y figuras del capítulo de resultados.
# Solo para mirar con el script abierto: no guarda nada ni se usa en el libro.
# Ejecutar desde la raíz del proyecto (tesis/).
# ------------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggalluvial)   # si no está instalado: install.packages("ggalluvial")

load("input/datos_igualdad_oportunidades.RData")
d <- datos_io


# 1. Tabla: percepción de igualdad de oportunidades (p1_9) según ola ----------

table(d$p1_9_ord, d$ola)                               # frecuencias
round(100 * prop.table(table(d$p1_9_ord, d$ola), 2), 1) # % por ola


# 2. Figura: distribución de p1_9 según ola y cohorte -------------------------

ggplot(dplyr::filter(d, !is.na(p1_9_ord)), aes(x = factor(ola), fill = p1_9_ord)) +
  geom_bar(position = "fill") +
  facet_wrap(~ cohorte_media)                          # 0 = básica, 1 = media


# 3. Datos de panel: estudiantes que responden ambas olas ---------------------

w <- d |>
  dplyr::select(id_estudiante, ola, p1_9) |>
  pivot_wider(names_from = ola, values_from = p1_9, names_prefix = "o") |>
  dplyr::filter(!is.na(o1), !is.na(o2)) |>
  left_join(dplyr::filter(d, ola == 1) |> dplyr::select(id_estudiante, cohorte_media), by = "id_estudiante")

table(ola1 = w$o1, ola2 = w$o2)                        # matriz de transiciones


# 4. Figura: aluvial de transiciones entre olas (total) -----------------------

ggplot(count(w, o1, o2), aes(axis1 = factor(o1), axis2 = factor(o2), y = n)) +
  geom_alluvium() +
  geom_stratum()


# 5. Figura: aluvial de transiciones según cohorte ----------------------------

ggplot(count(w, cohorte_media, o1, o2), aes(axis1 = factor(o1), axis2 = factor(o2), y = n)) +
  geom_alluvium() +
  geom_stratum() +
  facet_wrap(~ cohorte_media, scales = "free_y")


# 6. Tabla: variables independientes (ola 2, casos completos) -----------------

vi <- c("p2_3", "notas", "univ", "merit_soc", "privilegio", "pref_merit", "pref_nomerit",
        "mujer_otro", "extranjero", "cohorte_media")
m2 <- na.omit(d[d$ola == 2, c("id_estudiante", "p1_9_ord", vi)])

round(sapply(m2[vi], mean), 2)                         # medias (y % en las dicotómicas)
round(sapply(m2[vi], sd), 2)                           # desviaciones estándar

items <- c("p2_3", paste0("p1_", 1:8))                 # ordinales de 1 a 4
m2i <- d[d$ola == 2 & d$id_estudiante %in% m2$id_estudiante, items]
round(100 * sapply(m2i, function(x) prop.table(table(factor(x, 1:4)))), 1)  # % por categoría


# 7. Figura: correlaciones policóricas (ola 1) --------------------------------

x_w1 <- d |> dplyr::filter(ola == 1) |> dplyr::select(p1_9, paste0("p1_", 1:8)) |> mutate(across(everything(), as.numeric))
rho <- psych::polychoric(x_w1)$rho
round(rho, 2)

ggplot(as.data.frame(as.table(rho)), aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = round(Freq, 2)))
