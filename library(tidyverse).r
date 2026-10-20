library(tidyverse)
options(scipen = 999)
library(lme4)
library(ordinal)
library(lmerTest)



load("input/datos_igualdad_oportunidades.RData")
colnames(datos_io)

datos_io$trat_ctrl <- ifelse(datos_io$ola == 2, 0, datos_io$trat)
datos_io$id <- factor(datos_io$id_estudiante)


lme <- clmm(p1_9_ord ~ merit_esc + notas + univ + ola2 +
              mujer_otro + extranjero + cohorte_media + trat_ctrl +
              (1 | id_estudiante), data = datos_io)


lme <- lmer(p1_9 ~ merit_esc + notas + univ + ola2 +
              mujer_otro + extranjero + cohorte_media + trat_ctrl +
              (1 | id_curso), data = datos_io, REML = FALSE)

summary(lme)

m2 <- clmm(p1_9_ord ~ merit_esc * educ_madre_ord + notas + univ + ola2 +
             mujer_otro + extranjero + cohorte_media + trat_ctrl +
             (1 | id_curso), data = datos_io, link = "logit", nAGQ = 5)

summary(m2)





# --- Descomposición en tres componentes -------------------------------
# x - media general = (x - media estudiante)            -> _w : dentro del estudiante (cambio entre olas)
#                   + (media estudiante - media curso)  -> _b : entre estudiantes del mismo curso
#                   + (media curso - media general)     -> _m_cur_gmc : entre cursos
for (v in c("merit_esc", "notas", "univ", "pref_merit", "mujer_otro", "extranjero")) {
  datos_io[[paste0(v, "_w")]] <- datos_io[[paste0(v, "_cwc_est")]]
  datos_io[[paste0(v, "_b")]] <- datos_io[[paste0(v, "_m_est")]] - datos_io[[paste0(v, "_m_cur")]]
}

# Solo estudiantes con curso (se excluyen los 35 que solo están en ola 2)
d3 <- subset(datos_io, !is.na(id_curso))

# --- Fórmula -----------------------------------------------------------
f3 <- p1_9 ~
  merit_esc_w + merit_esc_b + merit_esc_m_cur_gmc +     # H1 en los tres niveles
  notas_w + notas_b + notas_m_cur_gmc +                 # rendimiento
  univ_w + univ_b + univ_m_cur_gmc +                    # H3
  ola2 +                                                # H5
  mujer_otro_b + mujer_otro_m_cur_gmc +
  extranjero_b + extranjero_m_cur_gmc +
  pref_merit_w + pref_merit_b + pref_merit_m_cur_gmc +
  cohorte_media + trat_ctrl +
  (1 | id_curso) + (1 | id_estudiante)

# --- Lineal --------------------------------------------------------------
m3_lin <- lmer(f3, data = d3, REML = FALSE)
summary(m3_lin)

# --- Logit ordinal -------------------------------------------------------
m3_ord <- clmm(update(f3, p1_9_ord ~ .), data = d3, link = "logit")
summary(m3_ord)


install.packages("performance")   # si no lo tienes
library(performance)

r2_nakagawa(m3_lin)
icc(m3_lin)   # de paso, la proporción de varianza por estudiante y por curso



library(performance)

f_fijo <- lme4::nobars(f3)   # la misma fórmula, sin (1 | id_curso) + (1 | id_estudiante)

# Lineal: R² clásico
m_ols <- lm(f_fijo, data = d3)
summary(m_ols)$r.squared
summary(m_ols)$adj.r.squared

# Ordinal: pseudo-R² (no existe R² clásico para logit ordinal)
library(ordinal)
m_clm <- clm(update(f_fijo, p1_9_ord ~ .), data = d3, link = "logit")
r2_nagelkerke(m_clm)
r2_mcfadden(m_clm)



library(lme4)
library(ordinal)
library(performance)

load("input/datos_igualdad_oportunidades.RData")
d3 <- subset(datos_io, !is.na(id_curso))

f_todo <- p1_9 ~
  merit_esc_cwc_est    + merit_esc_m_est_cwc_cur    + merit_esc_m_cur_gmc +     # meritocracia escolar
  merit_soc_cwc_est    + merit_soc_m_est_cwc_cur    + merit_soc_m_cur_gmc +     # percepción meritocrática
  privilegio_cwc_est   + privilegio_m_est_cwc_cur   + privilegio_m_cur_gmc +    # percepción no meritocrática
  pref_merit_cwc_est   + pref_merit_m_est_cwc_cur   + pref_merit_m_cur_gmc +    # preferencia meritocrática
  pref_nomerit_cwc_est + pref_nomerit_m_est_cwc_cur + pref_nomerit_m_cur_gmc +  # preferencia no meritocrática
  notas_cwc_est + notas_m_est_cwc_cur + notas_m_cur_gmc +
  univ_cwc_est  + univ_m_est_cwc_cur  + univ_m_cur_gmc +
  ola2 + mujer_otro_m_est_cwc_cur + mujer_otro_m_cur_gmc +
  extranjero_m_est_cwc_cur + extranjero_m_cur_gmc +
  cohorte_media + trat_ctrl +
  (1 | id_curso) + (1 | id_estudiante)

m_lin <- lmer(f_todo, data = d3, REML = FALSE)
summary(m_lin)
check_collinearity(m_lin)
r2_nakagawa(m_lin)

m_ord <- clmm(update(f_todo, p1_9_ord ~ .), data = d3, link = "logit")
summary(m_ord)

