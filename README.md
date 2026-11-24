# codigos
# primer codigo en R
#primer codigo- filtra meses y distritos y los 4 modelos los hace correr 


install.packages("DHARMa")
install.packages("car")
install.packages("performance")
install.packages("glmmTMB")
install.packages("readxl")
install.packages("lme4")

library(car)
library(performance) ## Colinealidad
library(glmmTMB) ### modelo BN
library(DHARMa)
library(readxl)
library(lme4)

muert3 <- read_excel("muert3.xlsx")
View(muert3)


## Limpiar los distritos: San Bartolo, Santa Maria del Mar, Santa Rosa, Punta Negra, Pucusana y
## Punta Hermosa.

muert4=muert3[-which(muert3$DISTR %in% c(29,38,39,27,24,26)),]

## Limpiar los meses enero-abril 2022

muert5=muert4[-which(muert4$FECHA1 %in% c("2022-01","2022-02","2022-03","2022-04")),]
muert5$sin_mes <- sin(2 * pi * as.numeric(muert5$MES) / 12)
muert5$cos_mes <- cos(2 * pi * as.numeric(muert5$MES) / 12)

muert5$TEMP_z <- as.numeric(scale(muert5$TEMP))
muert5$HUMED_z <- as.numeric(scale(muert5$HUMED))

#### modelo lineal sin transformación logarítmica
model1 <- lmer(TASA_M ~ TEMP+HUMED+ sin_mes+cos_mes + (1 | DISTR), data = muert5)
summary(model1)

#### modelo lineal con transformación logarítmica
model2 <- lmer(log(TASA_M) ~ TEMP+HUMED+ sin_mes+cos_mes + (1 | DISTR), data = muert5)
summary(model2)

#### modelo Poisson
model.po=glmer(TNUM ~ TEMP_z+HUMED_z+ sin_mes+cos_mes+ offset(log(Pob))+(1 | DISTR), data = muert5,
               family = poisson(link = "log"))

# Revisar colinealidad de los efectos fijos
check_collinearity(model.po)

# Revisar sobredisperciÃ³n
check_overdispersion(model.po)


#### Binomial Negativa

model.nb <- glmmTMB(
  TNUM ~ TEMP_z + HUMED_z + sin_mes + cos_mes + offset(log(Pob)) + (1 | DISTR),
  data = muert5,
  family = nbinom2 # Binomial negativa estÃ¡ndar
)

summary(model.nb)


# Comparar AIC (el valor mÃ¡s bajo indica el mejor ajuste)
AIC(model.po, model.nb)


#_____________________________________________________________
# Predicciones
#_____________________________________________________________
# 1. Modelo Normal (lmer sobre TASA_M) -> Devuelve la Tasa predicha

muert5$pred_normal <- predict(model1)*muert5$Pob/10000
muert5$pred_normal2 <- exp(predict(model2))*muert5$Pob/10000 ## con transformaciÃ³n log

# 2. Modelo de Poisson (glmer con offset) -> Devuelve el conteo esperado de muertes
muert5$pred_poisson <- predict(model.po, type = "response")

# 3. Modelo Binomial Negativo (glmmTMB con offset) -> Devuelve el conteo esperado de muertes
muert5$pred_nb <- predict(model.nb, type = "response")

##################################################################################
##################################################################################

# segundo codigo en R
#segundo codigo- compara los 4 modelos 

library(lme4)
library(glmmTMB)
library(ggplot2)
library(dplyr)
library(tidyr)
library(writexl)

# 2. Preparar datos limpios y variables necesarias (incluyendo componentes cÃ­clicos)
c_offset <- 0.01 # Constante pequeÃ±a para evitar log(0) en el modelo Log-Normal

datos_limpios <- muert5 %>%
  filter(!is.na(TNUM), !is.na(TASA_M), !is.na(TEMP), !is.na(HUMED), 
         !is.na(MES), !is.na(Pob), !is.na(DISTR)) %>%
  mutate(
    TEMP_z = as.numeric(scale(TEMP)),
    HUMED_z = as.numeric(scale(HUMED)),
    sin_mes = sin(2 * pi * as.numeric(MES) / 12),
    cos_mes = cos(2 * pi * as.numeric(MES) / 12),
    log_TASA_M = log(TASA_M + c_offset) # Variable respuesta para el modelo Log-Normal
  )

# Detectar automÃ¡ticamente el multiplicador de tu TASA_M (en tu script 1 usas 10000)
fila_ejemplo <- which(datos_limpios$TNUM > 0)[1]
multiplicador <- round((datos_limpios$TASA_M[fila_ejemplo] * datos_limpios$Pob[fila_ejemplo]) / datos_limpios$TNUM[fila_ejemplo])

# 3. Ajustar los 4 modelos usando el factor cÃ­clico (sin_mes + cos_mes)
# Modelo 1: Normal directo sobre TASA_M
mod_normal <- lmer(
  TASA_M ~ TEMP_z + HUMED_z + sin_mes + cos_mes + (1 | DISTR),
  data = datos_limpios
)

# Modelo 2: Log-Normal sobre log(TASA_M + c)
mod_lognormal <- lmer(
  log_TASA_M ~ TEMP_z + HUMED_z + sin_mes + cos_mes + (1 | DISTR),
  data = datos_limpios
)

# Modelo 3: Poisson sobre conteos con offset
mod_poisson <- glmer(
  TNUM ~ TEMP_z + HUMED_z + sin_mes + cos_mes + offset(log(Pob)) + (1 | DISTR),
  data = datos_limpios,
  family = poisson(link = "log"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

# Modelo 4: Binomial Negativo sobre conteos con offset
mod_nb <- glmmTMB(
  TNUM ~ TEMP_z + HUMED_z + sin_mes + cos_mes + offset(log(Pob)) + (1 | DISTR),
  data = datos_limpios,
  family = nbinom2
)

# 4. Obtener todas las predicciones en la escala original de TASA_M
sigma2_ln <- sigma(mod_lognormal)^2

datos_limpios <- datos_limpios %>%
  mutate(
    tasa_pred_normal    = predict(mod_normal),
    tasa_pred_lognormal = pmax(0, exp(predict(mod_lognormal) + sigma2_ln / 2) - c_offset),
    tasa_pred_poisson   = (predict(mod_poisson, type = "response") / Pob) * multiplicador,
    tasa_pred_nb        = (predict(mod_nb, type = "response") / Pob) * multiplicador
  )

# 5. Tabla resumen de contraste evaluando errores en TASA_M
tabla_comparacion_4 <- data.frame(
  Modelo = c(
    "1. Normal (TASA_M)", 
    "2. Log-Normal (log TASA_M)", 
    "3. Poisson (glmer)", 
    "4. Binomial Negativo (glmmTMB)"
  ),
  AIC = c(NA, NA, AIC(mod_poisson), AIC(mod_nb)),
  RMSE_Tasa = c(
    sqrt(mean((datos_limpios$TASA_M - datos_limpios$tasa_pred_normal)^2)),
    sqrt(mean((datos_limpios$TASA_M - datos_limpios$tasa_pred_lognormal)^2)),
    sqrt(mean((datos_limpios$TASA_M - datos_limpios$tasa_pred_poisson)^2)),
    sqrt(mean((datos_limpios$TASA_M - datos_limpios$tasa_pred_nb)^2))
  ),
  MAE_Tasa = c(
    mean(abs(datos_limpios$TASA_M - datos_limpios$tasa_pred_normal)),
    mean(abs(datos_limpios$TASA_M - datos_limpios$tasa_pred_lognormal)),
    mean(abs(datos_limpios$TASA_M - datos_limpios$tasa_pred_poisson)),
    mean(abs(datos_limpios$TASA_M - datos_limpios$tasa_pred_nb))
  ),
  Tasas_Negativas = c(
    sum(datos_limpios$tasa_pred_normal < 0),
    sum(datos_limpios$tasa_pred_lognormal < 0),
    sum(datos_limpios$tasa_pred_poisson < 0),
    sum(datos_limpios$tasa_pred_nb < 0)
  )
)

print(tabla_comparacion_4)

# Exportar a Excel
write_xlsx(tabla_comparacion_4, "comparacion_4_modelos_ciclico.xlsx")

# 6. GrÃ¡fico comparativo de los 4 modelos
datos_grafico <- datos_limpios %>%
  select(TASA_M, starts_with("tasa_pred_")) %>%
  pivot_longer(cols = starts_with("tasa_pred_"), names_to = "Modelo", values_to = "Tasa_Predicha") %>%
  mutate(Modelo = recode(Modelo,
                         "tasa_pred_normal"    = "1. Normal",
                         "tasa_pred_lognormal" = "2. Log-Normal",
                         "tasa_pred_poisson"   = "3. Poisson",
                         "tasa_pred_nb"        = "4. Binomial Negativo"
  ))

ggplot(datos_grafico, aes(x = TASA_M, y = Tasa_Predicha, color = Modelo)) +
  geom_point(alpha = 0.4) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  facet_wrap(~ Modelo, ncol = 2) +
  labs(
    title = "ComparaciÃ³n de 4 Modelos: Tasa Observada vs. Tasa Predicha",
    subtitle = "EvaluaciÃ³n en escala original de TASA_M con estacionalidad cÃ­clica (sin/cos)",
    x = "Tasa Observada (TASA_M)",
    y = "Tasa Predicha"
  ) +
  theme_minimal() +
  theme(legend.position = "none")






