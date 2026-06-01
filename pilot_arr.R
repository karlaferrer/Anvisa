library (readxl)
library (janitor)
library(dplyr)
library(tidyr)


# Organização -------------------------------------------------------------

cases <- read_excel("GLP/glp0106.xlsx", sheet = "Cases")
drugs <- read_excel("GLP/glp0106.xlsx", sheet = "Drugs")
reactions <-  read_excel("GLP/glp0106.xlsx", sheet = "Reactions")
link <-  read_excel("GLP/glp0106.xlsx", sheet = "Drug - reaction link")

#Clean column names
cases <- clean_names(cases)
drugs <- clean_names(drugs)
reactions <- clean_names(reactions)
link <- clean_names(link)

#usos indevidos em nivel PT
#Usos fora da indicação e usos impróprios intencionais/problemas de uso do produto (HLGT)
usos <- c("Extração intencional de dispositivo médico pelo paciente",
          "Extração intencional de sistema de liberação de medicamento pelo paciente",
          "Omissão intencional da dose",
          "Problema de utilização intencional de dispositivo",
          "Problema de utilização intencional do produto",
          "Uso de produto para melhorar o desempenho",
          "Suspeita de uso não indicado no documento de referência",
          "Uso de dispositivo não descrito em bula (off label)",
          "Uso não descrito em bula (off label)",
          "Uso impróprio intencional de dispositivo",
          "Uso impróprio intencional do produto em criança",
          "Uso impróprio intencional do sistema de liberação de medicamento",
          "Uso indevido intencional do produto")

#pares medicamentos eventos de interesse - usos indevidos
link2 <- link |>
  filter(who_drug_active_ingredient_variant %in%
            c("Semaglutide",
              "Liraglutide",
              "Tirzepatide",
              "Insulin degludec;Liraglutide",
              "Insulin glargine;Lixisenatide",
              "Dulaglutide",
             "Lixisenatide")
            ) |> 
  filter(med_dra_preferred_term %in% usos
  )

#juntar dados demográficos
link2 <- link2 |> 
  left_join(cases, by ="umc_report_id", keep = F) 

library(lubridate)
#padronizar data
link2 <- link2 |> 
  mutate(
    data_pad = ymd(national_pv_centre_initial_receive_date),
    month_yr = format_ISO8601(data_pad, precision = "ym")
  )

# Agrupar reports de uso indevido por mes/ano
link2_g <- link2 |> 
  summarise(n_distinct((umc_report_id)),
            .by = c(who_drug_active_ingredient_variant.x, month_yr)) |> 
  rename( casos = "n_distinct((umc_report_id))")

#medicamentos
drugs2 <- drugs |>
  filter(who_drug_active_ingredient_variant %in%
           c("Semaglutide",
             "Liraglutide",
             "Tirzepatide",
             "Insulin degludec;Liraglutide",
             "Insulin glargine;Lixisenatide",
             "Dulaglutide",
             "Lixisenatide")
  )
  
drugs2 <- drugs2 |>
  left_join(cases, by = "umc_report_id", keep = F ) 

#criar campo data
drugs2 <- drugs2 |> 
  mutate(
    data_pad = ymd(national_pv_centre_initial_receive_date),
    month_yr = format_ISO8601(data_pad, precision = "ym")
  )
    
#agrupar por mes e ano e ingrediente
drugs_g <- drugs2 |> 
  group_by(who_drug_active_ingredient_variant.x, month_yr) |>
  summarise(n_distinct(umc_report_id)) |> 
  rename( total = "n_distinct(umc_report_id)")
  
#juntar para formar o percentual
drugs_g <- drugs_g |> 
  left_join(link2_g, by =c("who_drug_active_ingredient_variant.x",
                           "month_yr"), keep = F) |>
  replace_na(list(casos = 0)) |>
  mutate(
    perc = (casos / total) * 100
  )

#casos de pancreatite
panc <- c("Pancreatite",
          "Pancreatite aguda",
          "Pancreatite necrosante")

link_panc <- link |>
  filter(who_drug_active_ingredient_variant %in%
           c("Semaglutide",
             "Liraglutide",
             "Tirzepatide",
             "Insulin degludec;Liraglutide",
             "Insulin glargine;Lixisenatide",
             "Dulaglutide",
             "Lixisenatide")
  ) |> 
  filter(med_dra_preferred_term %in% panc)

#numero de notificacoes
length(unique(link_panc$umc_report_id))

#juntar dados demográficos
link_panc <- link_panc |> 
  left_join(cases, by ="umc_report_id", keep = F) 

#padronizar data
link_panc <- link_panc|> 
  mutate(
    data_pad = ymd(national_pv_centre_initial_receive_date),
    month_yr = format_ISO8601(data_pad, precision = "ym")
  )

# Agrupar reports de pancreatite por mes/ano
panc_g <- link_panc |> 
  summarise(n_distinct((umc_report_id)),
            .by = c(who_drug_active_ingredient_variant.x, month_yr)) |> 
  rename( casos_p = "n_distinct((umc_report_id))")

#juntar para formar o percentual - panc
drugs_g <- drugs_g |> 
  left_join(panc_g, by =c("who_drug_active_ingredient_variant.x",
                          "month_yr"), keep = F) |>
  replace_na(list(casos_p = 0)) |>
  mutate(
    perc_p = (casos_p / total) * 100
  )

#complementar meses que faltam
#vetor de meses e ano completo
datas <- seq(
  from = as.Date("2016-08-01"),
  to   = as.Date("2026-05-31"),
  by   = "month"
)

vetor_mes_ano <- format_ISO8601(datas, precision = "ym")
vetor_mes_ano <- as.data.frame(vetor_mes_ano)

library (stringr)
drugs_g <- vetor_mes_ano |> 
  left_join(drugs_g, by = c("vetor_mes_ano"= "month_yr")) |> 
  replace_na(list(total = 0,casos = 0, perc = 0,casos_p = 0, perc_p = 0)) |> 
  replace_na(list(who_drug_active_ingredient_variant.x = "Semaglutide")) |> 
  mutate(mes = str_sub(vetor_mes_ano, -2, -1),
         ano = str_sub(vetor_mes_ano, 1, 4)
  )

#criar variavel data
drugs_g$date <- ym(drugs_g$vetor_mes_ano)
drugs_g$month_yr = format_ISO8601(drugs_g$date, precision = "ym")

# Análise exploratoria ----------------------------------------------------

#Serie historica do perc de uso indevido com mediana
metrica <- "perc"
ylab <- "Usos fora da indicação (%)"
tit <- "Notificações com usos fora da indicação (%)"

library (ggplot2)
plot_serie <- drugs_g |>
  filter( month_yr > "2018-01") |>
  filter(who_drug_active_ingredient_variant.x %in%
           c("Semaglutide",
             "Tirzepatide")) |> 
  ggplot() +
  #geom_line(size = 0.3)+
  geom_vline(xintercept = as.Date("2025-07-01"),
             linetype="dotted", color = "black")+
  geom_hline(aes(yintercept = median(.data[[metrica]])), color = "darkgreen", linetype="dotted")+
  geom_line(aes(x= ym(month_yr),y=.data[[metrica]],group = who_drug_active_ingredient_variant.x,
                color = who_drug_active_ingredient_variant.x),linewidth = 0.4)+
  geom_point(aes(x= ym(month_yr),y=.data[[metrica]], group = who_drug_active_ingredient_variant.x,
                 color = who_drug_active_ingredient_variant.x), size = 0.8)+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  xlab("ano")+
  ylim(c(0,100))+
  ylab(ylab)+
  xlab(" ") + 
  labs(title = tit)+
  #labs(colour = NULL) +
  #scale_color_manual(values=c("black", "darkred")) +
  theme_bw()+
  theme(legend.position = "bottom")+
  theme(text = element_text(size = 8)) +
  theme(legend.title = element_blank())+
  theme(axis.text.x=element_text(angle = 60,hjust=1))+
  scale_color_manual(
    values = c(
      "Liraglutide" = "#0072B2",  # azul
      "Semaglutide" = "#D55E00", # laranja
      "Tirzepatide" = "#009E73", # verde
      "Dulaglutide" = "#CC79A7", # roxo/rosa
      "Insulin glargine;Lixisenatide" = "#E69F00",   # amarelo-ouro
      "Insulin degludec;Liraglutide" = "#56B4E9" # azul claro
    )
  )

ggsave(paste0("GLP/plot_serie_", metrica, ".png"), 
       plot_serie,
       width = 15,
       height = 10,
       unit = "cm",
       dpi = 300)

#tabela com IMC
#registros unicos
of_un <- link2 |>
  distinct(umc_report_id,
           .keep_all = TRUE)

# idade, sexo, IMC por medicamento
t3 <- of_un  |> 
  group_by(who_drug_active_ingredient_variant.x) |> 
  summarise(
    med_age = median(age, na.rm = TRUE),
    n_age = sum(!is.na(age)),
    prop_fem = mean(sex == "Female", na.rm = TRUE)*100,
    med_IMC = median(weight_kg / (height_cm/100)^2 , na.rm = TRUE),
    n_weight = sum(!is.na(weight_kg)),
    n_height = sum(!is.na(height_cm)),
    Total = n(),
  )

# linha do total 
t4 <- of_un |> 
  #group_by(who_drug_active_ingredient_variant_x) |> 
  summarise(
    med_age = median(age, na.rm = TRUE),
    n_age = sum(!is.na(age)),
    prop_fem = mean(sex == "Female", na.rm = TRUE)*100,
    med_IMC = median(weight_kg / (height_cm/100)^2 , na.rm = TRUE),
    n_weight = sum(!is.na(weight_kg)),
    n_height = sum(!is.na(height_cm)),
    Total = n()
  )

t4 <- cbind("Total", t4)
names(t4)[1] <- "who_drug_active_ingredient_variant.x"

t3 <- rbind(t3,t4)
rm(t4)


# Modelos -----------------------------------------------------------------

#variaveis para modelagem

#adicionar time para tendencia
drugs_g <- drugs_g |> 
  mutate(time = 1: nrow(drugs_g))

drugs_g$mes <- as.numeric(drugs_g$mes)
drugs_g$ano <- as.numeric(drugs_g$ano)

#incluir covariaveis step e ramp
#a partir de 2021
drugs_g <- drugs_g |>
  filter(ano >= 2021) |>
  mutate(
    step = case_when(
      ano >= 2026 ~ 1,
      ano == 2025 & mes >= 6 ~ 1,
      TRUE ~ 0
    ),
    ramp = case_when(
      ano < 2025 ~ 0,
      ano == 2025 & mes < 6 ~ 0,
      TRUE ~ (ano - 2025) * 12 + (mes - 6) + 1)
  )


# Pancreatite -------------------------------------------------------------


#Serie historica dos casos de pancreatite com mediana
library (ggplot2)
library(RColorBrewer)
plot_panc <- drugs_g |>
  filter( month_yr > "2018-01") |>
  filter(who_drug_active_ingredient_variant.x %in%
          c("Semaglutide",
           "Tirzepatide")) |> 
  ggplot() +
  geom_vline(xintercept = as.Date("2025-07-01"),
             linetype="dotted", color = "black")+
  geom_hline(aes(yintercept = median(perc_p)), color = "darkgreen", linetype="dotted")+
  geom_line(aes(x= ym(month_yr),y=perc_p,group = who_drug_active_ingredient_variant.x,
                color = who_drug_active_ingredient_variant.x),linewidth = 0.4)+
  geom_point(aes(x= ym(month_yr),y=perc_p, group = who_drug_active_ingredient_variant.x,
                 color = who_drug_active_ingredient_variant.x), size = 0.8)+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  xlab("ano")+
  ylim(c(0,100))+
  ylab("Casos de pancreatite (%)")+
  xlab(" ") + 
  labs(title = "Percentual das notificações com casos de pancreatite")+
  theme_bw()+
  theme(legend.position = "bottom")+
  theme(text = element_text(size = 8)) +
  theme(legend.title = element_blank())+
  theme(axis.text.x=element_text(angle = 60,hjust=1))+
  scale_color_manual(
    values = c(
      "Liraglutide" = "#0072B2",  # azul
      "Semaglutide" = "#D55E00", # laranja
      "Tirzepatide" = "#009E73", # verde
      "Dulaglutide" = "#CC79A7", # roxo/rosa
      "Insulin glargine;Lixisenatide" = "#E69F00",   # amarelo-ouro
      "Insulin degludec;Liraglutide" = "#56B4E9" # azul claro
    )
  )

ggsave("GLP/plot_panc_perc.png", 
       plot_panc,
       width = 15,
       height = 10,
       unit = "cm",
       dpi = 300)

#tabela descritiva
#registros unicos
panc_un <- link_panc |>
  distinct(umc_report_id,
           who_drug_active_ingredient_variant.x,
           med_dra_preferred_term.x,
           .keep_all = TRUE)


# idade, sexo, IMC por medicamento
t1 <- panc_un  |> 
  group_by(who_drug_active_ingredient_variant.x) |> 
  summarise(
    med_age = median(age, na.rm = TRUE),
    n_age = sum(!is.na(age)),
    prop_fem = mean(sex == "Female", na.rm = TRUE)*100,
    med_IMC = median(weight_kg / (height_cm/100)^2 , na.rm = TRUE),
    n_weight = sum(!is.na(weight_kg)),
    n_height = sum(!is.na(height_cm)),
    Total = n(),
  )

# linha do total 
t2 <- panc_un |> 
  #group_by(who_drug_active_ingredient_variant_x) |> 
  summarise(
    med_age = median(age, na.rm = TRUE),
    n_age = sum(!is.na(age)),
    prop_fem = mean(sex == "Female", na.rm = TRUE)*100,
    med_IMC = median(weight_kg / (height_cm/100)^2 , na.rm = TRUE),
    n_weight = sum(!is.na(weight_kg)),
    n_height = sum(!is.na(height_cm)),
    Total = n()
  )

t2 <- cbind("Total", t2)
names(t2)[1] <- "who_drug_active_ingredient_variant.x"

t1 <- rbind(t1,t2)
rm(t2)

# Exploratoria ------------------------------------------------------------
library(dplyr)
library(tsibble)
library(fabletools)
library(feasts)

sema_ts <- sema |> 
  mutate(
    date = yearmonth(vetor_mes_ano, format = "%Y-%m")
  ) |>
  as_tsibble(index = date)

#decomposicao
sema_ts |> 
  model(STL(casos ~ season(window = "periodic"))) %>%
  components() |> 
  autoplot()

ts_y <- ts(sema$casos,
           start = c(2019, 9),
           frequency = 12)

#serie bruta
plot(ts_y)

#meses
monthplot(ts_y)

#acf
sema_ts |>ACF(casos) |> 
  autoplot()

acf(ts_y)
#serie de memoria longa
#correlacao temporal forte
#sazonalidade nao é clara

acf(diff(ts_y))


# Modelos -----------------------------------------------------------------

#modelos para serie temporais inflados de zeros
#glmmtmb

#preparacao do dataset
sema <- drugs_g |> 
  filter(who_drug_active_ingredient_variant.x %in% "Semaglutide")

#vetor de meses e ano completo
datas <- seq(
  from = as.Date("2019-09-01"),
  to   = as.Date("2026-05-18"),
  by   = "month"
)

vetor_mes_ano <- format_ISO8601(datas, precision = "ym")

vetor_mes_ano <- as.data.frame(vetor_mes_ano)

library (stringr)
sema <- vetor_mes_ano |> 
  left_join(sema, by = c("vetor_mes_ano"= "month_yr")) |> 
  replace_na(list(total = 0,casos = 0, perc = 0,casos_p = 0, perc_p = 0)) |> 
  replace_na(list(who_drug_active_ingredient_variant.x = "Semaglutide")) |> 
  mutate(mes = str_sub(vetor_mes_ano, -2, -1),
         ano = str_sub(vetor_mes_ano, 1, 4)
         )
  
#retirar mes de maio incompleto e comecar em jan/21
library(dplyr)
library(lubridate)
sema$date <- ym(sema$vetor_mes_ano)
sema <- sema |> 
  filter (date < "2026-05-01") |> 
  filter (date > "2020-12-01")


#adicionar time para tendencia
sema <- sema |> 
  mutate(time = 1: nrow(sema))

sema$mes <- as.numeric(sema$mes)
sema$ano <- as.numeric(sema$ano)

#incluir covariaveis step e ramp
sema <- sema |> 
  mutate(
    step = case_when(
      ano >= 2026 ~ 1,
      ano == 2025 & mes >= 6 ~ 1,
      TRUE ~ 0
    ),
    ramp = case_when(
      ano < 2025 ~ 0,
      ano == 2025 & mes < 6 ~ 0,
      TRUE ~ (ano - 2025) * 12 + (mes - 6) + 1)
    )


library(glmmTMB)
# modelo com harmonicos e tendencia suave
m2 <- glmmTMB(
  casos ~
    time + #tendencia sem spline
    step +
    ramp +
    sin(2*pi*time/12) + #sazonalidade suave com harmonicos
    cos(2*pi*time/12),
  
  family = nbinom2(),
  data = sema
)

#checar autocorrelacao residual
acf(residuals(m2))

library(DHARMa)
res <- simulateResiduals(m2)
testTemporalAutocorrelation(
  res,
  time = sema$time
)

#checar residuos
plot(res)
testZeroInflation(res)
testDispersion(res)

# modelo com efeito simples de cada mes e tendencia suave
m3 <- glmmTMB(
  casos ~
    time + #tendencia sem spline
    step +
    ramp +
    factor(mes),
  
  family = nbinom2(),
  data = sema
)

#checar autocorrelacao residual
acf(residuals(m3))

library(DHARMa)
res3 <- simulateResiduals(m3)
testTemporalAutocorrelation(
  res3,
  time = sema$time
)

#checar residuos
plot(res3)
testZeroInflation(res3)
testDispersion(res3)

library(splines)

#modelo com harmonicos para sazonalidade - suave
#e com spline para tendencia - ajuste de tendencia nao linear
library(splines)
m4 <- glmmTMB(
  casos ~
    ns(time, df = 3) +
    step +
    ramp +
    sin(2*pi*time/12) +
    cos(2*pi*time/12),
  
  family = nbinom2(),
  data = sema
)

summary(m4)

#diagnostico m4
library(DHARMa)
res4 <- simulateResiduals(m4)
testTemporalAutocorrelation(
  res4,
  time = sema$time
)

#checar residuos
plot(res4)
testZeroInflation(res4)
testDispersion(res4)

#IC para as covariaveis step e ramp
exp(confint(m4, parm = c("step", "ramp")))

#modelo conceitual
#log(μt)=f(time)+βstep​stept​+βramp​rampt​+sazonalidade

#Após a intervenção

#O nível inicial:
#praticamente não muda muito (step não significativo).
#Mas a inclinação:
#passa a cair progressivamente (ramp negativo).

#a intervenção não produziu uma alteração abrupta imediata nas contagens, 
#mas esteve associada a uma redução gradual e progressiva da série ao longo do 
#tempo após sua implementacao

#IMPORTANTE
#efeito adicional pós-intervenção ajustado pela tendência basal e sazonalidade.
#O termo de mudança de inclinação pós-intervenção (ramp) foi estatisticamente 
#significativo (β = -0.234; p = 0.004), indicando redução gradual na tendência 
#temporal após a intervenção. A exponencial do coeficiente sugere diminuição 
#média de aproximadamente 21% por unidade temporal no número esperado de casos, 
#ajustado para tendência temporal basal, sazonalidade e sobredispersão.

#efeito cumulativo

#modelo com harmonicos para sazonalidade - suave
#e com spline para tendencia - ajuste de tendencia nao linear mais suave
library(splines)
m5 <- glmmTMB(
  casos ~
    ns(time, df = 3) +
    step +
    ramp +
    sin(2*pi*time/12) +
    cos(2*pi*time/12),
  
  family = nbinom2(),
  data = sema
)

summary(m5)

#diagnostico m5
library(DHARMa)
res5 <- simulateResiduals(m5)
testTemporalAutocorrelation(
  res5,
  time = sema$time
)

#checar residuos
plot(res5)
testZeroInflation(res5)
testDispersion(res5)

#tendencia piecewise
#usa tendencia basal da serie 
#ate a intervencao
#durante a intervencao tendencia fica estavel 
sema <- sema |> 
  mutate(
    pre_time = case_when(time < 70 ~ time,
                         TRUE ~ 70)
  )

m6 <- glmmTMB(
  casos ~
    pre_time +
    step +
    ramp +
    sin(2*pi*time/12) +
    cos(2*pi*time/12),
  
  family = nbinom2(),
  data = sema
)

#diagnostico m6
library(DHARMa)
res6 <- simulateResiduals(m6)
testTemporalAutocorrelation(
  res6,
  time = sema$time
)

#checar residuos
plot(res6)
testZeroInflation(res6)
testDispersion(res6)


# Valores preditos --------------------------------------------------------

pred <- predict(
  m2,
  type = "link",
  se.fit = TRUE
)

sema$pred <- exp(pred$fit)

sema$lwr <- exp(pred$fit - 1.96 * pred$se.fit)

sema$upr <- exp(pred$fit + 1.96 * pred$se.fit)

library(ggplot2)
ggplot(sema, aes(x = time)) +
  
  geom_ribbon(aes(ymin = lwr,
                  ymax = upr),
              alpha = 0.2) +
  
  geom_line(aes(y = pred),
            linewidth = 1, color ="red") +
  
  geom_point(aes(y = casos),
             alpha = 0.7) 
  
  #geom_line(aes(y = casos),
   #         alpha = 0.5)

#IC para as covariaveis step e ramp
exp(confint(m2, parm = c("step", "ramp")))

#efeito acumulado da intervenção

# Contrafato --------------------------------------------------------------
#criar o contrafato
contra <- sema
contra$step <- 0
contra$ramp <- 0

#predicao do contrafato
pred_cf <- predict(
  m2,
  newdata = contra,
  type = "link",
  se.fit = TRUE
)

contra$pred_cf <- exp(pred_cf$fit)
contra$lwr_cf <- exp(pred_cf$fit - 1.96 * pred_cf$se.fit)
contra$upr_cf <- exp(pred_cf$fit + 1.96 * pred_cf$se.fit)

data_interv <- ym("2025-06")

contra_pos <- contra |>
  dplyr::filter(date >= data_interv)

# no grafico
plot_m2 <- ggplot(sema, aes(x = date)) +
  
  geom_ribbon(aes(ymin = lwr,
                  ymax = upr),
              fill = "red",
              alpha = 0.2) +
  
  geom_line(aes(y = pred),
            color = "red",
            linewidth = 0.8) +
  
  geom_ribbon(data = contra_pos,
              aes(ymin = lwr_cf,
                  ymax = upr_cf),
              fill = "blue",
              alpha = 0.15) +
  
    geom_line(data = contra_pos,
            aes(y = pred_cf),
            color = "blue",
            linewidth = 0.8) +
  
 # geom_line(aes(y = casos),
  #          color = "black",
   #         alpha = 0.7) +
  
  geom_point(aes(y = casos),
             color = "black",
             size = 1) +
  
  geom_vline(xintercept = data_interv,
             linetype = "dashed") +
  ylab("Usos fora da indicação")+
  xlab(" ") + 
  labs(title = "Notificações com usos fora da indicação")+
  
  theme_bw()

ggsave("GLP/plot_m2.png", 
       plot_m2,
       width = 15,
       height = 10,
       unit = "cm",
       dpi = 300)

#IC para o modelo m2
exp(confint(m2, parm = c("step", "ramp")))

#total de notificações para semaglutida

#Serie historica do perc de uso indevido com mediana
library (ggplot2)
plot_serie <- drugs_g |>
  filter( month_yr > "2018-01" & month_yr < "2026-05" ) |>
  filter(who_drug_active_ingredient_variant.x %in%
           c("Semaglutide")) |> 
  ggplot() +
  #geom_line(size = 0.3)+
  geom_vline(xintercept = as.Date("2025-07-01"),
             linetype="dotted", color = "black")+
  geom_hline(aes(yintercept = median(total)), color = "darkgreen", linetype="dotted")+
  geom_line(aes(x= ym(month_yr),y=total, group = who_drug_active_ingredient_variant.x,
                color = who_drug_active_ingredient_variant.x),linewidth = 0.4)+
  geom_point(aes(x= ym(month_yr),y=total, group = who_drug_active_ingredient_variant.x,
                 color = who_drug_active_ingredient_variant.x), size = 0.8)+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  xlab("ano")+
  ylim(c(0,100))+
  ylab("N. notificações")+
  xlab(" ") + 
  labs(title = "Total de Notificações - Semaglutida")+
  #labs(colour = NULL) +
  #scale_color_manual(values=c("black", "darkred")) +
  theme_bw()+
  theme(legend.position = "bottom")+
  theme(text = element_text(size = 8)) +
  theme(legend.title = element_blank())+
  theme(axis.text.x=element_text(angle = 60,hjust=1))+
  scale_color_manual(
    values = c(
      "Liraglutide" = "#0072B2",  # azul
      "Semaglutide" = "#D55E00", # laranja
      "Tirzepatide" = "#009E73", # verde
      "Dulaglutide" = "#CC79A7", # roxo/rosa
      "Insulin glargine;Lixisenatide" = "#E69F00",   # amarelo-ouro
      "Insulin degludec;Liraglutide" = "#56B4E9" # azul claro
    )
  )

ggsave("GLP/plot_sema.png", 
       plot_serie,
       width = 15,
       height = 10,
       unit = "cm",
       dpi = 300)


# INLA  -------------------------------------------------------------------
library(INLA)
#contrafato - sem vacinacao
contrafato = sema |>  
  filter(step == 1) |> 
  mutate(
    step = 0,
    ramp = 0,
    casos = NA
  )

eq = casos ~ 1 + step + ramp +
#Sazonalidade mensal
f(mes, model = "rw2", cyclic = T) + 
#potencial tendencia temporal (linear)
time

#dataset
sema.inla <- sema |> 
  bind_rows(contrafato) 
  

m_i = inla(formula = eq, family = "nbinomial", 
            #offset = log(total/10^5), 
            control.compute = control.compute(waic = T, dic = T), 
            control.predictor=list(compute = TRUE, link = 1),
            data = sema.inla)

# Plot

  idx = which(is.na(sema.inla$casos))
  
  predicao<- bind_cols(exp(m_final$summary.linear.predictor[idx,]), time = sema.inla[idx,]$time,
                       ofs = exp(m_final$offset.linear.predictor)[idx], ano = sema.inla[idx,]$ano,
                       mes = sema.inla[idx,]$mes) |>
    mutate (data = paste (ano, mes, sep = "-")) |>
    mutate(data = ym(data))
  
  predicao2<- bind_cols(exp(m_final$summary.linear.predictor[-idx,]), time = sema.inla[-idx,]$time,
                        ofs = exp(m_final$offset.linear.predictor)[-idx], ano = sema.inla[-idx,]$ano,
                        mes = sema.inla[-idx,]$mes) |>
    mutate (data = paste (ano, mes, sep = "-")) |>
    mutate(data = ym(data))
  
  contra_plot <- ggplot() +
    geom_ribbon(data = predicao2,
                aes(x = as.Date(data),
                    y = (`0.5quant`)/ofs,
                    color ="Ajustado",
                    ymin = (`0.025quant`)/ofs,
                    ymax = (`0.975quant`)/ofs),
                alpha = 0.5, fill = "lightblue3", linetype = 0)+
    geom_line(data = predicao2,
              mapping = aes(x = as.Date(data), y = (`0.5quant`)/ofs, color ="Ajustado")
    ) +
    geom_ribbon(data = predicao,
                mapping = aes(x = as.Date(data),
                              y = (`0.5quant`)/ofs,
                              color ="Contrafato",
                              ymin = (`0.025quant`)/ofs,
                              ymax = (`0.975quant`)/ofs),
                alpha = 0.5, fill = "aquamarine3", linetype = 0) +
    geom_line(data = predicao,
              mapping = aes(x = as.Date(data), y = (`0.5quant`)/ofs, color ="Contrafato")
    ) +
    geom_point(data = sema.inla[-idx,],
               mapping = aes(x = as.Date(date), y = casos, color = "Observado"), size = 1
    ) +
    geom_vline(xintercept = as.Date("2025-06-01"),
              linetype="dashed", color = "black") +
    labs(y = "Usos fora da indicação", x = " ") +
    scale_color_manual(values=c("turquoise4", "darkblue", "black")) +
    labs(colour = NULL) +
    theme_bw()+
    theme(legend.position = "bottom")+
    #ggtitle(ea[x])+
    theme(plot.title = element_text(size = 13))+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(text = element_text(size = 13))

  ggsave("glp/inla_plot.png", contra_plot,
         width = 17, 
         height = 14, 
         unit = "cm", 
         dpi = 300)


# Inla com ar1 para trend -------------------------------------------------

  hyper = list(
    prec = list(
      prior = "pc.prec",
      param = c(1, 0.01)
    )
  )
  m_final <- inla(
    casos ~
      step +
      ramp +
      sin(2*pi*time/12) +
      cos(2*pi*time/12) +
      f(time, model = "ar1"), #ou rw1
    
    family = "nbinomial",
    control.compute = control.compute(waic = T, dic = T), 
    control.predictor=list(compute = TRUE, link = 1),
    data = sema.inla
  )
  
  #Modelo linear generalizado (GLM) para contagens com distribuição binomial negativa, aplicado em uma série temporal interrompida 
  #(Interrupted Time Series, ITS)
  #ajustando por componentes de tendência + intervenção + sazonalidade.
  #Regressão harmônica sazonal- sazonalidade harmônica - Fourier terms;
  #Interrupted time series analysis using 
  #negative binomial regression with harmonic seasonal adjustment.
  
  #ttodos os efeitos têm forma previamente especificada.
  #tendencia: linear
  #intervenção: mudança abrupta (step);mudança linear gradual (ramp)
  #Sazonalidade:senoidal anual.

# Modelo pancreatite ------------------------------------------------------

  
  #preparar dataset
  

  #modelo pancreatite
  library(glmmTMB)
  # modelo com harmonicos e tendencia suave
  m2p <- glmmTMB(
    casos_p ~
      time + #tendencia sem spline
      step +
      ramp +
      sin(2*pi*time/12) + #sazonalidade suave com harmonicos
      cos(2*pi*time/12),
    
    family = nbinom2(),
    data = sema
  )
  
  #checar autocorrelacao residual
  acf(residuals(m2p))
  
  library(DHARMa)
  res_p <- simulateResiduals(m2p)
  testTemporalAutocorrelation(
    res_p,
    time = sema$time
  )
  
  #checar residuos
  plot(res_p)
  testZeroInflation(res_p)
  testDispersion(res_p)
  
  #IC para as covariaveis step e ramp
  exp(confint(m2p, parm = c("step", "ramp")))
  
  # Contrafato pancreatite --------------------------------------------------------------

  #valores preditos
  predp <- predict(
    m2p,
    type = "link",
    se.fit = TRUE
  )
  
  sema$predp <- exp(predp$fit)
  
  sema$lwrp <- exp(predp$fit - 1.96 * predp$se.fit)
  
  sema$uprp <- exp(predp$fit + 1.96 * predp$se.fit)
  
  
  #criar o contrafato
  contra <- sema
  contra$step <- 0
  contra$ramp <- 0
  
  #predicao do contrafato
  pred_cfp <- predict(
    m2p,
    newdata = contra,
    type = "link",
    se.fit = TRUE
  )
  
  contra$pred_cfp <- exp(pred_cfp$fit)
  contra$lwr_cfp <- exp(pred_cfp$fit - 1.96 * pred_cfp$se.fit)
  contra$upr_cfp <- exp(pred_cfp$fit + 1.96 * pred_cfp$se.fit)
  
  data_interv <- ym("2025-06")
  
  contra_pos <- contra |>
    dplyr::filter(date >= data_interv)
  
  
  # no grafico
  library(ggplot2)
  plot_m2p <- ggplot(sema, aes(x = date)) +
    
    geom_ribbon(aes(ymin = lwrp,
                    ymax = uprp),
                fill = "red",
                alpha = 0.2) +
    
    geom_line(aes(y = predp),
              color = "red",
              linewidth = 0.8) +
    
    geom_ribbon(data = contra_pos,
                aes(ymin = lwr_cfp,
                    ymax = upr_cfp),
                fill = "blue",
                alpha = 0.15) +
    
    geom_line(data = contra_pos,
              aes(y = pred_cfp),
              color = "blue",
              linewidth = 0.8) +
    
    # geom_line(aes(y = casos),
    #          color = "black",
    #         alpha = 0.7) +
    
    geom_point(aes(y = casos_p),
               color = "black",
               size = 1) +
    
    geom_vline(xintercept = data_interv,
               linetype = "dashed") +
    ylab("Casos pancreatite")+
    xlab(" ") + 
    labs(title = "Notificações com casos de pancreatite")+
    
    theme_bw()
  
  ggsave("GLP/plot_m2p.png", 
         plot_m2p,
         width = 15,
         height = 10,
         unit = "cm",
         dpi = 300)
  