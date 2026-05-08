library (readxl)
library (janitor)
library(dplyr)
library(tidyr)


# Organização -------------------------------------------------------------

cases <- read_excel("GLP/glp0605.xlsx", sheet = "Cases")
drugs <- read_excel("GLP/glp0605.xlsx", sheet = "Drugs")
reactions <-  read_excel("GLP/glp0605.xlsx", sheet = "Reactions")
link <-  read_excel("GLP/glp0605.xlsx", sheet = "Drug - reaction link")

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
              "Dulaglutide",
             "Lixisenatide")
            ) |> 
  filter(med_dra_preferred_term %in% usos
  )

# reports por medicamento
link |> 
  group_by(who_drug_active_ingredient_variant) |> 
  summarise(n_distinct((umc_report_id)))

#juntar dados demográficos
link2 <- link2 |> 
  left_join(cases, by ="umc_report_id", keep = F) 

library(lubridate)
#padronizar data
link2 <- link2 |> 
  mutate(
    data_pad = ymd(vigi_base_initial_date),
    month_yr = format_ISO8601(data_pad, precision = "ym")
  )

# reports por mes/ano
link2_g <- link2 |> 
  group_by(month_yr) |> 
  summarise(n_distinct((umc_report_id)))

#pares medicamentos de interesse e todos os eventos (denominador)
link3 <- link |>
  filter(who_drug_active_ingredient_variant %in%
           c("Semaglutide",
             "Liraglutide",
             "Tirzepatide",
             "Insulin degludec;Liraglutide",
             "Dulaglutide",
             "Lixisenatide")
  ) 
  

#juntar dados demográficos
link3 <- link3 |> 
  left_join(cases, by ="umc_report_id", keep = F) 



#padronizar data
link3 <- link3 |> 
  mutate(
    data_pad = ymd(vigi_base_initial_date),
    month_yr = format_ISO8601(data_pad, precision = "ym")
  )

# reports por mes/ano
teste2 <- link3 |> 
  group_by(month_yr) |> 
  summarise(n_distinct((umc_report_id)))

#criar campo data no cases
cases <- cases |> 
  mutate(
    data_pad = ymd(vigi_base_initial_date),
    month_yr = format_ISO8601(data_pad, precision = "ym")
  )

# reports por mes/ano
cases_g <- cases |> 
  group_by(month_yr) |> 
  summarise(n_distinct((umc_report_id)))

cases_g <- cases_g |>
  rename(total = "n_distinct((umc_report_id))") |>
  left_join(link2_g, by ="month_yr", keep = F) |>
  rename(usos = "n_distinct((umc_report_id))") |>  
  replace_na(list(usos = 0)) |> 
  mutate(
    perc = (usos/total)*100
  )

