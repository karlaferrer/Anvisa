library (readxl)
library (janitor)
library(dplyr)


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
link2 |> 
  group_by(who_drug_active_ingredient_variant) |> 
  summarise(n_distinct((umc_report_id)))

