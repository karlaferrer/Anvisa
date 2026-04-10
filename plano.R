library (readxl)
library (janitor)
library(dplyr)


# Organização -------------------------------------------------------------

cases <- read_excel("GLP/BR_VigiLyze.xlsx", sheet = "Cases")
drugs <- read_excel("GLP/BR_VigiLyze.xlsx", sheet = "Drugs")
reactions <-  read_excel("GLP/BR_VigiLyze.xlsx", sheet = "Reactions")

#Clean column names
cases <- clean_names(cases)
drugs <- clean_names(drugs)
reactions <- clean_names(reactions)

#selecionar casos de interesse
drugs2 <- drugs |>
  filter(who_drug_active_ingredient_variant %in%
           c("Semaglutide",
             "Dulaglutide",
             "Liraglutide",
             "Lixisenatide",
             "Tirzepatide",
             "Insulin degludec;Liraglutide"
           ) )|>
  #filter (Role %in% "Suspect") |>
  distinct()

#pares unicos medicamento e ID
#tabela pro join de características
unicos <- drugs2 |> 
  select(umc_report_id, who_drug_active_ingredient_variant) |> 
  distinct()

#casos com mais de um medicamento reportado
teste <- drugs2 |>              
group_by(`UMC report ID`, `WHODrug active ingredient variant`) |>
  count() |> 
  filter (n>1)

teste2 <- drugs2 |> 
  filter(drugs2$`UMC report ID` %in% teste$`UMC report ID`)

unicosm <- unicos |> 
  group_by(`UMC report ID`) |> 
  count() |> 
  filter (n>1)
             
#juntar dados demográficos
unicos <- unicos |> 
  left_join(cases, by ="umc_report_id", keep = F) 

# Tabela 1 ----------------------------------------------------------------

#proporcao de sexo feminino  
round(prop.table(table(unicos$who_drug_active_ingredient_variant_x, unicos$sex),1)*100,1)


#total de NA para idade
sum(!is.na(unicos$height_cm))

# idade, sexo, IMC por medicamento
t1 <- unicos |> 
  group_by(who_drug_active_ingredient_variant_x) |> 
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
t2 <- unicos |> 
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
names(t2)[1] <- "who_drug_active_ingredient_variant_x"

t1 <- rbind(t1,t2)
rm(t2)

# Curva epidemica ---------------------------------------------------------
library (lubridate)

#drugs2$teste <- as.Date(drugs2$start_date, format = c("%Y-%m-%d"))

library(dplyr)
library(stringr)
library(lubridate)

#padronizar campos de dados para pegar o ano pelo menos
drugs2 <- drugs2 |> 
  mutate(
    data_padronizada = case_when(
      str_detect(start_date, "^\\d{4}$") ~ paste0(start_date, "-01-01"),
      str_detect(start_date, "^\\d{4}-\\d{2}$") ~ paste0(start_date, "-01"),
      TRUE ~ start_date
    ),
    data_final = ymd(data_padronizada),
    ano = year(data_final)
  )

