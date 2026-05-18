library (readxl)
library (janitor)
library(dplyr)
library(tidyr)


# Organização -------------------------------------------------------------

cases <- read_excel("GLP/glp1805.xlsx", sheet = "Cases")
drugs <- read_excel("GLP/glp1805.xlsx", sheet = "Drugs")
reactions <-  read_excel("GLP/glp1805.xlsx", sheet = "Reactions")
link <-  read_excel("GLP/glp1805.xlsx", sheet = "Drug - reaction link")

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
            .by = c(who_drug_active_ingredient_variant.x, month_yr))


#criar campo data no cases
cases <- cases |> 
  mutate(
    data_pad = ymd(national_pv_centre_initial_receive_date),
    month_yr = format_ISO8601(data_pad, precision = "ym")
  )

cases_g <- cases |>
  filter(month_yr > "2021-03") |> 
  left_join(link2, by = "umc_report_id", keep = F ) 

cases_g <- cases_g |> 
  group_by(who_drug_active_ingredient_variant.y, month_yr.y) |>
  summarise(n_distinct(umc_report_id)) |> 
  rename( total = "n_distinct(umc_report_id)")
  
#juntar para formar o percentual
link2_g <- link2_g |> 
  left_join(cases_g, by =c("who_drug_active_ingredient_variant.x" = "who_drug_active_ingredient_variant.y",
                           "month_yr" = "month_yr.y"), keep = F) |>
  rename(usos = "n_distinct((umc_report_id))") |>  
  replace_na(list(usos = 0)) |>
  replace_na(list(total = 0)) |>
  mutate(
    perc = (usos / total) * 100
  )

#Serie historica do perc de uso indevido com mediana
library (ggplot2)
library(RColorBrewer)
plot_serie <- link2_g |>
  #filter( month_yr > "2018-01") |>
  ggplot() +
  #geom_line(size = 0.3)+
  geom_vline(xintercept = as.Date("2025-07-01"),
             linetype="dotted", color = "black", alpha = 0.5)+
  geom_hline(aes(yintercept = median(perc)), color = "darkgreen", alpha = 0.5, linetype="dotted")+
  geom_line(aes(x= ym(month_yr),y=perc,group = who_drug_active_ingredient_variant.x,
                color = who_drug_active_ingredient_variant.x),linewidth = 0.4)+
  geom_point(aes(x= ym(month_yr),y=perc, group = who_drug_active_ingredient_variant.x,
                 color = who_drug_active_ingredient_variant.x), size = 0.8)+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  xlab("ano")+
  ylim(c(0,40))+
  ylab("Usos fora da indicação (%)")+
  xlab(" ") + 
  labs(title = "Percentual de notificações com usos fora da indicação")+
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

ggsave("GLP/plot_serie.png", 
       plot_serie,
       width = 15,
       height = 10,
       unit = "cm",
       dpi = 300)

#tabela com IMC
#registros unicos
library(dplyr)

library(dplyr)

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

# Pancreatite -------------------------------------------------------------

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

#registros unicos
panc_un <- link_panc |>
  distinct(umc_report_id,
           who_drug_active_ingredient_variant.y,
           med_dra_preferred_term.y,
           .keep_all = TRUE)


# reports por mes/ano
panc_g <- panc_un |> 
  group_by(month_yr, who_drug_active_ingredient_variant.x) |> 
  summarise(n_distinct(umc_report_id))

colnames(panc_g)[3] <-"casos" 

#Serie historica dos casos de pancreatite com mediana
library (ggplot2)
library(RColorBrewer)
plot_panc <- panc_g |>
  filter( month_yr > "2019-12") |>  
  ggplot() +
  #geom_line(size = 0.3)+
  geom_vline(xintercept = as.Date("2025-07-01"),
             linetype="dashed", color = "black", alpha = 0.5)+
  geom_line(aes(x= ym(month_yr),y=casos,group = who_drug_active_ingredient_variant.x,
                color = who_drug_active_ingredient_variant.x),linewidth = 0.4)+
  geom_point(aes(x= ym(month_yr),y=casos,group = who_drug_active_ingredient_variant.x,
                 color = who_drug_active_ingredient_variant.x), size = 0.5)+
  geom_hline(aes(yintercept = median(casos)), color = "darkgreen", alpha = 0.5,linetype="dashed")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  xlab("ano")+
  ylim(c(0,8))+
  ylab("n. casos")+
  xlab(" ") + 
  #labs(colour = NULL) +
  #scale_color_manual(values=c("black", "darkred")) +
  theme_bw()+
  theme(legend.position = "bottom")+
  theme(text = element_text(size = 8)) +
  theme(legend.title = element_blank())+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
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


ggsave("GLP/plot_panc.png", 
       plot_panc,
       width = 15,
       height = 10,
       unit = "cm",
       dpi = 300)

#tabela descritiva

          

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
