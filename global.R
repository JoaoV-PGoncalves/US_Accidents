# ================================ Pacotes requeridos ==========================

library(shiny)
require(data.table)
require(tidyverse)
require(leaflet)
require(leaflet.extras)
require(leaflet.extras2)
require(sf)
require(glue)
require(htmltools)
library(dplyr)
library(bs4Dash)
library(readxl)
library(plotly)
library(DT)
require(scales)
require(tidyr)
library(hrbrthemes)
library(bslib)

# =============================== Bases de dados ===============================

# Base de amostra
load("99_dados//acidentes_US_sample.RData")

# Alterando o nome, para não ter que alterar todas as variáveis após
acidentes_arredondado <- acidentes_US_sample
acidentes_US_date_sample <- acidentes_US_sample

# Shapefiles 
estados <- st_read("99_dados//Estados//tl_2024_us_state.shp")

# ========================== Tratando para Dados Geograficos ===================

# Arredondando latitudes e longitudes
acidentes_arredondado$`Start Lat` <- round(acidentes_arredondado$`Start Lat`,2)
acidentes_arredondado$`Start Lng` <- round(acidentes_arredondado$`Start Lng`,2)

# Atribuindo uma nova coluna para duração em minutos
acidentes_arredondado <- acidentes_arredondado %>%
  mutate(Duration_minutes = as.numeric(difftime(`End Time`, `Start Time`, units = "mins")))

# Alterando a base shp para facilitar o entendimento
nomeestados <- as.data.frame(estados) %>% select(STUSPS,NAME)
names(nomeestados) <- c("STUSPS","NOMEESTADO")

# Juntando as informações já tratadas
acidentes <- left_join(acidentes_arredondado,nomeestados, by = c("State"="STUSPS"))

# ======================= Tratando para Dados Temporais =======================

# Seleciona as variaveis de 'Tempo'
acidentes_US_date_sample <- acidentes_US_date_sample[, `:=`(Start_Time = `Start Time`,
                                                            End_Time = `End Time`,
                                                            Civil_Twilight = `Civil Twilight`)]

acidentes_US_date_sample <- acidentes_US_date_sample[, .(ID, Severity, Start_Time, End_Time, Civil_Twilight)]



# Extrai separadamente informaçoes de data, hora, ano, mes, dia e dia da semana a partir do Start_Time
acidentes_US_date_sample[, `:=`(
  Date = as.Date(Start_Time),
  Hour = hour(Start_Time),
  Year = year(Start_Time),
  Month = sprintf("%02d", month(Start_Time)),
  Day = day(Start_Time),
  Weekday = factor(format(Start_Time, "%A"), 
                   levels = c("segunda-feira", "terça-feira", "quarta-feira", 
                              "quinta-feira", "sexta-feira", "sábado", "domingo"),
                   labels = c("seg", "ter", "qua", "qui", "sex", "sáb", "dom"))
)]

# Cria uma nova variavel categorica Ano-Mes e Tipo de Dia (final de semana ou nao)
acidentes_US_date_sample[, `:=`(Year_Month = paste0(Year, "-", Month),
                                `Tipo de Dia` = ifelse(Weekday == "dom" | Weekday == "sáb", "Final de semana", "Dia da semana")),]

# Ordena os dias da semana (já categorizados automaticamente pelo wday) e transforma em fator
weekday_order <- c("seg", "ter", "qua", "qui", "sex", "sáb", "dom")
acidentes_US_date_sample[, Weekday := factor(Weekday, levels = weekday_order)]

# 1.3.3 Cria a variavel categorica Time_Range de forma programática
acidentes_US_date_sample[, Time_Range := sprintf("%02d:00:00", Hour)]

# Ordenar Time_Range como fator
time_range_order <- sprintf("%02d:00:00", 23:0)
acidentes_US_date_sample[, Time_Range := factor(Time_Range, levels = time_range_order)]

# Cria a variável Day_Period
acidentes_US_date_sample[, Day_Period := ifelse(Hour >= 6 & Hour < 18, "06:00:00 -> 18:00:00", "18:00:00 -> 06:00:00")]

# Transforma a variável Day_Period em fator
acidentes_US_date_sample[, Day_Period := factor(Day_Period, levels = c("06:00:00 -> 18:00:00", "18:00:00 -> 06:00:00"))]

# Transforma a variavel 'Duration' em minutos
acidentes_US_date_sample[, Duration := round(as.numeric(difftime(End_Time, Start_Time, units = "mins")))]

# Remover coluna End_Time
acidentes_US_date_sample[, End_Time := NULL] # Remove a coluna 'End_Time' (nao sera utilizada)


# ======================= Tratando para Dados Climaticos =======================

# Definindo uma varíavel sem NA's para as variáveis climáticas
condicoes_climaticas <- acidentes %>%
  select(`Precipitation(in)`, `Wind Speed(mph)`, `Humidity(%)`, `Visibility(mi)`,
         `Pressure(in)`, `Wind Chill(F)`, `Temperature(F)`) %>%
  na.omit()
