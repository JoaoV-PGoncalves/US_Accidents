library(data.table)
library(tidyverse)
library(magrittr)
library(here)
library(R.utils)
library(plotly)
library(scales)

# 1 Extracting, Trasnforming and Saving Data

## 1.1 Load data
# acidentes_US <- fread("C:/Users/joaov/Downloads/CE302-Data-Analysis-And-Communication/01_data/011_raw/US_accidents.zip")
# save(acidentes_US, file = "acidentes_US.RData")
# load("C:/Users/joaov/Downloads/1CE302-Data-Analysis-And-Communication/02_scripts/acidentes_US.RData")


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


# 1.4 Saving data
save(acidentes_US_date, file = "acidentes_US_date.RData")