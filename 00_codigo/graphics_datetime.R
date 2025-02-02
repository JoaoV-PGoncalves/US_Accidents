library(data.table)
library(tidyverse)
library(magrittr)
library(here)
library(R.utils)
library(plotly)
library(scales)

# =============================== Loading data =================================
load("C:/Users/joaov/Downloads/1CE302-Data-Analysis-And-Communication/01_data/012_clean/acidentes_US_date.RData")
# =============================== Building plots ===============================
# 
## =============================== Accidents per Year ==========================
acidentes_ano_barplot0 <- acidentes_US_date[Civil_Twilight == "Day" | Civil_Twilight == "Night",
                                            .(.N,
                                              Distinct_date = uniqueN(Date),
                                              Media_diaria = round(.N/uniqueN(Date),0)),
                                            by = Year][
                                              order(Year)][
                                                , Percent_variation := round((Media_diaria - shift(Media_diaria))/shift(Media_diaria), 2)] %>% 
  setorder(Year) %>% # order data set increasingly by year
  as.data.frame() %>% # Transforming data.table into data.frame
  ggplot() + 
  aes(x = Year, y = N,
      text = paste("Ano:", Year,
                   "<br>Qtd. Acidentes:", round(N/1000, 1), "k",
                   "<br>Disa Obs. no Ano:", Distinct_date,
                   "<br>Acidentes por Dia:", round(Media_diaria/1000, 1), "k")) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = percent(Percent_variation)), size = 3) + 
  scale_y_continuous(
    labels = label_number(scale = 1/1000, suffix = "k") # Divide por 1000 e adiciona o sufixo "k"
  ) +
  labs(title = "Qtd. de Acidentes por Ano", x = "Ano", y = "Qtd. Acidentes") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  plotly()
acidentes_ano_barplot0 %>% ggplotly(tooltip = "text")

## =============== Accidents per Year and Civil Twilight ======================
acidentes_ano_barplot1 <- acidentes_US_date[Civil_Twilight == "Day" | Civil_Twilight == "Night", .N, by = .(Year, Civil_Twilight)][
  , N_year := sum(N), by = Year][
    , Percent := round((N/N_year), 2)] %>% 
  setorder(Year, Civil_Twilight) %>% # order data set increasingly by year
  as.data.frame() %>% # Transforming data.table into data.frame
  ggplot() + 
  aes(x = Year, y = N,
      text = paste("Ano:", Year,
                   "<br>Qtd. Acidentes:", round(N/1000, 1), "k",
                   "<br>% Total Acidentes:", round(100*Percent, 0), "%")) + 
  geom_bar(aes(fill = Civil_Twilight), stat = "identity") +
  scale_y_continuous(
    labels = label_number(scale = 1/1000, suffix = "k") # Divide por 1000 e adiciona o sufixo "k"
  ) +
  scale_fill_manual(values = c('Day' = '#D9A404', 'Night' = '#0F3BBF')) +
  labs(title = "Qtd. de Acidentes por Ano", x = "Ano", y = "Qtd. Acidentes") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
acidentes_ano_barplot1 %>% ggplotly(tooltip = "text")

## =============================== Accidents per Year and Month ================
acidentes_ano_mes_barplot0 <- acidentes_US_date[Civil_Twilight == "Day" | Civil_Twilight == "Night",
                                                .(.N,
                                                  Distinct_date = uniqueN(Date),
                                                  Media_diaria = round(.N/uniqueN(Date),0)),
                                                by = Year_Month] %>%
  setorder(Year_Month) %>% # order data set increasingly by year-month
  as.data.frame() %>% # Transforming data.table into data.frame
  ggplot() + 
  geom_bar(stat = "identity", aes(x = Year_Month, y = N,
                                  text = paste("Ano-Mes:", Year_Month,
                                               "<br>Qtd. Acidentes:", round(N/1000, 1), "k",
                                               "<br>Dias no Mes:", Distinct_date,
                                               "<br>Acidentes por Dia:", round(Media_diaria/1000, 1), "k"))) +
  scale_y_continuous(
    labels = label_number(scale = 1/1000, suffix = "k") # Divide por 1000 e adiciona o sufixo "k"
  ) +
  labs(title = "Qtd. de Acidentes por Ano e Mes", x = "Ano-Mes", y = "Qtd. Acidentes") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        plot.title = element_text(hjust = 0.5))
acidentes_ano_mes_barplot0 %>% ggplotly(tooltip = "text")

## ============= Accidents per Year and Month and Civil Twilght ================
acidentes_ano_mes_barplot1 <- acidentes_US_date[Civil_Twilight == "Day" | Civil_Twilight == "Night", .N, by = .(Year_Month, Civil_Twilight)][
  , N_year_month := sum(N), by = Year_Month][
    , Percent := round((N/N_year_month), 2)] %>%
  setorder(Year_Month, Civil_Twilight) %>% # order data set increasingly by year-month
  as.data.frame() %>% # Transforming data.table into data.frame
  ggplot() + 
  geom_bar(stat = "identity", aes(x = Year_Month, y = N, fill = Civil_Twilight,
                                  text = paste("Ano-Mes:", Year_Month,
                                               "<br>Qtd. Acidentes:", round(N/1000, 1), "k",
                                               "<br>% Total Acidentes:", round(100*Percent, 0), "%"))) +
  scale_y_continuous(
    labels = label_number(scale = 1/1000, suffix = "k") # Divide por 1000 e adiciona o sufixo "k"
  ) +
  scale_fill_manual(values = c('Day' = '#D9A404', 'Night' = '#0F3BBF')) + 
  labs(title = "Qtd. de Acidentes por Ano e Mes", x = "Ano-Mes", y = "Qtd. Acidentes") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        plot.title = element_text(hjust = 0.5))
acidentes_ano_mes_barplot1 %>% ggplotly(tooltip = "text")

## =============================== Accidents per Month =========================
acidentes_mes_barplot0 <- acidentes_US_date[Civil_Twilight == "Day" | Civil_Twilight == "Night",
                                            .(.N,
                                              Distinct_date = uniqueN(Date),
                                              Media_diaria = round(.N/uniqueN(Date),0)),
                                            by = Month][
                                              order(Month)][
                                                , Percent_variation := round((Media_diaria - shift(Media_diaria))/shift(Media_diaria), 2)] %>%
  setorder(Month) %>% # order data set increasingly by month
  as.data.frame() %>% # Transforming data.table into data.frame
  ggplot() + 
  aes(x = Month, y = N,
      text = paste("Mes:", Month,
                   "<br>Qtd. Acidentes:", round(N/1000, 1), "k",
                   "<br>Dias Obs. no Mes:", Distinct_date,
                   "<br>Acidentes por Dia:", round(Media_diaria/1000, 1), "k")) + 
  geom_text(aes(label = percent(Percent_variation)), size = 3) +
  geom_bar(stat = "identity") +
  scale_y_continuous(
    labels = label_number(scale = 1/1000, suffix = "k") # Divide por 1000 e adiciona o sufixo "k"
  ) +
  labs(title = "Qtd. de Acidentes por Mes", x = "Mes", y = "Qtd. Acidentes") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))
acidentes_mes_barplot0 %>% ggplotly(tooltip = "text")

## ====================== Accidents per Month and Civil Twilght ================
acidentes_mes_barplot1 <- acidentes_US_date[Civil_Twilight == "Day" | Civil_Twilight == "Night", .N, by = .(Month, Civil_Twilight)][
  , N_month := sum(N), by = Month][
    , Percent := round((N/N_month), 2)] %>% 
  setorder(Month, Civil_Twilight) %>% # order data set increasingly by month
  as.data.frame() %>% # Transforming data.table into data.frame
  ggplot() + 
  aes(x = Month, y = N,
      text = paste("Mes:", Month,
                   "<br>Qtd. Acidentes:", round(N/1000, 1), "k",
                   "<br>% Total Acidentes:", round(100*Percent, 0), "%")) + 
  geom_bar(aes(fill = Civil_Twilight), stat = "identity") +
  scale_fill_manual(values = c('Day' = '#D9A404', 'Night' = '#0F3BBF')) +
  scale_y_continuous(
    labels = label_number(scale = 1/1000, suffix = "k") # Divide por 1000 e adiciona o sufixo "k"
  ) +
  labs(title = "Qtd. de Acidentes por Mes", x = "Mes", y = "Qtd. Acidentes") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))
acidentes_mes_barplot1 %>% ggplotly(tooltip = "text")

# 2.4.0.0 Ploting accidents count per date with line graphic
acidentes_data_timeseries0 <- acidentes_US_date[Civil_Twilight == "Day" | Civil_Twilight == "Night", .(.N), by = .(Date)] %>% 
  setorder(Date) %>% # order data set increasingly by date
  as.data.frame() %>%  # Transforming data.table into data.frame
  ggplot() + 
  aes(x = Date, y = N) + 
  geom_bar(stat = "identity", alpha = 0.4) +
  geom_point(size = 0.5, alpha = 0.5,
             aes(text = paste("Data:", Date, "<br>Qtd. Acidentes:", N/1000, "k"))) +
  scale_y_continuous(
    labels = label_number(scale = 1/1000, suffix = "k") # Divide por 1000 e adiciona o sufixo "k"
  ) +
  labs(title = "Qtd. de Acidentes por Data", x = "Data", y = "Qtd. Acidentes") +
  theme_minimal()
acidentes_data_timeseries0 %>% ggplotly(tooltip = "text")

# 2.4.0.1 Ploting accidents count per date with line graphic and smooth adjust
acidentes_data_timeseries0 <- acidentes_US_date[Civil_Twilight == "Day" | Civil_Twilight == "Night", .(.N), by = .(Date)] %>% 
  setorder(Date) %>% # order data set increasingly by date
  as.data.frame() %>%  # Transforming data.table into data.frame
  ggplot() + 
  aes(x = Date, y = N) + 
  geom_bar(stat = "identity", alpha = 0.4) +
  geom_point(size = 0.5, alpha = 0.5,
             aes(text = paste("Data:", Date, "<br>Qtd. Acidentes:", N/1000, "k"))) +
  geom_smooth(method = "gam", color = "black", size = 0.5) +
  scale_y_continuous(
    labels = label_number(scale = 1/1000, suffix = "k") # Divide por 1000 e adiciona o sufixo "k"
  ) +
  labs(title = "Qtd. de Acidentes por Data", x = "Data", y = "Qtd. Acidentes") +
  theme_minimal()
acidentes_data_timeseries0 %>% ggplotly(tooltip = "text")

# 2.4.0.2 Ploting accidents count per date (is weekend  not) with line graphic and smooth adjust
acidentes_data_timeseries0 <- acidentes_US_date[Civil_Twilight == "Day" | Civil_Twilight == "Night", .(.N), by = .(Date, `Tipo de Dia`)] %>% 
  setorder(Date) %>% # order data set increasingly by date
  as.data.frame() %>%  # Transforming data.table into data.frame
  ggplot() + 
  aes(x = Date, y = N) + 
  geom_bar(stat = "identity", alpha = 0.4) +
  geom_point(size = 0.5, alpha = 0.5,
             aes(color = `Tipo de Dia`,
                 text = paste("Data:", Date,
                              "<br>Qtd. Acidentes:", N/1000, "k",
                              "<br>Tipo de dia:", `Tipo de Dia`))) +
  geom_smooth(method = "gam", size = 0.5, se = FALSE, aes(color = `Tipo de Dia`, group = `Tipo de Dia`)) +
  scale_y_continuous(
    labels = label_number(scale = 1/1000, suffix = "k") # Divide por 1000 e adiciona o sufixo "k"
  ) +
  scale_color_manual(values = c("Final de semana" = "#E00704", "Dia da semana" = "#04E048")) +
  labs(title = "Qtd. de Acidentes por Data", x = "Data", y = "Qtd. Acidentes") +
  theme_minimal()
acidentes_data_timeseries0 %>% ggplotly(tooltip = "text")

# 2.4.0.1 Ploting accidents count per date with line graphic and smooth adjust
acidentes_data_timeseries0 <- acidentes_US_date[Civil_Twilight == "Day" | Civil_Twilight == "Night",
                                                .(.N),
                                                by = .(Date)] %>% 
  setorder(Date) %>% # order data set increasingly by date
  as.data.frame() %>%  # Transforming data.table into data.frame
  ggplot() + 
  aes(x = Date, y = N) + 
  geom_bar(stat = "identity", alpha = 0.4) +
  geom_point(size = 0.5, alpha = 0.5,
             aes(text = paste("Data:", Date, "<br>Qtd. Acidentes:", N/1000, "k"))) +
  geom_smooth(method = "gam", color = "black", size = 0.5) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +  
  scale_y_continuous(
    labels = label_number(scale = 1/1000, suffix = "k") # Divide por 1000 e adiciona o sufixo "k"
  ) +
  labs(title = "Qtd. de Acidentes por Data", x = "Data", y = "Qtd. Acidentes") +
  theme_minimal()
acidentes_data_timeseries0 %>% ggplotly(tooltip = "text")

# 2.4.1 Ploting accidents count per date and Civil Twilight with line graphic
acidentes_data_timeseries1 <- acidentes_US_date[Civil_Twilight == "Day" | Civil_Twilight == "Night", .(.N), by = .(Date, Civil_Twilight)] %>% 
  setorder(Date, Civil_Twilight) %>% # order data set increasingly by date
  as.data.frame() %>%  # Transforming data.table into data.frame
  ggplot() + 
  aes(x = Date, y = N,
      text = paste("Data:", Date, "<br>Qtd. Acidentes:", N/1000, "k")) + 
  geom_bar(stat = "identity", alpha = 0.4) +
  geom_point(aes(color = Civil_Twilight), alpha = 0.4, size = 0.4) + 
  scale_y_continuous(
    labels = label_number(scale = 1/1000, suffix = "k") # Divide por 1000 e adiciona o sufixo "k"
  ) +
  scale_color_manual(values = c('Day' = '#D9A404', 'Night' = '#0F3BBF')) +
  labs(title = "Qtd. de Acidentes por Data", x = "Data", y = "Qtd. Acidentes") +
  theme_minimal()
acidentes_data_timeseries1 %>% ggplotly(tooltip = "text")

# 2.5.0 Ploting accidents count per weekday and time range
acidentes_data_2table0 <- acidentes_US_date[Civil_Twilight == "Day" | Civil_Twilight == "Night", .N, by = .(Weekday, Time_Range)] %>%
  setorder(Weekday, -Time_Range) %>% # order data set increasingly by weekday and time range
  as.data.frame() %>% # Transforming data.table into data.frame
  ggplot() + 
  aes(x = Weekday, y = Time_Range, fill = N) +
  geom_tile() + 
  scale_fill_gradient(low = "white", high = "black") +
  labs(title = "Agenda Diaria de Acidentes", x = "Dia da Semana", y = "Horario") + 
  theme_minimal() + 
  scale_x_discrete(position = "top") +
  theme(
    axis.title.x.top = element_text(),
    axis.text.x.top = element_text(),
    axis.line.x.top = element_line(),
    axis.ticks.x.top = element_line()
    
  )
acidentes_data_2table0

# 2.5.1 Ploting accident severity mean per weekday and time range
acidentes_data_2table1 <- acidentes_US_date[Civil_Twilight == "Day" | Civil_Twilight == "Night", .(.N, Severity = mean(Severity)), by = .(Weekday, Time_Range)] %>%
  setorder(Weekday, -Time_Range) %>% # order data set increasingly by weekday and time range
  as.data.frame() %>% # Transforming data.table into data.frame
  ggplot() + 
  aes(x = Weekday, y = Time_Range, fill = Severity) +
  geom_tile() + 
  scale_fill_gradient(low = "white", high = "black") +
  labs(title = "Agenda Diaria da Severidade Média de Acidentes", x = "Dia da Semana", y = "Horario") + 
  theme_minimal() + 
  scale_x_discrete(position = "top") +
  theme(
    axis.title.x.top = element_text(),
    axis.text.x.top = element_text(),
    axis.line.x.top = element_line(),
    axis.ticks.x.top = element_line()
    
  )
acidentes_data_2table1

# 2.6.0 Ploting severity mean avearge per date with points
severity_data_points0 <- acidentes_US_date[(Civil_Twilight == "Day" | Civil_Twilight == "Night") & Year > 2016,
                                           .(Severidade_Media = mean(Severity)),
                                           by = .(Date)][order(Date)] %>%
  ggplot() + 
  aes(x = Date, y = Severidade_Media) + 
  geom_point(size = 0.5, alpha = 0.1,
             aes(text = paste("Data:", Date,
                              "<br>Severidade Média:", round(Severidade_Media, 1)))) +
  geom_smooth(method = "lm", size = 0.5, se = FALSE, color = "black") +
  ylim(0, 4) +
  labs(title = "Severidade Média por Data", x = "Data", y = "Severidade") +
  theme_minimal()
severity_data_points0 %>% ggplotly(tooltip = "text")

# 2.6.1 Ploting severity mean average per date and day type with points
severity_data_points1 <- acidentes_US_date[(Civil_Twilight == "Day" | Civil_Twilight == "Night") & Year > 2016,
                                           .(Severidade_Media = mean(Severity)),
                                           by = .(Date, `Tipo de Dia`)][order(Date)] %>%
  ggplot() + 
  aes(x = Date, y = Severidade_Media) + 
  geom_point(size = 0.5, alpha = 0.1,
             aes(color = `Tipo de Dia`,
                 text = paste("Data:", Date,
                              "<br>Severidade Média:", round(Severidade_Media, 1),
                              "<br>Tipo do Dia:", `Tipo de Dia`))) +
  geom_smooth(method = "lm", size = 0.5, se = FALSE, aes(color = `Tipo de Dia`, group = `Tipo de Dia`)) +
  scale_color_manual(values = c("Final de semana" = "#E00704", "Dia da semana" = "#04E048")) +
  ylim(0, 4) +
  labs(title = "Severidade Média dos Acidentes por Data", x = "Data", y = "Severidade") +
  theme_minimal()
severity_data_points1 %>% ggplotly(tooltip = "text")