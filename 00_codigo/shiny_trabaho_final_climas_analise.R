require(tidyverse)
library(ggplot2)
require(data.table)
library(reshape2)
library(corrplot)

dados <-  read.csv("UFPR/2° SEMESTRE/FUNDAMENTOS DE PROGRAMAÇÃO PARA ESTATÍSTICA/data/US accidents/US_Accidents_March23.csv")

dados <- dados %>%
  mutate(Weather_Cluster = case_when(
    grepl("Rain|Drizzle|Shower|Showers|Light Rain|Heavy Rain|Rain Showers|Light Rain Showers|Rain and Sleet|Rain Shower|Rain / Windy|Rain Showers", Weather_Condition, ignore.case = TRUE) ~ "Rain",
    grepl("Snow|Blowing Snow|Light Snow|Heavy Snow|Snow Grains|Snow Showers|Snow and Thunder|Snow / Windy|Blowing Snow / Windy|Light Snow Shower|Heavy Snow / Windy|Heavy Snow with Thunder|Snow and Sleet", Weather_Condition, ignore.case = TRUE) ~ "Snow",
    grepl("Thunder|Tornado|Thunderstorm|Light Thunderstorm|Heavy Thunderstorms and Rain|Thunder / Windy|Thunder / Wintry Mix|Thunderstorms and Snow|Light Thunderstorms and Snow|Heavy Thunderstorms with Small Hail", Weather_Condition, ignore.case = TRUE) ~ "Thunderstorm",
    grepl("Wind|Windy|Blowing Dust|Blowing Snow|Duststorm|Dust Whirls|Blowing Dust / Windy|Widespread Dust|Sand / Windy|Windy", Weather_Condition, ignore.case = TRUE) ~ "Wind",
    grepl("Fog|Haze|Shallow Fog|Patches of Fog|Mist|Light Fog|Fog / Windy|Partial Fog", Weather_Condition, ignore.case = TRUE) ~ "Fog",
    grepl("Clear|Fair|Mostly Cloudy|Partly Cloudy|Overcast|Cloudy", Weather_Condition, ignore.case = TRUE) ~ "Clear",
    grepl("Smoke|Volcanic Ash|Haze", Weather_Condition, ignore.case = TRUE) ~ "Hazardous", 
    grepl("Sleet|Ice Pellets|Freezing Rain|Light Freezing Rain|Freezing Drizzle|Light Freezing Drizzle|Heavy Freezing Rain", Weather_Condition, ignore.case = TRUE) ~ "Ice",
    grepl("Squalls|Hail|Small Hail|Thunder / Wintry Mix|Hail", Weather_Condition, ignore.case = TRUE) ~ "Hail",
    grepl("Mist|Blowing Snow Nearby|Sand|Blowing Snow", Weather_Condition, ignore.case = TRUE) ~ "Dust & Mist",
    grepl("Volcanic Ash|Blowing Sand|Sand", Weather_Condition, ignore.case = TRUE) ~ "Dust Storm",
    grepl("Fair / Windy|Fair", Weather_Condition, ignore.case = TRUE) ~ "Fair / Windy",
    TRUE ~ "Other" 
  ))

resumo <- dados %>%
  group_by(State, Weather_Cluster) %>%
  summarise(Frequencia = n(), .groups = "drop")

ggplot(resumo, aes(x = Weather_Cluster, y = State, fill = Frequencia)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Mapa de Calor: Severidade x Condições Climáticas",
       x = "Condições Climáticas",
       y = "Severidade",
       fill = "Frequência") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  


ggplot(dados, aes(x =Weather_Condition, y = Severity, fill = frequencia))+
  geom_tile(color = "white") 
  scale_fill_gradient(low = "lightblue", high = "darkblue") + 
  labs(
    title = "Heatmap de Frequências",
    x = "Variável Nominal",
    y = "Variável Ordinal",
    fill = "Frequência"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), 
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
  )



condicoes_count <- dados %>%
  group_by(Weather_Cluster) %>%
  tally(name = "Count")
ggplot(condicoes_count, aes(x = reorder(Weather_Cluster, -Count), y = Count, fill = Weather_Cluster)) +
  geom_bar(stat = "identity") +
  labs(title = "Número de Acidentes por Condição Climática", x = "Condição Climática", y = "Número de Acidentes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(dados, aes(x = Precipitation.in.)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black") +
  labs(title = "Distribuição de Precipitação", x = "Precipitação (in.)", y = "Contagem")


ggplot(dados, aes(x = Visibility.mi.)) +
  geom_histogram(binwidth = 1, fill = "green", color = "black") +
  labs(title = "Distribuição de Visibilidade", x = "Visibilidade (milhas)", y = "Contagem")


climatic_data <- dados %>%
  select(Temperature.F., Humidity..., Pressure.in., Wind_Speed.mph., Visibility.mi.) %>%
  na.omit()

cor_matrix <- cor(climatic_data)
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black")

#-----------------------------------------------------------------------------
city_precipitation <- dados %>%
  group_by(City) %>%
  summarise(
    Total_Precipitation = sum(Precipitation.in., na.rm = TRUE),
    Accident_Count = n()
  ) %>%
  arrange(desc(Total_Precipitation))

top_cities <- city_precipitation %>% top_n(10, Total_Precipitation)

ggplot(top_cities, aes(x = reorder(City, -Total_Precipitation), y = Total_Precipitation)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(
    title = "Top 10 Cidades com Maior Precipitação",
    x = "Cidade",
    y = "Total de Precipitação (in.)"
  )
dados <- dados %>%
  mutate(Choveu = ifelse(Precipitation.in. > 0, "Sim", "Não"))


accident_chuva <- dados %>%
  group_by(Choveu) %>%
  summarise(Accident_Count = n())

ggplot(accident_chuva, aes(x = Choveu, y = Accident_Count, fill = Choveu)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Acidentes em Cidades com Chuva vs. Sem Chuva",
    x = "Choveu?",
    y = "Número de Acidentes"
  ) +
  theme_minimal()

top_cities_dados <- dados %>% filter(City %in% top_cities$City)

temporal_precipitation <- top_cities_dados %>%
  group_by(City, Month = floor_date(Start_Time, "month")) %>%
  summarise(Total_Precipitation = sum(Precipitation.in., na.rm = TRUE))

ggplot(temporal_precipitation, aes(x = Month, y = Total_Precipitation, color = City)) +
  geom_line() +
  labs(
    title = "Tendência de Precipitação ao Longo do Tempo",
    x = "Mês",
    y = "Total de Precipitação (in.)"
  ) +
  theme_minimal()


t_test_result <- t.test(
  dados$Severity[dados$Choveu == "Sim"],
  dados$Severity[dados$Choveu == "Não"]
)

t_test_result

#----------------------------------------------------------------------------

dados <- dados %>%
  mutate(Temp_Range = cut(
    Temperature.F.,
    breaks = seq(floor(min(Temperature.F.)), ceiling(max(Temperature.F.)), by = 5),
    include.lowest = TRUE
  ))


temp_accidents <- data %>%
  group_by(Temp_Range) %>%
  summarise(Accident_Count = n()) %>%
  arrange(Temp_Range)

ggplot(temp_accidents, aes(x = Temp_Range, y = Accident_Count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(
    title = "Quantidade de Acidentes por Faixa de Temperatura",
    x = "Faixa de Temperatura (°F)",
    y = "Quantidade de Acidentes"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(temp_accidents, aes(x = Temp_Range, y = Accident_Count, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(
    title = "Tendência de Acidentes por Faixa de Temperatura",
    x = "Faixa de Temperatura (°F)",
    y = "Quantidade de Acidentes"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(dados, aes(x = Temperature.F., y = Severity)) +
  geom_jitter(alpha = 0.3, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Relação entre Temperatura e Gravidade dos Acidentes",
    x = "Temperatura (°F)",
    y = "Gravidade do Acidente"
  ) +
  theme_minimal()
correlation <- cor(dados$Temperature.F., dados$Severity, use = "complete.obs")

cat("Correlação entre Temperatura e Gravidade dos Acidentes: ", correlation)

#-----------------------------------------------------------------------------
ggplot(dados, aes(x = Temperature.F., y = Distance.mi., color = as.factor(Severity))) +
  geom_point(alpha = 0.6, size = 2) +
  scale_color_brewer(palette = "Set3") + 
  labs(
    title = "Relação entre Temperatura e Distância de Acidentes por Severidade",
    x = "Temperatura (°F)",
    y = "Distância (milhas)",
    color = "Severidade"
  ) +
  theme_minimal()
