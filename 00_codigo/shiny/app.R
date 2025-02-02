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

# =============================== Bases de dados ===============================

# load("../../99_dados/acidentes_US.RData")
load("../../99_dados/acidentes_arredondado.RData")
load("../../99_dados/acidentes_US_date.RData")
estados <- st_read("../../99_dados/Estados_US/tl_2024_us_state.shp")

# ============================== Tratando ======================================

nomeestados <- as.data.frame(estados) %>% select(STUSPS,NAME)
names(nomeestados) <- c("STUSPS","NOMEESTADO")

acidentes <- left_join(acidentes_arredondado,nomeestados, by = c("State"="STUSPS"))

qtd_total_acidentes <- acidentes_US_date %>% nrow() %>% round(0)
severidade_media <- acidentes_US_date %>% summarize(mean(Severity)) %>% round(2)
duracao_media <- acidentes_US_date %>% summarize(mean(Duration)) %>% round(2)

acidentes <- left_join(acidentes_arredondado,nomeestados, by = c("State"="STUSPS"))
naci <- acidentes %>% group_by(City) %>% 
  summarise(n = n())
naes <- acidentes %>% group_by(NOMEESTADO) %>% 
  summarise(n = n())

# ============================== Shiny =========================================
# ============================ User interface (UI) ================================
ui <- dashboardPage(
  dark = NULL,
  help = NULL,
  fullscreen = TRUE,
  
  # =========================== Abas ===========================================
  title = "Análise de acidentes nos EUA",
  header = dashboardHeader(
    title = dashboardBrand(
      title = "Análise de acidentes",
      image = "http://web.leg.ufpr.br/img/logo-leg-circle.png"
    )
  ),
  
  sidebar = dashboardSidebar(
    sidebarMenu(
      id = "sidebarMenuid",
      menuItem("Início", tabName = "inicio", icon = icon("home")),
      menuItem("Visualização geográfica", tabName = "Mapa", icon = icon("globe")),
      menuItem("Dados Temporais", tabName = "datas", icon = icon("bar-chart")),
      menuItem("Sinalização e infraestrutura", tabName = "Sinalização_e_infraestrutura", icon = icon("chart-line")),
      menuItem("Aba4", tabName = "Condições climáticas", icon = icon("table"))
    )
  ),
  
  body = dashboardBody(
    tabItems(
      tabItem(
        tabName = "inicio",
        jumbotron(
          title = "Análise de acidentes nos EUA",
          status = "info",
          lead = "Visualização e análise de fatores de acidentes nos EUA (2013 - 2023)",
          btnName = "Download",
          href = "https://www.kaggle.com/datasets/sobhanmoosavi/us-accidents/data",
          "Você pode fazer o download da base de dados, acessando o link abaixo:"
        )
      ),
      tabItem(
        tabName = "Mapa",
        fluidRow(
          column(width = 4, infoBox(title = "Média de acidentes por estado",
                                    value = round(mean(naci$n)),
                                    icon = icon("skull"), color = "primary",
                                    width = 12)),
          column(width = 4, infoBox(title = "Média de acidentes por cidade",
                                    value = round(mean(naes$n)),
                                    icon = icon("city"), color = "primary",
                                    width = 12)),
          column(width = 4, infoBox(title = "Total de cidades",
                                    value = nrow(naci), icon = icon("map-marker-alt"), color = "primary",
                                    width = 12))
        ),
        fluidRow(
          column(
            width = 3,
            selectInput(
              inputId = "filtro_estado",
              label = "Selecione o(s) Estado(s):",
              choices = c("Todos", unique(acidentes$NOMEESTADO)),
              selected = "Todos",
              multiple = TRUE
            )
          ),
          column(
            width = 3,
            selectInput(
              inputId = "filtro_severidade",
              label = "Selecione a Severidade:",
              choices = c("Todos", unique(acidentes$Severity)),
              selected = "Todos",
              multiple = TRUE
            )
          ),
          column(
            width = 3,
            selectInput(
              inputId = "filtro_clima",
              label = "Selecione as Condições Climáticas:",
              choices = c("Todos", unique(acidentes$Weather_Condition)),
              selected = "Todos",
              multiple = TRUE
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            box(
              width = NULL,
              title = "Visualização geográfica",
              status = "primary",
              solidHeader = TRUE,
              leafletOutput("plotmapa", height = 500)
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            box(
              width = NULL,
              title = "Tabela de dados",
              status = "primary",
              solidHeader = TRUE,
              dataTableOutput("tabelaleaflet")
            )
          )
        )
      ),
      tabItem(tabName = "datas",
              
              #============================ Cards ========================
              fluidRow(
                column(
                  width = 3,
                  dateRangeInput(
                    inputId = "date_filter",
                    label = "Selecione o Período:",
                    start = min(acidentes_US_date$Date),
                    end = max(acidentes_US_date$Date)
                  )
                ),
                column(
                  width = 3,
                  selectInput(
                    inputId = "filtro_tipo_dia",
                    label = "Selecione o Tipo de Dia da Semana",
                    choices = c("Todos", unique(acidentes_US_date$`Tipo de Dia`)), # Lista de estados
                    selected = "Todos"
                  )
                ),
                column(
                  width = 3,
                  selectInput(
                    inputId = "civil_twilight",
                    label = "Selecione o crepusculo civil:",
                    choices = c("Todos", unique(acidentes_US_date$Civil_Twilight)), # Lista de estados
                    selected = "Todos"
                  )
                ),
                column(
                  width = 3,
                  sliderInput(
                    inputId = "filtro_duracao",
                    label = "Selecione o intervalo de duraçao:",
                    min = min(acidentes_US_date$Duration),
                    max = max(acidentes_US_date$Duration),
                    value = c(20, 80) # Intervalo inicial selecionado
                  )
                )
              ),
              fluidRow(
                column(width = 4, infoBox(title = "Qtd. Total (un.)",
                                          value = qtd_total_acidentes,
                                          icon = icon("home"),
                                          color = "primary",
                                          width = 12)),
                column(width = 4, infoBox(title = "Severidade Média (1-4)",
                                          value = severidade_media,
                                          icon = icon("home"),
                                          color = "primary",
                                          width = 12)),
                column(width = 4, infoBox(title = "Duração Média (min.)",
                                          value = duracao_media,
                                          icon = icon("home"),
                                          color = "primary",
                                          width = 12))
              ),
              # #============================ Coluna 1 ========================
              tabsetPanel(
                tabPanel("Ano",
                         fluidRow(
                           box(
                             width = 12,
                             title = "Gráfico 1",
                             status = "primary",
                             solidHeader = TRUE,
                             plotlyOutput("ano", width = "100%")
                           )
                         )
                ),
                tabPanel("Mês",
                         fluidRow(
                           box(
                             width = 12,
                             title = "Gráfico 1",
                             status = "primary",
                             solidHeader = TRUE,
                             plotlyOutput("mes", width = "100%")
                           )
                         ),
                         fluidRow(
                           box(
                             width = 12,
                             title = "Gráfico 2",
                             status = "primary",
                             solidHeader = TRUE,
                             plotlyOutput("mes2", width = "100%")
                           )
                         )
                ),
                tabPanel("Data",
                         fluidRow(
                           box(
                             width = 12,
                             title = "Gráfico 1",
                             status = "primary",
                             solidHeader = TRUE,
                             plotlyOutput("date", width = "100%")
                           )
                         ),
                         fluidRow(
                           box(
                             width = 12,
                             title = "Gráfico 2",
                             status = "primary",
                             solidHeader = TRUE,
                             plotlyOutput("date2", width = "100%")
                           )
                         )
                ),
                tabPanel("Agenda Diária",
                         fluidRow(
                           box(
                             width = 12,
                             title = "Gráfico 1",
                             status = "primary",
                             solidHeader = TRUE,
                             plotlyOutput("agenda", width = "100%")
                           )
                         ),
                         fluidRow(
                           box(
                             width = 12,
                             title = "Gráfico 2",
                             status = "primary",
                             solidHeader = TRUE,
                             plotlyOutput("agenda2", width = "100%")
                           )
                         )
                ),
                tabPanel("Severidade por Data",
                         fluidRow(
                           box(
                             width = 12,
                             title = "Gráfico 1",
                             status = "primary",
                             solidHeader = TRUE,
                             plotlyOutput("severidade_date", width = "100%")
                           )
                         ),
                         fluidRow(
                           box(
                             width = 12,
                             title = "Gráfico 2",
                             status = "primary",
                             solidHeader = TRUE,
                             plotlyOutput("severidade_date2", width = "100%")
                           )
                         )
                )
              )
      ),
      tabItem(tabName = "Sinalização_e_infraestrutura",
              # fluidRow(
              #   dateRangeInput(
              #     inputId = "filtro_data",
              #     label = "Período:",
              #     choices = c("Todos",unique(acidentes_US$State)),
              #     selected = "Todos",
              #     multiple = TRUE
              #   ),
              #   selectInput(
              #     inputId = "filtro_estado",
              #     label = "Selecione o Estado:",
              #     choices = c("Todos",unique(acidentes_US$State)),
              #     selected = "Todos",
              #     multiple = TRUE
              #   ),
              #   selectInput(
              #     inputId = "filtro_clima",
              #     label = "Clima:",
              #     choices = c("Todos",unique(acidentes_US$weather_condition)),
              #     selected = "Todos",
              #     multiple = TRUE
              #   ),
              #   selectInput(
              #     inputId = "Infraestrutura",
              #     label = "Infraestrutura:",
              #     choices = c("Todos",unique(acidentes_US[ , .(Amenity, Bump, Crossing, Give_Way, Junction, No_Exit,
              #                                                  Railway, Roundabout, Station, Stop, Traffic_Calming,
              #                                                  Traffic_Signal, Turning_Loop)])),
              #     selected = "Todos",
              #     multiple = TRUE
              #   ),
              # ),
              fluidRow(
                column(
                  width = 8,
                  box(
                    width = NULL,
                    title = "Tabela de Infraestrutura",
                    status = "primary",
                    solidHeader = TRUE,
                    plotOutput("plot_poi")
                  )
                )
              ),
              
              fluidRow(
                column(
                  width = 12,
                  box(
                    width = NULL,
                    title = "Tabela de dados",
                    status = "primary",
                    solidHeader = TRUE,
                    tableOutput("PoI_table")
                  )
                )
              )
      )
    )
  )
)

# ============================ Servidor (Server) ================================
server <- function(input, output, session) {
  #### Aba 1
  # Função de filtragem para o mapa e tabela
  # dados_filtrados <- reactive({
  # 
  #   dados <- acidentes_arredondado
  # 
  #   # Filtro de estado
  #   if (!"Todos" %in% input$filtro_estado) {
  #     dados <- dados %>% filter(State %in% input$filtro_estado)
  #   }
  # 
  #   # Filtro de severidade
  #   if (!"Todos" %in% input$filtro_severidade) {
  #     dados <- dados %>% filter(Severity %in% input$filtro_severidade)
  #   }
  # 
  #   # Filtro de condições climáticas
  #   if (!"Todos" %in% input$filtro_clima) {
  #     dados <- dados %>% filter(Weather_Condition %in% input$filtro_clima)
  #   }
  # 
  #   # Filtro de data
  #   dados <- dados %>% filter(Start_Time >= input$filtro_data[1] & Start_Time <= input$filtro_data[2])
  # 
  #   return(dados)
  # })
  
  output$plotmapa <- renderLeaflet({
    
    dados <- acidentes_arredondado
    
    # Inputs relacionados
    # filtro_estado
    # filtro_severidade 
    # filtro_clima
    # filtro_data
    
    if (input$filtro_estado != "Todos") {
      dados <- dados %>% filter(str_detect(NOMEESTADO, input$filtro_estado))
    }
    
    if (input$filtro_severidade != "Todos") {
      dados <- dados %>% filter(str_detect(Severity, input$filtro_estado))
    }
    
    if (input$filtro_clima != "Todos") {
      dados <- dados %>% filter(str_detect(Weather_Condition, input$filtro_clima))
    }
    
    
    # Dados de acidentes por estado
    acidentes_p_estado <- dados %>% count(State)
    acidentes_p_estado <- left_join(acidentes_p_estado, estados, by = c("State" = "STUSPS"))
    acidentes_p_estado <- st_as_sf(acidentes_p_estado)
    
    # Prepara o mapa de calor
    mapa_de_calor <- dados
      mapa_de_calor <- mapa_de_calor %>% 
      group_by(Start_Lat, Start_Lng) %>% 
      summarise(n = n())
    
    # =============================== Paleta de cores ==============================
    cores <- c("white", "red", "darkred")
    palestado <- colorNumeric(palette = cores, domain = c(0, mean(acidentes_p_estado$n), max(acidentes_p_estado$n)))
    palecidade <- colorNumeric(palette = cores, domain = c(0, mean(mapa_de_calor$n), max(mapa_de_calor$n)))
    
    # =============================== Leaflet ======================================
    mapa <- acidentes_p_estado %>% leaflet() %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~palestado(n),
        weight = 2,
        color = "black",
        fillOpacity = 0.4,
        label = ~NAME, # Atualize para usar o nome correto da coluna do estado
        popup = ~glue("<b>Estado: </b> {NAME}<br>
                   <b>Quantidade de acidentes: </b> {n}"),
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        group = "Nível estadual"
      ) %>%
      addLegend(
        "bottomright",
        pal = palestado,
        values = ~n,
        title = "Número de Acidentes",
        opacity = 1,
        group = "Nível estadual"
      ) %>% 
      addHeatmap(data = mapa_de_calor,
                 lng = ~Start_Lng,
                 lat = ~Start_Lat,
                 intensity = ~n,
                 blur = 20,
                 radius = 15,
                 group = "Mapa de calor"
      ) %>% 
      addLayersControl(baseGroups = c("Nível estadual", "Nível municipal", "Mapa de calor"),
                       position = "topright")
    mapa
  })
  
  # Filtra os dados de acidentes para a tabela com base nos estados selecionados
  output$tabelaleaflet <- renderDataTable({
    dados <- acidentes_arredondado 
    
    if (input$filtro_estado != "Todos") {
      dados <- dados %>% filter(str_detect(NOMEESTADO, input$filtro_estado))
    }
    
    if (input$filtro_severidade != "Todos") {
      dados <- dados %>% filter(str_detect(Severity, input$filtro_estado))
    }
    
    if (input$filtro_clima != "Todos") {
      dados <- dados %>% filter(str_detect(Weather_Condition, input$filtro_clima))
    }
    dados <- dados %>% 
      group_by(NOMEESTADO, City) %>%
      summarise(n = n()) %>% 
      data.table()
  })
  
  ################################ Aba 2 ########################################
  
  
  acidentes_US_date_filtered <- reactive({
    acidentes_US_date[Date >= input$date_filter[1] & Date <= input$date_filter[2],
                      .(ID, Severity, Start_Time, Civil_Twilight, Date, Hour, Year, Month, Weekday, `Tipo de Dia`, Time_Range, Duration)]
  })
  
  output$ano <- renderPlotly({
    acidentes_ano_barplot0 <- acidentes_US_date_filtered()[Civil_Twilight == "Day" | Civil_Twilight == "Night",
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
  })
  
  output$mes <- renderPlotly({
    acidentes_mes_barplot0 <- acidentes_US_date_filtered()[Civil_Twilight == "Day" | Civil_Twilight == "Night",
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
  })
  
  output$mes2 <- renderPlotly({
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
  })
  
  output$date <- renderPlotly({
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
  })
  
  output$date2 <- renderPlotly({
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
  })
  
  output$agenda <- renderPlotly({
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
  })
  
  output$agenda2 <- renderPlotly({
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
  })
  
  output$severidade_date <- renderPlotly({
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
  })
  
  output$severidade_date2 <- renderPlotly({
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
  })
  
  output$contingencyTable <- renderDataTable({
    tabela5 <- acidentes_US_date[, .N, by = .(Weekday, Time_Range)] %>% 
      dcast(Time_Range ~ Weekday, value.var = "N", fill = 0)
    tabela5
  })
  
  
  ############################## Aba 4 ########################################
  output$plot_poi <- renderPlot({
    poi <- acidentes_US[State %in% input$filtro_estado &
                          Weather_Condition %in% input$filtro_clima &
                          Amenity %in% input$Infraestrutura &
                          Bump %in% input$Infraestrutura &
                          Crossing %in% input$Infraestrutura &
                          Give_Way %in% input$Infraestrutura &
                          Junction %in% input$Infraestrutura &
                          No_Exit %in% input$Infraestrutura &
                          Railway %in% input$Infraestrutura &
                          Roundabout %in% input$Infraestrutura &
                          Station %in% input$Infraestrutura &
                          Stop %in% input$Infraestrutura &
                          Traffic_Calming %in% input$Infraestrutura &
                          Traffic_Signal %in% input$Infraestrutura &
                          Turning_Loop %in% input$Infraestrutura,] %>% 
      acidentes_US[, .(Severity, Start_Time, City, State, `Temperature(F)`,
                       Amenity, Bump, Crossing, Give_Way, Junction, No_Exit,
                       Railway, Roundabout, Station, Stop, Traffic_Calming,
                       Traffic_Signal, Turning_Loop)]
    
    poi_true <- poi %>% 
      select(Amenity:Turning_Loop) %>% 
      pivot_longer(Amenity:Turning_Loop, names_to = "PoI") %>% 
      filter(value == 1) %>%
      group_by(PoI) %>% 
      summarise(t = n()) %>% 
      arrange(desc(t))
    
    poi_false <- poi %>% 
      select(Amenity:Turning_Loop) %>% 
      pivot_longer(Amenity:Turning_Loop, names_to = "PoI") %>% 
      filter(value == FALSE) %>%
      group_by(PoI) %>% 
      summarise(f = n()) %>% 
      arrange(f)
    
    # grouped bar plot of true and false values
    poi_true %>% 
      left_join(poi_false, by = "PoI") %>% 
      pivot_longer(t:f) %>% 
      ggplot()+
      aes(x = PoI, y = value, fill = name)+
      geom_col(position = "dodge")+
      theme_minimal()
  })
  
  output$PoI_table <- renderTable({
    poi_Severity <- acidentes_US[, .(Severity,Amenity, Bump, Crossing, Give_Way, Junction, No_Exit,
                                     Railway, Roundabout, Station, Stop, Traffic_Calming,
                                     Traffic_Signal, Turning_Loop)]
    
    poi_sum <- poi_Severity[, PoI_Sum := rowSums(.SD), .SDcols = c("Amenity", "Bump", "Crossing", "Give_Way", "Junction", "No_Exit",
                                                                   "Railway", "Roundabout", "Station", "Stop", "Traffic_Calming",
                                                                   "Traffic_Signal", "Turning_Loop")]
    
    poi_sum[, .(Severity, PoI_Sum)] %>% table
  })
}

# ============================ Executar o App ================================
shinyApp(ui, server)
