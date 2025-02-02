
# ============================== Requerindo pacotes ============================

library(shiny)
library(data.table)
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(leaflet.extras2)
library(sf)
library(glue)
library(htmltools)
library(bs4Dash)
library(readxl)
library(plotly)
library(DT)
library(scales)
library(tidyr)
library(hrbrthemes)
library(bslib)

# =================================== Shiny ====================================
# ============================ UI - User Interface =============================
ui <- dashboardPage(
  dark = 0,
  help = NULL,
  fullscreen = TRUE,
  
  # ================================ UI - Abas =================================
  title = "Análise de acidentes",
  header = dashboardHeader(
    title = dashboardBrand(
      title = "Análise de acidentes",
      image = "http://web.leg.ufpr.br/img/logo-leg-circle.png"
    )
  ),
  
  # ========================== UI - Barra lateral ==============================
  sidebar = dashboardSidebar(
    sidebarMenu(
      id = "sidebarMenuid",
      menuItem("Início", tabName = "inicio", icon = icon("home")),
      menuItem("Visualização geográfica", tabName = "Mapa", icon = icon("globe")),
      menuItem("Dados Temporais", tabName = "datas", icon = icon("bar-chart")),
      menuItem("Condições Climáticas", tabName = 'clima', icon = icon("cloud")),
      menuItem("Sinalização e infraestrutura", tabName = "Sinalização_e_infraestrutura", icon = icon("chart-line")),
      menuItem("Duração de acidentes", tabName = "duracao", icon = icon("clock")),
      menuItem("Relatorio", tabName = "relatorio")
    )
  ),
  
  # ======================== UI - Definindo conteúdo das abas ==================
  body = dashboardBody(
    tabItems(
      # ============================ UI - Início =============================
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
      
      # ========================== UI - Localização ===========================
      tabItem(
        tabName = "Mapa",
        fluidRow(
          column(width = 4, uiOutput(outputId = "media_estado_box")),
          column(width = 4, uiOutput(outputId = "media_cidade_box")),
          column(width = 4, uiOutput(outputId = "total_cidades_box"))
        ),
        fluidRow(
          column(
            width = 3,
            selectInput("filtro_estado", "Selecione o(s) Estado(s):",
                        choices = c("Todos", unique(acidentes$NOMEESTADO)),
                        selected = "Todos", multiple = TRUE)
          ),
          column(
            width = 3,
            selectInput("filtro_severidade", "Selecione a Severidade:",
                        choices = c("Todos", unique(acidentes$Severity)),
                        selected = "Todos", multiple = TRUE)
          ),
          column(
            width = 3,
            selectInput("filtro_clima", "Selecione as Condições Climáticas:",
                        choices = c("Todos", unique(acidentes$Weather_Condition)),
                        selected = "Todos", multiple = TRUE)
          ),
          column(width = 3, actionButton("aplicar", "Aplicar Filtro"))
        ),
        fluidRow(
          column(
            width = 12,
            box(
              width = NULL, title = "Visualização geográfica", status = "primary",
              solidHeader = TRUE, leafletOutput("plotmapa", height = 500),
              maximizable = TRUE
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            box(
              width = NULL, title = "Tabela de dados", status = "primary",
              solidHeader = TRUE, maximizable = TRUE, dataTableOutput("tabelaleaflet")
            )
          )
        )
      ),
      
      # ============================= UI - Datas ===============================
      tabItem(
        tabName = "datas",
        fluidRow(
          column(
            width = 3,
            dateRangeInput("date_filter", "Selecione o Período:",
                           start = min(acidentes_US_date_sample$Date), end = max(acidentes_US_date_sample$Date))
          ),
          column(
            width = 3,
            selectInput("filtro_time_range", "Faixa Horária:",
                        choices = c("Todos", levels(acidentes_US_date_sample$Time_Range)),
                        selected = "Todos", multiple = TRUE)
          ),
          column(
            width = 3,
            selectInput("filtro_dias_semana", "Dias da Semana:",
                        choices = c("Todos", levels(acidentes_US_date_sample$Weekday)),
                        selected = "Todos", multiple = TRUE)
          ),
          column(
            width = 3,
            selectInput("filtro_severidade", "Severidade:",
                        choices = c("Todos", "1", "2", "3", "4"), selected = "Todos", multiple = TRUE)
          ),
          column(width = 3, actionButton("aplicar_data", "Aplicar Filtro"))
        ),
        fluidRow(
          column(width = 4, uiOutput("qtd_total_acidentes_date")),
          column(width = 4, uiOutput("severidade_media_date")),
          column(width = 4, uiOutput("duracao_media_date"))
        ),
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
                       card(
                         card_header("Visualização de Acidentes"),
                         radioButtons("tipo_periodo_mes", "Selecione o tipo de período:",
                                      choices = c("Crepúsculo Civil" = "civil_twilight",
                                                  "Período do Dia" = "day_period"),
                                      inline = TRUE),
                         plotlyOutput("mes2", width = "100%")
                       )
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
                       card(
                         card_header("Visualização de Acidentes"),
                         radioButtons("tipo_periodo_date", "Selecione o tipo de período:",
                                      choices = c("Crepúsculo Civil" = "civil_twilight",
                                                  "Período do Dia" = "day_period"),
                                      inline = TRUE),
                         plotlyOutput("date2", width = "100%")
                       )
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
      
      # ================================ UI - Clima ==================================
      
      tabItem(
        tabName = "clima",
        fluidRow(
          column(
            width = 3,
            selectInput(
              "variable",
              "Selecione a variável:",
              choices = names(condicoes_climaticas),
              selected = "Precipitation(in)"
            )
          ),
          column(
            width = 3,
            sliderInput(
              "bins",
              "Selecione a quantidade de barras:",
              min = 5,
              max = 50,
              value = 30
            )
          ),
          column(
            width = 3,
            actionButton("aplicar_clima", "Aplicar Filtro")
          )
        ),
        
        fluidRow(
          column(
            width = 12,
            box(
              width = 12,
              title = "Histograma de Váriáveis climáticas",
              status = "primary",
              solidHeader = TRUE,
              maximizable = TRUE,
              elevation = 3,
              plotOutput("histogram")
            )
          )
        ),
        fluidRow(
          column(
            width = 3,
            selectInput(
              "variable_x",
              "Escolha a variável do eixo x:",
              choices = names(condicoes_climaticas),
              selected = "Precipitation(in)"
            )
          ),
          column(
            width = 3,
            selectInput(
              "variable_y",
              "Escolha a variável do eixo y:",
              choices = names(condicoes_climaticas),
              selected = "Precipitation(in)"
            )
          ),
          column(
            width = 3,
            actionButton("aplicar_clima_correlacao", "Aplicar Filtro")
          )
        ),
        fluidRow(
          column(12,
                 verbatimTextOutput("correlation_text")
          )
        ),
        fluidRow(
          column(
            width = 12,
            box(
              width = 12,
              title = "Correlação",
              status = "primary",
              solidHeader = TRUE,
              maximizable = TRUE,
              elevation = 3,
              plotOutput("correlacao")
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            box(
              width = 12,
              title = "Resumo  das Variáveis Climáticas",
              status = "primary",
              solidHeader = TRUE,
              maximizable = TRUE,
              elevation = 3,
              DTOutput("tabela_resumo")
            )
          )
        )
      ),
      
      # ============================ UI - Infraestrutura =======================
      
      tabItem(
        tabName = "Sinalização_e_infraestrutura",
        fluidRow(
          column(
            width = 4,
            selectInput(
              inputId = "filtro_poi",
              label = "Ponto de interesse:",
              choices = c("Amenity", "Bump", "Crossing", "Give Way", "Junction", "No Exit",
                          "Railway", "Roundabout", "Station", "Stop", "Traffic Calming",
                          "Traffic Signal", "Turning Loop"),
              multiple = TRUE,
              selected = c("Amenity", "Bump", "Crossing", "Give Way", "Junction", "No Exit",
                           "Railway", "Roundabout", "Station", "Stop", "Traffic Calming",
                           "Traffic Signal", "Turning Loop")
            )
          ),
          column(
            width = 3,
            actionButton("filter_button", "Apply Filter", 
                         class = "btn-primary",
                         style = "margin-top: 25px;")
          )
        ),
        fluidRow(
          column(
            width = 12,
            box(
              width = NULL,
              title = "Acidentes por ponto de interesse",
              status = "primary",
              solidHeader = TRUE,
              plotlyOutput("plot_poi")
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            tabsetPanel(
              tabPanel(
                "Severidade por ponto de interesse absoluto",
                fluidRow(
                  box(
                    width = 12,
                    status = "primary",
                    fluidRow(
                      column(
                        width = 6,
                        plotlyOutput("absolute_severity")
                      ),
                      column(
                        width = 5,
                        offset = 1,
                        DTOutput("PoI_table")
                      )
                    )
                  )
                )
              ),
              tabPanel(
                "Severidade por ponto de interesse relativo",
                fluidRow(
                  box(
                    width = 12,
                    status = "primary",
                    plotlyOutput("severity_poi")
                  )
                )
              )
            )
          )
        )
      ),
      
      # =============================== UI - Duração ===========================
      
      tabItem(
        tabName = "duracao",
        fluidRow(
          column(width = 4, uiOutput("duracao_media")),
          column(width = 4, uiOutput("duracao_min")),
          column(width = 4, uiOutput("duracao_max"))
        ),
        fluidRow(
          column(
            width = 3,
            selectInput(
              "filtro_esta",
              "Selecione o(s) Estado(s):",
              choices = c("Todos", sort(unique(acidentes$NOMEESTADO))),
              selected = "Todos",
              multiple = TRUE
            )
          ),
          column(
            width = 3,
            selectInput(
              "filtro_sever",
              "Selecione a Severidade:",
              choices = c("Todos", sort(unique(acidentes$Severity))),
              selected = "Todos",
              multiple = TRUE
            )
          ),
          column(
            width = 3,
            selectInput(
              "filtro_clim",
              "Selecione as Condições Climáticas:",
              choices = c("Todos", sort(unique(acidentes$Weather_Condition))),
              selected = "Todos",
              multiple = TRUE
            )
          ),
          column(
            width = 3,
            actionButton("aplicar_duracao", "Aplicar Filtro", 
                         class = "btn-primary",
                         style = "margin-top: 25px;")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Duração de acidentes por severidade",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("duracaoid", height = 500),
            maximizable = TRUE
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Tabela de dados",
            status = "primary",
            solidHeader = TRUE,
            maximizable = TRUE,
            dataTableOutput("tabeladuracao")
          )
        )
      ),
      # ============================ UI - Relatório ============================
      tabItem(
        tabName = "relatorio",
        fluidRow(
          tags$iframe(
            src = "relatorio/Relatorio.html",
            style = "position: absolute; top: 50px; left: 250; width: 90%; height: calc(100vh - 50px); border: none;"
          )
        )
      )
    )
  )
)
