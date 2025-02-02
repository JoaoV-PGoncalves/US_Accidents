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

# ============================ Servidor (Server) ===============================
server <- function(input, output, session) {
  
  # ====================== Server - Filtros Localização ========================  
  
  # Dados reativos baseados nos filtros
  dados_filtrados <- eventReactive(input$aplicar, {
    withProgress(message = 'Aplicando filtros...', {
      dados <- acidentes
      
      for(i in 1:5) {
        incProgress(0.1, detail = paste("Carregando dados... Etapa", i))
        Sys.sleep(0.1)
      }
      
      if (!("Todos" %in% input$filtro_estado)) {
        for(i in 1:5) {
          incProgress(0.1, detail = paste("Filtrando estados... Etapa", i))
          Sys.sleep(0.1)
        }
        dados <- dados %>% filter(NOMEESTADO %in% input$filtro_estado)
      }
      
      if (!("Todos" %in% input$filtro_severidade)) {
        for(i in 1:5) {
          incProgress(0.1, detail = paste("Filtrando severidade... Etapa", i))
          Sys.sleep(0.1)
        }
        dados <- dados %>% filter(Severity %in% input$filtro_severidade)
      }
      
      if (!("Todos" %in% input$filtro_clima)) {
        for(i in 1:5) {
          incProgress(0.1, detail = paste("Filtrando clima... Etapa", i))
          Sys.sleep(0.1)
        }
        dados <- dados %>% filter(Weather_Condition %in% input$filtro_clima)
      }
      
      return(dados)
    })
  }, ignoreNULL = FALSE)
  
  # ============================ Server - Localização ==========================
  
  output$plotmapa <- renderLeaflet({
    dados <- dados_filtrados()
    
    validate(
      need(nrow(dados) > 0, "Nenhum dado corresponde aos filtros selecionados.")
    )
    
    # Contagem por estado usando NOMEESTADO
    acidentes_p_estado <- dados %>% 
      group_by(State, NOMEESTADO) %>% 
      summarise(n = n(), .groups = "drop")
    
    
    # Juntando com a geometria dos estados
    acidentes_p_estado <- left_join(acidentes_p_estado, estados, by = c("State" = "STUSPS"))
    acidentes_p_estado <- st_as_sf(acidentes_p_estado)
    
    # Substituindo NA por 0 para estados sem acidentes
    acidentes_p_estado$n[is.na(acidentes_p_estado$n)] <- 0
    
    mapa_de_calor <- dados %>%
      group_by(`Start Lat`, `Start Lng`) %>%
      summarise(n = n(), .groups = "drop")
    
    cores <- c("white", "red", "darkred")
    palestado <- colorNumeric(palette = cores, domain = c(0, max(acidentes_p_estado$n, na.rm = TRUE)))
    
    leaflet() %>%
      addTiles() %>%
      addPolygons(
        data = acidentes_p_estado,
        fillColor = ~palestado(n),
        weight = 2,
        color = "black",
        fillOpacity = 0.4,
        label = ~NAME,
        popup = ~glue("<b>Estado: </b> {NAME}<br><b>Quantidade de acidentes: </b> {n}"),
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#666",
          fillOpacity = 0.6,
          bringToFront = TRUE
        ),
        group = "Nível estadual"
      ) %>%
      addLegend(
        "bottomright",
        pal = palestado,
        values = acidentes_p_estado$n,
        title = "Número de Acidentes",
        opacity = 1,
        group = "Nível estadual"
      ) %>%
      addHeatmap(
        data = mapa_de_calor,
        lng = ~`Start Lng`,
        lat = ~`Start Lat`,
        intensity = ~n,
        blur = 20,
        radius = 15,
        group = "Mapa de calor"
      ) %>%
      addLayersControl(
        baseGroups = c("Nível estadual", "Mapa de calor"),
        position = "topright"
      )
  })
  
  output$tabelaleaflet <- renderDataTable({
    dados <- dados_filtrados()
    
    validate(
      need(nrow(dados) > 0, "Nenhum dado corresponde aos filtros selecionados.")
    )
    
    dados <- dados %>%
      group_by(NOMEESTADO, City) %>%
      summarise(n = n(), .groups = "drop") %>%
      arrange(desc(n))
    
    names(dados) <- c("Estado","Cidade","Quantidade de acidentes")
    dados <- dados %>% select(Cidade, Estado, `Quantidade de acidentes`)
    
    return(dados)
  })
  
  # Outputs reativos para as infoBoxes
  output$media_estado_box <- renderUI({
    dados <- dados_filtrados()
    
    media_acidentes_estado <- dados %>%
      group_by(NOMEESTADO) %>%
      summarise(n = n()) %>%
      summarise(media = round(mean(n))) %>%
      pull(media)
    
    infoBox(
      title = "Média de acidentes por estado",
      value = media_acidentes_estado,
      icon = icon("skull"),
      color = "primary",
      width = 12
    )
  })
  
  output$media_cidade_box <- renderUI({
    dados <- dados_filtrados()
    
    media_acidentes_cidade <- dados %>%
      group_by(City) %>%
      summarise(n = n()) %>%
      summarise(media = round(mean(n))) %>%
      pull(media)
    
    infoBox(
      title = "Média de acidentes por cidade",
      value = media_acidentes_cidade,
      icon = icon("city"),
      color = "primary",
      width = 12
    )
  })
  
  output$total_cidades_box <- renderUI({
    dados <- dados_filtrados()
    
    n_cidades <- dados %>%
      distinct(City) %>%
      nrow()
    
    infoBox(
      title = "Total de cidades",
      value = n_cidades,
      icon = icon("map-marker-alt"),
      color = "primary",
      width = 12
    )
  })
  
  # =========================== Server - Filtro Datas ==========================
  
  dados_filtrados_data <- reactive({
    # Armazena o valor do botão para triggerar a atualização
    input$aplicar_data
    
    isolate({
      # Comece com todos os dados
      dados <- acidentes_US_date_sample
      
      # Filtro de data
      dados <- dados[Date >= input$date_filter[1] & Date <= input$date_filter[2]]
      
      # Filtro de tipo de dia
      if (!("Todos" %in% input$filtro_time_range)) {
        dados <- dados[Time_Range %in% input$filtro_time_range]
      } else {
        dados
      }
      
      # Filtro de dias da semana
      if (!("Todos" %in% input$filtro_dias_semana)) {
        dados <- dados[Weekday %in% input$filtro_dias_semana]
      } else {
        dados
      }
      
      # Filtro de Severidade
      if (input$filtro_severidade == "Todos") {  # If "Todos" is selected
        dados  # Return all data
      } else {
        dados <- dados[Severity %in% input$filtro_severidade]  # Filter by selected severity
      }
      
      return(dados)
    })
  })
  
  # ============================== Server - Datas ==============================
  
  output$ano <- renderPlotly({
    dados <- req(dados_filtrados_data())
    
    validate(
      need(nrow(dados) > 0, "Sem dados para exibir")
    )
    
    acidentes_ano_barplot <- dados[Civil_Twilight == "Day" | Civil_Twilight == "Night",
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
                       "<br>Dias Obs. no Ano:", Distinct_date,
                       "<br>Acidentes por Dia:", round(Media_diaria/1000, 1), "k")) + 
      geom_bar(stat = "identity", fill = "darkred") + 
      geom_text(aes(label = percent(Percent_variation)), size = 3) + 
      scale_y_continuous(
        labels = label_number(scale = 1/1000, suffix = "k") # Divide por 1000 e adiciona o sufixo "k"
      ) +
      labs(title = "Qtd. de Acidentes por Ano", x = "Ano", y = "Qtd. Acidentes") +
      theme_minimal() + 
      theme(plot.title = element_text(hjust = 0.5)) + 
      plotly()
    acidentes_ano_barplot %>% ggplotly(tooltip = "text")
  })
  
  output$mes <- renderPlotly({
    dados <- req(dados_filtrados_data())
    
    validate(
      need(nrow(dados) > 0, "Sem dados para exibir")
    )
    
    acidentes_mes_barplot <- dados[Civil_Twilight == "Day" | Civil_Twilight == "Night",
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
      geom_bar(stat = "identity", fill = "darkred") +
      scale_y_continuous(
        labels = label_number(scale = 1/1000, suffix = "k") # Divide por 1000 e adiciona o sufixo "k"
      ) +
      labs(title = "Qtd. de Acidentes por Mes", x = "Mes", y = "Qtd. Acidentes") +
      theme_minimal() + 
      theme(plot.title = element_text(hjust = 0.5))
    acidentes_mes_barplot %>% ggplotly(tooltip = "text")
  })
  
  output$mes2 <- renderPlotly({
    dados <- req(dados_filtrados_data())
    
    validate(
      need(nrow(dados) > 0, "Sem dados para exibir")
    )
    
    if(input$tipo_periodo_mes == "civil_twilight") {
      # Código do gráfico para Civil Twilight
      acidentes_mes_barplot_CV <- dados[Civil_Twilight == "Day" | Civil_Twilight == "Night", .N, by = .(Month, Civil_Twilight)][
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
      acidentes_mes_barplot_CV %>% ggplotly(tooltip = "text") %>%
        layout(
          legend = list(
            title = list(text = 'Tipo de Dia'),  # Título da legenda
            font = list(size = 14),  # Tamanho da fonte
            itemsizing = 'constant',  # Define o tamanho fixo dos itens da legenda
            traceorder = 'normal',  # Ordem das legendas
            marker = list(
              size = 10
            )
          )
        )
    } else {
      # Código do gráfico para Day Period
      
      acidentes_mes_barplot_DP <- dados[Day_Period == "06:00:00 -> 18:00:00" | Day_Period == "18:00:00 -> 06:00:00", .N, by = .(Month, Day_Period)][
        , N_month := sum(N), by = Month][
          , Percent := round((N/N_month), 2)] %>% 
        setorder(Month, Day_Period) %>% # order data set increasingly by month
        as.data.frame() %>% # Transforming data.table into data.frame
        ggplot() + 
        aes(x = Month, y = N,
            text = paste("Mes:", Month,
                         "<br>Qtd. Acidentes:", round(N/1000, 1), "k",
                         "<br>% Total Acidentes:", round(100*Percent, 0), "%")) + 
        geom_bar(aes(fill = Day_Period), stat = "identity") +
        scale_fill_manual(values = c("06:00:00 -> 18:00:00" = '#D9A404', "18:00:00 -> 06:00:00" = '#0F3BBF')) +
        scale_y_continuous(
          labels = label_number(scale = 1/1000, suffix = "k") # Divide por 1000 e adiciona o sufixo "k"
        ) +
        labs(title = "Qtd. de Acidentes por Mes", x = "Mes", y = "Qtd. Acidentes") +
        theme_minimal() + 
        theme(plot.title = element_text(hjust = 0.5))
      acidentes_mes_barplot_DP %>% ggplotly(tooltip = "text") %>%
        layout(
          legend = list(
            title = list(text = 'Periodo do Dia'),  # Título da legenda
            font = list(size = 14),  # Tamanho da fonte
            itemsizing = 'constant',  # Define o tamanho fixo dos itens da legenda
            traceorder = 'normal',  # Ordem das legendas
            marker = list(
              size = 10
            )
          )
        )
    }
  })
  
  output$date <- renderPlotly({
    
    dados <- req(dados_filtrados_data())
    
    validate(
      need(nrow(dados) > 0, "Sem dados para exibir")
    )
    
    acidentes_data_timeseries <- dados[Civil_Twilight == "Day" | Civil_Twilight == "Night", .(.N), by = .(Date)] %>% 
      setorder(Date) %>% # order data set increasingly by date
      as.data.frame() %>%  # Transforming data.table into data.frame
      ggplot() + 
      aes(x = Date, y = N) + 
      geom_bar(stat = "identity", alpha = 0.4, fill = "darkred", alpha = 0.6) +
      geom_point(size = 0.5, alpha = 0.5, color = "darkred",
                 aes(text = paste("Data:", Date, "<br>Qtd. Acidentes:", N/1000, "k"))) +
      geom_smooth(method = "gam", color = "darkred", size = 0.5) +
      scale_y_continuous(
        labels = label_number(scale = 1/1000, suffix = "k") # Divide por 1000 e adiciona o sufixo "k"
      ) +
      labs(title = "Qtd. de Acidentes por Data", x = "Data", y = "Qtd. Acidentes") +
      theme_minimal()
    acidentes_data_timeseries %>% ggplotly(tooltip = "text")
  })
  
  output$date2 <- renderPlotly({
    
    dados <- req(dados_filtrados_data())
    
    validate(
      need(nrow(dados) > 0, "Sem dados para exibir")
    )
    
    if(input$tipo_periodo_date == "civil_twilight") {
      acidentes_data_timeseries_CT <- dados[Civil_Twilight == "Day" | Civil_Twilight == "Night", .(.N), by = .(Date, Civil_Twilight)] %>% 
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
        labs(title = "Qtd. de Acidentes por Data",
             x = "Data", y = "Qtd. Acidentes") +
        theme_minimal()
      acidentes_data_timeseries_CT %>% ggplotly(tooltip = "text") %>%
        layout(
          legend = list(
            title = list(text = 'Crepusculo Civil'),  # Título da legenda
            font = list(size = 14),  # Tamanho da fonte
            itemsizing = 'constant',  # Define o tamanho fixo dos itens da legenda
            traceorder = 'normal',  # Ordem das legendas
            marker = list(
              size = 10
            )
          )
        )
    } else {
      # Código do gráfico para Day Period
      acidentes_data_timeseries_6_18 <- dados[Day_Period == "06:00:00 -> 18:00:00" | Day_Period == "18:00:00 -> 06:00:00", .(.N), by = .(Date, Day_Period)] %>% 
        setorder(Date, Day_Period) %>% # order data set increasingly by date
        as.data.frame() %>%  # Transforming data.table into data.frame
        ggplot() + 
        aes(x = Date, y = N,
            text = paste("Data:", Date, "<br>Qtd. Acidentes:", N/1000, "k")) + 
        geom_bar(stat = "identity", alpha = 0.4) +
        geom_point(aes(color = Day_Period), alpha = 0.4, size = 0.4) + 
        scale_y_continuous(
          labels = label_number(scale = 1/1000, suffix = "k") # Divide por 1000 e adiciona o sufixo "k"
        ) +
        scale_color_manual(values = c('06:00:00 -> 18:00:00' = '#D9A404', '18:00:00 -> 06:00:00' = '#0F3BBF')) +
        labs(title = "Qtd. de Acidentes por Data", x = "Data", y = "Qtd. Acidentes") +
        theme_minimal()
      acidentes_data_timeseries_6_18 %>% ggplotly(tooltip = "text") %>%
        layout(
          legend = list(
            title = list(text = 'Periodo do Dia'),  # Título da legenda
            font = list(size = 14),  # Tamanho da fonte
            itemsizing = 'constant',  # Define o tamanho fixo dos itens da legenda
            traceorder = 'normal',  # Ordem das legendas
            marker = list(
              size = 10
            )
          )
        )
    }
  })
  
  output$agenda <- renderPlotly({
    
    dados <- req(dados_filtrados_data())
    
    validate(
      need(nrow(dados) > 0, "Sem dados para exibir")
    )
    
    # Adicionando labels customizados no tooltip
    acidentes_data_matriz_AWKTR <- dados[Civil_Twilight == "Day" | Civil_Twilight == "Night", .N, by = .(Weekday, Time_Range)] %>%
      setorder(Weekday, -Time_Range) %>% # Ordenação por weekday e time range
      as.data.frame() %>% # Transformando em data.frame para ggplot
      ggplot() + 
      aes(
        x = Weekday, 
        y = Time_Range, 
        fill = N, 
        text = paste(
          "Dia da Semana: ", Weekday, "<br>",
          "Faixa horária: ", Time_Range, "<br>",
          "Qtd. de Acidentes: ", N
        )
      ) +
      geom_tile() + 
      scale_fill_gradient(
        low = "white", 
        high = "darkred",
        name = "Número de Acidentes" # Alterando o título da legenda
      ) +
      labs(
        title = "Agenda Diária de Acidentes", 
        x = "Dia da Semana", 
        y = "Horário"
      ) + 
      theme_minimal() + 
      scale_x_discrete(position = "top") +
      theme(
        axis.title.x.top = element_text(),
        axis.text.x.top = element_text(),
        axis.line.x.top = element_line(),
        axis.ticks.x.top = element_line()
      )
    
    # Transformando ggplot para um gráfico interativo do plotly
    ggplotly(acidentes_data_matriz_AWKTR, tooltip = "text")
  })
  
  
  output$agenda2 <- renderPlotly({
    
    dados <- req(dados_filtrados_data())
    
    validate(
      need(nrow(dados) > 0, "Sem dados para exibir")
    )
    
    # Adicionando labels customizados no tooltip
    acidentes_data_matriz_SWKTR <- dados[Civil_Twilight == "Day" | Civil_Twilight == "Night", .(.N, Severity = mean(Severity)), by = .(Weekday, Time_Range)] %>%
      setorder(Weekday, -Time_Range) %>% # Ordenação por weekday e time range
      as.data.frame() %>% # Transformando em data.frame para ggplot
      ggplot() + 
      aes(
        x = Weekday, 
        y = Time_Range, 
        fill = Severity, 
        text = paste(
          "Dia da Semana: ", Weekday, "<br>",
          "Faixa horária: ", Time_Range, "<br>",
          "Severidade Média: ", round(Severity, 2), "<br>",
          "Qtd. Acidentes: ", N
        )
      ) +
      geom_tile() + 
      scale_fill_gradient(
        low = "white", 
        high = "darkred",
        name = "Severidade Média"
      ) +
      labs(
        title = "Agenda Diária da Severidade Média de Acidentes", 
        x = "Dia da Semana", 
        y = "Horário"
      ) + 
      theme_minimal() + 
      scale_x_discrete(position = "top") +
      theme(
        axis.title.x.top = element_text(),
        axis.text.x.top = element_text(),
        axis.line.x.top = element_line(),
        axis.ticks.x.top = element_line()
      )
    
    # Transformando ggplot para um gráfico interativo do plotly
    ggplotly(acidentes_data_matriz_SWKTR, tooltip = "text")
  })
  
  
  output$severidade_date <- renderPlotly({
    
    dados <- req(dados_filtrados_data())
    
    validate(
      need(nrow(dados) > 0, "Sem dados para exibir")
    )
    
    severity_data_points <- dados[(Civil_Twilight == "Day" | Civil_Twilight == "Night") & Year > 2016,
                                  .(Severidade_Media = mean(Severity)),
                                  by = .(Date)][order(Date)] %>%
      ggplot() + 
      aes(x = Date, y = Severidade_Media) + 
      geom_point(size = 0.5, alpha = 0.1, color = "darkred",
                 aes(text = paste("Data:", Date,
                                  "<br>Severidade Média:", round(Severidade_Media, 1)))) +
      geom_smooth(method = "lm", size = 0.5, se = FALSE, color = "darkred") +
      ylim(0, 4) +
      labs(title = "Severidade Média por Data", x = "Data", y = "Severidade") +
      theme_minimal()
    severity_data_points %>% ggplotly(tooltip = "text")
  })
  
  output$severidade_date2 <- renderPlotly({
    
    
    dados <- req(dados_filtrados_data())
    
    validate(
      need(nrow(dados) > 0, "Sem dados para exibir")
    )
    
    severity_data_points_WK <- dados[(Civil_Twilight == "Day" | Civil_Twilight == "Night") & Year > 2016,
                                     .(Severidade_Media = mean(Severity)),
                                     by = .(Date, `Tipo de Dia`)] %>% 
      setorder(Date) %>%
      as.data.frame() %>% 
      ggplot() + 
      aes(x = Date, y = Severidade_Media,
          text = paste("Data:", Date,
                       "<br>Severidade Média:", round(Severidade_Media, 1),
                       "<br>Tipo do Dia:", `Tipo de Dia`)) + 
      geom_point(size = 0.5, alpha = 0.1, aes(color = `Tipo de Dia`)) + 
      geom_smooth(method = "lm", size = 0.5, se = FALSE, aes(group = `Tipo de Dia`, color = `Tipo de Dia`)) + 
      scale_color_manual(values = c(
        "Final de semana" = "#E00704", 
        "Dia da semana" = "#04E048"
      )) + 
      ylim(0, 4) + 
      labs(title = "Severidade Média dos Acidentes por Data", 
           x = "Data", 
           y = "Severidade") +
      theme_minimal()
    severity_data_points_WK %>% ggplotly(tooltip = "text") %>%
      layout(
        legend = list(
          title = list(text = 'Tipo de Dia'),  # Título da legenda
          font = list(size = 14),  # Tamanho da fonte
          itemsizing = 'constant',  # Define o tamanho fixo dos itens da legenda
          traceorder = 'normal',  # Ordem das legendas
          marker = list(
            size = 10
          )
        )
      )
  })
  
  output$contingencyTable <- renderDataTable({
    
    dados <- req(dados_filtrados_data())
    
    validate(
      need(nrow(dados) > 0, "Sem dados para exibir")
    )
    
    tabela5 <- dados[, .N, by = .(Weekday, Time_Range)] %>% 
      dcast(Time_Range ~ Weekday, value.var = "N", fill = 0)
    tabela5
  })
  
  output$qtd_total_acidentes_date <- renderUI({
    dados <- req(dados_filtrados_data())
    
    validate(
      need(nrow(dados) > 0, "Sem dados para exibir")
    )
    
    qtd_total_acidentes <- nrow(dados)
    
    infoBox(title = "Qtd. Total (un.)",
            value = qtd_total_acidentes,
            icon = icon("home"),
            color = "primary",
            width = 12)
  })
  
  output$severidade_media_date <- renderUI({
    dados <- req(dados_filtrados_data())
    
    validate(
      need(nrow(dados) > 0, "Sem dados para exibir")
    )
    
    severidade_media <- round(mean(dados$Severity), 2)
    
    infoBox(title = "Severidade Média (1-4)",
            value = severidade_media,
            icon = icon("home"),
            color = "primary",
            width = 12)
  })
  
  output$duracao_media_date <- renderUI({
    dados <- req(dados_filtrados_data())
    
    validate(
      need(nrow(dados) > 0, "Sem dados para exibir")
    )
    
    duracao_media <- round(mean(dados$Duration), 0)
    
    infoBox(title = "Duração Média (min.)",
            value = duracao_media,
            icon = icon("home"),
            color = "primary",
            width = 12)
  })
  
  # ========================== Server - Filtros clima ============================
  
  filtered_settings <- reactiveVal(list(
    variable = "Precipitation(in)",  # valor inicial do selectInput
    bins = 30                        # valor inicial do sliderInput
  ))
  
  correlation_settings <- reactiveVal(list(
    variable_x = "Precipitation(in)", # valor inicial do primeiro selectInput
    variable_y = "Precipitation(in)"  # valor inicial do segundo selectInput
  ))
  
  # Update filtered settings quando o botão é clicado
  observeEvent(input$aplicar_clima, {
    filtered_settings(list(
      variable = input$variable,
      bins = input$bins
    ))
  })
  
  observeEvent(input$aplicar_clima_correlacao, {
    correlation_settings(list(
      variable_x = input$variable_x,
      variable_y = input$variable_y
    ))
  })
  
  # ============================= Server - Clima ===============================
  
  # Histograma
  output$histogram <- renderPlot({
    req(filtered_settings())
    settings <- filtered_settings()
    
    ggplot(condicoes_climaticas, aes(x = .data[[settings$variable]])) +
      geom_histogram(fill = "darkred", 
                     color = "white", 
                     bins = settings$bins) +
      theme_minimal() +
      scale_y_continuous(labels = scales::comma_format(accuracy = 1)) +  # Formatação para números inteiros
      labs(title = paste("Histograma de", settings$variable),
           x = settings$variable,
           y = "Contagem")
  })
  
  # Texto da correlação
  output$correlation_text <- renderText({
    req(correlation_settings())
    settings <- correlation_settings()
    
    correlation <- cor(condicoes_climaticas[[settings$variable_x]], 
                       condicoes_climaticas[[settings$variable_y]], 
                       use = "complete.obs")
    
    paste("Coeficiente de Correlação de Pearson:", round(correlation, 3))
  })
  
  # Gráfico de correlação
  output$correlacao <- renderPlot({
    req(correlation_settings())
    settings <- correlation_settings()
    
    ggplot(condicoes_climaticas, 
           aes(x = .data[[settings$variable_x]], 
               y = .data[[settings$variable_y]])) +
      geom_point(color = "steelblue", alpha = 0.6) +
      geom_smooth(method = "lm", color = "red", se = TRUE) +
      theme_minimal() +
      labs(title = paste("Correlação entre", settings$variable_x, "e", settings$variable_y),
           x = settings$variable_x,
           y = settings$variable_y)
  })
  
  # Tabela de resumo
  output$tabela_resumo <- renderDT({
    resumo <- data.frame(
      Variável = names(condicoes_climaticas),
      Média = sapply(condicoes_climaticas, mean, na.rm = TRUE),
      Mediana = sapply(condicoes_climaticas, median, na.rm = TRUE),
      "Desvio Padrão" = sapply(condicoes_climaticas, sd, na.rm = TRUE),
      Mínimo = sapply(condicoes_climaticas, min, na.rm = TRUE),
      Máximo = sapply(condicoes_climaticas, max, na.rm = TRUE)
    )
    
    resumo[, -1] <- round(resumo[, -1], 2)
    
    datatable(
      resumo,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'tp'
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    )
  })
  
  # ======================== Server - Filtro Infraestrutura ====================
  
  dados_filtrados_infra <- eventReactive(input$filter_button, {
    acidentes_US_sample %>% 
      select(Severity, all_of(input$filtro_poi))
  }, ignoreNULL = FALSE)
  
  # ========================== Server - Infraestrutura =========================
  
  output$plot_poi <- renderPlotly({
    # Usando dados filtrados
    poi <- dados_filtrados_infra() %>% 
      select(-Severity)
    
    barplot <- poi %>%
      pivot_longer(everything(), names_to = "PoI", values_to = "Presenca") %>%
      mutate(Presenca = ifelse(Presenca == FALSE, "Não presente", "Presente")) %>%  # Modificação aqui
      ggplot(aes(x = PoI, fill = Presenca)) +
      geom_bar() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Pontos de Interesse",
           x = NULL,
           y = NULL,
           fill = "Presença de POI") +
      theme_minimal() +
      scale_y_continuous(labels = comma) +
      scale_fill_manual(values = c("darkgreen", "darkred"))  # Removida a parte de labels aqui pois já está definida no mutate
    
    ggplotly(barplot)
  })
  
  # Modificado para DT
  output$PoI_table <- renderDT({
    dados <- dados_filtrados_infra()
    
    poi_sum <- dados %>%
      mutate(Total = rowSums(select(., -Severity)))
    
    tabela <- poi_sum %>%
      count(Severity, Total) %>%
      pivot_wider(names_from = Total, values_from = n, values_fill = 0)
    
    datatable(tabela,
              options = list(
                dom = 'tp',
                pageLength = 10
              ),
              rownames = FALSE) %>%
      formatStyle(names(tabela),
                  backgroundColor = '#f8f9fa')
  })
  
  output$absolute_severity <- renderPlotly({
    dados <- dados_filtrados_infra()
    
    poi_sum <- dados %>%
      mutate(Total = rowSums(select(., -Severity)))
    
    plot_data <- poi_sum %>%
      count(Severity, Total) %>%
      group_by(Severity) %>%
      mutate(sum_PoI = sum(n))
    
    absolute_severity_bars <- plot_data %>%
      ggplot(aes(x = factor(Severity), y = n, fill = factor(Total))) +
      geom_bar(stat = "identity", position = "dodge", aes(text = paste("Severidade: ",factor(Severity),
                                                                       "<br>Frequencia: ", n,
                                                                       "<br>Total: ",factor(Total)))) +
      scale_y_continuous(labels = comma) +
      theme_minimal() +
      labs(
        title = "Severidade de acidentes por ponto de interesse",
        x = NULL,
        y = NULL,
        fill = "Total POIs"
      )
    
    ggplotly(absolute_severity_bars, tooltip = "text")
  })
  
  output$severity_poi <- renderPlotly({
    dados <- dados_filtrados_infra()
    
    poi_sum <- dados %>%
      mutate(Total = rowSums(select(., -Severity)))
    
    plot_data <- poi_sum %>%
      count(Severity, Total) %>%
      group_by(Severity) %>%
      mutate(
        sum_PoI = sum(n),
        Rel_Freq = n / sum_PoI
      )
    
    severity_bars <- plot_data %>%
      ggplot(aes(x = factor(Severity), y = Rel_Freq, fill = factor(Total))) +
      geom_bar(stat = "identity", position = "dodge", aes(text = paste("Severidade: ",factor(Severity),
                                                                       "<br>Frequencia: ", n,
                                                                       "<br>Total: ",factor(Total)))) +
      scale_y_continuous(labels = percent_format()) +
      theme_minimal() +
      labs(
        title = "Porcentagem de acidentes por severidade",
        x = NULL,
        y = NULL,
        fill = "Total POIs"
      )
    
    ggplotly(severity_bars, tooltip = "text")
  })
  
  # ============================ Server - Filtro duração =======================
  
  dados_filtrados_duracao <- eventReactive(input$aplicar_duracao, {
    dados <- acidentes
    
    if (!("Todos" %in% input$filtro_esta)) {
      dados <- dados %>% filter(NOMEESTADO %in% input$filtro_esta)
    }
    if (!("Todos" %in% input$filtro_sever)) {
      dados <- dados %>% filter(Severity %in% input$filtro_sever)
    }
    if (!("Todos" %in% input$filtro_clim)) {
      dados <- dados %>% filter(Weather_Condition %in% input$filtro_clim)
    }
    
    dados
  }, ignoreNULL = FALSE)
  
  # ============================ Server - Duração ==============================
  
  output$duracao_media <- renderUI({
    dados <- dados_filtrados_duracao()
    duracao_media <- dados %>%
      summarise(media = round(mean(Duration_minutes, na.rm = TRUE), 1)) %>%
      pull(media)
    
    infoBox(
      title = "Duração média dos acidentes (min)",
      value = duracao_media,
      icon = icon("clock"),
      color = "primary",
      width = 12
    )
  })
  
  output$duracao_min <- renderUI({
    dados <- dados_filtrados_duracao()
    duracao_min <- dados %>%
      summarise(min = round(min(Duration_minutes, na.rm = TRUE), 1)) %>%
      pull(min)
    
    infoBox(
      title = "Duração mínima dos acidentes (min)",
      value = duracao_min,
      icon = icon("clock"),
      color = "primary",
      width = 12
    )
  })
  
  output$duracao_max <- renderUI({
    dados <- dados_filtrados_duracao()
    duracao_max <- dados %>%
      summarise(max = round(max(Duration_minutes, na.rm = TRUE), 1)) %>%
      pull(max)
    
    infoBox(
      title = "Duração máxima dos acidentes (min)",
      value = duracao_max,
      icon = icon("clock"),
      color = "primary",
      width = 12
    )
  })
  
  output$duracaoid <- renderPlotly({
    dados <- dados_filtrados_duracao()
    
    validate(need(nrow(dados) > 0, "Nenhum dado corresponde aos filtros selecionados."))
    
    p <- dados %>%
      ggplot(aes(x = as.factor(Severity), y = Duration_minutes)) +
      geom_boxplot(fill = "darkred", outlier.alpha = 0.5, outlier.shape = NA) +
      labs(
        title = "Distribuição da Duração dos Acidentes por Severidade",
        x = "Severidade",
        y = "Duração (minutos)"
      ) +
      scale_y_continuous(limits = quantile(dados$Duration_minutes,c(0.0,0.9)))+
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12)
      )
    
    ggplotly(p) %>% 
      layout(showlegend = FALSE)
  })
  
  output$tabeladuracao <- renderDataTable({
    dados <- dados_filtrados_duracao()
    
    validate(need(nrow(dados) > 0, "Nenhum dado corresponde aos filtros selecionados."))
    
    resumo <- dados %>%
      group_by(Severity) %>%
      summarise(
        `Quantidade de Acidentes` = n(),
        `Duração Média (min)` = round(mean(Duration_minutes, na.rm = TRUE), 1),
        `Duração Mínima (min)` = round(min(Duration_minutes, na.rm = TRUE), 1),
        `Duração Máxima (min)` = round(max(Duration_minutes, na.rm = TRUE), 1),
        `Desvio Padrão (min)` = round(sd(Duration_minutes, na.rm = TRUE), 1)
      ) %>%
      arrange(Severity)
    
    DT::datatable(
      resumo,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'tp',
        language = list(
          info = "Mostrando _START_ a _END_ de _TOTAL_ registros",
          paginate = list(
            "previous" = "Anterior",
            "next" = "Próximo"
          )
        )
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    )
  })
}
