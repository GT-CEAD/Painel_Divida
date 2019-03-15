library(shiny)
library(shinydashboard)
library(flexdashboard)
library(DT)
library(readxl)
library(plotly)
library(datasets)
library(stringr)
library(dplyr)
library(tidyr)
library(lubridate)


load("custo_medio.Rdata")
load("Tesouro_Direto.Rdata")
load("DPF_Mercado.Rdata")
load("Perfil_Vencimentos.Rdata")

DPF_Fonte <- read_excel("DPFOrcamentoFonte.xlsx",
                        col_types = c("numeric", "text","text", "text","text", "numeric", "numeric","text", "text","numeric", "text", "numeric"))

leiloes <- read_excel("historicoleiloes.xlsx", skip=6)

names(DPF_Fonte)[12]<- "Valor"
names(DPF_Fonte)[5]<- "Fonte"
names(DPF_Fonte)[11]<- "Natureza_Depesa_Nome"
names(DPF_Fonte)[10]<- "Natureza_Depesa_Codigo"

datatable(DPF_Fonte) %>% formatCurrency(12, '\U20AC', digits = 0)

server = function(input, output, session) { 
  
  
  
  #Grafico Ilustrativo - Relatorios Publicados
  
  output$plot1 <- renderPlotly({
    plot_ly(data=mtcars,
            x=~wt,
            y=~mpg,
            type="scatter",
            mode="markers")
    
    
  })
  
  #Tabela com a execução orçamentaria
  
  output$Fontes <- renderDataTable(#DPF_Fonte [,1:12], rownames = FALSE
    
    {
    
      data <- DPF_Fonte
    
      if (input$exe != "todos") {
      data <- data[data$Exercicio == input$exe,]}

    if (input$cart != "todos") {
      data <- data[data$Carteira == input$cart,]}

    if (input$font != "todos") {
      data <- data[data$Fonte == input$font,]}

    if (input$mov != "todos") {
      data <- data[data$Natureza_Depesa_Nome == input$mov,]}

    data}
  
  )
  
  #Tabela com leiloes
  
  output$leiloes <- renderDataTable(leiloes, options = list(paging = FALSE, searching=FALSE) ,rownames = FALSE)
  
  #Construindo os Boxes com informações dos Grandes Numeros
  
  output$estoque <-   renderInfoBox({infoBox(title= strong("Estoque DPF:"), 
                                             subtitle = "Intervalor PAF: x a y",
                                             value = "R$ 3,756 Bilhões", 
                                             #icon = icon("file"),
                                             color = "teal", 
                                             fill = TRUE)
  })
  
  output$prefixado <-   renderInfoBox({infoBox(title= strong("Prefixado:"), 
                                               subtitle = "Intervalor PAF: x a y",
                                               value = "30%", 
                                               #icon = icon("file"),
                                               color = "blue", 
                                               fill = TRUE)
  })
  
  output$posfixados <-   renderInfoBox({infoBox(title= strong("Posfixado:"), 
                                                subtitle = "Intervalor PAF: x a y",
                                                value = "30%", 
                                                #icon = icon("file"),
                                                color = "yellow", 
                                                fill = TRUE)
    
  })
  
  output$preço <-   renderInfoBox({infoBox(title= strong("Índice de Preços"), 
                                           subtitle = "Intervalor PAF: x a y",
                                           value = "30%", 
                                           #icon = icon("file"),
                                           color = "red", 
                                           fill = TRUE)
  })
  
  output$cambial <-   renderInfoBox({infoBox(title= strong("Cambial"), 
                                             subtitle = "Intervalor PAF: x a y",
                                             value = "10%", 
                                             #icon = icon("file"),
                                             color = "green", 
                                             fill = TRUE)
  })
  
  output$prazomedio <-   renderInfoBox({infoBox(title= strong("Prazo Médio"), 
                                                subtitle = "Intervalor PAF: x a y",
                                                value = "4.25", 
                                                #icon = icon("file"),
                                                color = "purple", 
                                                fill = TRUE)
  })
  
  output$vincendomeses <-   renderInfoBox({infoBox(title= strong("% Vincendo 12 Meses"), 
                                                   subtitle = "Intervalor PAF: x a y",
                                                   value = "17%", 
                                                   #icon = icon("file"),
                                                   color = "black", 
                                                   fill = TRUE)
  })
  
  # Construindo Gauges (Velocimetro) dos Grandes Numeros
  
  output$plt1 <-  flexdashboard::renderGauge({
    gauge(4.75, min = 4.25, max = 5.10, symbol = "",label = paste("Intervalo Meta PAF"),gaugeSectors(
      success = c(100, 6), warning = c(5,1), danger = c(0, 1), colors = c("#CC6699")
    ))
    
  })
  
  output$plt2 <-  flexdashboard::renderGauge({
    gauge(28, min = 26, max = 33, symbol = '%',label = paste("Intervalo Meta PAF"),gaugeSectors(
      success = c(100, 6), warning = c(5,1), danger = c(0, 1), colors = c("#CC6699")
    ))
    
  })
  
  output$plt3 <-  flexdashboard::renderGauge({
    gauge(32, min = 26, max = 33, symbol = '%',label = paste("Intervalo Meta PAF"),gaugeSectors(
      success = c(100, 6), warning = c(5,1), danger = c(0, 1), colors = c("#CC6699")
    ))
    
  })
  
  output$plt4 <-  flexdashboard::renderGauge({
    gauge(30, min = 26, max = 33, symbol = '%',label = paste("Intervalo Meta PAF"),gaugeSectors(
      success = c(100, 6), warning = c(5,1), danger = c(0, 1), colors = c("#CC6699")
    ))
    
  })
  
  output$Calculadora <- renderUI({
    tags$iframe(src="https://lucasgurgelleite.shinyapps.io/Calculadora_Divida/", height=800, width=1300)
  
  
  
})
  
  #Gráficos Custo Medio
  
  #Grafico - Custo Medio Estoque DPF
  
  output$DPF <- renderPlotly({
    
    
    
    plot_ly(custo_medio_estoque, x=~Periodo) %>%
      
      add_lines(y = ~DPF, name = "DPF", line = list(color = '#4682B4'
      )) %>%
      
      add_lines(y = ~DPMFi, name = "DPMFi", visible = "legendonly", line = list(color = '#FFA500'
      )) %>%
      
      add_lines(y = ~DPFe, name = "DPFe", visible = "legendonly", line = list(color = '#008000'
      ), yaxis = 'y2') %>%
      
      add_lines(y = ~Selic, name = "SELIC", visible = "legendonly", line = list(color = '	#A0522D', width = 3, dash = 'dot')) %>%
      
      layout(margin = list(b = 20),annotations = 
               list(x = 1, y = -0.1, text = "<b>Fonte:</b> Anexo 4.2 - RMD. Selic - acumulada mês anualizada base 252 - Banco Central.", 
                    showarrow = F, xref='paper', yref='paper', 
                    xanchor='right', yanchor='auto', xshift=100, yshift=-80,
                    font=list(size=12, color="#696969")),
             title = "",
             xaxis = list(title = "", 
                          rangeselector = list(
                            buttons = list(
                              list(
                                count = 3,
                                label = "3 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 6,
                                label = "6 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 1,
                                label = "1 ano",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 3,
                                label = "3 anos",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 5,
                                label = "5 anos",
                                step = "year",
                                stepmode = "backward"),
                              
                              list(step = "all"))),
                          
                          rangeslider = list(type = "date")),
             yaxis = list(side = 'left', title = '% a.a - Acumulada 12 meses', showgrid = FALSE, zeroline = FALSE, ticksuffix = '%', hoverformat = '.2f'),
             yaxis2 = list(side = 'right', overlaying = "y" ,title='Eixo - DPFe  ' ,titlefont = list(color = '#008000') ,showgrid = FALSE, zeroline = FALSE ,ticksuffix = '%', hoverformat = '.2f')) %>%
      config(displayModeBar = FALSE) %>%
      layout(hovermode = 'compare')
    
    
    
    
    
  }) 
  
  #Grafico - Custo Medio Estoque DPMFi
  
  output$DPMFi <- renderPlotly({
    plot_ly(custo_medio_estoque, x = ~Periodo) %>%
      add_lines(y = ~DPMFi, name = "DPMFi", line = list(color = '#FFA500'
      )) %>%
      add_lines(y = ~LFT, name = "LFT", visible = "legendonly", line = list(color = '#FF4500'
      )) %>%
      add_lines(y = ~LTN, name = "LTN", visible = "legendonly", line = list(color = '#B22222'
      )) %>%
      add_lines(y = ~NTN_B, name = "NTN-B", visible = "legendonly", line = list(color = '#2F4F4F'
      ) ) %>%
      add_lines(y = ~NTN_C, name = "NTN-C", visible = "legendonly", line = list(color = '#D8BFD8'
      ) ) %>%
      add_lines(y = ~NTN_D, name = "NTN-D", visible = "legendonly", line = list(color = '#EEE8AA'
      )) %>%
      add_lines(y = ~NTN_F, name = "NTN-F", visible = "legendonly", line = list(color = '#FA8072'
      )) %>%
      add_lines(y = ~TDA, name = "TDA", visible = "legendonly", line = list(color = '#FFB6C1'
      )) %>%
      add_lines(y = ~Divida_Securitizada, name = "Dívida Securitizada", visible = "legendonly", line = list(color = '#DA70D6'
      )) %>%
      add_lines(y = ~Demais, name = "Demais", visible = "legendonly", line = list(color = '#708090'
      )) %>%
      
      layout(margin = list(b = 20),annotations = 
               list(x = 1, y = -0.1, text = "<b>Fonte:</b> Anexo 4.2 - RMD.", 
                    showarrow = F, xref='paper', yref='paper', 
                    xanchor='right', yanchor='auto', xshift=100, yshift=-80,
                    font=list(size=12, color="#696969")),
             title = "",
             xaxis = list(title = "",
                          rangeselector = list(
                            buttons = list(
                              list(
                                count = 3,
                                label = "3 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 6,
                                label = "6 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 1,
                                label = "1 ano",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 3,
                                label = "3 anos",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 5,
                                label = "5 anos",
                                step = "year",
                                stepmode = "backward"),
                              
                              list(step = "all"))),
                          
                          rangeslider = list(type = "date")),
             
             yaxis = list(side = 'left', title = '% a.a - Acumulada 12 meses', showgrid = FALSE, zeroline = FALSE, ticksuffix = '%', hoverformat = '.2f')) %>%
      config(displayModeBar = FALSE) %>%
      layout(hovermode = 'compare')
    
    
  }) 
  
  #Grafico - Custo Medio Estoque DPFe
  
  output$DPFe <- renderPlotly({
    plot_ly(custo_medio_estoque, x = ~Periodo) %>%
      add_lines(y = ~DPFe, name = "DPFe", line = list(color = '#008000'
      )) %>%
      add_lines(y = ~Global_USD, name = "Global USD", visible = "legendonly", line = list(color = '	#20B2AA'
      )) %>%
      add_lines(y = ~Euro, name = "Euro", visible = "legendonly", line = list(color = '	#9ACD32'
      )) %>%
      add_lines(y = ~Global_BRL, name = "Global BRL", visible = "legendonly", line = list(color = '	#6A5ACD'
      )) %>%
      add_lines(y = ~Reestruturada, name = "Reestruturada", visible = "legendonly", line = list(color = '	#9370DB'
      )) %>%
      add_lines(y = ~Demais_DPFe, name = "Demais", visible = "legendonly", line = list(color = '	#00BFFF'
      )) %>%
      add_lines(y = ~Organismos_Multilaterias, name = "Organismos Multilaterais", visible = "legendonly", line = list(color = '	#DCDCDC'
      )) %>%
      add_lines(y = ~Credores_Privados, name = "Credores Privados/ Ag. Gov.", visible = "legendonly", line = list(color = '	#1C1C1C'
      ) ) %>%
      
      layout(margin = list(b = 20),annotations = 
               list(x = 1, y = -0.1, text = "<b>Fonte:</b> Anexo 4.2 - RMD.", 
                    showarrow = F, xref='paper', yref='paper', 
                    xanchor='right', yanchor='auto', xshift=100, yshift=-80,
                    font=list(size=12, color="#696969")),
             title = "",
             xaxis = list(title = "",
                          rangeselector = list(
                            buttons = list(
                              list(
                                count = 3,
                                label = "3 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 6,
                                label = "6 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 1,
                                label = "1 ano",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 3,
                                label = "3 anos",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 5,
                                label = "5 anos",
                                step = "year",
                                stepmode = "backward"),
                              
                              list(step = "all"))),
                          
                          rangeslider = list(type = "date")),
             
             yaxis = list(side = 'left', title = '% a.a - Acumulada 12 meses', showgrid = FALSE, zeroline = FALSE, ticksuffix = '%', hoverformat = '.2f')) %>%
    config(displayModeBar = FALSE) %>%
      layout(hovermode = 'compare')
    
    
    
    
  }) 
  
  
  
  #Grafico - Custo Medio Emissao DPMFi
  
  output$Emissao <- renderPlotly({
    plot_ly(custo_medio_emissao, x = ~Periodo) %>%
      add_lines(y = ~DPMFi, name = "DPMFi", line = list(color = '#FFA500'
      )) %>%
      add_lines(y = ~LFT, name = "LFT", visible = "legendonly", line = list(color = '#FF4500'
      )) %>%
      add_lines(y = ~LTN, name = "LTN", visible = "legendonly", line = list(color = '#B22222'
      )) %>%
      add_lines(y = ~NTN_B, name = "NTN-B", visible = "legendonly", line = list(color = '#2F4F4F'
      ) ) %>%
      add_lines(y = ~NTN_F, name = "NTN-F", visible = "legendonly", line = list(color = '#FA8072'
      )) %>%
      
      layout(margin = list(b = 20),annotations = 
               list(x = 1, y = -0.1, text = "<b>Fonte:</b> Anexo 4.3 - RMD.", 
                    showarrow = F, xref='paper', yref='paper', 
                    xanchor='right', yanchor='auto', xshift=100, yshift=-80,
                    font=list(size=12, color="#696969")),
             title = "",
             xaxis = list(title = "",
                          
                          rangeselector = list(
                            buttons = list(
                              list(
                                count = 3,
                                label = "3 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 6,
                                label = "6 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 1,
                                label = "1 ano",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 3,
                                label = "3 anos",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 5,
                                label = "5 anos",
                                step = "year",
                                stepmode = "backward"),
                              
                              list(step = "all"))),
                          
                          rangeslider = list(type = "date")
             ),
             
             yaxis = list(side = 'left', title = '% a.a - Acumulada 12 meses', showgrid = FALSE, zeroline = FALSE, ticksuffix = '%', hoverformat = '.2f')) %>%
      config(displayModeBar = FALSE) %>%
      layout(hovermode = 'compare')
    
    
  }) 
  
# TESOURO DIRETO - ESTOQUE, EMISSÂO E RESGATE
  
  #Grafico - Estoque - Indicadores
  
  output$Tesouro_Estoque <- renderPlotly({
    
    plot_ly(estoque_tesouro, x = ~Periodo) %>% 
      add_trace(y = ~Selic, type = 'bar', name = 'Selic', marker = list(color = 'Orange')) %>%
      add_trace(y = ~Prefixados,  type = "bar",name = 'Prefixados', marker = list(color = '#4682B4')) %>%
      add_trace(y = ~Indexados_Inflacao, type = "bar",name = 'Indexados Inflação', marker = list(color = '#ADD8E6')) %>%
      add_lines(y=~Estoque, name = "Estoque") %>%
      layout(margin = list(b = 20),annotations = 
               list(x = 1, y = -0.1, text = "<b>Fonte:</b> Anexo 1.5 - RMD.", 
                    showarrow = F, xref='paper', yref='paper', 
                    xanchor='right', yanchor='auto', xshift=100, yshift=-80,
                    font=list(size=12, color="#696969")),
             title = "",
             xaxis = list(title = "",
                          rangeselector = list(
                            buttons = list(
                              list(
                                count = 3,
                                label = "3 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 6,
                                label = "6 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 1,
                                label = "1 ano",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 3,
                                label = "3 anos",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 5,
                                label = "5 anos",
                                step = "year",
                                stepmode = "backward"),
                              
                              list(step = "all"))),
                          
                          rangeslider = list(type = "date")),
             
             yaxis = list(title = "R$ - Milhões", zeroline = TRUE,
                          showline = FALSE, showgrid = FALSE, hoverformat = ',.4r'), barmode = 'stack') %>%
    config(displayModeBar = FALSE) %>%
      layout(hovermode = 'compare')
    
    
    
  })
  
  #Grafico - Emissão - Indicadores
  
  output$Tesouro_Emissao <- renderPlotly({  
    
    plot_ly(emissao_tesouro, x = ~Periodo) %>% 
      add_trace(y = ~Selic, type = 'bar', name = 'Selic', marker = list(color = 'Orange')) %>%
      add_trace(y = ~Prefixados,  type = "bar",name = 'Prefixados', marker = list(color = '#4682B4')) %>%
      add_trace(y = ~Indexados_Inflacao, type = "bar",name = 'Indexados Inflação', marker = list(color = '#ADD8E6')) %>%
      add_lines(y=~Emissoes, name = "Emissão",  line = list(color = 'rgb(49,130,189'
      )) %>%
      layout(margin = list(b = 20),annotations = 
               list(x = 1, y = -0.1, text = "<b>Fonte:</b> Anexo 1.5 - RMD.", 
                    showarrow = F, xref='paper', yref='paper', 
                    xanchor='right', yanchor='auto', xshift=100, yshift=-80,
                    font=list(size=12, color="#696969")),
             title = "",
             xaxis = list(title = "",
                          rangeselector = list(
                            buttons = list(
                              list(
                                count = 3,
                                label = "3 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 6,
                                label = "6 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 1,
                                label = "1 ano",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 3,
                                label = "3 anos",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 5,
                                label = "5 anos",
                                step = "year",
                                stepmode = "backward"),
                              
                              list(step = "all"))),
                          
                          rangeslider = list(type = "date")),
             
             yaxis = list(title = "R$ - Milhões", zeroline = TRUE, 
                          showline = FALSE, showgrid = FALSE, hoverformat = ',.4r'), barmode = 'stack') %>%
    config(displayModeBar = FALSE) %>%
      layout(hovermode = 'compare')
    
  })
  
  #Grafico - Resgates - Recompras
  
  output$Tesouro_Recompra <- renderPlotly({  
    
    plot_ly(resgates_recompra, x = ~Periodo) %>% 
      add_trace(y = ~Selic, type = 'bar', name = 'Selic', marker = list(color = 'Orange')) %>%
      add_trace(y = ~Prefixados,  type = "bar",name = 'Prefixados', marker = list(color = '#4682B4')) %>%
      add_trace(y = ~Indexados_Inflacao, type = "bar",name = 'Indexados Inflação', marker = list(color = '#ADD8E6')) %>%
      add_lines(y=~Recompras, name = "Resgates - Recompras", line = list(color = 'rgb(204,204,204)'
      )) %>%
      layout(margin = list(b = 20),annotations = 
               list(x = 1, y = -0.1, text = "<b>Fonte:</b> Anexo 1.5 - RMD.", 
                    showarrow = F, xref='paper', yref='paper', 
                    xanchor='right', yanchor='auto', xshift=100, yshift=-80,
                    font=list(size=12, color="#696969")),
             title = "",
             xaxis = list(title = "",
                          rangeselector = list(
                            buttons = list(
                              list(
                                count = 3,
                                label = "3 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 6,
                                label = "6 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 1,
                                label = "1 ano",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 3,
                                label = "3 anos",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 5,
                                label = "5 anos",
                                step = "year",
                                stepmode = "backward"),
                              
                              list(step = "all"))),
                          
                          rangeslider = list(type = "date")),
             
             yaxis = list(title = "R$ - Milhões", zeroline = TRUE,
                          showline = FALSE, showgrid = FALSE,hoverformat = ',.4r'), barmode = 'stack') %>%
    config(displayModeBar = FALSE) %>%
      layout(hovermode = 'compare')
    
    
    
  })
  
  #Grafico - Resgates - Vencimentos
  
  output$Tesouro_Vencimentos <- renderPlotly({  
    
    plot_ly(resgates_vencimento, x = ~Periodo) %>% 
      add_trace(y = ~Selic, type = 'bar', name = 'Selic', marker = list(color = 'Orange')) %>%
      add_trace(y = ~Prefixados,  type = "bar",name = 'Prefixados', marker = list(color = '#4682B4')) %>%
      add_trace(y = ~Indexados_Inflacao, type = "bar",name = 'Indexados Inflação', marker = list(color = '#ADD8E6')) %>%
      add_lines(y=~Vencimentos, name = "Resgates - Vencimentos", line = list(color = 'rgb(204,204,204)'
      )) %>%
      layout(margin = list(b = 20),annotations = 
               list(x = 1, y = -0.1, text = "<b>Fonte:</b> Anexo 1.5 - RMD.", 
                    showarrow = F, xref='paper', yref='paper', 
                    xanchor='right', yanchor='auto', xshift=100, yshift=-80,
                    font=list(size=12, color="#696969")),
             title = "",
             xaxis = list(title = "",
                          rangeselector = list(
                            buttons = list(
                              list(
                                count = 3,
                                label = "3 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 6,
                                label = "6 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 1,
                                label = "1 ano",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 3,
                                label = "3 anos",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 5,
                                label = "5 anos",
                                step = "year",
                                stepmode = "backward"),
                              
                              list(step = "all"))),
                          
                          rangeslider = list(type = "date")),
             
             yaxis = list(title = "R$ - Milhões", zeroline = TRUE,
                          showline = FALSE, showgrid = FALSE,hoverformat = ',.4r'), barmode = 'stack') %>%
    config(displayModeBar = FALSE) 
    
    
  }) 
  
  #Grafico - Emissão Líquida - Emissoes/Resgates
  
  output$Tesouro_Emissao_Liquida <- renderPlotly({
    
    plot_ly(tesouro_direto, x = ~Periodo) %>%
      add_trace(y = ~Emissoes,visible = "legendonly",type = "bar" ,name = 'Emissoes', marker = list(color = 'rgb(49,130,189')) %>%
      add_trace(y = ~Resgates,visible = "legendonly",type = "bar" ,name = 'Resgates', marker = list(color = 'rgb(204,204,204)')) %>%
      add_lines(y = ~Emissao_Liquida, name = "Emissão Líquida" ,line = list(color = '#FFD700'
      )) %>%
      
      layout(margin = list(b = 20),annotations = 
               list(x = 1, y = -0.1, text = "<b>Fonte:</b> Anexo 1.5 - RMD.", 
                    showarrow = F, xref='paper', yref='paper', 
                    xanchor='right', yanchor='auto', xshift=100, yshift=-80,
                    font=list(size=12, color="#696969")),
             title = "",
             xaxis = list(title = "",
                          rangeselector = list(
                            buttons = list(
                              list(
                                count = 3,
                                label = "3 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 6,
                                label = "6 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 1,
                                label = "1 ano",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 3,
                                label = "3 anos",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 5,
                                label = "5 anos",
                                step = "year",
                                stepmode = "backward"),
                              
                              list(step = "all"))),
                          
                          rangeslider = list(type = "date")),
             
             yaxis = list(title = "R$ - Milhões", zeroline = TRUE,
                          showline = FALSE, showgrid = FALSE , hoverformat = ',.4r')) %>%
    config(displayModeBar = FALSE) %>%
      layout(hovermode = 'compare')
    
    
  })
  
  #Grafico - Resgates - Total Recompras e Vencimentos
  
  output$Tesouro_Resgate <- renderPlotly({
    
    plot_ly(resgates_vencimento, x = ~Periodo) %>% 
      add_trace(y = ~Recompras, type = 'bar', name = 'Recompras', marker = list(color = '#A9A9A9')) %>%
      add_trace(y = ~Vencimentos,  type = "bar",name = 'Vencimentos' , marker = list(color = '	#4F4F4F')) %>%
      add_lines(y=~Resgates, name = "Resgates Totais", line = list(color = '#C0C0C0'
      )) %>%
      layout(margin = list(b = 20),annotations = 
               list(x = 1, y = -0.1, text = "<b>Fonte:</b> Anexo 1.5 - RMD.", 
                    showarrow = F, xref='paper', yref='paper', 
                    xanchor='right', yanchor='auto', xshift=100, yshift=-80,
                    font=list(size=12, color="#696969")),
             title = "",
             xaxis = list(title = "",
                          rangeselector = list(
                            buttons = list(
                              list(
                                count = 3,
                                label = "3 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 6,
                                label = "6 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 1,
                                label = "1 ano",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 3,
                                label = "3 anos",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 5,
                                label = "5 anos",
                                step = "year",
                                stepmode = "backward"),
                              
                              list(step = "all"))),
                          
                          rangeslider = list(type = "date")),
             
             yaxis = list(title = "R$ - Milhões", zeroline = TRUE,
                          showline = FALSE, showgrid = FALSE,hoverformat = ',.4r'), barmode = 'stack') %>%
    config(displayModeBar = FALSE) %>%
      layout(hovermode = 'compare')
    
    
    
    
  })
  
  #Grafico - Estoque por prazo
  
  output$Tesouro_Prazo <- renderPlotly({
    
    plot_ly(prazo_estoque, x = ~Periodo) %>% 
      add_trace(y = ~ate_1_ano, type = 'bar', name = 'Até 1 ano', marker = list(color = '#7FFFD4')) %>%
      add_trace(y = ~entre_1_5_anos,  type = "bar",name = 'Entre 1 e 5 anos', marker = list(color = '#66CDAA')) %>%
      add_trace(y = ~acima_5_anos_correto, type = "bar",name = 'Acima de 5 anos', marker = list(color = '#5F9EA0')) %>%
      add_lines(y=~Total, name = "Estoque") %>%
      layout(margin = list(b = 20),annotations = 
               list(x = 1, y = -0.1, text = "<b>Fonte:</b> Anexo 2.2 - Balanço Tesouro Direto.", 
                    showarrow = F, xref='paper', yref='paper', 
                    xanchor='right', yanchor='auto', xshift=100, yshift=-80,
                    font=list(size=12, color="#696969")),
             title = "",
             xaxis = list(title = "",
                          rangeselector = list(
                            buttons = list(
                              list(
                                count = 3,
                                label = "3 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 6,
                                label = "6 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 1,
                                label = "1 ano",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 3,
                                label = "3 anos",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 5,
                                label = "5 anos",
                                step = "year",
                                stepmode = "backward"),
                              
                              list(step = "all"))),
                          
                          rangeslider = list(type = "date")),
             
             yaxis = list(title = "R$ - Milhões", zeroline = TRUE,
                          showline = FALSE, showgrid = FALSE, hoverformat = ',.4r'), barmode = 'stack') %>%
    config(displayModeBar = FALSE) %>%
      layout(hovermode = 'compare')
    
    
  })
  
  #Grafico - Emissao por Prazo
  
  output$Recompra_Emissao <- renderPlotly({
    
    plot_ly(prazo_emissao, x = ~Periodo) %>% 
      add_trace(y = ~ate_1_ano, type = 'bar', name = 'Até 1 ano', marker = list(color = '#7FFFD4')) %>%
      add_trace(y = ~entre_1_5_anos,  type = "bar",name = 'Entre 1 e 5 anos', marker = list(color = '#66CDAA')) %>%
      add_trace(y = ~acima_5_anos_correto, type = "bar",name = 'Acima de 5 anos', marker = list(color = '#5F9EA0')) %>%
      add_lines(y=~Total, name = "Emissão",line = list(color = 'rgb(49,130,189)'
      )) %>%
      layout(margin = list( b = 20),annotations = 
               list(x = 1, y = -0.1, text = "<b>Fonte:</b> Anexo 1.3 - Balanço Tesouro Direto.", 
                    showarrow = F, xref='paper', yref='paper', 
                    xanchor='right', yanchor='auto', xshift=100, yshift=-80,
                    font=list(size=12, color="#696969")),
             title = "",
             xaxis = list(title = "",
                          rangeselector = list(
                            buttons = list(
                              list(
                                count = 3,
                                label = "3 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 6,
                                label = "6 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 1,
                                label = "1 ano",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 3,
                                label = "3 anos",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 5,
                                label = "5 anos",
                                step = "year",
                                stepmode = "backward"),
                              
                              list(step = "all"))),
                          
                          rangeslider = list(type = "date")),
             
             yaxis = list(title = "R$ - Milhões", zeroline = TRUE,
                          showline = FALSE, showgrid = FALSE, hoverformat = ',.4r'), barmode = 'stack') %>%
    config(displayModeBar = FALSE) %>%
      layout(hovermode = 'compare')
    
    
    
    
  })
  
  #Grafico - Emissão - Faixa de Inves
  
  output$Faixa_Emissao <- renderPlotly({
    
    plot_ly(faixa_emissao, x = ~Periodo) %>% 
      add_trace(y = ~ate_1, type = 'bar', name = 'Até R$ 1 mil', marker = list(color = '#FA8072')) %>%
      add_trace(y = ~entre_1_a_5,  type = "bar",name = 'Entre R$ 1 e R$ 5 mil', marker = list(color = '#D2B48C')) %>%
      add_trace(y = ~acima_5, type = "bar",name = 'Acima de R$ 5 mil', marker = list(color = '#FFDAB9')) %>%
      
      layout(margin = list( b = 20) ,annotations = 
               list(x = 1, y = -0.1, text = "<b>Fonte:</b> Anexo 1.5 - Balanço Tesouro Direto.", 
                    showarrow = F, xref='paper', yref='paper', 
                    xanchor='right', yanchor='auto', xshift=100, yshift=-80,
                    font=list(size=12, color="#696969")),
             title = "",
             xaxis = list(title = "",
                          rangeselector = list(
                            buttons = list(
                              list(
                                count = 3,
                                label = "3 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 6,
                                label = "6 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 1,
                                label = "1 ano",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 3,
                                label = "3 anos",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 5,
                                label = "5 anos",
                                step = "year",
                                stepmode = "backward"),
                              
                              list(step = "all"))),
                          
                          rangeslider = list(type = "date")),
             
             yaxis = list(title = "Operações por Faixa de Aplicação (%)", zeroline = TRUE,  ticksuffix = '%',
                          showline = FALSE, showgrid = FALSE, hoverformat = '.2f'), barmode = 'stack') %>%
    config(displayModeBar = FALSE) %>%
      layout(hovermode = 'compare')
    
    
  })
  
  #Grafico - Detentores - Por Região
  
  output$Tesouro_Investidores <- renderPlotly({
    
    plot_ly(balanco_detentores, x = ~Periodo) %>% 
      add_trace(y = ~Sudeste, type = "bar",name = 'Sudeste', marker = list(color = '#32CD32')) %>%
      add_trace(y = ~Nordeste, type = 'bar', name = 'Nordeste', marker = list(color = '#556B2F')) %>%
      add_trace(y = ~Sul, type = "bar",name = 'Sul', marker = list(color = '6B8E23')) %>%
      add_trace(y = ~Centro_Oeste, type = "bar",name = 'Centro Oeste', marker = list(color = '	#808000')) %>%
      add_trace(y = ~Norte,  type = "bar",name = 'Norte', marker = list(color = '#9ACD32')) %>%
      
      layout(margin = list( b = 20),annotations = 
               list(x = 1, y = -0.1, text = "<b>Fonte:</b> Anexo 3.1 - Balanço Tesouro Direto.", 
                    showarrow = F, xref='paper', yref='paper', 
                    xanchor='right', yanchor='auto', xshift=100, yshift=-80,
                    font=list(size=12, color="#696969")),
             title = "",
             xaxis = list(title = "",
                          rangeselector = list(
                            buttons = list(
                              list(
                                count = 3,
                                label = "3 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 6,
                                label = "6 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 1,
                                label = "1 ano",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 3,
                                label = "3 anos",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 5,
                                label = "5 anos",
                                step = "year",
                                stepmode = "backward"),
                              
                              list(step = "all"))),
                          
                          rangeslider = list(type = "date")),
             
             yaxis = list(title = "% Por Região", zeroline = TRUE,  ticksuffix = '%',
                          showline = FALSE, showgrid = FALSE, hoverformat = '.2f'), barmode = 'stack') %>%
    config(displayModeBar = FALSE) %>%
      layout(hovermode = 'compare')
    
  })
  
  #Grafico - Detentores - Genero
  
  output$Genero_Investidores <- renderPlotly({
    
    plot_ly(balanco_detentores, x = ~Periodo, y = ~Homens_Mulheres, name = 'Homens',fill = 'tozeroy' ,type = 'scatter', mode = 'none', stackgroup = 'one', groupnorm = 'percent', fillcolor = '#87CEFA',
            hoverinfo = "text",
            text = ~paste(round(Homens, 2), '% ', as.Date(Periodo,"%b/%Y"))) %>%
      
      add_trace(y = ~Mulheres, fill = 'tozeroy',mode ='none',groupnorm = 'percent',stackgroup = 'one',name = 'Mulheres', fillcolor = '#CD5C5C',
                hoverinfo = "text",
                text = ~paste(round(Mulheres, 2), '% ', as.Date(Periodo, "%b/%Y"))) %>%
      
      layout(margin = list(b=20),annotations = 
               list(x = 1, y = -0.1, text = "<b>Fonte:</b> Anexo 3.1 - Balanço Tesouro Direto.", 
                    showarrow = F, xref='paper', yref='paper', 
                    xanchor='right', yanchor='auto', xshift=100, yshift=-5,
                    font=list(size=12, color="#696969")),title = '',
             xaxis = list(title = "",
                          showgrid = FALSE),
             yaxis = list(title = "% Por Gênero",
                          showgrid = FALSE,
                          ticksuffix = '%', hoverformat = '.2f')) %>%
    config(displayModeBar = FALSE) %>%
    layout(hovermode = 'compare')
    
  }) 
  
  #Grafico - Detentores - Cadastrados e Ativos
  
  output$Tesouro_Investidoresa <- renderPlotly({
    
    plot_ly(balanco_detentores) %>%
      add_trace(x = ~Periodo, y = ~Total, type = 'bar', name = 'Cadastrados',
                marker = list(color = '#FFB6C1')
                ) %>%        
      add_trace(x = ~Periodo, y = ~Total_Investidores_Ativo, type = 'bar', name = 'Ativos',
                marker = list(color = '#CD5C5C')
                ) %>%
      add_trace(x = ~Periodo, y = ~Percentual_investidores, visible = "legendonly",type = 'scatter', mode = 'lines', name = '% Ativo / Cadastrados', yaxis = 'y2',
                line = list(width = 1,color = '#CD5C5C'))%>%
      layout(margin = list(b = 20),annotations = 
               list(x = 1, y = -0.1, text = "<b>Fonte:</b> Anexo 3.1 - Balanço Tesouro Direto.", 
                    showarrow = F, xref='paper', yref='paper', 
                    xanchor='right', yanchor='auto', xshift=100, yshift=-80,
                    font=list(size=12, color="#696969")),title = '',
             xaxis = list(title = "", 
                          rangeselector = list(
                            buttons = list(
                              list(
                                count = 3,
                                label = "3 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 6,
                                label = "6 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 1,
                                label = "1 ano",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 3,
                                label = "3 anos",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 5,
                                label = "5 anos",
                                step = "year",
                                stepmode = "backward"),
                              
                              list(step = "all"))),
                          
                          rangeslider = list(type = "date")),
             yaxis = list(side = 'left', title = 'Milhões', showgrid = FALSE, zeroline = FALSE,hoverformat = ',.4r'),
             yaxis2 = list(side = 'right', overlaying = "y", title = '% Ativo/Cadastrados', showgrid = FALSE, zeroline = FALSE, ticksuffix = '%',hoverformat = '.2f')) %>%
    config(displayModeBar = FALSE) %>%
    layout(hovermode = 'compare')
    
  })
  
  #Evolução - Estoque - DPF
  
  output$Estoque_tipo <- renderPlotly({
    
      plot_ly(Estoque, x=~Periodo) %>%
      
      add_lines(y = ~DPF, name = "DPF", line = list(color = '#4682B4'
      )) %>%
      
      add_lines(y = ~DPMFi, name = "DPMFi", visible = "legendonly", line = list(color = '#FFA500'
      )) %>%
      
      add_lines(y = ~DPFe, name = "DPFe", visible = "legendonly", line = list(color = '#008000'
      )) %>%
      
      
      layout(margin = list(b = 20),annotations = 
               list(x = 1, y = -0.1, text = "<b>Fonte:</b> Anexo 2.1 - RMD.", 
                    showarrow = F, xref='paper', yref='paper', 
                    xanchor='right', yanchor='auto', xshift=100, yshift=-80,
                    font=list(size=12, color="#696969")),
             title = "",
             xaxis = list(title = "", 
                          rangeselector = list(
                            buttons = list(
                              list(
                                count = 3,
                                label = "3 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 6,
                                label = "6 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 1,
                                label = "1 ano",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 3,
                                label = "3 anos",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 5,
                                label = "5 anos",
                                step = "year",
                                stepmode = "backward"),
                              
                              list(step = "all"))),
                          
                          rangeslider = list(type = "date")),
             yaxis = list(side = 'left', title = 'R$ - Bilhões', showgrid = FALSE, zeroline = FALSE, hoverformat = ',.4r')) %>%
              
      config(displayModeBar = FALSE) %>%
      layout(hovermode = 'compare')
    
  }) 
  ##Evolução - Estoque -Percentual - Variando DPF, DPMFi e DPFe
  
  estoque_percentual_DPF <- plot_ly(Estoque, x = ~Periodo) %>% 
    add_trace(y = ~DPMFi_percentual, type = 'bar', name = 'DPMFi', marker = list(color = '#F0E68C')) %>%
    add_trace(y = ~DPFe_percentual,  type = "bar",name = 'DPFe', marker = list(color = '#008000')) %>%
    
    layout(margin = list( b = 20) ,annotations = 
             list(x = 1, y = -0.1, text = "<b>Fonte:</b> Anexo 2.1 - RMD.", 
                  showarrow = F, xref='paper', yref='paper', 
                  xanchor='right', yanchor='auto', xshift=100, yshift=-80,
                  font=list(size=12, color="#696969")),
           title = "",
           xaxis = list(title = "",
                        rangeselector = list(
                          buttons = list(
                            list(
                              count = 3,
                              label = "3 meses",
                              step = "month",
                              stepmode = "backward"),
                            list(
                              count = 6,
                              label = "6 meses",
                              step = "month",
                              stepmode = "backward"),
                            list(
                              count = 1,
                              label = "1 ano",
                              step = "year",
                              stepmode = "backward"),
                            list(
                              count = 3,
                              label = "3 anos",
                              step = "year",
                              stepmode = "backward"),
                            list(
                              count = 5,
                              label = "5 anos",
                              step = "year",
                              stepmode = "backward"),
                            
                            list(step = "all"))),
                        
                        rangeslider = list(type = "date")),
           
           yaxis = list(title = "Composição DPF", zeroline = TRUE,  tickformat = '%',
                        showline = FALSE, showgrid = FALSE,hoverformat = '.0%' ,hoverformat = '.2f'), barmode = 'stack') %>%
    config(displayModeBar = FALSE) %>%
    layout(hovermode = 'compare')
  
  estoque_percentual_DPMFi <- plot_ly(Estoque, x = ~Periodo) %>% 
    
    add_trace(y = ~LFT_percentual, type = 'bar', name = 'LFT', marker = list(color = 'Orange')) %>%
    add_trace(y = ~LTN_percentual,  type = "bar",name = 'LTN', marker = list(color = '#4682B4')) %>%
    add_trace(y = ~NTN_B_percentual,  type = "bar",name = 'NTN-B', marker = list(color = '#ADD8E6')) %>%
    add_trace(y = ~NTN_F_percentual,  type = "bar",name = 'NTN-F',marker = list(color = '#48D1CC')) %>%
    add_trace(y = ~NTN_C_percentual,  type = "bar",name = 'NTN-C',marker = list(color = '#D8BFD8')) %>%
    add_trace(y = ~Demais_percentual,  type = "bar",name = 'Demais',marker = list(color = '#708090')) %>%
    add_trace(y = ~NTN_D_percentual,  type = "bar",name = 'NTN-D',marker = list(color = '#EEE8AA')) %>%
    add_trace(y = ~TDA_percentual,  type = "bar",name = 'TDA',marker = list(color = '#FFB6C1')) %>%
    add_trace(y = ~Divida_Securitizada_percentual,  type = "bar",name = 'Securitizada',marker = list(color = '#DA70D6')) %>%
    
    layout(margin = list( b = 20) ,annotations = 
             list(x = 1, y = -0.1, text = "<b>Fonte:</b> Anexo 2.1 - RMD.", 
                  showarrow = F, xref='paper', yref='paper', 
                  xanchor='right', yanchor='auto', xshift=100, yshift=-80,
                  font=list(size=12, color="#696969")),
           title = "",
           xaxis = list(title = "",
                        rangeselector = list(
                          buttons = list(
                            list(
                              count = 3,
                              label = "3 meses",
                              step = "month",
                              stepmode = "backward"),
                            list(
                              count = 6,
                              label = "6 meses",
                              step = "month",
                              stepmode = "backward"),
                            list(
                              count = 1,
                              label = "1 ano",
                              step = "year",
                              stepmode = "backward"),
                            list(
                              count = 3,
                              label = "3 anos",
                              step = "year",
                              stepmode = "backward"),
                            list(
                              count = 5,
                              label = "5 anos",
                              step = "year",
                              stepmode = "backward"),
                            
                            list(step = "all"))),
                        
                        rangeslider = list(type = "date")),
           
           yaxis = list(title = "Composição DPMFi", zeroline = TRUE,  tickformat = '%',
                        showline = FALSE, showgrid = FALSE,hoverformat = '.0%' ,hoverformat = '.2f'), barmode = 'stack') %>%
    config(displayModeBar = FALSE) %>%
    layout(hovermode = 'compare')
  
  
  
  estoque_percentual_DPFe <- plot_ly(Estoque, x = ~Periodo) %>% 
    
    
    add_trace(y = ~Global_USD_percentual,  type = "bar",name = 'Global USD', marker = list(color = '#20B2AA')) %>%
    add_trace(y = ~Euro_percentual,  type = "bar",name = 'Euro', marker = list(color = '#9ACD32')) %>%
    add_trace(y = ~Global_BRL_percentual,  type = "bar",name = 'Global BRL',marker = list(color = '#6A5ACD')) %>%
    add_trace(y = ~Reestruturada_percentual,  type = "bar",name = 'Reestruturada',marker = list(color = '#D8BFD8')) %>%
    add_trace(y = ~Demais_DPFe_percentual,  type = "bar",name = 'Demais',marker = list(color = '#9370DB')) %>%
    add_trace(y = ~Organismos_Multilaterais_percentual,  type = "bar",name = 'Organismos Multilaterais',marker = list(color = '#DCDCDC')) %>%
    add_trace(y = ~Credores_Privados_percentual,  type = "bar",name = 'Credores Privados/ Ag. Gov.',marker = list(color = '#1C1C1C')) %>%
    add_trace(y = ~Clube_de_Paris_percentual,  type = "bar",name = 'Clube de Paris',marker = list(color = '#FF69B4')) %>%
    
    
    
    
    layout(margin = list( b = 20) ,annotations = 
             list(x = 1, y = -0.1, text = "<b>Fonte:</b> Anexo 2.1 - RMD.", 
                  showarrow = F, xref='paper', yref='paper', 
                  xanchor='right', yanchor='auto', xshift=100, yshift=-80,
                  font=list(size=12, color="#696969")),
           title = "",
           xaxis = list(title = "",
                        rangeselector = list(
                          buttons = list(
                            list(
                              count = 3,
                              label = "3 meses",
                              step = "month",
                              stepmode = "backward"),
                            list(
                              count = 6,
                              label = "6 meses",
                              step = "month",
                              stepmode = "backward"),
                            list(
                              count = 1,
                              label = "1 ano",
                              step = "year",
                              stepmode = "backward"),
                            list(
                              count = 3,
                              label = "3 anos",
                              step = "year",
                              stepmode = "backward"),
                            list(
                              count = 5,
                              label = "5 anos",
                              step = "year",
                              stepmode = "backward"),
                            
                            list(step = "all"))),
                        
                        rangeslider = list(type = "date")),
           
           yaxis = list(title = "Composição DPFe", zeroline = TRUE,  tickformat = '%',
                        showline = FALSE, showgrid = FALSE,hoverformat = '.0%' ,hoverformat = '.2f'), barmode = 'stack') %>%
    config(displayModeBar = FALSE) %>%
    layout(hovermode = 'compare')
  
  output$estoque_percentual <- renderPlotly({
    
    get(input$Tipo_teste)
  })
  
  
  
  #Evolução - Estoque - DPMFi
  output$Estoque_DPMFi <- renderPlotly({
    plot_ly(Estoque, x = ~Periodo) %>%
      add_lines(y = ~DPMFi, name = "DPMFi", line = list(color = '#FFA500'
      )) %>%
      add_lines(y = ~LFT, name = "LFT", visible = "legendonly", line = list(color = '#FF4500'
      )) %>%
      add_lines(y = ~LTN, name = "LTN", visible = "legendonly", line = list(color = '#B22222'
      )) %>%
      add_lines(y = ~NTN_B, name = "NTN-B", visible = "legendonly", line = list(color = '#2F4F4F'
      ) ) %>%
      add_lines(y = ~NTN_C, name = "NTN-C", visible = "legendonly", line = list(color = '#D8BFD8'
      ) ) %>%
      add_lines(y = ~NTN_D, name = "NTN-D", visible = "legendonly", line = list(color = '#EEE8AA'
      )) %>%
      add_lines(y = ~NTN_F, name = "NTN-F", visible = "legendonly", line = list(color = '#FA8072'
      )) %>%
      add_lines(y = ~TDA, name = "TDA", visible = "legendonly", line = list(color = '#FFB6C1'
      )) %>%
      add_lines(y = ~Divida_Securitizada, name = "Dívida Securitizada", visible = "legendonly", line = list(color = '#DA70D6'
      )) %>%
      add_lines(y = ~Demais, name = "Demais", visible = "legendonly", line = list(color = '#708090'
      )) %>%
      
      layout(margin = list(b = 20),annotations = 
               list(x = 1, y = -0.1, text = "<b>Fonte:</b> Anexo 2.1 - RMD.", 
                    showarrow = F, xref='paper', yref='paper', 
                    xanchor='right', yanchor='auto', xshift=100, yshift=-80,
                    font=list(size=12, color="#696969")),
             title = "",
             xaxis = list(title = "",
                          rangeselector = list(
                            buttons = list(
                              list(
                                count = 3,
                                label = "3 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 6,
                                label = "6 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 1,
                                label = "1 ano",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 3,
                                label = "3 anos",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 5,
                                label = "5 anos",
                                step = "year",
                                stepmode = "backward"),
                              
                              list(step = "all"))),
                          
                          rangeslider = list(type = "date")),
             
             yaxis = list(side = 'left', title = 'R$ - Bilhões', showgrid = FALSE, zeroline = FALSE, hoverformat = ',.4r')) %>%
      config(displayModeBar = FALSE) %>%
      layout(hovermode = 'compare')
    
    
  }) 
  
  #Evolução - Estoque - - DPFe
  
  output$Estoque_DPFe <- renderPlotly({
    plot_ly(Estoque, x = ~Periodo) %>%
      add_lines(y = ~DPFe, name = "DPFe", line = list(color = '#008000'
      )) %>%
      add_lines(y = ~Global_USD, name = "Global USD", visible = "legendonly", line = list(color = '	#20B2AA'
      )) %>%
      add_lines(y = ~Euro, name = "Euro", visible = "legendonly", line = list(color = '	#9ACD32'
      )) %>%
      add_lines(y = ~Global_BRL, name = "Global BRL", visible = "legendonly", line = list(color = '	#6A5ACD'
      )) %>%
      add_lines(y = ~Reestruturada, name = "Reestruturada", visible = "legendonly", line = list(color = '	#9370DB'
      )) %>%
      add_lines(y = ~Demais_DPFe, name = "Demais", visible = "legendonly", line = list(color = '	#00BFFF'
      )) %>%
      add_lines(y = ~Organismos_Multilaterais, name = "Organismos Multilaterais", visible = "legendonly", line = list(color = '	#DCDCDC'
      )) %>%
      add_lines(y = ~Credores_Privados, name = "Credores Privados/ Ag. Gov.", visible = "legendonly", line = list(color = '	#1C1C1C'
      ) ) %>%
      add_lines(y = ~Clube_de_Paris, name = "Clube de Paris", visible = "legendonly", line = list(color = '	#FF69B4'
      ) ) %>%
      
      layout(margin = list(b = 20),annotations = 
               list(x = 1, y = -0.1, text = "<b>Fonte:</b> Anexo 2.1 - RMD.", 
                    showarrow = F, xref='paper', yref='paper', 
                    xanchor='right', yanchor='auto', xshift=100, yshift=-80,
                    font=list(size=12, color="#696969")),
             title = "",
             xaxis = list(title = "",
                          rangeselector = list(
                            buttons = list(
                              list(
                                count = 3,
                                label = "3 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 6,
                                label = "6 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 1,
                                label = "1 ano",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 3,
                                label = "3 anos",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 5,
                                label = "5 anos",
                                step = "year",
                                stepmode = "backward"),
                              
                              list(step = "all"))),
                          
                          rangeslider = list(type = "date")),
             
             yaxis = list(side = 'left', title = 'R$ - Bilhões', showgrid = FALSE, zeroline = FALSE, hoverformat = ',.4r')) %>%
      config(displayModeBar = FALSE) %>%
      layout(hovermode = 'compare')
    
    
    
    
  }) 
     
  
  #Indexadores - Estoque - DPF
  
  output$composicao_DPF <- renderPlotly({  
    
    plot_ly(composicao_DPF, x = ~Periodo) %>% 
      add_trace(y = ~Flutuante, type = 'bar', name = 'Flutuante', marker = list(color = 'Orange')) %>%
      add_trace(y = ~Prefixado,  type = "bar",name = 'Prefixados',marker = list(color = '#4682B4')) %>%
      add_trace(y = ~Indice_Precos, type = "bar",name = 'Indexados Inflação',marker = list(color = '#ADD8E6')) %>%
      add_trace(y = ~Cambio, type = "bar",name = 'Cambial',marker = list(color = '#008000')) %>%
      add_lines(y=~Total, name = "Total - DPF", line = list(color = '#4682B4')
                #
      ) %>%
      layout(margin = list(b = 20),annotations = 
               list(x = 1, y = -0.1, text = "<b>Fonte:</b> Anexo 2.4 - RMD.", 
                    showarrow = F, xref='paper', yref='paper', 
                    xanchor='right', yanchor='auto', xshift=100, yshift=-80,
                    font=list(size=12, color="#696969")),
             title = "",
             xaxis = list(title = "",
                          rangeselector = list(
                            buttons = list(
                              list(
                                count = 3,
                                label = "3 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 6,
                                label = "6 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 1,
                                label = "1 ano",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 3,
                                label = "3 anos",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 5,
                                label = "5 anos",
                                step = "year",
                                stepmode = "backward"),
                              
                              list(step = "all"))),
                          
                          rangeslider = list(type = "date")),
             
             yaxis = list(title = "R$ - Bilhões", zeroline = TRUE,
                          showline = FALSE, showgrid = FALSE,hoverformat = ',.4r'), barmode = 'stack') %>%
      config(displayModeBar = FALSE) %>%
      layout(hovermode = 'compare')
    
  
  }) 
  
  #Indexadores - Estoque - DPMFi
  output$composicao_DPMFi <- renderPlotly({  
    
    plot_ly(composicao_DPMFi, x = ~Periodo) %>% 
      add_trace(y = ~Flutuante, type = 'bar', name = 'Flutuante', marker = list(color = 'Orange')) %>%
      add_trace(y = ~Prefixado,  type = "bar",name = 'Prefixados',marker = list(color = '#4682B4')) %>%
      add_trace(y = ~Indice_Precos, type = "bar",name = 'Indexados Inflação',marker = list(color = '#ADD8E6')) %>%
      add_trace(y = ~Cambio, type = "bar",name = 'Cambial',marker = list(color = '#008000')) %>%
      add_lines(y=~Total, name = "Total - DPMFi", line = list(color = '#FFA500')
                
      ) %>%
      layout(margin = list(b = 20),annotations = 
               list(x = 1, y = -0.1, text = "<b>Fonte:</b> Anexo 2.5 - RMD.", 
                    showarrow = F, xref='paper', yref='paper', 
                    xanchor='right', yanchor='auto', xshift=100, yshift=-80,
                    font=list(size=12, color="#696969")),
             title = "",
             xaxis = list(title = "",
                          rangeselector = list(
                            buttons = list(
                              list(
                                count = 3,
                                label = "3 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 6,
                                label = "6 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 1,
                                label = "1 ano",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 3,
                                label = "3 anos",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 5,
                                label = "5 anos",
                                step = "year",
                                stepmode = "backward"),
                              
                              list(step = "all"))),
                          
                          rangeslider = list(type = "date")),
             
             yaxis = list(title = "R$ - Bilhões", zeroline = TRUE,
                          showline = FALSE, showgrid = FALSE,hoverformat = ',.4r'), barmode = 'stack') %>%
      config(displayModeBar = FALSE) %>%
      layout(hovermode = 'compare')
    
    
  }) 
  
  #Indexadores - Estoque - DPFe
  
  output$composicao_DPFe <- renderPlotly({  
    
    plot_ly(composicao_DPFe, x = ~Periodo) %>% 
      add_trace(y = ~Dolar, type = 'bar', name = 'Dolar', marker = list(color = '#20B2AA')) %>%
      add_trace(y = ~Euro,  type = "bar",name = 'Euro',marker = list(color = '#9ACD32')) %>%
      add_trace(y = ~Real, type = "bar",name = 'Real',marker = list(color = '#6A5ACD')) %>%
      add_trace(y = ~Demais, type = "bar",name = 'Demais',marker = list(color = '#9370DB')) %>%
      add_lines(y=~Total, name = "Total - DPFe", line = list(color = '#008000')
                #
      ) %>%
      layout(margin = list(b = 20),annotations = 
               list(x = 1, y = -0.1, text = "<b>Fonte:</b> Anexo 2.5 - RMD.", 
                    showarrow = F, xref='paper', yref='paper', 
                    xanchor='right', yanchor='auto', xshift=100, yshift=-80,
                    font=list(size=12, color="#696969")),
             title = "",
             xaxis = list(title = "",
                          rangeselector = list(
                            buttons = list(
                              list(
                                count = 3,
                                label = "3 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 6,
                                label = "6 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 1,
                                label = "1 ano",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 3,
                                label = "3 anos",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 5,
                                label = "5 anos",
                                step = "year",
                                stepmode = "backward"),
                              
                              list(step = "all"))),
                          
                          rangeslider = list(type = "date")),
             
             yaxis = list(title = "R$ - Bilhões", zeroline = TRUE,
                          showline = FALSE, showgrid = FALSE,hoverformat = ',.4r'), barmode = 'stack') %>%
      config(displayModeBar = FALSE) %>%
      layout(hovermode = 'compare')
    
    
  })
  
  #Indexadores - Composição Percentual - Variando
  
  #Nomeando - Indexadores - Composição Percentual - DPF
  indexador_percentual_DPF <- plot_ly(composicao_DPF, x = ~Periodo) %>% 
    add_trace(y = ~Flutuante_percentual, type = 'bar', name = 'FLUTUANTE', marker = list(color = 'Orange')) %>%
    add_trace(y = ~Prefixado_percentual,  type = "bar",name = 'PREFIXADOS',marker = list(color = '#4682B4')) %>%
    add_trace(y = ~Indice_Precos_percentual, type = "bar",name = 'INDEXADOS INFLAÇÃO',marker = list(color = '#ADD8E6')) %>%
    add_trace(y = ~Cambio_percentual, type = "bar",name = 'CAMBIAL',marker = list(color = '#008000')) %>%
    
    layout(margin = list( b = 20) ,annotations = 
             list(x = 1, y = -0.1, text = "<b>Fonte:</b> Anexo 2.4 - RMD.", 
                  showarrow = F, xref='paper', yref='paper', 
                  xanchor='right', yanchor='auto', xshift=100, yshift=-80,
                  font=list(size=12, color="#696969")),
           title = "",
           xaxis = list(title = "",
                        rangeselector = list(
                          buttons = list(
                            list(
                              count = 3,
                              label = "3 meses",
                              step = "month",
                              stepmode = "backward"),
                            list(
                              count = 6,
                              label = "6 meses",
                              step = "month",
                              stepmode = "backward"),
                            list(
                              count = 1,
                              label = "1 ano",
                              step = "year",
                              stepmode = "backward"),
                            list(
                              count = 3,
                              label = "3 anos",
                              step = "year",
                              stepmode = "backward"),
                            list(
                              count = 5,
                              label = "5 anos",
                              step = "year",
                              stepmode = "backward"),
                            
                            list(step = "all"))),
                        
                        rangeslider = list(type = "date")),
           
           yaxis = list(title = "Indexadores DPF", zeroline = TRUE,  tickformat = '%',
                        showline = FALSE, showgrid = FALSE,hoverformat = '.0%' ,hoverformat = '.2f'), barmode = 'stack') %>%
    config(displayModeBar = FALSE) %>%
    layout(hovermode = 'compare')
  
  #Nomeando - Indexadores - Composição Percentual - DPMFi
  
  indexador_percentual_DPMFi <- plot_ly(composicao_DPMFi, x = ~Periodo) %>% 
    add_trace(y = ~Flutuante_percentual, type = 'bar', name = 'FLUTUANTE', marker = list(color = 'Orange')) %>%
    add_trace(y = ~Prefixado_percentual,  type = "bar",name = 'PREFIXADOS',marker = list(color = '#4682B4')) %>%
    add_trace(y = ~Indice_Precos_percentual, type = "bar",name = 'INDEXADOS INFLAÇÃO',marker = list(color = '#ADD8E6')) %>%
    add_trace(y = ~Cambio_percentual, type = "bar",name = 'CAMBIAL',marker = list(color = '#008000')) %>%
    
    
    layout(margin = list( b = 20) ,annotations = 
             list(x = 1, y = -0.1, text = "<b>Fonte:</b> Anexo 2.5 - RMD.", 
                  showarrow = F, xref='paper', yref='paper', 
                  xanchor='right', yanchor='auto', xshift=100, yshift=-80,
                  font=list(size=12, color="#696969")),
           title = "",
           xaxis = list(title = "",
                        rangeselector = list(
                          buttons = list(
                            list(
                              count = 3,
                              label = "3 meses",
                              step = "month",
                              stepmode = "backward"),
                            list(
                              count = 6,
                              label = "6 meses",
                              step = "month",
                              stepmode = "backward"),
                            list(
                              count = 1,
                              label = "1 ano",
                              step = "year",
                              stepmode = "backward"),
                            list(
                              count = 3,
                              label = "3 anos",
                              step = "year",
                              stepmode = "backward"),
                            list(
                              count = 5,
                              label = "5 anos",
                              step = "year",
                              stepmode = "backward"),
                            
                            list(step = "all"))),
                        
                        rangeslider = list(type = "date")),
           
           yaxis = list(title = "Indexadores DPMFi", zeroline = TRUE,  tickformat = '%',
                        showline = FALSE, showgrid = FALSE,hoverformat = '.0%' ,hoverformat = '.2f'), barmode = 'stack') %>%
    config(displayModeBar = FALSE) %>%
    layout(hovermode = 'compare')
  
  #Nomeando - Indexadores - Composição Percentual - DPFe
  
  indexador_percentual_DPFe <- plot_ly(composicao_DPFe, x = ~Periodo) %>% 
    
    add_trace(y = ~Dolar_percentual, type = 'bar', name = 'DOLAR', marker = list(color = '#20B2AA')) %>%
    add_trace(y = ~Euro_percentual,  type = "bar",name = 'EURO',marker = list(color = '#9ACD32')) %>%
    add_trace(y = ~Real_percentual, type = "bar",name = 'REAL',marker = list(color = '#6A5ACD')) %>%
    add_trace(y = ~Demais_percentual, type = "bar",name = 'DEMAIS',marker = list(color = '#9370DB')) %>%
    
    
    layout(margin = list( b = 20) ,annotations = 
             list(x = 1, y = -0.1, text = "<b>Fonte:</b> Anexo 2.6 - RMD.", 
                  showarrow = F, xref='paper', yref='paper', 
                  xanchor='right', yanchor='auto', xshift=100, yshift=-80,
                  font=list(size=12, color="#696969")),
           title = "",
           xaxis = list(title = "",
                        rangeselector = list(
                          buttons = list(
                            list(
                              count = 3,
                              label = "3 meses",
                              step = "month",
                              stepmode = "backward"),
                            list(
                              count = 6,
                              label = "6 meses",
                              step = "month",
                              stepmode = "backward"),
                            list(
                              count = 1,
                              label = "1 ano",
                              step = "year",
                              stepmode = "backward"),
                            list(
                              count = 3,
                              label = "3 anos",
                              step = "year",
                              stepmode = "backward"),
                            list(
                              count = 5,
                              label = "5 anos",
                              step = "year",
                              stepmode = "backward"),
                            
                            list(step = "all"))),
                        
                        rangeslider = list(type = "date")),
           
           yaxis = list(title = "Indexadores DPFe", zeroline = TRUE,  tickformat = '%',
                        showline = FALSE, showgrid = FALSE,hoverformat = '.0%' ,hoverformat = '.2f'), barmode = 'stack') %>%
    config(displayModeBar = FALSE) %>%
    layout(hovermode = 'compare')
  
  output$indexador_percentual <- renderPlotly({
    
    get(input$Tipo)
  })
  
  
  #Detentores Estoque Nominal DPMFi
   
  output$Detentores_DPMFi <- renderPlotly({  
    
    plot_ly(DPMFi, x = ~Periodo) %>% 
      add_trace(y = ~Inst_Financeira, type = 'bar', name = 'Instituição Financeira',marker = list(color = '#994d00') ) %>%
      add_trace(y = ~Fundos,  type = "bar",name = 'Fundos de Investimentos',marker = list(color = '#cc6600')) %>%
      add_trace(y = ~Previdencia, type = "bar",name = 'Previdencia', marker = list(color = '#ff8000')) %>%
      add_trace(y = ~Nao_Residente, type = "bar",name = 'Não-Residentes', marker = list(color = '#ffa64d')) %>%
      add_trace(y = ~Governo, type = "bar",name = 'Governo', marker = list(color = '#ffbf80')) %>%
      add_trace(y = ~Seguradoras, type = "bar",name = 'Seguradoras', marker = list(color = '#ffd9b3')) %>%
      add_trace(y = ~Outros, type = "bar",name = 'Outros',  marker = list(color = '#ffd9b3')) %>%
      add_lines(y=~Total, name = "Total - DPMFi", line = list(color = '#FFA500')
                
      ) %>%
      layout(margin = list(b = 20),annotations = 
               list(x = 1, y = -0.1, text = "<b>Fonte:</b> Anexo 2.7 - RMD.", 
                    showarrow = F, xref='paper', yref='paper', 
                    xanchor='right', yanchor='auto', xshift=100, yshift=-80,
                    font=list(size=12, color="#696969")),
             title = "",
             xaxis = list(title = "",
                          rangeselector = list(
                            buttons = list(
                              list(
                                count = 3,
                                label = "3 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 6,
                                label = "6 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 1,
                                label = "1 ano",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 3,
                                label = "3 anos",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 5,
                                label = "5 anos",
                                step = "year",
                                stepmode = "backward"),
                              
                              list(step = "all"))),
                          
                          rangeslider = list(type = "date")),
             
             yaxis = list(title = "R$ - Bilhões", zeroline = TRUE,
                          showline = FALSE, showgrid = FALSE,hoverformat = ',.4r'), barmode = 'stack') %>%
      config(displayModeBar = FALSE) %>%
      layout(hovermode = 'compare')
    
    
  })
  
  #Percentual Detentores - Variando por Título
  
  output$detentor_percentual <- renderPlotly({
    
    plot_ly(get(input$Titulo), x = ~Periodo) %>% 
      
      add_trace(y = ~Inst_Financeira_percentual, type = 'bar', name = 'Instituição Financeira',marker = list(color = '#994d00') ) %>%
      add_trace(y = ~Fundos_percentual,  type = "bar",name = 'Fundos de Investimentos',marker = list(color = '#cc6600')) %>%
      add_trace(y = ~Previdencia_percentual, type = "bar",name = 'Previdencia', marker = list(color = '#ff8000')) %>%
      add_trace(y = ~Nao_Residente_percentual, type = "bar",name = 'Não-Residentes', marker = list(color = '#ffa64d')) %>%
      add_trace(y = ~Governo_percentual, type = "bar",name = 'Governo', marker = list(color = '#ffbf80')) %>%
      add_trace(y = ~Seguradoras_percentual, type = "bar",name = 'Seguradoras', marker = list(color = '#ffd9b3')) %>%
      add_trace(y = ~Outros_percentual, type = "bar",name = 'Outros',  marker = list(color = '#ffd9b3')) %>%
               
      layout(margin = list( b = 20) ,annotations = 
               list(x = 1, y = -0.1, text = "<b>Fonte:</b> Anexo 2.7 - RMD.", 
                    showarrow = F, xref='paper', yref='paper', 
                    xanchor='right', yanchor='auto', xshift=100, yshift=-80,
                    font=list(size=12, color="#696969")),
             title = "",
             xaxis = list(title = "",
                          rangeselector = list(
                            buttons = list(
                              list(
                                count = 3,
                                label = "3 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 6,
                                label = "6 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 1,
                                label = "1 ano",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 3,
                                label = "3 anos",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 5,
                                label = "5 anos",
                                step = "year",
                                stepmode = "backward"),
                              
                              list(step = "all"))),
                          
                          rangeslider = list(type = "date")),
             
             yaxis = list(title = input$Titulo, zeroline = TRUE,  tickformat = '%',
                          showline = FALSE, showgrid = FALSE,hoverformat = '.0%' ,hoverformat = '.2f'), barmode = 'stack') %>%
      config(displayModeBar = FALSE) %>%
      layout(hovermode = 'compare')
    
    
  })   
  
  #Percentual Detentores - Variando por dententor
  
  
  output$carteira_detentor <- renderPlotly({
    
    plot_ly(get(input$Detentores), x = ~Periodo) %>% 
      add_trace(y = ~LFT_percentual, type = 'bar', name = 'LFT', marker = list(color = 'Orange')) %>%
      add_trace(y = ~LTN_percentual,  type = "bar",name = 'LTN',marker = list(color = '#4682B4')) %>%
      add_trace(y = ~NTN_B_percentual, type = "bar",name = 'NTN-B',marker = list(color = '#ADD8E6')) %>%
      add_trace(y = ~NTN_F_percentual, type = "bar",name = 'NTN-F',marker = list(color = '#48D1CC')) %>%
      add_trace(y = ~Outros_percentual, type = "bar",name = 'Outros',marker = list(color = '#AFEEEE')) %>%
      
      
       layout(margin = list( b = 20) ,annotations = 
               list(x = 1, y = -0.1, text = "<b>Fonte:</b> Anexo 2.8 - RMD.", 
                    showarrow = F, xref='paper', yref='paper', 
                    xanchor='right', yanchor='auto', xshift=100, yshift=-80,
                    font=list(size=12, color="#696969")),
             title = "",
             xaxis = list(title = "",
                          rangeselector = list(
                            buttons = list(
                              list(
                                count = 3,
                                label = "3 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 6,
                                label = "6 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 1,
                                label = "1 ano",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 3,
                                label = "3 anos",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 5,
                                label = "5 anos",
                                step = "year",
                                stepmode = "backward"),
                              
                              list(step = "all"))),
                          
                          rangeslider = list(type = "date")),
             
             yaxis = list(title = (input$Detentores), zeroline = TRUE,  tickformat = '%',
                          showline = FALSE, showgrid = FALSE,hoverformat = '.0%' ,hoverformat = '.2f'), barmode = 'stack') %>%
      config(displayModeBar = FALSE) %>%
      layout(hovermode = 'compare')
    
    
  })   
  
  # PERFIL DE VENCIMENTOS -
  
  #Grafico - Composição Estoque Vencimentos - DPF
  
  output$Vencimentos_DPF <- renderPlotly({
    
    plot_ly(DPF_vencimentos, x = ~Periodo) %>% 
      add_trace(y = ~Ate_12_Meses, type = 'bar', name = 'Até 12 meses', marker = list(color = '#7FFFD4')) %>%
      add_trace(y = ~De_1_2_anos,  type = "bar",name = 'Entre 1 e 2 anos', marker = list(color = '#66CDAA')) %>%
      add_trace(y = ~De_2_5_anos, type = "bar",name = 'Entre 2 e 5 anos', marker = list(color = '  #329a77')) %>%
      add_trace(y = ~Acima_5_anos, type = "bar",name = 'Acima de 5 anos', marker = list(color = '#5F9EA0')) %>%
      add_lines(y=~Total, name = "Estoque - DPF", line = list(color = 'Red')) %>%
      layout(margin = list(b = 20),annotations = 
               list(x = 1, y = -0.1, text = "<b>Fonte:</b> Anexo 3.1 - RMD.", 
                    showarrow = F, xref='paper', yref='paper', 
                    xanchor='right', yanchor='auto', xshift=100, yshift=-80,
                    font=list(size=12, color="#696969")),
             title = "",
             xaxis = list(title = "",
                          rangeselector = list(
                            buttons = list(
                              list(
                                count = 3,
                                label = "3 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 6,
                                label = "6 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 1,
                                label = "1 ano",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 3,
                                label = "3 anos",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 5,
                                label = "5 anos",
                                step = "year",
                                stepmode = "backward"),
                              
                              list(step = "all"))),
                          
                          rangeslider = list(type = "date")),
             
             yaxis = list(title = "R$ - Milhões", zeroline = TRUE,
                          showline = FALSE, showgrid = FALSE, hoverformat = ',.4r'), barmode = 'stack') %>%
      config(displayModeBar = FALSE) %>%
      layout(hovermode = 'compare')
    
    
  })
  
  #Grafico - Composição Estoque Vencimentos - DPMFi
  
  output$Vencimentos_DPMFi <- renderPlotly({
    
    plot_ly(DPMFi_vencimentos, x = ~Periodo) %>% 
      add_trace(y = ~Ate_12_Meses, type = 'bar', name = 'Até 12 meses', marker = list(color = '#7FFFD4')) %>%
      add_trace(y = ~De_1_2_anos,  type = "bar",name = 'Entre 1 e 2 anos', marker = list(color = '#66CDAA')) %>%
      add_trace(y = ~De_2_5_anos, type = "bar",name = 'Entre 2 e 5 anos', marker = list(color = '  #329a77')) %>%
      add_trace(y = ~Acima_5_anos, type = "bar",name = 'Acima de 5 anos', marker = list(color = '#5F9EA0')) %>%
      add_lines(y=~Total, name = "Estoque - DPMFi", line = list(color = 'Red')) %>%
      layout(margin = list(b = 20),annotations = 
               list(x = 1, y = -0.1, text = "<b>Fonte:</b> Anexo 3.1 - RMD.", 
                    showarrow = F, xref='paper', yref='paper', 
                    xanchor='right', yanchor='auto', xshift=100, yshift=-80,
                    font=list(size=12, color="#696969")),
             title = "",
             xaxis = list(title = "",
                          rangeselector = list(
                            buttons = list(
                              list(
                                count = 3,
                                label = "3 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 6,
                                label = "6 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 1,
                                label = "1 ano",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 3,
                                label = "3 anos",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 5,
                                label = "5 anos",
                                step = "year",
                                stepmode = "backward"),
                              
                              list(step = "all"))),
                          
                          rangeslider = list(type = "date")),
             
             yaxis = list(title = "R$ - Milhões", zeroline = TRUE,
                          showline = FALSE, showgrid = FALSE, hoverformat = ',.4r'), barmode = 'stack') %>%
      config(displayModeBar = FALSE) %>%
      layout(hovermode = 'compare')
    
    
  })
  
  #Grafico - Composição Estoque Vencimentos - DPMFi
  
  output$Vencimentos_DPFe <- renderPlotly({
    
    plot_ly(DPFe_vencimentos, x = ~Periodo) %>% 
      add_trace(y = ~Ate_12_Meses, type = 'bar', name = 'Até 12 meses', marker = list(color = '#7FFFD4')) %>%
      add_trace(y = ~De_1_2_anos,  type = "bar",name = 'Entre 1 e 2 anos', marker = list(color = '#66CDAA')) %>%
      add_trace(y = ~De_2_5_anos, type = "bar",name = 'Entre 2 e 5 anos', marker = list(color = '  #329a77')) %>%
      add_trace(y = ~Acima_5_anos, type = "bar",name = 'Acima de 5 anos', marker = list(color = '#5F9EA0')) %>%
      add_lines(y=~Total, name = "Estoque - DPFe", line = list(color = 'Red')) %>%
      layout(margin = list(b = 20),annotations = 
               list(x = 1, y = -0.1, text = "<b>Fonte:</b> Anexo 3.1 - RMD.", 
                    showarrow = F, xref='paper', yref='paper', 
                    xanchor='right', yanchor='auto', xshift=100, yshift=-80,
                    font=list(size=12, color="#696969")),
             title = "",
             xaxis = list(title = "",
                          rangeselector = list(
                            buttons = list(
                              list(
                                count = 3,
                                label = "3 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 6,
                                label = "6 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 1,
                                label = "1 ano",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 3,
                                label = "3 anos",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 5,
                                label = "5 anos",
                                step = "year",
                                stepmode = "backward"),
                              
                              list(step = "all"))),
                          
                          rangeslider = list(type = "date")),
             
             yaxis = list(title = "R$ - Milhões", zeroline = TRUE,
                          showline = FALSE, showgrid = FALSE, hoverformat = ',.4r'), barmode = 'stack') %>%
      config(displayModeBar = FALSE) %>%
      layout(hovermode = 'compare')
    
    
  })
  
  
  # GRAFICO - Composição Percentual dos Vencimentos - DPF, DPMFi e DPFe
  
    Vencimentos_percentual_DPF <- plot_ly(DPF_vencimentos, x = ~Periodo) %>% 
      add_trace(y = ~Ate_12_Meses_percentual, type = 'bar', name = 'Até 12 meses', marker = list(color = '#7FFFD4')) %>%
      add_trace(y = ~De_1_2_anos_percentual,  type = "bar",name = 'Entre 1 e 2 anos', marker = list(color = '#66CDAA')) %>%
      add_trace(y = ~De_2_5_anos_percentual, type = "bar",name = 'Entre 2 e 5 anos', marker = list(color = '  #329a77')) %>%
      add_trace(y = ~Acima_5_anos_percentual, type = "bar",name = 'Acima de 5 anos', marker = list(color = '#5F9EA0')) %>%
      
    
    layout(margin = list( b = 20) ,annotations = 
             list(x = 1, y = -0.1, text = "<b>Fonte:</b> Anexo 3.1 - RMD.", 
                  showarrow = F, xref='paper', yref='paper', 
                  xanchor='right', yanchor='auto', xshift=100, yshift=-80,
                  font=list(size=12, color="#696969")),
           title = "",
           xaxis = list(title = "",
                        rangeselector = list(
                          buttons = list(
                            list(
                              count = 3,
                              label = "3 meses",
                              step = "month",
                              stepmode = "backward"),
                            list(
                              count = 6,
                              label = "6 meses",
                              step = "month",
                              stepmode = "backward"),
                            list(
                              count = 1,
                              label = "1 ano",
                              step = "year",
                              stepmode = "backward"),
                            list(
                              count = 3,
                              label = "3 anos",
                              step = "year",
                              stepmode = "backward"),
                            list(
                              count = 5,
                              label = "5 anos",
                              step = "year",
                              stepmode = "backward"),
                            
                            list(step = "all"))),
                        
                        rangeslider = list(type = "date")),
           
           yaxis = list(title = "Vencimentos DPF", zeroline = TRUE,  tickformat = '%',
                        showline = FALSE, showgrid = FALSE,hoverformat = '.0%' ,hoverformat = '.2f'), barmode = 'stack') %>%
    config(displayModeBar = FALSE) %>%
    layout(hovermode = 'compare')
  
    Vencimentos_percentual_DPMFi <- plot_ly(DPMFi_vencimentos, x = ~Periodo) %>% 
      add_trace(y = ~Ate_12_Meses_percentual, type = 'bar', name = 'Até 12 meses', marker = list(color = '#7FFFD4')) %>%
      add_trace(y = ~De_1_2_anos_percentual,  type = "bar",name = 'Entre 1 e 2 anos', marker = list(color = '#66CDAA')) %>%
      add_trace(y = ~De_2_5_anos_percentual, type = "bar",name = 'Entre 2 e 5 anos', marker = list(color = '  #329a77')) %>%
      add_trace(y = ~Acima_5_anos_percentual, type = "bar",name = 'Acima de 5 anos', marker = list(color = '#5F9EA0')) %>%
      
      
      layout(margin = list( b = 20) ,annotations = 
               list(x = 1, y = -0.1, text = "<b>Fonte:</b> Anexo 3.1 - RMD.", 
                    showarrow = F, xref='paper', yref='paper', 
                    xanchor='right', yanchor='auto', xshift=100, yshift=-80,
                    font=list(size=12, color="#696969")),
             title = "",
             xaxis = list(title = "",
                          rangeselector = list(
                            buttons = list(
                              list(
                                count = 3,
                                label = "3 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 6,
                                label = "6 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 1,
                                label = "1 ano",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 3,
                                label = "3 anos",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 5,
                                label = "5 anos",
                                step = "year",
                                stepmode = "backward"),
                              
                              list(step = "all"))),
                          
                          rangeslider = list(type = "date")),
             
             yaxis = list(title = "Vencimentos DPMFi", zeroline = TRUE,  tickformat = '%',
                          showline = FALSE, showgrid = FALSE,hoverformat = '.0%' ,hoverformat = '.2f'), barmode = 'stack') %>%
      config(displayModeBar = FALSE) %>%
      layout(hovermode = 'compare')
    
    Vencimentos_percentual_DPFe <- plot_ly(DPFe_vencimentos, x = ~Periodo) %>% 
      add_trace(y = ~Ate_12_Meses_percentual, type = 'bar', name = 'Até 12 meses', marker = list(color = '#7FFFD4')) %>%
      add_trace(y = ~De_1_2_anos_percentual,  type = "bar",name = 'Entre 1 e 2 anos', marker = list(color = '#66CDAA')) %>%
      add_trace(y = ~De_2_5_anos_percentual, type = "bar",name = 'Entre 2 e 5 anos', marker = list(color = '  #329a77')) %>%
      add_trace(y = ~Acima_5_anos_percentual, type = "bar",name = 'Acima de 5 anos', marker = list(color = '#5F9EA0')) %>%
      
      
      layout(margin = list( b = 20) ,annotations = 
               list(x = 1, y = -0.1, text = "<b>Fonte:</b> Anexo 3.1 - RMD.", 
                    showarrow = F, xref='paper', yref='paper', 
                    xanchor='right', yanchor='auto', xshift=100, yshift=-80,
                    font=list(size=12, color="#696969")),
             title = "",
             xaxis = list(title = "",
                          rangeselector = list(
                            buttons = list(
                              list(
                                count = 3,
                                label = "3 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 6,
                                label = "6 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 1,
                                label = "1 ano",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 3,
                                label = "3 anos",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 5,
                                label = "5 anos",
                                step = "year",
                                stepmode = "backward"),
                              
                              list(step = "all"))),
                          
                          rangeslider = list(type = "date")),
             
             yaxis = list(title = "Vencimentos DPFe", zeroline = TRUE,  tickformat = '%',
                          showline = FALSE, showgrid = FALSE,hoverformat = '.0%' ,hoverformat = '.2f'), barmode = 'stack') %>%
      config(displayModeBar = FALSE) %>%
      layout(hovermode = 'compare')
    
    
    output$Vencimentos_Percentual <- renderPlotly({ 
      
      get(input$Vencimentos)

      }) 
    
    # GRAFICO - Composição Percentual dos Vencimentos - DPF, DPMFi e DPFe
    
    Indexador_Vencimentos_percentual_Prefixados <- plot_ly(Prefixados_vencimentos, x = ~Periodo) %>% 
      add_trace(y = ~Ate_12_Meses_percentual, type = 'bar', name = 'Até 12 meses', marker = list(color = '#7FFFD4')) %>%
      add_trace(y = ~De_1_2_anos_percentual,  type = "bar",name = 'Entre 1 e 2 anos', marker = list(color = '#66CDAA')) %>%
      add_trace(y = ~De_2_5_anos_percentual, type = "bar",name = 'Entre 2 e 5 anos', marker = list(color = '  #329a77')) %>%
      add_trace(y = ~Acima_5_anos_percentual, type = "bar",name = 'Acima de 5 anos', marker = list(color = '#5F9EA0')) %>%
      
      
      layout(margin = list( b = 20) ,annotations = 
               list(x = 1, y = -0.1, text = "<b>Fonte:</b> Anexo 3.2 - RMD.", 
                    showarrow = F, xref='paper', yref='paper', 
                    xanchor='right', yanchor='auto', xshift=100, yshift=-80,
                    font=list(size=12, color="#696969")),
             title = "",
             xaxis = list(title = "",
                          rangeselector = list(
                            buttons = list(
                              list(
                                count = 3,
                                label = "3 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 6,
                                label = "6 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 1,
                                label = "1 ano",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 3,
                                label = "3 anos",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 5,
                                label = "5 anos",
                                step = "year",
                                stepmode = "backward"),
                              
                              list(step = "all"))),
                          
                          rangeslider = list(type = "date")),
             
             yaxis = list(title = "Prefixados", zeroline = TRUE,  tickformat = '%',
                          showline = FALSE, showgrid = FALSE,hoverformat = '.0%' ,hoverformat = '.2f'), barmode = 'stack') %>%
      config(displayModeBar = FALSE) %>%
      layout(hovermode = 'compare')
    
    Indexador_Vencimentos_percentual_Flutuante <- plot_ly(Taxa_Flutuante_vencimentos, x = ~Periodo) %>% 
      add_trace(y = ~Ate_12_Meses_percentual, type = 'bar', name = 'Até 12 meses', marker = list(color = '#7FFFD4')) %>%
      add_trace(y = ~De_1_2_anos_percentual,  type = "bar",name = 'Entre 1 e 2 anos', marker = list(color = '#66CDAA')) %>%
      add_trace(y = ~De_2_5_anos_percentual, type = "bar",name = 'Entre 2 e 5 anos', marker = list(color = '  #329a77')) %>%
      add_trace(y = ~Acima_5_anos_percentual, type = "bar",name = 'Acima de 5 anos', marker = list(color = '#5F9EA0')) %>%
      
      
      layout(margin = list( b = 20) ,annotations = 
               list(x = 1, y = -0.1, text = "<b>Fonte:</b> Anexo 3.2 - RMD.", 
                    showarrow = F, xref='paper', yref='paper', 
                    xanchor='right', yanchor='auto', xshift=100, yshift=-80,
                    font=list(size=12, color="#696969")),
             title = "",
             xaxis = list(title = "",
                          rangeselector = list(
                            buttons = list(
                              list(
                                count = 3,
                                label = "3 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 6,
                                label = "6 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 1,
                                label = "1 ano",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 3,
                                label = "3 anos",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 5,
                                label = "5 anos",
                                step = "year",
                                stepmode = "backward"),
                              
                              list(step = "all"))),
                          
                          rangeslider = list(type = "date")),
             
             yaxis = list(title = "Taxa Flutuante", zeroline = TRUE,  tickformat = '%',
                          showline = FALSE, showgrid = FALSE,hoverformat = '.0%' ,hoverformat = '.2f'), barmode = 'stack') %>%
      config(displayModeBar = FALSE) %>%
      layout(hovermode = 'compare')
    
    Indexador_Vencimentos_percentual_Preco <- plot_ly(Precos_vencimentos, x = ~Periodo) %>% 
      add_trace(y = ~Ate_12_Meses_percentual, type = 'bar', name = 'Até 12 meses', marker = list(color = '#7FFFD4')) %>%
      add_trace(y = ~De_1_2_anos_percentual,  type = "bar",name = 'Entre 1 e 2 anos', marker = list(color = '#66CDAA')) %>%
      add_trace(y = ~De_2_5_anos_percentual, type = "bar",name = 'Entre 2 e 5 anos', marker = list(color = '  #329a77')) %>%
      add_trace(y = ~Acima_5_anos_percentual, type = "bar",name = 'Acima de 5 anos', marker = list(color = '#5F9EA0')) %>%
      
      
      layout(margin = list( b = 20) ,annotations = 
               list(x = 1, y = -0.1, text = "<b>Fonte:</b> Anexo 3.2 - RMD.", 
                    showarrow = F, xref='paper', yref='paper', 
                    xanchor='right', yanchor='auto', xshift=100, yshift=-80,
                    font=list(size=12, color="#696969")),
             title = "",
             xaxis = list(title = "",
                          rangeselector = list(
                            buttons = list(
                              list(
                                count = 3,
                                label = "3 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 6,
                                label = "6 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 1,
                                label = "1 ano",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 3,
                                label = "3 anos",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 5,
                                label = "5 anos",
                                step = "year",
                                stepmode = "backward"),
                              
                              list(step = "all"))),
                          
                          rangeslider = list(type = "date")),
             
             yaxis = list(title = "Índice de Preços", zeroline = TRUE,  tickformat = '%',
                          showline = FALSE, showgrid = FALSE,hoverformat = '.0%' ,hoverformat = '.2f'), barmode = 'stack') %>%
      config(displayModeBar = FALSE) %>%
      layout(hovermode = 'compare')
    
    Indexador_Vencimentos_percentual_Cambio <- plot_ly(Cambio_vencimentos, x = ~Periodo) %>% 
      add_trace(y = ~Ate_12_Meses_percentual, type = 'bar', name = 'Até 12 meses', marker = list(color = '#7FFFD4')) %>%
      add_trace(y = ~De_1_2_anos_percentual,  type = "bar",name = 'Entre 1 e 2 anos', marker = list(color = '#66CDAA')) %>%
      add_trace(y = ~De_2_5_anos_percentual, type = "bar",name = 'Entre 2 e 5 anos', marker = list(color = '  #329a77')) %>%
      add_trace(y = ~Acima_5_anos_percentual, type = "bar",name = 'Acima de 5 anos', marker = list(color = '#5F9EA0')) %>%
      
      
      layout(margin = list( b = 20) ,annotations = 
               list(x = 1, y = -0.1, text = "<b>Fonte:</b> Anexo 3.2 - RMD.", 
                    showarrow = F, xref='paper', yref='paper', 
                    xanchor='right', yanchor='auto', xshift=100, yshift=-80,
                    font=list(size=12, color="#696969")),
             title = "",
             xaxis = list(title = "",
                          rangeselector = list(
                            buttons = list(
                              list(
                                count = 3,
                                label = "3 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 6,
                                label = "6 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 1,
                                label = "1 ano",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 3,
                                label = "3 anos",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 5,
                                label = "5 anos",
                                step = "year",
                                stepmode = "backward"),
                              
                              list(step = "all"))),
                          
                          rangeslider = list(type = "date")),
             
             yaxis = list(title = "Câmbio", zeroline = TRUE,  tickformat = '%',
                          showline = FALSE, showgrid = FALSE,hoverformat = '.0%' ,hoverformat = '.2f'), barmode = 'stack') %>%
      config(displayModeBar = FALSE) %>%
      layout(hovermode = 'compare')
    
    Indexador_Vencimentos_percentual_Demais <- plot_ly(Demais_vencimentos, x = ~Periodo) %>% 
      add_trace(y = ~Ate_12_Meses_percentual, type = 'bar', name = 'Até 12 meses', marker = list(color = '#7FFFD4')) %>%
      add_trace(y = ~De_1_2_anos_percentual,  type = "bar",name = 'Entre 1 e 2 anos', marker = list(color = '#66CDAA')) %>%
      add_trace(y = ~De_2_5_anos_percentual, type = "bar",name = 'Entre 2 e 5 anos', marker = list(color = '  #329a77')) %>%
      add_trace(y = ~Acima_5_anos_percentual, type = "bar",name = 'Acima de 5 anos', marker = list(color = '#5F9EA0')) %>%
      
      
      layout(margin = list( b = 20) ,annotations = 
               list(x = 1, y = -0.1, text = "<b>Fonte:</b> Anexo 3.2 - RMD.", 
                    showarrow = F, xref='paper', yref='paper', 
                    xanchor='right', yanchor='auto', xshift=100, yshift=-80,
                    font=list(size=12, color="#696969")),
             title = "",
             xaxis = list(title = "",
                          rangeselector = list(
                            buttons = list(
                              list(
                                count = 3,
                                label = "3 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 6,
                                label = "6 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 1,
                                label = "1 ano",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 3,
                                label = "3 anos",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 5,
                                label = "5 anos",
                                step = "year",
                                stepmode = "backward"),
                              
                              list(step = "all"))),
                          
                          rangeslider = list(type = "date")),
             
             yaxis = list(title = "Demais", zeroline = TRUE,  tickformat = '%',
                          showline = FALSE, showgrid = FALSE,hoverformat = '.0%' ,hoverformat = '.2f'), barmode = 'stack') %>%
      config(displayModeBar = FALSE) %>%
      layout(hovermode = 'compare')
    
    output$Indexador_Vencimentos_Percentual <- renderPlotly({ 
      
      get(input$Indexador_Vencimentos)
      
    }) 
    
 #Grafico - Vincendo 12 meses - Indexador
    
    output$Vincendo_12_Meses_Indexador <- renderPlotly({  
      
      plot_ly(Vencer_12_meses_correto, x = ~Periodo) %>% 
        add_trace(y = ~Flutuante, type = 'bar', name = 'Flutuante', marker = list(color = 'Orange')) %>%
        add_trace(y = ~Prefixado,  type = "bar",name = 'Prefixados',marker = list(color = '#4682B4')) %>%
        add_trace(y = ~Indice_Precos, type = "bar",name = 'Indexados Inflação',marker = list(color = '#ADD8E6')) %>%
        add_trace(y = ~Cambio, type = "bar",name = 'Cambial',marker = list(color = '#008000')) %>%
        add_trace(y = ~Demais, type = "bar",visible = "legendonly",name = 'Demais',marker = list(color = '#b2e6d5')) %>%
        add_lines(y=~Total, name = "Total - A vencer 12 Meses - DPMFi", line = list(color = '#FFA500')
                  
        ) %>%
        layout(margin = list(b = 20),annotations = 
                 list(x = 1, y = -0.1, text = "<b>Fonte:</b> Anexo 3.3 - RMD.", 
                      showarrow = F, xref='paper', yref='paper', 
                      xanchor='right', yanchor='auto', xshift=100, yshift=-80,
                      font=list(size=12, color="#696969")),
               title = "",
               xaxis = list(title = "",
                            rangeselector = list(
                              buttons = list(
                                list(
                                  count = 3,
                                  label = "3 meses",
                                  step = "month",
                                  stepmode = "backward"),
                                list(
                                  count = 6,
                                  label = "6 meses",
                                  step = "month",
                                  stepmode = "backward"),
                                list(
                                  count = 1,
                                  label = "1 ano",
                                  step = "year",
                                  stepmode = "backward"),
                                list(
                                  count = 3,
                                  label = "3 anos",
                                  step = "year",
                                  stepmode = "backward"),
                                list(
                                  count = 5,
                                  label = "5 anos",
                                  step = "year",
                                  stepmode = "backward"),
                                
                                list(step = "all"))),
                            
                            rangeslider = list(type = "date")),
               
               yaxis = list(title = "R$ - Bilhões", zeroline = TRUE,
                            showline = FALSE, showgrid = FALSE,hoverformat = ',.4r'), barmode = 'stack') %>%
        config(displayModeBar = FALSE) %>%
        layout(hovermode = 'compare')
      
      
    }) 
    
    
    output$Vincendo_12_Meses_Indexador_Percentual <- renderPlotly({  
      
    
     plot_ly(Vencer_12_meses_correto, x = ~Periodo) %>% 
      add_trace(y = ~Flutuante_percentual, type = 'bar', name = 'FLUTUANTE', marker = list(color = 'Orange')) %>%
      add_trace(y = ~Prefixado_percentual,  type = "bar",name = 'PREFIXADOS',marker = list(color = '#4682B4')) %>%
      add_trace(y = ~Indice_Precos_percentual, type = "bar",name = 'INDEXADOS INFLAÇÃO',marker = list(color = '#ADD8E6')) %>%
      add_trace(y = ~Cambio_percentual, type = "bar",name = 'CAMBIAL',marker = list(color = '#008000')) %>%
      #add_trace(y = ~Demais_percentual, type = "bar",visible = "legendonly",name = 'Demais',marker = list(color = '#b2e6d5')) %>%
        
      
      layout(margin = list( b = 20) ,annotations = 
               list(x = 1, y = -0.1, text = "<b>Fonte:</b> Anexo 3.3 - RMD.", 
                    showarrow = F, xref='paper', yref='paper', 
                    xanchor='right', yanchor='auto', xshift=100, yshift=-80,
                    font=list(size=12, color="#696969")),
             title = "",
             xaxis = list(title = "",
                          rangeselector = list(
                            buttons = list(
                              list(
                                count = 3,
                                label = "3 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 6,
                                label = "6 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 1,
                                label = "1 ano",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 3,
                                label = "3 anos",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 5,
                                label = "5 anos",
                                step = "year",
                                stepmode = "backward"),
                              
                              list(step = "all"))),
                          
                          rangeslider = list(type = "date")),
             
             yaxis = list(title = "A Vencer 12 Meses", zeroline = TRUE,  tickformat = '%',
                          showline = FALSE, showgrid = FALSE,hoverformat = '.0%' ,hoverformat = '.2f'), barmode = 'stack') %>%
      config(displayModeBar = FALSE) %>%
      layout(hovermode = 'compare')
      
    })
    
    #GRAFICO - PRAZO MEDIO _ DPF 
    
    output$prazo_medio_DPF <- renderPlotly({
      
      
      plot_ly(Prazo_medio, x=~Periodo) %>%
        
        add_lines(y = ~DPF, name = "DPF", line = list(color = '#4682B4'
        )) %>%
        
        add_lines(y = ~DPMFi, name = "DPMFi", visible = "legendonly", line = list(color = '#FFA500'
        )) %>%
        
        add_lines(y = ~DPFe, name = "DPFe", visible = "legendonly", line = list(color = '#008000'
        ), yaxis = 'y2') %>%
        
        
        layout(margin = list(b = 20),annotations = 
                 list(x = 1, y = -0.1, text = "<b>Fonte:</b> Anexo 3.7 - RMD.", 
                      showarrow = F, xref='paper', yref='paper', 
                      xanchor='right', yanchor='auto', xshift=100, yshift=-80,
                      font=list(size=12, color="#696969")),
               title = "",
               xaxis = list(title = "", 
                            rangeselector = list(
                              buttons = list(
                                list(
                                  count = 3,
                                  label = "3 meses",
                                  step = "month",
                                  stepmode = "backward"),
                                list(
                                  count = 6,
                                  label = "6 meses",
                                  step = "month",
                                  stepmode = "backward"),
                                list(
                                  count = 1,
                                  label = "1 ano",
                                  step = "year",
                                  stepmode = "backward"),
                                list(
                                  count = 3,
                                  label = "3 anos",
                                  step = "year",
                                  stepmode = "backward"),
                                list(
                                  count = 5,
                                  label = "5 anos",
                                  step = "year",
                                  stepmode = "backward"),
                                
                                list(step = "all"))),
                            
                            rangeslider = list(type = "date")),
               yaxis = list(side = 'left', title = 'Prazo Médio - Anos', showgrid = FALSE, zeroline = FALSE, hoverformat = '.2f'),
               yaxis2 = list(side = 'right', overlaying = "y" ,title='Eixo - DPFe  ' ,titlefont = list(color = '#008000') ,showgrid = FALSE, zeroline = FALSE , hoverformat = '.2f')) %>%
        config(displayModeBar = FALSE) %>%
        layout(hovermode = 'compare')
      
      
    }) 
    
#GRAFICO - PRAZO MEDIO DPMFi - Por titulo e Indexador
    
    Prazo_medio_DPMFi_titulo <- plot_ly(Prazo_medio, x = ~Periodo) %>%
        add_lines(y = ~DPMFi, name = "DPMFi", line = list(color = '#FFA500'
        )) %>%
        add_lines(y = ~LFT, name = "LFT", visible = "legendonly", line = list(color = '#FF4500'
        )) %>%
        add_lines(y = ~LTN, name = "LTN", visible = "legendonly", line = list(color = '#B22222'
        )) %>%
        add_lines(y = ~NTN_B, name = "NTN-B", visible = "legendonly", line = list(color = '#2F4F4F'
        ) ) %>%
        add_lines(y = ~NTN_C, name = "NTN-C", visible = "legendonly", line = list(color = '#D8BFD8'
        ) ) %>%
        add_lines(y = ~NTN_D, name = "NTN-D", visible = "legendonly", line = list(color = '#EEE8AA'
        )) %>%
        add_lines(y = ~NTN_F, name = "NTN-F", visible = "legendonly", line = list(color = '#FA8072'
        )) %>%
        add_lines(y = ~TDA, name = "TDA", visible = "legendonly", line = list(color = '#FFB6C1'
        )) %>%
        add_lines(y = ~Divida_Securitizada, name = "Dívida Securitizada", visible = "legendonly", line = list(color = '#DA70D6'
        )) %>%
        add_lines(y = ~Demais, name = "Demais", visible = "legendonly", line = list(color = '#708090'
        )) %>%
        
        layout(margin = list(b = 20),annotations = 
                 list(x = 1, y = -0.1, text = "<b>Fonte:</b> Anexo 2.1 - RMD.", 
                      showarrow = F, xref='paper', yref='paper', 
                      xanchor='right', yanchor='auto', xshift=100, yshift=-80,
                      font=list(size=12, color="#696969")),
               title = "",
               xaxis = list(title = "",
                            rangeselector = list(
                              buttons = list(
                                list(
                                  count = 3,
                                  label = "3 meses",
                                  step = "month",
                                  stepmode = "backward"),
                                list(
                                  count = 6,
                                  label = "6 meses",
                                  step = "month",
                                  stepmode = "backward"),
                                list(
                                  count = 1,
                                  label = "1 ano",
                                  step = "year",
                                  stepmode = "backward"),
                                list(
                                  count = 3,
                                  label = "3 anos",
                                  step = "year",
                                  stepmode = "backward"),
                                list(
                                  count = 5,
                                  label = "5 anos",
                                  step = "year",
                                  stepmode = "backward"),
                                
                                list(step = "all"))),
                            
                            rangeslider = list(type = "date")),
               
               yaxis = list(side = 'left', title = 'Prazo Médio - Anos', showgrid = FALSE, zeroline = FALSE, hoverformat = ',.4r')) %>%
        config(displayModeBar = FALSE) %>%
        layout(hovermode = 'compare')
      
      
    
    
    Prazo_medio_DPMFi_Indexador <- plot_ly(Prazo_medio_indexador, x = ~Periodo) %>%
        add_lines(y = ~DPMFi, name = "DPMFi", line = list(color = '#FFA500'
        )) %>%
        add_lines(y = ~Taxa_Flutuante, name = "FLUTUANTE", visible = "legendonly", line = list(color = '#FF4500'
        )) %>%
        add_lines(y = ~Prefixados, name = "PREFIXADOS", visible = "legendonly", line = list(color = '#4682B4'
        )) %>%
        add_lines(y = ~Indice_Precos, name = "ÍNDEXADOS INFLAÇÃO", visible = "legendonly", line = list(color = '#ADD8E6'
        ) ) %>%
        add_lines(y = ~Cambio, name = "CAMBIAL", visible = "legendonly", line = list(color = '#008000'
        ) ) %>%
        #add_lines(y = ~Demais, name = "Demais", visible = "legendonly", line = list(color = '#b2e6d5'
        #)) %>%
        
        layout(margin = list(b = 20),annotations = 
                 list(x = 1, y = -0.1, text = "<b>Fonte:</b> Anexo 3.8 - RMD.", 
                      showarrow = F, xref='paper', yref='paper', 
                      xanchor='right', yanchor='auto', xshift=100, yshift=-80,
                      font=list(size=12, color="#696969")),
               title = "",
               xaxis = list(title = "",
                            rangeselector = list(
                              buttons = list(
                                list(
                                  count = 3,
                                  label = "3 meses",
                                  step = "month",
                                  stepmode = "backward"),
                                list(
                                  count = 6,
                                  label = "6 meses",
                                  step = "month",
                                  stepmode = "backward"),
                                list(
                                  count = 1,
                                  label = "1 ano",
                                  step = "year",
                                  stepmode = "backward"),
                                list(
                                  count = 3,
                                  label = "3 anos",
                                  step = "year",
                                  stepmode = "backward"),
                                list(
                                  count = 5,
                                  label = "5 anos",
                                  step = "year",
                                  stepmode = "backward"),
                                
                                list(step = "all"))),
                            
                            rangeslider = list(type = "date")),
               
               yaxis = list(side = 'left', title = 'Prazo Médio - Anos', showgrid = FALSE, zeroline = FALSE, hoverformat = ',.4r')) %>%
        config(displayModeBar = FALSE) %>%
        layout(hovermode = 'compare')
      
      
    
    
    output$prazo_medio_DPMFi <- renderPlotly({
      
      get(input$Prazo_Tipo)
      
    })
    
   
    output$prazo_medio_DPFe <- renderPlotly({
      
      plot_ly(Prazo_medio, x = ~Periodo) %>%
        add_lines(y = ~DPFe, name = "DPFe", line = list(color = '#008000'
        )) %>%
        add_lines(y = ~Global_USD, name = "Global USD", visible = "legendonly", line = list(color = '	#20B2AA'
        )) %>%
        add_lines(y = ~Euro, name = "Euro", visible = "legendonly", line = list(color = '	#9ACD32'
        )) %>%
        add_lines(y = ~Global_BRL, name = "Global BRL", visible = "legendonly", line = list(color = '	#6A5ACD'
        )) %>%
        add_lines(y = ~Reestruturada, name = "Reestruturada", visible = "legendonly", line = list(color = '	#9370DB'
        )) %>%
        add_lines(y = ~Demais_DPFe, name = "Demais", visible = "legendonly", line = list(color = '	#00BFFF'
        )) %>%
        add_lines(y = ~Organismos_Multilaterais, name = "Organismos Multilaterais", visible = "legendonly", line = list(color = '	#DCDCDC'
        )) %>%
        add_lines(y = ~Credores_Privados, name = "Credores Privados/ Ag. Gov.", visible = "legendonly", line = list(color = '	#1C1C1C'
        ) ) %>%
        add_lines(y = ~Clube_de_Paris, name = "Clube de Paris", visible = "legendonly", line = list(color = '	#FF69B4'
        ) ) %>%
        
        layout(margin = list(b = 20),annotations = 
                 list(x = 1, y = -0.1, text = "<b>Fonte:</b> Anexo 2.1 - RMD.", 
                      showarrow = F, xref='paper', yref='paper', 
                      xanchor='right', yanchor='auto', xshift=100, yshift=-80,
                      font=list(size=12, color="#696969")),
               title = "",
               xaxis = list(title = "",
                            rangeselector = list(
                              buttons = list(
                                list(
                                  count = 3,
                                  label = "3 meses",
                                  step = "month",
                                  stepmode = "backward"),
                                list(
                                  count = 6,
                                  label = "6 meses",
                                  step = "month",
                                  stepmode = "backward"),
                                list(
                                  count = 1,
                                  label = "1 ano",
                                  step = "year",
                                  stepmode = "backward"),
                                list(
                                  count = 3,
                                  label = "3 anos",
                                  step = "year",
                                  stepmode = "backward"),
                                list(
                                  count = 5,
                                  label = "5 anos",
                                  step = "year",
                                  stepmode = "backward"),
                                
                                list(step = "all"))),
                            
                            rangeslider = list(type = "date")),
               
               yaxis = list(side = 'left', title = 'Prazo Médio - Anos', showgrid = FALSE, zeroline = FALSE, hoverformat = ',.4r')) %>%
        config(displayModeBar = FALSE) %>%
        layout(hovermode = 'compare')
      
      
      
      
    }) 
    
    
    output$emissoes_prazo_medio_DPMFi <- renderPlotly({
      
      
      plot_ly(Prazo_medio_emissoes, x = ~Periodo) %>%
      
      add_lines(y = ~Total, name = "Emissão Oferta Pública - Total", line = list(color = '#FFA500'
      )) %>%
      add_lines(data = Prazo_medio,y = ~DPMFi, name = "Estoque - DPMFi", visible = "legendonly", line = list(color = '#FF4500'
      )) %>%
      add_lines(data =Prazo_medio_emissoes,y = ~Prefixado, name = "Emissão - Prefixado", visible = "legendonly", line = list(color = '#4682B4'
      )) %>%
      add_lines(y = ~Indice_precos, name = "Emissão - Indexado Inflação", visible = "legendonly", line = list(color = '#ADD8E6'
      ) ) %>%
      add_lines(y = ~Taxa_Flutuante, name = "Emissão - Flutuante", visible = "legendonly", line = list(color = '#993d00'
      ) ) %>%
      
      
      layout(margin = list(b = 20),annotations = 
               list(x = 1, y = -0.1, text = "<b>Fonte:</b> Anexo 3.8 - RMD.", 
                    showarrow = F, xref='paper', yref='paper', 
                    xanchor='right', yanchor='auto', xshift=100, yshift=-80,
                    font=list(size=12, color="#696969")),
             title = "",
             xaxis = list(title = "",
                          rangeselector = list(
                            buttons = list(
                              list(
                                count = 3,
                                label = "3 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 6,
                                label = "6 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 1,
                                label = "1 ano",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 3,
                                label = "3 anos",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 5,
                                label = "5 anos",
                                step = "year",
                                stepmode = "backward"),
                              
                              list(step = "all"))),
                          
                          rangeslider = list(type = "date")),
             
             yaxis = list(side = 'left', title = 'Prazo Médio - Anos', showgrid = FALSE, zeroline = FALSE, hoverformat = ',.4r')) %>%
      config(displayModeBar = FALSE) %>%
      layout(hovermode = 'compare')
    
    })
    
     
}
  

  

