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


#setwd("C:/Users/lucas.leite/Desktop/Lucas/RStudio/9.  Trabalho Divida/Modelo")

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


header <- dashboardHeader(title = "Dívida Pública Federal", titleWidth = 270, tags$li(class="dropdown", tags$a(href="https://github.com/gt-cead", icon("github"), "Source Code", target ="_blank")))



sidebar <- dashboardSidebar(width =270,
                            
                            sidebarSearchForm(label = "Procurar", "searchText", "searchButton"),
                            sidebarMenu(
                              # Setting id makes input$tabs give the tabName of currently-selected tab
                              
                              id = "tabs",
                              
                              menuItem("Grandes Números", tabName = "grandesnumeros", icon = icon("dashboard"),
                                       menuSubItem("Resultados Esperados 2018", tabName = "resultados"),
                                       menuSubItem("Diretrizes e Objetivos", tabName = "objetivos")),
                              
                              menuItem("Necessidade Financiamento", tabName = "necessidade" ,icon = icon("code-fork"), 
                                       menuSubItem("Bruta", tabName = "bruta"),
                                       menuSubItem("Líquida", tabName = "liquida"),
                                       menuSubItem("Execução Orçamentária", tabName = "orcamento")),
                              
                              menuItem("Operações no Mercado", tabName = "mercadoprimario" ,icon = icon("calendar"),
                                       menuSubItem("Leilões Realizados", tabName = "leiloes"),
                                       menuSubItem("Emissões e Resgastes", tabName = "resgates")),
                              
                              menuItem("DPF em Mercado", tabName = "estoque" ,icon = icon("bar-chart-o"),
                                       menuSubItem("Evolução", tabName = "evolucao"),
                                       menuSubItem("Fatores de Variação", tabName = "fatores"),
                                       menuSubItem("Indexadores", tabName = "composicao"),
                                       menuSubItem("Detentores", tabName = "detentores")),
                              
                              menuItem("Perfil Vencimentos", tabName = "vencimentos", icon = icon("bar-chart-o"),
                                       menuSubItem("Composição dos Vencimentos", tabName = "vincendo"),
                                       menuSubItem("Prazo Médio", tabName = "prazomedio"),
                                       menuSubItem("Vida Média", tabName = "vidamedia")),
                              
                              menuItem("Custo Médio", tabName = "custos", icon = icon("bar-chart-o"),
                                       menuSubItem("Custo Médio Estoque", tabName = "custoestoque"),
                                       menuSubItem("Custo Médio Emissão", tabName = "custoemissao")),
                              
                              menuItem("Programa Tesouro Direto", tabName = "tesourodireto", icon = icon("child"),
                                       menuSubItem("Estoque, Emissões e Resgates", tabName = "tesouroestoque"),
                                       menuSubItem("Detentores", tabName = "tesourodetentores")),
                              
                              menuItem("Relatórios Publicados", tabName = "relatoriopublicados", icon = icon("file"),
                                       menuSubItem("Plano Anual de Financiamento - PAF", tabName = "paf"),
                                       menuSubItem("Relatorio Anual da Dívida - RAD", tabName = "rad"),
                                       menuSubItem("Relatorio Mensal da Dívida - RMD", tabName = "rmd"),
                                       menuSubItem("Relatorio de Garantias - RG", tabName = "garantias")),
                              
                              menuItem("Projeção Dívida - Faça a sua", tabName = "projecaodivida", icon = icon("cog", class = "fa-pulse"))
                              
                            )
)


body <- dashboardBody(
  
  tabItems(
    
    tabItem("resultados",
            strong("Estoque Nominal:"),
            
            br(), br(),
            
            fluidRow(
              
              infoBoxOutput("estoque", width = 3)),
            
            strong("Composição por Indexador"),
            
            fluidRow(
              infoBoxOutput("prefixado", width = 3),  
              
              infoBoxOutput("posfixados", width = 3),
              
              infoBoxOutput("preço", width = 3),
              
              infoBoxOutput("cambial", width = 3)),
            
            strong("Perfil de Vencimento"),
            
            fluidRow(
              infoBoxOutput("prazomedio", width = 3),  
              
              infoBoxOutput("vincendomeses", width = 3)),
            
            strong("Emissões e Resgate"),
            
            fluidRow(
              box(flexdashboard::gaugeOutput("plt1"), title =" Saldo DPF - Bilhões R$",solidHeader = T ,width = 3),
              
              box(flexdashboard::gaugeOutput("plt2"), title =" Indexador Prefixado",solidHeader = T , status= "primary" ,width = 3),
              
              box(flexdashboard::gaugeOutput("plt3"), title =" Indexador Posfixado", solidHeader = T , status= "warning",width = 3),
              
              box(flexdashboard::gaugeOutput("plt4"), title =" Indexador Índice de Preço", solidHeader = T , status= "danger" ,width  = 3))
            
            
            
    ),
    
    tabItem(tabName = "objetivos",
            box(title =" Gestão da Dívida Pública Federal", solidHeader = T , status= "primary",img(src="objetivos.PNG"), width = 10)
    ),
    
    tabItem(tabName = "bruta",
            fluidRow(box(title =" 2017", solidHeader = T , status= "primary",img(src="bruta2017.PNG"), collapsible = TRUE ,width = 4),
                     box(title =" 2016", solidHeader = T , status= "primary",img(src="bruta2016.PNG"),collapsible = TRUE ,width = 4),
                     box(title =" 2015", solidHeader = T , status= "primary",img(src="bruta2015.PNG"), collapsible = TRUE ,width = 4)
            )),
    
    
    tabItem(tabName = "liquida",
            fluidRow(box(title =" 2017", solidHeader = T , status= "primary",img(src="liquida2017.PNG"), collapsible = TRUE ,width = 4),
                     box(title =" 2016", solidHeader = T , status= "primary",img(src="liquida2016.PNG"),collapsible = TRUE ,width = 7),
                     box(title =" 2015", solidHeader = T , status= "primary",img(src="liquida2015.PNG"), collapsible = TRUE ,width = 7)
            )),
    
    tabItem(tabName = "orcamento", 
            fluidRow(
              column(3,
                     selectInput("exe",
                                 "Exercicio:",
                                 c("todos",
                                   unique(as.character(DPF_Fonte$Exercicio))))),
              column(3,
                     selectInput("cart",
                                 "Tipo de Carteira",
                                 c("todos",
                                   unique(as.character(DPF_Fonte$Carteira))))),
               column(3,
                      selectInput("font",
                                  "Tipo de Fonte",
                                  c("todos",
                                    unique(as.character(DPF_Fonte$Fonte))))),
              column(3,
                     selectInput("mov",
                                 "Natureza Despesa Por Nome",
                                 c("todos",
                                   unique(as.character(DPF_Fonte$Natureza_Despesa_Nome))))),
              
              dataTableOutput("Fontes"))
            
            
            
    ),
    
    tabItem(tabName = "leiloes",
            box(title =" Leilões da Dívida Pública", img(src="leiloes.PNG") ,solidHeader = T , status= "primary", width = 10),
            dataTableOutput("leiloes")
    ),
    
    tabItem(tabName = "resgates",
            box(title =" Resgates e Emissão da DPF", img(src="emissoes.PNG") ,solidHeader = T , status= "primary", width = 10),
            box(title =" Resgates e Emissão da DPF", img(src="emissoesdpf.PNG") ,solidHeader = T , status= "primary", width = 10)
    ),
    
    tabItem(tabName = "evolucao",
            fluidRow(
              tabBox(id="tabchart1", width = 1000, height=560,    
                     tabPanel(h4("DPF em Mercado"), 
                              
                              tabBox(id="tabchart2", width = 1000, height=560,    
                                     tabPanel("Evolução", tabBox(id="tabchart3", width = 1000, height=560, 
                                                                tabPanel("DPF", plotlyOutput("Estoque_tipo")),
                                                                tabPanel("DPMFi", plotlyOutput("Estoque_DPMFi")),
                                                                tabPanel("DPFe", plotlyOutput("Estoque_DPFe")),
                                                                tabPanel("Composição (%)", selectInput("Tipo_teste", "Tipo",
                                                                                                                  choices=c("DPF" = "estoque_percentual_DPF", "DPMFi" = "estoque_percentual_DPMFi","DPFe" = "estoque_percentual_DPFe")), 
                                                                         plotlyOutput("estoque_percentual"))))     
                                                                         
                                                                         
                              ))))
            
            
            
    ),
    
    tabItem(tabName = "fatores",
            box(title =" Fatores de Variação da DPF", img(src="fatores.PNG") ,solidHeader = T , status= "primary", width = 10)
    ),
    
    tabItem(tabName = "composicao",
            fluidRow(
              tabBox(id="tabchart1", width = 1000, height=560,    
                     tabPanel(h4("DPF em Mercado"), 
                              
                              tabBox(id="tabchart2", width = 1000, height=560,    
                                     tabPanel("Indexadores", tabBox(id="tabchart3", width = 1000, height=560, 
                                                                 tabPanel("DPF", plotlyOutput("composicao_DPF")),
                                                                 tabPanel("DPMFi", plotlyOutput("composicao_DPMFi")),
                                                                 tabPanel("DPFe", plotlyOutput("composicao_DPFe")),
                                                                 tabPanel("Composição Indexadores (%)", selectInput("Tipo", "Tipo",
                                                                                                                    choices=c("DPF" = "indexador_percentual_DPF", "DPMFi" = "indexador_percentual_DPMFi","DPFe" = "indexador_percentual_DPFe")), 
                                                                          plotlyOutput("indexador_percentual"))))      
                                                                 
                                                                 
                                     ))))
            
            
            ),
    
    tabItem(tabName = "detentores",
            fluidRow(
              tabBox(id="tabchart1", width = 1000, height=560,    
                     tabPanel(h4("DPF em Mercado"), 
                              
                              tabBox(id="tabchart2", width = 1000, height=560,    
                                     tabPanel("Detentores", tabBox(id="tabchart3", width = 1000, height=560, 
                                                                    tabPanel("DPMFi", plotlyOutput("Detentores_DPMFi")),
                                                                    tabPanel("Por Título (%)",selectInput("Titulo", "Título",
                                                                                                                 choices=c("DPMFi" = "DPMFi","LFT" = "LFT", "LTN" = "LTN","NTN-F" = "NTN_F","NTN-B" = "NTN_B","Outros" = "outros")) ,plotlyOutput("detentor_percentual")),
                                                                    tabPanel("Por Detentor (%)",selectInput("Detentores", "Detentores",
                                                                                                                 choices=c("Instituição Financeira" = "Instituicao_Financeira", "Fundos de Investimento" = "Fundos","Previdência" = "Previdencia","Não-Residentes" = "Nao_Residentes","Governo","Seguradoras","Outros")), 
                                                                            plotlyOutput("carteira_detentor"))))     
                                                                                
                                                                    
                                                                    
                                     ))))
            
            
    ),
    
    tabItem(tabName = "vincendo",
            fluidRow(
              tabBox(id="tabchart1", width = 1000, height=560,    
                     tabPanel(h4("Perfil dos Vencimentos"), 
                              
                              tabBox(id="tabchart2", width = 1000, height=560,    
                                     tabPanel("Composição dos Vencimentos", tabBox(id="tabchart3", width = 1000, height=560, 
                                                                    tabPanel("DPF", plotlyOutput("Vencimentos_DPF")),
                                                                    tabPanel("DPMFi", plotlyOutput("Vencimentos_DPMFi")),
                                                                    tabPanel("DPFe", plotlyOutput("Vencimentos_DPFe")),
                                                                    tabPanel("Composição (%)", selectInput("Vencimentos", "Tipo",
                                                                                                                       choices=c("DPF" = "Vencimentos_percentual_DPF", "DPMFi" = "Vencimentos_percentual_DPMFi","DPFe" = "Vencimentos_percentual_DPFe")), 
                                                                             plotlyOutput("Vencimentos_Percentual")),
                                                                    tabPanel("Indexador DPMFi (%)", selectInput("Indexador_Vencimentos", "Indexador",
                                                                            choices=c("Prefixados" = "Indexador_Vencimentos_percentual_Prefixados", "Taxa Flutuante" = "Indexador_Vencimentos_percentual_Flutuante","Índice de Preços" = "Indexador_Vencimentos_percentual_Preco", "Câmbio" = "Indexador_Vencimentos_percentual_Cambio", "Demais" = "Indexador_Vencimentos_percentual_Demais" )), 
                                              plotlyOutput("Indexador_Vencimentos_Percentual")))),
                                  
                                              tabPanel("A Vencer em 12 Meses - DPMFi", tabBox(id="tabchart3", width = 1000, height=560, 
                                                                                              tabPanel("Por Indexador", plotlyOutput("Vincendo_12_Meses_Indexador")),
                                                                                              tabPanel("Composição Indexadores(%)", plotlyOutput("Vincendo_12_Meses_Indexador_Percentual")))))
                                                             
                                                                                                           
                                     
                                     
                              )))
            
            
            
            
            
            
    ),
    
    tabItem(tabName = "prazomedio",
            fluidRow(
              tabBox(id="tabchart1", width = 1000, height=560,    
                     tabPanel(h4("Perfil dos Vencimentos"), 
                              
                              tabBox(id="tabchart2", width = 1000, height=560,    
                                     tabPanel("Prazo Médio", tabBox(id="tabchart3", width = 1000, height=560, 
                                                                                   tabPanel("DPF", plotlyOutput("prazo_medio_DPF")),
                                                                                   tabPanel("DPMFi",selectInput("Prazo_Tipo", "",
                                                                                                                choices=c("Por Título" = "Prazo_medio_DPMFi_titulo", "Por Indexador" = "Prazo_medio_DPMFi_Indexador")),
                                                                                            plotlyOutput("prazo_medio_DPMFi")),
                                                                                   tabPanel("DPFe", plotlyOutput("prazo_medio_DPFe")),
                                                                                   tabPanel("Emissões DPMFi", plotlyOutput("emissoes_prazo_medio_DPMFi"))
                                     ))))))
                                                                                   
                                                                                                                          
            
            
    ),
    
    tabItem(tabName = "vidamedia",
            fluidRow(box(title =" Prazo Medio - Estoque DPMFi", img(src="vidamedia.PNG") ,solidHeader = T , status= "primary", width = 8))
            
            
    ),
    
    tabItem(tabName = "custoestoque",
            fluidRow(
              tabBox(id="tabchart1", width = 1000, height=600,    
                     tabPanel(h4("Custo Médio Estoque "), 
                     
              tabBox(id="tabchart2", width = 1000, height=600,    
                     tabPanel("DPF", plotlyOutput("DPF")),
                     tabPanel("DPMFi", plotlyOutput("DPMFi")),
                     tabPanel("DPFe", plotlyOutput("DPFe"))))))
    ),
           
            
            
    
    
    tabItem(tabName = "custoemissao",
            fluidRow(tabBox(id="tabchart1", width = 1000, height=560,    
                            tabPanel(h4("Custo Médio Emissão "), 
                                     
                                     tabBox(id="tabchart2", width = 1000, height=560,    
                                            tabPanel("DPMFi", plotlyOutput("Emissao"))))))
            
    ),
    
    tabItem(tabName = "tesouroestoque",
            fluidRow(
              tabBox(id="tabchart1", width = 1000, height=560,    
                     tabPanel(h4("Tesouro Direto"), 
                              
                              tabBox(id="tabchart2", width = 1000, height=560,    
                                     tabPanel("Estoque", tabBox(id="tabchart3", width = 1000, height=560, 
                                                                 tabPanel("Indexadores", plotlyOutput("Tesouro_Estoque")),
                                                                 tabPanel("Prazo", plotlyOutput("Tesouro_Prazo")))),
                                     
                                     tabPanel("Emissões", tabBox(id="tabchart4", width = 1000, height=560, 
                                                                 tabPanel("Indicadores", plotlyOutput("Tesouro_Emissao")),
                                                                 tabPanel("Prazo", plotlyOutput("Recompra_Emissao")),
                                                                 tabPanel("Faixa de Investimento", plotlyOutput("Faixa_Emissao"))
                                     )),
                                
                                    
                                     tabPanel("Resgates", tabBox(id="tabchart5", width = 1500, height=560,
                                            tabPanel("Total", plotlyOutput("Tesouro_Resgate")),
                                            tabPanel("Recompras", plotlyOutput('Tesouro_Recompra')),
                                             tabPanel("Vencimentos", plotlyOutput("Tesouro_Vencimentos"))
                                              )),
                                     tabPanel("Emissão Líquida",plotlyOutput("Tesouro_Emissao_Liquida"))
                                     
                                     ))))
    ),
    
    tabItem(tabName = "tesourodetentores",
            fluidRow(
              tabBox(id="tabchart1",  width = 500,  
                     tabPanel(h4("Tesouro Direto - Detentores"), 
                              fluidRow(plotlyOutput("Tesouro_Investidoresa"), width = 3),
            
            fluidRow(column(6,plotlyOutput("Tesouro_Investidores")),column(6,plotlyOutput("Genero_Investidores"))))))
          
    ),
    
    tabItem(tabName = "paf", 
            fluidRow(
              tabBox(id="tabchart1",
                     tabPanel("Vencimento DPF", plotlyOutput("plot1")),
                     tabPanel("DPMFi", plotlyOutput("plot2")),
                     tabPanel("DPFe", plotlyOutput("plot3"))))
    ),
    
    tabItem(tabName = "projecaodivida", htmlOutput("Calculadora") )
            
            #fluidRow(box(title =" BOX - Projeções de Mercado", img(src="formula.PNG") ,solidHeader = T , status= "primary", width = 6))
            
            
            
    )
    
    
    
    
    
    
    
    
  )



ui = dashboardPage(header, sidebar, body)
