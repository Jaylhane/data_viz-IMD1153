#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr) # easier data wrangling 
library(viridis) # colour blind friendly palette, works in B&W also
#library(Interpol.T) #  will generate a large dataset on initial load
library(lubridate) # for easy date manipulation
library(ggExtra) # because remembering ggplot theme options is beyond me
library(tidyr) 
library(qcc)

# Carregando os dados

df <- read.csv("dados_q6.csv", stringsAsFactors = TRUE, fileEncoding = "UTF-8")
unique(df$mes)

df$a_m_d <- as_date(df$a_m_d)
df <- df %>% 
    mutate(mes=factor(mes,
        levels = c("jan","fev","mar","abr",
                   "mai","jun","jul","ago",
                   "set","out","nov","dez")
    ))

# Define UI for application that draws a histogram

ui <- fluidPage(
    
    # Application title
    titlePanel("Desempenho por tipo de produto ao longo do ano"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            selectInput(inputId = "produto",
                        label = "Escolha o(s) produto(s) ¹",
                        choices = sort(unique(df$product_category_name)),
                        selected = tail(df$product_category_name,5),
                        multiple = TRUE,
                        selectize = TRUE,
                        width = NULL,
                        size = NULL),
            helpText("¹ Possível selecionar mais de um"),
            
            selectInput(inputId = "regiao",
                        label = "Escolha a regiao ²",
                        choices = unique(df$regiao),
                        selected = "Nordeste",
                        multiple = FALSE,
                        selectize = TRUE,
                        width = NULL,
                        size = NULL),
            helpText("² Possível selecionar apenas uma região por vez"),
            
            selectInput( inputId = "estado",
                         label = "Escolha o(s) estado(s)³*",
                         choices = NULL,
                         multiple = TRUE,
                         selectize = TRUE,
                         width = NULL,
                         size = NULL),
            
            helpText("³ Os estados disponíveis para seleção estão de acordo com a região selecionada\n * É possível selecionar mais de um estado - exclua os estados que não deseja visualizar")
            
        ),
        
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("mapa_calor", height = 700),
            hr(),
            h4("Gráfico de Pareto para identificar o desempenho por dia da semana"),
            plotOutput("pareto_chart")
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # selecionando os dados de acordo com regiao e produto
    
    df_est_prod_finder <- reactive({
        req(input$produto)
        req(input$regiao)
        
        filter(df, product_category_name %in% input$produto) %>% 
            filter(regiao == input$regiao) 
    })
    
    
    ###-------selecionando o estado (reativo)
    
    observeEvent(df_est_prod_finder(), {
        estados <- unique(df_est_prod_finder()$customer_state)
        updateSelectInput(inputId = "estado", choices = estados, selected = unique(estados))
    })
    
    escolha_estado <- reactive({
        req(input$estado)
        filter(df_est_prod_finder(), customer_state %in% input$estado)
    })
    
    #Selecionando o estado
    
    estado_finder <- reactive({
        req(input$estado)
        estad_finder <- input$estado
        estado_finder
    })
    
    ###-----x-----###
    
    #selecionando conjunto de dados com a seleção de produto e região
    
    df_finder <- reactive({
        req(input$produto)
        req(input$regiao)
        req(input$estado)
        
        filter(df, product_category_name %in% input$produto) %>% 
            filter(regiao == input$regiao) %>% 
            filter(customer_state %in% input$estado)
    })
    
    output$mapa_calor <- renderPlot({
        input$produto
        input$regiao
        input$estado
        
        isolate({  
            
            df2 <- df_finder() %>% 
                group_by(hora, ano, mes, dia, product_category_name, customer_state, regiao) %>% 
                summarise(qtd=sum(order_item_id))
            
            
            df2 <- df2 %>% 
                fill(qtd)
            
            produto_selecionado <- input$produto
            
            regiao_selecionada <- input$regiao
            
            estado_selecionado <- input$estado
            
            
            p <-ggplot(df2,aes(dia,hora,fill=qtd))+
                geom_tile(color= "white",size=0.1) + 
                scale_fill_viridis(name="Qtd comprada",option = "E", begin = 1 , end = 0)
            p <-p + facet_grid(ano~mes)
            p <-p + scale_y_continuous(trans = "reverse", breaks = unique(df$hora))
            p <-p + scale_x_continuous(breaks =c(1,10,20,31))
            p <-p + theme_minimal(base_size = 8)
            p <-p + labs(title= paste("Quantidade por Hora - Produto(s):",paste0(produto_selecionado, collapse = ", ")), 
                         subtitle = paste("Região: ",regiao_selecionada," Estado(s): ", paste0(estado_selecionado, collapse = ", ")),
                         x="Dia", y="Hora")
            p <-p + theme(legend.position = "top")+
                theme(plot.title=element_text(size = 14))+
                theme(axis.text.y=element_text(size=6)) +
                theme(strip.background = element_rect(colour="white"))+
                theme(plot.title=element_text(hjust=0))+
                theme(axis.ticks=element_blank())+
                theme(axis.text=element_text(size=7))+
                theme(legend.title=element_text(size=8))+
                theme(legend.text=element_text(size=6))+
                removeGrid()#ggExtra
            p
        })
    })
    
    ## Grafico de pareto para ver o desempenho por dia da semana
    
    output$pareto_chart <- renderPlot({
        input$produto
        input$regiao
        input$estado
        
        isolate({
            
            tabela_dia_semana <- table(weekdays(df_finder()$a_m_d))
            
            pareto.chart(tabela_dia_semana, ylab="Frequência")
        })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
