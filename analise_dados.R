library(tidyverse)
library(lubridate)
library(ggplot2)

# https://www.r-graph-gallery.com/283-the-hourly-heatmap.html
dados <- read.csv("G:/.shortcut-targets-by-id/1yy0fYXXpAvbcLDIiutJQMWxUgLaDtMG_/grupo 3/Conjunto de Dados/dados_gerais.csv", stringsAsFactors = TRUE) %>% 
  na.omit()

df <- dados %>% 
  select(product_category_name, order_purchase_timestamp, order_item_id, customer_state) 

df <- df %>% 
  mutate(a_m_d = as_date(as_datetime(order_purchase_timestamp)),
         hora = hour(as_datetime(order_purchase_timestamp)),
         ano = year(as_date(order_purchase_timestamp)),
         mes = month(as_date(order_purchase_timestamp),label=TRUE),
         dia = day(as_date(order_purchase_timestamp)))

df <- df %>% 
  mutate(regiao=
           case_when(
             customer_state%in%c("SC","RS","PR")~"Sul",
             customer_state%in%c("ES","MG","RJ","SP")~"Sudeste",
             customer_state%in%c("GO","MT","MS")~"Centro-Oeste",
             customer_state%in%c("AC","AP","AM","PA","RO","RR","TO")~"Norte",
             TRUE ~"Nordeste"
           ))

df <- df %>% 
  filter(ano%in%c(2017,2018))

write.csv(df, file = "dados_q6.csv", fileEncoding = "UTF-8", row.names = FALSE )


produtos <- unique(df$product_category_name)

input_produto <- "moveis_decoracao"

input_regiao <- "Nordeste"

input_estado <- c("RN","PB")

df2 <- df %>% 
  group_by(hora, ano, mes, dia, product_category_name, customer_state, regiao) %>% 
  summarise(qtd=sum(order_item_id))

df2 <- df2 %>% 
  filter(product_category_name==input_produto, regiao==input_regiao, customer_state%in%input_estado )

df2 <- df2 %>% 
  fill(qtd)

p <-ggplot(df2,aes(dia,hora,fill=qtd))+
  geom_tile(color= "white",size=0.1) + 
  scale_fill_viridis(name="Qtd comprada",option = "B", begin = 1 , end = 0)
p <-p + facet_grid(ano~mes)
p <-p + scale_y_continuous(trans = "reverse", breaks = unique(df$hora))
p <-p + scale_x_continuous(breaks =c(1,10,20,31))
p <-p + theme_minimal(base_size = 8)
p <-p + labs(title= paste("Quantidade por Hora - Produto:",input_produto), 
             subtitle = paste("Região: ",input_regiao," Estado(s): ", paste0(input_estado, collapse = ", ")),
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

# you will want to expand your plot screen before this bit!
p #awesomeness

## Grafico de pareto para ver o desempenho por dia da semana e hora

tabela_dia_semana <- table(weekdays(df2$a_m_d))

pareto.chart(tabela_dia_semana, ylab="Frequência")

date