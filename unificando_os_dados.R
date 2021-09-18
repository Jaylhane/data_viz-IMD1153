
# Carregando Bibliotecas
library(dplyr)

# Carregando Dados - Brazilian E-commerce 
customers <- read.csv("olist_customers_dataset.csv", header = T, stringsAsFactors = TRUE)
geolocation <- read.csv("olist_geolocation_dataset.csv", header = T)
items <- read.csv("olist_order_items_dataset.csv", header = T)
payments <- read.csv("olist_order_payments_dataset.csv", header = T, stringsAsFactors = TRUE)
reviews <- read.csv("olist_order_reviews_dataset.csv", header = T, encoding = "UTF-8")
orders <- read.csv("olist_orders_dataset.csv", header = T)
products <- read.csv("olist_products_dataset.csv", header = T, stringsAsFactors = TRUE)
sellers <- read.csv("olist_sellers_dataset.csv", header = T, stringsAsFactors = TRUE)
categorias <- read.csv("product_category_name_translation.csv", header = T)

#Carregando Dados - Marketing Funil 

#deals <- read.csv("olist_closed_deals_dataset.csv", header = T)
#leads <- read.csv("olist_marketing_qualified_leads_dataset.csv", header = T)

dados <- full_join(reviews, orders)
dados <- full_join(dados, customers)
dados <- full_join(dados, payments)
dados <- full_join(dados, items)
dados <- full_join(dados, products)
dados <- full_join(dados, sellers)

#dados <- full_join(dados, geolocation, by = c("customer_zip_code_prefix" = "geolocation_zip_code_prefix"))
#dados <- full_join(dados, geolocation, by = c("seller_zip_code_prefix" = "geolocation_zip_code_prefix"))

# Os dados de geolocation possuem apenas as informações de geolocalização, 
# para concatenar essas informações no conjunto de dados o interessante seria
# juntar na base de customers e na base de seller essa informação separadamente. 
# Não é tão simples incluir essas informações na base única

write.csv(dados, file = "dados_gerais.csv", fileEncoding = "UTF-8", row.names = FALSE)

