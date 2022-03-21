library(readr)
library(ggplot2)
library(dplyr)


#https://dados.gov.br/dataset/serie-historica-de-precos-de-combustiveis-por-revenda/resource/07a93382-8860-4ea9-9005-bca747ba9ed2

combustivel <- read_csv2("combustivel.csv",
                         col_types = cols('Data da Coleta'=col_date(format = "%m/%d/%Y")))
gasolina <- combustivel %>% 
  select(Regiao='Regiao - Sigla',
         Estado='Estado - Sigla',
         Municipio,
         Estabelecimento=Revenda,
         CNPJ='CNPJ da Revenda',
         #'Nome da Rua',
         #'Numero Rua',
         #Complemento,
         #Bairro,
         Cep,
         Produto,
         Data_coleta='Data da Coleta',
         Preco='Valor de Venda',
         #'Valor de Compra',
         #'Unidade de Medida',
         Bandeira
  )
gasolina <- filter(gasolina, Produto == "GASOLINA")
gasolina%>%
  ggplot(aes(Estado))+
  theme_classic()+
  geom_bar()+
  labs(
    y = "Amostras",
    title = "QUANTIDADE DE AMOSTRAS POR ESTADO"
  )


#preço medio da gasolina por região do pais

p_reg<-group_by(gasolina,Regiao)%>%summarise(Total=mean(Preco))
p_reg%>%
  ggplot(aes(Regiao,Total,color=Regiao))+
  theme_light()+
  labs(title = "Preço médio da gasolina por região do país", y="Preço")+
  geom_point(size=5)

#variação do preço por estado

gasolina%>%
  ggplot(aes(Estado,Preco))+
  theme_light()+
  scale_y_continuous(breaks = seq(5.2,8,0.2), limits = c(5.2,8))+
labs(y="Preço", title= "Variação do Preço por Estado")+
  geom_violin()



#Municipio com o maior preço medio de gasolina

maior_preco <- group_by(gasolina,Municipio, Estado)%>%summarise(Total=mean(Preco))

maior_preco%>%
  arrange(desc(Total))%>%head(100)%>%
  ggplot(aes(Estado,Total))+
  theme_light()+
  scale_y_continuous(breaks = seq(6.8,8,0.2), limits = c(6.8,8))+
  labs(y="Preço", title= "Variação do Preço", subtitle = "os 100 municipios(separados por estado) com a gasolina mais cara")+
  geom_violin()


#Municipio com o menor preço medio de gasolina

menor_preco <- group_by(gasolina,Municipio, Estado)%>%summarise(Total=mean(Preco))

menor_preco%>%
  arrange(Total)%>%head(100)%>%
  ggplot(aes(Estado,Total,color=Estado))+
  theme_light()+
  scale_y_continuous(breaks = seq(5.9,6.5,0.1), limits = c(5.9,6.5))+
  labs(y="Preço", title= "Variação do Preço", subtitle = "os 100 municipios(separados por estado) com a gasolina mais barata")+
  geom_point(size=5)

