# Limpar
rm(list=ls())


# Desbloquear para definir diretório de trabalho

setwd("C:/Users/Caio Azevedo/Documents/Documentos Caio/Github/ferramentas_qualidade")

# Desbloquear para instalar pacotes a serem utilizados

#install.packages("read.dbc")

# Carregando pacotes a serem utilizados

library(read.dbc)


# Carregamento e configuração dos dados----

base<-read.dbc("C:/Users/Caio Azevedo/Documents/Documentos Caio/Github/ferramentas_qualidade/Data/DNPB2017.dbc")

#Exportando----
write.table(base,file='Data/base.csv',sep=';')
