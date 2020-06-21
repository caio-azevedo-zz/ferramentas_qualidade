# Limpar
rm(list=ls())


# Diretório
setwd("C:/Users/Caio Azevedo/Documents/Documentos Caio/Github/Orientacao_especializacao")


# Pacotes
library(read.dbc)
library(dplyr)
library(xtable)
library(ggplot2)
library(e1071)

#Configuração dos gráficos

cleanup = theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_line(color = "white"),
                panel.background = element_blank(),
                legend.position = "bottom",
                axis.line = element_line(color = "black"),
                axis.title = element_text(size = 16),
                legend.text = element_text(size = 14),
                axis.text = element_text(size = 14))



# Carregamento e configuração dos dados----
base<-read.dbc("DNPB2017.dbc")

base<-select(base,NUMERODN,IDADEMAE, ESTCIVMAE, ESCMAE, GESTACAO, 
              CONSULTAS, 
              PESO,DTNASC)
base<-base %>% 
  mutate("Mês"=substr(DTNASC,3,4)) %>% 
  select(-DTNASC)

dados<-base[complete.cases(base),]


dados<-dados %>% 
  mutate("PESO"=as.character(PESO)) %>% 
  mutate("PESO"=as.numeric(PESO)) %>% 
  mutate("SG"=factor(GESTACAO,levels=c(1,2,3,4,5,6,9),
                     labels=c("Menos de 22","22 a 27 ","28 a 31 ","32 a 36 ",
                              "37 a 41 ","42 ou mais","Ignorado"))) %>% 
  mutate("RN"= ifelse(PESO>= 2500,"PN",ifelse(PESO<2500,"BP",NA))) 


#Tabela de frequência para o peso do recem-nascido----

dados<-dados %>% 
  mutate("Peso"= ifelse(PESO>=0 & PESO<1500,1,
                        ifelse(PESO>=1500 & PESO<2500,2,
                               ifelse(PESO>=2500 & PESO<3000,3,
                                      ifelse(PESO>=3000 & PESO<3500,4,
                                             ifelse(PESO>=3500 & PESO<4000,5,
                                                    ifelse(PESO>=4000,6,0)))))))
freq.peso<-dados %>% 
  group_by(Peso) %>% 
  summarise(Freq=n()) %>% 
  mutate(Perc= round(Freq/sum(Freq)*100,digits = 3),
         Ac = round(cumsum(Perc),digits = 3)) %>% 
  mutate("Peso do recém-nascido"=factor(Peso,levels=c(1,2,3,4,5,6),
                                        labels=c(
                                          "Menor que 1500gr" ,
                                          "De 1500gr à 2499gr" ,
                                          "De 2500gr à 2999gr" ,
                                          "De 3000gr à 3499gr",
                                          "De 3500gr à 3999gr",
                                          "Maior que 4000gr"))) %>% 
  select("Peso do recém-nascido","Freq","Perc","Ac")  

ggplot(dados, aes(PESO))+geom_histogram(colour="black", fill="grey")+ 
  xlab("Peso (em gramas)") + ylab("Frequência") + cleanup 
dev.copy(pdf,"101.pdf")
dev.off()

#Tabela de frequência para as consultas----

freq.cons<-dados %>% 
  group_by(CONSULTAS) %>% 
  summarise("Frequência"=n()) %>% 
  mutate("Frequência relativa (%)"= round(Frequência/sum(Frequência)*100,digits=3)) %>%  
  mutate("Número de consultas"=factor(CONSULTAS,levels=c(1,2,3,4,9),
                                      labels=c("Nenhuma","De 1 a 3","De 4 a 6",
                                               "7 e mais","Ignorado"))) %>% 
  select(`Número de consultas`,`Frequência`,`Frequência relativa (%)`)

freq.cons_BP<-dados %>% 
  filter(RN=="BP") %>% 
  group_by(CONSULTAS) %>% 
  summarise("Frequência"=n()) %>% 
  mutate("Frequência relativa (%)"= round(Frequência/sum(Frequência)*100,digits=3)) %>% 
  mutate("Número de consultas"=factor(CONSULTAS,levels=c(1,2,3,4,9),
                                      labels=c("Nenhuma","De 1 a 3","De 4 a 6",
                                               "7 e mais","Ignorado"))) %>% 
  select("Número de consultas", "Frequência", "Frequência relativa (%)")

freq.cons_peso<-inner_join(freq.cons_BP,freq.cons, c("Número de consultas"))

freq.cons_peso<-freq.cons_peso %>% 
  mutate("Proporção"= round((Frequência.x/Frequência.y)*100, digits=3))
rm(freq.cons, freq.cons_BP)

#Gráficos da distribuição de consultas----

consultas1<-dados %>% 
  filter(CONSULTAS==1)

ggplot(consultas1, aes(PESO))+geom_histogram(colour="black", fill="grey")+ 
  xlab("Peso (em gramas)") + ylab("Frequência") + cleanup 
dev.copy(pdf,"102.pdf")
dev.off()

consultas2<-dados %>% 
  filter(CONSULTAS==2)

ggplot(consultas2, aes(PESO))+geom_histogram(colour="black", fill="grey")+ 
  xlab("Peso (em gramas)") + ylab("Frequência") + cleanup 
dev.copy(pdf,"103.pdf")
dev.off()

consultas3<-dados %>% 
  filter(CONSULTAS==3)

ggplot(consultas3, aes(PESO))+geom_histogram(colour="black", fill="grey")+ 
  xlab("Peso (em gramas)") + ylab("Frequência") + cleanup 
dev.copy(pdf,"104.pdf")
dev.off()

consultas4<-dados %>% 
  filter(CONSULTAS==4)

ggplot(consultas4, aes(PESO))+geom_histogram(colour="black", fill="grey")+ 
  xlab("Peso (em gramas)") + ylab("Frequência") + cleanup 
dev.copy(pdf,"105.pdf")
dev.off()

rm(consultas1, consultas2, consultas3, consultas4)

#Análise descritiva das consultas----

descr<-dados %>% 
  group_by(CONSULTAS) %>%
  summarise("Média"=mean(PESO),
            "Mínimo"=min(PESO),
            "Máximo"=max(PESO),
            "Mediana"=median(PESO),
            "Desvio Padrão"=sqrt(var(PESO)),
            "Assimetria"=skewness(PESO),
            "Curtose"=kurtosis(PESO))
  
#Tabela e gráficos da distribuição da duração da gestação----

freq.gest<-dados %>% 
  group_by(GESTACAO) %>% 
  summarise("Frequência"=n()) %>% 
  mutate("Frequência relativa (%)"= round(Frequência/sum(Frequência)*100,
                                          digits=3)) %>%  
  mutate("Semanas de Gestação"=factor(GESTACAO,levels=c(1,2,3,4,5,6,9),
                                      labels=c("Menos de 22","22 a 27 ","28 a 31 ","32 a 36 ",
                                               "37 a 41 ","42 ou mais","Ignorado"))) %>% 
  select(`Semanas de Gestação`,`Frequência`,`Frequência relativa (%)`)

freq.gest_BP<-dados %>% 
  filter(RN=="BP") %>% 
  group_by(SG) %>% 
  summarise("Frequência"=n()) %>% 
  mutate("Frequência relativa (%)"= round(Frequência/sum(Frequência)*100,
                                          digits=3))

freq.gest_peso<-inner_join(freq.gest_BP,freq.gest, c("SG"="Semanas de Gestação"))

freq.gest_peso<-freq.gest_peso %>% 
  mutate("Proporção"= round((Frequência.x/Frequência.y), digits=3))

rm(freq.gest,freq.gest_BP)


#Tabela da distribuição dos pesos por mês----

freq.mes<-dados %>% 
  group_by(Mês) %>% 
  summarise("Frequência"=n()) %>% 
  mutate("Frequência relativa (%)"= round(Frequência/sum(Frequência)*100,
                                          digits=3)) %>%  
  select(`Mês`,`Frequência`,`Frequência relativa (%)`)

freq.mes_BP<-dados %>% 
  filter(RN=="BP") %>% 
  group_by(Mês) %>% 
  summarise("Frequência"=n()) %>% 
  mutate("Frequência relativa (%)"= round(Frequência/sum(Frequência)*100,
                                          digits=3))

freq.mes_peso<-inner_join(freq.mes_BP,freq.mes, c("Mês"))

freq.mes_peso<-freq.mes_peso %>% 
  mutate("Proporção"= round((Frequência.x/Frequência.y), digits=3))

rm(freq.mes, freq.mes_BP)

#Gráfico do BPN por mês----

graf<-freq.mes_peso %>% 
  select(Mês, Proporção) %>% 
  mutate("Mês"= as.character.Date(Mês))

ggplot(data=graf, aes(x=Mês, y=Proporção*100)) + geom_point(size=5)+ 
  geom_line(lwd=2) + geom_hline(aes(yintercept = 8), linetype=2, col=4, lwd=1.3)+
  geom_hline(aes(yintercept = 7), linetype=2, col=4, lwd=1.3)+ 
  xlab("Mês") + 
  ylab("Prevalência BPN")+  
  cleanup
dev.copy(pdf,"106.pdf")
dev.off()

rm(graf)
#Tabela de frequência para a idade da mãe----

dados<-dados %>% 
  mutate("IDADEMAE"=as.character(IDADEMAE)) %>% 
  mutate("IDADEMAE"=as.numeric(IDADEMAE))

dados<-dados %>% 
  mutate("IDADE"= ifelse(IDADEMAE>=8 & IDADEMAE<15,1,
                         ifelse(IDADEMAE>=15 & IDADEMAE<19,2,
                                ifelse(IDADEMAE>=19 & IDADEMAE<25,3,
                                       ifelse(IDADEMAE>=25 & IDADEMAE<31,4,
                                              ifelse(IDADEMAE>=31 & IDADEMAE<41,5,
                                                     ifelse(IDADEMAE>=41 ,6,0)))))))
freq.idade<-dados %>% 
  group_by(IDADE) %>% 
  summarise("Frequência"=n()) %>% 
  mutate("Frequência relativa (%)"= round(Frequência/sum(Frequência)*100,digits=3)) %>%  
  mutate("Idade da mãe"=factor(IDADE,levels=c(1,2,3,4,5,6),
                               labels=c(
                                 "De 8 a 14 anos" ,
                                 "De 15 a 18 anos" ,
                                 "De 19 a 24 anos" ,
                                 "De 25 a 30 anos" ,
                                 "De 31 a 40 anos",
                                 " Mais de 41 anos "))) %>% 
  select("Idade da mãe","Frequência","Frequência relativa (%)")

freq.idade_BP<-dados %>% 
  filter(RN=="BP") %>% 
  group_by(IDADE) %>% 
  summarise("Frequência"=n()) %>% 
  mutate("Frequência relativa (%)"= round(Frequência/sum(Frequência)*100,digits=3)) %>%  
  mutate("Idade da mãe"=factor(IDADE,levels=c(1,2,3,4,5,6),
                               labels=c(
                                 "De 8 a 14 anos" ,
                                 "De 15 a 18 anos" ,
                                 "De 19 a 24 anos" ,
                                 "De 25 a 30 anos" ,
                                 "De 31 a 40 anos",
                                 " Mais de 41 anos "))) %>%  
    select("Idade da mãe", "Frequência", "Frequência relativa (%)")

freq.idade_peso<-inner_join(freq.idade_BP,freq.idade, 
                            c("Idade da mãe"))

freq.idade_peso<-freq.idade_peso %>% 
  mutate("Proporção"= round((Frequência.x/Frequência.y)*100, digits=3))
rm(freq.idade, freq.idade_BP)


#Tabela de frequência da escolaridade da mãe----
freq.esc<-dados %>% 
  group_by(ESCMAE) %>% 
  summarise("Frequência"=n()) %>% 
  mutate("Frequência relativa (%)"= round(Frequência/sum(Frequência)*100,digits=3)) %>%  
  mutate("Escolaridade da mãe"=factor(ESCMAE,levels=c(1,2,3,4,5,9),
                                      labels=c("Nenhum","1 a 3 anos","4 a 7 anos",
                                               "8 a 11 anos ","12 ou mais ","Ignorado"))) %>% 
  select(`Escolaridade da mãe`,`Frequência`,`Frequência relativa (%)`)

freq.esc_BP<-dados %>% 
  filter(RN=="BP") %>% 
  group_by(ESCMAE) %>% 
  summarise("Frequência"=n()) %>% 
  mutate("Frequência relativa (%)"= round(Frequência/sum(Frequência)*100,digits=3)) %>% 
  mutate("Escolaridade da mãe"=factor(ESCMAE,levels=c(1,2,3,4,5,9),
                                      labels=c("Nenhum","1 a 3 anos","4 a 7 anos",
                                               "8 a 11 anos ","12 ou mais ","Ignorado"))) %>% 
  select("Escolaridade da mãe", "Frequência", "Frequência relativa (%)")

freq.esc_peso<-inner_join(freq.esc_BP,freq.esc, c("Escolaridade da mãe"))

freq.esc_peso<-freq.esc_peso %>% 
  mutate("Proporção"= round((Frequência.x/Frequência.y)*100, digits=3))
rm(freq.esc, freq.esc_BP)

#Boxplot Consultas----

dados<-dados %>% 
  mutate("Consultas"=factor(CONSULTAS,levels=c(1,2,3,4,9),
                            labels=c("Nenhuma","De 1 a 3","De 4 a 6",
                                     "7 e mais","Ignorado")))
ggplot(dados, aes(Consultas,PESO))+geom_boxplot(colour="black", fill="grey")+ 
  xlab("Consultas pré-natal") + ylab("Peso") + cleanup
dev.copy(pdf,"107.pdf")
dev.off()
  

#Tabela----
freq.cons_peso<-freq.cons_peso %>% 
  rename("Variável"="Número de consultas")
freq.esc_peso<-freq.esc_peso %>% 
  rename("Variável"="Escolaridade da mãe")
freq.idade_peso<-freq.idade_peso %>% 
  rename("Variável"="Idade da mãe")
tabela<-rbind(freq.cons_peso,freq.esc_peso,freq.idade_peso)
tabela<-tabela %>% 
  select("Variável","Frequência relativa (%).x","Frequência relativa (%).y",
         "Proporção")


#Saída das tabelas----

print(xtable(freq.peso, caption = "Distribuição de frequências para 
             o peso dos recém-nascidos no Estado da Paraíba em 2017", 
             label = "tab1"),
      caption.placement = "top",
      include.rownames = FALSE,
      format.args = list(big.mark = ".", decimal.mark = ","))

print(xtable(freq.gest_peso, caption = "Distribuição de frequências para a 
             duração da gestação em 2017 no Estado da Paraíba.", 
             label = "tab3"),
      caption.placement = "top",
      include.rownames = FALSE,
      format.args = list(big.mark = ".", decimal.mark = ","))

print(xtable(tabela, caption = "Distribuição de frequência (ano 2017)", 
             label = "tab4"),
      caption.placement = "top",
      include.rownames = FALSE,
      format.args = list(big.mark = ".", decimal.mark = ","))

print(xtable(descr, caption = "Análise descritiva (ano 2017)", 
             label = "tab4"),
      caption.placement = "top",
      include.rownames = FALSE,
      format.args = list(big.mark = ".", decimal.mark = ","))