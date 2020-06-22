# Limpar
rm(list=ls())


# Desbloquear para definir diretório de trabalho

#setwd("C:/Users/Caio Azevedo/Documents/Documentos Caio/Github/ferramentas_qualidade/Figuras")

# Desbloquear para instalar pacotes a serem utilizados

#install.packages("dplyr")
#install.packages("xtable")
#install.packages("ggplot2")
#install.packages("stargazer")
#install.packages("e1071")

# Carregando pacotes a serem utilizados


library(dplyr)
library(xtable)
library(ggplot2)
library(stargazer)
library(e1071)

# Configuração dos gráficos

cleanup = theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_line(color = "white"),
                panel.background = element_blank(),
                legend.position = "bottom",
                axis.line = element_line(color = "black"),
                axis.title = element_text(size = 16),
                legend.text = element_text(size = 14),
                axis.text = element_text(size = 14))



# Carregamento e configuração dos dados----

# Exportando os dados disponíveis no GitHub

site <- "https://raw.githubusercontent.com/caio-azevedo/ferramentas_qualidade/master/Data/base.csv"

dados<- read.table(site, header=T, sep=";")

base<-select(dados,NUMERODN,IDADEMAE, ESTCIVMAE, ESCMAE, GESTACAO, 
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


#Tabela 2: Distribuição de frequências para o peso dos recém-nascidos ----

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


# Figura 1: Histograma do peso dos recém-nascidos----

ggplot(dados, aes(PESO))+geom_histogram(colour="black", fill="grey")+ 
  xlab("Peso (em gramas)") + ylab("Frequência") + cleanup 

#Desbloquear para gerar figura

#dev.copy(jpeg,"fig1.jpeg")
#dev.off()

# Tabela 3: Distribuição de frequências para a duração da gestação----

dados<-dados %>% 
  mutate("GESTACAO"=as.character(GESTACAO)) %>% 
  mutate("GESTACAO"=as.numeric(GESTACAO))


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
  mutate("Proporção"= round((Frequência.x/Frequência.y)*100, digits=3))

rm(freq.gest,freq.gest_BP)

# Tabela 4: Distribuição de frequências - Número de consultas pré-natal ----

dados<-dados %>% 
  mutate("CONSULTAS"=as.character(CONSULTAS)) %>% 
  mutate("CONSULTAS"=as.numeric(CONSULTAS))

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

# Tabela 4: Distribuição de frequências - Anos de escolaridade da mãe ----

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


# Tabela 4: Distribuição de frequências - Estado Civil da Mãe ----

freq.estciv<-dados %>% 
  group_by(ESTCIVMAE) %>% 
  summarise("Frequência"=n()) %>% 
  mutate("Frequência relativa (%)"= round(Frequência/sum(Frequência)*100,digits=3)) %>%  
  mutate("Estado Civil da mãe"=factor(ESTCIVMAE,levels=c(1,2,3,4,5,9),
                                      labels=c("Solteira","Casada","Viúva",
                                               "Divorciada","União consensual","Ignorado"))) %>% 
  select(`Estado Civil da mãe`,`Frequência`,`Frequência relativa (%)`)

freq.estciv_BP<-dados %>% 
  filter(RN=="BP") %>% 
  group_by(ESTCIVMAE) %>% 
  summarise("Frequência"=n()) %>% 
  mutate("Frequência relativa (%)"= round(Frequência/sum(Frequência)*100,digits=3)) %>% 
  mutate("Estado Civil da mãe"=factor(ESTCIVMAE,levels=c(1,2,3,4,5,9),
                                      labels=c("Solteira","Casada","Viúva",
                                               "Divorciada","União consensual","Ignorado"))) %>% 
  select("Estado Civil da mãe", "Frequência", "Frequência relativa (%)")

freq.estciv_peso<-inner_join(freq.estciv_BP,freq.estciv, c("Estado Civil da mãe"))

freq.estciv_peso<-freq.estciv_peso %>% 
  mutate("Proporção"= round((Frequência.x/Frequência.y)*100, digits=3))
rm(freq.estciv, freq.estciv_BP)

# Tabela 4: Distribuição de frequências - Idade da mãe ----

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

#Junção para formação da Tabela 4: Distribuição de frequências

freq.cons_peso<-freq.cons_peso %>% 
  rename("Variável"="Número de consultas")
freq.esc_peso<-freq.esc_peso %>% 
  rename("Variável"="Escolaridade da mãe")
freq.estciv_peso<-freq.estciv_peso %>% 
  rename("Variável"="Estado Civil da mãe")
freq.idade_peso<-freq.idade_peso %>% 
  rename("Variável"="Idade da mãe")
tabela<-rbind(freq.cons_peso,freq.esc_peso,freq.estciv_peso,freq.idade_peso)
tabela<-tabela %>% 
  select("Variável","Frequência relativa (%).x","Frequência relativa (%).y",
         "Proporção")

# Tabela 5: Resultados dos modelos de Escolha Binária ----

dados<-dados %>% 
  mutate("BPN"= ifelse(PESO>= 2500,0,1)) %>% 
  mutate("estciv"=ifelse(ESTCIVMAE==1|ESTCIVMAE==4,1,0)) %>% 
  mutate("gest31"=ifelse(GESTACAO<=3,1,0)) %>% 
  mutate("gest36"=ifelse(GESTACAO==4,1,0)) %>% 
  mutate("idade41"=ifelse(IDADEMAE>=41,1,0)) %>% 
  mutate("idade14"=ifelse(IDADEMAE<=14,1,0)) %>% 
  mutate("cons03"=ifelse(CONSULTAS<=2,1,0)) %>% 
  mutate("cons06"=ifelse(CONSULTAS==3,1,0)) 



mpl<- lm(BPN ~ estciv + gest31 + gest36 + idade14 + idade41 + cons03 + cons06,
         data=dados)

logit <- glm(BPN ~ estciv + gest31 + gest36 + idade14 + idade41 + cons03 + cons06, 
             family = binomial(link = "logit"), 
             data = dados)

probit <- glm(BPN ~ estciv + gest31 + gest36 + idade14 + idade41 + cons03 + cons06 , 
              family = binomial(link = "probit"), 
              data = dados)


# Figura 3: Carta Controle ----

# Tabela da distribuição dos pesos por mês para construção da Figura 3: Carta Controle

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

# Geração de Figura 3

graf<-freq.mes_peso %>% 
  select(Mês, Proporção) %>% 
  mutate("Mês"= as.character.Date(Mês))

ggplot(data=graf, aes(x=Mês, y=Proporção*100)) + geom_point(size=5)+ 
  geom_line(lwd=2) + geom_hline(aes(yintercept = 8), linetype=2, col=4, lwd=1.3)+
  geom_hline(aes(yintercept = 7), linetype=2, col=4, lwd=1.3)+ 
  xlab("Mês") + 
  ylab("Prevalência BPN")+  
  cleanup

#Desbloquear para gerar figura

#dev.copy(jpeg,"fig3.jpeg")
#dev.off()

rm(graf)


#Figura 4: Histograma dos pesos por número de consultas ----

consultas1<-dados %>% 
  filter(CONSULTAS==1)

ggplot(consultas1, aes(PESO))+geom_histogram(colour="black", fill="grey")+ 
  xlab("Peso (em gramas)") + ylab("Frequência") + cleanup 

#Desbloquear para gerar figura

#dev.copy(jpeg,"fig4(a).jpeg")
#dev.off()

consultas2<-dados %>% 
  filter(CONSULTAS==2)

ggplot(consultas2, aes(PESO))+geom_histogram(colour="black", fill="grey")+ 
  xlab("Peso (em gramas)") + ylab("Frequência") + cleanup 

#Desbloquear para gerar figura

#dev.copy(jpeg,"fig4(b).jpeg")
#dev.off()

consultas3<-dados %>% 
  filter(CONSULTAS==3)

ggplot(consultas3, aes(PESO))+geom_histogram(colour="black", fill="grey")+ 
  xlab("Peso (em gramas)") + ylab("Frequência") + cleanup 

#Desbloquear para gerar figura

#dev.copy(jpeg,"fig4(c).jpeg")
#dev.off()

consultas4<-dados %>% 
  filter(CONSULTAS==4)

ggplot(consultas4, aes(PESO))+geom_histogram(colour="black", fill="grey")+ 
  xlab("Peso (em gramas)") + ylab("Frequência") + cleanup 

#Desbloquear para gerar figura

#dev.copy(jpeg,"fig4(d).jpeg")
#dev.off()

# Gerar as 4 Figuras empilhadas

par(mfrow=c(2,2))
plot(density(consultas1$PESO, kernel = c("gaussian")),main="(a) Nenhuma Consulta",
     xlab="Peso (em gramas)", ylab="Frequência")
plot(density(consultas2$PESO, kernel = c("gaussian")),main="(b) 1 a 3 consultas",
     xlab="Peso (em gramas)", ylab="Frequência")
plot(density(consultas3$PESO, kernel = c("gaussian")),main="(c) 4 a 6 consultas",
     xlab="Peso (em gramas)", ylab="Frequência")
plot(density(consultas4$PESO, kernel = c("gaussian")),main="(d) Mais que 7 consultas",
     xlab="Peso (em gramas)", ylab="Frequência")



#Desbloquear para gerar figura

#dev.copy(jpeg,"fig4.jpeg")
#dev.off()

graphics.off()

rm(consultas1, consultas2, consultas3, consultas4)

# Tabela 6: Análise descritiva----

descr<-dados %>% 
  group_by(CONSULTAS) %>%
  summarise("Média"=mean(PESO),
            "Mínimo"=min(PESO),
            "Máximo"=max(PESO),
            "Mediana"=median(PESO),
            "Desvio Padrão"=sqrt(var(PESO)),
            "Assimetria"=skewness(PESO),
            "Curtose"=kurtosis(PESO))

# Saída das tabelas em Latex ----

# Tabela 2

print(xtable(freq.peso, caption = "Distribuição de frequências para 
             o peso dos recém-nascidos no Estado da Paraíba em 2017", 
             label = "tab2"),
      caption.placement = "top",
      include.rownames = FALSE,
      format.args = list(big.mark = ".", decimal.mark = ","))

# Tabela 3

print(xtable(freq.gest_peso, caption = "Distribuição de frequências para a 
             duração da gestação em 2017 no Estado da Paraíba.", 
             label = "tab3"),
      caption.placement = "top",
      include.rownames = FALSE,
      format.args = list(big.mark = ".", decimal.mark = ","))

# Tabela 4

print(xtable(tabela, caption = "Distribuição de frequência (ano 2017)", 
             label = "tab4"),
      caption.placement = "top",
      include.rownames = FALSE,
      format.args = list(big.mark = ".", decimal.mark = ","))

# Tabela 5

stargazer(mpl,logit,probit, decimal.mark = ",")

# Tabela 6

print(xtable(descr, caption = "Análise descritiva (ano 2017)", 
             label = "tab6"),
      caption.placement = "top",
      include.rownames = FALSE,
      format.args = list(big.mark = ".", decimal.mark = ","))