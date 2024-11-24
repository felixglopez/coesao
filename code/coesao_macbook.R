
#exploração do survey sobre confiança social, pobreza e Estado
#survey realizado pela Quaest em agosto/2022
##FELIX LOPEZ - IPEA
library()$results[,1]
library(rlang)
library(devtools)
library(tidyr)
library(tidyverse)
library(haven)
library(readr)
library(labelled)
library(janitor)
library(questionr)
library(cowplot)
library(tibble)
library(magrittr)
library(testthat)
library(roxygen2)
library(Hmisc)
library(knitr)
library(devtools)
library(surveytoolbox)
library(dplyr)
library(sjmisc)
library(ggplot2)
library(missMDA)
library(sjlabelled)
library(datawizard)
library(sjstats)
library(likert)
library(sjPlot)
library(parameters)
library(rmarkdown)
library(tinytex)

glimpse(coesao_experimental$renda)

#abrindo o banco original


library(haven)
coesao <- read_sav("~/Library/CloudStorage/OneDrive-Pessoal/IPEA/PESQUISAS ATIVAS/CATEDRAS UFRJ_2020/Survey coesao social/Questionário - versao final PDF/COESAO_SOCIAL_OUT22.sav")
View(coesao)

coesao <- read_spss ("coesao_social_out22.sav")
coesao <- read_sav("coesao_social_out22.sav")


describe(coesao_experimental$rendaf)

#criando o banco de uso
coesao_experimental <- coesao

#criando um ID único por linha, pois o banco veio sem
tibble::rowid_to_column(coesao_experimental, "ID")

#achar variáveis ou palavras no questionário
find_var(coesao_experimental, "cientistas")

#vendo o nome das variáveis
names(coesao_experimental)

#convertendo 888 e 999 em NA
coesao_experimental[coesao_experimental == 888] <- NA
coesao_experimental[coesao_experimental == 999] <- NA

#quartis do tempo de duração das entrevistas
summary(coesao_experimental$Duration_numeric)

#para ver as características de cada variável, label, rotulos
str(coesao_experimental$comunidade)
str(coesao_experimental$relig)
class(coesao_experimental$comunidade)

#vendo as primeira linhas do banco
head(coesao_experimental, n =  20)

skimr::skim(coesao_experimental$amostra)

names(coesao_experimental)

View(coesao_experimental)
#ANALISANDO MISSINGS

#somando misssing em cada variáveis
sum(is.na(coesao_experimental$I_1_conf ))

#proporcao de missing no total de casos 
mean(is.na(coesao_experimental$I_1_conf))

#distribuiçao por escolaridade e renda (e outras quaisquer)
coesao_experimental %>%
  filter(rendaf >= 20000)
  #group_by(renda) %>% 
  plot_frq(relig) %>% 
  plot_grid()



#pacote para testar a consistência interna de variáveis
install.packages("psych")
library(psych)

####ANALISANDO CONSISTÊNCIA INTERNA DAS RESPOSTAS###########

#ESSE PACOTE E VARIÁVEL ANALISA O ALPHA DE CRONBACH PARA MEDIR
#consistencia interna das variáveis de confiança interpessoal
alpha(coesao_experimental[,c("I_1_conf", "I_2_conf", "I_3_conf", "I_4_conf", "I_5_conf", "I_6_conf")])


#consistencia interna das variáveis de corrupção e justiça
#procedimental
#baixa consistencia
alpha(coesao_experimental[,c("I_1_corrup", "I_2_corrup", "I_3_corrup", "I_4_corrup")])

#baixa consistencia
alpha(coesao_experimental[,c("I_2_corrup", "I_2_injus")])

alpha(coesao_experimental[,c("T_lula_bolso_2", "beneficios"), ckeck.keys = TRUE])  


#para ver os rótulos das questões

coesao$I_5_conf %>% attr('labels')


##CRIAR UM HTML COM TODAS AS RESPOSTA TABULADAS ABSOLUTAS E FREQUENCIA###
coesao_experimental %>%   sjPlot::view_df(
                            show.frq = T, 
                            show.id = TRUE,
                            show.prc = TRUE, 
                            verbose = T, 
                            use.viewer = F, 
                            alternate.rows = T, 
                            show.values = T,
                            show.na= T,
                            wrap.labels = 70,
                            show.string.values = T,
                            CSS = list(css.table = "border: 2px solid;",
                                       css.tdata = "border: 1px solid;",
                                       css.arc = "color:blue;"),
                             file = "~/Documents/GitHub/atlas-estado/felix/coesao/coesao_experimental.html")
#para ver os noemes das colunas
colnames(coesao_experimental)

#para procurar por alguma palavra no questionário
coesao_experimental %>% look_for("mulheres", labels = TRUE, ignore.case = TRUE, details = TRUE)

###INICIANDO###
#PLOTANDO VARIÁVEIS SOCIODEMOGRÁFICAS

plot_frq(coesao_experimental$sit,
         wrap.title = 120,
         wrap.labels = 120,
         #show.ci = T,
         geom.colors = "grey",
         coord.flip = T) +
  theme(plot.title = element_text(hjust = 0.5),text = element_text(size =12)) +  
  ggtitle("Ocupação")


plot_frq(coesao_experimental$pea,
         wrap.title = 120,
         wrap.labels = 120,
         #show.ci = T,
         vjust = "top",
         geom.colors = "grey") +
  theme(plot.title = element_text(hjust = 0.5),text = element_text(size =12)) +  
  ggtitle("População Economicamente Ativa")


plot_frq(coesao_experimental$renda,
         wrap.title = 120,
         wrap.labels = 120,
         #show.ci = T,
         geom.colors = "grey",
         vjust = "top")
         #coord.flip = T) +
  theme(plot.title = element_text(hjust = 0.5),text = element_text(size =12)) +  
  ggtitle("Renda")

  
plot_frq(coesao_experimental$rendaf,
           wrap.title = 120,
           wrap.labels = 120,
           #show.ci = T,
           geom.colors = "grey",
           vjust = "top",
           coord.flip = T) +
  theme(plot.title = element_text(hjust = 0.5),text = element_text(size =12)) +  
    ggtitle("Renda familiar")
  

plot_frq(coesao_experimental$cor,
         wrap.title = 120,
         wrap.labels = 120,
         #show.ci = T,
         geom.colors = "grey",
         vjust = "outward",
         coord.flip = F) +
  theme(plot.title = element_text(hjust = 0.5),text = element_text(size =12)) +  
  ggtitle("Cor")


plot_frq(coesao_experimental$relig,
         wrap.title = 120,
         wrap.labels = 120,
         #show.ci = T,
         geom.colors = "grey",
         vjust = "top",
         coord.flip = T) +
  theme(plot.title = element_text(hjust = 0.5),text = element_text(size =12)) +  
  ggtitle("Religião")


#criar blocos de questões#################



####CONFIANCA INTERPESSOAL, INSTITUCIONAL, IMPARCIALIDADE E CORRUPÇÃO####


#CRIANDO O BLOCO CONFIANÇA
confianca_interpessoal  <- coesao_experimental %>% 
  select(23:28)

head(confianca_interpessoal, n=10)
summary(confianca_interpessoal)
names(coesao_experimental)

#grafico barras com distribuiçao das respostas
plot_frq(confianca_interpessoal, I_1_conf, show.ci = T)
plot_frq(confianca_interpessoal, I_2_conf, show.ci = T)
plot_frq(confianca_interpessoal, I_3_conf, show.ci = T)
plot_frq(confianca_interpessoal, I_4_conf, show.ci = T)
plot_frq(confianca_interpessoal, I_5_conf, show.ci = T)
plot_frq(confianca_interpessoal, I_6_conf, show.ci = T)

#uma forma mais simples de ir alternado as variaveis
#basta alterarr o numero apos o nome do dataset
plot_frq(confianca_interpessoal, 1, show.n = T, sort.frq = "none")

#para plotar qualquer questão
coesao_experimental %>% 
  group_by(regiao) %>% 
plot_frq(I_5_conf, 
         show.n = T, 
         show.values = TRUE, 
         sort.frq = "none", 
         show.ci = TRUE,
         show.na = TRUE,
         coord.flip = F,
         vjust = "outward",
         geom.colors = "black")




#ver o valor das variáveis
get_values(coesao_experimental$I_1_corrup)

plot_grpfrq(confianca_interpessoal,
            type = c("bar"),
            bar.pos = c("dodge"),
            weight.by = NULL,
            intr.var = NULL,
            title = "",
            title.wtd.suffix = NULL,
            legend.title = NULL,
            axis.titles = NULL,
            geom.colors = "Paired",
            facet.grid = F,
            show.grpcnt = T,
            coord.flip = F)


plot_grpfrq(coesao_experimental$I_1_conf, coesao_experimental$regiao,
            type = c("bar"),
            bar.pos = c("dodge"),
            weight.by = NULL,
            intr.var = NULL,
            title = "",
            title.wtd.suffix = NULL,
            legend.title = NULL,
            axis.titles = NULL,
            geom.colors = "Paired",
            facet.grid = T,
            show.grpcnt = T,
            show.axis.values = F,
            show.na = T,
            vjust = "outward",
            coord.flip = F)

plot_stackfrq(confianca_interpessoal,# coesao_experimental$regiao,
  title = "Confiança interpessoal",
  legend.title = NULL,
  legend.labels = NULL,
  axis.titles = NULL,
  axis.labels = NULL,
  weight.by = NULL,
  sort.frq = NULL,
  wrap.title = 50,
  wrap.labels = 20,
  wrap.legend.title = 30,
  wrap.legend.labels = 28,
  geom.size = 0.5,
  geom.colors = "Paired",
  show.prc = TRUE,
  show.n = F,
  show.total = TRUE,
  show.axis.prc = TRUE,
  show.legend = TRUE,
  grid.breaks = 0.2,
  expand.grid = FALSE,
  digits = 1,
  vjust = "center",
  coord.flip = T)

###ESCALA LIKERT DA SEIS QUESTÕES SOBRE CONFIANÇCA INTERPESSOAL
plot_likert(confianca_interpessoal,
            title = "Confiança interpessoal",
            legend.title = "Grau de confiança",
            axis.labels = c("Brasileiros", "Família (pais e irmãos)", "Amigos", "Vizinhos", "Membros de sua igreja", 
                            "Pessoas que encontra pela primeira vez"),
            catcount = 4,
            wrap.labels = 60,
            geom.colors = "Spectral",
            reverse.colors = T,
            show.legend = T,
            grid.range = 0.9,
            sort.frq = "pos.desc"
            #values = "sum.outside"
            )



#CRIANDO O BLOCO CONFIANÇA
confianca_institucional  <- coesao_experimental %>% 
  select(29:38)

glimpse(confianca_institucional)

names(coesao_experimental)

plot_likert(confianca_institucional,
            title = "Confiança em instituições",
            legend.title = "Grau de confiança",
            axis.labels = c("Igreja", "Polícia", "Militares", "Congresso Nacional", "Funcionários Públicos",
                            "SUS", "Cientistas", "Judiciário", "Governo Federal", "Partidos Políticos"),
            catcount = 4,
            wrap.labels = 60,
            geom.colors = "Spectral",
            reverse.colors = T,
            show.legend = T,
            grid.range = 0.9,
            sort.frq = "pos.desc"
            #values = "sum.outside"
) + theme(legend.position = "bottom")



###ESCALA LIKERT DA QUATRO QUESTÕES SOBRE CORRUPÇÃO
corrupcao_institucional <- coesao_experimental %>% 
  select(39:42)
glimpse(corrupcao_institucional)


plot_likert(corrupcao_institucional,
            title = "Grau de corrupção em instituições",
            legend.title = "Nível de corrupção",
            axis.labels = c("Policiais", "Juízes", "Políticos", "Burocratas"),
            catcount = 4,
            wrap.labels = 60,
            geom.colors = "Spectral",
            reverse.colors = T,
            show.legend = T,
            grid.range = 0.9,
            sort.frq = "pos.desc",
            #values = "sum.outside"
)


###ESCALA LIKERT DA QUATRO QUESTÕES SOBRE IMPARCIALIDADE INSTITUCIONAL###
imparcialidade_institucional <- coesao_experimental %>% 
  select(43:46)
glimpse(imparcialidade_institucional)

plot_likert(imparcialidade_institucional,
            title = "Decisões injustas nas instituições",
            legend.title = "Frequência da injustiça",
            axis.labels = c("Policiais", "Juízes", "Políticos", "Burocratas"),
            catcount = 4,
            wrap.labels = 60,
            geom.colors = "Spectral",
            reverse.colors = T,
            show.legend = T,
            grid.range = 0.9,
            sort.frq = "pos.desc",
            #values = "sum.outside"
)



#barras empilhadas da confiança institucional
coesao_experimental %>%
  #group_by(sexo) %>%
  select(27:36) %>% 
  plot_stackfrq(title = "Confiança institucional",
                legend.title = NULL,
                legend.labels = NULL,
                axis.titles = NULL,
                axis.labels = NULL,
                weight.by = NULL,
                sort.frq = NULL,
                wrap.title = 50,
                wrap.labels = 60,
                wrap.legend.title = 30,
                wrap.legend.labels = 28,
                geom.size = 0.5,
                geom.colors = "Paired",
                show.prc = TRUE,
                show.n = F,
                show.total = TRUE,
                show.axis.prc = TRUE,
                show.legend = TRUE,
                grid.breaks = 0.2,
                expand.grid = FALSE,
                digits = 1,
                vjust = "center",
                coord.flip = T)

#idem ao item anterior
coesao_experimental %>%
  group_by(sexo) %>%
  select(27:36) %>% 
  plot_stackfrq()


###Plotando gráficos agrupados por regiões, sexo, religião etc.###

#confiança interpessoal, sexo
plot_xtab(coesao_experimental$sexo, coesao_experimental$I_1_conf, margin = "row", bar.pos = "stack",
          show.summary = TRUE, coord.flip = F)

#confiança interpessoal, região
plot_xtab(coesao_experimental$regiao, coesao_experimental$I_1_conf, margin = "row", bar.pos = "stack",
          show.summary = TRUE, coord.flip = F)


plot_xtab(coesao_experimental$zona , coesao_experimental$I_1_conf, margin = "row", bar.pos = "stack",
          show.summary = TRUE, coord.flip = F)


plot_xtab(coesao_experimental$idadef , coesao_experimental$I_1_conf, margin = "row", bar.pos = "stack",
          show.summary = TRUE, coord.flip = F)

plot_xtab(coesao_experimental$esc , coesao_experimental$I_1_conf, margin = "row", bar.pos = "stack",
          show.summary = TRUE, coord.flip = F)

plot_xtab(coesao_experimental$relig , coesao_experimental$I_1_conf, margin = "row", bar.pos = "stack",
          show.summary = TRUE, coord.flip = F)


plot_xtab(coesao_experimental$renda , coesao_experimental$I_1_conf, margin = "row", bar.pos = "stack",
          show.summary = TRUE, coord.flip = F)


plot_xtab(coesao_experimental$cor , coesao_experimental$I_1_conf, margin = "row", bar.pos = "stack",
          show.summary = TRUE, coord.flip = F)



####FIM BLOCO CORRUPÇÃO IMPARCIALIDADE CONFIANÇA ####


###BLOCO PROXIMIDADE SOCIAL###

names(coesao_experimental)

proximidade_casar <- coesao_experimental %>% 
  select(47, 49, 51, 53, 55)

glimpse(proximidade_casar$perfil6_1)

plot_likert(proximidade_casar,
            title = "Proximidade social - filhos",
            legend.title = "Incômodo em relação ao filho",
            axis.labels = c("Homossexual", "Favelado", "Candomblé/Umbanda", "Evangélico", "Pobre"),
            catcount = 4,
            wrap.labels = 60,
            geom.colors = "Spectral",
            reverse.colors = T,
            show.legend = T,
            #grid.range = 0.9,
            sort.frq = "pos.desc"
            #values = "sum.outside"
)

#criando o df da proximidade como amigo em relaçao a alguns tipos sociais            
proximidade_amigo <- coesao_experimental %>% 
  select(48, 50, 52, 54, 56)

plot_likert(proximidade_amigo,
            title = "Proximidade social - amigos",
            legend.title = "Incômodo em relação a ter amigos com tal perfil",
            axis.labels = c("Homossexual", "Favelado", "Candomblé/Umbanda", "Evangélico", "Pobre"),
            catcount = 4,
            wrap.labels = 60,
            geom.colors = "Spectral",
            reverse.colors = T,
            show.legend = T,
            grid.range = 1,
            sort.frq = "pos.desc",
            #values = "sum.outside"
)


plot_likert(imparcialidade_institucional,
            title = "Decisões injustas nas instituições",
            legend.title = "Frequência da injustiça",
            axis.labels = c("Policiais", "Juízes", "Políticos", "Burocratas"),
            catcount = 4,
            wrap.labels = 60,
            geom.colors = "Spectral",
            reverse.colors = T,
            show.legend = T,
            grid.range = 1,
            sort.frq = "pos.desc",
            #values = "sum.outside"
)

#grupo para verificar pertencimento a associações
associativismo <- coesao_experimental %>% 
  select(I_1_org:I_5_org)


plot_likert(associativismo,
            title = "Frequência do associativismo",
            legend.title = "Tipo de associação",
            axis.labels = c("ONGs", "Associação moradores/bairro", "Assoc. Profissionais/Sindicatos", "org. religiosas", "clubes recreativos/esportivos"),
            catcount = 3,
            wrap.labels = 60,
            geom.colors = "Spectral",
            reverse.colors = T,
            show.legend = T,
            grid.range = 1,
            sort.frq = "pos.desc",
            #values = "sum.outside"
)


#distribuição da questao sobre bem-estar meu e dos outros
coesao_experimental %>% 
  #group_by(regiao) %>% 
  plot_frq(bem_estar, 
           show.n = T, 
           show.values = TRUE, 
           sort.frq = "none", 
           show.ci = TRUE,
           show.na = TRUE,
           coord.flip = F,
           vjust = "outward",
           geom.colors = "black")

#bem-estar + renda familiar
sjPlot::sjplot(data = coesao_experimental, coesao_experimental$bem_estar, coesao_experimental$renda, 
               fun = "xtab",
               type  = "bar",
               geom.colors = "Set1")

#BEM-ESTAR E ESCOLARIDADE
sjPlot::sjplot(data = coesao_experimental, coesao_experimental$bem_estar, coesao_experimental$esc, 
               fun = "xtab",
               type  = "bar",
               geom.colors = "Set1"
               )
#BEM-ESTAR E RELIGIAO
sjPlot::sjplot(data = coesao_experimental, coesao_experimental$bem_estar, coesao_experimental$relig, 
               fun = "xtab",
               type  = "bar",
               geom.colors = "Set1",
               expand.grid = TRUE,
)

#BEM-ESTAR E RENDA
sjPlot::sjplot(data = coesao_experimental, coesao_experimental$bem_estar, coesao_experimental$renda, 
               #>>>>>>> 64ed2609c93f31b543113065af5fa2528784f2b9
               fun = "xtab",
               type  = "bar",
               geom.colors = "Set1")

#Responsabilidade por moradores de rua

summary(coesao_experimental$sit_rua1)

sjPlot::sjplot(data = coesao_experimental, coesao_experimental$sit_rua1)

plot_frq(coesao_experimental$sit_rua1, 
         show.n = T, 
         show.values = TRUE, 
         sort.frq = "none", 
         show.ci = TRUE,
         show.na = TRUE,
         coord.flip = F,
         vjust = "outward",
<<<<<<< HEAD
         geom.colors = "black")
=======
         geom.colors = "GRAY")

>>>>>>> c7091beb123d974579da215666492b9fe7ce461e


<<<<<<< HEAD
=======

>>>>>>> c7091beb123d974579da215666492b9fe7ce461e
plot_frq(coesao_experimental$sit_rua3, 
         show.n = T, 
         show.values = TRUE, 
         sort.frq = "none", 
         show.ci = TRUE,
         show.na = TRUE,
         coord.flip = F,
         vjust = "outward",
         geom.colors = "grey")

#cruzando a questão da situação de rua com a renda

sjPlot::sjplot(data = coesao_experimental, coesao_experimental$sit_rua3, coesao_experimental$renda) 


  
  #morador de rua prejudica a sociedade ou só elas mesmas?


#cruzando a questão da situação de rua com a renda
<<<<<<< HEAD
sjPlot::sjplot(data = coesao_experimental, coesao_experimental$sit_rua3, coesao_experimental$renda)

  coesao_experimental %>% 
=======
sjPlot::sjplot(data = coesao_experimental, coesao_experimental$sit_rua3, coesao_experimental$renda, 
<<<<<<< HEAD
=======
=======
>>>>>>> 5ceee4fe34ba39eef0459bcda94af12acdac901f
coesao_experimental %>% 
>>>>>>> f137415b8ca2eaaaeb86bfd93a807da9df5d693e
  #group_by(regiao) %>% 
  plot_frq(bem_estar, 
           show.n = T, 
           show.values = TRUE, 
           sort.frq = "none", 
           show.ci = TRUE,
           show.na = TRUE,
           coord.flip = F,
           vjust = "outward",
           geom.colors = "black")



<<<<<<< HEAD


sjPlot::sjplot(data = coesao_experimental, coesao_experimental$bem_estar, coesao_experimental$renda,
=======
#<<<<<<< HEAD
=======
sjPlot::sjplot(data = coesao_experimental, coesao_experimental$bem_estar, coesao_experimental$renda, 
>>>>>>> 64ed2609c93f31b543113065af5fa2528784f2b9
>>>>>>> c7091beb123d974579da215666492b9fe7ce461e
>>>>>>> f137415b8ca2eaaaeb86bfd93a807da9df5d693e
               fun = "xtab",
               type  = "bar",
               geom.colors = "Set1")

<<<<<<< HEAD
=======
<<<<<<< HEAD
=======
<<<<<<< HEAD
>>>>>>> 5ceee4fe34ba39eef0459bcda94af12acdac901f
>>>>>>> c7091beb123d974579da215666492b9fe7ce461e
>>>>>>> f137415b8ca2eaaaeb86bfd93a807da9df5d693e
#cruzando a questão da situação de rua com a escolaridade
sjPlot::sjplot(data = coesao_experimental, coesao_experimental$sit_rua3, coesao_experimental$esc, 
               fun = "xtab",
               type  = "bar",
               geom.colors = "Set1")


#Responsável por combater a pobreza (primeira opção)

sjPlot::sjplot(data = coesao_experimental, coesao_experimental$responsa_O1)

plot_frq(coesao_experimental$responsa_O1, 
         type = "bar",
<<<<<<< HEAD

sjPlot::sjplot(data = coesao_experimental, coesao_experimental$bem_estar, coesao_experimental$esc,
=======
<<<<<<< HEAD
=======
<<<<<<< HEAD
         
=======
=======
>>>>>>> 5ceee4fe34ba39eef0459bcda94af12acdac901f
sjPlot::sjplot(data = coesao_experimental, coesao_experimental$bem_estar, coesao_experimental$esc, 
>>>>>>> f137415b8ca2eaaaeb86bfd93a807da9df5d693e
               fun = "xtab",
               type  = "bar",
               geom.colors = "Set1")

sjPlot::sjplot(data = coesao_experimental, coesao_experimental$bem_estar, coesao_experimental$relig, 
               fun = "xtab",
               type  = "bar",
               geom.colors = "Set1",
               expand.grid = T)

#Responsabilidade por moradores de rua

summary(coesao_experimental$sit_rua1)

sjPlot::sjplot(data = coesao_experimental, 
               coesao_experimental$sit_rua1)

plot_frq(coesao_experimental$sit_rua1, 
<<<<<<< HEAD
=======
<<<<<<< HEAD
#>>>>>>> 64ed2609c93f31b543113065af5fa2528784f2b9
=======
>>>>>>> 64ed2609c93f31b543113065af5fa2528784f2b9
>>>>>>> 5ceee4fe34ba39eef0459bcda94af12acdac901f
>>>>>>> c7091beb123d974579da215666492b9fe7ce461e
>>>>>>> f137415b8ca2eaaaeb86bfd93a807da9df5d693e
         show.n = T, 
         show.values = TRUE, 
         sort.frq = "none", 
         show.ci = TRUE,
         show.na = TRUE,
         coord.flip = F,
<<<<<<< HEAD
=======
<<<<<<< HEAD
=======
<<<<<<< HEAD
=======
<<<<<<< HEAD
>>>>>>> 5ceee4fe34ba39eef0459bcda94af12acdac901f
>>>>>>> c7091beb123d974579da215666492b9fe7ce461e
>>>>>>> f137415b8ca2eaaaeb86bfd93a807da9df5d693e
         vjust = "inward",
         #hjust = "center",
         expand.grid = T,
         geom.colors = "grey")


#Responsável por combater a pobreza (segunda opção)

#sjPlot::sjplot(data = coesao_experimental, coesao_experimental$responsa_O2)

plot_frq(coesao_experimental$responsa_O2, 
         type = "bar",
         show.n = T, 
         show.values = TRUE, 
         sort.frq = "none", 
         show.ci = TRUE,
         show.na = TRUE,
         coord.flip = F,
         vjust = "inward",
         #hjust = "center",
         expand.grid = T,
         geom.colors = "grey")

#grupo para verificar concordância em relação à opiniões sobre a pobreza e desigualdade social
bloco_pobreza <- coesao_experimental %>% 
  select(I_1_desig:I_4_desig)

summary(bloco_pobreza)

bloco_pobreza$I_4_desig %>% attr('labels')

plot_likert(bloco_pobreza, 
            title = "Concordância em relação à aspectos da pobreza  e desigualdade",
            legend.title = "Questões",
            axis.labels = c("Para acabar com a pobreza e reduzir a desigualdade falta somente vontade política dos governantes",
                            "A pobreza e a desigualdade só podem ser combatidas se as elites e os ricos se organizarem para isso",
                            "Os ricos é que devem pagar pelas políticas de combate à pobreza e desigualdade, mesmo que seja o governo que cuide da gestão dessa política",
                            "Se os mais pobres se esforçassem mais haveria menos pobreza e desigualdade"),
            catcount = 4,
            #cat.neutral = 5, (notar que existe a resposta neutra, mas espontânea)
            wrap.labels = 50,
            geom.colors = "Spectral",
            reverse.colors = T,
            show.legend = T,
            grid.range = 0.9,
            sort.frq = "pos.desc",
            show.n = TRUE
            #values = "sum.outside"
            )



#consistencia interna das questões do bloco pobreza e desigualdade
alpha(bloco_pobreza[,c("I_1_desig", "I_2_desig", "I_3_desig", "I_4_desig")])

alpha(bloco_pobreza[,c("I_2_desig", "I_4_desig")]) #consistencia alta

alpha(bloco_pobreza[,c("I_1_desig", "I_4_desig")])#consistência alta


sjPlot::sjplot(data = coesao_experimental, coesao_experimental$I_1_desig, coesao_experimental$esc, 
              fun = "xtab",
             type  = "bar",
             geom.colors = "Set1")

#para ver como tabela
sjPlot::sjtab(data = coesao_experimental, coesao_experimental$I_1_desig, coesao_experimental$esc, 
               fun = "xtab")
#               type  = "bar",
 #              geom.colors = "Set1")


plot_gpt(coesao_experimental, coesao_experimental$I_1_desig,
         grp = coesao_experimental$renda)



#distribuiçao por escolaridade e renda
coesao_experimental %>% 
  group_by(renda) %>% 
  plot_frq(cor) %>% 
  plot_grid()


escolaridade_renda <- plot_grpfrq(
  var.cnt = coesao_experimental$esc,
  var.grp = coesao_experimental$renda,
  #margin = "row",
  bar.pos = "stack",
  show.summary = T,
  coord.flip = T)

escolaridade_renda


#pivot tables (cross tables) de escolaridade e renda
tab_xtab(
         var.row = coesao_experimental$esc,
         var.col = coesao_experimental$renda,
         show.row.prc = T
         )

#pivot tables (cross tables) de escolaridade e renda
coesao_experimental %>% 
  group_by(coesao_experimental$renda) %>% 
  plot_frq(coesao_experimental$I_1_desig) %>%
  plot_grid()


#teste
coesao_experimental %>% 
  group_by(renda) %>% 
  plot_frq(sit_rua1,
  type = "histogram",
  show.mean = T,
  normal.curve = T) %>% 
  plot_grid()
<<<<<<< HEAD
=======
<<<<<<< HEAD
=======
<<<<<<< HEAD
=======
=======
>>>>>>> 5ceee4fe34ba39eef0459bcda94af12acdac901f
>>>>>>> f137415b8ca2eaaaeb86bfd93a807da9df5d693e
         vjust = "outward",
         geom.colors = "black")


plot_frq(coesao_experimental$sit_rua3, 
         show.n = T, 
         show.values = TRUE, 
         sort.frq = "none", 
         show.ci = TRUE,
         show.na = TRUE,
         coord.flip = F,
         vjust = "outward",
         geom.colors = "black")

#cruzando a questão da situação de rua com a renda
sjPlot::sjplot(data = coesao_experimental, coesao_experimental$sit_rua3, coesao_experimental$renda, 
               fun = "xtab",
               type  = "bar",
               geom.colors = "Set1")
<<<<<<< HEAD

=======
>>>>>>> 64ed2609c93f31b543113065af5fa2528784f2b9
>>>>>>> c7091beb123d974579da215666492b9fe7ce461e
>>>>>>> f137415b8ca2eaaaeb86bfd93a807da9df5d693e


salario_escolaridade <- lm(rendaf ~ esc, data = coesao_experimental)
plot_model(salario_escolaridade, type = "pred")





#MOTIVO PRA SER RICO
plot_frq(coesao_experimental$motiv_rico_O1, 
         type = "bar",
         show.n = T, 
         show.values = TRUE, 
         sort.frq = "none", 
         show.ci = TRUE,
         show.na = TRUE,
         coord.flip = F,
         #vjust = "inward",
         #hjust = "center",
         expand.grid = T,
         geom.colors = "grey")+
  theme(text = element_text(size = 15))


plot_frq(coesao_experimental$motiv_rico_O2, 
         type = "bar",
         show.n = T, 
         show.values = TRUE, 
         sort.frq = "none", 
         show.ci = TRUE,
         show.na = TRUE,
         coord.flip = F,
         #vjust = "inward",
         #hjust = "center",
         expand.grid = T,
         geom.colors = "grey")+
  theme(text = element_text(size = 15))

skim(coesao_experimental$motiv_pobre_O1)

plot_frq(coesao_experimental$motiv_pobre_O1, 
         type = "bar",
         show.n = T, 
         show.values = TRUE, 
         sort.frq = "none", 
         show.ci = TRUE,
         show.na = TRUE,
         coord.flip = F,
         #vjust = "inward",
         #hjust = "center",
         expand.grid = T,
         geom.colors = "grey")+
  theme(text = element_text(size = 15))

plot_frq(coesao_experimental$motiv_pobre_O2, 
         type = "bar",
         show.n = T, 
         show.values = TRUE, 
         sort.frq = "none", 
         show.ci = TRUE,
         show.na = TRUE,
         coord.flip = F,
         vjust = "inward",
         #hjust = "center",
         expand.grid = T,
         geom.colors = "grey")+
  theme(text = element_text(size = 15))



papel_estado <- coesao_experimental %>% 
  select(I_1_resp_est:I_6_resp_est)

library(Hmisc)

label(papel_estado)
get_label(papel_estado)
summary(papel_estado)

#ESSE PLOT PRECISA SER CORRIGIDO (O PRÓXIMO É BARRA EMPILHADA)
sjPlot::plot_likert(papel_estado,
            title = "Papel do Estado nas políticas",
            legend.title = "Grau de concordância 1(-) a 10(+)",
            axis.labels = c("Garantir emprego", "Saúde gratuita", "Auxiliar empresas a crescer",
                            "Reduzir diferenças de renda", "Educação universitária para todos", 
                            "Garantir moradia decente"),
            #catcount = 10,
            #cat.neutral = 5,
            wrap.labels = 60,
            geom.colors = "Spectral",
            reverse.colors = F,
            show.legend = T,
            grid.range = 0.9,
            sort.frq = NULL
            #values = "sum.outside"
)



plot_stackfrq(papel_estado,
              title = "Papel do Estado na economia",
              legend.title = "grau de concordância 0 a 10",
              axis.labels = c("Garantir emprego", "Saúde gratuita", "Auxiliar empresas a crescer",
                              "Reduzir diferenças de renda", "Educação universitária para todos", 
                              "Garantir moradia decente"),
              wrap.labels = 60,
              show.total = T,
              geom.colors = "Spectral",
              geom.size = 0.9,
              #reverse.colors = F,
              show.legend = T,
              #grid.range = 0.6,
              sort.frq = "firs.desc") +
              theme(text = element_text(size = 15))

summary(papel_estado)
head(papel_estado)

papel_estado_1 <- papel_estado[c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9 , 10), ] 


#CRIANDO UM ÍNDICE DE GRAU DE APOIO AO ESTADO
coesao_experimental$I_apoio_estado <- rowMeans(
  coesao_experimental[,c("I_1_resp_est", "I_2_resp_est", "I_3_resp_est", "I_4_resp_est", "I_5_resp_est", "I_6_resp_est")])

#QUARTIS DO ÍNDICE DE APOIO AO ESTADO
summary(coesao_experimental$I_apoio_estado)



ggplot(coesao_experimental, mapping=aes(x=coesao_experimental$I_1_resp_est, y = coesao_experimental$renda)) + 
  stat_summary(fun.data=mean_sdl, geom="bar")

papel_estado %>% 
  summarise(media = mean())

ggplot(coesao_experimental)+
  geom_histogram(aes(x = coesao_experimental$I_1_resp_est))


#64. QUAIS OS PROBLEMAS QUE A POBREZA E DESIG TRAZEM PRA SOCIEDADE (1 E 2)
plot_frq(coesao_experimental$conseq_des_O1, 
         type = "bar",
         show.n = T, 
         show.values = TRUE, 
         sort.frq = "none", 
         show.ci = TRUE,
         show.na = TRUE,
         coord.flip = F,
         #vjust = "inward",
         #hjust = "center",
         expand.grid = T,
         geom.colors = "grey")+
  theme(text = element_text(size = 15))

# 64 (SEGUNDA CONSEQUENCIA)
plot_frq(coesao_experimental$conseq_des_O2, 
         type = "bar",
         show.n = T, 
         show.values = TRUE, 
         sort.frq = "none", 
         show.ci = TRUE,
         show.na = TRUE,
         coord.flip = F,
         #vjust = "inward",
         #hjust = "center",
         expand.grid = T,
         geom.colors = "grey")+
  theme(text = element_text(size = 15))

# 65 SOLUÇÃO PARA A DESIGUALDADE NO NOSSO PAÍS
plot_frq(coesao_experimental$solucao_des_O1, 
         type = "bar",
         show.n = T, 
         show.values = TRUE, 
         sort.frq = "none", 
         show.ci = TRUE,
         show.na = TRUE,
         coord.flip = F,
         vjust = "inward",
         #hjust = "center",
         expand.grid = T,
         wrap.title = 120,
         geom.colors = "grey") +
  theme(text = element_text(size =15))


# 66. EM QUE POSIÇÃO DA HIERARQUIA SOCIAL ESTÁ?
plot_frq(coesao_experimental$classe_social, 
         show.n = T, 
         show.values = TRUE,
         show.ci = TRUE,
         show.na = TRUE,
         expand.grid = T,
         wrap.title = 120,
         geom.colors = "grey") +
  theme(text = element_text(size =20))

#68 A DISTÂNCIA ENTRE RICOS E POBRES NO BRASIL É MUITO GRANDE
plot_frq(coesao_experimental$rico_pobre, 
         show.n = T, 
         show.values = TRUE,
         show.ci = TRUE,
         show.na = TRUE,
         expand.grid = T,         
         wrap.title = 120,
         geom.colors = "grey") +
  theme(text = element_text(size =20))



#69 O GOVERNO DEVE CRIAR PROGRAMAS DE DISTRIBUICAO DE RENDA?
plot_frq(coesao_experimental$prog_renda, 
         show.n = T, 
         show.values = TRUE,
         show.ci = TRUE,
         show.na = TRUE,
         expand.grid = T,
         geom.colors = "grey",
         wrap.title = 120) +
  theme(text = element_text(size =15))

# 69, MAS CRUZANDO COM RENDA
plot_xtab(coesao_experimental$prog_renda, coesao_experimental$renda,
          wrap.labels = 40,
          wrap.title = 120)+
  theme(text = element_text(size =15))


#70 BENEFICIOCIS ASSISTENCIAIS DEIXAM AS PESSOAS ACOMODADAS
plot_frq(coesao_experimental$beneficios,
  show.n = T, 
  show.values = TRUE,
  show.ci = TRUE,
  show.na = TRUE,
  expand.grid = T,
  geom.colors = "grey",
  wrap.labels = 40,
  wrap.title = 120)+
  theme(text = element_text(size =15))

#beneficios deixam acomodada - agrupado por renda
plot_xtab(coesao_experimental$beneficios, coesao_experimental$renda,
          wrap.labels = 40,
          wrap.title = 120)+
  theme(text = element_text(size =15))

#beneficios deixam acomodada - agrupado por sexo
plot_xtab(coesao_experimental$beneficios, coesao_experimental$sexo,
          wrap.labels = 40,
          wrap.title = 120)+
  theme(text = element_text(size =15))




#CRIANDO O BANCO COMO FATORES, AGORA
names(coesao)
papel_estado_factor <- papel_estado
glimpse(papel_estado)
papel_estado_factor <- lapply (papel_estado_factor, 
                               function(x){factor(x, levels = c("0 - Nenhuma responsabilidade", "1", "2", "3",
                                                                "4", "5", "6", "7", "8", "9", "10 - Total Responsabilidade"))})

lik <- likert(as.data.frame(papel_estado_factor))

#RESPONSABILIDADE DO ESTADO
plot(lik, wrap = 60, text.size=3)+
  theme(legend.text=element_text(size=rel(0.6)),
        text = element_text(size =10))+
  ggtitle("Responsabilidade do Estado")


str(coesao_experimental$renda)
class(coesao_experimental$renda)

#CRUZANDO AS QUESTÕES SOBRE USO APROPRIADO DOS RECURSOS COM FAIXAS DE RENDA
lik_renda <- likert(as.data.frame(papel_estado_factor), grouping = coesao_experimental$renda)

plot(lik_renda,
     wrap = 60, 
     text.size=5,
     cat.neutral.color = "black",
     show.n = T,
     values = "show") +
  ggtitle("Responsabilidade do Estado, por faixas de renda")

f$f <- factor(df$f, levels=c('a','b','c'),
              +   labels=c('Treatment A: XYZ','Treatment B: YZX','Treatment C: ZYX'))    

#69 O GOVERNO DEVE CRIAR PROGRAMAS DE DISTRIBUICAO DE RENDA?
plot_frq(coesao_experimental$prog_renda, 
         show.n = T, 
         show.values = TRUE,
         show.ci = TRUE,
         show.na = TRUE,
         expand.grid = T,
         geom.colors = "grey")+
  theme(plot.title = element_text(hjust = 0.5),text = element_text(size =13)) +
  ggtitle("O governo deve criar programas de distribuição de renda?")



plot(lik, type = "heat", wrap = 60, text.size=3)


#PAPEL DO ESTADO
coesao_factor <- plot_xtab(coesao_experimental$prog_renda, coesao_experimental$renda)


#BENEFICIOCIS ASSISTENCIAIS DEIXAM AS PESSOAS ACOMODADAS
plot_frq(coesao_experimental$beneficios,
         show.n = T, 
         show.values = TRUE,
         show.ci = TRUE,
         show.na = TRUE,
         expand.grid = T,
         geom.colors = "grey") +
  theme(plot.title = element_text(hjust = 0.5),text = element_text(size =13)) +
  ggtitle("Benefícios assistenciais acomodam pessoas?"
  )

#beneficios deixam acomodada - agrupado por renda
plot_xtab(coesao_experimental$beneficios, coesao_experimental$renda)+
  theme(plot.title = element_text(hjust = 0.5),text = element_text(size =13)) +
  ggtitle("Benefícios assistenciais acomodam pessoas? (Agrupado por renda)"
  )

#USO APROPRIADO DO RECURSO DOS IMPOSTOS
#CRIAR UM PLOT COM OS TRES AGRUPADOS###########3
plot_frq(coesao_experimental$I_1_aval,
         show.n = T, 
         show.values = TRUE,
         show.ci = TRUE,
         show.na = TRUE,
         #expand.grid = T,
         show.sd = T,
         geom.colors = "grey",
         wrap.labels = 40,
         wrap.title = 120)+
  theme(text = element_text(size =13),
        panel.background=element_blank())

plot_frq(coesao_experimental$I_2_aval,
         show.n = T, 
         show.values = TRUE,
         show.ci = TRUE,
         show.na = TRUE,
         #expand.grid = T,
         show.sd = T,
         geom.colors = "grey",
         wrap.labels = 40,
         wrap.title = 120)+
  theme(text = element_text(size =15),
        panel.background=element_blank())


plot_frq(coesao_experimental$I_3_aval,
         show.n = T, 
         show.values = TRUE,
         show.ci = TRUE,
         show.na = TRUE,
         #expand.grid = T,
         show.sd = T,
         geom.colors = "grey",
         wrap.labels = 40,
         wrap.title = 120)+
  theme(text = element_text(size =15),
        panel.background=element_blank())

# 74 a 77 - DIMENSÕES DO USO DOS IMPOSTOS
avaliacao_uso_impostos <- coesao_experimental %>%  
  select(I_1_imposto:I_4_imposto)


summary(avaliacao_uso_impostos)

plot_likert(avaliacao_uso_impostos,
            axis.labels = c("Em geral, o governo gasta bem o dinheiro dos impostos", 
                            "A qualidade da saúde e da educação pública seria melhor se o governo aumentasse os impostos dos ricos para financiar essas políticas",
                          "Não há recursos  suficientes para financiar um sistema de saúde e a educação pública de qualidade do país",
                          "Os serviços de saúde e educação só terão mehor qualidade se forem melhor adminsitrados"),
            catcount = 4,
            cat.neutral = 5,
            wrap.labels = 60,
            geom.colors = "Spectral",
            reverse.colors = F,
            show.legend = T) +
            #grid.range = 0.6,
            #sort.frq = NULL
            #values = "sum.outside"
  theme(plot.title = element_text(hjust = 0.5),text = element_text(size =13)) +
  ggtitle("Aspectos do uso dos impostos")

  
#testando meios alternativos de explorar grouped tabs com fatores (estudar forcats) estuar plot_likert
#FORMA ALTERNATIVA DE MOSTRA
beneficios <- lapply (coesao_experimental$beneficios, 
                               function(x){factor(x, levels = c("1", "2", "3","4", "5"), labels = ("Concorda totalmente","Concorda em parte","Discorda totalmente","Discorda em parte","Não concorda nem discorda"))})

#78 O governo federal deve aumentar os impostos de pessoas ricas para combater pobreza
plot_frq(coesao_experimental$exp_1,
         show.n = T, 
         drop.empty = T,
         show.values = TRUE,
         show.ci = TRUE,
         show.na = F,
         #expand.grid = T,
         show.sd = T,
         geom.colors = "grey",
         wrap.labels = 40,
         wrap.title = 120)+
  theme(text = element_text(size =15),
        panel.background=element_blank()) +
  ggtitle("Experimento: O governo federal deve aumentar os impostos de pessoas ricas para combater pobreza")
<<<<<<< HEAD

#78 (experimento 2) O governo federal deve aumentar os impostos de pessoas ricas 
#para combater pobreza
plot_frq(coesao_experimental$exp_2,
         show.n = T, 
         drop.empty = T,
         show.values = TRUE,
         show.ci = TRUE,
         show.na = F,
         #expand.grid = T,
         show.sd = T,
         geom.colors = "grey",
         wrap.labels = 40,
         wrap.title = 120)+
  theme(text = element_text(size =15),
        panel.background=element_blank()) +
  ggtitle("Experimento 2: O governo federal deve aumentar os impostos de pessoas ricas para combater pobreza")

#78 (experimento 3) O governo federal deve aumentar os impostos de pessoas ricas 
#para combater pobreza
plot_frq(coesao_experimental$exp_3,
         show.n = T, 
         drop.empty = T,
         show.values = TRUE,
         show.ci = TRUE,
         show.na = F,
         #expand.grid = T,
         show.sd = T,
         geom.colors = "grey",
         wrap.labels = 40,
         wrap.title = 120)+
  theme(text = element_text(size =15),
        panel.background=element_blank()) +
  ggtitle("Experimento 3: O governo federal deve aumentar os impostos de pessoas ricas para combater pobreza")

#QUESTOES 81 A 83
#criando o bloco com as três questões
propensao_distribuir <- coesao_experimental %>% 
  select(I_1_exp_neut:I_3_exp_neut)


plot_likert(propensao_distribuir,
            axis.labels = c("Poderia pagar mais impostos se fossem usados para melhorar a vida dos pobres", 
                            "Os mais ricos devem pagar pelos custos de combater a pobreza",
                            "A pobreza é natural em qualquer sociedade"),
            catcount = 4,
            cat.neutral = 5,
            geom.colors = "Spectral",
            reverse.colors = F,
            show.legend = T,
            wrap.labels = 60,
            wrap.title = 120,
            values = "sum.outside",
            grid.range = 0.7)+
  theme(plot.title = element_text(hjust = 0.5),text = element_text(size =13)) +  ggtitle("Questões que abordam percepções sobre redistribuição")

#CRIAR BIVARIADAS COM AS QUESTÕES ACIMA

#MEDINDO CORRELAÇÃO ENTRE AS QUESTÕES ACIMA E APOIO A BOLSONARO E LULA

corr_distr_bolsonaro <- ggplot(coesao_experimental, mapping = aes(coesao_experimental$I_2_exp_neut, coesao_experimental$T_lula_bolso_2))

corr_distr_bolsonaro + 
  geom_point(alpha = 0.3) +
  geom_smooth(method = 'lm', color = "black")



#86 IGUALDADE OU LIBERDADE
plot_frq(coesao_experimental$liber_igual,
         wrap.labels = 120,
         geom.colors = "grey") +
  theme(plot.title = element_text(hjust = 0.5),text = element_text(size =15)) +  ggtitle("Liberdade ou igualdade")


#consistencia das questões sobre protecao à diversidade e direitos civis / religiosos
alpha(coesao_experimental[,c("I_1_divers", "I_2_divers", "I_3_divers", "I_4_divers", "I_5_divers"), na.rm = TRUE])


###CRIANDO UM ÍNDICE DE APOIO À DIVERSIDADE###
#criando um índice geral de apoio aos direitos e diversidade

coesao_experimental$I_divers_geral <- rowMeans(
  coesao_experimental[,c("I_1_divers", "I_2_divers", "I_3_divers", "I_4_divers", "I_5_divers")])

view(coesao_experimental)
names(coesao_experimental)
summary(coesao_experimental$I_divers_geral)
ggplot(coesao_experimental)+
  geom_histogram(aes(x = I_divers_geral))



diversidade <- coesao_experimental %>% 
  select(I_1_divers:I_5_divers)

summary(diversidade)

#ESSE PLOT PRECISA SER CORRIGIDO (O PRÓXIMO É BARRA EMPILHADA)
plot_likert(diversidade,
            title = "Apoio à diversidade",
            legend.title = "Grau de concordância 1(-) a 10(+)",
            axis.labels = c("Que vivamos", "pessoas do mesmo sexo casar",
                            "leis contra o racismo",
                            "Lei maria a penha",
                            "liberdade religiosa"),
            catcount = 11,
            #cat.neutral = 5,
            wrap.labels = 60,
            geom.colors = "Spectral",
            reverse.colors = F,
            show.legend = T,
            grid.range = 0.6,
            sort.frq = NULL
            #values = "sum.outside"
)

plot_grpfrq(diversidade,
              title = "Apoio à diversidade",
              legend.title = "Grau de concordância 1(-) a 10(+)",
              axis.labels = c("Que vivamos em uma sociedade com diferentes valores e opiniões", 
                              "Que pessoas do mesmo sexo possam se casar",
                              "Que existam leis contra o racismo",
                              "Que existam lei de combate à violência contra as mulheres, como a Lei Maria da Penha",
                              "Leis protegendo a liberdade religiosa"),
              wrap.labels = 60,
              show.total = T,
              geom.colors = "Spectral",
              geom.size = 0.6,
              show.legend = T,
              sort.frq = "last.desc") +
  theme(text = element_text(size = 15))


# 93 ESQUERDA E DIREITA

get_label(coesao_experimental$ideologia)
summary(coesao_experimental$ideologia)

plot_frq(coesao_experimental$ideologia,
         wrap.title = 120,
         wrap.labels = 120,
         geom.colors = "grey") +
  theme(plot.title = element_text(hjust = 0.5),text = element_text(size =15)) +  
  ggtitle("Autoposicionamento no espectro ideológico")
=======

#78 (experimento 2) O governo federal deve aumentar os impostos de pessoas ricas 
#para combater pobreza
plot_frq(coesao_experimental$exp_2,
         show.n = T, 
         drop.empty = T,
         show.values = TRUE,
         show.ci = TRUE,
         show.na = F,
         #expand.grid = T,
         show.sd = T,
         geom.colors = "grey",
         wrap.labels = 40,
         wrap.title = 120)+
  theme(text = element_text(size =15),
        panel.background=element_blank()) +
  ggtitle("Experimento 2: O governo federal deve aumentar os impostos de pessoas ricas para combater pobreza")

#78 (experimento 3) O governo federal deve aumentar os impostos de pessoas ricas 
#para combater pobreza
plot_frq(coesao_experimental$exp_3,
         show.n = T, 
         drop.empty = T,
         show.values = TRUE,
         show.ci = TRUE,
         show.na = F,
         #expand.grid = T,
         show.sd = T,
         geom.colors = "grey",
         wrap.labels = 40,
         wrap.title = 120)+
  theme(text = element_text(size =15),
        panel.background=element_blank()) +
  ggtitle("Experimento 3: O governo federal deve aumentar os impostos de pessoas ricas para combater pobreza")


#CORRELAÇÃO 


   plot_likert(coesao_experimental, coesao_experimental$I_7_inst)

   
   confianca_institucional_bolsonarista  <- coesao_experimental %>% 
     filter(coesao_experimental$T_lula_bolso_2>6)
     select(29:38)
   
   
 
   plot_likert(confianca_institucional_bolsonarista,
               title = "Confiança em instituições",
               legend.title = "Grau de confiança",
               axis.labels = c("Igreja", "Polícia", "Militares", "Congresso Nacional", "Funcionários Públicos",
                               "SUS", "Cientistas", "Judiciário", "Governo Federal", "Partidos Políticos"),
               #catcount = 4,
               wrap.labels = 60,
               geom.colors = "Spectral",
               reverse.colors = T,
               show.legend = T,
               grid.range = 0.9,
               sort.frq = "pos.desc",
               values = "sum.outside"
   ) + theme(legend.position = "bottom")
   