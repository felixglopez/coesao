<<<<<<< HEAD
#exploração do survey sobre coesão social


#bibliotecas necessárias
install.packages("readr")
library(readr)
install.packages("cli")
library(cli)
install.packages("haven")
library(haven)
install.packages("tidyverse")
library(tidyverse)
install.packages("rlang")
library(rlang)
library (janitor)
library(labelled)
library(questionr)
=======
library(tidyverse)
library(haven)
library(readr)
install.packages("labelled")
library(labelled)
library(janitor)
install.packages("questionr")
library(questionr)
??questionr


library(tibble)
library(magrittr)
>>>>>>> 70bc9bf63b0e349951f618d05c43fe3ae875e8f7
library(testthat)
library(roxygen2)
library(Hmisc)
library(knitr)
library(devtools)
<<<<<<< HEAD
library(sjmisc)
library(ggplot2)
library(missMDA)
library(sjPlot)
library(sjlabelled)
library(sjstats)
library(datawizard)
library(questionr)
install.packages("vctrs")
library(vctrs)
update
update.packages("vctrs")


rm(list=ls())

coesao <- read_spss("coesao_social_ago22.sav")

coesao_exploracao <- coesao
  
coesao_exploracao[coesao_exploracao==888] <- NA
coesao_exploracao[coesao_exploracao==999] <- NA
coesao_exploracao %>% 
  select(3:128)


view(coesao_exploracao)

#ajustar os nomes
clean_names(coesao_exploracao) 
glimpse(coesao_exploracao)
View(coesao_exploracao)
head(coesao_exploracao)

#ver os rotulos de cada variável
coesao_exploracao$estado %>% attr('labels')

sessionInfo()

#ver numero de linhas (sample) e numero de colunas (variaveis)
dim(coesao_exploracao)



#ver nome das colunas
colnames(coesao_exploracao)


#sumário das variáveis
summary(coesao_exploracao$rendaf)
summary(coesao_exploracao$liber_igual)



#vendo todas as perguntas e alternativas de resposta (do pacote labelled)
coesao_exploracao %>% look_for("homossexual", labels = TRUE, ignore.case = TRUE, details = TRUE)



#formas de explorar os labels das variaveis. Mostra todo o questionário ou data frame de interesse
coesao_exploracao %>%   sjPlot::view_df(show.frq = T, 
=======
install.packages("surveytoolbox")
library(surveytoolbox)
library(dplyr)
library(sjmisc)
library(ggplot2)
library(missMDA)
install.packages("sjPlot")
library(sjlabelled)
library(sjstats)
library(datawizard)
library(sjstats)
install.packages("sjstats")
install.packages("likert")
library(sjstats)
library(likert)



coesao <- read_spss ("coesao_social_ago22.sav")
glimpse(coesao)

head(coesao$comunidade)

#para ver os rótulos das questões
coesao$comunidade %>% attr('labels')

coesao$relig %>% attr('labels') 

coesao %>%   sjPlot::view_df(show.frq = T, 
>>>>>>> 70bc9bf63b0e349951f618d05c43fe3ae875e8f7
                             show.prc = TRUE, 
                             verbose = T, 
                             use.viewer = F, 
                             alternate.rows = T, 
                             show.values = T,
                             show.string.values = T,
<<<<<<< HEAD
                             file = "C:/Users/R1705296/Documents/atlas-estado/felix/coesao/coesao_exploracao_1.html")


#criando bloco de análise###############

#integrando questoes sobre confianca

bloco_confianca <- coesao_exploracao %>% 
select(21:22)

# integrando questões 6, 7, 8, 9, 10, 12, 14, 17, 24, 25, 26, 27
elites_bloco0 <- elites %>% 
  dplyr::select(1, 6:10, 12, 14, 17, 24:27)




#BLOCO 0
# integrando questões 6, 7, 8, 9, 10, 12, 14, 17, 24, 25, 26, 27
elites_bloco0 <- elites %>% 
  dplyr::select(1, 6:10, 12, 14, 17, 24:27)

glimpse(elites_bloco0)
view(elites_bloco0)


freq(elites$OPINIAO_POLITICAS_DESTRIBUICAO_RENDA, sort = "none", cum = TRUE, na.last = T, exclude = c("NR (não ler)", "NS (não ler)", NA), digits = 0, valid = F, levels = "labels")
plot_frq(elites_bloco0$OPINIAO_POLITICAS_DESTRIBUICAO_RENDA, 2, show.n = T, sort.frq = "none")

plot_grpfrq(coesao_exploracao$I_1_conf, coesao_exploracao$I_2_conf,
            type = c("bar"),
            bar.pos = c("dodge"),
            #weight.by = NULL,
            #intr.var = NULL,
            title = "",
            title.wtd.suffix = NULL,
            legend.title = NULL,
            axis.titles = NULL,
            #geom.colors = "Paired",
            facet.grid = F,
            show.grpcnt = T,
            coord.flip = T)


plot_likert (elites_bloco0,
             groups = NULL,
             groups.titles = NULL,
             title = "Grau de atuação/regulação do Estado na economia",
             #legend.title = "Tipo de elite",
             legend.labels = NULL,
             axis.titles = NULL,
             axis.labels = NULL,
             catcount = NULL,
             cat.neutral = NULL,
             sort.frq = NULL,
             weight.by = NULL,
             title.wtd.suffix = NULL,
             wrap.title = 50,
             wrap.labels = 30,
             wrap.legend.title = 30,
             wrap.legend.labels = 28,
             geom.size = 0.6,
             geom.colors = "BrBG",
             #cat.neutral.color = "grey70",
             #intercept.line.color = "grey50",
             reverse.colors = FALSE,
             values = "sum.inside",
             show.n = TRUE,
             show.legend = TRUE,
             show.prc.sign = FALSE,
             expand.grid = F,
             digits = 0,
             reverse.scale = FALSE,
             coord.flip = T,
             sort.groups = TRUE,
             legend.pos = "bottom",
             rel_heights = 1,
             group.legend.options = list(nrow = T, byrow = TRUE))










##################################3


#PRIMEIRO BLOCO
#criar primeiro bloco de questões: REGULACAO GOVENAMENTAL NA ECONOMIA
elites_bloco1 <- elites %>%   select(1, 6:10)



elites_bloco1$CORTAR_GASTOS_GOVERNO

elites_bloco1 %>%
  replace_na(condition ~.x >87)
replace_na(replace = list(elites_bloco1 = c("NS", "NR")))


#names <- c(1:3,5)
elites_bloco1[, names] <- lapply(elites_bloco1[, names], factor)
str(elites_bloco1)
head(elites_bloco1)

#elites_bloco1 %>% 
# select(1, 6, 7, 8, 9, 10)

elites_bloco1[elites_bloco1 == 88] <- NA  
elites_bloco1[elites_bloco1==99] <- NA

summary(elites_bloco1)

#BLOCO 1 - CORTAR GASTOS DO GOVERNO
freq(elites_bloco1$CORTAR_GASTOS_GOVERNO, sort = "none", cum = TRUE, na.last = T, exclude = c("NR (não ler)", "NS (não ler)", NA), digits = 0, valid = F, levels = "labels")

plot_frq(elites_bloco1, 2, show.n = T, sort.frq = "none")

plot_grpfrq(elites_bloco1$SETOR, elites_bloco1$CORTAR_GASTOS_GOVERNO,
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
            coord.flip = T)



head(elites_bloco1)

#BLOCO 1 - FINANCIAR PROJETO
freq(elites_bloco1$FINANCIAR_PROJETOS_CRIAR_NOVOS_EMPRE, sort = "none", cum = TRUE, na.last = T, exclude = c("NR (não ler)", "NS (não ler)", NA), digits = 0, valid = F, levels = "labels")

plot_frq(elites_bloco1, elites_bloco1$FINANCIAR_PROJETOS_CRIAR_NOVOS_EMPRE, show.n = T, sort.frq = "desc")

plot_grpfrq(elites_bloco1$SETOR, elites_bloco1$FINANCIAR_PROJETOS_CRIAR_NOVOS_EMPRE,
            type = c("bar"),
            bar.pos = c("dodge"),
            show.prc = T,
            weight.by = NULL,
            intr.var = NULL,
            title = "",
            title.wtd.suffix = NULL,
            legend.title = NULL,
            axis.titles = NULL,
            geom.colors = "Paired",
            facet.grid = F,
            show.grpcnt = T,
            coord.flip = T)



#BLOCO 1 - DIMINUIR REGULAMENTACAO

freq(elites_bloco1$DIMINUIR_REGULAMENTACAO_GOV_NEG_PRIV, sort = "none", cum = TRUE, na.last = T, exclude = c("NR (não ler)", "NS (não ler)", NA), digits = 0, valid = F, levels = "labels")

plot_frq(elites_bloco1, 4, show.n = T, sort.frq = "none")

plot_grpfrq(elites_bloco1$SETOR, elites_bloco1$DIMINUIR_REGULAMENTACAO_GOV_NEG_PRIV,
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
            coord.flip = T)

#BLOCO 1 - APOIAR INDUSTRIA E PROTEGER EMPREGOS
freq(elites_bloco1$APOIAR_IND_CRISE_PROTEGER_EMPREGOS, sort = "none", cum = TRUE, na.last = T, exclude = c("NR (não ler)", "NS (não ler)", NA), digits = 0, valid = F, levels = "labels")

plot_frq(elites_bloco1, 5, show.n = T, sort.frq = "none")

plot_grpfrq(elites_bloco1$SETOR, elites_bloco1$APOIAR_IND_CRISE_PROTEGER_EMPREGOS,
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
            coord.flip = T)


#BLOCO 1 - SUBSIDIO DO ESTADO AO SETOR PRODUTIVO

freq(elites_bloco1$SUBSIDIO_ESTADO_SETOR_PRODUTIVO, sort = "none", cum = TRUE, na.last = T, exclude = c("NR (não ler)", "NS (não ler)", NA), digits = 0, valid = F, levels = "labels")

plot_frq(elites_bloco1, 6, show.n = T, sort.frq = "none")

plot_grpfrq(elites_survey_NA$setor, elites_survey_NA$subsidio_estado_setor_produtivo,
            type = c("bar"),
            bar.pos = c("dodge"),
            title = "",
            geom.colors = "Paired",
            facet.grid = F,
            show.grpcnt = T,
            coord.flip = T,
            summary.pos = T)


#BLOCO 1- LIKERT COM TODAS

plot_likert(
  elites_survey_NA,
  groups = NULL,
  groups.titles = NULL,
  title = "Grau de atuação/regulação do Estado na economia",
  legend.title = "Tipo de elite",
  legend.labels = NULL,
  axis.titles = NULL,
  axis.labels = NULL,
  catcount = NULL,
  cat.neutral = "Nem a favor, nem contra",
  sort.frq = NULL,
  weight.by = NULL,
  title.wtd.suffix = NULL,
  wrap.title = 50,
  wrap.labels = 30,
  wrap.legend.title = 30,
  wrap.legend.labels = 28,
  geom.size = 0.6,
  geom.colors = "BrBG",
  cat.neutral.color = "grey70",
  intercept.line.color = "grey50",
  reverse.colors = FALSE,
  values = "show",
  show.n = TRUE,
  show.legend = TRUE,
  show.prc.sign = FALSE,
  grid.range = 1,
  grid.breaks = 0.2,
  expand.grid = F,
  digits = 0,
  reverse.scale = FALSE,
  coord.flip = TRUE,
  sort.groups = TRUE,
  legend.pos = "bottom",
  rel_heights = 1,
  group.legend.options = list(nrow = T, byrow = TRUE),
  cowplot.options = list(label_x = 0.01, hjust = 0, align = "v")
)



#BLOCO 2
elites_bloco2 <- elites %>% 
  select(elites$SETOR, BOLSA_FAMILIA_ESTIMULA_ECO, BOLSA_FAMILIA_ESTIMULA_CLIENTEISMO_POL, BOLSA_FAMILIA_DESINCENTIVO_TRABALHO, BOLSA_FAMILIA_REDUZ_DESIGUALDADE,
         BOLSA_FAMILIA_ESTIMULA_TER_MAIS_FILHOS, BOLSA_FAMILIA_ESTIMULA_FREQUENCIA_ESCO)

elites_bloco2 <- elites_bloco2 %>% 
  select(c(2, 4:9))


head(elites_bloco2)


elites_bloco2[elites_bloco2 == 88] <- NA  
elites_bloco2[elites_bloco2==99] <- NA

#names <- c(1:3,5)
elites_bloco2[, names] <- lapply(elites_bloco2[, names], factor)
str(elites_bloco2)

view(elites_bloco2)
glimpse(elites_bloco2)
summary(elites_bloco2)

#BLOCO 2 - BOLSA FAMILIA ESTIMULA A ECONOMIA
freq(elites_bloco2$BOLSA_FAMILIA_ESTIMULA_ECO, sort = "none", cum = TRUE, na.last = T, exclude = c("NR (não ler)", "NS (não ler)", NA), digits = 0, valid = F, levels = "labels")

plot_frq(elites_bloco2, 2, show.n = T, sort.frq = "none")

plot_grpfrq(elites_bloco2$SETOR, elites_bloco2$BOLSA_FAMILIA_ESTIMULA_ECO,
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
            coord.flip = T)


#BLOCO 2 - BOLSA FAMILIA ESTIMULA CLIENTELISMO
freq(elites_bloco2$BOLSA_FAMILIA_ESTIMULA_CLIENTEISMO_POL, sort = "none", cum = TRUE, na.last = T, exclude = c("NR (não ler)", "NS (não ler)", NA), digits = 0, valid = F, levels = "labels")

plot_frq(elites_bloco2, 3, show.n = T, sort.frq = "none")

plot_grpfrq(elites_bloco2$SETOR, elites_bloco2$BOLSA_FAMILIA_ESTIMULA_CLIENTEISMO_POL,
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
            coord.flip = T)


#BLOCO 2 - BOLSA FAMILIA DESINCENTIVA O TRABALHO
freq(elites_bloco2$BOLSA_FAMILIA_DESINCENTIVO_TRABALHO, sort = "none", cum = TRUE, na.last = T, exclude = c("NR (não ler)", "NS (não ler)", NA), digits = 0, valid = F, levels = "labels")

plot_frq(elites_bloco2, 4, show.n = T, sort.frq = "none")

plot_grpfrq(elites_bloco2$SETOR, elites_bloco2$BOLSA_FAMILIA_DESINCENTIVO_TRABALHO,
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
            coord.flip = T)



#BLOCO 2 - BOLSA FAMILIA REDUZ DESIGUALDADE
freq(elites_bloco2$BOLSA_FAMILIA_REDUZ_DESIGUALDADE, sort = "none", cum = TRUE, na.last = T, exclude = c("NR (não ler)", "NS (não ler)", NA), digits = 0, valid = F, levels = "labels")

plot_frq(elites_bloco2, 5, show.n = T, sort.frq = "none")

plot_grpfrq(elites_bloco2$SETOR, elites_bloco2$BOLSA_FAMILIA_REDUZ_DESIGUALDADE,
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
            coord.flip = T)


#BLOCO 2 - BOLSA FAMILIA ESTIMULA TER FILHOS
freq(elites_bloco2$BOLSA_FAMILIA_ESTIMULA_TER_MAIS_FILHOS, sort = "none", cum = TRUE, na.last = T, exclude = c("NR (não ler)", "NS (não ler)", NA), digits = 0, valid = F, levels = "labels")

plot_frq(elites_bloco2, 6, show.n = T, sort.frq = "none")

plot_grpfrq(elites_bloco2$SETOR, elites_bloco2$BOLSA_FAMILIA_ESTIMULA_TER_MAIS_FILHOS,
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
            coord.flip = T)


#BLOCO 2 - BOLSA FAMILIA ESTIMULA PRESENCA ESCOLAR
freq(elites_bloco2$BOLSA_FAMILIA_ESTIMULA_FREQUENCIA_ESCO, sort = "none", cum = TRUE, na.last = T, exclude = c("NR (não ler)", "NS (não ler)", NA), digits = 0, valid = F, levels = "labels")

plot_frq(elites_bloco2, 7, show.n = T, sort.frq = "none")

plot_grpfrq(elites_bloco2$SETOR, elites_bloco2$BOLSA_FAMILIA_ESTIMULA_FREQUENCIA_ESCO,
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
            coord.flip = T)



#BLOCO 3 DESEJAVEL E VIAVEL

elites_bloco3 <- elites %>% 
  select(1, 81:99)

glimpse(elites_bloco3)


#formas de explorar os labels das variaveis????
elites_bloco1 %>% sjPlot::view_df()


#names <- c(1:3,5)
elites_bloco3[, names] <- lapply(elites_bloco3[, names], factor)


elites_bloco3[elites_bloco3 == 88] <- NA  
elites_bloco3[elites_bloco3==99] <- NA

summary(elites_bloco3)
glimpse(elites_bloco3)  

freq(elites_bloco3$ACESSO_UNIVER_GRATUITO_SAUDE_DESEJAVEL, sort = "none", cum = TRUE, na.last = T, exclude = c("NR (não ler)", "NS (não ler)", NA), digits = 0, valid = F, levels = "labels")

plot_frq(elites_bloco3, 3, show.n = T, sort.frq = "none")

plot_grpfrq(elites_bloco3$ACESSO_UNIVER_GRATUITO_SAUDE_VIAVEL, elites_bloco3$ACESSO_UNIVER_GRATUITO_SAUDE_DESEJAVEL, #elites_bloco3$ACESSO_UNIVER_GRATUITO_SAUDE_VIAVEL,
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
            coord.flip = T)



pacman::p_load("esquisse")

esquisser(elites_bloco3)

#PAREI AQUI
#codigo abaixo... testando como integrar todas as questoes do bloco 3 em um só grafico pra comparar
#desejavel x possivel em cada elite.

elites_bloco3 <- as.data.frame(elites_bloco3)
labelled::to_factor(elites_bloco3, labelled_only = TRUE, strict = TRUE, unclass = TRUE)


view(elites_bloco3)
elites_bloco3_long <- pivot_longer(elites_bloco3$SETOR, 
                                   cols = (3:4), 
                                   names_to = "coluna 1", "coluna 2" , 
                                   values_to = "valores", 
                                   values_drop_na = TRUE)

head(elites_bloco3_long)
glimpse(elites_bloco3_long)                                   

=======
                             file = "~/Documents/GitHub/atlas-estado/felix/coesao/coesao.html")
colnames(coesao)

coesao %>% look_for("mulheres", labels = TRUE, ignore.case = TRUE, details = TRUE)
>>>>>>> 70bc9bf63b0e349951f618d05c43fe3ae875e8f7
