# Procedimentos de data wrangling da base de microdados TIC Domicilios 2020
#25/09/2022
#===============================================================================
# Carregamento (e instalacao, se necessario) de pacotes requeridos
if (!require("install.load")) install.packages("install.load")
suppressMessages(install.load::install_load("car","tidyverse", "survey","haven",
                                            "labelled","dplyr","rpart",
                                            "rpart.plot",'stringr', "caret",
                                            "ggplot2","gridExtra"))
#===============================================================================
# carregar a base de microdados a partir do painel Files
# Microdados TIC Domicilios 2020: ticdom_2020_base_de_microdados_v1.0.RData
# Sao geradas duas bases de dados: basedom e baseind

load("ticdom_2020_base_de_microdados_v1.0.rdata")
#===============================================================================
# Converter os objetos basedom e baseind em dataframes

import_dom <- as.data.frame(basedom[["variables"]]) 
import_ind <- as.data.frame(baseind[["variables"]])
#===============================================================================
# Criar dataframes Domicilios e Individuos apenas com variaveis selecionadas

dados_dom <- import_dom[,c("QUEST","AREA","COD_REGIAO","RENDA_FAMILIAR","A1_A",
                            "A1_B","A1_C","A4")]

dados_ind <- import_ind[,c("QUEST","SEXO","IDADE","RACA","C9_D","C10_B",
                            "C10_D","J2_L","R2_C","R2_D","GRAU_INSTRUCAO")]
#===============================================================================
#cRIAR DATASET
# Unir objetos Domicilios e Individuos num unico objeto (numero total de
# observacoes igual a base Individuos)

base_filtro <- right_join(dados_dom, dados_ind,
          by = "QUEST")
#===============================================================================
# Converter notacao cientifica em decimal

options(scipen = 1e9)
#===============================================================================
# Remover categorias não sabe, não respondeu, não aplicável
#Nao se aplica para variavel RACA
# Remover valores faltantes NA

base_filtro <- base_filtro %>% 
                    filter(R2_C != 97 & R2_C != 98 & R2_C != 99 & 
                             R2_D != 97 & R2_D != 98 & R2_D != 99 & 
                             C9_D != 97 &  C9_D != 98 & C9_D != 99 &
                             J2_L != 97 & J2_L != 98 & J2_L != 99 &
                             A4 != 97 & A4 != 98 & A4 != 99 &
                             RENDA_FAMILIAR != 97 & RENDA_FAMILIAR != 98 & 
                             RENDA_FAMILIAR != 99&
                             A1_A != 97 & A1_A != 98 & A1_A != 99&
                             A1_B != 97 & A1_B != 98 & A1_B != 99 &
                             A1_C != 97 & A1_C != 98 & A1_C != 99 &
                             C10_D != 97 & C10_D != 98 & C10_D != 99&
                             C10_B != 97 & C10_B != 98 & C10_B != 99) %>%
                     na.omit(base_filtro)

#===============================================================================
# Alterar o nome das linhas, excluir a coluna QUEST

rownames(base_filtro) <- base_filtro$QUEST
base_filtro$QUEST <- NULL
#===============================================================================
# Renomear (decodificar) as observacoes

base_factor <- as_factor(base_filtro)
#===============================================================================
# Verificacao da Dimensao e Nomes das variaveis do dataset base_factor

dim(base_factor)
names(base_factor)

#===============================================================================
# Arvore de Classificacao, Raiz = Respondente sabe verificar se informacao 
#esta certa R2_C = 1

arvore <- rpart(R2_C ~ AREA+COD_REGIAO+RENDA_FAMILIAR+A1_C+A1_A+A1_B+A4+SEXO+
                  IDADE+RACA+C9_D+C10_B+C10_D+J2_L+R2_D,
                data=base_factor,parms = list(split = 'gini'),method='class')

#===============================================================================
# Visualizacao da arvore
# Definicao da paleta de cores

paleta = scales::viridis_pal(begin=.5, end=.6)(20)

# Plotar a Arvore

rpart.plot::rpart.plot(arvore, box.palette = paleta)

#===============================================================================
# Avaliacao elementar da arvore
arvore

# Predicao com a arvore
#Criar arvore com dataset base_filtro (variaveis codificadas)
#Arvore2 possui mesmo crescimento da Arvore.

arvore2 <- rpart(R2_C ~ AREA+COD_REGIAO+RENDA_FAMILIAR+A1_C+A1_A+A1_B+A4+SEXO+
                  IDADE+RACA+C9_D+C10_B+C10_D+J2_L+R2_D,
                data=base_filtro,parms = list(split = 'gini'),method='class')

# Criar funcao para verificar a probabilidade da resposta Sim.
prob = predict(arvore2, base_filtro) # R2_C=1

# Classificacao dos respondentes R2_C = Sim (=class)
class = prob[,2]>.5

# Matriz de confusao
tab <- table(class, base_filtro$R2_C)
tab

# Calculo da acuracia
acc <- (tab[1,1] + tab[2,2])/ sum(tab)
acc

#===============================================================================
# CURVA ROC PARA ARVORE

p_arvore = stats::predict(arvore2, base_filtro)
c_arvore = base::factor(ifelse(p_arvore[,2]>.5, "Y", "N"))


# Avaliar a árvore

aval_arvore <- data.frame(obs=base_filtro$R2_C, 
                          pred=c_arvore,
                          Y = p_arvore[,2],
                          N = 1-p_arvore[,2]
)

# Usar o mesmo dataframe para plotar a curva ROC:
CurvaROC <- ggplot(aval_arvore, aes(d = obs, m = Y, colour='a')) + 
  plotROC::geom_roc(n.cuts = 0) +
  scale_color_viridis_d(direction = -1, begin=0, end=.1) +
  theme(legend.position = "none") +
  ggtitle("Curva ROC para Árvore de Classificação")

CurvaROC


## AREA SOB A CURVA (AUC)
aval_arvore_factor <- as_factor(aval_arvore)
summary(aval_arvore_factor$obs)

aval_arvore_factor <- aval_arvore_factor %>% mutate(obs=droplevels(obs))
levels(aval_arvore_factor$obs)
levels(aval_arvore_factor$obs) <- c('N','Y')
summary(aval_arvore_factor$obs)
caret::multiClassSummary(aval_arvore_factor, lev=levels(aval_arvore_factor$obs))
summary(arvore)


#===============================================================================
# ANALISE DESCRITIVA PARA ARVORE
# base temporaria para manter a base original intacta
tmp <- base_factor
tmp$R2_C <- as.integer(base_factor$R2_C=="Sim")

#===============================================================================
# Funcao para analise descritiva para variavel R2_C = "Sim" (group='1')

descritiva <- function(var){
  # Sumariza a taxa de respondentes=SIM por categoria da variavel em analise
  tgc <- Rmisc::summarySE(tmp, measurevar="R2_C", groupvars=c(var))
    ggplot(tgc) + 
      geom_bar(aes(x=tgc[,var], weight=N/150, fill=as.factor(tgc[,var]))) + 
      geom_errorbar(aes(x=tgc[,var], y=R2_C, ymin=R2_C-se, ymax=R2_C+se, colour='blue'), width=.2) +
      geom_point(aes(x=tgc[,var], y=R2_C, colour='blue', group='1')) +
      geom_line(aes(x=tgc[,var], y=R2_C, colour='blue', group='1')) +
      scale_color_viridis_d(direction = -1, begin=0, end=.25) +
      scale_fill_viridis_d(direction = -1, begin=.3, end=1) +
      theme(panel.background = element_rect(fill = "white", colour = "black", linetype = "solid"),
      panel.grid.major = element_line(size = 0.15, linetype = 'dashed', colour = "grey")) +
      theme(legend.position = "none") +
      xlab("REGIÃO") + ylab("Sabe checar notícias (%)") + 
      scale_y_continuous(sec.axis = sec_axis(~.*150, name = "Respondentes"), labels = scales::percent)
}

descritiva("A4")
descritiva("SEXO")
descritiva ('IDADE')
descritiva ('AREA')
descritiva ("R2_D")
descritiva("COD_REGIAO")

#===============================================================================




