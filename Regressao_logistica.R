##################################################################################
#                  REGRESSAO LOGISTICA           #
##################################################################################
#Pacotes utilizados
pacotes <- c("plotly","tidyverse","knitr","kableExtra","fastDummies","rgl","car",
             "reshape2","jtools","lmtest","caret","pROC","ROCR","nnet","magick",
             "cowplot")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#############################################################################
# CRIAR DUMMIES PARA VARIAVEIS CATEGORICAS POLICOTOMICAS

base_dummy <- dummy_columns(.data = base_filtro,
                              select_columns = c("AREA", 
                                                 "COD_REGIAO",
                                                 "SEXO"),
                              remove_selected_columns = T,
                              remove_first_dummy = T)


#========================================================================
#MODELO COM VARIAVEIS SELECIONADAS    
#selecao de variaveis a partir da arvore de decisao
  
modelo_dummy <- glm(formula = R2_C ~ R2_D+AREA_2+C9_D+COD_REGIAO_2+
                                   COD_REGIAO_3+COD_REGIAO_4+COD_REGIAO_5+A4+
                                   IDADE+C10_D+SEXO_2, 
                                 data = base_dummy, 
                                 family = "binomial")

#Parametros do modelo
summary(modelo_dummy)

#Valor do LL do modelo
logLik(modelo_dummy)


#Outro modo de apresentar os outputs do modelo
summ(modelo_dummy, confint = T, digits = 3, ci.width = .95)
export_summs(modelo_dummy, scale = F, digits = 6)
  

#=============================================================================  
# matriz de confusao modelo
  confusionMatrix(
    table(predict(modelo_dummy, type = "response") >= 0.5, 
          base_dummy$R2_C == 1)[2:1, 2:1]
  )
#===============================================================================
#Predicao com o modelo
# Qual a probabilidade media saber verificar?
  predict(object = modelo_dummy,
          data.frame(R2_D =1, AREA_2 =0, C9_D =1, COD_REGIAO_2 =1, COD_REGIAO_3 =0, 
                       COD_REGIAO_4 =0, COD_REGIAO_5 =0, A4 =1, IDADE =15, C10_D =0, 
                       SEXO_2=1),
          type = "response")
  
  #=============================================================================
  #Curva ROC para o modelo
   
  ROC <- roc(response = base_dummy$R2_C, 
             predictor = modelo_dummy$fitted.values)
  
ggplotly(
    ggroc(ROC, color = "#0000FF", size = 1) +
      geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1),
                   color="grey40",
                   size = 0.2) +
      labs(x = "Especificidade",
           y = "Sensitividade",
           title = paste("Área sob a curva:",
                         round(ROC$auc, 2),
                         "|",
                         "Índice Gini",
                         round((ROC$auc[1] - 0.5) / 0.5, 2))) +
      theme_bw()
  )
  
#==========================================================================

