###################ANALISE DESCRITIVA DO DATASET##########

# Criar dataframes Domicilios e Individuos apenas com variaveis selecionadas

dados_ind2 <- import_ind[,c("QUEST","SEXO","IDADE","RACA","C9_D","C10_B",
                            "C10_D","J2_L","R2_C","R2_D", "C5_D","C5_F","J5",
                            "J2_L","J2_N","J3")]
#===============================================================================
# Unir objetos Domicilios e Individuos num unico objeto (numero total de
# observacoes igual a base Individuos)

base_filtro2 <- right_join(dados_dom, dados_ind2,
                          by = "QUEST")
#===============================================================================
# Converter notacao cientifica em decimal

options(scipen = 1e9)
#===============================================================================
# Selecionar apenas respostas SIM e NAO das variaveis para preparar a
#base de dados para uso no modelo preditivo.
# Excluir outras opcoes de resposta e valores faltantes

base_filtro2 <- base_filtro2 %>% 
          filter(R2_C != 97 & R2_C != 98 & R2_C != 99 & 
           R2_D != 97 & R2_D != 98 & R2_D != 99 & 
           C9_D != 97 &  C9_D != 98 & C9_D != 99 &
           J2_L != 97 & J2_L != 98 & J2_L != 99 &
           A4 != 97 & A4 != 98 & A4 != 99 &
           RENDA_FAMILIAR != 97 & RENDA_FAMILIAR != 98 & RENDA_FAMILIAR != 99&
           A1_A != 97 & A1_A != 98 & A1_A != 99&
           A1_B != 97 & A1_B != 98 & A1_B != 99 &
           A1_C != 97 & A1_C != 98 & A1_C != 99 &
           C10_D != 97 & C10_D != 98 & C10_D != 99&
           C10_B != 97 & C10_B != 98 & C10_B != 99&
           C5_D != 97 & C5_D != 98 & C5_D != 99&
           C5_F != 97 & C5_F != 98 & C5_F != 99&
           J2_N != 97 & J2_N != 98 & J2_N != 99&
           J3 != 97 & J3 != 98 & J3 != 99) %>%
         na.omit(base_filtro2)

#===============================================================================
# Alterar o nome das linhas, excluir a coluna QUEST

rownames(base_filtro2) <- base_filtro2$QUEST
base_filtro2$QUEST <- NULL
#===============================================================================
# Renomear (decodificar) as observacoes

base_filtro2_factor <- as_factor(base_filtro2) 
base_filtro2 <- droplevels(base_filtro2) #retirar categorias não utilizadas
base_filtro2_factor <- droplevels(base_filtro2_factor) #retirar categorias não utilizadas

#===============================================================================
#AREA DO DOMICILIO

table_area <- table(base_factor$AREA, base_factor$COD_REGIAO)
table_area

dom_rural <- sum(table_area[2,])/sum(table_area) #domicilios rural
dom_rural
dom_urb <- sum(table_area[1,])/sum(table_area) #domicilios urbana
dom_urb

#===============================================================================
#GENERO
summary(base_factor$SEXO)

#===============================================================================
#RESPOSTAS SIM PARA VARIAVEL-PESQUISA POR IDADE

table_var_idade <- table(base_factor$R2_C, base_factor$IDADE)
table_var_idade

#===============================================================================
#Tabela 1 do TCC
table1 <- select(base_filtro2_factor,c("A4", "A1_A","A1_B","A1_C","C5_F"))
#selecionar apenas respostas SIM para A4 (internet no domicilio)
table1 <- table1[table1$A4 == 'Sim',]
summary(table1)


#===============================================================================
#Tabela 2: tem intenet no domicilio VERSUS Acesso internet na TV 
#(todos usuarios têm celular, 323 usuarios)
table2 <- table(base_filtro2_factor$A4, base_filtro2_factor$C5_F)
table2

#porcentagem TOTAL dos usuarios que usaram internet na TV
porc1 <- (table2[1,2]+table2[2,2])/sum(table2)
porc1

#porcentagem dos usuarios que tem internet no domicilio e usaram internet na TV
porc2 <- (table2[2,2])/(table2[2,1]+table2[2,2])
porc2

#===============================================================================
# Relação dos usuários que usaram internet na TV (181) VERSUS renda familiar
table3 <- select(base_filtro2_factor,c("RENDA_FAMILIAR","C5_F"))
table3 <- table(table3)
table3

#porcentagem dos usuarios com renda familiar 1
Faixa1 <- (table3[1,2])/(table3[1,1]+table3[1,2])
#porcentagem dos usuarios com renda familiar 2
Faixa2 <- (table3[2,2])/(table3[2,1]+table3[2,2])
#porcentagem dos usuarios com renda familiar 3
Faixa3 <- (table3[3,2])/(table3[3,1]+table3[3,2])
#porcentagem dos usuarios com renda familiar 4
Faixa4 <- (table3[4,2])/(table3[4,1]+table3[4,2])
#porcentagem dos usuarios com renda familiar 5
Faixa5 <- (table3[5,2])/(table3[5,1]+table3[5,2])
#porcentagem dos usuarios com renda familiar 9
Sem_renda <- (table3[9,2])/(table3[9,1]+table3[9,2])

tv_renda <- data.frame(Faixa1, Faixa2, Faixa3,Faixa4, Faixa5,Sem_renda)
#=======================================================================
#ANALISE DESCRITIVA GRAFICA
tmp2 <- base_filtro2
tmp2 <- tmp2%>% 
  filter(  RENDA_FAMILIAR != 6  & 
           RENDA_FAMILIAR !=7&
           RENDA_FAMILIAR !=8&
           RENDA_FAMILIAR !=9)

descritiva2 <- function(var){
  # Sumariza a taxa de respondentes=SIM por categoria da variavel em analise
  tgc <- Rmisc::summarySE(tmp2, measurevar="C5_F", groupvars=c(var))
  ggplot(tgc) + 
    geom_bar(aes(x=tgc[,var], weight=N/200, fill=as.factor(tgc[,var]))) + 
    geom_errorbar(aes(x=tgc[,var], y=C5_F, ymin=C5_F-se, ymax=C5_F+se, colour='black'), width=.2) +
    geom_point(aes(x=tgc[,var], y=C5_F, colour='black', group='Sim')) +
    geom_line(aes(x=tgc[,var], y=C5_F, colour='black', group='Sim')) +
    scale_color_viridis_d(direction = -1, begin=0, end=.25) +
    scale_fill_viridis_d(direction = -1, begin=.3, end=1) +
    theme(panel.background = element_rect(fill = "white", colour = "black", linetype = "solid"),
          panel.grid.major = element_line(size = 0.15, linetype = 'dashed', colour = "grey")) +
    theme(legend.position = "none") +
    xlab("FAIXA DA RENDA FAMILIAR") + ylab("Internet na TV (%)") +
    scale_y_continuous(sec.axis = sec_axis(~.*200, name = "Respondentes"), labels = scales::percent)
    
}

descritiva2("RENDA_FAMILIAR")
graf_descritiva2 <- descritiva2("RENDA_FAMILIAR")

#=========================================================================
base_filtro2_factor$C5_F <- as.integer(base_filtro2_factor$C5_F=="Sim")

descritiva3 <- function(var){
  # Sumariza a taxa de respondentes=SIM por categoria da variavel em analise
  tgc <- Rmisc::summarySE(base_filtro2_factor, measurevar="C5_F", groupvars=c(var))
  ggplot(tgc) + 
    geom_bar(aes(x=tgc[,var], weight=N/200, fill=as.factor(tgc[,var]))) + 
    geom_errorbar(aes(x=tgc[,var], y=C5_F, ymin=C5_F-se, ymax=C5_F+se, colour='black'), width=.2) +
    geom_point(aes(x=tgc[,var], y=C5_F, colour='black', group='1')) +
    geom_line(aes(x=tgc[,var], y=C5_F, colour='black', group='1')) +
    scale_color_viridis_d(direction = -1, begin=0, end=.25) +
    scale_fill_viridis_d(direction = -1, begin=.3, end=1) +
    theme(panel.background = element_rect(fill = "white", colour = "black", linetype = "solid"),
          panel.grid.major = element_line(size = 0.15, linetype = 'dashed', colour = "grey")) +
    theme(legend.position = "none") +
    xlab("REGIÃO") + ylab("Internet na TV (%)") +
    scale_y_continuous(sec.axis = sec_axis(~.*200, name = "Respondentes"), labels = scales::percent)

    }

descritiva3("COD_REGIAO")
graf_descritiva3 <- descritiva3("COD_REGIAO")
descritiva3("IDADE")
graf_descritiva4 <- descritiva3("IDADE")
#============================================================================
tmp3 <- base_filtro2_factor
tmp3 <- tmp3%>% 
  filter(  J5 != "Não sabe"  & 
           J5 != "Não respondeu")

descritiva4 <- function(var){
  # Sumariza a taxa de respondentes=SIM por categoria da variavel em analise
  tgc <- Rmisc::summarySE(tmp3, measurevar="C5_F", groupvars=c(var))
  ggplot(tgc) + 
    geom_bar(aes(x=tgc[,var], weight=N/700, fill=as.factor(tgc[,var]))) + 
    geom_errorbar(aes(x=tgc[,var], y=C5_F, ymin=C5_F-se, ymax=C5_F+se, colour='grey'), width=.2) +
    geom_point(aes(x=tgc[,var], y=C5_F, colour='grey', group='1')) +
    geom_line(aes(x=tgc[,var], y=C5_F, colour='grey', group='1')) +
    scale_color_viridis_d(direction = -1, begin=0, end=.25) +
    scale_fill_viridis_d(direction = -1, begin=.5, end=.8) +
    theme(panel.background = element_rect(fill = "white", colour = "black", linetype = "solid"),
          panel.grid.major = element_line(size = 0.15, linetype = 'dashed', colour = "grey")) +
    theme(legend.position = "none") +
    xlab("POSSUI APARELHO CELULAR") + ylab("Internet na TV (%)") +
    scale_y_continuous(sec.axis = sec_axis(~.*700, name = "Respondentes"), labels = scales::percent)
  
}

descritiva4 ("J5")
graf_descritiva5 <- descritiva4("J5")
#==============================================================================

#Imprimir graficos na mesma tela
library(gridExtra)
grid.arrange(graf_descritiva4, graf_descritiva2,graf_descritiva3, nrow = 3)
#==========================================================================
#ANALISE SOBRE USUARIO ACESSA  INTERNET PELO CELULAR (J2_L) VERSUS SE POSSUI
#APARELHO CELULAR (J5)

table4 <- table(base_filtro2_factor$J2_L, base_filtro2_factor$J5)
table4

#porcentagem dos usuarios que acessaram internet no celular e POSSUEM celular
porc3 <- (table4[2,2])/(table4[1,1]+table4[1,2]++table4[2,1]+table4[2,2])
porc3

#porcentagem dos usuarios que acessaram internet no celular e NAO POSSUEM celular
porc4 <- (table4[2,1])/(table4[1,1]+table4[1,2]+table4[2,1]+table4[2,2])
porc4

#usuarios que possuem aparelho celular

summary(base_filtro2_factor$J5)

#usuarios que acessaram internet pelo celular

n_uso_celular <- (table4[2,1]+table4[2,2])
n_uso_celular

#porcentagem de usuarios que acessaram internet pelo celular em relação
#a amostra total (323 observacoes)
porc5 <- (n_celular)/sum(table4)
porc5
#=========================================================================
#ANALISE DO USO DA INTERNET NO CELULAR

#Usuarios que possuem computador de mesa,notebook,tablet e celular
table5 <- select(base_filtro2_factor, c("A1_A", "A1_B","A1_C","J5"))
summary(table5)

#No celular: usou internet (J3), info Google (J2_L), redes sociais (J2_N)
table6 <- select(base_filtro2_factor, c("J3", "J2_L","J2_N"))
summary(table6)
#=========================================================================

