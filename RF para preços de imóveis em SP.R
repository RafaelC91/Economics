library(data.table) # leitura de arquivo
library(dplyr) # processamento de dados
# library(ggplot2) # visualizacao
library(tidyverse) # arrumação de dados

library(knitr) # viz de tabelas
library(kableExtra) # viz de tabelas

library(gtools) 
library(rpart) # para o cart
library(rpart.plot) # para plotar a árvore
library(rattle) # plot analysis for data mining
library(yardstick) # para a curva roc - há também o pacote pROC

library(caret) # para o ajuste de modelos (reg e class)
library(randomForest)
library(fastDummies)
library(xgboost) # eXtreme Gradient Boosting Training


library(report) # para relatorios de eda
library(DataExplorer) # para relatorios de eda
library(skimr) # para relatorios de eda
library(corrr) # network_plot


require(rattle)
rm(list=ls())
options(scipen=999999)

rm(list=ls())
#  sincronizar google drive entre computadores
setwd(paste0(sub(
  "trabalho/.*",
  "",
  dirname(rstudioapi::getSourceEditorContext()$path)
), "trabalho"))


#dados espacial
load("database/LGFSRR_housing_imputed_geocenter_2022-06-11.RData")
names(dadoscapA_coord)
dadoscapA$dist_cen<-dadoscapA_coord$DIST_CEN_M
dadoscapH$dist_cen<-dadoscapH_coord$DIST_CEN_M

rm(dadoscapA_coord,dadoscapH_coord)

dados<-rbind(dadoscapA,dadoscapH)
rm(dadoscapA,dadoscapH)

table(is.na(dados$dist_cen))
table(is.na(dados$DISTRITO))
mean(dados$dist_cen,na.rm = TRUE)

dist_cen<-dados |> 
  group_by(DISTRITO) |> 
  summarise(dist_cen_mean=mean(dist_cen,na.rm=TRUE)) |> 
  ungroup()

distritos_lb<-dados |> 
  group_by(DISTRITO) |> 
  slice_head() |> 
  pull(DISTRITO)

categtipo_lb<-dados |> 
  mutate(categtipo=paste0(categ,"-",tipo)) |> 
  group_by(categtipo) |> 
  slice_head() |> 
  pull(categtipo)

dados_imp<-dados |> 
  as.data.frame() |> 
  left_join(dist_cen,by=c("DISTRITO")) |> 
  select(Rprice,
         bed,bath_imp,garage_imp,area_imp,dist_cen,dist_cen_mean,
         categ,tipo,DISTRITO) |> 
  mutate(bath_imp=parse_number(as.character(bath_imp)),
         garage_imp=parse_number(as.character(garage_imp)),
         categtipo=paste0(categ,"-",tipo),
         distrito=factor(DISTRITO,levels=distritos_lb),
         categtipo=factor(categtipo,levels=categtipo_lb)) |> 
  select(-c(DISTRITO,categ,tipo,distrito)) |> 
  na.omit()
rm(categtipo_lb,distritos_lb,dist_cen)

#### ADICIONA VARS
dados_imp<-dados_imp |> 
  mutate(bed_area=bed*area_imp,
         bath_area=bath_imp*area_imp,
         garage_area=garage_imp*area_imp)

# com RPrice
dec_Rprice<-quantile(dados_imp$Rprice, prob = seq(0, 1, length = 21), type = 5)
dec_Rprice[1]<-dec_Rprice[1]-1

dadosrf_Rprice<-dados_imp |> 
  ungroup() |> 
  mutate(Rprice_cut=cut(Rprice/1000,
                        breaks=dec_Rprice/1000,
                        ordered_result = T,
                        dig.lab=3)) |> 
  na.omit() |> 
  select(-c(Rprice))
table(dadosrf_Rprice$Rprice_cut) #mantem cut para comparação ao final


set.seed(1)

partition <- createDataPartition(y = dadosrf_Rprice$Rprice_cut, p = 0.8, list = FALSE)
train_data <- dadosrf_Rprice[partition, ]
test_data <- dadosrf_Rprice[-partition, ]

set.seed(100) 

#RandomForest
price_cut_rf <- randomForest(
  Rprice_cut ~ .,
  data = dadosrf_Rprice,
  ntree = 50,
  mtry = 3,
  importance = T
)

# avaliação
# no conjunto de treino
p_treino <- predict(price_cut_rf, train_data, type='prob') # Probabilidade predita
c_treino <- predict(price_cut_rf, train_data)              # Classificação

# no conjunto de teste
p_teste <- predict(price_cut_rf, test_data, type='prob')
c_teste <- predict(price_cut_rf, test_data)

# resultados de treino
res_treino <- data.frame(obs = train_data$Rprice_cut, 
                         pred = c_treino
                         )


head(res_treino)
ggplot(res_treino)+
  geom_point(aes(x=pred,y=obs),position = "jitter")

# resultados de teste
res_teste <- data.frame(obs = test_data$Rprice_cut, 
                        pred = c_teste
                        )

ggplot(res_teste)+
  geom_point(aes(x=pred,y=obs),position = "jitter")

# Kappa: 
cm_rf <- confusionMatrix(res_teste$pred, res_teste$obs, positive = "Yes")
cm_rf

# ----------------------------------------------------

# Curva ROC              

# ----------------------------------------------------
# 
# df_roc_curve = res_teste
# 
# names(df_roc_curve) = c("truth", "Class2", "Class1", "predicted")

roc_curve1 <- ggplot2::ggplot(res_teste, aes(d = obs, m = Yes )) + 
  
  plotROC::geom_roc(n.cuts = 0, color="blue") +
  
  plotROC::geom_roc(data=res_treino,
                    aes(d = obs, m = Yes ),
                    n.cuts = 0, color = "red") +
  
  scale_color_viridis_d(direction = -1, begin=0, end=.25) +
  
  theme(legend.position = "none") +

  ggtitle(paste0("Curva ROC ", "| AUC-treino = ",
                 round(tcs_treino[1]*100, 2), "%",
                 "| AUC-teste = ",
                 round(tcs_teste[1]*100, 2), "%")
  )
?geom_roc

roc_curve1

