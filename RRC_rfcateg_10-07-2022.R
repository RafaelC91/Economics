
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
# dadoscapA$dist_cen<-dadoscapA_coord$DIST_CEN_M
# dadoscapH$dist_cen<-dadoscapH_coord$DIST_CEN_M

rm(dadoscapA_coord,dadoscapH_coord)

dados<-rbind(dadoscapA,dadoscapH)
rm(dadoscapA,dadoscapH)

distritos_lb<-dados |> 
  group_by(DISTRITO) |> 
  slice_head() |> 
  pull(DISTRITO)

categ_lb<-dados |> 
  group_by(categ) |> 
  slice_head() |> 
  pull(categ)

dados_imp<-dados |> 
  as.data.frame() |> 
  select(Rprice,
         bed,bath_imp,garage_imp,area_imp,
         categ,tipo,DISTRITO) |> 
  mutate(bath_imp=parse_number(as.character(bath_imp)),
         garage_imp=parse_number(as.character(garage_imp)),
         distrito=factor(DISTRITO,levels=distritos_lb),
         categ=factor(categ,levels=categ_lb)) |> 
  select(-c(DISTRITO,tipo,distrito)) |> 
  na.omit()
rm(distritos_lb,categ_lb)

#### ADICIONA VARS
dados_imp<-dados_imp |> 
  mutate(bed_area=bed*area_imp,
         bath_area=bath_imp*area_imp,
         garage_area=garage_imp*area_imp)

# com RPrice
# dec_Rprice<-quantile(dados_imp$Rprice, prob = seq(0, 1, length = 21), type = 5)
# dec_Rprice[1]<-dec_Rprice[1]-1
# 

#Linear relationship

dados_imp |> 
  mutate(categ=if_else(categ=="Apartamentos",1,0)) |> 
  correlate(method = "pearson", use = "pairwise.complete.obs") |> 
  network_plot( min_cor = 0 ,curved=F)


 dadosrf_Rprice<-dados_imp
#   ungroup() |> 
#   mutate(Rprice_cut=cut(Rprice/1000,
#                         breaks=dec_Rprice/1000,
#                         ordered_result = T,
#                         dig.lab=3)) |> 
#   na.omit() |> 
#   select(-c(Rprice))
# table(dadosrf_Rprice$Rprice_cut) #mantem cut para comparação ao final

 
 
 
 
 
 
 

set.seed(1)

partition <- createDataPartition(y = dadosrf_Rprice$categ, p = 0.8, list = FALSE)
train_data <- dadosrf_Rprice[partition, ]
test_data <- dadosrf_Rprice[-partition, ]

#
preProcess(train_data)


#RandomForest
set.seed(100) 

categ_rf <- randomForest(
  categ ~ .,
  data = dadosrf_Rprice,
  ntree = 50,
  mtry = 3,
  importance = T
)

# avaliação
# no conjunto de treino
p_treino <- predict(categ_rf, train_data, type='prob') # Probabilidade predita
c_treino <- predict(categ_rf, train_data)              # Classificação

# no conjunto de teste
p_teste <- predict(categ_rf, test_data, type='prob')
c_teste <- predict(categ_rf, test_data)

# resultados de treino
res_treino <- data.frame(obs = train_data$categ, 
                         pred = c_treino,
                         Casas = p_treino[ , 2],
                         Apartamentos = p_treino[ , 1] 
                         )


head(res_treino)
ggplot(res_treino)+
  geom_point(aes(x=pred,y=obs),position = "jitter")

# resultados de teste
res_teste <- data.frame(obs = test_data$categ, 
                        pred = c_teste,
                        Casas = p_teste[ , 2],
                        Apartamentos = p_teste[ , 1])

ggplot(res_teste)+
  geom_point(aes(x=pred,y=obs),position = "jitter")

# two class summary para o conjunto de treinamento
tcs_treino <- caret::twoClassSummary(res_treino, 
                                     lev=levels(res_treino$obs))
tcs_treino

# two class summary para o conjunto de teste
tcs_teste <- caret::twoClassSummary(res_teste, 
                                    lev= levels(res_teste$obs))
tcs_teste


# Kappa: 

cm_rf <- confusionMatrix(res_teste$pred, res_teste$obs, positive = "Casas")
cm_rf

cm_rf <- confusionMatrix(res_teste$pred, res_teste$obs, positive = "Apartamentos")
cm_rf



# ----------------------------------------------------

# Curva ROC              
df_roc_curve = res_teste

names(df_roc_curve) = c("truth", "Class2", "Class1", "predicted")


roc_curve1 <- ggplot2::ggplot(res_teste, aes(d = obs, m = Apartamentos )) + 
  plotROC::geom_roc(n.cuts = 0, color="blue") +
  plotROC::geom_roc(data=res_treino,
                    aes(d = obs, m = Apartamentos ),
                    n.cuts = 0, color = "red") +
  scale_color_viridis_d(direction = -1, begin=0, end=.25) +
  theme(legend.position = "none") +
  ggtitle(paste0("Curva ROC ", "| AUC-treino = ",
                 round(tcs_treino[1]*100, 2), "%",
                 "| AUC-teste = ",
                 round(tcs_teste[1]*100, 2), "%")
  )

roc_curve1


####

auc_rf_teste <- ModelMetrics::auc(actual = res_teste$obs, predicted = res_teste$pred)
auc_rf_treino <- ModelMetrics::auc(actual = res_treino$obs, predicted = res_treino$pred)

df_roc_curve <- res_teste

names(df_roc_curve) = c("truth", "estimate", "Class1", "Class2")

options(yardstick.event_first = FALSE) # por default o programa considerava que a primeira classe era 'No'

yardstick::roc_curve(df_roc_curve, truth, Class1 ) |>
  autoplot() +
  labs(title = "Real Estate Akinator",
       subtitle = paste0("AUC = ", round(auc_rf_teste, 4)) )

yardstick::roc_curve(df_roc_curve, truth, Class2 ) |>
  autoplot() +
  labs(title = "Real Estate Akinator",
       subtitle = paste0("AUC = ", round(auc_rf_teste, 4)) )

#####

# ----------------------------------------------------
# usando o pacote caret
# ----------------------------------------------------

set.seed(123)

# Specify 10-fold cross validation
ctrl <- trainControl(method = "cv",  number = 15) 

categ_caret_rf <- train(
  categ ~ .,
  data = dadosrf_Rprice,
  method = "treebag",
  trControl = ctrl,
  importance = TRUE
)

categ_caret_rf
categ_caret_rf$finalModel
plot(varImp(categ_caret_rf))  
varImp(categ_caret_rf) 


# ----------------------------------------------------
# previsao
# ----------------------------------------------------

pred <- predict(categ_caret_rf, test_data)

cm_caret_rf <- confusionMatrix(pred, test_data$categ, positive = "Apartamentos")
cm_caret_rf

# ----------------------------------------------------
# Comparando os dois modelos
# ----------------------------------------------------

# note que o objeto matriz de confusao possui varias propriedades
cm_caret_rf$byClass # modelo 2 com caret e cv

cm_rf$byClass # modelo 1 com randomForest





# ----------------------------------------------------

# grid-search com caret

# ----------------------------------------------------

# number: number of folds for training
# repeats: keep the number for training

# O objeto gerado por trainControl vai controlar o algoritmo 
controle <- caret::trainControl(
  method = 'repeatedcv', # Solicita um K-Fold com repetições
  number = 10, # Número de FOLDS (o k do k-fold)
  repeats = 5, # Número de repetições
  search = 'grid', # especifica o grid-search
  summaryFunction = twoClassSummary, # Função de avaliação de performance
  classProbs = TRUE # Necessário para calcular a curva ROC
)

# agora vamos especificar o grid
grid <- base::expand.grid( .mtry=c(1:10) )

# Vamos treinar todos os modelos do grid-search com cross-validation
categ_gridsearch_rf <- caret::train(categ ~ .,         # Fórmula (todas as variáveis)
                              data = train_data,    # Base de dados
                              method = 'rf',        # Random-forest
                              metric ='ROC',        # Escolhe o melhor por essa métrica
                              trControl = controle, # Parâmetros de controle do algoritmo
                              ntree = 50,          # Numero de árvores
                              tuneGrid = grid)      # Percorre o grid especificado aqui

plot(categ_gridsearch_rf)



# ----------------------------------------------------

# avaliando o modelo tunado

# ----------------------------------------------------

pred_tuned <- predict(categ_gridsearch_rf, test_data)

cm_rf_tuned <- confusionMatrix(pred_tuned, test_data$categ, positive = "Apartamentos")
cm_rf_tuned


# ----------------------------------------------------
# Comparando os tres modelos
# ----------------------------------------------------

# acuracia

acuracia = c( cm_rf$overall[1], cm_caret_rf$overall[1], cm_rf_tuned$overall[1] )
names(acuracia) = c("RF", "Caret", "Tuned")

acuracia

cm_rf$byClass # modelo 1 com randomForest

cm_caret_rf$byClass # modelo 2 com caret e cv

cm_rf_tuned$byClass # modelo 3 tunado



## grafico
graphfinal<-rbind(cm_rf$byClass,cm_caret_rf$byClass,cm_rf_tuned$byClass) |> 
  as.data.frame()
graphfinal$model<-c("01-rf","02-caret","03tuned")
graphfinal<-graphfinal |> 
  pivot_longer(-model,names_to = "key",values_to = "value")

graphfinal |> 
  ggplot()+
  geom_col(aes(x=value,y=(model),fill=model))+
  geom_text(aes(x=0,y=model,label=round(value,3)),hjust=-0.1)+
  scale_y_discrete(limits=rev(c("01-rf","02-caret","03tuned")))+
  xlab(NULL)+
  ylab(NULL)+
  facet_wrap(~key)+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.title = element_blank())




