# Packages ----------------------------------------------------------------
library(cluster)    
library(dendextend) 
library(factoextra) 
library(fpc)        
library(tidyverse)
library(sf)
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(ca)
library(cabootcrs)

library(hrbrthemes) # para o theme_ipsum
library(viridis)
rm(list=ls())


#  sincronizar google drive entre computadores
setwd(paste0(sub(
  "trabalho/.*",
  "",
  dirname(rstudioapi::getSourceEditorContext()$path)
), "trabalho"))



# A GET DATA --------------------------------------------------------------
load("database/LGFSRR_housing_imputed_2022-05-23.RData")
rm(dados_imprf)

# A-1 set analysis range (fazer isso em todas analise cluster)
dados_imp<-dados_imp |> 
  filter(Rprice %in% 1:50000 & area_imp %in% 10:5000) |> 
  mutate(Rpricearea=Rprice/area_imp)

# A-2 Apartment and House data (aqui nem precisa... mas fica ai o cod)
# dadosh<-dados_imp |> 
#   filter(categ=="Apartamentos")
# dadosa<-dados_imp |> 
#   filter(categ=="Casas")


## cluster
# Capital (SP)
dadoscap<-dados_imp|>
  as.data.frame() |> 
  filter(city_nm=="São Paulo")|>
  select(district,categ,tipo,Rprice,Rpricearea,area_imp,bed,bath_imp,garage_imp)

dadoscap<-dadoscap |> 
  group_by(district,categ) |> 
  summarize(Rprice=mean(Rprice),
            Rpricearea=mean(Rpricearea),
            area_imp=mean(area_imp),
            bed=mean(bed),
            bath_imp=mean(as.numeric(bath_imp)),
            garage_imp=mean(as.numeric(garage_imp)))

# dados apto
dadoscapA<-dadoscap |> 
  ungroup() |> 
  filter(categ=="Apartamentos") |> 
  select(-categ,-Rprice,-area_imp) |> 
  column_to_rownames("district") 

dadoscapAstd<-dadoscapA |> 
  scale() 

?scale

# dados casa
dadoscapH<-dadoscap |> 
  ungroup() |> 
  filter(categ=="Casas") |> 
  select(-categ,-Rprice,-area_imp) |> 
  column_to_rownames("district") 

dadoscapHstd<-dadoscapH |> 
  scale() 

dadoscapAstd |>
  as.data.frame() |>
  pivot_longer(everything(), names_to = "key", values_to = "value") |>
  ggplot(aes(x = key, y = value), color = factor(key)) +
  geom_violin(aes(y = value, fill = factor(key)), alpha = 0.5) +
  geom_boxplot(aes(y = value, fill = factor(key)),
               width = 0.25,
               notch = F) +
  facet_wrap(vars(key), nrow = 2, ncol = 2,scale="free") +
  theme_light()

dadoscapHstd |>
  as.data.frame() |>
  pivot_longer(everything(), names_to = "key", values_to = "value") |>
  ggplot(aes(x = key, y = value), color = factor(key)) +
  geom_violin(aes(y = value, fill = factor(key)), alpha = 0.5) +
  geom_boxplot(aes(y = value, fill = factor(key)),
               width = 0.25,
               notch = F) +
  facet_wrap(vars(key), nrow = 2, ncol = 2,scale="free") +
  theme_light()

# gera MATRIZ e clusters
matriz_capA <- dist(dadoscapAstd, method = "euclidean")
matriz_capH <- dist(dadoscapHstd, method = "euclidean")

?dist

hcWA <- hclust(matriz_capA, method = "ward.D" )
hcWH <- hclust(matriz_capH, method = "ward.D" )


hcWA1 <- hclust(matriz_capA, method = "complete")  ### por outros Critérios de agrupamento (distância entre grupos/método de encadeamento)
hcWH1 <- hclust(matriz_capH, method = "complete")

hcWA2 <- hclust(matriz_capA, method = "single") 
hcWH2 <- hclust(matriz_capH, method = "single")

hcWA3 <- hclust(matriz_capA, method = "average") 
hcWH3 <- hclust(matriz_capH, method = "average")


?hclust

#criar grupos para bairros
grupoA <- cutree(hcWA, k = 7) 
grupoH <- cutree(hcWH, k = 7) 

?cutree

### aptos

plot(hcWA, cex = 0.6, hang = -1)
rect.hclust(hcWA, k = 7)               ### 7 cortes para ward Aptos parece bom

plot(hcWA1, cex = 0.6, hang = -1)
rect.hclust(hcWA1, k = 7)              ### 7 para complete

plot(hcWA2, cex = 0.6, hang = -1)      ### 7 para single (ruim)
rect.hclust(hcWA2, k = 7)       

plot(hcWA3, cex = 0.6, hang = -1)      ### 7 para average
rect.hclust(hcWA3, k = 7) 


#### casas

plot(hcWH, cex = 0.6, hang = -1)       ### 7 cortes para ward 
rect.hclust(hcWH, k = 7)

plot(hcWH1, cex = 0.6, hang = -1)      ### 7 cortes para complete
rect.hclust(hcWH1, k = 7)

plot(hcWH3, cex =0.6, hang = -1)      ### 7 para average
rect.hclust(hcWH3, k = 7) 

dend1 <- as.dendrogram(hcWA)         ### comparar ward com average para aptos
dend2 <- as.dendrogram(hcWA3)
dend_list <- dendlist(dend1, dend2) 
tanglegram(dend1, dend2, main = paste( "Emaranhado = ", round(entanglement(dend_list), 2) ) )
# EMARANHADO, quanto menor, mais iguais os dendogramas sao. No caso deu 0.28

nrow(dadoscapA)==length(grupoA)
nrow(dadoscapH)==length(grupoH)

# adicionar grupos ao dados n normalizados 
dadoscapA<-as.data.frame(dadoscapA)
dadoscapA$grupo <- grupoA

dadoscapH<-as.data.frame(dadoscapH)
dadoscapH$grupo <- grupoH

descritiva_dadoscapA <- dadoscapA |>
  group_by( grupo ) |> 
  summarise(n = n(),
            Rpricearea  = mean( Rpricearea ), 
            bed  = mean(bed ),
            bath_imp = mean(bath_imp ),
            garage_imp = mean(garage_imp)) |> 
  ungroup()

descritiva_dadoscapH <- dadoscapH |>
  group_by( grupo ) |> 
  summarise(n = n(),
            Rpricearea  = mean( Rpricearea ), 
            bed  = mean(bed ),
            bath_imp = mean(bath_imp ),
            garage_imp = mean(garage_imp)) |> 
  ungroup()
descritiva_dadoscapH


#SAVE GROUPS
##########(importante salvar os grupos para análises posteriores)########
########## importante alterar o nome para analises com numero de clusters diferentes########
write.csv(dadoscapA,
          paste0("database/01 cluster/",
                 "LGRR_cluster07_spcap_apto_",
                 Sys.Date(),
                 ".csv"))
write.csv(dadoscapH,
          paste0("database/01 cluster/",
                 "LGRR_cluster07_spcap_hous_",
                 Sys.Date(),
                 ".csv"))

## FUTHER ANALYSIS
#brincando....
dadoscapA |> 
  ggplot()+
  geom_point(aes(y=Rpricearea,x=bed,color=factor(grupo)))
dadoscapA |> 
  ggplot()+
  geom_point(aes(y=Rpricearea,x=bath_imp,color=factor(grupo)))
dadoscapA |> 
  ggplot()+
  geom_point(aes(y=Rpricearea,x=garage_imp,color=factor(grupo)))

dadoscapA |> 
  ggplot()+
  geom_point(aes(y=bath_imp,x=bed,color=factor(grupo)))


### GRAFICO DOS GRUPOS
descritiva_dadoscapA |>
  as.data.frame() |>
  mutate(Rpricearea=Rpricearea/10) |> 
  pivot_longer(Rpricearea:garage_imp, names_to = "key", values_to = "value") |>
  ggplot() +
  geom_col(aes(y = value, x=factor(key),fill = factor(key)), alpha = 0.5) +
  facet_wrap(vars(grupo), nrow = 1, ncol = 7,scale="fixed") +
  theme_light()

descritiva_dadoscapH |>
  as.data.frame() |>
  mutate(Rpricearea=Rpricearea/10) |> 
  pivot_longer(Rpricearea:garage_imp, names_to = "key", values_to = "value") |>
  ggplot() +
  geom_col(aes(y = value, x=factor(key),fill = factor(key)), alpha = 0.5) +
  facet_wrap(vars(grupo), nrow = 1, ncol = 7,scale="fixed") +
  theme_light()

# PARAR DIFERENTES CLUSTERS, ALTERAR OS ARQUIVOS AQUI
cA<-read_csv("database/01 cluster/LGRR_cluster07_spcap_apto_2022-06-07.csv") # 7 grupos para aptos
cH<-read_csv("database/01 cluster/LGRR_cluster07_spcap_hous_2022-06-07.csv") # 7 grupos para casas

#geodata
geodata<-read_csv("database/SQL/LGRR_tab_location_SP_capital_DISTRICTS_2022-06-05.csv")

#shape
spshp<-read_sf(dsn="database/SQL/geodatabase/LAYER_DISTRITO",
               layer="DEINFO_DISTRITO") #shape file da cidade de SP com distritos

logshp<-read_sf(dsn="database/SQL/geodatabase/LOG2020_CEM_RMSP",
                layer="LOG2020_CEM_RMSP") #shape file com linhas dos logradouros
logshp<-logshp |> 
  filter(ID %in% geodata$ID) #filtra os logradouros pelos logradouros da base

logshp_center <- st_transform(logshp,32617) #coverte para ISO (formato + adequado para calculo de centroide)
logshp_center <- st_centroid(logshp_center) #calculando o ponto central da Rua(anterir era traço da rua)

# A-1 set analysis range (magic happens, bitch!)
dados_imp<-dados_imp |> 
  filter(Rprice %in% 1:50000 & area_imp %in% 10:5000) |> 
  mutate(Rpricearea=Rprice/area_imp)

# Capital (SP)
dadoscap<-dados_imp|>
  as.data.frame() |> 
  filter(city_nm=="São Paulo")|>
  select(adID,district,categ,tipo,Rprice,Rpricearea,area_imp,bed,bath_imp,garage_imp) |> 
  left_join(geodata,
            by="adID")
#rm(dados_imp,geodata)

dadoscapA<-dadoscap |> 
  filter(categ=="Apartamentos") |> 
  left_join(cA[,c("...1","grupo")],
            by=c("district.x"="...1")) 

dadoscapH<-dadoscap |> 
  filter(categ=="Casas")|> 
  left_join(cH[,c("...1","grupo")],
            by=c("district.x"="...1")) 
#rm(dadoscap,cA,cH)

## MAPA
dadoscapA_coord<-right_join(logshp_center,dadoscapA,
                            by="ID")
#dadoscapA_coord

ggplot(data = spshp) +
  geom_sf()+
  geom_sf(data=dadoscapA_coord,aes(colour=factor(grupo)))

dadoscapH_coord<-right_join(logshp_center,dadoscapH,
                            by="ID")
ggplot(data = spshp) +
  geom_sf()+
  geom_sf(data=dadoscapH_coord,aes(colour=factor(grupo)))

#### MAPAS SEM CLUSTER (PARA APROVEITAR EM OUTROS MOMENTOS)
ggplot(data = spshp) +
  geom_sf()+
  geom_sf(data=dadoscapA_coord,aes(colour=(Rpricearea)))

ggplot(data = spshp) +
  geom_sf()+
  geom_sf(data=dadoscapA_coord,aes(colour=Rprice))+
  scale_color_viridis_c(direction=-1,option="inferno")


ggplot(data = spshp) +
  geom_sf()+
  geom_sf(data=dadoscapH_coord,aes(colour=Rprice))+
  scale_color_viridis_c(direction=-1,option="inferno")

ggplot(data = spshp) +
  geom_sf()+
  geom_sf(data=dadoscapH_coord,aes(colour=Rpricearea))+
  scale_color_viridis_c(direction=-1,option="inferno")



# -------------------------------------------------------

# KMEANS

# -------------------------------------------------------


# roda o algoritmo - note que temos que dar o valor de k (centers)
kmeansA <- kmeans(dadoscapAstd, centers = 4)  #Kmeans apto
kmeansH <- kmeans(dadoscapHstd, centers = 4)  #Kmeans casa

# Visualizar os clusters - dev.off() se der problema
fviz_cluster(kmeansA, data = dadoscapAstd, main = " K-Means (K = 4)")
fviz_cluster(kmeansH, data = dadoscapHstd, main = " K-Means (K = 4)")

# Elbow
fviz_nbclust(dadoscapAstd, kmeans, method = "wss")
fviz_nbclust(dadoscapHstd, kmeans, method = "wss")

# Silhouette
fviz_nbclust(dadoscapAstd, kmeans, method = "silhouette")
fviz_nbclust(dadoscapHstd, kmeans, method = "silhouette")

gap <- clusGap(dadoscapAstd, 
               FUN = kmeans, 
               nstart = 20,
               K.max = 10, 
               B = 50)

print(gap, method = "firstmax")

fviz_gap_stat( gap )

kmeansAcluster <- kmeansA$cluster |> as.data.frame() 
kmeansHcluster <- kmeansH$cluster |> as.data.frame()

View(kmeansAcluster)
View(kmeansHcluster)


dim(kmeansAcluster)
rownames(kmeansAcluster)
colnames(kmeansAcluster)

table(kmeansAcluster)
geodata

df <- tibble::rownames_to_column(kmeansAcluster, "VALUE") |> rename("district" = "VALUE")
View(df)

df1 <- tibble::rownames_to_column(kmeansHcluster, "VALUE") |> rename("district" = "VALUE")
View(df1)

xx <- right_join(geodata, df, by = "district" )
xx
View(xx)

xx1 <- right_join(geodata, df1, by = "district" )
xx1

#SAVE GROUPS
##########(importante salvar os grupos para análises posteriores)########
########## importante alterar o nome para analises com numero de clusters diferentes########
write.csv(dadoscapA,
          paste0("database/01 cluster/",
                 "LGRR_cluster07_spcap_apto_",
                 Sys.Date(),
                 ".csv"))
write.csv(dadoscapH,
          paste0("database/01 cluster/",
                 "LGRR_cluster07_spcap_hous_",
                 Sys.Date(),
                 ".csv"))




# -------------------------------------------------------

# FAMD

# -------------------------------------------------------

#corrigir separador para as 2 variáveis
dados_imp <- dados_imp |>
  mutate(across(c("a_c_fee_imp","tax_imp"),
                ~if_else(str_detect(., "\\."),
                         as.numeric(str_remove_all(sprintf(., fmt = "%0.3f"),"\\.")),
                         .)))

# selecionar variáveis da capital

dadoscapfamd <- dados_imp |> as.data.frame() |> 
  filter(city_nm=="São Paulo")|> select(district,
                                        categ,tipo,
                                        Rprice,
                                        Rpricearea,
                                        area_imp,bed,
                                        bath_imp,
                                        garage_imp,
                                        a_c_fee_imp,
                                        tax_imp)

#analise FAMD
FAMDcap <- FAMD(dadoscapfamd, ncp = 5, graph = FALSE)

?FAMD

print(FAMDcap)

FAMDcap$var$coord # coordenadas das variáveis

FAMDcap$ind$coord # coordenadas dos individuos

FAMDcap$quali.var$coord # resultado da ACM - coords das categorias das vars quali

FAMDcap$quanti.var$coord # resultado da PCA - vars quanti

FAMDcap$eig

get_eigenvalue( FAMDcap ) # autovalores/inércias/vars explicada por cada dimensão

fviz_eig( FAMDcap ) # scree-plot


fviz_famd_ind( FAMDcap )  # coordenadas dos indivíduos (ERRO)

fviz_famd_var(FAMDcap, repel = TRUE) # coordenadas das variáveis

# Contribuição das vars para a primeira dimensão
fviz_contrib(FAMDcap, "var", axes = 1)

# Contribuição das vars para a segunda dimensão
fviz_contrib(FAMDcap, "var", axes = 2)


# gráfico das variáveis quanti
fviz_famd_var(FAMDcap, "quanti.var", repel = TRUE, col.var = "black")

# variáveis que mais contribuem
fviz_famd_var(FAMDcap, "quanti.var", col.var =  "contrib",
              gradient.cols = c("#00AFBB", "#E7B800"),
              repel = TRUE)

# olhando para o pca das varas quanti
pca = prcomp( scale(dadoscapfamd |> select(Rprice, 
                                           Rpricearea,
                                           area_imp,
                                           bed,
                                           a_c_fee_imp,
                                           tax_imp) ) )

fviz_eig(pca, choice = "eigenvalue") # apenas precisariamos de dois fatores        

fviz_pca_var(pca) # gráfico das variáveis quanti (invertido?)




fviz_famd_var(FAMDcap, "quali.var" ) # gráfico das vars qualis (ERRO)

# gráfico de indivíduos
ind <- get_famd_ind(FAMDcap)
ind
fviz_famd_ind(FAMDcap, repel = TRUE) # (ERRO)




