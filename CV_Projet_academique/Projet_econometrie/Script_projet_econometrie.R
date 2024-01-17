rm(list = ls())
#installer et charger les packages 
packages_a_installer <- c("tidyverse","readr","ggplot2","ggpubr","dplyr",
"gmodels","stargazer","DataExplorer","cowplot","htmltools","psych","crosstable",
"flextable","knitr","kableExtra","pROC","htmlTable","shiny","outliers","car","lmtest", "VIM","MASS","margins")
for (package in packages_a_installer) 
  {if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package)}}
for (package in packages_a_installer) {library(package, character.only = TRUE)}
rm(package, packages_a_installer)

#charger la base de données 
url <- "https://dl.dropboxusercontent.com/scl/fi/5he67pzzxa9ce4h8o90m7/car_base.csv?rlkey=udczfyeyq8z1tfwhcsd7r0ucb&"
df <- read_csv(url, col_names = TRUE)

#on regarde le nombre de modalités dans différentes catégories
sapply(df, function(x) nlevels(factor(x)))

#944 modalités dans model, on ne la gardera pas, on supprime les autres variables inutiles
#on voit également en regardant l'ID qu'il y a des doublons (moins d'ID que d'observations) 
df <- df %>% 
  distinct() %>% 
  dplyr::select(-ID,-Model ) 
colnames(df)

#renommer les variables
nom_colonnes <- c("Prix", "Fabricant","Année","Catégorie","Intérieur_cuir","Carburant",
"Cylindrée","Kilométrage","Type_boite","Roues_motrices","Côté_conduite","Couleur")
colnames(df)[1:12] <- nom_colonnes
rm(nom_colonnes, url)
str(df)

#on nettoie la colonne kilométrage en enlevant les caractères non numériques et on fait une colonne de dizaine de Km
df$Kilométrage <- as.numeric(gsub("[^0-9.]", "", df$Kilométrage))
df$D_Km <- df$Kilométrage/10000

#on renomme les modalités des carburants pour mieux les comprendre 
df <- df %>%
  mutate(Carburant = ifelse(Carburant == "Diesel", "Diesel","Essence"))

#création variable binaire Turbo, boite auto, intérieur cuir et conduite à droite pour gagner en précision
df$Turbo <- ifelse(grepl("Turbo",df$Cylindrée),1,0)
df$Cylindrée <- as.numeric(gsub("[^0-9.]", "", df$Cylindrée))
df$Conduite_droite <-  ifelse(df$Côté_conduite== "Right-hand drive", 1, 0)
df$Côté_conduite <- NULL
df$Boîte_auto <- ifelse(df$Type_boite =="automatic" , 1, 0)
df$Type_boite <- NULL
df$Intérieur_cuir <- ifelse(df$Intérieur_cuir =="Yes", 1, 0)

#idem pour le carburant et la marque de la voiture et pour la couleur noire
df$Ess <- ifelse(df$Carburant =="Essence",1, 0)
df$Ford <- ifelse(df$Fabricant=="FORD", 1,0)
df <- df %>%
  mutate(black = ifelse(Couleur == "Black", 1, 0)) 
df$Couleur <- NULL

# Filtrer le dataframe pour ne conserver que les catégories de voitures pour les particuliers 
categories_a_garder <- c("Cabriolet", "Coupe", "Hatchback", "Sedan", "Minivan", "Universal")
df <- df %>%
  filter(Catégorie %in% categories_a_garder)

#détection et suppression des outliers 
variables <- c("Prix", "Kilométrage","Cylindrée")
for (variable in variables) {
  Q1 <- quantile(df[[variable]], 0.25)
  Q3 <- quantile(df[[variable]], 0.75)
  IQR <- Q3 - Q1
  limite_superieure <- Q3 + 1.5 * IQR
  limite_inferieure <- Q1 - 1.5 * IQR}

#on fait un df2 qui est sans outlier 
df2 <- df %>%
  filter(across(all_of(variables), ~ between(., quantile(., 0.25) - 1.5 * IQR(.), quantile(., 0.75) + 1.5 * IQR(.))))
str(df2)

#on identifie les valeurs aberrantes pour le prix et le kilométrage (avant suppression et après)
#graphiquement
 a <-ggplot(df, aes(y = Kilométrage)) +
   geom_boxplot(outlier.colour = "red", outlier.size = 2) +
   labs(y = "Kilométrage", title = "Kilométrage avec outliers") +
   theme_classic2() +
   scale_y_continuous(labels = scales::label_number_si())
b <-  ggplot(df, aes(y = Prix)) +
  geom_boxplot(outlier.colour = "red", outlier.size = 2) +
  labs(y = "Prix (en $)", title = "Prix avec outliers") +
  theme_classic2() +
  scale_y_continuous(labels = scales::label_number_si())
c <- ggplot(df2, aes(y = Kilométrage)) +
  geom_boxplot(outlier.colour = "red", outlier.size = 2) +
  labs(y = "Kilométrage", title = "Kilométrage sans outliers") +
  theme_classic2() +
  scale_y_continuous(labels = scales::label_number_si())
d <- ggplot(df2, aes(y = Prix)) +
   geom_boxplot(outlier.colour = "red", outlier.size = 2) +
   labs(y = "Prix  (en $)", title = "Prix  sans outliers") +
   theme_classic2() +
   scale_y_continuous(labels = scales::label_number_si())
plot_grid(a,c,b,d, ncol = 2, nrow = 2)
rm(list=setdiff(ls(), c("df", "df2")))
df2$Kilométrage <- NULL

#autres firgures pour rendre compte de la distribution : répartition des ventes par fabricant
summary_data <- df2 %>%
  group_by(Fabricant) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
a <- ggplot(summary_data, aes(x = reorder(Fabricant, -count), y = count)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_text(aes(label = count), vjust = -0.5, color = "black") +  
  labs(title = "Nombre de voitures par fabricant",
       x = "Fabricant",
       y = "Nombre de voitures") +
  theme_classic2() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# et catégories de voiture les plus chères en moyenne 
#histogramme des catégories les plus chères et nombre d'observation dans chaque catégorie
summary_data <- df2 %>%
  group_by(Catégorie) %>%
  summarise(mean_price = mean(Prix), count = n())
summary_data <- summary_data %>%
  arrange(desc(mean_price))
b <- ggplot(summary_data, aes(x = reorder(Catégorie, -mean_price), y = mean_price)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_text(aes(label = count), vjust = -0.5, color = "black") +  
  labs(title = "Catégories de voitures les plus chères en moyenne",
       x = "Catégorie de voiture",
       y = "Prix moyen (en $) ") +
  theme_classic2() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
{variables_numeriques <- df2[, sapply(df2, is.numeric)]
matrice_correlation <- cor(variables_numeriques)
c <- corrplot::corrplot(matrice_correlation, method = "circle", type = "upper", tl.col = "black", tl.srt = 45)
c}
plot_grid(a,b, ncol = 2, nrow = 1)

#statistiques conditionnelles avec noir =1 ou =0 
group_black_1 <- df2 %>% filter(black == 1)
group_black_0 <- df2 %>% filter(black == 0)
summary(group_black_0)
summary(group_black_1)
summary(df2)

#modèle de régression simple dite naîve 
mod12 <- lm(log(Prix)~ black , data= df2)
stargazer(mod12, type= "text")

#pour capturer la non linéarité potentielle du kilométrage
df2$D_Km2 <- df2$D_Km^2

# on fera un modèle de double différence pour savoir si les voitures noires de chez ford sont les moins chères (CF introduction)
#régression finale avec toutes les variables de contrôle + interractions, etc. incorporées 
mod <- lm(log(Prix) ~ black  + Catégorie + Ford + D_Km +   Année +
            Ess + log(Cylindrée) + Turbo +  Intérieur_cuir + Boîte_auto +
            Roues_motrices + Conduite_droite + D_Km2 +Année*D_Km +Ford*black , data = df2)
stargazer(mod, type = "text")
summary(margins(mod))

#on vérifie si les résidus suivent une loi Normale 
hist(mod$residuals, main = "Histogramme des résidus", col = "lightblue", border = "black", breaks = "FD")
#ça n'en as pas trop l'air et Shapiro test confirme cela

#on vérifie l'homoscédasticité
bptest <- lmtest::bptest(mod)
print(bptest)
#il y a une hétéroscédasticité !! attention !!

#certains prérequis aux MCO ont été violés un modèle robuste serait peut-être plus approprié ? 
mod2 <- rlm(log(Prix) ~ black  + Catégorie + Ford + D_Km +   Année +
             Ess + log(Cylindrée) + Turbo +  Intérieur_cuir + Boîte_auto +
             Roues_motrices + Conduite_droite + D_Km2 +Année*D_Km +Ford*black , data = df2)
stargazer(mod2, type = "text")


