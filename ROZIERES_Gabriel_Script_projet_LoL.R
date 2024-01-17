#on vide toutes les données de l'environnement au cas où
rm(list = ls())

#on installe les packages nécessaires si on ne les a pas : 
packages_a_installer <- c("tidyverse","readr","ggplot2","ggpubr","dplyr",
               "gmodels","stargazer","DataExplorer","cowplot","htmltools","psych",
              "crosstable","flextable","knitr","kableExtra","pROC","htmlTable","shiny")

for (package in packages_a_installer) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package)}}

#et on les charge
for (package in packages_a_installer) {
  library(package, character.only = TRUE)}

rm(package,packages_a_installer)

#importation de la base de données sans avoir à la télécharger (stockée dans mon github)
url <- "https://github.com/gaabsi/Base-LoL-/raw/main/high_diamond_ranked_10min.csv"
df <- read_csv(url, col_names = TRUE)
rm(url)

#on nettoie la base en supprimant les variables redondantes 
# (le nombre de kills côté bleu = le nombre de morts côté rouge)

#on supprime les colonnes de l'équipe rouge car on se concentre sur la bleue
{cols_to_remove <- c(0)
i <- 1
while (i <= ncol(df)) {
  col_name <- colnames(df)[i]
  if (substring(col_name, 1, 3) == "red") {
    cols_to_remove <- c(cols_to_remove, col_name)
  }
  i <- i + 1}
df2 <- df[, !(names(df) %in% cols_to_remove)]}
rm(col_name, cols_to_remove, i)

#on renomme les colonnes
colnames(df2)
noms_colonnes <- c("Victoire", "WardsPlacees", "WardsDetruites", "Firstblood", "Kills",
 "Morts", "Assists", "EliteMob", "Drake", "Herald", "Tour", "TotGold",
 "NivMoy", "TotXP", "TotCs", "JGCS", "GoldDiff", "XPDiff", "CSpermin", "Goldpermin")
colnames(df2)[2:21] <- noms_colonnes

#on s'assure que la colonne Victoire ne contient que des 1 et des 0 sinon tout sera faussé 
df2 <- df2 %>%
  filter(Victoire==1 | Victoire==0)

#aucun changement , il n'y avait pas d'autres valeurs mais mieux vaut être sûr

# on voit que certaines colonnes peuvent être combinées pour faire un ratio (expérience de joueur)
df2$KDA <- (2*df2$Kills+df2$Assists)/(2*df2$Morts) 

#on peut voir que certaines valeurs de KDA sont "-Inf",cela s'explique: si ils ont 0 morts,
#le calcul est impossible donc je leur attribue 0,5 morts afin de ne pas fausser le calcul
#et on relance le calcul qui cette fois-ci ne donnera pas -Inf 
df2$Morts <- ifelse(df2$Morts == 0, 0.5, df2$Morts) 
df2$KDA <- (2*df2$Kills+df2$Assists)/(2*df2$Morts) 
df3 <- df2

# certaines variables ne sont pas intéressantes ([5]) et d'autres sont répétitives (Goldpermin = TotGold/10, ...)
df3 <- df3 %>%
  select(-CSpermin, -Goldpermin,-Firstblood,-gameId) 

#mais on a un autre problème qui est lié à notre problématique (CF document complet)
# la différence n'est pas une information connue de notre joueur au cours de la partie
# il s'agit de quelque chose de calculé après et pas une information disponible 
#pendant la partie alors nous n'utiliserons pas ces indicateurs bien que plus précis
#quant à l'état de la partie

#on enlève les variables diff comme expliqué
df4 <- df3
{df3$CsDiff <- NULL
df3$XPDiff <- NULL
df3$GoldDiff <- NULL}

#on rajoute de nouvelles agrégations pour mieux visualiser la base plus tard 
df3 <- df3 %>%
  mutate(Objectifs = Tour + Herald + Drake, Score_vision = WardsDetruites + WardsPlacees)

# on regarde quelles variables sont correlées et à quoi (sans les wards placées et détruites au début)
plot_correlation(df3[, !names(df3) %in% c("WardsPlacees", "WardsDetruites")])

#on crée une variable catégorielle en 2 modalités à partir de la variable binaire 
#pour avoir des graph plus clairs par la suite
df3$Resultat <- ifelse(df3$Victoire == 1, "Victoire", "Défaite")

#on regarde les variables qui sont correlées à la victoire et on fait
#un tableau descriptif (sans les quartiles qui seront traités plus tard) et un de moyennes conditionnelles  
#pas de variables trop corrélées pour éviter la multicolinéarité
{tabl_sum <- as.data.frame(round(describe(df3[, c("EliteMob", "Tour", "TotGold", "TotXP", "TotCs", "KDA", "Score_vision")]),2)) %>%
  select(-vars, -n, -trimmed, -kurtosis, -se, -skew, -mean, -mad) %>%
  setNames(c("écart-type", "médiane", "min", "max", "range")) %>%
  kable(format = "html") %>%
  kable_styling(full_width = FALSE, position = "center") %>%
  add_header_above(c(" " = 1, "Summary Stats" = 5)) %>%
  column_spec(1, bold = TRUE, width = "4em",background = "#DCDCDC") %>%
  column_spec(2:5, bold = FALSE)
tabl_sum}

df3 %>% 
  group_by(Resultat) %>% 
  summarise(Nb_Gold = mean(TotGold),
            Nb_XP = mean(TotXP),
            Nb_CS = mean(TotCs),
            KDA = mean(KDA), 
            Nb_Elite_Mob = mean(EliteMob),
            Nb_Tours = mean(Tour)) %>% 
  flextable::as_flextable(show_coltype = FALSE) %>% 
  flextable::set_caption("Moyennes selon l'issue de la partie") 

#la moyenne des tours semble basse : sûrement très peu de tours prises ? (oui) : 
parties_1_tour_min <- df3 %>%
  filter(Tour != 0) %>%
  nrow()
parties_1_tour_min

#création de boxplot pour certaines variables intéressantes à creuser (cela nous permet d'identifier les valeurs aberrantes et les quartiles)
{a <- ggplot(df3, aes(y=KDA, fill=Resultat)) +
  geom_boxplot(  outlier.colour = "red"
                 , outlier.size = 2)+
  theme_classic2() +
  labs(
    y="KDA" ,
    title="KDA selon l'issue de la partie")+
  ylim(0,12)

b <- ggplot(df3, aes(y=WardsDetruites, fill=Resultat)) +
  geom_boxplot(  outlier.colour = "red"
                 , outlier.size = 2)+
  theme_classic2() +
  labs(
    y="Wards détruites" ,
    title="Wards détruites selon l'issue de la partie")+
  ylim(0,30)

c <- ggplot(df3, aes(y=TotCs, fill=Resultat)) +
  geom_boxplot(  outlier.colour = "red"
                 , outlier.size = 2)+
  theme_classic2() +
  labs(
    y="Nombre de Cs " ,
    title="CS selon l'issue de la partie")+
  ylim(80,290)

d  <- ggplot(df3, aes(y=Objectifs, fill=Resultat)) +
  geom_boxplot(  outlier.colour = "red"
                 , outlier.size = 2)+
  theme_classic2() +
  labs(
    y="Objectifs Globaux " ,
    title="Nombre d'objectifs selon l'issue de la partie")+
  ylim(0,6)}

plot_grid(a,b,c,d, ncol=2, nrow = 2)
rm(a,b,c,d,parties_1_tour_min,noms_colonnes,tabl_sum)

#on s'intéresse aux wards placées en particulier car il s'agit d'une variable importante selon moi : 
{a <- ggplot(df3, aes(x = Resultat, y = WardsPlacees, fill = Resultat)) +
  geom_bar(stat = "sum", position = "dodge") +
  theme_classic2() +
  labs(title = "Somme des Wards Placées par Résultat",
       x = "Résultat",
       y = "Somme des Wards Placées") +
  scale_fill_manual(values = c("Victoire" = "#00bfc4", "Défaite" = "#f8766d"))

b <- ggplot(df3, aes(y=WardsPlacees, fill=Resultat)) +
  geom_boxplot(  outlier.colour = "red"
                 , outlier.size = 2)+
  theme_classic2() +
  labs(
    y="Wards placées" ,
    title="Wards placées selon l'issue de la partie")+
  ylim(0,30)}

plot_grid(a,b, ncol=2, nrow = 1)
rm(a,b)

#les wards placées n'ont pas l'air d'influer le résulat de la partie 
#on fait un test de student pour être sur de  cette intuition
victoire <- df3$WardsPlacees[df3$Resultat == "Victoire"]
defaite <- df3$WardsPlacees[df3$Resultat == "Défaite"]
 result <- t.test(victoire, defaite)
 
 table_data <- data.frame(
   "T_stat" = result$statistic,
   "P_value" = result$p.value,
   "Intervalle_confiance" = paste("(", round(result$conf.int[1],3), 
                                  "; ", round(result$conf.int[2],3), ")"))    

 rownames(table_data) <- "T-Test_Result"

#j'ai rajouté t() car la transposée du tableau était plus esthétique que sa version initiale 
kable(t(table_data), format = "html", align = "c") %>%
   kable_styling("striped") %>% 
  add_header_above(c(" " = 0, "Les wards placées influent-elles sur le résulat ?" = 2))
rm(victoire, defaite, result, table_data)

#on ne peut pas rejeter H0 , différence pas signif. -> poser des wards ne semble pas influer sur le résultat
#ce résultat est très surprenant ! 

#testons si le côté de jeu influe ou non sur la victoire (beaucoup de joueurs le pensent)
t_test_result <- t.test(df$blueWins, mu = 0.5, alternative = "two.sided")

#puis on le sort en tableau pour mettre dans le rapport 
table_data <- data.frame(
  "T_stat" = t_test_result$statistic,
  "P_value" = t_test_result$p.value,
  "Intervalle_confiance" = paste("(", round(t_test_result$conf.int[1], 3), "; ", round(t_test_result$conf.int[2], 3), ")")
)
rownames(table_data) <- "T-Test_Result"
kable(t(table_data), format = "html", align = "c") %>%
  kable_styling("striped") %>% 
  add_header_above(c(" " = 0, "Le côté influe-t-il sur le résultat ?" = 2))
rm(t_test_result, table_data)

#on peut en conclure que non, vraisemblablement, le côté ne semble pas influer le résultat de la partie 

#on peut faire un test de Student sur le reste des variables (pas anova car seulement victoire et défaite donc même résultat à ttest et anova)
#afin de savoir quelles variables influencent réellement l'issue de la partie

#on liste les variables à tester (on rajoute wardsplacees pour que si on lit juste ce tableau là, on puisse comprendre)
variables_a_ttester <- c("Herald","Drake", "TotCs", "TotXP", "TotGold", "WardsDetruites"
                         , "JGCS", "Tour", "WardsPlacees","Assists", "Morts", "Kills")
results_t_tests <- list()

#on veut pour chaque variable la stat t, la p-val et le seuil de rejet (le même pour tt le monde  ici mais utile pour la suite)
dfreed <- nrow(df3[df3$Resultat == "Victoire", ]) + nrow(df3[df3$Resultat == "Défaite", ]) - 2

#on pose alpha=0.01 pour notre test
alpha <- 0.01

#on boucle les tests sur nos variables
for (variable in variables_a_ttester) {
  t_test_result <- t.test(df3[df3$Resultat == "Victoire", variable],
                          df3[df3$Resultat == "Défaite", variable])
  seuil_rejet <- qt(1 - (alpha) / 2, dfreed)
  results_t_tests[[variable]] <- c(p_value = t_test_result$p.value, 
                                   t_statistic = t_test_result$statistic,
                                   seuil_rejet = seuil_rejet)}

#on convertit en data frame pour la suite et on la transpose (je préfère)
resultats_tt <- as.data.frame(results_t_tests)
resultats_tt2 <- t(resultats_tt)
resultats_tt2 <- as.data.frame(resultats_tt2)

#on crée la colonne qui nous dit si on rejette H0 au seuil de test de 99% ou pas 
resultats_tt2 <- resultats_tt2 %>%
  mutate(Rejet_H0 = ifelse(abs(t_statistic.t) > seuil_rejet, "H0* rejeté",
                           "H0* non-rejeté"))

resultats_tt2 %>%
  kable(format = "html") %>%
  kable_styling(full_width = FALSE, position = "center") %>% 
  row_spec(which(resultats_tt2$p_value > 0.01), background = "#ffcccb") %>%
  row_spec(which(resultats_tt2$p_value < 0.01), background = "#d3f8d3") %>% 
  add_header_above(c(" " = 1, "Quelles variables influencent la victoire ? " = 4)) %>%
  column_spec(1, bold = TRUE, width = "4em",background = "lightgrey") %>%
  column_spec(2:5, bold = FALSE) %>%
  add_footnote("*H0 = la variable n'influe pas sur la victoire")
               
rm(resultats_tt,resultats_tt2,results_t_tests,alpha,
   dfreed,seuil_rejet,variable,variables_a_ttester,t_test_result)

#on passe aux modeles de régression (avec un echantillon d'entrainement et de test)
#avec une graine de reproductibilité pour avoir les mêmes résultats que dans le rapport (ne pas la run si vous voulez un résultat différent)
set.seed(777)
echantillon <- sample(1:nrow(df3), 2 / 3 * nrow(df3))
entrainement <- df3[echantillon, ]
test <- df3[-echantillon, ]
rm(echantillon)

#pour éviter d'avoir à taper les formules de nos modèles à chaque fois on la
#définit ici (toujours avec la contrainte de problématique : important pour la suite)
#on a également enlevé les variables qui expriment la même information (tour, cs, JGCS ... : ces variables ne donnent que de l'or et/ou de l'xp)
formule_formulée <- Victoire ~ Drake + Herald + TotGold + TotXP  + Kills + Morts + Assists 
       
#pourquoi cette formule ? après plusieurs tests il en ressort qu'il s'agit 
#de celle qui prédit le mieux et qui est la + cohérente avec la réalité du jeu
# le TotCs est négligeable car un cs donne Or+Xp donc information répétitive
  
#premier modele : probit et ses coeff 
mod_probit <- glm(formula = formule_formulée,data = entrainement,
                     family = binomial(link = "probit"))
stargazer(mod_probit,type = "text")
   
#second modele : logit (logistique) et ses coeff
mod_log <- glm(formula = formule_formulée, data = entrainement , 
                  family = binomial(link = "logit"))
stargazer(mod_log,type = "text")
  
#on associe les valeurs prédites par les modèles à chaque observation à notre data frame de test
predict_logit <- predict(mod_log, newdata = test, type = "response")
test$predict_logit <- predict_logit
predict_probit <-  predict(mod_probit, newdata = test, type = "response")
test$predict_probit <- predict_probit
  
# performance du model (ROC, AUC-ROC) 
# et graph synthétique performance modèle aux couleurs officielles du jeu
ROC_logit <- roc(test$Victoire, predict_logit)
ROC_probit <- roc(test$Victoire, predict_probit)
  
{plot(ROC_logit,legacy.axes= TRUE, main="Courbe ROC Logit", xlab="Taux de faux positifs", 
       ylab="Taux de vrais positifs")
  legend(x = 0.75, y = 0.5,
         legend = paste("AUC-ROC =", round(ROC_logit$auc, 4)),
         bg = "#005A82", box.lty = 1, box.lwd = 2, cex = 0.7 ,
         text.col = "#C8AA6E", 
         box.col = "#C8AA6E" )}
  
{plot(ROC_probit,legacy.axes= TRUE, main="Courbe ROC Probit", xlab="Taux de faux positifs", 
       ylab="Taux de vrais positifs") 
  legend(x = 0.75, y = 0.5,
         legend = paste("AUC-ROC =", round(ROC_probit$auc, 4)),
         bg = "#005A82", box.lty = 1, box.lwd = 2, cex = 0.7 ,
         text.col = "#C8AA6E", 
         box.col = "#C8AA6E" )}

#nous faisons donc désormais la matrice de confusion de chacun de nos deux modèles
#on donne un nom à nos 2 modalités  prédites (on considère predict > 0.5 -> victoire) et on leur associe des intervalles 
#on aurait pu choisir de définir plus de catégories afin d'être plus précis mais cela serait moins cohérent  notre objectif de prédiction
#on commence avec le modèle logistique 
{Loose_predict <- test$predict_logit >= 0 & test$predict_logit < 0.5
Win_predict <- test$predict_logit >= 0.5 & test$predict_logit <= 1}
  
#on reporte ces valeurs dans notre data set pour voir à quelle prédiction d'issue de la partie le modèle l'aurait associé 
test$ProbWin <- ifelse(Loose_predict, "Loose","Win") 

#on vérifie nos résultats obtenus en comparant les valeurs attendues aux valeurs
# réelles (matrice de confusion)
CrossTable(test$ProbWin, test$Resultat,
             prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)
  
#idem pour le modele probit  (en plus court cette fois-ci)
  test$ProbWin2 <- cut(test$predict_probit, breaks = c(0,0.5,1),
                       labels = c("Loose", "Win"),
                       include.lowest = TRUE, right = FALSE)
  CrossTable(test$ProbWin2, test$Resultat,
             prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)
  
#tableau de synthèse de choix du modèle pour l'interprétation
#nous utiliserons ces métriques : AUC-ROC ; accuracy ; recall ;F1 score ; facilité d'interprétation
#calcul de l'accuracy  (taux de bon classement)
  bien_classe <- function(prediction_var, resultat_var, prob_conditions) {
    sum(Reduce(`|`, Map(function(cond) test[[prediction_var]] == cond[1] & 
                          test[[resultat_var]] == cond[2], prob_conditions))) / nrow(test)}
  
{prob_conditions <- list(
    c("Loose", "Défaite"),
    c("Win", "Victoire"))
bons_classements_logit <- round(bien_classe("ProbWin", "Resultat", prob_conditions), 3)
bons_classements_probit <- round(bien_classe("ProbWin2", "Resultat", prob_conditions), 3)}
  
#calcul du recall (sensibilité de notre model)
{recall <- function(true_positive, false_negative) {return(true_positive / (true_positive + false_negative))}
recall_logit <- round(recall(sum((test$Resultat == "Victoire") & (test$ProbWin == "Win")),
                             sum((test$Resultat == "Défaite") & (test$ProbWin == "Win"))),3)
recall_probit <- round(recall(sum((test$Resultat == "Victoire") & (test$ProbWin2 == "Win")),
                              sum((test$Resultat == "Défaite") & (test$ProbWin2 == "Win"))),3)}

#calcul du F1 Score (métrique utilisant l'accuracy et le recall )
{f1_logit <-round( 2 * (bons_classements_logit * recall_logit) / (bons_classements_logit + recall_logit) ,3) 
f1_probit <- round( 2 * (bons_classements_probit * recall_probit) / (bons_classements_probit + recall_probit),3)}

#on récupère les résultats qu'on met dans des vecteurs et on ajoute l'interprétabilité (selon mon avis personnel ici)
{auc_roc <- c(round(ROC_logit$auc, 3), round(ROC_probit$auc, 3))
accuracy <- c(bons_classements_logit, bons_classements_probit)
recall <- c(recall_logit, recall_probit)
F1_score <- c(f1_logit,f1_probit)
interpretabilite <- c("Bonne", "Moyenne")}
  
#on crée un data frame avec nos vecteurs pour en faire un tableau synthétique  
#on rajoute la création d'une colonne interprétant les résultats et nous donnant pour chaque métrique le meilleur modèle 
{choix_modele <- data.frame(
Logit = c(auc_roc[1], accuracy[1],recall_logit,f1_logit,interpretabilite[1]),
Probit = c(auc_roc[2], accuracy[2], recall_probit,f1_probit,interpretabilite[2]),
choix_du_modèle = c(ifelse(auc_roc[1] > auc_roc[2], "Modèle Logit", ifelse(auc_roc[1] < auc_roc[2], "Modèle Probit", "Égalité")),
  ifelse(accuracy[1] > accuracy[2], "Modèle Logit", ifelse(accuracy[1] < accuracy[2], "Modèle Probit", "Égalité")),
  ifelse(recall_logit > recall_probit, "Modèle Logit", ifelse(recall_logit < recall_probit, "Modèle Probit", "Égalité")),
ifelse(f1_logit > f1_probit, "Modèle Logit", ifelse(f1_logit < f1_probit, "Modèle Probit", "Égalité")),
ifelse(interpretabilite[1] == "Bonne", "Modèle Logit", "Modèle Probit")),
      row.names = c("AUC-ROC", "Accuracy", "Recall","F1 Score","Interprétabilité"))
    colnames(choix_modele)[1:3] <- c("Logit", "Probit", "Meilleure métrique")}
    
#on crée notre tableau de sortie     
choix_modele %>%
  kable(format = "html") %>% 
  kable_styling(full_width = FALSE, position = "center") %>%
  add_header_above(c(" " = 1, "Choix du modèle" = 3)) %>%
  column_spec(1, bold = TRUE, width = "4em", background = "gainsboro")

#étant donné que les 2 modeles ont environ les mêmes performances on choisit d'en 
#garder un : ici le logit (plus simple à interpréter)

#(POUR INTERPRETATION COEFF VOIR RAPPORT)

#on trace le tableau d'interprétation des coeffs 
#on donne le signe des coefficients du modèle puis on rajoute une colonne où on les exponentialise 
#afin d'avoir l'effet marginal de chaque variable sur le rapport des cotes 
coeff_log <- coefficients(mod_log)
coeff_log_exp <- as.data.frame(exp(coeff_log))

#on rajoute de nouvelles colonnes afin d'avoir nos coeffs du modèle, leur effet et leur exponentielle
#(comme on a dit un peu plus haut)
coeff_log_exp <- coeff_log_exp %>%
mutate(Coeff = round(log(exp(coeff_log)),6),
       Effet = ifelse(Coeff > 0, "Positif", "Négatif"),
        var_odd_ratio = paste0(round((exp(coeff_log) - 1) * 100, 3), "%"))

#on rajoute nos significativités en se basant sur le stargazer plus haut 
#(on automatise pour pouvoir prendre un autre échantillon et pouvoir également l'interpréter)
p_values <- summary(mod_log)$coefficients[, "Pr(>|z|)"]
coeff_log_exp$p_value <- p_values
get_stars <- function(p_value) {
  if (p_value < 0.01) {
    return("***")} else if (p_value < 0.05) {
    return("**")} else if (p_value < 0.1) {
    return("*")} else {return("none")}}
coeff_log_exp$Signif <- sapply(p_values, get_stars)

#on recode nos significativités avec des icones pour avoir un tableau un peu plus visuel
coeff_log_exp <- coeff_log_exp %>% 
  mutate(Sign_Icon = ifelse(Signif == "***", "✅✅✅",
                            ifelse(Signif == "**", "✅✅", 
                                   ifelse(Signif=="*", "✅", "❌"))))

#on ajoute les intervalles de confiance
coeff_log_exp$Inter_Conf <- apply(confint(mod_log), 1, function(x) paste0("[", round(x[1], 4), " ; ", round(x[2], 4), "]"))

#on réordonne le tableau pour le rendre plus lisible  
coeff_log_exp <- coeff_log_exp %>% 
  select(Sign_Icon,Coeff,Inter_Conf, Effet, `exp(coeff_log)`, var_odd_ratio)

#on enlève l'intercept qu'on analysera plus tard
coeff_log_exp <- coeff_log_exp %>% 
  slice(-1)

#on travaille notre output avec un tableau qui parle visuellement avec des couleurs etc. 
# et note de bas pour expliquer les icones (au cas où)
{coeff_log_exp %>%
  kable(format = "html") %>%
  kable_styling(full_width = FALSE, position = "center") %>%
  row_spec(which(coeff_log_exp$Coeff < 0), background = "#ffcccb") %>%
  row_spec(which(coeff_log_exp$Coeff > 0), background = "#d3f8d3") %>% 
  row_spec(which(coeff_log_exp$Sign_Icon == "❌"), background = "lightgrey") %>%
  add_header_above(c(" " = 1, "Coefficients du modèle Logit" = 6)) %>%
  column_spec(1, bold = TRUE, width = "4em") %>%
  column_spec(2:6, bold = FALSE) %>%
  add_footnote("✅✅✅ = p<0.01, ✅✅ = p<0.05, ✅= p<0.1, ❌ = p>0.1 ") %>% 
  add_footnote("intervalle de confiance à 95%")}

#étrange que dans notre échantillon la prise du hérald n'est pas significative, regardons le
#nombre de parties où le herald a été pris : 
sum(entrainement$Herald == 1 , na.rm = TRUE)
#il y a pourtant assez de parties où il a été pris, le résultat est plutôt surprenant

#on pourrait se demander nos chances de victoire si toutes les variables sont à 0 ? (si toute l'équipe ne joue pas)
constante <- coef(mod_log)[1]
prob_victoire <- round((exp(constante) / (1 + exp(constante))),5)
{tte_var0 <- data.frame(
  Variables = "Toutes les variables égales à zéro",
  Probabilite_Victoire = prob_victoire)
caption <- tags$caption(style = "text-align: center; font-weight: bold;", "Probabilités de victoire")
htmlTable(tte_var0, rnames = FALSE, caption = caption, center = TRUE)}
#elles sont infimes (et heureusement!)


#limites du modèle et causes possibles : VOIR RAPPORT 

#POUR VOIR LE RAPPORT : 
#Envie de consulter ou télécharger le rapport ? 

lien_dropbox_telecharger <- "https://www.dropbox.com/scl/fi/pdsjrowjin488ki0jn1hc/Rapport_R_LoL_final.pdf?rlkey=lqdgfmo7ythb0imo0u5c3xcvj&dl=1"
lien_dropbox_visualiser <- "https://www.dropbox.com/scl/fi/pdsjrowjin488ki0jn1hc/Rapport_R_LoL_final.pdf?rlkey=lqdgfmo7ythb0imo0u5c3xcvj&dl=0"

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background-image: url('https://img.freepik.com/photos-gratuite/textures-bois-blanc-pour-fond_74190-8108.jpg');
        background-size: cover;
        background-position: center;
        height: 100vh;
        margin: 0;
        overflow: hidden;}
      h2 { text-align: center; margin-top: 120px; color: black ;font-family: 'Times New Roman', Times, serif; }"))),
  titlePanel("Télécharger ou Visualiser le PDF du rapport"),
  tags$div(
    style = "display: flex; flex-direction: column; align-items: center; justify-content: center; height: 80vh; margin-top: -145px;",
      tags$img(src = "https://media.giphy.com/media/RVTTCmNEtg10gtYimN/giphy.gif", height = 150, width = 300, animate = TRUE),
    tags$div(
      style = "display: flex; flex-direction: row; justify-content: center; margin-top: 50px;",
      downloadButton("download_report", "Télécharger le PDF du rapport", style = "margin-right: 5px;"),
      actionButton("visualiser_report", "Visualiser le PDF du rapport "))))
server <- function(input, output) {
  output$download_report <- downloadHandler(
    filename = function() {
      paste("Rapport_R_LoL_final_", Sys.Date(), ".pdf", sep = "")},
    content = function(file) {download.file(lien_dropbox_telecharger, destfile = file, mode = "wb")})
observeEvent(input$visualiser_report, {browseURL(lien_dropbox_visualiser)})}

shinyApp(ui, server)

