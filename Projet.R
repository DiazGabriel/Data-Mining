#######################NOTE#########################
 
#  - Compléter le pré traitement avec des graphe (boite à moustache, plot ....)
#  - Pas de 7 portes dans immatriculation ?? implique aucune vente de familliale
#      Voir à retoucher les conditions
#  - Boucle de supression des valeur NA dans clients trop longue ... Revoir
#  - Traitement à la main dans le CSV, voir comment faire en R :
#       Clients_8 a des " sur chaque ligne .. sa fout la merde au chargement du fichier
#       Les clients ont une colone "2eme voiture" transformé en "x2eme.voiture" par R 
#       a cause du 2 et de l'espace, sa rend le truc intraitable en SQL





###################################################

##### Installation des librairies #####

install.packages("sqldf")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("C50")
install.packages("tree")
install.packages("randomForest")
install.packages("e1071")
install.packages("nnet")
install.packages("kknn")
install.packages("ROCR")

library(ROCR)
library(sqldf)
library(rpart)
library(C50)
library(tree)
library(randomForest)
library(e1071)
library(nnet)
library(kknn)
library(rpart.plot)

##### Chargement des données #####

Clients_0 <- read.csv("C:/Users/diazg/Desktop/Files/Cours/MIAGE/M2/0_Projet_BigData/Pasquier/Clients_0.csv", header = TRUE, sep = ";", dec = ".", check.names = FALSE)
Clients_8 <- read.csv("C:/Users/diazg/Desktop/Files/Cours/MIAGE/M2/0_Projet_BigData/Pasquier/Clients_8.csv", header = TRUE, sep = ",", dec = ".", check.names = FALSE)
Immatriculations <- read.csv("C:/Users/diazg/Desktop/Files/Cours/MIAGE/M2/0_Projet_BigData/Pasquier/Immatriculations.csv", header = TRUE, sep = ",", dec = ".", check.names = FALSE)
Marketing <- read.csv("C:/Users/diazg/Desktop/Files/Cours/MIAGE/M2/0_Projet_BigData/Pasquier/Marketing.csv", header = TRUE, sep = ",", dec = ".", check.names = FALSE)
Catalogue <- read.csv("C:/Users/diazg/Desktop/Files/Cours/MIAGE/M2/0_Projet_BigData/Pasquier/Catalogue.csv", header = TRUE, sep = ",", dec = ".", check.names = FALSE)


##### Pré traitement des données #####

### 1/ Fichiers Clients :

# Les deux fichiers clients ont les mêmes zones, on commance par les binder en un même data frame 
Clients <- rbind(Clients_0, Clients_8)

# On commance par homogénéisé les données
#   - Sexe contient plusieurs type de valeurs (conversion en 'M' et 'F' uniquement)
#   - Certaines lignes contiennent des valeurs inconnues (-1; N/D, ?) 
#     Correspond à 2000 lignes sur les 200 000 (1%) non pénalisant donc on supprime

for (i in 1:nrow(Clients)) {
  
  if (Clients[i,2] == "Masculin" || Clients[i,2] == "Homme") {
    Clients[i,2] <- 'M'
  }
  if (Clients[i,2] == "Féminin" || Clients[i,2] == "Femme") {
    Clients[i,2] <- 'F'
  }
}

for (i in 1:nrow(Clients)) {
  for (j in 1:length(Clients)) {
    if (Clients[i,j] == -1 || Clients[i,j] == "N/D" || Clients[i,j] == "?") {
      # On supprime la ligne en cours
      Clients <- Clients[-i,]
      i <- i - 1
    }
  }
}

### 2/ Immatriculation 

# En concervant le principe que l'immatriculation est une clé unique (1 immat = 1 voiture)
# il est abhérent d'avoir des doubles 

test <- sqldf("select immatriculation, count(*) 
               from Immatriculations 
               group by immatriculation having count(*) > 1")

# resultat : 3366 immatriculation en double ; on va les supprimer
# On triche en passant par un SELECT car sqldf ne permet pas d'opération DELETE sur un data frame
# les objets d'entrés doivent être identiques en sortie

Immatriculations <- sqldf("select * 
                           from Immatriculations 
                           where immatriculation not in (select immatriculation 
                                                         from Immatriculations 
                                                         group by immatriculation having count(*) > 1)")


##### Construction des catéories de voitures #####

# On commance par construire un data frame contenant les modèles disctint de véhicule sans nbPorte, couleur, occasion et prix

Categorie <- sqldf("select distinct nom, marque, puissance, longueur, nbPlaces
                    from Catalogue")

# On va définir 4 catégories :
#  - Familiale (7 places)
#  - Citadine (chv < 140)
#  - Luxe (Très longue et chv > 140)
#  - sport (chv > 140, place < 7 et longueur < très longue --- Les autres)

Classification <- c();
for (i in 1:nrow(Categorie)) {
    if (Categorie[i,5] == 7) {
      Classification <- append(Classification, "Familliale")
    } else if (Categorie[i,3] < 140) {
      Classification <- append(Classification, "Citadine")
    } else if (Categorie[i,4] == "très longue") {
      Classification <- append(Classification, "Luxe")
    } else {
      Classification <- append(Classification, "Sport")
    }
}
Categorie$Classification <- Classification


##### Construction de l'ensemble d'apprentissage et de test #####

# L'EA va être formé par le fichier clients auquel on va lier la catégorie de voiture
# Pour ce faire, on utiliser la table immatriculation pour lier les 2

Clients <- sqldf("select cl.age, cl.sexe, cl.taux, cl.situationFamiliale, cl.nbEnfantsAcharge, 
                         cl.sec_voiture, ca.Classification
                  from Clients cl,(select * 
                                   from Immatriculations i, Categorie c
                                   where i.nom = c.nom
                                   ) as ca
                  where cl.immatriculation = ca.immatriculation")

# Regardons comment sont répartie nos catégories de véhicule

table(Clients$Classification)


# Pour la suite, on vas avoir besoin de deux ensemble : EA et ET
#    - EA = 70% de Clients
#    - ET = 30% de Clients

# Avant toute chose, nous devons modifier le type de la colonne Classification en factor afin de la 
# rendre traitable par les classifieurs

Clients$Classification <- as.factor(Clients$Classification) 

Pivot <- nrow(Clients)*0.7

EA <- Clients[1:Pivot,]
ET <- Clients[Pivot:nrow(Clients),]




##### Phase d'apprentissage #####

# Nous allons comparer différentes méthode de prédictions et concerver la meilleur
# Parmis ces méthodes nous étudierons : 
# rpart, C50, tree + Random Forest, SVM, Naives Bayes, Neural Network, KKNN 


# 1/ Rpart 

Rpart <- rpart(Classification ~ ., data=EA)
plot(rpart)
text(rpart, pretty = 1)
prp(rpart)

# 2/ C5.0

c50 <- C5.0(Clients ~ ., EA)
plot(c50, type="simple")


# 3/ Tree

tree <- tree(Clients ~ ., EA)

plot(tree)
text(tree, pretty = 1)


# 4/ RandomForest

RandomForest <- randomForest(projet_EA$defaut~., data=projet_EA, na.action = na.roughfix) 
plot(RandomForest)


# 5/ SVM

SVM <- svm(projet_EA$defaut~., data=projet_EA, probability = T)
plot(SVM)


# 6/ NaiveBayes

NaiveBayes <- naiveBayes(defaut~., data=projet_EA)
plot(NaiveBayes)


# 7/ NNET

NNET <- nnet(defaut~., data=projet_EA, size=10)
plot(NNET)


# 8/ KKNN

KKNN <- kknn(defaut~., projet_EA , projet_ET)
plot(KKNN)


##### Phases de test #####

# L'ensemble de test construit précedemment vas nous permettre de valider (ou non) les prédictions
# faites à partir de l'ensemble d'apprentissage

# 1/ Rpart
test_rpart <- predict(rpart, ET, type="class") # type = prob/vector pour sortir une proba 
table(test_rpart)


# 2/ C50
test_c50 <- predict(c50, ET, type="class")
table(test_c50)


# 3/ tree
test_tree <- predict(tree, ET, type="class")
table(test_tree)


# 4/ RandomForest
test_RandomForest <- predict(RandomForest, ET, type='class' )
table(test_RandomForest)


# 5/ SVM
test_SVM <- predict(SVM, ET, type='C-classification', na.action = na.roughfix)
table(test_SVM)


# 6/ NaiveBayes
test_NaiveBayes <- predict(NaiveBayes, ET, type='class')
table(test_NaiveBayes)


# 7/ NNET
test_NNET <- predict(NNET, ET, type='class')
table(test_NNET)


# 8/ KKNN
test_KKNN <- summary(KKNN)
table(test_KKNN$fitted.values)


# Afin de comparer visuellement nos résultats, on ajoute les prédictions à l'ensemble de test
ET$rpart <- test_rpart
ET$c50 <- test_c50
ET$tree <- test_tree
ET$RandomForest <- test_RandomForest
ET$SVM <- test_SVM
ET$NaiveBayes <- test_NaiveBayes
ET$NNET <- test_NNET
ET$KKNN <- test_KKNN

View(ET[,c("Classification", "rpart", "c50", "tree", "RandomForest", "SVM", "NaiveBayes", "NNET", "KKNN")])



##### Evaluation des classifieurs #####

# Afin de se donner une première idée, on peux calculer les taux de succès de nos classifieurs

taux_rpart <- length(ET[ET$Classification==ET$rpart,"ID"])/nrow(ET)
taux_c50 <- length(ET[ET$Classification==ET$c50,"ID"])/nrow(ET)
taux_tree <- length(ET[ET$Classification==ET$tree,"ID"])/nrow(ET)
taux_RandomForest <- length(ET[ET$Classification==ET$RandomForest,"ID"])/nrow(ET)
taux_SVM <- length(ET[ET$Classification==ET$SVM,"ID"])/nrow(ET)
taux_NaiveBayes <- length(ET[ET$Classification==ET$NaiveBayes,"ID"])/nrow(ET)
taux_NNET <- length(ET[ET$Classification==ET$NNET,"ID"])/nrow(ET)
taux_KKNN <- length(ET[ET$Classification==ET$KKNN,"ID"])/nrow(ET)


# Afin de déterminer lequel de nos 8 classifeurs est le plus précis, nous allons tracer leurs courbes
# ROC respectives et comparer leurs indices AUC


# 1/ rpart 

prob_rpart <- predict(rpart, ET, type = "prob")
roc_pred_rpart <- prediction(prob_rpart[,2], ET$Classification) 
roc_perf_rpart <- performance(roc_pred_rpart,"tpr","fpr") 



