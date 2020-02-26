##### Installation des librairies #####

install.packages("ggplot2")
install.packages("sqldf")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("C50")
install.packages("tree")
install.packages("randomForest")
install.packages("e1071")
install.packages("nnet")
install.packages("kknn")


library(ggplot2)
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

Clients_0 <- read.csv("C:/Users/diazg/Desktop/Files/Cours/MIAGE/M2/0_Projet_BigData/Pasquier/Clients_0.csv", header = TRUE, sep = ",", dec = ".", check.names = FALSE)
Clients_8 <- read.csv("C:/Users/diazg/Desktop/Files/Cours/MIAGE/M2/0_Projet_BigData/Pasquier/Clients_8.csv", header = TRUE, sep = ",", dec = ".", check.names = FALSE)
Immatriculations <- read.csv("C:/Users/diazg/Desktop/Files/Cours/MIAGE/M2/0_Projet_BigData/Pasquier/Immatriculations.csv", header = TRUE, sep = ",", dec = ".", check.names = FALSE)
Marketing <- read.csv("C:/Users/diazg/Desktop/Files/Cours/MIAGE/M2/0_Projet_BigData/Pasquier/Marketing.csv", header = TRUE, sep = ",", dec = ".", check.names = FALSE)
Catalogue <- read.csv("C:/Users/diazg/Desktop/Files/Cours/MIAGE/M2/0_Projet_BigData/Pasquier/Catalogue.csv", header = TRUE, sep = ",", dec = ".", check.names = FALSE)


##### Pré-traitement des données #####

# Pour commencer, regardons visuellement la répartition de nos données :

# Boite à moustache :

boxplot(as.integer(Clients_0[,1])) # age
boxplot(as.integer(Clients_0[,2])) # sexe
boxplot(as.integer(Clients_0[,3])) # taux 
boxplot(as.integer(Clients_0[,4])) # situation familiale
boxplot(as.integer(Clients_0[,5])) # nb enfants
boxplot(as.integer(Clients_0[,6])) # 2eme voiture


boxplot(as.integer(Clients_8[,1])) # age
boxplot(as.integer(Clients_8[,2])) # sexe
boxplot(as.integer(Clients_8[,3])) # taux 
boxplot(as.integer(Clients_8[,4])) # situation familiale
boxplot(as.integer(Clients_8[,5])) # nb enfants
boxplot(as.integer(Clients_8[,6])) # 2eme voiture


boxplot(as.integer(Marketing[,1])) # age
boxplot(as.integer(Marketing[,2])) # sexe
boxplot(as.integer(Marketing[,3])) # taux 
boxplot(as.integer(Marketing[,4])) # situation familiale
boxplot(as.integer(Marketing[,5])) # nb enfants
boxplot(as.integer(Marketing[,6])) # 2eme voiture

# Nuage de points :

ggplot(Catalogue)
ggplot(Immatriculations)


# Au vu des graphiques, on s'appercoit que certaines colonnes comportent des données très éparses. 
# Nous allons regarder cela de plus près et traiter ce qui est nécessaire.


### 1/ Fichiers Clients :

# Les deux fichiers clients ont les mêmes zones, on commence par les binder en un même data frame 
Clients <- rbind(Clients_0, Clients_8)

# Avant toutes choses, on renomme la colonne "2eme voiture" en "sec_voiture" afin de la rendre traitable 
# plus facilement

colnames(Clients)[6] <- "sec_voiture"

# On commence par homogénéiser les données
#   - Sexe contient plusieurs types de valeurs (conversion en 'M' et 'F' uniquement)
#   - Certaines lignes contiennent des valeurs inconnues (-1; N/D, ?, '') 
#     correspond à 2000 lignes sur les 200 000 (1%) - non pénalisant donc on supprime

for (i in 1:nrow(Clients)) {
  
  if (Clients[i,2] == "Masculin" || Clients[i,2] == "Homme") {
    Clients[i,2] <- 'M'
  }
  if (Clients[i,2] == "Féminin" || Clients[i,2] == "Femme") {
    Clients[i,2] <- 'F'
  }
}

Clients <- sqldf("select * 
                  from Clients
                  where age not in ('-1', 'N/D', '?', ' ') and
                        sexe not in ('-1', 'N/D', '?', ' ') and
                        taux not in ('-1', 'N/D', '?', ' ') and
                        situationFamiliale not in ('-1', 'N/D', '?', ' ') and
                        nbEnfantsAcharge not in ('-1', 'N/D', '?', ' ') and
                        sec_voiture not in ('-1', 'N/D', '?', ' ') and
                        immatriculation not in ('-1', 'N/D', '?', ' ')")


### 2/ Immatriculation 

# En conservant le principe que l'immatriculation est une clé unique (1 immat = 1 voiture)
# il est aberrant d'avoir des doubles 

test <- sqldf("select immatriculation, count(*) 
              from Immatriculations 
              group by immatriculation having count(*) > 1")

# résultat : 3366 immatriculations en double - on va les supprimer
# On triche en passant par un SELECT car sqldf ne permet pas d'opération DELETE sur un data frame
# les objets d'entrée doivent être identiques en sortie

Immatriculations <- sqldf("select * 
                          from Immatriculations 
                          where immatriculation not in (select immatriculation 
                                                        from Immatriculations 
                                                        group by immatriculation having count(*) > 1)")

### 3/ Marketing

# comme pour clients, on renomme la colonne "2eme voiture" en "sec_voiture"

colnames(Marketing)[6] <- "sec_voiture"

# On transforme la colonne sec_voiture en factor de booleen pour la rendre traitable plus facilement 

Marketing$sec_voiture <- as.logical(Marketing$sec_voiture)
Marketing$sec_voiture <- as.factor(Marketing$sec_voiture)


##### Construction des catéories de voitures #####

# On commence par construire un data frame contenant les modèles disctints de véhicule
# on ne conserve que les vehicules neufs pour avoir un prix de référence (au moins une ligne occasion = false par modèle)

Categorie <- sqldf("select distinct nom, marque, puissance, longueur, nbPlaces, 
                           nbPortes, occasion, prix
                    from Catalogue
                    where occasion = 'false'")

# On va définir 5 catégories :
#  - Familiale (longueur = longue ou très longue, chv <= 130, nbPlaces >= 5) ou (nbPlaces = 7) 
#  - Citadine (chv <= 150, longueur = courte)
#  - Luxe (chv >= 180, prix >= 90 000)
#  - Sport (chv >= 180, prix >= 30000)
#  - Berline (chv <= 180)

Classification <- c();
for (i in 1:nrow(Categorie)) {
  valeur <- " "
  if (Categorie$puissance[i] <= 180) {
    valeur <- "Berline"
  } 
  if (Categorie$puissance[i] <= 150 && Categorie$longueur[i] == "courte") {
    valeur <- "Citadine"
  } 
  if (((Categorie$longueur[i] == "longue" || Categorie$longueur[i] == "très longue") && 
      Categorie$puissance[i] <= 130 &&  Categorie$nbPlaces[i] >= 5) || Categorie$nbPlaces[i] == 7) {
    valeur <- "Familiale"
  }
  if (Categorie$puissance[i] >= 180 && Categorie$prix[i] >= 30000) {
    valeur <- "Sport"
  }
  if (Categorie$puissance[i] >= 180 && Categorie$prix[i] >= 90000) {
    valeur <- "Luxe"
  }
  
  Classification <- append(Classification, valeur)
}
Categorie$Classification <- Classification


##### Construction de l'ensemble d'apprentissage et de test #####

# L'EA va être formé par le fichier Clients auquel on va lier la catégorie de voiture
# Pour se faire, on utilise la table Immatriculations comme table de relation

Clients <- sqldf("select distinct cl.age, cl.sexe, cl.taux, cl.situationFamiliale, cl.nbEnfantsAcharge, 
                         cl.sec_voiture, ca.Classification
                  from Clients cl inner join Immatriculations i 
                         on cl.immatriculation = i.immatriculation 
                                  inner join Categorie ca 
                         on i.nom = ca.nom")


# Regardons comment sont réparties nos catégories de véhicule

table(Clients$Classification)


# Avant toute chose, nous devons modifier le type des colonnes de Clients afin de les 
# rendre traitables par les classifieurs
# Les zones sexe, situation familiale et sec_voiture sont des factor (liste de valeurs)
# R garde en mémoire les valeurs éronnées supprimées précedement (?, N/D ..) ce qui aggrandit la liste de 
# valeurs et faussera la prediction - les valeurs seront prises en compte dans les arbres d'apprentissage
# mais jamais testables.
# Pour contrer cela on les passe en character avant des les reconvertir

str(Clients)

Clients$age <- as.integer(Clients$age)
Clients$sexe <- as.character(Clients$sexe)
Clients$sexe <- as.factor(Clients$sexe)
Clients$taux <- as.integer(Clients$taux)
Clients$situationFamiliale <- as.character(Clients$situationFamiliale)
Clients$situationFamiliale <- as.factor(Clients$situationFamiliale)
Clients$nbEnfantsAcharge <- as.integer(Clients$nbEnfantsAcharge)
Clients$sec_voiture <- as.logical(Clients$sec_voiture)
Clients$sec_voiture <- as.factor(Clients$sec_voiture)
Clients$Classification <- as.factor(Clients$Classification)



# Pour la suite, on va avoir besoin de deux ensembles : EA et ET
#    - EA = 70% de Clients
#    - ET = 30% de Clients

Pivot <- nrow(Clients)*0.7

EA <- Clients[1:Pivot,]
ET <- Clients[Pivot:nrow(Clients),]




##### Phase d'apprentissage #####

# Nous allons comparer différentes méthodes de prédictions et conserver la meilleure
# Parmi ces méthodes nous étudierons : 
# rpart, C50, tree + Random Forest, Naives Bayes, Neural Network, KKNN 


# 1/ Rpart 

Rpart <- rpart(Classification ~ ., EA)

plot(Rpart)
text(Rpart, pretty = 1)
prp(Rpart)


# 2/ C5.0

c50 <- C5.0(Classification ~ ., EA)
plot(c50, type="simple")


# 3/ Tree

tree <- tree(Classification ~ ., EA)

plot(tree)
text(tree, pretty = 1)


# 4/ RandomForest

RandomForest <- randomForest(Classification~., EA, na.action = na.roughfix) 
plot(RandomForest)


# 5/ NaiveBayes

NaiveBayes <- naiveBayes(Classification ~ ., EA)


# 6/ NNET

NNET <- nnet(Classification ~ ., EA, size=10)


# 7/ KKNN

KKNN <- kknn(Classification ~ ., EA, ET)

##### Phases de test #####

# L'ensemble de test construit précédemment va nous permettre de valider (ou non) les prédictions 

# 1/ Rpart
test_rpart <- predict(Rpart, ET, type="class") # type = prob/vector pour sortir une proba 
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


# 5/ NaiveBayes
test_NaiveBayes <- predict(NaiveBayes, ET, type='class')
table(test_NaiveBayes)


# 6/ NNET
test_NNET <- predict(NNET, ET, type='class')
table(test_NNET)


# 7/ KKNN
test_KKNN <- summary(KKNN)
table(test_KKNN$fitted.values)



# Afin de comparer visuellement nos résultats, on ajoute les prédictions à l'ensemble de test
ET$rpart <- test_rpart
ET$c50 <- test_c50
ET$tree <- test_tree
ET$RandomForest <- test_RandomForest
ET$NaiveBayes <- test_NaiveBayes
ET$NNET <- test_NNET
ET$KKNN <- test_KKNN

View(ET[,c("Classification", "rpart", "c50", "tree", "RandomForest", "NaiveBayes", "NNET", "KKNN")])



##### Evaluation des classifieurs #####

# Commençons par observer les matrices de confusions de nos classifieurs

mc_rpart <- table(ET$Classification, test_rpart)
mc_c50 <- table(ET$Classification, test_c50)
mc_tree <- table(ET$Classification, test_tree)
mc_RandomForest <- table(ET$Classification, test_RandomForest)
mc_NaiveBayes <- table(ET$Classification, test_NaiveBayes)
mc_NNET <- table(ET$Classification, test_NNET)
mc_KKNN <- table(ET$Classification, test_KKNN)


# Le but de l'algorithme est de prédire quel type de voitures les clients sont sucéptibles d'acheter
# Dans le domaine de la vente, il est plus génant de se tromper réguliérement que de prédire juste tout le temps
# partant de ce constat, nous allons départager nos classifieurs en minimisant le pourcentage d'erreurs

# 1/ rpart 

nbr_err_rpart <- mc_rpart[1,2] + mc_rpart[1,3] + mc_rpart[1,4] + mc_rpart[1,5] + 
                 mc_rpart[2,1] + mc_rpart[2,3] + mc_rpart[2,4] + mc_rpart[2,5] +
                 mc_rpart[3,1] + mc_rpart[3,2] + mc_rpart[3,4] + mc_rpart[3,5] +
                 mc_rpart[4,1] + mc_rpart[4,2] + mc_rpart[4,3] + mc_rpart[4,5] +
                 mc_rpart[5,1] + mc_rpart[5,2] + mc_rpart[5,3] + mc_rpart[5,4]
taux_err_rpart <- (nbr_err_rpart/nrow(ET))*100


# 2/ c50 

nbr_err_c50 <- mc_c50[1,2] + mc_c50[1,3] + mc_c50[1,4] + mc_c50[1,5] + 
               mc_c50[2,1] + mc_c50[2,3] + mc_c50[2,4] + mc_c50[2,5] +
               mc_c50[3,1] + mc_c50[3,2] + mc_c50[3,4] + mc_c50[3,5] +
               mc_c50[4,1] + mc_c50[4,2] + mc_c50[4,3] + mc_c50[4,5] +
               mc_c50[5,1] + mc_c50[5,2] + mc_c50[5,3] + mc_c50[5,4]
taux_err_c50 <- (nbr_err_c50/nrow(ET))*100


# 3/ tree

nbr_err_tree <- mc_tree[1,2] + mc_tree[1,3] + mc_tree[1,4] + mc_tree[1,5] + 
                mc_tree[2,1] + mc_tree[2,3] + mc_tree[2,4] + mc_tree[2,5] +
                mc_tree[3,1] + mc_tree[3,2] + mc_tree[3,4] + mc_tree[3,5] +
                mc_tree[4,1] + mc_tree[4,2] + mc_tree[4,3] + mc_tree[4,5] +
                mc_tree[5,1] + mc_tree[5,2] + mc_tree[5,3] + mc_tree[5,4]
taux_err_tree <- (nbr_err_tree/nrow(ET))*100


# 4/ Random Forest

nbr_err_RandomForest <- mc_RandomForest[1,2] + mc_RandomForest[1,3] + mc_RandomForest[1,4] + mc_RandomForest[1,5] + 
                        mc_RandomForest[2,1] + mc_RandomForest[2,3] + mc_RandomForest[2,4] + mc_RandomForest[2,5] +
                        mc_RandomForest[3,1] + mc_RandomForest[3,2] + mc_RandomForest[3,4] + mc_RandomForest[3,5] +
                        mc_RandomForest[4,1] + mc_RandomForest[4,2] + mc_RandomForest[4,3] + mc_RandomForest[4,5] +
                        mc_RandomForest[5,1] + mc_RandomForest[5,2] + mc_RandomForest[5,3] + mc_RandomForest[5,4]
taux_err_RandomForest <- (nbr_err_RandomForest/nrow(ET))*100


# 5/ Naive Bayes

nbr_err_NaiveBayes <- mc_NaiveBayes[1,2] + mc_NaiveBayes[1,3] + mc_NaiveBayes[1,4] + mc_NaiveBayes[1,5] + 
                      mc_NaiveBayes[2,1] + mc_NaiveBayes[2,3] + mc_NaiveBayes[2,4] + mc_NaiveBayes[2,5] +
                      mc_NaiveBayes[3,1] + mc_NaiveBayes[3,2] + mc_NaiveBayes[3,4] + mc_NaiveBayes[3,5] +
                      mc_NaiveBayes[4,1] + mc_NaiveBayes[4,2] + mc_NaiveBayes[4,3] + mc_NaiveBayes[4,5] +
                      mc_NaiveBayes[5,1] + mc_NaiveBayes[5,2] + mc_NaiveBayes[5,3] + mc_NaiveBayes[5,4]
taux_err_NaiveBayes <- (nbr_err_NaiveBayes/nrow(ET))*100


# 6/ NNET

nbr_err_NNET <- mc_NNET[1,2] + mc_NNET[1,3] + mc_NNET[1,4] + 
                mc_NNET[2,1] + mc_NNET[2,3] + mc_NNET[2,4] +
                mc_NNET[3,1] + mc_NNET[3,2] + mc_NNET[3,3] + mc_NNET[3,4] +
                mc_NNET[4,1] + mc_NNET[4,2] + mc_NNET[4,4] +
                mc_NNET[5,1] + mc_NNET[5,2] + mc_NNET[5,3] 
taux_err_NNET <- (nbr_err_NNET/nrow(ET))*100


# 7/ KKNN

nbr_err_KKNN <- mc_KKNN[1,2] + mc_KKNN[1,3] + mc_KKNN[1,4] + mc_KKNN[1,5] + 
                mc_KKNN[2,1] + mc_KKNN[2,3] + mc_KKNN[2,4] + mc_KKNN[2,5] +
                mc_KKNN[3,1] + mc_KKNN[3,2] + mc_KKNN[3,4] + mc_KKNN[3,5] +
                mc_KKNN[4,1] + mc_KKNN[4,2] + mc_KKNN[4,3] + mc_KKNN[4,5] +
                mc_KKNN[5,1] + mc_KKNN[5,2] + mc_KKNN[5,3] + mc_KKNN[5,4]
taux_err_KKNN <- (nbr_err_KKNN/nrow(ET))*100


Performance <- data.frame(Pourcentage_FP=c(taux_err_rpart,taux_err_c50,taux_err_tree,taux_err_RandomForest,taux_err_NaiveBayes,taux_err_NNET,taux_err_KKNN),row.names = c("rpart", "C5.0","tree","RandomForest","NaiveBayes","NNET","KKNN"))
View(Performance)

##### Application du meilleur modèle de prédiction sur le fichier Marketing #####

# Au vu des résultats, le classifieur ayant le plus petit taux d'échec est le C50
# c'est donc lui que nous utiliserons pour prédire quelle catégorie de voiture les clients de la 
# table marketing sont sucéptibles d'acheter

Prediction <- predict(c50, Marketing, type="class")
table(Prediction)

Marketing <- cbind(Marketing, Prediction)

# Une fois nos prédictions effectuées, il nous reste plus qu'à écrire le fichier marketing dans un CSV 

write.csv(Marketing,'C:/Users/diazg/Desktop/Files/Cours/MIAGE/M2/0_Projet_BigData/Pasquier/Prediction.csv')


