# regression_logistique
Ce projet a pour but de créer un package R permettant de mettre au point un algorithme de régression logistique.
L'optimisation de la fonction de coût se fera à l'aide d'une descente de gradient. Pour illsutrer le fonctionnement de notre package on a construit une application Rshiny. 

## Installation du package sur R

Pour installer notre package sur R, suivez les étapes ci-dessous :

### Étape 1 : Installer le package `devtools`

Avant d'installer le package, vous devez vous assurer que le package `devtools` est installé. Si ce n'est pas encore fait, vous pouvez l'installer en utilisant la commande suivante :

```r
install.packages("devtools")
```
### Étape 2 : Installer le package depuis GitHub
Une fois devtools installé, vous pouvez installer notre package directement depuis GitHub avec la commande suivante :

```r
devtools::install_github("votre_nom_utilisateur/votre_repo")
```


## Application shiny 
Voici l'architecture dossier de l'application shiny :

```r
/app
│
├── /global            # Contient tous les packages utilisés dans le projet
│
├── /ui                # Contient les fichiers relatifs à l'interface utilisateur
│
├── /server            # Contient tout le backend de l'application
│
└── /www               # Contient les fichiers statiques comme les styles et images
    ├── style.css      # Le fichier CSS pour la mise en forme de l'application
    └── images.jpg     # Image utilisée dans l'application

```

### Fonctionnalités de l'application Shiny
L'application Shiny permet de naviguer à travers plusieurs pages grâce à une barre latérale. Les principales pages de l'application sont les suivantes :

**Accueil** :
Page d'introduction qui présente le projet, les fonctionnalités disponibles et l'équipe.


![image](https://github.com/user-attachments/assets/fb4b6db0-da21-435e-8c00-c9fdac3be735)

**Lecture des données** :
Permet de télécharger des fichiers CSV ou Excel et de préparer les données pour l'analyse. cette page contient 3 onglet , l'onglet de chargement des données et pretraitement des données , l'onglet structure des donées qui reprend le fonction str de R afin de vverifier la transformation des données si besoin , l'onglet résumé statistique qui fait un summary des onnés
![image](https://github.com/user-attachments/assets/35a99d81-5656-4e35-a9e6-b8a18da710fb)


**Statistiques et Visualisation**:
Fournit les distibutions des variables.
![image](https://github.com/user-attachments/assets/f119bae2-73f7-4c3f-a2ad-bc82386a0c38)




**Modélisation et Prédiction** :
Cette page utilise notre package de régression logistique multinomiale pour effectuer l'ensemble du processus de modélisation. Elle permet de diviser les données (split), d'ajuster le modèle (fit), de réaliser des prédictions et d'évaluer les performances du modèle à l'aide de diverses métriques.


![image](https://github.com/user-attachments/assets/506af335-d952-4598-a986-2879524c912b)


Pour un tutoriel complet sur l'utilisation de l'application, nous vous invitons à visionner notre vidéo explicative en cliquant sur le lien ci-dessous



