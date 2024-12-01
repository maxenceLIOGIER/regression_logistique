# regression_logistique
Ce projet a pour but de créer un package R permettant de mettre au point un algorithme de régression logistique.
L'optimisation de la fonction de coût se fera à l'aide d'une descente de gradient. Pour illsutrer le fonctionnement de notre package on a construit une application Rshiny. 

## 1. Installation du package sur R

Pour installer notre package sur R, suivez les étapes ci-dessous :

### Étape 1 : Installer et charger le package `devtools`

Avant d'installer le package, vous devez vous assurer que le package `devtools` est installé. Si ce n'est pas encore fait, vous pouvez l'installer en utilisant les commandes suivantes :

```r
install.packages("devtools")
library(devtools)
```
### Étape 2 : Installer Rtools
aller sur le lien: https://cran.r-project.org/bin/windows/Rtools/rtools44/rtools.html télécharger Rtools44 installer puis éxectuer le.

### Étape 3 : Installer le package depuis GitHub
Une fois devtools installé, vous pouvez installer notre package directement depuis GitHub avec la commande suivante :

```r
install_github("votre_nom_utilisateur/votre_repo")
```
Pour plus de détails, vous sur le fonctionnement du package vous pouvez lire le tuorial.md en anglais qui explique à peu près tout de A à Z.


## 2. Application R shiny 
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
Pour exécuter l'application Shiny, placez-vous dans le répertoire contenant **global.R**, **ui.R** et **server.R**, puis lancez l'application avec `runApp()`. Assurez-vous que tous les packages mentionnés dans **global.R** sont installés sur votre machine avec install.packages().

### Fonctionnalités de l'application Shiny
L'application Shiny permet de naviguer à travers plusieurs pages grâce à une barre latérale. Les principales pages de l'application sont les suivantes :

**1. Accueil** :

Page d'introduction qui présente le projet, les fonctionnalités disponibles et l'équipe.


![image](https://github.com/user-attachments/assets/fb4b6db0-da21-435e-8c00-c9fdac3be735)

**2. Lecture des données** :

Cette page permet de télécharger des fichiers CSV ou Excel et de préparer les données pour l'analyse. Elle contient trois onglets : l'onglet **Chargement et Prétraitement des données**, l'onglet **Structure des données** qui affiche la fonction `str()` de R pour vérifier la transformation des données si nécessaire, et l'onglet **Résumé statistique** qui génère un résumé statistique des données à l'aide de la fonction `summary()`.


![image](https://github.com/user-attachments/assets/35a99d81-5656-4e35-a9e6-b8a18da710fb)


**3. Statistiques et Visualisation**:

Fournit les distibutions des variables.

![image](https://github.com/user-attachments/assets/f119bae2-73f7-4c3f-a2ad-bc82386a0c38)




**4. Modélisation et Prédiction** :

Cette page utilise notre package de régression logistique multinomiale pour effectuer l'ensemble du processus de modélisation. Elle permet de diviser les données (split), d'ajuster le modèle (fit), de réaliser des prédictions et d'évaluer les performances du modèle à l'aide de diverses métriques.


![image](https://github.com/user-attachments/assets/506af335-d952-4598-a986-2879524c912b)


Pour un tutoriel complet sur l'utilisation de l'application, nous vous invitons à visionner notre vidéo explicative en cliquant sur le lien ci-dessous:

https://youtu.be/qKJapa4DgGE



