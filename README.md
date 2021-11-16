---
title: "Script 1"
author: "Boris Mericskay et Florent Demoraes"
date: "12/11/2021"
output:
  html_document: default
  pdf_document: default
---


---

PRÉPARATION DES DONNÉES DVF OPENDATA
---
Ce script a comme objectif de préparer les données DVF en opendata préalablement à les analyses (nettoyage, filtrage, agrégation,...).

## Préparation du projet

### Définition de l'environnement de travail

On définit ici le dossier qui centralise les données et où les différents jeux de données seront exportés

```{r setup, include=FALSE} 
knitr::opts_knit$set(root.dir = 'C:/DVF')
knitr::opts_chunk$set(warning = FALSE, message = FALSE) 
```

### Chargement des packages R nécessaires

Seul le package `tidyverse` dédié à la manipulation de données est nécessaire ici.

```{r}
library(tidyverse)
```


### Import du jeu de données brut

```{r}

DVF <- read.csv("DATA/DVF_brut.csv", encoding="UTF-8", stringsAsFactors=FALSE)
```

---

## Filtre et nettoyage des données

### Etape1> Sélection des mutations de type "Ventes" de "Maison" et "Appartement'

```{r}
etape1 <- DVF %>% filter(nature_mutation == "Vente")
etape1bis <- etape1 %>% filter(type_local == "Maison" | type_local == "Appartement")
```

### Etape2> Sélection et renommage des variables

```{r}
etape2 <- etape1bis %>% select(id = id_mutation, disposition = numero_disposition, parcelle= id_parcelle, date = date_mutation, nature = nature_mutation, codecommune = code_commune, departement = code_departement, type = type_local, surface = surface_reelle_bati, piece = nombre_pieces_principales, prix = valeur_fonciere, latitude, longitude)
```

### Etape3> Remplacement des cellules vides par des NA et suppression des NA

```{r}
etape2[etape2==""] <- NA
etape3 <- etape2 %>% na.omit() # pour supprimer les valeurs manquantes
```

---

## Suppression des doublons et des mutations multiventes

### Etape4> Sélections des mutations simples

#### Regrouper les transactions selon l'ID, la surface et la vente
```{r}
unique <- etape3 %>% distinct(id, prix, surface)
nbunique <- unique %>% group_by(id) %>% summarise(nb = n())
```
#### Suppression des doublons et des mutations multiventes
```{r}
etape4 <- nbunique %>% filter(nb==1)
```
### Etape5> Jointure attributaire pour récupérer les informations de la mutation
```{r}
merge <- merge(etape4,etape3, by="id")
etape5 <- merge %>% distinct(id, .keep_all = TRUE) %>% select(id, date, type, nature, codecommune, prix, surface, piece, latitude, longitude)
```

#### Modification des formats des colonnes
```{r}
etape5$prix <- as.integer(etape5$prix)
etape5$surface <- as.numeric(etape5$surface)
etape5$piece <- as.numeric(etape5$piece)
```

---

## Suppression des valeurs aberrantes

#### Fixer un seuil minimal des prix (percentile)
```{r}
quantile(etape5$prix, 0.01)
```
#### Fixer un seuil maximal des prix (histogramme)
```{r}
ggplot(etape5, aes(x=prix)) + geom_histogram(bins = 100) + xlim(10000,5000000)
```
#### Créer deux jeux de données (maisons / appartements)
```{r}
Maisons <- etape5 %>% filter(type == 'Maison')
Appartement <- etape5 %>% filter (type == 'Appartement')    
```
 
#### Fixer un seuil maximal des surfaces (histogramme)
```{r}
hist(Maisons$surface, nclass = 500, xlim = c(0,600))
hist(Appartement$surface, nclass = 500, xlim = c(0,300))

```
### Etape 6> Sélection des bornes de prix et de surface
```{r}
etape6 <- etape5 %>% filter(between(prix, 15000, 5000000)) %>%
                     filter(case_when(type=='Appartement' ~  between(surface, 10, 200)) | 
                            case_when(type=='Maison' ~  between(surface, 10, 350)))
```
---
## Création du prix au m² et filtre des valeurs extrêmes et aberrantes 

### Etape7> Création de la variable prix/m²
```{r}
etape7 <- etape6 %>% mutate(prixm2 = prix/surface)
etape7$prixm2 <- round(etape7$prixm2)
```

### Etape8 > Sélection des bornes de prix au m²

#### Fixer un seuil minimal des prix au m² (percentile)
```{r}
quantile(etape7$prixm2, 0.01)
```

#### Fixer un seuil maximal des prix au m² (histogramme)
```{r}
hist(etape7$prixm2, breaks = 1000, xlim = c(0,10000))
```

#### Filtres des valeurs
```{r}
etape8 <- etape7 %>% filter(between(prixm2,330,8000))
```
#### Transformation de la date en année
```{r}
etape8$date <- as.character(etape8$date)
etape8 <- etape8 %>% mutate(ANNEE = substr(etape8$date, 1, 4))
```

---

## Enrichissement des mutations avec de nouvelles variables 


### Etape9 >Ajout de variables relatives à la commune et à l'EPCI (Liste des communes du Code Officiel Géographique)
```{r}
COG <- read.csv("DATA/COG.csv", sep=";", stringsAsFactors=FALSE)
COG <- COG %>% mutate(codecommune = CODGEO)
etape9 <- merge(etape8, COG, "codecommune")
```
### Etape10 >Ajout de variables relatives au département (Liste des départements du Code Officiel Géographique)
```{r}
Dep <- read.csv("DATA/departement2020.csv", encoding="UTF-8", sep=";", stringsAsFactors=FALSE)
Dep <- Dep %>% mutate(DEP = dep)
etape10 <- merge(etape9, Dep, "DEP")
```
### Etape11 >Ajout de la typologie des communes de l'INSEE
```{r}
TYPOINSEE <- read.csv("DATA/TYPOINSEE.csv", sep=";", stringsAsFactors=FALSE)
TYPOINSEE <- TYPOINSEE %>% mutate(codecommune = Code)
etape11 <- merge(etape10, TYPOINSEE, 'codecommune')
```

### Etape12 > Structuration du jeu de données final
```{r}
DVFOK <- etape11 %>% select(id, date, annee = ANNEE, type, prix, surface, prixm2, codecommune, commune = LIBGEO, Dep = nccenr, EPCI= LIBEPCI, Typo_INSEE = typo, latitude, longitude)
```

### Ecrire le jeu de données final en csv
```{r , echo=TRUE}
write.csv(DVFOK, 'Exports/DVFOK.csv', row.names = FALSE, fileEncoding = "UTF-8", quote = FALSE)
```

---

## Diagnostic des types de mutations

### Nombre de mutations uniques
```{r}
length(unique(etape3$id))
```

### Nombre de mutations multiventes
```{r}
Mutationscomplexes <- nbunique %>% filter(nb>1)
length(unique(Mutationscomplexes$id))
```

### Nombre de mutations monoventes/monolignes
```{r}
uniquesimple <- etape3 %>% group_by(id) %>% summarise(prix = max(prix), surface = max(surface), nb = n())
nbtransactionstimplemonoligne <-uniquesimple %>% filter(nb==1)
length(unique(nbtransactionstimplemonoligne$id))
```
### Nombre de mutations monoventes/multilignes
```{r}
(length(unique(etape3$id)))-(length(unique(nbtransactionstimplemonoligne$id)))
```
### Diagnostic du nombre de biens par mutation
```{r}
diaggroup <- nbunique %>% mutate(as.character(nb)) %>% group_by(nb) %>% summarise(tot = n())
```

### Nombre de biens et nombre moyen de biens pour les mutations multiventes
```{r}
length(Mutationscomplexes$id)
sum(Mutationscomplexes$nb)
mean(Mutationscomplexes$nb)
```
