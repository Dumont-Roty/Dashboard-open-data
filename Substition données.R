library(kableExtra)
library(knitr)
library(FactoMineR)
library(factoextra)
library(tidyverse)
library(corrplot)
library(ggplot2)
library(dplyr)

# Importation des données
d_usager <- read.csv("Data/usagers-2023.csv",
                     sep = ";")

d_lieux <- read.csv("Data/lieux-2023.csv",
                    sep = ";")

d_cara <- read.csv("Data/caract-2023.csv",
                   sep = ";")

d_vehi <- read.csv("Data/vehicules-2023.csv",
                   sep = ";")


colnames(d_cara)[1] <- "Num_Acc"

df <- inner_join(d_cara, d_vehi, by = "Num_Acc")
df <- inner_join(df, d_usager, by = "Num_Acc")
df <- inner_join(df, d_lieux, by = "Num_Acc")

df_brut <- inner_join(d_cara, d_vehi, by = "Num_Acc")
df_brut <- inner_join(df, d_usager, by = "Num_Acc")
df_brut <- inner_join(df, d_lieux, by = "Num_Acc")

df$code_departement <- df$dep

## Fonction pour remplacer les valeurs dans les colonnes
remplacer_valeurs <- function(df, colonne, mapping) {
  df[[colonne]] <- mapping[as.character(df[[colonne]])]
  return(df)
}

## Fonction pour mapper les départements aux régions
map_departements_to_regions <- function(dep) {
  if (dep %in% c("01", "03", "07", 15, 26, 38, 42, 43, 63, 69, 73, 74)) {
    return("Auvergne-Rhône-Alpes")
  } else if (dep %in% c(21, 25, 39, 58, 70, 71, 89, 90)) {
    return("Bourgogne-Franche-Comté")
  } else if (dep %in% c(22, 29, 35, 56)) {
    return("Bretagne")
  } else if (dep %in% c(18, 28, 36, 37, 41, 45)) {
    return("Centre-Val de Loire")
  } else if (dep %in% c(75, 77, 78, 91, 92, 93, 94, 95)) {
    return("Île-de-France")
  } else if (dep %in% c(14, 27, 50, 61, 76)) {
    return("Normandie")
  } else if (dep %in% c(16, 17, 19, 23, 24, 33, 40, 47, 64, 79, 86, 87)) {
    return("Nouvelle-Aquitaine")
  } else if (dep %in% c("09", 11, 12, 30, 31, 32, 34, 46, 48, 65, 66, 81, 82)) {
    return("Occitanie")
  } else if (dep %in% c(44, 49, 53, 72, 85)) {
    return("Pays de la Loire")
  } else if (dep %in% c("04", "05", "06", 13, 83, 84)) {
    return("Provence-Alpes-Côte d'Azur")
  } else if (dep %in% c("02", 59, 62, 80, 60)) {
    return("Hauts-de-France")
  } else if (dep %in% c("08", 10, 51, 52, 54, 55, 57, 67, 68, 88)) {
    return("Grand Est")
  } else if (dep %in% c("2A", "2B")) {
    return("Corse")
  } else if (dep %in% c(971, 972, 973, 974, 976)) {
    return("DOM-TOM")
  } else {
    return(NA)
  }
}


lum_map <- c(
  "1" = "Plein jour",
  "2" = "Crépuscule ou aube",
  "3" = "Nuit sans éclairage public",
  "4" = "Nuit avec éclairage public non allumé",
  "5" = "Nuit avec éclairage public allumé"
)


col_map <- c(
  "-1" = "Non renseigné",
  "1" = "Deux véhicules - frontale",
  "2" = "Deux véhicules – par l’arrière",
  "3" = "Deux véhicules – par le coté",
  "4" = "Trois véhicules et plus – en chaîne",
  "5" = "Trois véhicules et plus - collisions multiples",
  "6" = "Autre collision",
  "7" = "Sans collision"
)


catr_map <- c(
  "1" = "Autoroute",
  "2" = "Route nationale",
  "3" = "Route Départementale",
  "4" = "Voie Communale",
  "5" = "Hors réseau public",
  "6" = "Parc de stationnement",
  "7" = "Routes de métropole urbaine",
  "9" = "Autre"
)


grav_map <- c(
  "1" = "Indemne",
  "2" = "Tué",
  "3" = "Blessé hospitalisé",
  "4" = "Blessé léger"
)



atm_map <- c(
  "-1" = "Non renseigné",
  "1" = "Normale",
  "2" = "Pluie légère",
  "3" = "Pluie forte",
  "4" = "Neige - grêle",
  "5" = "Brouillard - fumée",
  "6" = "Vent fort - tempête",
  "7" = "Temps éblouissant",
  "8" = "Temps couvert",
  "9" = "Autre"
)


catv_map <- c(
  "0"  = "Indéterminable",
  "1"  = "Bicyclette",
  "2"  = "Cyclomoteur <50cm3",
  "3"  = "Voiturette (Quadricycle à moteur carrossé)",
  "4"  = "Scooter immatriculé",
  "5"  = "Motocyclette",
  "6"  = "Side-car",
  "7"  = "VL seul",
  "8"  = "VL + caravane",
  "9"  = "VL + remorque",
  "10" = "VU seul 1,5T <= PTAC <= 3,5T avec ou sans remorque",
  "11" = "VU + caravane",
  "12" = "VU + remorque",
  "13" = "PL seul 3,5T < PTCA <= 7,5T",
  "14" = "PL seul > 7,5T",
  "15" = "PL > 3,5T + remorque",
  "16" = "Tracteur routier seul",
  "17" = "Tracteur routier + semi-remorque",
  "18" = "Transport en commun",
  "19" = "Tramway",
  "20" = "Engin spécial",
  "21" = "Tracteur agricole",
  "30" = "Scooter < 50 cm3",
  "31" = "Motocyclette > 50 cm3 et <= 125 cm3",
  "32" = "Scooter > 50 cm3 et <= 125 cm3",
  "33" = "Motocyclette > 125 cm3",
  "34" = "Scooter > 125 cm3",
  "35" = "Quad léger <= 50 cm3 (Quadricycle à moteur non carrossé)",
  "36" = "Quad lourd > 50 cm3 (Quadricycle à moteur non carrossé)",
  "37" = "Autobus",
  "38" = "Autocar",
  "39" = "Train",
  "40" = "Tramway",
  "41" = "3RM <= 50 cm3",
  "42" = "3RM > 50 cm3 <= 125 cm3",
  "43" = "3RM > 125 cm3",
  "50" = "EDP à moteur",
  "60" = "EDP sans moteur",
  "80" = "VAE",
  "99" = "Autre véhicule"
)



motor_map <- c(
  "-1" = "Non renseigné",
  "0" = "Inconnue",
  "1" = "Hydrocarbures",
  "2" = "Hybride électrique",
  "3" = "Electrique",
  "4" = "Hydrogène",
  "5" = "Humaine",
  "6" = "Autre"
)


sexe_map <- c(
  "1" = "Masculin",
  "2" = "Féminin"
)


trajet_map <- c(
  "-1" = "Non renseigné",
  "0" = "Non renseigné",
  "1" = "Domicile <-> travail",
  "2" = "Domicile <-> école",
  "3" = "Courses <-> achats",
  "4" = "Utilisation professionnelle",
  "5" = "Promenade <-> loisirs",
  "9" = "Autre"
)



agg_map <- c(
  "1" = "Hors agglomération",
  "2" = "En agglomération"
)


prof_map <- c(
  "-1" = "Non renseigné",
  "1" = "Plat",
  "2" = "Pente",
  "3" = "Sommet de côte",
  "4" = "Bas de côte"
)


surf_map <- c(
  "-1" = "Non renseigné",
  "1" = "Normale",
  "2" = "Mouillée",
  "3" = "Flaques",
  "4" = "Inondée",
  "5" = "Enneigée",
  "6" = "Boue",
  "7" = "Verglacée",
  "8" = "Corps gras – huile",
  "9" = "Autre"
)


manv_map <- c(
  "-1" = "Non renseigné",
  "0" = "Inconnue",
  "1" = "Sans changement de direction",
  "2" = "Même sens, même file",
  "3" = "Entre 2 files",
  "4" = "En marche arrière",
  "5" = "A contresens",
  "6" = "En franchissant le terre-plein central",
  "7" = "Dans le couloir bus, dans le même sens",
  "8" = "Dans le couloir bus, dans le sens inverse",
  "9" = "En s’insérant",
  "10" = "En faisant demi-tour sur la chaussée",
  "11" = "Changeant de file à gauche",
  "12" = "Changeant de file à droite",
  "13" = "Déporté à gauche",
  "14" = "Déporté à droite",
  "15" = "Tournant à gauche",
  "16" = "Tournant à droite",
  "17" = "Dépassant à gauche",
  "18" = "Dépassant à droite",
  "19" = "Traversant la chaussée",
  "20" = "Manœuvre de stationnement",
  "21" = "Manœuvre d’évitement",
  "22" = "Ouverture de porte",
  "23" = "Arrêté (hors stationnement)",
  "24" = "En stationnement (avec occupants)",
  "25" = "Circulant sur trottoir",
  "26" = "Autres manœuvres"
)


int_map <- c(
  "1" = "Hors intersection",
  "2" = "Intersection en X",
  "3" = "Intersection en T",
  "4" = "Intersection en Y",
  "5" = "Intersection à plus de 4 branches",
  "6" = "Giratoire",
  "7" = "Place",
  "8" = "Passage à niveau",
  "9" = "Autre intersection"
)


senc_map <- c(
  "-1" = "Non renseigné",
  "0" = "Inconnu",
  "1" = "PK ou PR ou numéro d’adresse postale croissant",
  "2" = "PK ou PR ou numéro d’adresse postale décroissant",
  "3" = "Absence de repère"
)


obs_map <- c(
  "-1" = "Non renseigné",
  "0" = "Sans objet",
  "1" = "Véhicule en stationnement",
  "2" = "Arbre",
  "3" = "Glissière métallique",
  "4" = "Glissière béton",
  "5" = "Autre glissière",
  "6" = "Bâtiment, mur, pile de pont",
  "7" = "Support de signalisation verticale ou poste d’appel d’urgence",
  "8" = "Poteau",
  "9" = "Mobilier urbain",
  "10" = "Parapet",
  "11" = "Ilot, refuge, borne haute",
  "12" = "Bordure de trottoir",
  "13" = "Fossé, talus, paroi rocheuse",
  "14" = "Autre obstacle fixe sur chaussée",
  "15" = "Autre obstacle fixe sur trottoir ou accotement",
  "16" = "Sortie de chaussée sans obstacle",
  "17" = "Buse – tête d’aqueduc"
)


obsm_map <- c(
  "-1" = "Non renseigné",
  "0" = "Aucun",
  "1" = "Piéton",
  "2" = "Véhicule",
  "4" = "Véhicule sur rail",
  "5" = "Animal domestique",
  "6" = "Animal sauvage",
  "9" = "Autre"
)


dep_map <- c(
  "01" = "Ain",
  "02" = "Aisne",
  "03" = "Allier",
  "04" = "Alpes-de-Haute-Provence",
  "05" = "Hautes-Alpes",
  "06" = "Alpes-Maritimes",
  "07" = "Ardèche",
  "08" = "Ardennes",
  "09" = "Ariège",
  "10" = "Aube",
  "11" = "Aude",
  "12" = "Aveyron",
  "13" = "Bouches-du-Rhône",
  "14" = "Calvados",
  "15" = "Cantal",
  "16" = "Charente",
  "17" = "Charente-Maritime",
  "18" = "Cher",
  "19" = "Corrèze",
  "2A" = "Corse-du-Sud",
  "2B" = "Haute-Corse",
  "21" = "Côte-d'Or",
  "22" = "Côtes-d'Armor",
  "23" = "Creuse",
  "24" = "Dordogne",
  "25" = "Doubs",
  "26" = "Drôme",
  "27" = "Eure",
  "28" = "Eure-et-Loir",
  "29" = "Finistère",
  "30" = "Gard",
  "31" = "Haute-Garonne",
  "32" = "Gers",
  "33" = "Gironde",
  "34" = "Hérault",
  "35" = "Ille-et-Vilaine",
  "36" = "Indre",
  "37" = "Indre-et-Loire",
  "38" = "Isère",
  "39" = "Jura",
  "40" = "Landes",
  "41" = "Loir-et-Cher",
  "42" = "Loire",
  "43" = "Haute-Loire",
  "44" = "Loire-Atlantique",
  "45" = "Loiret",
  "46" = "Lot",
  "47" = "Lot-et-Garonne",
  "48" = "Lozère",
  "49" = "Maine-et-Loire",
  "50" = "Manche",
  "51" = "Marne",
  "52" = "Haute-Marne",
  "53" = "Mayenne",
  "54" = "Meurthe-et-Moselle",
  "55" = "Meuse",
  "56" = "Morbihan",
  "57" = "Moselle",
  "58" = "Nièvre",
  "59" = "Nord",
  "60" = "Oise",
  "61" = "Orne",
  "62" = "Pas-de-Calais",
  "63" = "Puy-de-Dôme",
  "64" = "Pyrénées-Atlantiques",
  "65" = "Hautes-Pyrénées",
  "66" = "Pyrénées-Orientales",
  "67" = "Bas-Rhin",
  "68" = "Haut-Rhin",
  "69" = "Rhône",
  "70" = "Haute-Saône",
  "71" = "Saône-et-Loire",
  "72" = "Sarthe",
  "73" = "Savoie",
  "74" = "Haute-Savoie",
  "75" = "Paris",
  "76" = "Seine-Maritime",
  "77" = "Seine-et-Marne",
  "78" = "Yvelines",
  "79" = "Deux-Sèvres",
  "80" = "Somme",
  "81" = "Tarn",
  "82" = "Tarn-et-Garonne",
  "83" = "Var",
  "84" = "Vaucluse",
  "85" = "Vendée",
  "86" = "Vienne",
  "87" = "Haute-Vienne",
  "88" = "Vosges",
  "89" = "Yonne",
  "90" = "Territoire de Belfort",
  "91" = "Essonne",
  "92" = "Hauts-de-Seine",
  "93" = "Seine-Saint-Denis",
  "94" = "Val-de-Marne",
  "95" = "Val-d'Oise",
  "971" = "Guadeloupe",
  "972" = "Martinique",
  "973" = "Guyane",
  "974" = "La Réunion",
  "976" = "Mayotte"
)





df <- remplacer_valeurs(df, "lum", lum_map)
df <- remplacer_valeurs(df, "col", col_map)
df <- remplacer_valeurs(df, "catr", catr_map)
df <- remplacer_valeurs(df, "grav", grav_map)

df <- remplacer_valeurs(df, "atm", atm_map)
df <- remplacer_valeurs(df, "catv", catv_map)
df <- remplacer_valeurs(df, "motor", motor_map)
df <- remplacer_valeurs(df, "sexe", sexe_map)
df <- remplacer_valeurs(df, "trajet", trajet_map)

df <- remplacer_valeurs(df, "agg", agg_map)
df <- remplacer_valeurs(df, "prof", prof_map)
df <- remplacer_valeurs(df, "surf", surf_map)
df <- remplacer_valeurs(df, "manv", manv_map)

df <- remplacer_valeurs(df, "int", int_map)
df <- remplacer_valeurs(df, "senc", senc_map)
df <- remplacer_valeurs(df, "obs", obs_map)
df <- remplacer_valeurs(df, "obsm", obsm_map)
df <- df |> mutate(region = sapply(dep, map_departements_to_regions))
df <- remplacer_valeurs(df, "dep", dep_map)

# Ecriture des données dans un fichier csv
write.csv(df, "Data/modified_data.csv", row.names = TRUE)
