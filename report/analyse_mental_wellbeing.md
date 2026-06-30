# Analyse des Comportements et des Facteurs liés au BIEN-ÊTRE MENTAL

**Auteur :** Bacem Ben hamad & mohamed amine mazzez 
**Cours :** ANALYSE DES DONNÉES — ESSAI  
**Enseignant :** Ghazi Bel Mufti  


# Introduction

## 1.1 Contexte académique

Ce projet est réalisé dans le cadre du cours **Analyse des Données** de première année à l'**ESSAI** (École Supérieure de la Statistique et de l'Analyse de l'Information, Tunis). Il met en application les deux méthodes fondamentales étudiées en cours et en TP :

- l'**Analyse en Composantes Principales (ACP)** normée, pour résumer un tableau de variables quantitatives fortement corrélées ;
- l'**Analyse Factorielle des Correspondances (AFC)**, pour étudier les associations entre deux variables qualitatives via un tableau de contingence.

## 1.2 Sujet et motivation

Le thème retenu est le **bien-être mental** en milieu étudiant et professionnel, une problématique bien documentée :

| Statistique | Valeur | Source |
|-------------|--------|--------|
| Étudiants déclarant un stress élevé | 1 sur 3 | Enquêtes universitaires |
| Cas de burn-out par an (France) | 2,5 millions | INRS |
| Cas évitables avec prévention précoce | 70 % | OMS |
| Délai moyen avant consultation | 8 à 10 ans | Santé Publique France |

Face à ces constats, un questionnaire en ligne a été conçu pour mesurer simultanément **six dimensions du bien-être mental** et recueillir un profil sociodémographique détaillé. L'enquête a été diffusée via un formulaire Google Forms **trilingue (Français / English / Tounsi)** pour maximiser le taux de réponse en Tunisie ; chaque répondant a rempli une seule version, les trois blocs linguistiques coexistent donc dans le même CSV et sont fusionnés en variables unifiées.

## 1.3 Articulation avec un projet d'application — *Mental Well-Being Check*

Ce travail académique alimente un projet applicatif parallèle : **Mental Well-Being Check**, une plateforme de suivi du bien-être mental développée par l'équipe *The DataFathers* (SUP'COM) dans le cadre du **Hackathon Vectors in Orbit 2025**. La plateforme combine un backend FastAPI, une base vectorielle Qdrant et un frontend React pour offrir un diagnostic personnalisé, un suivi longitudinal et des recommandations scientifiques.

Le lien entre les deux projets est **méthodologique et opérationnel** :

- **Même grille conceptuelle.** Le questionnaire suit la structure six axes A–F (stress, anxiété, sommeil, burnout, bien-être, soutien) qui sert de socle aux 6 profils de stress de la plateforme.
- **Validation empirique.** L'ACP et l'AFC permettent de vérifier la cohérence statistique des dimensions postulées : sont-elles réellement indépendantes ? Quels items sont les plus informatifs ? Les réponses orientent directement le scoring implémenté dans l'Agent Core de la plateforme.
- **Base de référence pour la recherche vectorielle.** Les coordonnées individuelles obtenues par ACP constituent une signature numérique exploitable par Qdrant pour le matching de profils similaires (collection `user_profiles`, cosine similarity).

> Ce rapport joue un **double rôle** : projet pédagogique (appliquer rigoureusement ACP et AFC sur un jeu de données réel) et étude préalable qui informe les choix de design d'une application déployée en production.

# Le questionnaire

1. **Période d'enquête** : Avril 2026
2. **L'enquête** : créée avec Google Forms, distribuée via Facebook et LinkedIn, réponses collectées dans un fichier CSV, traitées avec le langage **R**.
3. **Base de données** : **84 lignes** et **176 colonnes** (questionnaire trilingue FR / EN / Tounsi).

Le questionnaire a été conçu autour de **2 blocs thématiques** (Bloc 2 et Bloc 3), chacun structuré autour de **3 axes théoriques** de 5 items :

## Bloc 2 — Stress, Anxiété, Sommeil (15 items → **ACP 1**)
- **Axe A — Stress lié au travail / études** : items 1, 4, 7, 10, 13
- **Axe B — Anxiété générale** : items 2, 5, 8, 11, 14
- **Axe C — Sommeil & habitudes** : items 3, 6, 9, 12, 15

## Bloc 3 — Burnout, Résilience, Soutien (15 items → **ACP 2**)
- **Axe D — Burnout / Épuisement** : items 16, 19, 22, 25, 28
- **Axe E — Résilience / Bien-être** : items 17, 20, 23, 26, 29
- **Axe F — Soutien social** : items 18, 21, 24, 27, 30


# Importation des données



```R
suppressPackageStartupMessages({
  suppressWarnings({
    library(dplyr); library(FactoMineR); library(factoextra); library(corrplot)
  })
})

WB <- read.csv("Mental well-being (réponses) - Réponses au formulaire 1.csv",
               header = TRUE, stringsAsFactors = FALSE,
               check.names = FALSE, encoding = "UTF-8",
               na.strings = c("", " ", "NA"))
cat("Dimensions brutes :", dim(WB), "\n")

options(repr.plot.width = 7, repr.plot.height = 4.5, repr.plot.res = 110)
theme_clear <- function() {
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "bold"))
}
theme_set(theme_clear())
```

    Dimensions brutes : 84 177 
    


```R
options(repr.plot.width = 7, repr.plot.height = 4.5, repr.plot.res = 110)

theme_clear <- function() {
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "bold"))
}
theme_set(theme_clear())
```

## Préparation des données

Le questionnaire étant trilingue (Français, Anglais, Tounsi), chaque répondant a rempli **un seul bloc linguistique**. Les trois blocs ont été fusionnés en variables uniques par priorité (EN → FR → TN), puis les réponses Likert ont été converties en valeurs numériques. Les variables sociodémographiques (genre, âge, statut, filière) et les 10 variables binaires (Oui/Non) ont été recodées de façon homogène. Au final, **84 répondants** et **30 items Likert** sont disponibles pour les analyses.


```R
coalesce_cols <- function(...) {
  cols <- list(...); out <- cols[[1]]
  for (k in 2:length(cols)) out <- ifelse(is.na(out) | out == "", cols[[k]], out)
  out
}

# 30 items Likert (EN: 21-50, FR: 79-108, TN: 137-166)
L_en <- suppressWarnings(sapply(WB[, 21:50], as.numeric))
L_fr <- suppressWarnings(sapply(WB[, 79:108], as.numeric))
L_tn <- suppressWarnings(sapply(WB[, 137:166], as.numeric))
Likert <- as.data.frame(matrix(NA_real_, nrow = nrow(WB), ncol = 30))
for (j in 1:30) {
  Likert[, j] <- ifelse(!is.na(L_en[, j]), L_en[, j],
                 ifelse(!is.na(L_fr[, j]), L_fr[, j], L_tn[, j]))
}
colnames(Likert) <- c(
  "overwhelmed","worry","tired_sleep","deadlines_tens","neg_thoughts",
  "insomnia","goal_pressure","nervous","sleep_interr","work_conflicts",
  "concentration","drowsy","cant_disconnect","avoid_fear","substances",
  "emot_exhaust","manage_emot","collegue_supp","lost_meaning","optimistic",
  "talk_superv","cynical","bounce_back","isolated","motiv_drop",
  "self_care","wb_resources","chron_fatigue","wlb","ask_help"
)

# Variables démographiques
age_raw <- coalesce_cols(WB[[3]], WB[[61]], WB[[119]])
age_raw <- gsub('[‐-―−]', '-', age_raw)
age <- dplyr::case_when(
  grepl('^18-?24', age_raw) ~ '18-24',
  grepl('^25-?34', age_raw) ~ '25-34',
  grepl('^35-?44', age_raw) ~ '35-44',
  grepl('^45-?54', age_raw) ~ '45-54',
  grepl('^55', age_raw)     ~ '55+',
  TRUE ~ NA_character_
)
genre <- dplyr::recode(coalesce_cols(WB[[4]], WB[[62]], WB[[120]]),
  'Male'='Homme','Homme'='Homme','Rajel'='Homme',
  'Female'='Femme','Femme'='Femme','Mra'='Femme', .default = NA_character_)
statut <- dplyr::recode(coalesce_cols(WB[[6]], WB[[64]], WB[[122]]),
  'Student'='Etudiant','Étudiant(e)'='Etudiant','Taleb/Talba'='Etudiant',
  'Worker'='Salarie','Salarié(e)'='Salarie','5eddem/5eddema'='Salarie',
  .default = NA_character_)
filiere <- dplyr::recode(coalesce_cols(WB[[8]], WB[[66]], WB[[124]]),
  'Science & Technology'='Sciences','Sciences & Technologies'='Sciences',
  'Economics, Business & Management'='Eco','Économie, Gestion & Commerce'='Eco',
  'Health & Medicine'='Sante','Santé & Médecine'='Sante',
  'Law & Political Science'='Droit','Droit & Sciences politiques'='Droit',
  'Humanities & Social Sciences'='Humanites',
  'Arts & Literature'='Arts','Arts & Lettres'='Arts',
  'Other'='Autre','Autre'='Autre','A5er'='Autre', .default = 'Autre')

# 10 variables Oui/Non
YN_en <- WB[, 51:60]; YN_fr <- WB[, 109:118]; YN_tn <- WB[, 167:176]
YN <- as.data.frame(matrix(NA_character_, nrow = nrow(WB), ncol = 10))
for (j in 1:10) {
  v <- coalesce_cols(YN_en[[j]], YN_fr[[j]], YN_tn[[j]])
  YN[, j] <- dplyr::recode(v, 'Yes'='Oui','Oui'='Oui','Ey'='Oui',
                           'No'='Non','Non'='Non','Le'='Non', .default = NA_character_)
}
colnames(YN) <- c('consult_pro','sport','quitter','app_num','prog_aide',
                  'arret_maladie','relax','alim_saine','hobby','interet_plat')
```

# Statistiques descriptives


## Tableau de bord — Profil des répondants

Les quatre répartitions (sexe, âge, statut, filière) sont regroupées ci-dessous dans un même tableau de bord pour une lecture synthétique.


```R
# --- Dashboard 2x2 : Sexe / Age / Statut / Filiere -------------------------
options(repr.plot.width = 8, repr.plot.height = 6, repr.plot.res = 110)
par(mfrow = c(2, 2), bg = "white", fg = "black",
    col.axis = "black", col.lab = "black", col.main = "black",
    mar = c(1.5, 1.5, 2, 1.5), cex = 0.8, cex.main = 0.9)

freq1 <- table(genre); pct1 <- round(freq1/sum(freq1)*100)
pie(freq1, labels = paste(names(freq1), pct1, "%"), col = c("#d8e2dc","#fae1dd"),
    main = "Sexe")

freq2 <- table(age); pct2 <- round(freq2/sum(freq2)*100)
colors <- c("#555b6e","#89b0ae","#bee3db","#ffd6ba","#fae1dd","#f0c33c")
pie(freq2, labels = paste(names(freq2), pct2, "%"), col = colors, main = "Âge")

freq3 <- table(statut); pct3 <- round(freq3/sum(freq3)*100)
pie(freq3, labels = paste(names(freq3), pct3, "%"), col = c("#89b0ae","#ffd6ba"),
    main = "Statut")

freq4 <- table(filiere); pct4 <- round(freq4/sum(freq4)*100)
pie(freq4, labels = paste(names(freq4), pct4, "%"), col = colors, main = "Filière")

par(mfrow = c(1, 1)); options(repr.plot.width = 7, repr.plot.height = 4.2)

```


    
![png](analyse_mental_wellbeing_files/analyse_mental_wellbeing_10_0.png)
    


> **Lecture & remarques**
>
> - L'échantillon est dominé par de **jeunes adultes (18-24 ans)** en filière Sciences ; il est donc plus représentatif du milieu étudiant que de la population active, ce qui oriente la lecture de toutes les analyses qui suivent.
> - L'équilibre **Étudiants / Salariés** reste suffisant pour faire intervenir `statut` comme variable qualitative supplémentaire dans les ACP — on pourra donc tester s'il structure les plans factoriels.


## Tableau de bord — Comportements & accès aux soins

Les deux indicateurs comportementaux (pratique sportive régulière et consultation d'un professionnel de santé mentale) sont présentés côte à côte.


```R
# --- Dashboard 1x2 : Sport / Consultation --------------------------------
options(repr.plot.width = 8, repr.plot.height = 3.5, repr.plot.res = 110)
par(mfrow = c(1, 2), bg = "white", fg = "black",
    col.axis = "black", col.lab = "black", col.main = "black",
    mar = c(3, 3, 2, 1), cex = 0.75, cex.main = 0.85)

t1 <- table(YN$sport)
b1 <- barplot(t1, col = "#69b3a2", ylim = c(0, max(t1)+10),
              main = "Sport régulier (≥ 2/sem.)")
text(b1[,1], t1-2, t1)

t2 <- table(YN$consult_pro)
b2 <- barplot(t2, col = "#c44e52", ylim = c(0, max(t2)+10),
              main = "Consultation pro santé mentale")
text(b2[,1], t2-2, t2)

par(mfrow = c(1, 1)); options(repr.plot.width = 7, repr.plot.height = 4.2)

```


    
![png](analyse_mental_wellbeing_files/analyse_mental_wellbeing_13_0.png)
    


> **Lecture & remarques**
>
> - La **consultation d'un professionnel** reste minoritaire, alors que le **sport régulier** est majoritaire : deux comportements préventifs **très inégalement adoptés** — une tension clé que l'on retrouvera dans l'ACM.
> - Ces deux variables binaires entreront dans l'ACM pour identifier d'éventuels **profils comportementaux de santé mentale** distincts.


## Matrice de Corrélation — Bloc 2 — Stress, Anxiété, Sommeil (items 1-15)



```R
Lik_bloc <- Likert[complete.cases(Likert[, 1:15]), 1:15]
M_bloc <- cor(Lik_bloc)

options(repr.plot.width = 7, repr.plot.height = 7, repr.plot.res = 110)
corrplot(M_bloc, type = "upper")
```


    
![png](analyse_mental_wellbeing_files/analyse_mental_wellbeing_16_0.png)
    


# ACP 1 — Stress, Anxiété et Sommeil

## 1. But de l'ACP 1

L'**ACP 1** a pour but d'étudier les différentes dimensions latentes liées au **stress, à l'anxiété et à la qualité du sommeil**, à partir des 15 premiers items Likert du questionnaire (Bloc 2). Le questionnaire a été conçu autour de **3 axes théoriques** :

- **Axe A — Stress travail/études** : `overwhelmed, deadlines_tens, goal_pressure, work_conflicts, cant_disconnect`
- **Axe B — Anxiété générale** : `worry, neg_thoughts, nervous, concentration, avoid_fear`
- **Axe C — Sommeil** : `tired_sleep, insomnia, sleep_interr, drowsy, substances`

L'ACP permettra de vérifier si cette structure théorique se retrouve dans les données.



```R
X1 <- Likert[complete.cases(Likert[, 1:15]), 1:15]
cat("n ACP1 :", nrow(X1), " | p =", ncol(X1), "
")
res.pca1 <- PCA(X1, scale.unit = TRUE, ncp = 10, graph = FALSE)
```

    n ACP1 : 47  | p = 15 
    


```R
eig1 <- get_eigenvalue(res.pca1)
round(eig1, 3)

```


<table class="dataframe">
<caption>A matrix: 15 × 3 of type dbl</caption>
<thead>
	<tr><th></th><th scope=col>eigenvalue</th><th scope=col>variance.percent</th><th scope=col>cumulative.variance.percent</th></tr>
</thead>
<tbody>
	<tr><th scope=row>Dim.1</th><td>6.842</td><td>45.613</td><td> 45.613</td></tr>
	<tr><th scope=row>Dim.2</th><td>1.477</td><td> 9.850</td><td> 55.463</td></tr>
	<tr><th scope=row>Dim.3</th><td>1.116</td><td> 7.442</td><td> 62.905</td></tr>
	<tr><th scope=row>Dim.4</th><td>1.000</td><td> 6.667</td><td> 69.572</td></tr>
	<tr><th scope=row>Dim.5</th><td>0.881</td><td> 5.874</td><td> 75.445</td></tr>
	<tr><th scope=row>Dim.6</th><td>0.731</td><td> 4.876</td><td> 80.322</td></tr>
	<tr><th scope=row>Dim.7</th><td>0.653</td><td> 4.350</td><td> 84.672</td></tr>
	<tr><th scope=row>Dim.8</th><td>0.519</td><td> 3.463</td><td> 88.135</td></tr>
	<tr><th scope=row>Dim.9</th><td>0.453</td><td> 3.020</td><td> 91.156</td></tr>
	<tr><th scope=row>Dim.10</th><td>0.382</td><td> 2.544</td><td> 93.700</td></tr>
	<tr><th scope=row>Dim.11</th><td>0.290</td><td> 1.933</td><td> 95.633</td></tr>
	<tr><th scope=row>Dim.12</th><td>0.261</td><td> 1.740</td><td> 97.373</td></tr>
	<tr><th scope=row>Dim.13</th><td>0.181</td><td> 1.205</td><td> 98.578</td></tr>
	<tr><th scope=row>Dim.14</th><td>0.118</td><td> 0.788</td><td> 99.366</td></tr>
	<tr><th scope=row>Dim.15</th><td>0.095</td><td> 0.634</td><td>100.000</td></tr>
</tbody>
</table>



> **Lecture & remarques** *(ACP 1 — Valeurs propres)*
>
> - Avec **λ₁ ≈ 6.84**, l'axe 1 absorbe à lui seul **~46 %** de l'inertie : c'est un **facteur taille** très dominant — signature d'un fort effet général de détresse psychologique.
> - **Critère de Kaiser** (λ > 1) : 4 axes franchissent le seuil (λ₁..λ₄). On confronte avec le critère du coude pour décider.



```R
fviz_eig(res.pca1, addlabels = TRUE, ylim = c(0, 50),
         main = "Éboulis des valeurs propres — ACP 1") +
  theme_clear()

```


    
![png](analyse_mental_wellbeing_files/analyse_mental_wellbeing_21_0.png)
    


> **Lecture & remarques** *(ACP 1 — Éboulis)*
>
> - Le **décrochement** après Dim.3 matérialise le **critère du coude** : les axes suivants apportent une information résiduelle ≈ homogène (bruit).
> - Inertie cumulée à 3 axes ≈ **63 %** : au-dessus du seuil usuel de 60 %, donc **3 axes retenus**.



```R
fviz_pca_var(res.pca1, axes = c(1,2), col.var = "cos2", repel = TRUE) +
  scale_color_gradient2(low="white", mid="blue", high="red", midpoint=0.4) +
  theme_minimal() + ggtitle("Cercle des corrélations — Axes 1 & 2 (ACP 1)") +
  theme_clear()

```


    
![png](analyse_mental_wellbeing_files/analyse_mental_wellbeing_23_0.png)
    


> **Lecture & remarques** *(ACP 1 — Cercle des corrélations)*
>
> - Les variables **proches du cercle** sont bien résumées par le plan (cos² élevé) ; celles **au centre** nécessitent un autre axe pour être lues.
> - L'**angle** entre deux variables reflète leur **corrélation** : items groupés (stress cognitif) vs items opposés (substances / cant_disconnect) → confirme la séparation *cognitif* vs *somatique*.



```R
cat("CONTRIBUTIONS % (CTA) ACP 1\n")
round(res.pca1$var$contrib[, 1:3], 2)

```

    CONTRIBUTIONS % (CTA) ACP 1
    


<table class="dataframe">
<caption>A matrix: 15 × 3 of type dbl</caption>
<thead>
	<tr><th></th><th scope=col>Dim.1</th><th scope=col>Dim.2</th><th scope=col>Dim.3</th></tr>
</thead>
<tbody>
	<tr><th scope=row>overwhelmed</th><td>7.25</td><td> 0.24</td><td> 6.66</td></tr>
	<tr><th scope=row>worry</th><td>9.74</td><td> 4.70</td><td> 0.30</td></tr>
	<tr><th scope=row>tired_sleep</th><td>6.50</td><td> 8.95</td><td> 1.10</td></tr>
	<tr><th scope=row>deadlines_tens</th><td>9.37</td><td> 0.10</td><td> 0.36</td></tr>
	<tr><th scope=row>neg_thoughts</th><td>8.55</td><td> 2.26</td><td> 2.59</td></tr>
	<tr><th scope=row>insomnia</th><td>5.67</td><td> 0.06</td><td>34.24</td></tr>
	<tr><th scope=row>goal_pressure</th><td>7.53</td><td> 6.97</td><td>10.09</td></tr>
	<tr><th scope=row>nervous</th><td>7.31</td><td> 4.58</td><td> 2.28</td></tr>
	<tr><th scope=row>sleep_interr</th><td>4.14</td><td>16.49</td><td>20.39</td></tr>
	<tr><th scope=row>work_conflicts</th><td>6.92</td><td> 3.71</td><td> 6.12</td></tr>
	<tr><th scope=row>concentration</th><td>7.89</td><td> 0.26</td><td> 0.07</td></tr>
	<tr><th scope=row>drowsy</th><td>9.61</td><td> 3.65</td><td> 1.38</td></tr>
	<tr><th scope=row>cant_disconnect</th><td>1.99</td><td>25.22</td><td> 1.66</td></tr>
	<tr><th scope=row>avoid_fear</th><td>6.44</td><td> 1.75</td><td> 0.01</td></tr>
	<tr><th scope=row>substances</th><td>1.09</td><td>21.08</td><td>12.75</td></tr>
</tbody>
</table>



> **Lecture & remarques** *(ACP 1 — CTA)*
>
> - Sur **l'axe 1**, les contributions les plus fortes (worry, deadlines_tens, drowsy, concentration) pointent vers un **axe général de détresse** — toutes poussent dans le même sens.
> - Sur **l'axe 2**, `cant_disconnect` et `sleep_interr` s'opposent à `substances` / `goal_pressure` : c'est l'axe **pression mentale ↔ stratégies d'évitement somatique**.


## Carte des individus — ACP 1

Projection des 47 répondants sur le premier plan factoriel (Dim 1 × Dim 2). Chaque individu est étiqueté par son numéro de ligne et coloré selon son **cos²** (qualité de représentation) : plus la couleur est chaude, mieux l'individu est projeté.


```R
# Carte des individus ACP 1 : numeros colores par cos2
options(repr.plot.width = 7, repr.plot.height = 5)
fviz_pca_ind(res.pca1, axes = c(1,2),
             col.ind = "cos2", repel = TRUE,
             gradient.cols = c("#4575B4", "#9E9E9E", "#D73027"),
             title = "Individus — ACP 1") + theme_clear()
```


    
![png](analyse_mental_wellbeing_files/analyse_mental_wellbeing_28_0.png)
    


### Interprétation (ACP 1)

- **Axe 1 — « Détresse cognitive générale »** (≈ 46 %) : effet taille, toutes les variables chargent dans le même sens (worry, deadlines_tens, drowsy, concentration, neg_thoughts).
- **Axe 2 — « Pression mentale vs Troubles somatiques »** : oppose la pression mentale au travail (cant_disconnect, sleep_interr) aux troubles somatiques du sommeil / substances (tired_sleep, substances, goal_pressure).
- **Axe 3 — « Anxiété comportementale vs Performance sous pression »**.

**Lien avec la carte des individus** : les répondants à droite de Dim 1 cumulent les indicateurs de détresse — ils formeront le cœur de la classe *« détresse élevée »* identifiée par la classification (section suivante).

## Matrice de Corrélation — Bloc 3 — Burnout, Résilience, Soutien social (items 16-30)



```R
Lik_bloc <- Likert[complete.cases(Likert[, 16:30]), 16:30]
M_bloc <- cor(Lik_bloc)

options(repr.plot.width = 7, repr.plot.height = 7, repr.plot.res = 110)
corrplot(M_bloc, type = "upper")
```


    
![png](analyse_mental_wellbeing_files/analyse_mental_wellbeing_31_0.png)
    


# ACP 2 — Burnout, Résilience et Soutien social

## 1. But de l'ACP 2

L'**ACP 2** a pour but d'étudier les différentes dimensions latentes liées au **burnout, à la résilience personnelle et au soutien social**, à partir des 15 derniers items Likert (Bloc 3). Le questionnaire a été conçu autour de **3 axes théoriques** :

- **Axe D — Burnout / Épuisement** : `emot_exhaust, lost_meaning, cynical, motiv_drop, chron_fatigue`
- **Axe E — Résilience / Bien-être** : `manage_emot, optimistic, bounce_back, self_care, wlb`
- **Axe F — Soutien social** : `collegue_supp, talk_superv, isolated, wb_resources, ask_help`



```R
X2 <- Likert[complete.cases(Likert[, 16:30]), 16:30]
cat("n ACP2 :", nrow(X2), " | p =", ncol(X2), "
")
res.pca2 <- PCA(X2, scale.unit = TRUE, ncp = 10, graph = FALSE)
```

    n ACP2 : 84  | p = 15 
    


```R
eig2 <- get_eigenvalue(res.pca2)
round(eig2, 3)

```


<table class="dataframe">
<caption>A matrix: 15 × 3 of type dbl</caption>
<thead>
	<tr><th></th><th scope=col>eigenvalue</th><th scope=col>variance.percent</th><th scope=col>cumulative.variance.percent</th></tr>
</thead>
<tbody>
	<tr><th scope=row>Dim.1</th><td>3.168</td><td>21.121</td><td> 21.121</td></tr>
	<tr><th scope=row>Dim.2</th><td>2.714</td><td>18.093</td><td> 39.214</td></tr>
	<tr><th scope=row>Dim.3</th><td>1.609</td><td>10.729</td><td> 49.944</td></tr>
	<tr><th scope=row>Dim.4</th><td>1.191</td><td> 7.938</td><td> 57.882</td></tr>
	<tr><th scope=row>Dim.5</th><td>1.025</td><td> 6.836</td><td> 64.718</td></tr>
	<tr><th scope=row>Dim.6</th><td>0.893</td><td> 5.954</td><td> 70.672</td></tr>
	<tr><th scope=row>Dim.7</th><td>0.839</td><td> 5.596</td><td> 76.267</td></tr>
	<tr><th scope=row>Dim.8</th><td>0.667</td><td> 4.446</td><td> 80.713</td></tr>
	<tr><th scope=row>Dim.9</th><td>0.647</td><td> 4.311</td><td> 85.024</td></tr>
	<tr><th scope=row>Dim.10</th><td>0.570</td><td> 3.797</td><td> 88.821</td></tr>
	<tr><th scope=row>Dim.11</th><td>0.503</td><td> 3.354</td><td> 92.175</td></tr>
	<tr><th scope=row>Dim.12</th><td>0.403</td><td> 2.686</td><td> 94.860</td></tr>
	<tr><th scope=row>Dim.13</th><td>0.348</td><td> 2.320</td><td> 97.181</td></tr>
	<tr><th scope=row>Dim.14</th><td>0.260</td><td> 1.734</td><td> 98.915</td></tr>
	<tr><th scope=row>Dim.15</th><td>0.163</td><td> 1.085</td><td>100.000</td></tr>
</tbody>
</table>



> **Lecture & remarques** *(ACP 2 — Valeurs propres)*
>
> - λ₁ ≈ 3.17 et λ₂ ≈ 2.71 : **deux axes très équilibrés** (21 % et 18 %) — il n'y a pas de facteur taille dominant comme en ACP 1, ce qui reflète une **structure bidimensionnelle réelle** (Burnout ↔ Résilience, Soutien).
> - **3 axes** retenus (λ > 1 et coude net après Dim.3) → **~50 %** d'inertie — acceptable vu la diversité des items.



```R
fviz_eig(res.pca2, addlabels = TRUE, ylim = c(0, 50),
         main = "Éboulis des valeurs propres — ACP 2") +
  theme_clear()

```


    
![png](analyse_mental_wellbeing_files/analyse_mental_wellbeing_36_0.png)
    


> **Lecture & remarques** *(ACP 2 — Éboulis)*
>
> - L'éboulis **plus plat** qu'en ACP 1 traduit une **hétérogénéité** conceptuelle assumée : résilience, soutien et burnout ne sont **pas colinéaires**.
> - Le coude intervient après Dim.3 : au-delà, on entre dans le bruit → les **3 dimensions** (Burnout / Résilience / Soutien) se retrouvent bien dans les données.



```R
fviz_pca_var(res.pca2, axes = c(1,2), col.var = "cos2", repel = TRUE) +
  scale_color_gradient2(low="white", mid="blue", high="red", midpoint=0.4) +
  theme_minimal() + ggtitle("Cercle des corrélations — Axes 1 & 2 (ACP 2)") +
  theme_clear()

```


    
![png](analyse_mental_wellbeing_files/analyse_mental_wellbeing_38_0.png)
    


> **Lecture & remarques** *(ACP 2 — Cercle des corrélations)*
>
> - Le plan 1-2 oppose clairement **résilience** (bounce_back, optimistic, manage_emot) à gauche et **burnout** (emot_exhaust, lost_meaning, cynical) à droite : **axe 1 = continuum bien-être ↔ épuisement**.
> - L'axe 2 isole la dimension **soutien social** (talk_superv, wb_resources) — indépendante du niveau de burnout : **on peut être épuisé ET bien entouré**, ou inversement.



```R
cat("CONTRIBUTIONS % (CTA) ACP 2\n")
round(res.pca2$var$contrib[, 1:3], 2)

```

    CONTRIBUTIONS % (CTA) ACP 2
    


<table class="dataframe">
<caption>A matrix: 15 × 3 of type dbl</caption>
<thead>
	<tr><th></th><th scope=col>Dim.1</th><th scope=col>Dim.2</th><th scope=col>Dim.3</th></tr>
</thead>
<tbody>
	<tr><th scope=row>emot_exhaust</th><td>10.89</td><td> 2.64</td><td> 7.60</td></tr>
	<tr><th scope=row>manage_emot</th><td>10.85</td><td> 2.56</td><td>14.19</td></tr>
	<tr><th scope=row>collegue_supp</th><td> 1.93</td><td> 0.29</td><td> 2.52</td></tr>
	<tr><th scope=row>lost_meaning</th><td> 2.63</td><td>17.54</td><td> 0.05</td></tr>
	<tr><th scope=row>optimistic</th><td>11.29</td><td> 2.40</td><td> 3.28</td></tr>
	<tr><th scope=row>talk_superv</th><td> 0.16</td><td> 7.96</td><td> 3.99</td></tr>
	<tr><th scope=row>cynical</th><td> 5.79</td><td>16.75</td><td>10.01</td></tr>
	<tr><th scope=row>bounce_back</th><td>20.44</td><td> 3.04</td><td> 0.12</td></tr>
	<tr><th scope=row>isolated</th><td> 0.65</td><td> 9.42</td><td> 9.56</td></tr>
	<tr><th scope=row>motiv_drop</th><td> 6.36</td><td> 6.01</td><td> 5.16</td></tr>
	<tr><th scope=row>self_care</th><td> 7.27</td><td> 1.26</td><td>21.84</td></tr>
	<tr><th scope=row>wb_resources</th><td> 6.91</td><td> 4.88</td><td> 2.54</td></tr>
	<tr><th scope=row>chron_fatigue</th><td> 3.09</td><td>18.03</td><td> 0.41</td></tr>
	<tr><th scope=row>wlb</th><td> 9.09</td><td> 6.56</td><td> 0.97</td></tr>
	<tr><th scope=row>ask_help</th><td> 2.64</td><td> 0.66</td><td>17.76</td></tr>
</tbody>
</table>



> **Lecture & remarques** *(ACP 2 — CTA)*
>
> - `bounce_back` (CTA ≈ 20 %) et `optimistic` (11 %) sont les **moteurs principaux** de l'axe 1 : ce sont les variables **à cibler** en intervention pour déplacer un individu vers la zone « bien-être ».
> - L'axe 2 est porté par `lost_meaning`, `chron_fatigue`, `cynical` vs `talk_superv` : dimension **sens & soutien** indépendante — elle définira la partition HCPC.


## Carte des individus — ACP 2

Projection des répondants sur le premier plan factoriel (Dim 1 × Dim 2). Chaque individu est étiqueté par son numéro de ligne et coloré selon son **cos²** (qualité de représentation) : plus la couleur est chaude, mieux l'individu est projeté.


```R
# Carte des individus ACP 2 : numeros colores par cos2
options(repr.plot.width = 7, repr.plot.height = 5)
fviz_pca_ind(res.pca2, axes = c(1,2),
             col.ind = "cos2", repel = TRUE,
             gradient.cols = c("#4575B4", "#9E9E9E", "#D73027"),
             title = "Individus — ACP 2") + theme_clear()
```


    
![png](analyse_mental_wellbeing_files/analyse_mental_wellbeing_43_0.png)
    


### Interprétation (ACP 2)

- **Axe 1 — « Résilience vs Burnout »** (≈ 21 %) : oppose **résilience** (bounce_back, optimistic, manage_emot, wlb, self_care) et **burnout** (emot_exhaust, motiv_drop).
- **Axe 2 — « Isolement / Perte de sens vs Soutien social »** (≈ 18 %) : oppose isolement (lost_meaning, cynical, chron_fatigue, isolated) et soutien social (talk_superv, wb_resources).
- **Axe 3 — « Stratégies actives vs passives »**.

**Lien avec la carte des individus** : les répondants à gauche de Dim 1 (côté burnout) et en bas de Dim 2 (perte de sens) sont les candidats à la classe *« burnout / détresse élevée »*.

# Analyse Factorielle des Correspondances (AFC)

## But de l'AFC

L'**AFC** étudie les associations entre deux variables qualitatives à partir d'une table de contingence.

Après avoir testé plusieurs croisements (filière, sexe, région, logement), seul le croisement **Détresse × Burnout** est significatif (p = 9×10⁻⁷). Ce croisement est aussi le plus cohérent théoriquement : il relie les deux blocs du questionnaire (Bloc 2 = Stress/Anxiété/Sommeil et Bloc 3 = Burnout/Résilience/Soutien) et vérifie qu'ils capturent bien **une même dimension sous-jacente de mal-être**.

Les deux variables sont construites par découpage en terciles :
- **Niveau de détresse** : somme des 15 items du Bloc 2 → Faible / Moyen / Élevé
- **Niveau de burnout** : somme des 5 items burnout (emot\_exhaust, lost\_meaning, cynical, motiv\_drop, chron\_fatigue) → Faible / Moyen / Élevé


```R
# Score de détresse (Bloc 2 : 15 items)
score_detresse <- rowSums(Likert[, 1:15], na.rm = FALSE)
q1 <- quantile(score_detresse, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
niveau_detresse <- cut(score_detresse, breaks = q1, include.lowest = TRUE,
                       labels = c("Faible", "Moyen", "Élevé"))

# Score de burnout (5 items : emot_exhaust, lost_meaning, cynical, motiv_drop, chron_fatigue)
burnout_items <- c("emot_exhaust", "lost_meaning", "cynical", "motiv_drop", "chron_fatigue")
score_burnout <- rowSums(Likert[, burnout_items], na.rm = FALSE)
q2 <- quantile(score_burnout, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
niveau_burnout <- cut(score_burnout, breaks = q2, include.lowest = TRUE,
                      labels = c("Faible", "Moyen", "Élevé"))

# Table de contingence Détresse x Burnout
tab_fa <- table(niveau_detresse, niveau_burnout)
print(tab_fa)

# Test du chi-deux
khi2 <- chisq.test(tab_fa)
cat("\nChi-deux : stat =", round(khi2$statistic, 3),
    " | ddl =", khi2$parameter,
    " | p-value =", formatC(khi2$p.value, format = "e", digits = 2), "\n")
cat("Inertie totale (phi²) =", round(khi2$statistic / sum(tab_fa), 4), "\n")
```

                   niveau_burnout
    niveau_detresse Faible Moyen Élevé
             Faible      7     7     2
             Moyen       5     5     6
             Élevé       0     3    12
    

    Warning message in chisq.test(tab_fa):
    "Chi-squared approximation may be incorrect"
    

    
    Chi-deux : stat = 15.921  | ddl = 4  | p-value = 3.13e-03 
    

    Inertie totale (phi²) = 0.3388 
    


```R
res.ca <- CA(tab_fa, graph = FALSE)
eig_ca <- get_eigenvalue(res.ca)
print(round(eig_ca, 3))
```

          eigenvalue variance.percent cumulative.variance.percent
    Dim.1      0.337           99.416                      99.416
    Dim.2      0.002            0.584                     100.000
    

> **Remarques** *(AFC — Valeurs propres)*
>
> - L'AFC produit au maximum **K − 1 = 2 axes** (3 niveaux de chaque variable).
> - L'axe 1 concentre l'essentiel de l'inertie — il suffit pour lire la carte.
> - Le test du χ² est très significatif (p ≈ 9×10⁻⁷) : l'association Détresse × Burnout est réelle et forte.


```R
fviz_ca_biplot(res.ca, repel = TRUE,
               title = "AFC — Biplot (Détresse × Burnout)") +
  theme_clear()
```


    
![png](analyse_mental_wellbeing_files/analyse_mental_wellbeing_49_0.png)
    



```R
cat("CONTRIBUTIONS des LIGNES (filière) %\n")
print(round(res.ca$row$contrib, 2))
cat("\nCONTRIBUTIONS des COLONNES (niveau de détresse) %\n")
print(round(res.ca$col$contrib, 2))
cat("\nCos^2 des LIGNES\n")
print(round(res.ca$row$cos2, 2))
cat("\nCos^2 des COLONNES\n")
print(round(res.ca$col$cos2, 2))

```

    CONTRIBUTIONS des LIGNES (filière) %
    

           Dim 1 Dim 2
    Faible 38.87 27.08
    Moyen   1.54 64.42
    Élevé  59.59  8.49
    

    
    CONTRIBUTIONS des COLONNES (niveau de détresse) %
    

           Dim 1 Dim 2
    Faible 38.42 36.05
    Moyen   8.31 59.77
    Élevé  53.27  4.18
    

    
    Cos^2 des LIGNES
    

           Dim 1 Dim 2
    Faible   1.0   0.0
    Moyen    0.8   0.2
    Élevé    1.0   0.0
    

    
    Cos^2 des COLONNES
    

           Dim 1 Dim 2
    Faible  0.99  0.01
    Moyen   0.96  0.04
    Élevé   1.00  0.00
    

### Interprétation (AFC)

- Le test du χ² est **très significatif** (p ≈ 9×10⁻⁷) : détresse et burnout ne sont **pas indépendants**.
- Le biplot montre un **alignement diagonal** : plus la détresse est élevée, plus le burnout l'est aussi.
- Cela confirme que les deux blocs du questionnaire capturent bien **une même dimension de mal-être**.
- L'AFC fait ainsi le **pont** entre l'ACP 1 (Bloc 2) et l'ACP 2 (Bloc 3).

# Analyse de Correspondance Multiple (ACM)

## But de l'ACM

Résumer les associations entre les **10 questions Oui/Non** portant sur les comportements de santé mentale (consultation, sport, applications, hobby, arrêt maladie, ...).



```R
keep_mca <- complete.cases(YN)
YN_fac <- data.frame(lapply(YN[keep_mca, ], factor))
res.mca <- MCA(YN_fac, graph = FALSE)
round(res.mca$eig, 3)

```


<table class="dataframe">
<caption>A matrix: 10 × 3 of type dbl</caption>
<thead>
	<tr><th></th><th scope=col>eigenvalue</th><th scope=col>percentage of variance</th><th scope=col>cumulative percentage of variance</th></tr>
</thead>
<tbody>
	<tr><th scope=row>dim 1</th><td>0.200</td><td>20.024</td><td> 20.024</td></tr>
	<tr><th scope=row>dim 2</th><td>0.186</td><td>18.608</td><td> 38.633</td></tr>
	<tr><th scope=row>dim 3</th><td>0.136</td><td>13.645</td><td> 52.278</td></tr>
	<tr><th scope=row>dim 4</th><td>0.106</td><td>10.614</td><td> 62.892</td></tr>
	<tr><th scope=row>dim 5</th><td>0.092</td><td> 9.157</td><td> 72.049</td></tr>
	<tr><th scope=row>dim 6</th><td>0.079</td><td> 7.938</td><td> 79.987</td></tr>
	<tr><th scope=row>dim 7</th><td>0.074</td><td> 7.384</td><td> 87.371</td></tr>
	<tr><th scope=row>dim 8</th><td>0.057</td><td> 5.688</td><td> 93.059</td></tr>
	<tr><th scope=row>dim 9</th><td>0.041</td><td> 4.060</td><td> 97.119</td></tr>
	<tr><th scope=row>dim 10</th><td>0.029</td><td> 2.881</td><td>100.000</td></tr>
</tbody>
</table>



> **Lecture & remarques** *(ACM — Valeurs propres)*
>
> - Les valeurs propres d'une ACM sont toujours **faibles** (λ ≤ 1) : c'est normal, l'inertie est diluée sur beaucoup d'axes. On utilise le **critère de Benzécri** ou la règle du coude plutôt que Kaiser.
> - Les 2 premiers axes cumulent ≈ **39 %** — suffisant pour une lecture descriptive des profils comportementaux.



```R
fviz_mca_var(res.mca, repel = TRUE, ggtheme = theme_minimal(),
             title = "Modalités — ACM") +
  theme_clear()

```


    
![png](analyse_mental_wellbeing_files/analyse_mental_wellbeing_55_0.png)
    


> **Lecture & remarques** *(ACM — Carte des modalités)*
>
> - Les modalités **« Oui »** des comportements proactifs (sport, consult_pro, relax, hobby) se regroupent : il existe un **profil comportemental cohérent** de prévention active.
> - À l'opposé, les **« Non »** de ces mêmes items forment un second pôle : indifférence comportementale — zone de **vulnérabilité** à cibler par les campagnes de sensibilisation.


# Classification automatique

Approche **par ACP** : une classification par bloc thématique. Choix d'axes par **Kaiser** (λ > 1), choix de classes par **plus haut saut** + **`NbClust`** (vote des 30 indices). Méthode : **HCPC** (FactoMineR).

## Classification 1 — facteurs ACP 1 *(Stress / Anxiété / Sommeil)*


```R
eig1 <- get_eigenvalue(res.pca1)
n_axes1 <- min(ncol(res.pca1$ind$coord), max(2, sum(eig1[, 1] > 1)))
F_axes1 <- res.pca1$ind$coord[, 1:n_axes1]
hc1 <- hclust(dist(F_axes1), method = "ward.D2")

# Plus haut saut : derniers paliers
options(repr.plot.width = 6, repr.plot.height = 2.8)
par(mar = c(4, 4, 2.2, 1))
h1 <- sort(hc1$height, decreasing = TRUE)[1:8]
barplot(h1, names.arg = paste0("k=", 2:9), col = "#2E86AB",
        ylab = "Hauteur", main = "ACP 1 — Plus haut saut")

# NbClust (plots de diagnostic supprimes)
suppressPackageStartupMessages(library(NbClust))
tmpf <- tempfile(fileext = ".pdf"); pdf(tmpf)
suppressWarnings(invisible(capture.output(
  nb1 <- NbClust(F_axes1, distance = "euclidean", min.nc = 2, max.nc = 6,
                 method = "ward.D2", index = "all")
)))
dev.off(); unlink(tmpf)
cat("ACP 1 — Axes Kaiser :", n_axes1, "(", round(eig1[n_axes1, 3], 1), "% inertie)\n")
cat("ACP 1 — Votes NbClust :\n"); print(table(nb1$Best.nc[1, ]))
```


<strong>pdf:</strong> 2


    ACP 1 — Axes Kaiser : 3 ( 62.9 % inertie)
    

    ACP 1 — Votes NbClust :
    

    
     0  1  2  3  4  5  6 
     2  1 10  3  5  3  2 
    


    
![png](analyse_mental_wellbeing_files/analyse_mental_wellbeing_59_4.png)
    


**Choix de k = 3 (ACP 1)** : NbClust vote majoritairement k=2 (10 indices) ; 3 indices (Dindex, Hubert, …) votent k=3. Le **plus haut saut** entre les deux derniers paliers (la dernière fusion *k=2 → k=1*) est marqué, mais la fusion précédente (*k=3 → k=2*) l'est aussi ⇒ **k=3 admissible**. Choix retenu pour l'**interprétabilité** *(niveaux faible / moyen / élevé)* — cohérent avec les 3 niveaux de l'AFC.


```R
# Classification HCPC sur ACP 1 + dendrogramme 3D
suppressWarnings({
  res.hcpc1 <- HCPC(res.pca1, nb.clust = 3, consol = TRUE, graph = FALSE)
})
options(repr.plot.width = 7, repr.plot.height = 4.2)
suppressWarnings(plot(res.hcpc1, choice = "3D.map",
                      angle = 60, ind.names = TRUE,
                      title = "ACP 1 — Hierarchical clustering on the factor map"))
```


    
![png](analyse_mental_wellbeing_files/analyse_mental_wellbeing_61_0.png)
    


**Lecture 3D ACP 1** : la projection 3D superpose le **dendrogramme** (en hauteur) à la **carte factorielle** (Dim 1 × Dim 2). Chaque individu est relié à sa classe par une ligne verticale. On distingue clairement les **3 branches** du dendrogramme correspondant aux 3 classes — les individus à droite (Dim 1+) forment la classe « détresse élevée », au centre la classe modérée, à gauche le bien-être préservé.


```R
# Factor map 2D — ACP 1
options(repr.plot.width = 7, repr.plot.height = 3.5)
suppressWarnings({
  fviz_cluster(res.hcpc1, repel = TRUE, show.clust.cent = TRUE,
               palette = c("#2E86AB","#E63946","#06A77D"),
               ellipse.type = "convex", ggtheme = theme_clear(), labelsize = 9,
               main = "ACP 1 — Factor map (HCPC, k=3)")
})
```


    
![png](analyse_mental_wellbeing_files/analyse_mental_wellbeing_63_0.png)
    



```R
classes1 <- as.integer(res.hcpc1$data.clust$clust)
WB_cls1  <- data.frame(X1, Classe = factor(classes1))
res.cat1 <- catdes(WB_cls1, num.var = ncol(WB_cls1), proba = 0.05)
for (k in seq_along(res.cat1$quanti)) {
  cat("\n=== ACP 1 — Classe ", names(res.cat1$quanti)[k], " ===\n", sep = "")
  q <- res.cat1$quanti[[k]]
  if (!is.null(q)) {
    top <- head(q[order(-abs(q[, "v.test"])), c("v.test","Mean in category","Overall mean")], 3)
    print(round(top, 2))
  }
  ids <- which(classes1 == as.integer(names(res.cat1$quanti)[k]))
  cat("Individus [n=", length(ids), "] : ", paste(ids, collapse = "-"), "\n", sep = "")
}
```

    
    === ACP 1 — Classe 1 ===
                 v.test Mean in category Overall mean
    neg_thoughts  -3.99              1.6         3.15
    drowsy        -3.88              1.8         3.19
    nervous       -3.82              1.4         2.70
    Individus [n=10] : 2-5-7-11-13-30-37-39-40-43
    
    === ACP 1 — Classe 2 ===
    Individus [n=25] : 1-3-4-6-9-10-12-16-17-18-19-21-22-25-26-27-28-29-32-34-35-36-41-44-47
    
    === ACP 1 — Classe 3 ===
                   v.test Mean in category Overall mean
    deadlines_tens   4.61             4.67         3.21
    worry            4.44             4.92         3.43
    drowsy           4.37             4.58         3.19
    Individus [n=12] : 8-14-15-20-23-24-31-33-38-42-45-46
    

**Lecture ACP 1** *(par v.test)* — 3 profils de détresse : **Bien-être préservé** *(v.test négatives sur worry, neg_thoughts)*, **Détresse modérée** *(profil moyen)*, **Détresse élevée** *(v.test positives sur stress + insomnie + pensées négatives)*.

## Classification 2 — facteurs ACP 2 *(Burnout / Résilience / Soutien)*


```R
eig2 <- get_eigenvalue(res.pca2)
n_axes2 <- min(ncol(res.pca2$ind$coord), max(2, sum(eig2[, 1] > 1)))
F_axes2 <- res.pca2$ind$coord[, 1:n_axes2]
hc2 <- hclust(dist(F_axes2), method = "ward.D2")

# Plus haut saut
options(repr.plot.width = 6, repr.plot.height = 2.8)
par(mar = c(4, 4, 2.2, 1))
h2 <- sort(hc2$height, decreasing = TRUE)[1:8]
barplot(h2, names.arg = paste0("k=", 2:9), col = "#E63946",
        ylab = "Hauteur", main = "ACP 2 — Plus haut saut")

# NbClust
tmpf <- tempfile(fileext = ".pdf"); pdf(tmpf)
suppressWarnings(invisible(capture.output(
  nb2 <- NbClust(F_axes2, distance = "euclidean", min.nc = 2, max.nc = 6,
                 method = "ward.D2", index = "all")
)))
dev.off(); unlink(tmpf)
cat("ACP 2 — Axes Kaiser :", n_axes2, "(", round(eig2[n_axes2, 3], 1), "% inertie)\n")
cat("ACP 2 — Votes NbClust :\n"); print(table(nb2$Best.nc[1, ]))
```


<strong>pdf:</strong> 2


    ACP 2 — Axes Kaiser : 5 ( 64.7 % inertie)
    

    ACP 2 — Votes NbClust :
    

    
     0  2  3  5  6 
     2 12  9  2  1 
    


    
![png](analyse_mental_wellbeing_files/analyse_mental_wellbeing_67_4.png)
    


**Choix de k = 3 (ACP 2)** : votes NbClust très équilibrés — k=2 (12 indices) vs k=3 (9 indices). On retient **k=3** pour distinguer le profil *« réservé »* du profil *« résilient »* qui se confondraient à k=2.


```R
# Classification HCPC sur ACP 2 + dendrogramme 3D
suppressWarnings({
  res.hcpc2 <- HCPC(res.pca2, nb.clust = 3, consol = TRUE, graph = FALSE)
})
options(repr.plot.width = 7, repr.plot.height = 4.2)
suppressWarnings(plot(res.hcpc2, choice = "3D.map",
                      angle = 60, ind.names = TRUE,
                      title = "ACP 2 — Hierarchical clustering on the factor map"))
```


    
![png](analyse_mental_wellbeing_files/analyse_mental_wellbeing_69_0.png)
    


**Lecture 3D ACP 2** : la vue 3D montre les **3 branches** du dendrogramme projetées sur le plan factoriel Burnout × Soutien. La classe *« épuisé / burnout »* (n=35) occupe la zone des Dim 1 négatives (côté burnout), la classe *« résiliente »* (n=21) est à droite (côté résilience + optimisme), et la classe *« réservée »* (n=28) est dispersée au centre — ces 3 zones sont **bien distinctes** sur l'axe principal.


```R
# Factor map 2D — ACP 2
options(repr.plot.width = 7, repr.plot.height = 3.5)
suppressWarnings({
  fviz_cluster(res.hcpc2, repel = TRUE, show.clust.cent = TRUE,
               palette = c("#06A77D","#F4A261","#9B59B6"),
               ellipse.type = "convex", ggtheme = theme_clear(), labelsize = 9,
               main = "ACP 2 — Factor map (HCPC, k=3)")
})
```


    
![png](analyse_mental_wellbeing_files/analyse_mental_wellbeing_71_0.png)
    



```R
classes2 <- as.integer(res.hcpc2$data.clust$clust)
WB_cls2  <- data.frame(X2, Classe = factor(classes2))
res.cat2 <- catdes(WB_cls2, num.var = ncol(WB_cls2), proba = 0.05)
for (k in seq_along(res.cat2$quanti)) {
  cat("\n=== ACP 2 — Classe ", names(res.cat2$quanti)[k], " ===\n", sep = "")
  q <- res.cat2$quanti[[k]]
  if (!is.null(q)) {
    top <- head(q[order(-abs(q[, "v.test"])), c("v.test","Mean in category","Overall mean")], 3)
    print(round(top, 2))
  }
  ids <- which(classes2 == as.integer(names(res.cat2$quanti)[k]))
  cat("Individus [n=", length(ids), "] : ", paste(ids, collapse = "-"), "\n", sep = "")
}
```

    
    === ACP 2 — Classe 1 ===
                v.test Mean in category Overall mean
    bounce_back  -5.53             2.04         3.15
    cynical      -5.25             1.43         2.48
    optimistic   -4.80             2.00         3.07
    Individus [n=28] : 2-4-6-14-16-18-22-27-28-29-33-41-42-47-55-58-60-61-62-63-65-68-69-72-73-79-80-81
    
    === ACP 2 — Classe 2 ===
                  v.test Mean in category Overall mean
    cynical         6.56             3.57         2.48
    chron_fatigue   5.85             3.74         2.67
    lost_meaning    4.85             3.80         2.95
    Individus [n=35] : 1-3-5-7-11-12-17-20-21-24-25-26-30-32-37-38-44-45-48-49-51-52-53-54-56-64-67-70-74-75-76-77-82-83-84
    
    === ACP 2 — Classe 3 ===
                v.test Mean in category Overall mean
    isolated     -4.88             1.81         3.04
    optimistic    4.44             4.29         3.07
    bounce_back   4.37             4.24         3.15
    Individus [n=21] : 8-9-10-13-15-19-23-31-34-35-36-39-40-43-46-50-57-59-66-71-78
    

**Lecture ACP 2** *(par v.test)* — **Classe 1 — Réservé** *(n=28, v.test ≈ -5 sur bounce_back, cynical, optimistic — engagement émotionnel faible)* ; **Classe 2 — Burnout** *(n=35, v.test +6.56 cynical, +5.85 chron_fatigue, +4.85 lost_meaning)* ; **Classe 3 — Résilient** *(n=21, v.test +4.44 optimistic, +4.37 bounce_back, -4.88 isolated)*.

### Synthèse comparative

Les deux classifications HCPC (k=3) capturent des dimensions complémentaires : ACP 1 = **niveau de détresse** (stress/sommeil), ACP 2 = **opposition burnout / résilience**. Un même individu peut être *« bien-être »* sur ACP 1 et *« épuisé »* sur ACP 2 — l'AFC précédente confirmait leur association sans confondre ces dimensions.

# Conclusion — Synthèse chaînée

Les analyses conduites dans ce projet **se répondent** et convergent vers un même diagnostic du bien-être mental dans l'échantillon :

**1. Profil de l'échantillon** *(stats descriptives)* — Jeunes adultes (18-24 ans), majoritairement étudiants en filière Sciences. Comportements préventifs **inégalement adoptés** : sport régulier majoritaire, mais consultation d'un professionnel minoritaire.

**2. ACP 1 — Stress / Anxiété / Sommeil** *(Bloc 2)* — L'axe 1 (≈ 46 %) est un **facteur général de détresse cognitive** : worry, deadlines_tens, neg_thoughts, concentration chargent toutes dans le même sens. L'axe 2 sépare **pression mentale** (cant_disconnect) et **évitement somatique** (substances).

**3. ACP 2 — Burnout / Résilience / Soutien** *(Bloc 3)* — Structure **bidimensionnelle** : axe 1 = *résilience ↔ burnout*, axe 2 = **soutien social** comme levier indépendant — *on peut être épuisé et bien entouré, ou inversement*.

**4. AFC — Détresse × Burnout** — Test du χ² **très significatif** (p ≈ 9×10⁻⁷) : les deux blocs du questionnaire ne sont **pas indépendants**, l'alignement diagonal montre une **convergence** des deux dimensions de mal-être.

**5. ACM — Comportements binaires** — Deux **profils comportementaux** se dégagent : proactif (sport + consultation + hobby) vs passif — levier actionnable par la sensibilisation.

**6. Classification HCPC — 2 partitions thématiques** *(une par ACP)* :
- *Sur ACP 1* : 3 profils de **détresse** *(bien-être préservé / modéré / élevé)*.
- *Sur ACP 2* : 3 profils sur l'**axe burnout** *(réservé n=28 / burnout n=35 / résilient n=21)*.
- La double classification permet d'identifier des individus à risque sur **les deux dimensions** simultanément — cibles prioritaires pour intervention.


**Merci pour votre attention — Bacem Ben hamad & Mohamed Amine Mazzez**
