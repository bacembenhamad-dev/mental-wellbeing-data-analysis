# Analyse des Facteurs liés au Bien-Être Mental

Projet académique du cours **Analyse des Données** (ESSAI — École Supérieure de la Statistique et de l'Analyse de l'Information, Tunis), appliquant l'**ACP**, l'**AFC** et l'**ACM** à une enquête originale sur le bien-être mental en milieu étudiant et professionnel.

**Auteurs :** Bacem Ben Ahmad & Mohamed Amine Mazzez
**Enseignant :** Ghazi Bel Mufti

---

## Contexte

Le stress et le burnout touchent une large part des étudiants et jeunes professionnels :

| Statistique | Valeur | Source |
|---|---|---|
| Étudiants déclarant un stress élevé | 1 sur 3 | Enquêtes universitaires |
| Cas de burn-out par an (France) | 2,5 millions | INRS |
| Cas évitables avec prévention précoce | 70 % | OMS |
| Délai moyen avant consultation | 8 à 10 ans | Santé Publique France |

Pour étudier ce phénomène, un questionnaire trilingue (Français / English / Tounsi) a été conçu et diffusé via Google Forms, mesurant **six dimensions du bien-être mental** sur **84 répondants**.

## Le questionnaire

30 items Likert répartis en 2 blocs de 3 axes théoriques chacun :

| Bloc | Axe | Items |
|---|---|---|
| **Bloc 2** — Stress, Anxiété, Sommeil | A. Stress travail/études | 1, 4, 7, 10, 13 |
| | B. Anxiété générale | 2, 5, 8, 11, 14 |
| | C. Sommeil & habitudes | 3, 6, 9, 12, 15 |
| **Bloc 3** — Burnout, Résilience, Soutien | D. Burnout / Épuisement | 16, 19, 22, 25, 28 |
| | E. Résilience / Bien-être | 17, 20, 23, 26, 29 |
| | F. Soutien social | 18, 21, 24, 27, 30 |

Plus un profil sociodémographique détaillé et 10 variables comportementales Oui/Non (sport, suivi d'un professionnel, etc.).

## Méthodes appliquées

- **Statistiques descriptives** — profil des répondants, tableaux de bord comportementaux
- **ACP normée** ×2 — une par bloc (Stress/Anxiété/Sommeil, puis Burnout/Résilience/Soutien)
- **AFC** — association entre score de détresse et score de burnout (table de contingence + test du χ²)
- **ACM** — structure des variables comportementales binaires
- **Classification (HCPC)** — profils à risque combinant les deux dimensions

## Résultats clés

1. **ACP 1** : un facteur général de détresse cognitive domine (~46 % de variance) — *worry*, *deadlines_tens*, *neg_thoughts*, *concentration* chargent dans le même sens.
2. **ACP 2** : structure bidimensionnelle — axe résilience ↔ burnout, et soutien social comme levier *indépendant* (on peut être épuisé et bien entouré, ou l'inverse).
3. **AFC** : détresse et burnout sont **significativement liés** (χ², p ≈ 9×10⁻⁷) — convergence des deux dimensions de mal-être.
4. **ACM** : deux profils comportementaux se dégagent — proactif (sport + consultation + hobby) vs passif.
5. **Classification** : 3 profils de détresse (préservé / modéré / élevé) et 3 profils de burnout (réservé n=28 / burnout n=35 / résilient n=21), permettant d'identifier les individus à risque sur les deux axes simultanément.

## Structure du dépôt

```
.
├── survey/
│   └── mental_wellbeing_survey.csv       # réponses brutes du formulaire (84 × 177)
├── notebooks/
│   ├── analyse_mental_wellbeing.Rmd      # analyse R Markdown (source)
│   └── analyse_mental_wellbeing.ipynb    # version Jupyter avec sorties/figures
├── report/
│   ├── analyse_mental_wellbeing.pdf      # rapport final (recommandé pour lecture)
│   ├── analyse_mental_wellbeing.html     # rapport interactif avec graphiques
│   ├── analyse_mental_wellbeing.md       # export markdown
│   └── analyse_mental_wellbeing.tex      # source LaTeX
├── docs/
│   ├── guide_bienetre_mental.html        # guide explicatif de l'enquête
│   └── guide_google_forms.html           # guide de diffusion du formulaire
└── exercises/
    └── decathlon_pca.R                   # TP ACP (FactoMineR, jeu de données decathlon)
```

> 📄 Pour une lecture rapide, consultez directement [`report/analyse_mental_wellbeing.pdf`](report/analyse_mental_wellbeing.pdf) ou [`report/analyse_mental_wellbeing.html`](report/analyse_mental_wellbeing.html) (figures incluses).

## Reproduire l'analyse

L'analyse est écrite en **R**. Packages requis :

```r
install.packages(c("dplyr", "FactoMineR", "factoextra", "corrplot"))
```

Puis ouvrir [`notebooks/analyse_mental_wellbeing.Rmd`](notebooks/analyse_mental_wellbeing.Rmd) dans RStudio et "Knit", ou exécuter le notebook [`notebooks/analyse_mental_wellbeing.ipynb`](notebooks/analyse_mental_wellbeing.ipynb) avec un noyau R Jupyter (IRkernel).

## Lien avec *Mental Well-Being Check*

Ce travail académique alimente un projet applicatif parallèle, **Mental Well-Being Check** (équipe *The DataFathers*, SUP'COM — Hackathon Vectors in Orbit 2025) : les six dimensions du questionnaire correspondent aux profils de stress de la plateforme, et les coordonnées factorielles issues de l'ACP servent de signature numérique pour le matching de profils similaires (Qdrant, cosine similarity).

## Licence

Travail académique réalisé dans le cadre du cours *Analyse des Données* — ESSAI. Les données du questionnaire sont anonymisées.
