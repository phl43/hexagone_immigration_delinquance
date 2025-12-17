library(tidyverse)
library(readxl)
library(RColorBrewer)
library(ggrepel)
library(furrr)

#########################################################################################################################################################
########################################### GRAPHIQUES SUR LES MIS EN CAUSE ET LES PERSONNES SOUS ÉCROU EN FRANCE #######################################
#########################################################################################################################################################

# source : https://www.interieur.gouv.fr/Interstats/Actualites/Insecurite-et-delinquance-en-2024-bilan-statistique-et-atlas-departemental
mis_en_cause_france <- read_excel("mis_en_cause_france.xlsx") %>%
  pivot_longer(
    cols = -pays,
    names_to  = "crime",
    values_to = "mis_en_cause"
  ) %>%
  filter(!(crime %in% c("Usage de stupéfiants")))

# source : https://www.justice.gouv.fr/documentation/etudes-et-statistiques/series-statistiques-personnes-placees-main-justice-1 (données_allemagne pour 2024)
personnes_écrouées_france <- tibble(
  pays = c("France", "UE et Royaume-Uni", "Europe hors UE et Royaume-Uni", "Algérie", "Maroc et Tunisie", "Autres pays d'Afrique", "Amérique", "Asie et Océanie"),
  personnes_sous_écrou = c(71298, 3213, 2431, 4229, 2226 + 1442, 3852, 1751, 1151)
)

# source : https://www.insee.fr/fr/statistiques/2381750 et https://www.insee.fr/fr/statistiques/8327319
population_france <- tibble(
  pays = c("France", "Afrique", "Amérique et Océanie", "Amérique", "Asie", "Asie et Océanie", "Europe hors UE", "Europe hors UE et Royaume-Uni", "UE", "UE et Royaume-Uni", "Algérie", "Maroc et Tunisie", "Autres pays d'Afrique"),
  N = c(68606 - 6028, 2764, 337, 171 + 9 + 150, 802, 802 + 7, 2126 - 1006 - 570, 2126 - 1006 - 570 - 130, 1006 + 570, 1006 + 570 + 130, 642, 848, 1275)
)

données_mis_en_cause_france <- inner_join(mis_en_cause_france, population_france, by = "pays") %>%
  mutate(taux = mis_en_cause / N * 100)

données_personnes_écrouées_france <- inner_join(personnes_écrouées_france, population_france, by = "pays") %>%
  mutate(taux = personnes_sous_écrou / N * 100)

ggplot(données_mis_en_cause_france, mapping = aes(x = pays, y = taux, fill = pays)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = round(taux, 1)),
            position = position_stack(vjust = 0.5),
            color = "black",
            size = 2.5) +
  scale_fill_brewer(palette = "Set3") +
  facet_wrap(~ crime, scales = "free_y") +
  theme_bw() +
  labs(
    title = stringr::str_wrap("Nombre de mis en cause pour 100 000 habitants en France par nationalité en 2024", width = 90),
    x = "Pays/Région",
    y = "Nombre de mis en cause pour 100 000 habitants",
    caption = "Source : Ministère de l'Intérieur/SSMSI et INSEE - Graphique de Philippe Lemoine (@phl43) pour Hexagone (@Hexagone_org)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 24),
    axis.title = element_text(face = "bold", size = 16),
    plot.caption = element_text(size = 12),
    legend.position = "none"
  )

ggsave(
  "Nombre de mis en cause pour 100 000 habitants en France par nationalité en 2024.png",
  width = 24,
  height = 20
)

ggplot(données_personnes_écrouées_france, mapping = aes(x = pays, y = taux)) +
  geom_bar(stat = "identity", color = "black", fill = "steelblue") +
  geom_text(aes(label = round(taux)),
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 6) +
  theme_bw() +
  labs(
    title = stringr::str_wrap("Nombre de personnes écrouées pour 1 000 habitants en France par nationalité en 2024", width = 90),
    x = "Pays/Région",
    y = "Nombre de personnes écrouées pour 1 000 habitants",
    caption = "Source : Ministère de la Justice/Direction de l'administration pénitentiaire et INSEE - Graphique de Philippe Lemoine (@phl43) pour Hexagone (@Hexagone_org)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 24),
    axis.title = element_text(face = "bold", size = 16),
    plot.caption = element_text(size = 12)
  )

ggsave(
  "Nombre de personnes écrouées pour 1 000 habitants en France par nationalité en 2024.png",
  width = 24,
  height = 12
)

#########################################################################################################################################################
################################################### GRAPHIQUES SUR LES SUSPECTS EN ALLEMAGNE ############################################################
#########################################################################################################################################################

# source : https://www.bka.de/DE/AktuelleInformationen/StatistikenLagebilder/PolizeilicheKriminalstatistik/PKS2024/PKSTabellen/BundTVNationalitaet/bundTVNationalitaet.html?nn=240862 (tableau T62)
suspects_allemagne <- read_excel(
  "suspects_allemagne.xlsx",
  na = "NA"
) %>%
  pivot_longer(
    cols = -crime,
    names_to  = "pays",
    values_to = "suspects_allemagne"
  ) %>%
  filter(
    !(pays %in% c("Nombre total de suspects", "Nombre total de non-Allemands", "Non résolu")),
    crime %in% c("Total des infractions pénales, à l'exclusion des violations de la loi sur le séjour des étrangers, de la loi sur l'asile et de la loi sur la liberté de circulation/UE (code 725000)", "Crimes violents")
  ) %>%
  mutate(
    crime = ifelse(
      crime == "Total des infractions pénales, à l'exclusion des violations de la loi sur le séjour des étrangers, de la loi sur l'asile et de la loi sur la liberté de circulation/UE (code 725000)",
      "Total des infractions à l'exclusion des violations de la loi sur le séjour des étrangers",
      crime
    )
  )

top_pays <- suspects_allemagne %>%
  filter(crime == "Total des infractions à l'exclusion des violations de la loi sur le séjour des étrangers") %>%
  slice_max(order_by = suspects_allemagne, n = 50, with_ties = FALSE) %>%
  pull(pays)

# source : https://www-genesis.destatis.de/datenbank/online/statistic/12521/details (tableau 12521-0003 pour 31/12/2023)
population_étrangers_allemagne <- read_excel(
  "population_étrangers_allemagne.xlsx",
  col_types = c("numeric", "text", "text", "numeric"),
  na = "-"
) %>%
  group_by(pays) %>%
  summarize(
    population = sum(N, na.rm = TRUE),
    hommes_15_45 = sum(
      N[sexe == "homme" & dplyr::between(âge, 15, 45)],
      na.rm = TRUE
    ),
    hommes_21_25 = sum(
      N[sexe == "homme" & dplyr::between(âge, 21, 25)],
      na.rm = TRUE
    ),
    part_hommes_15_45 = hommes_15_45 / population
  ) %>%
  select(pays, population, hommes_15_45, hommes_21_25, part_hommes_15_45) %>%
  add_row(
    pays = "Étrangers",
    population = sum(.$population, na.rm = TRUE),
    hommes_15_45 = sum(.$hommes_15_45, na.rm = TRUE),
    hommes_21_25 = sum(.$hommes_21_25, na.rm = TRUE),
    part_hommes_15_45 = hommes_15_45 / population
  )

# source : https://www-genesis.destatis.de/datenbank/online/statistic/12411/details (tableau 12411-0007 pour 31/12/2023)
population_allemands_allemagne <- read_excel(
  "population_allemands_allemagne.xlsx",
  col_types = c("text", "numeric", "numeric"),
  na = "-"
) %>%
  summarize(
    population = sum(N, na.rm = TRUE),
    hommes_15_45 = sum(
      N[sexe == "homme" & dplyr::between(âge, 15, 45)],
      na.rm = TRUE
    ),
    hommes_21_25 = sum(
      N[sexe == "homme" & dplyr::between(âge, 21, 25)],
      na.rm = TRUE
    ),
    part_hommes_15_45 = hommes_15_45 / population
  ) %>%
  mutate(pays = "Allemagne") %>%
  select(pays, population, hommes_15_45, hommes_21_25, part_hommes_15_45)

population_allemagne <- rbind(population_étrangers_allemagne, population_allemands_allemagne)

données_allemagne <- inner_join(suspects_allemagne, population_allemagne, by = "pays") %>%
  mutate(taux = suspects_allemagne / population * 1000) %>%
  mutate(
    crime = factor(
      crime,
      levels = c("Total des infractions à l'exclusion des violations de la loi sur le séjour des étrangers", "Crimes violents")
    )
  )

ggplot(données_allemagne %>% filter(pays %in% top_pays), mapping = aes(x = pays, y = taux)) +
  geom_bar(stat = "identity", color = "black", fill = "steelblue") +
  geom_text(aes(label = round(taux, 1)),
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 2.5) +
  facet_wrap(~ crime, ncol = 1, scales = "free_y") +
  theme_bw() +
  labs(
    title = stringr::str_wrap("Nombre de suspects pour 1 000 habitants en Allemagne par nationalité en 2024 (50 pays avec le plus de suspects)", width = 90),
    x = "Pays",
    y = "Nombre de suspects pour 1 000 habitants",
    caption = "Source : BKA - Graphique de Philippe Lemoine (@phl43) pour Hexagone (@Hexagone_org)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 24),
    axis.title = element_text(face = "bold", size = 16),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    plot.caption = element_text(size = 12)
  )

ggsave(
  "Nombre de suspects en Allemagne pour 1 000 habitants en Allemagne par nationalité en 2024.png",
  width = 24,
  height = 12,
)

R2_total_tous <- summary(lm(taux ~ part_hommes_15_45, données_allemagne %>% filter(crime == "Total des infractions à l'exclusion des violations de la loi sur le séjour des étrangers"), weights = population))$r.squared
R2_violents_tous <- summary(lm(taux ~ part_hommes_15_45, données_allemagne %>% filter(crime == "Crimes violents"), weights = population))$r.squared

pays_arabes <- c(
  "Algérie",
  "Bahreïn",
  "Égypte",
  "Irak",
  "Jordanie",
  "Koweit",
  "Liban",
  "Libye",
  "Qatar",
  "Arabie Saoudite",
  "Syrie",
  "Tunisie",
  "Émirats arabes unis",
  "Yémen"
)

R2_total_arabes <- summary(lm(taux ~ part_hommes_15_45, données_allemagne %>% filter(crime == "Total des infractions à l'exclusion des violations de la loi sur le séjour des étrangers", pays %in% pays_arabes), weights = population))$r.squared
R2_violents_arabes <- summary(lm(taux ~ part_hommes_15_45, données_allemagne %>% filter(crime == "Crimes violents", pays %in% pays_arabes), weights = population))$r.squared

#########################################################################################################################################################
########################################### ANALYSE DE PUISSANCE CALIBRÉES AVEC DES DONNÉES FRANÇAISES ##################################################
#########################################################################################################################################################

simulation <- function(
    ratio_criminalité,
    part_initiale_immigrés,
    taux_criminalité_moyen,
    changement_moyen_proportion_immigrés,
    cv_changement_proportion_immigrés,
    sd_variation_idiosyncratique,
    erreur_mesure_changement_proportion_immigrés,
    erreur_mesure_changement_taux_criminalité,
    nb_départements,
    durée,
    alpha
) {
  # la fonction prend comme argument le ratio du taux de criminalité des immigrés sur le taux de criminalité des non-immigrés, mais pour la simulation on a besoin du ratio
  # du taux de criminalité des immigrés sur le taux de criminalité moyen dans la population, sachant qu'il y a déjà des immigrés dans la population en début de période (ce
  # qui veut dire que je suppose implicitement que les immigrés nouvellement arrivés ont le même taux de criminalité violente par rapport aux autochtones que ceux qui étaient
  # déjà en France en début de période)
  ratio_criminalité <- 1 / ((1 - part_initiale_immigrés) / ratio_criminalité + part_initiale_immigrés)
  
  nb_observations <- nb_départements * durée
  
  changement_proportion_immigrés = rnorm(nb_observations, changement_moyen_proportion_immigrés, cv_changement_proportion_immigrés * changement_moyen_proportion_immigrés)
  changement_taux_criminalité = (ratio_criminalité - 1) * taux_criminalité_moyen * changement_proportion_immigrés + rnorm(nb_observations, 0, sd_variation_idiosyncratique)
  
  var_changement_proportion_immigrés <- var(changement_proportion_immigrés)
  var_changement_taux_criminalité <- var(changement_taux_criminalité)
  
  sd_erreur_changement_proportion_immigrés <- ifelse(
    erreur_mesure_changement_proportion_immigrés < 1,
    sqrt(var_changement_proportion_immigrés * (1 / erreur_mesure_changement_proportion_immigrés - 1)),
    0
  )
  sd_erreur_changement_taux_criminalité <- ifelse(
    erreur_mesure_changement_taux_criminalité < 1,
    sqrt(var_changement_taux_criminalité * (1 / erreur_mesure_changement_taux_criminalité - 1)),
    0
  )
  
  changement_proportion_immigrés <- changement_proportion_immigrés + rnorm(nb_observations, 0, sd_erreur_changement_proportion_immigrés)
  changement_taux_criminalité <- changement_taux_criminalité + rnorm(nb_observations, 0, sd_erreur_changement_taux_criminalité)
  
  X <- matrix(changement_proportion_immigrés, nrow = durée, ncol = nb_départements)
  Y <- matrix(changement_taux_criminalité, nrow = durée, ncol = nb_départements)
  
  X_bar_année <- rowMeans(X)
  Y_bar_année <- rowMeans(Y)
  
  X_tilde <- X - X_bar_année
  Y_tilde <- Y - Y_bar_année
  
  Sxx <- sum(X_tilde * X_tilde)
  beta_hat <- sum(X_tilde * Y_tilde) / Sxx
  
  E_tilde <- Y_tilde - beta_hat * X_tilde
  
  score_par_département <- colSums(X_tilde * E_tilde)
  
  meat <- sum(score_par_département * score_par_département)
  
  var_beta <- (nb_départements / (nb_départements)) * (meat / (Sxx * Sxx))
  se_beta <- sqrt(var_beta)
  
  t_stat <- beta_hat / se_beta
  p <- 2 * stats::pt(abs(t_stat), df = nb_départements - 1, lower.tail = FALSE)
  
  beta_hat > 0 && p < alpha
}

calculer_puissance <- function(
    ratio_criminalité,
    part_initiale_immigrés,
    taux_criminalité_moyen,
    changement_moyen_proportion_immigrés,
    cv_changement_proportion_immigrés,
    sd_variation_idiosyncratique,
    erreur_mesure_changement_proportion_immigrés,
    erreur_mesure_changement_taux_criminalité,
    nb_départements,
    durée,
    nb_simulations,
    alpha = 0.05
) {
  rejets <- 0L
  
  for (s in seq_len(nb_simulations)) {
    rejet <- simulation(
      ratio_criminalité,
      part_initiale_immigrés,
      taux_criminalité_moyen,
      changement_moyen_proportion_immigrés,
      cv_changement_proportion_immigrés,
      sd_variation_idiosyncratique,
      erreur_mesure_changement_proportion_immigrés,
      erreur_mesure_changement_taux_criminalité,
      nb_départements,
      durée,
      alpha
    )
    
    rejets <- rejets + as.integer(rejet)
  }
  
  rejets / nb_simulations
}

# il est sans doute possible de récupérer ces données plus proprement à partir de l'API de l'INSEE, mais je n'ai pas le temps
schémas_url <- c(
  "https://www.insee.fr/fr/statistiques/tableaux/4177160/DEP/%s/rp2016_td_img1A.csv",
  "https://www.insee.fr/fr/statistiques/tableaux/4515410/DEP/%s/rp2017_td_img1A.csv",
  "https://www.insee.fr/fr/statistiques/tableaux/5397749/DEP/%s/rp2018_td_img1A.csv",
  "https://www.insee.fr/fr/statistiques/tableaux/6455262/DEP/%s/rp2019_td_img1A.csv",
  "https://www.insee.fr/fr/statistiques/tableaux/7633125/DEP/%s/rp2020_td_img1A.csv",
  "https://www.insee.fr/fr/statistiques/tableaux/8202123/DEP/%s/rp2021_td_img1A.csv",
  "https://www.insee.fr/fr/statistiques/tableaux/8582065/DEP/%s/rp2022_td_img1A.csv"
)

départements <- deps <- c(sprintf("%02d", c(1:19, 21:95)), "2A", "2B")

skip <- c(7, 7, 7, 7, 7, 8, 8)

données_immigrés_france <- tibble()

for (i in 2016:2022) {
  for (j in 1:length(départements)) {
    tmp <- read_lines(sprintf(schémas_url[i - 2015], départements[j]), skip = skip[i - 2015], n_max = 4) %>%
      paste(collapse = "\n") %>%
      read_delim(delim = ";", col_names = FALSE) %>%
      select(groupe = 1, N = 6)

    tmp <- tribble(
      ~département, ~année, ~proportion_immigrés,
      départements[j], i, as.numeric(tmp$N[1] / tmp$N[3])
    )

    données_immigrés_france <- rbind(données_immigrés_france, tmp)

    Sys.sleep(1)
  }
}

crimes_violents <- c(
  "Homicides",
  "Tentatives d'homicide",
  "Violences physiques intrafamiliales",
  "Violences physiques hors cadre familial",
  "Violences sexuelles",
  "Vols avec armes",
  "Vols violents sans arme"
)

# source : https://www.data.gouv.fr/datasets/bases-statistiques-communale-departementale-et-regionale-de-la-delinquance-enregistree-par-la-police-et-la-gendarmerie-nationales/
données_crimes_violents_france <- read_delim("données_ssmsi_france.csv", delim = ";") %>%
  filter(
    indicateur %in% crimes_violents,
    unite_de_compte == "Victime"
  ) %>%
  group_by(annee, Code_departement) %>%
  summarize(
    taux_criminalité = sum(nombre) / mean(insee_pop)
  ) %>%
  ungroup() %>%
  rename(
    département = Code_departement,
    année = annee
  )

données_france_immigration_criminalité <- inner_join(données_immigrés_france, données_crimes_violents_france, by = c("année", "département")) %>%
  group_by(département) %>%
  arrange(année, .by_group = TRUE) %>%
  mutate(
    changement_proportion_immigrés = proportion_immigrés - lag(proportion_immigrés),
    changement_taux_criminalité = taux_criminalité - lag(taux_criminalité)
  ) %>%
  ungroup() %>%
  drop_na()

taux_criminalité_moyen <- mean(données_france_immigration_criminalité$taux_criminalité)
changement_moyen_proportion_immigrés <- mean(données_france_immigration_criminalité$changement_proportion_immigrés)
cv_changement_proportion_immigrés <- sd(données_france_immigration_criminalité$changement_proportion_immigrés) / changement_moyen_proportion_immigrés

estimation_données_réelles <- fixest::feols(
  changement_taux_criminalité ~ changement_proportion_immigrés | année,
  data = données_france_immigration_criminalité
)

sd_variation_idiosyncratique <- sd(resid(estimation_données_réelles))

plan(multisession, workers = parallel::detectCores() - 1)

part_initiale_immigrés <- 0.1

nb_simulations <- 10000

durée_simulation <- 4

alpha <- 0.05

grille <- expand_grid(
  ratio_criminalité = seq(1, 30, 0.05),
  changement_moyen_proportion_immigrés = seq(0.001, 0.005, 0.001)
)

résultats_sans_erreur_mesure <- grille %>%
  mutate(
    puissance = furrr::future_pmap_dbl(
      list(
        ratio_criminalité,
        changement_moyen_proportion_immigrés
      ),
      ~ calculer_puissance(
        ratio_criminalité = ..1,
        part_initiale_immigrés = part_initiale_immigrés,
        taux_criminalité_moyen = taux_criminalité_moyen,
        changement_moyen_proportion_immigrés = ..2,
        cv_changement_proportion_immigrés = cv_changement_proportion_immigrés,
        sd_variation_idiosyncratique = sd_variation_idiosyncratique,
        erreur_mesure_changement_proportion_immigrés = 1,
        erreur_mesure_changement_taux_criminalité = 1,
        nb_départements = length(départements),
        durée = durée_simulation,
        nb_simulations = nb_simulations,
        alpha = alpha
      ),
      .options = furrr::furrr_options(seed = TRUE)
    )
  )

ggplot(résultats_sans_erreur_mesure, mapping = aes(x = ratio_criminalité, y = puissance, group = changement_moyen_proportion_immigrés, color = factor(changement_moyen_proportion_immigrés))) +
  geom_line() +
  theme_bw() +
  scale_color_brewer(
    palette = "Set1",
    name = "Taux d'immigration nette",
    labels = \(x) x %>%
      as.double() %>%
      (`*`)(100) %>%
      sprintf("%.1f%%", .) %>%
      stringr::str_replace("\\.", ",")
  ) +
  scale_x_continuous(
    breaks = seq(0, 30, by = 2.5),
    labels = \(x) x %>%
      as.character() %>%
      stringr::str_replace("\\.0$", "") %>%
      stringr::str_replace("\\.", ",")
  ) +
  scale_y_continuous(
    labels = \(y) y %>%
      as.double() %>%
      (`*`)(100) %>%
      sprintf("%.0f%%", .) %>%
      stringr::str_replace("\\.", ",")
  ) +
  labs(
    title = stringr::str_wrap("Puissance statistique d'une analyse de l'effet de l'immigration sur la délinquance en France à partir de données agrégées au niveau des départements avec 5 ans de données (sans erreur de mesure)", width = 90),
    x = "Taux de criminalité des immigrés part rapport aux non-immigrés",
    y = "Puissance",
    caption = "Source : INSEE - Graphique de Philippe Lemoine (@phl43) pour Hexagone (@Hexagone_org)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 24),
    axis.title = element_text(face = "bold", size = 16),
    plot.caption = element_text(size = 12)
  )

ggsave(
  "Puissance statistique d'une analyse de l'effet de l'immigration sur la délinquance en France à partir de données agrégées au niveau des départements avec 5 ans de données (sans erreur de mesure).png",
  width = 24,
  height = 12
)

#########################################################################################################################################################
################################################### GRAPHIQUES SUR LES CONDAMNÉS AU DANEMARK ############################################################
#########################################################################################################################################################

# source : https://www.statistikbanken.dk/statbank5a/SelectTable/Omrade0.asp?SubjectCode=4&ShowNews=OFF&PLanguage=1 (STRAFNA10)
condamnés_danemark <- read_excel("condamnés_danemark.xlsx") %>%
  pivot_longer(
    cols = -groupe,
    names_to  = "crime",
    values_to = "condamnés"
  )

# source : https://www.statistikbanken.dk/statbank5a/SelectTable/Omrade0.asp?SubjectCode=1&ShowNews=OFF&PLanguage=1 (FT et IEPCT)
population_danemark <- read_excel("population_danemark.xlsx")

données_danemark <- inner_join(condamnés_danemark, population_danemark, by = "groupe") %>%
  mutate(taux = condamnés / N * 100000)

ggplot(données_danemark %>% filter(crime %in% c("Ensemble des crimes", "Atteintes aux biens", "Crimes violents", "Crimes sexuels")), mapping = aes(x = groupe, y = taux, fill = groupe)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = round(taux, 1)),
            position = position_stack(vjust = 0.5),
            color = "black",
            size = 2.5) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_fill_brewer(palette = "Set3") +
  facet_wrap(~ crime, ncol = 2, scales = "free_y") +
  theme_bw() +
  labs(
    title = stringr::str_wrap("Nombre de condamnés pour 100 000 habitants au Danemark par origine en 2023", width = 90),
    x = "Origine",
    y = "Nombre de condamnés pour 100 000 habitants",
    caption = "Source : Statistics Denmark - Graphique de Philippe Lemoine (@phl43) pour Hexagone (@Hexagone_org)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 24),
    axis.title = element_text(face = "bold", size = 16),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    plot.caption = element_text(size = 12),
    legend.position = "none"
  )

ggsave(
  "Nombre de condamnés pour 100 000 habitants au Danemark par origine en 2023.png",
  width = 24,
  height = 12
)

crimes_avec_ajustement <- c(
  "Ensemble des crimes",
  "Crimes violents",
  "Agressions simples",
  "Agressions graves",
  "Menaces",
  "Vols",
  "Trafic de stupéfiants"
)

données_danemark_non_ajustés <- données_danemark %>%
  filter(crime %in% crimes_avec_ajustement) %>%
  group_by(crime) %>% 
  mutate(
    taux_ensemble = sum(condamnés) / sum(N) * 100000,
    indice = taux / taux_ensemble * 100
  ) %>%
  ungroup() %>%
  mutate(ajustement = "Aucun") %>%
  select(groupe, crime, ajustement, indice)

# source : https://www.justitsministeriet.dk/wp-content/uploads/2025/05/Kriminalitet-og-herkomst-2023-WT.pdf (graphique 19)
données_danemark_ajustés <- tribble(
  ~groupe, ~crime, ~ajustement, ~indice,
  "Danois non-immigrés avec au moins un parent non-immigré", "Ensemble des crimes", "Âge et sexe", 88,
  "Immigrés occidentaux", "Ensemble des crimes", "Âge et sexe", 85,
  "Immigrés non-occidentaux", "Ensemble des crimes", "Âge et sexe", 158,
  "Enfants de deux parents immigrés occidentaux", "Ensemble des crimes", "Âge et sexe", 131,
  "Enfants de deux parents immigrés non-occidentaux", "Ensemble des crimes", "Âge et sexe", 262,
  "Danois non-immigrés avec au moins un parent non-immigré", "Crimes violents", "Âge et sexe", 89,
  "Immigrés occidentaux", "Crimes violents", "Âge et sexe", 51,
  "Immigrés non-occidentaux", "Crimes violents", "Âge et sexe", 154,
  "Enfants de deux parents immigrés occidentaux", "Crimes violents", "Âge et sexe", 145,
  "Enfants de deux parents immigrés non-occidentaux", "Crimes violents", "Âge et sexe", 293,
  "Danois non-immigrés avec au moins un parent non-immigré", "Agressions simples", "Âge et sexe", 89,
  "Immigrés occidentaux", "Agressions simples", "Âge et sexe", 62,
  "Immigrés non-occidentaux", "Agressions simples", "Âge et sexe", 156,
  "Enfants de deux parents immigrés occidentaux", "Agressions simples", "Âge et sexe", 165,
  "Enfants de deux parents immigrés non-occidentaux", "Agressions simples", "Âge et sexe", 266,
  "Danois non-immigrés avec au moins un parent non-immigré", "Agressions graves", "Âge et sexe", 83,
  "Immigrés occidentaux", "Agressions graves", "Âge et sexe", 28,
  "Immigrés non-occidentaux", "Agressions graves", "Âge et sexe", 79,
  "Enfants de deux parents immigrés occidentaux", "Agressions graves", "Âge et sexe", NA,
  "Enfants de deux parents immigrés non-occidentaux", "Agressions graves", "Âge et sexe", 367,
  "Danois non-immigrés avec au moins un parent non-immigré", "Menaces", "Âge et sexe", 92,
  "Immigrés occidentaux", "Menaces", "Âge et sexe", 51,
  "Immigrés non-occidentaux", "Menaces", "Âge et sexe", 150,
  "Enfants de deux parents immigrés occidentaux", "Menaces", "Âge et sexe", NA,
  "Enfants de deux parents immigrés non-occidentaux", "Menaces", "Âge et sexe", 275,
  "Danois non-immigrés avec au moins un parent non-immigré", "Vols", "Âge et sexe", 70,
  "Immigrés occidentaux", "Vols", "Âge et sexe", 57,
  "Immigrés non-occidentaux", "Vols", "Âge et sexe", 228,
  "Enfants de deux parents immigrés occidentaux", "Vols", "Âge et sexe", NA,
  "Enfants de deux parents immigrés non-occidentaux", "Vols", "Âge et sexe", 435,
  "Danois non-immigrés avec au moins un parent non-immigré", "Trafic de stupéfiants", "Âge et sexe", 83,
  "Immigrés occidentaux", "Trafic de stupéfiants", "Âge et sexe", 29,
  "Immigrés non-occidentaux", "Trafic de stupéfiants", "Âge et sexe", 168,
  "Enfants de deux parents immigrés occidentaux", "Trafic de stupéfiants", "Âge et sexe", 158,
  "Enfants de deux parents immigrés non-occidentaux", "Trafic de stupéfiants", "Âge et sexe", 390,
)

données_danemark_ajustés <- rbind(
  données_danemark_non_ajustés,
  données_danemark_ajustés
) %>%
  mutate(
    groupe = factor(
      groupe,
      levels = c(
        "Danois non-immigrés avec au moins un parent non-immigré",
        "Immigrés occidentaux",
        "Enfants de deux parents immigrés occidentaux",
        "Immigrés non-occidentaux",
        "Enfants de deux parents immigrés non-occidentaux"
      )
    ),
    ajustement = factor(
      ajustement,
      levels = c("Aucun", "Âge et sexe")
    )
  )

ggplot(données_danemark_ajustés, mapping = aes(x = groupe, y = indice, fill = groupe, group = ajustement, alpha = ajustement)) +
  geom_bar(stat = "identity", color = "black", position = position_dodge(width = 0.9)) +
  geom_text(aes(y = indice / 2, label = round(indice)),
            position = position_dodge(width = 0.9),
            vjust = 0.5,
            color = "black",
            alpha = 1,
            size = 2.5) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_fill_brewer(palette = "Set3", guide = "none") +
  scale_alpha_manual(
    name = "Ajustement",
    values = c("Aucun" = 1, "Âge et sexe" = 0.5)
  ) +
  facet_wrap(~ crime, ncol = 2, scales = "free_y") +
  theme_bw() +
  labs(
    title = stringr::str_wrap("Indices de criminalité au Danemark avec et sans ajustement pour l'âge et le sexe à partir des données sur les condamnations en 2023", width = 90),
    x = "Origine",
    y = "Indice de criminalité",
    caption = "Source : Statistics Denmark - Graphique de Philippe Lemoine (@phl43) pour Hexagone (@Hexagone_org)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 24),
    axis.title = element_text(face = "bold", size = 16),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    plot.caption = element_text(size = 12),
    legend.position = "right"
  )

ggsave(
  "Indices de criminalité au Danemark avec et sans ajustement pour l'âge et le sexe à partir des données sur les condamnations en 2023.png",
  width = 24,
  height = 12
)

#########################################################################################################################################################
############################################ GRAPHIQUES SUR LES SUSPECTS PAR SEXE ET ÂGE EN ALLEMAGNE ###################################################
#########################################################################################################################################################

# source : https://www.bka.de/DE/AktuelleInformationen/StatistikenLagebilder/PolizeilicheKriminalstatistik/PKS2024/PKSTabellen/BundTVNationalitaet/bundTVNationalitaet.html?nn=240862 (tableaux T40, T50 et T51-55)
suspects_allemagne_hommes_21_25 <- rbind(
  read_excel(
    "suspects_allemagne_allemands_par_sexe_et_âge.xlsx",
    na = "NA"
  ) %>%
    mutate(pays = "Allemagne"),
  read_excel(
    "suspects_allemagne_étrangers_par_sexe_et_âge.xlsx",
    na = "NA"
  ) %>%
    mutate(pays = "Étrangers"),
  read_excel(
    "suspects_allemagne_italiens_par_sexe_et_âge.xlsx",
    na = "NA"
  ) %>%
    mutate(pays = "Italie"),
  read_excel(
    "suspects_allemagne_polonais_par_sexe_et_âge.xlsx",
    na = "NA"
  ) %>%
    mutate(pays = "Pologne"),
  read_excel(
    "suspects_allemagne_roumains_par_sexe_et_âge.xlsx",
    na = "NA"
  ) %>%
    mutate(pays = "Roumanie"),
  read_excel(
    "suspects_allemagne_turcs_par_sexe_et_âge.xlsx",
    na = "NA"
  ) %>%
    mutate(pays = "Turquie")
) %>%
  filter(
    sexe == "M",
    crime %in% c("Total des infractions pénales, à l'exclusion des violations de la loi sur le séjour des étrangers, de la loi sur l'asile et de la loi sur la liberté de circulation/UE (code 725000)", "Crimes violents")
  ) %>%
  mutate(
    crime = ifelse(
      crime == "Total des infractions pénales, à l'exclusion des violations de la loi sur le séjour des étrangers, de la loi sur l'asile et de la loi sur la liberté de circulation/UE (code 725000)",
      "Total des infractions à l'exclusion des violations de la loi sur le séjour des étrangers",
      crime
    ),
    nb_suspects_hommes_21_25 = `Adultes 21-25 ans`
  ) %>%
  select(pays, crime, nb_suspects_hommes_21_25)

données_allemagne_hommes_21_25 <- inner_join(suspects_allemagne_hommes_21_25, population_allemagne, by = "pays") %>%
  mutate(taux = nb_suspects_hommes_21_25 / hommes_21_25 * 1000) %>%
  mutate(
    crime = factor(
      crime,
      levels = c("Total des infractions à l'exclusion des violations de la loi sur le séjour des étrangers", "Crimes violents")
    )
  )

ggplot(données_allemagne_hommes_21_25, mapping = aes(x = pays, y = taux)) +
  geom_bar(stat = "identity", color = "black", fill = "steelblue") +
  geom_text(aes(label = round(taux, 1)),
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 6) +
  facet_wrap(~ crime, ncol = 1, scales = "free_y") +
  theme_bw() +
  labs(
    title = stringr::str_wrap("Nombre de suspects pour 1 000 habitants pour les hommes entre 21 et 25 ans en Allemagne par nationalité en 2024", width = 90),
    x = "Pays",
    y = "Nombre de suspects pour 1 000 habitants",
    caption = "Source : BKA - Graphique de Philippe Lemoine (@phl43) pour Hexagone (@Hexagone_org)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 24),
    axis.title = element_text(face = "bold", size = 16),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    plot.caption = element_text(size = 12)
  )

ggsave(
  "Nombre de suspects pour 1 000 habitants pour les hommes entre 21 et 25 ans en Allemagne par nationalité en 2024.png",
  width = 24,
  height = 12
)

#########################################################################################################################################################
########################### GRAPHIQUES SUR LES SUSPECTS AVEC AJUSTEMENT POUR LES CARACTÉRISTIQUES SOCIO-DÉMOGRAPHIQUES EN SUÈDE #########################
#########################################################################################################################################################

# source : https://bra.se/rapporter/arkiv/2021-08-25-misstankta-for-brott-bland-personer-med-inrikes-respektive-utrikes-bakgrund (graphiques 6 et 8)
taux_suspicion_suède <- tribble(
  ~pays, ~ajustement, ~taux,
  "Suède", "Aucun", 3.18,
  "Autres pays nordiques", "Aucun", 3.4,
  "Autres pays nordiques", "Âge et sexe", 4.6,
  "Autres pays nordiques", "Âge, sexe, revenu et éducation", 3.7,
  "UE à 15 et Europe de l'Ouest sans les pays nordiques", "Aucun", 3.4,
  "UE à 15 et Europe de l'Ouest sans les pays nordiques", "Âge et sexe", 3.4,
  "UE à 15 et Europe de l'Ouest sans les pays nordiques", "Âge, sexe, revenu et éducation", 2.9,
  "Autres pays de l'UE", "Aucun", 6.8,
  "Autres pays de l'UE", "Âge et sexe", 6.5,
  "Autres pays de l'UE", "Âge, sexe, revenu et éducation", 5.5,
  "États-Unis, Canada, Australie et Nouvelle-Zélande", "Aucun", 3.6,
  "États-Unis, Canada, Australie et Nouvelle-Zélande", "Âge et sexe", 3.1,
  "États-Unis, Canada, Australie et Nouvelle-Zélande", "Âge, sexe, revenu et éducation", 2.8,
  "Amérique centrale et Caraïbes", "Aucun", 10.1,
  "Amérique centrale et Caraïbes", "Âge et sexe", 8.2,
  "Amérique centrale et Caraïbes", "Âge, sexe, revenu et éducation", 6.4,
  "Amérique du Sud", "Aucun", 9.2,
  "Amérique du Sud", "Âge et sexe", 7.9,
  "Amérique du Sud", "Âge, sexe, revenu et éducation", 6.7,
  "Asie de l'Ouest", "Aucun", 11.9,
  "Asie de l'Ouest", "Âge et sexe", 9.2,
  "Asie de l'Ouest", "Âge, sexe, revenu et éducation", 7.2,
  "Asie centrale", "Aucun", 15.8,
  "Asie centrale", "Âge et sexe", 10.2,
  "Asie centrale", "Âge, sexe, revenu et éducation", 9.2,
  "Asie du Sud", "Aucun", 6.2,
  "Asie du Sud", "Âge et sexe", 5.5,
  "Asie du Sud", "Âge, sexe, revenu et éducation", 4.7,
  "Asie du Sud-Est et Océanie", "Aucun", 5.7,
  "Asie du Sud-Est et Océanie", "Âge et sexe", 5.1,
  "Asie du Sud-Est et Océanie", "Âge, sexe, revenu et éducation", 4,
  "Asie de l'Est", "Aucun", 3.3,
  "Asie de l'Est", "Âge et sexe", 3,
  "Asie de l'Est", "Âge, sexe, revenu et éducation", 2.6,
  "Afrique du Nord", "Aucun", 14.8,
  "Afrique du Nord", "Âge et sexe", 11.6,
  "Afrique du Nord", "Âge, sexe, revenu et éducation", 9.6,
  "Afrique de l'Est", "Aucun", 12.9,
  "Afrique de l'Est", "Âge et sexe", 8.6,
  "Afrique de l'Est", "Âge, sexe, revenu et éducation", 7.1,
  "Reste de l'Afrique", "Aucun", 18.7,
  "Reste de l'Afrique", "Âge et sexe", 12.6,
  "Reste de l'Afrique", "Âge, sexe, revenu et éducation", 10.1
) %>%
  mutate(
    pays = factor(
      pays,
      levels = c(
        "Suède",
        "Autres pays nordiques",
        "UE à 15 et Europe de l'Ouest sans les pays nordiques",
        "Autres pays de l'UE",
        "États-Unis, Canada, Australie et Nouvelle-Zélande",
        "Amérique centrale et Caraïbes",
        "Amérique du Sud",
        "Asie de l'Ouest",
        "Asie centrale",
        "Asie du Sud",
        "Asie du Sud-Est et Océanie",
        "Asie de l'Est",
        "Afrique du Nord",
        "Afrique de l'Est",
        "Reste de l'Afrique"
      )
    ),
    ajustement = factor(
      ajustement,
      levels = c("Aucun", "Âge et sexe", "Âge, sexe, revenu et éducation")
    )
  )

ggplot(taux_suspicion_suède, mapping = aes(x = pays, y = taux, fill = pays, group = ajustement, alpha = ajustement)) +
  geom_bar(stat = "identity", color = "black", position = position_dodge(width = 0.9)) +
  geom_text(aes(y = taux / 2, label = round(taux, 1)),
            position = position_dodge(width = 0.9),
            vjust = 0.5,
            color = "black",
            alpha = 1,
            size = 2.5) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_fill_manual(values = colorRampPalette(brewer.pal(12, "Set3"))(15), guide = "none") +
  scale_alpha_manual(
    name = "Ajustement",
    values = c("Aucun" = 1, "Âge et sexe" = 0.75, "Âge, sexe, revenu et éducation" = 0.5)
  ) +
  theme_bw() +
  labs(
    title = stringr::str_wrap("Nombre de suspects pour 100 habitants en Suède entre 2015 et 2018 avec et sans ajustement pour l'âge, le sexe, le revenu et l'éducation", width = 90),
    x = "Origine",
    y = "Nombre de suspects pour 100 habitants",
    caption = "Source : Brå - Graphique de Philippe Lemoine (@phl43) pour Hexagone (@Hexagone_org)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 24),
    axis.title = element_text(face = "bold", size = 16),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    plot.caption = element_text(size = 12),
    legend.position = "right"
  )

ggsave(
  "Nombre de suspects pour 100 habitants en Suède entre 2015 et 2018 avec et sans ajustement pour l'âge, le sexe, le revenu et l'éducation.png",
  width = 24,
  height = 12
)

# source : https://bra.se/rapporter/arkiv/2021-08-25-misstankta-for-brott-bland-personer-med-inrikes-respektive-utrikes-bakgrund (graphiques 9, 10, 12 et 13)
données_suède_suspects <- read_excel("suspects_suède.xlsx") %>%
  mutate(
    taux = taux * 10,
    groupe = factor(
      groupe,
      levels = c(
        "Né en Suède avec deux parents nés en Suède",
        "Né en Suède avec parent né en Suède et un à l'étranger",
        "Né en Suède avec deux parents nés à l'étranger",
        "Né à l'étranger"
      )
    ),
    ajustement = factor(
      ajustement,
      levels = c("Aucun", "Âge et sexe", "Âge, sexe, revenu, éducation et type de municipalité")
    )
  )

ggplot(données_suède_suspects, mapping = aes(x = groupe, y = taux, fill = groupe, group = ajustement, alpha = ajustement)) +
  geom_bar(stat = "identity", color = "black", position = position_dodge(width = 0.9)) +
  geom_text(aes(y = taux / 2, label = round(taux, 1)),
            position = position_dodge(width = 0.9),
            vjust = 0.5,
            color = "black",
            alpha = 1,
            size = 2.5) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_fill_brewer(palette = "Set3", guide = "none") +
  scale_alpha_manual(
    name = "Ajustement",
    values = c("Aucun" = 1, "Âge et sexe" = 0.75, "Âge, sexe, revenu, éducation et type de municipalité" = 0.5)
  ) +
  facet_wrap(~ crime, ncol = 2, scales = "free_y") +
  theme_bw() +
  labs(
    title = stringr::str_wrap("Nombre de suspects pour 10 000 habitants en Suède avec et sans ajustement socio-démographique à partir des données sur les condamnations entre 2015 et 2018", width = 90),
    x = "Origine",
    y = "Nombre de suspects pour 10 000 habitants",
    caption = "Source : Brå - Graphique de Philippe Lemoine (@phl43) pour Hexagone (@Hexagone_org)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 24),
    axis.title = element_text(face = "bold", size = 16),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    plot.caption = element_text(size = 12),
    legend.position = "right"
  )

ggsave(
  "Nombre de suspects pour 10 000 habitants en Suède avec et sans ajustement socio-démographique à partir des données sur les condamnations entre 2015 et 2018.png",
  width = 24,
  height = 20
)

#########################################################################################################################################################
######################### GRAPHIQUES SUR LES CONDAMNÉS AVEC AJUSTEMENT POUR LES CARACTÉRISTIQUES SOCIO-DÉMOGRAPHIQUES AU DANEMARK #######################
#########################################################################################################################################################

# source : https://en.rockwoolfonden.dk/publications/etniske-minoriteters-overrepraesentation-i-strafferetlige-domme/ (tableau 4 et annexe F, que j'ai mise sur GitHub)
condamnés_danemark_logit <- tribble(
  ~groupe, ~crime, ~ajustement, ~odds_ratio,
  "Immigrés occidentaux", "Ensemble des infractions criminelles", "Âge seulement", 1.090,
  "Immigrés occidentaux", "Atteintes aux biens", "Âge seulement", 1.191,
  "Immigrés occidentaux", "Crimes violents", "Âge seulement", 1.050,
  "Immigrés occidentaux", "Autres infractions criminelles", "Âge seulement", 0.812,
  "Enfants d'immigrés occidentaux", "Ensemble des infractions criminelles", "Âge seulement", 1.060,
  "Enfants d'immigrés occidentaux", "Atteintes aux biens", "Âge seulement", 1.023,
  "Enfants d'immigrés occidentaux", "Crimes violents", "Âge seulement", 0.977,
  "Enfants d'immigrés occidentaux", "Autres infractions criminelles", "Âge seulement", 1.340,
  "Immigrés moyen-orientaux", "Ensemble des infractions criminelles", "Âge seulement", 2.729,
  "Immigrés moyen-orientaux", "Atteintes aux biens", "Âge seulement", 2.362,
  "Immigrés moyen-orientaux", "Crimes violents", "Âge seulement", 3.200,
  "Immigrés moyen-orientaux", "Autres infractions criminelles", "Âge seulement", 3.739,
  "Enfants d'immigrés moyen-orientaux", "Ensemble des infractions criminelles", "Âge seulement", 2.777,
  "Enfants d'immigrés moyen-orientaux", "Atteintes aux biens", "Âge seulement", 2.093,
  "Enfants d'immigrés moyen-orientaux", "Crimes violents", "Âge seulement", 3.354,
  "Enfants d'immigrés moyen-orientaux", "Autres infractions criminelles", "Âge seulement", 5.525,
  "Immigrés asiatiques", "Ensemble des infractions criminelles", "Âge seulement", 1.374,
  "Immigrés asiatiques", "Atteintes aux biens", "Âge seulement", 1.531,
  "Immigrés asiatiques", "Crimes violents", "Âge seulement", 1.278,
  "Immigrés asiatiques", "Autres infractions criminelles", "Âge seulement", 0.732,
  "Enfants d'immigrés asiatiques", "Ensemble des infractions criminelles", "Âge seulement", 0.859,
  "Enfants d'immigrés asiatiques", "Atteintes aux biens", "Âge seulement", 0.561,
  "Enfants d'immigrés asiatiques", "Crimes violents", "Âge seulement", 1.575,
  "Enfants d'immigrés asiatiques", "Autres infractions criminelles", "Âge seulement", 0.639,
  "Autres immigrés non-occidentaux", "Ensemble des infractions criminelles", "Âge seulement", 2.424,
  "Autres immigrés non-occidentaux", "Atteintes aux biens", "Âge seulement", 2.410,
  "Autres immigrés non-occidentaux", "Crimes violents", "Âge seulement", 2.405,
  "Autres immigrés non-occidentaux", "Autres infractions criminelles", "Âge seulement", 2.618,
  "Autres enfants d'immigrés non-occidentaux", "Ensemble des infractions criminelles", "Âge seulement", 2.193,
  "Autres enfants d'immigrés non-occidentaux", "Atteintes aux biens", "Âge seulement", 2.075,
  "Autres enfants d'immigrés non-occidentaux", "Crimes violents", "Âge seulement", 2.249,
  "Autres enfants d'immigrés non-occidentaux", "Autres infractions criminelles", "Âge seulement", 2.747,
  "Immigrés occidentaux", "Ensemble des infractions criminelles", "Âge et caractéristiques socio-économiques", 1.021,
  "Immigrés occidentaux", "Atteintes aux biens", "Âge et caractéristiques socio-économiques", 1.106,
  "Immigrés occidentaux", "Crimes violents", "Âge et caractéristiques socio-économiques", 0.986,
  "Immigrés occidentaux", "Autres infractions criminelles", "Âge et caractéristiques socio-économiques", 0.691,
  "Enfants d'immigrés occidentaux", "Ensemble des infractions criminelles", "Âge et caractéristiques socio-économiques", 0.720,
  "Enfants d'immigrés occidentaux", "Atteintes aux biens", "Âge et caractéristiques socio-économiques", 0.708,
  "Enfants d'immigrés occidentaux", "Crimes violents", "Âge et caractéristiques socio-économiques", 0.672,
  "Enfants d'immigrés occidentaux", "Autres infractions criminelles", "Âge et caractéristiques socio-économiques", 0.833,
  "Immigrés moyen-orientaux", "Ensemble des infractions criminelles", "Âge et caractéristiques socio-économiques", 1.406,
  "Immigrés moyen-orientaux", "Atteintes aux biens", "Âge et caractéristiques socio-économiques", 1.123,
  "Immigrés moyen-orientaux", "Crimes violents", "Âge et caractéristiques socio-économiques", 1.791,
  "Immigrés moyen-orientaux", "Autres infractions criminelles", "Âge et caractéristiques socio-économiques", 2.195,
  "Enfants d'immigrés moyen-orientaux", "Ensemble des infractions criminelles", "Âge et caractéristiques socio-économiques", 1.479,
  "Enfants d'immigrés moyen-orientaux", "Atteintes aux biens", "Âge et caractéristiques socio-économiques", 1.108,
  "Enfants d'immigrés moyen-orientaux", "Crimes violents", "Âge et caractéristiques socio-économiques", 1.769,
  "Enfants d'immigrés moyen-orientaux", "Autres infractions criminelles", "Âge et caractéristiques socio-économiques", 3.075,
  "Immigrés asiatiques", "Ensemble des infractions criminelles", "Âge et caractéristiques socio-économiques", 1.078,
  "Immigrés asiatiques", "Atteintes aux biens", "Âge et caractéristiques socio-économiques", 1.184,
  "Immigrés asiatiques", "Crimes violents", "Âge et caractéristiques socio-économiques", 1.010,
  "Immigrés asiatiques", "Autres infractions criminelles", "Âge et caractéristiques socio-économiques", 0.559,
  "Enfants d'immigrés asiatiques", "Ensemble des infractions criminelles", "Âge et caractéristiques socio-économiques", 0.722,
  "Enfants d'immigrés asiatiques", "Atteintes aux biens", "Âge et caractéristiques socio-économiques", 0.475,
  "Enfants d'immigrés asiatiques", "Crimes violents", "Âge et caractéristiques socio-économiques", 1.308,
  "Enfants d'immigrés asiatiques", "Autres infractions criminelles", "Âge et caractéristiques socio-économiques", 0.563,
  "Autres immigrés non-occidentaux", "Ensemble des infractions criminelles", "Âge et caractéristiques socio-économiques", 1.393,
  "Autres immigrés non-occidentaux", "Atteintes aux biens", "Âge et caractéristiques socio-économiques", 1.297,
  "Autres immigrés non-occidentaux", "Crimes violents", "Âge et caractéristiques socio-économiques", 1.481,
  "Autres immigrés non-occidentaux", "Autres infractions criminelles", "Âge et caractéristiques socio-économiques", 1.619,
  "Autres enfants d'immigrés non-occidentaux", "Ensemble des infractions criminelles", "Âge et caractéristiques socio-économiques", 1.208,
  "Autres enfants d'immigrés non-occidentaux", "Atteintes aux biens", "Âge et caractéristiques socio-économiques", 1.148,
  "Autres enfants d'immigrés non-occidentaux", "Crimes violents", "Âge et caractéristiques socio-économiques", 1.244,
  "Autres enfants d'immigrés non-occidentaux", "Autres infractions criminelles", "Âge et caractéristiques socio-économiques", 1.447
) %>%
  mutate(
    groupe = factor(
      groupe,
      levels = c(
        "Immigrés occidentaux",
        "Enfants d'immigrés occidentaux",
        "Immigrés moyen-orientaux",
        "Enfants d'immigrés moyen-orientaux",
        "Immigrés asiatiques",
        "Enfants d'immigrés asiatiques",
        "Autres immigrés non-occidentaux",
        "Autres enfants d'immigrés non-occidentaux"
      )
    ),
    crime = factor(
      crime,
      levels = c(
        "Ensemble des infractions criminelles",
        "Atteintes aux biens",
        "Crimes violents",
        "Autres infractions criminelles"
      )
    ),
    ajustement = factor(
      ajustement,
      levels = c("Âge seulement", "Âge et caractéristiques socio-économiques")
    )
  )

# source : https://en.rockwoolfonden.dk/publications/etniske-minoriteters-overrepraesentation-i-strafferetlige-domme/ (tableau 5 et annexe F, que j'ai mise sur GitHub)
condamnés_danemark_poisson <- tribble(
  ~groupe, ~crime, ~ajustement, ~ratio_incidence,
  "Immigrés occidentaux", "Ensemble des infractions criminelles", "Âge seulement", 0.866,
  "Immigrés occidentaux", "Atteintes aux biens", "Âge seulement", 0.865,
  "Immigrés occidentaux", "Crimes violents", "Âge seulement", 0.882,
  "Immigrés occidentaux", "Autres infractions criminelles", "Âge seulement", 0.830,
  "Enfants d'immigrés occidentaux", "Ensemble des infractions criminelles", "Âge seulement", 1.338,
  "Enfants d'immigrés occidentaux", "Atteintes aux biens", "Âge seulement", 1.313,
  "Enfants d'immigrés occidentaux", "Crimes violents", "Âge seulement", 1.340,
  "Enfants d'immigrés occidentaux", "Autres infractions criminelles", "Âge seulement", 1.609,
  "Immigrés moyen-orientaux", "Ensemble des infractions criminelles", "Âge seulement", 2.729,
  "Immigrés moyen-orientaux", "Atteintes aux biens", "Âge seulement", 2.877,
  "Immigrés moyen-orientaux", "Crimes violents", "Âge seulement", 3.630,
  "Immigrés moyen-orientaux", "Autres infractions criminelles", "Âge seulement", 3.698,
  "Enfants d'immigrés moyen-orientaux", "Ensemble des infractions criminelles", "Âge seulement", 3.065,
  "Enfants d'immigrés moyen-orientaux", "Atteintes aux biens", "Âge seulement", 2.556,
  "Enfants d'immigrés moyen-orientaux", "Crimes violents", "Âge seulement", 3.815,
  "Enfants d'immigrés moyen-orientaux", "Autres infractions criminelles", "Âge seulement", 6.074,
  "Immigrés asiatiques", "Ensemble des infractions criminelles", "Âge seulement", 1.377,
  "Immigrés asiatiques", "Atteintes aux biens", "Âge seulement", 1.440,
  "Immigrés asiatiques", "Crimes violents", "Âge seulement", 1.262,
  "Immigrés asiatiques", "Autres infractions criminelles", "Âge seulement", 1.200,
  "Enfants d'immigrés asiatiques", "Ensemble des infractions criminelles", "Âge seulement", 1.023,
  "Enfants d'immigrés asiatiques", "Atteintes aux biens", "Âge seulement", 1.047,
  "Enfants d'immigrés asiatiques", "Crimes violents", "Âge seulement", 0.973,
  "Enfants d'immigrés asiatiques", "Autres infractions criminelles", "Âge seulement", 0.914,
  "Autres immigrés non-occidentaux", "Ensemble des infractions criminelles", "Âge seulement", 2.568,
  "Autres immigrés non-occidentaux", "Atteintes aux biens", "Âge seulement", 2.492,
  "Autres immigrés non-occidentaux", "Crimes violents", "Âge seulement", 2.725,
  "Autres immigrés non-occidentaux", "Autres infractions criminelles", "Âge seulement", 2.752,
  "Autres enfants d'immigrés non-occidentaux", "Ensemble des infractions criminelles", "Âge seulement", 2.685,
  "Autres enfants d'immigrés non-occidentaux", "Atteintes aux biens", "Âge seulement", 2.448,
  "Autres enfants d'immigrés non-occidentaux", "Crimes violents", "Âge seulement", 2.864,
  "Autres enfants d'immigrés non-occidentaux", "Autres infractions criminelles", "Âge seulement", 4.737,
  "Immigrés occidentaux", "Ensemble des infractions criminelles", "Âge et caractéristiques socio-économiques", 0.768,
  "Immigrés occidentaux", "Atteintes aux biens", "Âge et caractéristiques socio-économiques", 0.756,
  "Immigrés occidentaux", "Crimes violents", "Âge et caractéristiques socio-économiques", 0.798,
  "Immigrés occidentaux", "Autres infractions criminelles", "Âge et caractéristiques socio-économiques", 0.740,
  "Enfants d'immigrés occidentaux", "Ensemble des infractions criminelles", "Âge et caractéristiques socio-économiques", 0.901,
  "Enfants d'immigrés occidentaux", "Atteintes aux biens", "Âge et caractéristiques socio-économiques", 0.874,
  "Enfants d'immigrés occidentaux", "Crimes violents", "Âge et caractéristiques socio-économiques", 0.937,
  "Enfants d'immigrés occidentaux", "Autres infractions criminelles", "Âge et caractéristiques socio-économiques", 1.053,
  "Immigrés moyen-orientaux", "Ensemble des infractions criminelles", "Âge et caractéristiques socio-économiques", 1.409,
  "Immigrés moyen-orientaux", "Atteintes aux biens", "Âge et caractéristiques socio-économiques", 1.169,
  "Immigrés moyen-orientaux", "Crimes violents", "Âge et caractéristiques socio-économiques", 1.959,
  "Immigrés moyen-orientaux", "Autres infractions criminelles", "Âge et caractéristiques socio-économiques", 2.024,
  "Enfants d'immigrés moyen-orientaux", "Ensemble des infractions criminelles", "Âge et caractéristiques socio-économiques", 1.636,
  "Enfants d'immigrés moyen-orientaux", "Atteintes aux biens", "Âge et caractéristiques socio-économiques", 1.345,
  "Enfants d'immigrés moyen-orientaux", "Crimes violents", "Âge et caractéristiques socio-économiques", 2.079,
  "Enfants d'immigrés moyen-orientaux", "Autres infractions criminelles", "Âge et caractéristiques socio-économiques", 3.547,
  "Immigrés asiatiques", "Ensemble des infractions criminelles", "Âge et caractéristiques socio-économiques", 1.053,
  "Immigrés asiatiques", "Atteintes aux biens", "Âge et caractéristiques socio-économiques", 1.080,
  "Immigrés asiatiques", "Crimes violents", "Âge et caractéristiques socio-économiques", 0.997,
  "Immigrés asiatiques", "Autres infractions criminelles", "Âge et caractéristiques socio-économiques", 0.960,
  "Enfants d'immigrés asiatiques", "Ensemble des infractions criminelles", "Âge et caractéristiques socio-économiques", 0.872,
  "Enfants d'immigrés asiatiques", "Atteintes aux biens", "Âge et caractéristiques socio-économiques", 0.880,
  "Enfants d'immigrés asiatiques", "Crimes violents", "Âge et caractéristiques socio-économiques", 0.845,
  "Enfants d'immigrés asiatiques", "Autres infractions criminelles", "Âge et caractéristiques socio-économiques", 0.829,
  "Autres immigrés non-occidentaux", "Ensemble des infractions criminelles", "Âge et caractéristiques socio-économiques", 1.380,
  "Autres immigrés non-occidentaux", "Atteintes aux biens", "Âge et caractéristiques socio-économiques", 1.272,
  "Autres immigrés non-occidentaux", "Crimes violents", "Âge et caractéristiques socio-économiques", 1.613,
  "Autres immigrés non-occidentaux", "Autres infractions criminelles", "Âge et caractéristiques socio-économiques", 1.632,
  "Autres enfants d'immigrés non-occidentaux", "Ensemble des infractions criminelles", "Âge et caractéristiques socio-économiques", 1.451,
  "Autres enfants d'immigrés non-occidentaux", "Atteintes aux biens", "Âge et caractéristiques socio-économiques", 1.295,
  "Autres enfants d'immigrés non-occidentaux", "Crimes violents", "Âge et caractéristiques socio-économiques", 1.620,
  "Autres enfants d'immigrés non-occidentaux", "Autres infractions criminelles", "Âge et caractéristiques socio-économiques", 2.679
) %>%
  mutate(
    groupe = factor(
      groupe,
      levels = c(
        "Immigrés occidentaux",
        "Enfants d'immigrés occidentaux",
        "Immigrés moyen-orientaux",
        "Enfants d'immigrés moyen-orientaux",
        "Immigrés asiatiques",
        "Enfants d'immigrés asiatiques",
        "Autres immigrés non-occidentaux",
        "Autres enfants d'immigrés non-occidentaux"
      )
    ),
    crime = factor(
      crime,
      levels = c(
        "Ensemble des infractions criminelles",
        "Atteintes aux biens",
        "Crimes violents",
        "Autres infractions criminelles"
      )
    ),
    ajustement = factor(
      ajustement,
      levels = c("Âge seulement", "Âge et caractéristiques socio-économiques")
    )
  )

ggplot(condamnés_danemark_logit, mapping = aes(x = odds_ratio, y = groupe, color = ajustement)) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_errorbarh(
    aes(
      xmin = pmin(1, odds_ratio),
      xmax = pmax(1, odds_ratio),
      group = ajustement
    ),
    width = 0,
    linewidth = 1,
    color = "grey70",
    position = position_dodge(width = 0.6),
    show.legend = FALSE
  ) +
  geom_point(size = 3, position = position_dodge(width = 0.6)) +
  geom_text_repel(
    aes(label = round(odds_ratio, 2), x = ifelse(odds_ratio >= 1, odds_ratio * 1.06, odds_ratio / 1.06), hjust = ifelse(odds_ratio >= 1, 0, 1)),
    position = position_dodge(width = 0.6),
    direction = "y",
    box.padding = 0.15,
    point.padding = 0.10
  ) +
  scale_x_log10(expand = expansion(mult = c(0.02, 0.35))) +
  scale_color_brewer(
    name = "Ajustement",
    palette = "Set1"
  ) +
  facet_wrap(~ crime, ncol = 2, scales = "free_x") +
  theme_bw() +
  labs(
    title = stringr::str_wrap("Effet de l'origine des individus de sexe masculin entre 15 et 45 ans sur la probabilité d'etre condamné pour un crime en Suède en 2006 par rapport aux Danois dont les deux parents sont nés au Danemark avec et sans ajustement pour les caractéristiques socio-économiques", width = 90),
    x = "Origine",
    y = "Odds ratio",
    caption = "Source : Lars Højsgaard Andersen et Torben Tranæs, \"Etniske minoriteters overrepræsentation i strafferetlige domme\", 2011 - Graphique de Philippe Lemoine (@phl43) pour Hexagone (@Hexagone_org)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 24),
    axis.title = element_text(face = "bold", size = 16),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    plot.caption = element_text(size = 12),
    legend.position = "right"
  )

ggsave(
  "Effet de l'origine des individus de sexe masculin entre 15 et 45 ans sur la probabilité d'etre condamné pour un crime en Suède en 2006 par rapport aux Danois dont les deux parents sont nés au Danemark.png",
  width = 24,
  height = 20
)

ggplot(condamnés_danemark_poisson, mapping = aes(x = ratio_incidence, y = groupe, color = ajustement)) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_errorbarh(
    aes(
      xmin = pmin(1, ratio_incidence),
      xmax = pmax(1, ratio_incidence),
      group = ajustement
    ),
    width = 0,
    linewidth = 1,
    color = "grey70",
    position = position_dodge(width = 0.6),
    show.legend = FALSE
  ) +
  geom_point(size = 3, position = position_dodge(width = 0.6)) +
  geom_text_repel(
    aes(label = round(ratio_incidence, 2), x = ifelse(ratio_incidence >= 1, ratio_incidence * 1.06, ratio_incidence / 1.06), hjust = ifelse(ratio_incidence >= 1, 0, 1)),
    position = position_dodge(width = 0.6),
    direction = "y",
    box.padding = 0.15,
    point.padding = 0.10
  ) +
  scale_x_log10(expand = expansion(mult = c(0.02, 0.35))) +
  scale_color_brewer(
    name = "Ajustement",
    palette = "Set1"
  ) +
  facet_wrap(~ crime, ncol = 2, scales = "free_x") +
  theme_bw() +
  labs(
    title = stringr::str_wrap("Effet de l'origine des individus de sexe masculin entre 15 et 45 ans sur le nombre de condamnations pour un crime en Suède entre 2002 et 2006 par rapport aux Danois dont les deux parents sont nés au Danemark avec et sans ajustement pour les caractéristiques socio-économiques", width = 90),
    x = "Origine",
    y = "Ratio d'incidence",
    caption = "Source : Lars Højsgaard Andersen et Torben Tranæs, \"Etniske minoriteters overrepræsentation i strafferetlige domme\", 2011 - Graphique de Philippe Lemoine (@phl43) pour Hexagone (@Hexagone_org)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 24),
    axis.title = element_text(face = "bold", size = 16),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    plot.caption = element_text(size = 12),
    legend.position = "right"
  )

ggsave(
  "Effet de l'origine des individus de sexe masculin entre 15 et 45 ans sur le nombre de condamnations pour un crime en Suède entre 2002 et 2006 par rapport aux Danois dont les deux parents sont nés au Danemark.png",
  width = 24,
  height = 20
)

#########################################################################################################################################################
################################################### GRAPHIQUES SUR LES DONNÉES DE L'ENQUÊTE DE LAGRANGE #################################################
#########################################################################################################################################################

effets_lagrange <- tribble(
  ~origine, ~odds_ratio, ~significance,
  "Europe", exp(0.15), "",
  "Maghreb", exp(0.84), "***",
  "Afrique sahélienne", exp(1.36), "***",
  "Reste de l'Afrique", exp(0.74), "**",
  "Turquie", exp(0.53), "",
  "Reste du monde", exp(-0.28), ""
) %>%
  mutate(
    origine = factor(
      origine,
      levels = c(
        "Europe",
        "Maghreb",
        "Afrique sahélienne",
        "Reste de l'Afrique",
        "Turquie",
        "Reste du monde"
      )
    )
  )

ggplot(effets_lagrange, mapping = aes(x = odds_ratio, y = origine)) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_segment(aes(x = 1, xend = odds_ratio), linewidth = 1, color = "grey") +
  geom_point(color = "orange", size = 3) +
  geom_text_repel(
    aes(label = paste0(round(odds_ratio, 2), significance)),
    direction = "y",
    box.padding = 0.5,
    point.padding = 0.5,
    size = 6
  ) +
  annotate(
    "label",
    x = Inf, y = Inf,
    label = "*: p < 0.05\n**: p < 0.01\n***: p < 0.001",
    hjust = 1.05, vjust = 1.05,
    size = 6
  ) +
  scale_x_log10() +
  coord_cartesian(clip = "off") +
  theme_bw() +
  labs(
    title = stringr::str_wrap("Effet de l'origine des adolescents sur la probabilité de faire l'objet d'un signalement au parquet pour un délit par rapport aux individus sans ascendance migratoire dans un groupe de communes répartis sur trois départements en France entre 1999 et 2006", width = 90),
    x = "Origine",
    y = "Odds ratio",
    caption = "Source : Hugues Lagrange, \"Réussite scolaire et inconduites adolescentes : origine culturelle, mixité et capital social\", 2010 - Graphique de Philippe Lemoine (@phl43) pour Hexagone (@Hexagone_org)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 24),
    axis.title = element_text(face = "bold", size = 16),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    plot.caption = element_text(size = 12)
  )

ggsave(
  "Effet de l'origine des adolescents sur la probabilité de faire l'objet d'un signalement au parquet pour un délit par rapport aux individus sans ascendance migratoire.png",
  width = 24,
  height = 12
)

#########################################################################################################################################################
################################################### GRAPHIQUES SUR LES DONNÉES SARISLAN ET AL. (2021) ###################################################
#########################################################################################################################################################

effets_sariaslan <- tribble(
  ~ajustement, ~risque_relatif, ~borne_inférieure, ~borne_supérieure,
  "Comparaison entre individus dans la population en général", 0.768337893180008, 0.760869929777212, 0.775879154889939,
  "Comparaison entre individus dans la population en général après ajustement pour les caractéristiques familiales observables", 0.920723733290233, 0.912229765655194, 0.929296790085593,
  "Comparaison au sein d'une même famille", 0.997615014954199, 0.978363430304455, 1.01724541947808
)

ggplot(effets_sariaslan, mapping = aes(x = risque_relatif, y = ajustement)) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_errorbarh(aes(xmin = borne_inférieure, xmax = borne_supérieure), width = 0.15) +
  geom_point(color = "orange", size = 3) +
  geom_text_repel(
    aes(label = round(risque_relatif, 2)),
    direction = "y",
    box.padding = 0.5,
    point.padding = 0.5
  ) +
  scale_x_log10() +
  scale_y_discrete(labels = \(x) str_wrap(x, width = 25)) +
  coord_cartesian(clip = "off") +
  theme_bw() +
  labs(
    title = stringr::str_wrap("Effet d'une augmentation du revenu familial de $15 000 sur la probabilité d'être arrêté pour un crime violent pour tous les individus nés en Finlande entre 1986 et 1996", width = 90),
    x = "Risque relatif",
    y = "Méthode",
    caption = "Source : Amir Sariaslan et al., \"No causal associations between childhood family income and subsequent psychiatric disorders, substance misuse and violent crime arrests: a nationwide Finnish study of >650 000 individuals and their siblings\", 2021 - Graphique de Philippe Lemoine (@phl43) pour Hexagone (@Hexagone_org)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 24),
    axis.title = element_text(face = "bold", size = 16),
    plot.caption = element_text(size = 12)
  )

ggsave(
  "Effet d'une augmentation du revenu familial de $15 000 sur la probabilité d'être arrêté pour un crime violent pour tous les individus nés en Finlande entre 1986 et 1996.png",
  width = 24,
  height = 12
)
