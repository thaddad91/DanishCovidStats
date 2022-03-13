library("tidyverse")
library("dplyr")
library("lubridate")
library("forcats")
library("ggplot2")

tibbed <-
  read_csv2("06_nyindlagte_pr_vaccinationsstatus_pr_aldersgrp_pr_uge.csv")
tibbed %>%
  mutate(across(Aldersgruppe, as.factor)) %>%
  mutate(
    Aldersgruppe = fct_relevel(
      Aldersgruppe,
      "5-11",
      "12-15",
      "16-19",
      "20-39",
      "40-64",
      "65-79",
      "80+"
    )
  ) %>%
  mutate(across(Vaccinationsstatus, as.factor)) %>%
  mutate(
    Vaccinationsstatus = recode(
      Vaccinationsstatus,
      Uvaccinerede = "unvac",
      `P�begyndt vaccination` = "first_vac",
      `Forventet fuld effekt` = "twice_vac",
      `F�rste revaccination` = "boosted"
    )
  ) %>%
  mutate(
    Vaccinationsstatus = fct_relevel(
      Vaccinationsstatus,
      "unvac",
      "first_vac",
      "twice_vac",
      "boosted"
    )
  ) %>%
  filter(
    .,
    Uge == "2022-W01" |
      Uge == "2022-W02" |
      Uge == "2022-W03" |
      Uge == "2022-W04" |
      Uge == "2022-W05" | 
      Uge == "2022-W06" | 
      Uge == "2022-W07"
  ) %>%
  mutate(Boost_status = fct_collapse(
    Vaccinationsstatus,
    non_boost = c("unvac",
                  "first_vac",
                  "twice_vac")
  )) %>%
  group_by(Aldersgruppe,
           Boost_status,
           `Nyindlagte pr. 100.000`) %>%
  
  ggplot(.,
         aes(fill = Boost_status,
             x = Aldersgruppe,
             y = `Nyindlagte pr. 100.000`)) +
  geom_bar(position = "dodge", stat = 'identity') +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(angle = 90),
    aspect.ratio = 1
  ) +
  labs(y = "Per 100k", 
       x = "Age category", 
       title = "Hospital admissions in Denmark") +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap( ~ Uge)
