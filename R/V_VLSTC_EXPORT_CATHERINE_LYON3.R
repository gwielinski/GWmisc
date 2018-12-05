## df source
df <- reservation %>% filter(ResTypeService == "AuM",
                            ResDateDebutYear %in% c(2013:2018),
                            # ResDateDebutSaison == "Fall",
                            UserEmployer == FALSE,
                            permis_corporate == FALSE,
                            permis_employe == FALSE,
                            permis_event == FALSE) %>% 
  filter(ResDateDebutYW < as.Date("2018-02-25")) %>% 
  group_by(CustomerID2, ResDateDebutYear, ResDateDebutYW) %>% 
  summarise(n = n_distinct(autNoReservation))

## calcul de nombre de membres actifs
actifs <- df %>% 
  group_by(ResDateDebutYear) %>% 
  summarise(membre_actif = n_distinct(CustomerID2))

## statistiques descriptives
df %>% 
  left_join(actifs, by = "ResDateDebutYear") %>% 
  mutate(n = if_else(n <= as.integer(10), as.integer(10), n)) %>% 
  mutate(n = if_else(n >= as.integer(20), as.integer(20), n)) %>% 
  group_by(ResDateDebutYear, n, membre_actif) %>% 
  summarise(N = length(CustomerID2),
            nb_customer_unique = n_distinct(CustomerID2)) %>%
  group_by(ResDateDebutYear) %>% 
  mutate(NN = N / sum(N),
         NN_customer_unique = nb_customer_unique / membre_actif) %>% 
  write.table(., "clipboard", sep="\t", row.names=FALSE)

## Statistiques avancées
df$n_label <- "11_a_19"
df$n_label[df$n >= 20] <- "20_et_plus"
df$n_label[df$n <= 10] <- "10_et_moins"

df2 <- reservation %>% 
  filter(ResTypeService == "AuM",
         ResDateDebutYear %in% c(2013:2018),
         # ResDateDebutSaison == "Fall",
         UserEmployer == FALSE,
         permis_corporate == FALSE,
         permis_employe == FALSE,
         permis_event == FALSE) %>% 
  filter(ResDateDebutYW < as.Date("2018-02-25")) %>% 
  select(autNoReservation, CustomerID2, ResDateDebutMinute, ResDateDebutHeure, ResDateDebutWdayLabel, ResDateDebutYear, ResDateDebutYW)

df2$moment_temporel <- "autre"
df2$moment_temporel[df2$ResDateDebutHeure %in% c(7:9) & df2$ResDateDebutWdayLabel %in% c("Mon", "Tues", "Wed", "Thurs", "Fri")] <- "pointe_am"
df2$moment_temporel[df2$ResDateDebutHeure %in% c(16:18) & df2$ResDateDebutWdayLabel %in% c("Mon", "Tues", "Wed", "Thurs", "Fri")] <- "pointe_pm"
df2$moment_temporel[df2$ResDateDebutWdayLabel %in% c("Sat", "Sun")] <- "week_end"
df2$moment_temporel[df2$ResDateDebutHeure %in% c(19:23, 0, 1) & df2$ResDateDebutWdayLabel %in% c("Mon", "Tues", "Wed", "Thurs", "Fri")] <- "semaine_apres_19h"

df2 %>%
  left_join(df %>% select(-n), 
            by = c("CustomerID2", "ResDateDebutYear", "ResDateDebutYW")) %>% 
  group_by(ResDateDebutYear, n_label, moment_temporel) %>% 
  summarise(n_trip = n_distinct(autNoReservation)) %>% 
  group_by(ResDateDebutYear, n_label) %>% 
  mutate(n_trip_prop = n_trip / sum(n_trip)) %>% 
  write.table(., "clipboard", sep="\t", row.names=FALSE)

##################################

df2$moment_temporel <- "autre"
df2$moment_temporel[(df2$ResDateDebutMinute + df2$ResDateDebutHeure*60) >= 6*60 & (df2$ResDateDebutMinute + df2$ResDateDebutHeure*60) <= 9*60-1 & df2$ResDateDebutWdayLabel %in% c("Mon", "Tues", "Wed", "Thurs", "Fri")] <- "pointe_am"
df2$moment_temporel[(df2$ResDateDebutMinute + df2$ResDateDebutHeure*60) >= (15*60+30) & (df2$ResDateDebutMinute + df2$ResDateDebutHeure*60) <= (18*60+29) & df2$ResDateDebutWdayLabel %in% c("Mon", "Tues", "Wed", "Thurs", "Fri")] <- "pointe_pm"
df2$moment_temporel[df2$ResDateDebutWdayLabel %in% c("Sat", "Sun")] <- "week_end"
df2$moment_temporel[df2$ResDateDebutHeure %in% c(19:23, 0, 1) & df2$ResDateDebutWdayLabel %in% c("Mon", "Tues", "Wed", "Thurs", "Fri")] <- "semaine_apres_19h"

df2 %>%
  left_join(df %>% select(-n), 
            by = c("CustomerID2", "ResDateDebutYear", "ResDateDebutYW")) %>% 
  group_by(ResDateDebutYear, n_label, moment_temporel) %>% 
  summarise(n_trip = n_distinct(autNoReservation)) %>% 
  group_by(ResDateDebutYear, n_label) %>% 
  mutate(n_trip_prop = n_trip / sum(n_trip)) %>% 
  write.table(., "clipboard", sep="\t", row.names=FALSE)
  