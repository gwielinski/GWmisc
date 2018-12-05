## This script will export multiple graphs requested by Catherine
## Ce script vise à apporter des précisions sur la caractérisation des membres
# 0) Libraries
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

########################################################################################################### Graphique 1
library(scales)
g1 <- ggplot(cube_membre_aum %>% 
               filter(month(ResDateDebutYW) %in% c(1:9)) %>% 
               mutate(annee = factor(year(ResDateDebutYW))) %>% 
               filter(annee != "2018") %>% 
               group_by(annee, n) %>% 
               summarise(y = length(CustomerID2)) %>% 
               group_by(annee) %>% 
               mutate(Y = y / sum(y)) %>% 
               ungroup() %>% 
               mutate(n = if_else(n >= 10, 10, n)) %>% 
               group_by(annee, n) %>% 
               summarise(Y = sum(Y)) %>% 
               ungroup(),
             aes(x = n, y = Y, fill = annee)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.5, color = "black") +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 1)) +
  scale_x_continuous(breaks = 0:10) + 
  scale_y_continuous(labels = percent, breaks = 0:10/10) +
  theme() +
  labs(x = "Nombre d'emprunts par semaine", y = "Proportion des membre-semaine") +
  theme_bw(base_size = 16, base_family = "Helvetica")+
  ggtitle(label = "Proportion du nombre de membres-semaine selon \n le nombre d'emprunts dans une semaine (Auto-mobile)")+
  theme(legend.position = "bottom",
        legend.title = element_text(size = 16, face="bold"),
        strip.text = element_text(face="bold"),
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)
  )

svg(filename = ".\\Output\\VLSTC\\Lyon_MTL_G1.svg", width = 6*2, height = 4*2)
g1
dev.off()

########################################################################################################### Graphique 1 Qc
library(scales)
g1 <- ggplot(cube_membre_aum_qc %>% 
               filter(month(ResDateDebutYW) %in% c(1:9)) %>% 
               mutate(annee = factor(year(ResDateDebutYW))) %>% 
               filter(annee != "2018") %>% 
               group_by(annee, n) %>% 
               summarise(y = length(CustomerID2)) %>% 
               group_by(annee) %>% 
               mutate(Y = y / sum(y)) %>% 
               ungroup() %>% 
               mutate(n = if_else(n >= 10, 10, n)) %>% 
               group_by(annee, n) %>% 
               summarise(Y = sum(Y)) %>% 
               ungroup(),
             aes(x = n, y = Y, fill = annee)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.5, color = "black") +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 1)) +
  scale_x_continuous(breaks = 0:10) + 
  scale_y_continuous(labels = percent, breaks = 0:10/10) +
  theme() +
  labs(x = "Nombre d'emprunts par semaine", y = "Proportion des membre-semaine") +
  theme_bw(base_size = 16, base_family = "Helvetica")+
  ggtitle(label = "Proportion du nombre de membres-semaine selon \n le nombre d'emprunts dans une semaine (Auto-mobile Qc)")+
  theme(legend.position = "bottom",
        legend.title = element_text(size = 16, face="bold"),
        strip.text = element_text(face="bold"),
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)
  )

svg(filename = ".\\Output\\VLSTC\\Lyon_MTL_G1_qc.svg", width = 6*2, height = 4*2)
g1
dev.off()


########################################################################################################### Graphique 2
library(scales)
g2 <- ggplot(cube_membre_aum_direct %>% 
               filter(month(ResDateDebutYW) %in% c(1:9)) %>% 
               filter(year(ResDateDebutYW) %in% c(2016:2017)) %>% 
               mutate(annee = factor(year(ResDateDebutYW))) %>% 
               filter(annee != "2018") %>% 
               group_by(annee, n) %>% 
               summarise(y = length(ProviderNo)) %>% 
               group_by(annee) %>% 
               mutate(Y = y / sum(y)) %>% 
               ungroup() %>% 
               mutate(n = if_else(n >= 10, 10, n)) %>% 
               group_by(annee, n) %>% 
               summarise(Y = sum(Y)) %>% 
               ungroup(),
             aes(x = n, y = Y, fill = annee)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.5, color = "black") +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 1)) +
  scale_x_continuous(breaks = 0:10) + 
  scale_y_continuous(labels = percent, breaks = 0:10/10) +
  theme() +
  labs(x = "Nombre d'emprunts par semaine (déplacements directs seulement)", y = "Proportion des membres-semaine") +
  theme_bw(base_size = 16, base_family = "Helvetica")+
  ggtitle(label = "Proportion du nombre de membres-semaine selon le nombre d'emprunts \n qualifiés de déplacements directs dans une semaine (Auto-mobile)")+
  theme(legend.position = "bottom",
        legend.title = element_text(size = 16, face="bold"),
        strip.text = element_text(face="bold"),
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)
  )

svg(filename = ".\\Output\\VLSTC\\Lyon_MTL_G2.svg", width = 6*2, height = 4*2)
g2
dev.off()

########################################################################################################### Graphique 2 Qc
library(scales)
g2 <- ggplot(cube_membre_aum_direct_qc %>% 
               filter(month(ResDateDebutYW) %in% c(1:9)) %>% 
               filter(year(ResDateDebutYW) %in% c(2016:2017)) %>% 
               mutate(annee = factor(year(ResDateDebutYW))) %>% 
               filter(annee != "2018") %>% 
               group_by(annee, n) %>% 
               summarise(y = length(ProviderNo)) %>% 
               group_by(annee) %>% 
               mutate(Y = y / sum(y)) %>% 
               ungroup() %>% 
               mutate(n = if_else(n >= 10, 10, n)) %>% 
               group_by(annee, n) %>% 
               summarise(Y = sum(Y)) %>% 
               ungroup(),
             aes(x = n, y = Y, fill = annee)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.5, color = "black") +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 1)) +
  scale_x_continuous(breaks = 0:10) + 
  scale_y_continuous(labels = percent, breaks = 0:10/10) +
  theme() +
  labs(x = "Nombre d'emprunts par semaine (déplacement direct seulement)", y = "Proportion des membre-semaine") +
  theme_bw(base_size = 16, base_family = "Helvetica")+
  ggtitle(label = "Proportion du nombre de membre-semaine selon \n le nombre d'emprunts directs dans une semaine (Auto-mobile Qc)")+
  theme(legend.position = "bottom",
        legend.title = element_text(size = 16, face="bold"),
        strip.text = element_text(face="bold"),
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)
  )

svg(filename = ".\\Output\\VLSTC\\Lyon_MTL_G2_qc.svg", width = 6*2, height = 4*2)
g2
dev.off()

########################################################################################################### Graphique 3a

g3 <- ggplot(cube_membre_aum_month %>% 
               filter(year(ResDateDebutYM) %in% c(2015:2017)) %>% 
         mutate(actif = if_else(n > 0, TRUE, FALSE)) %>% 
         group_by(ResDateDebutYM, actif) %>% 
         summarise(N = n_distinct(CustomerID2)) %>% 
         group_by(ResDateDebutYM) %>% 
         mutate(N = N/sum(N)) %>% 
         filter(actif == TRUE) %>% 
         ungroup(),
       aes(x = ResDateDebutYM,
           y = N)) +
  geom_line() +
  coord_cartesian(ylim = c(0, 1)) +
  scale_y_continuous(labels = percent, breaks = 0:10/10) +
  scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +
  theme() +
  labs(x = "Période (mois)", y = "Proportion des membres qui sont actifs") +
  theme_bw(base_size = 16, base_family = "Helvetica")+
  ggtitle(label = "Évolution de la proportion des membres Auto-mobile actifs dans un mois")+
  theme(legend.position = "bottom",
        legend.title = element_text(size = 16, face="bold"),
        strip.text = element_text(face="bold"),
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)
  )

svg(filename = ".\\Output\\VLSTC\\Lyon_MTL_G3.svg", width = 6*2, height = 4*2)
g3
dev.off()

########################################################################################################### Graphique 3b

g3b <- ggplot(cube_membre_aum_direct_month %>% 
                filter(year(ResDateDebutYM) %in% c(2016:2017)) %>% 
               mutate(actif = if_else(n > 0, TRUE, FALSE)) %>% 
               group_by(ResDateDebutYM, actif) %>% 
               summarise(N = n_distinct(ProviderNo)) %>% 
               group_by(ResDateDebutYM) %>% 
               mutate(N = N/sum(N)) %>% 
               filter(actif == TRUE) %>% 
               ungroup(),
             aes(x = ResDateDebutYM,
                 y = N)) +
  geom_line() +
  coord_cartesian(ylim = c(0, 1)) +
  scale_y_continuous(labels = percent, breaks = 0:10/10) +
  scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +
  theme() +
  labs(x = "Période (mois)", y = "Proportion des membres qui sont actifs") +
  theme_bw(base_size = 16, base_family = "Helvetica")+
  ggtitle(label = "Évolution de la proportion des membres Auto-mobile actifs \n dans un mois (emprunts directs)")+
  theme(legend.position = "bottom",
        legend.title = element_text(size = 16, face="bold"),
        strip.text = element_text(face="bold"),
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)
  )

svg(filename = ".\\Output\\VLSTC\\Lyon_MTL_G3b.svg", width = 6*2, height = 4*2)
g3b
dev.off()

########################################################################################################### Graphique 3a Qc

g3 <- ggplot(cube_membre_aum_month_qc %>% 
               filter(year(ResDateDebutYM) %in% c(2015:2017)) %>% 
               mutate(actif = if_else(n > 0, TRUE, FALSE)) %>% 
               group_by(ResDateDebutYM, actif) %>% 
               summarise(N = n_distinct(CustomerID2)) %>% 
               group_by(ResDateDebutYM) %>% 
               mutate(N = N/sum(N)) %>% 
               filter(actif == TRUE) %>% 
               ungroup(),
             aes(x = ResDateDebutYM,
                 y = N)) +
  geom_line() +
  coord_cartesian(ylim = c(0, 1)) +
  scale_y_continuous(labels = percent, breaks = 0:10/10) +
  scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +
  theme() +
  labs(x = "Période (mois)", y = "Proportion des membres qui sont actifs") +
  theme_bw(base_size = 16, base_family = "Helvetica")+
  ggtitle(label = "Évolution de la proportion des membres Auto-mobile actifs dans un mois (Qc)")+
  theme(legend.position = "bottom",
        legend.title = element_text(size = 16, face="bold"),
        strip.text = element_text(face="bold"),
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)
  )

svg(filename = ".\\Output\\VLSTC\\Lyon_MTL_G3_qc.svg", width = 6*2, height = 4*2)
g3
dev.off()

########################################################################################################### Graphique 3b Qc

g3b <- ggplot(cube_membre_aum_direct_month_qc %>% 
                filter(year(ResDateDebutYM) %in% c(2016:2017)) %>% 
                mutate(actif = if_else(n > 0, TRUE, FALSE)) %>% 
                group_by(ResDateDebutYM, actif) %>% 
                summarise(N = n_distinct(ProviderNo)) %>% 
                group_by(ResDateDebutYM) %>% 
                mutate(N = N/sum(N)) %>% 
                filter(actif == TRUE) %>% 
                ungroup(),
              aes(x = ResDateDebutYM,
                  y = N)) +
  geom_line() +
  coord_cartesian(ylim = c(0, 1)) +
  scale_y_continuous(labels = percent, breaks = 0:10/10) +
  scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +
  theme() +
  labs(x = "Période (mois)", y = "Proportion des membres qui sont actifs") +
  theme_bw(base_size = 16, base_family = "Helvetica")+
  ggtitle(label = "Évolution de la proportion des membres Auto-mobile actifs \n dans un mois (Qc) (emprunts directs)")+
  theme(legend.position = "bottom",
        legend.title = element_text(size = 16, face="bold"),
        strip.text = element_text(face="bold"),
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)
  )

svg(filename = ".\\Output\\VLSTC\\Lyon_MTL_G3b_qc.svg", width = 6*2, height = 4*2)
g3b
dev.off()

########################################################################################################### Graphique 4
library(scales)
g4 <- ggplot(cube_membre_aum_month %>% 
               filter(year(ResDateDebutYM) %in% c(2015:2017)) %>% 
               mutate(actif = if_else(n > 0, TRUE, FALSE)) %>% 
               filter(actif == TRUE) %>% 
               group_by(ResDateDebutYM) %>% 
               summarise(moy_n = mean(n),
                         sd_n = sd(n)) %>% 
               ungroup(),
             aes(x = ResDateDebutYM,
                 y = moy_n)) +
  geom_errorbar(aes(ymin=if_else(moy_n-sd_n < 1, 1, moy_n-sd_n), ymax=moy_n+sd_n), colour="black") +
  geom_line() +
  coord_cartesian(ylim = c(0, 30)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +
  theme() +
  labs(x = "Période (mois)", y = "Moyenne du nombre d'emprunts") +
  theme_bw(base_size = 16, base_family = "Helvetica")+
  ggtitle(label = "Évolution du nombre moyen d'emprunts par membre actif")+
  theme(legend.position = "bottom",
        legend.title = element_text(size = 16, face="bold"),
        strip.text = element_text(face="bold"),
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)
  )

svg(filename = ".\\Output\\VLSTC\\Lyon_MTL_G4.svg", width = 6*2, height = 4*2)
g4
dev.off()
########################################################################################################### Graphique 4 QC
g4 <- ggplot(cube_membre_aum_month_qc %>% 
               filter(year(ResDateDebutYM) %in% c(2015:2017)) %>% 
               mutate(actif = if_else(n > 0, TRUE, FALSE)) %>% 
               filter(actif == TRUE) %>% 
               group_by(ResDateDebutYM) %>% 
               summarise(moy_n = mean(n),
                         sd_n = sd(n)) %>% 
               ungroup(),
             aes(x = ResDateDebutYM,
                 y = moy_n)) +
  geom_errorbar(aes(ymin=if_else(moy_n-sd_n < 1, 1, moy_n-sd_n), ymax=moy_n+sd_n), colour="black") +
  geom_line() +
  coord_cartesian(ylim = c(0, 30)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +
  theme() +
  labs(x = "Période (mois)", y = "Moyenne du nombre d'emprunts") +
  theme_bw(base_size = 16, base_family = "Helvetica")+
  ggtitle(label = "Évolution du nombre moyen d'emprunts par membre actif (Qc)")+
  theme(legend.position = "bottom",
        legend.title = element_text(size = 16, face="bold"),
        strip.text = element_text(face="bold"),
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)
  )

svg(filename = ".\\Output\\VLSTC\\Lyon_MTL_G4_qc.svg", width = 6*2, height = 4*2)
g4
dev.off()

########################################################################################################### Graphique 4b
g4b <- ggplot(cube_membre_aum_direct_month %>% 
                filter(year(ResDateDebutYM) %in% c(2016:2017)) %>% 
               mutate(actif = if_else(n > 0, TRUE, FALSE)) %>% 
               filter(actif == TRUE) %>% 
               group_by(ResDateDebutYM) %>% 
               summarise(moy_n = mean(n),
                         sd_n = sd(n)) %>% 
               ungroup(),
             aes(x = ResDateDebutYM,
                 y = moy_n)) +
  geom_errorbar(aes(ymin=if_else(moy_n-sd_n < 1, 1, moy_n-sd_n), ymax=moy_n+sd_n), colour="black") +
  geom_line() +
  #coord_cartesian(ylim = c(0, 30)) +
  coord_x_date(xlim = c(as.Date("2015-01-01"), as.Date("2018-01-01")), ylim = c(0, 30)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +
  theme() +
  labs(x = "Période (mois)", y = "Moyenne du nombre d'emprunts") +
  theme_bw(base_size = 16, base_family = "Helvetica")+
  ggtitle(label = "Évolution du nombre moyen d'emprunts directs par membre actif")+
  theme(legend.position = "bottom",
        legend.title = element_text(size = 16, face="bold"),
        strip.text = element_text(face="bold"),
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)
  )

svg(filename = ".\\Output\\VLSTC\\Lyon_MTL_G4b.svg", width = 6*2, height = 4*2)
g4b
dev.off()


########################################################################################################### Graphique 4b QC
g4b <- ggplot(cube_membre_aum_direct_month_qc %>% 
                filter(year(ResDateDebutYM) %in% c(2016:2017)) %>% 
                mutate(actif = if_else(n > 0, TRUE, FALSE)) %>% 
                filter(actif == TRUE) %>% 
                group_by(ResDateDebutYM) %>% 
                summarise(moy_n = mean(n),
                          sd_n = sd(n)) %>% 
                ungroup(),
              aes(x = ResDateDebutYM,
                  y = moy_n)) +
  geom_errorbar(aes(ymin=if_else(moy_n-sd_n < 1, 1, moy_n-sd_n), ymax=moy_n+sd_n), colour="black") +
  geom_line() +
  coord_cartesian(ylim = c(0, 30)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +
  theme() +
  labs(x = "Période (mois)", y = "Moyenne du nombre d'emprunts") +
  theme_bw(base_size = 16, base_family = "Helvetica")+
  ggtitle(label = "Évolution du nombre moyen d'emprunts directs par membre actif  (Qc)") +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 16, face="bold"),
        strip.text = element_text(face="bold"),
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)
  )

svg(filename = ".\\Output\\VLSTC\\Lyon_MTL_G4b_qc.svg", width = 6*2, height = 4*2)
g4b
dev.off()

########################################################################################################### Graphique 4c
g4c <- ggplot(cube_membre_aum_month,
              aes(x = factor(ResDateDebutYM),
                  y = dist_km,
                  group = factor(ResDateDebutYM))) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 350)) +
 # scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +
  theme() +
  labs(x = "Période (mois)", y = "Moyenne mensuelle") +
  theme_bw(base_size = 16, base_family = "Helvetica")+
  ggtitle(label = "Évolution de la distance totale moyenne mensuelle par membre actif")+
  theme(legend.position = "bottom",
        legend.title = element_text(size = 16, face="bold"),
        strip.text = element_text(face="bold"),
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)
  )

svg(filename = ".\\Output\\VLSTC\\Lyon_MTL_G4c.svg", width = 6*2, height = 4*2)
g4c
dev.off()
########################################################################################################### Graphique 4c QC
g4c <- ggplot(cube_membre_aum_month_qc,
              aes(x = factor(ResDateDebutYM),
                  y = dist_km,
                  group = factor(ResDateDebutYM))) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 350)) +
  theme() +
  labs(x = "Période (mois)", y = "Moyenne mensuelle") +
  theme_bw(base_size = 16, base_family = "Helvetica")+
  ggtitle(label = "Évolution de la distance totale moyenne mensuelle par membre actif (Qc)")+
  theme(legend.position = "bottom",
        legend.title = element_text(size = 16, face="bold"),
        strip.text = element_text(face="bold"),
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)
  )

svg(filename = ".\\Output\\VLSTC\\Lyon_MTL_G4c_qc.svg", width = 6*2, height = 4*2)
g4c
dev.off()

########################################################################################################### Graphique 5
pred_lsi_trip %>% 
  filter(year(ResDateDebutYM) %in% c(2015:2017)) %>% 
  filter(month(ResDateDebutYM) %in% c(9:11)) %>% 
  group_by(prediction, city) %>% 
  summarise(n = n_distinct(reservation_id)) %>% 
  group_by(city) %>% 
  mutate(N = n / sum(n)) %>% 
  ungroup()
  
########################################################################################################### Graphique 6
g6 <- ggplot(reservation %>% filter(ResTypeService == "AuM",
                                    ResDureeHr < 1,
                                    ResDureeHr > 0,
                                    ResDateDebutYear %in% c(2015:2017),
                                    ResDateDebutSaison == "Fall",
                                    UserEmployer == FALSE,
                                    permis_corporate == FALSE,
                                    permis_employe == FALSE,
                                    permis_event == FALSE) %>% 
               mutate(ResDateDebutYear = factor(ResDateDebutYear)) %>% 
               group_by(CustomerID2, ResDateDebutYear, ResDateDebutYM) %>%
               summarise(n = n_distinct(autNoReservation),
                         n_day = n_distinct(ResDateDebutYMD),
                         dist_km = sum(ResDistanceKm, na.rm = T),
                         duree_hr = sum(ResDureeHr, na.rm = T)) %>% 
               ungroup() %>% 
               # mutate(n = if_else(n >= 40, 40, as.double(n))) %>% 
               mutate(n_group = cut(n, 
                                    breaks = c(0, 1, 2, 3, 4,5,10,15,20,25,30,35,40,10000000000000),
                                    labels = c("1", "2", "3", "4", "5", "6-10", "11-15", "16-20", "21-25", "26-30", "31-35", "36-40", "40+"),
                                    ordered_result = TRUE)) %>% 
               group_by(ResDateDebutYear, n_group) %>% 
               summarise(N = length(CustomerID2)) %>% 
               group_by(ResDateDebutYear) %>% 
               mutate(NN = N / sum(N)) %>% 
               ungroup(),
              aes(x = n_group,
                  y = NN,
                  fill = ResDateDebutYear)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.5, color = "black") +
  coord_cartesian(ylim = c(0, 0.50)) +
 # scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(labels = percent, breaks = 0:5/10) +
  theme() +
  labs(x = "Nb d'emprunts mensuels", y = "Proportion des membres-mois") +
  theme_bw(base_size = 16, base_family = "Helvetica")+
  ggtitle(label = "Évolution du nombre d'emprunts mensuels par membre-mois") +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 16, face="bold"),
        strip.text = element_text(face="bold"),
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_fill_discrete(name = "Année")

svg(filename = ".\\Output\\VLSTC\\Lyon_MTL_G6.svg", width = 6*2, height = 4*2)
g6
dev.off()
########################################################################################################### Graphique 6b
g6b <- ggplot(reservation %>% filter(ResTypeService == "AuM",
                                    ResDureeHr < 1,
                                    ResDureeHr > 0,
                                    ResDateDebutYear %in% c(2015:2017),
                                    ResDateDebutSaison == "Fall",
                                    UserEmployer == FALSE,
                                    permis_corporate == FALSE,
                                    permis_employe == FALSE,
                                    permis_event == FALSE) %>% 
               mutate(ResDateDebutYear = factor(ResDateDebutYear)) %>% 
               group_by(CustomerID2, ResDateDebutYear, ResDateDebutYM) %>%
               summarise(n = n_distinct(autNoReservation),
                         n_day = n_distinct(ResDateDebutYMD),
                         dist_km = sum(ResDistanceKm, na.rm = T),
                         duree_hr = sum(ResDureeHr, na.rm = T)) %>% 
               ungroup() %>% 
                # mutate(n = if_else(n >= 40, 40, as.double(n))) %>% 
                mutate(n_day_group = cut(n_day, 
                                     breaks = c(0, 1, 2, 3, 4,5,10,15,20,25,10000000000000),
                                     labels = c("1", "2", "3", "4", "5", "6-10", "11-15", "16-20", "21-25", "25+"),
                                     ordered_result = TRUE)) %>% 
               group_by(ResDateDebutYear, n_day_group) %>% 
               summarise(N = length(CustomerID2)) %>% 
               group_by(ResDateDebutYear) %>% 
               mutate(NN = N / sum(N)) %>% 
               ungroup(),
             aes(x = n_day_group,
                 y = NN,
                 fill = ResDateDebutYear)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.5, color = "black") +
  coord_cartesian(ylim = c(0, 0.50)) +
#  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(labels = percent, breaks = 0:5/10) +
  theme() +
  labs(x = "Nb de jours actifs mensuels", y = "Proportion des membres-mois") +
  theme_bw(base_size = 16, base_family = "Helvetica")+
  ggtitle(label = "Évolution du nombre de jours actifs mensuels par membre-mois") +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 16, face="bold"),
        strip.text = element_text(face="bold"),
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_fill_discrete(name = "Année")

svg(filename = ".\\Output\\VLSTC\\Lyon_MTL_G6b.svg", width = 6*2, height = 4*2)
g6b
dev.off()

########################################################################################################### Graphique 6 QC
g6 <- ggplot(reservationQc %>% filter(ResTypeService == "AuM",
                                    ResDureeHr < 1,
                                    ResDureeHr > 0,
                                    ResDateDebutYear %in% c(2015:2017),
                                    ResDateDebutSaison == "Fall",
                                    UserEmployer == FALSE,
                                    permis_corporate == FALSE,
                                    permis_employe == FALSE,
                                    permis_event == FALSE) %>% 
               mutate(ResDateDebutYear = factor(ResDateDebutYear)) %>% 
               group_by(CustomerID2, ResDateDebutYear, ResDateDebutYM) %>%
               summarise(n = n_distinct(autNoReservation),
                         n_day = n_distinct(ResDateDebutYMD),
                         dist_km = sum(ResDistanceKm, na.rm = T),
                         duree_hr = sum(ResDureeHr, na.rm = T)) %>% 
               ungroup() %>% 
               # mutate(n = if_else(n >= 40, 40, as.double(n))) %>% 
               mutate(n_group = cut(n, 
                                    breaks = c(0, 1, 2, 3, 4,5,10,15,20,25,30,35,40,10000000000000),
                                    labels = c("1", "2", "3", "4", "5", "6-10", "11-15", "16-20", "21-25", "26-30", "31-35", "36-40", "40+"),
                                    ordered_result = TRUE)) %>% 
               group_by(ResDateDebutYear, n_group) %>% 
               summarise(N = length(CustomerID2)) %>% 
               group_by(ResDateDebutYear) %>% 
               mutate(NN = N / sum(N)) %>% 
               ungroup(),
             aes(x = n_group,
                 y = NN,
                 fill = ResDateDebutYear)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.5, color = "black") +
  coord_cartesian(ylim = c(0, 0.50)) +
#  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(labels = percent, breaks = 0:5/10) +
  theme() +
  labs(x = "Nb d'emprunts mensuels", y = "Proportion des membres-mois") +
  theme_bw(base_size = 16, base_family = "Helvetica")+
  ggtitle(label = "Évolution du nombre d'emprunts mensuels par membre-mois (Qc)") +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 16, face="bold"),
        strip.text = element_text(face="bold"),
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_fill_discrete(name = "Année")

svg(filename = ".\\Output\\VLSTC\\Lyon_MTL_G6_qc.svg", width = 6*2, height = 4*2)
g6
dev.off()
########################################################################################################### Graphique 6b QC
g6b <- ggplot(reservationQc %>% filter(ResTypeService == "AuM",
                                     ResDureeHr < 1,
                                     ResDureeHr > 0,
                                     ResDateDebutYear %in% c(2015:2017),
                                     ResDateDebutSaison == "Fall",
                                     UserEmployer == FALSE,
                                     permis_corporate == FALSE,
                                     permis_employe == FALSE,
                                     permis_event == FALSE) %>% 
                mutate(ResDateDebutYear = factor(ResDateDebutYear)) %>% 
                group_by(CustomerID2, ResDateDebutYear, ResDateDebutYM) %>%
                summarise(n = n_distinct(autNoReservation),
                          n_day = n_distinct(ResDateDebutYMD),
                          dist_km = sum(ResDistanceKm, na.rm = T),
                          duree_hr = sum(ResDureeHr, na.rm = T)) %>% 
                ungroup() %>% 
                # mutate(n = if_else(n >= 40, 40, as.double(n))) %>% 
                mutate(n_day_group = cut(n_day, 
                                         breaks = c(0, 1, 2, 3, 4,5,10,15,20,25,10000000000000),
                                         labels = c("1", "2", "3", "4", "5", "6-10", "11-15", "16-20", "21-25", "25+"),
                                         ordered_result = TRUE)) %>% 
                group_by(ResDateDebutYear, n_day_group) %>% 
                summarise(N = length(CustomerID2)) %>% 
                group_by(ResDateDebutYear) %>% 
                mutate(NN = N / sum(N)) %>% 
                ungroup(),
              aes(x = n_day_group,
                  y = NN,
                  fill = ResDateDebutYear)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.5, color = "black") +
  coord_cartesian(ylim = c(0, 0.50)) +
  #  scale_x_continuous(breaks = 1:10) +
  theme() +
  scale_y_continuous(labels = percent, breaks = 0:5/10) +
  labs(x = "Nb de jours actifs mensuels", y = "Proportion des membres-mois") +
  theme_bw(base_size = 16, base_family = "Helvetica")+
  ggtitle(label = "Évolution du nombre de jours actifs mensuels par membre-mois (QC)") +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 16, face="bold"),
        strip.text = element_text(face="bold"),
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_fill_discrete(name = "Année")

svg(filename = ".\\Output\\VLSTC\\Lyon_MTL_G6b_qc.svg", width = 6*2, height = 4*2)
g6b
dev.off()

########################################################################################################### Graphique 7
normalit<-function(m){
  (m - min(m))/(max(m)-min(m))
}
g7 <- ggplot(reservation %>% 
               mutate(first_aum_year = year(User2FirstDateAuM),
                      first_aum_month = month(User2FirstDateAuM)) %>% 
               filter(first_aum_year %in% 2015:2017) %>% 
               group_by(CustomerID2, first_aum_year, first_aum_month) %>% 
               summarise() %>% 
               group_by(first_aum_year, first_aum_month) %>% 
               summarise(n = n_distinct(CustomerID2)) %>% 
               ungroup() %>% 
               mutate(N = normalit(n)) %>% 
               mutate(first_aum_YM = as.Date(ISOdate(first_aum_year, first_aum_month, 1))),
             aes(x = first_aum_YM, y = N)
               ) +
  geom_line() +
  scale_x_date(date_breaks = "4 months") +
  theme() +
  labs(x = "Période (mois)", y = "Nombre de nouveaux membres mensuels (normalisé)") +
  theme_bw(base_size = 16, base_family = "Helvetica")+
  ggtitle(label = "Évolution du nombre de nouveaux membres Auto-mobile \n (date du 1er emprunt)") +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 16, face="bold"),
        strip.text = element_text(face="bold"),
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)
  )

svg(filename = ".\\Output\\VLSTC\\Lyon_MTL_g7.svg", width = 6*2, height = 4*2)
g7
dev.off()

########################################################################################################### Graphique 7 Qc
g7 <- ggplot(reservationQc %>% 
               mutate(first_aum_year = year(User2FirstDateAuM),
                      first_aum_month = month(User2FirstDateAuM)) %>% 
               filter(first_aum_year %in% 2015:2017) %>% 
               group_by(CustomerID2, first_aum_year, first_aum_month) %>% 
               summarise() %>% 
               group_by(first_aum_year, first_aum_month) %>% 
               summarise(n = n_distinct(CustomerID2)) %>% 
               ungroup() %>% 
               mutate(N = normalit(n)) %>% 
               mutate(first_aum_YM = as.Date(ISOdate(first_aum_year, first_aum_month, 1))),
             aes(x = first_aum_YM, y = N)
) +
  geom_line() +
  scale_x_date(date_breaks = "4 months") +
  theme() +
  labs(x = "Période (mois)", y = "Nombre de nouveaux membres mensuels (normalisé)") +
  theme_bw(base_size = 16, base_family = "Helvetica")+
  ggtitle(label = "Évolution du nombre de nouveaux membres Auto-mobile \n (date du 1er emprunt) (Qc)") +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 16, face="bold"),
        strip.text = element_text(face="bold"),
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)
  )

svg(filename = ".\\Output\\VLSTC\\Lyon_MTL_g7_qc.svg", width = 6*2, height = 4*2)
g7
dev.off()
########################################################################################################### Graphique 7b
normalit<-function(m){
  (m - min(m))/(max(m)-min(m))
}
g7b <- ggplot(reservation %>% 
                filter(ResDateDebutYear %in% 2015:2017) %>% 
               group_by(ResDateDebutYM) %>% 
               summarise(n = n_distinct(CustomerID2)) %>% 
               ungroup() %>% 
               mutate(N = normalit(n)),
             aes(x = ResDateDebutYM, y = N)
) +
  geom_line() +
  scale_x_date(date_breaks = "4 months") +
  theme() +
  labs(x = "Période (mois)", y = "Nombre de membres actifs mensuels (normalisé)") +
  theme_bw(base_size = 16, base_family = "Helvetica")+
  ggtitle(label = "Évolution du nombre de membres actifs Auto-mobile") +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 16, face="bold"),
        strip.text = element_text(face="bold"),
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)
  )

svg(filename = ".\\Output\\VLSTC\\Lyon_MTL_g7b.svg", width = 6*2, height = 4*2)
g7b
dev.off()

########################################################################################################### Graphique 7b Qc
g7b <- ggplot(reservationQc %>% 
                filter(ResDateDebutYear %in% 2015:2017) %>% 
                group_by(ResDateDebutYM) %>% 
                summarise(n = n_distinct(CustomerID2)) %>% 
                ungroup() %>% 
                mutate(N = normalit(n)),
              aes(x = ResDateDebutYM, y = N)
) +
  geom_line() +
  scale_x_date(date_breaks = "4 months") +
  theme() +
  labs(x = "Période (mois)", y = "Nombre de membres actifs mensuels (normalisé)") +
  theme_bw(base_size = 16, base_family = "Helvetica")+
  ggtitle(label = "Évolution du nombre de membres actifs Auto-mobile (Qc)") +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 16, face="bold"),
        strip.text = element_text(face="bold"),
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)
  )

svg(filename = ".\\Output\\VLSTC\\Lyon_MTL_g7b_qc.svg", width = 6*2, height = 4*2)
g7b
dev.off()

########################################################################################################### Graphique 7b
normalit<-function(m){
  (m - min(m))/(max(m)-min(m))
}
g7c <- ggplot(cube_membre_aum_month %>% 
                group_by(ResDateDebutYM) %>% 
                summarise(n = n_distinct(CustomerID2)) %>% 
                ungroup() %>% 
                mutate(N = normalit(n)),
              aes(x = ResDateDebutYM, y = N)
) +
  geom_line() +
  scale_x_date(date_breaks = "2 months") +
  theme() +
  labs(x = "Période (mois)", y = "Nombre de membres (normalisé)") +
  theme_bw(base_size = 16, base_family = "Helvetica")+
  ggtitle(label = "Évolution du nombre de membres disposés à effectuer un emprunt Auto-mobile  \n selon la méthode employée") +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 16, face="bold"),
        strip.text = element_text(face="bold"),
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)
  )

svg(filename = ".\\Output\\VLSTC\\Lyon_MTL_g7c.svg", width = 6*2, height = 4*2)
g7c
dev.off()

########################################################################################################### Graphique 7c Qc
g7c <- ggplot(cube_membre_aum_month_qc %>% 
                group_by(ResDateDebutYM) %>% 
                summarise(n = n_distinct(CustomerID2)) %>% 
                ungroup() %>% 
                mutate(N = normalit(n)),
              aes(x = ResDateDebutYM, y = N)
) +
  geom_line() +
  scale_x_date(date_breaks = "2 months") +
  theme() +
  labs(x = "Période (mois)", y = "Nombre de membres (normalisé)") +
  theme_bw(base_size = 16, base_family = "Helvetica")+
  ggtitle(label = "Évolution du nombre de membres disposés à effectuer un emprunt Auto-mobile \n selon la méthode employée (Qc)") +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 16, face="bold"),
        strip.text = element_text(face="bold"),
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)
  )

svg(filename = ".\\Output\\VLSTC\\Lyon_MTL_g7c_qc.svg", width = 6*2, height = 4*2)
g7c
dev.off()
