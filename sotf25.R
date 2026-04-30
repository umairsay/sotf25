# State of the Football 2025
# 10 charts · 40 teams · 689 players · La Liga & Premier League 24/25

library(tidyverse)
library(ggrepel)
library(car)
library(factoextra)
library(broom)
library(ggbeeswarm)

# --- DATA ---

la_teams   <- read_csv("/Users/umairs/Downloads/LaLigaPremTeamsPlayers2425/TeamStats24 - LaLiga24.csv")
pr_teams   <- read_csv("/Users/umairs/Downloads/LaLigaPremTeamsPlayers2425/TeamStats24 - Prem24.csv")
la_players <- read_csv("/Users/umairs/Downloads/LaLigaPremTeamsPlayers2425/PlayerStats24 - LaLiga24.csv")
pr_players <- read_csv("/Users/umairs/Downloads/LaLigaPremTeamsPlayers2425/PlayerStats24 - Prem24.csv")

la_teams$league <- "La Liga"; pr_teams$league <- "Prem"
la_players$league <- "La Liga"; pr_players$league <- "Prem"

teams   <- bind_rows(la_teams, pr_teams)
players <- bind_rows(la_players, pr_players)

teams   <- teams   %>% rename_with(~ str_replace_all(., "%", "pct"))
players <- players %>% rename_with(~ str_replace_all(., "%", "pct"))

team_bridge <- tribble(
  ~Team_player,          ~Team,
  "Real Madrid",         "Real",
  "Celta Vigo",          "Celta",
  "Real Betis",          "Betis",
  "Athletic Club",       "Athletic",
  "Real Valladolid",     "Valladolid",
  "Deportivo Alaves",    "Alaves",
  "Rayo Vallecano",      "Rayo",
  "Real Sociedad",       "Sociedad",
  "Crystal Palace",      "C. Palace",
  "Tottenham",           "Spurs",
  "Nottingham Forest",   "Nottm Forest"
)

players <- players %>%
  left_join(team_bridge, by = c("Team" = "Team_player")) %>%
  mutate(Team = coalesce(Team.y, Team)) %>%
  select(-Team.y)

teams <- teams %>%
  mutate(
    press_balance  = OLOS - LOS,
    line_asymmetry = DLINE - ODLINE,
    conversion_gap = xGDiff - xTDiff
  )


# --- CLUSTERING & PCA ---

clust_vars   <- c("FTILT", "xGDiff", "DLINE", "LOS", "BUILD")
teams_scaled <- teams %>%
  column_to_rownames("Team") %>%
  select(all_of(clust_vars)) %>%
  scale()

dist_mat   <- dist(teams_scaled, method = "euclidean")
hclust_fit <- hclust(dist_mat, method = "ward.D2")

teams <- teams %>%
  mutate(cluster = as.factor(cutree(hclust_fit, k = 4)))

pca_fit    <- prcomp(teams_scaled, scale. = FALSE)
pca_coords <- as_tibble(pca_fit$x[, 1:2]) %>%
  mutate(Team = teams$Team, league = teams$league, cluster = teams$cluster)


# --- PLAYER Z-SCORES ---

player_vars <- c("xTP", "xTC", "PRCV", "PCpct", "PPAS", "PCRY",
                 "HDA", "xG", "PBX", "AERA", "AERpct", "TKLA", "TKLpct")

players_z <- players %>%
  group_by(POS) %>%
  mutate(across(all_of(player_vars),
                ~ (. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE),
                .names = "z_{.col}")) %>%
  ungroup() %>%
  mutate(
    creativity      = z_xTP + z_PRCV + z_PCpct,
    threat_creation = z_xTC + z_PCRY,
    pressing        = z_HDA,
    complete        = z_xTP + z_HDA + z_PCRY,
    directness      = z_PBX + z_PCRY,
    goal_threat     = z_xG
  )

players_teams <- players_z %>%
  left_join(
    teams %>% select(Team, league, FTILT, xGDiff, DLINE, BUILD, LOS,
                     OLOS, ODLINE, xTDiff, cluster, press_balance),
    by = c("Team", "league")
  )

pos_labels <- c("ATT" = "Attackers", "DEF" = "Defenders", "MID" = "Midfielders")


# --- FLUIDITY ---

med_ppas_att <- median(players$PPAS[players$POS == "ATT"])
med_hda_att  <- median(players$HDA[players$POS == "ATT"])
med_xtp_def  <- median(players$xTP[players$POS == "DEF"])
med_pcry_def <- median(players$PCRY[players$POS == "DEF"])
med_xg_mid   <- median(players$xG[players$POS == "MID"])
med_hda_mid  <- median(players$HDA[players$POS == "MID"])

players <- players %>%
  mutate(is_fluid = case_when(
    POS == "ATT" & (PPAS > med_ppas_att * 1.5 | HDA > med_hda_att * 1.5) ~ TRUE,
    POS == "DEF" & (xTP > med_xtp_def * 1.5 | PCRY > med_pcry_def * 1.5) ~ TRUE,
    POS == "MID" & (xG > med_xg_mid * 2.0 | HDA > med_hda_mid * 1.5) ~ TRUE,
    TRUE ~ FALSE
  ))

fluidity <- players %>%
  group_by(Team, league) %>%
  summarise(fluid_n = sum(is_fluid), total_n = n(),
            fluidity_pct = fluid_n / total_n * 100, .groups = "drop") %>%
  left_join(teams %>% select(Team, league, FTILT, xGDiff, cluster),
            by = c("Team", "league"))


# --- UNIQUENESS ---

prem_idx   <- which(teams$league == "Prem")
laliga_idx <- which(teams$league == "La Liga")

prem_centroid   <- colMeans(teams_scaled[prem_idx, ])
laliga_centroid <- colMeans(teams_scaled[laliga_idx, ])

teams$centroid_dist <- NA_real_
for (i in seq_len(nrow(teams))) {
  row_scaled <- teams_scaled[i, ]
  if (teams$league[i] == "Prem") {
    teams$centroid_dist[i] <- sqrt(sum((row_scaled - prem_centroid)^2))
  } else {
    teams$centroid_dist[i] <- sqrt(sum((row_scaled - laliga_centroid)^2))
  }
}


# --- REGRESSIONS ---

prem_std <- lm(scale(xGDiff) ~ scale(FTILT) + scale(DLINE) + scale(BUILD) +
                 scale(LOS) + scale(OLOS),
               data = teams %>% filter(league == "Prem"))

laliga_std <- lm(scale(xGDiff) ~ scale(FTILT) + scale(DLINE) + scale(BUILD) +
                   scale(LOS) + scale(OLOS),
                 data = teams %>% filter(league == "La Liga"))


# --- PALETTE ---

col_ll <- "#e8c84a"; col_pr <- "#6baed6"
shp_ll <- 17; shp_pr <- 16

base_theme <- theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), legend.position = "top")


# ============================================================
# 1 / 10  TACTICAL DISTRIBUTIONS
# ============================================================

tac_vars   <- c("FTILT", "xGDiff", "DLINE", "LOS", "BUILD", "POS")
tac_labels <- c(
  FTILT  = "Territory\n(% of final-third touches)",
  xGDiff = "Chance Quality\n(xG created minus allowed)",
  DLINE  = "How High They Defend\n(metres from own goal)",
  LOS    = "Turnovers in Danger\n(lower = safer on the ball)",
  BUILD  = "Passing Out From the Back\n(completion %)",
  POS    = "Possession %"
)

teams %>%
  pivot_longer(all_of(tac_vars), names_to = "variable", values_to = "value") %>%
  mutate(variable = factor(variable, levels = names(tac_labels), labels = tac_labels)) %>%
  ggplot(aes(x = value, fill = league, colour = league)) +
  geom_density(alpha = 0.35, linewidth = 0.8) +
  facet_wrap(~ variable, scales = "free") +
  scale_fill_manual(values = c("La Liga" = col_ll, "Prem" = col_pr)) +
  scale_colour_manual(values = c("La Liga" = col_ll, "Prem" = col_pr)) +
  labs(title = "Tactical Distribution: La Liga vs Premier League 24/25",
       subtitle = "Six dimensions, two leagues. Each curve shows how teams spread across that dimension.",
       x = NULL, y = NULL, fill = NULL, colour = NULL) +
  base_theme +
  theme(strip.text = element_text(face = "bold", size = 9))


# ============================================================
# 2 / 10  TACTICAL MAP
# ============================================================

ggplot(pca_coords, aes(x = PC1, y = PC2)) +
  geom_point(aes(colour = cluster, shape = league), size = 3.5, alpha = 0.9) +
  geom_text_repel(aes(label = Team), size = 3, max.overlaps = 30,
                  segment.colour = "grey70") +
  scale_shape_manual(values = c("La Liga" = shp_ll, "Prem" = shp_pr)) +
  labs(title = "40 Teams in Tactical Space",
       subtitle = "Teams that are close together play similarly. Distance means difference.",
       x = "← More dominant",
       y = "More pressing & intensity →",
       colour = "Cluster", shape = "League") +
  base_theme + theme(legend.position = "right")


# ============================================================
# 3 / 10  WHAT EACH LEAGUE REWARDS
# ============================================================

coef_compare <- bind_rows(
  tidy(prem_std) %>%
    filter(term != "(Intercept)") %>%
    mutate(league = "Prem", term = str_remove_all(term, "scale\\(|\\)")),
  tidy(laliga_std) %>%
    filter(term != "(Intercept)") %>%
    mutate(league = "La Liga", term = str_remove_all(term, "scale\\(|\\)"))
) %>%
  mutate(term = case_match(term,
                           "FTILT" ~ "Territory",
                           "BUILD" ~ "Playing out from the back",
                           "DLINE" ~ "Defensive line height",
                           "LOS"   ~ "Turnovers in danger",
                           "OLOS"  ~ "Pressing effectiveness"
  ))

ggplot(coef_compare, aes(x = estimate, y = reorder(term, abs(estimate)),
                         colour = league)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(size = 3.5, position = position_dodge(width = 0.5)) +
  geom_errorbarh(aes(xmin = estimate - 1.96 * std.error,
                     xmax = estimate + 1.96 * std.error),
                 height = 0.2, position = position_dodge(width = 0.5)) +
  scale_colour_manual(values = c("La Liga" = col_ll, "Prem" = col_pr)) +
  labs(title = "What Each League Rewards",
       subtitle = "How much each tactical dimension helps or hurts a team's results",
       x = "Effect on results →", y = NULL, colour = NULL) +
  base_theme


# ============================================================
# 4 / 10  BUILDUP VS RESULTS
# The picture version of the most important finding.
# Prem trendline slopes down. La Liga is flat.
# ============================================================

build_labels <- c("Barcelona", "Real", "Arsenal", "Man City",
                  "Liverpool", "Getafe", "Ipswich", "Southampton",
                  "C. Palace", "Bournemouth", "Athletic", "Valladolid")

teams %>%
  ggplot(aes(x = BUILD, y = xGDiff)) +
  geom_hline(yintercept = 0, linetype = "dotted", colour = "grey50") +
  geom_point(aes(colour = league, shape = league), size = 3.5, alpha = 0.85) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
              colour = "grey40", linewidth = 0.8, linetype = "dashed") +
  geom_text_repel(data = teams %>% filter(Team %in% build_labels),
                  aes(label = Team), size = 2.8, max.overlaps = 20,
                  segment.colour = "grey60") +
  scale_colour_manual(values = c("La Liga" = col_ll, "Prem" = col_pr)) +
  scale_shape_manual(values = c("La Liga" = shp_ll, "Prem" = shp_pr)) +
  facet_wrap(~ league, scales = "free_x") +
  labs(title = "Buildup Quality vs Results",
       subtitle = "In the Prem, playing out from the back is associated with worse outcomes. In La Liga, it is not.",
       x = "Buildup pass completion % →",
       y = "Better results →",
       colour = NULL, shape = NULL) +
  base_theme +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold"))


# ============================================================
# 5 / 10  PLAYER ARCHETYPES
#
# Label philosophy — balance of expected and unexpected:
#
#   Pedri          — anchor. Top right. Confirms the metric works.
#   Lamine Yamal   — top right, 17 years old in elite company
#   Jérémy Doku    — far right, pure carrier, expected there
#   Erling Haaland — bottom left, pure finisher — most prolific striker
#                    in the Prem scores almost nothing here because he
#                    positions, times runs, and finishes. Nothing else.
#   Jude Bellingham — top LEFT not top right. One of the biggest names
#                    in football. People expect him far right. He plays
#                    as an advanced #10, scores from runs, doesn't carry
#                    or cross. The metric is honest about what he does.
#   Óscar Mingueza — right back in midfielder space. Nobody expects that.
#   Bryan Zaragoza — alone bottom right. Bayern loanee at Osasuna.
#                    Dribbles to danger constantly. No passing creativity.
# ============================================================

label_arch <- players_z %>%
  filter(Player %in% c(
    "Pedri",
    "Lamine Yamal",
    "Jérémy Doku",
    "Erling Haaland",
    "Jude Bellingham",
    "Óscar Mingueza",
    "Bryan Zaragoza"
  ))

ggplot(players_z, aes(x = threat_creation, y = creativity)) +
  geom_point(aes(colour = league, shape = POS), alpha = 0.35, size = 2) +
  geom_point(data = label_arch,
             aes(colour = league, shape = POS), alpha = 0.95, size = 3) +
  geom_text_repel(data = label_arch, aes(label = Player), size = 2.7,
                  max.overlaps = 20, segment.colour = "grey60",
                  min.segment.length = 0.3, box.padding = 0.5,
                  point.padding = 0.3, force = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
  scale_colour_manual(values = c("La Liga" = col_ll, "Prem" = col_pr)) +
  scale_shape_manual(values = c("ATT" = 17, "MID" = 16, "DEF" = 15)) +
  labs(title = "Player Archetypes: Creativity vs Threat Creation",
       subtitle = "Each dot is a player. Scores are compared to others in the same position.",
       x = "More threat creation →",
       y = "More creativity →",
       colour = "League", shape = "Position") +
  base_theme + theme(legend.position = "right")


# ============================================================
# 6 / 10  SYSTEM vs PLAYER
#
# Label philosophy — balance of expected and unexpected:
#
#   Pedri          — MID, anchor. Highest above line at most dominant club.
#   Lamine Yamal   — ATT, above line, 17 at Barcelona — expected there
#   Erling Haaland — ATT, below line even at Man City. Pure finisher.
#   Nicolas Pépé   — ATT, Arsenal's then-record £72m signing. Complete
#                    flop. Written off. Now above trendline at Villarreal.
#   Daley Blind    — DEF, creative at mid-table Girona. Data found him.
#   Tom Cairney    — MID, above his system quietly at Fulham at 33.
# ============================================================

label_sys <- players_teams %>%
  filter(Player %in% c(
    "Pedri",
    "Lamine Yamal",
    "Erling Haaland",
    "Nicolas Pépé",
    "Daley Blind",
    "Tom Cairney"
  ))

ggplot(players_teams, aes(x = FTILT, y = creativity)) +
  geom_point(aes(colour = league, shape = POS), alpha = 0.3, size = 1.8) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
              colour = "grey40", linewidth = 0.8, linetype = "dashed") +
  geom_point(data = label_sys,
             aes(colour = league, shape = POS), alpha = 0.95, size = 2.8) +
  geom_text_repel(data = label_sys, aes(label = Player),
                  size = 2.6, max.overlaps = 20, segment.colour = "grey60",
                  min.segment.length = 0.3, box.padding = 0.4,
                  point.padding = 0.3, force = 3) +
  scale_colour_manual(values = c("La Liga" = col_ll, "Prem" = col_pr)) +
  scale_shape_manual(values = c("ATT" = 17, "MID" = 16, "DEF" = 15)) +
  facet_wrap(~ POS, labeller = labeller(POS = pos_labels)) +
  labs(title = "Does the System Produce the Player?",
       subtitle = "How dominant a team is vs how creative its players are — split by position",
       x = "Team territorial dominance",
       y = "Individual creativity",
       colour = "League", shape = "Position") +
  base_theme +
  theme(legend.position = "right", strip.text = element_text(face = "bold"))


# ============================================================
# 7 / 10  CREATIVE MIDFIELDERS
#
# Label philosophy — balance of expected and unexpected:
#
#   Pedri          — far right La Liga, anchor
#   Luka Modric    — far right, 39 years old, still there
#   Martin Ødegaard — right of Prem body, expected there
#   Tom Cairney    — further right than Ødegaard. Nobody sees that coming.
#   Mauro Arambarri — far left at -4.23. Getafe identity in one dot.
#   Declan Rice    — 0.60. Arsenal Player of Season, £105m, considered
#                    world's best midfielder. The gap between reputation
#                    and position on this chart is the story.
# ============================================================

mid_data <- players_z %>% filter(POS == "MID")

mid_labels <- mid_data %>%
  filter(Player %in% c(
    "Pedri",
    "Luka Modric",
    "Martin Ødegaard",
    "Tom Cairney",
    "Mauro Arambarri",
    "Declan Rice"
  ))

ggplot(mid_data, aes(x = creativity, y = league, colour = league)) +
  geom_beeswarm(size = 2, alpha = 0.5, cex = 3) +
  geom_point(data = mid_labels,
             aes(colour = league), size = 3, alpha = 0.95) +
  geom_text_repel(
    data = mid_labels,
    aes(label = Player), size = 2.7, max.overlaps = 20,
    segment.colour = "grey60", direction = "x",
    nudge_y = 0.2, min.segment.length = 0.2,
    box.padding = 0.35, point.padding = 0.25, force = 3
  ) +
  scale_colour_manual(values = c("La Liga" = col_ll, "Prem" = col_pr)) +
  labs(title = "Creative Midfielders: La Liga vs Premier League",
       subtitle = "Every dot is a midfielder. La Liga's tail stretches further right.",
       x = "More creative →", y = NULL, colour = NULL) +
  base_theme +
  theme(legend.position = "none")


# ============================================================
# 8 / 10  POSITIONAL FLUIDITY
# ============================================================

fluid_labels <- c("Barcelona", "Man City", "Liverpool", "Arsenal",
                  "Ipswich", "Valladolid", "Southampton", "Leicester",
                  "Newcastle", "Brighton", "Getafe", "Atletico", "Real",
                  "Bournemouth", "Fulham", "Athletic", "Celta")

ggplot(fluidity, aes(x = fluidity_pct, y = FTILT)) +
  geom_point(aes(colour = league, shape = league), size = 3.5, alpha = 0.85) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
              colour = "grey40", linewidth = 0.8, linetype = "dashed") +
  geom_text_repel(data = fluidity %>% filter(Team %in% fluid_labels),
                  aes(label = Team), size = 2.8, max.overlaps = 25,
                  segment.colour = "grey60") +
  scale_colour_manual(values = c("La Liga" = col_ll, "Prem" = col_pr)) +
  scale_shape_manual(values = c("La Liga" = shp_ll, "Prem" = shp_pr)) +
  labs(title = "Positional Fluidity vs Territorial Dominance",
       subtitle = paste0("Squads where players break positional conventions dominate the pitch — r = ",
                         round(cor(fluidity$fluidity_pct, fluidity$FTILT, use = "complete.obs"), 3)),
       x = "% of squad breaking position →",
       y = "More territory →",
       colour = NULL, shape = NULL) +
  base_theme


# ============================================================
# 9 / 10  THE COMPLETE MIDFIELDER
# ============================================================

players_z %>%
  filter(POS == "MID") %>%
  slice_max(complete, n = 18) %>%
  mutate(Player = reorder(Player, complete)) %>%
  ggplot(aes(x = complete, y = Player)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey70", linewidth = 0.6) +
  geom_segment(aes(x = 0, xend = complete, y = Player, yend = Player,
                   colour = league), linewidth = 0.8, alpha = 0.6) +
  geom_point(aes(colour = league), size = 4) +
  scale_colour_manual(values = c("La Liga" = col_ll, "Prem" = col_pr)) +
  labs(title = "The Complete Midfielder: Create, Press, Carry",
       subtitle = "Top 18 midfielders across both leagues. Longer bar = more complete.",
       x = "More complete →",
       y = NULL, colour = "League") +
  base_theme +
  theme(legend.position = "right", panel.grid.major.y = element_line(colour = "grey93"))


# ============================================================
# 10 / 10  UNIQUENESS vs PERFORMANCE
# ============================================================

diff_labels <- c("Barcelona", "Arsenal", "Man City", "Liverpool",
                 "Getafe", "Atletico", "Real", "Everton",
                 "Bournemouth", "Spurs", "C. Palace", "Ipswich",
                 "Southampton", "Leicester", "Nottm Forest",
                 "Valladolid", "Athletic", "Fulham", "Brighton",
                 "Chelsea", "Celta", "Villarreal")

ggplot(teams, aes(x = centroid_dist, y = xGDiff)) +
  geom_hline(yintercept = 0, linetype = "dotted", colour = "grey50") +
  geom_point(aes(colour = league, shape = league), size = 3.5, alpha = 0.85) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = TRUE,
              colour = "grey40", linewidth = 0.8, linetype = "dashed") +
  geom_text_repel(data = teams %>% filter(Team %in% diff_labels),
                  aes(label = Team), size = 2.8, max.overlaps = 30,
                  segment.colour = "grey60") +
  scale_colour_manual(values = c("La Liga" = col_ll, "Prem" = col_pr)) +
  scale_shape_manual(values = c("La Liga" = shp_ll, "Prem" = shp_pr)) +
  labs(title = "Uniqueness vs Performance",
       subtitle = "How tactically different a team is from their league's average, vs how well they performed",
       x = "More unique →",
       y = "Better results →",
       colour = NULL, shape = NULL) +
  base_theme


# ============================================================
# PROSE AMMUNITION (console only)
# ============================================================

levene_results <- map_dfr(tac_vars, function(var) {
  test <- leveneTest(teams[[var]] ~ as.factor(teams$league))
  tibble(variable = var, F = round(test$`F value`[1], 3), p = round(test$`Pr(>F)`[1], 3))
})
print(levene_results)

teams %>% select(Team, league, OLOS, LOS, press_balance) %>% arrange(desc(press_balance)) %>% print(n = 10)
teams %>% select(Team, league, ODLINE, FTILT) %>% arrange(ODLINE) %>% print(n = 10)
teams %>% select(Team, league, line_asymmetry, FTILT) %>% arrange(desc(line_asymmetry)) %>% print(n = 10)
teams %>% select(Team, league, xGDiff, xTDiff, conversion_gap) %>% arrange(desc(conversion_gap)) %>% print(n = 10)

players %>% group_by(Team, league) %>%
  summarise(mean_aera = round(mean(AERA, na.rm = TRUE), 2), .groups = "drop") %>%
  arrange(desc(mean_aera)) %>% print(n = 10)

players_teams %>% group_by(POS) %>%
  group_map(~ tidy(lm(creativity ~ FTILT, data = .x))) %>%
  bind_rows(.id = "POS") %>% filter(term == "FTILT") %>%
  select(POS, estimate, std.error, p.value) %>% print()

players_teams %>% group_by(cluster) %>%
  summarise(n = n(), mean_creative = round(mean(creativity), 2),
            mean_threat = round(mean(threat_creation), 2)) %>% print()

players_teams %>% filter(cluster %in% c("3","4"), creativity > 3) %>%
  select(Player, Team, league, POS, creativity, FTILT) %>%
  arrange(desc(creativity)) %>% print(n = 20)

quad <- lm(xGDiff ~ centroid_dist + I(centroid_dist^2), data = teams)
cat("Quadratic p:", round(tidy(quad)$p.value[2:3], 4), " R²:", round(summary(quad)$r.squared, 3), "\n")

cat("\nKey player scores:\n")
players_z %>%
  filter(Player %in% c("Erling Haaland", "Robert Lewandowski",
                       "Pedri", "Óscar Mingueza", "Trent Alexander-Arnold",
                       "Lamine Yamal", "Jérémy Doku", "Frenkie de Jong",
                       "Declan Rice", "Manuel Ugarte",
                       "Tom Cairney", "Luka Modric",
                       "Daley Blind", "Patrick Dorgu",
                       "Jude Bellingham", "Bryan Zaragoza",
                       "Nicolas Pépé", "Martin Ødegaard",
                       "Mauro Arambarri")) %>%
  select(Player, Team, POS, creativity, threat_creation, complete) %>%
  arrange(POS, desc(creativity)) %>%
  print(n = 25)