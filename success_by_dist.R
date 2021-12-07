library(dplyr)
library(ggplot2)
library(mgcv)

#Load all nfl plays since 2010
pbp <- nflfastR::load_pbp(2010:2021)

#Filter for just kick data
kicks <- pbp %>%
  filter((field_goal_attempt == 1 | extra_point_attempt == 1),
         season_type == "REG",
         (is.na(field_goal_result) | field_goal_result != "blocked"),
         (is.na(extra_point_result) | !(extra_point_result %in% c("blocked", "aborted")))) %>%
  select(game_id,
         play_id,
         game_half,
         home_team,
         away_team,
         posteam,
         score_differential,
         half_seconds_remaining,
         game_seconds_remaining,
         spread_line,
         down,
         ydstogo,
         yardline_100,
         posteam_timeouts_remaining,
         defteam_timeouts_remaining,
         home_opening_kickoff,
         field_goal_attempt,
         extra_point_attempt,
         field_goal_result,
         extra_point_result,
         yardline_100,
         kick_distance,
         season_type,
         play_type,
         desc)

#Assign win probability estimate if the kick is good
kicks_good <- kicks %>%
  mutate(posteam = case_when(posteam == home_team ~ away_team,
                             TRUE ~ home_team), #switch possession to other team after kick
         new_half_seconds_remaining = case_when(field_goal_attempt == 1 ~ pmax(half_seconds_remaining - 5, 0),
                                                TRUE ~ half_seconds_remaining), #Guesstimate that a field goal takes 5 seconds off the clock
         game_seconds_remaining = game_seconds_remaining - (half_seconds_remaining - new_half_seconds_remaining), #adjust game time remaining accordingly
         half_seconds_remaining = new_half_seconds_remaining, #replace variable name
         down = 1,
         ydstogo = 10,
         yardline_100 = 75, #assume following kickoff goes for touchback
         new_posteam_timeouts_remaining = defteam_timeouts_remaining,
         defteam_timeouts_remaining = posteam_timeouts_remaining,
         posteam_timeouts_remaining = new_posteam_timeouts_remaining,
         score_differential = -1.0 * case_when(field_goal_attempt == 1 ~ score_differential + 3,
                                               TRUE ~ score_differential + 1), #multiply by negative 1 to switch team perspective
         receive_2h_ko = case_when(game_half == "Half1" & home_opening_kickoff == 1 & posteam == away_team ~ 1,
                                   game_half == "Half1" & home_opening_kickoff == 0 & posteam == home_team ~ 1,
                                   TRUE ~ 0)) %>%
  nflfastR::calculate_win_probability() %>%
  mutate(wp = case_when(game_seconds_remaining == 0 & score_differential < 0 ~ 0.01,
                        game_seconds_remaining == 0 & score_differential > 0 ~ 0.99,
                        TRUE ~ wp), #adjustment for game ending kicks
         wp_good = 1.0 - wp) %>% #flip to kicking team perspective
  select(game_id,
         play_id,
         wp_good)


#Assign win probability estimate if the kick is no good
kicks_bad <- kicks %>%
  mutate(posteam = case_when(posteam == home_team ~ away_team,
                             TRUE ~ home_team), #switch possession to other team after kick
         new_half_seconds_remaining = case_when(field_goal_attempt == 1 ~ pmax(half_seconds_remaining - 5, 0),
                                                TRUE ~ half_seconds_remaining), #Guesstimate that a field goal takes 5 seconds off the clock
         game_seconds_remaining = game_seconds_remaining - (half_seconds_remaining - new_half_seconds_remaining), #adjust game time remaining accordingly
         half_seconds_remaining = new_half_seconds_remaining, #replace variable name
         down = 1,
         ydstogo = 10,
         yardline_100 = case_when(field_goal_attempt == 1 ~ 110 - kick_distance, #team takes over from spot of kick
                                  TRUE ~ 75),
         new_posteam_timeouts_remaining = defteam_timeouts_remaining,
         defteam_timeouts_remaining = posteam_timeouts_remaining,
         posteam_timeouts_remaining = new_posteam_timeouts_remaining,
         score_differential = -1.0 * score_differential,
         receive_2h_ko = case_when(game_half == "Half1" & home_opening_kickoff == 1 & posteam == away_team ~ 1,
                                   game_half == "Half1" & home_opening_kickoff == 0 & posteam == home_team ~ 1,
                                   TRUE ~ 0)) %>%
  nflfastR::calculate_win_probability() %>%
  mutate(wp = case_when(game_seconds_remaining == 0 & score_differential < 0 ~ 0.01,
                        game_seconds_remaining == 0 & score_differential > 0 ~ 0.99,
                        TRUE ~ wp), #adjustment for game ending kicks
         wp_bad = 1.0 - wp) %>% #Flip to kicking team perspective
  select(game_id,
         play_id,
         wp_bad)

#Join together and calculate win probability swing
kicks_joined <- kicks %>%
  inner_join(kicks_good, by = c("game_id", "play_id")) %>%
  inner_join(kicks_bad, by = c("game_id", "play_id")) %>%
  mutate(wp_swing = pmax(wp_good - wp_bad, 0),  #There's some really interesting behavior in blowouts
         success = case_when(field_goal_attempt == 1 & field_goal_result == "made" ~ 1,
                             extra_point_attempt == 1 & extra_point_result == "good" ~ 1,
                             TRUE ~ 0))

#Bin kick success rate by every 5 yds
simple_bins <- kicks_joined %>%
  mutate(kick_dist_5yd = ceiling(kick_distance / 5) * 5) %>%
  group_by(kick_dist_5yd) %>%
  summarize(n = n(),
            success = mean(success)) %>%
  ungroup()

#Build simple binomial gam to smooth out success rate
kick_dist_gam <- gam(formula = as.formula(success ~ kick_distance),
                     data = kicks_joined,
                     family = binomial())

#Create df of kick distances to use with our gam
smooth_df <- data.frame(kick_distance = seq(0, 80, 0.01))
#Initialize column
smooth_df$success <- NA
smooth_df$success <- predict(kick_dist_gam, newdata = smooth_df, type = "response")


ggplot(simple_bins, aes(x = kick_dist_5yd, y = success)) +
  geom_point() +
  geom_line() +
  theme_minimal()

#We want to extend the left bin to cover all the way to 0 yd line
bins_left <- simple_bins %>%
  filter(kick_dist_5yd == 20)
bins_right <- simple_bins %>%
  filter(kick_dist_5yd != 20)


ten_yd_lines <- data.frame(x = seq(0, 100, 10),
                           xend = seq(0, 100, 10),
                           y = rep(0, 11),
                           yend = rep(160/3, 11))
five_yd_lines <- data.frame(x = seq(5, 95, 10),
                           xend = seq(5, 95, 10),
                           y = rep(0, 10),
                           yend = rep(160/3, 10))
hash_x <- 1:99

#Alright now let's make a football field
#Decided to keep everything in terms of yards, which had positives and negatives
#Biggest negative is that the width of a football field is 160/3 yds, so that'll show up in the code a lot
ggplot() +
  #green sideline rectangle and gray field rectangle
  geom_rect(aes(xmin = -12, xmax = 112, ymin = -2, ymax = 2 + 160/3), color = "white", fill = "forestgreen") +
  geom_rect(aes(xmin = -10, xmax = 110, ymin = 0, ymax = 160/3), color = "white", fill = "gray") +
  #Major and minor yard lines
  geom_segment(data = ten_yd_lines, aes(x = x, xend = xend, y = y, yend = yend), color = "white", size = 1.2) +
  geom_segment(data = five_yd_lines, aes(x = x, xend = xend, y = y, yend = yend), color = "white", size = 0.8) +
  #4 sets of hash marks
  geom_segment(aes(x = rep(hash_x, 4), xend = rep(hash_x, 4), 
                   y = c(rep(0, 99), rep(70.75/3, 99), rep(160/3 - 70.75/3 - 2/3, 99), rep(160/3 - 2/3, 99)),
                   yend = c(rep(2/3, 99), rep(70.75/3 + 2/3, 99), rep(160/3 - 70.75/3, 99), rep(160/3, 99))),
               color = "white", size = 0.5) +
  #plot the yard line numbers
  annotate("text", label = c(1:5, 4:2), x = seq(10, 80, 10) - 0.25, y = rep(12, 8), hjust = 1, vjust = 0, color = "white") +
  annotate("text", label = rep(0, 8), x = seq(10, 80, 10) + 0.25, y = rep(12, 8), hjust = 0, vjust = 0, color = "white") +
  annotate("text", label = c(1:5, 4:2), x = seq(10, 80, 10) + 0.25, y = rep(160/3 - 12, 8), hjust = 1, vjust = 1, color = "white", angle = 180) +
  annotate("text", label = rep(0, 8), x = seq(10, 80, 10) - 0.25, y = rep(160/3 - 12, 8), hjust = 0, vjust = 1, color = "white", angle = 180) +
  annotate("text", label = rep("◄", 8), x = rep(seq(10, 40, 10), 2) - 2, y = c(rep(13, 4), rep(160/3 - 11, 4)), hjust = 1, vjust = 0.5, color = "white") +
  annotate("text", label = rep("►", 6), x = rep(seq(60, 80, 10), 2) + 2, y = c(rep(13, 3), rep(160/3 - 11, 3)), hjust = 0, vjust = 0.5, color = "white") + 
  #Endzone text
  shadowtext::geom_shadowtext(aes(x = -5, y = 160/6), angle = 90, color = "white", label = "E N D Z O N E", bg.colour = "forestgreen", size = 10) +
  #Field goal posts
  geom_segment(aes(x = -10, xend = -10, y = 80/3 - 9.25/3, yend = 80/3 + 9.25/3), color = "yellow", size = 1) +
  coord_fixed(xlim = c(-12, 75), ylim = c(-2, 2 + 160/3), expand = FALSE) +
  geom_col(data = bins_right, aes(x = kick_dist_5yd, y = success * 160/3), 
           fill = "forestgreen", color = "black", alpha = 0.2, width = 5, position = position_nudge(x = -2.5)) +
  geom_col(data = bins_left, aes(x = kick_dist_5yd, y = success * 160/3), 
           fill = "forestgreen", color = "black", alpha = 0.2, width = 20, position = position_nudge(x = -10)) +
  geom_line(data = smooth_df, aes(x = kick_distance, y = success * 160/3), color = "chocolate4", size = 3) +
  geom_line(data = smooth_df, aes(x = kick_distance, y = success * 160/3), color = "white", size = 1) +
  xlab("Kick Distance") +
  ylab("Kick Success Rate") +
  scale_x_continuous(breaks = seq(0, 70, 10), labels = paste0(seq(0, 70, 10), "yd")) +
  scale_y_continuous(breaks = seq(0, 160/3, length.out = 11), labels = paste0(seq(0, 100, 10), "%")) +
  ggtitle("Kick Success Rate vs Kick Distance",
          subtitle = "NFL Regular Season Kicks Since 2010 - Excluding Blocked Kicks") +
  labs(caption = "Data: {nflfastR} | Plot: @rstats_james") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.background = element_rect(fill = "gray100"),
        plot.title = element_text(color = "chocolate4", face = "bold"),
        plot.subtitle = element_text(size = 8),
        axis.title = element_text(color = "chocolate4", face = "bold"),
        plot.caption = element_text(family = "mono", size = 6, hjust = 0.95),
        plot.margin = unit(c(0.2, 0, 0.2, 0.2), "cm"))

#Save our plot!
ggsave("success_by_dist.png")


#
###
#####
###
#

#Build another gam, this time with our wp_swing feature
wp_swing_gam <- gam(formula = as.formula(success ~ s(kick_distance, wp_swing)),
                    data = kicks_joined,
                    family = binomial())

#prediction dataframe
wp_smooth_df <- expand.grid(kick_distance = seq(0, 80, 0.1),
                            wp_swing = c(0, 0.1, 0.4, 0.7, 1.0))
#Initialize column
wp_smooth_df$success <- NA
wp_smooth_df$success <- predict(wp_swing_gam, newdata = wp_smooth_df, type = "response")

#Make a single column for identifying kick type
kicks_joined <- kicks_joined %>%
  mutate(kick_type = case_when(field_goal_attempt == 1 ~ "field goal",
                               extra_point_attempt == 1 ~ "extra point",
                               TRUE ~ "other"))

#Plot the distribution of Win Probability Swing for all kicks
ggplot() +
  geom_histogram(data = kicks_joined, aes(x = wp_swing, group = kick_type, fill = kick_type), position = position_stack()) +
  scale_fill_manual(values = c("extra point" = "#D50A0A",
                               "field goal" = "#013369"), #use some official NFL colors
                    name = "Kick Type",
                    labels = c("Extra Point", "Field Goal")) +
  geom_point(aes(x = 0.98, y = 175), size = 13, color = "gold", shape = 21, stroke = 4) +
  geom_segment(aes(x = 0.7, xend = 0.93, y = 2500, yend = 500), arrow = arrow(type = "closed"), color = "gold", size = 3) +
  geom_label(aes(x = 0.7, y = 2500), label = "Potential\nGame-Winning\nKicks", size = 4) +
  scale_x_continuous(labels = scales::percent) +
  xlab("Win Probability Swing") +
  ylab("# of Kicks") +
  ggtitle("NFL Kick Situation Leverage",
          subtitle = "[Win Probability Swing] = [Win% if Kick Succeeds] - [Win% if Kick Fails]\n\nRegular Season Kicks Since 2010") +
  labs(caption = "Data: {nflfastR} | Plot: @rstats_james") +
  theme_minimal() +
  theme(legend.position = c(0.8, 0.815),
        legend.title = element_text(hjust = 0.5),
        legend.background = element_blank(),
        legend.box.background = element_rect(color = "black"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "gray95"),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(family = "mono", size = 6, hjust = 0.95),
        plot.margin = unit(c(0.2, 0, 0.2, 0.2), "cm"))

#Save our plot!
ggsave("Kick_Leverage_Distribution.png")

#Make a display factor column with specific level order for legend
wp_smooth_df <- wp_smooth_df %>%
  mutate(display_wp_swing = factor(scales::percent(wp_swing), levels = c("0%", "10%", "40%", "70%", "100%")))

#Plot predicted kick success rates vs kick distance by Win Probability Swing
ggplot() +
  #green sideline rectangle and gray field rectangle
  geom_rect(aes(xmin = -12, xmax = 112, ymin = -2, ymax = 2 + 160/3), color = "white", fill = "forestgreen") +
  geom_rect(aes(xmin = -10, xmax = 110, ymin = 0, ymax = 160/3), color = "white", fill = "gray") +
  #Major and minor yard lines
  geom_segment(data = ten_yd_lines, aes(x = x, xend = xend, y = y, yend = yend), color = "white", size = 1.2) +
  geom_segment(data = five_yd_lines, aes(x = x, xend = xend, y = y, yend = yend), color = "white", size = 0.8) +
  #4 sets of hash marks
  geom_segment(aes(x = rep(hash_x, 4), xend = rep(hash_x, 4), 
                   y = c(rep(0, 99), rep(70.75/3, 99), rep(160/3 - 70.75/3 + 2/3, 99), rep(160/3 - 2/3, 99)),
                   yend = c(rep(2/3, 99), rep(70.75/3 - 2/3, 99), rep(160/3 - 70.75/3, 99), rep(160/3, 99))),
               color = "white", size = 0.5) +
  geom_segment(aes(x = rep(seq(5 - 1/3, 95 - 1/3, 5), 2), xend = rep(seq(5 + 1/3, 95 + 1/3, 5), 2),
                   y = c(rep(70.75/3, 19), rep(160/3 - 70.75/3, 19)), yend = c(rep(70.75/3, 19), rep(160/3 - 70.75/3, 19))),
               color = "white", size = 0.8) +
  #plot the yard line numbers
  annotate("text", label = c(1:5, 4:2), x = seq(10, 80, 10) - 0.25, y = rep(12, 8), hjust = 1, vjust = 0, color = "white") +
  annotate("text", label = rep(0, 8), x = seq(10, 80, 10) + 0.25, y = rep(12, 8), hjust = 0, vjust = 0, color = "white") +
  annotate("text", label = c(1:5, 4:2), x = seq(10, 80, 10) + 0.25, y = rep(160/3 - 12, 8), hjust = 1, vjust = 1, color = "white", angle = 180) +
  annotate("text", label = rep(0, 8), x = seq(10, 80, 10) - 0.25, y = rep(160/3 - 12, 8), hjust = 0, vjust = 1, color = "white", angle = 180) +
  annotate("text", label = rep("◄", 8), x = rep(seq(10, 40, 10), 2) - 2, y = c(rep(13, 4), rep(160/3 - 11, 4)), hjust = 1, vjust = 0.5, color = "white") +
  annotate("text", label = rep("►", 6), x = rep(seq(60, 80, 10), 2) + 2, y = c(rep(13, 3), rep(160/3 - 11, 3)), hjust = 0, vjust = 0.5, color = "white") + 
  #Endzone text
  shadowtext::geom_shadowtext(aes(x = -5, y = 160/6), angle = 90, color = "white", label = "E N D Z O N E", bg.colour = "forestgreen", size = 10) +
  #Field goal posts
  geom_segment(aes(x = -10, xend = -10, y = 80/3 - 9.25/3, yend = 80/3 + 9.25/3), color = "yellow", size = 1) +
  coord_fixed(xlim = c(-12, 75), ylim = c(-2, 2 + 160/3), expand = FALSE) +
  geom_line(data = wp_smooth_df, aes(x = kick_distance, y = success * 160/3, color = display_wp_swing), size = 0.8) +
  xlab("Kick Distance") +
  ylab("Kick Success Rate") +
  scale_x_continuous(breaks = seq(0, 70, 10), labels = paste0(seq(0, 70, 10), "yd")) +
  scale_y_continuous(breaks = seq(0, 160/3, length.out = 11), labels = paste0(seq(0, 100, 10), "%")) +
  scale_color_discrete(name = "Win Probability\nSwing") +
  ggtitle("Kick Success Rate by Situation Leverage",
          subtitle = "Do Kickers Perform Worse When Under Pressure?") +
  labs(caption = "Data: {nflfastR} | Plot: @rstats_james") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = c(0.9, 0.75),
        legend.box.background = element_rect(fill = "white", color = "black"),
        legend.title = element_text(hjust = 0.5, size = 10),
        legend.text = element_text(size = 9),
        plot.background = element_rect(fill = "gray95"),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 8),
        axis.title = element_text(face = "bold"),
        plot.caption = element_text(family = "mono", size = 6, hjust = 0.95),
        plot.margin = unit(c(0.2, 0, 0.2, 0.2), "cm"))

#Save our plot!
ggsave("success_by_dist_wp_swing.png")
