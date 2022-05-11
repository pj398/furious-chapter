# --------------------------------------------------------------------------- #
source("scripts/ff_data.R")
library(tidyverse)
# --------------------------------------------------------------------------- #
# Analysing and visualising the data----

# Tidy the data
ff_tidy <- as_tibble(ff1$node_list) %>%
  mutate(film = "The Fast and the \nFurious (2001)") %>%
  bind_rows(mutate(ff2$node_list, film = "2 Fast 2 Furious (2003)")) %>%
  bind_rows(mutate(ff3$node_list, 
                   film = "The Fast and the \nFurious: Tokyo Drift (2006)")) %>%
  bind_rows(mutate(ff4$node_list, film = "Fast & Furious (2009)")) %>%
  bind_rows(mutate(ff5$node_list, film = "Fast Five (2011)")) %>%
  bind_rows(mutate(ff6$node_list, film = "Fast & Furious 6 (2013)")) %>%
  bind_rows(mutate(ff7$node_list, film = "Furious 7 (2015)")) %>%
  bind_rows(mutate(ff8$node_list, film = "The Fate of the \nFurious (2017)")) %>%
  select(-char_ID) %>%
  na_if("")

# ---- Ethnicity ---- #

# Create custom colour palette
my_cols <- c("White" = "#f7942a",
             "Black" = "#d96496",
             "Asian/Asian-American" = "#e693ac",
             "MENA" = "#cf2536",
             "Multi" = "#584587",
             "Latinx" = "#1c8525",
             "Other/Unknown" = "#454545")

# Stacked bar charts of no. chars by ethnicity for each film
p_chars_ethn <- ff_tidy %>%
  group_by(film, ethnicity) %>%
  summarise(total_lines = sum(nlines), count = n()) %>%
  ggplot(aes(x = factor(film, 
                        levels = levels(as.factor(film))[c(6, 1, 7, 2, 
                                                           4, 3, 5, 8)]), 
             y = count, fill = ethnicity)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = my_cols) +
  #scale_fill_brewer(type = "qual", palette = "Set1", aesthetics = "fill") +
  scale_x_discrete(name = "Film", guide = guide_axis(angle = 45)) +
  scale_y_continuous(labels = scales::percent_format(), 
                     breaks = seq(0, 1, 0.1), name = "Percentage") +
  labs(title = "Proportion of speaking characters by ethnic group", 
       fill = "Ethnic group") +
  theme_light() +
  theme(text = element_text(size = 12), 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.y = element_blank(), 
        panel.border = element_blank(), 
        axis.line = element_line(colour="black"))

# ggsave("figs/ethn_chars.png", width = 9, height = 6, units = "in")

# Stacked bar charts of lines spoken by ethnicity for each film
p_lines_ethn <- ff_tidy %>%
  group_by(film, ethnicity) %>%
  summarise(total_lines = sum(nlines), count = n()) %>%
  ggplot(aes(x = factor(film, 
                        levels = levels(as.factor(film))[c(6, 1, 7, 2, 
                                                           4, 3, 5, 8)]), 
             y = total_lines, fill = ethnicity)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = my_cols) +
  # scale_fill_brewer(type = "qual", palette = "Set1", aesthetics = "fill") +
  scale_x_discrete(name = "Film", guide = guide_axis(angle = 45)) +
  scale_y_continuous(labels = scales::percent_format(), 
                     breaks = seq(0, 1, 0.1), name = "Percentage") +
  labs(title = "Proportion of lines spoken by ethnic group", 
       fill = "Ethnic group") +
  theme_light() +
  theme(text = element_text(size = 12), 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.y = element_blank(), 
        panel.border = element_blank(), 
        axis.line = element_line(colour="black"))

# ggsave("figs/ethn_lines.png", width = 9, height = 6, units = "in")

# Patch these together
# p_chars_ethn + p_lines_ethn + plot_layout(guides = "collect")

# ---- Benchmarks ---- #

# Compare with Hollywood Diversity Report data

hdr <- read_csv("data/hdr_ethnic_props.csv")

hdr_tidy <- hdr %>% 
  pivot_longer(cols = -Group, names_to = "Year", values_to = "Percentage") %>%
  mutate(Percentage = parse_number(Percentage), Year = parse_number(Year))

hdr_cols <- c("White" = "#f7942a",
              "Black" = "#d96496",
              "Asian" = "#e693ac",
              "MENA" = "#cf2536",
              "Multi" = "#584587",
              "Latinx" = "#1c8525",
              "Native" = "#454545")

hdr_tidy %>%
  ggplot(aes(x = Year, y = Percentage, fill = Group)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = hdr_cols) +
  scale_y_continuous(labels = scales::percent_format(), 
                     breaks = seq(0, 1, 0.1), name = "Percentage") +
  labs(title = "Proportion of characters by ethnic group in top-grossing films", 
       fill = "Ethnic group", 
       subtitle = "Source: Hollywood Diversity Report (2020)") +
  theme_light() +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.y = element_blank(), 
        panel.border = element_blank(), 
        axis.line = element_line(colour="black"))

# ggsave("figs/hdr_ethn_props.png", width = 9, height = 6, units = "in")

# And the Annenberg data----

aii <- read_csv("data/aii_ethnic_props.csv")

aii_tidy <- aii %>% 
  pivot_longer(cols = -Year, names_to = "Group", values_to = "Percentage") %>%
  mutate(Percentage = parse_number(Percentage))

aii_cols <- c("White" = "#f7942a",
              "Black" = "#d96496",
              "Asian" = "#e693ac",
              "Latino" = "#1c8525",
              "Multiracial" = "#584587", 
              "MENA" = "#cf2536", 
              "Other" = "#454545")

aii_tidy %>% ggplot(aes(x = Year, y = Percentage, fill = Group)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = aii_cols) +
  scale_x_continuous(name = "Year", 
                     breaks = seq.int(min(aii$Year), max(aii$Year))) +
  scale_y_continuous(labels = scales::percent_format(), 
                     breaks = seq(0, 1, 0.1), name = "Percentage") +
  labs(title = "Proportion of characters by ethnic group in top-grossing films", 
       fill = "Ethnic group", 
       subtitle = "Source: Annenberg Inclusion Inititative (2020)") +
  theme_light() +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.y = element_blank(), 
        panel.border = element_blank(), 
        axis.line = element_line(colour="black"))

# ggsave("figs/aii_ethn_props.png", width = 9, height = 6, units = "in")

# And the AII race by genre data ----

race_genre <- readxl::read_excel("Quick stats.xlsx", 
                                 sheet = "race X genre")

race_genre <- race_genre %>%
  pivot_longer(cols = -Year, names_to = "Genre", values_to = "Percentage")

race_genre %>%
  ggplot(aes(x = Year, y = Percentage, colour = Genre)) +
  geom_line(size = 1.5) +
  scale_colour_manual(values = c("Action/Adventure" = "#f7942a",
                                 "Animation" = "#d96496",
                                 "Comedy" = "#1c8525")) +
  scale_x_continuous(breaks = seq.int(min(race_genre$Year), max(race_genre$Year))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     name = "Percentage") +
  labs(title = "Proportion of characters from non-white ethnic groups in top-grossing films by genre",
       subtitle = "Source: Annenberg Inclusion Initiative (2020)") +
  theme_light() +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(), 
        panel.border = element_blank(), 
        axis.line = element_line(colour="black"))

# ggsave("figs/aii_ethn_genre.png", width = 9, height = 6, units = "in")

# ---- Age ---- #

# Plot the average age of characters by gender and film
p_mean_age <- ff_tidy %>%
  group_by(film, gender) %>%
  summarise(mean_age = mean(age, na.rm = TRUE),
            median_age = median(age, na.rm = TRUE)) %>%
  ggplot(aes(x = factor(film, levels = levels(as.factor(film))[c(6, 1, 7, 2, 
                                                                 4, 3, 5, 8)]), 
             y = mean_age, fill = gender)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(name = "Mean age") +
  scale_x_discrete(name = "Film", guide = guide_axis(angle = 45)) +
  labs(title = "Mean age by gender and film", 
       fill = "Gender") +
  theme_light() +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.y = element_blank(), 
        panel.border = element_blank(), 
        axis.line = element_line(colour="black"))

# Plot the median age of characters by gender and film
p_median_age <- ff_tidy %>%
  group_by(film, gender) %>%
  summarise(mean_age = mean(age, na.rm = TRUE),
            median_age = median(age, na.rm = TRUE)) %>%
  ggplot(aes(x = factor(film, levels = levels(as.factor(film))[c(6, 1, 7, 2, 
                                                                 4, 3, 5, 8)]), 
             y = median_age, fill = gender)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(name = "Median age") +
  scale_x_discrete(name = "Film", guide = guide_axis(angle = 45)) +
  labs(title = "Median age by gender and film", 
       fill = "Gender") +
  theme_light() +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.y = element_blank(), 
        panel.border = element_blank(), 
        axis.line = element_line(colour="black"))

# Patch these together
p_mean_age + p_median_age + plot_layout(guides = "collect")

# Check the number of characters aged 40+ in each film (minimum 3 lines spoken)
over_40s <- function(film) {
  c("Male" = length(which(film$node_list$gender == "Male" & 
                            film$node_list$age > 39 &
                            film$node_list$nlines > 2)),
    "Female" = length(which(film$node_list$gender == "Female" & 
                              film$node_list$age > 39 &
                              film$node_list$nlines > 2)))
}

# Plot these counts
bind_rows(over_40s(ff1), over_40s(ff2), over_40s(ff3), over_40s(ff4), 
          over_40s(ff5), over_40s(ff6), over_40s(ff7), over_40s(ff8)) %>%
  mutate(Film = c("The Fast and the Furious (2001)", "2 Fast 2 Furious (2003)",
                  "The Fast and the Furious: Tokyo Drift (2006)",
                  "Fast & Furious (2009)", "Fast Five (2011)",
                  "Fast & Furious 6 (2013)", "Furious 7 (2015)",
                  "The Fate of the Furious (2017)")) %>%
  pivot_longer(cols = c(Male, Female), 
               names_to = "Gender",
               values_to = "Count") %>%
  ggplot(aes(x = factor(Film, levels = levels(as.factor(Film))[c(6, 1, 7, 2, 
                                                                 4, 3, 5, 8)]), 
             y = Count, fill = Gender)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(name = "No. characters over 40") +
  scale_x_discrete(name = "Film", guide = guide_axis(angle = 45)) +
  labs(title = "No. over 40s per film by gender (min. 3 lines)", 
       fill = "Gender") +
  theme_light() +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.y = element_blank(), 
        panel.border = element_blank(), 
        axis.line = element_line(colour="black"))

# --------------------------------------------------------------------------- #
# Reproducing the above graphs in B+W friendly format ----

# Stacked bar charts of no. chars by ethnicity for each film
p_chars_ethn_bw <- ff_tidy %>%
  group_by(film, ethnicity) %>%
  summarise(total_lines = sum(nlines), count = n()) %>%
  ggplot(aes(x = factor(film, 
                        levels = levels(as.factor(film))[c(6, 1, 7, 2, 
                                                           4, 3, 5, 8)]), 
             y = count, fill = ethnicity)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_grey() +
  scale_x_discrete(name = "Film", guide = guide_axis(angle = 45)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     breaks = seq(0, 1, 0.1), name = "Percentage") +
  labs(title = "Proportion of speaking characters by ethnic group", 
       fill = "Ethnic group") +
  theme_light() +
  theme(text = element_text(size = 12), 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.y = element_blank(), 
        panel.border = element_blank(), 
        axis.line = element_line(colour="black"))

# ggsave("figs/ethn_chars_bw.png", width = 9, height = 6, units = "in")

# Stacked bar charts of lines spoken by ethnicity for each film
p_lines_ethn_bw <- ff_tidy %>%
  group_by(film, ethnicity) %>%
  summarise(total_lines = sum(nlines), count = n()) %>%
  ggplot(aes(x = factor(film, 
                        levels = levels(as.factor(film))[c(6, 1, 7, 2, 
                                                           4, 3, 5, 8)]), 
             y = total_lines, fill = ethnicity)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_grey() +
  scale_x_discrete(name = "Film", guide = guide_axis(angle = 45)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     breaks = seq(0, 1, 0.1), name = "Percentage") +
  labs(title = "Proportion of lines spoken by ethnic group", 
       fill = "Ethnic group") +
  theme_light() +
  theme(text = element_text(size = 12), 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.y = element_blank(), 
        panel.border = element_blank(), 
        axis.line = element_line(colour="black"))

# ggsave("figs/ethn_lines_bw.png", width = 9, height = 6, units = "in")

# Patch these together
# p_chars_ethn_bw + p_lines_ethn_bw + plot_layout(guides = "collect")

hdr_tidy %>%
  ggplot(aes(x = Year, y = Percentage, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_grey() +
  scale_y_continuous(#labels = scales::percent_format(accuracy = 1), 
                     breaks = seq(0, 80, 10), limits = c(0, 80), 
                     name = "Percentage") +
  labs(title = "Proportion of characters by ethnic group in top-grossing films", 
       fill = "Ethnic group", 
       subtitle = "Source: Hollywood Diversity Report (2020)") +
  theme_light() +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.y = element_blank(), 
        panel.border = element_blank(), 
        axis.line = element_line(colour="black"))

# ggsave("figs/hdr_ethn_props_bw.png", width = 9, height = 6, units = "in")

aii_tidy %>% ggplot(aes(x = Year, y = Percentage, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_grey() +
  scale_y_continuous(#labels = scales::percent_format(accuracy = 1), 
                     breaks = seq(0, 80, 10), limits = c(0, 80), 
                     name = "Percentage") +
  scale_x_continuous(name = "Year",
                     breaks = seq.int(min(aii$Year), max(aii$Year))) +
  labs(title = "Proportion of characters by ethnic group in top-grossing films", 
       fill = "Ethnic group", 
       subtitle = "Source: Annenberg Inclusion Inititative (2020)") +
  theme_light() +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.y = element_blank(), 
        panel.border = element_blank(), 
        axis.line = element_line(colour="black"))

# ggsave("figs/aii_ethn_props_bw.png", width = 9, height = 6, units = "in")

race_genre %>%
  ggplot(aes(x = Year, y = Percentage, colour = Genre)) +
  geom_line(aes(linetype = Genre), size = 1.5, alpha = 0.75) +
  scale_colour_grey(start = 0.15, end = 0.75) +
  scale_x_continuous(breaks = seq.int(min(race_genre$Year), max(race_genre$Year))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     name = "Percentage") +
  scale_linetype_manual(values = c("Action/Adventure" = "solid",
                                     "Animation" = "dashed",
                                     "Comedy" = "dotdash")) +
  labs(title = "Proportion of characters from non-white ethnic groups in top-grossing films by genre",
       subtitle = "Source: Annenberg Inclusion Initiative (2020)") +
  theme_light() +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(), 
        panel.border = element_blank(), 
        axis.line = element_line(colour="black"), 
        legend.position = "bottom",
        legend.key.width = unit(50, "pt"))

# ggsave("figs/aii_ethn_genre_bw.png", width = 9, height = 6, units = "in")

# --------------------------------------------------------------------------- #
# Calculating the effect of coding Mia as multi vs Latinx ----


ff_tidy2 <- ff_tidy
ff_tidy2$ethnicity[grep("Mia", ff_tidy2$char_name)] <- "Latinx"

# Mia as Multi - percentage of characters
ff_tidy %>%
  group_by(film, ethnicity) %>%
  summarise(total_lines = sum(nlines), count = n()) %>%
  mutate(chars_pc = (count/sum(count)*100),
         chars_pc = paste0(round(chars_pc, digits = 1), "%")) %>%
  ggplot(aes(x = factor(film, 
                        levels = levels(as.factor(film))[c(6, 1, 7, 2, 
                                                           4, 3, 5, 8)]), 
             y = total_lines, fill = ethnicity)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = chars_pc), colour = "white", position = position_fill(vjust = 0.5)) +
  scale_fill_manual(values = my_cols) +
  # scale_fill_brewer(type = "qual", palette = "Set1", aesthetics = "fill") +
  scale_x_discrete(name = "Film", guide = guide_axis(angle = 45)) +
  scale_y_continuous(labels = scales::percent_format(), 
                     breaks = seq(0, 1, 0.1), name = "Percentage") +
  labs(title = "Proportion of lines spoken by ethnic group", 
       fill = "Ethnic group") +
  theme_light() +
  theme(text = element_text(size = 12), 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.y = element_blank(), 
        panel.border = element_blank(), 
        axis.line = element_line(colour="black"))

# Mia as Latinx - percentage of characters
ff_tidy2 %>%
  group_by(film, ethnicity) %>%
  summarise(total_lines = sum(nlines), count = n()) %>%
  mutate(chars_pc = (count/sum(count)*100),
         chars_pc = paste0(round(chars_pc, digits = 1), "%")) %>%
  ggplot(aes(x = factor(film, 
                        levels = levels(as.factor(film))[c(6, 1, 7, 2, 
                                                           4, 3, 5, 8)]), 
             y = total_lines, fill = ethnicity)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = chars_pc), colour = "white", position = position_fill(vjust = 0.5)) +
  scale_fill_manual(values = my_cols) +
  # scale_fill_brewer(type = "qual", palette = "Set1", aesthetics = "fill") +
  scale_x_discrete(name = "Film", guide = guide_axis(angle = 45)) +
  scale_y_continuous(labels = scales::percent_format(), 
                     breaks = seq(0, 1, 0.1), name = "Percentage") +
  labs(title = "Proportion of lines spoken by ethnic group", 
       fill = "Ethnic group") +
  theme_light() +
  theme(text = element_text(size = 12), 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.y = element_blank(), 
        panel.border = element_blank(), 
        axis.line = element_line(colour="black"))

# Mia as Multi - percentage of lines
ff_tidy %>%
  group_by(film, ethnicity) %>%
  summarise(total_lines = sum(nlines), count = n()) %>%
  mutate(lines_pc = (total_lines/sum(total_lines)*100), 
         lines_pc = paste0(round(lines_pc, digits = 1), "%")) %>%
  ggplot(aes(x = factor(film, 
                        levels = levels(as.factor(film))[c(6, 1, 7, 2, 
                                                           4, 3, 5, 8)]), 
             y = total_lines, fill = ethnicity)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = lines_pc), colour = "white", position = position_fill(vjust = 0.5)) +
  scale_fill_manual(values = my_cols) +
  # scale_fill_brewer(type = "qual", palette = "Set1", aesthetics = "fill") +
  scale_x_discrete(name = "Film", guide = guide_axis(angle = 45)) +
  scale_y_continuous(labels = scales::percent_format(), 
                     breaks = seq(0, 1, 0.1), name = "Percentage") +
  labs(title = "Proportion of lines spoken by ethnic group", 
       fill = "Ethnic group") +
  theme_light() +
  theme(text = element_text(size = 12), 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.y = element_blank(), 
        panel.border = element_blank(), 
        axis.line = element_line(colour="black"))

# Mia as Latinx - percentage of lines
ff_tidy2 %>%
  group_by(film, ethnicity) %>%
  summarise(total_lines = sum(nlines), count = n()) %>%
  mutate(lines_pc = (total_lines/sum(total_lines)*100), 
         lines_pc = paste0(round(lines_pc, digits = 1), "%")) %>%
  ggplot(aes(x = factor(film, 
                        levels = levels(as.factor(film))[c(6, 1, 7, 2, 
                                                           4, 3, 5, 8)]), 
             y = total_lines, fill = ethnicity)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = lines_pc), colour = "white", position = position_fill(vjust = 0.5)) +
  scale_fill_manual(values = my_cols) +
  # scale_fill_brewer(type = "qual", palette = "Set1", aesthetics = "fill") +
  scale_x_discrete(name = "Film", guide = guide_axis(angle = 45)) +
  scale_y_continuous(labels = scales::percent_format(), 
                     breaks = seq(0, 1, 0.1), name = "Percentage") +
  labs(title = "Proportion of lines spoken by ethnic group", 
       fill = "Ethnic group") +
  theme_light() +
  theme(text = element_text(size = 12), 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.y = element_blank(), 
        panel.border = element_blank(), 
        axis.line = element_line(colour="black"))

# --------------------------------------------------------------------------- #