# --------------------------------------------------------------------------- #
# Set up----
library(movienet)
library(tidyverse)

# --------------------------------------------------------------------------- #
# Read in data----
ff1 <- qread_film(events_file = "data/ff1_dialogue.csv",
                  nodes_file = "data/ff1_nodelist.csv")

check_for_errors(adjacency = ff1$adjacency, event_list = ff1$event_list)

ff2 <- qread_film(events_file = "data/ff2_dialogue.csv",
                  nodes_file = "data/ff2_nodelist.csv")

check_for_errors(adjacency = ff2$adjacency, event_list = ff2$event_list)

# --------------------------------------------------------------------------- #
# Network visualisation----
library(igraph, warn.conflicts = FALSE, quietly = TRUE)
library(ggraph, quietly = TRUE)
library(graphlayouts, quietly = TRUE)
library(extrafont, quietly = TRUE, quietly = TRUE)
library(scales, quietly = TRUE)

g1 <- graph_from_adjacency_matrix(ff1$adjacency, weighted = TRUE, diag = FALSE)
V(g1)$gender <- ff1$node_list$gender
V(g1)$name <- ff1$node_list$char_name
V(g1)$size <- scales::rescale(ff1$node_list$nlines, to = c(2, 15))
g1 <- graphlayouts::reorder_edges(g1, "weight", desc = FALSE)

g1 %>% ggraph(layout = "stress") +
  geom_edge_link(aes(colour = weight, 
                     start_cap = circle(node1.size * 1.1, unit = "pt"),
                     end_cap = circle(node2.size * 1.1, unit = "pt")), n = 2, 
                 arrow = arrow(angle = 20, length = unit(0.14, "inches"), 
                               ends = "last", type = "closed")) +
  geom_node_point(aes(fill = gender), size = V(g1)$size, shape = 21, 
                  stroke = 0.5, colour = "black") +
  geom_node_text(aes(label = name), size = 4.5, 
                 nudge_y = scales::rescale(V(g1)$size, to = c(0.06, 0.15))) +
  scale_fill_manual(values = c("Male" = "#55467a", "Female" = "#ded649"), 
                    aesthetics = c("fill")) +
  scale_edge_color_gradient(low = "grey85", high = "grey25", trans = "sqrt") +
  labs(title = "Character interactions in The Fast and the Furious (2001)") +
  scale_x_continuous(expand = c(0.1, 0.1)) +
  theme_graph() +
  theme(legend.position = "none", 
        plot.title = element_text(size = 16, hjust = 0.5))


g2 <- graph_from_adjacency_matrix(ff2$adjacency, weighted = TRUE, diag = FALSE)
V(g2)$gender <- ff2$node_list$gender
V(g2)$name <- ff2$node_list$char_name
V(g2)$size <- scales::rescale(ff2$node_list$nlines, to = c(2, 15))
g2 <- graphlayouts::reorder_edges(g2, "weight", desc = FALSE)

g2 %>% ggraph(layout = "stress") +
  geom_edge_link(aes(colour = weight, 
                     start_cap = circle(node1.size * 1.1, unit = "pt"),
                     end_cap = circle(node2.size * 1.1, unit = "pt")), n = 2, 
                 arrow = arrow(angle = 20, length = unit(0.14, "inches"), 
                               ends = "last", type = "closed")) +
  geom_node_point(aes(fill = gender), size = V(g2)$size, shape = 21, 
                  stroke = 0.5, colour = "black") +
  geom_node_text(aes(label = name), size = 4.5, 
                 nudge_y = scales::rescale(V(g2)$size, to = c(0.06, 0.15))) +
  scale_fill_manual(values = c("Male" = "#55467a", "Female" = "#ded649"), 
                    aesthetics = c("fill")) +
  scale_edge_color_gradient(low = "grey85", high = "grey25", trans = "sqrt") +
  labs(title = "Character interactions in 2 Fast 2 Furious (2003)") +
  scale_x_continuous(expand = c(0.1, 0.1)) +
  theme_graph() +
  theme(legend.position = "none", 
        plot.title = element_text(size = 16, hjust = 0.5))

# --------------------------------------------------------------------------- #
# Narrative centrality----
ff1_scores <- narrative_centrality(ff1$event_list, 
                                   chars = ff1$node_list$char_name)

ff2_scores <- narrative_centrality(ff1$event_list, 
                                   chars = ff1$node_list$char_name)

# --------------------------------------------------------------------------- #
# Non-network analysis----

# Stacked bar charts of lines spoken by ethnicity for each film
as_tibble(ff1$node_list) %>%
  mutate(film = "The Fast and the Furious (2001)") %>%
  bind_rows(mutate(ff2$node_list, film = "2 Fast 2 Furious (2003)")) %>%
  select(-char_ID) %>%
  group_by(film, ethnicity) %>%
  summarise(total_lines = sum(nlines), count = n()) %>%
  ggplot(aes(x = film, y = total_lines, fill = ethnicity)) +
  geom_bar(stat = "identity", position = "fill")

# Stacked bar charts of no. chars by ethnicity for each film
as_tibble(ff1$node_list) %>%
  mutate(film = "The Fast and the Furious (2001)") %>%
  bind_rows(mutate(ff2$node_list, film = "2 Fast 2 Furious (2003)")) %>%
  select(-char_ID) %>%
  group_by(film, ethnicity) %>%
  summarise(total_lines = sum(nlines), count = n()) %>%
  ggplot(aes(x = film, y = count, fill = ethnicity)) +
  geom_bar(stat = "identity", position = "fill")

# --------------------------------------------------------------------------- #