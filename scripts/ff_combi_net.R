#-----------------------------------------------------------------------------#
#         ---- The combined Fast & Furious franchise network ----
#
#-----------------------------------------------------------------------------#
# Data prep----

# ffcombo <- readxl::read_excel("data/FF_combined.xlsx", sheet = "all_dialogue")
# ffcombo[is.na(ffcombo)] <- 0
# write_csv(ffcombo, "data/ff_all_dialogue.csv")
# 
# ffnodes <- readxl::read_excel("data/FF_combined.xlsx", sheet = "all_nodelist")
# write_csv(ffnodes, "data/ff_all_nodelist.csv")

# Read in the data----

ff_all <- charinet::qread_film("data/ff_all_dialogue.csv", 
                               "data/ff_all_nodelist.csv", 
                               start_at = 4)

#-----------------------------------------------------------------------------#
# Plot the master network----

# Create an object ggraph can use
library(igraph)

g <- igraph::graph_from_adjacency_matrix(ff_all$adjacency,
                                         weighted = TRUE,
                                         diag = FALSE)
V(g)$gender <- ff_all$node_list$gender
V(g)$name <- ff_all$node_list$char_name
V(g)$size <- scales::rescale(ff_all[[2]]$nlines, to = c(2, 15))
g <- graphlayouts::reorder_edges(g, "weight", desc = FALSE)


# Create the plot in ggraph
library(extrafont)
library(ggraph)

ggraph(g, layout = "stress") +
  geom_edge_link(aes(edge_colour = weight, 
                     # start_cap = circle(node1.size * 1.1, unit = "pt"),
                     end_cap = circle(node2.size * 1.1, unit = "pt")), n = 2, 
                 arrow = arrow(angle = 20,length = unit(0.14, "inches"), 
                               ends = "last", type = "closed")) +
  geom_node_point(aes(fill = gender), size = V(g)$size, shape = 21, 
                  stroke = 0.5, colour = "black") +
  geom_node_text(aes(label = name), size = 4.5, 
                 nudge_y = scales::rescale(V(g)$size, to = c(0.06, 0.15))) +
  scale_fill_manual(values = c("Male" = "#55467a", "Female" = "#ded649"), 
                    aesthetics = c("fill")) +
  scale_edge_colour_gradient(low = "grey85", high = "grey25", trans = "sqrt") +
  labs(title = "Character interactions in the Fast & Furious series") +
  scale_x_continuous(expand = c(0.1, 0.1)) +
  theme_graph() +
  theme(legend.position = "none", 
        plot.title = element_text(size = 16, hjust = 0.5))

# And plot it!
plot(p)

# Save it
# ggsave("figs/networks/ff_master.png", width = 22, height = 16, units = "in")

#-----------------------------------------------------------------------------#

# How about a parallel edge version?

ggraph(g, layout = "stress") +
  geom_edge_parallel(aes(edge_colour = weight, 
                     # start_cap = circle(node1.size * 1.1, unit = "pt"),
                     end_cap = circle(node2.size * 1.1, unit = "pt")), n = 2, 
                 arrow = arrow(angle = 20,length = unit(7, "pt"), 
                               ends = "last", type = "closed"), 
                 sep = unit(3, "pt")) +
  geom_node_point(aes(fill = gender), size = V(g)$size, shape = 21, 
                  stroke = 0.5, colour = "black") +
  geom_node_text(aes(label = name), size = 4.5, 
                 nudge_y = scales::rescale(V(g)$size, to = c(0.06, 0.15))) +
  scale_fill_manual(values = c("Male" = "#55467a", "Female" = "#ded649"), 
                    aesthetics = c("fill")) +
  scale_edge_colour_gradient(low = "grey85", high = "grey25", trans = "sqrt") +
  labs(title = "Character interactions in the Fast & Furious series") +
  scale_x_continuous(expand = c(0.1, 0.1)) +
  theme_graph() +
  theme(legend.position = "none", 
        plot.title = element_text(size = 16, hjust = 0.5))

# Save it
# ggsave("figs/networks/ff_master_par.png", 
#        width = 22, height = 16, units = "in")


#-----------------------------------------------------------------------------#

# Tweak Cipher's position

# > which(V(g)$name == "Cipher")
# [1] 102

new_coord <- graphlayouts::layout_with_stress(g)
new_coord[102, 2] <- new_coord[102, 2] * 0.9 # Bump Cipher
new_coord[79, 1] <- new_coord[79, 1] * 1.1 # Bump Shaw

ggraph(g, layout = "manual", 
       x = new_coord[ , 1], 
       y = new_coord[ , 2]) +
  geom_edge_parallel(aes(edge_colour = weight, 
                         # start_cap = circle(node1.size * 1.1, unit = "pt"),
                         end_cap = circle(node2.size * 1.1, unit = "pt")), n = 2, 
                     arrow = arrow(angle = 20,length = unit(7, "pt"), 
                                   ends = "last", type = "closed"), 
                     sep = unit(3, "pt")) +
  geom_node_point(aes(fill = gender), size = V(g)$size, shape = 21, 
                  stroke = 0.5, colour = "black") +
  geom_node_text(aes(label = name), size = 4.5, 
                 nudge_y = scales::rescale(V(g)$size, to = c(0.06, 0.15))) +
  scale_fill_manual(values = c("Male" = "#55467a", "Female" = "#ded649"), 
                    aesthetics = c("fill")) +
  scale_edge_colour_gradient(low = "grey85", high = "grey25", trans = "sqrt") +
  labs(title = "Character interactions in the Fast & Furious series") +
  scale_x_continuous(expand = c(0.1, 0.1)) +
  theme_graph() +
  theme(legend.position = "none", 
        plot.title = element_text(size = 16, hjust = 0.5))

#-----------------------------------------------------------------------------#

# Let's get radial

ggraph(g, layout = "centrality", cent = ff_all[[2]]$nlines) +
  graphlayouts::draw_circle(use = "cent") +
  geom_edge_parallel(aes(edge_colour = weight
                         # , start_cap = circle(node1.size * 1.1, unit = "pt")
                         , end_cap = rectangle(height = 20, 
                                               width = (stringr::str_length(node2.name) * 10), 
                                               width_unit = "pt")
                         ), n = 2, 
                     arrow = arrow(angle = 20,length = unit(7, "pt"), 
                                   ends = "last", type = "closed"), 
                     sep = unit(3, "pt")) +
  geom_node_label(aes(fill = gender, colour = gender, label = name), 
                  size = 5, label.size = 0) +
  # geom_node_text(aes(label = name), size = 5, 
  #                nudge_y = scales::rescale(V(g)$size, to = c(0.06, 0.15))) +
  scale_colour_manual(values = c("Male" = "#55467a", "Female" = "#ded649"), 
                    aesthetics = c("fill")) +
  scale_fill_manual(values = c("Male" = "#ded649", "Female" = "#55467a"), 
                    aesthetics = c("colour")) +
  scale_edge_colour_gradient(low = "grey85", high = "grey25", trans = "sqrt") +
  labs(title = "Character interactions in the Fast & Furious series") +
  scale_x_continuous(expand = c(0.1, 0.1)) +
  theme_graph() +
  theme(legend.position = "none", 
        plot.title = element_text(size = 16, hjust = 0.5))

# Save it
# ggsave("figs/networks/ff_master_rad.png", width = 22, height = 16, units = "in")

#-----------------------------------------------------------------------------#

# Limit the network to a tie threshold

thresh_network <- function(adjacency, nties = 3) {
  new_adj <- adjacency
  for(i in 1:nrow(adjacency)) {
    for(j in 1:ncol(adjacency)) {
      new_adj[i, j] <- ifelse(adjacency[i, j] >= nties, adjacency[i, j], 0)
    }
  }
  return(new_adj)
}

ff_a5 <- thresh_network(ff_all$adjacency, nties = 5)

g5 <- igraph::graph_from_adjacency_matrix(ff_a5,
                                         weighted = TRUE,
                                         diag = FALSE)
V(g5)$gender <- ff_all$node_list$gender
V(g5)$name <- ff_all$node_list$char_name
V(g5)$size <- scales::rescale(ff_all[[2]]$nlines, to = c(2, 15))
g5 <- graphlayouts::reorder_edges(g5, "weight", desc = FALSE)

ggraph(g5, layout = graphlayouts::layout_with_stress(g)) +
  geom_edge_parallel(aes(edge_colour = weight, 
                         # start_cap = circle(node1.size * 1.1, unit = "pt"),
                         end_cap = circle(node2.size * 1.1, unit = "pt")), n = 2, 
                     arrow = arrow(angle = 20,length = unit(7, "pt"), 
                                   ends = "last", type = "closed"), 
                     sep = unit(3, "pt")) +
  geom_node_point(aes(fill = gender), size = V(g)$size, shape = 21, 
                  stroke = 0.5, colour = "black") +
  geom_node_text(aes(label = name), size = 4.5, 
                 nudge_y = scales::rescale(V(g)$size, to = c(0.06, 0.15))) +
  scale_fill_manual(values = c("Male" = "#55467a", "Female" = "#ded649"), 
                    aesthetics = c("fill")) +
  scale_edge_colour_gradient(low = "grey85", high = "grey25", trans = "sqrt") +
  labs(title = "Character interactions in the Fast & Furious series") +
  scale_x_continuous(expand = c(0.1, 0.1)) +
  theme_graph() +
  theme(legend.position = "none", 
        plot.title = element_text(size = 16, hjust = 0.5))

# ggsave("figs/networks/ff_master_par.png", 
#        width = 22, height = 16, units = "in")
