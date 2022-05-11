# --------------------------------------------------------------------------- #
source("scripts/ff_data.R")
# --------------------------------------------------------------------------- #
# Network visualisation----

# These were originally colouring nodes by gender using:
# node_fill = ifelse(ff3$node_list$gender == "Male", "#696969", "#d9d9d9")

plot_charinet(
  ff1$adjacency, 
  char_names = ff1$node_list$char_name, 
  degree = ff1$node_list$nlines, 
  node_fill = "#d9d9d9",
  parallel_edges = TRUE, 
  title = "The Fast and the Furious (2001)", 
  cutoff = 1
  )
ggplot2::ggsave("figs/networks/ff1_network_bw.png",
                width = 12, height = 9, units = "in")

plot_charinet(
  ff2$adjacency, 
  char_names = ff2$node_list$char_name, 
  degree = ff2$node_list$nlines, 
  node_fill = "#d9d9d9",
  parallel_edges = TRUE, 
  title = "2 Fast 2 Furious (2003)", 
  cutoff = 1
  )
ggplot2::ggsave("figs/networks/ff2_network_bw.png",
                width = 12, height = 9, units = "in")

plot_charinet(
  ff3$adjacency, 
  char_names = ff3$node_list$char_name, 
  degree = ff3$node_list$nlines, 
  node_fill = "#d9d9d9",
  parallel_edges = TRUE, 
  title = "The Fast and the Furious: Tokyo Drift (2006)", 
  cutoff = 1
  )
ggplot2::ggsave("figs/networks/ff3_network_bw.png",
                width = 12, height = 9, units = "in")

plot_charinet(
  ff4$adjacency, 
  char_names = ff4$node_list$char_name, 
  degree = ff4$node_list$nlines, 
  node_fill = "#d9d9d9",
  parallel_edges = TRUE, 
  title = "Fast & Furious (2009)", 
  cutoff = 1
  )
ggplot2::ggsave("figs/networks/ff4_network_bw.png", 
                width = 12, height = 9, units = "in")

plot_charinet(
  ff5$adjacency, 
  char_names = ff5$node_list$char_name, 
  degree = ff5$node_list$nlines, 
  node_fill = "#d9d9d9",
  parallel_edges = TRUE, 
  title = "Fast Five (2011)", 
  cutoff = 1
  )
ggplot2::ggsave("figs/networks/ff5_network_bw.png", 
                width = 12, height = 9, units = "in")

plot_charinet(
  ff6$adjacency, 
  char_names = ff6$node_list$char_name, 
  degree = ff6$node_list$nlines, 
  node_fill = "#d9d9d9",
  parallel_edges = TRUE, 
  title = "Fast & Furious 6 (2013)", 
  cutoff = 1
  )
ggplot2::ggsave("figs/networks/ff6_network_bw.png", 
                width = 12, height = 9, units = "in")

plot_charinet(
  ff7$adjacency, 
  char_names = ff7$node_list$char_name, 
  degree = ff7$node_list$nlines, 
  node_fill = "#d9d9d9",
  parallel_edges = TRUE, 
  title = "Furious 7 (2015)", 
  cutoff = 1
  )
ggplot2::ggsave("figs/networks/ff7_network_bw.png", 
                width = 12, height = 9, units = "in")

plot_charinet(
  ff8$adjacency, 
  char_names = ff8$node_list$char_name, 
  degree = ff8$node_list$nlines, 
  node_fill = "#d9d9d9",
  parallel_edges = TRUE, 
  title = "The Fate of the Furious (2017)", 
  cutoff = 1
  )
ggplot2::ggsave("figs/networks/ff8_network_bw.png", 
                width = 12, height = 9, units = "in")

# Plot the networks as patchwork
# library(patchwork, quietly = TRUE)
# 
# ggsave("figs/networks_4x2.png", width = 9, height = 24, units = "in")

# --------------------------------------------------------------------------- #

# # Define a plotting function
# plot_film_gg <- function(adjacency = NULL, nodelist = NULL,  
#                          filmtitle = "") {
#   if(is.null(adjacency)) {
#     stop("Please provide an adjacency matrix via the 'adjacency' argument")
#   }
#   if(is.null(nodelist)) {
#     stop("Please provide a node list via the 'nodelist' argument")
#   }
#   
#   # Create the igraph object
#   g <- igraph::graph_from_adjacency_matrix(adjacency, weighted = TRUE, 
#                                            diag = FALSE)
#   V(g)$gender <- nodelist$gender
#   V(g)$name <- nodelist$char_name
#   V(g)$size <- scales::rescale(nodelist$nlines, to = c(2, 15))
#   g <- graphlayouts::reorder_edges(g, "weight", desc = FALSE)
#   # Create the ggraph plot
#   p <-  ggraph(g, layout = "stress") +
#     geom_edge_link(aes(edge_colour = weight, 
#                        # start_cap = circle(node1.size * 1.1, unit = "pt"),
#                        end_cap = circle(node2.size * 1.1, unit = "pt")), n = 2, 
#                    arrow = arrow(angle = 20,length = unit(0.14, "inches"), 
#                                  ends = "last", type = "closed")) +
#     geom_node_point(aes(fill = gender), size = V(g)$size, shape = 21, 
#                     stroke = 0.5, colour = "black") +
#     geom_node_text(aes(label = name), size = 4.5, 
#                    nudge_y = scales::rescale(V(g)$size, to = c(0.06, 0.15))) +
#     scale_fill_manual(values = c("Male" = "#55467a", "Female" = "#ded649"), 
#                       aesthetics = c("fill")) +
#     scale_edge_colour_gradient(low = "grey85", high = "grey25", trans = "sqrt") +
#     labs(title = filmtitle) +
#     scale_x_continuous(expand = c(0.1, 0.1)) +
#     theme_graph() +
#     theme(legend.position = "none", 
#           plot.title = element_text(size = 16, hjust = 0.5))
#   # And plot it!
#   plot(p)
# }