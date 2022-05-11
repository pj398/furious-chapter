# --------------------------------------------------------------------------- #
# SET UP:

# source("scripts/ff_data.R")

library(charinet)

# --------------------------------------------------------------------------- #
# 1. Get the scores

ff1_scores <- narrative_centrality(event_list = ff1$event_list, 
                                   char_names = ff1$node_list$char_name, 
                                   from = 3)
ff2_scores <- narrative_centrality(event_list = ff2$event_list, 
                                   char_names = ff2$node_list$char_name, 
                                   from = 3)
ff3_scores <- narrative_centrality(event_list = ff3$event_list, 
                                   char_names = ff3$node_list$char_name, 
                                   from = 3)
ff4_scores <- narrative_centrality(event_list = ff4$event_list, 
                                   char_names = ff4$node_list$char_name, 
                                   from = 3)
ff5_scores <- narrative_centrality(event_list = ff5$event_list, 
                                   char_names = ff5$node_list$char_name, 
                                   from = 3)
ff6_scores <- narrative_centrality(event_list = ff6$event_list, 
                                   char_names = ff6$node_list$char_name, 
                                   from = 3)
ff7_scores <- narrative_centrality(event_list = ff7$event_list, 
                                   char_names = ff7$node_list$char_name, 
                                   from = 3)
ff8_scores <- narrative_centrality(event_list = ff8$event_list, 
                                   char_names = ff8$node_list$char_name, 
                                   from = 3)

# 2. Plot the scores
plot_nc(ff1_scores$out_scores)
plot_nc(ff2_scores$out_scores)
plot_nc(ff3_scores$out_scores)
plot_nc(ff4_scores$out_scores)
plot_nc(ff5_scores$out_scores)
plot_nc(ff6_scores$out_scores)
plot_nc(ff7_scores$out_scores)
plot_nc(ff8_scores$out_scores)

# --------------------------------------------------------------------------- #