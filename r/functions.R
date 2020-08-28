# SETUP AND GENERAL FUNCTIONS

# function: write summary table to html
write_sum_tab <- function(model, output){
  out <- paste0("output/tables/", output)
  tab_model(model, file = out)
}

# function; calculation of convex hulls
convex_hull <- function(data) data[chull(data$NMDS1, data$NMDS2),] 

#######################################
#### 1. MAP AND DEPTH DISTRIBUTION ####
#######################################

# function to plot map and study site
plot_map <- function(mapdata, studysite){
  Fig1A <- ggplot(data = mapdata) +
  geom_sf(lwd = 0.01, fill = "grey23", color = "black", alpha = 0.75) +
  geom_point(data = studysite, aes(x = long, y = lat), color = "black", fill = "firebrick", stroke = 1, size = 3, shape = 23) +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_text(color = "black", size = 12))
}

# function to process depth data
process_depth_data <- function(data){
  data %>% mutate(depth.integ = round(depth)) %>%
  mutate(site.rec = recode(site, hogahome = "Hoga Home Reef", pinnacle = "Pinnacle Reef"))
}

# split depth data by site
split_depth_data <- function(data, ...){
  data %>% filter(site == ...)
}

# function plot depth distributions 
# hoga
plot_depth_dist_hoga <- function(data){
  Fig1B <- ggplot(data, aes(y = depth.integ, fill = species, color = species)) +
    geom_density(alpha = 0.5, color = "grey23") +
    scale_fill_fish(option = "Acanthurus_leucosternon", discrete = T) +
    scale_color_fish(option = "Acanthurus_leucosternon", discrete = T) +
    scale_y_reverse(limits = c(18,4), breaks = seq(18, 4, by = -2)) +
    scale_x_continuous(limits = c(0, 0.5)) +
    theme_bw() +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 12),
          axis.text = element_text(color = "black", size = 12),
          strip.background = element_rect(fill="white"),
          strip.text = element_text(colour = 'black'),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          legend.text = element_text(face = "italic")) +
    ylab("Depth (m)")
}

# pinnacle
plot_depth_dist_pinn <- function(data){
  Fig1B <- ggplot(data, aes(y = depth.integ, fill = species, color = species)) +
    geom_density(alpha = 0.5, color = "grey23") +
    scale_fill_fish(option = "Acanthurus_leucosternon", discrete = T) +
    scale_color_fish(option = "Acanthurus_leucosternon", discrete = T) +
    scale_y_reverse(limits = c(18,4), breaks = seq(18, 4, by = -2)) +
    scale_x_continuous(limits = c(0, 0.5)) +
    theme_bw() +
    theme(legend.position = c(0.4, 0.9),
          legend.title = element_blank(),
          legend.key = element_rect(size = 2),
          legend.key.size = unit(0.75, "cm"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 12),
          axis.text = element_text(color = "black", size = 12),
          strip.background = element_rect(fill="white"),
          strip.text = element_text(colour = 'black'),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          legend.text = element_text(face = "italic", size = 10),
          legend.background = element_blank()) +
    ylab("Depth (m)") +
    add_fishape(family = "Labridae",
                option = "Labroides_bicolor",
                xmin = 0.35, xmax = 0.5, ymax = 10.25,
                fill = fish(option = "Acanthurus_leucosternon", n = 5)[1]) +
    add_fishape(family = "Labridae",
                option = "Labroides_dimidiatus",
                xmin = 0.35, xmax = 0.5, ymax = 8.75,
                fill = fish(option = "Acanthurus_leucosternon", n = 5)[3]) +
    add_fishape(family = "Labridae",
                option = "Labroides_pectoralis",
                xmin = 0.35, xmax = 0.5, ymax = 7.0,
                fill = fish(option = "Acanthurus_leucosternon", n = 5)[5])
}

combine_figs <- function(f1,f2,f3){
  Fig1 <- f1 / (f2 | f3) + plot_annotation(tag_levels = 'A') & 
    theme(plot.tag = element_text(size = 14))
  ggsave("output/plots/Fig1_Labroides.png", Fig1, width = 9, height = 9)
}

################################
#### 2. HABITAT PREFERENCES ####
################################

# process habitat data
process_habitat <- function(data){
  data %>% select(coral, zoospong, rock, rubble, sand, depth)
}

# run MDS on habitat data
run_mds <- function(data.proc){
  metaMDS(data.proc, distance = "bray")
}


# store MDS values and combine with metadata from wide format data
store_mds_values <- function(mds.object, ...){
as.tibble(scores(mds.object)) %>%
  mutate(species = ...$species,
         terrsize = ...$terrsize,
         depth = ...$depth) %>% 
  inner_join(...)
}

# store MDS points for drivers
store_mds_points <- function(data, ...){
  as.data.frame(scores(data, ...)) %>%
  add_column(var = rownames(.))
}

# plots MDS results
# ggplot with MDS results, location convex hulls, and SIMPER species highlighted
plot_mds_results <- function(scores, hulls, points){
  Fig2 <- ggplot(scores, aes(x = NMDS1, y = NMDS2)) +
  #overlay convex hulls
  geom_polygon(data = hulls, aes(x = NMDS1, y = NMDS2, 
                                         fill = species), 
               alpha = 0.4, lty = 1, lwd = 0.1, color = "grey23") +
  #overlay points
  geom_point(aes(fill = species, shape = as.factor(age)), color = "grey23", size = 2) +
  geom_segment(data = points, aes(x=0, xend=NMDS1, y=0, yend=NMDS2),
               arrow = arrow(length = unit(0.2, "cm")), color = "grey23", lwd = 0.25,
               inherit.aes = F) +
  geom_text_repel(data = points, aes(x=NMDS1, NMDS2, label=var), size=3.5, inherit.aes = F) +
  theme_bw()+
  theme(legend.position = c(0.8, 0.9),
        legend.title = element_blank(),
        legend.box = "horizontal",
        axis.text = element_text(color = "black", size = 12),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.text = element_text(face = "italic", size = 10),
        legend.background = element_blank()) +
  scale_color_fish_d(option = "Acanthurus_leucosternon") +
  scale_fill_fish_d(option = "Acanthurus_leucosternon") +
  scale_shape_manual(values = c(21,23), labels = c("adult", "juvenile")) +
  xlab("NMDS1") +
  ylab("NMDS2") +
  scale_y_continuous(limits = c(-1.25, 1.25), breaks = seq(-1.25, 1.25, 0.25)) +
  scale_x_continuous(limits = c(-1.5, 1), breaks = seq(-1.5, 1, 0.25))
  ggsave("output/plots/Fig2_Labroides.png", Fig2, width = 8, height = 6)
}


