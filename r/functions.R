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
  metaMDS(data.proc, distance = "bray", k = 2, trymax = 1000)
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

#################################
#### 3. TEMPORAL DIFFERENCES ####
#################################

# function to process temporal data
process_temporal <- function(data){
  data %>%
    pivot_longer(c(4:12), names_to = "time", values_to = "value") %>%
    mutate(tod = case_when(grepl("am", time) ~ "am",
                           grepl("noon", time) ~ "noon",
                           grepl("pm", time) ~ "pm")) %>%
    mutate(variable = case_when(grepl("spp", time) ~ "nb_spec",
                                grepl("client", time) ~ "nb_client",
                                grepl("time", time) ~ "seconds")) %>%
    select(-time) %>%
    pivot_wider(names_from = variable, values_from = value)%>%
    filter(nb_spec > 0)
}

# function to filter temporal data by species; necessary because no full representation of all species across stations and times
split_temp_data <- function(data, ...){
  data %>% filter(species == ...)
}

# generic brms function for temporal models
#' @param response specify response variable
#' @param dataset data to be used
#' @param ... flexible descriptor for family
run_brms_temp <- function(response, dataset, ...){
  brm(paste0(response," ~ tod + (1|station)"),
      data = dataset, family = ...,
      control = list(adapt_delta = 0.95, max_treedepth = 10), iter = 5000)
}

# generic brms function for temporal models
#' @param response specify response variable
#' @param dataset data to be used
#' @param ... flexible descriptor for family
run_brms_temp_norand <- function(response, dataset, ...){
  brm(paste0(response," ~ tod"),
      data = dataset, family = ...,
      control = list(adapt_delta = 0.95, max_treedepth = 10), iter = 5000)
}

# species specific behaviors
# function to summarize across different time periods for all species

summarize_temporal <- function(data){
  data %>%
  group_by(species, station) %>%
  summarize(mean_nb_spec = mean(nb_spec),
            mean_nb_client = mean(nb_client),
            mean_seconds = mean(seconds))
}

# generic function to analyze species-specific differences
#' @param response specify response variable
#' @param dataset data to be used
#' @param ... flexible descriptor for family
run_brms_species <- function(response, dataset){
  brm(paste0("log(", response,") ~ species"),
      data = dataset,
      control = list(adapt_delta = 0.95, max_treedepth = 10), iter = 5000)
}



# predict from models
# generic predict function for morphology
#' @param raw raw data the model was run on
#' @param ... variables in the model
#' @param mod brms model object
#' @param draws number of draws from posterior
#' @param backtrans logical backtransform predictions
#' @param transform type of backtransformation to apply

predict_from_brms <- function(raw, ..., mod, draws, backtrans = F, transform = NA){
  
  if (backtrans & is.na(transform)){
    stop("Need to specify transformation")
  }
  
  prediction.new <- raw %>%
    modelr::data_grid(...) %>%
    add_fitted_draws(mod, n = draws) %>%
    as_tibble()
  
  if (backtrans){
    if (transform == "exp"){
      value <- exp(prediction.new$.value)
    } else if (transform == "e10"){
      value <- 10^prediction.new$.value
    }
    prediction.new <- prediction.new %>%
      mutate(pred.corr = value)
  }
  return(prediction.new)
}

# plot predictions for nbspec
plot_nbspec = function(newdata, rawdata, ..., expr, uppery, breaky)
Fig3A <-  ggplot(newdata, aes(x = species, y = pred.corr, fill = species)) +
  geom_violin(trim = T, draw_quantiles = c(0.025,0.5,0.975), color = "grey23", alpha = 0.5, position = position_dodge(width = 0.5)) +
  geom_jitter(data = rawdata, aes(x = species, y = ..., group = species, fill = species), position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.5), alpha = 0.5, color = "grey23", shape = 23) +
  theme_bw() + theme(legend.position = "none",
                     axis.text = element_text(color = "black", size = 12),
                     panel.grid = element_blank(),
                     axis.text.x = element_text(face = "italic", angle = 45, hjust = 1),
                     axis.title = element_text(color = "black", size = 12)) +
  ylab(expr) +
  xlab("") +
  scale_color_fish_d(option = "Acanthurus_leucosternon") +
  scale_fill_fish_d(option = "Acanthurus_leucosternon") +
  scale_y_continuous(limits = c(0,uppery), breaks = seq(0,uppery,breaky)) +
  add_fishape(family = "Labridae",
              option = "Labroides_bicolor", scaled = TRUE,
              xmin = 0, xmax = 0.3, ymin = 0.9, ymax = 1,
              xlim = c(0.5,3.5), ylim = c(0,uppery),
              fill = fish(option = "Acanthurus_leucosternon", n = 5)[1])+
  add_fishape(family = "Labridae",
              option = "Labroides_dimidiatus", scaled = TRUE,
              xmin = 0.35, xmax = 0.65, ymin = 0.9, ymax = 1,
              xlim = c(0.5,3.5), ylim = c(0,uppery),
              fill = fish(option = "Acanthurus_leucosternon", n = 5)[3])+
  add_fishape(family = "Labridae",
              option = "Labroides_pectoralis",scaled = TRUE,
              xmin = 0.7, xmax = 1, ymin = 0.9, ymax = 1,
              xlim = c(0.5,3.5), ylim = c(0,uppery),
              fill = fish(option = "Acanthurus_leucosternon", n = 5)[5])

# combine figs
combine_figs_3 <- function(f1,f2,f3){
  Fig3 <- f1 | f2 | f3 + plot_annotation(tag_levels = 'A') & 
    theme(plot.tag = element_text(size = 14))
  ggsave("output/plots/Fig3_Labroides.png", Fig3, width = 10, height = 5)
}

#########################
#### 4. INTERACTIONS ####
#########################

# function to process count data
process_counts = function(raw, ID){
  raw %>%
  left_join(ID) %>%
  select(-species_common) %>%
  pivot_longer(cols = 1:36, names_to = "site", values_to = "abun") %>%
  group_by(species_scientific, family) %>%
  summarize(mean.abun = mean(abun)) %>%
  ungroup() %>%
  mutate(species_scientific = sub(" ", ".", species_scientific, ))
}

# function to process interaction data for network plot
inter_for_network <- function(interactions, clients, stations){
  interactions %>%
    pivot_longer(names_to = "station", values_to = "interaction", -species_common) %>%
    left_join(clients[-4]) %>%
    left_join(stations) %>%
    group_by(family, species) %>%
    summarize(sum.interaction = sum(interaction)) %>%
    group_by(species) %>%
    mutate(rel.inter = sum.interaction/sum(sum.interaction)) %>%
    filter(rel.inter > 0)
}

# function to plot network
plot_network_results <- function(interactions){
  Fig4 <- ggplot(data = interactions) +
    geom_net(layout.alg = "circle", aes(from_id = family, to_id = species,  linewidth = rel.inter*7), 
             labelon = T, repel = TRUE, curvature = 0,directed = F,  size = 3) +
    theme_net() +
    theme(legend.position = "top") +
    scale_color_fish_d(option = "Acanthurus_leucosternon")
  ggsave("output/plots/Fig4_Labroides.pdf", Fig4, width = 10, height = 10, useDingbats = F)
}
  
# function to process data for interaction MDS (Supp. Fig.)
inter_for_network_mds <- function(interactions, clients, stations){
  interactions %>%
    pivot_longer(names_to = "station", values_to = "interaction", -species_common) %>%
    left_join(clients[-4]) %>%
    left_join(stations) %>%
    select(-family, -species_common) %>% 
    group_by(station) %>%
    mutate(rel.inter = interaction/sum(interaction))%>%
    select(-interaction) %>% 
    pivot_wider(names_from = species_scientific, values_from = rel.inter, values_fill = list(rel.inter = 0))
}

# compile cleaning interaction mds data
# store MDS values and combine with metadata from wide format data
store_mds_values_clean <- function(mds.object, ...){
  as.tibble(scores(mds.object)) %>%
    mutate(station = ...$station) %>% 
    inner_join(...)
}

# function to get SIMPER results in clean data
create_simper <- function(mds.data){
  summary(as.list(simper(mds.data[-c(1:2)], mds.data$species)))
}

# function to extract results for every species pair
get_simper_pairs <- function(simperdata, ...){
  as.data.frame(simperdata[[...]]) %>%
    mutate(species_scientific = rownames(.)) %>%
    filter(average >0.025)
}

# function to combine simper results with mds points
store_simper <- function(sim1, sim2, sim3){
  bind_rows(sim1, sim2, sim3) %>%
  select(species_scientific) %>%
  distinct()
}

# combine mds coordinates with SIMPER
get_simper_coords <- function(mdsdata, simp){
  as.data.frame(scores(mdsdata, "species")) %>%
    add_column(species_scientific = rownames(.)) %>%
    right_join(simp)
}

# plot cleaning-interaction mds
plot_clean_mds <- function(scores, hulls, points){
  FigS2 <- ggplot(scores, aes(x = NMDS1, y = NMDS2)) +
  #overlay convex hulls
  geom_polygon(data = hulls, aes(x = NMDS1, y = NMDS2, 
                                         fill = as.factor(species)), 
               alpha = 0.4, lty = 1, lwd = 0.1, color = "grey23") +
  #overlay points
  geom_point(aes(fill = as.factor(species)), color = "grey23", size = 2, shape = 23) +
  geom_segment(data = points, aes(x=0, xend=NMDS1, y=0, yend=NMDS2),
               arrow = arrow(length = unit(0.2, "cm")), color = "grey23", lwd = 0.1,
               inherit.aes = F) +
  geom_text_repel(data = points, aes(x=NMDS1, NMDS2, label = species_scientific), size=3, inherit.aes = F) +
  theme_bw()+
  theme(legend.position = c(0.1, 0.9),
        legend.title = element_blank(),
        legend.box = "horizontal",
        axis.text = element_text(color = "black", size = 12),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.text = element_text(face = "italic", size = 10),
        legend.background = element_blank()) +
  scale_color_fish_d(option = "Acanthurus_leucosternon") +
  scale_fill_fish_d(option = "Acanthurus_leucosternon") +
  xlab("NMDS1") +
  ylab("NMDS2") 
ggsave("output/plots/FigS2.pdf", FigS2, width = 8, height = 6, useDingbats = TRUE)
}
# function to process interaction data
process_cleaning_interactions <- function(interactions, clients, stations){
interactions %>%
  pivot_longer(names_to = "station", values_to = "interaction", -species_common) %>%
  left_join(clients[-4]) %>%
  left_join(stations) %>%
  group_by(species_scientific, station) %>%
  summarize(avg.interaction = mean(interaction)) %>%
  ungroup() %>%
  group_by(station) %>%
  mutate(rel.inter = avg.interaction/sum(avg.interaction)) %>%
  select(-avg.interaction) %>% 
  pivot_wider(names_from = species_scientific, values_from = rel.inter, values_fill = list(rel.inter = 0))%>%
  data.frame()
}

############################
#### 5. SPECIALIZATION #####
############################

# function to create helper with clients and relative abundances
get_rel_abus <- function(interactions, metadata){
  as.data.frame(colnames(interactions[-1])) %>%
  rename(species_scientific = 1) %>%
  left_join(metadata) %>%
  select(-family) %>%
  replace_na(list(mean.abun = 0.01))
}

# function to calculate d-values (values of specialization)
get_d_vals <- function(interactions, abundance){
  dfun(interactions, abuns = abundance$mean.abun)
}

# function to get species-specific d-indices
get_spec_indices <- function(dvalues, interactions, stations)
spec.ind <- as.data.frame(dvalues$dprim) %>%
  add_column(interactions$station) %>%
  rename(special = 1, station = 2) %>%
  inner_join(stations) %>%
  mutate(speciesname = recode(species, '1' = "L. bicolor", '2' = "L. dimidiatus", '3' = "L. pectoralis"))

# function to analyze d-values
#' @param response specify response variable
#' @param dataset data to be used
#' @param ... flexible descriptor for family
run_brms_d <- function(response, dataset){
  brm(paste0(response," ~ speciesname"),
      data = dataset,
      control = list(adapt_delta = 0.95, max_treedepth = 10), iter = 5000)
}

# function to plot specialization
plot_specialization <- function(prediction, raw){
  Fig5 = ggplot(prediction, aes(y = .value, x = speciesname, fill = speciesname, color = speciesname)) +
  geom_violin(trim = T, draw_quantiles = c(0.025,0.5,0.975), color = "grey23", alpha = 0.75, position = position_dodge(width = 0.5)) +
  geom_jitter(data = raw, aes(x = speciesname, y = special, group = speciesname, fill = speciesname, shape = speciesname), position = position_jitterdodge(jitter.width = 0.25, dodge.width = 0.5), alpha = 0.75, color = "black") +
  theme_bw() + theme(legend.position = "none",
                     legend.title = element_blank(),
                     panel.grid = element_blank(),
                     axis.text.x = element_text(color = "black", face = "italic"),
                     axis.text = element_text(color = "black")) +
  ylab("d' specialization index") +
  xlab("") +
  scale_color_fish_d(option = "Acanthurus_leucosternon") +
  scale_fill_fish_d(option = "Acanthurus_leucosternon") +
  scale_shape_manual(values = c(21, 22, 23)) +
  scale_y_continuous(limits = c(0,1)) +
  scale_x_discrete() +
  add_fishape(family = "Labridae",
              option = "Labroides_bicolor", scaled = TRUE,
              xmin = 0, xmax = 0.3, ymin = 0.9, ymax = 1,
              xlim = c(0.5,3.5), ylim = c(0,1),
              fill = fish(option = "Acanthurus_leucosternon", n = 5)[1])+
  add_fishape(family = "Labridae",
              option = "Labroides_dimidiatus", scaled = TRUE,
              xmin = 0.35, xmax = 0.65, ymin = 0.9, ymax = 1,
              xlim = c(0.5,3.5), ylim = c(0,1),
              fill = fish(option = "Acanthurus_leucosternon", n = 5)[3])+
  add_fishape(family = "Labridae",
              option = "Labroides_pectoralis",scaled = TRUE,
              xmin = 0.7, xmax = 1, ymin = 0.9, ymax = 1,
              xlim = c(0.5,3.5), ylim = c(0,1),
              fill = fish(option = "Acanthurus_leucosternon", n = 5)[5])
ggsave("output/plots/Fig5_Labroides.pdf", Fig5, width = 6, height = 6)
}


# run niche partitioning analysis
niche.part = run_niche_null(clean.inter.proc)


##############################
#### 6. CLIENT ATTRIBUTES ####
##############################

# function to process client data
process_clients <- function(clients, abundance){
  clients %>%
    mutate(species_scientific = sub(" ", ".", species_scientific)) %>%
    full_join(abundance) %>%
    replace_na(list(mean.abun = 0.01)) %>%
    mutate(L.dimidiatus = case_when(cleaner_code %in% c(1,4,5,7) ~ 1,
                                    TRUE ~ 0)) %>%
    mutate(L.pectoralis = case_when(cleaner_code %in% c(2,5,6,7) ~ 1,
                                    TRUE ~ 0)) %>%
    mutate(L.bicolor = case_when(cleaner_code %in% c(3,5,6,7) ~ 1,
                                 TRUE ~ 0)) %>%
    mutate(log.mean.abun = log10(mean.abun))
}

# function to analyze client selectivity
#' @param response specify response variable
#' @param dataset data to be used
#' @param ... flexible descriptor for family
run_brms_client <- function(response, dataset, ...){
  brm(paste0(response," ~ log.mean.abun + client_size"),
      data = dataset, family = ...,
      control = list(adapt_delta = 0.95, max_treedepth = 10), iter = 5000)
}

# generic predict function for morphology
#' @param raw raw data the model was run on
#' @param ... variables in the model
#' @param mod brms model object
#' @param draws number of draws from posterior
#' @param backtrans logical backtransform predictions
#' @param transform type of backtransformation to apply

predict_from_brms_client <- function(raw, v1, v2, mod, draws, name, size = F){
  
  if (size){
    prediction.new <- raw %>%
      modelr::data_grid(client_size = seq(min(v1), 
                                          max(v1), 
                                          1), 
                        log.mean.abun = median(v2)) %>%
      add_fitted_draws(mod, n = draws) %>%
      mutate(species = name)

    } else if (size == F) {
      prediction.new <- raw %>%
        modelr::data_grid(client_size = median(v1), 
                          log.mean.abun = seq(min(v2), 
                                              max(v2), 
                                              0.1)) %>%
        add_fitted_draws(mod, n = draws) %>%
        mutate(species = name) %>%
        mutate(mean.abun = 10^log.mean.abun) 
  }
  return(prediction.new)
}

# function to combine prediction datasets
combine_predictions = function(p1, p2, p3){
  bind_rows(p1, p2, p3) %>%
    unite(grouping, c(species, .draw), sep = "_", remove = F)
} 

# function to plot bernoulli model outputs
plot_bernoulli_models <- function(predictions, ..., expr, lowerx, upperx, breaks, size = F){
  
  if (size){
    
  ggplot(predictions, aes(x = ..., y = .value, group = grouping)) +
    geom_line(aes(color = species), alpha = 0.5) +
    theme_bw() +
    scale_color_fish_d(option = "Acanthurus_leucosternon") +
    theme(legend.position ="top",
          legend.title = element_blank(),
          axis.text = element_text(color = 'black')) +
    scale_x_continuous(limits = c(lowerx, upperx), breaks= seq(lowerx, upperx, breaks)) +
    ylab("Predicted probability of cleaning interaction") +
    xlab(expr)
    
  }
  
  else if (size == F) {
    ggplot(predictions, aes(x = ..., y = .value, group = grouping)) +
      geom_line(aes(color = species), alpha = 0.5) +
      theme_bw() +
      scale_color_fish_d(option = "Acanthurus_leucosternon") +
      theme(legend.position ="top",
            legend.title = element_blank(),
            axis.text = element_text(color = 'black')) +
      scale_x_log10() +
      ylab("Predicted probability of cleaning interaction") +
      xlab(expr)
  }
}

# funciton to combine client figures
comb_fig6 <- function(f1, f2){
  Fig6 <- f1/f2 +
  plot_annotation(tag_levels = 'A')
ggsave("output/plots/Fig6_Labroides.pdf", Fig6, height = 10, width = 8)
}

# function to run niche partitioning analysis
run_niche_null <- function(data){
  output.nichepart <- EcoSimR::niche_null_model(speciesData=data,
                                       algo="ra3", metric="pianka", 
                                       suppressProg=TRUE,nReps=1000)
}
 
#### end of function script #### 