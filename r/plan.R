plan <- drake_plan(
  
  ##########################
  #### I. Load datasets ####
  ##########################
  
  # depth distributions of cleaner wrasse species
  depth = read.csv(file = "data/labroides_depth_sjb.csv"),
  
  # habitat composition of cleaner wrasse territories
  habitat = read.csv("data/labroides_habitat_sjb.csv") %>%
    mutate(species = recode(species, "1" = "L. dimidiatus", "2" = "L. pectoralis", "3" = "L. bicolor")),
  
  # temporal distribution in cleaning activity
  temp = read.csv("data/labroides_temporal_sjb.csv") %>%
    mutate(species = recode(cleanspp, "1" = "L. dimidiatus", "2" = "L. pectoralis", "3" = "L. bicolor")),
  
  # cleaning interaction datasets
  clean.interac = read.csv(file = "data/labroides_cleaninginteractions_sjb.csv"),
  clean.clients = read.csv(file = "data/labroides_clients_meta_sjb.csv"),
  clean.stations = read.csv(file = "data/labroides_sites_meta_sjb.csv"),
  fishID.helper = read.csv(file = "data/fishID_helper.csv"),
  fishID.all = read.csv(file = "data/fishID.merged.all.csv"),
  fish.abun = read.csv(file = "data/labroides_fishcounts_sjb.csv"),
  client.size = read.csv(file = "data/labroides_client_sizes.csv") %>%
    left_join(fishID.all),
  
  ######################
  #### II. ANALYSIS ####
  ######################
  
  ########################
  ########################
  #### 1. MAP & DEPTH ####
  ########################
  ########################
  
  # identify study site by GPS coordinates
  study.loc = data.frame("lat" = -5.472212, "long" = 123.763191),
  
  # extract map of Indonesia
  indonesia = ne_countries(scale = "large", returnclass = "sf", country = "Indonesia"),
  
  # plot map: Fig1A
  output.fig1a = plot_map(indonesia, study.loc),
  
  # process depth data to round depth estimates and name sites
  depth.proc = process_depth_data(depth),
  
  # split depth data into two sites
  depth.hoga = split_depth_data(depth.proc, "hogahome"),
  depth.pinnacle = split_depth_data(depth.proc, "pinnacle"),
  
  # plot depth distributions
  hoga.plot = plot_depth_dist_hoga(depth.hoga),
  pinnacle.plot = plot_depth_dist_pinn(depth.pinnacle),
  
  # combine figures for Fig1
  fig1 = combine_figs(output.fig1a, hoga.plot, pinnacle.plot),
  
  ################################
  #### 2. HABITAT PREFERENCES ####
  ################################
  
  # process habitat
  habitat.processed = process_habitat(habitat),
  
  # run mds 
  hab.mds = run_mds(habitat.processed),
  
  # run permanova and dispersion test
  hab.perm = adonis(habitat.processed ~ species, habitat, distance = "bray"),
  hab.disp = betadisper(vegdist(habitat.processed, distance = "Bray"), habitat$species),
  p.test = permutest(hab.disp, pairwise = TRUE, permutations = 999),
  
  # store mds values
  hab.scores = store_mds_values(hab.mds, habitat),
  mds.points = store_mds_points(hab.mds, "species"),
  
  # calculate species hulls
  species.hulls = plyr::ddply(hab.scores, "species", convex_hull),
  
  # plot mds results
  fig2 = plot_mds_results(hab.scores, species.hulls, mds.points),
  
  
  #################################
  #### 3. TEMPORAL DIFFERENCES ####
  #################################
  
  # process temporal data
  temp.proc = process_temporal(temp),
  
  # split into species; necessary to run models with repeated observations
  temp.ldim = split_temp_data(temp.proc, "L. dimidiatus"),
  temp.lpec = split_temp_data(temp.proc, "L. pectoralis"),
  temp.lbic = split_temp_data(temp.proc, "L. bicolor"),
  
  # run set of brms models to compare temporal cleaning interactions
  # Labroides dimidiatus
  ldim.nspec = run_brms_temp("nb_spec", temp.ldim, "poisson"),
  ldim.nclient = run_brms_temp("nb_client", temp.ldim, "negbinomial"),
  ldim.seconds = run_brms_temp("seconds", temp.ldim, "negbinomial"),
  
  
  output.supptab2a = tab_model(ldim.nspec, ldim.nclient, ldim.seconds,
            pred.labels = c("Intercept (AM)", "Noon", "PM"),
            dv.labels = c("M1: Species richness", "M2: Number of clients", "M3: Seconds inspected"),
            file = "output/tables/LabroidesSuppTab2A.doc"),
  
  # Labroides pectoralis
  lpec.nspec = run_brms_temp("nb_spec", temp.lpec, "poisson"),
  lpec.nclient = run_brms_temp("nb_client", temp.lpec, "negbinomial"),
  lpec.seconds = run_brms_temp("seconds", temp.lpec, "negbinomial"),
  
  
  output.supptab2b = tab_model(lpec.nspec, lpec.nclient, lpec.seconds,
                              pred.labels = c("Intercept (AM)", "Noon", "PM"),
                              dv.labels = c("M1: Species richness", "M2: Number of clients", "M3: Seconds inspected"),
                              file = "output/tables/LabroidesSuppTab2B.doc"),
  
  # Labroides bicolor
  lbic.nspec = run_brms_temp_norand("nb_spec", temp.lbic, "poisson"),
  lbic.nclient = run_brms_temp_norand("nb_client", temp.lbic, "negbinomial"),
  lbic.seconds = run_brms_temp_norand("seconds", temp.lbic, "negbinomial"),
  
  
  output.supptab2c = tab_model(lbic.nspec, lbic.nclient, lbic.seconds,
                               pred.labels = c("Intercept (AM)", "Noon", "PM"),
                               dv.labels = c("M1: Species richness", "M2: Number of clients", "M3: Seconds inspected"),
                               file = "output/tables/LabroidesSuppTab2C.doc"),
  
  # no difference between time periods for all species and their behaviors - combine all periods
  temp.sum = summarize_temporal(temp.proc),
  
  # generic model for species comparisons: three metrics (nb species, nb clients, seconds)
  nb.spec.mod = run_brms_species("mean_nb_spec", temp.sum),
  nb.clients.mod = run_brms_species("mean_nb_client", temp.sum),
  seconds.mod = run_brms_species("mean_seconds", temp.sum),
  
  # predict from models
  # number of species
  nbspec.pred = predict_from_brms(raw = temp.sum, species,
                                  mod = nb.spec.mod,
                                  draws = 1000,
                                  backtrans = TRUE,
                                  transform = "exp"),
  # number of clients
  nbclients.pred = predict_from_brms(raw = temp.sum, species,
                                  mod = nb.clients.mod,
                                  draws = 1000,
                                  backtrans = TRUE,
                                  transform = "exp"),
  
  # seconds 
  seconds.pred = predict_from_brms(raw = temp.sum, species,
                                     mod = seconds.mod,
                                     draws = 1000,
                                     backtrans = TRUE,
                                     transform = "exp"),
  
  # plot results
  output.fig3a = plot_nbspec(newdata = nbspec.pred,
                             rawdata = temp.sum,
                             expr = Species~cleaned~(number~of~species~"15min"^{-1}))
  
  
)

  

  