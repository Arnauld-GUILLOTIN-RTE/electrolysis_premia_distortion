

####################################################################################################

### This code is to be run as part of an R project set on the parent folder of this file.
### Results reproducing paper figures were obtained using R version 4.4.2.

####################################################################################################
#                                                                                                  #
#                     Set up environment, paths, config and data links                             #
#                                                                                                  #
####################################################################################################

{
    renv::restore()

    path_parent <- getwd()
    source(file.path(path_parent, "src", "config.R"))
    source(file.path(path_parent, "src", "graph_functions.R"))

    path_simulations <- file.path(path_parent, "data", "simulations")
    path_output <- file.path(path_parent, "data", "output")
    path_output_temp <- file.path(path_parent, "data", "temp")
    path_input <- file.path(path_parent, "data", "input")
    

    ### Setting path to simulations of the NT2040 scenario
    opts_list <- list(
        "pr0"= setSimulationPath(file.path(path_simulations, study_vec[1]), simulation = output_vec[1]),
        "pr1"= setSimulationPath(file.path(path_simulations, study_vec[2]), simulation = output_vec[2]),
        "pr2"= setSimulationPath(file.path(path_simulations, study_vec[3]), simulation = output_vec[3]),
        "pr3"= setSimulationPath(file.path(path_simulations, study_vec[4]), simulation = output_vec[4]))
}

####################################################################################################
#                                                                                                  #
#                               Reproduce figures of the paper                                     #
#                                                                                                  #
####################################################################################################

{
### Import simulation data for each premium level for the NT2040 scenario.
### Can be quite RAM-demanding
data_mcy <- create_dataset(mcYear_example, opts_list) # For the example weather year
data_avg <- create_dataset(NULL, opts_list) # Average over weather years

# Fig. 2a and 3a are obtained from the Excel file "<datetime>_study_summary_2040.xlsx"
# generated in "data/output" by the following command
summarise_studies(parent_folder = path_parent, study_folder = path_simulations, study_name_list = study_vec, 
study_output_list = output_vec, file_suffix = "NT2040")

# Fig. 2b
h2_supply <- plot_h2_supply(data_avg)
ggsave(file.path(path_output, "fig2b.png"), h2_supply,
    width  = 6.3, height = 2.5, units  = "in", dpi = 300)

# Fig. 3b
elec_supply <- plot_elec_supply(data_avg)
ggsave(file.path(path_output, "fig3b.png"), elec_supply,
      width  = 6.3, height = 2, units  = "in", dpi = 300)


# Fig. 4a (a widget in the Viewer panel, not a ggplot)
stack_0_mcy <- plot_stack(data_mcy[["pr0"]], areas = c("nl00"), stack_type = "elec_mix_nl", 
    dateRange = dateRange, mcy=mcYear_example, stack_title = "Fig. 4a", y_min = -100, y_max = 100)
stack_0_mcy

# Fig. 4b (a widget in the Viewer panel, not a ggplot)
stack_3_mcy <- plot_stack(data_mcy[["pr3"]], areas = c("nl00"), stack_type = "elec_mix_nl", 
    dateRange = dateRange, mcy=mcYear_example, stack_title = "Fig. 4b", y_min = -100, y_max = 100)
stack_3_mcy

# Fig. 4c
electrolysis_cons <- plot_electrolysis_cons(area = POWER_AREAS,
    study_data_list = opts_list, dateRange=dateRange, mcy=NULL)
ggsave(file.path(path_output, "/fig4c.png"), electrolysis_cons, 
       width = 7, height = 4, units = "in", dpi = 300)


### For Fig 5 and 6, all simulation output data is loaded, which can exceed available RAM.
### Therefore, an intermediate file was created which contains already-processed 
### output data. The following call load this data, then computes FSCD.

fscd_data <- compute_fscd_data(study_names = study_vec, output_names = output_vec,
                               path_output_temp=path_output_temp)

### If you do want to reprocess the output data from scratch, run the following 
### function call instead. User might need to set a high RAM availability to R, 
### which the R console will warn them about should the need arise.

# fscd_data <- compute_fscd_data(study_names = study_vec, output_names = output_vec,
#                                load_RDS_sim_data = FALSE, path_output_temp=path_output_temp)

# Fig. 5a
plot_fscd_eu <- plot_fscd_eu_agg_concat(fscd_data[["fscd_per_pr_eu_agg_allmcy"]][["pr0"]])
ggsave(file.path(path_output, "Fig5a.png"), plot_fscd_eu,
       width  = 11, height = 4, units  = "in", dpi = 300)

# Fig. 5b
plot_fscd_eu_change_pr <- plot_fscd_change_with_pr(fscd_data[["stat_per_pr_eu_agg_allmcy"]])
ggsave(filename = file.path(path_output,"Fig5b.png"), plot_fscd_eu_change_pr,
    width  = 7, height = 3.5, units  = "in", dpi = 300)

# Fig. 6
fscd_heatmap <- plot_fscd_change_heatmap(fscd_data[["stat_per_pr_country_agg_allmcy"]])
ggsave(file.path(path_output, "Fig6.png"), fscd_heatmap,
    width  = 12, height = 6, units  = "in", dpi = 300)


### Plot functions for Fig7 also create summary Excel files in data/output for aggregated annual averages

# Fig. 7a
opcosts <- boxplot_opcosts(opts_list, path_output)
ggsave(file.path(path_output, "/fig7a.png"), opcosts,
       width  = 3.3, height = 4.5, units  = "in", dpi = 300)

# Fig. 7b
emissions <- boxplot_emissions(opts_list, path_output)
ggsave(file.path(path_output, "/fig7b.png"), emissions,
       width  = 3.3, height = 4.5, units  = "in", dpi = 300)

# Fig. 7c
gas_use <- boxplot_gas(opts_list, path_output, path_parent)
ggsave(file.path(path_output, "/fig7c.png"), gas_use,
       width  = 3.3, height = 4.5, units  = "in", dpi = 300)

# Appendix Fig. B.8
marginal_cost_gap_grid <- plot_heatmap_marginal_cost_gap()
ggsave(file.path(path_output, "figB8.png"), marginal_cost_gap_grid,
       width  = 10, height = 7, units  = "in", dpi = 300)

# Appendix Fig. B.9. This takes a long time to display.
price_gap <- price_gap_plot(opts_list = opts_list, mcYear="all", input_path = path_input)
ggsave(file.path(path_output, "/FigB9.png"),  price_gap,
  width  = 10, height = 3, units  = "in", dpi = 300)

### Appendix C figures are the same as Fig7, but on the sensitivity analysis
### The input data for the sensitivity must thus be set:

opts_list_sens <- list(
    "pr0"= setSimulationPath(file.path(path_simulations, study_vec_sens[1]), simulation = output_vec_sens[1]),
    "pr1"= setSimulationPath(file.path(path_simulations, study_vec_sens[2]), simulation = output_vec_sens[2]),
    "pr2"= setSimulationPath(file.path(path_simulations, study_vec_sens[3]), simulation = output_vec_sens[3]),
    "pr3"= setSimulationPath(file.path(path_simulations, study_vec_sens[4]), simulation = output_vec_sens[4]))

# Appendix Fig. C.10a
opcosts_sens <- boxplot_opcosts(opts_list_sens, path_output)
ggsave(file.path(path_output, "/figC10a.png"), opcosts_sens,
       width  = 3.3, height = 4.5, units  = "in", dpi = 300)

# Appendix Fig. C.10b
emissions_sens <- boxplot_emissions(opts_list_sens, path_output)
ggsave(file.path(path_output, "/figC10b.png"), emissions_sens,
       width  = 3.3, height = 4.5, units  = "in", dpi = 300)

# Appendix Fig. C.10c
gas_use_sens <- boxplot_gas(opts_list_sens, path_output, path_parent)
ggsave(file.path(path_output, "/figC10c.png"), gas_use_sens,
       width  = 3.3, height = 4.5, units  = "in", dpi = 300)
}
