# Libs
{
  library(antaresRead)
  library(antaresViz)
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(stringr)
  library(ggplot2)
  library(patchwork)
  library(readxl)
  library(writexl)
  library(openxlsx)
  library(magrittr)
  library(scattermore)
  library(sf)
  library(e1071)
  library(cowplot)
  library(signal)
  library(scales)
  library(gridExtra)
  library(antaresProcessing)
}

source(file.path(path_parent, "src", "data_functions.R"))

{
    greens <- colorRampPalette(c("#DBECD0", "#70AD47"))(4)
    greens_darker <- colorRampPalette(c("#B7D7A2", "#4a732f"))(4)
    greys <- colorRampPalette(c("#dbdbdb", "#4d4d4d"))(4)
    blues <- colorRampPalette(c("#8eaeff", "#0011c8"))(4)
    lighter_blues <- colorRampPalette(c("#8eaeff", "#3556fc"))(4)
    oranges <- colorRampPalette(c("#fad2a7", "#995e00"))(4)
    lighter_oranges <- colorRampPalette(c("#f7cc9e", "#d95800"))(4)
    reds <- colorRampPalette(c("#ffb9b9", "#8b0000"))(4)
    yellows <- colorRampPalette(c("#fff8b9", "#c7a600"))(4) 
    purples <- colorRampPalette(c("#C198E0", "#7030A0"))(3)
    purples_darker <- colorRampPalette(c("#a050b3", "#2e004d"))(4)
}

setProdStackAlias(
    name="elec_mix_nl",
    variables = alist(
      `Other non disp.` = `MISC. NDG`,
      `Other thermal`  = othernonres,
      `Run-of-river hydro` = `H. ROR`,
      `Offshore wind`= `WIND OFFSHORE`,
      `Onshore wind` = `WIND ONSHORE`,
      `Solar` = `SOLAR CONCRT.` + `SOLAR PV`,
      `Gas` = GAS,
      Demand = LOAD,
      Electrolysis=z_h2,
      `Balance` = BALANCE,
      `Battery` = `0_battery_pump` + battery_turb,
      Curtailed = `SPIL. ENRG`
    ), 
    colors=c(
      "#cca8ff",
      "#29e2ff",
      "#7eddff",
      "#078c88",
      "#66feb5",
      "#ffb820",
      "#d26505",
      "black",
      "green",
      "#4f4f4f",
      "#fa95f5",
      "#ff7d7d"
    ),
    description = "Power supply NL"
  )

#' Summarize Antares Studies and Export to Excel
#'
#' Creates Excel summaries of annual aggregated power and hydrogen system data
#' across multiple simulation studies and Monte Carlo years.
#'
#' @param parent_folder Character. Path to the parent project folder.
#' @param study_folder Character. Path to the simulations folder.
#' @param study_name_list Character vector. Names of studies to process (e.g., c("NT2040_0", "NT2040_1")).
#' @param study_output_list Numeric or character vector. Output identifiers (-1 for most recent).
#' @param file_suffix Character. Suffix for the output Excel file name.
#'
#' @return Invisibly saves an Excel file with timestamped name in \code{data/output}.
#'   Contains sheets: year_agg_power_data, year_area_power_data, year_agg_h2_data, year_area_h2_data
#'
#' @details Uses a template Excel file located at \code{data/input/summary_template.xlsx}.
#'   Data is appended to sheets for each study.
#'
#' @keywords internal
summarise_studies <- function(parent_folder, study_folder, study_name_list, study_output_list, file_suffix = ""){
  start_time <- Sys.time() %>% format("%Y%m%d-%H%M")
  template_file_path <- file.path(parent_folder, "data","input", "summary_template.xlsx")
  save_file_path <- file.path(parent_folder, "data", "output", paste0(start_time, "_summary_", file_suffix,".xlsx"))
  file.copy(from = template_file_path, to = save_file_path)
  
  for (i in 1:length(study_name_list)){
    study_name <- study_name_list[i]
    output_name <- study_output_list[i]
    message(paste0("Processing "), study_name, " for output ", output_name,"\n")
    study_data <- create_study_summary(study_folder, study_name, output_name)
    append_to_excel(study_data$year_agg_power_data %>% as.data.table(), save_file_path, "year_agg_power_data", study_name)
    append_to_excel(study_data$year_area_power_data %>% as.data.table(), save_file_path, "year_area_power_data", study_name)
    append_to_excel(study_data$year_agg_h2_data %>% as.data.table(), save_file_path, "year_agg_h2_data", study_name)
    append_to_excel(study_data$year_area_h2_data %>% as.data.table(), save_file_path, "year_area_h2_data", study_name)
  }
}

#' Plot Electricity Supply Differences Across Premium Levels
#'
#' Creates a stacked bar plot showing changes in decarbonised and fossil power
#' generation across different hydrogen premium levels for different countries.
#'
#' @param data_antares_hourly_list List. Hourly Antares data containing power generation
#'   by area and premium level. Structure: list of data frames with technology columns.
#'
#' @return ggplot object. Bar plot with countries on x-axis
#'
#' @details The plot shows differential power generation (pr1-pr0, pr2-pr1, pr3-pr2)
#'   for decarbonised and fossil technologies. Facets represent countries.
#'
plot_elec_supply <- function(data_antares_hourly_list){

    fill_cols_power <- c(
      "Decarbonised power generation.pr1-pr0" = blues[1],
      "Decarbonised power generation.pr2-pr1" = blues[2],
      "Decarbonised power generation.pr3-pr2" = blues[3],
      "Fossil power generation.pr1-pr0" = oranges[1],
      "Fossil power generation.pr2-pr1" = oranges[2],
      "Fossil power generation.pr3-pr2" = oranges[3]
    )

    labels_power <- c(
      "Decarbonised power generation.pr1-pr0" = "Decarbonised power generation: 0 → 1 EUR/kg",
      "Decarbonised power generation.pr2-pr1" = "Decarbonised power generation: 1 → 2 EUR/kg",
      "Decarbonised power generation.pr3-pr2" = "Decarbonised power generation: 2 → 3 EUR/kg",
      "Fossil power generation.pr1-pr0" = "Fossil power generation: 0 → 1 EUR/kg",
      "Fossil power generation.pr2-pr1" = "Fossil power generation: 1 → 2 EUR/kg",
      "Fossil power generation.pr3-pr2" = "Fossil power generation: 2 → 3 EUR/kg"
    )

    df_power <- get_data_diff(data_antares_hourly_list)  %>% dplyr::filter(panel == "Power generation")

    res_plot <- ggplot(df_power, aes(
      x = premium_diff,
      y = value,
      fill = interaction(variable, premium_diff)
    )) +
      geom_col(width = 0.9) +
      facet_wrap(~ short_area, nrow = 1, strip.position = "bottom") +
      scale_x_discrete(
        expand = expansion(add = 1),
        breaks = "pr2-pr1",
        labels=""
      ) +
      scale_fill_manual(
        values = fill_cols_power,
        labels = labels_power,
        guide = guide_legend(nrow = 3, ncol=2)
      ) +
      labs(x = NULL, y = "TWh", fill = NULL) +
      theme_bw() +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.4),
        panel.border = element_blank(),
        panel.spacing = unit(0, "cm"),
        legend.position = "bottom",
        legend.key.width = unit(0.2, "cm"),
        legend.key.height = unit(0.2, "cm"),
        
        
        strip.background = element_rect(fill = "white", color = NA),
        strip.text = element_text(color = "black", face = "bold"),
        strip.placement = "outside",
        
        
        axis.text.y = element_text(color = "black"),
        axis.ticks = element_blank(),
        axis.text.x = element_text(margin = margin(t = -10))) 
    
    return(res_plot)
      
}

#' Plot Hydrogen Supply Sources Across Premium Levels
#'
#' Creates a stacked bar plot showing changes in electrolysis, hydrogen imports,
#' and trade balance across different hydrogen premium levels.
#'
#' @param data_antares_hourly_list List. Hourly Antares data containing hydrogen supply
#'   by area and premium level. Structure: list of data frames with hydrogen tech columns.
#'
#' @return ggplot object. Bar plot with countries on x-axis.
#'
#' @details Shows differential hydrogen supply (pr1-pr0, pr2-pr1, pr3-pr2) from
#'   electrolysis, third-country imports, and net trade with neighbours. Facets represent countries.
#'
plot_h2_supply <- function(data_antares_hourly_list){

    fill_cols_h2 <- c(
      "Electrolysis.pr1-pr0" = greens[2],
      "Electrolysis.pr2-pr1" = greens[3],
      "Electrolysis.pr3-pr2" = greens[4],
      
      "Hydrogen imported from third countries.pr1-pr0" = purples[1],
      "Hydrogen imported from third countries.pr2-pr1" = purples[2],
      "Hydrogen imported from third countries.pr3-pr2" = purples[3],
      
      "Hydrogen trade balance.pr1-pr0" = oranges[1],
      "Hydrogen trade balance.pr2-pr1" = oranges[2],
      "Hydrogen trade balance.pr3-pr2" = oranges[3]
      )

    labels_h2 <- c(
      "Electrolysis.pr1-pr0" = "Electrolysis: 0 → 1 EUR/kg",
      "Electrolysis.pr2-pr1" = "Electrolysis: 1 → 2 EUR/kg",
      "Electrolysis.pr3-pr2" = "Electrolysis: 2 → 3 EUR/kg",
      "Hydrogen imported from third countries.pr1-pr0" =
        "Import from third countries: 0 → 1 EUR/kg",
      "Hydrogen imported from third countries.pr2-pr1" =
        "Import from third countries: 1 → 2 EUR/kg",
      "Hydrogen imported from third countries.pr3-pr2" =
        "Import from third countries: 2 → 3 EUR/kg",
      "Hydrogen trade balance.pr1-pr0" = "EU Net trade: 0 → 1 EUR/kg",
      "Hydrogen trade balance.pr2-pr1" = "EU Net trade: 1 → 2 EUR/kg",
      "Hydrogen trade balance.pr3-pr2" = "EU Net trade: 2 → 3 EUR/kg"
    )

    df_h2 <- get_data_diff(data_antares_hourly_list)  %>% dplyr::filter(panel == "Hydrogen supply")

    res_plot <- ggplot(
        df_h2, aes(
      x = premium_diff,
      y = value,
      fill = interaction(variable, premium_diff)
    )) +
      geom_col(width = 0.9) +
      facet_wrap(~ short_area, nrow = 1, strip.position = "bottom") +
      scale_x_discrete(
        expand = expansion(add = 1),
        breaks = "pr2-pr1",
        labels=""
      ) +
      scale_fill_manual(
        values = fill_cols_h2,
        labels = labels_h2,
        guide = guide_legend(nrow = 3, ncol=3)
      ) +
      labs(x = NULL, y = "TWh", fill = NULL) +
      theme_bw() +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.4),
        panel.border = element_blank(),
        panel.spacing = unit(0, "cm"),
        legend.position = "bottom",
        legend.key.width = unit(0.2, "cm"),
        legend.key.height = unit(0.2, "cm"),
        
        
        strip.background = element_rect(fill = "white", color = NA),
        strip.text = element_text(color = "black", face = "bold"),
        strip.placement = "outside",
        
        
        axis.text.y = element_text(color = "black"),   
        axis.ticks = element_blank(),
        axis.text.x = element_text(margin = margin(t = -10))
        )

    return(res_plot)
}

#' Create Production Stack Visualization
#'
#' Generates an interactive production stack plot for specified area(s) and time period,
#' using the AntaresViz visualization framework.
#'
#' @param data List. Antares data format with "areas" element containing hourly generation data.
#' @param areas Character or character vector. Area code(s) to visualize (e.g., "nl00").
#' @param dateRange POSIXct vector of length 2. Start and end dates for visualization.
#' @param stack_type Character. Stack configuration name (e.g., "elec_mix_nl").
#' @param mcy Numeric. Monte Carlo year identifier (for reference in title).
#' @param stack_title Character. Title for the stack visualization.
#' @param y_min Numeric. Minimum y-axis limit (GWh).
#' @param y_max Numeric. Maximum y-axis limit (GWh).
#'
#' @return Interactive widget (prodStack object from antaresViz package).
#'
#' @details Automatically aggregates multiple areas. Returns to Viewer panel in RStudio.
#'
plot_stack <- function(data, areas, dateRange, stack_type,mcy, stack_title, y_min, y_max){
  areas_vec <- c(areas)
  if(length(areas_vec) == 1){
    stack_data <- data[["areas"]] %>%
      dplyr::filter(area %in% areas_vec) %>%
      group_by(area, mcYear, timeId, time, day, month, hour) %>%
      summarise(across(-all_of(c()), sum)) %>%
      ungroup() %>%
      mutate(BALANCE = -1 * BALANCE) %>%
      mutate(LOAD = -1 * LOAD) %>%
      mutate(`SPIL. ENRG` = -1 * `SPIL. ENRG`) %>%
      mutate(area = "area_list") %>%
      as.antaresDataTable(., synthesis=FALSE, timeStep="hourly", type="areas")
  } else{
    stack_data <- data[["areas"]] %>%
      dplyr::filter(area %in% areas_vec) %>%
      group_by(mcYear, timeId, time, day, month, hour) %>%
      summarise(across(-all_of(c("area")), sum)) %>%
      ungroup() %>%
      mutate(BALANCE = 0) %>%
      mutate(LOAD = -1 * LOAD) %>%
      mutate(`SPIL. ENRG` = -1 * `SPIL. ENRG`) %>%
      mutate(area = "area_list") %>%
      as.antaresDataTable(., synthesis=FALSE, timeStep="hourly", type="areas")
  }

  stack <- prodStack(stack_data, areas = "area_list", dateRange = dateRange, unit = "GWh",
            interactive = FALSE, stack = stack_type, main = stack_title, legend = TRUE,
            yMin = y_min, yMax = y_max, width = "1500px", height="1000px")
  stack
}

#' Plot Electrolysis consumption Over Time
#'
#' Creates a line plot showing hourly electrolysis consumption across premium levels
#' for specified aggregated areas and for a given time period.
#'
#' @param area Character or character vector. Area code(s) for power nodes.
#' @param study_data_list List. Antares simulation data for each premium level (pr0, pr1, pr2, pr3).
#' @param dateRange POSIXct vector of length 2. Start and end dates for time series.
#' @param mcy Numeric or NULL. Monte Carlo year (-1 uses all years, NULL uses average).
#'
#' @return ggplot object. Line plot with separate colored line per premium level (0, 1, 2, 3 EUR/kg).
#'
#' @details Extracts flows from area to z_h2 node of model, and converts to GW units.
#'   Groups over time and aggregates across input areas.
#'
plot_electrolysis_cons <- function(area, study_data_list, dateRange, mcy){
  area <- as.vector(area)
  plot_data <- import_electrolysis_cons(study_data_list, mcy) %>%
    dplyr::filter(time >= dateRange[1] & time <= dateRange[2] & link %in% paste0(area, " - z_h2")) %>%
    mutate(`UCAP LIN.` = `UCAP LIN.` / 1000) %>%
    group_by(time, sim) %>%
    summarise(across(all_of(c("UCAP LIN.")), sum))
  
  elz_plot <- ggplot(plot_data, aes(x = time, y = `UCAP LIN.`, color = sim)) +
    geom_line(size = 1) +
    labs(
      y = "GW",
      x="",
      color = "Premium level (EUR/kg)"
    ) +
    scale_color_manual(values = greens_darker, labels = c("0", "1", "2", "3")) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
    theme_minimal(base_size = 14) +
    scale_x_datetime(
      date_breaks = "1 day",
      expand = c(0, 0),
      date_labels = "%b %d" 
    )+
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major = element_line(color = "grey80", size = 0.5),
      panel.grid.minor = element_line(color = "grey80", size = 0.5)
    
    )
  
  return(elz_plot)
}

#' Create Operating Costs Boxplot
#'
#' Generates a boxplot comparing operating costs across sectors (electricity, hydrogen, total)
#' and hydrogen premium levels. Within each boxplot: annual aggregated, all individual Monte Carlo years.
#'
#' @param opts_list List. Antares output for each premium level (pr0, pr1, pr2, pr3).
#' @param outputs_folder Character. Path to output folder for saving statistics Excel file.
#'
#' @return ggplot object. Boxplot with sector on x-axis, premium level as fill color.
#'
#' @details Calls \code{get_opcosts()} internally. Creates summary statistics file
#'   \code{opcosts_stats.xlsx} in outputs folder. Y-axis in billions EUR/year.
#'
boxplot_opcosts <- function(opts_list, outputs_folder){
  opcosts_agg <- get_opcosts(opts_list, outputs_folder)
    opcost_boxplot <- ggplot(
        opcosts_agg %>%
        pivot_longer(cols = c(`OP. COST_h2`, `OP. COST_elec`, opcost_total), names_to = "sector", values_to = "opcost") %>%
        mutate(premium = sub(" EUR/kg", "", premium),
                sector = case_when(
            sector=="OP. COST_h2"~ "Hydrogen system",
            sector=="OP. COST_elec"~ "Electricity system",
            sector=="opcost_total"~ "Energy system"
        )) %>%
        mutate(sector = factor(sector, levels = c("Electricity system", "Hydrogen system", "Energy system"))), aes(x = sector, y = opcost, fill = premium)) +
        geom_boxplot(position = position_dodge(width = 0.8), outlier.size = -1) +
        labs(x = "", 
            y = "Bil. EUR/a", 
            fill = "Premium level (EUR/kg)",
            title = "") +
        theme_minimal()+
        guides(
        fill = guide_legend(
            title.position = "top",
            title.hjust = 0.5
        )
        )+
        theme(legend.position = "bottom",
            panel.grid.minor.y = element_blank(),
            panel.grid.major.y = element_line(color = "grey70"),
            panel.grid.major.x = element_line(color = "grey70"),
            axis.text.x = element_text(size = 10, angle = 45, hjust = 1, color="black"),    
            axis.text.y = element_text(size = 10, color="black"),      
            axis.title.y = element_text(size = 10),
            legend.title = element_text(size = 10),     
            legend.text  = element_text(size = 10),
            plot.title = element_text(size = 10))+
        scale_fill_manual(values = lighter_blues) +
        scale_y_continuous(breaks = scales::breaks_width(25))+
        expand_limits(y = 0)
    return(opcost_boxplot)
}

#' Create CO2 Emissions Boxplot
#'
#' Generates a boxplot comparing CO2 emissions across sectors (electricity, hydrogen, total)
#' and hydrogen premium levels. Within each boxplot: annual aggregated, all individual Monte Carlo years.
#'
#' @param opts_list List. Antares options for each premium level (pr0, pr1, pr2, pr3).
#' @param outputs_folder Character. Path to output folder for saving statistics Excel file.
#'
#' @return ggplot object. Boxplot with sector on x-axis, premium level as fill color.
#'
#' @details Calls \code{get_emissions()} internally. Creates summary statistics file
#'   \code{emission_stats.xlsx} in outputs folder. Y-axis in million tonnes/year.
#'
boxplot_emissions <- function(opts_list, outputs_folder){
  emission_agg <- get_emissions(opts_list, outputs_folder)
    emission_boxplot <- ggplot(
        emission_agg %>%
        pivot_longer(cols = c(`CO2 EMIS._h2`, `CO2 EMIS._elec`, CO2_EMIS_total), names_to = "sector", values_to = "emissions") %>%
        mutate(premium = sub(" EUR/kg", "", premium),
                sector = case_when(
            sector=="CO2 EMIS._h2"~ "Hydrogen system",
            sector=="CO2 EMIS._elec"~ "Electricity system",
            sector=="CO2_EMIS_total"~ "Energy system"
        )) %>%
        mutate(sector = factor(sector, levels = c("Electricity system", "Hydrogen system", "Energy system"))), 
        aes(x = sector, y = emissions, fill = premium)) +
        geom_boxplot(position = position_dodge(width = 0.8), outlier.size = -1) +
        labs(x = "", 
            y = "Mil. t/a", 
            fill = "Premium level (EUR/kg)",
            title = "") +
        theme_minimal()+
        guides(
        fill = guide_legend(
            title.position = "top",
            title.hjust = 0.5
        )
        )+
        theme(legend.position = "bottom", 
            panel.grid.minor.y = element_blank(),
            panel.grid.major.y = element_line(color = "grey70"),
            panel.grid.major.x = element_line(color = "grey70"),
            axis.text.x = element_text(size = 10, angle = 45, hjust = 1, color="black"),
            axis.text.y = element_text(size = 10, color="black"),    
            axis.title.y = element_text(size = 10),
            legend.title = element_text(size = 10),   
            legend.text  = element_text(size = 10),
            plot.title = element_text(size = 10))+
        scale_fill_manual(values = greys) +
        scale_y_continuous(breaks = scales::breaks_width(25))+
        expand_limits(y = 0)

    return(emission_boxplot)
}

#' Create Natural Gas Consumption Boxplot
#'
#' Generates a boxplot comparing natural gas consumption across sectors (electricity, hydrogen, total)
#' and hydrogen premium levels. Within each boxplot: annual aggregated, all individual Monte Carlo years.
#'
#' @param opts_list List. Antares options for each premium level (pr0, pr1, pr2, pr3).
#' @param outputs_folder Character. Path to output folder for saving statistics Excel file.
#' @param parent_folder Character. Path to parent project folder (for gas efficiencies file).
#'
#' @return ggplot object. Boxplot with sector on x-axis, premium level as fill color.
#'
#' @details Calls \code{get_gas_use()} internally. Creates summary statistics file
#'   \code{gas_stats.xlsx} in outputs folder. Y-axis in TWh (natural gas equivalent).
#'
boxplot_gas <- function(opts_list, outputs_folder, parent_folder){
  gas_use <- get_gas_use(opts_list, outputs_folder, parent_folder)
    gas_cons_plot <- ggplot(
        gas_use %>%
        mutate(sector = factor(sector, levels = c("Electricity system", "Hydrogen system", "Energy system"))), 
        aes(x = sector, y = methane_cons_TWh, fill = pr)) +
        geom_boxplot(position = position_dodge(width = 0.8), outlier.size = -1) +
        labs(x = "", 
            y = "TWh", 
            fill = "Premium level (EUR/kg)",
            title = "") +
        theme_minimal()+
        guides(
        fill = guide_legend(
            title.position = "top",
            title.hjust = 0.5
        )
        )+
        theme(legend.position = "bottom", 
            panel.grid.minor.y = element_blank(),
            panel.grid.major.y = element_line(color = "grey70"),
            panel.grid.major.x = element_line(color = "grey70"),
            axis.text.x = element_text(size = 10, angle = 45, hjust = 1, color="black"),
            axis.text.y = element_text(size = 10, color="black"), 
            axis.title.y = element_text(size = 10),
            legend.title = element_text(size = 10),
            legend.text  = element_text(size = 10),
            plot.title = element_text(size = 10))+
        scale_fill_manual(values = lighter_oranges) +
        scale_y_continuous(breaks = scales::breaks_width(100))+
        expand_limits(y = 0)
 
    return(gas_cons_plot)
}

#' Create Marginal Cost Gap Heatmap
#'
#' Generates a heatmap showing hydrogen production marginal cost gaps across continuous carbon prices
#' and hydrogen premium levels for alternative technologies vs. benchmarks.
#'
#' @return ggplot object. Faceted heatmap with technology rows, H2 benchmark columns.
#'
#' @details Uses exogenous model parameters (variable costs, carbon intensity, efficiency) defined in config.R.
#'   Grid spans carbon price 0-300 EUR/tCO2 and premium 0-3 EUR/kg H2 (100 points each).
#'   Vertical dashed line marks reference carbon price of the model.
#'   No simulation output data used; purely illustrative of model parameters.
#'
plot_heatmap_marginal_cost_gap <- function(){
  grid <- expand.grid(carbon_price = CARBON_PRICE_RANGE, premium = PREMIUM_RANGE)

  grid$smr <- mapply(production_cost, "smr", grid$carbon_price, grid$premium)
  grid$smr_ccs <- mapply(production_cost, "smr_ccs", grid$carbon_price, grid$premium)
  grid$import <- mapply(production_cost, "import_ship_de", grid$carbon_price, grid$premium)
  grid$res_smr <- mapply(production_cost, "res", grid$carbon_price, grid$premium) - grid$smr
  grid$nuclear_smr <- mapply(production_cost, "nuclear", grid$carbon_price, grid$premium) - grid$smr
  grid$ccgt_smr <- mapply(production_cost, "ccgt", grid$carbon_price, grid$premium)- grid$smr
  grid$coal_smr <- mapply(production_cost, "coal", grid$carbon_price, grid$premium)- grid$smr
  grid$ocgt_smr <- mapply(production_cost, "ocgt", grid$carbon_price, grid$premium)- grid$smr
  grid$res_smr_ccs <- mapply(production_cost, "res", grid$carbon_price, grid$premium) - grid$smr_ccs
  grid$nuclear_smr_ccs <- mapply(production_cost, "nuclear", grid$carbon_price, grid$premium) - grid$smr_ccs
  grid$ccgt_smr_ccs <- mapply(production_cost, "ccgt", grid$carbon_price, grid$premium)- grid$smr_ccs
  grid$coal_smr_ccs <- mapply(production_cost, "coal", grid$carbon_price, grid$premium)- grid$smr_ccs
  grid$ocgt_smr_ccs <- mapply(production_cost, "ocgt", grid$carbon_price, grid$premium)- grid$smr_ccs
  grid$res_import <- mapply(production_cost, "res", grid$carbon_price, grid$premium) - grid$import
  grid$nuclear_import <- mapply(production_cost, "nuclear", grid$carbon_price, grid$premium) - grid$import
  grid$ccgt_import <- mapply(production_cost, "ccgt", grid$carbon_price, grid$premium)- grid$import
  grid$coal_import <- mapply(production_cost, "coal", grid$carbon_price, grid$premium)- grid$import
  grid$ocgt_import <- mapply(production_cost, "ocgt", grid$carbon_price, grid$premium)- grid$import
  
  grid_long <- grid %>%
    pivot_longer(cols = as.vector(outer(
                                        c("res" ,"nuclear", "ccgt", "ocgt", "coal"),
                                        c("_smr", "_smr_ccs", "_import"), 
                                        paste0)),
                 names_to = "variable",
                 values_to = "value")
  
  grid_long$variable <- factor(grid_long$variable,
                               levels = as.vector(outer(
                                 c("res", "nuclear", "ccgt", "ocgt", "coal"),
                                 c("_smr_ccs", "_smr", "_import"), 
                                 paste0))
                               )
  
  grid_long <- grid_long %>%
    separate(variable,
             into = c("tech", "benchmark"),
             sep = "_",
             extra = "merge")
  
  grid_long$tech <- factor(
    grid_long$tech,
    levels = c("res", "nuclear", "ccgt", "ocgt", "coal"),
    labels = c("VRE", "Nuclear", "Gas CCGT", "Gas OCGT", "Coal")
  )
  
  grid_long$benchmark <- factor(
    grid_long$benchmark,
    levels = c("smr_ccs", "smr", "import"),
    labels = c("SMR with CCS", "SMR", "Imports from\nthird countries")
  )
  
  val_min <- min(grid_long$value, na.rm = TRUE)
  val_max <- max(grid_long$value, na.rm = TRUE)

    heatmap <- ggplot(grid_long, aes(x = premium, y = carbon_price, fill = value)) +
    geom_tile() +
    facet_grid(
        rows = vars(benchmark),
        cols = vars(tech)
    ) +
    scale_fill_gradient2(
        low = "darkblue",
        mid = "white",
        high = "black",
        midpoint = 0,      # White centered at 0
        limits = c(val_min, val_max)  # Shared scale across all facets
    ) +
    geom_hline(yintercept = CARBON_PRICE_REF, 
                linetype = "dotted", 
                color = "black") +
    labs(
        title = "",
        y = expression("Carbon Price (EUR / t"[CO2]*")"),
        x = expression("Premium (EUR / kg"[H2]*")"),
        fill = expression("Marginal cost gap (EUR/MWh"[H2]*")")
    ) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_minimal() +
    theme(
        strip.text = element_text(size = 12, face = "bold"),
        legend.position = "bottom",
        panel.border = element_rect(color = "grey60", fill = NA, size=0.4),
        panel.spacing = unit(1, "lines"),
        axis.text.x = element_text(size = 12, color = "black"), 
        axis.text.y = element_text(size = 12, color = "black") 
    )
    return(heatmap)
}

#' Create Price Arbitrage Scatter Plot
#'
#' Generates a hexbin scatter plot showing the relationship between electrolysis
#' hourly load factor and price arbitrage (η*(p_h2 + premium) - p_elec) across all premium levels,
#' for all hours of all areas of the selected Monte Carlo year(s). Faceted by premium level.
#'
#' @param opts_list List. Antares options for each premium level (pr0, pr1, pr2, pr3).
#' @param mcYear Numeric or "all". Monte Carlo year(s) to include.
#' @param input_path Character. Path to input data folder containing electrolysis_capacities.csv.
#'
#' @return ggplot object. Faceted hexbin density plot with one facet per premium level.
#'
#' @details Uses log10 color scale (viridis 'magma' palette) for bin count visualization.
#'   Load factor computed as electrolysis hourly production / installed capacity wiithin each area.
#'
price_gap_plot <- function(opts_list, mcYear, input_path){
    price_gap_data <- price_elz_scatter_data(opts_list = opts_list, mcYear = "all", input_path=input_path) %>%
    dplyr::filter(!link_area %in% c("dekf", "dkkf", "ukni")) # areas with no electrolysis
    {
    categories <- unique(price_gap_data$premium)
    category_colors <- setNames(RColorBrewer::brewer.pal(length(categories), "Set1"), categories)
    
    p <- ggplot() + theme_minimal()

    for(i in 1:length(categories)){
        subset_data <- subset(price_gap_data, premium == categories[i])
        
        p <- p + geom_scattermore(
        data = subset_data,
        aes(x = load_factor, y = price_gap, color = categories[i]),
        pointsize = 10
        )
    }
    
    # Add manual scale to create legend
    p <- p + scale_color_manual(
        name = "Premium",
        values = category_colors
    )
    }
    {
    custom_titles <- c(
        "0 EUR/kg" = "pr == 0 * \"EUR/kg\"[H2]",
        "1 EUR/kg" = "pr == 1 * \"EUR/kg\"[H2]",
        "2 EUR/kg" = "pr == 2 * \"EUR/kg\"[H2]",
        "3 EUR/kg" = "pr == 3 * \"EUR/kg\"[H2]"
    )
    price_gap_data$premium_2 <- factor(
        price_gap_data$premium,
        levels = names(custom_titles),
        labels = custom_titles
    )
    
    
    
    price_arbitrage_plot <- ggplot(price_gap_data, aes(x = load_factor, y = price_gap)
    ) +
        geom_scattermore(pointsize = 1, color = "black", alpha = 0.03) +
        stat_bin2d(
        aes(fill = after_stat(count)),
        bins = 60,
        alpha = 0.9
        ) +
        scale_fill_viridis_c(trans = "log10", option = "magma") +
        facet_wrap(~ premium_2, nrow = 1, ncol = 4, labeller = label_parsed) +
        theme_minimal()+
        theme(panel.border = element_rect(color = "grey50", fill = NA, size = 0.5)) +
        ylab(expression(eta%*%(p[h]+pr) -p[e])) +
        labs(x = "Electrolysis hourly load factor")   
        }
        return(price_arbitrage_plot)
}

#' Create Flexibility Solution Contribution Distribution (FSCD) Boxplot
#'
#' Generates boxplot showing FSCD per solution, across annual, weekly, and daily timescales.
#'
#' @param fscd_data List. FSCD contributions with elements:
#'   \describe{
#'     \item{lf_contributions}{Annual timescale contributions (data frame)}
#'     \item{mf_contributions}{Weekly timescale contributions (data frame)}
#'     \item{hf_contributions}{Daily timescale contributions (data frame)}
#'   }
#' @param param_title Character. Plot title (default: empty).
#' @param save Logical. Whether to save plot to file (default: FALSE).
#' @param save_name Character. Filename suffix if saving (default: empty).
#' @param cols_to_keep_ordered Character vector. Technologies to display in order.
#' @param timescale_to_keep Character vector. Timescales to include ("Annual", "Weekly", "Daily").
#' @param remove_yaxis_title Logical. Whether to hide y-axis label (default: FALSE).
#' @param y_min Numeric. Minimum y-axis limit (default: -25).
#' @param y_max Numeric. Maximum y-axis limit (default: 75).
#'
#' @return ggplot object. Boxplot with technologies on x-axis, contribution (%) on y-axis,
#'   colored by timescale. Mean values shown as points.
#'
plot_fscd <- function(fscd_data, param_title="", save = FALSE, 
                    save_name = "", cols_to_keep_ordered = c("Nuclear","Conventional hydro","Gas", "Other thermal","Battery",
                                                                "Pumped storage hydro", "Interconnectors","Electrolysis",
                                                                "Demand response","Curtailment", "Loss of load"), 
                    timescale_to_keep = c("Annual", "Weekly", "Daily"),
                    remove_yaxis_title = FALSE,
                    y_min = -25,
                    y_max = 75){
# FSCD data as a list containing lf_contributions, mf_contributions, hf_contributions
lf_contributions <- fscd_data[["lf_contributions"]]
mf_contributions <- fscd_data[["mf_contributions"]]
hf_contributions <- fscd_data[["hf_contributions"]]

lfs <- nrow(lf_contributions)
mfs <- nrow(mf_contributions)
hfs <- nrow(hf_contributions)

flex_contributions <- c(lf_contributions$nuclear,mf_contributions$nuclear,hf_contributions$nuclear,
                        lf_contributions$hydro,mf_contributions$hydro,hf_contributions$hydro,
                        lf_contributions$gas,mf_contributions$gas,hf_contributions$gas,
                        lf_contributions$other_thermal,mf_contributions$other_thermal,hf_contributions$other_thermal,
                        lf_contributions$battery,mf_contributions$battery,hf_contributions$battery,
                        lf_contributions$psh,mf_contributions$psh,hf_contributions$psh,
                        lf_contributions$interco,mf_contributions$interco,hf_contributions$interco,
                        lf_contributions$electrolysis,mf_contributions$electrolysis,hf_contributions$electrolysis,
                        lf_contributions$demand_resp,mf_contributions$demand_resp,hf_contributions$demand_resp,
                        lf_contributions$curtailment,mf_contributions$curtailment,hf_contributions$curtailment,
                        lf_contributions$loss_of_load,mf_contributions$loss_of_load,hf_contributions$loss_of_load)
horizon_tags       <- factor(rep(c(rep("Annual",lfs),rep("Weekly",mfs),rep("Daily",hfs)),times=11),
                                levels=c("Annual","Weekly","Daily"))
lever_tags         <- factor(c(rep("Nuclear",(lfs+mfs+hfs)),
                                rep("Conventional hydro",(lfs+mfs+hfs)),
                                rep("Gas",(lfs+mfs+hfs)),
                                rep("Other thermal",(lfs+mfs+hfs)),
                                rep("Battery",(lfs+mfs+hfs)),
                                rep("Pumped storage hydro",(lfs+mfs+hfs)),
                                rep("Interconnectors",(lfs+mfs+hfs)),
                                rep("Electrolysis",(lfs+mfs+hfs)),
                                rep("Demand response",(lfs+mfs+hfs)),
                                rep("Curtailment",(lfs+mfs+hfs)),
                                rep("Loss of load",(lfs+mfs+hfs))),
                                levels=c("Nuclear","Conventional hydro","Gas", "Other thermal","Battery",
                                "Pumped storage hydro", "Interconnectors","Electrolysis",
                                "Demand response","Curtailment", "Loss of load"))

to_plot <- data.frame(flex_contributions,horizon_tags,lever_tags) %>%
    dplyr::filter(lever_tags %in% cols_to_keep_ordered) %>%
    mutate(lever_tags = factor(lever_tags, levels = cols_to_keep_ordered)) %>%
    dplyr::filter(horizon_tags %in% timescale_to_keep)

plot_result <- ggplot(to_plot, aes(x=lever_tags, y=flex_contributions, colour=horizon_tags)) +
    geom_boxplot(position=position_dodge(0.5),
                outlier.size = -1) +
    stat_summary(
    fun = mean,                    
    geom = "point",                
    shape = 16,                    
    size = 3,
    stroke=2,
    aes(colour = horizon_tags))+
    theme_bw()+
    coord_cartesian(ylim=c(y_min,y_max)) +
    labs(title=param_title ,x="", y=ifelse(remove_yaxis_title,"","Flexibility contribution (%)")) +
    theme(text = element_text(size=11, color = "black"),
    axis.title.x = element_text(size = 11, margin = margin(t = 0), color = "black"),
    axis.title.y = element_text(size = 11, color = "black", margin = margin(r = 2)),
    axis.text.x  = element_text(size = 11, angle = 35, hjust = 1, color = "black"),
    axis.text.y  = element_text(size = 11, color = "black"),
    legend.title = element_text(size = 11),
    legend.text  = element_text(size = 11),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    legend.box.spacing = unit(0.01, "cm"),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
    panel.grid.minor.y = element_blank()) +
    scale_y_continuous(breaks = seq(y_min, y_max, by = 25))+
    scale_colour_manual(name="Timescale",
                        values = c("Annual" = "purple", 
                                    "Weekly" = "deepskyblue4", 
                                    "Daily" = "goldenrod4"))


if(save==TRUE){
    ggsave(
    paste0(outputs_folder, "/fig_fscd_", save_name,".png"),
    plot_result,
    width  = 5,
    height = 6,
    units  = "in",
    dpi = 300
    )
}
return(plot_result)
}

#' Create Annual FSCD Boxplot for EU Aggregated Data
#'
#' Wrapper function to plot FSCD contributions at annual timescale.
#'
#' @param fscd_eu_agg_data List. EU-aggregated FSCD data (from \code{compute_fscd_data()}).
#'
#' @return ggplot object. FSCD boxplot for annual timescale.
#'
#' @seealso \code{\link{plot_fscd}}
plot_annual_fscd_eu_agg <- function(fscd_eu_agg_data){
    annual_fscd <- plot_fscd(fscd_eu_agg_data, 
            cols_to_keep_ordered = c("Curtailment", "Gas", "Electrolysis", "Nuclear", 
                                    "Demand response", "Conventional hydro","Other thermal", 
                                    "Pumped storage hydro", "Battery", "Loss of load"), 
            timescale_to_keep = "Annual",
            param_title = "Annual timescale")
    return(annual_fscd)
}

#' Create Weekly FSCD Boxplot for EU Aggregated Data
#'
#' Wrapper function to plot FSCD contributions at weekly timescale.
#'
#' @param fscd_eu_agg_data List. EU-aggregated FSCD data (from \code{compute_fscd_data()}).
#'
#' @return ggplot object. FSCD boxplot for weekly timescale.
#'
#' @seealso \code{\link{plot_fscd}}
plot_weekly_fscd_eu_agg <- function(fscd_eu_agg_data){
    weekly_fscd <- plot_fscd(fscd_eu_agg_data, 
            cols_to_keep_ordered = c("Electrolysis", "Curtailment", "Gas", "Conventional hydro",
                                    "Nuclear", "Pumped storage hydro", "Other thermal",
                                    "Demand response", "Battery", "Loss of load"), 
            timescale_to_keep = "Weekly",
            param_title = "Weekly timescale",
            remove_yaxis_title=TRUE)
    return(weekly_fscd)
}

#' Create Daily FSCD Boxplot for EU Aggregated Data
#'
#' Wrapper function to plot FSCD contributions at daily timescale.
#'
#' @param fscd_eu_agg_data List. EU-aggregated FSCD data (from \code{compute_fscd_data()}).
#'
#' @return ggplot object. FSCD boxplot for daily timescale.
#'
#' @seealso \code{\link{plot_fscd}}
plot_daily_fscd_eu_agg <- function(fscd_eu_agg_data){
    daily_fscd <- plot_fscd(fscd_eu_agg_data, 
          cols_to_keep_ordered = c("Curtailment", "Battery", "Pumped storage hydro",
                                   "Electrolysis", "Conventional hydro", "Demand response",
                                   "Gas", "Nuclear", "Other thermal", "Loss of load"), 
          timescale_to_keep = "Daily",
          param_title = "Daily timescale",
          remove_yaxis_title=TRUE)
}

#' Concatenate FSCD Plots Across Timescales
#'
#' Combines annual, weekly, and daily FSCD boxplots in a single row layout.
#'
#' @param fscd_eu_agg_data List. EU-aggregated FSCD data (from \code{compute_fscd_data()}).
#'
#' @return ggplot object (via cowplot::plot_grid). Single-row layout with three FSCD plots.
#'
#' @seealso \code{\link{plot_annual_fscd_eu_agg}}, \code{\link{plot_weekly_fscd_eu_agg}},
#'   \code{\link{plot_daily_fscd_eu_agg}}
plot_fscd_eu_agg_concat <- function(fscd_eu_agg_data){
    annual_fscd <- plot_annual_fscd_eu_agg(fscd_eu_agg_data)
    weekly_fscd <- plot_weekly_fscd_eu_agg(fscd_eu_agg_data)
    daily_fscd <- plot_daily_fscd_eu_agg(fscd_eu_agg_data)

    fscd_row_plot <- plot_grid(
        annual_fscd,
        weekly_fscd,
        daily_fscd,
        nrow = 1
        )
    return(fscd_row_plot)
}

#' Plot FSCD Change Across Premium Levels with Uncertainty Bands
#'
#' Creates a line plot with interquartile range bands showing how flexibility contributions
#' change with hydrogen premium levels across annual, weekly, and daily timescales for a subset of solutions.
#'
#' @param eu_stats_nested List. Nested statistics by premium level (from \code{compute_fscd_data()}).
#'   Expected structure: list with names c("pr0", "pr1", "pr2", "pr3"), each containing
#'   statistical summary data frames with columns for technology, contribution, stat.
#'
#' @return ggplot object. Line plot with 25-75th percentile shaded bands and median line,
#'   faceted by timescale, one color per solution (gas, electrolysis, curtailment, battery).
#'
#' @details Y-axis limits fixed to (-6, 42) percentage points. X-axis shows premium levels 0-3 EUR/kg.
#'   Ribbons represent interquartile range (p25-p75); lines show median (p50).
#'
plot_fscd_change_with_pr <- function(eu_stats_nested){
    eu_stats<- eu_stats_nested %>% get_fscd_eu_stats()
  stat_fscd_per_contrib <- list()
  four_technologies <- c("gas", "electrolysis", "curtailment", "battery")
  
  fscd_p25 <- eu_stats[["p25"]] %>% pivot_wider(names_from = stat, values_from = value)
  fscd_p50 <- eu_stats[["p50"]] %>% pivot_wider(names_from = stat, values_from = value)
  fscd_p75 <- eu_stats[["p75"]] %>% pivot_wider(names_from = stat, values_from = value)
  
  fscd_merged <- merge(x = fscd_p25, y=fscd_p50, 
                       by.x = c("technology", "pr", "contrib"), by.y= c("technology", "pr", "contrib")) %>%
    merge(x= ., y = fscd_p75, by.x = c("technology", "pr", "contrib"), by.y= c("technology", "pr", "contrib")) %>%
    dplyr::filter(technology %in% four_technologies) %>%
      mutate(technology = factor(technology, levels = four_technologies)) %>%
    mutate(contrib = factor(contrib, 
           levels = c("lf_contributions", "mf_contributions", "hf_contributions")))
  
  fscd_change <- ggplot(
    fscd_merged,
    aes(x = factor(pr, levels = c("pr0", "pr1", "pr2", "pr3")),
        group = technology, color = technology, fill = technology)) +
    geom_ribbon(aes(ymin = p25, ymax = p75), alpha = 0.2, color = NA) +
    geom_line(aes(y = p50), size = 0.5) +
    facet_wrap(~ contrib, nrow = 1, scales = "fixed",
               labeller = as_labeller(c(
                 lf_contributions = "Annual timescale",
                 mf_contributions = "Weekly timescale",
                 hf_contributions = "Daily timescale"
               ))) +
    scale_color_discrete(labels = c(gas = "Gas", battery = "Battery", electrolysis = "Electrolysis",
                                    curtailment = "Curtailment")) +
    scale_fill_discrete(labels = c(gas = "Gas", battery = "Battery", electrolysis = "Electrolysis",
    curtailment = "Curtailment")) +
    scale_x_discrete(labels = c(pr0 = "0",pr1 = "1",pr2 = "2",pr3 = "3"),
                     expand = expansion(add = 0)) +
    labs(x = expression("Premium level ("*EUR/kg["H2"]*")"), 
         y = "Flexibility contribution (%)\n", 
         title = "",
         color = "Flexibility solution: ",
         fill = "Flexibility solution: ") +
    scale_y_continuous(expand = expansion(add = 0), limits = c(-6, 42)) +
    theme_minimal() +
    theme(legend.position = "bottom",
          panel.spacing = unit(1.5, "lines"),
          panel.border = element_rect(color = "black", size  = 0.2, fill  = NA),
          axis.text.x = element_text(color = "black"),
          axis.text.y = element_text(color = "black"),
          axis.title.x = element_text(color = "black", margin = margin(t = 15)),
          axis.title.y = element_text(color = "black"),
          panel.grid.major = element_line(color = "grey85", size = 0.3),
          panel.grid.minor = element_line(color = "grey85", size = 0.3))
  
  

}

#' Create FSCD Change Heatmap by Country and Premium Level
#'
#' Generates a heatmap showing absolute changes in average FSCD of a solution.
#' across countries, technologies, timescales, and premium level transitions.
#'
#' @param country_stats_nested List. Country-level statistics by premium (from \code{compute_fscd_data()}).
#'   Expected to contain average contribution differences across premium transitions.
#'
#' @return ggplot object. Heatmap with countries on x-axis, technologies on y-axis (right side),
#'   faceted by premium transition and timescale. Color scale: red (negative) to green (positive).
#'
plot_fscd_change_heatmap <- function(country_stats_nested){
    country_stats_diff <- get_fscd_country_stats_diff(country_stats_nested)
    res_load_lf_mod_order <- c("DE", "FR", "ES", "UK", "NL", "IT", "SE", "PL", "DK", "CH", "AT", "BE", "PT", "CZ", "IE", "FI")

    heatmap_avg_contrib_diff <- ggplot(country_stats_diff[["avg"]] %>%
            dplyr::filter(technology %in% c("battery", "demand_resp", "electrolysis", "gas", "interco", "other_thermal", "psh")) %>%
            dplyr::mutate(
            technology = factor(technology,
                                levels = c("battery", "other_thermal",  "demand_resp", "psh", "electrolysis",
                                            "gas",  "interco")),
            bz = factor(bz, levels = res_load_lf_mod_order),
            pr = factor(pr, levels = c("pr1-pr0", "pr2-pr1", "pr3-pr2")),
            contrib = factor(contrib, levels = c("lf_contributions", "mf_contributions", "hf_contributions"))
            ),
        aes(x = bz, y = technology, fill = value)) +
    geom_tile(color = NA) +
    facet_grid(rows = vars(pr),
                cols=vars(contrib),
                labeller = labeller(
                contrib = c(lf_contributions = "Annual timescale", mf_contributions = "Weekly timescale", hf_contributions = "Daily timescale"),
                pr = c(`pr1-pr0` = "0 → 1 EUR/kg", `pr2-pr1` = "1 → 2 EUR/kg", `pr3-pr2` = "2 → 3 EUR/kg")
                ),
                switch="y") +
    scale_y_discrete(
        position="right",
        labels = c(
        battery = "Battery",
        demand_resp = "Demand Response",
        electrolysis = "Electrolysis",
        gas = "Gas",
        interco = "Interconnector",
        other_thermal = "Other thermal",
        psh = "Pumped storage hydro"
    )) +
    scale_fill_gradient2(
        name = "Absolute change in FSCD average (%)",
        low = "red",
        mid = "white",
        high = "forestgreen",
        midpoint = 0)+
    labs(
        y = "Flexibility solution",
        x = ""
    )+
    theme_minimal() + 
    theme(
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1, color="black"),
        axis.text.y = element_text(angle = 0, color="black"),
        axis.title.y.right = element_text(
        color = "black", face="bold",
        margin = margin(l = 25)
        ),
        legend.position = "bottom",
        text = element_text(color = "black"),
        strip.text = element_text(face = "bold"),
        strip.placement = "outside",
        strip.text.y = element_text(angle = 0),
        strip.text.y.left = element_text(angle = 0, hjust = 0),
        axis.title.x = element_text(color = "black"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background  = element_rect(fill = "white", color = NA)
    )
    return(heatmap_avg_contrib_diff)
}