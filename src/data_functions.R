# Copyright (c) 2026, RTE (https://www.rte-france.com)
# 
# Authors: 
#      Arnauld  GUILLOTIN (author)
#      Thomas HEGGARTY (author)
# 
# SPDX-License-Identifier: MIT
# 
# This file is part of the project electrolysis_prema_distortion, containing code that supports an academic
# paper and enables reproduction of paper figures.

####################################################################################################

# Technology efficiency for gas-based power generation, from model parameters
get_gas_eff <- function(parent_folder){
    gas_eff <- file.path(parent_folder,"data", "input", "gas_efficiencies.csv") %>%
        read.csv2()
    return(gas_eff)
}

create_study_summary <- function(study_folder, study_name, output_name, year=NULL){

  # Getting input data 
  {
    study_path <- file.path(study_folder, study_name)
    print(study_path)
    gas_eff <- read.csv2(file.path(path_parent, "data", "input", "gas_efficiencies.csv"))
    setSimulationPath(path=study_path, simulation=output_name)
  }
  
  # Preprocessing and overarching variables' definition

  hydro_open_areas <- getAreas(select="^2_[a-z0-9]+")
  hydro_open_links <- paste0(hydro_open_areas, " - ", substr(hydro_open_areas, 3,6))
  hydro_res_areas <- getAreas(select="^3_[a-z0-9]+")
  hydro_swell_areas <- getAreas(select="^4_[a-z0-9]+")
  h2_storage_areas <- getAreas(select="^xh2_[a-z0-9]+")
  h2_storage_links <- paste0(substr(h2_storage_areas,13,14), "00h2 - ", h2_storage_areas)
  virtual_areas <- c(
    "z_h2", "_idsr", "0_battery_pump", 
    "0_pump_open", "0_turb_open", 
    "1_pump_closed", "1_turb_closed",
    hydro_open_areas, hydro_res_areas, hydro_swell_areas, h2_storage_areas
  )
  
  power_areas <- getAreas(select="^[a-z0-9]{4}$|^[a-z0-9]{4}_dres_(loc|sys)$")
  h2_areas <- getAreas(select="00h2|h2_loc$") %>% Filter(function(x) !(x %in% c("ibfi00h2", "ibit00h2")), .)
  
  # Filter useful cols to keep to reduce data size
  power_gen_cols <- c("area", "LOAD", "SPIL. ENRG", "z_h2", "BALANCE", "ROW BAL.", "UNSP. ENRG",  
                      "H. ROR", "WIND OFFSHORE", "WIND ONSHORE", "SOLAR CONCRT.", "SOLAR PV",
                      "NUCLEAR", "GAS", "COAL", "LIGNITE", "OIL", "MISC. DTG", "MISC. NDG", "MRG. PRICE", 
                      "CO2 EMIS.", "0_battery_pump", "1_pump_closed", "1_turb_closed")
  
  h2_prod_cols <- c("area", "LOAD", "SPIL. ENRG", "z_h2", "BALANCE", "ROW BAL.", "UNSP. ENRG",
                    "MISC. DTG", "Other1_injection", "Other1_withdrawal", "MRG. PRICE"
                    )
  
  clusters <- readClusterDesc() %>% mutate(area=as.character(area)) %>%
    mutate(cluster=as.character(cluster)) %>% as.data.table()
  
  h2_import_clusters <- clusters %>% dplyr::filter(cluster  %like% "^h2_import_[a-z]+-[a-z]+") %>%
    dplyr::filter(area %in% h2_areas) %>%  select("cluster") %>% unlist(., use.names = FALSE)
  
  smr_clusters <- clusters %>% dplyr::filter(cluster  %like% "^[a-z]+00h2_smr") %>%
    dplyr::filter(area %in% h2_areas) %>%  select("cluster") %>% unlist(., use.names = FALSE)
  
  gas_clusters <- clusters %>% dplyr::filter(cluster  %like% "^gas_ccgt|gas_ocgt|gas_conventional[a-z 0-9]+") %>%
    dplyr::filter(area %in% power_areas) %>%  select("cluster") %>% unlist(., use.names = FALSE)
  
  h2_clusters <- clusters %>% dplyr::filter(cluster  %like% "^hydrogen_ccgt[a-z 0-9]+") %>%
    dplyr::filter(area %in% power_areas) %>%  select("cluster") %>% unlist(., use.names = FALSE)
  
  dsr_clusters <- clusters %>% dplyr::filter(cluster  %like% "_dsr_") %>%
    dplyr::filter(area %in% power_areas) %>%  select("cluster") %>% unlist(., use.names = FALSE)
    
  battery_clusters <- clusters %>% dplyr::filter(cluster  %like% "_battery_turb$") %>%
    dplyr::filter(area %in% power_areas) %>%  select("cluster") %>% unlist(., use.names = FALSE)
  
  othernonres_clusters <- clusters %>% dplyr::filter(cluster  %like% "othernonres") %>%
    dplyr::filter(area %in% power_areas) %>%  select("cluster") %>% unlist(., use.names = FALSE)
  
  #########################################
  
  raw_year_data <- readAntares(areas="all", links="all", mcYears=year, timeStep = "annual")
  year_data <- removeVirtualAreas(x = raw_year_data, storageFlexibility = virtual_areas, newCols = TRUE)
  
  #########################################
  
  year_area_power_data <- year_data[["areas"]] %>%
    dplyr::filter(area %in% power_areas)  %>%
    select(all_of(c(power_gen_cols))) %>%
    mutate(z_h2 = -1*z_h2) %>%
    .[, lapply(.SD, \(x) if (is.numeric(x)) x * 1e-6 else x)]
  
  {
    gas_gen_data <- readAntaresClusters(clusters = gas_clusters, selected = c("production"), timeStep = "annual") %>%
    select(c("area", "cluster", "production")) %>% 
    mutate(production = production / 1e6) %>%
    merge(., gas_eff, by.x="cluster", by.y="antares_name", all.x=TRUE) %>%
    mutate(tech = if_else(
      str_detect(tolower(cluster), "ccgt"), "CCGT", if_else(
        str_detect(tolower(cluster), "ocgt"), "OCGT", "Conventional"))) %>%
    mutate(methane_cons_TWh = production/efficiency) %>%
    group_by(area, tech) %>%
    summarise(across(c("production","methane_cons_TWh"), sum)) %>%
    ungroup() %>%
    pivot_longer(cols = c(production, methane_cons_TWh),
                 names_to = "metric",
                 values_to = "value") %>%
    pivot_wider(names_from = c(tech, metric),
                values_from = value,
                names_sep = "_",
                values_fill = 0)
    year_area_power_data  <- merge(year_area_power_data, gas_gen_data, by.x="area", by.y="area", all.x = TRUE)
  }
  {
    dsr_data <- readAntaresClusters(clusters = dsr_clusters, selected = c("production"), timeStep = "annual") %>%
      select(c("area", "production")) %>% 
      mutate(production = production / 1e6) %>%
      group_by(area) %>% summarise(across(c("production"), sum)) %>% ungroup()
      setnames(dsr_data, old = "production", new = "dsr")
    
    year_area_power_data  <- merge(year_area_power_data, dsr_data, by.x="area", by.y="area", all.x = TRUE)
  }
  {
    h2_gen_data <- readAntaresClusters(clusters = h2_clusters, selected = c("production"), timeStep = "annual") %>%
      select(c("area", "production")) %>% 
      mutate(production = production / 1e6) %>%
      group_by(area) %>% summarise(across(c("production"), sum)) %>% ungroup()
      setnames(h2_gen_data, old = "production", new = "h2_cluster")
    
    year_area_power_data  <- merge(year_area_power_data, h2_gen_data, by.x="area", by.y="area", all.x = TRUE)
  }
  {
    battery_turb_data <- readAntaresClusters(clusters = battery_clusters, selected = c("production"), timeStep = "annual") %>%
      select(c("area", "production")) %>% 
      mutate(production = production / 1e6) %>%
      group_by(area) %>% summarise(across(c("production"), sum)) %>% ungroup()
    setnames(battery_turb_data, old = "production", new = "battery_turb")
    
    year_area_power_data  <- merge(year_area_power_data, battery_turb_data, by.x="area", by.y="area", all.x = TRUE)
  }
  {
    othernonres_data <- readAntaresClusters(clusters = othernonres_clusters, selected = c("production"), timeStep = "annual") %>%
      select(c("area", "production")) %>% 
      mutate(production = production / 1e6) %>%
      group_by(area) %>% summarise(across(c("production"), sum)) %>% ungroup()
    setnames(othernonres_data, old = "production", new = "othernonres")
    
    year_area_power_data  <- merge(year_area_power_data, othernonres_data, by.x="area", by.y="area", all.x = TRUE)
  }
  
  {
    hydro_res_data <- year_data[["areas"]] %>% 
      select(all_of(c(hydro_res_areas,hydro_swell_areas))) %>%
      group_by() %>%
      summarise(across(c(hydro_res_areas,hydro_swell_areas), sum)) %>%
      t() %>% as.data.frame() %>%
      tibble::rownames_to_column("rowname") %>%
      mutate(area = sub("^[34]_([A-Za-z0-9]{4}).*", "\\1", rowname),
             V1  = V1 *1e-6) %>%
      group_by(area) %>% summarise(across(c("V1"), sum)) %>%
      rename(hydro_res = V1)
      
    year_area_power_data  <- merge(year_area_power_data, hydro_res_data, by.x="area", by.y="area", all.x = TRUE)
  }
  
  {
    hydro_open_data <- readAntares(links=hydro_open_links, mcYears=year) %>%
      mutate(
        `2_open_turb` = if_else(`FLOW LIN.` > 0, `FLOW LIN.` / 1e6, 0),
        `2_open_pump` = if_else(`FLOW LIN.` < 0, `FLOW LIN.` / 1e6, 0)
      ) %>% group_by(link) %>%  summarise(across(c("2_open_turb","2_open_pump"), sum)) %>%
      mutate(area = substr(link,21, 24)) %>% select(c("area", "2_open_turb", "2_open_pump"))
    
    year_area_power_data  <- merge(year_area_power_data, hydro_open_data, by.x="area", by.y="area", all.x = TRUE)
  }
   
  year_area_power_data[is.na(year_area_power_data)] <- 0
  year_agg_power_data <- year_area_power_data %>%
    select(-c("area", "MRG. PRICE")) %>%
    .[, lapply(.SD, sum)]
  
  #########################################
  
  year_area_h2_data <- year_data[["areas"]] %>%
    dplyr::filter(area %in% h2_areas) %>%
    select(all_of(h2_prod_cols)) %>%
    .[, lapply(.SD, \(x) if (is.numeric(x)) x * 1e-6 else x)]
  
  {
    h2_import_data <- readAntaresClusters(clusters = h2_import_clusters, selected = c("production"), timeStep = "annual") %>%
      select(c("area", "production")) %>% 
      mutate(production = production / 1e6) %>%
      group_by(area) %>% summarise(across(c("production"), sum)) %>% ungroup()
    setnames(h2_import_data, old = "production", new = "h2_imports")
    
    year_area_h2_data  <- merge(year_area_h2_data, h2_import_data, by.x="area", by.y="area", all.x = TRUE)
  }
  {
    smr_prod_data <- readAntaresClusters(clusters = smr_clusters, selected = c("production"), timeStep = "annual") %>%
      select(c("area", "cluster", "production")) %>% 
      mutate(tech=str_extract(cluster, "(?<=smr_).+$")) %>%
      mutate(smr_em_factor=ifelse(tech == "ccs", 0.02231, 0.203874)) %>%
      mutate(emissions_Mt = smr_em_factor*production / 1e6) %>%
      mutate(production = production / 1e6) %>%
      group_by(area, tech) %>% summarise(across(c("production", "emissions_Mt"), sum)) %>% ungroup() %>%
      pivot_longer(cols = c(production, emissions_Mt),
                   names_to = "metric",
                   values_to = "value") %>%
        pivot_wider(names_from = c(tech, metric),
                    values_from = value,
                    names_sep = "_",
                    values_fill = 0) %>%
      mutate(smr_wo_ccs_methane_TWh = wo_ccs_production / SMR_EFFICIENCY) %>%
      mutate(smr_ccs_methane_TWh = ccs_production / SMR_CCS_EFFICIENCY)
    
    year_area_h2_data  <- merge(year_area_h2_data, smr_prod_data, by.x="area", by.y="area", all.x = TRUE)
  }
  
  {
    h2_storage_data <- readAntares(links=h2_storage_links, mcYears = year) %>%
      mutate(
        `h2_stor_pump` = if_else(`FLOW LIN.` > 0, `FLOW LIN.` / 1e6, 0),
        `h2_stor_turb` = if_else(`FLOW LIN.` < 0, `FLOW LIN.` / 1e6, 0)
      ) %>% group_by(link) %>%  summarise(across(c("h2_stor_pump","h2_stor_turb"), sum)) %>%
      mutate(area = substr(link,1, 4) %>% paste0(., "h2")) %>% select(c("area", "h2_stor_pump", "h2_stor_turb"))
    
    year_area_h2_data  <- merge(year_area_h2_data, h2_storage_data, by.x="area", by.y="area", all.x = TRUE)
  }
  
  year_area_h2_data[is.na(year_area_h2_data)] <- 0
  year_agg_h2_data <- year_area_h2_data %>%
    select(-c("area", "MRG. PRICE")) %>%
    .[, lapply(.SD, sum)]
  
  output <- list(year_agg_power_data, year_area_power_data, year_agg_h2_data, year_area_h2_data) %>% 
    setNames(c("year_agg_power_data", "year_area_power_data", "year_agg_h2_data", "year_area_h2_data"))
  
  output
}

append_to_excel <- function(dt, file, sheet, study_name) {
  dt <- cbind(study_name, dt)
  # Load workbook (works with file and sheets guaranteed to exist)
  wb <- loadWorkbook(file)
  
  # Read existing data from the sheet — handle empty sheets safely
  existing_data <- tryCatch(
    read.xlsx(wb, sheet = sheet),
    error = function(e) NULL
  )
  
  if (is.null(existing_data) || nrow(existing_data) == 0) {
    # Sheet is empty or no data — just write the new data
    removeWorksheet(wb, sheet)
    addWorksheet(wb, sheet)

    writeData(wb, sheet, dt, colNames = TRUE)
    message(paste0("Sheet ", sheet," empty - new data written."))
  } else {
    # Sheet has existing data — append new data
    existing_dt <- as.data.table(existing_data)
    combined_dt <- rbind(existing_dt, dt, use.names = FALSE, fill = TRUE)
    
    # Overwrite the sheet
    removeWorksheet(wb, sheet)
    addWorksheet(wb, sheet)
    writeData(wb, sheet, combined_dt, colNames = TRUE)
    message("New data appended to existing ", sheet, " data.")
  }
  
  saveWorkbook(wb, file, overwrite = TRUE)
  invisible(TRUE)
}

create_dataset <- function(mcy, opts_list){
  data_list <- list(
    "pr0" = read_and_format_data(opts_list[["pr0"]], mcy),
    "pr1" = read_and_format_data(opts_list[["pr1"]], mcy),
    "pr2" = read_and_format_data(opts_list[["pr2"]], mcy),
    "pr3" = read_and_format_data(opts_list[["pr3"]], mcy) 
  )
  
  data_list
}

read_and_format_data <- function(opts, mcy){
  
  if(is.null(mcy)){mcy_label <- 0}else{mcy_label <- mcy}
  
  # Define virtual nodes
  hydro_open_areas <- getAreas(select="^2_[a-z0-9]+")
  hydro_res_areas <- getAreas(select="^3_[a-z0-9]+")
  hydro_swell_areas <- getAreas(select="^4_[a-z0-9]+")
  h2_storage_areas <- getAreas(select="^xh2_[a-z0-9]+")
  virtual_areas <- c(
    "z_h2", "_idsr", "0_battery_pump", "0_pump_open", "0_turb_open", "1_pump_closed", 
    "1_turb_closed", hydro_open_areas, hydro_res_areas, hydro_swell_areas, h2_storage_areas
  )
  
  # Import data for 1 mcYear
  all_data <- readAntares(opts=opts, areas="all", links="all", mcYears=mcy, clusters="all") %>%
    removeVirtualAreas(x = ., storageFlexibility = virtual_areas, newCols = TRUE)
  
  # Process cluster and flow data for both H2 and power
  cluster_data <- all_data[["clusters"]] %>%
    dplyr::filter(area %in% c(POWER_AREAS, H2_AREAS)) %>%
    mutate(cluster_type = case_when(
      grepl("h2_import", cluster, ignore.case = TRUE) ~ "import",
      grepl("smr_ccs", cluster, ignore.case = TRUE) ~ "smr_ccs",
      grepl("smr_wo_ccs", cluster, ignore.case = TRUE) ~ "smr",
      grepl("dsr", cluster, ignore.case = TRUE) ~ "DSR",
      grepl("battery_turb", cluster, ignore.case = TRUE) ~ "battery_turb",
      grepl("gas_ccgt", cluster, ignore.case = TRUE) ~ "ccgt",
      grepl("gas_conventional", cluster, ignore.case = TRUE) ~ "conventional",
      grepl("hydrogen", cluster, ignore.case = TRUE) ~ "h2_ccgt",
      grepl("othernonres", cluster, ignore.case = TRUE) ~ "othernonres",
      grepl("gas_ocgt", cluster, ignore.case = TRUE) ~ "ocgt",
      grepl("nuclear", cluster, ignore.case = TRUE)  ~ "nuclear",
      grepl("hard coal", cluster, ignore.case = TRUE)  ~ "hard_coal",
      grepl("lignite", cluster, ignore.case = TRUE)  ~ "lignite",
      grepl("oil", cluster, ignore.case = TRUE)  ~ "oil",
      TRUE ~ "zz_other_cluster" # check that this is not present in output
    )) %>%
    select(c("area", "timeId", "cluster_type", "production")) %>%
    group_by(area, cluster_type, timeId) %>%
    summarise(across(c("production"), sum))%>%
    pivot_wider(
      names_from = cluster_type,
      values_from = production,
      values_fill = list(production = 0)
    )

  flows_data <- readAntares(
    opts = opts,
    links = getLinks(areas = c(POWER_AREAS, H2_AREAS), internalOnly = TRUE), 
    mcYears = mcy
  ) %>%
    select(c("link", "timeId", "FLOW LIN.")) %>%
    pivot_wider(names_from = link, values_from = `FLOW LIN.`)

  area_data <- all_data[["areas"]] %>%
    select(-matches("_std|_min|_max")) %>%
    mutate(h2_stor = rowSums(select(., all_of(h2_storage_areas)), na.rm = TRUE)) %>%
    mutate(pump_open = rowSums(select(., all_of(hydro_open_areas)), na.rm = TRUE)) %>%
    mutate(hydro_res = rowSums(select(., all_of(hydro_res_areas)), na.rm = TRUE)) %>%
    mutate(hydro_swell = rowSums(select(., all_of(hydro_swell_areas)), na.rm = TRUE)) %>%
    select(-all_of(c(h2_storage_areas, hydro_open_areas, hydro_res_areas, hydro_swell_areas))) %>%
    merge(x=., y=cluster_data, by=c("area", "timeId"), all.x=TRUE) %>%
    mutate(across(where(is.numeric), ~replace_na(.x, 0))) %>%
    mutate(mcYear = mcy_label)

  return(list("areas"= area_data, "clusters"=cluster_data, "flows"=flows_data))
}

get_data_diff <- function(data_antares_hourly_list){
    res <- table_for_delta_subplots(data_antares_hourly_list) %>%
        select("short_area", "energy_system", "variable", "pr1-pr0", "pr2-pr1", "pr3-pr2") %>%
        dplyr::filter((energy_system == "elec" & variable %in% c("CO2 EMIS.", "carbon_intensive_gen", "decarbonised_gen")) |
                    (energy_system == "h2" & variable %in% c("import", "z_h2", "CO2 EMIS.", "BALANCE", "SMR")),
                    !(short_area %in% c("no", "ib"))) %>%
        pivot_longer(cols = c("pr1-pr0", "pr2-pr1", "pr3-pr2"), names_to = "premium_diff", values_to=paste0("value")) %>%
        mutate(premium_diff = factor(premium_diff, levels = c("pr1-pr0", "pr2-pr1", "pr3-pr2")),
            short_area = toupper(short_area),
            variable = case_when(
                variable == "z_h2" ~ "Electrolysis",
                variable == "import" ~ "Hydrogen imported from third countries",
                variable == "prod" & energy_system == "elec" ~ "Additional power generation",
                variable == "carbon_intensive_gen" & energy_system == "elec" ~ "Fossil power generation",
                variable == "decarbonised_gen" & energy_system == "elec" ~ "Decarbonised power generation",
                (variable == "CO2 EMIS." & energy_system == "elec")  ~ "Electricity emissions",
                (variable == "CO2 EMIS." & energy_system == "h2")  ~ "Hydrogen emissions",
                (variable == "BALANCE" & energy_system == "h2")  ~ "Hydrogen trade balance"),
            panel = case_when(
                variable %in% c("Hydrogen imported from third countries", "Electrolysis", "Hydrogen trade balance", "SMR") ~
                "Hydrogen supply",
                variable %in% c("Fossil power generation", "Decarbonised power generation")  ~
                "Power generation",
                variable %in% c("Electricity emissions", "Hydrogen emissions") ~
                "Emissions")) %>%
        mutate(value = if_else(variable == "Hydrogen trade balance", -value, value))

    return(res)
}

import_electrolysis_cons <- function(study_data_list, mcy){
  electrolysis_data <- list()
  for (i in 1:length(study_data_list)){
    opts <- study_data_list[[i]]
    electrolysis_data[[as.character(i-1)]] <- readAntares(opts=opts, links=getLinks(areas = "z_h2"), mcYears = mcy) %>%
      select("link", "time", "UCAP LIN.")
  }
  electrolysis_data <- rbindlist(electrolysis_data, use.names = TRUE, idcol = "sim") 
  electrolysis_data
}

table_for_delta_subplots <- function(data_antares_hourly_list){
  temp_list <- list()
  
  for (i in 0:3){
    data_name <- paste0("pr", i)
    data_antares_hourly <- data_antares_hourly_list[[data_name]][["areas"]]
    id_cols <- c("short_area", "energy_system")
    cols_to_select <- c("LOAD", "BALANCE", "WIND ONSHORE", "WIND OFFSHORE", "SOLAR PV", "SOLAR CONCRT.", "H. ROR", "MISC. NDG",
                        "NUCLEAR", "LIGNITE", "COAL", "GAS", "OIL",  "MISC. DTG", "UNSP. ENRG", "SPIL. ENRG",
                        "_idsr", "z_h2", "0_battery_pump", "DSR", "battery_turb", "ccgt", "conventional", "hydro_res", "hydro_swell",
                        "h2_ccgt", "othernonres", "ocgt", "import", "smr", "smr_ccs", "CO2 EMIS.", "OP. COST", "actual_gen",
                        "prod", "carbon_intensive_gen", "decarbonised_gen", "prod_2", "gas", "misc", "prod_diff", "prod_detailed_diff", "SMR"
    )
    
    temp_list[[data_name]] <- data_antares_hourly %>%
      dplyr::filter(area %in% (c(POWER_AREAS, H2_AREAS) %>% setdiff(., c("nom1", "nos1", "nos0")))) %>%
      mutate(area = as.character(area),
             short_area = substr(as.character(area), 1, 2),
             energy_system = case_when(area %in% POWER_AREAS ~ "elec",
                                       area %in% H2_AREAS ~ "h2",
                                       TRUE ~ "other"),
             prod = `WIND ONSHORE` + `WIND OFFSHORE` + `SOLAR PV` + `SOLAR CONCRT.` + hydro_res + hydro_swell +
               `H. ROR` + `MISC. NDG` + NUCLEAR + LIGNITE + COAL + GAS + OIL + `MISC. DTG`,
             decarbonised_gen = `WIND ONSHORE` + `WIND OFFSHORE` + `SOLAR PV` + 
               `SOLAR CONCRT.` + hydro_res + hydro_swell + `H. ROR` +
               `MISC. NDG` + NUCLEAR + h2_ccgt,
             carbon_intensive_gen = LIGNITE + COAL + conventional + ccgt + ocgt + OIL + othernonres,
             prod_2 = decarbonised_gen + carbon_intensive_gen,
             gas = GAS - conventional - ocgt - ccgt - h2_ccgt,
             misc = `MISC. DTG` - DSR - `battery_turb` - othernonres,
             prod_diff = prod - prod_2,
             prod_detailed_diff = prod_2 - DSR,
             actual_gen = `WIND ONSHORE` + `WIND OFFSHORE` + `SOLAR PV` + `SOLAR CONCRT.` + hydro_res + hydro_swell +
               `H. ROR` + `MISC. NDG` + NUCLEAR + LIGNITE + COAL + OIL + GAS + othernonres,
             SMR = smr+smr_ccs) %>%
      select(all_of(c(id_cols, cols_to_select))) %>%
      group_by(short_area, energy_system) %>%
      summarise(across(all_of(c(cols_to_select)), sum)) %>%
      pivot_longer(cols = -all_of(id_cols), names_to = "variable", values_to=paste0("value_",data_name))
  }
  
  collate_table <- temp_list[["pr0"]] %>%
    merge(x= ., y= temp_list[["pr1"]],
          by.x = c(id_cols, "variable"), by.y=c(id_cols, "variable"),
          all.x = TRUE, all.y = TRUE) %>%
    merge(x= ., y= temp_list[["pr2"]],
          by.x = c(id_cols, "variable"), by.y=c(id_cols, "variable"),
          all.x = TRUE, all.y = TRUE) %>%
    merge(x= ., y= temp_list[["pr3"]],
          by.x = c(id_cols, "variable"), by.y=c(id_cols, "variable"),
          all.x = TRUE, all.y = TRUE) %>%
    mutate(`pr1-pr0` = (value_pr1 - value_pr0)/1e6,
           `pr2-pr1` = (value_pr2 - value_pr1)/1e6,
           `pr3-pr2` = (value_pr3 - value_pr2)/1e6)
  return(collate_table)
}

get_col_annual_summary_per_area_mcy_sim <- function(opts_list, col_name){
  result <- list()
  for (i in 1:length(opts_list)){
    opts <- opts_list[[i]]
    data <- readAntares(opts=opts, areas = c(POWER_AREAS, H2_AREAS), 
                        mcYears = "all", select = col_name, timeStep = "annual") %>%
      select(-all_of(c("timeId", "time"))) 
    result[[paste0(i-1, " EUR/kg")]] <- data
  }
  return(result)
}

get_opcosts <- function(opts_list, outputs_folder){
    opcosts <- get_col_annual_summary_per_area_mcy_sim(opts_list=opts_list, "OP. COST") %>%
        rbindlist(idcol="premium") %>% mutate(`OP. COST` = `OP. COST`/1e9) %>% #GEUR/a 
        dplyr::filter(!area %in% c("beof", "dekf", "dkkf", "dkbh", "dkns", "nlll", "itvi"))
        # Ignore virtual and offshore areas    
    
    # Group_by to aggregate over all areas
    opcosts_elec <- opcosts %>% dplyr::filter(area %in% POWER_AREAS)
    opcosts_elec_agg <- opcosts_elec %>% 
        group_by(premium, mcYear) %>% 
        summarise(across(all_of(c("OP. COST")), sum)) %>% 
        ungroup() 
    opcosts_h2 <- opcosts %>% dplyr::filter(area %in% H2_AREAS)
    opcosts_h2_agg <- opcosts_h2 %>%
        group_by(premium, mcYear) %>% 
        summarise(across(all_of(c("OP. COST")), sum)) %>% 
        ungroup()
    
    opcosts_agg <- opcosts_h2_agg %>%
        merge(x = ., y=opcosts_elec_agg, by.x=c("premium", "mcYear"), by.y=c("premium", "mcYear"), 
            all.x=TRUE, all.y=TRUE, suffixes = c("_h2", "_elec")) %>%
        mutate(opcost_total = `OP. COST_h2` + `OP. COST_elec`)
    
    opcosts_stats <- opcosts_agg %>%
        group_by(premium) %>%
        summarise(
        mean_h2   = mean(`OP. COST_h2`),
        mean_elec   = mean(`OP. COST_elec`),
        mean_total   = mean(opcost_total))
    
    write_xlsx(opcosts_stats, path = file.path(outputs_folder,"opcosts_stats.xlsx"))

  return(opcosts_agg)
}

get_emissions <- function(opts_list, outputs_folder){
    emission <- get_col_annual_summary_per_area_mcy_sim(opts_list, "CO2 EMIS.") %>%
        rbindlist(idcol="premium") %>% mutate(`CO2 EMIS.` = `CO2 EMIS.`/1e6) %>% # Mt/a 
        dplyr::filter(!area %in% c("beof", "dekf", "dkkf", "dkbh", "dkns", "nlll", "itvi"))
        # Ignore virtual and offshore areas    
  
  emission_elec <- emission %>% dplyr::filter(area %in% POWER_AREAS)
  emission_elec_agg <- emission_elec %>% 
    group_by(premium, mcYear) %>% 
    summarise(across(all_of(c("CO2 EMIS.")), sum)) %>% 
    ungroup() 

  emission_h2 <- emission %>% dplyr::filter(area %in% H2_AREAS)
  emission_h2_agg <- emission_h2 %>%
    group_by(premium, mcYear) %>% 
    summarise(across(all_of(c("CO2 EMIS.")), sum)) %>% 
    ungroup()
  
  emission_agg <- emission_h2_agg %>%
    merge(x = ., y=emission_elec_agg, by.x=c("premium", "mcYear"), by.y=c("premium", "mcYear"), 
          all.x=TRUE, all.y=TRUE, suffixes = c("_h2", "_elec")) %>%
    mutate(CO2_EMIS_total = `CO2 EMIS._h2` + `CO2 EMIS._elec`)
  
  emission_stats <- emission_agg %>%
    group_by(premium) %>%
    summarise(
      mean_h2   = mean(`CO2 EMIS._h2`),
      mean_elec   = mean(`CO2 EMIS._elec`),
      mean_total   = mean(CO2_EMIS_total))
  
  write_xlsx(emission_stats, path = file.path(outputs_folder,"emission_stats.xlsx"))

  return(emission_agg)
}

get_gas_use <- function(opts_list, outputs_folder, parent_folder){
    gas_eff <- get_gas_eff(parent_folder)
    result <- list()

    for (i in 1:length(opts_list)){
        opts <- opts_list[[i]] 
        data <- readAntares(opts=opts, clusters = c(POWER_AREAS, H2_AREAS), mcYears = "all", 
                            select = "production", timeStep = "annual") %>%
        mutate(cluster_type = case_when(
            grepl("gas_ccgt", cluster, ignore.case = TRUE) ~ "gas_ccgt",
            grepl("gas_conventional", cluster, ignore.case = TRUE) ~ "gas_conventional",
            grepl("gas_ocgt", cluster, ignore.case = TRUE) ~ "gas_ocgt",
            grepl("smr_ccs", cluster, ignore.case = TRUE) ~ "smr_ccs",
            grepl("smr_wo_ccs", cluster, ignore.case = TRUE) ~ "smr_wo_ccs",
            TRUE ~ "zz_other_cluster")) %>% # technologies that do not use gas, to be filtered out
        dplyr::filter(cluster_type != "zz_other_cluster") %>% 
        merge(x = ., y = gas_eff, by.x = "cluster", by.y = "antares_name", all.x = TRUE) %>%
        mutate(efficiency = case_when(
                cluster_type == "smr_wo_ccs" ~ SMR_EFFICIENCY, # SMR efficiency without CCS, from model parameters
                cluster_type == "smr_ccs" ~ SMR_CCS_EFFICIENCY, # SMR with CCS efficiency, from model parameters
                TRUE ~ efficiency),
                methane_cons_TWh = production / efficiency / 1e6) %>%
        group_by(area, mcYear, cluster_type) %>% 
        summarise(across(all_of(c("methane_cons_TWh")), sum)) %>% 
        ungroup()
        result[[as.character(i-1)]] <- data
    }
    gas_use <- rbindlist(result, use.names = TRUE, idcol="pr") %>%
        mutate(sector = case_when(
            substr(cluster_type, 1, 3) == "smr" ~ "Hydrogen system", 
            substr(cluster_type, 1, 3) == "gas" ~ "Electricity system",
            TRUE ~ "zz_other")) %>%
        group_by(mcYear, sector, pr) %>% 
        summarise(across(all_of(c("methane_cons_TWh")), sum)) %>% 
        ungroup()
        
    gas_use <- gas_use %>% 
        bind_rows(
            gas_use %>%
            group_by(mcYear, pr) %>%
            summarise(methane_cons_TWh = sum(methane_cons_TWh), .groups = "drop") %>%
            mutate(sector = "Energy system")
        )
    
    gas_stats <- gas_use %>%
        group_by(pr, sector) %>%
        summarise(
        mean   = mean(`methane_cons_TWh`)) %>%
        pivot_wider(names_from = "sector", values_from = "mean")
  
    write_xlsx(gas_stats, path = file.path(outputs_folder,"gas_stats.xlsx"))

    return(gas_use)
}

production_cost <- function(tech, carbon_price, premium){
  total_cost <- (VARIABLE_COST[[tech]] + 
                   CARBON_INTENSITY[[tech]]*(carbon_price-147))/
    CONVERSION[[tech]]
  if(CONVERSION[[tech]] != 1){total_cost <- total_cost - premium/H2_ENERGY_DENSITY*1000}
  total_cost
}

price_elz_scatter_data <- function(opts_list, mcYear, elec_areas = POWER_AREAS, input_path){
  result_per_premium <- list()
  electrolysis_capacity <- file.path(input_path, "electrolysis_capacities.csv") %>% 
    read.csv2() %>%
    mutate(area = tolower(area))
  
  for (i in 1:length(opts_list)){
    elec_areas <- as.vector(elec_areas)
    elec_links <- paste0(elec_areas, " - z_h2")
    h2_areas <- substr(elec_areas, 1, 2) %>% paste0("00h2") %>% unique()

    data_h2_areas <- readAntares(opts = opts_list[[i]], areas = h2_areas, mcYears = mcYear, select = "MRG. PRICE") %>% 
      select(all_of(c("area", "timeId", "mcYear", "MRG. PRICE"))) %>%
      mutate(short_area_h2 = substr(area, 1, 2),
             area_h2 = area) %>% select(-area)
    
    data_elec_areas <- readAntares(opts = opts_list[[i]], areas = elec_areas, mcYears = mcYear, select = "MRG. PRICE") %>% 
      select(all_of(c("area", "timeId", "mcYear", "MRG. PRICE"))) %>%
      mutate(short_area = substr(area, 1, 2)) 
    
    data_links_elec <- readAntares(opts = opts_list[[i]], areas = NULL, links = elec_links, mcYears = mcYear, select = "UCAP LIN.") %>% 
      select(all_of(c("link", "timeId", "mcYear", "UCAP LIN."))) %>%
      mutate(link_area = substr(link, 1, 4)) %>%
      select(-link) %>%
      merge(x=., y=data_elec_areas, all.x = TRUE,
            by.x = c("link_area", "timeId", "mcYear"),
            by.y = c("area", "timeId", "mcYear")) %>%
      merge(x=., y=data_h2_areas, all.x = TRUE,
            by.x = c("short_area", "timeId", "mcYear"),
            by.y = c("short_area_h2", "timeId", "mcYear")) %>%
      merge(x=., y=electrolysis_capacity, all.x=TRUE,
            by.x = c("link_area"),
            by.y = c("area")) %>%
      mutate(price_gap = ELECTROLYSIS_EFFICIENCY *(`MRG. PRICE.y` + (i-1)/H2_ENERGY_DENSITY*1000) - `MRG. PRICE.x`,
             load_factor = `UCAP LIN.` /  capacity)
    
    result_per_premium[[paste0(i-1, " EUR/kg")]] <- data_links_elec
  }
  res <- rbindlist(result_per_premium, idcol = "premium")
  return(res)
}


compute_fscd_data <- function(study_names, output_names, load_RDS_sim_data = TRUE, path_output_temp){
      sim_data <- list()
      if(load_RDS_sim_data == TRUE){
          cat("\nLoading data from intermediate RDS")
          sim_data$pr0 <- readRDS(file.path(path_output_temp, "temp_simdata_pr0.rds"))
          sim_data$pr1 <- readRDS(file.path(path_output_temp, "temp_simdata_pr1.rds"))
          sim_data$pr2 <- readRDS(file.path(path_output_temp, "temp_simdata_pr2.rds"))
          sim_data$pr3 <- readRDS(file.path(path_output_temp, "temp_simdata_pr3.rds"))
      }else{
          cat("\nProcessing output data from scratch")
          cat("\npr0")
          sim_data$pr0 <- curate_data_all_mcy_all_areas(study_names[1], output_names[1])
          saveRDS(sim_data$pr0, file.path(path_output_temp, "temp_simdata_pr0.rds"))
          gc()
          cat("\npr1")
          sim_data$pr1 <- curate_data_all_mcy_all_areas(study_names[2], output_names[2])
          saveRDS(sim_data$pr1, file.path(path_output_temp, "temp_simdata_pr1.rds"))
          gc()
          cat("\npr2")
          sim_data$pr2 <- curate_data_all_mcy_all_areas(study_names[3], output_names[3])
          saveRDS(sim_data$pr2, file.path(path_output_temp, "temp_simdata_pr2.rds"))
          gc()
          cat("\npr3")
          sim_data$pr3 <- curate_data_all_mcy_all_areas(study_names[4], output_names[4])
          saveRDS(sim_data$pr3, file.path(path_output_temp, "temp_simdata_pr3.rds"))
          gc()
    }
    # Computing FSCD data from sim data
      
    {
        # EU-aggregated
        cat("\nComputing EU FSCD data")
        fscd_per_pr_eu_agg_allmcy <- list()
        stat_per_pr_eu_agg_allmcy <- list()
        for(pr in c("pr0", "pr1", "pr2", "pr3")){
        sim_data_eu_agg <- sim_data[[pr]] %>% eu_agg_before_fsms()
        fscd_per_pr_eu_agg_allmcy[[pr]] <- concat_fscd_from_several_mcy(sim_data_eu_agg)
        stat_per_pr_eu_agg_allmcy[[pr]] <- list()
        for(contrib in c("lf_contributions", "mf_contributions", "hf_contributions")){
            stat_per_pr_eu_agg_allmcy[[pr]][[contrib]] <- fscd_per_pr_eu_agg_allmcy[[pr]][[contrib]] %>% summarise_contributions()
        }
        }
        
        # Country-aggregated
        fscd_per_pr_country_agg_allmcy <- list()
        stat_per_pr_country_agg_allmcy <- list()
        for(pr in c("pr0", "pr1", "pr2", "pr3")){
        fscd_per_pr_country_agg_allmcy[[pr]] <- list()
        stat_per_pr_country_agg_allmcy[[pr]] <- list()
        sim_data_countries <- sim_data[[pr]] %>% country_agg_before_fsms()
        for(country in COUNTRIES){
            cat(paste0("\nComputing FSCD for premium level ", pr, " and country ", country))
            sim_data_country <- sim_data_countries %>% dplyr::filter(area == country)
            fscd_per_pr_country_agg_allmcy[[pr]][[country]] <- concat_fscd_from_several_mcy(sim_data_country)
            stat_per_pr_country_agg_allmcy[[pr]][[country]] <- list()
            for(contrib in c("lf_contributions", "mf_contributions", "hf_contributions")){
            stat_per_pr_country_agg_allmcy[[pr]][[country]][[contrib]] <- fscd_per_pr_country_agg_allmcy[[pr]][[country]][[contrib]] %>% summarise_contributions()
            }
        }
        }
    }
    return(list(
        "fscd_per_pr_eu_agg_allmcy" = fscd_per_pr_eu_agg_allmcy,
        "stat_per_pr_eu_agg_allmcy" = stat_per_pr_eu_agg_allmcy,
        "stat_per_pr_country_agg_allmcy" = stat_per_pr_country_agg_allmcy
    ))
}


get_fscd_country_stats <-function(stat_per_pr_country_agg_allmcy){
    country_stats <- list()
    for(stat in c("p25", "p50", "p75", "avg", "neg")){
    country_stats_nested <- stat_per_pr_country_agg_allmcy
    country_stats[[stat]] <- agg_table_fscd_stat(country_stats_nested, stat) 
    }
    return(country_stats)
}

get_fscd_eu_stats <-function(stat_per_pr_eu_agg_allmcy){
    eu_stats <- list()
    for(stat in c("p25", "p50", "p75", "avg", "neg")){
    eu_stats_nested <- stat_per_pr_eu_agg_allmcy
    eu_stats[[stat]] <- agg_table_fscd_stat_eu(eu_stats_nested, stat) 
    }
    return(eu_stats)
}

get_fscd_country_stats_diff <-function(stat_per_pr_country_agg_allmcy){
    country_stats_diff <- list()
    nested_stat_per_pr_country_agg_allmcy <- compute_fscd_stat_diff(stat_per_pr_country_agg_allmcy) 
    for(stat in c("p25", "p50", "p75", "avg", "neg")){
    country_stats_diff_nested <- nested_stat_per_pr_country_agg_allmcy
    country_stats_diff[[stat]] <- agg_table_fscd_stat(country_stats_diff_nested, stat) 
    }
    return(country_stats_diff)
}


lp_filter <- function(data_to_filter){
data_to_filter = data_to_filter-mean(data_to_filter)
spectral_data  = fft(data_to_filter, inverse=FALSE)

nb_smpl     = length(spectral_data)
cutoff_freq = 6

spectral_data[(cutoff_freq+1):(nb_smpl-cutoff_freq-1)]  = 0
lf_data = Re(fft(spectral_data, inverse=TRUE))/nb_smpl
return(lf_data)
}

mp_filter <- function(data_to_filter){
data_to_filter = data_to_filter-mean(data_to_filter)
spectral_data  = fft(data_to_filter, inverse=FALSE)

nb_smpl = length(spectral_data)
freq_1  = 6
freq_2  = 180

spectral_data[0:freq_1]                      = 0
spectral_data[(freq_2+1):(nb_smpl-freq_2-1)] = 0
spectral_data[(nb_smpl-freq_1):nb_smpl]      = 0

mf_data = Re(fft(spectral_data, inverse=TRUE))/nb_smpl
return(mf_data)
}

hp_filter <- function(data_to_filter){
data_to_filter = data_to_filter-mean(data_to_filter)
spectral_data  = fft(data_to_filter, inverse=FALSE)

nb_smpl = length(spectral_data)
freq_1  = 6
freq_2  = 180

spectral_data[0:freq_2]                 = 0
spectral_data[(nb_smpl-freq_2):nb_smpl] = 0

hf_data = Re(fft(spectral_data, inverse=TRUE))/nb_smpl
return(hf_data)
}

calculate_fs_contributions_one_mcy <- function(modulation){

total_contribution <- modulation$res_load
ind <- which(abs(total_contribution) >= 0.2*max(abs(total_contribution)))
total_contribution_2 <- total_contribution[ind]

nuclear      <- modulation$NUCLEAR[ind]                                /total_contribution_2*100
hydro        <- modulation$hydro_res[ind]                              /total_contribution_2*100
gas          <- modulation$GAS[ind]                                    /total_contribution_2*100
other_thermal<- (modulation$COAL[ind]+ modulation$LIGNITE[ind] +
                    modulation$othernonres[ind] + modulation$OIL[ind])  /total_contribution_2*100
battery      <- modulation$battery[ind]                                /total_contribution_2*100
psh          <- modulation$psh[ind]                                    /total_contribution_2*100
interco      <- modulation$Balance[ind]                                /total_contribution_2*100
electrolysis <- modulation$z_h2[ind]                                   /total_contribution_2*100
demand_resp  <- modulation$dsr[ind]                                    /total_contribution_2*100
curtailment  <- modulation$Spillage[ind]                               /total_contribution_2*100
loss_of_load <- modulation$`UNSP. ENRG`[ind]                           /total_contribution_2*100

contributions <- data.frame(
    nuclear,hydro,gas,other_thermal,battery,psh,interco,
    electrolysis,demand_resp,curtailment, loss_of_load
)
return(contributions)
}


curate_data_all_mcy_all_areas <- function(study_name, output_name){
{
    zones <- c("at00", "be00", "ch00", "cz00", "de00", "dke1",
                "dkw1", "es00", "fi00", "fr00", "ie00", "itca", "itcn", "itcs", "itn1",
                "its1", "itsa", "itsi", "nl00", "nom1", "non1", "nos0", "pl00",
                "pt00", "se01", "se02", "se03", "se04", "uk00", "ukni"
    )
    cols_to_select <- c("NUCLEAR", "LIGNITE", "COAL", "GAS", "OIL",  
                        "MISC. DTG", "UNSP. ENRG", "z_h2", "hydro_res", 
                        "ccgt", "conventional", "h2_ccgt", "othernonres", 
                        "ocgt", "Spillage", "Balance", "battery", "psh", "dsr", "res_load" 
    )

    expected_cols <- c("LOAD","WIND ONSHORE","WIND OFFSHORE","SOLAR PV","SOLAR CONCRT.","H. ROR",
                        "MISC. NDG","NUCLEAR","LIGNITE","COAL","GAS","OIL","MISC. DTG","UNSP. ENRG",
                        "z_h2","hydro_res","ccgt","conventional","h2_ccgt","othernonres","ocgt",
                        "Spillage","Balance","battery","psh","dsr"
    )

    # Get raw sim data + curation
    setSimulationPath(file.path(path_simulations, study_name), output_name)

    hydro_open_areas <- getAreas(select="^2_[a-z0-9]+") # STEP avec apports
    hydro_res_areas <- getAreas(select="^3_[a-z0-9]+") # Reservoir (sans pompage)
    hydro_swell_areas <- getAreas(select="^4_[a-z0-9]+") # Mini reservoir (sans pompage)
    h2_storage_areas <- getAreas(select="^xh2_[a-z0-9]+")
    
    virtual_areas <- c(
        "z_h2", "_idsr", "0_battery_pump", "1_pump_closed", "1_turb_closed",
        hydro_open_areas, hydro_res_areas, hydro_swell_areas, h2_storage_areas
    )
}
    
# Load and post-process simulation data for all areas 1 mcy
simulation_data <- readAntares(areas="all", links="all", mcYears="all") %>%
    removeVirtualAreas(., storageFlexibility=virtual_areas, newCols=TRUE) %>%
    extract2("areas") %>%
    mutate(
    h2_stor = rowSums(select(., all_of(h2_storage_areas)), na.rm = TRUE),
    hydro_closed_psh = rowSums(select(., all_of(c("1_pump_closed", "1_turb_closed"))), na.rm=TRUE),
    hydro_open_psh = rowSums(select(., all_of(hydro_open_areas)), na.rm = TRUE),
    hydro_res = rowSums(select(., all_of(c(hydro_res_areas, hydro_swell_areas))), na.rm = TRUE)
    ) %>%
    select(-all_of(c("1_pump_closed", "1_turb_closed", h2_storage_areas, hydro_open_areas, hydro_res_areas, hydro_swell_areas))) %>%
    dplyr::filter(area %in% zones) %>%
    mutate(across(where(is.numeric), ~replace_na(.x, 0))) 


# Adding cluster-specific data, preparing for flex contribution calculation
clusters_data <- readAntares(clusters=zones, mcYears = "all") %>%
    # grouping clusters by type (custom-defined) to distinguish techs using 
    # the same fuel or that are not a built-in col in sim_data
    # This excludes fuel oil, coal (not black/brown distinction made), nuclear
    # but includes gas (several plant technologies), othernonres, batteries and dsr (data
    # to be included in the structure, otherwise considered as "other")
    mutate(cluster_type = case_when(
    grepl("dsr", cluster, ignore.case = TRUE) ~ "DSR",
    grepl("battery_turb", cluster, ignore.case = TRUE) ~ "battery_turb",
    grepl("gas_ccgt", cluster, ignore.case = TRUE) ~ "ccgt",
    grepl("gas_conventional", cluster, ignore.case = TRUE) ~ "conventional",
    grepl("hydrogen", cluster, ignore.case = TRUE) ~ "h2_ccgt",
    grepl("othernonres", cluster, ignore.case = TRUE) ~ "othernonres",
    grepl("gas_ocgt", cluster, ignore.case = TRUE) ~ "ocgt",
    grepl("nuclear", cluster, ignore.case = TRUE) ~ "nuclear",
    grepl("hard coal", cluster, ignore.case = TRUE) ~ "hard_coal",
    grepl("lignite", cluster, ignore.case = TRUE) ~ "lignite",
    grepl("oil", cluster, ignore.case = TRUE) ~ "fuel_oil",
    TRUE ~ "zzz" # to check: must be NA everywhere
    )) %>%
    dplyr::filter(!cluster_type %in% c("nuclear", "hard_coal", "lignite", "fuel_oil")) %>% # these types are not needed, already accounted for in sim_data
    group_by(area, cluster_type, mcYear, timeId) %>%
    summarise(across(c("production"), sum)) %>%
    pivot_wider(names_from = cluster_type, values_from = production) %>% #setting cluster_type as column
    mutate(across(all_of(c("DSR", "battery_turb", "ccgt", "conventional", "h2_ccgt", "othernonres", "ocgt")), ~replace_na(., 0)))
    
# Merging sim data and cluster data, by area and time
cur_data <- merge(x=simulation_data, y=clusters_data, 
                    by.x = c("area", "mcYear", "timeId"), by.y=c("area", "mcYear", "timeId"), 
                    all.x=TRUE, all.y=TRUE) %>%
    mutate(
    Spillage = -1*`SPIL. ENRG`, # Changing col sign to account for the effect on flexibility provision, counted upward
    Balance = -1*BALANCE,
    battery = battery_turb +  `0_battery_pump`, # categorising
    psh = hydro_open_psh  + hydro_closed_psh,
    dsr = `_idsr` + DSR,
    res_load = 1*(LOAD - `WIND ONSHORE` - `WIND OFFSHORE` - `SOLAR PV` -`SOLAR CONCRT.` - `H. ROR` - `MISC. NDG` - `ROW BAL.`)) %>%
    select(all_of(c("area", "timeId", "mcYear", cols_to_select))) %>%
    mutate(across(all_of(cols_to_select), ~ .x / 1000)) # units from MWh to GWh

return(cur_data)
}

remove_data_id <- function(data_with_id){
data_without_id <- data_with_id %>% select(-any_of(c("area", "mcYear", "timeId")))
return(data_without_id)
}

eu_agg_before_fsms <- function(disaggregated_sim_data){
agg_result <- disaggregated_sim_data %>%
    group_by(timeId, mcYear) %>%
    summarise(across(-c(area), sum),
            .groups = "drop")
return(agg_result)
}

country_agg_before_fsms <- function(disaggregated_sim_data){
dt <- as.data.table(disaggregated_sim_data)
dt[, short_area := toupper(substr(area, 1, 2))]
dt[, area := NULL]  # remove original area column
agg_result <- dt[, lapply(.SD, sum),
                    by = .(short_area, mcYear, timeId),
                    .SDcols = setdiff(names(dt), c("short_area", "timeId", "mcYear"))]
setnames(agg_result, "short_area", "area")
return(agg_result)
}

compute_fsms <- function(preprocessed_data){
lf = lfmod = mfmod = hfmod = preprocessed_data # all variables affected the format of load_curve
# Compute FSMS
for (i in 1:ncol(preprocessed_data)){
    # Get the unoffset part of the col (avg removed)
    data_to_filter <- preprocessed_data[,i]-mean(preprocessed_data[,i])
    # in lfmod, mfmod and hfmod, applying the Fourier and inverse transforms to the unoffset col, affecting result to the according table 
    lfmod[,i] <- lp_filter(data_to_filter)
    mfmod[,i] <- mp_filter(data_to_filter)
    hfmod[,i] <- hp_filter(data_to_filter)
}
return(list("lf_mod" = lfmod, "mf_mod"=mfmod, "hf_mod"=hfmod))  
}



agg_table_fscd_stat<- function(x, stat_name) {
rbindlist(
    Map(function(pr, pr_list) {
    rbindlist(
        Map(function(bz, bz_list) {
        rbindlist(
            Map(function(contrib, c_list) {
            tbl <- c_list[[stat_name]]
            if (is.null(tbl)) return(NULL)
            tbl <- as.data.table(tbl)
            tbl[, `:=`(pr = pr, bz = bz, contrib = contrib)]
            tbl
            }, names(bz_list), bz_list),
            fill = TRUE
        )
        }, names(pr_list), pr_list),
        fill = TRUE
    )
    }, names(x), x),
    fill = TRUE
)
}

agg_table_fscd_stat_eu <- function(x, stat_name) {
rbindlist(
    Map(function(pr, pr_list) {
    rbindlist(
        Map(function(contrib, c_list) {
        tbl <- c_list[[stat_name]]
        if (is.null(tbl)) return(NULL)
        tbl <- as.data.table(tbl)
        tbl[, `:=`(pr = pr, contrib = contrib)]
        tbl
        }, names(pr_list), pr_list),
        fill = TRUE
    )
    }, names(x), x),
    fill = TRUE
)
}

summarise_contributions <- function(df) {
stats <- df %>%
    remove_data_id %>%
    summarise(across(everything(),
                    list(
                        p25 = ~ quantile(.x, 0.25, na.rm = TRUE),
                        p50 = ~ median(.x, na.rm = TRUE),
                        p75 = ~ quantile(.x, 0.75, na.rm = TRUE),
                        avg = ~ mean(.x, na.rm = TRUE),
                        neg = ~ sum(.x < 0, na.rm = TRUE) / 35 / 8736
                    ), .names = "{.col}__{.fn}"
    )
    ) %>% 
    pivot_longer(cols = everything(), names_to = c("technology", "stat"),
                names_sep = "__", values_to = "value")
result <- list(
    "p25" = stats %>% dplyr::filter(stat == "p25"),
    "p50" = stats %>% dplyr::filter(stat == "p50"),
    "p75" = stats %>% dplyr::filter(stat == "p75"),
    "avg" = stats %>% dplyr::filter(stat == "avg"),
    "neg" = stats %>% dplyr::filter(stat == "neg")
)
return(result)
}



concat_fscd_from_several_mcy <- function(simulation_data_one_pr){
# sim_data contains preprocessed antares data for 1 geo_scope only (8736 h * 35 years)
# So if this is to be done at EU scope, should be agregated first
# Same if >1 bidding zones from a single country, must be aggregated: do that first
# If working on a single BZ: to be filtered on prior to using function
  fscd_per_mcy_list <- list("lfmod" = list(), "mfmod" = list(), "hfmod" = list())
  
  for(mcy in 1:35){
      fsms_this_mcy <- simulation_data_one_pr %>%
      dplyr::filter(mcYear==mcy) %>%
      remove_data_id() %>%
      as.data.frame() %>%
      compute_fsms() # contains FSMS data with keys lfmod, mfmod, hfmod
      fscd_per_mcy_list[["lfmod"]][[mcy]] <- fsms_this_mcy[["lf_mod"]] %>% calculate_fs_contributions_one_mcy()
      fscd_per_mcy_list[["mfmod"]][[mcy]] <- fsms_this_mcy[["mf_mod"]] %>% calculate_fs_contributions_one_mcy()
      fscd_per_mcy_list[["hfmod"]][[mcy]] <- fsms_this_mcy[["hf_mod"]] %>% calculate_fs_contributions_one_mcy()
    }
  fscd_concat <- list(
      "lf_contributions" = rbindlist(fscd_per_mcy_list[["lfmod"]], use.names = TRUE, idcol = "mcYear"),
      "mf_contributions" = rbindlist(fscd_per_mcy_list[["mfmod"]], use.names = TRUE, idcol = "mcYear"),
      "hf_contributions" = rbindlist(fscd_per_mcy_list[["hfmod"]], use.names = TRUE, idcol = "mcYear")
    )
    return(fscd_concat)
}

compute_fscd_stat_diff <- function(nested_fscd_stats_by_country){
  pr_diff_list <- c("pr1-pr0", "pr2-pr1", "pr3-pr2")
  contrib_list <- c("lf_contributions", "mf_contributions", "hf_contributions")
  stat_list <- c("p25", "p50", "p75", "avg", "neg")
  nested_res <- list()
  
  for(pr_diff in pr_diff_list){
      nested_res[[pr_diff]] <- list()
      pr_a <- strsplit(pr_diff, "-", fixed = TRUE)[[1]][1] # computing stat diff pr_a - pr_b
      pr_b <- strsplit(pr_diff, "-", fixed = TRUE)[[1]][2]
      for(country in COUNTRIES){
        nested_res[[pr_diff]][[country]]<- list()
        for(contrib in contrib_list){
            nested_res[[pr_diff]][[country]][[contrib]]<- list()
            for(stat in stat_list){
              stat_a <- nested_fscd_stats_by_country[[pr_a]][[country]][[contrib]][[stat]]
              stat_b <- nested_fscd_stats_by_country[[pr_b]][[country]][[contrib]][[stat]]
              stat_diff <- merge(stat_a, stat_b, suffixes = c("_a", "_b"),
                                  by.x= c("technology", "stat"), by.y = c("technology", "stat")) %>%
                  mutate(value = value_a - value_b) %>%
                  select(c("technology", "stat", "value"))
              nested_res[[pr_diff]][[country]][[contrib]][[stat]] <- stat_diff
            }
          }
      }
  }
  return(nested_res)
}
