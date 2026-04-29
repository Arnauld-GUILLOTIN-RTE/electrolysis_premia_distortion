### Geographic parameters for data processing
POWER_AREAS <- c("at00", "be00", "beof", "ch00", "cz00", "de00", "dekf", "dkbh", "dke1", "dkkf",
                "dkns", "dkw1", "es00", "fi00", "fr00", "ie00", "itca", "itcn", "itcs", "itn1",
                "its1", "itsa", "itsi", "itvi", "nl00", "nlll", "nom1", "non1", "nos0", "pl00",
                "pt00", "se01", "se02", "se03", "se04", "uk00", "ukni")

H2_AREAS <- c("at00h2", "be00h2", "ch00h2", "cz00h2", "de00h2", "dk00h2", "es00h2", "fi00h2",
            "fr00h2", "ie00h2", "it00h2", "nl00h2", "pl00h2", "pt00h2", "se00h2", "uk00h2", "ibit00h2", "ibfi00h2")

COUNTRIES <- c("AT", "BE", "CH", "CZ", "DE", "DK", "ES", "FI", "FR", "IE", 
                "IT", "NL", "PL", "PT", "SE", "UK")

### Parameters for calculations
ELECTROLYSIS_EFFICIENCY = 0.637
SMR_EFFICIENCY = 0.76
SMR_CCS_EFFICIENCY = 0.6945
H2_ENERGY_DENSITY = 33.3  # kWh/kg

# Parameters used for plotting marginal cost heatmap of Fig. B.8
# Not based on output data: equals model parameters.
CARBON_PRICE_RANGE = seq(0, 300, length.out = 100) # EUR/tCO2
PREMIUM_RANGE = seq(0, 3, length.out = 100) # EUR/kgH2
CARBON_PRICE_REF = 147 # EUR/tCO2
CONVERSION <- list("res" = ELECTROLYSIS_EFFICIENCY, "nuclear"=ELECTROLYSIS_EFFICIENCY, 
                    "ccgt" = ELECTROLYSIS_EFFICIENCY, "coal"= ELECTROLYSIS_EFFICIENCY, 
                    "smr"= 1, "ocgt" = ELECTROLYSIS_EFFICIENCY, "smr_ccs" = 1, "import_ship_de"=1)

VARIABLE_COST <- list("res" = 0, "nuclear"=27.337, "ccgt" = 99.996, "ocgt"=132.795, 
                        "coal"= 124.337, "smr"= 75.98, "smr_ccs" = 54.402, "import_ship_de"=108.41) # EUR/MWh energy
CARBON_INTENSITY <- list("res" = 0, "nuclear"=0, "ccgt" = 0.276685, "ocgt"=0.368913, 
                        "coal"= 0.735652, "smr"= 0.203874, "smr_ccs" = 0.02231, "import_ship_de"=0) # tCO2/MWh energy


### Parameters for output data gathering and processing
  # NT2040 scenario 
  study_vec = c("NT2040_0", "NT2040_1", "NT2040_2", "NT2040_3")
  output_vec <- c(rep(-1,length(study_vec)))
  # simulation=-1 selects the most recent simulation output, which should be
  # unique provided simulation data was downloaded from the associated Zenodo
  # record (see README).

  #Sensitity scenario
  study_vec_sens <- c("SENS_0", "SENS_1", "SENS_2", "SENS_3")
  output_vec_sens <- c(rep(-1,length(study_vec_sens)))

  # Some of the figures in the paper illustrate effects on an 
  # example weather year:
  mcYear_example = 11

  ### Fig4 params corresponding to the week displayed in the paper. 
  dateBegin <- as.POSIXct("2018-05-07", tz = "UTC")
  dateEnd <- dateBegin + 3600*24*7-1
  dateRange <- c(dateBegin, dateEnd)