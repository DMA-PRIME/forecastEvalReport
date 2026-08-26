#------------------------------------------------------------------------------#
#                                                                              #
#                        Options File Template                                 #
#                                                                              #
#------------------------------------------------------------------------------#
# About:                                                                       #
#                                                                              #
# This file configures the forecast evaluation report. Fill in each            #
# field below with the information specific to your model and data. Fields     #
# marked REQUIRED must have a value before the report can be generated.        #
# Fields marked OPTIONAL may be set to NA if they do not apply to your         #
# current report. Do not rename the `report_options` list or remove any        #
# fields from it.                                                              #
#                                                                              #
# ---- Workflow -------------------------------------------------------------- #
#                                                                              #
# Follow these steps in order to produce your report:                          #
#                                                                              #
# STEP 1 (already done): Create this options file.                             #
#   create_options_template()                                                  #
#                                                                              #
# STEP 2 (do this now): Fill in all fields in this file EXCEPT                 #
#   variables.crosswalk.file. Leave that field as NA for now.                  #
#                                                                              #
# STEP 3: Build the variables crosswalk. Run this after completing Step 2:     #
#   build_crosswalk_from_options('path/to/report_options_template.R')          #
#   This reads your options file, validates your model files, and writes a     #
#   seeded crosswalk CSV to your working directory.                            #
#                                                                              #
# STEP 4: Complete the crosswalk file. Open the CSV written in Step 3 and      #
#   fill in the following columns for each row:                                #
#     clean_name_full  -- full report-facing name of each variable             #
#     clean_name_abb   -- abbreviated report-facing name                       #
#     definition       -- required for outcome, aux_variable, general_term     #
#     on_right_axis    -- TRUE/FALSE for aux_variable rows only                #
#     convert_percent  -- TRUE/FALSE for aux_variable rows only                #
#     binning          -- incident, severity, or burden (if applicable)        #
#     cohort           -- dx or dx_cond_lab (if applicable)                    #
#     file             -- file path for aux_variable rows                      #
#   You may also add rows for additional data sources, auxiliary variables,    #
#   or custom glossary terms.                                                  #
#                                                                              #
# STEP 5: Add the crosswalk file path to this options file. Update the         #
#   variables.crosswalk.file field below with the path to the completed        #
#   crosswalk CSV from Step 3.                                                 #
#                                                                              #
# STEP 6: Generate the report. Run:                                            #
#   generate_report('path/to/report_options_template.R')                       #
#                                                                              #
#------------------------------------------------------------------------------#

report_options <- list(

#------------------------------------------------------------------------------#
# Contact Information                                                          #
#------------------------------------------------------------------------------#
# About: To ensure proper attribution of all information included in the       #
# report, please provide your name and email address. If more than one         #
# person should be credited, include all names and emails within quotes and    #
# separated by commas. For example:                                            #
#                                                                              #
#                    name  = 'John Smith, Jane Doe'                            #
#                    email = 'john@example.com, jane@example.com'              #
#                                                                              #
# The report will display the name(s) and email(s) exactly as provided.        #
#------------------------------------------------------------------------------#

  ##################################################################
  # REQUIRED. Your full name(s) as they should appear in the       #
  # report. Separate multiple names with commas.                   #
  ##################################################################
  name  = "Li Shandross, Evan Ray, Alexander Webber, Sarabeth Mathis, Rebecca Borchering",

  ##################################################################
  # REQUIRED. Your email address(es). Separate multiple addresses  #
  # with commas.                                                   #
  ##################################################################
  email = "lshandross@umass.edu, elray@umass.edu, rpe5@cdc.gov, nqr2@cdc.gov and xhq2@cdc.gov",

#------------------------------------------------------------------------------#
# Model and Report Configuration                                               #
#------------------------------------------------------------------------------#
# About: This section identifies the disease, forecasting context, model       #
# type, and working directory for this report. The `reason` field tells the    #
# report system which format your model files are in and which validators      #
# to apply. The `general.model.type` field is a plain-language label for       #
# your model and will appear in the report title and filename. The             #
# `base.box` field should be your primary working directory (typically your    #
# Box or network folder root) and is used as a path reference in downstream    #
# processing steps.                                                            #
#------------------------------------------------------------------------------#

  ##################################################################
  # REQUIRED. The disease being forecast. This value is used       #
  # throughout the report for labeling and will appear in the      #
  # output filename. Common values:                                #
  #   'influenza' -- for FluSight or MetroCast submissions         #
  #   'covid_19'  -- for COVID-19 Forecast Hub submissions         #
  #   'RSV'       -- for RSV Forecast Hub submissions              #
  # Any other string is accepted for internal or custom models.    #
  ##################################################################
  disease = "influenza",

  ##################################################################
  # REQUIRED. The forecasting context for this report. This tells  #
  # the report system which file format and validation rules to    #
  # apply to your model files. Allowed values (case-insensitive):  #
  #   'FluSight'  -- CDC FluSight forecasting challenge            #
  #   'COVIDHub'  -- COVID-19 Forecast Hub                         #
  #   'RSVHub'    -- RSV Forecast Hub                              #
  #   'MetroCast' -- Flu MetroCast Hub                             #
  #   'DoD'       -- Department of Defense forecasting             #
  #   'Software'  -- Internal software/general format model        #
  #   'Internal'  -- Internal general format model (default)       #
  # If left NA, defaults to 'Internal'.                            #
  ##################################################################
  reason = 'FluSight',

  ##################################################################
  # REQUIRED. The general type or name of your forecasting model.  #
  # This is a plain-language label that will appear in the report  #
  # header and output filename. Use a short, descriptive name.     #
  # Examples: 'ARIMA', 'ETS', 'Prophet', 'Random Forest',          #
  #           'SARIMA', 'LSTM', 'Ensemble'                         #
  ##################################################################
  general.model.type = "FluSight Ensemble",

  ##################################################################
  # OPTIONAL. A plain-language label for the target population     #
  # studied in this report. This label appears as-is in the report #
  # wherever the population is referenced. If left NA, no          #
  # population label will be displayed.                            #
  # Examples: 'General Population', 'Health System Patients',      #
  #           'Pediatric Inpatients', 'Adults 65+'                 #
  ##################################################################
  population.label = "General Population",

  ##################################################################
  # REQUIRED. Your primary working directory. This is typically    #
  # the root of your Box, OneDrive, or network folder where        #
  # project files are stored. Used as a path reference in          #
  # downstream processing. Provide the full directory path.        #
  # Example: 'C:/Users/USERNAME/Box/'                              #
  ##################################################################
  base.box = "C:/Users/ambleic/Box/BoxPHI-PHMR Projects/",

#------------------------------------------------------------------------------#
# Model Files                                                                  #
#------------------------------------------------------------------------------#
# About: This section specifies the file paths to your model output files.     #
# At least one of implementation.model.file or evaluation.model.file must      #
# be provided -- you may provide both. Set any unused path to NA.              #
#                                                                              #
# The implementation model file contains the operational forecast from your    #
# finalized, post-training model. For hubverse-format submissions (FluSight,   #
# COVIDHub, RSVHub, MetroCast) the file must follow the hubverse column        #
# specification. For Software or Internal reasons, it must follow the          #
# general forecast format (16 columns including location_general, disease,     #
# population, etc.).                                                           #
#                                                                              #
# The evaluation model file contains forecasts from the training/validation/   #
# testing period. It must always follow the general forecast format and must   #
# contain only historical rows (target_end_date < reference_date). Filter      #
# your file to historical rows before providing it here.                       #
#------------------------------------------------------------------------------#

  ##################################################################
  # REQUIRED (at least one). Full file path to the implementation  #
  # model CSV. The format must match the `reason` field above.     #
  # Set to NA if only providing an evaluation file.                #
  # Example: 'C:/Users/ambleic/Box/Models/impl_2024-10-19.csv'     #
  ##################################################################
  implementation.model.file = "C:/Users/ambleic/Box/BoxPHI-PHMR Projects/Amanda/forecastEvalReport/Forecasts/flusight-total_influenza_admi-general_population-state_national_MULTI-flusight_ensemb/Forecast-2026-05-23.csv",

  ##################################################################
  # REQUIRED (at least one). Full file path to the evaluation      #
  # model CSV. Must follow the general forecast format. Only rows  #
  # where target_end_date < reference_date are accepted -- filter  #
  # your file to historical rows before providing it here.         #
  # Set to NA if only providing an implementation file.            #
  # Example: 'C:/Users/ambleic/Box/Models/eval_2024-10-19.csv'     #
  ##################################################################
  evaluation.model.file = NA,

#------------------------------------------------------------------------------#
# Outcome Information                                                          #
#------------------------------------------------------------------------------#
# About: This section identifies the target outcome data used to evaluate      #
# the model's forecasts. The outcome data is the 'ground truth' or observed    #
# data against which forecasts are compared. The `outcome.data.label` field    #
# is a short label for the data source (e.g., 'CDC(NHSN)') that will be        #
# used in plots and tables. The `outcome.data` field is the full file path     #
# to the observed data CSV. The `outcome.name` field is the exact column       #
# name in that CSV that contains the target variable -- it must match the      #
# column name character-for-character.                                         #
#------------------------------------------------------------------------------#

  ##################################################################
  # REQUIRED. A short label for the outcome data source. This will #
  # appear in plots, tables, and the variables crosswalk. Use a    #
  # consistent label that identifies the data source clearly.      #
  # This is also the merge key for the outcome rows in the cross   #
  #                                                                #
  #   Examples: 'CDC(NHSN)', 'Health_System', 'RFA', 'CDC(NSSP)'   #
  ##################################################################
  outcome.data.label = "CDC(NHSN)",

  ##################################################################
  # REQUIRED. Full file path to the outcome (observed/truth) data  #
  # CSV. This file contains the observed values that the model's   #
  # forecasts are evaluated against.                               #
  #                                                                #
  #       Example: 'C:/Users/USER/Box/Data/outcome_data.csv'       #
  ##################################################################
  outcome.data = "C:/Users/ambleic/Downloads/target-hospital-admissions (6).csv",

  ##################################################################
  # REQUIRED. The exact column name of the target variable in the  #
  # outcome data CSV. This must match the column name in the file  #
  # at outcome.data character-for-character, including case.       #
  #                                                                #
  #   Examples: 'cases', 'hospitalizations', 'wk_inc_flu_hosp',    #
  #           'weekly_ed_visits', 'confirmed_admissions'           #
  ##################################################################
  outcome.name = "Total.Influenza.Admissions",

#------------------------------------------------------------------------------#
# Training Data (Optional)                                                     #
#------------------------------------------------------------------------------#
# About: This section identifies the training data used during model           #
# development. Training data is any external data source incorporated into     #
# the model during the fitting process beyond the primary outcome data.        #
# All three fields must be filled in together -- you cannot provide some       #
# but not others. If you did not use a separate training data source, set      #
# all three fields to NA.                                                      #
#                                                                              #
# The `training_data_source` field (note: underscore, not dot) is a short      #
# label for the training data source, similar to outcome.data.label.           #
#------------------------------------------------------------------------------#

  ##################################################################
  # OPTIONAL. Full file path to the training data CSV. Set to NA   #
  # if not using a separate training data source. If provided, all #
  # three training fields must be filled in.                       #
  # Example: 'C:/Users/ambleic/Box/Data/training_data.csv'         #
  ##################################################################
  training.data.file = NA,

  ##################################################################
  # OPTIONAL. The exact column name of the training variable in    #
  # the training data CSV. Must match the column name character-   #
  # for-character. Set to NA if not using training data.           #
  # Example: 'google_trends_flu', 'wastewater_signal'              #
  ##################################################################
  training.variable.name = NA,

  ##################################################################
  # OPTIONAL. A short label for the training data source. Used     #
  # for labeling in plots and the variables crosswalk. Set to NA   #
  # if not using training data.                                    #
  # Examples: 'CDC(NSSP)', 'Google_Trends', 'Biobot_Wastewater'    #
  ##################################################################
  training_data_source = NA,

#------------------------------------------------------------------------------#
# Model Descriptions                                                           #
#------------------------------------------------------------------------------#
# About: This section provides plain-language descriptions of each model       #
# included in the report. The descriptions are used in the report's methods    #
# section to help readers understand what each model does without requiring    #
# technical expertise. Provide one row per model. The `model` column should    #
# contain a short model identifier (the same label used in your model files)   #
# and the `description` column should contain a plain-language explanation     #
# of the model's approach and any key assumptions.                             #
#                                                                              #
# If reporting on a single model, provide one row. If comparing multiple       #
# models, add one row per model using c() for the column vectors.              #
#------------------------------------------------------------------------------#

  ##################################################################
  # REQUIRED. A data frame with two columns:                       #
  #   model       -- short model identifier (character)            #
  #   description -- plain-language model description (character)  #
  #                                                                #
  # Single model example:                                          #
  #                                                                #
  #   model.descriptions = data.frame(                             #
  #     model       = 'ARIMA',                                     #
  #     description = 'A seasonal ARIMA model fit to weekly        #
  #                    hospital admissions data. The model uses    #
  #                    autoregressive and moving average terms to  #
  #                    capture temporal patterns in the data.'     #
  #   )                                                            #
  #                                                                #
  # Multiple model example:                                        #
  #                                                                #
  #   model.descriptions = data.frame(                             #
  #     model       = c('ARIMA', 'ETS'),                           #
  #     description = c('A seasonal ARIMA model...', 'An           #
  #                      exponential smoothing model...')          #
  #   )                                                            #
  ##################################################################
  model.descriptions = data.frame(
    model       = "FluSight Ensemble",
    description = "The Hubverse package hubEnsembles is used to generate the FluSight-ensemble forecast A median of quantile outputs is used for all eligible quantile-based forecasts. The mean is used to ensemble probabilities from eligible categorical rate-trend forecasts."
  ),

#------------------------------------------------------------------------------#
# Variables Crosswalk                                                          #
#------------------------------------------------------------------------------#
# About: The variables crosswalk maps all variables in the report (data        #
# sources, outcomes, auxiliary variables, and glossary terms) to their         #
# report-facing names, definitions, and metadata. It powers the report's       #
# data dictionary, plot labels, and documentation sections.                    #
#                                                                              #
# IMPORTANT: Leave this field as NA when first filling in this file.           #
# The crosswalk must be built and completed before it can be referenced        #
# here. Follow the workflow below:                                             #
#                                                                              #
# 1. Fill in all other fields in this file first (Steps 2).                    #
# 2. Run build_crosswalk_from_options() to generate the seeded crosswalk:      #
#      build_crosswalk_from_options('path/to/report_options_template.R')       #
# 3. Open the crosswalk CSV and complete the required fields (Step 4).         #
# 4. Return to this file and fill in the path to the completed crosswalk       #
#    in the variables.crosswalk.file field below (Step 5).                     #
# 5. Run generate_report() to produce the final report (Step 6).               #
#                                                                              #
# The crosswalk filename is automatically constructed from your name,          #
# disease, reason, and model type when build_crosswalk_from_options() runs.    #
#------------------------------------------------------------------------------#

  ##################################################################
  # REQUIRED (for Step 6 only). Full file path to your completed   #
  # and validated variables crosswalk CSV. Leave as NA until you   #
  # have completed Steps 2 through 5 above.                        #
  #                                                                #
  #         Example: 'C:/Users/USERNAME/Box/Crosswalks/            #
  #               NAME-influenza-FluSight-ARIMA.csv'               #
  ##################################################################
  variables.crosswalk.file = "C:/Users/ambleic/Box/BoxPHI-PHMR Projects/Amanda/forecastEvalReport/Li_Shandross,_Evan_Ray,_Alexander_Webber,_Sarabeth_Mathis,_Rebecca_Borchering-influenza-FluSight-FluSight_Ensemble.csv",

#------------------------------------------------------------------------------#
# Geographic Context (Optional)                                                #
#------------------------------------------------------------------------------#
# About: When working with county-level or HSA-level forecasts, location       #
# names can be ambiguous across states -- for example, 'Washington County'     #
# exists in many states. The `state.context` field restricts location          #
# matching to a single state, eliminating this ambiguity. If your model        #
# covers multiple states or uses national/state-level locations (as in         #
# FluSight, COVIDHub, or RSVHub), leave this NA.                               #
#                                                                              #
# This field only affects validation of county and HSA location values.        #
# It has no effect on state-level, national, ZCTA, ZIP, or region rows.        #
#------------------------------------------------------------------------------#

  ##################################################################
  # OPTIONAL. The U.S. state for county and HSA location           #
  # disambiguation. Accepts either a two-letter abbreviation or a  #
  # full state name (case-insensitive). Set to NA if not working   #
  # with county or HSA level data, or if your model covers         #
  # multiple states.                                               #
  # Examples: 'SC', 'South Carolina', 'TX', 'Texas'                #
  ##################################################################
  state.context = "South Carolina"

)


