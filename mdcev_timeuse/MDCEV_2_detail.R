# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

### Clear memory
# rm(list = ls())

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName    = "MDCEV_care_2_detail",
  modelDescr   = "MDCEV model on ATUS with detailed categories on care trip purposes and socio-demographics",
  indivID      = "id", 
  outputDirectory = "output"
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

### Loading data from package
### if data is to be loaded from a file (e.g. called data.csv), 
### the code would be: database = read.csv("data.csv",header=TRUE)
database = read.csv("atus2022_mdcev_detail.csv",header=TRUE)
database$travel_personal  	= database$trv_personal
database$travel_household 	= database$trv_household
database$travel_care_child = database$trv_care_child
database$travel_care_adult = database$trv_care_adult
database$travel_errand  	 = rowSums(database[,c("trv_ppservice", "trv_hhservice", "trv_admin")])
database$travel_groceries	 = database$trv_groceries
database$travel_buyfood  	 = database$trv_buyfood
database$travel_work     	 = database$trv_work
database$travel_school   	 = database$trv_school
database$travel_leisure  	 = rowSums(database[,c("trv_religious","trv_volunteer","trv_shopping","trv_eating","trv_social","trv_leisure", "trv_sport","trv_other")])


# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta = c(alpha_base              = -20,
                
                gamma_travel_personal   = 1,
                gamma_travel_household 	= 1,
                gamma_travel_care_child = 1,
                gamma_travel_care_adult = 1,
                gamma_travel_errand     = 1,
                gamma_travel_groceries 	= 1,
                gamma_travel_buyfood 	 	= 1,
                gamma_travel_work       = 1,
                gamma_travel_school     = 1,
                gamma_travel_leisure    = 1,
                
                delta_travel_personal   = 0,
                delta_travel_household 	= 0,
                delta_travel_care_child = 0,
                delta_travel_care_adult = 0,
                delta_travel_errand     = 0,
                delta_travel_groceries 	= 0,
                delta_travel_buyfood 	 	= 0,
                delta_travel_work       = 0,
                delta_travel_school     = 0,
                delta_travel_leisure    = 0,
                
                # delta_age_personal    	= 0,
                # delta_age_household 	 	= 0,
                # delta_age_care_child  	= 0,
                # delta_age_care_adult  	= 0,
                # delta_age_errand      	= 0,
                # delta_age_groceries 	 	= 0,
                # delta_age_buyfood 	 	 	= 0,
                # delta_age_work        	= 0,
                # delta_age_school        = 0,
                # delta_age_shopping 	 	 	= 0,
                # delta_age_eating 	 	 	 	= 0,
                # delta_age_social 	 	 	 	= 0,
                # delta_age_leisure     	= 0,
                # delta_age_religious 	 	= 0,
                # delta_age_volunteer 	 	= 0,

                delta_female_personal   = 0,
                delta_female_household 	= 0,
                delta_female_care_child = 0,
                delta_female_care_adult = 0,
                delta_female_errand     = 0,
                delta_female_groceries 	= 0,
                delta_female_buyfood 	 	= 0,
                delta_female_work       = 0,
                delta_female_school     = 0,
                delta_female_leisure    = 0,
                
                # delta_income_personal   = 0,
                # delta_income_household 	= 0,
                # delta_income_care_child = 0,
                # delta_income_care_adult = 0,
                # delta_income_errand     = 0,
                # delta_income_groceries 	= 0,
                # delta_income_buyfood 	 	= 0,
                # delta_income_work       = 0,
                # delta_income_shopping 	= 0,
                # delta_income_eating 	 	= 0,
                # delta_income_social 	 	= 0,
                # delta_income_leisure    = 0,
                # delta_income_religious 	= 0,
                # delta_income_volunteer 	= 0,
                
                #care for hh child
                delta_spouse_carechild	    = 0,
                delta_spousework_carechild	= 0,
                delta_mom_carechild	  	  	= 0,
                delta_youngest_carechild	  = 0,
                delta_childtime_carechild	  = 0,
                delta_trvwchild_carechild	  = 0,
                delta_trvwspouse_carechild	= 0,
                delta_trvwfam_carechild	    = 0,

                #care for adult
                delta_eldertime_careadult  	= 0,
                delta_trvwelder_careadult  	= 0,
                delta_trvelderact_careadult = 0,

                #care for buy groceries
                # delta_spouse_groceries	   = 0,
                # delta_spousework_groceries = 0,
                # delta_children_groceries   = 0,
                # delta_youngest_groceries   = 0,
                # delta_trvwchild_groceries	 = 0,
                # delta_trvwspouse_groceries = 0,
                # delta_trvwfam_groceries	   = 0,

                #care for buy food
                # delta_spouse_buyfood	   = 0,
                # delta_spousework_buyfood = 0,
                # delta_children_buyfood   = 0,
                # delta_youngest_buyfood   = 0,
                # delta_trvwchild_buyfood	 = 0,
                # delta_trvwspouse_buyfood = 0,
                # delta_trvwfam_buyfood	   = 0,

                #work-related
                # delta_work                = 0,
                delta_work_multijob      	= 0,
                delta_work_FTjob         	= 0,
                delta_work_PTjob         	= 0,

                delta_age_school          = 0,
                delta_school              = 0,
                
                sig	= 1)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("alpha_base","sig")

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### Define individual alternatives
  alternatives  = c("outside",
                    "travel_personal",
                    "travel_household",
                    "travel_care_child", 
                    "travel_care_adult", 
                    "travel_errand", 
                    "travel_groceries",
                    "travel_buyfood",
                    "travel_work", 
                    "travel_school",
                    "travel_leisure")
  
  ### Define availabilities
  avail = list(outside		       = 1,
               travel_personal	 = 1,
               travel_household	 = 1,
               travel_care_child = 1,
               travel_care_adult = 1,
               travel_errand	   = 1,
               travel_groceries	 = 1,
               travel_buyfood	   = 1,
               travel_work    	 = 1,
               travel_school  	 = 1,
               travel_leisure	   = 1)
  
  ### Define continuous consumption for individual alternatives
  continuousChoice = list(outside           = act/60, 
                          travel_personal   = travel_personal/60, 
                          travel_household  = travel_household/60,
                          travel_care_child = travel_care_child/60, 
                          travel_care_adult = travel_care_adult/60, 
                          travel_errand     = travel_errand/60, 
                          travel_groceries  = travel_groceries/60,
                          travel_buyfood    = travel_buyfood/60,
                          travel_work       = travel_work/60, 
                          travel_school     = travel_school/60,
                          travel_leisure    = travel_leisure/60)
  
  ### Define utilities for individual alternatives
  V = list()
  V[["outside"            ]] = 0
  V[["travel_personal"    ]] = delta_travel_personal   + delta_female_personal   * female #+ delta_age_personal   * age + delta_income_personal   * income
  V[["travel_household"   ]] = delta_travel_household  + delta_female_household  * female #+ delta_age_household  * age + delta_income_household  * income
  V[["travel_care_child"  ]] = delta_travel_care_child + delta_female_care_child * female + delta_spouse_carechild * spouse + delta_spousework_carechild * spouse * spousework + delta_mom_carechild * children * female * work + delta_youngest_carechild * children * youngest + delta_childtime_carechild * children * childtime + delta_trvwchild_carechild * children * trv.care_w.hhchild + delta_trvwspouse_carechild * spouse * trv.care_w.spouse + delta_trvwfam_carechild * children * spouse * trv.care_w.hhchild * trv.care_w.spouse #+ delta_age_care_child * age + delta_income_care_child * income    
  V[["travel_care_adult"  ]] = delta_travel_care_adult + delta_female_care_adult * female + delta_eldertime_careadult * eldertime + delta_trvwelder_careadult * trv.care_w.elder + delta_trvelderact_careadult * eldertime * trv.care_w.elder #+ delta_age_care_adult * age + delta_income_care_adult * income #
  V[["travel_errand"      ]] = delta_travel_errand     + delta_female_errand     * female #+ delta_age_errand     * age + delta_income_errand     * income  
  V[["travel_groceries"   ]] = delta_travel_groceries  + delta_female_groceries  * female #+ delta_age_groceries  * age + delta_income_groceries  * income #+ delta_trvwchild_groceries * children * trv.groceries_w.hhchild + delta_trvwspouse_groceries * spouse * trv.groceries_w.spouse #+ delta_spouse_groceries * spouse + delta_children_groceries * children + delta_spousework_groceries * spouse * spousework + delta_youngest_groceries * children * youngest + delta_trvwfam_carechild * children * spouse * trv.groceries_w.hhchild * trv.groceries_w.spouse  
  V[["travel_buyfood"     ]] = delta_travel_buyfood    + delta_female_buyfood    * female #+ delta_age_buyfood    * age + delta_income_buyfood    * income #+ delta_trvwchild_buyfood * children * trv.food_w.hhchild + delta_trvwspouse_buyfood * spouse * trv.food_w.spouse #+ delta_spouse_buyfood * spouse + delta_children_buyfood * children + delta_spousework_buyfood * spouse * spousework + delta_youngest_buyfood * children * youngest + delta_trvwfam_carechild * children * spouse * trv.food_w.hhchild * trv.food_w.spouse  
  V[["travel_work"        ]] = delta_travel_work       + delta_female_work       * female + delta_work_multijob * work * multijob + delta_work_FTjob * work * FTjob + delta_work_PTjob * work * PTjob #+ delta_age_work       * age + delta_income_work       * income #
  V[["travel_school"      ]] = delta_travel_school     + delta_female_school     * female * school + delta_age_school * (age<25) * school + delta_school * school
  V[["travel_leisure"     ]] = delta_travel_leisure    + delta_female_leisure    * female #+ delta_age_leisure    * age + delta_income_leisure    * income  
  
  
  ### Define alpha parameters
  alpha = list(outside           = 1 /(1 + exp(-alpha_base)), 
               travel_personal   = 1 /(1 + exp(-alpha_base)),
               travel_household  = 1 /(1 + exp(-alpha_base)),
               travel_care_child = 1 /(1 + exp(-alpha_base)), 
               travel_care_adult = 1 /(1 + exp(-alpha_base)), 
               travel_errand     = 1 /(1 + exp(-alpha_base)), 
               travel_groceries  = 1 /(1 + exp(-alpha_base)),
               travel_buyfood    = 1 /(1 + exp(-alpha_base)),
               travel_work       = 1 /(1 + exp(-alpha_base)), 
               travel_school     = 1 /(1 + exp(-alpha_base)), 
               travel_leisure    = 1 /(1 + exp(-alpha_base)))
               
  ### Define gamma parameters
  gamma = list(travel_personal   = gamma_travel_personal,
               travel_household  = gamma_travel_household,
               travel_care_child = gamma_travel_care_child,
               travel_care_adult = gamma_travel_care_adult,
               travel_errand     = gamma_travel_errand,
               travel_groceries  = gamma_travel_groceries,
               travel_buyfood    = gamma_travel_buyfood,
               travel_work       = gamma_travel_work,    
               travel_school     = gamma_travel_school,
               travel_leisure    = gamma_travel_leisure)
  
  ### Define costs for individual alternatives
  cost = list(outside           = 1, 
              travel_personal   = 1,
              travel_household  = 1, 
              travel_care_child = 1,
              travel_care_adult = 1,
              travel_errand     = 1, 
              travel_groceries  = 1,
              travel_buyfood    = 1,
              travel_work       = 1, 
              travel_school     = 1,
              travel_leisure    = 1)
  
  ### Define settings for MDCEV model
  mdcev_settings <- list(alternatives      = alternatives,
                         avail             = avail,
                         continuousChoice  = continuousChoice,
                         utilities         = V,
                         alpha             = alpha,
                         gamma             = gamma, 
                         sigma             = sig, 
                         cost              = cost,
                         budget            = 24)
  
  ### Compute probabilities using MDCEV model
  P[["model"]] = apollo_mdcev(mdcev_settings, functionality)
  
  ### Take product across observation for same individual
  # P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

# ################################################################# #
#### MODEL OUTPUTS                                               ####
# ################################################################# #

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(model)

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO FILE, using model name)               ----
# ----------------------------------------------------------------- #

apollo_saveOutput(model)

# ################################################################# #
##### POST-PROCESSING                                            ####
# ################################################################# #

### Print outputs of additional diagnostics to new output file (remember to close file writing when complete)
apollo_sink()

# ----------------------------------------------------------------- #
#---- LR TEST AGAINST MNL MODEL                                  ----
# ----------------------------------------------------------------- #

apollo_lrTest("../output/MDCEV_care_1_general", model)

# ----------------------------------------------------------------- #
#---- COMBINED RESULTS OF GEV MODELS                             ----
# ----------------------------------------------------------------- #

apollo_combineResults(combineResults_settings = list(modelNames=c("../output/MDCEV_care_1_general",
                                                                  "../output/MDCEV_care_2_detail"
                                                                  )))

# ----------------------------------------------------------------- #
#---- switch off writing to file                                 ----
# ----------------------------------------------------------------- #

apollo_sink()
