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
  modelName    = "MDCEV_care_0_constant",
  modelDescr   = "MDCEV model on ATUS with general categories of trip purposes and constants only",
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
database$travel_care     	= rowSums(database[,c("trv_personal","trv_household","trv_care_child","trv_care_adult","trv_ppservice", "trv_hhservice", "trv_admin","trv_groceries","trv_buyfood")])
database$travel_work     	= rowSums(database[,c("trv_work","trv_school")])
database$travel_leisure  	= rowSums(database[,c("trv_shopping","trv_eating","trv_social","trv_leisure", "trv_sport","trv_religious","trv_volunteer","trv_other")])


# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta = c(alpha_base              = -20,
                
                gamma_travel_care       = 1,
                gamma_travel_work       = 1,
                gamma_travel_leisure    = 1,
                
                delta_travel_care       = 0,
                delta_travel_work       = 0,
                delta_travel_leisure    = 0,
                
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
                    "travel_care",
                    "travel_work",
                    "travel_leisure")
  
  ### Define availabilities
  avail = list(outside		      = 1,
               travel_care      = 1,
               travel_work    	= 1,
               travel_leisure	  = 1)
  
  ### Define continuous consumption for individual alternatives
  continuousChoice = list(outside           = act/60, 
                          travel_care       = travel_care/60, 
                          travel_work       = travel_work/60, 
                          travel_leisure    = travel_leisure/60)
  
  ### Define utilities for individual alternatives
  V = list()
  V[["outside"            ]] = 0
  V[["travel_care"        ]] = delta_travel_care    
  V[["travel_work"        ]] = delta_travel_work    
  V[["travel_leisure"     ]] = delta_travel_leisure   
  
  
  ### Define alpha parameters
  alpha = list(outside           = 1 /(1 + exp(-alpha_base)), 
               travel_care       = 1 /(1 + exp(-alpha_base)), 
               travel_work       = 1 /(1 + exp(-alpha_base)), 
               travel_leisure    = 1 /(1 + exp(-alpha_base)))
               
  ### Define gamma parameters
  gamma = list(travel_care       = gamma_travel_care,
               travel_work       = gamma_travel_work,   
               travel_leisure    = gamma_travel_leisure
               )
  
  ### Define costs for individual alternatives
  cost = list(outside           = 1, 
              travel_care       = 1,
              travel_work       = 1, 
              travel_leisure    = 1
              )
  
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
