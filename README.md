# MDCEV on Time Use in Mobility of Care

## Application of Multiple Discrete-Continuous Extreme Value (MDCEV) to estimate factors affecting time use in the Mobility of Care compared to commuting and traveling for leisure

This analysis is made as part of course project of TTP 289A Advanced Discrete Choice of Modeling by Dr. David Bunch taken in UC Davis. 
It applies MDCEV in time use analysis with the topic of the mobility of care. This time use analysis utilizes American Time Use Survey (ATUS). 

This study is documented in **"MDCEV_mobilityofcare"** which contains the background of this study, the description of the ATUS as data source, the methods of MDCEV, the findings, and also the discussion.

### Description of Code in the Repository

The process of data wrangling as well as visualization of the descriptive statistics are documented in **"atus2022_mobilityofcare.Rmd"**. 
The original data is taken from ATUS website stored in the ATUS folder. 
This process produce a new data set for discrete choice modeling, **"atus2022_mdcev_detail.csv"**. 

This new data set is used for three MDCEV modeling.

1. **"MDCEV_0_constant.R"**

  This model only features constant of the alternatives with no attributes and socio-demographics. 
  The alternatives are the generalized categories of trip purposes: work trips, care trips, and leisure trips. 

2. **"MDCEV_1_general.R"**

  This model features not only the constant of the alternatives but also the socio-demographics. 
  The alternatives are the generalized categories of trip purposes: work trips, school trips, care trips, and leisure trips. 

3. **"MDCEV_2_detail.R"**

  This model features not only the constant of the alternatives but also the socio-demographics. 
  The alternatives are work trips, school trips, leisure trips, and more detailed care trips:
  1. trips for taking care of household,
  2. trips for taking care of household children,
  3. trips for taking care of household adult,
  4. trips for grocery shopping,
  5. trips for buying food,
  6. trips for doing errands.
