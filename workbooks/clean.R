library(lubridate)
library(tidyverse)

clean_date_str = function(date_str) {
  mdy = mdy(date_str)
  ymd = ymd(date_str)
  
  if(is.na(mdy)){
    if(is.na(ymd)){
      return(NA)
    }
    else {return(ymd)}
  }
  else {return(mdy)}
}

# fmt_df_dates = function(df) {
#   df = df %>% mutate('START.DATE' = clean_data_str(START.DATE), 'END.DATE' = clean_data_str(END.DATE))
#   return(df)

clean_text_col = function(txt) {
  # Remove all chars not letters or spaces
  txt = gsub("[^[:alnum:]#()-]", " ", txt)
  
  # Change all chars to lower
  txt = tolower(txt)
  
  # Remove other
  txt = gsub("other", "", txt)
  
  # Replace all multi space with single space
  # Remove all white space in begin and end
  txt = gsub("\\s+", " ", str_trim(txt))
  return(txt)
}

clean_storage = function(storage_data) {
  storage_clean = read_csv("../data/raw_data.csv")
  storage_clean = select(storage_clean, -c('Program Number','Location 1','Address 2','Tank Number','NYS Municipal Boundaries','New York Zip Codes','Counties'))
  
  storage_clean %>% rename(
    "Program.Type"="Program Type",
    "Site.Type"="Site Type Name", 
    "Facility.Name"="Program Facility Name", 
    'Address'="Address 1", 
    'ZIP'="ZIP Code",
    "DEC.Region"=`NYSDEC Region`,
    'Tank.Location'=`Tank Location`,
    'Tank.Status'=`Tank Status`,
    "Install.Date"=`Install Date`,
    "Quantity"=`Capacity in Gallons`,
    "Tank.Type"=`Tank Type`,
    "Close.Date" = `Close Date`,
    "Material"=`Material Name`,
    "Exp.Date" = `Expiration Date`,
    "Site.Status" = `Site Status Name`
  )
  
  
  # Date Clean
  storage_clean$`Install.Date` = clean_date_str(storage_clean$`Install.Date`)
  storage_clean$`Close.Date` = clean_date_str(storage_clean$`Close.Date`)
  storage_clean$`Expiration.Date` = clean_date_str(storage_clean$`Expiration.Date`)
  
  
  
  storage_clean = storage_clean %>% 
    mutate(Material = gsub("kerosene [#1 fuel oil] (on-site consumption)", "#1 fuel oil", Material,ignore.case = F, fixed = T)) %>% 
    mutate(Material = gsub("kerosene [#1 fuel oil] (resale/redistribute)", "#1 fuel oil", Material,ignore.case = F, fixed = T)) %>% 
    mutate(Material = gsub("#2 fuel oil (on-site consumption)", "#2 fuel oil", Material,ignore.case = F, fixed = T)) %>% 
    mutate(Material = gsub("#2 fuel oil (resale/redistribute)", "#2 fuel oil", Material,ignore.case = F, fixed = T)) %>% 
    mutate(Material = gsub("#4 fuel oil (on-site consumption)", "#4 fuel oil", Material,ignore.case = F, fixed = T)) %>% 
    mutate(Material = gsub("#4 fuel oil (resale/redistribute)", "#4 fuel oil", Material,ignore.case = F, fixed = T)) %>% 
    mutate(Material = gsub("#5 fuel oil (on-site consumption)", "#5 fuel oil", Material,ignore.case = F, fixed = T)) %>% 
    mutate(Material = gsub("#5 fuel oil (resale/redistribute)", "#5 fuel oil", Material,ignore.case = F, fixed = T)) %>% 
    mutate(Material = gsub("#6 fuel oil (on-site consumption)", "#6 fuel oil", Material,ignore.case = F, fixed = T)) %>% 
    mutate(Material = gsub("#6 fuel oil (resale/redistribute)", "#6 fuel oil", Material,ignore.case = F, fixed = T)) %>%
    mutate(Material = gsub("gasoline/ethanol", "ethanol", Material,ignore.case = F, fixed = T)) %>% 
    mutate(Material = gsub("Diesel (E-Gen)", "diesel", Material,ignore.case = F, fixed = T)) %>% 
    mutate(Material = gsub("used oil (heating, on-site consumption)", "used oil (heating)", Material,ignore.case = F, fixed = T)) %>% 
    mutate(Material = gsub("waste oil/used oil", "waste oil", Material,ignore.case = F, fixed = T)) %>% 
    mutate(Material = gsub("Biodiesel (E-Gen)", "biodiesel", Material,ignore.case = F, fixed = T)) %>% 
    mutate(Material = gsub("biodiesel (heating, on-site consumption)", "biodiesel", Material,ignore.case = F, fixed = T)) %>% 
    
    mutate(Site.Type = gsub("Municipality (Incl. Waste Water Treatment Plants, Utilities, Swimming Pools, etc.)", "Municipality", Site.Type,ignore.case = F, fixed = T)) %>% 
    mutate(Site.Type = gsub("Religious Building (Church, Synagogue, Mosque, Temple, etc.)", "Relgious Building", Site.Type,ignore.case = F, fixed = T)) %>% 
    mutate(Site.Type = gsub("Manufacturing (Other than Chemical)/Processing", "Non-Chemical Manufacturing/Processing", Site.Type,ignore.case = F, fixed = T)) %>% 
    mutate(Site.Type = gsub("Unknown", "Other", Site.Type,ignore.case = F, fixed = T)) %>% 
    mutate(Site.Type = gsub("Apartment Building/Office Building", "Apartment/Office Building", Site.Type,ignore.case = F, fixed = T)) %>% 
    mutate(Site.Type = gsub("Other Wholesale/Retail Sales", "Retail Gasoline Sales", Site.Type,ignore.case = F, fixed = T)) %>% 
    mutate(Site.Type = gsub("Auto Service/Repair (No Gasoline Sales)", "Auto Service", Site.Type,ignore.case = F, fixed = T)) %>% 
    mutate(Site.Type = gsub("Storage Terminal/Petroleum Distributor", "Petroleum Storage/Distribution", Site.Type,ignore.case = F, fixed = T)) %>% 
    mutate(Site.Type = gsub("Hospital/Nursing Home/Health Care", "Hospital/Health Care", Site.Type,ignore.case = F, fixed = T)) %>% 
    mutate(Site.Type = gsub("Airport/Airline/Air Taxi", "Airport", Site.Type,ignore.case = F, fixed = T)) %>% 
    mutate(Site.Type = gsub("Trucking/Transportation/Fleet Operation", "Trucking/Transport", Site.Type,ignore.case = F, fixed = T)) %>% 
    
    mutate(Tank.Location = gsub("Aboveground - in contact with impervious barrier", "Aboveground (impervious barrier)", Tank.Location,ignore.case = F, fixed = T)) %>% 
    mutate(Tank.Location = gsub("Underground including vaulted with no access for inspection", "Underground (no access)", Tank.Location,ignore.case = F, fixed = T)) %>% 
    mutate(Tank.Location = gsub("Aboveground - in contact with soil", "Aboveground (soil contact)", Tank.Location,ignore.case = F, fixed = T)) %>% 
    mutate(Tank.Location = gsub("Aboveground on saddles, legs, stilts, rack or cradle"  ,"Aboveground (no ground contact)", Tank.Location,ignore.case = F, fixed = T)) %>%
    mutate(Tank.Location = gsub("Aboveground in Subterranean vault with access for inspections", "Aboveground (subterranean, with access)", Tank.Location,ignore.case = F, fixed = T)) %>% 
    mutate(Tank.Location = gsub("Tank with 10% or more below ground", "Tank (>10% below ground)", Tank.Location,ignore.case = F, fixed = T)) %>% 
    mutate(Locality = gsub("new york city", "new york", Locality,ignore.case = F, fixed = T)) %>% 
    mutate(County = gsub("New York City", "New York", County.Name ,ignore.case = F, fixed = T))
  
  
  storage_clean = storage_clean %>% mutate('Material.Family' = case_when(
    Material %in% petrols[[1]] ~ "Petroleum",
    Material %in% hazards[[1]] ~ "Hazardous Material",
    
    
    Material == 'ethylene glycol' ~ "Commodity Chemical",
    Material == 'methanol' ~ "Commodity Chemical",
    Material == 'toluene' ~ "Commodity Chemical",
    Material == 'p-nitrotoluene' ~ "Commodity Chemical",
    Material == 'white caustic' ~ "Commodity Chemical",
    
    
    Material == 'propylene glycol, allyl ether' ~ "Commodity Chemical",
    Material == 'fluosilicic acid' ~ "Commodity Chemical",
    
    Material == 'diethylene glycol' ~ "Commodity Chemical",
    Material == 'ethanol' ~ "Commodity Chemical",
    
    Material == '#1 fuel oil' ~ "Petroleum",
    Material == '1,1,1-Trichloroethane(TCA)' ~ "Greenhouse Gas",
    Material == '1,1,2-trichloro-1,2,2-triflouroethane' ~ "Greenhouse Gas",
    Material == '1,2-dichloro-1,1,2,2-tetrafluoroethane' ~ "Greenhouse Gas",
    
    Material == 'benzene' ~ "Petrochemical",
    Material == 'benzene,1-methylethyl-' ~ "Petrochemical",
    Material == 'benzo(b)fluoranthene' ~ "Petrochemical",
    Material == 'benzotrichloride' ~ "Petrochemical",
    Material == 'benzoyl chloride' ~ "Petrochemical",
    Material == 'butadiene' ~ "Petrochemical",
    Material == 'pseudocumene' ~ "Petrochemical",
    
    Material == 'aluminum sulfate' ~ "Commodity Chemical",
    Material == 'adipic acid' ~ "Commodity Chemical",
    Material == 'acetone' ~ "Commodity Chemical",
    Material == '2-propanone' ~ "Commodity Chemical",
    Material == '2-propenoic acid, 2-methyl-, methyl ester' ~ "Commodity Chemical",
    Material == 'acetic acid' ~ "Commodity Chemical",
    Material == 'acetic anhydride' ~ "Commodity Chemical",
    Material == 'ammonia' ~ "Commodity Chemical",
    Material == 'ammonium acetate' ~ "Commodity Chemical",
    Material == 'ammonium bicarbonate' ~ "Commodity Chemical",
    Material == 'ammonium bisulfite' ~ "Commodity Chemical",
    Material == 'ammonium chloride' ~ "Commodity Chemical",
    Material == 'ammonium fluoborate' ~ "Commodity Chemical",
    Material == 'ammonium hydroxide' ~ "Commodity Chemical",
    Material == 'ammonium silicofluoride' ~ "Commodity Chemical",
    Material == 'ammonium sulfamate' ~ "Commodity Chemical",
    Material == 'ammonium thiocyanate' ~ "Commodity Chemical",
    Material == 'ammonium thiosulfate' ~ "Commodity Chemical",
    Material == 'aniline' ~ "Commodity Chemical",
    Material == 'chlorine' ~ "Commodity Chemical",
    Material == 'caustic soda' ~ "Commodity Chemical",
    Material == 'butyl acetate' ~ "Commodity Chemical",
    Material == 'caustic potash' ~ "Commodity Chemical",
    Material == 'caustic soda' ~ "Commodity Chemical",
    
    
    Material == 'chlorobenzene' ~ "Commodity Chemical",
    Material == 'chlorodifluoromethane' ~ "Greenhouse Gas",
    Material == 'chloroethane' ~ "Commodity Chemical",
    Material == 'chromic acetate' ~ "Commodity Chemical",
    Material == 'chromic sulfate' ~ "Commodity Chemical",
    Material == 'chromium' ~ "Commodity Chemical",
    
    Material == 'chromous chloride' ~ "Commodity Chemical",
    Material == 'clarified oil (resale/redistribute)' ~ "Petroleum",
    Material == 'coal tar pitch volatiles' ~ "Petrochemical",
    Material == 'copper' ~ "Commodity Chemical",
    Material == 'cumene' ~ "Petrochemical",
    
    
    Material == 'cupric sulfate' ~ "Commodity Chemical",
    Material == 'cyclohexane' ~ "Commodity Chemical",
    Material == 'cyclohexanone' ~ "Commodity Chemical",
    Material == 'cyclohexylamine' ~ "Commodity Chemical",
    Material == 'DEG' ~ "Hazardous Material",
    Material == 'dibenzofuran' ~ "Petrochemical",
    
    
    Material == 'hydrogen peroxide' ~ "Commodity Chemical",
    Material == 'hydrogen chloride' ~ "Commodity Chemical",
    Material == 'hydrochloric acid' ~ "Commodity Chemical",
    Material == 'muriatic acid' ~ "Commodity Chemical",
    Material == 'lead' ~ "Commodity Chemical",
    Material == 'hexane' ~ "Petrochemical",
    
    
    Material == 'MEK' ~ "Commodity Chemical",
    Material == 'MDI (methylene diphenyl diisocyanate)' ~ "Commodity Chemical",
    Material == 'phosphoric acid' ~ "Commodity Chemical",
    Material == 'sodium' ~ "Commodity Chemical",
    Material == 'sulfuric acid' ~ "Commodity Chemical",
    Material == 'used oil (heating)' ~ "Petroleum",
    Material == 'waste oil' ~ "Petroleum",
    Material == 'zinc' ~ "Commodity Chemical",
    
    Material %in% other[[1]] ~ "Other",
    TRUE ~ "Hazardous Material"
  ))
  
}

