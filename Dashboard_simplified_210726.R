# 05-tabs.R



# 0 - Preparations --------------------------------------------------------

#import libraries
library(tidyverse)
library(shinydashboard)
library(shiny)
library(viridis)
library("dashboardthemes")
library("lattice")
#set working directory (to current)
##########################

# Import data -------------------------------------------------------------

allResults <- read.delim("world_allProd.csv", header = TRUE, sep = ",")
Food_contents <- read.delim("VCommod_Food_Contents_MR.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)
Nut_ha_agg <- read.delim("nut_ha_agg_V.csv", header = TRUE, sep = ",")
Population <- read.delim("VPopulationNumbers_MR.csv", header = TRUE, sep = ";")
tresholds <- read.delim("Nutrient_tresholdValues_210610.csv", header = TRUE, sep = ";")
CropRotCat <- read.delim("crops_CropRotCategory.csv", header = TRUE, sep = ";")
CroppingAreas <- read.delim("VActCropsGrass_QuantityActUnits_MR.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)
FoodProductionContents <- read.delim("VCommod_Food_Contents_Prod_MR.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)


#Results of vegan/organic variable shares 
allResults_ov1 <- read.delim("ov_0to100/world_allProd_Run1.csv", header = TRUE, sep = ",")
Food_contents_ov1 <- read.delim("ov_0to100/VCommod_Food_Contents_MR_Run1.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)
CroppingAreas_ov1 <- read.delim("ov_0to100/VActCropsGrass_QuantityActUnits_MR_Run1.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)
FoodProductionContents_ov1 <- read.delim("ov_0to100/VCommod_Food_Contents_Prod_MR_Run1.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)

allResults_ov2 <- read.delim("ov_0to100/world_allProd_Run2.csv", header = TRUE, sep = ",")
Food_contents_ov2 <- read.delim("ov_0to100/VCommod_Food_Contents_MR_Run2.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)
CroppingAreas_ov2 <- read.delim("ov_0to100/VActCropsGrass_QuantityActUnits_MR_Run2.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)
FoodProductionContents_ov2 <- read.delim("ov_0to100/VCommod_Food_Contents_Prod_MR_Run2.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)

allResults_ov3 <- read.delim("ov_0to100/world_allProd_Run3.csv", header = TRUE, sep = ",")
Food_contents_ov3 <- read.delim("ov_0to100/VCommod_Food_Contents_MR_Run3.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)
CroppingAreas_ov3 <- read.delim("ov_0to100/VActCropsGrass_QuantityActUnits_MR_Run3.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)
FoodProductionContents_ov3 <- read.delim("ov_0to100/VCommod_Food_Contents_Prod_MR_Run3.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)

allResults_ov1 %>%
  rbind(allResults_ov2) %>%
  rbind(allResults_ov3) -> allResults_ov


Food_contents_ov1 %>%
  rbind(Food_contents_ov2) %>%
  rbind(Food_contents_ov3) -> Food_contents_ov

CroppingAreas_ov1 %>%
  rbind(CroppingAreas_ov2) %>%
  rbind(CroppingAreas_ov3) -> CroppingAreas_ov

FoodProductionContents_ov1 %>%
  rbind(FoodProductionContents_ov2) %>%
  rbind(FoodProductionContents_ov3) -> FoodProductionContents_ov


# Prepare and extract data ------------------------------------------------

#join crop rotation category to cropping areas
CroppingAreas %>%
  left_join(CropRotCat, by = c("Activities" = "Activity")) %>%
  mutate(CropRotGroup = ifelse(Activities == "Cereals (Rice Milled Eqv)", "Cereals2_V", CropRotGroup )) %>%
  mutate(CropRotGroup = ifelse(Activities == "Maize, green", "Cereals2_V", CropRotGroup )) %>%
  mutate(Regions = as.factor(Regions), Activities = as.factor(Activities), 
         ProductionSystems = as.factor(ProductionSystems), 
         ProductionConditions = as.factor(ProductionConditions),
         Scenarios = as.factor(Scenarios), CropRotGroup = as.factor(CropRotGroup))  -> CroppingAreas_RotCat

CroppingAreas_ov %>%
  left_join(CropRotCat, by = c("Activities" = "Activity")) %>%
  mutate(CropRotGroup = ifelse(Activities == "Cereals (Rice Milled Eqv)", "Cereals2_V", CropRotGroup )) %>%
  mutate(CropRotGroup = ifelse(Activities == "Maize, green", "Cereals2_V", CropRotGroup ))%>%
  mutate(Regions = as.factor(Regions), Activities = as.factor(Activities), 
         ProductionSystems = as.factor(ProductionSystems), 
         ProductionConditions = as.factor(ProductionConditions),
         Scenarios = as.factor(Scenarios), CropRotGroup = as.factor(CropRotGroup))  -> CroppingAreas_RotCat_ov



#dataframe Food contents
Food_contents %>%
  mutate(Val = ifelse(Val == "Eps" , 0, Val)) %>%
  mutate(Regions = as.factor(Regions), Commodities = as.factor(Commodities), Contents = as.factor(Contents), ProductionSystems = as.factor(ProductionSystems), 
         ProductionConditions = as.factor(ProductionConditions), Scenarios = as.factor(Scenarios), Val = as.numeric(Val)) -> Food_contents

Food_contents_ov %>%
  mutate(Val = ifelse(Val == "Eps" , 0, Val)) %>%
  mutate(Regions = as.factor(Regions), Commodities = as.factor(Commodities), Contents = as.factor(Contents), ProductionSystems = as.factor(ProductionSystems), 
         ProductionConditions = as.factor(ProductionConditions), Scenarios = as.factor(Scenarios), Val = as.numeric(Val)) -> Food_contents_ov

#code below does only change labels?!!!! -> find better way....
# levels(Food_contents$Scenarios) <- c("Baseline","BaselineDerived","FOFA_BAU_2050","FOFA_2050_VeganBAU_NoFreeAreaUse",
#                                "FOFA_2050_VeganBAU", "FOFA_2050_VeganOptimized_conv",
#                                "FOFA_2050_VeganBarbieri",
#                                "FOFA_2050_VeganSchmidt",
#                                "FOFA_2050_VeganOptimized")
# levels(Food_contents$Scenarios)


#dataframe Food Contents Production (not DAQ)
FoodProductionContents %>%
  rename(Regions = Dim1, Commodities = Dim2, Contents = Dim3, ProductionSystems = Dim4, ProductionConditions = Dim5, Scenarios = Dim6 ) %>%
  mutate(Regions = as.factor(Regions), Commodities = as.factor(Commodities), Contents = as.factor(Contents), ProductionSystems = as.factor(ProductionSystems),
         ProductionConditions = as.factor(ProductionConditions), 
         Scenarios = as.factor(Scenarios)) -> FoodProductionContents


FoodProductionContents_ov %>%
  rename(Regions = Dim1, Commodities = Dim2, Contents = Dim3, ProductionSystems = Dim4, ProductionConditions = Dim5, Scenarios = Dim6 ) %>%
  mutate(Regions = as.factor(Regions), Commodities = as.factor(Commodities), Contents = as.factor(Contents), ProductionSystems = as.factor(ProductionSystems),
         ProductionConditions = as.factor(ProductionConditions), 
         Scenarios = as.factor(Scenarios)) -> FoodProductionContents_ov


#dataframe allResults
allResults %>% #special handling of "cote d'ivoire"
  mutate(Regions = as.character(Regions)) %>%
  mutate(Regions = ifelse(Regions == "C\xf0te d'Ivoire", "Cote d'Ivoire", Regions))%>%
  mutate(Regions = as.factor(Regions), 
         Organic_Vegan_Results_Indicators = as.factor(Organic_Vegan_Results_Indicators), 
         Scenarios = as.factor(Scenarios)) -> allResults

allResults_ov %>% #special handling of "cote d'ivoire"
  mutate(Regions = as.character(Regions)) %>%
  mutate(Regions = ifelse(Regions == "C\xf0te d'Ivoire", "Cote d'Ivoire", Regions))%>%
  mutate(Regions = as.factor(Regions), 
         Organic_Vegan_Results_Indicators = as.factor(Organic_Vegan_Results_Indicators), 
         Scenarios = as.factor(Scenarios)) -> allResults_ov



#extract selections
###################
#Scenarios
allResults %>%
  select(Scenarios) %>%
  distinct() %>%
  filter(Scenarios != "Baseline")-> scenarios

scenarios <- unique(scenarios$Scenarios)

#nutrients
Food_contents %>% #get rid of contents which are not relevant for human nutrition 
  select(Contents) %>%
  distinct() %>%
  filter(!(Contents %in% c( "Calories - domestic prod. (kcal)", "Protein - domestic prod. (t)", 
                            "N (t)", "P2O5 (t)", "DM (t)", "FeedME (MJ)", 
                            "FeedGE (MJ)", "FeedXP (t)", "FeedME in DM (MJ)",
                            "FeedGE in DM (MJ)", "FeedXP in DM (t)", "Milk solid contents (t)"))) -> nutrients 

#environmental indicators
allResults %>%
  select(Organic_Vegan_Results_Indicators) %>%
  distinct() -> indicators

#not necessary - delete later
nutrients2 <- Nut_ha_agg %>%
  select(Contents) %>%
  distinct() 

#countries
allResults %>%
  select(Regions) %>%
  distinct() %>%
  filter(Regions != "World") -> countries

countries <- countries$Regions

#crop rotation categories
cropRotationCategories <- unique(CropRotCat$CropRotGroup)

allResults %>%
  select(Organic_Vegan_Results_Indicators) %>%
  filter(grepl("GHG",Organic_Vegan_Results_Indicators)) %>%
  distinct() -> GHG_indicators 

GHG_indicators <- GHG_indicators$Organic_Vegan_Results_Indicators
water_indicators <- c("Irrigation water (m3) - water stress adjusted", "Irrigation water (m3)")
####

Population %>% #extract values population size 2050 and 2012
  group_by(PopulationGroups, Scenarios) %>%
  summarise(sum = sum(Level)) %>%
  filter(Scenarios %in% c("Baseline", "BaselineDerived", "FOFA_BAU_2050")) %>%
  filter(PopulationGroups == "PopulationAll")-> Population_summary


Food_contents %>% #extract total food content (all countries) and add Population Nr -> nutrient amount per day
  filter(Regions != "World") %>% 
  group_by(Commodities, Contents, Scenarios, ProductionSystems) %>%
  summarise(sum = sum(Val)) %>% #sum over all countries -> global data
  mutate(PopulationNr = ifelse(Scenarios %in% c("Baseline", "BaselineDerived"), Population_summary$sum[1], Population_summary$sum[3])) %>%
  mutate(Sum_per_capita_day = sum/PopulationNr/365) -> Food_contents_summary

Food_contents_summary %>% #add tresholds for each nutrient
  left_join(tresholds, by = c("Contents" = "Name_GAMS")) %>%
  mutate(treshold_low_adapted = ifelse(Contents %in% c("Protein (t)", "Fat (t)"), 
                                       treshold_low/1000/1000, 
                                       treshold_low)) %>%
  mutate(treshold_upper_adapted = ifelse(Contents %in% c("Protein (t)", "Fat (t)"), 
                                         treshold_upper/1000/1000, 
                                         treshold_upper)) -> Food_contents_summary_tresholds



# Testing -----------------------------------------------------------------

######tests - delete later

Food_contents_summary %>%
  filter(Contents == "Calories (kcal)",
         Commodities %in% c("All animal based Commodities", "All crop based Commodities", "All perm Crop Commodities")) %>%
  group_by(Contents, Scenarios,ProductionSystems) %>%
  summarise(AnimalProd = sum[Commodities == "All animal based Commodities"], 
            CropProd = sum[Commodities == "All crop based Commodities"],
            PermProd = sum[Commodities == "All perm Crop Commodities"]) %>%
  mutate(CropWoPerm = CropProd - PermProd) %>% 
  mutate(Pop = ifelse(Scenarios %in% c("BaselineDerived", "Baseline"), Population_summary$sum[Population_summary$Scenarios == "BaselineDerived"], 
                      Population_summary$sum[Population_summary$Scenarios == "FOFA_BAU_2050"])) %>%
  mutate(AnimalProd_PP = AnimalProd / Pop / 365, CropProd_PP = CropWoPerm / Pop / 365, PermProd_PP = PermProd / 365/Pop) %>%
  select(1:3,9:11) %>%
  pivot_longer(4:6, names_to = "Organic_Vegan_Results_Indicators", values_to = "sum_per_capita_day") %>%
  ggplot(aes(x = Scenarios, y = sum_per_capita_day, fill = Organic_Vegan_Results_Indicators)) +
    geom_bar(stat = "identity", position = "stack") +
    ggtitle("kcal per capita and day")+
    theme(axis.text.x = element_text(angle = 45, hjust=1)) 


Food_contents %>%
  filter(
    Regions != "World",
    Contents == "Calories (kcal)",
         Commodities %in% c("All animal based Commodities", "All crop based Commodities", "All perm Crop Commodities")) %>%
  group_by(Commodities, Contents, Scenarios,ProductionSystems, ProductionConditions) %>%
  summarise(sum_global = sum(Val)) %>%
  mutate(Pop  = ifelse(Scenarios %in% c("BaselineDerived", "Baseline"), Population_summary$sum[Population_summary$Scenarios == "BaselineDerived"], 
                            Population_summary$sum[Population_summary$Scenarios == "FOFA_BAU_2050"])) %>%
  mutate(sum_global_PP = sum_global / Pop / 365) %>%
  group_by(Scenarios) %>%
  mutate(sum_global_AC = sum_global[Commodities == "All animal based Commodities"] + sum_global[Commodities == "All crop based Commodities"], sum_global_AC_PP = sum_global_AC / Pop /365 ) -> test
  
  


Food_contents %>%
  filter(
    Regions != "World",
    Contents == "Calories (kcal)",
    Commodities %in% c("All Commodities")) %>%
  group_by(Commodities, Contents, Scenarios,ProductionSystems, ProductionConditions) %>%
  summarise(sum_global = sum(Val)) %>%
  mutate(Pop  = ifelse(Scenarios %in% c("BaselineDerived", "Baseline"), Population_summary$sum[Population_summary$Scenarios == "BaselineDerived"], 
                       Population_summary$sum[Population_summary$Scenarios == "FOFA_BAU_2050"])) %>%
  mutate(sum_global_PP = sum_global / Pop / 365)

# 
# Food_contents_summary_tresholds  %>%
#   #Food_contents_summary %>%
#   filter(Contents == "Calcium, Ca (mg)") %>%
#   #filter(Scenarios != "Baseline") %>%
#   filter(Commodities %in% c("All animal based Commodities", "All crop based Commodities", "All perm Crop Commodities")) %>%
#   group_by(Contents, Scenarios, ProductionSystems, Sum_per_capita_day, treshold_low) %>%
#   summarise(ProdPerm = sum)
#   
#   
#   
#   ggplot(aes(x = Scenarios, y = Sum_per_capita_day, fill = Commodities)) +
#   theme_bw() +
#   geom_bar(stat = "identity", position = "stack") +
#   ggtitle("kcal per capita and day")+
#   theme(axis.text.x = element_text(angle = 45, hjust=1)) +
#  geom_hline(yintercept=20, linetype="dashed", color = "red")
#  geom_hline(yintercept=intercept1$treshold_low_adapted, linetype="dashed", color = "red") +
#  geom_hline(yintercept=intercept2$treshold_upper_adapted, linetype="dashed", color = "blue")


# 
# 
# allResults %>%
#   filter(Regions == "World") %>%
#   filter(Organic_Vegan_Results_Indicators %in% c("Total GHG emissions - animals (t CO2e)","Tot GHG em - crops/grass, with Defor/OrgSoils (t CO2e)", "Tot GHG em - crops/grass, no Defor/OrgSoils (t CO2e)")) %>%
#   mutate(Organic_Vegan_Results_Indicators = ifelse(Organic_Vegan_Results_Indicators == "Tot GHG em - crops/grass, with Defor/OrgSoils (t CO2e)", "TotalCropGHG", as.character(Organic_Vegan_Results_Indicators) )) %>%
#   mutate(Organic_Vegan_Results_Indicators = ifelse(Organic_Vegan_Results_Indicators == "Tot GHG em - crops/grass, no Defor/OrgSoils (t CO2e)", "CropGHG_noDef", as.character(Organic_Vegan_Results_Indicators) )) %>%
#   pivot_wider(names_from = Organic_Vegan_Results_Indicators, values_from = Val) %>%
#   mutate(Deforestation = TotalCropGHG - CropGHG_noDef) %>%
#   pivot_longer(cols = 3:6, names_to = "Organic_Vegan_Results_Indicators", values_to = "Val" ) %>%
#   filter(Organic_Vegan_Results_Indicators %in% c("Total GHG emissions - animals (t CO2e)", "CropGHG_noDef", "Deforestation")) %>%
#   ggplot(aes(fill = Organic_Vegan_Results_Indicators, x = Scenarios, y = Val)) +
#   theme_bw() +
#   geom_bar(position="stack", stat="identity") +
#   theme(axis.text.x = element_text(angle = 45, hjust=1))

# allResults %>%
#   filter(Regions == "World") %>%
#   filter(Organic_Vegan_Results_Indicators %in% c("OECD N balance: inputs (tN)", "OECD N balance: outputs (tN)", "OECD N balance: Inputs - outputs (tN)")) %>%
#   mutate(sum_2 = ifelse(Organic_Vegan_Results_Indicators == "OECD N balance: outputs (tN)", -Val, Val)) %>%
#   ggplot(aes(fill = Organic_Vegan_Results_Indicators, x = Scenarios, y = sum_2)) +
#   theme_bw() +
#   geom_bar(position="dodge", stat="identity")+
#   theme(axis.text.x = element_text(angle = 45, hjust=1))
# 
# Population %>%
#   filter(PopulationGroups == "PopulationAll") -> PopulationCountries2050
# 
# 
# Food_contents %>% 
#   left_join(PopulationCountries2050[,c(1,3, 4)], by = c("Regions" = "Regions", "Scenarios" = "Scenarios")) %>%
#   rename(Pop = Level) %>%
#   mutate(Val_per_capita_day = Val / Pop / 365) %>%
#   #Food_contents_summary %>%
#   filter(Contents == "Proteins (t)") %>%
#   #filter(Scenarios != "Baseline") %>%
#   filter(Regions %in% c("Afghanistan", "Switzerland")) %>%
#   filter(Commodities != "All Commodities") %>%
#   ggplot(aes(x = Scenarios, y = Val_per_capita_day, fill = Commodities)) +
#   facet_grid(~ Regions)+
#   theme_bw() +
#   geom_bar(stat = "identity", position = "stack") +
#   ggtitle(paste0("kcal", " per capita and day"))+
#   theme(axis.text.x = element_text(angle = 45, hjust=1))
# 
# 
# 
# allResults %>%
#   filter(Regions %in% c("Switzerland", "Brazil", "Germany")) %>%
#   filter(Organic_Vegan_Results_Indicators %in% c("Cropland: permanent (ha)", "Cropland: temporary (without temp. grassland) (ha)", "Grassland: temporary (ha)")) %>%
#   ggplot(aes(fill = Organic_Vegan_Results_Indicators, x = Scenarios, y = Val)) +
#   facet_grid(. ~ Regions ) +
#   theme_minimal() +
#   #geom_bar(position="stack", stat="identity")+
#   geom_col(width = 0.5, position = "stack")+
#   theme(axis.text.x = element_text(angle = 45, hjust=1)) +
#   scale_fill_manual(values=c('coral4','darkgoldenrod1','green'))
# 
# Population %>%
#   #      filter(Scenarios %in% input$selSC) %>%
#   filter(PopulationGroups == "PopulationAll") -> PopulationCountries2050
# 
# CroppingAreas_RotCat %>%
#   group_by(Regions, CropRotGroup, Scenarios) %>%
#   filter(ProductionSystems == "AllProdSyst") %>%
#   filter(Regions %in% c("Switzerland", "Brazil")) %>%
#   filter(!is.na(CropRotGroup)) %>%
#   summarise(sum = sum(Val)) %>%
#   ggplot(aes(x = Scenarios, y = sum, fill = CropRotGroup)) +
#   theme_bw() +
#   facet_grid(. ~ Regions) +
#   geom_bar(stat = "identity", position = "stack") 
# 
# CroppingAreas_RotCat %>%
#   group_by(Regions, CropRotGroup, Scenarios) %>%
#   filter(ProductionSystems == "AllProdSyst") %>%
#   filter(Regions %in% c("Switzerland", "Brazil")) %>%
#   filter(!is.na(CropRotGroup)) %>%
#   summarise(sum = sum(Val)) %>%
#   group_by(Regions, Scenarios) %>%
#   mutate(perc = sum / sum(sum)) %>%
#   ggplot(aes(x = Scenarios, y = perc, fill = CropRotGroup, group=perc)) +
#   theme_bw() +
#   scale_y_continuous(labels = scales::percent) + 
#   facet_grid(. ~ Regions) +
#   geom_bar(stat = "identity", position = "stack")
# 
# CroppingAreas_RotCat %>%
#   filter(ProductionSystems == "AllProdSyst") %>%
#   filter(Scenarios == "BaselineDerived") %>%
#   filter(Regions %in% c("Switzerland", "Brazil")) %>%
#   filter(CropRotGroup == "Cereals1_V") %>%
#   group_by(Regions) %>%
#   mutate(perc = Val/(sum(Val))) %>%
#   ggplot(aes(x = Scenarios, y = perc, fill = Activities)) +
#   theme_bw() +
#   facet_grid(. ~ Regions) +
#   geom_bar(stat = "identity", position = "stack") +
#   scale_y_continuous(labels = scales::percent)  +
#   theme(axis.text.x = element_text(angle = 45, hjust=1)) +
#   ggtitle("Crops in Crop Rotation Category") +
#   scale_fill_brewer(palette = "Set2")
# 
# 
# 
# Population %>%
#   #      filter(Scenarios %in% input$selSC) %>%
#   filter(PopulationGroups == "PopulationAll") -> PopulationCountries2050
# 
# 
# Food_contents_summary_tresholds %>%
#   #Food_contents_summary %>%
#   filter(Contents == "Calories (kcal)") %>%
#   ungroup %>%
#   select(treshold_low_adapted) %>%
#   distinct()-> intercept1 #treshold value low
# 
# Food_contents_summary_tresholds %>%
#   #Food_contents_summary %>%
#   ungroup() %>%
#   filter(Contents == "Calories (kcal)") %>%
#   select(treshold_upper_adapted) %>%
#   distinct() -> intercept2 #treshold value high
# 
# Food_contents_summary_tresholds[, c(2, 11:12)] %>%
#   distinct() -> contents_tresholds
# 
# FoodProductionContents %>%
#   left_join(contents_tresholds, by = c("Contents" = "Contents")) -> Food_contents_summary_tresholds_regions
# 
# Food_contents_summary_tresholds_regions  %>% 
#   left_join(PopulationCountries2050[,c(1,3, 4)], by = c("Regions" = "Regions", "Scenarios" = "Scenarios")) %>%
#   rename(Pop = Level) %>%
#   group_by(Regions) %>%
#   mutate(Pop = ifelse(Scenarios %in% c("Baseline", "BaselineDerived", "FOFA_BAU_2050"), Pop, Pop[Scenarios == "FOFA_BAU_2050"])) %>%
#   mutate(Val_per_capita_day = Val / Pop / 365) %>%
#   #Food_contents_summary %>%
#   filter(Contents == "Calcium, Ca (mg)") %>%
#   #filter(Contents %in% c("Calories (kcal)", "Protein (t)", "Fat (t)")) %>%
#   #filter(Commodities != "All Commodities") %>%
#   filter(Commodities %in% c("All animal based Commodities", "All crop based Commodities", "All perm Crop Commodities")) %>%
#   #filter(Scenarios != "Baseline") %>%
#  # filter(Scenarios %in% input$selSC) %>%
#   filter(Regions %in% c("Switzerland", "Brazil")) %>%
#   ggplot(aes(x = Scenarios, y = Val_per_capita_day, fill = Commodities)) +
#   theme_bw() +
#   scale_fill_brewer(palette = "Set2") +
#   facet_grid(. ~ Regions  ) +
#   geom_bar(stat = "identity", position = "stack") +
# #  geom_hline(yintercept=intercept1$treshold_low_adapted, linetype="dashed", color = "red") +
# #  geom_hline(yintercept=intercept2$treshold_upper_adapted, linetype="dashed", color = "blue") +
#   ggtitle(paste0("Calcium, Ca (mg)", " per capita and day"))+
#   theme(axis.text.x = element_text(angle = 45, hjust=1))
# 
# 
# FoodProductionContents %>%
#   left_join(PopulationCountries2050[,c(1,3, 4)], by = c("Regions" = "Regions", "Scenarios" = "Scenarios")) %>%
#   rename(Pop = Level) %>%
#   group_by(Regions) %>%
#   mutate(Pop = ifelse(Scenarios %in% c("Baseline", "BaselineDerived", "FOFA_BAU_2050"), Pop, Pop[Scenarios == "FOFA_BAU_2050"])) %>%
#   mutate(Val_per_capita_day = Val / Pop / 365)
# 
# 
# Population %>%
#   #      filter(Scenarios %in% input$selSC) %>%
#   filter(PopulationGroups == "PopulationAll") -> PopulationCountries2050
# 
# Food_contents_summary_tresholds %>%
#   #Food_contents_summary %>%
#   filter(Contents == "Calories (kcal)") %>%
#   ungroup %>%
#   select(treshold_low_adapted) %>%
#   distinct()-> intercept1 #treshold value low
# 
# Food_contents_summary_tresholds %>%
#   #Food_contents_summary %>%
#   ungroup() %>%
#   filter(Contents == "Calories (kcal)") %>%
#   select(treshold_upper_adapted) %>%
#   distinct() -> intercept2 #treshold value high
# 
# Food_contents_summary_tresholds[, c(2, 11:12)] %>%
#   distinct() -> contents_tresholds
# 
# FoodProductionContents %>%
#   left_join(contents_tresholds, by = c("Contents" = "Contents")) -> Food_contents_summary_tresholds_regions
# 
# 
# Food_contents_summary_tresholds_regions %>% 
#   left_join(PopulationCountries2050[,c(1,3, 4)], by = c("Regions" = "Regions", "Scenarios" = "Scenarios")) %>%
#   rename(Pop = Level) %>%
#   group_by(Regions) %>%
#   #!!!!!!!!!!!!Change this later (adapted now in SOLm)!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#   mutate(Pop = 9218982000) %>%
#   #mutate(Pop = ifelse(Scenarios %in% c("Baseline", "BaselineDerived", "FOFA_BAU_2050"), Pop, Pop[Scenarios == "FOFA_BAU_2050"])) %>%
#   mutate(Val_per_capita_day = Val / Pop / 365) %>%
#   #Food_contents_summary %>%
#   filter(Contents == "Calcium, Ca (mg)") %>%
#   #filter(Contents %in% c("Calories (kcal)", "Protein (t)", "Fat (t)")) %>%
#   #filter(Commodities != "All Commodities") %>%
#   filter(Commodities %in% c("All animal based Commodities", "All crop based Commodities", "All perm Crop Commodities")) %>%
#   #filter(Scenarios != "Baseline") %>%
# #  filter(Scenarios %in% input$selSC) %>%
#   filter(Regions %in% c("Switzerland", "Brazil")) %>%
#   ggplot(aes(x = Scenarios, y = Val_per_capita_day, fill = Commodities)) +
#   theme_bw() +
#   scale_fill_brewer(palette = "Set2") +
#   facet_grid(. ~ Regions  ) +
#   geom_bar(stat = "identity", position = "stack") +
# #  geom_hline(yintercept=intercept1$treshold_low_adapted, linetype="dashed", color = "red") +
# #  geom_hline(yintercept=intercept2$treshold_upper_adapted, linetype="dashed", color = "blue") +
#   ggtitle(paste0("Calcium, Ca (mg)", " per capita and day"))+
#   theme(axis.text.x = element_text(angle = 45, hjust=1))
# 


# Calculations o vs. v ----------------------------------------------------

#First, extract values of o and v

allResults_ov %>%
  mutate(o = ifelse(Scenarios %in% c("Baseline", "BaselineDerived", "FOFA_BAU_2050", "FOFA_2050_VeganBarbieri_0_0", "FOFA_2050_VeganBarbieri_0_20",
                                     "FOFA_2050_VeganBarbieri_0_40", "FOFA_2050_VeganBarbieri_0_60",
                                     "FOFA_2050_VeganBarbieri_0_80", "FOFA_2050_VeganBarbieri_0_100"), 0, 
                    ifelse(Scenarios %in% c("FOFA_2050_VeganBarbieri_100_0", "FOFA_2050_VeganBarbieri_100_20",
                                            "FOFA_2050_VeganBarbieri_100_40", 
                                            "FOFA_2050_VeganBarbieri_100_60", 
                                            "FOFA_2050_VeganBarbieri_100_80",
                                            "FOFA_2050_VeganBarbieri_100_100"), 100, 1))) %>%
  mutate(o = ifelse( o == 1, substring(as.character(Scenarios), 25,26), o)) %>%
  mutate(v = ifelse(Scenarios %in% c("Baseline", "BaselineDerived", "FOFA_BAU_2050"), 0,
                    ifelse(Scenarios %in% c("FOFA_2050_VeganBarbieri_0_100", "FOFA_2050_VeganBarbieri_25_100", 
                                            "FOFA_2050_VeganBarbieri_50_100", "FOFA_2050_VeganBarbieri_75_100",
                                            "FOFA_2050_VeganBarbieri_100_100"), 100, 
                    ifelse(o == 0,
                     substring(as.character(Scenarios), 27,28),
                     ifelse(o == "100", substring(as.character(Scenarios),29,30),
                     substring(as.character(Scenarios), 28,29)))))) %>%
  mutate( o = as.numeric(o)) %>%
  mutate( v = as.numeric(v)) -> allResults_ov_index

#extract indices
allResults_ov_index %>%
  select(3,5:6) %>%
  distinct() -> ov_index

#Food contents
Food_contents_ov %>%
  left_join(ov_index, by= c("Scenarios" = "Scenarios")) -> Food_contents_ov_index
  


#1) GHG - all activities - data must be wrong...
# allResults_ov_index %>%
#   filter(!(Scenarios %in% c("Baseline", "BaselineDerived", "FOFA_BAU_2050")), Regions != "World") %>%
#   filter(Organic_Vegan_Results_Indicators == "Tot GHG em - all act, with Defor/OrgSoils (t CO2e)") %>%
#   group_by(Regions) %>%
#   mutate(Change_perc = (Val / Val[Scenarios == "FOFA_2050_VeganBarbieri_0_0"] - 1)*100) -> GHG_ov_countries
# 

allResults_ov_index %>%
  filter(!(Scenarios %in% c("Baseline", "BaselineDerived", "FOFA_BAU_2050")), 
         Regions == "World",
         Organic_Vegan_Results_Indicators == "Tot GHG em - all act, with Defor/OrgSoils (t CO2e)") %>%
  mutate(Change_perc = (Val / Val[Scenarios == "FOFA_2050_VeganBarbieri_0_0"] - 1)*100) -> GHG_ov_global
  


# GHG_ov_countries %>%
#   group_by(Scenarios, o, v) %>%
#   summarise(sum_global = sum(Val)) %>%
#   ungroup() %>%
#   mutate(Change_perc = (sum_global / sum_global[Scenarios == "FOFA_2050_VeganBarbieri_0_0"] - 1)*100) -> GHG_ov_global
#   

pGHG <- ggplot(GHG_ov_global, aes(as.factor(o), as.factor(v), group=as.factor(v))) +
  geom_tile(aes(fill = Change_perc)) +
  geom_text(aes(label = round(Change_perc, 1))) +
  scale_fill_gradient2(low = "aquamarine4", midpoint = 0, high = "red1") +
  ggtitle("Relative Change in GHG emissions (global)") +
  xlab("Organic Share") +
  ylab("Vegan Share") +
  guides(fill = guide_legend(title = "Change in %"))
pGHG

ggsave("GHG_rel_ov.png", plot = pGHG, width = 5, height = 4, units = "in")



#2) Water Use
allResults_ov_index %>%
  filter(!(Scenarios %in% c("Baseline", "BaselineDerived", "FOFA_BAU_2050")), Regions != "World") %>%
  filter(Organic_Vegan_Results_Indicators == "Irrigation water (m3) - water stress adjusted") %>%
  group_by(Regions) %>%
  mutate(Change_perc = (Val / Val[Scenarios == "FOFA_2050_VeganBarbieri_0_0"] - 1)*100) -> water_ov_countries

water_ov_countries %>%
  group_by(Scenarios, o, v) %>%
  summarise(sum_global = sum(Val)) %>%
  ungroup() %>%
  mutate(Change_perc = (sum_global / sum_global[Scenarios == "FOFA_2050_VeganBarbieri_0_0"] - 1)*100) -> water_ov_global

pwater <- ggplot(water_ov_global, aes(as.factor(o), as.factor(v), group=as.factor(v))) +
  geom_tile(aes(fill = Change_perc)) +
  geom_text(aes(label = round(Change_perc, 1))) +
  scale_fill_gradient2(low = "aquamarine4", midpoint = 0, high = "red1") +
  ggtitle("Relative Change in irrigation water use (water stress adjusted, global)") +
  xlab("Organic Share") +
  ylab("Vegan Share") +
  guides(fill = guide_legend(title = "Change in %"))
pwater

ggsave("water_rel_ov.png", plot = pGHG, width = 5, height = 4, units = "in")

#nutrients, kcal
Food_contents_ov_index %>%
  #filter(!(Scenarios %in% c("Baseline", "BaselineDerived", "FOFA_BAU_2050")), Regions != "World") %>%
  filter(!(Scenarios %in% c("Baseline", "BaselineDerived", "FOFA_BAU_2050")), Regions == "World") %>%
  filter(Contents == "Calories (kcal)", ProductionSystems == "AllProdSyst", Commodities == "All Commodities") %>%
  mutate(Change_perc = (Val / Val[Scenarios == "FOFA_2050_VeganBarbieri_0_0"] - 1)*100) -> kcal_ov_global
  # ungroup() %>%
  # group_by(Regions) %>%
  # mutate(Change_perc = (Val / Val[Scenarios == "FOFA_2050_VeganBarbieri_0_0"] - 1)*100) -> kcal_ov_countries

# kcal_ov_countries %>%
#   group_by(Scenarios, o, v) %>%
#   summarise(sum_global = sum(Val)) %>%
#   ungroup() %>%
#   mutate(Change_perc = (sum_global / sum_global[Scenarios == "FOFA_2050_VeganBarbieri_0_0"] - 1)*100) -> kcal_ov_global

pkcal <- ggplot(kcal_ov_global, aes(as.factor(o), as.factor(v), group=as.factor(v))) +
  geom_tile(aes(fill = Change_perc)) +
  geom_text(aes(label = round(Change_perc, 1))) +
  scale_fill_gradient2(low = "red1", midpoint = 0, high = "aquamarine4") +
  ggtitle("Relative change in kcal production, global") +
  xlab("Organic Share") +
  ylab("Vegan Share") +
  guides(fill = guide_legend(title = "Change in %"))
pkcal

ggsave("kcal_rel_ov.png", plot = pkcal, width = 5, height = 4, units = "in")

#kcal - tresholds

kcal_ov_global %>% 
  mutate(minReq = tresholds$treshold_low[tresholds$Name_GAMS == "Calories (kcal)"] *365* Population_summary$sum[Population_summary$Scenarios == "FOFA_BAU_2050"]) %>%
  mutate(prodReq = (Val / minReq -1)*100) -> kcal_ov_global_Req

pkcal_Req <- ggplot(kcal_ov_global_Req, aes(as.factor(o), as.factor(v), group=as.factor(v))) +
  geom_tile(aes(fill = prodReq)) +
  geom_text(aes(label = round(prodReq, 1))) +
  scale_fill_gradient2(low = "red1", midpoint = 0, high = "aquamarine4") +
  ggtitle("Potential surplus production of kcal, global") +
  xlab("Organic Share") +
  ylab("Vegan Share") +
  guides(fill = guide_legend(title = "Change in %"))
pkcal_Req

ggsave("kcal_rel_ov.png", plot = pkcal_Req, width = 5, height = 4, units = "in")


#3) protein production
Food_contents_ov_index %>%
  filter(!(Scenarios %in% c("Baseline", "BaselineDerived", "FOFA_BAU_2050")), Regions == "World") %>%
  filter(Contents == "Protein (t)", ProductionSystems == "AllProdSyst", Commodities == "All Commodities") %>%
  mutate(Change_perc = (Val / Val[Scenarios == "FOFA_2050_VeganBarbieri_0_0"] - 1)*100)  -> protein_ov_global


# protein_ov_countries %>%
#   group_by(Scenarios, o, v) %>%
#   summarise(sum_global = sum(Val)) %>%
#   ungroup() %>%
#   mutate(Change_perc = (sum_global / sum_global[Scenarios == "FOFA_2050_VeganBarbieri_0_0"] - 1)*100) -> protein_ov_global

pprotein <- ggplot(protein_ov_global, aes(as.factor(o), as.factor(v), group=as.factor(v))) +
  geom_tile(aes(fill = Change_perc)) +
  geom_text(aes(label = round(Change_perc, 1))) +
  scale_fill_gradient2(low = "red1", midpoint = 0, high = "aquamarine4") +
  ggtitle("Relative change in protein production, global") +
  xlab("Organic Share") +
  ylab("Vegan Share") +
  guides(fill = guide_legend(title = "Change in %"))
pprotein

ggsave("protein_rel_ov.png", plot = pprotein, width = 5, height = 4, units = "in")

#proteins - tresholds
protein_ov_global %>%
  mutate(minReq = tresholds$treshold_low[tresholds$Name_GAMS == "Protein (t)"] /1000/1000* 365* Population_summary$sum[Population_summary$Scenarios == "FOFA_BAU_2050"]) %>%
  mutate(prodReq = (Val / minReq -1)*100) -> protein_ov_global_Req

pprotein_Req <- ggplot(protein_ov_global_Req, aes(as.factor(o), as.factor(v), group=as.factor(v))) +
  geom_tile(aes(fill = prodReq)) +
  geom_text(aes(label = round(prodReq, 1))) +
  scale_fill_gradient2(low = "red1", midpoint = 0, high = "aquamarine4") +
  ggtitle("Potential surplus production of proteins, global") +
  xlab("Organic Share") +
  ylab("Vegan Share") +
  guides(fill = guide_legend(title = "Change in %"))
pprotein_Req

ggsave("protein_rel_ov.png", plot = pprotein, width = 5, height = 4, units = "in")





#Fat
Food_contents_ov_index %>%
  filter(!(Scenarios %in% c("Baseline", "BaselineDerived", "FOFA_BAU_2050")), Regions != "World") %>%
  filter(Contents == "Fat (t)", ProductionSystems %in% c("Convent", "Organic")) %>%
  filter(Commodities == "All Commodities") %>%
  group_by(Commodities, Contents, ProductionSystems, ProductionConditions, Scenarios, o, v) %>%
  summarise(sum_global = sum(Val)) %>%
  ungroup() %>%
  group_by(Commodities, Contents, ProductionConditions, Scenarios, o,v) %>%
  summarise(sum_OrgConv_global = sum(sum_global)) %>%
  ungroup() %>%
  mutate(Change_perc = (sum_OrgConv_global / sum_OrgConv_global[Scenarios == "FOFA_2050_VeganBarbieri_0_0"] - 1)*100)-> fat_ov_global


pfat <- ggplot(fat_ov_global, aes(as.factor(o), as.factor(v), group=as.factor(v))) +
  geom_tile(aes(fill = Change_perc)) +
  geom_text(aes(label = round(Change_perc, 1))) +
  scale_fill_gradient2(low = "red1", midpoint = 0, high = "aquamarine4") +
  ggtitle("Relative change in fat production, global") +
  xlab("Organic Share") +
  ylab("Vegan Share") +
  guides(fill = guide_legend(title = "Change in %"))
pfat

ggsave("fat_rel_ov.png", plot = pfat, width = 5, height = 4, units = "in")


#fat tresholds
fat_ov_global %>%
  mutate(minReq = tresholds$treshold_low[tresholds$Name_GAMS == "Fat (t)"] /1000/1000* 365* Population_summary$sum[Population_summary$Scenarios == "FOFA_BAU_2050"]) %>%
  mutate(prodReq = (sum_OrgConv_global / minReq -1)*100) -> fat_ov_global_Req

pfat_Req <- ggplot(fat_ov_global_Req, aes(as.factor(o), as.factor(v), group=as.factor(v))) +
  geom_tile(aes(fill = prodReq)) +
  geom_text(aes(label = round(prodReq, 1))) +
  scale_fill_gradient2(low = "red1", midpoint = 0, high = "aquamarine4") +
  ggtitle("Potential surplus production of fat, global") +
  xlab("Organic Share") +
  ylab("Vegan Share") +
  guides(fill = guide_legend(title = "Change in %"))
pfat_Req

ggsave("fat_rel_ov.png", plot = pfat_Req, width = 5, height = 4, units = "in")



#Selenium - continue later...
Food_contents_ov_index %>%
  filter(!(Scenarios %in% c("Baseline", "BaselineDerived", "FOFA_BAU_2050")), Regions != "World") %>%
  filter(Contents == "Selenium, Se (ug)", ProductionSystems %in% c("Convent", "Organic")) %>%
  filter(Commodities == "All Commodities") %>%
  group_by(Regions, Scenarios) %>%
  summarise(ValOrgCon = sum(c(Val[ProductionSystems == "Convent"], Val[ProductionSystems == "Organic"]), na.rm = TRUE), o = o, v = v) %>%
  ungroup() %>%
  group_by(Regions) %>%
  mutate(Change_perc = (ValOrgCon / ValOrgCon[Scenarios == "FOFA_2050_VeganBarbieri_0_0"] - 1)*100) -> selenium_ov_countries

selenium_ov_countries %>%
  group_by(Scenarios, o, v) %>%
  summarise(sum_global = sum(ValOrgCon, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Change_perc = (sum_global / sum_global[Scenarios == "FOFA_2050_VeganBarbieri_0_0"] - 1)*100) -> selenium_ov_global


pselenium <- ggplot(selenium_ov_global, aes(as.factor(o), as.factor(v), group=as.factor(v))) +
  geom_tile(aes(fill = Change_perc)) +
  geom_text(aes(label = round(Change_perc, 1))) +
  scale_fill_gradient2(low = "red1", midpoint = 0, high = "aquamarine4") +
  ggtitle("Relative change in selenium production, global") +
  xlab("Organic Share") +
  ylab("Vegan Share") +
  guides(fill = guide_legend(title = "Change in %"))
pselenium

ggsave("selenium_rel_ov.png", plot = pselenium, width = 5, height = 4, units = "in")



# Bug search 03.08.2021 ---------------------------------------------------

Population %>%
  filter(Scenarios %in% c("BaselineDerived", "FOFA_BAU_2050"), PopulationGroups == "PopulationAll") %>%
  group_by(Scenarios) %>%
  summarise(sum = sum(Level)) %>% #sum all countries
  mutate( year = ifelse(Scenarios == "BaselineDerived", 2012, 2050)) -> Pop

#Testing sum of calories and calcium
Food_contents %>%
  filter(Contents == "Calories (kcal)", Commodities == "All Commodities", !(Regions == "World")) %>%
  group_by(Scenarios, ProductionSystems) %>%
  summarise(sum = sum(Val, na.rm = TRUE)) %>% #sum over all countries
  mutate(Pop = ifelse(Scenarios %in% c("BaselineDerived", "Baseline"), Pop$sum[Pop$year == 2012], Pop$sum[Pop$year == 2050])) %>%
  mutate(Val_pp = sum / Pop / 365) #calculate calories per day and person

#Compare to World

Food_contents %>%
  filter(Contents == "Calories (kcal)", Commodities == "All Commodities", Regions == "World", ProductionSystems == "AllProdSyst") %>%
  mutate(Pop = ifelse(Scenarios %in% c("BaselineDerived", "Baseline"), Pop$sum[Pop$year == 2012], Pop$sum[Pop$year == 2050])) %>%
  mutate(Val_pp = Val / Pop / 365) #calculate calories per day and person


Food_contents %>%
  filter(Contents == "Calcium, Ca (mg)", Commodities == "All Commodities", !(Regions == "World")) %>%
  group_by(Scenarios) %>%
  summarise(sum = sum(Val, na.rm = TRUE)) %>% #sum over all countries
  mutate(Pop = ifelse(Scenarios %in% c("BaselineDerived", "Baseline"), Pop$sum[Pop$year == 2012], Pop$sum[Pop$year == 2050])) %>%
  mutate(Val_pp = sum / Pop / 365) #calculate calcium per day and person

#Test sum of animal and crop-based

Food_contents %>%
  filter(Contents == "Calories (kcal)", Commodities %in% c("All animal based Commodities", "All crop based Commodities"), !(Regions == "World")) %>%
  #filter(Contents == "Calories (kcal)", Commodities %in% c("All Commodities"))%>%
  group_by(Regions, Scenarios, ProductionSystems)%>%
  summarise(AllCommod = sum(Val) ) %>% #sum over animal based and crop based commodities
  ungroup() %>%
  group_by(Scenarios, ProductionSystems) %>%
  summarise( global_Val = sum(AllCommod, na.rm = TRUE)) %>%
  mutate(Pop = ifelse(Scenarios %in% c("BaselineDerived", "Baseline"), Pop$sum[Pop$year == 2012], Pop$sum[Pop$year == 2050])) %>%
  mutate(Val_pp = global_Val / Pop / 365) #calculate calories per day and person


Food_contents %>%
  filter(Contents == "Calories (kcal)", Commodities %in% c("All animal based Commodities", "All crop based Commodities")) -> t1
  
t1 %>%
  select(1,2,3,6) %>%
  distinct() -> t2 # 12 entries missing, that means that there are some entries with multiple production systems...

unique(Food_contents$Regions)

#Start App below..
#--------------------------------------------------- ---------------------------------------------------
# 1 - Sidebar Defintion ---------------------------------------------------
#--------------------------------------------------- ---------------------------------------------------

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem("Summary", tabName = "dashboard1"),
    menuItem("Food Availability", tabName =  "dashboard2"),
    menuItem("Land Use", tabName =  "dashboard3"),
    menuItem("GHG", tabName =  "dashboard4"),
    menuItem("Water", tabName =  "dashboard5"),
    menuItem("N / P", tabName =  "dashboard6"),
    menuItem("Organic vs. Vegan", tabName =  "dashboard7"),
    menuItem("Sensitivity", tabName =  "dashboard8"),
    menuItem("Definitions", tabName =  "dashboard9"),
    checkboxGroupInput(inputId = "selSC",
                       label = "Scenario Selection",
                       choices = scenarios,
                       selected = c("BaselineDerived", "FOFA_BAU_2050")
    )
  )
)



# 2 - Body Definition -----------------------------------------------------


body <- dashboardBody(
  shinyDashboardThemes(
    theme = "poor_mans_flatly"
  ),
  #create tab structure
  tabItems(
    #first tab
    tabItem(
      tabName = "dashboard1",
      #Title
      ####################
      h1("Master Thesis Summary: Global Results"),
      h5("The figures presented in this dashboard were created in the scope of the master thesis
      'Modeling Environmental and Nutritional Impacts of Vegan Agriculture' (P. Krayer, 2020)"),
      h5("These results are based on model runs for different vegan and non-vegan scenarios
         conducted with the model SOLmV6 (Muller et al., 2017)."),
      
      #header 0: Settings
      ####################
      # h3("Settings"),
      # h6("Choice of Scenarios, what else?"),
      # fluidRow(
      #   box(
      #     title = "Scenarios",
      #     solidHeader = TRUE,
      #     status = "warning",
      #     checkboxGroupInput(inputId = "selSC",
      #                        label = "Choose scenario",
      #                        choices = scenarios,
      #                        #selected = list(scenarios)
      #                        
      #     )
      #     #          , actionButton("selectall", "Select All")
      #   )
      # ),
      
      
      #header 1: Food
      ####################
      fluidRow(
        box(width = 9,
          title = "Global Food Availability",
          solidHeader = TRUE,
          status = "primary",
          #test here - delete later
          plotOutput("plotFoodAvailability", height = 500),
          #plotOutput("plot1", height = 250),
          downloadButton("download11")
        ),
        box(width = 3,
          solidHeader = TRUE,
          status = "primary",
          title = "Nutrients",
          selectInput(inputId = "selFA",
                      label = "Choose indicator",
                      choices = nutrients,
                      selected = "Calories (kcal)"
                      
          )
        )
      ),
      
      #header 2: Land
      ####################
      fluidRow(
        box(width = 9,
          title = "Global Land Use",
          solidHeader = TRUE,
          status = "primary",
          plotOutput("plotLandUseGlobal", height = 500),
          downloadButton("download12")
        ),
        
        # #first row
        #   fluidRow(
        #     #first box
        #     box(plotOutput("plot1", height = 250)),
        #     #a new box
        #     box(
        #       title = "Controls",
        #       sliderInput("slider", "Number of observations:", 1, 100, 50)
        #     )
        #   ),
      ),
      
      #header 3: GHG
      ###################
      fluidRow(
        
        #first box
        box(width = 9,
          title = "Global GHG-Emissions",
          solidHeader = TRUE,
          status = "primary",
          plotOutput("plotGHG", height = 500),
          downloadButton("download13")
        )
        
        
      ),
      
      
      #header 4: Nutrients
      ###################
      fluidRow(
        
        #first box
        box(width = 9,
          title = "Nitrogen balance",
          solidHeader = TRUE,
          status = "primary",
          plotOutput("plotNutrients", height = 250),
          downloadButton("download14")
        ),
        #a new box
        box(width = 3,
          title = "Phosphorous balance",
          solidHeader = TRUE,
          status = "primary",
          plotOutput("plotNutrients2", height = 250),
          downloadButton("download15")
        )
      ),
      
      #header 5: Water Use
      ######################
      fluidRow(
        
        #first box
        box(width = 9,
          solidHeader = TRUE,
          status = "primary",
          plotOutput("plotWater", height = 500)),
        #a new box
        box(width = 3,
          solidHeader = TRUE,
          status = "primary",
          title = "Choice",
          selectInput(inputId = "water",
                      label = "Choose indicator",
                      choices = c("Irrigation water (m3) - water stress adjusted", "Irrigation water (m3)")
          )
        )
      )
      
      
      
    ), #end of tab
    #second tab
   


# Dashboard2 --------------------------------------------------------------

        #####################TAB 2
        tabItem(tabName = "dashboard2",
                h1("Food Availability"),
            
            fluidRow(
              column(width = 9,
              
              #first box
              
              # #a new box in coloumn
              # h3("Nutrient production per country"),
              # box(
              #   width=NULL,
              #   title = "Nutrient production per country",
              #   solidHeader = TRUE,
              #   status = "primary",
              #   plotOutput("plotNutrientCountry", height = 250)),
              # h3("Animal vs. plant-based production"),
              # box(
              #   width=NULL,
              #   title = "Select: Animal- vs. Plant-Production per country",
              #   solidHeader = TRUE,
              #   status = "info",
              #   selectInput(inputId = "selNutrientsAnimalPlant",
              #             label = "Choose nutrients",
              #             choices = nutrients
              #             #choices = c("Calories (kcal)", "Protein (t)", "Fat (t)")
              #   )
              # ),
              
              box(
                width=NULL,
                title = "Nutrient availability per country (DAQ)",
                solidHeader = TRUE,
                status = "primary",
                plotOutput("plotNutrientCountry2", height = 500)

              ),
              
              box(
                width=NULL,
                title = "Nutrient production per country",
                solidHeader = TRUE,
                status = "primary",
                plotOutput("plotNutrientCountry3", height = 250)
                
              ),
              
              box(
                width=NULL,
                title = "Nutrient production per ha",
                solidHeader = TRUE,
                status = "primary",
                plotOutput("plotNutrientHa", height = 250)
              )

            ),
            #end coloumn 1
            #fluidRow(
            column(width = 3,
              #first box
              box(
                width=NULL,
                solidHeader = TRUE,
                status = "primary",
                title = "Nutrients",
                selectInput(inputId = "selNutrients2",
                            label = "Choose nutrients",
                            choices = nutrients,
                            selected = "Calories (kcal)"
                )
              ),
              #a new box
            #column(width = 4,  
            box(
                width=NULL,
                solidHeader = TRUE,
                status = "primary",
                title = "Countries",
                # selectInput(inputId = "selNutrients3",
                #             label = "Choose nutrients",
                #             choices = nutrients),
                checkboxGroupInput(inputId = "selCountries2",
                                   label = "Choose countries",
                                   choices = countries,
                                   selected = "Switzerland") 
                
                
                
              )
            )
            )
            
            
            
            
            
    ),


# Dashboard3 --------------------------------------------------------------


    #####################TAB 3
    tabItem(tabName = "dashboard3",
            h1("Land Use"),
            
             
            fluidRow(
              
              column(width = 9,
                     #first box
                     box(width = NULL,
                         solidHeader = TRUE,
                         status = "primary",
                         title = "Cropping Areas",
                         plotOutput("plotCropCountry", height = 250)
                     ),
                     
                    #first box
                    box(width = NULL,
                      solidHeader = TRUE,
                      status = "primary",
                      title = "Cropping Patterns",
                      plotOutput("plotCropRot", height = 500)
                      ),

                    box(width = NULL,
                        solidHeader = TRUE,
                        status = "primary",
                        title = "Crops within Crop Rotation Category",
                        selectInput(inputId = "selCropRotGroup",
                                    label = "Crop Rotation Category",
                                    choices = cropRotationCategories,
                                    selected = "Cereals1_V"
                        ),
                        
                        plotOutput("plotCropInCropRot", height = 250)
                    )                   
                    
                    
              
              )
              ,
              column(width = 3,
              #a new box
              box(width = NULL,
                solidHeader = TRUE,
                status = "primary",
                title = "Countries",
                checkboxGroupInput(inputId = "selCountries",
                                   label = "Choose country",
                                   choices = countries, 
                                   selected = "Switzerland"
                )
                )
              )
            )
            
            
            
    ), 
    #####################TAB 4
    tabItem(tabName = "dashboard4",
            h1("GHG"),
            fluidRow(
              
              column(width = 9,
                     box(width = NULL,
                         solidHeader = TRUE,
                         status = "primary",
                         title = "GHG emissions per country",
                         plotOutput("plotGHGcountry", height = 250)
                     )                     
                     
                     
              ),
              
              column(width = 3,
                     box(width = NULL,
                         solidHeader = TRUE,
                         status = "primary",
                         title = "Countries",
                         checkboxGroupInput(inputId = "selCountriesGHG",
                                            label = "Choose country",
                                            choices = countries, 
                                            selected = "Switzerland"
                         )
                     )                    
                     
                     
              )
            )
            
            
    ),

# Dashboard5 - Water ------------------------------------------------------

    
#####################TAB 5
    tabItem(tabName = "dashboard5",
            h1("Water"),
            fluidRow(
              
              column(width = 9,
                     box(width = NULL,
                         solidHeader = TRUE,
                         status = "primary",
                         title = "Water use per country",
                         plotOutput("plotWaterCountry", height = 250)
                     )                     
                     
                     
              ),
              
              column(width = 3,
                     box(width = NULL,
                         solidHeader = TRUE,
                         status = "primary",
                         title = "Countries",
                         checkboxGroupInput(inputId = "selCountriesWater",
                                            label = "Choose country",
                                            choices = countries, 
                                            selected = "Switzerland"
                         )
                     )                    
                     
                     
              )
            )
            
    ),


# Dashboard6 - N ----------------------------------------------------------


    #####################TAB 6
    tabItem(tabName = "dashboard6",
            h1("Nutrient Fluxes"),
            fluidRow(
              
              column(width = 9,
                     box(width = NULL,
                         solidHeader = TRUE,
                         status = "primary",
                         title = "Nitrogen balance per country",
                         plotOutput("plotNcountry", height = 500)
                     )                     
                     
                     
              ),
              
              column(width = 3,
                     box(width = NULL,
                         solidHeader = TRUE,
                         status = "primary",
                         title = "Countries",
                         checkboxGroupInput(inputId = "selCountriesN",
                                            label = "Choose country",
                                            choices = countries, 
                                            selected = "Switzerland"
                         )
                     )                    
                     
                     
              )
            )
    ),


# Dashboard7 - org Veg ----------------------------------------------------


    #####################TAB 7
    tabItem(tabName = "dashboard7",
            h1("Compatibility of Organic and Vegan Agriculture"),
            h5("The following results are all based on Scenario XY."),
            h3("Nutritional Indicators", offset = 3),
            fluidRow(
              
              column(width = 4,
                     box(width = NULL,
                         solidHeader = TRUE,
                         status = "primary",
                         title = "Change in calorie production",
                         plotOutput("plot_ov_cal", height = 250)
                     ),
                     box(width = NULL,
                         solidHeader = TRUE,
                         status = "primary",
                         title = "Calorie production surplus",
                         plotOutput("plot_ov_cal_req", height = 250)
                     )


                     
                
              ),
              column(width = 4,

                     box(width = NULL,
                         solidHeader = TRUE,
                         status = "primary",
                         title = "Change in protein production",
                         plotOutput("plot_ov_prot", height = 250)
                     ),
                     
                            box(width = NULL,
                                solidHeader = TRUE,
                                status = "primary",
                                title = "Protein production surplus",
                                plotOutput("plot_ov_prot_req", height = 250)
                            )
                
                
              ),
              column(width = 4,
              box(width = NULL,
                  solidHeader = TRUE,
                  status = "primary",
                  title = "Change in fat production",
                  plotOutput("plot_ov_fat", height = 250)
              )       
              ,
              box(width = NULL,
                         solidHeader = TRUE,
                         status = "primary",
                         title = "Fat production surplus",
                         plotOutput("plot_ov_fat_req", height = 250)
                     )
                     
                )
              
            )
            ,
            h3("Environmental Indicators", offset = 3),
            fluidRow(
              box(width = 9,
                  solidHeader = TRUE,
                  status = "primary",
                  title = "Changes in GHG emissions",
                  plotOutput("plot_ov_GHG", height = 500)
              ),
              box(width = 3,
                solidHeader = TRUE,
                status = "primary",
                title = "Select Indicator",
                selectInput(inputId = "selGHG_ov",
                            label = "Select",
                            choices = GHG_indicators,
                            selected = "Tot GHG em - crops/grass, with Defor/OrgSoils (t CO2e)"

                )
              )

            )
            ,
            fluidRow(
              box(width = 9,
                  solidHeader = TRUE,
                  status = "primary",
                  title = "Changes in irrigation water use",
                  plotOutput("plot_ov_water", height = 500)
              ),
              box(width = 3,
                solidHeader = TRUE,
                status = "primary",
                title = "Select Indicator",
                selectInput(inputId = "selWater_ov",
                            label = "Select",
                            choices = water_indicators,
                            selected = "Irrigation water (m3) - water stress adjusted"
                )
              )

            )
            ),
    

# Dashboard8 - Sensitivity ------------------------------------------------


    #####################TAB 7
    tabItem(tabName = "dashboard8",
            h1("Sensitivity Analysis")
            
            
            ),
    
    
    #####################TAB 8
    tabItem(tabName = "dashboard9",
            h1("Definitions")
    )
    
    
    
    
  )

)




# 3 - Server Definition ---------------------------------------------------------------


server <- function(input, output) {

# Plots 1 -----------------------------------------------------------------


# Plot 1.1 ----------------------------------------------------------------

  
  # 1) plot nutrients
   output$plotFoodAvailability <- renderPlot({
    Food_contents_summary_tresholds %>%
      #Food_contents_summary %>%
      filter(Contents == input$selFA) %>%
      select(treshold_low_adapted) %>%
      unique() -> intercept1 #treshold value low

    Food_contents_summary_tresholds %>%
      #Food_contents_summary %>%
      filter(Contents == input$selFA) %>%
      select(treshold_upper_adapted) %>%
      unique() -> intercept2 #treshold value high
    
    
    Food_contents_summary %>%
      filter(Contents == input$selFA,
             Commodities %in% c("All animal based Commodities", "All crop based Commodities", "All perm Crop Commodities"),
              Scenarios %in% input$selSC) %>%
      group_by(Contents, Scenarios,ProductionSystems) %>%
      summarise(AnimalProd = sum[Commodities == "All animal based Commodities"], 
                CropProd = sum[Commodities == "All crop based Commodities"],
                PermProd = sum[Commodities == "All perm Crop Commodities"]) %>%
      mutate(CropWoPerm = CropProd - PermProd) %>% 
      mutate(Pop = ifelse(Scenarios %in% c("BaselineDerived", "Baseline"), Population_summary$sum[Population_summary$Scenarios == "BaselineDerived"], 
                          Population_summary$sum[Population_summary$Scenarios == "FOFA_BAU_2050"])) %>%
      mutate(AnimalProd_PP = AnimalProd / Pop / 365, CropProd_PP = CropWoPerm / Pop / 365, PermProd_PP = PermProd / 365/Pop) %>%
      select(1:3,9:11) %>%
      pivot_longer(4:6, names_to = "Commodities2", values_to = "Sum_per_capita_day") %>%
      mutate(cat = ifelse(Scenarios %in% c("Baseline", "BaselineDerived", "FOFA_BAU_2050"), "Reference",
                          ifelse(Scenarios %in% c("FOFA_2050_VeganBAU", "FOFA_2050_VeganBAU_NoFreeAreaUse", "FOFA_2050_VeganOptimized_conv"), "Vegan-Conventional",
                                 "Vegan-Organic"))) %>%
      ggplot(aes(x = Scenarios, y = Sum_per_capita_day, fill = Commodities2)) +
      theme_bw() +
      geom_bar(stat = "identity", position = "stack") +
      ggtitle(paste0(input$selFA, " per capita and day"))+
      facet_grid(. ~ cat, scales="free"  ) +
      theme(axis.text.x = element_text(angle = 45, hjust=1)) +
      #geom_hline(yintercept=20, linetype="dashed", color = "red")
      geom_hline(yintercept=intercept1$treshold_low_adapted, linetype="dashed", color = "red") +
      geom_hline(yintercept=intercept2$treshold_upper_adapted, linetype="dashed", color = "blue") +
      scale_fill_brewer(palette = "Set2")
    
    
# 
#     Food_contents_summary_tresholds %>%
#       #Food_contents_summary %>%
#       filter(Contents == input$selFA) %>%
#       #filter(Scenarios != "Baseline") %>%
#       filter(Scenarios %in% input$selSC) %>%
#       filter(Commodities %in% c("All animal based Commodities", "All crop based Commodities", "All perm Crop Commodities")) %>%
#       ggplot(aes(x = Scenarios, y = Sum_per_capita_day, fill = Commodities)) +
#       theme_bw() +
#       geom_bar(stat = "identity", position = "stack") +
#       ggtitle(paste0(input$selFA, " per capita and day"))+
#       theme(axis.text.x = element_text(angle = 45, hjust=1)) +
#       #geom_hline(yintercept=20, linetype="dashed", color = "red")
#       geom_hline(yintercept=intercept1$treshold_low_adapted, linetype="dashed", color = "red") +
#       geom_hline(yintercept=intercept2$treshold_upper_adapted, linetype="dashed", color = "blue") +
#       scale_fill_brewer(palette = "Set2")

  })

  #plot 2: Land use
  output$plotLandUseGlobal <- renderPlot({
    allResults %>%
      filter(Regions == "World") %>%
      filter(Scenarios %in% input$selSC) %>%
      filter(Organic_Vegan_Results_Indicators %in% c("Cropland: permanent (ha)", "Cropland: temporary (without temp. grassland) (ha)", "Grassland: temporary (ha)")) %>%
      mutate(cat = ifelse(Scenarios %in% c("Baseline", "BaselineDerived", "FOFA_BAU_2050"), "Reference",
                          ifelse(Scenarios %in% c("FOFA_2050_VeganBAU", "FOFA_2050_VeganBAU_NoFreeAreaUse", "FOFA_2050_VeganOptimized_conv"), "Vegan-Conventional",
                                 "Vegan-Organic"))) %>%
      ggplot(aes(fill = Organic_Vegan_Results_Indicators, x = Scenarios, y = Val)) +
      theme_bw() +
      facet_grid(. ~ cat, scales="free"  ) +
      geom_bar(position="stack", stat="identity")+
      theme(axis.text.x = element_text(angle = 45, hjust=1)) +
      scale_fill_manual(values=c('coral4','darkgoldenrod1','green'))
    


  })
  # 
  #plot 3: GHG
  output$plotGHG <- renderPlot({ 
    allResults %>%
      filter(Scenarios %in% input$selSC) %>%
      filter(Regions == "World") %>%
      filter(Organic_Vegan_Results_Indicators %in% c("Total GHG emissions - animals (t CO2e)","Tot GHG em - crops/grass, with Defor/OrgSoils (t CO2e)", "Tot GHG em - crops/grass, no Defor/OrgSoils (t CO2e)")) %>%
      mutate(Organic_Vegan_Results_Indicators = ifelse(Organic_Vegan_Results_Indicators == "Tot GHG em - crops/grass, with Defor/OrgSoils (t CO2e)", "TotalCropGHG", as.character(Organic_Vegan_Results_Indicators) )) %>%
      mutate(Organic_Vegan_Results_Indicators = ifelse(Organic_Vegan_Results_Indicators == "Tot GHG em - crops/grass, no Defor/OrgSoils (t CO2e)", "CropGHG_noDef", as.character(Organic_Vegan_Results_Indicators) )) %>%
      pivot_wider(names_from = Organic_Vegan_Results_Indicators, values_from = Val) %>%
      mutate(Deforestation = TotalCropGHG - CropGHG_noDef) %>%
      pivot_longer(cols = 3:6, names_to = "Organic_Vegan_Results_Indicators", values_to = "Val" ) %>%
      mutate(cat = ifelse(Scenarios %in% c("Baseline", "BaselineDerived", "FOFA_BAU_2050"), "Reference",
                          ifelse(Scenarios %in% c("FOFA_2050_VeganBAU", "FOFA_2050_VeganBAU_NoFreeAreaUse", "FOFA_2050_VeganOptimized_conv"), "Vegan-Conventional",
                                 "Vegan-Organic"))) %>%
      filter(Organic_Vegan_Results_Indicators %in% c("Total GHG emissions - animals (t CO2e)", "CropGHG_noDef", "Deforestation")) %>%
      ggplot(aes(fill = Organic_Vegan_Results_Indicators, x = Scenarios, y = Val)) +
      theme_bw() +
      geom_bar(position="stack", stat="identity") +
      facet_grid(. ~ cat, scales="free"  ) +
      theme(axis.text.x = element_text(angle = 45, hjust=1)) +
      scale_fill_manual(values=c('darkgoldenrod1','darkolivegreen','coral1'))
    
      
      
      
  })

  # plot 4a: Nutrients -N
  output$plotNutrients <- renderPlot({
    allResults %>%
      filter(Regions == "World") %>%
      filter(Scenarios %in% input$selSC) %>%
      filter(Organic_Vegan_Results_Indicators %in% c("OECD N balance: inputs (tN)", "OECD N balance: outputs (tN)", "OECD N balance: Inputs - outputs (tN)")) %>%
      mutate(sum_2 = ifelse(Organic_Vegan_Results_Indicators == "OECD N balance: outputs (tN)", -Val, Val)) %>%
      mutate(cat = ifelse(Scenarios %in% c("Baseline", "BaselineDerived", "FOFA_BAU_2050"), "Reference",
                          ifelse(Scenarios %in% c("FOFA_2050_VeganBAU", "FOFA_2050_VeganBAU_NoFreeAreaUse", "FOFA_2050_VeganOptimized_conv"), "Vegan-Conventional",
                                 "Vegan-Organic"))) %>%
      ggplot(aes(fill = Organic_Vegan_Results_Indicators, x = Scenarios, y = sum_2)) +
      theme_bw() +
      geom_bar(position="dodge", stat="identity")+
      facet_grid(. ~ cat, scales="free"  ) +
      theme(axis.text.x = element_text(angle = 45, hjust=1)) +
      scale_fill_manual(values=c('red','aquamarine2','aquamarine4'))
    
  })

  # 
  # #plot 4b: Nutrients -P
  # output$plotNutrients2 <- renderPlot({
  #   Results_summary %>%
  #     filter(Scenarios %in% input$selSC) %>%
  #     filter(ResultsIndicator %in% c("OECD P balance: inputs (tP2O5)", "OECD P balance: outputs (tP2O5)", "OECD P balance: Inputs - outputs (tP2O5)")) %>%
  #     mutate(ResultsIndicator = as.factor(ResultsIndicator)) %>%
  #     mutate(sum_2 = ifelse(ResultsIndicator == "OECD P balance cropland: outputs (tP2O5)", -sum, sum)) %>% 
  #     ggplot(aes(fill = ResultsIndicator, x = Scenarios, y = sum_2)) +
  #     theme_bw() +
  #     geom_bar(position="dodge", stat="identity")+
  #     theme(axis.text.x = element_text(angle = 45, hjust=1))
  # })
  # 
  # 
  # plot 1.5: Water Use
  output$plotWater <- renderPlot({
    allResults %>%
      filter(Regions == "World") %>%
      filter(Scenarios %in% input$selSC) %>%
      filter(Organic_Vegan_Results_Indicators %in% input$water) %>%
      mutate(cat = ifelse(Scenarios %in% c("Baseline", "BaselineDerived", "FOFA_BAU_2050"), "Reference",
                          ifelse(Scenarios %in% c("FOFA_2050_VeganBAU", "FOFA_2050_VeganBAU_NoFreeAreaUse", "FOFA_2050_VeganOptimized_conv"), "Vegan-Conventional",
                                 "Vegan-Organic"))) %>%
      ggplot(aes(x = Scenarios, y = Val)) +
      theme_bw() +
      geom_bar(stat="identity")+
      facet_grid(. ~ cat, scales="free"  ) +
      theme(axis.text.x = element_text(angle = 45, hjust=1)) +
      scale_fill_manual(values=c('aquamarine4'))
    
  })
  

# Plots 2 -----------------------------------------------------------------


# Plot 2.1 ----------------------------------------------------------------

  #plot 2.1: nutrient production animal vs. plant based
  output$plotNutrientCountry2 <- renderPlot({
    Population %>%
      #      filter(Scenarios %in% input$selSC) %>%
      filter(PopulationGroups == "PopulationAll") -> PopulationCountries2050
    
    Food_contents_summary_tresholds %>%
      #Food_contents_summary %>%
      filter(Contents == input$selNutrients2) %>%
      ungroup %>%
      select(treshold_low_adapted) %>%
      distinct()-> intercept1 #treshold value low
    
    Food_contents_summary_tresholds %>%
      #Food_contents_summary %>%
      ungroup() %>%
      filter(Contents == input$selNutrients2) %>%
      select(treshold_upper_adapted) %>%
      distinct() -> intercept2 #treshold value high
    
    Food_contents_summary_tresholds[, c(2, 11:12)] %>%
      distinct() -> contents_tresholds
    
    Food_contents %>%
      left_join(contents_tresholds, by = c("Contents" = "Contents")) -> Food_contents_summary_tresholds_regions
    
    
    Food_contents_summary_tresholds_regions %>% 
      left_join(PopulationCountries2050[,c(1,3, 4)], by = c("Regions" = "Regions", "Scenarios" = "Scenarios")) %>%
      rename(Pop = Level) %>%
      group_by(Regions) %>%
      mutate(Pop = ifelse(Scenarios %in% c("Baseline", "BaselineDerived", "FOFA_BAU_2050"), Pop, Pop[Scenarios == "FOFA_BAU_2050"])) %>%
      mutate(Val_per_capita_day = Val / Pop / 365) %>%
      #Food_contents_summary %>%
      filter(Contents == input$selNutrients2) %>%
      #filter(Contents %in% c("Calories (kcal)", "Protein (t)", "Fat (t)")) %>%
      #filter(Commodities != "All Commodities") %>%
      filter(Commodities %in% c("All animal based Commodities", "All crop based Commodities", "All perm Crop Commodities")) %>%
      #filter(Scenarios != "Baseline") %>%
      filter(Scenarios %in% input$selSC) %>%
      filter(Regions %in% input$selCountries2) %>%
      ggplot(aes(x = Scenarios, y = Val_per_capita_day, fill = Commodities)) +
      theme_bw() +
      scale_fill_brewer(palette = "Set2") +
      facet_grid(. ~ Regions  ) +
      geom_bar(stat = "identity", position = "stack") +
      geom_hline(yintercept=intercept1$treshold_low_adapted, linetype="dashed", color = "red") +
      geom_hline(yintercept=intercept2$treshold_upper_adapted, linetype="dashed", color = "blue") +
      ggtitle(paste0(input$selNutrients2, " per capita and day"))+
      theme(axis.text.x = element_text(angle = 45, hjust=1))
    
  })
  
  

# Plot 2.2 ----------------------------------------------------------------

  #Plot 2.2 Nutrient production per country
  output$plotNutrientCountry3 <- renderPlot({
    Population %>%
      #      filter(Scenarios %in% input$selSC) %>%
      filter(PopulationGroups == "PopulationAll") -> PopulationCountries2050
    
    Food_contents_summary_tresholds %>%
      #Food_contents_summary %>%
      filter(Contents == input$selNutrients2) %>%
      ungroup %>%
      select(treshold_low_adapted) %>%
      distinct()-> interceptLow #treshold value low
    
    Food_contents_summary_tresholds %>%
      #Food_contents_summary %>%
      ungroup() %>%
      filter(Contents == input$selNutrients2) %>%
      select(treshold_upper_adapted) %>%
      distinct() -> interceptHigh #treshold value high
    
    Food_contents_summary_tresholds[, c(2, 11:12)] %>%
      distinct() -> contents_tresholds2
    
    FoodProductionContents %>%
      left_join(contents_tresholds2, by = c("Contents" = "Contents")) -> Food_contents_summary_tresholds_regions2
    
    
    Food_contents_summary_tresholds_regions2 %>% 
      left_join(PopulationCountries2050[,c(1,3, 4)], by = c("Regions" = "Regions", "Scenarios" = "Scenarios")) %>%
      rename(Pop = Level) %>%
      group_by(Regions) %>%
      #!!!!!!!!!!!!Change this later (adapted now in SOLm)
      #mutate(Pop = 9218982000) %>%
      mutate(Pop = ifelse(Scenarios %in% c("Baseline", "BaselineDerived", "FOFA_BAU_2050"), Pop, Pop[Scenarios == "FOFA_BAU_2050"])) %>%
      mutate(Val_per_capita_day = Val / Pop / 365) %>%
      #Food_contents_summary %>%
      filter(Contents == input$selNutrients2) %>%
      #filter(Contents %in% c("Calories (kcal)", "Protein (t)", "Fat (t)")) %>%
      #filter(Commodities != "All Commodities") %>%
      filter(Commodities %in% c("All animal based Commodities", "All crop based Commodities", "All perm Crop Commodities")) %>%
      #filter(Scenarios != "Baseline") %>%
      filter(Scenarios %in% input$selSC) %>%
      filter(Regions %in% input$selCountries2) %>%
      ggplot(aes(x = Scenarios, y = Val_per_capita_day, fill = Commodities)) +
      theme_bw() +
      scale_fill_brewer(palette = "Set2") +
      facet_grid(. ~ Regions  ) +
      geom_bar(stat = "identity", position = "stack") +
      geom_hline(yintercept=interceptLow$treshold_low_adapted, linetype="dashed", color = "red") +
      geom_hline(yintercept=interceptHigh$treshold_upper_adapted, linetype="dashed", color = "blue") +
      ggtitle(paste0(input$selNutrients2, " per capita and day"))+
      theme(axis.text.x = element_text(angle = 45, hjust=1))
    
  })
  
  
  

# Plots 3 -----------------------------------------------------------------


# Plot 3.1 ----------------------------------------------------------------


  #plot 3.1: Cropland per country
  output$plotCropCountry <- renderPlot({
    allResults %>%
      filter(Scenarios %in% input$selSC) %>%
      filter(Regions %in% input$selCountries) %>%
      #filter(Regions %in% c("Switzerland", "Brazil", "Germany")) %>%
      filter(Organic_Vegan_Results_Indicators %in% c("Cropland: permanent (ha)", "Cropland: temporary (without temp. grassland) (ha)", "Grassland: temporary (ha)")) %>%
      ggplot(aes(fill = Organic_Vegan_Results_Indicators, x = Scenarios, y = Val)) +
      facet_grid(. ~ Regions ) +
      theme_bw() +
      #geom_bar(position="stack", stat="identity")+
      geom_col(width = 0.5, position = "stack")+
      theme(axis.text.x = element_text(angle = 45, hjust=1)) +
      scale_fill_manual(values=c('coral4','darkgoldenrod1','green'))+
      ggtitle("Cropping Area per country (ha)")
    
  })
  
  # #plot 4.2: Share of cropping groups
  output$plotCropRot <- renderPlot({
  CroppingAreas_RotCat %>%
    group_by(Regions, CropRotGroup, Scenarios) %>%
    filter(ProductionSystems == "AllProdSyst") %>%
    filter(Scenarios %in% input$selSC) %>%
    filter(Regions %in% input$selCountries) %>%
    filter(!is.na(CropRotGroup)) %>%
    summarise(sum = sum(Val)) %>%
    group_by(Regions, Scenarios) %>%
    mutate(perc = sum / sum(sum)) %>%
    arrange(desc(perc)) %>%
    ggplot(aes(x = Scenarios, y = perc, fill = CropRotGroup)) +
    scale_y_continuous(labels = scales::percent) + 
    theme_bw() +
    facet_grid(. ~ Regions) +
    geom_bar(stat = "identity", position = "stack") +
    theme(axis.text.x = element_text(angle = 45, hjust=1)) +
    ggtitle("Distribution in temporary cropping areas") +
    scale_fill_brewer(palette = "Set2")
  
  })
  
  # #plot 4.3: Share of cropping groups
  output$plotCropInCropRot <- renderPlot({
    CroppingAreas_RotCat %>%
      filter(ProductionSystems == "AllProdSyst") %>%
      filter(Scenarios %in% input$selSC) %>%
      filter(Regions %in% input$selCountries) %>%
      filter(CropRotGroup == input$selCropRotGroup ) %>%
      group_by(Regions, Scenarios) %>%
      mutate(perc = Val/(sum(Val))) %>%
      ggplot(aes(x = Scenarios, y = perc, fill = Activities)) +
      theme_bw() +
      facet_grid(. ~ Regions) +
      geom_bar(stat = "identity", position = "stack") +
      scale_y_continuous(labels = scales::percent)  +
      theme(axis.text.x = element_text(angle = 45, hjust=1)) +
      ggtitle("Crops in Crop Rotation Category") +
      scale_fill_brewer(palette = "Set2") 
    
  })
  
  # #plot 5.1: GHG
  output$plotGHGcountry <- renderPlot({
    allResults %>%
      filter(Scenarios %in% input$selSC) %>%
      filter(Regions %in% input$selCountriesGHG) %>%
      filter(Organic_Vegan_Results_Indicators %in% c("Total GHG emissions - animals (t CO2e)","Tot GHG em - crops/grass, with Defor/OrgSoils (t CO2e)", "Tot GHG em - crops/grass, no Defor/OrgSoils (t CO2e)")) %>%
      mutate(Organic_Vegan_Results_Indicators = ifelse(Organic_Vegan_Results_Indicators == "Tot GHG em - crops/grass, with Defor/OrgSoils (t CO2e)", "TotalCropGHG", as.character(Organic_Vegan_Results_Indicators) )) %>%
      mutate(Organic_Vegan_Results_Indicators = ifelse(Organic_Vegan_Results_Indicators == "Tot GHG em - crops/grass, no Defor/OrgSoils (t CO2e)", "CropGHG_noDef", as.character(Organic_Vegan_Results_Indicators) )) %>%
      pivot_wider(names_from = Organic_Vegan_Results_Indicators, values_from = Val) %>%
      mutate(Deforestation = TotalCropGHG - CropGHG_noDef) %>%
      pivot_longer(cols = 3:6, names_to = "Organic_Vegan_Results_Indicators", values_to = "Val" ) %>%
      filter(Organic_Vegan_Results_Indicators %in% c("Total GHG emissions - animals (t CO2e)", "CropGHG_noDef", "Deforestation")) %>%
      ggplot(aes(fill = Organic_Vegan_Results_Indicators, x = Scenarios, y = Val)) +
      facet_grid(. ~ Regions) +
      theme_bw() +
      geom_bar(position="stack", stat="identity") +
      theme(axis.text.x = element_text(angle = 45, hjust=1)) +
      scale_fill_manual(values=c('darkgoldenrod1','darkolivegreen','coral1'))
    


    
  })
  
  # #plot 6.1: Water
  output$plotWaterCountry <- renderPlot({
    allResults %>%
      filter(Regions %in% input$selCountriesWater) %>%
      filter(Scenarios %in% input$selSC) %>%
      filter(Organic_Vegan_Results_Indicators %in% input$water) %>%
      ggplot(aes(x = Scenarios, y = Val)) +
      facet_grid(. ~ Regions) +
      theme_bw() +
      geom_bar(stat="identity")+
      theme(axis.text.x = element_text(angle = 45, hjust=1)) +
      scale_fill_manual(values=c('aquamarine4')) 
    
  })
  
  # #plot 7.1: Nitrogen
  output$plotNcountry <- renderPlot({
    allResults %>%
      filter(Regions %in% input$selCountriesN) %>%
      filter(Scenarios %in% input$selSC) %>%
      filter(Organic_Vegan_Results_Indicators %in% c("OECD N balance: inputs (tN)", "OECD N balance: outputs (tN)", "OECD N balance: Inputs - outputs (tN)")) %>%
      mutate(sum_2 = ifelse(Organic_Vegan_Results_Indicators == "OECD N balance: outputs (tN)", -Val, Val)) %>%
      ggplot(aes(fill = Organic_Vegan_Results_Indicators, x = Scenarios, y = sum_2)) +
      theme_bw() +
      geom_bar(position="dodge", stat="identity")+
      facet_grid(. ~ Regions) +
      theme(axis.text.x = element_text(angle = 45, hjust=1)) +
      scale_fill_manual(values=c('red','aquamarine2','aquamarine4'))

    
  })
  
  
  
 

# Plots OV ----------------------------------------------------------------

  output$plot_ov_cal <- renderPlot({
    pkcal <- ggplot(kcal_ov_global, aes(as.factor(o), as.factor(v), group=as.factor(v))) +
      geom_tile(aes(fill = Change_perc)) +
      geom_text(aes(label = round(Change_perc, 1))) +
      scale_fill_gradient2(low = "red1", midpoint = 0, high = "aquamarine4") +
      ggtitle("Relative change in kcal production, global") +
      xlab("Organic Share") +
      ylab("Vegan Share") +
      guides(fill = guide_legend(title = "Change in %"))
    pkcal
  })
    
  output$plot_ov_cal_req <- renderPlot({
    pkcal_Req <- ggplot(kcal_ov_global_Req, aes(as.factor(o), as.factor(v), group=as.factor(v))) +
      geom_tile(aes(fill = prodReq)) +
      geom_text(aes(label = round(prodReq, 1))) +
      scale_fill_gradient2(low = "red1", midpoint = 0, high = "aquamarine4") +
      ggtitle("Potential surplus production of kcal, global") +
      xlab("Organic Share") +
      ylab("Vegan Share") +
      guides(fill = guide_legend(title = "Surplus in %"))
    pkcal_Req
  })
  
  output$plot_ov_prot <- renderPlot({
    pprotein <- ggplot(protein_ov_global, aes(as.factor(o), as.factor(v), group=as.factor(v))) +
      geom_tile(aes(fill = Change_perc)) +
      geom_text(aes(label = round(Change_perc, 1))) +
      scale_fill_gradient2(low = "red1", midpoint = 0, high = "aquamarine4") +
      ggtitle("Relative change in protein production, global") +
      xlab("Organic Share") +
      ylab("Vegan Share") +
      guides(fill = guide_legend(title = "Change in %"))
    pprotein
  })
  
  output$plot_ov_prot_req <- renderPlot({
    pprotein_Req <- ggplot(protein_ov_global_Req, aes(as.factor(o), as.factor(v), group=as.factor(v))) +
      geom_tile(aes(fill = prodReq)) +
      geom_text(aes(label = round(prodReq, 1))) +
      scale_fill_gradient2(low = "red1", midpoint = 0, high = "aquamarine4") +
      ggtitle("Potential surplus production of proteins, global") +
      xlab("Organic Share") +
      ylab("Vegan Share") +
      guides(fill = guide_legend(title = "Surplus in %"))
    pprotein_Req
  })
  
  output$plot_ov_fat <- renderPlot({
    pfat <- ggplot(fat_ov_global, aes(as.factor(o), as.factor(v), group=as.factor(v))) +
      geom_tile(aes(fill = Change_perc)) +
      geom_text(aes(label = round(Change_perc, 1))) +
      scale_fill_gradient2(low = "red1", midpoint = 0, high = "aquamarine4") +
      ggtitle("Relative change in fat production, global") +
      xlab("Organic Share") +
      ylab("Vegan Share") +
      guides(fill = guide_legend(title = "Change in %"))
    pfat
  })
  
  output$plot_ov_fat_req <- renderPlot({
    pfat_Req <- ggplot(fat_ov_global_Req, aes(as.factor(o), as.factor(v), group=as.factor(v))) +
      geom_tile(aes(fill = prodReq)) +
      geom_text(aes(label = round(prodReq, 1))) +
      scale_fill_gradient2(low = "red1", midpoint = 0, high = "aquamarine4") +
      ggtitle("Potential surplus production of fat, global") +
      xlab("Organic Share") +
      ylab("Vegan Share") +
      guides(fill = guide_legend(title = "Surplus in %"))
    pfat_Req
  })
  
  output$plot_ov_GHG <- renderPlot({
    allResults_ov_index %>%
      filter(!(Scenarios %in% c("Baseline", "BaselineDerived", "FOFA_BAU_2050")), 
             Regions == "World",
             Organic_Vegan_Results_Indicators == input$selGHG_ov) %>%
      mutate(Change_perc = (Val / Val[Scenarios == "FOFA_2050_VeganBarbieri_0_0"] - 1)*100) -> GHG_ov_global
    pGHG <- ggplot(GHG_ov_global, aes(as.factor(o), as.factor(v), group=as.factor(v))) +
      geom_tile(aes(fill = Change_perc)) +
      geom_text(aes(label = round(Change_perc, 1))) +
      scale_fill_gradient2(low = "aquamarine4", midpoint = 0, high = "red1") +
      ggtitle("Relative Change in GHG emissions (global)") +
      xlab("Organic Share") +
      ylab("Vegan Share") +
      guides(fill = guide_legend(title = "Change in %"))
    pGHG

  })
  
  output$plot_ov_water <- renderPlot({
    allResults_ov_index %>%
      filter(!(Scenarios %in% c("Baseline", "BaselineDerived", "FOFA_BAU_2050")), Regions != "World") %>%
      filter(Organic_Vegan_Results_Indicators == input$selWater_ov) %>%
      group_by(Regions) %>%
      mutate(Change_perc = (Val / Val[Scenarios == "FOFA_2050_VeganBarbieri_0_0"] - 1)*100) -> water_ov_countries
    
    water_ov_countries %>%
      group_by(Scenarios, o, v) %>%
      summarise(sum_global = sum(Val)) %>%
      ungroup() %>%
      mutate(Change_perc = (sum_global / sum_global[Scenarios == "FOFA_2050_VeganBarbieri_0_0"] - 1)*100) -> water_ov_global
    
    pwater <- ggplot(water_ov_global, aes(as.factor(o), as.factor(v), group=as.factor(v))) +
      geom_tile(aes(fill = Change_perc)) +
      geom_text(aes(label = round(Change_perc, 1))) +
      scale_fill_gradient2(low = "aquamarine4", midpoint = 0, high = "red1") +
      ggtitle("Relative Change in irrigation water use (water stress adjusted, global)") +
      xlab("Organic Share") +
      ylab("Vegan Share") +
      guides(fill = guide_legend(title = "Change in %"))
    pwater

    
  })
  
  
  

# Download Buttons --------------------------------------------------------

  # 1) plot nutrients
  PlotInput <- reactive({
    Food_contents_summary_tresholds %>%
      #Food_contents_summary %>%
      filter(Contents == input$selFA) %>%
      select(treshold_low_adapted) %>%
      unique() -> intercept1 #treshold value low
    
    Food_contents_summary_tresholds %>%
      #Food_contents_summary %>%
      filter(Contents == input$selFA) %>%
      select(treshold_upper_adapted) %>%
      
      unique() -> intercept2 #treshold value high
    #   Food_contents_summary_tresholds$x <- factor(Food_contents_summary_tresholds$x,                                    # Factor levels in decreasing order
    #                   levels = Food_contents_summary_tresholds$x[order(Food_contents_summary_tresholds$sum, decreasing = TRUE)])
    # # #evtl. delete
    # #   Food_contents_summary_tresholds %>%
    # #     mutate(x = factor(Scenarios,  # Factor levels in decreasing order
    # #                        levels = Food_contents_summary_tresholds$Scenarios[order(Food_contents_summary_tresholds$sum, decreasing = TRUE)])) -> Food_contents_summary_tresholds
    # #   
    # #   
    Food_contents_summary_tresholds %>%
      #Food_contents_summary %>%
      filter(Contents == input$selFA) %>%
      #filter(Scenarios != "Baseline") %>%
      filter(Scenarios %in% input$selSC) %>%
      filter(Commodities %in% c("All animal based Commodities", "All crop based Commodities", "All perm Crop Commodities")) %>%
      ggplot(aes(x = Scenarios, y = Sum_per_capita_day, fill = Commodities)) +
      theme_bw() +
      geom_bar(stat = "identity", position = "stack") +
      ggtitle(paste0(input$selFA, " per capita and day"))+
      theme(axis.text.x = element_text(angle = 45, hjust=1)) +
      #geom_hline(yintercept=20, linetype="dashed", color = "red")
      geom_hline(yintercept=intercept1$treshold_low_adapted, linetype="dashed", color = "red") +
      geom_hline(yintercept=intercept2$treshold_upper_adapted, linetype="dashed", color = "blue") +
      scale_fill_brewer(palette = "Set2")
    
  })
  
  #test alternative
  output$plot1 <- renderPlot({
    print(PlotInput())
  })
  
  output$download11 <- downloadHandler(
    filename = function() { "ShinyPlotDownload.png" },
    content = function(file) {
    ggsave(file, plot = PlotInput(), device = "png", width = 5, height = 4, dpi = 300, units = "in")
  }
  )
  
  
}



# 4 - Define Shiny App -----------------------------------------------------


shinyApp(server = server, 
         ui = dashboardPage(
           #downloadButton("download1"),
           #downloadLink("download2"),
           #skin = "blue",
          dashboardHeader(title = "Food System Analysis"),
          sidebar,
          body
         )
         )
