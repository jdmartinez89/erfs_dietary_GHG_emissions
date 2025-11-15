###########################
# Script for
# Martinez, J.D., Ramankutty, N. 2025. Dietary GHG emissions from 2.7 billion people already exceed the personal carbon footprint needed to achieve the 2°C climate goal

### Setup
# Load libraries
library(tidyverse)
library(countrycode)
`%nin%` = Negate(`%in%`)

# Read in data
# Data from Kim_et_al2019 refereed as Kim henceforth

impacts_item <- read_csv("Data_env/item_footprints_per_unit.csv")
impacts_group <- read_csv("Data_env/diet_footprints_by_country_diet_output_group.csv")
# inspect
unique(impacts_group$output_group)
unique(impacts_group$attribute)

### Wrangling
# Match Kim et al's groupings to Martinez et al's categories (referred as JD henceforth)
Kim_JD_cat <- data.frame(Kim_cat=unique(impacts_group$output_group),
                         JD_Cat=c(rep("Meat & Seafood",2),
                                  rep("Dairy & Eggs",2),"Fruits","Starches",NA,"Treenuts",
                                  rep("Meat & Seafood",3),
                                  "Pulses & Oilseeds","Meat & Seafood","Starches",
                                  "Sugars & Sweeteners","Oils & Fats","Vegetables"),
                         ani_veg=c(rep("Animal",4),rep("Vegetable",4),rep("Animal",3),
                                   "Vegetable","Animal",rep("Vegetable",4)))

### Total impacts by Kim food group and total
## Data to calculate total emissions
Kims_pop <- read_csv("Data_env/fao_population.csv")
impacts_country <- read_csv("Data_env/diet_footprints_by_country_diet.csv")
impacts_country <- impacts_country %>%
  filter(diet == "baseline",attribute%in% c("kg_co2e_excl_luc","kg_co2e_total")) %>% 
  left_join(Kims_pop)
## Kim's total impacts frame
total_impacts <- impacts_country %>% group_by(attribute) %>% 
  summarise(total_impact = sum(value*population))
# total_impacts values
Kims_non_LUC_CO2_total <- total_impacts[[1,2]]
Kims_LUC_CO2_total <- total_impacts[[2,2]]-total_impacts[[1,2]]
# impact per capita world
Kims_non_LUC_CO2_total/(sum(impacts_country$population)/2) ## used in MS
# impact by group
impacts_group_co2e <- impacts_group %>%
  filter(diet == "baseline",attribute%in% c("kg_co2e_excl_luc","kg_co2e_total")) %>% 
  left_join(Kims_pop) %>% left_join(Kim_JD_cat, by = c("output_group"="Kim_cat"))

total_impacts_by_fg <- impacts_group_co2e %>% group_by(JD_Cat,attribute) %>% 
  summarise(total_impact = sum(value*population))

total_impacts_by_ani_veg <- impacts_group_co2e %>% group_by(ani_veg,attribute) %>% 
  summarise(total_impact = sum(value*population))

## check of consistency between aggregations
total_impacts_by_attribute <- impacts_group_co2e %>% group_by(attribute) %>% 
  summarise(total_impact = sum(value*population))


### Aggregation from Kim's to JD's categories

baseline <- impacts_group %>% filter(diet == "baseline")
baseline_JD_cat <- left_join(baseline,Kim_JD_cat, by = c("output_group"="Kim_cat"))
by_country <- baseline_JD_cat %>% group_by(country_code,country,attribute,JD_Cat) %>% summarise(impact=sum(value)) %>%
  ungroup() %>%
  mutate(iso3= ifelse(country_code==41,"CHN",countrycode(country_code,"fao","iso3c")))

############
### FAO_GHG data to rescale Kim's
## (Run this only for the sensitivity analysis)
##########
# import data
FAO_CO2_data <- read.csv("Data/FAOSTAT_Ag_lands_CO2eq.csv")
# select relevant columns
FAO_CO2_data <- FAO_CO2_data %>% select(Element,Item,Year,Value, Unit)
# get the 3 year average
FAO_CO2_data_2011_13 <- FAO_CO2_data %>% group_by(Element,Item,Unit) %>% 
  summarise(CO2eq_ton = mean(Value)*1E6)
# get grand total CO2eq only
FAO_CO2_data_2011_13_totals <- FAO_CO2_data_2011_13 %>% filter(Element=="Emissions (CO2eq) (AR5)")
FAOs_non_LUC_CO2_total <- FAO_CO2_data_2011_13_totals[[1,4]]
FAOs_LUC_CO2_total <- FAO_CO2_data_2011_13_totals[[2,4]]
#calculate ratio by dividing by the total data form Kim
non_LUC_ratio <- FAOs_non_LUC_CO2_total/Kims_non_LUC_CO2_total
LUC_ratio <- FAOs_LUC_CO2_total/Kims_LUC_CO2_total

## get a LUC column on Kim"s data
LUC <- by_country %>%filter(attribute %in% c("kg_co2e_total","kg_co2e_excl_luc")) %>% 
  pivot_wider(names_from = attribute, values_from = impact) %>%
  mutate(LUC = kg_co2e_total-kg_co2e_excl_luc,
         kg_co2e_excl_luc = kg_co2e_excl_luc*non_LUC_ratio,
         LUC = LUC*LUC_ratio,
         kg_co2e_total = LUC+kg_co2e_excl_luc)
LUC_long <- LUC %>% pivot_longer(cols = c(kg_co2e_total,kg_co2e_excl_luc), names_to = "attribute", values_to = "impact") %>% select(-LUC)

## NOTE: this adjustment is only for the 2 categories that I have used in the remainder of the pipeline. CO2 subcategories are not adjusted so if I were to use them for analysis or paper figures then I should do so above here

# update Kim's data based on the adjustment to FAO climate data
by_country <- by_country %>%filter(attribute %nin% c("kg_co2e_total","kg_co2e_excl_luc")) %>%
  bind_rows(LUC_long)

## check of consistency between aggregations

test_n <- by_country %>%
  filter(attribute%in% c("kg_co2e_excl_luc","kg_co2e_total")) %>% 
  left_join(Kims_pop)
total_impacts_by_attribute_adj <- test_n %>% group_by(attribute) %>% 
  summarise(total_impact = sum(impact*population))

#####################
### Data join
# Impact by country by JD cat, now use caloric shares by food group to multiply those impacts

# Food consumption by decile data from Martinez et al
bias_adjusted <- readRDS("Data/bias_adjusted_iter_ch3.rds")

scam_rescaled <-bias_adjusted %>% rename(rescaled_prebias = rescaled, GrandT_demand_prebias=GrandT_demand) %>%  rename(rescaled=temp_d_fit, GrandT_demand = GrandT_demand_bias)

## get the three year average like Kim et al 2011-2013
three_y_avg <- scam_rescaled %>% filter(Year %in% c(2011:2013)) %>% ungroup() %>%
  group_by(iso3,JD_Cat, income_level) %>% summarise(across(c(FAO_pop,Food_Demand,Income,rescaled,GrandT_demand),mean))

## calculate the shares
shares_by_FG <- three_y_avg %>% ungroup() %>% mutate(share_of_FG = rescaled/(Food_Demand*10))

## join with footprint data and mutate
shares_footprint <- left_join(shares_by_FG, by_country, by = c("iso3", "JD_Cat")) %>%
  mutate(impact_by_income=share_of_FG*impact*10)

## NA table
shares_footprint_NA <- shares_footprint%>% filter(JD_Cat!="Grand Total") %>% filter(is.na(impact_by_income))
length(unique(shares_footprint_NA$iso3))
length(unique(by_country$country_code))

setdiff(unique(by_country$iso3), unique(shares_by_FG$iso3))
setdiff(unique(shares_by_FG$iso3),unique(by_country$iso3))
length(intersect(unique(shares_by_FG$iso3),unique(by_country$iso3)))
## The intersection is 'just' 112 countries
## I calculate population representativeness later on

## sum impacts by iso3 to get a Gran Total JD_Cat
GrandT_impact <- shares_footprint %>% filter(JD_Cat!="Grand Total") %>%
  drop_na(impact_by_income) %>% ungroup() %>% group_by(iso3,country,country_code,income_level, attribute) %>%
  summarise(impact=sum(impact),impact_by_income=sum(impact_by_income)) %>% 
  mutate(JD_Cat="Grand Total")

## create the df to rbind to main df
GrandT_FBS <- shares_by_FG %>% filter(JD_Cat=="Grand Total")

GrandT_FBS_impact <- left_join(GrandT_FBS,GrandT_impact,by = c("iso3", "JD_Cat", "income_level"))

GrandT_FBS_impact_NA <- GrandT_FBS_impact%>% filter(is.na(impact_by_income))
## rbind to main df
env_impact_df <- bind_rows(shares_footprint %>% drop_na(impact_by_income) %>% filter(JD_Cat!="Grand Total"),
                           GrandT_FBS_impact %>% drop_na(impact_by_income)) %>% mutate(region=countrycode(iso3,"iso3c","continent"))

## if saving a file choose correct file: FAO or not adjusted
# env_impact_df %>% saveRDS("Data/Ch2_env_impact_df_FAO_adj_2023.rds")
# env_impact_df %>% saveRDS("Data/Ch2_env_impact_df_2023.rds")

# env_impact_df <- readRDS("Data/Ch2_env_impact_df_FAO_adj_2023.rds")
env_impact_df <- readRDS("Data/Ch2_env_impact_df_2023.rds")

# env_impact_df %>% write_csv("Data/Data_Martinez_Ramankutty_2025.csv") # to add as SI for publication


### Number of countries in dataset match and population represented

## The intersection is 'just' 112 countries
length(intersect(unique(shares_by_FG$iso3),unique(by_country$iso3)))

## % of population
fs <- readRDS("Data/FAO_FBS_non_soviet_20200414.rds")
## filter for pop
pop_2011_13 <- fs %>% filter(Item=="Population", Year %in% c(2011:2013))
pop_2011_13_avg <- sum(pop_2011_13$Value)*1000/3
# temp <- pop_2011_13 %>% count(`Country Code`) ## Sudan is the only country to split during this period

## sum pop along shares df
shares_footprint_non_NA <- shares_footprint%>% filter(JD_Cat=="Meat & Seafood",
                                                      income_level=="decile1",
                                                      attribute=="kg_co2e_total") %>%
  filter(!is.na(impact_by_income))
length(unique(shares_footprint_non_NA$iso3))

sample_pop <- sum(shares_footprint_non_NA$FAO_pop)
sample_pop/pop_2011_13_avg

# if the 5Gt EAT-Lancet emissions are split between that sample pop it allows 
5E12/sample_pop
# 821.1042 kg CO2e per capita per year

# Otherwise it would be between all the pop available according to FAO
5E12/pop_2011_13_avg

### Key datasets

# kg_co2e_total
fig_df_cont <- env_impact_df %>%
  filter(attribute=="kg_co2e_total",JD_Cat=="Grand Total",income_level %nin% c("mean")) %>% #group_by(JD_Cat) %>% 
  # arrange(desc(impact_by_income)) %>% 
  arrange(desc(impact_by_income)) %>% 
  mutate(Country_decile = paste(country,income_level,sep = "_"),
         iso3_decile = paste(iso3,income_level,sep = "_"),
         Country_decile = fct_reorder(Country_decile, impact_by_income,.desc = T),
         iso3_decile = fct_reorder(iso3_decile, impact_by_income,.desc = T),
         width= cumsum(FAO_pop/10),
         width_min = width-(FAO_pop/10),
         width_min_t= width_min+(width - width_min)/2)
# kg_co2e_excl_luc
fig_df_cont_no_LUC <- env_impact_df %>%
  filter(attribute=="kg_co2e_excl_luc",JD_Cat=="Grand Total",income_level %nin% c("mean")) %>% #group_by(JD_Cat) %>% 
  # arrange(desc(impact_by_income)) %>% 
  arrange(desc(impact_by_income)) %>% 
  mutate(Country_decile = paste(country,income_level,sep = "_"),
         iso3_decile = paste(iso3,income_level,sep = "_"),
         Country_decile = fct_reorder(Country_decile, impact_by_income,.desc = T),
         iso3_decile = fct_reorder(iso3_decile, impact_by_income,.desc = T),
         width= cumsum(FAO_pop/10),
         width_min = width-(FAO_pop/10),
         width_min_t= width_min+(width - width_min)/2)


## For loop for x% goal in total emissions under curve
# IMPORTANT: this chunk is key but there are choices (comment out/in) that need to be made regarding which assumptions are used: FAO adjustment yes/no? and LUC included/not included?
  
## Data to calculate total emissions
Kims_pop <- read_csv("Data_env/fao_population.csv")
impacts_country <- read_csv("Data_env/diet_footprints_by_country_diet.csv")
impacts_country <- impacts_country %>%
  filter(diet == "baseline",attribute%in% c("kg_co2e_excl_luc","kg_co2e_total")) %>% 
  left_join(Kims_pop)
## total impacts
total_impacts <- impacts_country %>% group_by(attribute) %>% 
  summarise(total_impact = sum(value*population))

## Select target %s and total impacts!!!
target_perc <- c(1,.8,.7,.6,.5)
total_no_LUC <- total_impacts[[1,2]]
total_emi<- total_impacts[[2,2]]
# total_no_LUC <- FAOs_non_LUC_CO2_total
# total_emi<- FAOs_non_LUC_CO2_total+FAOs_LUC_CO2_total

# Objectives based on previous choices
objective_total <- 5E12+(total_emi-5E12)*(1-target_perc)
objective_non_LUC <- 5E12+(total_no_LUC-5E12)*(1-target_perc)

# Select whether to use LUC or non_LUC, just one of the next two pairs of lines should be run!!!
# target_df <- fig_df_cont
# objective <- objective_total
# or
target_df <- fig_df_cont_no_LUC
objective <- objective_non_LUC

## guess ceiling
floor <- 513.6
# ceiling_poor <- 1151.3 
ceiling_poor <-c(rep(910,5))
## initialize i
i <- 1
eat_poor <- target_df %>% arrange(impact_by_income) %>%
  mutate(impact_by_income=ifelse(impact_by_income>ceiling_poor[i],
                                 ceiling_poor[i],impact_by_income),
         impact_by_income=ifelse(impact_by_income<floor,floor,impact_by_income),
         target=cumsum(FAO_pop/10*impact_by_income))
## iteratively optimize
for (i in 1:length(ceiling_poor)){
  while(max(eat_poor$target)<=objective[i]){
    while(max(eat_poor$target)<=objective[i]-1E10){
      ceiling_poor[i] <- ceiling_poor[i]+1
      #update eat dataframe
      eat_poor <- target_df %>% arrange(impact_by_income) %>%
        mutate(impact_by_income=ifelse(impact_by_income>ceiling_poor[i],
                                       ceiling_poor[i],impact_by_income),
               impact_by_income=ifelse(impact_by_income<floor,floor,impact_by_income),
               target=cumsum(FAO_pop/10*impact_by_income))
    }
    ceiling_poor[i] <- ceiling_poor[i]+.1
    #update eat dataframe
    eat_poor <- target_df %>% arrange(impact_by_income) %>%
      mutate(impact_by_income=ifelse(impact_by_income>ceiling_poor[i],
                                     ceiling_poor[i],impact_by_income),
             impact_by_income=ifelse(impact_by_income<floor,floor,impact_by_income),
             target=cumsum(FAO_pop/10*impact_by_income))
  }
  ifelse(max(eat_poor$target)>objective[i],ceiling_poor[i] <- ceiling_poor[i]-.1, ceiling_poor[i] <- ceiling_poor[i])
}
# ceiling <- 1179.3
ceiling <- ceiling_poor
#initialize eat dataframe
eat <- target_df %>% arrange(impact_by_income) %>%
  mutate(impact_by_income=ifelse(impact_by_income>ceiling[1],
                                 ceiling[1],impact_by_income),
         target=cumsum(FAO_pop/10*impact_by_income))
## find actual ceilings through iteration
i <- 1
for (i in 1:length(ceiling)){
  while(max(eat$target)<=objective[i]){
    ceiling[i] <- ceiling[i]+.1
    #update eat dataframe
    eat <- target_df %>% arrange(impact_by_income) %>%
      mutate(impact_by_income=ifelse(impact_by_income>ceiling[i],
                                     ceiling[i],impact_by_income),
             target=cumsum(FAO_pop/10*impact_by_income))
  }
  ifelse(max(eat$target)>objective[i],ceiling[i] <- ceiling[i]-.1, ceiling[i] <- ceiling[i])
}
ceiling_vector <- ceiling
ceiling_poor_vector <- ceiling_poor


## Lastest Model, with FAO_GHG adjustment, includes LUC
# > ceiling
# [1]  909.8 1197.7 1388.4 1614.9 1922.6
# > ceiling_poor
# [1]  909.9 1186.9 1375.3 1599.6 1899.4

## Lastest Model, with FAO_GHG adjustment, excludes LUC
# > ceiling
# [1]  960.5 1100.4 1181.2 1271.9 1376.5
# > ceiling_poor
# [1]  940.1 1075.4 1153.2 1241.2 1341.1
ceiling <- c(960.5, 1100.4, 1181.2, 1271.9, 1376.5)
ceiling_poor <- c(940.1, 1075.4, 1153.2, 1241.2, 1341.1)

## Lastest Model, no FAO_GHG adjustment, includes LUC
# > ceiling
# [1]  917.9 1102.8 1213.0 1338.4 1484.6
# > ceiling_poor
# [1]  909.9 1087.8 1196.2 1319.1 1462.0

## Lastest Model, no FAO_GHG adjustment, excludes LUC
# > ceiling
# [1]  934.8 1083.5 1171.5 1269.5 1382.1
# > ceiling_poor
# [1]  919.7 1065.6 1151.5 1246.8 1355.6
ceiling <- c(934.8, 1083.5, 1171.5, 1269.5, 1382.1)
ceiling_poor <- c(919.7, 1065.6, 1151.5, 1246.8, 1355.6)

### Summary Table

## Initial values for summary table
# target_perc
# objective
# ceiling and ceiling_poor
summary_table <- data.frame(target_perc,sample_pop,objective,ceiling_vector,ceiling_poor_vector)

# People below min Lancet healthy diet in 2050
lacking <- target_df %>% filter(impact_by_income<=floor)
summary_table$below_2050 <- (max(lacking$width)-min(lacking$width_min))
# People above 2013
rm(excess_list)
excess_list <- vector("list", length(ceiling_vector))
for (i in 1:length(ceiling_vector)){
  excess_list[[i]] <- target_df %>%
    select(impact_by_income,width,width_min) %>% 
    filter(impact_by_income>=ceiling_vector[i])
}
# I could possibly just do one big lapply or at least one per list but I might figure that out later
summary_table$exceeding_2013 <- unlist(lapply(excess_list,function(x){
  max <- max(x$width)
  # max_perc <- max(x$width)/sample_pop
  # data.frame(max,max_perc)
}))
# People above 2013_poor
rm(excess_poor_list)
excess_poor_list <- vector("list", length(ceiling_poor_vector))
for (i in 1:length(ceiling_poor_vector)){
  excess_poor_list[[i]] <- target_df %>%
    filter(impact_by_income>=ceiling_poor_vector[i]) %>%
    mutate(impact_by_income=ifelse(impact_by_income>=ceiling_vector[i],
                                   ceiling_vector[i],impact_by_income))
}
summary_table$exceeding_poor_2013 <- unlist(lapply(excess_poor_list,function(x){
  max <- max(x$width)}))
# Calculate all commbinations and percentages
summary_table <- summary_table %>% mutate(
  exceeding_2013_perc = exceeding_2013/sample_pop,
  below_2013 = sample_pop-exceeding_2013,
  below_2013_perc = 1-exceeding_2013_perc,
  below_2050_perc = below_2050/sample_pop,
  exceeding_poor_2013_perc = exceeding_poor_2013/sample_pop,
  inbetween_2013_2050 = sample_pop-exceeding_2013-below_2050,
  inbetween_2013_2050_perc = inbetween_2013_2050/sample_pop,
  below_2050_perc_of_below_2013 = below_2050/below_2013, # Of those people eating diets that meet the target, X percent eat less than 2050 5Gt/population target
  exceeding_poor_2013_perc = exceeding_poor_2013/sample_pop,
  below_poor_2013_perc = 1-exceeding_poor_2013_perc,
  diff_in_exceeding = exceeding_poor_2013-exceeding_2013,
  diff_in_exceeding_perc = diff_in_exceeding/sample_pop
)
# write_csv(summary_table, "Data_Env/Table_1_MS_with_LUC.csv")
# write_csv(summary_table, "Data_Env/Table_1_MS_no_LUC.csv")
# write_csv(summary_table, "Data_Env/Table_1_MS_no_LUC_adj.csv")
# Since Oct 3 2022
# write_csv(summary_table, "Data_Env/Table_1_MS_no_FAO_adj_with_LUC.csv")
# write_csv(summary_table, "Data_Env/Table_1_MS_no_FAO_adj_no_LUC.csv")
# write_csv(summary_table, "Data_Env/Table_1_MS_with_FAO_adj_no_LUC.csv")
# write_csv(summary_table, "Data_Env/Table_1_MS_with_FAO_adj_with_LUC.csv")





# Without LUC
## 2.Variable width Figure

# kg_co2e_total
# kg_co2e_excl_luc
fig_df_no_LUC <- env_impact_df %>%
  filter(attribute=="kg_co2e_excl_luc",JD_Cat=="Grand Total",income_level %nin% c("mean")) %>% #group_by(JD_Cat) %>% 
  # arrange(desc(impact_by_income)) %>% 
  arrange(desc(impact_by_income)) %>% 
  mutate(#Country_FAO = fct_reorder(Country_FAO, rescaled),
         #iso3 = fct_reorder(iso3, rescaled),
         Country_decile = paste(country,income_level,sep = "_"),
         iso3_decile = paste(iso3,income_level,sep = "_"),
         Country_decile = fct_reorder(Country_decile, impact_by_income,.desc = F),
         iso3_decile = fct_reorder(iso3_decile, impact_by_income,.desc = F),
         width= cumsum(FAO_pop/10),
         width_min = width-(FAO_pop/10),
         width_min_t= width_min+(width - width_min)/2)

mean_ghgs_discrete <- weighted.mean(fig_df_no_LUC$impact,fig_df_no_LUC$FAO_pop)


ggplot(fig_df_no_LUC, aes(ymin=0, fill = as.factor(region)))+
  geom_rect(aes(xmin=width_min/1E9, xmax=width/1E9, ymax=impact_by_income/1000)) +
  # geom_rect(aes(xmin=width_min/1E9, xmax=width/1E9, ymax=Income)) +
  geom_hline(yintercept = mean_ghgs_discrete/1E3, size =.25, lty=2, color ="gray30")+
  geom_text(x=6, y=mean_ghgs_discrete/1E3+.2, label="mean", color = "gray30", size = 3, check_overlap = T)+
  ylab("Annual food-consumption GHG emissions [Tons CO2e/person]")+
  xlab("Cumulative Population [billions]")+
  #coord_flip()
  scale_y_continuous(n.breaks = 6)+
  # scale_y_log10()+
  scale_x_continuous(n.breaks = 7)+
  # scale_fill_brewer(palette = "Set3")+
  # scale_fill_manual(values = c('#8dd3c7','#fb8072','#80b1d3','#bebada','#ffffb3'))+
  # scale_fill_manual(values = c("#59A14F","#E15759", "#4E79A7","#F28E2B", "#76B7B2"))+
                                                                                                                                                      scale_fill_manual(values = c('#4daf4a','#e41a1c','#377eb8','#ff7f00','#984ea3'))+
    # scale_fill_manual(values = c('#e41a1c','#377eb8'))+
    # ggthemes::scale_fill_tableau(palette = "Color Blind")+
    theme_bw()+
    theme(axis.line.x = element_line(color="black", size = .25), 
          axis.line.y = element_line(color="black", size = .25),
          panel.border = element_blank(),
          panel.grid=element_blank(),
          legend.title=element_blank(),
          # legend.position=c(0.9,.8),
          legend.position="none")
  
  title <- "Lorenz_discrete_deciles_no_LUC"
  # ggsave(paste0(getwd(), "/Output/", title, "_SI_FAO_adj.png")
  #        , width = 120, height = 120, units = "mm"
  #        )
  
  
  Brazil-US-China
  
  ggplot(fig_df_no_LUC, aes(ymin=0))+
    geom_rect(aes(xmin=width_min/1E9, xmax=width/1E9, ymax=impact_by_income/1000)) +
    geom_rect(data=fig_df_no_LUC %>% filter(iso3 %in% c("BRA","USA","CHN","ETH")),
              aes(xmin=width_min/1E9, xmax=width/1E9, ymax=impact_by_income/1000,
                  fill = as.factor(iso3))) +
    geom_hline(yintercept = mean_ghgs_discrete/1E3, size =.25, lty=2, color ="gray30")+
    geom_text(x=6, y=mean_ghgs_discrete/1E3+.2, label="mean", color = "gray30", size = 3, check_overlap = T)+
    # ggrepel::geom_text_repel(data=fig_df %>% filter(iso3 %in% c("USA","CHN")),
    #                          aes(x=width_min_t/1E9, y=impact_by_income/1000,
    #                              label = iso3_decile), angle = 90,direction = "y")+
    # geom_text(data=fig_df %>% filter(iso3 %in% c("USA","CHN")),
    #                          aes(x=width_min_t/1E9, y=impact_by_income/1000,
    #                              label = iso3_decile))+
    ylab("Annual food-consumption GHG emissions [Tons CO2e/person]")+
    xlab("Cumulative Population [billions]")+
    scale_y_continuous(n.breaks = 6)+
    scale_x_continuous(n.breaks = 7)+
    scale_fill_manual(values = c('#e41a1c','#377eb8','#4daf4a','#ff7f00'))+
    theme_bw()+
    theme(axis.line.x = element_line(color="black", size = .25), 
          axis.line.y = element_line(color="black", size = .25),
          panel.border = element_blank(),
          panel.grid=element_blank(),
          legend.title=element_blank(),
          # legend.position=c(0.9,.8),
          legend.position="none")
  
  title <- "Lorenz_discrete_deciles_BRA_USA_CHN_ETH_no_LUC"
  ggsave(paste0(getwd(), "/Output/", title, "_SI_FAO_adj_2023.png")
         , width = 120, height = 120, units = "mm"
  )
  
  
  ### Cummulative emissions
  
  ##cumulative emissions
  cum_emi <- fig_df_no_LUC %>% mutate(cum_emi=cumsum(FAO_pop/10*impact_by_income),
                                      cum_income=cumsum(FAO_pop/10*Income)) 
  cum_emi_top <- cum_emi %>% filter(cum_emi<=max(cum_emi)/2)
  ## percentage of people that emit the first 50% of emissions
  max(cum_emi_top$width)/max(fig_df_no_LUC$width)
  ## how much income do they account for?
  max(cum_emi_top$cum_income)/max(cum_emi$cum_income)
  
  
  ## 2.Variable width Means
  
  # kg_co2e_total
  # kg_co2e_excl_luc
  fig_df_mean_no_LUC <- env_impact_df %>%
    filter(attribute=="kg_co2e_excl_luc",JD_Cat=="Grand Total",income_level %in% c("mean")) %>% #group_by(JD_Cat) %>% 
    # arrange(desc(impact_by_income)) %>% 
    arrange(desc(impact_by_income)) %>% 
    mutate(width= cumsum(FAO_pop),
           width_min = width-(FAO_pop),
           width_min_t= width_min+(width - width_min)/2)
  
  ## weighted mean
  mean_ghgs <- weighted.mean(fig_df_mean_no_LUC$impact,fig_df_mean_no_LUC$FAO_pop)
  
  ggplot(fig_df_mean_no_LUC, aes(ymin=0, fill = as.factor(region)))+
    geom_rect(aes(xmin=width_min/1E9, xmax=width/1E9, ymax=impact_by_income/1000)) +
    geom_hline(yintercept = mean_ghgs/1E3, size =.25, lty=2, color ="gray30")+
    geom_text(x=6, y=mean_ghgs/1000+.1, label="mean", color = "gray30", size = 3, check_overlap = T)+
    # ylab("Annual food-consumption GHG emissions [Tons CO2e/person]")+
    # xlab("Cumulative Population [billions]")+
    ylab(NULL)+
    xlab(NULL)+
    # ylim(NA,5.7)+
    scale_y_continuous(n.breaks = 4)+
    scale_x_continuous(n.breaks = 7)+
    scale_fill_manual(values = c('#4daf4a','#e41a1c','#377eb8','#ff7f00','#984ea3'))+
    theme_bw()+
    theme(axis.line.x = element_line(color="black", size = .25), 
          axis.line.y = element_line(color="black", size = .25),
          panel.border = element_blank(),
          panel.grid=element_blank(),
          legend.title=element_blank(),
          legend.position=c(0.9,.8))
  
  title <- "Lorenz_discrete_mean_nice_no_labels_no_LUC"
  ggsave(paste0(getwd(), "/Output/", title, "_SI_FAO_adj_2023.png")
         , width = 4.42, height = 2.9, units = "in"
  )
  
  ## How to get colors in your R object
  # unique(ggplot_build(p)$data[[1]]$fill)
  
  Brazil-US-China
  
  ggplot(fig_df_mean_no_LUC, aes(ymin=0))+
    geom_rect(aes(xmin=width_min/1E9, xmax=width/1E9, ymax=impact_by_income/1000)) +
    geom_rect(data=fig_df_mean_no_LUC %>% filter(iso3 %in% c("BRA","USA","CHN","ETH")),
              aes(xmin=width_min/1E9, xmax=width/1E9, ymax=impact_by_income/1000,
                  fill = as.factor(country))) +
    geom_hline(yintercept = mean_ghgs/1E3, size =.25, lty=2, color ="gray30")+
    geom_text(x=6, y=mean_ghgs/1000+.1, label="mean", color = "gray30", size = 3, check_overlap = T)+
    # ggrepel::geom_text_repel(data=fig_df %>% filter(iso3 %in% c("USA","CHN")),
    #                          aes(x=width_min_t/1E9, y=impact_by_income/1000,
    #                              label = iso3_decile), angle = 90,direction = "y")+
    # geom_text(data=fig_df %>% filter(iso3 %in% c("USA","CHN")),
    #                          aes(x=width_min_t/1E9, y=impact_by_income/1000,
    #                              label = iso3_decile))+
    ylab(NULL)+
    xlab(NULL)+
    scale_y_continuous(n.breaks = 4)+
    scale_x_continuous(n.breaks = 7)+
    scale_fill_manual(values = c('#e41a1c','#377eb8','#4daf4a','#ff7f00'))+
    theme_bw()+
    theme(axis.line.x = element_line(color="black", size = .25), 
          axis.line.y = element_line(color="black", size = .25),
          panel.border = element_blank(),
          panel.grid=element_blank(),
          legend.title=element_blank(),
          # legend.position=c(0.9,.8))#,
          legend.position="none")
  
  title <- "Lorenz_discrete_mean_BRA_USA_CHN_ETH_no_LUC"
  ggsave(paste0(getwd(), "/Output/", title, "_SI_FAO_adj_2023.png")
         , width = 4.42, height = 2.9, units = "in"
  )
  
  
  ## 1.Representative countries a la Ch1
  This figure emulates Ch1s figure so that there is consistency throughout the thesis
  
  title <- "Ch2_World_representatives"
  # fig_df_no_LUC
  
  ## List of representative countries, inspired by Ch1 but a few are chosen because of values and differences in values of emissions and not calories.
  key_coun3 <- c("BRA", "USA", "MEX","ITA","IND","IDN","CHN","JPN",#"HKG","COL"
                 "ZAF", "ETH","NGA","CAF","ZMB","TWN","RUS","DEU","AUS","GHA")
  
  # calculate precentage of total pop they represent within sample and in true total
  key_coun3_df <- fig_df_no_LUC %>% filter(iso3 %in% key_coun3)
  sum(key_coun3_df$FAO_pop)/10/pop_2011_13_avg
  sum(key_coun3_df$FAO_pop)/10/sample_pop
  
  ## filter, reorder, pivot to appropiate format to get top and bottom deciles
  rep_fig_df <- fig_df_no_LUC %>%
    filter(income_level %nin% c("mean","median")) %>% 
    arrange(impact_by_income) %>%
    mutate(country = ifelse(iso3=="TWN","Taiwan",country)) %>%
    select(c(iso3,country,region,income_level,impact_by_income,impact))
  rep_fig_df_wide <- rep_fig_df %>% pivot_wider(names_from= "income_level",
                                                values_from = "impact_by_income")
  rep_fig_df_wide <- rep_fig_df_wide %>% mutate(country = fct_reorder(country, decile10)) ## reorder by decile 10
  ## biggest and smallest difference
  diff_big_small <- rep_fig_df_wide %>% mutate(diff=decile10-decile1)
  rep_fig_df_long <- rep_fig_df_wide %>%
    pivot_longer(cols = starts_with("decile"),
                 names_to = "income_level", values_to = "impact_by_income")
  ## get mean impact to plot
  mean_df <- fig_df_no_LUC %>%
    filter(income_level %in% c("decile10")) %>% #or any decile actually
    select(c(iso3,country,region,income_level, impact)) %>%
    rename(impact_by_income=impact) %>% 
    mutate(country = ifelse(iso3=="TWN","Taiwan",country)) %>%
    mutate(country = as.factor(country),
           income_level = "mean")
  
  ## rbind to plot
  test <- bind_rows(rep_fig_df_long,mean_df)
  
  ## Plot with mean
  ggplot(test %>% filter(income_level %in% c("decile1","mean", "decile10"),
                         iso3 %in% c(key_coun3)),
         aes(x = impact_by_income, y = country))+
    geom_line(alpha=.5, size = .5)+
    geom_vline(xintercept = mean_ghgs_discrete, lty=2, color = "gray60")+
    geom_text(x=mean_ghgs_discrete-550, y=length(key_coun3)-2.4, label="global mean",
              color = "gray60", size = 3, check_overlap = T)+
    geom_point(aes(color = income_level, shape = income_level))+
    ylab(NULL)+ xlab("Annual food-consumption GHG emissions [kg CO2e/person]")+
    scale_colour_manual(name  ="Income level",
                        breaks=c("decile1","mean", "decile10"),
                        labels=c("Poorest 10%","Mean", "Richest 10%")
                        # ,values= c('#fc8d62','gray40','#8da0cb') ## or
                        ,values= c('#E9695F','gray40','#00B2B7')
    )+
    scale_shape_manual(name  ="Income level",
                       breaks=c("decile1","mean", "decile10"),
                       labels=c("Poorest 10%","Mean", "Richest 10%"),
                       values = c(19,4,15))+
    theme_minimal()+
    theme(legend.position="top",
          panel.grid.minor.x = element_blank())
  
  title <- "Ch2_World_representatives"
  # ggsave(paste0(getwd(), "/Output/", title, "_TWN.png")
  #        , width = 6, height = 4.5, units = "in"
  #        )
  ggsave(paste0(getwd(), "/Thesis_figs/", title, "_TWN.png")
         , width = 6, height = 4.5, units = "in"
  )
  
  ## Scatterplot impact vs total calories
  Is this a silly missing point?
    Could also do a facet by food category
  
  # for the mean 
  fig_df_no_LUC %>%
    filter(income_level %in% c("decile10")) %>%
    ggplot(aes(x=Food_Demand, y = impact))+
    geom_point()
  # for the deciles
  fig_df_no_LUC %>%
    filter(income_level %in% c("decile10", "decile1")) %>%
    ggplot(aes(x=GrandT_demand, y = impact_by_income, color= income_level))+
    geom_point(data= fig_df_no_LUC %>%
                 filter(income_level %in% c("decile2")),
               aes(x=Food_Demand, y = impact), color = "gray")+
    geom_point()
  
  ## 8.income vs ghg_intensity
  
  intensity_df <- fig_df_no_LUC %>% mutate(intensity = impact_by_income/GrandT_demand)
  intensity_df %>% 
    ggplot(aes(x=Income, y = intensity,color = region))+
    geom_point()+
    scale_x_log10()+
    ylab("GHG intensity [annual kgCO2e/daily kcal]")+
    theme_minimal()
  
  title <- "GHG_intensity"
  ggsave(paste0(getwd(), "/Output/", title, ".png")
         , width = 6, height = 4.5, units = "in"
  )
  
  
  ghg intensity vs income by food group
  
  
  intensity_fg <- env_impact_df %>%
    mutate(intensity_by_income = ifelse(JD_Cat == "Grand Total",impact_by_income/GrandT_demand,impact_by_income/rescaled),
           intensity = impact/Food_Demand)
  
  #######
  # for representative countries with ggrepel labels 
  #######
  intensity_fg %>% filter(attribute=="kg_co2e_excl_luc",income_level=="mean", iso3 %in% key_coun3) %>% 
    ggplot(aes(x=Income, y = intensity))+
    ggrepel::geom_text_repel(aes(label = iso3), size = 2.2)+
    geom_point(size=1.2)+
    geom_point(data = intensity_fg %>%
                 filter(attribute=="kg_co2e_excl_luc",income_level=="mean",
                        iso3 %nin% key_coun3), alpha =.1, size=1)+
    scale_x_log10()+
    ylim(0,NA)+
    ylab("GHG intensity [annual kgCO2e/daily kcal]")+
    facet_wrap(~JD_Cat,scales="free", ncol=3)+
    theme_bw()
  
  title <- "GHG_intensity_by_food_group_key_coun3"
  s <- 1.5
  ggsave(paste0(getwd(), "/Output/", title, "test5.png")
         , width = 165*s, height = 165*s, units = "mm"
  )
  ######
  # for all countries
  ######
  intensity_fg %>% filter(attribute=="kg_co2e_excl_luc",income_level=="mean") %>%
    ggplot(aes(x=Income, y = intensity,color = region))+
    geom_point()+
    scale_x_log10()+
    ylab("GHG intensity [annual kgCO2e/daily kcal]")+
    facet_wrap(~JD_Cat,scales="free")+
    theme_minimal()
  
  # title <- "GHG_intensity_by_food_group"
  # ggsave(paste0(getwd(), "/Output/", title, ".png")
  #        , width = 6, height = 4.5, units = "in"
  #        )
  
  
  ghg intensity by food group, i.e. kcal vs GHG
  
  intensity_fg <- env_impact_df %>%
    mutate(intensity_by_income = ifelse(JD_Cat == "Grand Total",impact_by_income/GrandT_demand,impact_by_income/rescaled),
           intensity = impact/Food_Demand)
  intensity_fg %>% filter(attribute=="kg_co2e_excl_luc",income_level=="mean",JD_Cat != "Grand Total") %>% 
    ggplot(aes(x=Food_Demand, y = impact,color = region))+
    geom_point()+
    # scale_x_log10()+
    ylab("GHG emissions [annual kgCO2e]")+
    facet_wrap(~JD_Cat,scales="free")+
    theme_minimal()
  
  title <- "GHG_vs_food_demand_by_food_group"
  ggsave(paste0(getwd(), "/Output/", title, ".png")
         , width = 6, height = 4.5, units = "in"
  )
  
  
  
  ## 4.Barplot top 10 decile by category
  
  # decile10 impact to reorder
  decile10_impact <- env_impact_df %>%
    filter(attribute=="kg_co2e_excl_luc",JD_Cat=="Grand Total",income_level %in% c("decile10")) %>% select(iso3,impact_by_income) %>% rename(grand_t_impact_10 = impact_by_income)
  
  bar_plot_df <- env_impact_df %>% left_join(decile10_impact) %>% 
    filter(attribute=="kg_co2e_excl_luc",JD_Cat!="Grand Total",income_level %nin% c("mean")) %>% #group_by(JD_Cat) %>% 
    arrange(desc(grand_t_impact_10)) %>% 
    mutate(country = ifelse(iso3=="TWN","Taiwan",country),
           iso3_decile = paste(iso3,income_level,sep = "_"),
           country = fct_reorder(country, grand_t_impact_10))
  
  bar_plot_df <- bar_plot_df %>% 
    mutate(JD_Cat = factor(JD_Cat,levels = c("Starches","Sugars & Sweeteners","Pulses & Oilseeds","Treenuts","Vegetables","Fruits","Oils & Fats","Meat & Seafood","Dairy & Eggs")))
  bar_plot_df$JD_Cat <- factor(bar_plot_df$JD_Cat, levels = rev(levels(bar_plot_df$JD_Cat)))
  ## assigning proper fixed colors to each food group
  library(RColorBrewer)
  #library(ggrepel)
  myColors <- rev(c('#a6cee3','#1f78b4','#6a3d9a','#cab2d6','#b2df8a','#33a02c','#fb9a99','#e31a1c','#ff7f00'))#'#fdbf6f',
  names(myColors) <- levels(bar_plot_df$JD_Cat)
  colScale <- scale_colour_manual(name = "",values = myColors)
  fillScale <- scale_fill_manual(name = "",values = myColors)
  
  #impacts
  (imp <- bar_plot_df %>% filter(iso3 %in% key_coun3, income_level%in% c("decile10"),JD_Cat!="Grand Total") %>%
      ggplot(aes(y=country,x=impact_by_income,fill = JD_Cat), color =NA)+
      geom_col()+
      scale_y_discrete(name = "", labels = NULL)+
      xlab("Annual GHG emissions [kg CO2e/person]")+
      xlim(0,6800)+
      # coord_flip()+
      fillScale+#colScale+
      theme_minimal()+
      theme(legend.position=c(.8,.4))+
      labs(subtitle = " ")
  )
  #calories
  (cal <- bar_plot_df %>% filter(iso3 %in% key_coun3, income_level=="decile10",JD_Cat!="Grand Total") %>%
      ggplot(aes(y=country,x=rescaled,fill = JD_Cat), color =NA)+
      geom_col()+
      # coord_flip()+
      scale_x_reverse(limits = c(5000,0))+
      xlab("Daily Food Demand [kcal/capita]")+
      # xlim(0,5000)+
      scale_y_discrete(name = "", position = "right")+
      fillScale+#colScale+
      theme_minimal()+
      theme(legend.position="none",
            axis.text.y.right=element_text(hjust=0.5))+
      labs(subtitle = "Richest 10%")
    # labs(subtitle = "Poorest 10%")
  )
  grid_p <-gridExtra::grid.arrange(cal, imp, ncol = 2, widths = c(100*4/7, 100*3/7))
  
  title <- "World_representatives_decile10_bar"
  # ggsave(paste0(getwd(), "/Output/", title, "_TWN_2023.png"),plot = grid_p 
  #        , width = 9, height = 4.5, units = "in"
  #        )
  
  Update disaggregating the meats category
  
  Get proportion of beef emissions from meat
  
  kg_co2e_excl_luc
  
  # Bovine meat separate
  Kim_JD_cat <- data.frame(Kim_cat=unique(impacts_group$output_group),
                           JD_Cat=c("Meat & Seafood","Bovine meat",
                                    rep("Dairy & Eggs",2),"Fruits","Starches",NA,"Treenuts",
                                    rep("Meat & Seafood",3),
                                    "Pulses & Oilseeds","Meat & Seafood","Starches",
                                    "Sugars & Sweeteners","Oils & Fats","Vegetables"),
                           ani_veg=c(rep("Animal",4),rep("Vegetable",4),rep("Animal",3),
                                     "Vegetable","Animal",rep("Vegetable",4)))
  
  
  
  ## Data to calculate total emissions
  Kims_pop <- read_csv("Data_env/fao_population.csv")
  impacts_country <- read_csv("Data_env/diet_footprints_by_country_diet.csv")
  impacts_country <- impacts_country %>%
    filter(diet == "baseline",attribute%in% c("kg_co2e_excl_luc","kg_co2e_total")) %>% 
    left_join(Kims_pop)
  ## Kims total impacts
  total_impacts <- impacts_country %>% group_by(attribute) %>% 
    summarise(total_impact = sum(value*population))
  # total_impacts
  Kims_non_LUC_CO2_total <- total_impacts[[1,2]]
  Kims_LUC_CO2_total <- total_impacts[[2,2]]-total_impacts[[1,2]]
  # impact per capita world
  Kims_non_LUC_CO2_total/(sum(impacts_country$population)/2) ## used in MS, maybe should also be used in the figures? one is 1212.92 the other 1211.25 so not that significant and also imperceptible in figures.
  # impact by group
  impacts_group_co2e <- impacts_group %>%
    filter(diet == "baseline",attribute%in% c("kg_co2e_excl_luc")) %>% 
    left_join(Kims_pop) %>% left_join(Kim_JD_cat, by = c("output_group"="Kim_cat"))
  
  total_impacts_by_fg <- impacts_group_co2e %>% group_by(JD_Cat,attribute, country) %>% 
    summarise(total_impact = sum(value*population))
  
  total_impacts_by_fg_iso3 <- impacts_group_co2e %>% group_by(JD_Cat,attribute) %>% 
    summarise(total_impact = sum(value*population))
  
  ## check of consistency between aggregations
  total_impacts_by_attribute <- impacts_group_co2e %>% group_by(attribute) %>% 
    summarise(total_impact = sum(value*population))
  
  baseline <- impacts_group %>% filter(diet == "baseline",attribute%in% c("kg_co2e_excl_luc"))
  baseline_JD_cat <- left_join(baseline,Kim_JD_cat, by = c("output_group"="Kim_cat"))
  by_country <- baseline_JD_cat %>% group_by(country_code,country,attribute,JD_Cat) %>% summarise(impact=sum(value)) %>%
    ungroup() %>%
    mutate(iso3= ifelse(country_code==41,"CHN",countrycode(country_code,"fao","iso3c"))) %>% drop_na()
  
  # calculate bovine meat proportion of total and of meat
  by_country_bovine <- by_country %>% filter(JD_Cat=="Bovine meat") %>% 
    select(iso3,country, impact) %>% rename(bovine_impact= impact)
  by_country_bovine_prop <- left_join(by_country, by_country_bovine, by = c("country", "iso3")) %>%
    filter(JD_Cat %in% c("Meat & Seafood")) %>% 
    mutate(bovine_prop=bovine_impact/(impact+bovine_impact))
  # looking at the proportion for all food groups might be interesting in itself but the reason I did this was to calculate proportion of meat emissions that are bovine.
  by_country_bovine_prop <- by_country_bovine_prop %>% 
    filter(JD_Cat %in% c("Meat & Seafood")) %>% 
    select(iso3,bovine_prop)
  
  
  ### Updated figure
  This one aims to split beet out of the Meats category
  
  env_impact_df <- readRDS("Data/Ch2_env_impact_df_2023.rds")
  
  # decile10 impact to reorder
  decile10_impact <- env_impact_df %>%
    filter(attribute=="kg_co2e_excl_luc",JD_Cat=="Grand Total",income_level %in% c("decile10")) %>% select(iso3,impact_by_income) %>% rename(grand_t_impact_10 = impact_by_income)
  # Split Meat & Seafood into Bovine meat and Other Meats & Seafood
  
  env_impact_df_prop <- env_impact_df %>% 
    filter(attribute=="kg_co2e_excl_luc",JD_Cat == "Meat & Seafood") %>% 
    left_join(by_country_bovine_prop)
  
  env_impact_df_bov <- env_impact_df_prop %>% 
    mutate(impact_by_income = impact_by_income*bovine_prop,
           JD_Cat="Bovine Meat") %>% select(-bovine_prop)
  
  env_impact_df_other_meat <- env_impact_df_prop %>% 
    mutate(impact_by_income = impact_by_income*(1-bovine_prop),
           JD_Cat="Other Meats & Seafood") %>% select(-bovine_prop)
  
  env_impact_df_sans_meat <- env_impact_df %>% 
    filter(attribute=="kg_co2e_excl_luc",JD_Cat != "Meat & Seafood")
  
  env_impact_df <- bind_rows(env_impact_df_sans_meat,
                             env_impact_df_bov,
                             env_impact_df_other_meat)
  
  bar_plot_df <- env_impact_df %>% left_join(decile10_impact) %>% 
    filter(attribute=="kg_co2e_excl_luc",JD_Cat!="Grand Total",income_level %nin% c("mean")) %>% #group_by(JD_Cat) %>% 
    arrange(desc(grand_t_impact_10)) %>% 
    mutate(country = ifelse(iso3=="TWN","Taiwan",country),
           iso3_decile = paste(iso3,income_level,sep = "_"),
           country = fct_reorder(country, grand_t_impact_10))
  
  bar_plot_df <- bar_plot_df %>% 
    mutate(JD_Cat = factor(JD_Cat,levels = c("Starches","Sugars & Sweeteners","Pulses & Oilseeds","Treenuts","Vegetables","Fruits","Oils & Fats","Bovine Meat","Other Meats & Seafood","Dairy & Eggs")))
  bar_plot_df$JD_Cat <- factor(bar_plot_df$JD_Cat, levels = rev(levels(bar_plot_df$JD_Cat)))
  ## assigning proper fixed colors to each food group
  library(RColorBrewer)
  #library(ggrepel)
  myColors <- rev(c('#a6cee3','#1f78b4','#6a3d9a','#cab2d6','#b2df8a','#33a02c','#fb9a99','#B22222','#FFA07A','#fdbf6f'))#'#fdbf6f',
  names(myColors) <- levels(bar_plot_df$JD_Cat)
  colScale <- scale_colour_manual(name = "",values = myColors)
  fillScale <- scale_fill_manual(name = "",values = myColors)
  
  #impacts
  (imp <- bar_plot_df %>% filter(iso3 %in% key_coun3, income_level%in% c("decile10"),JD_Cat!="Grand Total") %>%
      ggplot(aes(y=country,x=impact_by_income,fill = JD_Cat), color =NA)+
      geom_col()+
      scale_y_discrete(name = "", labels = NULL)+
      xlab("Annual GHG emissions [kg CO2e/person]")+
      xlim(0,6800)+
      # coord_flip()+
      fillScale+#colScale+
      theme_minimal()+
      theme(legend.position=c(.8,.4))+
      labs(subtitle = " ")
  )
  #calories
  # RESUME HERE: I need to figure out the calories counter part.
  (cal <- bar_plot_df %>% filter(iso3 %in% key_coun3, income_level=="decile10",JD_Cat!="Grand Total") %>%
      ggplot(aes(y=country,x=rescaled,fill = JD_Cat), color =NA)+
      geom_col()+
      # coord_flip()+
      scale_x_reverse(limits = c(5000,0))+
      xlab("Daily Food Demand [kcal/capita]")+
      # xlim(0,5000)+
      scale_y_discrete(name = "", position = "right")+
      fillScale+#colScale+
      theme_minimal()+
      theme(legend.position="none",
            axis.text.y.right=element_text(hjust=0.5))+
      labs(subtitle = "Richest 10%")
    # labs(subtitle = "Poorest 10%")
  )
  grid_p <-gridExtra::grid.arrange(cal, imp, ncol = 2, widths = c(100*4/7, 100*3/7))
  
  title <- "World_representatives_decile10_bar"
  # ggsave(paste0(getwd(), "/Output/", title, "_TWN_2025.png"),plot = grid_p 
  #        , width = 9, height = 4.5, units = "in"
  #        )
  
  
  
  mutate(Food_group = factor(Food_group,levels = c("Other", "Sugars", "Starches", "Pulses", "Treenuts", "Veggies", "Fruits", "Alternative milks", "Oils","Stimulants", "Eggs", "Dairy", "Poultry", "Pork", "Beef", "Seafood")))
  
  color_palette <- c(
    "Other" = "#D3D3D3",
    "Sugars" = "#800080",
    "Starches" = "#F4A460",
    "Pulses" = "#D2691E",
    "Treenuts" = "#FFD700",
    "Veggies" = "#32CD32",
    "Fruits" = '#b2df8a',#'#33a02c',"#98FB98",
    "Alternative milks" = "#ADD8E6",
    "Oils" = "#87CEFA",
    "Stimulants" = "#00CED1",
    "Eggs" = "#fdbf6f",
    "Dairy" = "#FFFACD",
    "Poultry" = "#FFA07A",
    "Pork" = "#FFC0CB",
    "Beef" = "#B22222",
    "Seafood" = "#FF6347"
  )
  sumarised$Food_group <- factor(sumarised$Food_group, levels = rev(levels(sumarised$Food_group)))
  
  
  
  # The following is an effort to bring both deciles into the same figure but it is visually somewhat unappealing
  
  (imp <- bar_plot_df %>% filter(iso3 %in% key_coun3, income_level%in% c("decile10","decile1"),JD_Cat!="Grand Total") %>%
      mutate(country = fct_rev(country)) %>% 
      ggplot(aes(y=income_level,x=impact_by_income,fill = JD_Cat), color =NA)+
      geom_col()+
      facet_wrap(~country, ncol=1,strip.position = "left")+
      scale_y_discrete(name = "", labels=c( "Poorest 10%","Richest 10%"), position = "right")+
      xlab("Annual GHG emissions [kg CO2e/person]")+
      xlim(0,6800)+
      # annotate("label", x = 4, y = 10, label = "test")+ 
      # scale_x_reverse()+
      # coord_flip()+
      fillScale+#colScale+
      theme_minimal()+
      theme(legend.position=c(.8,.4),
            strip.text.y = element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            panel.grid.major.y = element_blank())+
      labs(subtitle = " ")
  )
  (cal <- bar_plot_df %>% filter(iso3 %in% key_coun3, income_level%in% c("decile10","decile1"),JD_Cat!="Grand Total") %>%
      mutate(country = fct_rev(country)) %>% 
      ggplot(aes(y=income_level,x=rescaled,fill = JD_Cat), color =NA)+
      geom_col()+
      facet_wrap(~country, ncol=1,strip.position = "right")+
      scale_y_discrete(name = "", labels=c( "Poorest 10%","Richest 10%"))+
      xlab("Daily Food Demand [kcal/capita]")+
      # xlim(0,6800)+
      scale_x_reverse(limits = c(5000,0))+
      fillScale+#colScale+
      theme_minimal()+
      theme(legend.position="none",
            strip.text.y = element_text(angle = 0),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            panel.grid.major.y = element_blank())+
      labs(subtitle = " ")
  )
  grid_p <-gridExtra::grid.arrange(cal, imp, ncol = 2, widths = c(100*4/7, 100*3/7))
  
  title <- "World_representatives_both_deciles_bar"
  ggsave(paste0(getwd(), "/Output/", title, "_TWN_2025_v2.png"),plot = grid_p
         , width = 9, height = 9, units = "in"
  )
  
  ## Continuous Lorentz Line
  
  # kg_co2e_total
  fig_df_cont_no_LUC <- env_impact_df %>%
    filter(attribute=="kg_co2e_excl_luc",JD_Cat=="Grand Total",income_level %nin% c("mean")) %>% #group_by(JD_Cat) %>% 
    # arrange(desc(impact_by_income)) %>% 
    arrange(desc(impact_by_income)) %>% 
    mutate(Country_decile = paste(country,income_level,sep = "_"),
           iso3_decile = paste(iso3,income_level,sep = "_"),
           Country_decile = fct_reorder(Country_decile, impact_by_income,.desc = T),
           iso3_decile = fct_reorder(iso3_decile, impact_by_income,.desc = T),
           width= cumsum(FAO_pop/10),
           width_min = width-(FAO_pop/10),
           width_min_t= width_min+(width - width_min)/2)
  
  # kg_co2e_excl_luc
  fig_df_excl_luc <- env_impact_df %>%
    filter(attribute=="kg_co2e_excl_luc",JD_Cat=="Grand Total",income_level %nin% c("mean")) %>% #group_by(JD_Cat) %>% 
    # arrange(desc(impact_by_income)) %>% 
    arrange(desc(impact_by_income)) %>% 
    mutate(Country_decile = paste(country,income_level,sep = "_"),
           iso3_decile = paste(iso3,income_level,sep = "_"),
           Country_decile = fct_reorder(Country_decile, impact_by_income,.desc = T),
           iso3_decile = fct_reorder(iso3_decile, impact_by_income,.desc = T),
           width= cumsum(FAO_pop/10),
           width_min = width-(FAO_pop/10),
           width_min_t= width_min+(width - width_min)/2)
  
  floor <- 1
  ceiling <- 2.5
  excess <- fig_df_cont %>% filter(impact_by_income/1000>=ceiling)
  lacking <- fig_df_cont %>% filter(impact_by_income/1000<=floor)
  ggplot(fig_df_cont, aes(y= impact_by_income/1000, x = width_min_t/1E9))+
    # geom_smooth(se=F)+
    geom_ribbon(data=excess, aes(ymin=ceiling,ymax=impact_by_income/1000), fill="red", alpha=0.5) +
    geom_ribbon(data=lacking, aes(ymin=0,ymax=impact_by_income/1000), fill="red", alpha=0.5) +
    geom_ribbon(data=lacking, aes(ymin=impact_by_income/1000,ymax=floor), fill="green", alpha=0.5) +
    geom_line()+
    geom_hline(yintercept = c(floor,ceiling), color = "gray50",lty=2)+
    # geom_line(data=fig_df_excl_luc, aes(y= impact_by_income/1000, x = width_min_t/1E9),color="red")+
    ylab("Annual food-consumption GHG emissions [Tons CO2e/person]")+
    xlab("Cumulative Population [billions]")+
    theme_bw()#+
  # facet_wrap(~JD_Cat,scales = "free")
  
  title <- "Lorenz_continuous_excl_luc"
  # ggsave(paste0(getwd(), "/Output/", title, ".png")
  #        #, width = 200, height = 300, units = "mm"
  #        )
  
  ## share GHG vs Share pop
  
  sample_emi_no_luc <-sum(fig_df_cont_no_LUC$FAO_pop/10*fig_df_cont_no_LUC$impact_by_income)
  test_df_2 <- fig_df_cont_no_LUC %>% 
    mutate(cum_share_pop= cumsum(FAO_pop/10)/sample_pop,
           cum_share_ghg= cumsum(FAO_pop*impact_by_income/10)/sample_emi_no_luc)
  
  test_df <- fig_df_cont_no_LUC %>% 
    mutate(cum_share_pop= abs(cumsum(FAO_pop/10)/sample_pop-1),
           cum_share_ghg= abs(cumsum(FAO_pop*impact_by_income/10)/sample_emi_no_luc-1))
  
  test_df %>% ggplot(aes(x=cum_share_pop,y=cum_share_ghg))+
    geom_point()
  
# 70% of emissions come from the top 50%
#   50% of emissions come from the top 30%
#   30% of emissions come from the top 15% or the same as the bottom 50%
# 22% of emissions come from the top 10%
#   13% of emissions come from the top 5%
#   3.5% of emissions come from the top 1%
    
    
    (Not used) Get total emissions from graph
  
  total_emi <- sum(fig_df_cont$FAO_pop/10*fig_df_cont$impact_by_income)
  sample_emi_no_luc <-sum(fig_df_cont_no_LUC$FAO_pop/10*fig_df_cont_no_LUC$impact_by_income) 
  
  
  ## Chakravarty method
  ### summing up to Eat lancet in 2011-2013
  
  ceiling <- 944.3 ## found by trial and error, is there abetter way?
  eat <- fig_df_cont_no_LUC %>% arrange(impact_by_income) %>%
    mutate(impact_by_income=ifelse(impact_by_income>ceiling,ceiling,impact_by_income),
           target=cumsum(FAO_pop/10*impact_by_income))
  ## see what it sums up to
  max(eat$target)
  # > 4.999739e+12
  
  
#### bringing poor people up
# or rather out of mediocre diet up at least to what they'l be allowed by 2050, i.e. 513.6kg CO2e
# another example is Leach recomended diet*Poore and nemececk impact factors (from Silvia's work)
3.038 kg/day = 1.108 tons/year

## 513.6kg CO2e
floor <- 513.6
ceiling_poor <- 923.2 ## again found by trial and error, is there abetter way?
## the diff between ceiling and ceiling_poor is so tiny it is not perceptible on graph
eat_poor <- fig_df_cont_no_LUC %>% arrange(impact_by_income) %>%
 mutate(impact_by_income=ifelse(impact_by_income>ceiling_poor,ceiling_poor,impact_by_income),
        impact_by_income=ifelse(impact_by_income<floor,floor,impact_by_income),
        target=cumsum(FAO_pop/10*impact_by_income))

## see what it sums up to
max(eat_poor$target)
# > 4.999802e+12


#### Fig Chakravarty


# floor <- 1108
excess <- fig_df_cont_no_LUC %>% filter(impact_by_income>=ceiling)
excess_poor <- fig_df_cont_no_LUC %>% filter(impact_by_income>=ceiling_poor) %>%
 mutate(impact_by_income=ifelse(impact_by_income>=ceiling, ceiling,impact_by_income))
lacking <- fig_df_cont_no_LUC %>% filter(impact_by_income<=floor)

ggplot(fig_df_cont_no_LUC, aes(y= impact_by_income/1000, x = width_min_t/1E9))+
 # geom_smooth(se=F)+
 geom_ribbon(data=excess, aes(ymin=ceiling/1000,ymax=impact_by_income/1000),
             fill="blue", alpha=0.5) +
 geom_ribbon(data=excess_poor, aes(ymin=ceiling_poor/1000,ymax=impact_by_income/1000),
             fill="red", alpha=0.5) +
 geom_ribbon(data=lacking, aes(ymin=0,ymax=impact_by_income/1000), fill="green", alpha=0.5) +
 geom_ribbon(data=lacking, aes(ymin=impact_by_income/1000,ymax=floor/1000), fill="red", alpha=0.5) +
 geom_line()+
 # geom_hline(yintercept = c(floor/1000,ceiling/1000,ceiling_poor/1000), color = "gray50",lty=2)+
 # geom_line(data=fig_df_excl_luc, aes(y= impact_by_income/1000, x = width_min_t/1E9),color="red")+
 ylab("Food consumption GHG emissions [Tons CO2e/person/year]")+
 xlab("Cumulative Population [billions]")+
 scale_y_continuous(n.breaks = 7)+
 scale_x_continuous(n.breaks = 7)+
 # ylim(0,3)+
 theme_bw()+
 # theme(panel.grid.minor.x=element_blank())
 theme(axis.line.x = element_line(color="black", size = .25), 
       axis.line.y = element_line(color="black", size = .25),
       panel.border = element_blank(),
       panel.grid=element_blank())

title <- "True_chakravarty_style_no_LUC"
# ggsave(paste0(getwd(), "/Output/", title, ".png")
#        , width = 120, height = 120, units = "mm"
#        )

#### Counts and %

# People exceeding the 5Gt limit.
# count
max(excess$width)
# percentage
max(excess$width)/sample_pop

# People below min Lancet healthy diet in 2050
# count
(max(lacking$width)-min(lacking$width_min))
# percentage
(max(lacking$width)-min(lacking$width_min))/sample_pop

# People below 2013 limit and above 2050 limit
# count
(sample_pop-max(excess$width)-(max(lacking$width)-min(lacking$width_min)))
# Percentages
(sample_pop-max(excess$width)-(max(lacking$width)-min(lacking$width_min)))/sample_pop

# Of those people eating diets that meet the target, X percent eat less than 2050 5Gt/population target
(max(lacking$width)-min(lacking$width_min))/(sample_pop-max(excess$width)-(max(lacking$width)-min(lacking$width_min)))

# Same but with poor.
max(excess_poor$width)/sample_pop


### summing up to % of Eat lancet in 2011-2013

## all emissions from Kim - Eat Lancet 5Gt
target_perc <- .7
(8.25-5)*(1-target_perc)
5+(8.25-5)*(1-target_perc)
# > 5.975 Gt is the objective

## all non_LUC emissions from Kim - Eat Lancet 5Gt
target_perc <- .7
(7.48-5)*(1-target_perc)
5+(7.48-5)*(1-target_perc)
# (FAOs_non_LUC_CO2_total/1E12-5)*(1-target_perc)
# 5+(FAOs_non_LUC_CO2_total/1E12-5)*(1-target_perc)
# > 5.744 Gt is the objective without FAO_GHG scaling
# 5.627873 Gt is the objective with FAO_GHG scaling
objective[3]
# ceiling <- 1179.3 ## found by trial and error, is there abetter way? A for loop
# ceiling <- 1181.2
ceiling <- ceiling_vector[3]
eat <- fig_df_cont_no_LUC %>% arrange(impact_by_income) %>%
 mutate(impact_by_income=ifelse(impact_by_income>ceiling,ceiling,impact_by_income),
        target=cumsum(FAO_pop/10*impact_by_income))
## see what it sums up to
max(eat$target)
# > 5.743769e+12
max(eat$target)<objective[3]


#### bringing poor people up
# or rather out of mediocre diet up at least to what they'l be allowed by 2050, i.e. 513.6kg CO2e# another example is Leach recomended diet*Poore and nemececk impact factors (from Silvia's work)
3.038 kg/day = 1.108 tons/year

## all non_LUC emissions from Kim - Eat Lancet 5Gt
target_perc <- .7
# (7.48-5)*(1-target_perc)
# 5+(7.48-5)*(1-target_perc)
(FAOs_non_LUC_CO2_total/1E12-5)*(1-target_perc)
5+(FAOs_non_LUC_CO2_total/1E12-5)*(1-target_perc)
# 5.627873 Gt is the objective with FAO_GHG scaling

## 513.6kg CO2e is the result of 5Gt/2050 UN pop estimate
floor <- 513.6
ceiling_poor <- ceiling_poor_vector[3]#-.1
# ceiling_poor <- 1153.2 ## again found by trial and error, is there abetter way?
## the diff between ceiling and ceiling_poor is so tiny it is not perceptible on graph
eat_poor <- fig_df_cont_no_LUC %>% arrange(impact_by_income) %>%
  mutate(impact_by_income=ifelse(impact_by_income>ceiling_poor,ceiling_poor,impact_by_income),
         impact_by_income=ifelse(impact_by_income<floor,floor,impact_by_income),
         target=cumsum(FAO_pop/10*impact_by_income))

## see what it sums up to
max(eat_poor$target)
# > 5.743757e+12
max(eat_poor$target)<objective[3]



#### Fig Chakravarty


# floor <- 1108
excess <- fig_df_cont_no_LUC %>% filter(impact_by_income>=ceiling)
excess_poor <- fig_df_cont_no_LUC %>% filter(impact_by_income>=ceiling_poor) %>%
  mutate(impact_by_income=ifelse(impact_by_income>=ceiling, ceiling,impact_by_income))
lacking <- fig_df_cont_no_LUC %>% filter(impact_by_income<=floor)

ggplot(fig_df_cont_no_LUC, aes(y= impact_by_income/1000, x = width_min_t/1E9))+
  # geom_smooth(se=F)+
  geom_ribbon(data=excess, aes(ymin=ceiling/1000,ymax=impact_by_income/1000),
              fill="blue", alpha=0.5) +
  geom_ribbon(data=excess_poor, aes(ymin=ceiling_poor/1000,ymax=impact_by_income/1000),
              fill="red", alpha=0.5) +
  geom_ribbon(data=lacking, aes(ymin=0,ymax=impact_by_income/1000), fill="green", alpha=0.5) +
  geom_ribbon(data=lacking, aes(ymin=impact_by_income/1000,ymax=floor/1000), fill="red", alpha=0.5) +
  geom_line()+
  # geom_hline(yintercept = c(floor/1000,ceiling/1000,ceiling_poor/1000), color = "gray50",lty=2)+
  # geom_line(data=fig_df_excl_luc, aes(y= impact_by_income/1000, x = width_min_t/1E9),color="red")+
  ylab("Food consumption GHG emissions [Tons CO2e/person/year]")+
  xlab("Cumulative Population [billions]")+
  scale_y_continuous(n.breaks = 7)+
  scale_x_continuous(breaks = 0:6, limits = c(-1,7))+
  # ylim(0,3)+
  theme_bw()+
  # theme(panel.grid.minor.x=element_blank())
  theme(axis.line.x = element_line(color="black", size = .25), 
        axis.line.y = element_line(color="black", size = .25),
        panel.border = element_blank(),
        panel.grid=element_blank())

title <- "True_chakravarty_style_no_LUC"
ggsave(paste0(getwd(), "/Output/", title, "_long_text_FAO_adj.png")
       , width = 140, height = 120, units = "mm"
)

#### Counts and %

# People exceeding the 5Gt limit and inverse values
# count
max(excess$width)
# percentage
max(excess$width)/sample_pop

sample_pop-max(excess$width)
(sample_pop-max(excess$width))/sample_pop

# People below min Lancet healthy diet in 2050
# count
(max(lacking$width)-min(lacking$width_min))
# percentage
(max(lacking$width)-min(lacking$width_min))/sample_pop

# People below 2013 limit and above 2050 limit
# count
(sample_pop-max(excess$width)-(max(lacking$width)-min(lacking$width_min)))
# Percentages
(sample_pop-max(excess$width)-(max(lacking$width)-min(lacking$width_min)))/sample_pop

# Of those people eating diets that meet the target, X percent eat less than 2050 5Gt/population target
(max(lacking$width)-min(lacking$width_min))/(sample_pop-max(excess$width)-(max(lacking$width)-min(lacking$width_min)))

# Same but with poor.
max(excess_poor$width)/sample_pop
1-max(excess_poor$width)/sample_pop


# People by region that are in world's top 10% of emitters

top_10_ghgs <- fig_df_no_LUC %>% filter(width<sample_pop/10) %>% 
  mutate(WB_region = countrycode::countrycode(iso3,"iso3c","region23"))
top_10_ghgs %>% group_by(region) %>% summarise(share = sum(FAO_pop/10)/(sample_pop/10))

# by country
by_country_top_10_ghgs <-  top_10_ghgs %>% group_by(country) %>% summarise(share = sum(FAO_pop/10)/(sample_pop/10))
#test
sum(by_country_top_10_ghgs$share)

# by WB region
top_10_ghgs <- fig_df_no_LUC %>% filter(width<sample_pop/10) %>% 
  mutate(WB_region = countrycode::countrycode(iso3,"iso3c","region23"))
by_region_top_10_ghgs <- top_10_ghgs %>% group_by(WB_region) %>% summarise(share = sum(FAO_pop/10)/(sample_pop/10))
#test
sum(by_region_top_10_ghgs$share)


#test european pop
fig_df_no_LUC %>% group_by(region) %>% summarise(share = sum(FAO_pop/10)/(sample_pop))



### Gini

# global gini
acid::weighted.gini(fig_df_cont_no_LUC$impact_by_income, fig_df_cont_no_LUC$FAO_pop)$bcwGini
# between country Gini
acid::weighted.gini(fig_df_cont_no_LUC$impact, fig_df_cont_no_LUC$FAO_pop)$bcwGini
# average within country Gini
within_ineq_ghgs <- fig_df_cont_no_LUC %>% group_by(iso3) %>% 
  summarise(gini_decile_impact = acid::gini(impact_by_income)$bcGini,
            pop = mean(FAO_pop))
weighted.mean(within_ineq_ghgs$gini_decile_impact,within_ineq_ghgs$pop)## this might actually just be the same inequality from paper 1 but changes because I study a subset of countries. But all in all I just multiplied each decile by a constant within countries so the actual inequality within countries should not change.

#extras
## Figure that summarizes all assumptions
# The following chunk somewhat repeats a lot of the work from the previous skript. I think the code for this chapter is somehwat inefficient in that regard, it needs a lot of cleaning. I also need to have choices made, which makes replicating the results more difficult.

impacts_item <- read_csv("Data_env/item_footprints_per_unit.csv")

impacts_group <- read_csv("Data_env/diet_footprints_by_country_diet_output_group.csv")
Kim_JD_cat <- data.frame(Kim_cat=unique(impacts_group$output_group),
                        JD_Cat=c(rep("Meat & Seafood",2),rep("Dairy & Eggs",2),"Fruits","Starches",NA,"Treenuts",
                                 rep("Meat & Seafood",3),"Pulses & Oilseeds","Meat & Seafood","Starches",
                                 "Sugars & Sweeteners","Oils & Fats","Vegetables"))
## Offals is matched to Meat and Seafood, don't think this is a major problem it could also be Oils and Fats category but I am being consistent with what I did in the JD_basicFood_Mapping.csv file.

Kim_JD_cat <- data.frame(Kim_cat=unique(impacts_group$output_group),
                         JD_Cat=c(rep("Meat & Seafood",2),
                                  rep("Dairy & Eggs",2),"Fruits","Starches",NA,"Treenuts",
                                  rep("Meat & Seafood",3),
                                  "Pulses & Oilseeds","Meat & Seafood","Starches",
                                  "Sugars & Sweeteners","Oils & Fats","Vegetables"),
                         ani_veg=c(rep("Animal",4),rep("Vegetable",4),rep("Animal",3),
                                   "Vegetable","Animal",rep("Vegetable",4)))

## Data to calculate total emissions
Kims_pop <- read_csv("Data_env/fao_population.csv")
impacts_country <- read_csv("Data_env/diet_footprints_by_country_diet.csv")
impacts_country <- impacts_country %>%
  filter(diet == "baseline",attribute%in% c("kg_co2e_excl_luc","kg_co2e_total")) %>% 
  left_join(Kims_pop)
## Kims total impacts
total_impacts <- impacts_country %>% group_by(attribute) %>% 
  summarise(total_impact = sum(value*population))
total_impacts
Kims_non_LUC_CO2_total <- total_impacts[[1,2]]
Kims_LUC_CO2_total <- total_impacts[[2,2]]-total_impacts[[1,2]]

# impacts_group_co2e <- impacts_group %>%
#   filter(diet == "baseline",attribute%in% c("kg_co2e_excl_luc","kg_co2e_total")) %>% 
#   left_join(Kims_pop) %>% left_join(Kim_JD_cat, by = c("output_group"="Kim_cat"))
# 
# total_impacts_by_fg <- impacts_group_co2e %>% group_by(JD_Cat,attribute) %>% 
#   summarise(total_impact = sum(value*population))
# 
# total_impacts_by_ani_veg <- impacts_group_co2e %>% group_by(ani_veg,attribute) %>% 
#   summarise(total_impact = sum(value*population))

# ## check of consistency between aggregations
# total_impacts_by_attribute <- impacts_group_co2e %>% group_by(attribute) %>% 
#   summarise(total_impact = sum(value*population))

## Group by JD_Cat
baseline <- impacts_group %>% filter(diet == "baseline")
baseline_JD_cat <- left_join(baseline,Kim_JD_cat, by = c("output_group"="Kim_cat"))
by_country <- baseline_JD_cat %>% group_by(country_code,country,attribute,JD_Cat) %>% summarise(impact=sum(value)) %>%
  ungroup() %>%
  mutate(iso3= ifelse(country_code==41,"CHN",countrycode(country_code,"fao","iso3c")))


## FAO Adjust
# import data
FAO_CO2_data <- read.csv("Data/FAOSTAT_Ag_lands_CO2eq.csv")
# select relevant columns
FAO_CO2_data <- FAO_CO2_data %>% select(Element,Item,Year,Value, Unit)
# get the 3 year average
FAO_CO2_data_2011_13 <- FAO_CO2_data %>% group_by(Element,Item,Unit) %>% 
  summarise(CO2eq_ton = mean(Value)*1E6)
# get grand total CO2eq only
FAO_CO2_data_2011_13_totals <- FAO_CO2_data_2011_13 %>% filter(Element=="Emissions (CO2eq) (AR5)")
FAOs_non_LUC_CO2_total <- FAO_CO2_data_2011_13_totals[[1,4]]
FAOs_LUC_CO2_total <- FAO_CO2_data_2011_13_totals[[2,4]]
#calculate ratio by dividing by the total data form Kim
non_LUC_ratio <- FAOs_non_LUC_CO2_total/Kims_non_LUC_CO2_total
LUC_ratio <- FAOs_LUC_CO2_total/Kims_LUC_CO2_total

## get a LUC column on Kim"s data
LUC <- by_country %>%filter(attribute %in% c("kg_co2e_total","kg_co2e_excl_luc")) %>% 
  pivot_wider(names_from = attribute, values_from = impact) %>%
  mutate(LUC = kg_co2e_total-kg_co2e_excl_luc,
         kg_co2e_excl_luc = kg_co2e_excl_luc*non_LUC_ratio,
         LUC = LUC*LUC_ratio,
         kg_co2e_total = LUC+kg_co2e_excl_luc)
LUC_long <- LUC %>% pivot_longer(cols = c(kg_co2e_total,kg_co2e_excl_luc), names_to = "attribute", values_to = "impact") %>% select(-LUC)

## NOTE: this adjustment is only for the 2 categories that I have used in the remainder of the pipeline. CO2 subcategories are not adjusted so if I were to use them for analysis or paper figures then I should do so above here

# update Kim's data based on the adjustment to FAO climate data
by_country_FAO_adj <- by_country %>%filter(attribute %nin% c("kg_co2e_total","kg_co2e_excl_luc")) %>%
  bind_rows(LUC_long)

bias_adjusted <- readRDS("Data/bias_adjusted_iter_ch3.rds")

scam_rescaled <-bias_adjusted %>% rename(rescaled_prebias = rescaled, GrandT_demand_prebias=GrandT_demand) %>%  rename(rescaled=temp_d_fit, GrandT_demand = GrandT_demand_bias)

## get the three year average like Kim et al 2011-2013
three_y_avg <- scam_rescaled %>% filter(Year %in% c(2011:2013)) %>% ungroup() %>%
  group_by(iso3,JD_Cat, income_level) %>% summarise(across(c(FAO_pop,Food_Demand,Income,rescaled,GrandT_demand),mean))

## calculate the shares
shares_by_FG <- three_y_avg %>% ungroup() %>% mutate(share_of_FG = rescaled/(Food_Demand*10))

## join with footprint data and mutate
shares_footprint <- left_join(shares_by_FG, by_country, by = c("iso3", "JD_Cat")) %>%
  mutate(impact_by_income=share_of_FG*impact*10)

## NA table
shares_footprint_NA <- shares_footprint%>% filter(JD_Cat!="Grand Total") %>% filter(is.na(impact_by_income))

## sum impacts by iso3 to get a Gran Total JD_Cat
GrandT_impact <- shares_footprint %>% filter(JD_Cat!="Grand Total") %>%
  drop_na(impact_by_income) %>% ungroup() %>% group_by(iso3,country,country_code,income_level, attribute) %>%
  summarise(impact=sum(impact),impact_by_income=sum(impact_by_income)) %>% 
  mutate(JD_Cat="Grand Total")

## create the df to rbind to main df
GrandT_FBS <- shares_by_FG %>% filter(JD_Cat=="Grand Total")

GrandT_FBS_impact <- left_join(GrandT_FBS,GrandT_impact,by = c("iso3", "JD_Cat", "income_level"))

GrandT_FBS_impact_NA <- GrandT_FBS_impact%>% filter(is.na(impact_by_income))
## rbind to main df
env_impact_df <- bind_rows(shares_footprint %>% drop_na(impact_by_income) %>% filter(JD_Cat!="Grand Total"),
                           GrandT_FBS_impact %>% drop_na(impact_by_income)) %>% mutate(region=countrycode(iso3,"iso3c","continent"))

## join with FAO adjusted footprint data and mutate
shares_footprint_FAO_adj <- left_join(shares_by_FG, by_country_FAO_adj, by = c("iso3", "JD_Cat")) %>%
  mutate(impact_by_income=share_of_FG*impact*10)

## NA table
shares_footprint_NA_FAO_adj <- shares_footprint_FAO_adj%>% filter(JD_Cat!="Grand Total") %>% filter(is.na(impact_by_income))

## sum impacts by iso3 to get a Gran Total JD_Cat
GrandT_impact_FAO_adj <- shares_footprint_FAO_adj %>% filter(JD_Cat!="Grand Total") %>%
  drop_na(impact_by_income) %>% ungroup() %>% group_by(iso3,country,country_code,income_level, attribute) %>%
  summarise(impact=sum(impact),impact_by_income=sum(impact_by_income)) %>% 
  mutate(JD_Cat="Grand Total")

## create the df to rbind to main df
GrandT_FBS <- shares_by_FG %>% filter(JD_Cat=="Grand Total")

GrandT_FBS_impact_FAO_adj <- left_join(GrandT_FBS,GrandT_impact_FAO_adj,by = c("iso3", "JD_Cat", "income_level"))

GrandT_FBS_impact_NA_FAO_adj <- GrandT_FBS_impact_FAO_adj%>% filter(is.na(impact_by_income))
## rbind to main df
env_impact_df_FAO_adj <- bind_rows(shares_footprint %>% drop_na(impact_by_income) %>% filter(JD_Cat!="Grand Total"),
                                   GrandT_FBS_impact_FAO_adj %>% drop_na(impact_by_income)) %>% mutate(region=countrycode(iso3,"iso3c","continent"))


fs <- readRDS("Data/FAO_FBS_non_soviet_20200414.rds")
## filter for pop
pop_2011_13 <- fs %>% filter(Item=="Population", Year %in% c(2011:2013))
pop_2011_13_avg <- sum(pop_2011_13$Value)*1000/3
# temp <- pop_2011_13 %>% count(`Country Code`) ## Sudan is the only country to split during this period

## sum pop along shares df
shares_footprint_non_NA <- shares_footprint%>% filter(JD_Cat=="Meat & Seafood",
                                                      income_level=="decile1",
                                                      attribute=="kg_co2e_total") %>%
  filter(!is.na(impact_by_income))
length(unique(shares_footprint_non_NA$iso3))

sample_pop <- sum(shares_footprint_non_NA$FAO_pop)

###
# Key datasets
# kg_co2e_total
fig_df_cont <- env_impact_df %>%
  filter(attribute=="kg_co2e_total",JD_Cat=="Grand Total",income_level %nin% c("mean")) %>% #group_by(JD_Cat) %>% 
  # arrange(desc(impact_by_income)) %>% 
  arrange(desc(impact_by_income)) %>% 
  mutate(Country_decile = paste(country,income_level,sep = "_"),
         iso3_decile = paste(iso3,income_level,sep = "_"),
         Country_decile = fct_reorder(Country_decile, impact_by_income,.desc = T),
         iso3_decile = fct_reorder(iso3_decile, impact_by_income,.desc = T),
         width= cumsum(FAO_pop/10),
         width_min = width-(FAO_pop/10),
         width_min_t= width_min+(width - width_min)/2,
         assumptions = "+LUC -FAO_GHG")
# kg_co2e_excl_luc
fig_df_cont_no_LUC <- env_impact_df %>%
  filter(attribute=="kg_co2e_excl_luc",JD_Cat=="Grand Total",income_level %nin% c("mean")) %>% #group_by(JD_Cat) %>% 
  # arrange(desc(impact_by_income)) %>% 
  arrange(desc(impact_by_income)) %>% 
  mutate(Country_decile = paste(country,income_level,sep = "_"),
         iso3_decile = paste(iso3,income_level,sep = "_"),
         Country_decile = fct_reorder(Country_decile, impact_by_income,.desc = T),
         iso3_decile = fct_reorder(iso3_decile, impact_by_income,.desc = T),
         width= cumsum(FAO_pop/10),
         width_min = width-(FAO_pop/10),
         width_min_t= width_min+(width - width_min)/2,
         assumptions = "-LUC -FAO_GHG")

# Key datasets FAO_adj
# kg_co2e_total
fig_df_cont_FAO_adj <- env_impact_df_FAO_adj %>%
  filter(attribute=="kg_co2e_total",JD_Cat=="Grand Total",income_level %nin% c("mean")) %>% #group_by(JD_Cat) %>% 
  # arrange(desc(impact_by_income)) %>% 
  arrange(desc(impact_by_income)) %>% 
  mutate(Country_decile = paste(country,income_level,sep = "_"),
         iso3_decile = paste(iso3,income_level,sep = "_"),
         Country_decile = fct_reorder(Country_decile, impact_by_income,.desc = T),
         iso3_decile = fct_reorder(iso3_decile, impact_by_income,.desc = T),
         width= cumsum(FAO_pop/10),
         width_min = width-(FAO_pop/10),
         width_min_t= width_min+(width - width_min)/2,
         assumptions = "+LUC +FAO_GHG")
# kg_co2e_excl_luc
fig_df_cont_no_LUC_FAO_adj <- env_impact_df_FAO_adj %>%
  filter(attribute=="kg_co2e_excl_luc",JD_Cat=="Grand Total",income_level %nin% c("mean")) %>% #group_by(JD_Cat) %>% 
  # arrange(desc(impact_by_income)) %>% 
  arrange(desc(impact_by_income)) %>% 
  mutate(Country_decile = paste(country,income_level,sep = "_"),
         iso3_decile = paste(iso3,income_level,sep = "_"),
         Country_decile = fct_reorder(Country_decile, impact_by_income,.desc = T),
         iso3_decile = fct_reorder(iso3_decile, impact_by_income,.desc = T),
         width= cumsum(FAO_pop/10),
         width_min = width-(FAO_pop/10),
         width_min_t= width_min+(width - width_min)/2,
         assumptions = "-LUC +FAO_GHG")

test_bind_assumptions <- bind_rows(fig_df_cont,fig_df_cont_no_LUC,fig_df_cont_FAO_adj,fig_df_cont_no_LUC_FAO_adj)
## Actual figure
test_bind_assumptions %>%
  ggplot(aes(y= impact_by_income/1000, x = width_min_t/1E9,color = assumptions))+
  geom_line()+
  ylab("Food consumption GHG emissions [Tons CO2e/person/year]")+
  xlab("Cumulative Population [billions]")+
  scale_y_continuous(n.breaks = 7)+
  # scale_y_log10(n.breaks = 7)+
  scale_x_continuous(n.breaks = 7)+
  # ylim(0,3)+
  theme_bw()+
  # theme(panel.grid.minor.x=element_blank())
  theme(axis.line.x = element_line(color="black", size = .25), 
        axis.line.y = element_line(color="black", size = .25),
        panel.border = element_blank(),
        panel.grid=element_blank())

title <- "Merged_assumptions"
ggsave(paste0(getwd(), "/Output/", title, ".png")
       , width = 120, height = 120, units = "mm"
)


## GLEAM raw data check sums

GLEAM <- read.csv("Data_Env/gleam_i_raw_output_v20_rev3c.csv")

long_gleam <- GLEAM %>% pivot_longer(cols = 6:240, names_to = "country", values_to = "value")
sums_gleam <- long_gleam %>% filter(system =="All systems") %>% group_by(gleam_variable) %>% 
  summarise(sum=sum(value, na.rm = T))


## impact by COO distribution 
# Is this roughly what Tom asked for? I need to figure out a way of weighting this by production.


impact_dist <- read.csv("Data_Env/item_footprints_by_coo.csv")
# filter out other impacts and countries with country specific data
impact_dist_co2 <- impact_dist %>% filter(footprint_type =="kg_co2e_excl_luc",geographic_resolution=="country", fbs_item !="Offals, Edible")
# colnames(impact_dist)
# histogram
impact_dist_co2 %>% ggplot(aes(x=country_footprint))+
  geom_histogram()+
  facet_wrap(~fbs_item, scales = "free_x")

#boxplot
impact_dist_co2 %>% ggplot(aes(x=fbs_item, y=country_footprint))+
  geom_boxplot()+
  ylab("kg CO2e per kg of product")+xlab("")+
  # geom_jitter()+
  coord_flip()
multi <- 2
ggsave(paste0(getwd(), "/Output/footprint_boxplot.png")
       , width = 120*multi, height = 50*multi, units = "mm"
)
# raincloud? follow these: https://www.cedricscherer.com/2021/06/06/visualizing-distributions-with-raincloud-plots-and-how-to-create-them-with-ggplot2/#back1
# https://wellcomeopenresearch.org/articles/4-63
impact_dist_co2 %>% ggplot(aes(x=fbs_item, y=country_footprint))+
  # geom_boxplot()+
  ggdist::stat_halfeye(.width = 0,point_color = NA)+
  # geom_flat_violin()+
  # geom_jitter()+
  coord_flip()


##########
# EXTRAS

#######
# Including LUC
# this section was ommited from the papper for theoretical reasons explained in the main text but were developed anyways prior to making that choice. Keeping the code for reference

# With LUC
## Variable width Figure

# kg_co2e_total
# kg_co2e_excl_luc
fig_df <- env_impact_df %>%
  filter(attribute=="kg_co2e_total",JD_Cat=="Grand Total",income_level %nin% c("mean")) %>% #group_by(JD_Cat) %>% 
  # arrange(desc(impact_by_income)) %>% 
  arrange(desc(impact_by_income)) %>% 
  mutate(#Country_FAO = fct_reorder(Country_FAO, rescaled),
    #iso3 = fct_reorder(iso3, rescaled),
    Country_decile = paste(country,income_level,sep = "_"),
    iso3_decile = paste(iso3,income_level,sep = "_"),
    Country_decile = fct_reorder(Country_decile, impact_by_income,.desc = F),
    iso3_decile = fct_reorder(iso3_decile, impact_by_income,.desc = F),
    width= cumsum(FAO_pop/10),
    width_min = width-(FAO_pop/10),
    width_min_t= width_min+(width - width_min)/2)
## weighted mean
mean_ghgs <- weighted.mean(fig_df$impact,fig_df$FAO_pop) 

ggplot(fig_df, aes(ymin=0, fill = as.factor(region)))+
  geom_rect(aes(xmin=width_min/1E9, xmax=width/1E9, ymax=impact_by_income/1000)) +
  # geom_rect(aes(xmin=width_min/1E9, xmax=width/1E9, ymax=Income)) +
  geom_hline(yintercept = mean_ghgs/1E3, size =.25, lty=2, color ="gray30")+
  geom_text(x=6, y=mean_ghgs/1E3+max(fig_df$impact_by_income)/30000,
            label="mean", color = "gray30", size = 3, check_overlap = T)+
  ylab("Annual food-consumption GHG emissions [Tons CO2e/person]")+
  xlab("Cumulative Population [billions]")+
  #coord_flip()
  scale_y_continuous(n.breaks = 6)+
  # scale_y_log10()+
  scale_x_continuous(n.breaks = 7)+
  # scale_fill_brewer(palette = "Set3")+
  # scale_fill_manual(values = c('#8dd3c7','#fb8072','#80b1d3','#bebada','#ffffb3'))+
  # scale_fill_manual(values = c("#59A14F","#E15759", "#4E79A7","#F28E2B", "#76B7B2"))+
  scale_fill_manual(values = c('#4daf4a','#e41a1c','#377eb8','#ff7f00','#984ea3'))+
  # scale_fill_manual(values = c('#e41a1c','#377eb8'))+
  # ggthemes::scale_fill_tableau(palette = "Color Blind")+
  theme_bw()+
  theme(axis.line.x = element_line(color="black", size = .25), 
        axis.line.y = element_line(color="black", size = .25),
        panel.border = element_blank(),
        panel.grid=element_blank(),
        legend.title=element_blank(),
        # legend.position=c(0.9,.8),
        legend.position="none")

title <- "Lorenz_discrete_deciles_nice"
ggsave(paste0(getwd(), "/Output/", title, "_SI_LUC_FAO_adj.png")
       , width = 120, height = 120, units = "mm"
)


Brazil-US-China

ggplot(fig_df, aes(ymin=0))+
  geom_rect(aes(xmin=width_min/1E9, xmax=width/1E9, ymax=impact_by_income/1000)) +
  geom_rect(data=fig_df %>% filter(iso3 %in% c("BRA","USA","CHN","ETH")),
            aes(xmin=width_min/1E9, xmax=width/1E9, ymax=impact_by_income/1000,
                fill = as.factor(iso3))) +
  geom_hline(yintercept = mean_ghgs/1E3, size =.25, lty=2, color ="gray30")+
  geom_text(x=6, y=mean_ghgs/1E3+max(fig_df$impact_by_income)/30000,
            label="mean", color = "gray30", size = 3, check_overlap = T)+
  # ggrepel::geom_text_repel(data=fig_df %>% filter(iso3 %in% c("USA","CHN")),
  #                          aes(x=width_min_t/1E9, y=impact_by_income/1000,
  #                              label = iso3_decile), angle = 90,direction = "y")+
  # geom_text(data=fig_df %>% filter(iso3 %in% c("USA","CHN")),
  #                          aes(x=width_min_t/1E9, y=impact_by_income/1000,
  #                              label = iso3_decile))+
  ylab("Annual food-consumption GHG emissions [Tons CO2e/person]")+
  xlab("Cumulative Population [billions]")+
  scale_y_continuous(n.breaks = 6)+
  scale_x_continuous(n.breaks = 7)+
  scale_fill_manual(values = c('#e41a1c','#377eb8','#4daf4a','#ff7f00'))+
  theme_bw()+
  theme(axis.line.x = element_line(color="black", size = .25), 
        axis.line.y = element_line(color="black", size = .25),
        panel.border = element_blank(),
        panel.grid=element_blank(),
        legend.title=element_blank(),
        # legend.position=c(0.9,.8),
        legend.position="none")

title <- "Lorenz_discrete_deciles_BRA_USA_CHN_ETH"
ggsave(paste0(getwd(), "/Output/", title, "_SI_LUC_FAO_adj.png")
       , width = 120, height = 120, units = "mm"
)



### Cummulative emissions

##cummulative emissions
cum_emi <- fig_df %>% mutate(cum_emi=cumsum(FAO_pop/10*impact_by_income),
                             cum_income=cumsum(FAO_pop/10*Income)) 
cum_emi_top <- cum_emi %>% filter(cum_emi<=max(cum_emi)/2)
## percentage of people that emit the first 50% of emissions
max(cum_emi_top$width)/max(fig_df$width)
## how much income do they account for?
max(cum_emi_top$cum_income)/max(cum_emi$cum_income)


## Variable width Means

# kg_co2e_total
# kg_co2e_excl_luc
fig_df_mean <- env_impact_df %>%
  filter(attribute=="kg_co2e_total",JD_Cat=="Grand Total",income_level %in% c("mean")) %>% #group_by(JD_Cat) %>% 
  # arrange(desc(impact_by_income)) %>% 
  arrange(desc(impact_by_income)) %>% 
  mutate(width= cumsum(FAO_pop),
         width_min = width-(FAO_pop),
         width_min_t= width_min+(width - width_min)/2)
## weighted mean
mean_ghgs <- weighted.mean(fig_df_mean$impact,fig_df_mean$FAO_pop) 

ggplot(fig_df_mean, aes(ymin=0, fill = as.factor(region)))+
  geom_rect(aes(xmin=width_min/1E9, xmax=width/1E9, ymax=impact_by_income/1000)) +
  geom_hline(yintercept = mean_ghgs/1E3, size =.25, lty=2, color ="gray30")+
  geom_text(x=6, y=mean_ghgs/1E3+max(fig_df$impact_by_income)/30000,
            label="mean", color = "gray30", size = 3, check_overlap = T)+
  # ylab("Annual food-consumption GHG emissions [Tons CO2e/person]")+
  # xlab("Cumulative Population [billions]")+
  ylab(NULL)+
  xlab(NULL)+
  # ylim(NA,5.7)+
  #coord_flip()
  scale_y_continuous(n.breaks = 6)+
  scale_x_continuous(n.breaks = 7)+
  # scale_fill_brewer(palette = "Set3")+
  # scale_fill_manual(values = c('#8dd3c7','#fb8072','#80b1d3','#bebada','#ffffb3'))+
  # scale_fill_manual(values = c("#59A14F","#E15759", "#4E79A7","#F28E2B", "#76B7B2"))+
  scale_fill_manual(values = c('#4daf4a','#e41a1c','#377eb8','#ff7f00','#984ea3'))+
  # ggthemes::scale_fill_tableau(palette = "Color Blind")+
  theme_bw()+
  theme(axis.line.x = element_line(color="black", size = .25), 
        axis.line.y = element_line(color="black", size = .25),
        panel.border = element_blank(),
        panel.grid=element_blank(),
        legend.title=element_blank(),
        legend.position=c(0.9,.8))
# theme(axis.title.x=element_blank(),
#       axis.text.x=element_blank(),
#       axis.ticks.x=element_blank())+
# facet_wrap(~JD_Cat,scales = "free")

title <- "Lorenz_discrete_mean_nice_no_labels"
ggsave(paste0(getwd(), "/Output/", title, "_SI_LUC_FAO_adj.png")
       , width = 4.42, height = 3.96, units = "in"
)

## How to get colors in your R object
# unique(ggplot_build(p)$data[[1]]$fill)

Brazil-US-China

ggplot(fig_df_mean, aes(ymin=0))+
  geom_rect(aes(xmin=width_min/1E9, xmax=width/1E9, ymax=impact_by_income/1000)) +
  geom_rect(data=fig_df_mean %>% filter(iso3 %in% c("BRA","USA","CHN","ETH")),
            # geom_rect(data=fig_df_mean %>% filter(iso3 %in% c("USA","CHN")),
            aes(xmin=width_min/1E9, xmax=width/1E9, ymax=impact_by_income/1000,
                fill = as.factor(country))) +
  geom_hline(yintercept = mean_ghgs/1E3, size =.25, lty=2, color ="gray30")+
  geom_text(x=6, y=mean_ghgs/1E3+max(fig_df$impact_by_income)/30000,
            label="mean", color = "gray30", size = 3, check_overlap = T)+
  # ggrepel::geom_text_repel(data=fig_df %>% filter(iso3 %in% c("USA","CHN")),
  #                          aes(x=width_min_t/1E9, y=impact_by_income/1000,
  #                              label = iso3_decile), angle = 90,direction = "y")+
  # geom_text(data=fig_df %>% filter(iso3 %in% c("USA","CHN")),
  #                          aes(x=width_min_t/1E9, y=impact_by_income/1000,
  #                              label = iso3_decile))+
  ylab(NULL)+
  xlab(NULL)+
  scale_y_continuous(n.breaks = 6)+
  scale_x_continuous(n.breaks = 7)+
  # scale_fill_manual(values = c('#377eb8','#e41a1c'))+
  scale_fill_manual(values = c('#e41a1c','#377eb8','#4daf4a','#ff7f00'))+
  theme_bw()+
  theme(axis.line.x = element_line(color="black", size = .25), 
        axis.line.y = element_line(color="black", size = .25),
        panel.border = element_blank(),
        panel.grid=element_blank(),
        legend.title=element_blank(),
        # legend.position=c(0.9,.8))#,
        legend.position="none")

title <- "Lorenz_discrete_mean_USA_CHN"
ggsave(paste0(getwd(), "/Output/", title, "_SI_LUC_FAO_adj.png")
       , width = 4.42, height = 3.96, units = "in"
)


## Continuous Lorentz Line

# kg_co2e_total
fig_df_cont <- env_impact_df %>%
  filter(attribute=="kg_co2e_total",JD_Cat=="Grand Total",income_level %nin% c("mean")) %>% #group_by(JD_Cat) %>% 
  # arrange(desc(impact_by_income)) %>% 
  arrange(desc(impact_by_income)) %>% 
  mutate(Country_decile = paste(country,income_level,sep = "_"),
         iso3_decile = paste(iso3,income_level,sep = "_"),
         Country_decile = fct_reorder(Country_decile, impact_by_income,.desc = T),
         iso3_decile = fct_reorder(iso3_decile, impact_by_income,.desc = T),
         width= cumsum(FAO_pop/10),
         width_min = width-(FAO_pop/10),
         width_min_t= width_min+(width - width_min)/2)

# kg_co2e_excl_luc
fig_df_excl_luc <- env_impact_df %>%
  filter(attribute=="kg_co2e_excl_luc",JD_Cat=="Grand Total",income_level %nin% c("mean")) %>% #group_by(JD_Cat) %>% 
  # arrange(desc(impact_by_income)) %>% 
  arrange(desc(impact_by_income)) %>% 
  mutate(Country_decile = paste(country,income_level,sep = "_"),
         iso3_decile = paste(iso3,income_level,sep = "_"),
         Country_decile = fct_reorder(Country_decile, impact_by_income,.desc = T),
         iso3_decile = fct_reorder(iso3_decile, impact_by_income,.desc = T),
         width= cumsum(FAO_pop/10),
         width_min = width-(FAO_pop/10),
         width_min_t= width_min+(width - width_min)/2)

floor <- 1
ceiling <- 2.5
excess <- fig_df_cont %>% filter(impact_by_income/1000>=ceiling)
lacking <- fig_df_cont %>% filter(impact_by_income/1000<=floor)
ggplot(fig_df_cont, aes(y= impact_by_income/1000, x = width_min_t/1E9))+
  # geom_smooth(se=F)+
  geom_ribbon(data=excess, aes(ymin=ceiling,ymax=impact_by_income/1000), fill="red", alpha=0.5) +
  geom_ribbon(data=lacking, aes(ymin=0,ymax=impact_by_income/1000), fill="red", alpha=0.5) +
  geom_ribbon(data=lacking, aes(ymin=impact_by_income/1000,ymax=floor), fill="green", alpha=0.5) +
  geom_line()+
  geom_hline(yintercept = c(floor,ceiling), color = "gray50",lty=2)+
  # geom_line(data=fig_df_excl_luc, aes(y= impact_by_income/1000, x = width_min_t/1E9),color="red")+
  ylab("Annual food-consumption GHG emissions [Tons CO2e/person]")+
  xlab("Cumulative Population [billions]")+
  theme_bw()#+
# facet_wrap(~JD_Cat,scales = "free")

title <- "Lorenz_continuous_excl_luc"
# ggsave(paste0(getwd(), "/Output/", title, ".png")
#        #, width = 200, height = 300, units = "mm"
#        )


### Partial discussion 

# Total food related emissions accroding to Kim et al ammount to 8.25E12 kg CO2e = 8.25 Giga tons CO2e (did this one quickly in Excel with diet_footprints_by_whole_country_diet.csv, and 7.48 Giga tons CO2e when excluding LUC), the estimate for total emissions is roughly 45.5 Gt CO2e in 2011-2013 based on Ourworld in Data https://ourworldindata.org/greenhouse-gas-emissions
# The food emissions per capita from Kim et al is 1.336t CO2e/year
# 
# Looking at fig_df_cont I see 430.6 million above a 2.5 t CO2e threshold mentioned in Navin's faculty pledge and 490.2 above the 2.3 threshold in Oxfam report https://www.oxfam.ca/news/carbon-emissions-of-richest-one-per-cent-set-to-be-30-times-the-1-5c-limit-in-2030/
# https://www.oxfam.ca/publication/carbon-inequality-2030/
# but that latter source is talking about C02 and not CO2e
# 
# 2532.2 million people have a footprint smaller than 1t CO2e the (arbitrary?) threshold selected in Chakravarty 2009.
# Assuming not all of the 1t CO2e can go to food that number can be reduced.
# 
# One additional consideration to the comparisons from Chakravarty are that they present the main figure as a projection in 2030, not the most recent estimate they had which was 2003. Don't know how to handle that comaprison.
# 
# 
# Get estimates of the daily or yearly GHG emissions from following the EAT lancet diet.
# 
# "A global carbon budget" section on the actual paper is amazingly detailed.
# The Lancet paper mentions 5Gt CO2e (4.7-5.4 range) as the upper boundary of GHG emissions allowable consistent with 2°C temperature rise and based on Methane and Nitrous Oxide emissions being almost impossible to reduce.
# 
# The 5Gt CO2e will be split differently every year depending on population, using current and future UN population estimates(Where did I take them from?) that EAT Lancet estimate translates into yearly GHG emissions of 513.6kg CO2e per person in 2050, 584.9kg CO2e per person in 2030 and 701.7kg CO2e per person for the 2011-2013 period. The latter means 1.922kg CO2e per person per day and goes down to 1.407 in 2050.
# 
# For the period comparable to Kim et al (2011-2013) that means only 1343.1 million people are below that level. Of those people the highest income is 197 $2005PPP a month.
# 
# The EAT Lancet diet sums up to 1324 g of food a day. https://eatforum.org/content/uploads/2019/01/EAT-Lancet_Commission_Summary_Report.pdf
# 
# 
# The average level of greenhouse gas emissions was 6.0 kg CO2 equivalents per kg food per day in the following study, all cohort participants
# https://www.thelancet.com/journals/lanplh/article/PIIS2542-5196(21)00250-3/fulltext#seccestitle160
# 
# That study also says the people with the highest EAT lancet score had 5.0 kg CO2 equivalents per kg food per day
# 
# 
# The main problem I have with the environmental targets is that a change in inequality and increase in incomes will very unlikely lead to a balance of healthyness and climate targets, that is exactly why EAT-Lancet, Springman and all those papers need a scenario based approach where things don't follow business as usual.
# 
# GHG emissions from Ag have been somewhat stagnant. https://www.fao.org/3/cb3808en/cb3808en.pdf page 3
# 
# (Not used) Get total emissions from graph, not equal to total emissions from Kim

total_emi <- sum(fig_df_cont$FAO_pop/10*fig_df_cont$impact_by_income)


## Chakravarty method
### summing up to Eat lancet in 2011-2013

ceiling <- 927.8 ## found by trial and error, is there abetter way?
eat <- fig_df_cont %>% arrange(impact_by_income) %>%
  mutate(impact_by_income=ifelse(impact_by_income>ceiling,ceiling,impact_by_income),
         target=cumsum(FAO_pop/10*impact_by_income))
## see what it sums up to
max(eat$target)
# > 4.999702e+12


#### bringing poor people up
or rather out of mediocre diet up at least to what they'l be allowed by 2050, i.e. 513.6kg CO2e

another example is Leach recomended diet*Poore and nemececk impact factors (from Silvia's work)
3.038 kg/day = 1.108 tons/year

## 513.6kg CO2e
floor <- 513.6
ceiling_poor <- 911.1 ## again found by trial and error, is there abetter way?
## the diff between ceiling and ceiling_poor is so tiny it is not perceptible on graph
eat_poor <- fig_df_cont %>% arrange(impact_by_income) %>%
  mutate(impact_by_income=ifelse(impact_by_income>ceiling_poor,ceiling_poor,impact_by_income),
         impact_by_income=ifelse(impact_by_income<floor,floor,impact_by_income),
         target=cumsum(FAO_pop/10*impact_by_income))

## see what it sums up to
max(eat_poor$target)
# > 4.999679e+12



Wrong? approach Old Ceiling
Estimate a ceiling similar to Chakravarty where area under curve is below 5Gt

# target <- fig_df_cont %>% arrange(impact_by_income) %>%
#   mutate(target=cumsum(FAO_pop/10*impact_by_income))
# below_target <- target %>% filter(target<=5E12)
# # ceiling with no lifting out of poverty
# ceiling <- max(below_target$impact_by_income)

Wrong? approach Old Ceiling lifting poverty
(Not used?)


floor <- 1108
target_poor <- fig_df_cont %>% arrange(impact_by_income) %>%
  mutate(impact_by_income=ifelse(impact_by_income<floor,floor,impact_by_income),
         target=cumsum(FAO_pop/10*impact_by_income))
below_target_poor <- target_poor %>% filter(target<=5E12)
# ceiling with no lifting out of poverty
ceiling_poor <- max(below_target_poor$impact_by_income)


#### Fig Chakravarty


## after running the code at the end which optimizes and finds exact number for ceiling and ceiling_poor for different targets
ceiling <- ceiling_vector[3]
ceiling_poor <- ceiling_poor_vector[3]
# floor <- 1108
excess <- fig_df_cont %>% filter(impact_by_income>=ceiling)
excess_poor <- fig_df_cont %>% filter(impact_by_income>=ceiling_poor) %>%
  mutate(impact_by_income=ifelse(impact_by_income>=ceiling, ceiling,impact_by_income))
lacking <- fig_df_cont %>% filter(impact_by_income<=floor)

ggplot(fig_df_cont, aes(y= impact_by_income/1000, x = width_min_t/1E9))+
  # geom_smooth(se=F)+
  geom_ribbon(data=excess, aes(ymin=ceiling/1000,ymax=impact_by_income/1000),
              fill="blue", alpha=0.5) +
  geom_ribbon(data=excess_poor, aes(ymin=ceiling_poor/1000,ymax=impact_by_income/1000),
              fill="red", alpha=0.5) +
  geom_ribbon(data=lacking, aes(ymin=0,ymax=impact_by_income/1000), fill="green", alpha=0.5) +
  geom_ribbon(data=lacking, aes(ymin=impact_by_income/1000,ymax=floor/1000), fill="red", alpha=0.5) +
  geom_line()+
  # geom_hline(yintercept = c(floor/1000,ceiling/1000,ceiling_poor/1000), color = "gray50",lty=2)+
  # geom_line(data=fig_df_excl_luc, aes(y= impact_by_income/1000, x = width_min_t/1E9),color="red")+
  ylab("Food consumption GHG emissions [Tons CO2e/person/year]")+
  xlab("Cumulative Population [billions]")+
  scale_y_continuous(n.breaks = 7)+
  scale_x_continuous(n.breaks = 7)+
  # ylim(0,3)+
  theme_bw()+
  # theme(panel.grid.minor.x=element_blank())
  theme(axis.line.x = element_line(color="black", size = .25), 
        axis.line.y = element_line(color="black", size = .25),
        panel.border = element_blank(),
        panel.grid=element_blank())

title <- "True_chakravarty_style"
# ggsave(paste0(getwd(), "/Output/", title, "_SI_LUC.png")
#        , width = 120, height = 120, units = "mm"
#        )

#### Counts and %

# People exceeding
# count
max(excess$width)
# percentage
max(excess$width)/sample_pop

# People below min Lancet healthy diet in 2050
# count
(max(lacking$width)-min(lacking$width_min))
# percentage
(max(lacking$width)-min(lacking$width_min))/sample_pop

# People below 2013 limit and above 2050 limit
# count
(sample_pop-max(excess$width)-(max(lacking$width)-min(lacking$width_min)))
# Percentages
(sample_pop-max(excess$width)-(max(lacking$width)-min(lacking$width_min)))/sample_pop

# Of those people eating diets that meet the target, X percent eat less than 2050 5Gt/population target
(max(lacking$width)-min(lacking$width_min))/(sample_pop-max(excess$width)-(max(lacking$width)-min(lacking$width_min)))

# Same but with poor.
max(excess_poor$width)/sample_pop


### summing up to % of Eat lancet in 2011-2013

## all emissions from Kim - Eat Lancet 5Gt
target_perc <- .7
(8.25-5)*(1-target_perc)
5+(8.25-5)*(1-target_perc)
# > 5.975 Gt is the objective

## all FAO_GHG adjusted emissions from Kim - Eat Lancet 5Gt
target_perc <- .7
# ((FAOs_non_LUC_CO2_total+FAOs_LUC_CO2_total)/1E12-5)*(1-target_perc)
# 5+((FAOs_non_LUC_CO2_total+FAOs_LUC_CO2_total)/1E12-5)*(1-target_perc)

# ceiling <- 1179.3 ## found by trial and error, is there abetter way? A for loop
# ceiling <- 1213
ceiling <- ceiling_vector[3]
eat <- fig_df_cont %>% arrange(impact_by_income) %>%
  mutate(impact_by_income=ifelse(impact_by_income>ceiling,ceiling,impact_by_income),
         target=cumsum(FAO_pop/10*impact_by_income))
## see what it sums up to
max(eat$target)
# > 5.974987e+12


#### bringing poor people up
or rather out of mediocre diet up at least to what they'l be allowed by 2050, i.e. 513.6kg CO2e
                                                                            
                                                                            another example is Leach recomended diet*Poore and nemececk impact factors (from Silvia's work)
3.038 kg/day = 1.108 tons/year

## all emissions from Kim - Eat Lancet 5Gt
target_perc <- .7
(8.25-5)*(1-target_perc)
5+(8.25-5)*(1-target_perc)
# > 5.975 Gt is the objective
objective[3]
## all FAO_GHG adjusted emissions from Kim - Eat Lancet 5Gt
target_perc <- .7
# ((FAOs_non_LUC_CO2_total+FAOs_LUC_CO2_total)/1E12-5)*(1-target_perc)
# 5+((FAOs_non_LUC_CO2_total+FAOs_LUC_CO2_total)/1E12-5)*(1-target_perc)

## 513.6kg CO2e is the result of 5Gt/2050 UN pop estimate
floor <- 513.6
# ceiling_poor <- 1153.2 ## again found by trial and error, is there abetter way?
ceiling_poor <- ceiling_poor_vector[3]#-.1
## the diff between ceiling and ceiling_poor is so tiny it is not perceptible on graph
eat_poor <- fig_df_cont %>% arrange(impact_by_income) %>%
  mutate(impact_by_income=ifelse(impact_by_income>ceiling_poor,ceiling_poor,impact_by_income),
         impact_by_income=ifelse(impact_by_income<floor,floor,impact_by_income),
         target=cumsum(FAO_pop/10*impact_by_income))

## see what it sums up to
max(eat_poor$target)
max(eat_poor$target)<objective[3]
# > 5.975142e+12


#### Fig Chakravarty


# floor <- 1108
excess <- fig_df_cont %>% filter(impact_by_income>=ceiling)
excess_poor <- fig_df_cont %>% filter(impact_by_income>=ceiling_poor) %>%
  mutate(impact_by_income=ifelse(impact_by_income>=ceiling, ceiling,impact_by_income))
lacking <- fig_df_cont %>% filter(impact_by_income<=floor)

ggplot(fig_df_cont, aes(y= impact_by_income/1000, x = width_min_t/1E9))+
  # geom_smooth(se=F)+
  geom_ribbon(data=excess, aes(ymin=ceiling/1000,ymax=impact_by_income/1000),
              fill="blue", alpha=0.5) +
  geom_ribbon(data=excess_poor, aes(ymin=ceiling_poor/1000,ymax=impact_by_income/1000),
              fill="red", alpha=0.5) +
  geom_ribbon(data=lacking, aes(ymin=0,ymax=impact_by_income/1000), fill="green", alpha=0.5) +
  geom_ribbon(data=lacking, aes(ymin=impact_by_income/1000,ymax=floor/1000), fill="red", alpha=0.5) +
  geom_line()+
  # geom_hline(yintercept = c(floor/1000,ceiling/1000,ceiling_poor/1000), color = "gray50",lty=2)+
  # geom_line(data=fig_df_excl_luc, aes(y= impact_by_income/1000, x = width_min_t/1E9),color="red")+
  ylab("Food consumption GHG emissions [Tons CO2e/person/year]")+
  xlab("Cumulative Population [billions]")+
  scale_y_continuous(n.breaks = 7)+
  scale_x_continuous(n.breaks = 7)+
  # ylim(0,3)+
  theme_bw()+
  # theme(panel.grid.minor.x=element_blank())
  theme(axis.line.x = element_line(color="black", size = .25), 
        axis.line.y = element_line(color="black", size = .25),
        panel.border = element_blank(),
        panel.grid=element_blank())

title <- "True_chakravarty_style"
ggsave(paste0(getwd(), "/Output/", title, "_SI_LUC_FAO_adj.png")
       , width = 120, height = 120, units = "mm"
       )


### Gini

# global gini
acid::weighted.gini(fig_df_cont$impact_by_income, fig_df_cont$FAO_pop)$bcwGini
# between country Gini
acid::weighted.gini(fig_df_cont$impact, fig_df_cont$FAO_pop)$bcwGini
# average within country Gini
within_ineq_ghgs <- fig_df_cont %>% group_by(iso3) %>% 
  summarise(gini_decile_impact = acid::gini(impact_by_income)$bcGini,
            pop = mean(FAO_pop))
weighted.mean(within_ineq_ghgs$gini_decile_impact,within_ineq_ghgs$pop)## this might actually just be the same inequality from paper 1 but changes because I study a subset of countries. But all in all I just multiplied each decile by a constant within countries so the actual inequality within countries should not change.
