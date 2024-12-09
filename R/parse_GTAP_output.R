library(readxl)
library(tidyverse)
library(harrypotter)
library(usmap)
library(maps)
library(igraph)
library(patchwork)
library(png)
library(ggsci)
library(grid)

load_gtap_results_data <- function()
{
  folder <- 'data/'
  
  global_s1 <- paste0(folder,'Global_s1_updated_24_07_24.xlsx')
  global_s2 <- paste0(folder,'Global_s2_updated_24_07_24.xlsx')
  global_s3 <- paste0(folder,'Global_s3_updated_24_07_24 1.xlsx')
  macro     <- paste0(folder,'Macro_updated_24_07_24.xlsx')
  
  data <- data.frame()
  
  for(scenario in c(global_s1,global_s2,global_s3))
  {
    sheets <- readxl::excel_sheets(scenario)
    name   <- str_replace( str_split( scenario, '[.]' )[[1]][1], folder, '' )
    
    for(sheet in sheets)
    {
      tmp               <- readxl::read_xlsx(scenario,sheet = sheet)
      econ_variable     <- colnames(tmp)[1]
      tmp$econ_variable <- econ_variable
      
      colnames(tmp)[1]  <- "sector"
      tmp <- tmp %>% pivot_longer(-c(sector,econ_variable), names_to = 'region', values_to = 'value')
      tmp$scenario      <- name
      
      data <- bind_rows(data,tmp)
    }
  }
  
  return(data)
}

load_gtap_macro_data <- function()
{
  macro_data_file <- paste0(folder,'Macro_updated_24_07_24 1.xlsx')
  macro_data      <- tibble(NULL)
  
  sheets          <- readxl::excel_sheets(macro_data_file)
  sheets_to_skip  <- c("Expected R")
  
  for(sheet in sheets)
  {
    if(sheet %in% sheets_to_skip)
      next
    
    tmp               <- readxl::read_xlsx(macro_data_file,sheet = sheet)
    tmp               <- tmp[,1:4]
    econ_variable     <- colnames(tmp)[1]
    tmp$econ_variable <- econ_variable
    
    colnames(tmp)[1]  <- "region"
    tmp <- tmp %>% pivot_longer(-c(region,econ_variable), names_to = 'scenario', values_to = 'value')
    
    macro_data <- bind_rows(macro_data,tmp)
  }
  
  macro_data <- macro_data %>% filter(econ_variable %in% c('yp', 'qgdp','pgdp','tot','qxwreg'))
  
  return(macro_data)
}

data    <- load_gtap_results_data()
regions <- data$region |> unique()

# aggregate all countries other than US, Mexico and Canada into 'Rest of World'
tmp_data <- data %>% filter(!(region %in% c('US','Mexico','Canada', 'Australia') ) )
data_red <- data %>% filter(region %in% c('US','Mexico','Canada', 'Australia') )

tmp_data <- tmp_data %>%group_by(sector,econ_variable,scenario) %>% summarise(value = sum(value))
tmp_data$region <- "RestofWorld"

data_red <- bind_rows(data_red,tmp_data)

data_red <- data_red %>%
  mutate(econ_variable = case_when(econ_variable=='po' ~ 'Production costs (%)',
                                   econ_variable=='qo' ~ 'Output activities (%)',
                                   econ_variable=='qid' ~ 'Sectoral Investment (%)',
                                   econ_variable=='qxw' ~ 'Export quantity (%)',
                                   econ_variable=='qes[UnSkLab**]' ~ 'Employment (%)' ),
         sector        = case_when(sector=='OtherSec' ~ 'Other sectors',
                                   sector=='Rwmk' ~ 'Raw Milk',
                                   sector=='DrP' ~ 'Dairy Products',
                                   sector=='Meats' ~ 'Meats',
                                   sector=='Cattle' ~ 'Cattle',
                                   TRUE ~ sector),
         scenario      = case_when( str_starts(scenario, "Global_s1")~'Scenario 1',
                                    str_starts(scenario, "Global_s2")~'Scenario 2',
                                    str_starts(scenario, "Global_s3")~'Scenario 3')) %>%
  filter(scenario != 'Scenario 1')

data_red$region        <- factor(data_red$region, levels = c('Australia', 'Canada', 'Mexico','RestofWorld', 'US'))
data_red$econ_variable <- factor(data_red$econ_variable, levels = c('Production costs (%)', 'Export quantity (%)', 'Employment (%)', 'Sectoral Investment (%)', 'Output activities (%)'))
avg_data               <- data_red %>% group_by(region,econ_variable,scenario) %>% summarise(value=mean(value))

point_data_tmp <- data_red %>%
  filter(!(sector %in% c('Other sectors','Raw Milk','Dairy Products', 'Meats', 'Cattle'))) %>%
  group_by(region, econ_variable,scenario) %>%
  summarise(value = sum(value))
point_data_tmp$sector = 'Other sectors'

point_data <- data_red %>%
  filter(sector %in% c('Other sectors','Raw Milk','Dairy Products', 'Meats', 'Cattle'))
point_data <- bind_rows(point_data,point_data_tmp)

sector_plot <- data_red %>%
  group_by(region, econ_variable, scenario) %>%
  ggplot(aes(y=value,x=region, fill = scenario)) +
  # geom_bar(data = avg_data, stat = "identity",
  #          aes(value,x=region,fill=scenario),
  #          alpha=0.2,position='dodge2',width=0.85) +
  geom_boxplot(position = position_dodge(width = 0.9), alpha = 0.3, outlier.shape = NA) +
  geom_point(data = point_data, aes(fill=scenario,col=sector),position=position_dodge2(width = 0.85)) +
  scale_fill_manual(values = c("Scenario 2" = "lightblue","Scenario 3" = "brown1")) +
  scale_color_manual(values = c("Scenario 2" = "lightblue","Scenario 3" = "brown1")) +
  theme_light() +
  scale_color_lancet() +
  theme( axis.text.x = element_text( angle = 65, hjust = 1, size = 10 ),
         strip.text = element_text( color = "black"),
         strip.background =element_rect(fill="grey90")) +
  facet_wrap(~econ_variable, scales="free_y",nrow=1) +
  xlab('') + ylab('Sectoral impact (%)')+
  labs(color = "Sector", fill = "Scenario") +
  guides(color = guide_legend(order = 1), fill = guide_legend(order = 2))

# Regional impact / GDP
macro_data <- load_gtap_macro_data()

tmp_macro_data <- macro_data %>% filter(!(region %in% c('US','Mexico','Canada', 'Australia') ) )
macro_data_red <- macro_data %>% filter(region %in% c('US','Mexico','Canada', 'Australia') )

tmp_macro_data <- tmp_macro_data %>%group_by(econ_variable,scenario) %>% summarise(value = sum(value))
tmp_macro_data$region <- "RestofWorld"

macro_data_red$region        <- factor(macro_data_red$region, levels = c('RestofWorld', 'Australia', 'Canada', 'Mexico', 'US'))

macro_data_red <- bind_rows(macro_data_red,tmp_macro_data) %>%
  mutate(scenario = case_when(scenario=='S1' ~ 'Scenario 1',
                              scenario=='S2' ~ 'Scenario 2',
                              scenario=='S3' ~ 'Scenario 3'),
         value = case_when(econ_variable=='EV (million_USD)' ~ value / 1000,
                           TRUE ~ value),
         econ_variable = case_when(econ_variable=='yp' ~ 'Consumption (%)',
                                   econ_variable=='pgdp' ~ 'Price change (%)',
                                   econ_variable=='qgdp' ~ 'GDP (%)',
                                   econ_variable=='qxwreg' ~ 'Export quantity (%)',
                                   econ_variable=='tot' ~ 'Terms of Trade (%)'))

macro_data_red$econ_variable <- factor(macro_data_red$econ_variable, levels = c('Price change (%)', 'Consumption (%)', 'GDP (%)','Export quantity (%)'))

macro_plot <- macro_data_red %>% 
  ggplot(aes(x = region, y = value, fill = scenario, col = scenario)) +
  geom_bar(stat = "identity", position = 'dodge2', width = 0.75, alpha = 0.7) +
  scale_fill_manual(values = c("Scenario 1" = "lightgreen", "Scenario 2" = "lightblue", "Scenario 3" = "brown1")) +
  scale_color_manual(values = c("Scenario 1" = "lightgreen", "Scenario 2" = "lightblue", "Scenario 3" = "brown1")) +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 65, hjust = 1, size = 10),
    strip.text = element_text(color = "black"),
    strip.background = element_rect(fill = "grey90"),
    legend.position = 'right' # Changement de la position de la l?gende
  ) +
  facet_wrap(~econ_variable, scales = "free_y", nrow = 1) +
  xlab('') + 
  ylab('Macroeconomic impact (%)') +
  labs(fill = "Scenario", color = "Scenario") # Ajout des titres de l?gende

# Shocks from lit
assumptions = c(list(`Demand raw milk` = list(`S1`= 0, `S2` = .15, `S3`=0.5)),
                list(`Productivity raw milk` = list(`S1`= .05, `S2` = .15, `S3`=0.5)),
                list(`Demand dairy` = list(`S1`= 0, `S2` = .1, `S3`=0.25)),
                list(`Productivity  dairy` = list(`S1`= .025, `S2` = .1, `S3`=0.2)),
                list(`Demand meat` = list(`S1`= 0, `S2` = .05, `S3`=0.25)),
                list(`Productivity meat` = list(`S1`= 0, `S2` = .05, `S3`=0.20)),
                list(`Productivity cattle` = list(`S1`= .025, `S2` = .1, `S3`=0.2)),
                list(`Export restriction raw milk` = list(`S1`= 0, `S2` = .75, `S3`=0.98)),
                list(`Export restriction dairy`    = list(`S1`= 0, `S2` = .15, `S3`=0.75)),
                list(`Export restriction meat`     = list(`S1`= 0, `S2` = .15, `S3`=0.75)),
                list(`Export restriction cattle`   = list(`S1`= 0, `S2` = .15, `S3`=0.75)),
                list(`Subsidy by sector` = list(S1=0.041, S2=0.082,S3=0.164)),
                list(`Input productivity` = list(S1 = 0.01, S2 = 0.05, S3 = 0.1)))

df <- do.call(rbind, lapply(assumptions, data.frame, stringsAsFactors=FALSE))
df$factor <- rownames(df)

assumptions_tbl <- tibble(df) %>% dplyr::select(factor,S1,S2,S3) %>%
  tidyr::pivot_longer(-factor, names_to = 'scenario', values_to = 'value') %>%
  mutate(scenario = case_when(scenario=='S1' ~ 'Scenario 1',
                              scenario=='S2' ~ 'Scenario 2',
                              scenario=='S3' ~ 'Scenario 3') )

assumptions_plot <- assumptions_tbl %>% ggplot(aes(y=factor,x=value,col=scenario)) +
  geom_point(alpha=0.7) +
  scale_fill_manual(values = c("Scenario 1" = "lightgreen","Scenario 2" = "lightblue","Scenario 3" = "brown1")) +
  scale_color_manual(values = c("Scenario 1" = "lightgreen","Scenario 2" = "lightblue","Scenario 3" = "brown1")) +
  theme_light() +
  theme( axis.text.x = element_text( angle = 0, hjust = 1, size = 10 ),
         axis.text.y = element_text( angle = 0, hjust = 1, size = 10 ),
         strip.text = element_text( color = "black"),
         strip.background =element_rect(fill="grey90"),
         legend.position = 'none') +
  xlab('') + ylab('Assumed shocks')

# combined econ plot
layout_design <- "
ABBBB
CCCCC
"
Figure_2 <- assumptions_plot + macro_plot + sector_plot +
  plot_annotation(tag_levels ='A') +
  plot_layout(design = layout_design, guides = 'collect')

ggsave('Figure_2.png', Figure_2, width = 20, height = 10, units = 'in', dpi = 300)


