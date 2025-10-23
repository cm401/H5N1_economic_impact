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
library(ggplot2)

folder <- ' '<insert_your_local_path_here>/Replication package/Data_Results/'

global_s1 <- paste0(folder,'S1R1-28-04.xlsx')
global_s2 <- paste0(folder,'S2R1-28-04.xlsx')
global_s3 <- paste0(folder,'S3R1-29-04.xlsx')

data <- data.frame()

for (scenario in c(global_s1, global_s2, global_s3)) {
  
  sheets <- excel_sheets(scenario)
  name <- str_replace(basename(scenario), ".xlsx", "")
  
  main_sheets <- sheets[!grepl("(_SD|_M)$", sheets) & sheets != "Description"]
  
  for (sheet in main_sheets) {
    
    tmp <- read_xlsx(scenario, sheet = sheet)
    if (nrow(tmp) == 0) next
    
    colnames(tmp)[1] <- "Sector"
    tmp$econ_variable <- sheet
    
    tmp <- pivot_longer(tmp, 
                        cols = -c(Sector, econ_variable), 
                        names_to = "region", 
                        values_to = "value") %>%
      mutate(value = as.numeric(value),
             scenario = name)
    
    data <- bind_rows(data, tmp)
  }
}

regions <- data$region |> unique()

data_red <- data %>% filter(region %in% c('US','Mexico','Canada','Oceania','LatinAmer', 'WestEurope', 'EastAsia','RestofWorld') )

data_red <- data_red %>%
  mutate(
    econ_variable = case_when(
      econ_variable=='PO'  ~ 'Production costs (%)',
      econ_variable=='QO'  ~ 'Output activities (%)',
      econ_variable=='INV' ~ 'Sectoral Investment (%)',
      econ_variable=='QXW' ~ 'Export quantity (%)',
      econ_variable=='QES' ~ 'Employment (%)'
    ),
    Sector = case_when(
      Sector=='OtherSec'     ~ 'Other Sectors',
      Sector=='Rwmk'         ~ 'Raw Milk',
      Sector=='Drp'          ~ 'Dairy Products',
      Sector=='Meats'        ~ 'Meats',
      Sector=='Cattle'       ~ 'Cattle',
      Sector=='GrainsCrops'  ~ 'Crops',
      Sector=='MeatLstk'     ~ 'Other Meat',
      Sector=='ProcFood'     ~ 'Processed Food',
      Sector=='Mnfc'         ~ 'Manufacturing',
      TRUE ~ Sector
    ),
    scenario = case_when(
      str_starts(scenario,"S1R1-28-04") ~ 'Scenario 1',
      str_starts(scenario,"S2R1-28-04") ~ 'Scenario 2',
      str_starts(scenario,"S3R1-29-04") ~ 'Scenario 3'
    )
  ) %>%
  filter(scenario != 'Scenario 1')

data_red$region        <- factor(data_red$region, levels = c('Canada','Mexico','US','Oceania','LatinAmer','WestEurope','EastAsia','RestofWorld'))
data_red$econ_variable <- factor(data_red$econ_variable, levels = c('Production costs (%)','Export quantity (%)','Employment (%)','Sectoral Investment (%)','Output activities (%)'))

point_sectors <- c('Raw Milk','Dairy Products','Meats','Cattle','Crops','Other Meat','Processed Food','Manufacturing','Other Sectors')
point_data <- data_red %>% filter(Sector %in% point_sectors)

### SECTORAL PLOT #####

Sector_plot <- data_red %>%
  filter(econ_variable != "Output activities (%)") %>%
  group_by(region, econ_variable, scenario) %>%
  ggplot(aes(y = value, x = region, fill = scenario)) +
  geom_boxplot(position = position_dodge(width = 0.9), alpha = 0.3, outlier.shape = NA) +
  
  geom_point(
    data = point_data %>% filter(econ_variable != "Output activities (%)"),
    aes(fill = scenario, col = Sector),
    position = position_dodge2(width = 0.85)
  ) +
  
  scale_y_continuous(
    trans = scales::asinh_trans(),
    breaks = c(-500, -200, -100, -50, -20, -10, 0, 10, 20, 50, 100, 200, 500)
  ) +
  scale_fill_manual(values = c("Scenario 2" = "lightblue", "Scenario 3" = "brown1")) +
  scale_color_manual(values = c("Scenario 2" = "lightblue", "Scenario 3" = "brown1")) +
  theme_light() +
  scale_color_lancet() +
  theme(
    axis.text.x = element_text(angle = 65, hjust = 1, size = 10),
    strip.text = element_text(color = "black"),
    strip.background = element_rect(fill = "grey90")
  ) +
  facet_wrap(~econ_variable, scales = "free_y", nrow = 2, ncol = 2) +
  xlab('') +
  ylab('Sectoral impact (%)') +
  labs(color = "Sector", fill = "Scenario") +
  guides(color = guide_legend(order = 1), fill = guide_legend(order = 2))

Sector_plot

### QO SECTOR PLOT ####

Sector_plot_GDP <- data_red %>%
  filter(econ_variable == "Output activities (%)") %>%
  group_by(region, econ_variable, scenario) %>%
  ggplot(aes(y = value, x = region, fill = scenario)) +
  geom_boxplot(position = position_dodge(width = 0.9), alpha = 0.3, outlier.shape = NA) +
  geom_point(
    data = point_data %>% filter(econ_variable == "Output activities (%)"),
    aes(fill = scenario, col = Sector),
    position = position_dodge2(width = 0.85)
  ) +
  scale_y_continuous(
    trans = scales::asinh_trans(),
    breaks = c(-500, -200, -100, -50, -20, -10, 0, 10, 20, 50, 100, 200, 500)
  ) +
  scale_fill_manual(values = c("Scenario 2" = "lightblue", "Scenario 3" = "brown1")) +
  scale_color_manual(values = c("Scenario 2" = "lightblue", "Scenario 3" = "brown1")) +
  theme_light() +
  scale_color_lancet() +
  theme(
    axis.text.x = element_text(angle = 65, hjust = 1, size = 10),
    strip.text = element_text(color = "black"),
    strip.background = element_rect(fill = "grey90")
  ) +
  facet_wrap(~econ_variable, scales = "free_y", nrow = 1) +
  xlab('') + ylab('Output Sectoral impact (%)') +
  labs(color = "Sector", fill = "Scenario") +
  guides(color = guide_legend(order = 1), fill = guide_legend(order = 2))

#### MACRO PLOT #####

macro_data_file <- paste0(folder,'MacroR1-28-04.xlsx')
macro_data      <- tibble(NULL)

sheets          <- readxl::excel_sheets(macro_data_file)
sheets_to_skip  <- c("Description")

for(sheet in sheets) {
  if(sheet %in% sheets_to_skip || grepl("(_SD|_M)$", sheet))
    next
  
  tmp <- readxl::read_xlsx(macro_data_file, sheet = sheet)[, 1:4]
  econ_variable <- sheet
  tmp$econ_variable <- econ_variable
  colnames(tmp)[1] <- "region"
  tmp <- tmp %>% pivot_longer(-c(region, econ_variable), names_to = 'scenario', values_to = 'value')
  
  sd_sheet <- paste0(sheet, "_SD")
  if(sd_sheet %in% sheets) {
    tmp_sd <- readxl::read_xlsx(macro_data_file, sheet = sd_sheet)[, 1:4]
    tmp_sd$econ_variable <- econ_variable
    colnames(tmp_sd)[1] <- "region"
    tmp_sd <- tmp_sd %>% pivot_longer(-c(region, econ_variable), names_to = 'scenario', values_to = 'sd')
  
    tmp <- left_join(tmp, tmp_sd, by = c("region", "scenario", "econ_variable"))
  } else {
    tmp$sd <- NA_real_
  }
  
  macro_data <- bind_rows(macro_data, tmp)
}

macro_data <- macro_data %>% filter(econ_variable %in% c('Consumption', 'qgdp', 'pgdp', 'INV', 'Trade'))

macro_data_red <- macro_data %>% filter(region %in% c('US', 'Mexico', 'Canada','Oceania','LatinAmer', 'WestEurope', 'EastAsia','RestofWorld'))

macro_data_red$region <- factor(macro_data_red$region, levels = c('Canada', 'Mexico', 'US','Oceania','LatinAmer', 'WestEurope', 'EastAsia', 'RestofWorld'))

macro_data_red <- macro_data_red %>%
  mutate(
    scenario = case_when(
      scenario == 'S1' ~ 'Scenario 1',
      scenario == 'S2' ~ 'Scenario 2',
      scenario == 'S3' ~ 'Scenario 3',
      TRUE ~ scenario
    ),
    econ_variable = case_when(
      econ_variable == 'Consumption' ~ 'Consumption (%)',
      econ_variable == 'pgdp' ~ 'Price change (%)',
      econ_variable == 'qgdp' ~ 'GDP (%)',
      econ_variable == 'Trade' ~ 'Export quantity (%)',
      econ_variable == 'INV' ~ 'Investment (%)',
      TRUE ~ econ_variable
    )
  )

macro_data_red$econ_variable <- factor(
  macro_data_red$econ_variable,
  levels = c('Price change (%)', 'Consumption (%)', 'GDP (%)', 'Export quantity (%)', 'Investment (%)')
)

macro_plot <- macro_data_red %>%
  filter(econ_variable != "GDP (%)") %>%
  ggplot(aes(x = region, y = value, fill = scenario, col = scenario)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.75, alpha = 0.7)+
  scale_fill_manual(values = c("Scenario 1" = "lightgreen", "Scenario 2" = "lightblue", "Scenario 3" = "brown1")) +
  scale_color_manual(values = c("Scenario 1" = "lightgreen", "Scenario 2" = "lightblue", "Scenario 3" = "brown1")) +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 65, hjust = 1, size = 10),
    strip.text = element_text(color = "black"),
    strip.background = element_rect(fill = "grey90"),
    legend.position = 'right'
  ) +
  facet_wrap(~econ_variable, scales = "free_y", nrow = 2, ncol = 2) +
  xlab('') +
  ylab('Macroeconomic impact (%)') +
  labs(fill = "Scenario", color = "Scenario")

macro_plot

### GDP MACRO stand-alone ####

gdp_plot <- macro_data_red %>%
  filter(econ_variable == "GDP (%)") %>%
  ggplot(aes(x = region, y = value, fill = scenario, col = scenario)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.75, alpha = 0.7) +
  scale_fill_manual(values = c("Scenario 1" = "lightgreen", "Scenario 2" = "lightblue", "Scenario 3" = "brown1")) +
  scale_color_manual(values = c("Scenario 1" = "lightgreen", "Scenario 2" = "lightblue", "Scenario 3" = "brown1")) +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 65, hjust = 1, size = 10),
    strip.text = element_text(color = "black"),
    strip.background = element_rect(fill = "grey90"),
    legend.position = 'right'
  ) +
  xlab('') + 
  ylab('GDP impact (%)') +
  labs(fill = "Scenario", color = "Scenario") +
  facet_wrap(~econ_variable)


### COMBINED PLOT UPDATED ###

layout_design1 <- "AABB"

GDP_plot_1=gdp_plot+ Sector_plot_GDP+
  plot_annotation(tag_levels ='A') +
  plot_layout(design = layout_design1, guides = 'collect')

folder_figure <- ' '<insert_your_local_path_here>/Replication package/Figure/'

ggsave(
  filename = file.path(folder_figure, "GDP_plot_1.pdf"),
  plot = GDP_plot_1,
  device = cairo_pdf,
  dpi = 500,
  width = 12, height = 7, units = "in"
)

ggsave(
  filename = file.path(folder_figure, "Macro_plot.pdf"),
  plot = macro_plot,
  device = cairo_pdf,
  dpi = 500,
  width = 12, height = 7, units = "in"
)

ggsave(
  filename = file.path(folder_figure, "Sector_plot.pdf"),
  plot = Sector_plot,
  device = cairo_pdf,
  dpi = 500,
  width = 12, height = 7, units = "in"
)
