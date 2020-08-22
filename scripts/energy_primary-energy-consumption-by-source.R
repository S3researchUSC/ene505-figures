#  ---------------------------------------------------- INPUT DATA ----------------------------------------------------

data.file     = 'Table_1.3_Primary_Energy_Consumption_by_Source.xlsx' 

#  --------------------------------------------------- MAIN SCRIPT ---------------------------------------------------

# load libraries -------

  library(data.table)
  library(openxlsx)
  library(stringr)
  library(plyr)
  library(lubridate)
  library(ggplot2)
  library(hrbrthemes)
  library(directlabels)
  library(grid)
  library(extrafont)

# source color palettes and plot themes ------

  items = list.files(here::here('src'))
  sapply(here::here('src', items), source)

# load data ------
  
  dt_month = as.data.table(read.xlsx(here::here('data', data.file), sheet = 'Monthly Data', startRow = 11, detectDates = T))
  dt_month = dt_month[2:nrow(dt_month)]
  
  dt_annual = as.data.table(read.xlsx(here::here('data', data.file), sheet = 'Annual Data', startRow = 11, detectDates = T))
  dt_annual = dt_annual[2:nrow(dt_annual)]
  
# rename columns ----
  
  colnames(dt_annual) = c('year', 'Coal', 'Natural Gas', 'Petroleum', 'Total Fossil Fuels', 'Nuclear', 
                          'Hydroelectric', 'Geothermal', 'Solar', 'Wind', 'Biomass', 'Total Renewables', 'Total Primary Energy')
  
  colnames(dt_month) = c('month', 'Coal', 'Natural Gas', 'Petroleum', 'Total Fossil Fuels', 'Nuclear', 
                         'Hydroelectric', 'Geothermal', 'Solar', 'Wind', 'Biomass', 'Total Renewables', 'Total Primary Energy')
  
# melt data table from wide to long format -----
  
  dt_month = melt(dt_month, measure.vars = colnames(dt_month)[2:13],
                  variable.name = 'fuel', value.name = 'value')
  
  dt_annual = melt(dt_annual, measure.vars = colnames(dt_annual)[2:13],
                   variable.name = 'fuel', value.name = 'value')
  
# use lubridate package to create year and month columns ------
  
  dt_month[, year := year(month)]
  dt_month[, month_val := month(month)]
  dt_month[, month_name := month(month, label = T)]
  
# convert value column to numeric ----
  
  dt_month[, value := as.numeric(value)]
  dt_annual[, value := as.numeric(value)]
  
# remove NA month or year entries ----
  
  dt_month = dt_month[!is.na(month)]
  dt_annual = dt_annual[!is.na(year)]
  
# keep renewables only ------
  
  dt_annual_re = dt_annual[! fuel %in% c("Coal", "Petroleum", "Natural Gas", "Nuclear") ]
  dt_month_re = dt_month[! fuel %in% c("Coal", "Petroleum", "Natural Gas", "Nuclear") ]
  
# combine non-hydro into 'Other Renewables' ------
  
  dt_annual[, fuel2 := fuel]
  dt_annual[ fuel %in% c('Biomass', 'Geothermal', 'Solar', 'Wind'), fuel2 := 'Other Renewables' ]
  
  dt_month[, fuel2 := fuel]
  dt_month[ fuel %in% c('Biomass', 'Geothermal', 'Solar', 'Wind'), fuel2 := 'Other Renewables' ]
  
# aggregate by fuel ------
  
  dt_annual_agg = dt_annual[, .(value = sum(value, na.rm = T)), by = .(year, fuel2)]
  dt_month_agg = dt_month[, .(value = sum(value, na.rm = T)), by = .(month, year, month_val, month_name, fuel2 )]

# calculate proportion (or percentage) contributed by each fuel type, for each month, for each year -----
  
  dt_month_agg[, prop := value/sum(value, na.rm = T), by = c("year", "month_name")]
  dt_annual_agg[, prop := value/sum(value, na.rm = T), by = c("year")]
  
# separate totals from main ------
  
  dt_annual_main = dt_annual_agg[ ! fuel2 %in% c('Total Fossil Fuels', 'Total Renewables', 'Total Primary Energy')]
  dt_annual_tot = dt_annual_agg[  fuel2 %in% c('Total Fossil Fuels', 'Total Renewables', 'Total Primary Energy')]
  
  dt_annual_main[, prop := value/sum(value, na.rm = T), by = c('year')]
  
# get all fuels, non totals ------
  
  dt_annual_fuels = dt_annual[ ! fuel %in% c('Total Fossil Fuels', 'Total Renewables', 'Total Primary Energy')]
  dt_annual_fuels[, prop := value/sum(value, na.rm = T), by = c('year')]
  
  
#  ------------------------------------------------------- FIGURES -------------------------------------------------------
  
  
  # line, annual -------
    
    # create dataset of where to put the labels on line chart
    labs_line = dt_annual_main[year == max(year)]
    labs_line = labs_line[order(rank(value))]
    labs_line[2:3, position := c(8,9.5)]
    labs_line[is.na(position), position := value]
    
    fig_line_annual = ggplot(dt_annual_main, aes(x = year, y = value, group = fuel2, color = fuel2)) + 
      geom_line(size = 0.9) +
      labs(title = 'Annual U.S. primary energy consumption by source (1949-2019)',
           subtitle = 'Quadrillion BTU', 
           caption = 'Data: U.S. Energy Information Administration',
           x = NULL,
           y = NULL) +
      guides(color = 'none') +
      scale_x_continuous(breaks = seq(1949,2019,5), limits = c(1949, 2019), expand = c(0,0)) +
      scale_y_continuous(labels = scales::comma, expand = c(0,0)) +
      scale_color_manual(values = pal_fuel) + 
      theme_line +
      geom_text(data = labs_line, aes(x = Inf, y = position, label = paste0(' ', fuel2), color = fuel2), hjust = 0,
                size = 6, fontface = 'plain', family = 'Secca Soft')

    fig_line_annual = ggplotGrob(fig_line_annual)
    fig_line_annual$layout$clip[fig_line_annual$layout$name == "panel"] = "off"
    
    ggsave(fig_line_annual, 
           filename = here::here('figures', 'energy_primary-energy-consumption-by-source_annual_1949-2019_lts.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'energy_primary-energy-consumption-by-source_annual_1949-2019_lts.pdf'),
                outfile = here::here('figures', 'energy_primary-energy-consumption-by-source_annual_1949-2019_lts.pdf'))
    
    # save as png:
    # ggsave(fig_line_annual, 
    #        filename = here::here('figures', 'energy_primary-energy-consumption-by-source_annual_1949-2019_lts.png'), 
    #        width = 11.5, 
    #        height = 6.25, 
    #        dpi = 600)
    
  # area, annual (absolute) -------
    
    # create dataset of where to put the labels on area chart
    labs_area = dt_annual_main[year == max(year)][order(factor(fuel2, levels = rev(levels(factor(dt_annual_main[, fuel2])))))]
    labs_area[, cum_sum := cumsum(value)] 
    labs_area[, difference := diff(c(0,cum_sum))/2]
    labs_area[, position := cum_sum - difference]
    
    
    fig_area_annual_abs = ggplot(dt_annual_main, 
                                 aes(x = year, y = value, group = fuel2, fill = fuel2)) + 
      geom_area() +
      labs(title = 'Annual U.S. primary energy consumption by source (1949-2019)',
           subtitle = 'Quadrillion BTU', 
           caption = 'Data: U.S. Energy Information Administration',
           x = NULL,
           y = NULL,
           fill = NULL) +
      scale_x_continuous(breaks = seq(1949,2019,5), limits = c(1949, 2019), expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) +
      scale_fill_manual(values = pal_fuel) + 
      scale_color_manual(values = pal_fuel) + 
      guides(fill = 'none',
             color = 'none') +
      theme_area_labeled +
      geom_text(data = labs_area, aes(x = Inf, y = position, label = paste0(' ', fuel2), color = fuel2), 
                hjust = 0, size = 6.5, fontface = 'plain', family = 'Secca Soft') 
    
    fig_area_annual_abs = ggplotGrob(fig_area_annual_abs)
    fig_area_annual_abs$layout$clip[fig_area_annual_abs$layout$name == "panel"] = "off"
    
    ggsave(fig_area_annual_abs, 
           filename = here::here('figures', 'energy_primary-energy-consumption-by-source_annual_1949-2019_ats_absolute.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'energy_primary-energy-consumption-by-source_annual_1949-2019_ats_absolute.pdf'),
                outfile = here::here('figures', 'energy_primary-energy-consumption-by-source_annual_1949-2019_ats_absolute.pdf'))
    
    # save as png: 
    # ggsave(fig_area_annual_abs, 
    #        filename = here::here('figures', 'energy_primary-energy-consumption-by-source_annual_1949-2019_ats_absolute.png'), 
    #        width = 11.5, 
    #        height = 6.25, 
    #        dpi = 600)
    
  # area, annual (proportion) -------
    
    # create dataset of where to put the labels on area chart
    labs_area_prop = dt_annual_main[year == max(year)][order(factor(fuel2, levels = rev(levels(factor(dt_annual_main[, fuel2])))))]
    labs_area_prop[, cum_sum := cumsum(prop)] 
    labs_area_prop[, difference := diff(c(0,cum_sum))/2]
    labs_area_prop[, position := cum_sum - difference]
    
    fig_area_annual_prop = ggplot(dt_annual_main, aes(x = year, y = prop, group = fuel2, fill = fuel2)) + 
      geom_area() +
      labs(title = 'Annual U.S. primary energy consumption by source (1949-2019)',
           subtitle = 'Share of total primary energy consumption', 
           caption = 'Data: U.S. Energy Information Administration',
           x = NULL,
           y = NULL,
           fill = NULL) +
      scale_x_continuous(breaks = seq(1949,2019,5), limits = c(1949, 2019), expand = c(0,0)) +
      scale_y_continuous(labels = scales::percent, expand = c(0,0)) +
      scale_fill_manual(values = pal_fuel) + 
      scale_color_manual(values = pal_fuel) + 
      guides(fill = 'none',
             color = 'none') +
      theme_area_labeled + 
      geom_text(data = labs_area_prop, aes(x = Inf, y = position, label = paste0(' ', fuel2), color = fuel2), 
                hjust = 0, size = 6.5, fontface = 'plain', family = 'Secca Soft')  
    
    fig_area_annual_prop = ggplotGrob(fig_area_annual_prop)
    fig_area_annual_prop$layout$clip[fig_area_annual_prop$layout$name == "panel"] = "off"
    
    ggsave(fig_area_annual_prop, 
           filename = here::here('figures', 'energy_primary-energy-consumption-by-source_annual_1949-2019_ats_proportion.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'energy_primary-energy-consumption-by-source_annual_1949-2019_ats_proportion.pdf'),
                outfile = here::here('figures', 'energy_primary-energy-consumption-by-source_annual_1949-2019_ats_proportion.pdf'))
    
  # bar, 2019 ---------
    
    fig_bar_2019 = ggplot(dt_annual_fuels[year == max(year)], 
                          aes(x = reorder(fuel, value), y = value, group = fuel, fill = fuel)) + 
      geom_bar(stat = "identity") +
      labs(title = 'Annual U.S. primary energy consumption by source (2019)',
           subtitle = 'Quadrillion BTU', 
           caption = 'Data: U.S. Energy Information Administration',
           x = NULL,
           y = NULL, 
           fill = NULL) +
      # scale_x_continuous() +
      scale_y_continuous(labels = scales::comma, expand = c(0,0), breaks = seq(0,35,10), limits = c(0,40)) +
      scale_color_manual(values = pal_fuel) +
      scale_fill_manual(values = pal_fuel) + 
      guides(fill = 'none',
             color ='none') +
      theme_bar_flipped + 
      geom_text(data = dt_annual_fuels[year == max(year)], aes(x = fuel, y = value + 1.5, label = paste0(signif(prop*100,2), '%'), color = fuel), hjust = 0.5,
                size = 6, fontface = 'bold', family = 'Secca Soft') +
      coord_flip()
    
    ggsave(fig_bar_2019, 
           filename = here::here('figures', 'energy_primary-energy-consumption-by-source_2019_bar.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'energy_primary-energy-consumption-by-source_2019_bar.pdf'),
                outfile = here::here('figures', 'energy_primary-energy-consumption-by-source_2019_bar.pdf'))
    
    