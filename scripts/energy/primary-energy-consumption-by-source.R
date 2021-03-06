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
  
# create year and month columns ------
  
  dt_month[, year := year(month)]
  dt_month[, month_val := month(month)]
  dt_month[, month_name := month(month, label = T)]
  
# convert value column to numeric ----
  
  dt_month[, value := as.numeric(value)]
  dt_annual[, value := as.numeric(value)]
  
# remove NA month or year entries ----
  
  dt_month = dt_month[!is.na(month)]
  dt_annual = dt_annual[!is.na(year)]
  
# remove NA month or year entries ----
  
  dt_month = dt_month[!is.na(month)]
  dt_annual = dt_annual[!is.na(year)]
  
# get totals ------
  
  dt_annual_tot = dt_annual[ fuel %like% 'Total']
  dt_annual = dt_annual[ ! fuel %like% 'Total']
  
  dt_month_tot = dt_month[ fuel %like% 'Total']
  dt_month = dt_month[ ! fuel %like% 'Total']
  
# get renewables -------
  
  dt_annual_re = dt_annual[ fuel %in% c('Wind', 'Hydroelectric', 'Solar', 'Wind', 'Waste', 'Geothermal')]
  dt_month_re = dt_month[ fuel %in% c('Wind', 'Hydroelectric', 'Solar', 'Wind', 'Waste', 'Geothermal')]
  
# combine non-hydro into 'Other Renewables' ------
  
  dt_annual[, fuel2 := fuel]
  dt_annual[ fuel %in% c('Biomass', 'Geothermal', 'Solar', 'Wind'), fuel2 := 'Other Renewables' ]
  
  dt_month[, fuel2 := fuel]
  dt_month[ fuel %in% c('Biomass', 'Geothermal', 'Solar', 'Wind'), fuel2 := 'Other Renewables' ]
  
# aggregate by fuel ------
  
  dt_annual_agg = dt_annual[, .(value = sum(value, na.rm = T)), by = .(year, fuel2)]
  dt_month_agg = dt_month[, .(value = sum(value, na.rm = T)), by = .(month, year, month_val, month_name, fuel2 )]
  
# calculate proportions ------
  
  dt_annual[, prop := value/sum(value, na.rm = T), by = c('year')]
  dt_annual_agg[, prop := value/sum(value, na.rm = T), by = c('year')]
  dt_annual_tot[! fuel == 'Total Primary Energy' , prop := value/sum(value, na.rm = T), by = c('year')]
  dt_annual_re[, prop := value/sum(value, na.rm = T), by = c('year')]
  
  dt_month_re[, prop := value/sum(value, na.rm = T), by = c('month', 'year', 'month_val', 'month_name', 'fuel')]
  
#  ------------------------------------------------------- FIGURES -------------------------------------------------------
  
  # bar, 2019 ---------
  
  fig_bar_2019 = ggplot(dt_annual[year == max(year)], 
                        aes(x = reorder(fuel, value), y = value, group = fuel, fill = fuel)) + 
    geom_bar(stat = "identity") +
    labs(title = 'Annual U.S. primary energy consumption by source (2019)',
         subtitle = NULL, 
         caption = 'Data: U.S. Energy Information Administration',
         x = NULL,
         y = 'Quadrillion BTU', 
         fill = NULL) +
    # scale_x_continuous() +
    scale_y_continuous(labels = scales::comma, expand = c(0,0), breaks = seq(0,35,10), limits = c(0,40)) +
    scale_color_manual(values = pal_fuel) +
    scale_fill_manual(values = pal_fuel) + 
    guides(fill = 'none',
           color ='none') +
    theme_bar_flipped + 
    geom_text(data = dt_annual[year == max(year)], aes(x = fuel, y = value + 1.5, label = paste0(signif(prop*100,2), '%'), color = fuel), hjust = 0.5,
              size = 6, fontface = 'bold', family = 'Secca Soft') +
    coord_flip()
  
  ggsave(fig_bar_2019, 
         filename = here::here('figures', 'energy', 'primary-energy-consumption-by-source_2019_bar.pdf'), 
         width = 11.5, 
         height = 6.25)
  
  embed_fonts(here::here('figures', 'energy', 'primary-energy-consumption-by-source_2019_bar.pdf'),
              outfile = here::here('figures', 'energy', 'primary-energy-consumption-by-source_2019_bar.pdf'))
  
  
  # line, annual -------
    
    # create dataset of where to put the labels on line chart
    labs_line = dt_annual_agg[year == max(year)]
    labs_line = labs_line[order(rank(value))]
    labs_line[2:3, position := c(8,9.5)]
    labs_line[is.na(position), position := value]
    
    fig_line_annual = ggplot(dt_annual_agg, aes(x = year, y = value, group = fuel2, color = fuel2)) + 
      geom_line(size = 0.9) +
      labs(title = 'Annual U.S. primary energy consumption by source (1949-2019)',
           subtitle = 'Quadrillion BTU', 
           caption = 'Data: U.S. Energy Information Administration',
           x = NULL,
           y = NULL) +
      guides(color = 'none') +
      scale_x_continuous(breaks = seq(1949,2019,5), limits = c(1949, 2019), expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) +
      scale_color_manual(values = pal_fuel) + 
      theme_line +
      geom_text(data = labs_line, aes(x = Inf, y = position, label = paste0(' ', fuel2), color = fuel2), hjust = 0,
                size = 6.5, fontface = 'plain', family = 'Secca Soft')

    fig_line_annual = ggplotGrob(fig_line_annual)
    fig_line_annual$layout$clip[fig_line_annual$layout$name == "panel"] = "off"
    
    ggsave(fig_line_annual, 
           filename = here::here('figures', 'energy', 'primary-energy-consumption-by-source_annual_1949-2019_lts.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'energy', 'primary-energy-consumption-by-source_annual_1949-2019_lts.pdf'),
                outfile = here::here('figures', 'energy', 'primary-energy-consumption-by-source_annual_1949-2019_lts.pdf'))
    
    # save as png:
    # ggsave(fig_line_annual, 
    #        filename = here::here('figures', 'energy', 'primary-energy-consumption-by-source_annual_1949-2019_lts.png'), 
    #        width = 11.5, 
    #        height = 6.25, 
    #        dpi = 600)
    
  # area, annual (absolute) -------
    
    # create dataset of where to put the labels on area chart
    labs_area = dt_annual_agg[year == max(year)][order(factor(fuel2, levels = rev(levels(factor(dt_annual_agg[, fuel2])))))]
    labs_area[, cum_sum := cumsum(value)] 
    labs_area[, difference := diff(c(0,cum_sum))/2]
    labs_area[, position := cum_sum - difference]

    fig_area_annual_abs = ggplot(dt_annual_agg, 
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
           filename = here::here('figures', 'energy', 'primary-energy-consumption-by-source_annual_1949-2019_ats_absolute.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'energy', 'primary-energy-consumption-by-source_annual_1949-2019_ats_absolute.pdf'),
                outfile = here::here('figures', 'energy', 'primary-energy-consumption-by-source_annual_1949-2019_ats_absolute.pdf'))
    
    # save as png: 
    # ggsave(fig_area_annual_abs, 
    #        filename = here::here('figures', 'energy', 'primary-energy-consumption-by-source_annual_1949-2019_ats_absolute.png'), 
    #        width = 11.5, 
    #        height = 6.25, 
    #        dpi = 600)
    
  # area, annual (proportion) -------
    
    # create dataset of where to put the labels on area chart
    labs_area_prop = dt_annual_agg[year == max(year)][order(factor(fuel2, levels = rev(levels(factor(dt_annual_agg[, fuel2])))))]
    labs_area_prop[, cum_sum := cumsum(prop)] 
    labs_area_prop[, difference := diff(c(0,cum_sum))/2]
    labs_area_prop[, position := cum_sum - difference]
    
    fig_area_annual_prop = ggplot(dt_annual_agg, aes(x = year, y = prop, group = fuel2, fill = fuel2)) + 
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
           filename = here::here('figures', 'energy', 'primary-energy-consumption-by-source_annual_1949-2019_ats_proportion.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'energy', 'primary-energy-consumption-by-source_annual_1949-2019_ats_proportion.pdf'),
                outfile = here::here('figures', 'energy', 'primary-energy-consumption-by-source_annual_1949-2019_ats_proportion.pdf'))
    
  # line, annual, renewables -------
    
    # create dataset of where to put the labels on line chart
    labs_line_re = dt_annual_re[year == max(year)]
    labs_line_re = labs_line_re[order(rank(value))]
    labs_line_re[ , position := value]
    
    fig_line_annual_re = ggplot(dt_annual_re, aes(x = year, y = value, group = fuel, color = fuel)) + 
      geom_line(size = 0.9) +
      labs(title = 'Annual U.S. primary energy consumption from renewable sources (1949-2019)',
           subtitle = 'Quadrillion BTU', 
           caption = 'Data: U.S. Energy Information Administration',
           x = NULL,
           y = NULL) +
      guides(color = 'none') +
      scale_x_continuous(breaks = seq(1949,2019,5), limits = c(1949, 2019), expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) +
      scale_color_manual(values = pal_fuel) + 
      theme_line +
      geom_text(data = labs_line_re, aes(x = Inf, y = position, label = paste0(' ', fuel), color = fuel), hjust = 0,
                size = 6.5, fontface = 'plain', family = 'Secca Soft')
    
    fig_line_annual_re = ggplotGrob(fig_line_annual_re)
    fig_line_annual_re$layout$clip[fig_line_annual_re$layout$name == "panel"] = "off"
    
    ggsave(fig_line_annual_re, 
           filename = here::here('figures', 'energy', 'primary-energy-consumption-by-source_renewables_annual_1949-2019_lts.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'energy', 'primary-energy-consumption-by-source_renewables_annual_1949-2019_lts.pdf'),
                outfile = here::here('figures', 'energy', 'primary-energy-consumption-by-source_renewables_annual_1949-2019_lts.pdf'))
    
    # save as png:
    # ggsave(fig_line_annual, 
    #        filename = here::here('figures', 'energy', 'primary-energy-consumption-by-source_renewables_annual_1949-2019_lts.png'), 
    #        width = 11.5, 
    #        height = 6.25, 
    #        dpi = 600)
    
    
    
  # area, annual, renewables (absolute) -------
    
    # create dataset of where to put the labels on area chart
    labs_area_re = dt_annual_re[year == max(year)][order(factor(fuel, levels = rev(levels(factor(dt_annual_re[, fuel])))))]
    labs_area_re[, cum_sum := cumsum(value)] 
    labs_area_re[, difference := diff(c(0,cum_sum))/2]
    labs_area_re[, position := cum_sum - difference]
    
    fig_area_annual_abs_re = ggplot(dt_annual_re, 
                                 aes(x = year, y = value, group = fuel, fill = fuel)) + 
      geom_area() +
      labs(title = 'Annual U.S. primary energy consumption from renewable sources (1949-2019)',
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
      geom_text(data = labs_area_re, aes(x = Inf, y = position, label = paste0(' ', fuel), color = fuel), 
                hjust = 0, size = 6.5, fontface = 'plain', family = 'Secca Soft') 
    
    fig_area_annual_abs_re = ggplotGrob(fig_area_annual_abs_re)
    fig_area_annual_abs_re$layout$clip[fig_area_annual_abs_re$layout$name == "panel"] = "off"
    
    ggsave(fig_area_annual_abs_re, 
           filename = here::here('figures', 'energy', 'primary-energy-consumption-by-source_renewables_annual_1949-2019_ats_absolute.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'energy', 'primary-energy-consumption-by-source_renewables_annual_1949-2019_ats_absolute.pdf'),
                outfile = here::here('figures', 'energy', 'primary-energy-consumption-by-source_renewables_annual_1949-2019_ats_absolute.pdf'))
    
    # save as png: 
    # ggsave(fig_area_annual_abs, 
    #        filename = here::here('figures', 'energy', 'primary-energy-consumption-by-source_renewables_annual_1949-2019_ats_absolute.png'), 
    #        width = 11.5, 
    #        height = 6.25, 
    #        dpi = 600)
    
    
  # area, annual, renewables (proportion) -------
    
    # create dataset of where to put the labels on area chart
    labs_area_prop_re = dt_annual_re[year == max(year)][order(factor(fuel, levels = rev(levels(factor(dt_annual_re[, fuel])))))]
    labs_area_prop_re[, cum_sum := cumsum(prop)] 
    labs_area_prop_re[, difference := diff(c(0,cum_sum))/2]
    labs_area_prop_re[, position := cum_sum - difference]
    
    fig_area_annual_prop_re = ggplot(dt_annual_re, aes(x = year, y = prop, group = fuel, fill = fuel)) + 
      geom_area() +
      labs(title = 'Annual U.S. primary energy consumption from renewable sources (1949-2019)',
           subtitle = 'Share of renewable primary energy consumption', 
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
      geom_text(data = labs_area_prop_re, aes(x = Inf, y = position, label = paste0(' ', fuel), color = fuel), 
                hjust = 0, size = 6.5, fontface = 'plain', family = 'Secca Soft')  
    
    fig_area_annual_prop_re = ggplotGrob(fig_area_annual_prop_re)
    fig_area_annual_prop_re$layout$clip[fig_area_annual_prop_re$layout$name == "panel"] = "off"
    
    ggsave(fig_area_annual_prop_re, 
           filename = here::here('figures', 'energy', 'primary-energy-consumption-by-source_renewables_annual_1949-2019_ats_proportion.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'energy', 'primary-energy-consumption-by-source_renewables_annual_1949-2019_ats_proportion.pdf'),
                outfile = here::here('figures', 'energy', 'primary-energy-consumption-by-source_renewables_annual_1949-2019_ats_proportion.pdf'))
    
    
  # line, annual, totals  -------
    
    # create dataset of where to put the labels on line chart
    labs_line_tot = dt_annual_tot[year == max(year)]
    labs_line_tot = labs_line_tot[order(rank(value))]
    labs_line_tot[, position := value]

    fig_line_annual_tot = ggplot(dt_annual_tot, aes(x = year, y = value, group = fuel, color = fuel)) + 
      geom_line(size = 0.9) +
      labs(title = 'Annual U.S. primary energy consumption by source (1949-2019)',
           subtitle = 'Quadrillion BTU', 
           caption = 'Data: U.S. Energy Information Administration',
           x = NULL,
           y = NULL) +
      guides(color = 'none') +
      scale_x_continuous(breaks = seq(1949,2019,5), limits = c(1949, 2019), expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) +
      scale_color_manual(values = pal_fuel) + 
      theme_line +
      geom_text(data = labs_line_tot, aes(x = Inf, y = position, label = paste0(' ', fuel), color = fuel), hjust = 0,
                size = 6.1, fontface = 'plain', family = 'Secca Soft')
    
    fig_line_annual_tot = ggplotGrob(fig_line_annual_tot)
    fig_line_annual_tot$layout$clip[fig_line_annual_tot$layout$name == "panel"] = "off"
    
    ggsave(fig_line_annual_tot, 
           filename = here::here('figures', 'energy', 'primary-energy-consumption-by-source_totals_annual_1949-2019_lts.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'energy', 'primary-energy-consumption-by-source_totals_annual_1949-2019_lts.pdf'),
                outfile = here::here('figures', 'energy', 'primary-energy-consumption-by-source_totals_annual_1949-2019_lts.pdf'))
    
    # save as png:
    # ggsave(fig_line_annual_tot, 
    #        filename = here::here('figures', 'energy', 'primary-energy-consumption-by-source_totals_annual_1949-2019_lts.png'), 
    #        width = 11.5, 
    #        height = 6.25, 
    #        dpi = 600)
    
  # area, annual, totals (absolute) -------
    
    # create dataset of where to put the labels on area chart
    labs_area_tot = dt_annual_tot[year == max(year) & ! fuel == 'Total Primary Energy'][order(factor(fuel, levels = rev(levels(factor(dt_annual_tot[, fuel])))))]
    labs_area_tot[, cum_sum := cumsum(value)] 
    labs_area_tot[, difference := diff(c(0,cum_sum))/2]
    labs_area_tot[, position := cum_sum - difference]
    
    fig_area_annual_abs_tot = ggplot(dt_annual_tot[! fuel == 'Total Primary Energy'], aes(x = year, y = value, group = fuel, fill = fuel)) + 
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
      geom_text(data = labs_area_tot, aes(x = Inf, y = position, label = paste0(' ', fuel), color = fuel), 
                hjust = 0, size = 6.5, fontface = 'plain', family = 'Secca Soft') 
    
    fig_area_annual_abs_tot = ggplotGrob(fig_area_annual_abs_tot)
    fig_area_annual_abs_tot$layout$clip[fig_area_annual_abs_tot$layout$name == "panel"] = "off"
    
    ggsave(fig_area_annual_abs_tot, 
           filename = here::here('figures', 'energy', 'primary-energy-consumption-by-source_totals_annual_1949-2019_ats_absolute.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'energy', 'primary-energy-consumption-by-source_totals_annual_1949-2019_ats_absolute.pdf'),
                outfile = here::here('figures', 'energy', 'primary-energy-consumption-by-source_totals_annual_1949-2019_ats_absolute.pdf'))
    
    # save as png: 
    # ggsave(fig_area_annual_abs, 
    #        filename = here::here('figures', 'energy', 'primary-energy-consumption-by-source_annual_1949-2019_ats_absolute.png'), 
    #        width = 11.5, 
    #        height = 6.25, 
    #        dpi = 600)
    
  # area, annual, totals (proportion) -------
    
    # create dataset of where to put the labels on area chart
    labs_area_prop_tot = dt_annual_tot[year == max(year) & ! fuel == 'Total Primary Energy'][order(factor(fuel, levels = rev(levels(factor(dt_annual_tot[, fuel])))))]
    labs_area_prop_tot[, cum_sum := cumsum(prop)] 
    labs_area_prop_tot[, difference := diff(c(0,cum_sum))/2]
    labs_area_prop_tot[, position := cum_sum - difference]
    
    fig_area_annual_prop_tot = ggplot(dt_annual_tot[! fuel == 'Total Primary Energy'], aes(x = year, y = prop, group = fuel, fill = fuel)) + 
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
      geom_text(data = labs_area_prop_tot, aes(x = Inf, y = position, label = paste0(' ', fuel), color = fuel), 
                hjust = 0, size = 6.5, fontface = 'plain', family = 'Secca Soft')  
    
    fig_area_annual_prop_tot = ggplotGrob(fig_area_annual_prop_tot)
    fig_area_annual_prop_tot$layout$clip[fig_area_annual_prop_tot$layout$name == "panel"] = "off"
    
    ggsave(fig_area_annual_prop_tot, 
           filename = here::here('figures', 'energy', 'primary-energy-consumption-by-source_totals_annual_1949-2019_ats_proportion.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'energy', 'primary-energy-consumption-by-source_totals_annual_1949-2019_ats_proportion.pdf'),
                outfile = here::here('figures', 'energy', 'primary-energy-consumption-by-source_totals_annual_1949-2019_ats_proportion.pdf'))
    
    # save as png: 
    # ggsave(fig_area_annual_prop_tot, 
    #        filename = here::here('figures', 'energy', 'primary-energy-consumption-by-source_totals_annual_1949-2019_ats_proportion.png'), 
    #        width = 11.5, 
    #        height = 6.25, 
    #        dpi = 600)
  
  # line, monthly ------
  
    fig_line_month = ggplot(dt_month_agg, aes(x = month, y = value, group = fuel2, color = fuel2)) + 
      geom_line(size = 0.5) +
      labs(title = 'Monthly U.S. primary energy consumption by source (Jan 1973-May 2020)',
           subtitle = 'Quadrillion BTU',
           caption = 'Data: U.S. Energy Information Administration',
           x = NULL,
           y = NULL) +
      guides(color = 'none') +
      scale_x_date(breaks = seq(as.Date('1975-01-01'), as.Date('2020-01-01'), '5 years'), date_labels = "%b %Y", expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0,4,0.5), expand = c(0,0)) +
      scale_color_manual(values = pal_fuel) + 
      geom_dl(aes(label = fuel2), method = list(dl.trans(x = x + .3), 'last.bumpup',  cex = 1.2, fontfamily = 'Secca Soft', fontface = 'plain')) +
      theme_line +
      theme(plot.margin = unit(c(1,8,1,1), "lines"))
    
    fig_line_month = ggplotGrob(fig_line_month)
    fig_line_month$layout$clip[fig_line_month$layout$name == "panel"] = "off"
    
    ggsave(fig_line_month, 
           filename = here::here('figures', 'energy', 'primary-energy-consumption-by-source_month_Jan1973-May2020_lts.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'energy', 'primary-energy-consumption-by-source_month_Jan1973-May2020_lts.pdf'),
                outfile = here::here('figures', 'energy', 'primary-energy-consumption-by-source_month_Jan1973-May2020_lts.pdf'))
    
    # save as png:
    # ggsave(fig_line_month, 
    #        filename = here::here('figures', 'energy', 'primary-energy-consumption-by-source_month_Jan1973-May2020_lts.png'), 
    #        width = 11.5, 
    #        height = 6.25, 
    #        dpi = 600)
    
  # line, renewables, monthly -------
    
    fig_line_month_re = ggplot(dt_month_re, aes(x = month, y = value, group = fuel, color = fuel)) + 
      geom_line(size = 0.5) +
      labs(title = 'Monthly U.S. primary energy consumption from renewable sources (Jan 1973-May 2020)',
           subtitle = 'Quadrillion BTU', 
           caption = 'Data: U.S. Energy Information Administration',
           x = NULL,
           y = NULL) +
      guides(color = 'none') +
      scale_x_date(breaks = seq(as.Date('1975-01-01'), as.Date('2020-01-01'), '5 years'), date_labels = "%b %Y", expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0,0.35,0.05), expand = c(0,0)) +
      scale_color_manual(values = pal_fuel) + 
      geom_dl(aes(label = fuel), method = list(dl.trans(x = x + .3), 'last.bumpup',  cex = 1.2, fontfamily = 'Secca Soft', fontface = 'plain')) +
      theme_line +
      theme(plot.margin = unit(c(1,8,1,1), "lines"))
    
    fig_line_month_re = ggplotGrob(fig_line_month_re)
    fig_line_month_re$layout$clip[fig_line_month_re$layout$name == "panel"] = "off"
    
    ggsave(fig_line_month_re, 
           filename = here::here('figures', 'energy', 'primary-energy-consumption-by-source_renewables_month_Jan1973-May2020_lts.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'energy', 'primary-energy-consumption-by-source_renewables_month_Jan1973-May2020_lts.pdf'),
                outfile = here::here('figures', 'energy', 'primary-energy-consumption-by-source_renewables_month_Jan1973-May2020_lts.pdf'))
    
    # save as png:
    # ggsave(fig_line_month_re, 
    #        filename = here::here('figures', 'energy', 'primary-energy-consumption-by-source_renewables_month_Jan1973-May2020_lts.png'), 
    #        width = 11.5, 
    #        height = 6.25, 
    #        dpi = 600)
