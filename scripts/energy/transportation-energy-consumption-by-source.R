#  ---------------------------------------------------- INPUT DATA ----------------------------------------------------

data.file     = 'Table_2.5_Transportation_Sector_Energy_Consumption.xlsx' 

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
  
  colnames(dt_annual) = c('year', 'Coal', 'Natural Gas', 'Petroleum', 'Total Fossil Fuels', 'Biomass', 'Total Primary Energy',
                          'Electricity Retail Sales', 'Electrical System Losses', 'Total Energy')
  
  colnames(dt_month) = c('month', 'Coal', 'Natural Gas', 'Petroleum', 'Total Fossil Fuels', 'Biomass', 'Total Primary Energy',
                         'Electricity Retail Sales', 'Electrical System Losses', 'Total Energy')
  
# melt data table from wide to long format -----
  
  dt_month = melt(dt_month, measure.vars = colnames(dt_month)[2:10],
                  variable.name = 'fuel', value.name = 'value')
  
  dt_annual = melt(dt_annual, measure.vars = colnames(dt_annual)[2:10],
                   variable.name = 'fuel', value.name = 'value')
  
# create year and month columns ------
  
  dt_month[, year := year(month)]

# convert value column to numeric ----
  
  dt_month[, value := as.numeric(value)]
  dt_annual[, value := as.numeric(value)]
  
# remove NA month or year entries ----
  
  dt_month = dt_month[!is.na(month)]
  dt_annual = dt_annual[!is.na(year)]
  
# get totals ------
  
  dt_annual_tot = dt_annual[ fuel %like% 'Total' | fuel %like% 'Electric' ]
  dt_annual = dt_annual[ ! (fuel %like% 'Total' | fuel %like% 'Electric') ]
  
  dt_month_tot = dt_month[ fuel %like% 'Total' | fuel %like% 'Electric' ]
  dt_month = dt_month[ ! (fuel %like% 'Total' | fuel %like% 'Electric') ]
  

# calculate proportions ------
  
  dt_annual[, prop := value/sum(value, na.rm = T), by = c('year')]
  dt_annual_tot[! fuel == 'Total Primary Energy' , prop := value/sum(value, na.rm = T), by = c('year')]

#  ------------------------------------------------------- FIGURES -------------------------------------------------------
  
  # bar, 2019 ---------
  
    fig_bar_2019 = ggplot(dt_annual[year == max(year) & ! fuel == 'Coal'], 
                          aes(x = reorder(fuel, value), y = value/1000, group = fuel, fill = fuel)) + 
      geom_bar(stat = "identity") +
      labs(title = 'Annual U.S. transportation energy consumption by source (2019)',
           subtitle = NULL, 
           caption = 'Data: U.S. Energy Information Administration',
           x = NULL,
           y = 'Quadrillion BTU', 
           fill = NULL) +
      scale_y_continuous(expand = c(0,0), breaks = seq(0,30,5), limits = c(0,29)) +
      scale_color_manual(values = pal_fuel) +
      scale_fill_manual(values = pal_fuel) + 
      guides(fill = 'none',
             color ='none') +
      theme_bar_flipped + 
      geom_text(data = dt_annual[year == max(year) & ! fuel == 'Coal'], aes(x = fuel, y = value/1000 + 0.5, label = paste0(signif(prop*100,2), '%'), color = fuel), hjust = 0,
                size = 6, fontface = 'bold', family = 'Secca Soft') +
      coord_flip()
    
    ggsave(fig_bar_2019, 
           filename = here::here('figures', 'energy', 'transportation-energy-consumption-by-source_2019_bar.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'energy', 'transportation-energy-consumption-by-source_2019_bar.pdf'),
                outfile = here::here('figures', 'energy', 'transportation-energy-consumption-by-source_2019_bar.pdf'))
    
  # line, annual -------
    
    # create dataset of where to put the labels on line chart
    labs_line = dt_annual[year == max(year)]
    labs_line = labs_line[order(rank(value))]
    labs_line[, position := value/1000]
    labs_line[2:3, position := c(0.98,2)]
    # labs_line[is.na(position), position := value]
    
    fig_line_annual = ggplot(dt_annual, aes(x = year, y = value/1000, group = fuel, color = fuel)) + 
      geom_line(size = 0.9) +
      labs(title = 'Annual U.S. transportation energy consumption by source (1949-2019)',
           subtitle = 'Quadrillion BTU', 
           caption = 'Data: U.S. Energy Information Administration',
           x = NULL,
           y = NULL) +
      guides(color = 'none') +
      scale_x_continuous(breaks = seq(1949,2019,5), limits = c(1949, 2019), expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) +
      scale_color_manual(values = pal_fuel) + 
      theme_line +
      geom_text(data = labs_line, aes(x = Inf, y = position, label = paste0(' ', fuel), color = fuel), hjust = 0,
                size = 6, fontface = 'plain', family = 'Secca Soft')  +
      theme(plot.margin = unit(c(1,7,1,1), "lines"))
    
    fig_line_annual = ggplotGrob(fig_line_annual)
    fig_line_annual$layout$clip[fig_line_annual$layout$name == "panel"] = "off"
    
    ggsave(fig_line_annual, 
           filename = here::here('figures', 'energy', 'transportation-energy-consumption-by-source_annual_1949-2019_lts.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'energy', 'transportation-energy-consumption-by-source_annual_1949-2019_lts.pdf'),
                outfile = here::here('figures', 'energy', 'transportation-energy-consumption-by-source_annual_1949-2019_lts.pdf'))
    
    # save as png:
    # ggsave(fig_line_annual, 
    #        filename = here::here('figures', 'energy', 'transportation-energy-consumption-by-source_annual_1949-2019_lts.png'), 
    #        width = 11.5, 
    #        height = 6.25, 
    #        dpi = 600)
    
  # area, annual (absolute) -------
    
    # create dataset of where to put the labels on area chart
    labs_area = dt_annual[year == max(year)][order(factor(fuel, levels = rev(levels(factor(dt_annual[, fuel])))))]
    labs_area[, cum_sum := cumsum(value/1000)] 
    labs_area[, difference := diff(c(0,cum_sum))/2]
    labs_area[, position := cum_sum - difference]
    
    fig_area_annual_abs = ggplot(dt_annual, aes(x = year, y = value/1000, group = fuel, fill = fuel)) + 
      geom_area() +
      labs(title = 'Annual U.S. transportation energy consumption by source (1949-2019)',
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
      geom_text(data = labs_area[! fuel == 'Coal'], aes(x = Inf, y = position, label = paste0(' ', fuel), color = fuel), 
                hjust = 0, size = 6.5, fontface = 'plain', family = 'Secca Soft')   +
      geom_text(data = labs_area[ fuel == 'Coal'], aes(x = -Inf, y = 10, label = paste0(' ', fuel), color = fuel),
                hjust = 0, size = 6.5, fontface = 'plain', family = 'Secca Soft')  +
      theme(plot.margin = unit(c(1,7,1,1), "lines"))
    
    fig_area_annual_abs = ggplotGrob(fig_area_annual_abs)
    fig_area_annual_abs$layout$clip[fig_area_annual_abs$layout$name == "panel"] = "off"
    
    ggsave(fig_area_annual_abs, 
           filename = here::here('figures', 'energy', 'transportation-energy-consumption-by-source_annual_1949-2019_ats_absolute.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'energy', 'transportation-energy-consumption-by-source_annual_1949-2019_ats_absolute.pdf'),
                outfile = here::here('figures', 'energy', 'transportation-energy-consumption-by-source_annual_1949-2019_ats_absolute.pdf'))
    
    # save as png: 
    # ggsave(fig_area_annual_abs, 
    #        filename = here::here('figures', 'energy', 'transportation-energy-consumption-by-source_annual_1949-2019_ats_absolute.png'), 
    #        width = 11.5, 
    #        height = 6.25, 
    #        dpi = 600)
    
  # area, annual (proportion) -------
    
    # create dataset of where to put the labels on area chart
    labs_area_prop = dt_annual[year == max(year)][order(factor(fuel, levels = rev(levels(factor(dt_annual[, fuel])))))]
    labs_area_prop[, cum_sum := cumsum(prop)] 
    labs_area_prop[, difference := diff(c(0,cum_sum))/2]
    labs_area_prop[, position := cum_sum - difference]
    
    fig_area_annual_prop = ggplot(dt_annual, aes(x = year, y = prop, group = fuel, fill = fuel)) + 
      geom_area() +
      labs(title = 'Annual U.S. transportation energy consumption by source (1949-2019)',
           subtitle = 'Share of transportation energy consumption', 
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
      geom_text(data = labs_area_prop[! fuel == 'Coal'], aes(x = Inf, y = position, label = paste0(' ', fuel), color = fuel), 
                hjust = 0, size = 6.5, fontface = 'plain', family = 'Secca Soft')   +
      geom_text(data = labs_area_prop[ fuel == 'Coal'], aes(x = -Inf, y = 1.03, label = paste0(' ', fuel), color = fuel),
                hjust = 0, size = 6.5, fontface = 'plain', family = 'Secca Soft')  +
      theme(plot.margin = unit(c(1,7,1,1), "lines"))
    
    fig_area_annual_prop = ggplotGrob(fig_area_annual_prop)
    fig_area_annual_prop$layout$clip[fig_area_annual_prop$layout$name == "panel"] = "off"
    
    ggsave(fig_area_annual_prop, 
           filename = here::here('figures', 'energy', 'transportation-energy-consumption-by-source_annual_1949-2019_ats_proportion.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'energy', 'transportation-energy-consumption-by-source_annual_1949-2019_ats_proportion.pdf'),
                outfile = here::here('figures', 'energy', 'transportation-energy-consumption-by-source_annual_1949-2019_ats_proportion.pdf'))
  
    
  # line, monthly -------
    
    fig_line_month = ggplot(dt_month, aes(x = month, y = value/1000, group = fuel, color = fuel)) + 
      geom_line(size = 0.5) +
      labs(title = 'Monthly U.S. transportation energy consumption by source (Jan 1973-Apr 2020)',
           subtitle = 'Quadrillion BTU', 
           caption = 'Data: U.S. Energy Information Administration',
           x = NULL,
           y = NULL) +
      guides(color = 'none') +
      scale_x_date(breaks = seq(as.Date('1975-01-01'), as.Date('2020-01-01'), '5 years'), date_labels = "%b %Y", expand = c(0,0)) +
      scale_y_continuous(labels = scales::comma, expand = c(0,0)) +
      scale_color_manual(values = pal_fuel) + 
      geom_dl(aes(label = fuel), method = list(dl.trans(x = x + .3), 'last.bumpup',  cex = 1.2, fontfamily = 'Secca Soft', fontface = 'plain')) +
      theme_line +
      theme(plot.margin = unit(c(1,7,1,1), "lines"))
    
    fig_line_month = ggplotGrob(fig_line_month)
    fig_line_month$layout$clip[fig_line_month$layout$name == "panel"] = "off"
    
    ggsave(fig_line_month, 
           filename = here::here('figures', 'energy', 'transportation-energy-consumption-by-source_month_Jan1973-Apr2020_lts.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'energy', 'transportation-energy-consumption-by-source_month_Jan1973-Apr2020_lts.pdf'),
                outfile = here::here('figures', 'energy', 'transportation-energy-consumption-by-source_month_Jan1973-Apr2020_lts.pdf'))
    
    # save as png:
    # ggsave(fig_line_month, 
    #        filename = here::here('figures', 'energy', 'transportation-energy-consumption-by-source_month_Jan1973-Apr2020_lts.png'), 
    #        width = 11.5, 
    #        height = 6.25, 
    #        dpi = 600)
    