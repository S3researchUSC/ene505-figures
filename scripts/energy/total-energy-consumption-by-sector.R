#  ---------------------------------------------------- INPUT DATA ----------------------------------------------------

data.file     = 'Table_2.1_Energy_Consumption_by_Sector.xlsx' 

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

  dt_month = as.data.table(read.xlsx(here::here('data', data.file), sheet = 'Monthly Data', startRow = 11, cols = c(1,3,5,7,9), detectDates = T))
  dt_month = dt_month[2:nrow(dt_month)]
  
  dt_annual = as.data.table(read.xlsx(here::here('data', data.file), sheet = 'Annual Data', startRow = 11, cols = c(1,3,5,7,9), detectDates = T))
  dt_annual = dt_annual[2:nrow(dt_annual)]

# rename columns ----
  
  colnames(dt_month) = c('month', 'Residential', 'Commercial', 'Industrial', 'Transportation')
  colnames(dt_annual) = c('year', 'Residential', 'Commercial', 'Industrial', 'Transportation')
  
# melt data table from wide to long format -----
  
  dt_month = melt(dt_month, measure.vars = colnames(dt_month)[2:5],
                  variable.name = 'sector', value.name = 'value')
  
  dt_annual = melt(dt_annual, measure.vars = colnames(dt_annual)[2:5],
                   variable.name = 'sector', value.name = 'value')

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
  
# remove totals -----
  
  dt_month = dt_month[! sector == 'Total']
  dt_annual = dt_annual[! sector == 'Total']
  
# calculate proportion (or percentage) contributed by each fuel type, for each month, for each year -----
  
  dt_month[, prop := value/sum(value), by = c("year", "month_name")]
  dt_annual[, prop := value/sum(value), by = c("year")]
  
#  ------------------------------------------------------- FIGURES -------------------------------------------------------
  
  # bar, 2019 ---------
    
    fig_bar_2019 = ggplot(dt_annual[year == max(year)], aes(x = reorder(sector, value), y = value/1000, group = sector, fill = sector)) + 
      geom_bar(stat = "identity") +
      labs(title = 'Annual U.S. total energy consumption by sector (2019)',
           subtitle = NULL,
           caption = 'Data: U.S. Energy Information Administration', 
           x = NULL,
           y = 'Quadrillion BTU', 
           fill = NULL) +
      # scale_x_continuous() +
      scale_y_continuous(labels = scales::comma, expand = c(0,0), breaks = seq(0,35,10), limits = c(0,35)) +
      scale_color_manual(values = pal_sector) +
      scale_fill_manual(values = pal_sector) + 
      guides(fill = 'none',
             color ='none') +
      theme_bar_flipped + 
      geom_text(data = dt_annual[year == max(year)], aes(x = sector, y = value/1000 + 0.25, label = paste0(signif(prop*100,2), '%'), color = sector), hjust = 0,
                size = 6, fontface = 'bold', family = 'Secca Soft') +
      coord_flip()
    
    ggsave(fig_bar_2019, 
           filename = here::here('figures', 'energy', 'total-energy-consumption-by-sector_2019_bar.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'energy', 'total-energy-consumption-by-sector_2019_bar.pdf'),
                outfile = here::here('figures', 'energy', 'total-energy-consumption-by-sector_2019_bar.pdf'))
    
    # save as png: 
    # ggsave(fig_bar_2019,
    #        filename = here::here('figures', 'energy', 'total-energy-consumption-by-sector_2019_bar.png'),
    #        width = 11.5,
    #        height = 6.25,
    #        dpi = 600)
  
  # line, annual -------
  
    fig_line_annual = ggplot(dt_annual, aes(x = year, y = value/1000, group = sector, color = sector)) + 
      geom_line(size = 0.9) +
      labs(title = 'Annual U.S. total energy consumption by sector (1949-2019)',
           subtitle = 'Quadrillion BTU', 
           caption = 'Data: U.S. Energy Information Administration',
           x = NULL,
           y = NULL) +
      guides(color = 'none') +
      scale_x_continuous(breaks = seq(1949,2019,5), limits = c(1949, 2019), expand = c(0,0)) +
      scale_y_continuous(labels = scales::comma, expand = c(0,0)) +
      scale_color_manual(values = pal_sector) + 
      theme_line +
      geom_dl(aes(label = sector), method = list(dl.trans(x = x + .3), 'last.bumpup', cex = 1.5, fontfamily = 'Secca Soft', fontface = 'plain'))  +
      theme(plot.margin = unit(c(1,9,1,1), "lines"))
      
    fig_line_annual = ggplotGrob(fig_line_annual)
    fig_line_annual$layout$clip[fig_line_annual$layout$name == "panel"] = "off"
    
    ggsave(fig_line_annual, 
           filename = here::here('figures', 'energy', 'total-energy-consumption-by-sector_annual_1949-2019_lts.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'energy', 'total-energy-consumption-by-sector_annual_1949-2019_lts.pdf'),
                outfile = here::here('figures', 'energy', 'total-energy-consumption-by-sector_annual_1949-2019_lts.pdf'))
    
    # save as png:
    # ggsave(fig_line_annual, 
    #        filename = here::here('figures', 'energy', 'total-energy-consumption-by-sector_annual_1949-2019_lts.png'), 
    #        width = 11.5, 
    #        height = 6.25, 
    #        dpi = 600)
  
  # area, annual (absolute) -------
  
    # create dataset of where to put the labels on area chart
    labs_area = dt_annual[year == max(year)][order(factor(sector, levels = rev(c('Residential', 'Commercial', 'Industrial', 'Transportation'))))]
    labs_area[, cum_sum := cumsum(value/1000)] 
    labs_area[, difference := diff(c(0,cum_sum))/2]
    labs_area[, position := cum_sum - difference]
    
    fig_area_annual_abs = ggplot(dt_annual, aes(x = year, y = value/1000, group = sector, fill = sector)) + 
      geom_area() +
      labs(title = 'Annual U.S. total energy consumption by sector (1949-2019)',
           subtitle = 'Quadrillion BTU', 
           caption = 'Data: U.S. Energy Information Administration',
           x = NULL,
           y = NULL,
           fill = NULL) +
      scale_x_continuous(breaks = seq(1949,2019,5), limits = c(1949, 2019), expand = c(0,0)) +
      scale_y_continuous(labels = scales::comma, expand = c(0,0)) +
      scale_fill_manual(values = pal_sector) + 
      scale_color_manual(values = pal_sector) + 
      guides(fill = 'none',
             color = 'none') +
      theme_area_labeled +
      geom_text(data = labs_area, aes(x = Inf, y = position, label = paste0(' ', sector), color = sector), 
                hjust = 0, size = 6.5, fontface = 'plain', family = 'Secca Soft')  +
      theme(plot.margin = unit(c(1,9,1,1), "lines"))
    
    fig_area_annual_abs = ggplotGrob(fig_area_annual_abs)
    fig_area_annual_abs$layout$clip[fig_area_annual_abs$layout$name == "panel"] = "off"
    
    ggsave(fig_area_annual_abs, 
           filename = here::here('figures', 'energy', 'total-energy-consumption-by-sector_annual_1949-2019_ats_absolute.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'energy', 'total-energy-consumption-by-sector_annual_1949-2019_ats_absolute.pdf'),
                outfile = here::here('figures', 'energy', 'total-energy-consumption-by-sector_annual_1949-2019_ats_absolute.pdf'))
    
    # save as png: 
    # ggsave(fig_area_annual_abs, 
    #        filename = here::here('figures', 'energy', 'total-energy-consumption-by-sector_annual_1949-2019_ats_absolute.png'), 
    #        width = 11.5, 
    #        height = 6.25, 
    #        dpi = 600)
  
  # area, annual (proportion) -------
  
    # create dataset of where to put the labels on area chart
    labs_area_prop = dt_annual[year == max(year)][order(factor(sector, levels = rev(c('Residential', 'Commercial', 'Industrial', 'Transportation'))))]
    labs_area_prop[, cum_sum := cumsum(prop)] 
    labs_area_prop[, difference := diff(c(0,cum_sum))/2]
    labs_area_prop[, position := cum_sum - difference]
    
    fig_area_annual_prop = ggplot(dt_annual, aes(x = year, y = prop, group = sector, fill = sector)) + 
      geom_area() +
      labs(title = 'Annual U.S. total energy consumption by sector (1949-2019)',
           subtitle = 'Share of total energy consumption',
           caption = 'Data: U.S. Energy Information Administration',
           x = NULL,
           y = NULL,
           fill = NULL) +
      scale_x_continuous(breaks = seq(1949,2019,5), limits = c(1949, 2019), expand = c(0,0)) +
      scale_y_continuous(labels = scales::percent, expand = c(0,0)) +
      scale_color_manual(values = pal_sector) +
      scale_fill_manual(values = pal_sector) + 
      guides(fill = 'none',
             color = 'none') +
      theme_area_labeled + 
      geom_text(data = labs_area_prop, aes(x = Inf, y = position, label = paste0(' ', sector), color = sector), 
                hjust = 0, size = 6.5, fontface = 'plain', family = 'Secca Soft') +
      theme(plot.margin = unit(c(1,9,1,1), "lines"))
    
    fig_area_annual_prop = ggplotGrob(fig_area_annual_prop)
    fig_area_annual_prop$layout$clip[fig_area_annual_prop$layout$name == "panel"] = "off"
    
    ggsave(fig_area_annual_prop, 
           filename = here::here('figures', 'energy', 'total-energy-consumption-by-sector_annual_1949-2019_ats_proportion.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'energy', 'total-energy-consumption-by-sector_annual_1949-2019_ats_proportion.pdf'),
                outfile = here::here('figures', 'energy', 'total-energy-consumption-by-sector_annual_1949-2019_ats_proportion.pdf'))
    
    # save as png: 
    # ggsave(fig_area_annual_prop,
    #        filename = here::here('figures', 'energy', 'total-energy-consumption-by-sector_annual_1949-2019_ats_proportion.png'),
    #        width = 11.5,
    #        height = 6.25,
    #        dpi = 600)
  
  # line, monthly -------
  
    # create dataset of where to put the labels on line chart
    labs_line = dt_month[month == max(month)]
    labs_line = labs_line[order(rank(value))]
    labs_line[, position := value/1000]

    fig_line_month = ggplot(dt_month, aes(x = month, y = value/1000, group = sector, color = sector)) + 
      geom_line(size = 0.6) +
      labs(title = 'Monthly U.S. total energy consumption by sector (1949-2019)',
           subtitle = 'Quadrillion BTU', 
           caption = 'Data: U.S. Energy Information Administration',
           x = NULL,
           y = NULL) +
      guides(color = 'none') +
      scale_x_date(breaks = '5 years', date_labels = "%b %Y", expand = c(0,0)) +
      scale_y_continuous(labels = scales::comma, expand = c(0,0)) +
      scale_color_manual(values = pal_sector) + 
      theme_line +
      geom_text(data = labs_line, aes(x = ymd('2020-04-02'), y = position, label = paste0(' ', sector), color = sector), hjust = 0,
                size = 6.5, fontface = 'plain', family = 'Secca Soft') +
      theme(plot.margin = unit(c(1,9,1,1), "lines"))
    
    fig_line_month = ggplotGrob(fig_line_month)
    fig_line_month$layout$clip[fig_line_month$layout$name == "panel"] = "off"
    
    ggsave(fig_line_month, 
           filename = here::here('figures', 'energy', 'total-energy-consumption-by-sector_month_1949-2019_lts.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'energy', 'total-energy-consumption-by-sector_month_1949-2019_lts.pdf'),
                outfile = here::here('figures', 'energy', 'total-energy-consumption-by-sector_month_1949-2019_lts.pdf'))
    
    # save as png:
    # ggsave(fig_line_month, 
    #        filename = here::here('figures', 'energy', 'total-energy-consumption-by-sector_annual_1949-2019_lts.png'), 
    #        width = 11.5, 
    #        height = 6.25, 
    #        dpi = 600)
  