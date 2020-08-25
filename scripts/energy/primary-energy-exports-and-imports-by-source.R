#  ---------------------------------------------------- INPUT DATA ----------------------------------------------------

exp.file     = 'Table_1.4b_Primary_Energy_Exports_by_Source.xlsx' 
imp.file      = 'Table_1.4a_Primary_Energy_Imports_by_Source.xlsx'

#  --------------------------------------------------- MAIN SCRIPT ---------------------------------------------------

# load libraries -------

  library(data.table)
  library(openxlsx)
  library(stringr)
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
  
  exp_month = as.data.table(read.xlsx(here::here('data', exp.file), sheet = 'Monthly Data', startRow = 11, detectDates = T))
  exp_month = exp_month[2:nrow(exp_month)]
  
  exp_annual = as.data.table(read.xlsx(here::here('data', exp.file), sheet = 'Annual Data', startRow = 11, detectDates = T))
  exp_annual = exp_annual[2:nrow(exp_annual)]
  
  imp_month = as.data.table(read.xlsx(here::here('data', imp.file), sheet = 'Monthly Data', startRow = 11, detectDates = T))
  imp_month = imp_month[2:nrow(imp_month)]
  
  imp_annual = as.data.table(read.xlsx(here::here('data', imp.file), sheet = 'Annual Data', startRow = 11, detectDates = T))
  imp_annual = imp_annual[2:nrow(imp_annual)]
                
  
# rename columns ----
  
  colnames(exp_annual) = c('year', 'Coal', 'Coal Coke', 'Natural Gas', 'Crude Oil', 'Petroleum Products', 'Total Petroleum', 
                          'Biomass', 'Electricity', 'Total Energy')
  
  colnames(exp_month) = c('month',  'Coal', 'Coal Coke', 'Natural Gas', 'Crude Oil', 'Petroleum Products', 'Total Petroleum', 
                         'Biomass', 'Electricity', 'Total Energy')
  
  colnames(imp_annual) = c('year', 'Coal', 'Coal Coke', 'Natural Gas', 'Crude Oil', 'Petroleum Products', 'Total Petroleum', 
                           'Biomass', 'Electricity', 'Total Energy')
  
  colnames(imp_month) = c('month',  'Coal', 'Coal Coke', 'Natural Gas', 'Crude Oil', 'Petroleum Products', 'Total Petroleum', 
                          'Biomass', 'Electricity', 'Total Energy')
  
# melt data table from wide to long format -----
  
  exp_month = melt(exp_month, measure.vars = colnames(exp_month)[2:10],
                  variable.name = 'fuel', value.name = 'export')
  
  exp_annual = melt(exp_annual, measure.vars = colnames(exp_annual)[2:10],
                   variable.name = 'fuel', value.name = 'export')
  
  imp_month = melt(imp_month, measure.vars = colnames(imp_month)[2:10],
                   variable.name = 'fuel', value.name = 'import')
  
  imp_annual = melt(imp_annual, measure.vars = colnames(imp_annual)[2:10],
                    variable.name = 'fuel', value.name = 'import')
  
# combine exports and imports -----
  
  dt_annual = merge(exp_annual, imp_annual, on = c('year', 'fuel'), all.x = T)
  dt_month = merge(exp_month, imp_month, on = c('month', 'fuel'), all.x = T)
  
# create year and month columns ------
  
  dt_month[, year := year(month)]
  dt_month[, month_val := month(month)]
  dt_month[, month_name := month(month, label = T)]
  
# convert value column to numeric ----
  
  dt_month[ , (c('export', 'import')):= lapply(.SD, as.numeric), .SDcols = c('export', 'import')]
  dt_annual[ , (c('export', 'import')):= lapply(.SD, as.numeric), .SDcols = c('export', 'import')]

# remove NA month or year entries ----
  
  dt_month = dt_month[!is.na(month)]
  dt_annual = dt_annual[!is.na(year)]
  
# get totals ------
  
  dt_annual_tot = dt_annual[ fuel %like% 'Total']
  dt_annual = dt_annual[ ! fuel %like% 'Total']
  
  dt_month_tot = dt_month[ fuel %like% 'Total']
  dt_month = dt_month[ ! fuel %like% 'Total']

# longer data table ----
  
  dt_annual_long = melt(dt_annual[,1:4], measure.vars = colnames(dt_annual)[3:4],
                        variable.name = 'type', value.name = 'value')
  
  dt_annual_tot_long = melt(dt_annual_tot, measure.vars = colnames(dt_annual_tot)[3:4],
                        variable.name = 'type', value.name = 'value')
  
  dt_month_tot_long = melt(dt_month_tot, measure.vars = colnames(dt_month_tot)[3:4],
                            variable.name = 'type', value.name = 'value')
  
  
# calculate proportions -----
  
  dt_annual[, prop_exp := export/sum(export, na.rm = T), by = c('year')]
  dt_annual[, prop_imp := import/sum(import, na.rm = T), by = c('year')]
  
  dt_annual_long[, prop := value/sum(value, na.rm = T), by = c('year', 'type')]
  
#  ------------------------------------------------------- FIGURES -------------------------------------------------------
  
  # exports, bar, 2019 ---------
  
    fig_exp_bar_2019 = ggplot(dt_annual[year == max(year)], 
                          aes(x = reorder(fuel, export), y = export, group = fuel, fill = fuel)) + 
      geom_bar(stat = "identity") +
      labs(title = 'Annual U.S. primary energy exports by source (2019)',
           subtitle = NULL, 
           caption = 'Data: U.S. Energy Information Administration',
           x = NULL,
           y = 'Quadrillion BTU', 
           fill = NULL) +
      scale_y_continuous(labels = scales::comma, expand = c(0,0), breaks = seq(0,10,2), limits = c(0,11)) +
      scale_color_manual(values = pal_fuel) +
      scale_fill_manual(values = pal_fuel) + 
      guides(fill = 'none',
             color ='none') +
      theme_bar_flipped + 
      geom_text(data = dt_annual[year == max(year)], aes(x = fuel, y = export + 0.1, label = paste0(signif(prop_exp*100,2), '%'), color = fuel), hjust = 0,
                size = 6, fontface = 'bold', family = 'Secca Soft') +
      coord_flip()
    
    ggsave(fig_exp_bar_2019, 
           filename = here::here('figures', 'energy', 'primary-energy-exports-by-source_2019_bar.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'energy', 'primary-energy-exports-by-source_2019_bar.pdf'),
                outfile = here::here('figures', 'energy', 'primary-energy-exports-by-source_2019_bar.pdf'))
  
  # exports, line, annual -------
    
    # create dataset of where to put the labels on line chart
    labs_line = dt_annual[year == max(year)]
    labs_line = labs_line[order(rank(export))]
    labs_line[, position := export]
    labs_line[1:3, position := c(0,0.4,0.8)]
    
    fig_exp_line_annual = ggplot(dt_annual, aes(x = year, y = export, group = fuel, color = fuel)) + 
      geom_line(size = 0.9) +
      labs(title = 'Annual U.S. primary energy exports by source (1949-2019)',
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
                size = 6.5, fontface = 'plain', family = 'Secca Soft')
    
    fig_exp_line_annual = ggplotGrob(fig_exp_line_annual)
    fig_exp_line_annual$layout$clip[fig_exp_line_annual$layout$name == "panel"] = "off"
    
    ggsave(fig_exp_line_annual, 
           filename = here::here('figures', 'energy', 'primary-energy-exports-by-source_annual_1949-2019_lts.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'energy', 'primary-energy-exports-by-source_annual_1949-2019_lts.pdf'),
                outfile = here::here('figures', 'energy', 'primary-energy-exports-by-source_annual_1949-2019_lts.pdf'))
    
    # save as png:
    # ggsave(fig_exp_line_annual, 
    #        filename = here::here('figures', 'energy', 'primary-energy-exports-by-source_annual_1949-2019_lts.png'), 
    #        width = 11.5, 
    #        height = 6.25, 
    #        dpi = 600)
    
  # exports, area, annual (absolute) -------
    
    # create dataset of where to put the labels on area chart
    labs_area = dt_annual[year == max(year)][order(factor(fuel, levels = rev(levels(factor(dt_annual[, fuel])))))]
    labs_area[, cum_sum := cumsum(export)] 
    labs_area[, difference := diff(c(0,cum_sum))/2]
    labs_area[, position := cum_sum - difference]
    labs_area[1:2, position := c(0,1)]
    
    fig_exp_area_annual_abs = ggplot(dt_annual, 
                                 aes(x = year, y = export, group = fuel, fill = fuel)) + 
      geom_area() +
      labs(title = 'Annual U.S. primary energy exports by source (1949-2019)',
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
      geom_text(data = labs_area, aes(x = Inf, y = position, label = paste0(' ', fuel), color = fuel), 
                hjust = 0, size = 6.5, fontface = 'plain', family = 'Secca Soft') 
    
    fig_exp_area_annual_abs = ggplotGrob(fig_exp_area_annual_abs)
    fig_exp_area_annual_abs$layout$clip[fig_exp_area_annual_abs$layout$name == "panel"] = "off"
    
    ggsave(fig_exp_area_annual_abs, 
           filename = here::here('figures', 'energy', 'primary-energy-exports-by-source_annual_1949-2019_ats_absolute.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'energy', 'primary-energy-exports-by-source_annual_1949-2019_ats_absolute.pdf'),
                outfile = here::here('figures', 'energy', 'primary-energy-exports-by-source_annual_1949-2019_ats_absolute.pdf'))
    
    # save as png: 
    # ggsave(fig_exp_area_annual_abs, 
    #        filename = here::here('figures', 'energy', 'primary-energy-exports-by-source_annual_1949-2019_ats_absolute.png'), 
    #        width = 11.5, 
    #        height = 6.25, 
    #        dpi = 600)
    
  # exports, area, annual (proportion) -------
    
    # create dataset of where to put the labels on area chart
    labs_area_prop_exp = dt_annual[year == max(year)][order(factor(fuel, levels = rev(levels(factor(dt_annual[, fuel])))))]
    labs_area_prop_exp[, cum_sum := cumsum(prop_exp)] 
    labs_area_prop_exp[, difference := diff(c(0,cum_sum))/2]
    labs_area_prop_exp[, position := cum_sum - difference]
    labs_area_prop_exp[1:2, position := c(0,0.045)]
    
    fig_exp_area_annual_prop_exp = ggplot(dt_annual, aes(x = year, y = prop_exp, group = fuel, fill = fuel)) + 
      geom_area() +
      labs(title = 'Annual U.S. primary energy exports by source (1949-2019)',
           subtitle = 'Share of total primary energy exports', 
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
      geom_text(data = labs_area_prop_exp, aes(x = Inf, y = position, label = paste0(' ', fuel), color = fuel), 
                hjust = 0, size = 6.5, fontface = 'plain', family = 'Secca Soft')  
    
    fig_exp_area_annual_prop_exp = ggplotGrob(fig_exp_area_annual_prop_exp)
    fig_exp_area_annual_prop_exp$layout$clip[fig_exp_area_annual_prop_exp$layout$name == "panel"] = "off"
    
    ggsave(fig_exp_area_annual_prop_exp, 
           filename = here::here('figures', 'energy', 'primary-energy-exports-by-source_annual_1949-2019_ats_proportion.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'energy', 'primary-energy-exports-by-source_annual_1949-2019_ats_proportion.pdf'),
                outfile = here::here('figures', 'energy', 'primary-energy-exports-by-source_annual_1949-2019_ats_proportion.pdf'))
    
  # exports, line, annual, totals  -------
    
    # create dataset of where to put the labels on line chart
    labs_line_tot = dt_annual_tot[year == max(year)]
    labs_line_tot = labs_line_tot[order(rank(export))]
    labs_line_tot[, position := export]
    
    fig_exp_line_annual_tot = ggplot(dt_annual_tot, aes(x = year, y = export, group = fuel, color = fuel)) + 
      geom_line(size = 0.9) +
      labs(title = 'Annual U.S. primary energy exports by source (1949-2019)',
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
    
    fig_exp_line_annual_tot = ggplotGrob(fig_exp_line_annual_tot)
    fig_exp_line_annual_tot$layout$clip[fig_exp_line_annual_tot$layout$name == "panel"] = "off"
    
    ggsave(fig_exp_line_annual_tot, 
           filename = here::here('figures', 'energy', 'primary-energy-exports-by-source_totals_annual_1949-2019_lts.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'energy', 'primary-energy-exports-by-source_totals_annual_1949-2019_lts.pdf'),
                outfile = here::here('figures', 'energy', 'primary-energy-exports-by-source_totals_annual_1949-2019_lts.pdf'))
    
    # save as png:
    # ggsave(fig_exp_line_annual_tot, 
    #        filename = here::here('figures', 'energy', 'primary-energy-exports-by-source_totals_annual_1949-2019_lts.png'), 
    #        width = 11.5, 
    #        height = 6.25, 
    #        dpi = 600)
    
  # exports, line, monthly -------
    
    fig_exp_line_month = ggplot(dt_month, aes(x = month, y = export, group = fuel, color = fuel)) + 
      geom_line(size = 0.5) +
      labs(title = 'Monthly U.S. primary energy exports by source (Jan 1973-Apr 2020)',
           subtitle = 'Quadrillion BTU', 
           caption = 'Data: U.S. Energy Information Administration',
           x = NULL,
           y = NULL) +
      guides(color = 'none') +
      scale_x_date(breaks = seq(as.Date('1975-01-01'), as.Date('2020-01-01'), '5 years'), date_labels = "%b %Y", expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0), breaks = seq(0,0.9,0.1)) +
      scale_color_manual(values = pal_fuel) + 
      geom_dl(aes(label = fuel), method = list(dl.trans(x = x + .3), 'last.bumpup',  cex = 1.15, fontfamily = 'Secca Soft', fontface = 'plain')) +
      theme_line +
      theme(plot.margin = unit(c(1,9,1,1), "lines"))

    fig_exp_line_month = ggplotGrob(fig_exp_line_month)
    fig_exp_line_month$layout$clip[fig_exp_line_month$layout$name == "panel"] = "off"
    
    ggsave(fig_exp_line_month, 
           filename = here::here('figures', 'energy', 'primary-energy-exports-by-source_month_Jan1973-Apr2020_lts.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'energy', 'primary-energy-exports-by-source_month_Jan1973-Apr2020_lts.pdf'),
                outfile = here::here('figures', 'energy', 'primary-energy-exports-by-source_month_Jan1973-Apr2020_lts.pdf'))
    
    # save as png:
    # ggsave(fig_exp_line_month, 
    #        filename = here::here('figures', 'energy', 'primary-energy-exports-by-source_month_Jan1973-Apr2020_lts.png'), 
    #        width = 11.5, 
    #        height = 6.25, 
    #        dpi = 600)
    
    
  # imports, bar, 2019 ---------
    
    fig_imp_bar_2019 = ggplot(dt_annual[year == max(year)], 
                              aes(x = reorder(fuel, import), y = import, group = fuel, fill = fuel)) + 
      geom_bar(stat = "identity") +
      labs(title = 'Annual U.S. primary energy imports by source (2019)',
           subtitle = NULL, 
           caption = 'Data: U.S. Energy Information Administration',
           x = NULL,
           y = 'Quadrillion BTU', 
           fill = NULL) +
      scale_y_continuous(labels = scales::comma, expand = c(0,0), breaks = seq(0,16,2), limits = c(0,17)) +
      scale_color_manual(values = pal_fuel) +
      scale_fill_manual(values = pal_fuel) + 
      guides(fill = 'none',
             color ='none') +
      theme_bar_flipped + 
      geom_text(data = dt_annual[year == max(year)], aes(x = fuel, y = import + 0.1, label = paste0(signif(prop_imp*100,2), '%'), color = fuel), hjust = 0,
                size = 6, fontface = 'bold', family = 'Secca Soft') +
      coord_flip()
    
    ggsave(fig_imp_bar_2019, 
           filename = here::here('figures', 'energy', 'primary-energy-imports-by-source_2019_bar.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'energy', 'primary-energy-imports-by-source_2019_bar.pdf'),
                outfile = here::here('figures', 'energy', 'primary-energy-imports-by-source_2019_bar.pdf'))
    
  # imports, line, annual -------
    
    # create dataset of where to put the labels on line chart
    labs_line = dt_annual[year == max(year)]
    labs_line = labs_line[order(rank(import))]
    labs_line[, position := import]
    labs_line[ , flabel := fuel ]
    labs_line[ fuel == 'Coal', flabel := 'Coal, Coal Coke']
    labs_line[ fuel == 'Electricity', flabel := 'Biomass, Electricity']
    labs_line[ fuel == 'Coal Coke', flabel := NA ]
    labs_line[ fuel == 'Biomass', flabel := NA ]
    labs_line = labs_line[!is.na(flabel)]
    labs_line[1:2, position := c(0,0.9)]
    
    fig_imp_line_annual = ggplot(dt_annual, aes(x = year, y = import, group = fuel, color = fuel)) + 
      geom_line(size = 0.9) +
      labs(title = 'Annual U.S. primary energy imports by source (1949-2019)',
           subtitle = 'Quadrillion BTU', 
           caption = 'Data: U.S. Energy Information Administration',
           x = NULL,
           y = NULL) +
      guides(color = 'none') +
      scale_x_continuous(breaks = seq(1949,2019,5), limits = c(1949, 2019), expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) +
      scale_color_manual(values = pal_fuel) + 
      theme_line +
      geom_text(data = labs_line, aes(x = Inf, y = position, label = paste0(' ', flabel), color = fuel), hjust = 0,
                size = 6.5, fontface = 'plain', family = 'Secca Soft')
    
    fig_imp_line_annual = ggplotGrob(fig_imp_line_annual)
    fig_imp_line_annual$layout$clip[fig_imp_line_annual$layout$name == "panel"] = "off"
    
    ggsave(fig_imp_line_annual, 
           filename = here::here('figures', 'energy', 'primary-energy-imports-by-source_annual_1949-2019_lts.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'energy', 'primary-energy-imports-by-source_annual_1949-2019_lts.pdf'),
                outfile = here::here('figures', 'energy', 'primary-energy-imports-by-source_annual_1949-2019_lts.pdf'))
    
    # save as png:
    # ggsave(fig_imp_line_annual, 
    #        filename = here::here('figures', 'energy', 'primary-energy-imports-by-source_annual_1949-2019_lts.png'), 
    #        width = 11.5, 
    #        height = 6.25, 
    #        dpi = 600)
    
  # imports, area, annual (absolute) -------
    
    # create dataset of where to put the labels on area chart
    labs_area = dt_annual[year == max(year)][order(factor(fuel, levels = rev(levels(factor(dt_annual[, fuel])))))]
    labs_area[, cum_sum := cumsum(import)] 
    labs_area[, difference := diff(c(0,cum_sum))/2]
    labs_area[, position := cum_sum - difference]
    labs_area[1:2, position := c(0,1.1)]
    labs_area[7, position := 24.2]
    
    fig_imp_area_annual_abs = ggplot(dt_annual, 
                                     aes(x = year, y = import, group = fuel, fill = fuel)) + 
      geom_area() +
      labs(title = 'Annual U.S. primary energy imports by source (1949-2019)',
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
      geom_text(data = labs_area[ ! fuel %in% c('Biomass', 'Electricity')], aes(x = Inf, y = position, label = paste0(' ', fuel), color = fuel), 
                hjust = 0, size = 6.5, fontface = 'plain', family = 'Secca Soft') +
      geom_text(data = labs_area[ fuel %in% c('Biomass', 'Electricity')], aes(x = Inf, y = position, label = paste0(' ', fuel), color = fuel), 
                hjust = 0, size = 4.7, fontface = 'plain', family = 'Secca Soft') 
    
    fig_imp_area_annual_abs = ggplotGrob(fig_imp_area_annual_abs)
    fig_imp_area_annual_abs$layout$clip[fig_imp_area_annual_abs$layout$name == "panel"] = "off"
    
    ggsave(fig_imp_area_annual_abs, 
           filename = here::here('figures', 'energy', 'primary-energy-imports-by-source_annual_1949-2019_ats_absolute.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'energy', 'primary-energy-imports-by-source_annual_1949-2019_ats_absolute.pdf'),
                outfile = here::here('figures', 'energy', 'primary-energy-imports-by-source_annual_1949-2019_ats_absolute.pdf'))
    
    # save as png: 
    # ggsave(fig_imp_area_annual_abs, 
    #        filename = here::here('figures', 'energy', 'primary-energy-imports-by-source_annual_1949-2019_ats_absolute.png'), 
    #        width = 11.5, 
    #        height = 6.25, 
    #        dpi = 600)
    
  # imports, area, annual (proportion) -------
    
    # create dataset of where to put the labels on area chart
    labs_area_prop_imp = dt_annual[year == max(year)][order(factor(fuel, levels = rev(levels(factor(dt_annual[, fuel])))))]
    labs_area_prop_imp[, cum_sum := cumsum(prop_imp)] 
    labs_area_prop_imp[, difference := diff(c(0,cum_sum))/2]
    labs_area_prop_imp[, position := cum_sum - difference]
    labs_area_prop_imp[1:2, position := c(0,0.04)]
    labs_area_prop_imp[6:7, position := c(0.972,1.015)]
    
    fig_imp_area_annual_prop_imp = ggplot(dt_annual, aes(x = year, y = prop_imp, group = fuel, fill = fuel)) + 
      geom_area() +
      labs(title = 'Annual U.S. primary energy imports by source (1949-2019)',
           subtitle = 'Share of total primary energy imports', 
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
      geom_text(data = labs_area_prop_imp, aes(x = Inf, y = position, label = paste0(' ', fuel), color = fuel), 
                hjust = 0, size = 6.5, fontface = 'plain', family = 'Secca Soft')  
    
    fig_imp_area_annual_prop_imp = ggplotGrob(fig_imp_area_annual_prop_imp)
    fig_imp_area_annual_prop_imp$layout$clip[fig_imp_area_annual_prop_imp$layout$name == "panel"] = "off"
    
    ggsave(fig_imp_area_annual_prop_imp, 
           filename = here::here('figures', 'energy', 'primary-energy-imports-by-source_annual_1949-2019_ats_proportion.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'energy', 'primary-energy-imports-by-source_annual_1949-2019_ats_proportion.pdf'),
                outfile = here::here('figures', 'energy', 'primary-energy-imports-by-source_annual_1949-2019_ats_proportion.pdf'))
    
  # imports, line, annual, totals  -------
    
    # create dataset of where to put the labels on line chart
    labs_line_tot = dt_annual_tot[year == max(year)]
    labs_line_tot = labs_line_tot[order(rank(import))]
    labs_line_tot[, position := import]
    
    fig_imp_line_annual_tot = ggplot(dt_annual_tot, aes(x = year, y = import, group = fuel, color = fuel)) + 
      geom_line(size = 0.9) +
      labs(title = 'Annual U.S. primary energy imports by source (1949-2019)',
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
    
    fig_imp_line_annual_tot = ggplotGrob(fig_imp_line_annual_tot)
    fig_imp_line_annual_tot$layout$clip[fig_imp_line_annual_tot$layout$name == "panel"] = "off"
    
    ggsave(fig_imp_line_annual_tot, 
           filename = here::here('figures', 'energy', 'primary-energy-imports-by-source_totals_annual_1949-2019_lts.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'energy', 'primary-energy-imports-by-source_totals_annual_1949-2019_lts.pdf'),
                outfile = here::here('figures', 'energy', 'primary-energy-imports-by-source_totals_annual_1949-2019_lts.pdf'))
    
    # save as png:
    # ggsave(fig_imp_line_annual_tot, 
    #        filename = here::here('figures', 'energy', 'primary-energy-imports-by-source_totals_annual_1949-2019_lts.png'), 
    #        width = 11.5, 
    #        height = 6.25, 
    #        dpi = 600)
    
  # imports, line, monthly -------
    
    fig_imp_line_month = ggplot(dt_month, aes(x = month, y = import, group = fuel, color = fuel)) + 
      geom_line(size = 0.5) +
      labs(title = 'Monthly U.S. primary energy imports by source (Jan 1973-Apr 2020)',
           subtitle = 'Quadrillion BTU', 
           caption = 'Data: U.S. Energy Information Administration',
           x = NULL,
           y = NULL) +
      guides(color = 'none') +
      scale_x_date(breaks = seq(as.Date('1975-01-01'), as.Date('2020-01-01'), '5 years'), date_labels = "%b %Y", expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0), breaks = seq(0,2,0.2), limits = c(0,2)) +
      scale_color_manual(values = pal_fuel) + 
      geom_dl(aes(label = fuel), method = list(dl.trans(x = x + .3), 'last.bumpup',  cex = 1.15, fontfamily = 'Secca Soft', fontface = 'plain')) +
      theme_line +
      theme(plot.margin = unit(c(1,9,1,1), "lines"))
    
    fig_imp_line_month = ggplotGrob(fig_imp_line_month)
    fig_imp_line_month$layout$clip[fig_imp_line_month$layout$name == "panel"] = "off"
    
    ggsave(fig_imp_line_month, 
           filename = here::here('figures', 'energy', 'primary-energy-imports-by-source_month_Jan1973-Apr2020_lts.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'energy', 'primary-energy-imports-by-source_month_Jan1973-Apr2020_lts.pdf'),
                outfile = here::here('figures', 'energy', 'primary-energy-imports-by-source_month_Jan1973-Apr2020_lts.pdf'))
    
    # save as png:
    # ggsave(fig_imp_line_month, 
    #        filename = here::here('figures', 'energy', 'primary-energy-imports-by-source_month_Jan1973-Apr2020_lts.png'), 
    #        width = 11.5, 
    #        height = 6.25, 
    #        dpi = 600)
    
    
  # exports-imports, bar, 2019 ---------
    
    fig_all_bar_2019 = ggplot(dt_annual_long[year == max(year)], 
                              aes(x = reorder(fuel, value), y = value, group = type, fill = type)) + 
      geom_bar(position = "dodge", stat = "identity") +
      labs(title = 'Annual U.S. primary energy exports and imports by source (2019)',
           subtitle = NULL, 
           caption = 'Data: U.S. Energy Information Administration',
           x = NULL,
           y = 'Quadrillion BTU', 
           fill = NULL) +
      scale_y_continuous(expand = c(0,0), breaks = seq(0,16,2), limits = c(0,16)) +
      scale_color_manual(values = pal_type) +
      scale_fill_manual(values = pal_type, labels = c('Export', 'Import')) + 
      theme_bar_flipped  + 
      theme(legend.position = 'bottom') +
      coord_flip()

    ggsave(fig_all_bar_2019, 
           filename = here::here('figures', 'energy', 'primary-energy-exports-and-imports-by-source_2019_bar.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'energy', 'primary-energy-exports-and-imports-by-source_2019_bar.pdf'),
                outfile = here::here('figures', 'energy', 'primary-energy-exports-and-imports-by-source_2019_bar.pdf'))
    
  # exports-imports, line, annual -------
    
    fig_all_line_annual = ggplot(dt_annual_long, aes(x = year, y = value, color = fuel, linetype = type)) + 
      geom_line(size = 0.9) +
      labs(title = 'Annual U.S. primary energy exports and imports by source (1949-2019)',
           subtitle = 'Quadrillion BTU', 
           caption = 'Data: U.S. Energy Information Administration',
           x = NULL,
           y = NULL,
           color = NULL,
           linetype = NULL) +
      scale_x_continuous(breaks = seq(1949,2019,5), limits = c(1949, 2019), expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) +
      scale_color_manual(values = pal_fuel) + 
      scale_linetype_discrete(labels = c('Exports', 'Imports')) +
      theme_line +
      theme(legend.text = element_text(size = 14),
            legend.position = 'bottom',
            plot.margin = unit(c(1,2,1,1), "lines"))

    ggsave(fig_all_line_annual, 
           filename = here::here('figures', 'energy', 'primary-energy-exports-and-imports-by-source_annual_1949-2019_lts.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'energy', 'primary-energy-exports-and-imports-by-source_annual_1949-2019_lts.pdf'),
                outfile = here::here('figures', 'energy', 'primary-energy-exports-and-imports-by-source_annual_1949-2019_lts.pdf'))
    
    # save as png:
    # ggsave(fig_all_line_annual, 
    #        filename = here::here('figures', 'energy', 'primary-energy-exports-and-imports-by-source_annual_1949-2019_lts.png'), 
    #        width = 11.5, 
    #        height = 6.25, 
    #        dpi = 600)
    
  # exports-imports, line, annual (faceted) -------
  
    fig_all_line_annual_fac = ggplot(dt_annual_long, aes(x = year, y = value, color = fuel, linetype = type)) + 
      geom_line(size = 0.9) +
      facet_wrap( . ~ fuel, nrow = 3, scales = 'free_y') + 
      labs(title = 'Annual U.S. primary energy exports and imports by source (1949-2019)',
           subtitle = 'Quadrillion BTU', 
           caption = 'Data: U.S. Energy Information Administration',
           x = NULL,
           y = NULL,
           color = NULL,
           linetype = NULL) +
      guides(color = 'none') +
      scale_x_continuous(breaks = seq(1949,2019,20), limits = c(1949, 2019), expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) +
      scale_color_manual(values = pal_fuel) + 
      scale_linetype_discrete(labels = c('Exports', 'Imports')) +
      theme_line +
      theme(legend.text = element_text(size = 14),
            legend.position = 'bottom',
            plot.margin = unit(c(1,2,1,1), "lines"))
    
    ggsave(fig_all_line_annual_fac, 
           filename = here::here('figures', 'energy', 'primary-energy-exports-and-imports-by-source_annual_1949-2019_lts_faceted.pdf'), 
           width = 11.5, 
           height = 8)
    
    embed_fonts(here::here('figures', 'energy', 'primary-energy-exports-and-imports-by-source_annual_1949-2019_lts_faceted.pdf'),
                outfile = here::here('figures', 'energy', 'primary-energy-exports-and-imports-by-source_annual_1949-2019_lts_faceted.pdf'))
    
    # save as png:
    # ggsave(fig_all_line_annual, 
    #        filename = here::here('figures', 'energy', 'primary-energy-exports-and-imports-by-source_annual_1949-2019_lts_faceted.png'), 
    #        width = 11.5, 
    #        height = 6.25, 
    #        dpi = 600)
    
    
  # exports-imports, line, annual, totals -------
    
    fig_all_line_annual_tot = ggplot(dt_annual_tot_long[fuel == 'Total Energy'], aes(x = year, y = value, linetype = type)) + 
      geom_line(size = 1.1) +
      labs(title = 'Annual U.S. total primary energy exports and imports (1949-2019)',
           subtitle = 'Quadrillion BTU', 
           caption = 'Data: U.S. Energy Information Administration',
           x = NULL,
           y = NULL,
           color = NULL,
           linetype = NULL) +
      scale_x_continuous(breaks = seq(1949,2019,5), limits = c(1949, 2019), expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) +
      # scale_color_manual(values = pal_type) + 
      scale_linetype_discrete(labels = c('Exports', 'Imports')) +
      theme_line +
      theme(legend.text = element_text(size = 16),
            legend.position = 'bottom',
            plot.margin = unit(c(1,2,1,1), "lines"))
    
    ggsave(fig_all_line_annual_tot, 
           filename = here::here('figures', 'energy', 'primary-energy-exports-and-imports-by-source_totals_annual_1949-2019_lts.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'energy', 'primary-energy-exports-and-imports-by-source_totals_annual_1949-2019_lts.pdf'),
                outfile = here::here('figures', 'energy', 'primary-energy-exports-and-imports-by-source_totals_annual_1949-2019_lts.pdf'))
    
    # save as png:
    # ggsave(fig_all_line_annual_tot, 
    #        filename = here::here('figures', 'energy', 'primary-energy-exports-and-imports-by-source_totals_annual_1949-2019_lts.png'), 
    #        width = 11.5, 
    #        height = 6.25, 
    #        dpi = 600)
    
    
    
  # exports-imports, line, monthly, totals -------
    
    fig_all_line_month_tot = ggplot(dt_month_tot_long[fuel == 'Total Energy'], aes(x = month, y = value, linetype = type)) + 
      geom_line(size = 0.5) +
      labs(title = 'Monthly U.S. total primary energy exports and imports (Jan 1973-Apr 2020)',
           subtitle = 'Quadrillion BTU', 
           caption = 'Data: U.S. Energy Information Administration',
           x = NULL,
           y = NULL,
           color = NULL,
           linetype = NULL) +
      scale_x_date(breaks = seq(as.Date('1975-01-01'), as.Date('2020-01-01'), '5 years'), date_labels = "%b %Y", expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) +
      scale_linetype_discrete(labels = c('Exports', 'Imports')) +
      theme_line +
      theme(legend.text = element_text(size = 16),
            legend.position = 'bottom',
            plot.margin = unit(c(1,2,1,1), "lines"))
    
    ggsave(fig_all_line_month_tot, 
           filename = here::here('figures', 'energy', 'primary-energy-exports-and-imports-by-source_totals_month_Jan1973-Apr2020_lts.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'energy', 'primary-energy-exports-and-imports-by-source_totals_month_Jan1973-Apr2020_lts.pdf'),
                outfile = here::here('figures', 'energy', 'primary-energy-exports-and-imports-by-source_totals_month_Jan1973-Apr2020_lts.pdf'))
    
    # save as png:
    # ggsave(fig_all_line_month_tot, 
    #        filename = here::here('figures', 'energy', 'primary-energy-exports-and-imports-by-source_totals_monthly_Jan1973-Apr2020_lts.png'), 
    #        width = 11.5, 
    #        height = 6.25, 
    #        dpi = 600)
    
    