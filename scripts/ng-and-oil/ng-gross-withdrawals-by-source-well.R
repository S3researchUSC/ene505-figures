#  ---------------------------------------------------- INPUT DATA ----------------------------------------------------

month.file        = 'NG_PROD_SUM_DC_NUS_MMCF_M.xlsx' 
annnual.file      = 'NG_PROD_SUM_DC_NUS_MMCF_A.xlsx' 

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

  dt_month = as.data.table(read.xlsx(here::here('data', month.file), sheet = 'Data 1', startRow = 3, cols = c(1,3:6), detectDates = T))
  dt_annual = as.data.table(read.xlsx(here::here('data', annnual.file), sheet = 'Data 1', startRow = 3, cols = c(1,3:6), detectDates = T))

# rename columns ----
  
  colnames(dt_month) = c('date', 'Gas Wells', 'Oil Wells', 'Shale Gas', 'Coalbed Wells' )
  colnames(dt_annual) = c('date', 'Gas Wells', 'Oil Wells', 'Shale Gas', 'Coalbed Wells'  )
  
# set start and end dates ------
  
  dt_month = dt_month[ date >= '1993-01-15' & date <= '2018-12-15' ]
  dt_annual = dt_annual[ date >= '1967-06-30' & date <= '2018-06-30' ]

# melt data table from wide to long format -----
  
  dt_month = melt(dt_month, measure.vars = colnames(dt_month)[2:5],
                  variable.name = 'source', value.name = 'value')
  
  dt_annual = melt(dt_annual, measure.vars = colnames(dt_annual)[2:5],
                   variable.name = 'source', value.name = 'value')

# create year and month columns ------
  
  dt_annual[, year := year(date)]

# calculate proportions ------
  
  dt_month[, prop := value/sum(value, na.rm = T), by = c('date')]
  dt_annual[, prop := value/sum(value, na.rm = T), by = c('year')]

#  ------------------------------------------------------- FIGURES -------------------------------------------------------
  
  # bar, 2018 ---------
  
    fig_bar_2018 = ggplot(dt_annual[year == max(year)], 
                          aes(x = reorder(source, value), y = value, group = source, fill = source)) + 
      geom_bar(stat = "identity") +
      labs(title = 'Annual U.S. natural gas gross withdrawals by source well (2018)',
           subtitle = NULL, 
           caption = 'Data: U.S. Energy Information Administration',
           x = NULL,
           y = 'Million cubic feet', 
           fill = NULL) +
      scale_y_continuous(expand = c(0,0), labels = scales::comma, limits = c(0, 24.8e6)) +
      scale_color_manual(values = pal_wells) +
      scale_fill_manual(values = pal_wells) + 
      guides(fill = 'none',
             color ='none') +
      theme_bar_flipped + 
      geom_text(data = dt_annual[year == max(year)], aes(x = source, y = value + 50e3, label = paste0(signif(prop*100,2), '%'), color = source), hjust = 0,
                size = 6, fontface = 'bold', family = 'Secca Soft') +
      coord_flip()  +
    theme(plot.margin = unit(c(1,5,1,1), "lines"))
    
    ggsave(fig_bar_2018, 
           filename = here::here('figures', 'ng-and-oil', 'ng_gross-withdrawals-by-source-well_2018_bar.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'ng-and-oil', 'ng_gross-withdrawals-by-source-well_2018_bar.pdf'),
                outfile = here::here('figures', 'ng-and-oil', 'ng_gross-withdrawals-by-source-well_2018_bar.pdf'))
  
    
  # line, annual ------
    
    fig_line_annual = ggplot(dt_annual, aes(x = year, y = value, group = source, color = source)) + 
      geom_line(size = 0.9) +
      labs(title = 'Annual U.S. natural gas gross withdrawals by source well (1967-2018)',
           subtitle = 'Million cubic feet', 
           caption = 'Data: U.S. Energy Information Administration',
           x = NULL,
           y = NULL) +
      guides(color = 'none') +
      scale_x_continuous(breaks = seq(1970,2018,5), limits = c(1967, 2018), expand = c(0,0)) +
      scale_y_continuous(labels = scales::comma, expand = c(0,0)) +
      scale_color_manual(values = pal_wells) + 
      geom_dl(aes(label = source), method = list(dl.trans(x = x + .3), 'last.bumpup',  cex = 1.2, fontfamily = 'Secca Soft', fontface = 'plain')) +
      theme_line +
      theme(plot.margin = unit(c(1,7,1,1), "lines"))
    
    fig_line_annual = ggplotGrob(fig_line_annual)
    fig_line_annual$layout$clip[fig_line_annual$layout$name == "panel"] = "off"
    
    ggsave(fig_line_annual, 
           filename = here::here('figures', 'ng-and-oil', 'ng_gross-withdrawals-by-source-well_annual_1967-2018_lts.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'ng-and-oil', 'ng_gross-withdrawals-by-source-well_annual_1967-2018_lts.pdf'),
                outfile = here::here('figures', 'ng-and-oil', 'ng_gross-withdrawals-by-source-well_annual_1967-2018_lts.pdf'))

    
  # area, annual (absolute) -------
    
    # create dataset of where to put the labels on area chart
    labs_area = dt_annual[year == max(year)][order(factor(source, levels = rev(levels(factor(dt_annual[, source])))))]
    labs_area[, cum_sum := cumsum(value)] 
    labs_area[, difference := diff(c(0,cum_sum))/2]
    labs_area[, position := cum_sum - difference]
    
    fig_area_annual_abs = ggplot(dt_annual, aes(x = year, y = value, group = source, fill = source)) + 
      geom_area() +
      labs(title = 'Annual U.S. natural gas gross withdrawals by source well (1967-2018)',
           subtitle = 'Million cubic feet', 
           caption = 'Data: U.S. Energy Information Administration',
           x = NULL,
           y = NULL) +
      scale_x_continuous(breaks = seq(1970,2018,5), limits = c(1967, 2018), expand = c(0,0)) +
      scale_y_continuous(labels = scales::comma, breaks = seq(0,35e6,5e6), expand = c(0,0)) +
      scale_fill_manual(values = pal_wells) + 
      scale_color_manual(values = pal_wells) + 
      guides(fill = 'none',
             color = 'none') +
      theme_area_labeled +
      geom_text(data = labs_area, aes(x = Inf, y = position, label = paste0(' ', source), color = source),
                hjust = 0, size = 6.5, fontface = 'plain', family = 'Secca Soft')  +
      theme(plot.margin = unit(c(1,8,1,1), "lines"))
    
    fig_area_annual_abs = ggplotGrob(fig_area_annual_abs)
    fig_area_annual_abs$layout$clip[fig_area_annual_abs$layout$name == "panel"] = "off"
    
    ggsave(fig_area_annual_abs, 
           filename = here::here('figures', 'ng-and-oil', 'ng_gross-withdrawals-by-source-well_annual_1967-2018_ats_absolute.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'ng-and-oil', 'ng_gross-withdrawals-by-source-well_annual_1967-2018_ats_absolute.pdf'),
                outfile = here::here('figures', 'ng-and-oil', 'ng_gross-withdrawals-by-source-well_annual_1967-2018_ats_absolute.pdf'))

  # area, annual (proportion) -------
    
    # create dataset of where to put the labels on area chart
    labs_area_prop = dt_annual[year == max(year)][order(factor(source, levels = rev(levels(factor(dt_annual[, source])))))]
    labs_area_prop[, cum_sum := cumsum(prop)] 
    labs_area_prop[, difference := diff(c(0,cum_sum))/2]
    labs_area_prop[, position := cum_sum - difference]
    
    fig_area_annual_prop = ggplot(dt_annual, aes(x = year, y = prop, group = source, fill = source, color = source)) + 
      geom_area() +
      labs(title = 'Annual U.S. natural gas gross withdrawals by source well (1967-2018)',
           subtitle = 'Share of total natural gas gross withdrawals', 
           caption = 'Data: U.S. Energy Information Administration',
           x = NULL,
           y = NULL) +
      scale_x_continuous(breaks = seq(1970,2018,5), limits = c(1967, 2018), expand = c(0,0)) +
      scale_y_continuous(labels = scales::percent, expand = c(0,0)) +
      scale_fill_manual(values = pal_wells) + 
      scale_color_manual(values = pal_wells) + 
      guides(fill = 'none',
             color = 'none') +
      geom_text(data = labs_area_prop, aes(x = Inf, y = position, label = paste0(' ', source), color = source), 
                hjust = 0, size = 6.5, fontface = 'plain', family = 'Secca Soft')   +
      theme_area_labeled + 
      theme(plot.margin = unit(c(1,8,1,1), "lines"))
    
    fig_area_annual_prop = ggplotGrob(fig_area_annual_prop)
    fig_area_annual_prop$layout$clip[fig_area_annual_prop$layout$name == "panel"] = "off"
    
    ggsave(fig_area_annual_prop, 
           filename = here::here('figures', 'ng-and-oil', 'ng_gross-withdrawals-by-source-well_annual_1967-2018_ats_proportion.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'ng-and-oil', 'ng_gross-withdrawals-by-source-well_annual_1967-2018_ats_proportion.pdf'),
                outfile = here::here('figures', 'ng-and-oil', 'ng_gross-withdrawals-by-source-well_annual_1967-2018_ats_proportion.pdf'))

    
  # line, monthly ------
    
    fig_line_month = ggplot(dt_month, aes(x = date, y = value, group = source, color = source)) + 
      geom_line(size = 0.5) +
      labs(title = 'Monthly U.S. natural gas gross withdrawals by source well (Jan 1993-Dec 2018)',
           subtitle = 'Million cubic feet', 
           caption = 'Data: U.S. Energy Information Administration',
           x = NULL,
           y = NULL) +
      guides(color = 'none') +
      scale_x_date(breaks = seq(as.Date('1993-01-15'), as.Date('2018-12-15'), '3 years'), date_labels = "%b %Y", expand = c(0,0)) +
      scale_y_continuous(labels = scales::comma, expand = c(0,0)) +
      scale_color_manual(values = pal_wells) + 
      geom_dl(aes(label = source), method = list(dl.trans(x = x + .3), 'last.bumpup',  cex = 1.2, fontfamily = 'Secca Soft', fontface = 'plain')) +
      theme_line +
      theme(plot.margin = unit(c(1,7,1,1), "lines"))
    
    fig_line_month = ggplotGrob(fig_line_month)
    fig_line_month$layout$clip[fig_line_month$layout$name == "panel"] = "off"
    
    ggsave(fig_line_month, 
           filename = here::here('figures', 'ng-and-oil', 'ng_gross-withdrawals-by-source-well_month_Jan1993-Dec2018_lts.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'ng-and-oil', 'ng_gross-withdrawals-by-source-well_month_Jan1993-Dec2018_lts.pdf'),
                outfile = here::here('figures', 'ng-and-oil', 'ng_gross-withdrawals-by-source-well_month_Jan1993-Dec2018_lts.pdf'))
