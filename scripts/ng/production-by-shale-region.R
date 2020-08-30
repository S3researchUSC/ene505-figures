#  ---------------------------------------------------- INPUT DATA ----------------------------------------------------

# data downloaded from: https://www.eia.gov/petroleum/drilling/
data.file     = 'dpr-data.xlsx' 

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
  library(cowplot)

# source color palettes and plot themes ------

  items = list.files(here::here('src'))
  sapply(here::here('src', items), source)

# load data ------
  
  sheets = getSheetNames(here::here('data', data.file))[1:7]
  regions = sheets
  regions = gsub(" Region", "", regions)
  regions = regions[1:7]
  
  list_dt = lapply(sheets, openxlsx::read.xlsx, xlsxFile = here::here('data', data.file), startRow = 2, detectDates = TRUE)
  colnames = c('month', 'rig_count', 'oil_prod_per_rig', 'oil_legacy_prod_change', 'oil_total_prod', 'prod_per_rig', 'legacy_prod_change', 'total_prod')
  list_dt = lapply(list_dt, setNames, colnames)
  list_dt = lapply(1:7, function(x) cbind(list_dt[[x]], region = regions[x] ))
  
  dt_dpr = rbindlist(list_dt)
  
# remove months without  -----
  
  dt_dpr = dt_dpr[! is.na(rig_count)]
  
# calculate proportions ------
  
  dt_dpr[, oil_prop_prod := oil_total_prod/sum(oil_total_prod, na.rm = T), by = c('month')]
  dt_dpr[, prop_prod := total_prod/sum(total_prod, na.rm = T), by = c('month')]
  
# aggregate across shale regions ------
  
  dt_dpr_sum = dt_dpr[, lapply(.SD, sum, na.rm=TRUE), by = month, .SDcols = c('rig_count', 'oil_total_prod', 'total_prod') ] 
  dt_dpr_mean = dt_dpr[, lapply(.SD, mean, na.rm=TRUE), by = month, .SDcols = c( 'oil_prod_per_rig', 'prod_per_rig') ] 
  dt_dpr_agg = dt_dpr_sum[dt_dpr_mean, on = 'month']

#  ------------------------------------------------------- FIGURES -------------------------------------------------------
  
  # line, ng ------
  
    fig_line_ng = ggplot(dt_dpr, aes(x = month, y = total_prod/1000, group = region, color = region)) + 
      geom_line(size = 0.9) +
      labs(title = 'Natural gas production by shale region (Jan 2007-Jul 2020)',
           subtitle = 'Million cubic feet per day', 
           caption = 'Data: U.S. Energy Information Administration',
           x = NULL,
           y = NULL) +
      guides(color = 'none') +
      scale_x_date(breaks = seq(as.Date('2008-01-01'), as.Date('2020-01-01'), '2 years'), date_labels = "%b %Y", expand = c(0,0)) +
      scale_y_continuous(labels = scales::comma, expand = c(0,0)) +
      scale_color_manual(values = pal_shale) + 
      geom_dl(aes(label = region), method = list(dl.trans(x = x + .3), 'last.bumpup',  cex = 1.2, fontfamily = 'Secca Soft', fontface = 'plain')) +
      theme_line +
      theme(plot.margin = unit(c(1,6,1,1), "lines"))
    
    fig_line_ng = ggplotGrob(fig_line_ng)
    fig_line_ng$layout$clip[fig_line_ng$layout$name == "panel"] = "off"
    
    ggsave(fig_line_ng, 
           filename = here::here('figures', 'ng', 'production-by-shale-region_Jan2007-Jul2020_lts.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'ng', 'production-by-shale-region_Jan2007-Jul2020_lts.pdf'),
                outfile = here::here('figures', 'ng', 'production-by-shale-region_Jan2007-Jul2020_lts.pdf'))
  
  # area, ng (absolute) ------
    
    # create dataset of where to put the labels on area chart
    labs_area = dt_dpr[month == max(month)][order(factor(region, levels = rev(levels(factor(dt_dpr[, region])))))]
    labs_area[, cum_sum := cumsum(total_prod/1000)] 
    labs_area[, difference := diff(c(0,cum_sum))/2]
    labs_area[, position := cum_sum - difference]

    fig_area_ng = ggplot(dt_dpr, aes(x = month, y = total_prod/1000, group = region, fill = region, color = region)) + 
      geom_area() +
      labs(title = 'Natural gas production by shale region (Jan 2007-Jul 2020)',
           subtitle = 'Million cubic feet per day', 
           caption = 'Data: U.S. Energy Information Administration',
           x = NULL,
           y = NULL) +
      scale_x_date(breaks = seq(as.Date('2008-01-01'), as.Date('2020-01-01'), '2 years'), date_labels = "%b %Y", expand = c(0,0)) +
      scale_y_continuous(labels = scales::comma, expand = c(0,0)) +
      scale_fill_manual(values = pal_shale) + 
      scale_color_manual(values = pal_shale) + 
      guides(fill = 'none',
             color = 'none') +
      geom_text(data = labs_area, aes(x = max(dt_dpr[, month]), y = position, label = paste0(' ', region), color = region), 
                hjust = 0, size = 6.5, fontface = 'plain', family = 'Secca Soft')   +
      theme_area_labeled  +
      theme(plot.margin = unit(c(1,8,1,1), "lines"))
    
    fig_area_ng = ggplotGrob(fig_area_ng)
    fig_area_ng$layout$clip[fig_area_ng$layout$name == "panel"] = "off"
    
    ggsave(fig_area_ng, 
           filename = here::here('figures', 'ng', 'production-by-shale-region_Jan2007-Jul2020_ats_absolute.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'ng', 'production-by-shale-region_Jan2007-Jul2020_ats_absolute.pdf'),
                outfile = here::here('figures', 'ng', 'production-by-shale-region_Jan2007-Jul2020_ats_absolute.pdf'))
  
  # area, ng (proportion) -------
    
    # create dataset of where to put the labels on area chart
    labs_area_prop = dt_dpr[month == max(month)][order(factor(region, levels = rev(levels(factor(dt_dpr[, region])))))]
    labs_area_prop[, cum_sum := cumsum(prop_prod)] 
    labs_area_prop[, difference := diff(c(0,cum_sum))/2]
    labs_area_prop[, position := cum_sum - difference]
    
    fig_area_prop = ggplot(dt_dpr, aes(x = month, y = prop_prod, group = region, fill = region, color = region)) + 
      geom_area() +
      labs(title = 'Natural gas production by shale region (Jan 2007-Jul 2020)',
           subtitle = 'Share of total natural gas production', 
           caption = 'Data: U.S. Energy Information Administration',
           x = NULL,
           y = NULL) +
      scale_x_date(breaks = seq(as.Date('2008-01-01'), as.Date('2020-01-01'), '2 years'), date_labels = "%b %Y", expand = c(0,0)) +
      scale_y_continuous(labels = scales::percent, expand = c(0,0)) +
      scale_fill_manual(values = pal_shale) + 
      scale_color_manual(values = pal_shale) + 
      guides(fill = 'none',
             color = 'none') +
      geom_text(data = labs_area_prop, aes(x = max(dt_dpr[, month]), y = position, label = paste0(' ', region), color = region), 
                hjust = 0, size = 6.5, fontface = 'plain', family = 'Secca Soft')   +
      theme_area_labeled + 
      theme(plot.margin = unit(c(1,8,1,1), "lines"))
    
    fig_area_prop = ggplotGrob(fig_area_prop)
    fig_area_prop$layout$clip[fig_area_prop$layout$name == "panel"] = "off"
    
    ggsave(fig_area_prop, 
           filename = here::here('figures', 'ng', 'production-by-shale-region_Jan2007-Jul2020_ats_proportion.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'ng', 'production-by-shale-region_Jan2007-Jul2020_ats_proportion.pdf'),
                outfile = here::here('figures', 'ng', 'production-by-shale-region_Jan2007-Jul2020_ats_proportion.pdf'))
    
  # line, ng new prod vs rig count (aggregated) ------
    
    fig_line_rig = ggplot(dt_dpr_agg) + 
      geom_line(aes(x = month, y = prod_per_rig, col = 'ng', linetype = 'ng'), size = 1.2) +
      geom_line(aes(x = month, y = rig_count*(8000/1600), col = 'rig', linetype = 'rig'), size = 1.2) +
      labs(title = 'Average new-well natural gas production per rig versus total rig count (Jan 2007-Jul 2020)',
           caption = 'Data: U.S. Energy Information Administration',
           x = NULL,
           y = NULL,
           color = 'guide',
           linetype = 'guide') +
      scale_x_date(breaks = seq(as.Date('2008-01-01'), as.Date('2020-01-01'), '2 years'), date_labels = "%b %Y", expand = c(0,0)) +
      scale_y_continuous(labels = scales::comma, breaks = seq(0,8000,1000), limits = c(0,1.11*max(dt_dpr_agg[, prod_per_rig])), expand = c(0,0),
                         sec.axis = sec_axis(trans=~./(8000/1600), breaks = seq(0,1600,200), labels = scales::comma)) +
      scale_color_manual(breaks = c('ng', 'rig'),  
                         values = c(col_ng, col_rig),
                         labels = c('Average new-well natural gas production per rig (thousand cubic feet per day)', 'Total rig count (number of rigs)')) +
      scale_linetype_manual(breaks = c('ng', 'rig'), 
                            values = c(1, 2),  
                            labels = c('Average new-well natural gas production per rig (thousand cubic feet per day)', 'Total rig count (number of rigs)')) +
      annotate("text", label = "Thousand cubic feet per day", x = min(dt_dpr_agg[, month]), y = 1.11*max(dt_dpr_agg[, prod_per_rig]), 
               size = 6.5, fontface = 'plain', family = 'Secca Soft', hjust = 0.27) +
      annotate("text", label = "Number of rigs", x = max(dt_dpr_agg[, month]), y = 1.11*max(dt_dpr_agg[, prod_per_rig]), 
               size = 6.5, fontface = 'plain', family = 'Secca Soft') +
      guides(color = guide_legend(nrow = 2, title = NULL),
             linetype = guide_legend(title = NULL)) +
      theme_line + 
      theme(plot.margin = unit(c(1,2,1,1), "lines"),
            legend.key.width = unit(2.5,"line"))
    
    fig_line_rig = ggplotGrob(fig_line_rig)
    fig_line_rig$layout$clip[fig_line_rig$layout$name == "panel"] = "off"

    ggsave(fig_line_rig, 
           filename = here::here('figures', 'ng', 'new-well-production-vs-rig-count_Jan2007-Jul2020_lts_aggregated.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'ng', 'new-well-production-vs-rig-count_Jan2007-Jul2020_lts_aggregated.pdf'),
                outfile = here::here('figures', 'ng', 'new-well-production-vs-rig-count_Jan2007-Jul2020_lts_aggregated.pdf'))
    
  # line, ng new prod vs rig count (faceted) ------
    
    fig_line_rig_fac = ggplot(dt_dpr) + 
      geom_line(aes(x = month, y = prod_per_rig, color = region, linetype = '1'), size = 0.9) +
      geom_line(aes(x = month, y = rig_count*(25000/500), color = region, linetype = '3'), size = 0.9) +
      facet_wrap( . ~ region, nrow = 3) + 
      labs(title = 'New-well natural gas production per rig versus rig count by shale region (Jan 2007-Jul 2020)',
           caption = 'Data: U.S. Energy Information Administration',
           x = NULL,
           y = NULL,
           linetype = NULL) +
      guides(color = 'none',
             linetype = guide_legend(nrow = 2)) +
      scale_x_date(breaks = seq(as.Date('2008-01-01'), as.Date('2020-01-01'), '4 years'), date_labels = "%b %Y", expand = c(0,0)) +
      scale_y_continuous(labels = scales::comma, name = 'Thousand cubic feet per day', breaks = seq(0,25000,5000), expand = c(0,0),
                         sec.axis = sec_axis( trans=~./(25000/500), name = "Number of rigs", breaks = seq(0,500,100), labels = scales::comma)) +
      scale_color_manual(values = pal_shale) +
      scale_linetype_discrete(labels = c('New-well natural gas production per rig (thousand cubic feet per day)', 
                                         'Rig count (number of rigs)')) +
      theme_line +
      theme(plot.margin = unit(c(1,2,1,1), "lines"),
            plot.title = element_text(size = 24),
            legend.text = element_text(size = 22))
    
    ggsave(fig_line_rig_fac, 
           filename = here::here('figures', 'ng', 'new-well-production-vs-rig-count_Jan2007-Jul2020_lts_faceted.pdf'), 
           width = 17, 
           height = 9.7)
    
    embed_fonts(here::here('figures', 'ng', 'new-well-production-vs-rig-count_Jan2007-Jul2020_lts_faceted.pdf'),
                outfile = here::here('figures', 'ng', 'new-well-production-vs-rig-count_Jan2007-Jul2020_lts_faceted.pdf'))
    
