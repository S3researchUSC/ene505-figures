# plots generated:
# 

# ------------------------------------ inputs ------------------------------------

  coal_file       = "international-coal-by-country-1980-2018.csv"

# ------------------------------------ main script ------------------------------------

# load libraries -------

  library(data.table)
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

  # read in csv of coal data
    coal_data = fread(here::here('data', coal_file), skip = 1, header = T)
  
  # rename V2 column
    setnames(coal_data, 'V2', 'country')
  
  # remove whitespace from country names
    coal_data[, country := trimws(country, which = 'both')]
    
# melt data from wide to long format -----
    
  coal_long = melt(coal_data, measure.vars = as.character(1980:2018),
                  variable.name = 'year', value.name = 'value')
    
# convert year and values to numeric type -----
    
  coal_long[, year := as.numeric(as.character(year))]
  coal_long[, value := as.numeric(value)]
  
# categorize values by type using API -----
  
  coal_long[API %like% 'INTL.7-1', type := 'production']
  coal_long[API %like% 'INTL.7-2', type := 'consumption']
  coal_long[API %like% 'INTL.7-3', type := 'imports']
  coal_long[API %like% 'INTL.7-4', type := 'exports']
  coal_long[API %like% 'INTL.7-6', type := 'reserves']
  
# add units -----
  
  coal_long[, unit := ifelse(type == 'reserves',
                             'million short tons',
                             'thousand short tons')]

# remove rows that are just labels from the raw data ----
  
  coal_long = coal_long[!is.na(type)]
  
# rank values in each category (production, consumption, imports, exports, and reserves) in 2018 ------
  
  coal_max_year = coal_long[!is.na(value) & ! country == 'World']
  coal_max_year[, max_year := max(year), by = 'type']
  coal_max_year = coal_max_year[year == max_year]
  coal_max_year[, rank := frank(-value), by = 'type']
  rank_country = unique(coal_max_year[, c('country', 'type', 'rank')])
  
# merge with main coal data ----
  
  coal_ranked = merge(coal_long, rank_country, by = c('country', 'type'))

# label all non-top 5 countries as "Other" -----
  
  coal_ranked[, label := ifelse(rank %in% 1:5,
                                country,
                                'All Other Countries')]
  
# aggregate by new country grouping -----
  
  coal_agg = coal_ranked[, .(value = sum(value, na.rm = T)), by = .(year, label, type, unit)]
  setcolorder(coal_agg, c('year', 'type', 'label', 'value', 'unit'))
  
# calculate proportion (or percentage) contributed by type for each year -----
  
  coal_agg[, prop := value/sum(value), by = c('type', 'year')]
  
# get top 5 for each type -----
  
  top5_prod = rank_country[type == 'production' & rank %in% 1:5][order(rank), country]
  top5_cons = rank_country[type == 'consumption' & rank %in% 1:5][order(rank), country]
  top5_imp = rank_country[type == 'imports' & rank %in% 1:5][order(rank), country]
  top5_exp = rank_country[type == 'exports' & rank %in% 1:5][order(rank), country]
  top5_res = rank_country[type == 'reserves' & rank %in% 1:5][order(rank), country]
  
# ------------------------------------ figures ------------------------------------
  
  # palettes -----
  
    pal_prod = pal_5countries
    names(pal_prod)[2:6] = top5_prod
    
    pal_cons = pal_5countries
    names(pal_cons)[2:6] = top5_cons
    
    pal_imp = pal_5countries
    names(pal_imp)[2:6] = top5_imp
    
    pal_exp = pal_5countries
    names(pal_exp)[2:6] = top5_exp
    
  # line, production -----
  
    labs_line_prod = coal_agg[type == 'production' & year == max(year)]
    labs_line_prod = labs_line_prod[order(rank(value))]
    labs_line_prod[, position := value/1e3]
    labs_line_prod[1:4, position := c(435,600,750,930)]
    
    line_prod = ggplot(coal_agg[type == 'production'], aes(x = year, y = value/1e3, color = label)) + 
      geom_line(size = 0.9) +
      labs(title = 'Annual coal production by country (1980-2018)',
           subtitle = 'Million short tons', 
           caption = 'Top 5 coal producing countries in 2018 shown individually. All other countries aggregated. Data: U.S. Energy Information Administration', 
           x = NULL,
           y = NULL) +
      guides(color = 'none') +
      scale_x_continuous(breaks = seq(1980,2018,5), limits = c(1980, 2018), expand = c(0,0)) +
      scale_y_continuous(labels = scales::comma, expand = c(0,0)) +
      scale_color_manual(values = pal_prod) + 
      theme_line +
      geom_text(data = labs_line_prod, aes(x = Inf, y = position, label = paste0(' ', label), color = label), hjust = 0,
                size = 5.6, fontface = 'plain', family = 'Secca Soft')
    
    line_prod = ggplotGrob(line_prod)
    line_prod$layout$clip[line_prod$layout$name == "panel"] = "off"
    
    ggsave(line_prod, 
           filename = here::here('figures', 'world', 'coal-production-by-country_annual_1980-2018_lts.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'world', 'coal-production-by-country_annual_1980-2018_lts.pdf'),
                outfile = here::here('figures', 'world', 'coal-production-by-country_annual_1980-2018_lts.pdf'))
  
  # area, production (absolute) -----
  
    labs_area_prod = coal_agg[type == 'production' & year == max(year)][order(factor(label, levels = rev(c('All Other Countries', rev(top5_prod)))))]
    labs_area_prod[, cum_sum := cumsum(value/1e3)] 
    labs_area_prod[, difference := diff(c(0,cum_sum))/2]
    labs_area_prod[, position := cum_sum - difference]
    
    area_prod = ggplot(coal_agg[type == 'production'], 
                       aes(x = year, y = value/1e3, fill = factor(label, levels = c('All Other Countries', rev(top5_prod))))) + 
      geom_area() +
      labs(title = 'Annual coal production by country (1980-2018)',
           subtitle = 'Million short tons', 
           caption = 'Top 5 coal producing countries in 2018 shown individually. All other countries aggregated. Data: U.S. Energy Information Administration', 
           x = NULL,
           y = NULL) +
      scale_x_continuous(breaks = seq(1980,2018,5), limits = c(1980, 2018), expand = c(0,0)) +
      scale_y_continuous(labels = scales::comma, expand = c(0,0), breaks = seq(1e3,10e3,1e3)) +
      guides(fill = 'none',
             color = 'none') +
      scale_color_manual(values = pal_prod) + 
      scale_fill_manual(values = pal_prod) + 
      theme_area_labeled +
      geom_text(data = labs_area_prod, aes(x = Inf, y = position, label = paste0(' ', label), color = label), 
                hjust = 0, size = 6.5, fontface = 'plain', family = 'Secca Soft')
    
    area_prod = ggplotGrob(area_prod)
    area_prod$layout$clip[area_prod$layout$name == "panel"] = "off"
    
    ggsave(area_prod, 
           filename = here::here('figures', 'world', 'coal-production-by-country_annual_1980-2018_ats_absolute.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'world', 'coal-production-by-country_annual_1980-2018_ats_absolute.pdf'),
                outfile = here::here('figures', 'world', 'coal-production-by-country_annual_1980-2018_ats_absolute.pdf'))
    
    
  # area, production (proportion) -----
    
    labs_area_prod_prop = coal_agg[type == 'production' & year == max(year)][order(factor(label, levels = rev(c('All Other Countries', rev(top5_prod)))))]
    labs_area_prod_prop[, cum_sum := cumsum(prop)] 
    labs_area_prod_prop[, difference := diff(c(0,cum_sum))/2]
    labs_area_prod_prop[, position := cum_sum - difference]
    
    area_prod_prop = ggplot(coal_agg[type == 'production'], 
                       aes(x = year, y = prop, fill = factor(label, levels = c('All Other Countries', rev(top5_prod))))) + 
      geom_area() +
      labs(title = 'Annual coal production by country (1980-2018)',
           subtitle = 'Share of global coal production', 
           caption = 'Top 5 coal producing countries in 2018 shown individually. All other countries aggregated. Data: U.S. Energy Information Administration', 
           x = NULL,
           y = NULL) +
      scale_x_continuous(breaks = seq(1980,2018,5), limits = c(1980, 2018), expand = c(0,0)) +
      scale_y_continuous(labels = scales::percent, expand = c(0,0)) +
      guides(fill = 'none',
             color = 'none') +
      scale_color_manual(values = pal_prod) + 
      scale_fill_manual(values = pal_prod) + 
      theme_area_labeled +
      geom_text(data = labs_area_prod_prop, aes(x = Inf, y = position, label = paste0(' ', label), color = label), 
                hjust = 0, size = 6.5, fontface = 'plain', family = 'Secca Soft')
    
    area_prod_prop = ggplotGrob(area_prod_prop)
    area_prod_prop$layout$clip[area_prod_prop$layout$name == "panel"] = "off"
    
    ggsave(area_prod_prop, 
           filename = here::here('figures', 'world', 'coal-production-by-country_annual_1980-2018_ats_proportion.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'world', 'coal-production-by-country_annual_1980-2018_ats_proportion.pdf'),
                outfile = here::here('figures', 'world', 'coal-production-by-country_annual_1980-2018_ats_proportion.pdf'))
    
  # line, consumption -----
    
    labs_line_cons = coal_agg[type == 'consumption' & year == max(year)]
    labs_line_cons = labs_line_cons[order(rank(value))]
    labs_line_cons[, position := value/1e3]
    labs_line_cons[1:2, position := c(150,320)]
    
    line_cons = ggplot(coal_agg[type == 'consumption'], aes(x = year, y = value/1e3, color = label)) + 
      geom_line(size = 0.9) +
      labs(title = 'Annual coal consumption by country (1980-2018)',
           subtitle = 'Million short tons', 
           caption = 'Top 5 coal consuming countries in 2018 shown individually. All other countries aggregated. Data: U.S. Energy Information Administration', 
           x = NULL,
           y = NULL) +
      guides(color = 'none') +
      scale_x_continuous(breaks = seq(1980,2018,5), limits = c(1980, 2018), expand = c(0,0)) +
      scale_y_continuous(labels = scales::comma, expand = c(0,0), breaks = seq(0,5e3,1e3), limits = c(0,5e3)) +
      scale_color_manual(values = pal_cons) + 
      theme_line +
      # geom_dl(aes(label = label), method = list(dl.trans(x = x + .3), 'last.bumpup', cex = 1.5, fontfamily = 'Secca Soft', fontface = 'plain')) +
      geom_text(data = labs_line_cons, aes(x = Inf, y = position, label = paste0(' ', label), color = label), hjust = 0,
                size = 5.6, fontface = 'plain', family = 'Secca Soft') 
    
    line_cons = ggplotGrob(line_cons)
    line_cons$layout$clip[line_cons$layout$name == "panel"] = "off"
    
    ggsave(line_cons, 
           filename = here::here('figures', 'world', 'coal-consumption-by-country_annual_1980-2018_lts.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'world', 'coal-consumption-by-country_annual_1980-2018_lts.pdf'),
                outfile = here::here('figures', 'world', 'coal-consumption-by-country_annual_1980-2018_lts.pdf'))
    

  # area, consumption (absolute) -----
    
    labs_area_cons = coal_agg[type == 'consumption' & year == max(year)][order(factor(label, levels = rev(c('All Other Countries', rev(top5_cons)))))]
    labs_area_cons[, cum_sum := cumsum(value/1e3)] 
    labs_area_cons[, difference := diff(c(0,cum_sum))/2]
    labs_area_cons[, position := cum_sum - difference]
    labs_area_cons[4:5, position := c(6090,6490)]
    
    area_cons = ggplot(coal_agg[type == 'consumption'], 
                       aes(x = year, y = value/1e3, fill = factor(label, levels = c('All Other Countries', rev(top5_cons))))) + 
      geom_area() +
      labs(title = 'Annual coal consumption by country (1980-2018)',
           subtitle = 'Million short tons', 
           caption = 'Top 5 coal consuming countries in 2018 shown individually. All other countries aggregated. Data: U.S. Energy Information Administration', 
           x = NULL,
           y = NULL) +
      scale_x_continuous(breaks = seq(1980,2018,5), limits = c(1980, 2018), expand = c(0,0)) +
      scale_y_continuous(labels = scales::comma, expand = c(0,0), breaks = seq(1e3,10e3,1e3), limits = c(0,9e3)) +
      guides(fill = 'none',
             color = 'none') +
      scale_color_manual(values = pal_cons) + 
      scale_fill_manual(values = pal_cons) + 
      theme_area_labeled +
      geom_text(data = labs_area_cons, aes(x = Inf, y = position, label = paste0(' ', label), color = label), 
                hjust = 0, size = 6.5, fontface = 'plain', family = 'Secca Soft')
    
    area_cons = ggplotGrob(area_cons)
    area_cons$layout$clip[area_cons$layout$name == "panel"] = "off"
    
    ggsave(area_cons, 
           filename = here::here('figures', 'world', 'coal-consumption-by-country_annual_1980-2018_ats_absolute.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'world', 'coal-consumption-by-country_annual_1980-2018_ats_absolute.pdf'),
                outfile = here::here('figures', 'world', 'coal-consumption-by-country_annual_1980-2018_ats_absolute.pdf'))
    
  # area, consumption (proportion) -----
    
    labs_area_cons_prop = coal_agg[type == 'consumption' & year == max(year)][order(factor(label, levels = rev(c('All Other Countries', rev(top5_cons)))))]
    labs_area_cons_prop[, cum_sum := cumsum(prop)] 
    labs_area_cons_prop[, difference := diff(c(0,cum_sum))/2]
    labs_area_cons_prop[, position := cum_sum - difference]
    labs_area_cons_prop[4:5, position := c(0.728,0.769)]
    
    area_cons_prop = ggplot(coal_agg[type == 'consumption'], 
                            aes(x = year, y = prop, fill = factor(label, levels = c('All Other Countries', rev(top5_cons))))) + 
      geom_area() +
      labs(title = 'Annual coal consumption by country (1980-2018)',
           subtitle = 'Share of global coal consumption', 
           caption = 'Top 5 coal consuming countries in 2018 shown individually. All other countries aggregated. Data: U.S. Energy Information Administration', 
           x = NULL,
           y = NULL) +
      scale_x_continuous(breaks = seq(1980,2018,5), limits = c(1980, 2018), expand = c(0,0)) +
      scale_y_continuous(labels = scales::percent, expand = c(0,0)) +
      guides(fill = 'none',
             color = 'none') +
      scale_color_manual(values = pal_cons) + 
      scale_fill_manual(values = pal_cons) + 
      theme_area_labeled +
      geom_text(data = labs_area_cons_prop, aes(x = Inf, y = position, label = paste0(' ', label), color = label), 
                hjust = 0, size = 6.5, fontface = 'plain', family = 'Secca Soft')
    
    area_cons_prop = ggplotGrob(area_cons_prop)
    area_cons_prop$layout$clip[area_cons_prop$layout$name == "panel"] = "off"
    
    ggsave(area_cons_prop, 
           filename = here::here('figures', 'world', 'coal-consumption-by-country_annual_1980-2018_ats_proportion.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'world', 'coal-consumption-by-country_annual_1980-2018_ats_proportion.pdf'),
                outfile = here::here('figures', 'world', 'coal-consumption-by-country_annual_1980-2018_ats_proportion.pdf'))


    
  # line, imports -----
    
    line_imp = ggplot(coal_agg[type == 'imports'], aes(x = year, y = value/1e3, color = label)) + 
      geom_line(size = 0.9) +
      labs(title = 'Annual coal imports by country (1980-2018)',
           subtitle = 'Million short tons', 
           caption = 'Top 5 coal importing countries in 2018 shown individually. All other countries aggregated. Data: U.S. Energy Information Administration', 
           x = NULL,
           y = NULL) +
      guides(color = 'none') +
      scale_x_continuous(breaks = seq(1980,2018,5), limits = c(1980, 2018), expand = c(0,0)) +
      scale_y_continuous(labels = scales::comma, expand = c(0,0), breaks = seq(0,500,100), limits = c(0,500)) +
      scale_color_manual(values = pal_imp) + 
      theme_line +
      geom_dl(aes(label = label), method = list(dl.trans(x = x + .3), 'last.bumpup', cex = 1.5, fontfamily = 'Secca Soft', fontface = 'plain'))
    
    line_imp = ggplotGrob(line_imp)
    line_imp$layout$clip[line_imp$layout$name == "panel"] = "off"
    
    ggsave(line_imp, 
           filename = here::here('figures', 'world', 'coal-imports-by-country_annual_1980-2018_lts.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'world', 'coal-imports-by-country_annual_1980-2018_lts.pdf'),
                outfile = here::here('figures', 'world', 'coal-imports-by-country_annual_1980-2018_lts.pdf'))
    
  # area, imports (absolute) -----
    
    labs_area_imp = coal_agg[type == 'imports' & year == max(year)][order(factor(label, levels = rev(c('All Other Countries', rev(top5_imp)))))]
    labs_area_imp[, cum_sum := cumsum(value/1e3)] 
    labs_area_imp[, difference := diff(c(0,cum_sum))/2]
    labs_area_imp[, position := cum_sum - difference]
    
    area_imp = ggplot(coal_agg[type == 'imports'], 
                      aes(x = year, y = value/1e3, fill = factor(label, levels = c('All Other Countries', rev(top5_imp))))) + 
      geom_area() +
      labs(title = 'Annual coal imports by country (1980-2018)',
           subtitle = 'Million short tons', 
           caption = 'Top 5 coal importing countries in 2018 shown individually. All other countries aggregated. Data: U.S. Energy Information Administration', 
           x = NULL,
           y = NULL) +
      scale_x_continuous(breaks = seq(1980,2018,5), limits = c(1980, 2018), expand = c(0,0)) +
      scale_y_continuous(labels = scales::comma, expand = c(0,0), breaks = seq(0,1600,200)) +
      guides(fill = 'none',
             color = 'none') +
      scale_color_manual(values = pal_imp) + 
      scale_fill_manual(values = pal_imp) + 
      theme_area_labeled +
      geom_text(data = labs_area_imp, aes(x = Inf, y = position, label = paste0(' ', label), color = label), 
                hjust = 0, size = 6.5, fontface = 'plain', family = 'Secca Soft')
    
    area_imp = ggplotGrob(area_imp)
    area_imp$layout$clip[area_imp$layout$name == "panel"] = "off"
    
    ggsave(area_imp, 
           filename = here::here('figures', 'world', 'coal-imports-by-country_annual_1980-2018_ats_absolute.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'world', 'coal-imports-by-country_annual_1980-2018_ats_absolute.pdf'),
                outfile = here::here('figures', 'world', 'coal-imports-by-country_annual_1980-2018_ats_absolute.pdf'))
    
    
  # area, imports (proportion) -----
    
    labs_area_imp_prop = coal_agg[type == 'imports' & year == max(year)][order(factor(label, levels = rev(c('All Other Countries', rev(top5_imp)))))]
    labs_area_imp_prop[, cum_sum := cumsum(prop)] 
    labs_area_imp_prop[, difference := diff(c(0,cum_sum))/2]
    labs_area_imp_prop[, position := cum_sum - difference]
    
    area_imp_prop = ggplot(coal_agg[type == 'imports'], 
                           aes(x = year, y = prop, fill = factor(label, levels = c('All Other Countries', rev(top5_imp))))) + 
      geom_area() +
      labs(title = 'Annual coal imports by country (1980-2018)',
           subtitle = 'Share of global coal imports', 
           caption = 'Top 5 coal importing countries in 2018 shown individually. All other countries aggregated. Data: U.S. Energy Information Administration', 
           x = NULL,
           y = NULL) +
      scale_x_continuous(breaks = seq(1980,2018,5), limits = c(1980, 2018), expand = c(0,0)) +
      scale_y_continuous(labels = scales::percent, expand = c(0,0)) +
      guides(fill = 'none',
             color = 'none') +
      scale_color_manual(values = pal_imp) + 
      scale_fill_manual(values = pal_imp) + 
      theme_area_labeled +
      geom_text(data = labs_area_imp_prop, aes(x = Inf, y = position, label = paste0(' ', label), color = label), 
                hjust = 0, size = 6.5, fontface = 'plain', family = 'Secca Soft')
    
    area_imp_prop = ggplotGrob(area_imp_prop)
    area_imp_prop$layout$clip[area_imp_prop$layout$name == "panel"] = "off"
    
    ggsave(area_imp_prop, 
           filename = here::here('figures', 'world', 'coal-imports-by-country_annual_1980-2018_ats_proportion.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'world', 'coal-imports-by-country_annual_1980-2018_ats_proportion.pdf'),
                outfile = here::here('figures', 'world', 'coal-imports-by-country_annual_1980-2018_ats_proportion.pdf'))
    
    

  # line, exports -----
    
    line_exp = ggplot(coal_agg[type == 'exports'], aes(x = year, y = value/1e3, color = label)) + 
      geom_line(size = 0.9) +
      labs(title = 'Annual coal exports by country (1980-2018)',
           subtitle = 'Million short tons', 
           caption = 'Top 5 coal exporting countries in 2018 shown individually. All other countries aggregated. Data: U.S. Energy Information Administration', 
           x = NULL,
           y = NULL) +
      guides(color = 'none') +
      scale_x_continuous(breaks = seq(1980,2018,5), limits = c(1980, 2018), expand = c(0,0)) +
      scale_y_continuous(labels = scales::comma, expand = c(0,0), breaks = seq(0,500,100), limits = c(0,500)) +
      scale_color_manual(values = pal_exp) + 
      theme_line +
      geom_dl(aes(label = label), method = list(dl.trans(x = x + .3), 'last.bumpup', cex = 1.5, fontfamily = 'Secca Soft', fontface = 'plain'))
    
    line_exp = ggplotGrob(line_exp)
    line_exp$layout$clip[line_imp$layout$name == "panel"] = "off"
    
    ggsave(line_exp, 
           filename = here::here('figures', 'world', 'coal-exports-by-country_annual_1980-2018_lts.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'world', 'coal-exports-by-country_annual_1980-2018_lts.pdf'),
                outfile = here::here('figures', 'world', 'coal-exports-by-country_annual_1980-2018_lts.pdf'))
    
  # area, exports (absolute) -----
    
    labs_area_exp = coal_agg[type == 'exports' & year == max(year)][order(factor(label, levels = rev(c('All Other Countries', rev(top5_exp)))))]
    labs_area_exp[, cum_sum := cumsum(value/1e3)] 
    labs_area_exp[, difference := diff(c(0,cum_sum))/2]
    labs_area_exp[, position := cum_sum - difference]
    
    area_exp = ggplot(coal_agg[type == 'exports'], 
                      aes(x = year, y = value/1e3, fill = factor(label, levels = c('All Other Countries', rev(top5_exp))))) + 
      geom_area() +
      labs(title = 'Annual coal exports by country (1980-2018)',
           subtitle = 'Million short tons', 
           caption = 'Top 5 coal exporting countries in 2018 shown individually. All other countries aggregated. Data: U.S. Energy Information Administration', 
           x = NULL,
           y = NULL) +
      scale_x_continuous(breaks = seq(1980,2018,5), limits = c(1980, 2018), expand = c(0,0)) +
      scale_y_continuous(labels = scales::comma, expand = c(0,0), breaks = seq(0,1600,200), limits = c(0,1600)) +
      guides(fill = 'none',
             color = 'none') +
      scale_color_manual(values = pal_exp) + 
      scale_fill_manual(values = pal_exp) + 
      theme_area_labeled +
      geom_text(data = labs_area_exp, aes(x = Inf, y = position, label = paste0(' ', label), color = label), 
                hjust = 0, size = 6.5, fontface = 'plain', family = 'Secca Soft')
    
    area_exp = ggplotGrob(area_exp)
    area_exp$layout$clip[area_exp$layout$name == "panel"] = "off"
    
    ggsave(area_exp, 
           filename = here::here('figures', 'world', 'coal-exports-by-country_annual_1980-2018_ats_absolute.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'world', 'coal-exports-by-country_annual_1980-2018_ats_absolute.pdf'),
                outfile = here::here('figures', 'world', 'coal-exports-by-country_annual_1980-2018_ats_absolute.pdf'))
    
    
  # area, exports (proportion) -----
    
    labs_area_exp_prop = coal_agg[type == 'exports' & year == max(year)][order(factor(label, levels = rev(c('All Other Countries', rev(top5_exp)))))]
    labs_area_exp_prop[, cum_sum := cumsum(prop)] 
    labs_area_exp_prop[, difference := diff(c(0,cum_sum))/2]
    labs_area_exp_prop[, position := cum_sum - difference]
    
    area_exp_prop = ggplot(coal_agg[type == 'exports'], 
                           aes(x = year, y = prop, fill = factor(label, levels = c('All Other Countries', rev(top5_exp))))) + 
      geom_area() +
      labs(title = 'Annual coal exports by country (1980-2018)',
           subtitle = 'Share of global coal exports', 
           caption = 'Top 5 coal exporting countries in 2018 shown individually. All other countries aggregated. Data: U.S. Energy Information Administration', 
           x = NULL,
           y = NULL) +
      scale_x_continuous(breaks = seq(1980,2018,5), limits = c(1980, 2018), expand = c(0,0)) +
      scale_y_continuous(labels = scales::percent, expand = c(0,0)) +
      guides(fill = 'none',
             color = 'none') +
      scale_color_manual(values = pal_exp) + 
      scale_fill_manual(values = pal_exp) + 
      theme_area_labeled +
      geom_text(data = labs_area_exp_prop, aes(x = Inf, y = position, label = paste0(' ', label), color = label), 
                hjust = 0, size = 6.5, fontface = 'plain', family = 'Secca Soft')
    
    area_exp_prop = ggplotGrob(area_exp_prop)
    area_exp_prop$layout$clip[area_exp_prop$layout$name == "panel"] = "off"
    
    ggsave(area_exp_prop, 
           filename = here::here('figures', 'world', 'coal-exports-by-country_annual_1980-2018_ats_proportion.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'world', 'coal-exports-by-country_annual_1980-2018_ats_proportion.pdf'),
                outfile = here::here('figures', 'world', 'coal-exports-by-country_annual_1980-2018_ats_proportion.pdf'))
    
    
    