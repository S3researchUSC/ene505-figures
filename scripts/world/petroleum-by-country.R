# ------------------------------------ inputs ------------------------------------

  oil_file       = 'international-petroleum-crude-by-country-1980-2019.csv'

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
    oil_data = fread(here::here('data', oil_file), skip = 1, header = T)
  
  # rename V2 column
    setnames(oil_data, 'V2', 'country')
  
  # remove whitespace from country names
    oil_data[, country := trimws(country, which = 'both')]

# melt data from wide to long format -----

  oil_long = melt(oil_data, measure.vars = as.character(1973:2020), variable.name = 'year', value.name = 'value')

# convert year and values to numeric type -----

  oil_long[, year := as.numeric(as.character(year))]
  oil_long[, value := as.numeric(value)]
  
# remove crude production api -----
  
  oil_long = oil_long[! API %like% 'INTL.55-1']
  
# categorize values by type using API -----
  
  oil_long[API %like% 'INTL.53-1', type := 'production']
  oil_long[API %like% 'INTL.5-2', type := 'consumption']
  oil_long[API %like% 'INTL.57-3', type := 'imports']
  oil_long[API %like% 'INTL.57-4', type := 'exports']
  oil_long[API %like% 'INTL.57-6', type := 'reserves']
  
# add units -----
  
  oil_long[, unit := ifelse(type == 'reserves', 'billion barrels', 'thousand barrels per day')]
  
# remove rows that are just labels from the raw data ----
  
  oil_long = oil_long[!is.na(type)]
  
# rank values in each category (production, consumption, imports, exports, and reserves) in 2018 ------
  
  oil_max_year = oil_long[!is.na(value) & ! country == 'World']
  oil_max_year[, max_year := max(year), by = 'type']
  oil_max_year = oil_max_year[year == max_year]
  oil_max_year[, rank := frank(-value), by = 'type']
  rank_country = unique(oil_max_year[, c('country', 'type', 'rank')])
  
# merge with main coal data ----
  
  oil_ranked = merge(oil_long, rank_country, by = c('country', 'type'))
  
# label all non-top 5 countries as "Other" -----
  
  oil_ranked[, label := ifelse(rank %in% 1:5, country, 'All Other Countries')]
  
# aggregate by new country grouping -----
  
  oil_agg = oil_ranked[, .(value = sum(value, na.rm = T)), by = .(year, label, type, unit)]
  setcolorder(oil_agg, c('year', 'type', 'label', 'value', 'unit'))
  
# calculate proportion (or percentage) contributed by type for each year -----
  
  oil_agg[, prop := value/sum(value), by = c('type', 'year')]
  
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
  
    labs_line_prod = oil_agg[type == 'production' & year == 2019]
    setorder(labs_line_prod, 'value')
    labs_line_prod[, position := value/1e3]
    labs_line_prod[1:4, position := c(3.9,6,10.6,12.4)]

    line_prod = ggplot(oil_agg[type == 'production'], aes(x = year, y = value/1e3, color = label)) + 
      geom_line(size = 0.9) +
      labs(title = 'Annual petroleum production by country (1980-2019)',
           subtitle = 'Million barrels per day', 
           caption = 'Top 5 petroleum producing countries in 2019 shown individually. All other countries aggregated. Data: U.S. Energy Information Administration', 
           x = NULL,
           y = NULL) +
      guides(color = 'none') +
      scale_x_continuous(breaks = seq(1980,2019,5), limits = c(1980,2019), expand = c(0,0)) +
      scale_y_continuous(labels = scales::comma, expand = c(0,0), breaks = seq(0,50,10), limits = c(0,50)) +
      scale_color_manual(values = pal_prod) + 
      theme_line +
      geom_text(data = labs_line_prod, aes(x = Inf, y = position, label = paste0(' ', label), color = label), hjust = 0,
                size = 6.5, fontface = 'plain', family = 'Secca Soft')
    
    line_prod = ggplotGrob(line_prod)
    line_prod$layout$clip[line_prod$layout$name == "panel"] = "off"
    
    ggsave(line_prod, 
           filename = here::here('figures', 'world', 'petroleum-production-by-country_annual_1980-2019_lts.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'world', 'petroleum-production-by-country_annual_1980-2019_lts.pdf'),
                outfile = here::here('figures', 'world', 'petroleum-production-by-country_annual_1980-2019_lts.pdf'))
  
  # area, production (absolute) -----
  
    labs_area_prod = oil_agg[type == 'production' & year == 2019][order(factor(label, levels = rev(c('All Other Countries', rev(top5_prod)))))]
    labs_area_prod[, cum_sum := cumsum(value/1e3)] 
    labs_area_prod[, difference := diff(c(0,cum_sum))/2]
    labs_area_prod[, position := cum_sum - difference]
    
    area_prod = ggplot(oil_agg[type == 'production'], 
                      aes(x = year, y = value/1e3, fill = factor(label, levels = c('All Other Countries', rev(top5_prod))))) + 
      geom_area() +
      labs(title = 'Annual petroleum production by country (1980-2019)',
           subtitle = 'Million barrels per day', 
           caption = 'Top 5 petroleum producing countries in 2019 shown individually. All other countries aggregated. Data: U.S. Energy Information Administration', 
           x = NULL,
           y = NULL) +
      scale_x_continuous(breaks = seq(1980,2019,5), limits = c(1980,2019), expand = c(0,0)) +
      scale_y_continuous(labels = scales::comma, expand = c(0,0), breaks = seq(0,110,10), limits = c(0,110)) +
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
           filename = here::here('figures', 'world', 'petroleum-production-by-country_annual_1980-2019_ats_absolute.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'world', 'petroleum-production-by-country_annual_1980-2019_ats_absolute.pdf'),
                outfile = here::here('figures', 'world', 'petroleum-production-by-country_annual_1980-2019_ats_absolute.pdf'))
  
  
  # area, production (proportion) -----
  
    labs_area_prod_prop = oil_agg[type == 'production' & year == 2019][order(factor(label, levels = rev(c('All Other Countries', rev(top5_prod)))))]
    labs_area_prod_prop[, cum_sum := cumsum(prop)] 
    labs_area_prod_prop[, difference := diff(c(0,cum_sum))/2]
    labs_area_prod_prop[, position := cum_sum - difference]
    
    area_prod_prop = ggplot(oil_agg[type == 'production'], 
                           aes(x = year, y = prop, fill = factor(label, levels = c('All Other Countries', rev(top5_prod))))) + 
      geom_area() +
      labs(title = 'Annual petroleum production by country (1980-2019)',
           subtitle = 'Share of global petroleum production', 
           caption = 'Top 5 petroleum producing countries in 2019 shown individually. All other countries aggregated. Data: U.S. Energy Information Administration', 
           x = NULL,
           y = NULL) +
      scale_x_continuous(breaks = seq(1980,2019,5), limits = c(1980,2019), expand = c(0,0)) +
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
           filename = here::here('figures', 'world', 'petroleum-production-by-country_annual_1980-2019_ats_proportion.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'world', 'petroleum-production-by-country_annual_1980-2019_ats_proportion.pdf'),
                outfile = here::here('figures', 'world', 'petroleum-production-by-country_annual_1980-2019_ats_proportion.pdf'))
  

  # line, consumption -----
    
    labs_line_cons = oil_agg[type == 'consumption' & year == 2019]
    setorder(labs_line_cons, 'value')
    labs_line_cons[, position := value/1e3]
    labs_line_cons[1:4, position := c(1.1,2.1,3.15,4.25)]
    
    line_cons = ggplot(oil_agg[type == 'consumption'], aes(x = year, y = value/1e3, color = label)) + 
      geom_line(size = 0.9) +
      labs(title = 'Annual petroleum consumption by country (1973-2019)',
           subtitle = 'Million barrels per day', 
           caption = 'Top 5 petroleum consuming countries in 2019 shown individually. All other countries aggregated. Data: U.S. Energy Information Administration', 
           x = NULL,
           y = NULL) +
      guides(color = 'none') +
      scale_x_continuous(breaks = seq(1973,2019,5), limits = c(1973,2019), expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0), breaks = seq(0,24,4), limits = c(0,24)) +
      scale_color_manual(values = pal_cons) + 
      theme_line +
      geom_text(data = labs_line_cons, aes(x = Inf, y = position, label = paste0(' ', label), color = label), hjust = 0,
                size = 6.5, fontface = 'plain', family = 'Secca Soft')
    
    line_cons = ggplotGrob(line_cons)
    line_cons$layout$clip[line_cons$layout$name == "panel"] = "off"
    
    ggsave(line_cons, 
           filename = here::here('figures', 'world', 'petroleum-consumption-by-country_annual_1973-2019_lts.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'world', 'petroleum-consumption-by-country_annual_1973-2019_lts.pdf'),
                outfile = here::here('figures', 'world', 'petroleum-consumption-by-country_annual_1973-2019_lts.pdf'))
    
  # area, consumption (absolute) -----
    
    labs_area_cons = oil_agg[type == 'consumption' & year == 2019][order(factor(label, levels = rev(c('All Other Countries', rev(top5_cons)))))]
    labs_area_cons[, cum_sum := cumsum(value/1e3)] 
    labs_area_cons[, difference := diff(c(0,cum_sum))/2]
    labs_area_cons[, position := cum_sum - difference]
    
    area_cons = ggplot(oil_agg[type == 'consumption'], 
                       aes(x = year, y = value/1e3, fill = factor(label, levels = c('All Other Countries', rev(top5_cons))))) + 
      geom_area() +
      labs(title = 'Annual petroleum consumption by country (1973-2019)',
           subtitle = 'Million barrels per day', 
           caption = 'Top 5 petroleum consuming countries in 2019 shown individually. All other countries aggregated. Data: U.S. Energy Information Administration', 
           x = NULL,
           y = NULL) +
      scale_x_continuous(breaks = seq(1973,2019,5), limits = c(1973,2019), expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0), breaks = seq(0,55,5)) +
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
           filename = here::here('figures', 'world', 'petroleum-consumption-by-country_annual_1973-2019_ats_absolute.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'world', 'petroleum-consumption-by-country_annual_1973-2019_ats_absolute.pdf'),
                outfile = here::here('figures', 'world', 'petroleum-consumption-by-country_annual_1973-2019_ats_absolute.pdf'))
    
    
  # area, consumption (proportion) -----
    
    labs_area_cons_prop = oil_agg[type == 'consumption' & year == 2019][order(factor(label, levels = rev(c('All Other Countries', rev(top5_cons)))))]
    labs_area_cons_prop[, cum_sum := cumsum(prop)] 
    labs_area_cons_prop[, difference := diff(c(0,cum_sum))/2]
    labs_area_cons_prop[, position := cum_sum - difference]
    
    area_cons_prop = ggplot(oil_agg[type == 'consumption'], 
                            aes(x = year, y = prop, fill = factor(label, levels = c('All Other Countries', rev(top5_cons))))) + 
      geom_area() +
      labs(title = 'Annual petroleum consumption by country (1973-2019)',
           subtitle = 'Share of global petroleum consumption', 
           caption = 'Top 5 petroleum consuming countries in 2019 shown individually. All other countries aggregated. Data: U.S. Energy Information Administration', 
           x = NULL,
           y = NULL) +
      scale_x_continuous(breaks = seq(1973,2019,5), limits = c(1973,2019), expand = c(0,0)) +
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
           filename = here::here('figures', 'world', 'petroleum-consumption-by-country_annual_1973-2019_ats_proportion.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'world', 'petroleum-consumption-by-country_annual_1973-2019_ats_proportion.pdf'),
                outfile = here::here('figures', 'world', 'petroleum-consumption-by-country_annual_1973-2019_ats_proportion.pdf'))

  # line, imports -----
    
    labs_line_imp = oil_agg[type == 'imports' & year == 2016]
    setorder(labs_line_imp, 'value')
    labs_line_imp[, position := value/1e3]
    labs_line_imp[1:5, position := c(2.35,3.55,4.65,7.1,8.15)]
    
    line_imp = ggplot(oil_agg[type == 'imports'], aes(x = year, y = value/1e3, color = label)) + 
      geom_line(size = 0.9) +
      labs(title = 'Annual petroleum imports by country (1980-2016)',
           subtitle = 'Million barrels per day', 
           caption = 'Top 5 petroleum importing countries in 2016 shown individually. All other countries aggregated. Data: U.S. Energy Information Administration', 
           x = NULL,
           y = NULL) +
      guides(color = 'none') +
      scale_x_continuous(breaks = seq(1980,2016,5), limits = c(1980,2016), expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0), breaks = seq(0,24,4), limits = c(0,24)) +
      scale_color_manual(values = pal_imp) + 
      theme_line +
      geom_text(data = labs_line_imp, aes(x = Inf, y = position, label = paste0(' ', label), color = label), hjust = 0,
                size = 6.5, fontface = 'plain', family = 'Secca Soft')
    
    line_imp = ggplotGrob(line_imp)
    line_imp$layout$clip[line_imp$layout$name == "panel"] = "off"
    
    ggsave(line_imp, 
           filename = here::here('figures', 'world', 'petroleum-imports-by-country_annual_1980-2016_lts.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'world', 'petroleum-imports-by-country_annual_1980-2016_lts.pdf'),
                outfile = here::here('figures', 'world', 'petroleum-imports-by-country_annual_1980-2016_lts.pdf'))
    
  # area, imports (absolute) -----
    
    labs_area_imp = oil_agg[type == 'imports' & year == 2016][order(factor(label, levels = rev(c('All Other Countries', rev(top5_imp)))))]
    labs_area_imp[, cum_sum := cumsum(value/1e3)] 
    labs_area_imp[, difference := diff(c(0,cum_sum))/2]
    labs_area_imp[, position := cum_sum - difference]
    
    area_imp = ggplot(oil_agg[type == 'imports'], 
                       aes(x = year, y = value/1e3, fill = factor(label, levels = c('All Other Countries', rev(top5_imp))))) + 
      geom_area() +
      labs(title = 'Annual petroleum imports by country (1980-2016)',
           subtitle = 'Million barrels per day', 
           caption = 'Top 5 petroleum importing countries in 2016 shown individually. All other countries aggregated. Data: U.S. Energy Information Administration', 
           x = NULL,
           y = NULL) +
      scale_x_continuous(breaks = seq(1980,2016,5), limits = c(1980,2016), expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0), breaks = seq(0,50,5)) +
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
           filename = here::here('figures', 'world', 'petroleum-imports-by-country_annual_1980-2016_ats_absolute.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'world', 'petroleum-imports-by-country_annual_1980-2016_ats_absolute.pdf'),
                outfile = here::here('figures', 'world', 'petroleum-imports-by-country_annual_1980-2016_ats_absolute.pdf'))
    
    
  # area, imports (proportion) -----
    
    labs_area_imp_prop = oil_agg[type == 'imports' & year == 2016][order(factor(label, levels = rev(c('All Other Countries', rev(top5_imp)))))]
    labs_area_imp_prop[, cum_sum := cumsum(prop)] 
    labs_area_imp_prop[, difference := diff(c(0,cum_sum))/2]
    labs_area_imp_prop[, position := cum_sum - difference]
    
    area_imp_prop = ggplot(oil_agg[type == 'imports'], 
                            aes(x = year, y = prop, fill = factor(label, levels = c('All Other Countries', rev(top5_imp))))) + 
      geom_area() +
      labs(title = 'Annual petroleum imports by country (1980-2016)',
           subtitle = 'Share of global petroleum imports', 
           caption = 'Top 5 petroleum importing countries in 2016 shown individually. All other countries aggregated. Data: U.S. Energy Information Administration', 
           x = NULL,
           y = NULL) +
      scale_x_continuous(breaks = seq(1980,2016,5), limits = c(1980,2016), expand = c(0,0)) +
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
           filename = here::here('figures', 'world', 'petroleum-imports-by-country_annual_1980-2016_ats_proportion.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'world', 'petroleum-imports-by-country_annual_1980-2016_ats_proportion.pdf'),
                outfile = here::here('figures', 'world', 'petroleum-imports-by-country_annual_1980-2016_ats_proportion.pdf'))
    
  # line, exports -----
  
    labs_line_exp = oil_agg[type == 'exports' & year == 2016]
    setorder(labs_line_exp, 'value')
    labs_line_exp[, position := value/1e3]
    labs_line_exp[1:4, position := c(1.81,3.02,4.25,5.45)]
    
    line_exp = ggplot(oil_agg[type == 'exports'], aes(x = year, y = value/1e3, color = label)) + 
      geom_line(size = 0.9) +
      labs(title = 'Annual petroleum exports by country (1980-2016)',
           subtitle = 'Million barrels per day', 
           caption = 'Top 5 petroleum exporting countries in 2016 shown individually. All other countries aggregated. Data: U.S. Energy Information Administration', 
           x = NULL,
           y = NULL) +
      guides(color = 'none') +
      scale_x_continuous(breaks = seq(1980,2016,5), limits = c(1980,2016), expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0), breaks = seq(0,28,4), limits = c(0,28)) +
      scale_color_manual(values = pal_exp) + 
      theme_line +
      geom_text(data = labs_line_exp, aes(x = Inf, y = position, label = paste0(' ', label), color = label), hjust = 0,
                size = 6.0, fontface = 'plain', family = 'Secca Soft')
    
    line_exp = ggplotGrob(line_exp)
    line_exp$layout$clip[line_exp$layout$name == "panel"] = "off"
    
    ggsave(line_exp, 
           filename = here::here('figures', 'world', 'petroleum-exports-by-country_annual_1980-2016_lts.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'world', 'petroleum-exports-by-country_annual_1980-2016_lts.pdf'),
                outfile = here::here('figures', 'world', 'petroleum-exports-by-country_annual_1980-2016_lts.pdf'))
    
  # area, exports (absolute) -----
    
    labs_area_exp = oil_agg[type == 'exports' & year == 2016][order(factor(label, levels = rev(c('All Other Countries', rev(top5_exp)))))]
    labs_area_exp[, cum_sum := cumsum(value/1e3)] 
    labs_area_exp[, difference := diff(c(0,cum_sum))/2]
    labs_area_exp[, position := cum_sum - difference]
    
    area_exp = ggplot(oil_agg[type == 'exports'], 
                      aes(x = year, y = value/1e3, fill = factor(label, levels = c('All Other Countries', rev(top5_exp))))) + 
      geom_area() +
      labs(title = 'Annual petroleum exports by country (1980-2016)',
           subtitle = 'Million barrels per day', 
           caption = 'Top 5 petroleum exporting countries in 2016 shown individually. All other countries aggregated. Data: U.S. Energy Information Administration', 
           x = NULL,
           y = NULL) +
      scale_x_continuous(breaks = seq(1980,2016,5), limits = c(1980,2016), expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0), breaks = seq(0,50,5), limits = c(0,45)) +
      guides(fill = 'none',
             color = 'none') +
      scale_color_manual(values = pal_exp) + 
      scale_fill_manual(values = pal_exp) + 
      theme_area_labeled +
      geom_text(data = labs_area_exp, aes(x = Inf, y = position, label = paste0(' ', label), color = label), 
                hjust = 0, size = 6.0, fontface = 'plain', family = 'Secca Soft')
    
    area_exp = ggplotGrob(area_exp)
    area_exp$layout$clip[area_exp$layout$name == "panel"] = "off"
    
    ggsave(area_exp, 
           filename = here::here('figures', 'world', 'petroleum-exports-by-country_annual_1980-2016_ats_absolute.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'world', 'petroleum-exports-by-country_annual_1980-2016_ats_absolute.pdf'),
                outfile = here::here('figures', 'world', 'petroleum-exports-by-country_annual_1980-2016_ats_absolute.pdf'))
    
    
  # area, exports (proportion) -----
    
    labs_area_exp_prop = oil_agg[type == 'exports' & year == 2016][order(factor(label, levels = rev(c('All Other Countries', rev(top5_exp)))))]
    labs_area_exp_prop[, cum_sum := cumsum(prop)] 
    labs_area_exp_prop[, difference := diff(c(0,cum_sum))/2]
    labs_area_exp_prop[, position := cum_sum - difference]
    
    area_exp_prop = ggplot(oil_agg[type == 'exports'], 
                           aes(x = year, y = prop, fill = factor(label, levels = c('All Other Countries', rev(top5_exp))))) + 
      geom_area() +
      labs(title = 'Annual petroleum exports by country (1980-2016)',
           subtitle = 'Share of global petroleum exports', 
           caption = 'Top 5 petroleum exporting countries in 2016 shown individually. All other countries aggregated. Data: U.S. Energy Information Administration', 
           x = NULL,
           y = NULL) +
      scale_x_continuous(breaks = seq(1980,2016,5), limits = c(1980,2016), expand = c(0,0)) +
      scale_y_continuous(labels = scales::percent, expand = c(0,0)) +
      guides(fill = 'none',
             color = 'none') +
      scale_color_manual(values = pal_exp) + 
      scale_fill_manual(values = pal_exp) + 
      theme_area_labeled +
      geom_text(data = labs_area_exp_prop, aes(x = Inf, y = position, label = paste0(' ', label), color = label), 
                hjust = 0, size = 6.0, fontface = 'plain', family = 'Secca Soft')
    
    area_exp_prop = ggplotGrob(area_exp_prop)
    area_exp_prop$layout$clip[area_exp_prop$layout$name == "panel"] = "off"
    
    ggsave(area_exp_prop, 
           filename = here::here('figures', 'world', 'petroleum-exports-by-country_annual_1980-2016_ats_proportion.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'world', 'petroleum-exports-by-country_annual_1980-2016_ats_proportion.pdf'),
                outfile = here::here('figures', 'world', 'petroleum-exports-by-country_annual_1980-2016_ats_proportion.pdf'))
    