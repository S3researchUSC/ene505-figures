# Petroleum and NG_Rig Count and Productivity_2007-2015 #

# ---------------------------------------------------------------
# INPUT DATA ----------------------------------------------------
# ---------------------------------------------------------------

file.loc 	    = '/Users/MEAS/GitHub/ene505-figures' # location of scripts
data.loc      = '/Users/MEAS/Google Drive/TA Materials/ENE505 - Fall 2015/ENE 505 Charts' # location of data file(s)
data.file     = c('Petroleum and NG_Rig Count and Productivity_2007_2015.csv') # data file to be used
out.loc       = '/Users/MEAS/Google Drive/TA Materials/ENE505 - Fall 2015/ENE 505 Charts/20170827' # location of where to save figures
reg.cols      = c("Bakken" = "hotpink1",
                  "Eagle Ford" = "mediumpurple3",
                  "Haynesville" = "darkgoldenrod2", 
                  "Marcellus" = "brown3",
                  "Niobrara" = "springgreen4",
                  "Permian" = "steelblue3",
                  "Utica" = "hotpink4")

# ---------------------------------------------------------------
# MAIN SCRIPT ---------------------------------------------------
# ---------------------------------------------------------------

# load libraries -------
  library(data.table)
  library(ggplot2)
  library(hrbrthemes)
  library(lubridate)
  library(tidyr)
  library(plyr)


# import data ------
  setwd(data.loc)
  dt_raw = fread(data.file, header = T)[2:106, c(1:8,10:16, with = FALSE)]

# rename columns -----
  colnames(dt_raw) = c("Date",
                       "Bakken-Count",
                       "Eagle Ford-Count",
                       "Haynesville-Count",
                       "Marcellus-Count",
                       "Niobrara-Count",
                       "Permian-Count",
                       "Utica-Count",
                       "Bakken-Productivity",
                       "Eagle Ford-Productivity",
                       "Haynesville-Productivity",
                       "Marcellus-Productivity",
                       "Niobrara-Productivity",
                       "Permian-Productivity",
                       "Utica-Productivity")

# melt data from wide format to long -----
  dt_long = melt(dt_raw, measure.vars = list(2:15), variable.name = "region_type", value.name = "value")

# split shale region and type into separate columns -----
  dt_long[, c("region", "type") := tstrsplit(region_type, "-", type.convert = TRUE, fixed = TRUE)]
  # dt_long[ type == "NG", type := "Natural Gas"]

# convert month column into date format -----
  dt_long[, day := mdy(Date)]

# convert values to numeric -----
  dt_long[, value := as.numeric(gsub(",", "", value))]

# get list of types and regions -----
  type.list = levels(factor(dt_long[, type]))
  reg.list = levels(factor(dt_long[, region]))
  

# ---------------------------------------------------------------
# FIGURES -------------------------------------------------------
# ---------------------------------------------------------------
  
  setwd(out.loc)
  
  # DUAL AXIS LINE PLOT -------
  
  main = "2007 - 2014 Daily U.S. Oil and Natural Gas Rig Count and Productivity by Shale Region"
  sub = "Data: U.S. Energy Information Administration"
  xlab = ""
  ylab = list()
  for (i in 1:7)  {
    ylab[[i]] = "Total Rig Count"
    ylab[[i+7]] = "MMBTU per Rig"
  }
  ylab1 = "Total Rig Count"
  ylab2 = "MMBTU per Rig"
  xbreaks = c(seq(as.Date("2007-01-01"), as.Date("2015-01-01"), by = "2 years"))
  y1breaks = seq(0, 600, 100)
  y2breaks = seq(0, 300000, 50000)
  col = c("black", "black")

  # create lists of x and y data for each type and region
  xvals = list()
  yvals = list()
  
  k = 0
  for (i in 1:length(type.list)) {
    for (j in 1:length(reg.list)) {
      k = k + 1
      xvals[[k]] = dt_long[ type == type.list[i] & region == reg.list[j], day]
      yvals[[k]] = dt_long[ type == type.list[i] & region == reg.list[j], value]
    }
  }
  
 
  png(file="Petroleum and NG_Rig Count and Productivity_Single_2007-2014.png", width = 4400, height = 2500, res = 400)
  
  # set margins 
  mar.default <- c(3,6,3,6) + 0.1
  par(mar = mar.default + c(3, 0, 0, 2)) 
  
  # import roboto condensed fonts 
  quartzFonts(rcfont = c("Roboto Condensed Regular", "Roboto Condensed Bold", "Roboto Condensed Italic", "Roboto Condensed Light"))
  par(family = 'rcfont')
  
  # draw first series - with no axes
  plot(xvals[[1]], yvals[[1]], type = "l", axes = FALSE, lwd = 1.5,
       xlab = xlab, ylab = "", col = reg.cols[1], main = main, sub = sub, cex.lab=1.2, cex.axis=1.2, cex.main=1.4, cex.sub=1,
       xlim = range(xbreaks), ylim = c(min(y1breaks), max(y1breaks)),
       xaxs="i", yaxs="i")
    for (y in 2:7) {
        lines(xvals[[y]], yvals[[y]], type = "l", lwd = 1.5, col = reg.cols[[y]], xlab = xlab)
    }
  
  # add in the left hand vertical axis and its label
  axis(2, col = col[1], col.axis = col[1], las = 1, lwd = 0, at = y1breaks)  ## las=1 makes horizontal labels
  abline(h = y1breaks, col="gray60", lty=3)
  mtext(paste0(ylab1, "\n"), side = 2, col = "black", line = 1.5, cex=1.3)
  
  # Allow a second plot on the same graph
  par(new=TRUE)
  
  # Plot the second series:
  plot(xvals[[8]], yvals[[8]], type = "l", axes = FALSE, lwd = 1.5, lty = 2,
       xlab = xlab, ylab = "", col = reg.cols[1], main = main, sub = sub, cex.lab=1.2, cex.axis=1.2, cex.main=1.4, cex.sub=1,
       xlim = range(xbreaks), ylim = c(min(y2breaks), max(y2breaks)),
       xaxs="i", yaxs="i")
  for (z in 9:14) {
      lines(xvals[[z]], yvals[[z]], type = "l", lwd = 1.5, lty = 2, col = reg.cols[[z-7]], xlab = xlab)
    }
  
  ## add second vertical axis (on right) and its label
  mtext(paste0("\n", "\n", ylab2), side = 4, col = col[2], line = 4.5, cex=1.3)
  axis(4,  col = col[2], col.axis = col[2], las=1, lwd = 0, at = y2breaks)
  
  # Draw the horizontal time axis
  axis(1, at = xbreaks, labels = format(xbreaks, "%b %Y"), lwd.ticks = 0)
  
  # parameters for legend
  legx = "topleft"
  legy = NULL
  yleg1 = paste(gsub("\n$", "", ylab1))
  yleg2 = paste(ylab2)
  bty = "n"
  lwd = c(1, 2)
  
  # Add Legend
  legend(x = "top", y = 30, legend=c(yleg1, yleg2),
         text.col = col, lty = c(1, 2), lwd = 1.5, col = col,
         bty = bty)
  legend(x = "topleft", y = legy, legend = reg.list,
         text.col = reg.cols, lty = 1, lwd = 1.5, col = reg.cols,
         bty = bty)
  
  dev.off()
  
  # FACTED LINE PLOTS -------
  
  dummy <- data.table(day = range(dt_long[, day]), value = c(3, 600),
                      type = "Total Rig Count",
                      type_f = "Total Rig Count",
                      region = "Bakken", stringsAsFactors=FALSE)
  
  dt <- copy(dt_long)[ day >= min(xbreaks) & day <= max(xbreaks)]
  dt <- dt[ type == "Productivity", value := value/1000]
  dt <- dt[, type := revalue(type, c(Count = "Total Rig Count", Productivity = "Thousand MMBTU per Rig"))]
  dt <- dt[, type := factor(type, levels = rev(levels(factor(type))))]
  dt[, type_f := factor(type, levels=c('Total Rig Count', 'Thousand MMBTU per Rig'))]
  
  line_petro_ng_rigcount_product = ggplot(dt, aes(x = day, y = value, color = region, linetype = type)) + 
    geom_line(stat = "identity", size = 0.7) +
    geom_blank(data = dummy) +
    facet_grid(type_f ~ ., scales = "free_y", switch = "y") +
    labs(title = "2007 - 2014 Daily U.S. Oil and Natural Gas Rig Count and Productivity by Shale Region",
         subtitle = "Data: U.S. Energy Information Administration", 
         x = NULL,
         y = NULL,
         color = "Shale Region",
         linetype = "Source") +
    theme_ipsum_rc(grid = "Y") +
    scale_color_manual(values = reg.cols) +
    theme(plot.title = element_text(size = 20, hjust = 0.3, face = "bold"),
          plot.subtitle = element_text(size = 14, hjust = 0.5),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 12, face="bold"),
          axis.text.y = element_text(size = 12, face="bold"),
          legend.title = element_text(size = 13, face = "bold"),
          legend.text = element_text(size = 12),
          strip.placement = 'outside',
          strip.text.y = element_text(size = 14, face = "bold", hjust = 0.5, angle = 90)) + 
    scale_x_date(breaks = xbreaks, limits = range(xbreaks), date_labels = "%b %Y", expand = c(0,0))

  ggsave(line_petro_ng_rigcount_product, 
         filename = "Petroleum and NG_Rig Count and Productivity_Faceted_2007-2014.png", 
         width = 11.1, 
         height = 6.25, 
         dpi = 400)

