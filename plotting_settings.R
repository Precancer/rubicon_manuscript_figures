# PLOT SETTINGS FOR BARPLOTS
library(ggpattern)
# For two histologies 
pdfWidth = 5
pdfHeight = 3

# All samples, no splitting by histology 
pdfWidth = 2.5
pdfHeight = 2.5


pdf(pdfOut, width=pdfWidth, height=pdfHeight)
g <- ggplot(subset(data, ! is.na(get(group))), aes(x, y))
plot = g + 
  geom_bar_pattern(aes(fill = x, pattern_colour = x, color = x,
                       pattern=as.factor(get(group)), alpha = as.factor(get(group))),
                   stat = "identity", position = 'fill', pattern_alpha=0.5,
                   pattern_orientation='radial', width = 0.8, color= 'black', pattern_size = 1.5) +
  scale_pattern_color_manual(values = c(colors), na.value = 'grey') +
  scale_pattern_manual(values = c('FALSE'='circle', 'TRUE'='none', 
                                  'yes'='none', 'no'='circle',
                                  'High tTIN OR sTIN'='darkg', 
                                  'Rest'='circle'), na.value = 'none') +
  scale_fill_manual(values= colors, na.value = 'darkgrey') + 
  bbplot::bbc_style() +
  geom_label(aes(x=x, y = pct, label= round(pct * 100, 2)), fill = 'white', 
             size = 5, alpha = 0.8, position = position_fill(vjust = 0.2, reverse = F)) +
  scale_alpha_manual(values = c('FALSE' = 0, 'TRUE' = 1))  +
  scale_color_manual(values=c(colors), na.value = 'darkgrey') +
  guides(fill = 'none', color = 'none', pattern_color='none', alpha = 'none', 
         pattern = guide_legend(title = axisX)) +
  scale_x_discrete(labels=format_tme_class) +
  theme(axis.title.x = element_text(size = 10), axis.text.x = element_blank()) +
  xlab(f("{axisX}, n={nrSamples}", ))
if(split_by_histo) {
  print(plot + facet_grid(. ~ eval(histology)))
} else {
  print(plot)
}
dev.off()

# PLOT SETTINGS FOR BOXPLOTS
# For two histologies 
pdfWidth = 5
pdfHeight = 3

# All samples, no splitting by histology 
pdfWidth = 2.5
pdfHeight = 2.5

pdf(pdfOut, width=pdfWidth, height=pdfHeight)
g <- ggplot(data, aes(x, y))
plot = g + 
  geom_boxplot(width = 0.8, color= 'grey10', outlier.size = 0) +
  geom_quasirandom(aes(color=get(group)),size=2, width=.4) +
  geom_quasirandom(color='grey10', size=2, width=.4, pch = 21) +
  scale_fill_manual(values= colors, na.value = 'darkgrey') + 
  bbplot::bbc_style() +
  theme(
    axis.text.y = element_text(size = 12, color = 'black'),
    axis.title.y = element_text(size = 12, color = 'black'),
    plot.title=element_text(size=12, color= 'black'),
    axis.line = element_line(color='black'),
    axis.ticks.y = element_line(size = 1),
    strip.text = element_text(size = 15)) +
  scale_alpha_manual(values = c('FALSE' = 0, 'TRUE'=1, "NA"=0))  +
  scale_color_manual(values=c(colors), na.value = 'darkgrey') +
  guides(fill = 'none', color = 'none', pattern_color='none', alpha = 'none') +
  scale_pattern_manual(values = c('FALSE'='circle', 'TRUE'='none'), na.value = 'none') +
  scale_pattern_color_manual(values = c(colors), na.value = 'none') +
  scale_x_discrete(labels=split_by_space) +
  ylab(axisY)
if(axisX == 'ImmuneCluster') {
  plot= plot + theme(axis.text.x = element_blank())
} else {
  plot= plot + theme(axis.text.x = element_text(color='black', size = 7))
}
if(split_by_histo) {
  print(plot + facet_wrap(. ~ eval(histology), scale = 'free'))
} else { 
  print(plot)
}
