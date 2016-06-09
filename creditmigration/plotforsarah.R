require(ggplot2)
raw<-data.table::fread('temp1.csv')
df<-raw[`Export value`>200 & !is.na(`Average distance`)]
df[,`Export value`:=`Export value`/1000]
df %>%   ggplot(aes(x=Concentration,y=`Average distance`,size=`Export value`,label=Country))+ylab("Avg. distance with destination countries (km)")+xlab('Concentration of exporting countries')+
  geom_point(colour='yellow')+geom_text(size=3)+ggplot2::scale_size_area(max_size = 30)+guides(size=guide_legend(title='Export Value $mm'))
ggsave('plot1.png')
ggsave('plot1.pdf')
ggsave('plot1.jpg')
