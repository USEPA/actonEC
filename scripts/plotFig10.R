### Plot Figre 10, showing the spatio-temporal lag between max sedT and max ebullition
### Set up data frame for plotting:

dailyMassFlux14_g<-gather(data=dailyMassFlux14, value=value, key=key, dailyEbCh4mgM2h, sedT)
dailyMassFlux12_g<-gather(data=dailyMassFlux12, value=value, key=key, dailyEbCh4mgM2h, sedT)

spatioTemporalList<-list()
spatioTemporalList[[1]]<-select(dailyMassFlux12_g, -sedTbuoy, -sondeTmpr, -TmprAdj, -sedTsonde)
spatioTemporalList[[2]]<-dailyMassFlux14_g

spatioTemporal<-do.call("rbind", spatioTemporalList)

spatioTemporal<-mutate(spatioTemporal,
       site=replace(site, site=="(c) Deep AFT", "(b)  Deep Site"),
       site=replace(site, site=="(b) Shallow AFT", "(a) Shallow Site"),
       key = replace(key, key=="dailyEbCh4mgM2h", "Ebullition"))

ggplot(filter(spatioTemporal, date>"2017-04-05", date<"2017-11-01"),
       aes(date, value))+
  annotate("rect", xmin=(as.Date("2017-08-02")),
           xmax=(as.Date("2017-08-16")),
           ymin=-Inf, ymax=Inf, alpha=0.2)+
       annotate("rect", xmin=(as.Date("2017-09-10")),
                xmax=(as.Date("2017-09-24")),
                ymin=-Inf, ymax=Inf, alpha=0.6)+
  geom_line(alpha=1, aes(color=as.factor(key)), size=0.5)+
  scale_color_manual(values=c("#333333", "#CC0033"))+
  geom_smooth(span=0.3, se=FALSE)+
  facet_grid(key~site, scales="free_y")+
  #scale_x_date(labels=date_format("%b", tz="UTC"), 
   #                breaks=date_breaks("1 month"))+
  xlab("")+
  ylab(expression(~~~~~~~~~~sedT~(deg~C)~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Ebullition~(mg~CH[4]~m^-2~hr^-1)))+
  theme_bw()+
  theme(legend.position="none")

spatioTemporal.s<-spread(spatioTemporal, key=key, value = value)

         
write_csv(select(spatioTemporal.s, -siteT, -sdEbCh4mgM2h, -year, -monthday),
          path=paste0(projectWD, "/dataL2/Fig10data.csv"))
