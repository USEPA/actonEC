############
## Use results from the GRTS spatial surveys to:
#### Plot CH4 ebullition as a function of water depth at the site (Figure 6)
#### Plot CH4 ebullition as a fraction of total FCH4 (Figure 7) 



## regression of ebullition as a f(water depth), Figure 6
## water depth ("wtrDpth") was recorded at the time of funnel/chamber deployment 
## and was loaded in as part of the eqAreaData dataframe

### Some of the depths are missing. We'll gap-fill them using the mean of the site water depths 
### measured at the other site visits

fig7<-select(eqAreaDataSub, siteID, wtrDpth, ch4.erate.mg.h)%>%
  mutate(wtrDpthFilled = wtrDpth)


#nice code from: https://stackoverflow.com/questions/56121600/fill-gaps-using-a-group-mean-in-r
fig7<-fig7%>%
dplyr::group_by(siteID) %>%
  mutate(
    fill = mean(wtrDpth, na.rm=TRUE),
    wtrDpthFilled = case_when(!is.na(wtrDpthFilled) ~ wtrDpthFilled,
                              TRUE ~ fill)
  )

ggplot(fig7, aes(wtrDpthFilled, ch4.erate.mg.h))+
  geom_point(alpha=0.3)+
  geom_smooth(method="lm")+
  labs(x="Water Depth (m)", 
       y =expression(CH[4]~Ebullition~(mg~m^-2~hr^-1)))+
  theme_bw()

ebDepth<-lm(ch4.erate.mg.h~wtrDpthFilled, data=fig7)
summary(ebDepth)

# non-gap filled:
ebDepth<-lm(ch4.erate.mg.h~wtrDpth, data=eqAreaDataSub)
summary(ebDepth)

ggplot(eqAreaDataSub, aes(wtrDpth, ch4.erate.mg.h))+
  geom_point(alpha=0.3)+
  geom_smooth(method="lm")+
  labs(x="Water Depth (m)", 
       y =expression(CH[4]~Ebullition~(mg~m^-2~hr^-1)))+
  theme_bw()


write_csv(x = select(fig7, siteID, wtrDpth, wtrDpthFilled, ch4.erate.mg.h),
          path = paste0(projectWD, "/dataL2/Fig6data.csv"))


## box and whisker plot of ebullition:total by site, Figure 7
ggplot(eqAreaDataSub, aes(siteID, ch4.erate.mg.h/ch4.trate.mg.h))+
    geom_boxplot()+
  geom_point(alpha=0.3)+
  ylab(expression(Ebullitive~Fraction~of~Total~CH[4]~Efflux))+
  theme_bw()
  #geom_hline(yintercept=0.8)
  #scale_x_discrete(limits=eqAreaDataSub$siteID[order(eqAreaDataSub$wtrDpth)])

write_csv(x = select(eqAreaDataSub, siteID, ch4.erate.mg.h, ch4.trate.mg.h),
          path = paste0(projectWD, "/dataL2/Fig7data.csv"))



### More exploratory plots:


# ### aggregate by site 
# eqAreaDataBySite<-eqAreaDataSub%>%
#   group_by(siteID)%>%
#   dplyr::summarise(meanWtrD = mean(wtrDpth, na.rm=TRUE),
#                    meanErate = mean(ch4.erate.mg.h, na.rm=TRUE),
#                    sdErate = sd(ch4.erate.mg.h, na.rm=TRUE),
#                    meanTrate = mean(ch4.trate.mg.h, na.rm=TRUE),
#                    sd.Trate = sd(ch4.trate.mg.h, na.rm=TRUE),
#                    meanEtoT = mean(ch4.erate.mg.h/ch4.trate.mg.h, na.rm=TRUE),
#                    sdEtoT = sd(ch4.erate.mg.h/ch4.trate.mg.h, na.rm=TRUE))
# 
# waterOrder<-eqAreaDataBySite$meanWtrD
# orderList <- order(waterOrder)
# lakeLevels <- eqAreaDataBySite[orderList, "siteID"]
# eqAreaDataBySite$fsiteID<-factor(eqAreaDataBySite$siteID, levels = lakeLevels)
# 
# ggplot(eqAreaDataBySite, aes(siteID, meanEtoT))+
#   geom_point()+
#   geom_errorbar(data=eqAreaDataBySite,
#                 ymin=eqAreaDataBySite$meanEtoT-eqAreaDataBySite$sdEtoT,
#                 ymax=eqAreaDataBySite$meanEtoT+eqAreaDataBySite$sdEtoT)+
#   ylim(0, 1)
# 
# #erate vs. drate per site
# ggplot(eqAreaDataSub, aes(ch4.erate.mg.h/ch4.trate.mg.h, wtrDpth))+
#   geom_point(alpha=0.3)
# 
# ggplot(eqAreaDataSub, aes(ch4.erate.mg.h/ch4.trate.mg.h, siteDistFromShore))+
#   geom_point(alpha=0.3)
#   
# 
# 
# 
