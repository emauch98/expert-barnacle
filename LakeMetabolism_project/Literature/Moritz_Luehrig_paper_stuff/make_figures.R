# startup ------------------------------------------------------------------

# replace for your directory
setwd("D:/Temp/pond_exp_osf")

rm(list = ls())

source("./R/methods_packages.R")


# load data --------------------------------------------------------------------

## data
all<-fread("./data/ponds_sonde_data_all.txt")
all[, Date := as.Date(Time_seq, origin="2016-06-25", tz="UTC")]
all[, Date_time := as.POSIXct(Time_seq*86400, origin="2016-06-25 00:00:00", tz="UTC")]

hourly<-fread("./data/ponds_sonde_data_hourly_avg.txt")
hourly[, Date := as.Date(Time_seq, origin="2016-06-25", tz="UTC")]
hourly[, Date_time := ymd_hms(paste(Date, paste0(Hour,":00:00")))]

daily<-fread("./data/ponds_sonde_data_daily_avg.txt")
daily[, Date := as.Date(Time_seq, origin="2016-06-25", tz="UTC")]

## meta info
key<-fread("./data_raw/ponds_sonde_key.txt")
pond.treat.col<-c(C= "#0570b0" ,O = "grey50",D = "#f46d43", M = "#7fbc41", MD = "#dd3497",   "Additive" = "grey20")

## disturbance timepoints & phases
dist_seq = c(48, 62, 76, 90, 107, 472)
dist_dates<-ymd(c("2016-08-12", "2016-08-26", "2016-09-09", "2016-09-23", "2016-10-10", "2017-10-10"))
dist_phases = data.table(Subphase=1:12, Disturbance=c(F,F,T,T,T,T,T,F,F,F,T,F))

## exclude dates (phase 1 and ice cover)
time_seq_excl = unique(daily[Subphase==1 | (Subphase==8 & Time_seq>170),]$Time_seq)
daily = daily[!Time_seq %in% time_seq_excl,]
hourly = hourly[!Time_seq %in% time_seq_excl,]
all = all[!Time_seq %in% time_seq_excl,]

cols = c("Chlorophyll_RFU", "BGAPC_RFU", "ODO_sat", "fDOM_RFU")
cols.metab=c("GPP","NEP","R_day")
date_breaks = c(as.Date("2016-07-30"),as.Date("2016-09-20"), as.Date("2016-11-15"), 
                as.Date("2017-04-15"), as.Date("2017-06-15"), as.Date("2017-08-15"),
                as.Date("2017-10-15"), as.Date("2017-12-10"), as.Date("2018-02-05"))
# date_breaks = c(as.Date("2016-07-30"), as.Date("2016-09-20"), as.Date("2016-11-20"),
#                 as.Date("2017-05-20"), as.Date("2017-07-30"),
#                 as.Date("2017-10-20"), as.Date("2017-12-15"), as.Date("2018-02-05"))
ax_titel_size = 10
plot_margins = c(0.2,1.2,0.2,0.2)
data_rect = unique(daily[,c("Date","Time_seq","Phase")])
data_rect[, xmin := ymd(ifelse(Phase==1, "2016-08-12",
                               ifelse(Phase==3, "2017-10-8", NA)))]
data_rect[, xmax := ymd(ifelse(Phase==1, "2016-10-24",
                               ifelse(Phase==3, "2017-10-24", NA)))]

# plot-template ---------------------------------------------------------------------
## this is a cookie-cutter for the figure layout

treat_order = c("C", "O","D", "M", "MD", "Additive")
labs = c(C=bquote("Control"),  
         O=bquote("Oligotrophic Control"),
         D=bquote(italic("Dreissena")), 
         M=bquote(italic("Myriophyllum")),
         MD=bquote(italic("Myriophyllum + Dreissena")~(measured)), 
         Additive=bquote(italic("Myriophyllum + Dreissena")~(predicted~additive~effect)))

pond.treat.col.fill = copy(pond.treat.col)
pond.treat.col.fill[c("Additive")] = NA
pond.treat.col.2 = copy(pond.treat.col[1:5])

dist_dates_dt = data.table(dist_dates)
dist_dates_dt[, label := c(10, 20, 30, 40, 50, 50)]
dist_dates_dt[, Phase := c(rep(1,5),3)]

facet_labs = c("1"="Phase 1","2"= "Phase 2","3"= "Phase 3")

gg_body = list(
  facet_wrap(~ Phase, ncol=3, scales = "free_x", labeller = as_labeller(facet_labs)),
  theme_classic(),
  geom_ribbon(aes(x=Date, ymin = MEAN - SE, ymax = MEAN + SE, fill = Treatment, group = interaction(Treatment, Phase)), alpha=0.5),
  geom_line(aes(x=Date, y=MEAN, colour=Treatment, group=interaction(Treatment, Phase), size=Treatment)),
  geom_point(aes(x=Date, y=y, colour=Treatment), pch=15),
  scale_colour_manual(values=pond.treat.col, labels=labs),
  scale_size_manual(values=c(rep(0.6,6)), labels=labs),
  scale_fill_manual(values=pond.treat.col, labels=labs),
  scale_x_date(breaks=date_breaks, date_labels = "%b '%y", expand = c(0,0)),
  guides(colour = guide_legend(ncol=2, 
                               byrow=TRUE,
                               override.aes = list(linetype  = c(1,1,1,1,1,1),
                                                   shape = c(NA,NA, 15,15,15,NA),
                                                   fill=c(pond.treat.col.fill)))
         )
  )

dist_rect = list(
  geom_rect(data=data_rect, aes(xmin=xmin-4, xmax=xmax, ymin=-Inf, ymax=Inf),  fill="grey90"),
  geom_vline(data=dist_dates_dt, aes(xintercept=dist_dates), colour="grey50")
)

labs = c(C=bquote("Control"),  
         O=bquote("Oligotrophic Control"),
         D=bquote(italic("Dreissena")), 
         M=bquote(italic("Myriophyllum")),
         MD=bquote(italic("Myriophyllum + Dreissena")))

gg_body2= list(
  facet_wrap(~ Phase, ncol=3, scales = "free_x", labeller = as_labeller(facet_labs)),
  theme_classic(),
  geom_ribbon(aes(x=Date, ymin = MEAN - SE, ymax = MEAN + SE, fill = Treatment, group = interaction(Treatment, Phase)), alpha=0.5),
  geom_line(aes(x=Date, y=MEAN, colour=Treatment, group=interaction(Treatment, Phase), size=Treatment)),
  geom_point(aes(x=Date, y=y, colour=Treatment), pch=15),
  scale_size_manual(values=c(rep(0.6,5)), labels=labs),
  scale_colour_manual(values=pond.treat.col, labels=labs),
  scale_fill_manual(values=pond.treat.col, labels=labs),
  scale_x_date(breaks=date_breaks, date_labels = "%b '%y", expand = c(0,0)),
  guides(colour = guide_legend(ncol=2, 
                               byrow=TRUE,
                               override.aes = list(linetype  = c(1,1,1,1,1),
                                                   shape = c(NA,NA,15,15,15))),
         fill = guide_legend(ncol=2, byrow=TRUE),
         size = guide_legend(ncol=2, byrow=TRUE))
)

aD = function(x){format(as.Date(x, origin="2016-06-25", tz="UTC"),"%Y-%m")}

gg_body3 = list(
  theme_classic(),
  ylab("Macrophytes + Dreissena\nLRR (measured)"),
  xlab("Macrophytes + Dreissena\nLRR (expected additive response)"),
  geom_point(aes(x=Additive.MEAN, y=MD.MEAN, colour=dist, stroke = 0), size=3),
  geom_point(aes(x=Additive.MEAN, y=MD.MEAN, fill=Time_seq), pch=21, size=2, stroke = 0),
  geom_abline(intercept = 0, slope=1),
  scale_fill_viridis("Time", breaks=seq(0,600, 100), labels=aD),
  scale_colour_manual(values=c("red")),
  guides(fill = guide_colourbar(barwidth = 25), colour=F),
    theme(
      legend.position = "none"
    ))

theme_top =
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.text.y = element_text(colour = "black"),
    axis.ticks.y = element_line(colour = "black"),
    axis.title.x = element_blank(),
    panel.border=element_blank(),
    strip.text=element_text(size=12, colour="black"),
    strip.background=element_rect(colour="white", 
                                  fill="white")
  ) 

theme_mid =
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.text.y = element_text(colour = "black"),
    axis.ticks.y = element_line(colour = "black"),
    axis.title.x = element_blank(),
    panel.border = element_rect(colour = "gray", fill=NA, size=0.5),
    strip.text.x = element_blank(),
  )

theme_bottom =
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.direction = "horizontal",
    axis.title.x = element_blank(),
    panel.border = element_rect(colour = "gray", fill=NA, size=0.5),
    strip.text.x = element_blank(),
    axis.text = element_text(colour = "black"),
    axis.ticks = element_line(colour = "black"),
  )


# fig1a ---------------------------------------------------------------

daily  = fread("./data/sliding_daily.txt")

plot_data = daily[!Treatment=="MD" & !Treatment=="O" & Time_seq > 477 & Time_seq < 550,]
plot_data = plot_data[, m_r(Chlorophyll_RFU), by=c("Phase","Time_seq", "Treatment")]

length=72

## add additivity expectation
plot_data = dcast(plot_data, Time_seq~Treatment, value.var = "V1")
plot_data[,C := C-0.35]

plot_data[,Add := (M+D)-C]
plot_data[,Ant :=Add + c(seq(0, 1, length.out= 22), seq(1, 2, length.out= 50)) + 
        rnorm(length,mean = 0.1, sd=0.025)]
plot_data[,Syn :=Add - c(seq(0.1, 1, length.out= 12), seq(1, 2, length.out= 60)) + 
        rnorm(length,mean = 0.1, sd=0.025)]

plot_data = melt(plot_data, id=c("Time_seq"), value.name = "MEAN", variable.name = "Treatment")
plot_data[,Time_seq:=Time_seq-477]
plot_data[, Date := as.Date(Time_seq, origin="2016-06-25", tz="UTC")]
plot_data[, Treatment := factor(Treatment)]

plot_data[,MEAN:=MEAN+2]

cols.schem = c(C = "#0570b0", D = "#f46d43",  M = "#7fbc41", 
         Add = "grey20", Ant = "grey20", Syn = "grey20")
ltype = c(1,1,1,1,3,2)
alpha = c(1,1,1,0.5,0.5,0.5)

lab = c(C=expression("Control"),
        D=expression(italic("Dreissena")),
        M=expression(italic("Myriophyllum")),
        Add=expression("Additive effect"),
        Ant=expression("Non-additive effect\n(above additive)"),
        Syn=expression("Non-additive effect\n(below additive)")
        )

p1=
ggplot(plot_data) + theme_classic() + ylab("Ecosystem variable") + xlab("Time") +
  geom_line(aes(x=Time_seq, y=MEAN, colour=Treatment, linetype=Treatment), size=1) + 
  scale_colour_manual(values=cols.schem, labels = lab ) + 
  scale_y_continuous(breaks=c(0,2,4,6,8), limits=c(0,7)) +
  scale_linetype_manual(values=ltype, labels = lab ) + 
  guides(colour=guide_legend(keywidth = 2.5, keyheight = 2)) +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size=10),
    legend.text.align = 0,
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    # axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10)),
    plot.margin=unit(c(0.8,0.5,0.5,0.5),"cm"))

png("./figures/figure1a.png", width = 170, height = 85, res = 600, units="mm")
p1
dev.off()


# fig2 ---------------------------------------------------------------------

data = fread("./data/sliding_hourly_mean_7d.txt")
data = merge(data, key[,c("Pond","Replicate")])

plot_data = melt(data, id=c("Phase","Time_seq", "Hour", "Treatment","Replicate"), measure.vars = cols, variable.name = "Parameter")

## add additivity expectation
plot_data = dcast(plot_data, Phase+Time_seq+Hour+Replicate+Parameter~Treatment, value.var = "value")
plot_data[,Additive := (M+D)-C]
plot_data = melt(plot_data, id=c("Phase","Time_seq", "Hour","Replicate", "Parameter"), 
                 measure.vars = c("C", "D", "M", "MD", "O", "Additive"), variable.name = "Treatment")

## calculate mean per hourly sliding
plot_data = plot_data[, m_r(value), by=c("Phase","Time_seq", "Treatment","Replicate","Parameter")]
plot_data = plot_data[, m_se(V1), by=c("Phase","Time_seq", "Treatment","Parameter")]
plot_data = plot_data[complete.cases(MEAN) & !Time_seq %in% time_seq_excl,] # & !Treatment=="O" 
plot_data[, Date := as.Date(Time_seq, origin="2016-06-25", tz="UTC")]
plot_data[Treatment=="Additive", `:=` (MEAN = ifelse(MEAN<0, 0, MEAN), SE=0)]

## y pos for Pvalues
plot_data[,y := max(.SD, na.rm = T)/10, .SDcols="MEAN", by="Parameter"]
plot_data[,y := ifelse(Treatment=="M", -(y - y/2),
                       ifelse(Treatment=="D",-y,
                              ifelse(Treatment=="MD", -(y + y/2),NA)))]   
plot_data[,y := ifelse(Parameter=="ODO_sat",y+75, y)]

res = fread("./data/aov_sliding_hourly_mean_7d.txt")
res = res[Pval<=0.05,]
res = res[,.N,by=c("Treatment","Time_seq","Parameter")]
res = res[N>=23,]
res[, Date := as.Date(Time_seq, origin="2016-06-25", tz="UTC")]

plot_data = merge(plot_data, res, all=T)
plot_data[, y := ifelse(is.na(N), NA, y)]
plot_data[, Treatment := factor(Treatment, levels=treat_order)] 

## make figures
p1 =
  ggplot(plot_data[Parameter=="Chlorophyll_RFU",]) + ylab("Chlorophyll\nfluorescence mean") + 
  dist_rect + 
  geom_label(data=dist_dates_dt, aes(x=dist_dates, y=47, label=label),
                           label.size=0, size=2, colour = "grey50", 
                           fill="grey90",label.padding=unit(0.05, "lines")) +
  gg_body + theme_top + 
  geom_hline(yintercept = 0) + 
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 5), size=ax_titel_size))

p2 = ggplot(plot_data[Parameter=="BGAPC_RFU",]) + ylab("Phyocycanin\nfluorescence mean") +  
  dist_rect + 
  geom_label(data=dist_dates_dt, aes(x=dist_dates, y=9.2, label=label),
             label.size=0, size=2, colour = "grey50", 
             fill="grey90",label.padding=unit(0.05, "lines")) +
  gg_body + theme_mid + 
  geom_hline(yintercept = 0) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 5), size=ax_titel_size))


p3 = ggplot(plot_data[Parameter=="fDOM_RFU",]) + ylab("fDOM\nfluorescence mean") + 
  dist_rect + 
  geom_label(data=dist_dates_dt, aes(x=dist_dates, y=2.2, label=label),
             label.size=0, size=2, colour = "grey50", 
             fill="grey90",label.padding=unit(0.05, "lines")) +
  gg_body + theme_mid + 
  geom_hline(yintercept = 0) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 13, b = 0, l = 5), size=ax_titel_size))


p4 = ggplot(plot_data[Parameter=="ODO_sat",]) + ylab("Dissolved oxygen\n% saturation mean") + 
  dist_rect + 
  geom_label(data=dist_dates_dt, aes(x=dist_dates, y=210, label=label),
             label.size=0, size=2, colour = "grey50", 
             fill="grey90",label.padding=unit(0.05, "lines")) +
  gg_body + theme_bottom + 
  geom_hline(yintercept = 100) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 11, b = 0, l = 5), size=ax_titel_size))


png("./figures/figure2.png", width = 173, height = 250, units = 'mm', res = 600)
plot_grid(p1, p2, p3, p4, ncol=1, rel_heights =  c(0.229,0.2,0.2,0.32))
dev.off()


# fig3 ---------------------------------------------------------------------

data = fread("./data/sliding_hourly_cv_7d.txt")
data = merge(data, key[,c("Pond","Replicate")])

plot_data = melt(data, id=c("Phase","Time_seq", "Hour", "Treatment","Replicate"), measure.vars = cols, variable.name = "Parameter")

plot_data = plot_data[, m_r(value), by=c("Phase","Time_seq", "Treatment","Replicate","Parameter")]
plot_data = plot_data[, m_se(V1), by=c("Phase","Time_seq", "Treatment","Parameter")]
plot_data = plot_data[complete.cases(MEAN) & !Time_seq %in% time_seq_excl,] # & !Treatment=="O"  
plot_data[, Date := as.Date(Time_seq, origin="2016-06-25", tz="UTC")]

plot_data[,y := max(.SD, na.rm = T)/10, .SDcols="MEAN", by="Parameter"]
plot_data[,y := ifelse(Treatment=="M", -(y - y/2),
                       ifelse(Treatment=="D",-y,
                              ifelse(Treatment=="MD", -(y + y/2),NA)))]   

res = fread("./data/aov_sliding_hourly_cv_7d.txt")
res = res[Pval<=0.05,]
res = res[,.N,by=c("Treatment","Time_seq","Parameter")]
res = res[N>=23,]
res[, Date := as.Date(Time_seq, origin="2016-06-25", tz="UTC")]

plot_data = merge(plot_data, res, all=T)
plot_data[, y := ifelse(is.na(N), NA, y)]
plot_data[, Treatment := factor(Treatment, levels=treat_order)]

## make figures
p1 = ggplot(plot_data[Parameter=="Chlorophyll_RFU",]) + ylab("Chlorophyll\nfluorescence CV") + 
  dist_rect + 
  geom_label(data=dist_dates_dt, aes(x=dist_dates, y=0.8, label=label),
             label.size=0, size=2, colour = "grey50", 
             fill="grey90",label.padding=unit(0.05, "lines")) +
  gg_body2 + theme_top + 
  geom_hline(yintercept = 0) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 17, b = 0, l = 5), size=ax_titel_size))


p2 = ggplot(plot_data[Parameter=="BGAPC_RFU",]) + ylab("Phyocycanin\nfluorescence CV") +  
  dist_rect + 
  geom_label(data=dist_dates_dt, aes(x=dist_dates, y=0.6, label=label),
             label.size=0, size=2, colour = "grey50", 
             fill="grey90",label.padding=unit(0.05, "lines")) +
  gg_body2 + theme_mid + 
  geom_hline(yintercept = 0) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 17, b = 0, l = 5), size=ax_titel_size))


p3 = ggplot(plot_data[Parameter=="fDOM_RFU",]) + ylab("fDOM\nfluorescence CV") + 
  dist_rect + 
  geom_label(data=dist_dates_dt, aes(x=dist_dates, y=0.175, label=label),
             label.size=0, size=2, colour = "grey50", 
             fill="grey90",label.padding=unit(0.05, "lines")) +
  gg_body2 + theme_mid + 
  geom_hline(yintercept = 0) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 12, b = 0, l = 5), size=ax_titel_size))


p4 = ggplot(plot_data[Parameter=="ODO_sat",]) + ylab("Dissolved oxygen\n% saturation CV") + 
  dist_rect + 
  geom_label(data=dist_dates_dt, aes(x=dist_dates, y=0.2, label=label),
             label.size=0, size=2, colour = "grey50", 
             fill="grey90",label.padding=unit(0.05, "lines")) +
  gg_body2 + theme_bottom + 
  geom_hline(yintercept = 0) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 12, b = 0, l = 5), size=ax_titel_size))


png("./figures/figure3.png", width = 173, height = 250, units = 'mm', res = 600)
plot_grid(p1, p2, p3, p4, ncol=1, rel_heights =  c(0.229,0.2,0.2,0.32))
dev.off()


# fig4 -------------------------------------------------------------------

metab = fread("./data/metab_odum.txt")
metab = merge(metab, key[,c("Pond","Replicate")])

plot_data = melt(metab, id=c("Phase","Time_seq", "Treatment","Replicate"), 
                 measure.vars = cols.metab, variable.name = "Parameter")

## add additivity expectation
plot_data = dcast(plot_data, Phase+Time_seq+Replicate+Parameter~Treatment, value.var = "value")
plot_data[, Additive := (M+D)-C]
plot_data = melt(plot_data, id=c("Phase","Time_seq","Replicate", "Parameter"), 
                 measure.vars = c("C", "D", "M", "MD","O", "Additive"), variable.name = "Treatment")

## calculate mean per hourly sliding
plot_data = plot_data[, m_se(value), by=c("Phase","Time_seq", "Treatment","Parameter")]
plot_data = plot_data[complete.cases(MEAN) & !Time_seq %in% time_seq_excl,] # !Treatment=="O" & 
plot_data[, Date := as.Date(Time_seq, origin="2016-06-25", tz="UTC")]

## get p values
metab_aov = fread("./data/metab_odum_aov.txt")
metab_aov[,Parameter := ifelse(Parameter=="R","R_day", Parameter)]
plot_data = merge(plot_data, metab_aov[,c("Time_seq","Treatment","Parameter","P_value")], all=T)

## y pos for Pvalues
plot_data[,y := max(abs(.SD), na.rm = T)/10, .SDcols="MEAN", by="Parameter"]
plot_data[,y := ifelse(Treatment=="M", -(y - y/2),
                       ifelse(Treatment=="D",-y,
                              ifelse(Treatment=="MD", -(y + y/2),NA)))]   
plot_data[,y := ifelse(P_value<0.05, y, NA)]
plot_data[Parameter=="R_day",y := y+2]
plot_data[, Treatment := factor(Treatment, levels=treat_order)]
plot_data[Treatment=="Additive", SE := 0]

## fix NEP <0 values
plot_data[Parameter=="NEP", `:=` (MEAN = ifelse(MEAN<0, 0, MEAN),SE = ifelse(MEAN+SE<0, 0, SE))]
plot_data[Parameter=="R_day", `:=` (MEAN = ifelse(MEAN>0, 0, MEAN),SE = ifelse(MEAN+SE>0, 0, SE))]

## make figures
labs = c(C=bquote("Control"),  
         O=bquote("Oligotrophic Control"),
         D=bquote(italic("Dreissena")), 
         M=bquote(italic("Myriophyllum")),
         MD=bquote(italic("Myriophyllum + Dreissena")~(measured)), 
         Additive=bquote(italic("Myriophyllum + Dreissena")~(predicted~additive~effect)))

p1 = ggplot(plot_data[Parameter=="GPP",]) +  ylab(expression(atop(GPP,(g~O[2]~m^{-3}~d^{-1})))) +
  dist_rect + 
  geom_label(data=dist_dates_dt, aes(x=dist_dates, y=12, label=label),
             label.size=0, size=2, colour = "grey50", 
             fill="grey90",label.padding=unit(0.05, "lines")) +
  gg_body + theme_top + 
  geom_hline(yintercept = 0) +
  scale_size_manual(values=c(rep(0.6,5),0.4), labels = labs) +
  scale_y_continuous(breaks=seq(0,12,2)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 5), size=ax_titel_size))

p2 = ggplot(plot_data[Parameter=="NEP",]) +  ylab(expression(atop(NEP,(g~O[2]~m^{-3}~d^{-1})))) +
  dist_rect + 
  geom_label(data=dist_dates_dt, aes(x=dist_dates, y=5.8, label=label),
             label.size=0, size=2, colour = "grey50", 
             fill="grey90",label.padding=unit(0.05, "lines")) +
  gg_body + theme_mid + 
  geom_hline(yintercept = 0) +
  scale_size_manual(values=c(rep(0.6,5),0.4), labels = labs) +
  scale_y_continuous(breaks=seq(0,5,1)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 19, b = 0, l = 5), size=ax_titel_size))

p3 = ggplot(plot_data[Parameter=="R_day",]) +  ylab(expression(atop(R,(g~O[2]~m^{-3}~d^{-1})))) + 
  dist_rect + 
  geom_label(data=dist_dates_dt, aes(x=dist_dates, y=-9, label=label),
             label.size=0, size=2, colour = "grey50", 
             fill="grey90",label.padding=unit(0.05, "lines")) +
  gg_body + theme_bottom + 
  geom_hline(yintercept = 0) +
  scale_size_manual(values=c(rep(0.6,5),0.4), labels = labs) +
  scale_y_continuous(breaks=seq(-10,0,2)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 5), size=ax_titel_size))



png("./figures/figure4.png", width = 173, height = 210, units = 'mm', res = 600)
plot_grid(p1, p2, p3, ncol=1, rel_heights = c(0.33,0.3,0.47))
dev.off()

# fig5 ----------------------------------------------------------------
metab = fread("./data/metab_odum_cv_7d.txt")
metab = merge(metab, key[,c("Pond","Replicate")])

plot_data = melt(metab, id=c("Phase","Time_seq", "Treatment","Replicate"), 
                 measure.vars = cols.metab, variable.name = "Parameter")

## calculate mean per hourly sliding
plot_data = plot_data[, m_se(value), by=c("Phase","Time_seq", "Treatment","Parameter")]
plot_data = plot_data[complete.cases(MEAN) & !Time_seq %in% time_seq_excl,] #!Treatment=="O" & 
plot_data[, Date := as.Date(Time_seq, origin="2016-06-25", tz="UTC")]

## add pval
res = fread("./data/metab_odum_cv_aov.txt")
res[,Parameter := ifelse(Parameter=="R","R_day", Parameter)]
res = res[P_value<=0.05,]
plot_data = merge(plot_data, res[,c("Time_seq", "Parameter", "Treatment", "P_value")], all=T)

## y pos for Pvalues
plot_data[,y := max(abs(.SD), na.rm = T)/10, .SDcols="MEAN", by="Parameter"]
plot_data[,y := ifelse(Treatment=="M", -(y - y/2),
                       ifelse(Treatment=="D",-y,
                              ifelse(Treatment=="MD", -(y + y/2),NA)))]   
plot_data[,y:= ifelse(is.na(P_value),NA,y),]
plot_data[, Treatment := factor(Treatment, levels=treat_order)]

## make figures
p1 = ggplot(plot_data[Parameter=="GPP",]) +  ylab(expression(atop(GPP,(g~O[2]~m^{-3}~d^{-1})~CV))) +
  dist_rect + 
  geom_label(data=dist_dates_dt, aes(x=dist_dates, y=0.6, label=label),
             label.size=0, size=2, colour = "grey50", 
             fill="grey90",label.padding=unit(0.05, "lines")) +
  gg_body2 + theme_top + 
  geom_hline(yintercept = 0) + scale_y_continuous(breaks=seq(0,0.6,0.2)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 5), size=ax_titel_size))
# 
p2 = ggplot(plot_data[Parameter=="NEP",]) +  ylab(expression(atop(NEP,(g~O[2]~m^{-3}~d^{-1})~CV))) +
  dist_rect + 
  geom_label(data=dist_dates_dt, aes(x=dist_dates, y=1.55, label=label),
             label.size=0, size=2, colour = "grey50", 
             fill="grey90",label.padding=unit(0.05, "lines")) +
  gg_body2 + theme_mid + geom_hline(yintercept = 0) + scale_y_continuous(breaks=seq(0,1.6,0.2)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 5), size=ax_titel_size))

p3 = ggplot(plot_data[Parameter=="R_day",]) +  ylab(expression(atop(R,(g~O[2]~m^{-3}~d^{-1})~CV))) +
  dist_rect + 
  geom_label(data=dist_dates_dt, aes(x=dist_dates, y=1.25, label=label),
             label.size=0, size=2, colour = "grey50", 
             fill="grey90",label.padding=unit(0.05, "lines")) +
  gg_body2 + theme_bottom + geom_hline(yintercept = 0) + scale_y_continuous(breaks=seq(0,1.4,0.2)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 5), size=ax_titel_size))



png("./figures/figure5.png", width = 173, height = 210, units = 'mm', res = 600)
plot_grid(p1, p2, p3, ncol=1, rel_heights = c(0.33,0.3,0.47))
dev.off()


# figS1 ---------------------------------------------------------------------

data = fread("./data/sliding_hourly_ac_7d.txt")
data = merge(data, key[,c("Pond","Replicate")])

plot_data = melt(data, id=c("Phase","Time_seq", "Hour", "Treatment","Replicate"), measure.vars = cols, variable.name = "Parameter")

plot_data = plot_data[, m_r(value), by=c("Phase","Time_seq", "Treatment","Replicate","Parameter")]
plot_data = plot_data[, m_se(V1), by=c("Phase","Time_seq", "Treatment","Parameter")]
plot_data = plot_data[complete.cases(MEAN)  & !Time_seq %in% time_seq_excl,] # & !Treatment=="O"
plot_data[, Date := as.Date(Time_seq, origin="2016-06-25", tz="UTC")]
plot_data[Treatment=="Additive", `:=` (MEAN = ifelse(MEAN<0, 0, MEAN), SE=0)]

plot_data[,y := max(.SD, na.rm = T)/10, .SDcols="MEAN", by="Parameter"]
plot_data[,y := ifelse(Treatment=="M", -(y - y/2),
                       ifelse(Treatment=="D",-y,
                              ifelse(Treatment=="MD", -(y + y/2),NA)))]   

res = fread("./data/aov_sliding_hourly_ac_7d.txt")
res = res[Pval<=0.05,]
res = res[,.N,by=c("Treatment","Time_seq","Parameter")]
res = res[N>=23,]
res[, Date := as.Date(Time_seq, origin="2016-06-25", tz="UTC")]

plot_data = merge(plot_data, res, all=T)
plot_data[, y := ifelse(is.na(N), NA, y)]
plot_data[, Treatment := factor(Treatment, levels=treat_order)]

## make figures
p1 = ggplot(plot_data[Parameter=="Chlorophyll_RFU",]) + ylab("Chlorophyll\nfluorescence AC") + 
  dist_rect + 
  geom_label(data=dist_dates_dt, aes(x=dist_dates, y=0.5, label=label),
             label.size=0, size=2, colour = "grey50", 
             fill="grey90",label.padding=unit(0.05, "lines")) +
  gg_body2 + theme_top + scale_y_continuous(breaks = seq(0,1, 0.2)) +
  geom_hline(yintercept = 0) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 5), size=ax_titel_size))


p2 = ggplot(plot_data[Parameter=="BGAPC_RFU",]) + ylab("Phyocycanin\nfluorescence AC") +  
  dist_rect + 
  geom_label(data=dist_dates_dt, aes(x=dist_dates, y=0.5, label=label),
             label.size=0, size=2, colour = "grey50", 
             fill="grey90",label.padding=unit(0.05, "lines")) +
  gg_body2 + theme_mid + scale_y_continuous(breaks = seq(0,1, 0.2)) +
  geom_hline(yintercept = 0) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 5), size=ax_titel_size))


p3 = ggplot(plot_data[Parameter=="fDOM_RFU",]) + ylab("fDOM\nfluorescence AC") + 
  dist_rect + 
  geom_label(data=dist_dates_dt, aes(x=dist_dates, y=0.5, label=label),
             label.size=0, size=2, colour = "grey50", 
             fill="grey90",label.padding=unit(0.05, "lines")) +
  gg_body2 + theme_mid + scale_y_continuous(breaks = seq(0,1, 0.2)) +
  geom_hline(yintercept = 0) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 5), size=ax_titel_size))


p4 = ggplot(plot_data[Parameter=="ODO_sat",]) + ylab("Dissolved oxygen\n% saturation AC") + 
  dist_rect + 
  geom_label(data=dist_dates_dt, aes(x=dist_dates, y=0.5, label=label),
             label.size=0, size=2, colour = "grey50", 
             fill="grey90",label.padding=unit(0.05, "lines")) +
  gg_body2 + theme_bottom + scale_y_continuous(breaks = seq(0,1, 0.2)) +
  geom_hline(yintercept = 0) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 5), size=ax_titel_size))


png("./figures/figureS1.png", width = 173, height = 250, units = 'mm', res = 600)
plot_grid(p1, p2, p3, p4, ncol=1, rel_heights =  c(0.229,0.2,0.2,0.32))
dev.off()
