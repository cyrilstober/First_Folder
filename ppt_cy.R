
#-------------------------------------------------------------------------------
## set working directory

setwd("~/Desktop/ppt")


#-------------------------------------------------------------------------------


## first of all use install.packages("packageName")

# install.packages("terra")
# install.packages("rgdal") ## do this for the rest of the packages
# install.packages("raster")
# install.packages("ncdf4")

#-------------------------------------------------------------------------------
## import packages list
library(rgdal)
library(raster)
library(ncdf4)
library(terra)


#-------------------------------------------------------------------------------
#### set file path
d='~/Desktop/ppt'


#### create .nc4 list
lf = list.files(d, pattern = ".nc$")

#### creat function for converting .nc4 files to tif
f = function(x) {
  r= rast(paste0('NETCDF:',x))
  crs(r) <- "+proj=longlat +datum=WGS84"
  writeRaster(r,gsub(".nc$", "_from_nc_.tif", as.character(x)), overwrite=T)
}

#### apply the function and convert .nc4 files to tif
ppt_data<- lapply(lf,f)

#### create a list of .tif files
data_list = list.files(d, pattern = ".tif$") 

#-------------------------------------------------------------------------------
## import shape files of study area

# required package
library(rgdal)
dsn = "~/Desktop/ppt"

###
shpfile = readOGR(".","study_area_boundary")

options(stringsAsFactors = FALSE)

#change crs of shapefile to crs of one of the raster

shp2 <- spTransform(shpfile, crs("+proj=longlat +datum=WGS84 +no_defs"))


plot(shp2)

extent(shp2)

# set crop extent

crop_ext = extent(6.3,7,5.4,6.8)


ppt_rast <- rast(ppt_data)


### crop raster using crop_ext to readuce 
ppt_data_crop <- crop(ppt_rast, crop_ext)

ppt_mask <- mask(brick(ppt_data_crop), shp2)

plot(ppt_mask)


library(raster)
## increasing raster resolution
# check current resolution
res(ppt_mask)

#disaggregate from 0.0417x0.0417 resolution to 0.0035 (factor = 14.65)
## note: aggregate() reduces resolution
ppt_resolution <- disaggregate(ppt_mask, fact=70)
res(ppt_resolution)
#[1] 0.0005 0.0005

plot(ppt_resolution)


##------------------------------------------------------------------------------
### find mean temperature value

### loop for mean aet
fun_mean <- function(f) {
  #import tif as brick
  r <- brick(f)
  #crop brick
  r2 <- crop(r, crop_ext)
  #change resolution
  r3 <- disaggregate(r2, fact=16.67)
  #mask with polygon but you must have imported polygon already
  r4 = mask(r3, shp2)
  #calculate mean
  r5<- data.frame(cellStats(r4, stat='mean', na.rm=T))
  #print result
  print(r5)
}


#### apply loop function
summary <- data.frame(lapply(data_list, fun_mean))


#### transpose file
ppt_mean = data.frame(t(summary))


#-------------------------------------------------------------------------------
#####
######

###if it was a single line data use this to rearrange into a matrix
####ppt_mat <- as.data.frame(matrix(unlist(ppt_max), ncol = 12,
####nrow = 22, byrow = TRUE))

colnames(ppt_mean) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

rownames(ppt_mean) <- 1991:2021

dim(ppt_mean)

plot(ppt_mean)
print(ppt_mean[,1])
colnames(ppt_mean)
rownames(ppt_mean)


# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("forcats")
library(ggplot2)
library(tibble)
library(dplyr)
library(forcats)
# install.packages("tidyr")
library(tidyr)

ppt_ts <- ppt_mean %>%
  tibble::rownames_to_column(var = "year") %>%
  tidyr::pivot_longer(cols = -year, names_to = "month",
                      values_to = "ppt") %>%
  mutate(month = as.factor(month) %>%
           forcats::fct_relevel(month.abb))


#########
ppt_ts %>%
  ggplot(aes(x = month, y = year, fill = ppt )) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(y = "Year", x = "Month", fill = "Mean ppt.")


#########
Fig9=ppt_ts %>%
  ggplot(aes(x = month, y = ppt, fill = year, color = year, group = year)) +
  # geom_line(alpha = 0.2) +
  geom_point(alpha = 0.2) +
  geom_line(se = FALSE, span = 0.9) +
  labs(y = "Mean ppt (mm)", x = "Month")

ggsave("Fig9.jpeg",Fig9,dpi = 500)

# groups Nov, Dec, Jan+1, Feb+1
# groups Jun, Jul, Aug, Sep
# groups Feb, Mar, Apr, May

ppt_ts %>%
  ggplot(aes(x = year, y = ppt)) +
  geom_line() +
  scale_fill_viridis_c() +
  labs(y = "Precipitation (mm)", x = "Month", fill = "Mean ppt.")



# ndvi_ts
# evi_ts
# kndvi_ts
# tmax_ts
# ppt_ts


# ------------------------------------------------------------------------------

ppt_ts
### add data to timeseries data
start_date <- as.Date("1991/01/01")
ppt_ts$date <- seq(start_date, by = "month", length.out = 372)
library(scales)
figxxx=ggplot(ppt_ts, aes(x=date, y=ppt))+geom_point()+geom_line()+ stat_smooth(data = ppt_ts, aes(date, ppt),method="loess",span=0.1,se=T)+
  theme_bw()+ ggtitle("")+
  labs(x = "", y ="")+scale_x_date(breaks = seq(as.Date("1991-01-01"), as.Date("2021-12-01"), by="10 years"), 
                                   labels=date_format("%Y"))
figxxx
Figa=ppt_ts %>%
  ggplot(aes(x = date, y = ppt)) +
  geom_line() +
  geom_point() + scale_x_date(breaks = seq(as.Date("1991-01-01"), as.Date("2021-12-01"), by="2 years"), 
  labels=date_format("%Y"))
  theme_bw() +
  labs(title = "Variable trends", x = "Year", y="ppt (mm)") +
  theme(legend.position="none") +
  geom_smooth()+scale_x_date(limit=c(as.Date("1991-01-01"),as.Date("2021-12-01")))

write.csv(ppt_ts,"ppt_ts.csv")

ggsave("Figa.jpeg",Figa,dpi = 500)

library(scales)
#ppt_ts=filter(ppt_ts,month %in% c("Jan","Feb","Mar","Apr","May","Jun))


ppt_ts%>%
  ggplot(aes(x = date, y = ppt)) +
  geom_line() +
  geom_point() + scale_x_date(limits = as.Date(c("1991-01-01", "2021-12-01")),labels = date_format("%Y"))+
  scale_x_date(date_breaks = "months",date_labels = "%Y")+
  labs(title = "Variable trends", x = "Year", y= "ppt (mm)") +
  theme(legend.position="none") +
  geom_smooth()



fig45=ggplot(ppt_ts, aes(x=date, y=ppt))+geom_point()+geom_line()+ stat_smooth(data = ppt_ts, aes(date, ppt),method="loess",span=0.1,se=T)+
  theme_bw()+ ggtitle("")+
  labs(x = "Date", y ="Precipitation (mm)")+scale_x_date(breaks = seq(as.Date("1991-01-01"), as.Date("2021-12-01"), by="2 years"), 
                                                       labels=date_format("%Y"))


ppt_ts= ppt_ts %>% 
  mutate(season= ifelse(month=="Apr"|month=="May"|month=="Jun"|month=="Jul"|month=="Aug"|month=="Sep"|month=="Oct","Wet","Dry"))

fig45=ggplot(ppt_ts, aes(x=date, y=ppt))+geom_point(aes(col=season),size=8)+geom_line(size=3)+ stat_smooth(data = ppt_ts, aes(date, ppt),method="loess",span=0.1,se=T,size=3)+
  theme_bw()+ ggtitle("")+
  labs(x = "Date", y ="Precipitation (mm)")+scale_x_date(breaks = seq(as.Date("1991-01-01"), as.Date("2021-12-01"), by="2 years"), 
                                                         labels=date_format("%Y"))+theme(text = element_text(size = 35))
ggsave("Precipitation line graph.jpeg",fig45,dpi = 500,width = 45,height = 30)

fig46=ggplot(ppt_ts, aes(x=date, y=ppt,fill=season))+geom_bar(stat="identity")+
  theme_bw()+ ggtitle("")+
  labs(x = "Date", y ="Precipitation (mm)")+scale_x_date(breaks = seq(as.Date("1991-01-01"), as.Date("2021-12-01"), by="2 years"), 
                                                       labels=date_format("%Y"))+theme(text = element_text(size = 35))

ggsave("Precipitation bars.jpeg",fig46,dpi = 500,width = 45,height = 30)



ppt_ts3=data.frame(ppt_ts)
names(ppt_ts3)=c("Year","Month","PPT","Date")

ppt_ts3$Date=as.Date(ppt_ts3$Date,origin="1991-02-01")
ppt_ts3%>%
  ggplot(aes(x = Date, y = PPT)) +
  geom_line() +
  geom_point() + scale_x_date(labels = date_format("%Y"))+
  labs(title = "Variable trends", x = "Year", y= "ppt (mm)") +
  theme(legend.position="none") +
  geom_smooth()



Fig10=ppt_ts %>%
  ggplot(aes(x = month, y = ppt, group=year,color=year)) +
  geom_point() +
  geom_line()+
  labs(title = "Variable trends", x = "Month") +
  facet_wrap(~ year, ncol = 5, scale = "free_y") +
  #scale_color_continous() +
  theme_bw() +
  theme(legend.position='none')

ggsave("Fig10.jpeg",Fig10,dpi = 500)
# excluding some months
ppt_ts=filter(ppt_ts, month %in% c("Jul","Aug","Sep","Oct","Nov","Dec"))
Fig11=ppt_ts %>%
  ggplot(aes(x = month, y = ppt, group=year,color=year)) +
  geom_point() +
  geom_line()+
  labs(title = "Variable trends", x = "Month") +
  facet_wrap(~ year, ncol = 5, scale = "free_y") +
  #scale_color_continous() +
  theme_bw() +
  theme(legend.position='none')

ggsave("Fig11.jpeg",Fig11,dpi = 500)
cor(ppt_ts$ppt,Runoff_ts$q)

length(Runoff_ts$q)
#-------------------------------------------------------------------------------
### find correlation between variables
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

my_data <- data.frame(ppt_ts$ppt,Runoff_ts$q,temp_ts$tmax)

names(my_data) <- c("Precipitation", "Runoff","Temperature")

plot(my_data)
cor(my_data,method="pearson")



Data.num <- my_data[c("Precipitation","Runoff","Temperature")]

chart.Correlation(Data.num,
                  method="pearson",
                  histogram=TRUE,
                  pch=16)

library(corrplot)
cord1=cor(Data.num,method = "pearson")
corrplot(cord1,method = "number",type = "upper")

# ndvi_ts
# evi_ts
# kndvi_ts
# tmax_ts
# ppt_ts


# ------------------------------------------------------------------------------

ppt_data
### add data to timeseries data
start_date <- as.Date("1991/01/01")
ppt_ts$date <- seq(start_date, by = "month", length.out = 372)
Runoff_ts$date <- seq(start_date, by = "month", length.out = 372)
temp_ts$date <- seq(start_date, by = "month", length.out = 372)


### create a copy of the time series data
ppt_v <- ppt_ts
Runoff_v <- Runoff_ts
temp_v <- temp_ts


### create column called variable to aid merging

Runoff_v$variable <- "Runoff"
ppt_v$variable <- "Precipitation"
temp_v$variable <- "Temperature"


### rename column names to aid merging
names(ppt_v) = c("year","month","value","date","variable")
names(Runoff_v) = c("year","month","value","date","variable")
names(temp_v) = c("year","month","value","date","variable")


### merge by row to form a new dataframe called my_data_ts

my_data_ts <- rbind(ppt_v, Runoff_v,temp_v) %>%
  group_by(variable)


library(ggplot2)
library(grid)
library(gridExtra)

precp_df=subset(my_data_ts,my_data_ts$variable=="Precipitation")

g1 <- ggplot(precp_df, aes(date, value)) +
  geom_bar(stat = 'identity', fill = "blue") +
  theme_bw() +
  ylab("Precipitation (mm)") + xlab("")+
  labs(title = "") +
  scale_y_reverse()+
  theme(axis.title.y = element_text(size = 25),axis.title.x    = element_text(size = 25),
        axis.text.x     = element_text(size = 25),
        axis.ticks.x    = element_text(size = 25),
        axis.ticks.y    = element_text(size = 25))



g1 <- ggplot(precp_df, aes(date, value)) + 
  geom_bar(stat = 'identity', fill = "blue") +
  theme_bw() +
  ylab("Precipitation (mm)") + xlab("")+
  labs(title = "") +
  scale_y_reverse()+
  theme(axis.title.y = element_text(size = 16),axis.title.x    = element_text(size = 25))





runoff_df=subset(my_data_ts,my_data_ts$variable=="Runoff")

g2 <- ggplot(runoff_df, aes(date, value))+
  geom_line(color="red") +
  ylab("Runoff (mm)")  + xlab("Date")+
  theme_bw() +
  theme(axis.title.y = element_text(size = 25),axis.title.x    = element_text(size = 25),
        axis.text.x     = element_text(size = 25),
        axis.ticks.x    = element_text(size = 25),
        axis.ticks.y    = element_text(size = 25))


g2 <- ggplot(runoff_df, aes(date, value))+
  geom_line(color="red") +
  ylab("Runoff (mm)")  + xlab("Date")+ 
  theme_bw() +
  theme(axis.title.y = element_text(size = 16),axis.title.x    = element_text(size = 25))
# g1 <- ggplot_gtable(ggplot_build(g1))
# g2 <- ggplot_gtable(ggplot_build(g2))
# maxWidth = unit.pmax(g1$widths[2:3], g2$widths[2:3])
# 
# g1$widths[2:3] <- maxWidth
# g2$widths[2:3] <- maxWidth
g3=grid.arrange(g1, g2,ncol=1)

g3=grid.arrange(g1, g2, ncol = 1, heights = c(1, 3))

ggsave("Hydrograph.jpeg",g3,dpi = 500)


ggsave("Hydrograph3.jpeg",g3,dpi = 500,width=15,height=15)


getwd()

Temp_df=subset(my_data_ts,my_data_ts$variable=="Temperature")

d1 <- ggplot(Temp_df, aes(date, value)) +
  geom_bar(stat = 'identity', fill = "blue") +
  theme_bw() +
  ylab("Temperature (C)") + xlab("")+
  labs(title = "") +
  scale_y_reverse()+
  theme(axis.title.y = element_text(size = 16),axis.title.x    = element_blank(),
        axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank())
  
 
runoff_df=subset(my_data_ts,my_data_ts$variable=="Runoff")

L2 <- ggplot(runoff_df, aes(date, value))+
  geom_line(color="red") +
  ylab("Runoff (mm)")  + xlab("Date")+
  theme_bw() +
  theme(axis.title.y = element_text(size = 16),axis.title.x    = element_blank(),
        axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank())
d1 <- ggplot_gtable(ggplot_build(d1))
L2 <- ggplot_gtable(ggplot_build(L2))
maxWidth = unit.pmax(d1$widths[2:3], L2$widths[2:3])
# 
d1$widths[2:3] <- maxWidth
L2$widths[2:3] <- maxWidth
g4=grid.arrange(d1, L2,ncol=1)

g3=grid.arrange(d1, L2, ncol = 1, heights = c(1, 3))

ggsave("Hydrograph.jpeg",g3,dpi = 500)


Preci_df=subset(my_data_ts,my_data_ts$variable=="Precipitation")

c1 <- ggplot(Preci_df, aes(date, value)) +
  geom_bar(stat = 'identity', fill = "blue") +
  theme_bw() +
  ylab("Ppt (mm)") + xlab("")+
  labs(title = "") +
  scale_y_reverse()+
  theme(axis.title.y = element_text(size = 16),axis.title.x    = element_blank(),
        axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank())


runoff_df=subset(my_data_ts,my_data_ts$variable=="Runoff")

G2 <- ggplot(runoff_df, aes(date, value))+
  geom_line(color="red") +
  ylab("Runoff (mm)")  + xlab("Date")+
  theme_bw() +
  theme(axis.title.y = element_text(size = 16))
c1 <- ggplot_gtable(ggplot_build(c1))
G2 <- ggplot_gtable(ggplot_build(G2))
maxWidth = unit.pmax(c1$widths[2:3], G2$widths[2:3])
# 
c1$widths[2:3] <- maxWidth
G2$widths[2:3] <- maxWidth

c3=grid.arrange(c1, G2, ncol = 1, heights = c(1, 3))

ggsave("hydrograph5.jpeg",c3,dpi = 500)




g1 <- ggplot_gtable(ggplot_build(g1))
# g2 <- ggplot_gtable(ggplot_build(g2))
# maxWidth = unit.pmax(g1$widths[2:3], g2$widths[2:3])
# 
# g1$widths[2:3] <- maxWidth
# g2$widths[2:3] <- maxWidth
g3=grid.arrange(g1, g2,ncol=1)


bbnk= filter(Runoff_ts,year %in% c("2020"))
ppbt=filter(ppt_ts,year %in% c("2020"))


c1 <- ggplot(ppbt, aes(date, ppt)) +
  geom_bar(stat = 'identity', fill = "blue") +
  theme_bw() +
  ylab("Ppt (mm)") + xlab("")+
  labs(title = "") +
  scale_y_reverse()+
  theme(axis.title.y = element_text(size = 16),axis.title.x    = element_blank(),
        axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank())


G2 <- ggplot(bbnk, aes(date, q))+
  geom_line(color="red") +
  ylab("Runoff (mm)")  + xlab("Date")+
  theme_bw() +
  theme(axis.title.y = element_text(size = 16))
c1 <- ggplot_gtable(ggplot_build(c1))
G2 <- ggplot_gtable(ggplot_build(G2))
maxWidth = unit.pmax(c1$widths[2:3], G2$widths[2:3])
# 
c1$widths[2:3] <- maxWidth
G2$widths[2:3] <- maxWidth

c3=grid.arrange(c1, G2, ncol = 1, heights = c(1, 3))

ggsave("hydrograph1990.jpeg",c3,dpi = 500)

rn= filter(Runoff_ts,year %in% c("2006"))
bt=filter(ppt_ts,year %in% c("2006"))


c1 <- ggplot(bt, aes(date, ppt)) +
  geom_bar(stat = 'identity', fill = "blue") +
  theme_bw() +
  ylab("Ppt (mm)") + xlab("")+
  labs(title = "") +
  scale_y_reverse()+
  theme(axis.title.y = element_text(size = 16),axis.title.x    = element_blank(),
        axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank())


G2 <- ggplot(rn, aes(date, q))+
  geom_line(color="red") +
  ylab("Runoff (mm)")  + xlab("Date")+
  theme_bw() +
  theme(axis.title.y = element_text(size = 16))
c1 <- ggplot_gtable(ggplot_build(c1))
G2 <- ggplot_gtable(ggplot_build(G2))
maxWidth = unit.pmax(c1$widths[2:3], G2$widths[2:3])
# 
c1$widths[2:3] <- maxWidth
G2$widths[2:3] <- maxWidth

c3=grid.arrange(c1, G2, ncol = 1, heights = c(1, 3))


### delete unwanted column by names
my_data_ts <- select(my_data_ts, -c(year,month))
plot(my_data_ts)
install.packages("scale")
library(scale)
# ------------------------------------------------------------------------------
# Visualize the package downloads
fig2.0=my_data_ts %>%
  ggplot(aes(x = date, y = value, color = variable)) +
  geom_point() +
  labs(title = "Variable trends", x = "month") +
  theme_bw()+
  facet_wrap(~ variable, ncol = 3, scale = "free_y") +
  #scale_color_continous() +
  theme(legend.position='none')
ggsave("fig2.0.jpeg",fig2.0,dpi = 500)

install.packages("stringi")
library(stringi)
library(tidyquant)

tq_transmute_fun_options()$xts %>%
  stringr::str_subset("^apply")

mean_my_data_ts_w <- my_data_ts %>%
  tq_transmute(
    select = value,
    mutate_fun = apply.yearly,
    FUN = mean,
    na.rm = TRUE,
    col_rename = "mean_value")

##mean_my_data_ts_w1 <- my_data_ts %>% group_by(variable,year) %>%
##mutate(mean=mean(value,na.rm=T))


fig2=mean_my_data_ts_w %>%
  ggplot(aes(x = date, y = mean_value, color = variable)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "Average yearly trend in variables", x = "Year",
       y = "Mean yearly value") +
  facet_wrap(~ variable, ncol = 3, scale = "free_y") +
  expand_limits(y = 0) +
  scale_color_tq() +
  theme_tq() +
  theme(legend.position="none")

ggsave("fig2.jpeg",fig2,dpi = 500)


mean_my_data_ts_w <- my_data_ts %>%
  tq_transmute(
    select= value,
    mutate_fun = apply.monthly,
    FUN = mean,
    na.rm = TRUE,
    col_rename = "mean_value"
  )
mean_my_data_ts_w

fig3=mean_my_data_ts_w %>%
  ggplot(aes(x = date, y = mean_value, color = variable)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "Average monthly trend in variables", x = "Year",
       y = "Mean monthly value") +
  facet_wrap(~ variable, ncol = 3, scale = "free_y") +
  expand_limits(y = 0) +
  scale_color_tq() +
  theme_tq() +
  theme(legend.position="none")

ggsave("fig3.jpeg",fig3,dpi = 500)

# ------------------------------------------------------------------------------
library(tidyquant) # Loads tidyverse, tidyquant, financial pkgs, xts/zoo
library(cranlogs) # For inspecting package downloads over time
library(corrr) # Tidy correlation tables and correlation plotting
library(cowplot) # Multiple plots with plot_grid()


my_data_ts %>%
  ggplot(aes(x = date, y = value, color = variable)) +
  geom_point() +
  labs(title = "Variable trednds", x = "") +
  facet_wrap(~ variable, ncol = 3, scale = "free_y") +
  scale_color_tq() +
  theme_tq() +
  theme(legend.position="none")



fig4=my_data_ts %>%
  ggplot(aes(x = date, y = value, color = variable)) +
  # Data
  geom_point(alpha = 0.5) +
  facet_wrap(~ variable, ncol = 3, scale = "free_y") +
  # Aesthetics
  labs(title = "Variable trends", x = "",
       subtitle = "1990-01-01 through 2020-12-31",
       caption = "Variable trend for hydrology data") +
  scale_color_tq() +
  theme_tq() +
  theme(legend.position="none")

ggsave("fig4.jpeg",fig4,dpi = 500)

fig5=my_data_ts %>%
  ggplot(aes(x = date, y = value)) +
  # Data
  geom_point(alpha = 0.5, color = palette_light()[[1]], size = 2) +
  # Aesthetics
  labs(title = "Variable trends", x = "",
       subtitle = "1990-01-01 through 2020-12-31",
       caption = "Download data via Tera Climate") +
  scale_y_continuous(labels = scales::comma) +
  theme_tq() +
  theme(legend.position="none")

ggsave("fig5.jpeg",fig5,dpi = 500)

# "run" functions from TTR
tq_mutate_fun_options()$TTR %>%
  stringr::str_subset("^run")


# If first arg is x (and no y) --> us tq_mutate()
args(runSD)

# If first two arguments are x and y --> use tq_mutate_xy()
args(runCor)


all_downloads <- cran_downloads(from = "1990-01-01", to = "2020-12-31") %>%
  tibble::as_tibble()

my_data_ts$year=NULL

# Correlation table
my_data_ts_correlations <- my_data_ts %>%
  # Data wrangling
  spread(key = variable, value = value) %>%
  select(-date) %>%
  # Correlation and formating
  correlate()

my_data_ts_correlations

# Pretty printing
my_data_ts_correlations %>%
  shave(upper = F)

# Network plot
gg_all <- my_data_ts_correlations %>%
  network_plot(colours = c(palette_light()[[2]], "white", palette_light()[[4]]), legend = TRUE) +
  labs(
    title = "Correlations of variables",
    subtitle = "Looking at January through December, tidyquant is a clear outlier"
  ) +
  expand_limits(x = c(-0.99, 0.1), y = c(-0.9, 0.1)) +
  theme_tq() +
  theme(legend.position = "bottom")
gg_all


#--------------------
##
library(tidyquant) # Loads tidyverse, tidyquant, financial pkgs, xts/zoo
library(cranlogs) # For inspecting package downloads over time
library(timetk) # For consistent time series coercion functions
library(stringr) # Working with strings
library(forcats) # Working with factors/categorical data



my_data_ts %>%
  ggplot(aes(x = date, y = value, color = variable)) +
  geom_point() +
  labs(title = "Variable trednds", x = "") +
  facet_wrap(~ variable, ncol = 3, scale = "free_y") +
  scale_color_tq() +
  theme_tq() +
  theme(legend.position="none")


my_data_ts %>%
  ggplot(aes(x = date, y = value, color = variable)) +
  # Data
  geom_point(alpha = 0.5) +
  facet_wrap(~ variable, ncol = 3, scale = "free_y") +
  # Aesthetics
  labs(title = "Variable trednds", x = "",
       subtitle = "1990-01-01 through 2020-12-31",
       caption = "Download data via MODIS, TRMM and Terraclim") +
  scale_color_tq() +
  theme_tq() +
  theme(legend.position="none")


# Use tq_mutate() to get lags 1:28 using lag.xts()

k <- 1:48
col_names <- paste0("lag_", k)

tidyverse_lags <- my_data_ts %>%
  tq_mutate(
    select = value,
    mutate_fun = lag.xts,
    k = 1:48,
    col_rename = col_names
  )
tidyverse_lags


# Calculate the autocorrelations and 95% cutoffs
tidyverse_count_autocorrelations <- tidyverse_lags %>%
  gather(key = "lag", value = "lag_value", -c(variable, date, value)) %>%
  mutate(lag = str_sub(lag, start = 5) %>% as.numeric) %>%
  group_by(variable, lag) %>%
  summarize(
    cor = cor(x = value, y = lag_value, use = "pairwise.complete.obs"),
    cutoff_upper = 2/(n())^0.5,
    cutoff_lower = -2/(n())^0.5
  )
tidyverse_count_autocorrelations



tidyverse_count_autocorrelations %>%
  ggplot(aes(x = lag, y = cor, color = variable, group = variable)) +
  # Add horizontal line a y=0
  geom_hline(yintercept = 0) +
  # Plot autocorrelations
  geom_point(size = 2) +
  geom_segment(aes(xend = lag, yend = 0), size = 1) +
  # Add cutoffs
  geom_line(aes(y = cutoff_upper), color = "blue", linetype = 2) +
  geom_line(aes(y = cutoff_lower), color = "black", linetype = 2) +
  # Add facets
  facet_wrap(~ variable, ncol = 3) +
  # Aesthetics
  expand_limits(y = c(-1, 1)) +
  scale_color_tq() +
  theme_tq() +
  labs(
    title = paste0("Tidyverse ACF Plot: Lags ", rlang::expr_text(k)),
    subtitle = "",
    x = "Lags"
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# Get the absolute autocorrelations
tidyverse_absolute_autocorrelations <- tidyverse_count_autocorrelations %>%
  ungroup() %>%
  mutate(
    lag = as_factor(as.character(lag)),
    cor_abs = abs(cor)
  ) %>%
  select(lag, cor_abs) %>%
  group_by(lag)
tidyverse_absolute_autocorrelations



# Visualize boxplot of absolute autocorrelations
break_point <- 1.5*IQR(tidyverse_absolute_autocorrelations$cor_abs) %>% signif(3)
tidyverse_absolute_autocorrelations %>%
  ggplot(aes(x = fct_reorder(lag, cor_abs, .desc = TRUE) , y = cor_abs)) +
  # Add boxplot
  geom_boxplot(color = palette_light()[[1]]) +
  # Add horizontal line at outlier break point
  geom_hline(yintercept = break_point, color = "red") +
  annotate("text", label = paste0("Outlier Break Point = ", break_point),
           x = 11, y = break_point + .03, color = "red") +
  # Aesthetics
  expand_limits(y = c(0, 1)) +
  theme_tq() +
  labs(
    title = paste0("Absolute Autocorrelations: Lags ", rlang::expr_text(k)),
    subtitle = "Yearly patterns are consistently above outlier break point",
    x = "Lags"
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



# Bummer, man!
my_data_ts %>%
  lag.xts(k = 1:1116)


# Success! Got our lags 1 through 5. One problem: no original values
my_data_ts %>%
  tk_xts(silent = TRUE) %>%
  lag.xts(k = 1:12)


# Convert to xts
my_time_series_tbl <- my_data_ts %>%
  tq_mutate(
    select = value,
    mutate_fun = lag.xts,
    k = 1:12
  )


# Convert to xts
my_time_series_xts <- my_time_series_tbl%>%
  tk_xts(silent = TRUE)

# Get original values and lags in xts
my_lagged_time_series_xts <-
  merge.xts(my_time_series_xts, lag.xts(my_time_series_xts, k = 1:5))

# Convert back to tbl
my_lagged_time_series_xts %>%
  tk_tbl()



# Get original values and lags in xts
my_lagged_time_series_xts <-
  merge.xts(my_time_series_xts, lag.xts(my_time_series_xts, k = 1:5))

# Convert back to tbl
my_lagged_time_series_xts %>%
  tk_tbl()



# This is nice, we didn't need to coerce to xts and it merged for us
my_data_ts %>%
  tq_mutate(select = value,mutate_fun = lag.xts, k= 1:12 )



## finding the skewness of a data
library(rgdal)
library(moments)
library(ggpubr)


## check data skewness
hist(my_data$Precipitation, na.rm = TRUE)
hist(my_data$Runoff, na.rm = TRUE)
hist(my_data$Temperature)

skewness(my_data$Precipitation, na.rm = TRUE)

skewness(my_data$Temperature, na.rm = TRUE)
skewness(my_data$Runoff,na.rm = TRUE)

### transform temp and precipiatation data using sqrt transformation
my_data$Runoff.t <- sqrt(my_data$Runoff)
my_data$Precipitation.t<-log(my_data$Precipitation)
hist(my_data$Precipitation.t)

my_data$Precipitation.t <- sqrt(my_data$Precipitation)
hist(my_data$Precipitation.t)

my_data$Runoff.t2 <- log(my_data$Runoff)

hist(my_data$Runoff.t)
hist(my_data$Runoff.t2)

shapiro.test(my_data$Runoff)
shapiro.test(my_data$Runoff.t)
my_data2=subset(my_data,is.finite(my_data$Runoff.t2))
shapiro.test(my_data2$Runoff.t2)

#my_data$Precipitation.t <- sqrt(my_data$Precipitation)
my_data$Temperature.t <- sqrt(max(my_data$Temperature+1) - my_data$Temperature)


## delete column from dataframe
my_data.t <- subset(my_data, select = -c(Precipitation,Temperature))


## plot time series
ts_plot(my_data.t,
        title = "Timeseries of EVI, NDVI, KNDVI, Precipiatation and Temperature from 2000 to 2019",
        Xtitle = "Time")


ts_plot(new_data,
        title = "EVI timeseries for Oban forest between 2000 and 2020",
        Xtitle = "Time",
        Ytitle = "EVI value")

ts_plot(ndvi_ts)


install.packages("TSstudio")
library(TSstudio)

ts_info(evi_ts)

library(xts)

evi <- xts(df[,-1], order.by=as.Date(df[,1], "%m/%d/%Y"))
#
# daily returns
#
returns <- diff(stocks, arithmetic=FALSE ) - 1
#
# weekly open, high, low, close reports
#
to.weekly(stocks$Hero_close, name="Hero")



#########
########
#######
library(raster)
library(rgdal)

# List tif files
Files <- list.files(pattern = ".tif")

# Read shapefile with quadrats
uav <- readOGR("shapefile.shp")

# Name your output variable
# Raster Layer requires one value
# For RasterStack or Raster Brick, a value per layer
VAR = "evi"

# List collecting results of every run
DF <- list()

# Collecting information in a loop
for(i in all_tiffs) DF[[i]] <- {
  r <- raster(i)
  names(r) <- VAR # set a standard name for the layer
  imgext <- extract(x = r,  df = TRUE)
  # Discard NA values
  imgext <- imgext[!is.na(imgext[ , VAR]),]
  imgext
}

# Since all data frames have the same columns
# you can call 'rbind()'
DF <- do.call(rbind, DF)


pixel_test <- extract(x=ppt.mask)


ggplot() +
  geom_histogram(data = ppt.mask, aes(ppt.mask)))



`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.




install.packages("tidyquant")
install.packages("cranlogs")
install.packages("tidyverse")

library(tidyverse)
library(tidyquant)  # Loads tidyverse, tidquant, financial pkgs, xts/zoo
library(cranlogs)   # For inspecting package downloads over time

# Various tidyverse packages corresponding to my stickers :)
pkgs <- c(
  "tidyr", "lubridate", "dplyr",
  "broom", "tidyquant", "ggplot2", "purrr",
  "stringr", "knitr"
)

tidyverse_downloads <- cran_downloads(
  packages = pkgs,
  from     = "2017-01-01",
  to       = "2017-06-30") %>%
  tibble::as_tibble() %>%
  group_by(package)

tidyverse_downloads


# Visualize the package downloads
tidyverse_downloads %>%
  ggplot(aes(x = date, y = count, color = package)) +
  geom_point() +
  labs(title = "tidyverse packages: Daily downloads", x = "") +
  facet_wrap(~ package, ncol = 3, scale = "free_y") +
  scale_color_tq() +
  theme_tq() +
  theme(legend.position="none")


# "apply" functions from xts
tq_transmute_fun_options()$xts %>%
  stringr::str_subset("^apply")

mean_tidyverse_downloads_w <- tidyverse_downloads %>%
  tq_transmute(
    select     = count,
    mutate_fun = apply.weekly,
    FUN        = mean,
    na.rm      = TRUE,
    col_rename = "mean_count"
  )
mean_tidyverse_downloads_w


mean_tidyverse_downloads_w %>%
  ggplot(aes(x = date, y = mean_count, color = package)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "tidyverse packages: Average daily downloads by week", x = "",
       y = "Mean Daily Downloads by Week") +
  facet_wrap(~ package, ncol = 3, scale = "free_y") +
  expand_limits(y = 0) +
  scale_color_tq() +
  theme_tq() +
  theme(legend.position="none")

