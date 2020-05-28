remove(list = ls())

####loading package####
load_package<-function(x){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
  }
  require(x,character.only=TRUE)
}

load_package(spacetime)
load_package(xts)
load_package(dplyr)
load_package(tidyr)
load_package(RandomFields)
load_package(sf)
load_package(ggplot2)
load_package(ggmap)
load_package(readxl)
load_package(animation)
load_package(sp)
load_package(geobr)
load_package(grid)
load_package(gridExtra)
load_package(fields)
load_package(RColorBrewer)
load_package(ape)
load_package(broom)
load_package(FRK)
load_package(purr)
load_package(lattice)
load_package(stargazer)
load_package(lmtest)
load_package(gstat)
load_package(spData)
load_package(spdep)
load_package(RANN)
load_package(raster) 
load_package(rasterVis)
load_package(GISTools)
load_package(mapview)
load_package(mapdata)
load_package(magrittr)
load_package(osmdata)
load_package(STRbook)
load_package(RColorBrewer)
load_package(lubridate) 
load_package(INLA) 
load_package(IDE) 
load_package(STRbook)
load_package(sievetest)
load_package(tigris)
load_package(coda)

set.seed(1)


#####  DATA COVID-19 ------

br_covid <- read.csv("C:*****/caso_full.csv")
br_covid$date <- as.Date(br_covid$date)
summary(br_covid)

sp_covid <- subset(br_covid, br_covid$state == "SP")

##### DATA BRAZILIAN CITIES ------

br_mun <- st_read("C:*****/BRMUE250GC_SIR.shp")
br_mun <- st_transform(br_mun, crs = 4326)


# FILTERING CITIES IN SAO PAULO STATE
starts_with <- function(vars, match, ignore.case = TRUE) {
  if (ignore.case) match <- tolower(match)
  n <- nchar(match)
  
  if (ignore.case) vars <- tolower(vars)
  substr(vars, 1, n) == match
}

sp_mun <- subset(br_mun, starts_with(br_mun$CD_GEOCMU,"35"))
sp_mun <- st_transform(sp_mun, crs = 4326)

locs <- st_coordinates(st_centroid(sp_mun))
sp_mun["X"] <- locs[,1]
sp_mun["Y"] <- locs[,2]
sp_mun_df <- data.frame(sp_mun)
locs_sp <- subset(sp_mun_df, select = c(NM_MUNICIP, CD_GEOCMU, X, Y))

ggplot(sp_mun) + geom_sf()


##### DATA BRAZILIAN STATES -----

br_sta <- st_read("C:*****/BRUFE250GC_SIR.shp")
br_sta <- st_transform(br_sta, crs = 4326)
summary(br_sta)

br_covid_sta <- merge(br_sta, br_covid, by.x = "CD_GEOCUF", by.y = "city_ibge_code")
utd_cases_sta <- subset(br_covid_sta, br_covid_sta$date == max(br_covid_sta$date))
utd_cases_sta <- st_as_sf(utd_cases_sta)  


plot(utd_cases_sta["last_available_confirmed"], main = paste(format(max(sp_covid$date), "%d/%m/%Y")), 
     key.pos = 4, breaks = "jenks")


##### DATA POPULATION DENSITY -------

br_pop_hab <- read_excel("C:*****/br_pop_hab.xlsx")
sp_pop_hab <- subset(br_pop_hab, starts_with(br_pop_hab$Codigo,"35"))

##### BALANCING PANEL TIME X CITIES ------

aux_date <- rep(min(sp_covid$date):max(sp_covid$date), length(sp_mun$CD_GEOCMU))
aux_date <- sort(as.Date(aux_date))

total_days <- as.integer(difftime(max(sp_covid$date), min(sp_covid$date), unit = "days") + 1)
aux_muns <- data.frame(rep(sp_mun$CD_GEOCMU, total_days))

aux1_df <- data.frame(aux_date, aux_muns)
colnames(aux1_df) <- c("date", "CD_GEOCMU")
aux1_df <- arrange(aux1_df, CD_GEOCMU)

aux2_df <- merge(aux1_df, sp_mun, by.x = "CD_GEOCMU", by.y = "CD_GEOCMU")
colnames(sp_covid)[which(names(sp_covid) == "city_ibge_code")] <- "CD_GEOCMU"

aux3_df <- merge(aux2_df, sp_pop_hab, by.x = "CD_GEOCMU", by.y = "Codigo")

sp_covid_bal <- merge.data.frame(aux3_df, sp_covid, by = c("date", "CD_GEOCMU") , all.x = TRUE)

sp_covid_bal_df <- subset(as.data.frame(sp_covid_bal), select = - geometry)
sp_covid_bal_sf <- st_as_sf(sp_covid_bal)

remove (aux1_df, aux2_df, aux3_df, aux_muns)

# UP TO DATE INFORMATION
utd_cases_mun <- subset(sp_covid_bal_sf, sp_covid_bal$date == max(sp_covid_bal$date))
utd_cases_mun_sf <- st_as_sf(utd_cases_mun)

plot(utd_cases_mun_sf["last_available_confirmed_per_100k_inhabitants"], main = paste(format(max(sp_covid$date), "%d/%m/%Y")), 
     key.pos = 4, breaks = "jenks")

plot(utd_cases_mun_sf["ldens_hab"], main = paste(format(max(sp_covid$date), "%d/%m/%Y")), 
     key.pos = 4)

##### DATE ADJUSTMENTS -----

sp_covid_bal$date <- as.Date(sp_covid_bal$date)
sp_covid_bal_df$date <- as.Date(sp_covid_bal_df$date)
sp_covid_bal_df <- arrange(sp_covid_bal_df, date)

##### SAO PAULO STATE GEOMETRY ------

sp_sta <- st_read("C:*****/LimiteEstadualPolygon.shp")
sp_sta <- st_transform(sp_sta, crs = 4326)
sp_sta <- subset(sp_sta, select = - UF)

##### SOME PLOTS -----

colfunc <- colorRampPalette(c("blue4", "blue","blueviolet", "darkorchid", "deeppink", "hotpink", "salmon", "sandybrown", "gold", "yellow"))
sp_covid_sub <- subset(sp_covid_bal_df, sp_covid_bal$date %in% as.Date(c("2020-03-12", "2020-04-12","2020-05-12")))

plot1 <-  ggplot(sp_covid_sub) + geom_point(aes(x = X, y = Y, colour = last_available_confirmed), size = 1) + 
          xlab("Longitude") + ylab("Latitude") + 
          geom_sf(data = sp_sta, inherit.aes = FALSE, alpha = 0.1) +
          facet_grid(~date) +
          theme_bw() + 
          scale_colour_gradientn(colors = colfunc(12), na.value = "transparent", limits=c(1, 500))
plot1

IDs_sub <- list(3550308, 3518800, 3534401)  # 3 importante cities
sp_covid_sub <- filter(sp_covid_bal_df, (CD_GEOCMU %in% IDs_sub) & !is.na(last_available_confirmed_per_100k_inhabitants))

plot2 <-  ggplot(sp_covid_sub) + geom_line(aes(x = date, y = last_available_confirmed_per_100k_inhabitants)) +
          facet_wrap(~NM_MUNICIP, ncol = 5) +
          xlab("Date") +
          ylab("Confirmed per 100k inhab") +
          theme_bw() +
          theme(panel.spacing = unit(1, "lines"))
plot2

##### MORAN'S LOCAL I ----

utd_cases_mun_isl <- utd_cases_mun[! utd_cases_mun$CD_GEOCMU == 3520400,]   # removing Ilhabela (an island)
utd_cases_mun_isl <- subset(utd_cases_mun_isl, select = last_available_confirmed_per_100k_inhabitants)
utd_cases_mun_isl$last_available_confirmed_per_100k_inhabitants[is.na(utd_cases_mun_isl$last_available_confirmed_per_100k_inhabitants)] <- 0 

sp_viz <- poly2nb(as(utd_cases_mun_isl, "Spatial"))
ww <- nb2listw(sp_viz, style ='W', zero.policy = TRUE)

locMoran <- localmoran(utd_cases_mun_isl$last_available_confirmed_per_100k_inhabitants, ww)
sids.shade <- auto.shading(c(locMoran[,1],-locMoran[,1]), cols=brewer.pal(5,"PRGn"))
choropleth(utd_cases_mun_isl, locMoran[,1], shading=sids.shade)
choro.legend(-46.5, -20, sids.shade,fmt="%6.2f")
title(main = paste(format(max(sp_covid$date), "%d/%m/%Y")), cex.main=1)

##### KRIGING ----

utd_cases_mun_sp <- as(utd_cases_mun, "Spatial")

grdpts <- makegrid(utd_cases_mun_sp, cellsize =  0.07)
spgrd <- SpatialPoints(grdpts, proj4string = CRS(proj4string(utd_cases_mun_sp)))
spgrdWithin <- SpatialPixels(spgrd[utd_cases_mun_sp,])

plot(utd_cases_mun_sp)
plot(spgrdWithin,  add = T)

# CALCULATING THE DISTANCE FROM WHERE THE FIRST CONFIRMED CASE WAS REPORTED (ALBERT EINSTEIN'S HOSPITAL)
distance <- vector()
lat_first = -46.7152547
lon_first = -23.5999606

for(i in 1:length(spgrdWithin)){
  lat = spgrdWithin$x1[i]
  lon = spgrdWithin$x2[i]
  dlat = lat_first - lat
  dlon = lon_first - lon
  aux <- sqrt(dlat*dlat + dlon*dlon)
  distance[i] <- as.numeric(aux)
}
spgrdWithin$dist <- distance

distance_utd <- vector()

for(i in 1:nrow(utd_cases_mun)){
  lat = utd_cases_mun$X[i]
  lon = utd_cases_mun$Y[i]
  dlat = lat_first - lat
  dlon = lon_first - lon
  aux <- sqrt(dlat*dlat + dlon*dlon)
  distance_utd[i] <- as.numeric(aux)
}
utd_cases_mun$dist <- distance_utd

# INCLUDING VARIABLES IN THE GRID
pnts_sf <- st_as_sf(spgrdWithin, coords = c('x1', 'x2'), crs = st_crs(utd_cases_mun_sf))
intersection = as.integer(st_intersects(pnts_sf, utd_cases_mun_sf))

spgrdWithin$CD_GEOCMU <- utd_cases_mun_sf$CD_GEOCMU[intersection]
spgrdWithin$NM_MUNICIP <- utd_cases_mun_sf$NM_MUNICIP[intersection]
spgrdWithin$poverty <- utd_cases_mun_sf$poverty[intersection]
spgrdWithin$ldens_hab <- utd_cases_mun_sf$ldens_hab[intersection]
gridded(spgrdWithin) = TRUE

# ORDINARY KRIGING 
utd_cases_mun_sna <- utd_cases_mun[! is.na(utd_cases_mun$last_available_confirmed_per_100k_inhabitants),]
utd_cases_mun_sna_sp <- as(utd_cases_mun_sna, "Spatial")

vg <- variogram(last_available_confirmed_per_100k_inhabitants ~ 1, utd_cases_mun_sna)
f.vg.exp <- fit.variogram(vg, vgm("Exp", 30, 400))
plot(vg, f.vg.exp)
confirmed_kriged = krige(last_available_confirmed_per_100k_inhabitants ~ 1, 
                         utd_cases_mun_sna_sp, spgrdWithin, model = f.vg.exp)


# UNIVERSAL KRIGING

# MODEL 1
vg1 <- variogram(last_available_confirmed_per_100k_inhabitants ~ 1 + dist, utd_cases_mun_sna)
f.vg1.exp <- fit.variogram(vg1, vgm("Sph", 30, 400))
plot(vg1, f.vg1.exp)
confirmed_kriged_univ1 = krige(last_available_confirmed_per_100k_inhabitants ~ 1 + dist, 
                               utd_cases_mun_sna_sp, spgrdWithin, model = f.vg1.exp)

# MODEL 2
vg2 <- variogram(last_available_confirmed_per_100k_inhabitants ~ 1 + dist + ldens_hab, utd_cases_mun_sna)
f.vg2.exp <- fit.variogram(vg2, vgm("Sph", 30, 400))
plot(vg2, f.vg2.exp)
confirmed_kriged_univ2 = krige(last_available_confirmed_per_100k_inhabitants ~ 1 + dist + ldens_hab, 
                               utd_cases_mun_sna_sp, spgrdWithin, model = f.vg2.exp)


# PLOTS
grid.arrange(
spplot(confirmed_kriged["var1.pred"], main = "Ordinary Kriging - Prediction"),
spplot(confirmed_kriged_univ1["var1.pred"], main = "Universal Kriging (1) - Prediction"),
spplot(confirmed_kriged_univ2["var1.pred"], main = "Univeral Kriging (2) - Prediction"),
spplot(confirmed_kriged["var1.var"], main = "Ordinary Kriging - Variance"),
spplot(confirmed_kriged_univ1["var1.var"], main = "Universal Kriging (1) - Variance"),
spplot(confirmed_kriged_univ2["var1.var"], main = "Universal Kriging (2) - Variance"),
ncol = 3
)

# CROSS VALIDATION

locs_cv <- st_coordinates(st_centroid(utd_cases_mun_sna))
utd_cases_mun_sna["X"] <- locs_cv[,1]
utd_cases_mun_sna["Y"] <- locs_cv[,2]
utd_cases_mun_sna_df <- data.frame(utd_cases_mun_sna)
coordinates(utd_cases_mun_sna_df) = ~X+Y

confirmed_kriged_cv <- krige.cv(last_available_confirmed_per_100k_inhabitants ~ 1, 
                                utd_cases_mun_sna_df, model = f.vg.exp, nmax = 40, nfold = nrow(utd_cases_mun_sna_df))
paste("Ordinary Kriging: RMSE = ", round(sqrt(mean(confirmed_kriged_cv$residual^2)), 3))

confirmed_kriged_univ1_cv <- krige.cv(last_available_confirmed_per_100k_inhabitants ~ 1 + dist, 
                                      utd_cases_mun_sna_df, model = f.vg.exp, nmax = 40, nfold = nrow(utd_cases_mun_sna_df))
paste("Universal Kriging - Model 1: RMSE = ", round(sqrt(mean(confirmed_kriged_univ1_cv$residual^2)), 3))


confirmed_kriged_univ2_cv <- krige.cv(last_available_confirmed_per_100k_inhabitants ~ 1 + dist + ldens_hab, 
                                      utd_cases_mun_sna_df, model = f.vg.exp, nmax = 40, nfold = nrow(utd_cases_mun_sna_df))
paste("Universal Kriging - Model 2: RMSE = ", round(sqrt(mean(confirmed_kriged_univ2_cv$residual^2)), 3))


##### BAYESIAN KRIGING ---------------

library(spBayes)

y <- utd_cases_mun_sna_sp$last_available_confirmed_per_100k_inhabitants
X1 <- utd_cases_mun_sna_sp$dist
X2 <- utd_cases_mun_sna_sp$ldens_hab
X3 <- utd_cases_mun_sna_sp$poverty

coords <- coordinates(utd_cases_mun_sna_sp)

n.samples <- 1000
starting <- list("phi"=3/0.5, "sigma.sq"=50, "tau.sq"=1)
tuning <- list("phi"=0.1, "sigma.sq"=0.1, "tau.sq"=0.1)
tuning <- list("phi"=1, "sigma.sq"=0.1, "tau.sq"=0.1)
priors <- list("beta.Flat", "phi.Unif"=c(3/1, 3/0.1),
               "sigma.sq.IG"=c(2, 5), "tau.sq.IG"=c(2, 5))
cov.model <- "exponential"


m.1 <- spLM(y ~ X1 + 1, coords=coords, starting=starting, tuning=tuning,
            priors=priors, cov.model=cov.model, n.samples=n.samples)

m.1.pred <- spPredict(m.1,
                      pred.covars=cbind(rep(1, length(spgrdWithin)), spgrdWithin$dist),
                      pred.coords=coordinates(spgrdWithin),
                      start=0.5*n.samples)

y.1.hat <- apply(m.1.pred$p.y.predictive.samples, 1, mean)
y.1.hat.df <- data.frame(cbind(y.1.hat, coordinates(spgrdWithin)))
coordinates(y.1.hat.df) = ~x1+x2
gridded(y.1.hat.df) = TRUE

m.2 <- spLM(y ~ X1 + X2 + 1, coords=coords, starting=starting, tuning=tuning,
            priors=priors, cov.model=cov.model, n.samples=n.samples)

m.2.pred <- spPredict(m.2,
                      pred.covars=cbind(rep(1, length(spgrdWithin)), spgrdWithin$dist, spgrdWithin$ldens_hab),
                      pred.coords=coordinates(spgrdWithin),
                      start=0.5*n.samples)

y.2.hat <- apply(m.2.pred$p.y.predictive.samples, 1, mean)
y.2.hat.df <- data.frame(cbind(y.2.hat, coordinates(spgrdWithin)))
coordinates(y.2.hat.df) = ~x1+x2
gridded(y.2.hat.df) = TRUE

y.1.hat.sd <- apply(m.1.pred$p.y.predictive.samples, 1, var)
y.2.hat.sd <- apply(m.2.pred$p.y.predictive.samples, 1, var)
y.1.hat.sd.df <- data.frame(cbind(y.1.hat.sd, coordinates(spgrdWithin)))
y.2.hat.sd.df <- data.frame(cbind(y.2.hat.sd, coordinates(spgrdWithin)))

coordinates(y.1.hat.sd.df) = ~x1+x2
gridded(y.1.hat.sd.df) = TRUE

coordinates(y.2.hat.sd.df) = ~x1+x2
gridded(y.2.hat.sd.df) = TRUE

grid.arrange(
  spplot(y.1.hat.df, main = "Bayesian Kriging (1) - Prediction"),
  spplot(y.2.hat.df, main = "Bayesian Kriging (2) - Prediction"),
  spplot(y.1.hat.sd.df, main = "Bayesian Kriging (1) - Variance"),
  spplot(y.2.hat.sd.df, main = "Bayesian Kriging (2) - Variance"),
  ncol = 2
)


#### SPATIO-TEMPORAL ANALYSIS -----------

sp_covid_bal_df<- data.frame(sp_covid_bal)
sp_covid_bal_df$last_available_confirmed_per_100k_inhabitants[is.na(sp_covid_bal_df$last_available_confirmed_per_100k_inhabitants)] <- 0
summary(sp_covid_bal_df)

spat_part <- SpatialPoints(coords = locs[, c("X", "Y")])
temp_part <- as.Date(unique(aux_date))

STObj2 <- STFDF(sp = spat_part,
                time = temp_part,
                data = sp_covid_bal_df)

proj4string(STObj2) <- CRS("+proj=longlat +ellps=WGS84")

vv <- variogram(last_available_confirmed_per_100k_inhabitants ~ 1, 
                data = STObj2,
                width = 30,
                cutoff = 300,
                tlags = 0.01:6.01)
plot(vv)

suppressWarnings(proj4string(STObj3) <- CRS(proj4string(STObj3)))
STObj3 <- STObj2[,c("2020-05-02", "2020-05-04", "2020-05-06",
                    "2020-05-08", "2020-05-10", "2020-05-12")]
STObj3 <- subset(STObj3, !is.na(STObj3$last_available_confirmed_per_100k_inhabitants))
summary(STObj3)

sumMetricVgm <- vgmST(stModel = "separable",
                      space = vgm(1, "Sph", 4000, nugget = 0.1),
                      time = vgm(1, "Exp", 100, nugget = 0.1),
                      sill = 20)
sepVgm <- fit.StVariogram(vv, sumMetricVgm)
sepVgm
plot(vv, sepVgm, map=F)

gridded(spgrdWithin) <- TRUE
spgrdWithin_pred <- STF(sp=as(spgrdWithin, "SpatialPoints"), time = STObj3@time)
suppressWarnings(proj4string(spgrdWithin_pred) <- CRS(proj4string(STObj3)))

krige_sp <- krigeST(last_available_confirmed_per_100k_inhabitants ~ 1,
                    data = STObj3,
                    newdata = spgrdWithin_pred,
                    modelList = sumMetricVgm,
                    computeVar = TRUE)
summary(krige_sp)
gridded(krige_sp@sp) <- TRUE

stplot(krige_sp[, ,"var1.pred"]) 
stplot(krige_sp[, ,"var1.var"])


