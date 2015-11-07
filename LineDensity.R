# calculates length of all lines for each polygon
# example: railroad line (density) for each US state

library(rgeos)
library(rgdal)
library(maptools)

st <- readOGR("data", "US_States_NE_aea")
rr <- readOGR("data", "US_railroads_NE_aea")
path.clp <- gIntersection(st, rr, byid = TRUE) # clip lines to each polygon
path.lgth <- gLength(path.clp, byid = TRUE) # calculate length

# add up the length over polys
poly.id <- sapply(strsplit (names(path.lgth), " "), "[[", 1)
dfr.lgth <- data.frame(path.lgth, polyID=poly.id)
dfr.lgthsum <- aggregate(. ~ polyID, dfr.lgth, sum)

# merge back to a data frame that has *all* polyIDs, so we get the NAs
poly.all <- data.frame(polyID = row.names(st))
dfr.all <- merge (poly.all, dfr.lgthsum, by="polyID", all.x = TRUE)

# reorder for the join
idx <- order(as.numeric(as.character(dfr.all$polyID)))
dfr.all <- dfr.all[idx,]

# join dataframe back to polys
row.names(dfr.all) <- dfr.all$polyID
st.ll <- spCbind(st, dfr.all)

# for density plot normalize by area
st.ll@data$line.dens <- st.ll@data$path.lgth/st.ll@data$area_sqkm
library(classInt)
library(RColorBrewer)
brks <-  classIntervals(st.ll@data$line.dens, n = 5, style = "quantile")$brks #will warn if there are NAs..
brks[length(brks)] <- brks[length(brks)] + 1
pal  <- brewer.pal(5, "Reds")
spplot(st.ll, "line.dens", at = brks, col.regions=pal) 
