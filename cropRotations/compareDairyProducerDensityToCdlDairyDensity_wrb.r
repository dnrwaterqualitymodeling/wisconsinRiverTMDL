library(rgdal)
library(raster)
library(ggplot2)
library(classInt)
library(RColorBrewer)
library(classInt)
library(spatstat)
dir = "T:/Projects/Wisconsin_River/GIS_Datasets/Land_Management/Validations/DairyDensity_Comparisons"
wi = readOGR("T:/GIS/Statewide_Coverages/Political_Boundaries", "WI_State_Boundary")
wrb = readOGR("T:/Projects/Wisconsin_River/GIS_Datasets/Watersheds", "WRB_Basin")
plot_file1 = "T:/Projects/Wisconsin_River/Assessment_&_Analysis/Land_Mgt_Publication/Publication/Figures/dairy_producer_comparison_maps.png"
plot_file2 = "T:/Projects/Wisconsin_River/Assessment_&_Analysis/Land_Mgt_Publication/Publication/Figures/dairy_producer_comparison_scatterplots.png"
adjust = 0.1

# cafo = readOGR("T:/GIS/Statewide_Coverages/CAFOs/Facility_Locations", "CAFO_Facilities_November2011")
# cafo = cafo[-(92:108),]

#cafoDens = raster("CAFO_points_densities_2.img")
#cafoPts = readOGR("T:/GIS/Statewide_Coverages/DATCP_Dairy_Producers", "DATCP_MILK_PROD")
dairyProdPt = readOGR("T:/GIS/Statewide_Coverages/DATCP_Dairy_Producers", "DATCP_MILK_PROD")
#dairyProdFile = paste(dir, "DairyProducerDensity_Statewide.tif", sep="/")
dairyRastFile = "T:/Projects/Wisconsin_River/GIS_Datasets/Landcover/Rotations/CDLrotationsByCLU_Statewide.tif"
#dairyRastFile = paste(dir, "DairyPixelPoints_Density.img", sep="/")
#nondairyRastFile = paste(dir, "NonDairyPixelPoints_Density.img", sep="/")

#dairyProd = raster(dairyProdFile)
lc = raster(dairyRastFile)
lc_temp_file = tempfile(fileext=".img")
writeRaster(lc, lc_temp_file)
lc = raster(lc_temp_file)
dairyRast = getValues(lc)
dairyRast[dairyRast < 2] = NA
dairyRast[dairyRast > 3] = NA
dairyRast[dairyRast %in% 2:3] = 1
dairy_inds = which(dairyRast == 1)
dairy_inds_smpl = sample(dairy_inds, size=length(dairy_inds)/100)
rm(dairy_inds)
dairyRast_smpl = dairyRast
dairyRast_smpl[] = NA
dairyRast_smpl[dairy_inds_smpl] = 1
rm(dairy_inds_smpl)
dairyRast = setValues(lc, dairyRast_smpl)
rm(dairyRast_smpl)
dairyRast_pts = rasterToPoints(dairyRast)
rm(dairyRast)
dairyRast_ppp = ppp(
	x=coordinates(dairyRast_pts)[,1],
	y=coordinates(dairyRast_pts)[,2],
	window=owin(
		xrange=range(coordinates(dairyRast_pts)[,1]),
		yrange=range(coordinates(dairyRast_pts)[,2])
	)
)

nondairyRast = getValues(lc)
nondairyRast[nondairyRast <= 2] = 1
nondairyRast[nondairyRast %in% 2:3] = NA
nondairyRast[nondairyRast == 5] = 1
nondairyRast[nondairyRast > 5] = NA
nondairy_inds = which(nondairyRast == 1)
nondairy_inds_smpl = sample(nondairy_inds, size=length(nondairy_inds)/100)
rm(nondairy_inds)
nondairyRast_smpl = nondairyRast
nondairyRast_smpl[] = NA
nondairyRast_smpl[nondairy_inds_smpl] = 1
rm(nondairy_inds_smpl)
nondairyRast = setValues(lc, nondairyRast_smpl)
rm(nondairyRast_smpl)
nondairyRast_pts = rasterToPoints(nondairyRast)
rm(nondairyRast)
nondairyRast_ppp = ppp(
	x=coordinates(nondairyRast_pts)[,1],
	y=coordinates(nondairyRast_pts)[,2],
	window=owin(
		xrange=range(coordinates(nondairyRast_pts)[,1]),
		yrange=range(coordinates(nondairyRast_pts)[,2])
	)
)

dairyProdPt = ppp(
	x=coordinates(dairyProdPt)[,1],
	y=coordinates(dairyProdPt)[,2],
	window=owin(
		xrange=range(coordinates(dairyRast_pts)[,1]),
		yrange=range(coordinates(dairyRast_pts)[,2])
	)
)

#
#nonDairy = raster(nondairyRastFile)
#dairyProd = mask(dairyProd, wi)
#dairyRast = mask(dairyRast, wi)
#nonDairy = mask(nonDairy, wi)
#
#dairyProd[dairyProd == 0] = NA
#dairyRast[dairyRast == 0] = NA
#nonDairy[nonDairy == 0] = NA
#
#dairyProdPtWrb = dairyProdPt[!is.na(over(dairyProdPt, wrb)[,1]),]
#dairyProdWrb = trim(mask(dairyProd, wrb))
#dairyRastWrb = trim(mask(dairyRast, wrb))
#nonDairyWrb = trim(mask(nonDairy, wrb))

dairyRast_dens = density(dairyRast_ppp, adjust=adjust)
nondairyRast_dens = density(nondairyRast_ppp, adjust=adjust)
dairyProd_dens = density(dairyProdPt, adjust=adjust)

dairyRast_quad = quadratcount(
	dairyRast_ppp,
	nx = (max(dairyRast_ppp$x) - min(dairyRast_ppp$x)) / 10000,
	ny = (max(dairyRast_ppp$y) - min(dairyRast_ppp$y)) / 10000
)
nondairyRast_quad = quadratcount(
	nondairyRast_ppp,
	nx = (max(dairyRast_ppp$x) - min(dairyRast_ppp$x)) / 10000,
	ny = (max(dairyRast_ppp$y) - min(dairyRast_ppp$y)) / 10000
)
dairyProd_quad = quadratcount(
	dairyProdPt,
	nx = (max(dairyRast_ppp$x) - min(dairyRast_ppp$x)) / 10000,
	ny = (max(dairyRast_ppp$y) - min(dairyRast_ppp$y)) / 10000
)
#
#z = raster(
#	nrows = dim(dairyProd_quad)[1],
#	ncols = dim(dairyProd_quad)[2],
#	xmn = attr(dairyProd_quad, "tess")$window$xrange[1],
#	xmx = attr(dairyProd_quad, "tess")$window$xrange[2],
#	ymn = attr(dairyProd_quad, "tess")$window$yrange[1],
#	ymx = attr(dairyProd_quad, "tess")$window$yrange[2]
#)
#z = setValues(z, 1:length(dairyProd_quad))
#
#dairyRast_zone = zonal(dairyRast, z, fun="sum", na.rm=T)

model_data = data.frame(
	producer = as.vector(dairyProd_quad),
	dairy = as.vector(dairyRast_quad),
	nondairy = as.vector(nondairyRast_quad)
)
model_data[model_data$producer <= 0,] = NA

set.seed(10293)
smpl = sample(1:nrow(model_data), 0.9*nrow(model_data))
train_data = subset(
	model_data,
	1:nrow(model_data) %in% smpl
)
test_data = subset(
	model_data,
	!(1:nrow(model_data) %in% smpl)
)

dairyMod = lm(
	sqrt(dairy) ~ log(producer),
	data=train_data
)
nondairyMod = lm(
	sqrt(nondairy) ~ log(producer),
	data=train_data
)

test_data$dairy_pred = predict(dairyMod, test_data)
test_data$nondairy_pred = predict(nondairyMod, test_data)

template_grid = raster(
	nrows = dim(dairyProd_quad)[1],
	ncols = dim(dairyProd_quad)[2],
	xmn = attr(dairyProd_quad, "tess")$window$xrange[1],
	xmx = attr(dairyProd_quad, "tess")$window$xrange[2],
	ymn = attr(dairyProd_quad, "tess")$window$yrange[1],
	ymx = attr(dairyProd_quad, "tess")$window$yrange[2]
)
dairyProd_quad[dairyProd_quad == 0] = NA
dairyProd_cls = classIntervals(
	as.vector(log(dairyProd_quad)),
	6,
	style="quantile"
)
dairyProd_grid = setValues(template_grid, dairyProd_quad)
dairyRast_quad[dairyRast_quad == 0] = NA
dairyRast_cls = classIntervals(
	as.vector(sqrt(dairyRast_quad)),
	6,
	style="quantile"
)
dairyRast_quad = dairyRast_quad * 0.09 
dairyRast_grid = setValues(template_grid, dairyRast_quad)
nondairyRast_quad[nondairyRast_quad == 0] = NA
nondairyRast_cls = classIntervals(
	as.vector(sqrt(nondairyRast_quad)),
	6,
	style="quantile"
)
nondairyRast_quad = nondairyRast_quad * 0.09 
nondairyRast_grid = setValues(template_grid, nondairyRast_quad)


png(plot_file1, width=6, height=3, units="in", res=400)
par(mfrow=c(1,3), oma=c(0,0,0,3))
pal = brewer.pal(7, "Greys")[2:7]

par(mar=c(0,1,0,3))
plot(wi, col="white")
plot(
	log(dairyProd_grid),
	breaks=round(dairyProd_cls$brks, digits=1),
	col=pal,
	axes=F,
	box=F,
	add=T,
	legend.args=list(text=expression(ln(M)))
)
plot(wi, add=T)

plot(wi, col="white")
plot(
	sqrt(dairyRast_grid),
	breaks=round(dairyRast_cls$brks, digits=2),
	col=pal,
	axes=F,
	box=F,
	add=T,
	legend.args=list(text=expression(sqrt(A[d])))
)
plot(wi, add=T)

plot(wi, col="white")
plot(
	sqrt(nondairyRast_grid),
	breaks=round(nondairyRast_cls$brks, digits=2),
	col=pal,
	axes=F,
	box=F,
	add=T,
	legend.args=list(text=expression(sqrt(A[nd])))
)
plot(wi, add=T)
dev.off()

png(plot_file2, width=6, height=3, units="in", res=400)
par(mfrow=c(1,2))
plot(
	sqrt(dairy) ~ log(producer),
	data=test_data,
	pch=20,
	col="grey50",
	ylab=expression(sqrt(A[d])),
	xlab=expression(ln(M))
)
abline(lm(sqrt(dairy) ~ log(producer), data=test_data), lty=2)
plot(
	sqrt(nondairy) ~ log(producer),
	data=test_data,
	pch=20,
	col="grey50",
	ylab=expression(sqrt(A[nd])),
	xlab=expression(ln(M))
)
abline(lm(sqrt(nondairy) ~ log(producer), data=test_data), lty=2)
dev.off()

