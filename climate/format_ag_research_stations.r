library(raster)
library(rgdal)
options(stringsAsFactors=F)
net_dir = "T:/Projects/Wisconsin_River/GIS_Datasets/Climatological/stationData/observations"
out_dir = "T:/Projects/Wisconsin_River/Model_Inputs/SWAT_Inputs/climate"
stations = c("arl", "han", "mar", "spg")
# dem = raster("T:/GIS/Statewide_Coverages/DEM/10_meter/raw_prj.img")
dem = raster("T:/GIS/Statewide_Coverages/DEM/dem_30m")
variables = c("rel_hum", "solar", "wind")
cols = c("Average.Relative.Humidity..Percent.",
	"Average.Solar.Radiation..W.m2.",
	"Average.Wind..m.s.")
locs = rbind(
	c(-89.38, 43.31), 
	c(-89.53, 44.12),
	c(-90.13, 44.6),
	c(-89.91, 43.18))
dates = as.character(seq(as.Date("2002-01-01"), as.Date("2013-12-31"), by=1))
dates = data.frame(Date=dates)
warmup = as.character(seq(as.Date("1990-01-01"), as.Date("2001-12-31"), by=1))
wtm = CRS("+proj=tmerc +lat_0=0 +lon_0=-90 +k=0.9996 +x_0=520000 +y_0=-4480000 +ellps=GRS80 +units=m +no_defs")
wgs84 = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
for (v in variables) {
	station_file = paste(out_dir, "/", v, ".txt", sep="")
	write("ID,NAME,LAT,LONG,ELEVATION", station_file)
}
i = 0
for (s in stations) {

	print(s)
	i=i+1
	climate_file = paste(net_dir, "/", s, "_wind_sol_hum.csv", sep="")
	d = read.csv(climate_file)
	loc = locs[i,]
	dim(loc) = c(1,2)
	loc = SpatialPoints(loc, wgs84)
	loc_wtm = spTransform(loc, wtm)
	el = extract(dem, loc_wtm)
	j=0
	for (v in variables) {
		print(v)
		j=j+1
		nm = paste(v, "_", s, sep="")
		out_clim_file = paste(out_dir, "/", nm, ".txt", sep="")
		station_file = paste(out_dir, "/", v, ".txt", sep="")
		s_row = paste(i, nm, loc@coords[2], loc@coords[1], round(el,2), sep=",")
		write(s_row, station_file, append=T)
		vd = d[c("Date", cols[j])]
		vd = merge(dates, vd, all.x=T)[[cols[j]]]
		vd[is.na(vd) | vd <= -699 | vd >= 699] = NA
		if (v == "rel_hum") {
			vd[vd > 100] = 100
			vd = vd / 100
		} else if (v == "solar") {
			vd = vd * 0.0864
			if (s == "han") {
				del_bool = with(dates,
					(Date >= as.Date("2004-01-07") & Date <= as.Date("2004-01-28")) |
					Date == as.Date("2007-08-14"))
				vd[del_bool] = NA
			} else if (s == "spg") {
				del_bool = with(dates,
					(Date >= as.Date("2005-05-04") & Date <= as.Date("2005-05-25")))
				vd[del_bool] = NA
			}
		} else if (v == "wind") {
			if (s == "arl") {
				del_bool = with(dates,
					(Date >= as.Date("2013-12-20") & Date <= as.Date("2013-12-26")))
				vd[del_bool] = NA
			} else if (s == "han") {
				del_bool = with(dates,
					(Date >= as.Date("2005-01-02") & Date <= as.Date("2005-01-16")) |
					(Date >= as.Date("2009-08-31") & Date <= as.Date("2009-09-26")))
				vd[del_bool] = NA
			} else if (s == "spg") {
				del_bool = with(dates,
					(Date >= as.Date("2005-05-04") & Date <= as.Date("2005-09-25")) |
					(Date >= as.Date("2010-11-15") & Date <= as.Date("2012-03-13")))
				vd[del_bool] = NA
			}
		}
		vd = as.character(vd)
		vd[is.na(vd)] = "-99"
		write("19900101", out_clim_file)
		write(rep("-99", length(warmup)), out_clim_file, append=T)
		write(vd, out_clim_file, append=T)
	}
}