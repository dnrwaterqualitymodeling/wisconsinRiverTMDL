library(rgdal)
library(RColorBrewer)
library(classInt)
library(spdep)

wd <- "H:/sim_flow"
dir_doc = "C:/Users/evansdm/Documents"
file_wrb_flow_sites = "WRB_FlowSites_2"
dir_water_bud = "T:/Projects/Wisconsin_River/GIS_Datasets/Water_Budget"
file_wrb_regions = "WRB_Budget_Divisions"
file_flow_err = "ET_uncertainty.csv"
file_map_plot = paste(wd, "spatial_dist_flow_error.pdf",sep = '/')

dat_cols <- c("pbias_harg",
    "pbias_penman",
    "pbias_priestley",
    "nashsut_harg",      
    "nashsut_penman",    
    "nashsut_priestley")
#### for proper legend #####
# returns proper legend text for intervals created from classInterval-color object
properLegend <- function(prop_colr){
    legTxt <- attr(attr(prop_colr,'table'),'dimnames')[[1]]
    newTxt <- NULL
    for (rw in 1:length(legTxt)){
        r <- legTxt[rw]
        splt <- strsplit(r, split=',')[[1]]
        opn <- as.numeric(strsplit(splt[1],'[',fixed=T)[[1]][2])
        if (rw == length(legTxt)){
            cls <- as.numeric(strsplit(splt[2],']')[[1]][1])
        } else {
            cls <- as.numeric(strsplit(splt[2],')')[[1]][1])
        }
#         print(paste(opn,cls))
        opn <- round(opn, 3)
        cls <- round(cls, 3)
        nw <- paste('[',opn,',',cls,')',sep='')
        newTxt <- c(newTxt, nw)
    }
    return(newTxt)
}
################################
flow_sites <- readOGR(dir_doc, file_wrb_flow_sites)
names(flow_sites@data)[2] <- 'USGS_ID' 
wrb_regions = readOGR(dir_water_bud, file_wrb_regions)

flow_err_tbl <- read.csv(paste(wd, file_flow_err,sep ='/')) 
flow_err_tbl$USGS_ID <- paste(0, flow_err_tbl$USGS_ID, sep ='')

flow_sites <- merge(flow_sites, flow_err_tbl, by = "USGS_ID")
dats <- !is.na(flow_sites@data$pbias_harg)

pal = rev(brewer.pal(9, "RdYlBu"))
pdf(file_map_plot, height = 11, width = 8.5)

par(mfcol=c(3,2))
par(mar=c(0,0,0,0))
for (met in c("pbias", "nashsut")){
    met_cols <- grep(met, dat_cols)
    toClass <- flow_err_tbl[,dat_cols[met_cols]]
    toClass = as.matrix(toClass)
    prop_int = classIntervals(toClass, 9)
#     prop_colr = findColours(prop_int, pal)
    brks <- prop_int$brks
    for (i in met_cols){  
        toplot <- dat_cols[i]
        plot(wrb_regions)
        plot(flow_sites[dats,], 
            bg=pal[findInterval(flow_sites@data[dats, toplot],
                brks,
                all.inside = T)],
             add=T, pch=21, cex =2)
#         text(flow_sites@coords[,1],
#             flow_sites@coords[,2],
#             flow_sites@data$pbias_penman[nas],
#             pos = 3)
#         text(flow_sites@coords[,1],
#             flow_sites@coords[,2],
#             na.omit(flow_sites@data$USGS_ID[nas]),
#             pos = 2)
        txt <- properLegend(findColours(prop_int, pal))
        legend('bottomright', legend = txt, bg = 'white', bty = 'n', pch = 21,
               pt.bg = pal, cex=0.75, pt.cex = 2,
               title = paste(toplot))
    }
}
dev.off()

######
flow_dat <- flow_sites[dats,]
flw.crds <- coordinates(flow_dat)
k_near_nay <- knn2nb(knearneigh(flw.crds, k = 5, longlat = F, RANN = F))
fllistw <- nb2listw(k_near_nay)
geary.test(flow_dat@data$pbias_priestley, fllistw)
    
    
    
    