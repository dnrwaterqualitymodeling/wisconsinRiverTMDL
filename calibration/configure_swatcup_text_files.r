# CHANGE THESE ###########
# SWAT project

#projectDir = "D:/WRB2.Sufi2.SwatCup"
#projectDir = "D:/WRB2_driftless.Sufi2.SwatCup"
#projectDir = "D:/WRB2_sands.Sufi2.SwatCup"
#projectDir = "D:/WRB2_north_central.Sufi2.SwatCup"
#projectDir = "D:/WRB2_northern.Sufi2.SwatCup"

projectDir = "D:/WRB_generalized/WRB_central_sands.Sufi2.SwatCup"
projectDir = "D:/WRB_generalized/WRB.Sufi2.SwatCup"
#projectDir = "D:/WRB_generalized/WRB_driftless.Sufi2.SwatCup"
#projectDir = "D:/WRB_generalized/WRB_sands.Sufi2.SwatCup"
#projectDir = "D:/WRB_generalized/WRB_north_central.Sufi2.SwatCup"
#projectDir = "D:/WRB_generalized/WRB_northern.Sufi2.SwatCup"

simCount = 64
subbasinCount = 337
startYr = 2002
endYr = 2013
objFuncCode = 5
thresh = 0
monthly = T
pollutants = T

weight_by_sample_size = F

excl_ec_pine_rib = F
sands_east = F
sands_west = F
just_muskellunge = F

# Observations -- 
#	variable name, column index in output.rch, subbasin ID, observed data

# 1. Snowmelt, 1a. basin, 1b. ecoregion
#obsDir = "D:/usgs_raw/calibration/MAMJ_50_pct_exc"
#obsDir = "D:/usgs_raw/calibration/MAMJ_50_pct_exc_driftless"
#obsDir = "D:/usgs_raw/calibration/MAMJ_50_pct_exc_sands"
#obsDir = "D:/usgs_raw/calibration/MAMJ_50_pct_exc_north_central"
#obsDir = "D:/usgs_raw/calibration/MAMJ_50_pct_exc_northern"

# 2. Curve number and ESCO
#obsDir = "D:/usgs_raw/calibration/AMJJASO_50_pct_exc"
#obsDir = "D:/usgs_raw/calibration/AMJJASO_50_pct_exc_driftless"
#obsDir = "D:/usgs_raw/calibration/AMJJASO_sands"
#obsDir = "D:/usgs_raw/calibration/AMJJASO_north_central"
#obsDir = "D:/usgs_raw/calibration/JJASO_northern"

# 4. Groundwater
#obsDir = "D:/usgs_raw/calibration/AMJJASO_driftless"
#obsDir = "D:/usgs_raw/calibration/JJAS_50_pct_exc_driftless"
#obsDir = "D:/usgs_raw/calibration/JJAS_50_pct_exc_sands"
#obsDir = "D:/usgs_raw/calibration/JJAS_50_pct_exc_north_central"
#obsDir = "D:/usgs_raw/calibration/JJAS_50_pct_exc_northern"

# 5. Everything
obsDir = "D:/usgs_raw/calibration"
dir_pol = "D:/usgs_loads/tribs"

#gage_subbasin_lu =
#	read.csv("T:/Projects/Wisconsin_River/GIS_Datasets/observed/gauge_basin_lookup.csv",
gage_subbasin_lu = read.csv("D:/gauge_basin_lookup.csv",
	colClasses=c(rep("character", 5), "integer", "integer", "character"))
setInternet2(TRUE)

## Best ranges
parameterization = rbind(
## Snowmelt basin
	c("v__SMTMP.bsn",-0.5,0.5),
	c("v__SFTMP.bsn",-0.5,0.5),
	c("v__TIMP.bsn",0.18,0.22),
	c("v__SMFMN.bsn",0.4,1),
	c("v__SMFMX.bsn",3,3.6),
	c("v__SNOCOVMX.bsn",30,40),
	c("v__SNO50COV.bsn",0.4,0.55),
## Western Coulees and Ridges
	c("r__CN2.mgt________5-29,32,33,35,40-42,44,48-51,55,57,58,137,138,179-189,192,196,227-233,237,246,274,301,310",-0.13,-0.11),
	c("v__ESCO.hru________5-29,32,33,35,40-42,44,48-51,55,57,58,137,138,179-189,192,196,227-233,237,246,274,301,310",0.4,0.5),
	c("r__SURLAG.hru________5-29,32,33,35,40-42,44,48-51,55,57,58,137,138,179-189,192,196,227-233,237,246,274,301,310",-0.6,-0.5),
	c("r__SOL_BD().sol________5-29,32,33,35,40-42,44,48-51,55,57,58,137,138,179-189,192,196,227-233,237,246,274,301,310",-0.2,-0.15),
	c("r__SOL_Z().sol________5-29,32,33,35,40-42,44,48-51,55,57,58,137,138,179-189,192,196,227-233,237,246,274,301,310",0.1,0.3),
	c("r__SOL_K().sol________5-29,32,33,35,40-42,44,48-51,55,57,58,137,138,179-189,192,196,227-233,237,246,274,301,310",-0.2,-0.1),
	c("r__SOL_AWC().sol________5-29,32,33,35,40-42,44,48-51,55,57,58,137,138,179-189,192,196,227-233,237,246,274,301,310",0,0.05),
	c("r__SLSUBBSN.hru________5-29,32,33,35,40-42,44,48-51,55,57,58,137,138,179-189,192,196,227-233,237,246,274,301,310",0.4,0.5),
	c("v__CH_N1.sub________5-29,32,33,35,40-42,44,48-51,55,57,58,137,138,179-189,192,196,227-233,237,246,274,301,310",0.03,0.05),
	c("v__CH_N2.rte________5-29,32,33,35,40-42,44,48-51,55,57,58,137,138,179-189,192,196,227-233,237,246,274,301,310",0.06,0.08),
	c("r__ALPHA_BF.gw________5-29,32,33,35,40-42,44,48-51,55,57,58,137,138,179-189,192,196,227-233,237,246,274,301,310",-0.9,-0.8),
	c("v__GW_DELAY.gw________5-29,32,33,35,40-42,44,48-51,55,57,58,137,138,179-189,192,196,227-233,237,246,274,301,310",200,300),
	c("v__GWQMN.gw________5-29,32,33,35,40-42,44,48-51,55,57,58,137,138,179-189,192,196,227-233,237,246,274,301,310",0,500),
	c("v__GW_REVAP.gw________5-29,32,33,35,40-42,44,48-51,55,57,58,137,138,179-189,192,196,227-233,237,246,274,301,310",0.02,0.04),
	c("v__REVAPMN.gw________5-29,32,33,35,40-42,44,48-51,55,57,58,137,138,179-189,192,196,227-233,237,246,274,301,310",0,500),
## Forest Transition
	c("r__CN2.mgt________68-71,78-112,122,136,146,147,149-159,162,166,169,201,207,211-220,225,258,259,261-270,275,276,279,282,286-299,307,308,313,314,317,320-329,331,332",0.02,0.08),
	c("v__ESCO.hru________68-71,78-112,122,136,146,147,149-159,162,166,169,201,207,211-220,225,258,259,261-270,275,276,279,282,286-299,307,308,313,314,317,320-329,331,332",0.8,0.96),
	c("r__SURLAG.hru____A,B____68-71,78-112,122,136,146,147,149-159,162,166,169,201,207,211-220,225,258,259,261-270,275,276,279,282,286-299,307,308,313,314,317,320-329,331,332",-0.5,-0.2),
	c("r__SURLAG.hru____C,D____68-71,78-112,122,136,146,147,149-159,162,166,169,201,207,211-220,225,258,259,261-270,275,276,279,282,286-299,307,308,313,314,317,320-329,331,332",-0.2,0.1),
	c("v__CH_N1.sub________68-71,78-112,122,136,146,147,149-159,162,166,169,201,207,211-220,225,258,259,261-270,275,276,279,282,286-299,307,308,313,314,317,320-329,331,332",0.05,0.08),
	c("v__CH_N2.rte________68-71,78-112,122,136,146,147,149-159,162,166,169,201,207,211-220,225,258,259,261-270,275,276,279,282,286-299,307,308,313,314,317,320-329,331,332",0.055,0.065),
	c("r__SLSUBBSN.hru________68-71,78-112,122,136,146,147,149-159,162,166,169,201,207,211-220,225,258,259,261-270,275,276,279,282,286-299,307,308,313,314,317,320-329,331,332",0,0.3),
	c("r__ALPHA_BF.gw____A,B____68-71,78-112,122,136,146,147,149-159,162,166,169,201,207,211-220,225,258,259,261-270,275,276,279,282,286-299,307,308,313,314,317,320-329,331,332",-0.98,-0.9),
	c("r__ALPHA_BF.gw____C,D____68-71,78-112,122,136,146,147,149-159,162,166,169,201,207,211-220,225,258,259,261-270,275,276,279,282,286-299,307,308,313,314,317,320-329,331,332",2,3.5),
	c("v__GW_DELAY.gw____A,B____68-71,78-112,122,136,146,147,149-159,162,166,169,201,207,211-220,225,258,259,261-270,275,276,279,282,286-299,307,308,313,314,317,320-329,331,332",90,120),
	c("v__GW_DELAY.gw____C,D____68-71,78-112,122,136,146,147,149-159,162,166,169,201,207,211-220,225,258,259,261-270,275,276,279,282,286-299,307,308,313,314,317,320-329,331,332",0,2),
## Central Sand Plains
	c("r__SURLAG.hru____A,B____1-4,30,31,34,36-39,43,45-47,52-54,56,59-67,72-77,139-145,148,170-178,190,191,193-195,197-200,202-206,208,210,234-236,238-245,247-257,260,302-306,311,312,330,333",-0.9,-0.825),
	c("r__SURLAG.hru____C,D____1-4,30,31,34,36-39,43,45-47,52-54,56,59-67,72-77,139-145,148,170-178,190,191,193-195,197-200,202-206,208,210,234-236,238-245,247-257,260,302-306,311,312,330,333",-0.85,-0.7),
	c("v__ESCO.hru____A,B____1-4,30,31,34,36-39,43,45-47,52-54,56,59-67,72-77,139-145,148,170-178,190,191,193-195,197-200,202-206,208,210,234-236,238-245,247-257,260,302-306,311,312,330,333",0.5,0.75),
	c("v__ESCO.hru____C,D____1-4,30,31,34,36-39,43,45-47,52-54,56,59-67,72-77,139-145,148,170-178,190,191,193-195,197-200,202-206,208,210,234-236,238-245,247-257,260,302-306,311,312,330,333",0.6,0.8),
	c("r__CN2.mgt____A,B____1-4,30,31,34,36-39,43,45-47,52-54,56,59-67,72-77,139-145,148,170-178,190,191,193-195,197-200,202-206,208,210,234-236,238-245,247-257,260,302-306,311,312,330,333",-0.19,-0.15),
	c("r__CN2.mgt____C,D____1-4,30,31,34,36-39,43,45-47,52-54,56,59-67,72-77,139-145,148,170-178,190,191,193-195,197-200,202-206,208,210,234-236,238-245,247-257,260,302-306,311,312,330,333",-0.18,-0.15),
	c("r__SOL_Z().sol____A,B____1-4,30,31,34,36-39,43,45-47,52-54,56,59-67,72-77,139-145,148,170-178,190,191,193-195,197-200,202-206,208,210,234-236,238-245,247-257,260,302-306,311,312,330,333",-0.25,-0.18),
	c("r__SOL_BD().sol____A,B____1-4,30,31,34,36-39,43,45-47,52-54,56,59-67,72-77,139-145,148,170-178,190,191,193-195,197-200,202-206,208,210,234-236,238-245,247-257,260,302-306,311,312,330,333",0.15,0.22),
	c("v__GW_DELAY.gw____A,B____1-4,30,31,34,36-39,43,45-47,52-54,56,59-67,72-77,139-145,148,170-178,190,191,193-195,197-200,202-206,208,210,234-236,238-245,247-257,260,302-306,311,312,330,333",150,180),
	c("v__GW_DELAY.gw____C,D____1-4,30,31,34,36-39,43,45-47,52-54,56,59-67,72-77,139-145,148,170-178,190,191,193-195,197-200,202-206,208,210,234-236,238-245,247-257,260,302-306,311,312,330,333",2,5),
	c("v__GWQMN.gw____A,B____1-4,30,31,34,36-39,43,45-47,52-54,56,59-67,72-77,139-145,148,170-178,190,191,193-195,197-200,202-206,208,210,234-236,238-245,247-257,260,302-306,311,312,330,333",500,800),
	c("v__GWQMN.gw____C,D____1-4,30,31,34,36-39,43,45-47,52-54,56,59-67,72-77,139-145,148,170-178,190,191,193-195,197-200,202-206,208,210,234-236,238-245,247-257,260,302-306,311,312,330,333",1000,1200),
	c("r__ALPHA_BF.gw____A,B____1-4,30,31,34,36-39,43,45-47,52-54,56,59-67,72-77,139-145,148,170-178,190,191,193-195,197-200,202-206,208,210,234-236,238-245,247-257,260,302-306,311,312,330,333",-0.98,-0.9),
	c("r__ALPHA_BF.gw____C,D____1-4,30,31,34,36-39,43,45-47,52-54,56,59-67,72-77,139-145,148,170-178,190,191,193-195,197-200,202-206,208,210,234-236,238-245,247-257,260,302-306,311,312,330,333",2,3),
	c("v__GW_REVAP.gw________1-4,30,31,34,36-39,43,45-47,52-54,56,59-67,72-77,139-145,148,170-178,190,191,193-195,197-200,202-206,208,210,234-236,238-245,247-257,260,302-306,311,312,330,333",0.02,0.05),
## Northern Highlands
	c("r__CN2.mgt________113-121,123-135,160,161,163-165,167,168,209,221-224,226,271-273,277,278,280,281,283-285,300,309,315,316,318,319,334-337",-0.4,-0.25),
	c("v__R2ADJ.hru________113-121,123-135,160,161,163-165,167,168,209,221-224,226,271-273,277,278,280,281,283-285,300,309,315,316,318,319,334-337",1,3),
	c("r__SURLAG.hru________113-121,123-135,160,161,163-165,167,168,209,221-224,226,271-273,277,278,280,281,283-285,300,309,315,316,318,319,334-337",-0.99,-0.97),
	c("v__ESCO.hru________113-121,123-135,160,161,163-165,167,168,209,221-224,226,271-273,277,278,280,281,283-285,300,309,315,316,318,319,334-337",0.4,0.6),
	c("r__SOL_AWC().sol________113-121,123-135,160,161,163-165,167,168,209,221-224,226,271-273,277,278,280,281,283-285,300,309,315,316,318,319,334-337",0,0.15),
	c("r__SOL_Z().sol________113-121,123-135,160,161,163-165,167,168,209,221-224,226,271-273,277,278,280,281,283-285,300,309,315,316,318,319,334-337",-0.14,-0.1),
	c("r__PND_EVOL.pnd________113-121,123-135,160,161,163-165,167,168,209,221-224,226,271-273,277,278,280,281,283-285,300,309,315,316,318,319,334-337",0.5,1.5),
	c("r__PND_FR.pnd________113-121,123-135,160,161,163-165,167,168,209,221-224,226,271-273,277,278,280,281,283-285,300,309,315,316,318,319,334-337",-1,-0.2),
	c("v__GW_REVAP.gw________113-121,123-135,160,161,163-165,167,168,209,221-224,226,271-273,277,278,280,281,283-285,300,309,315,316,318,319,334-337",0.02,0.05),
	c("v__GWQMN.gw________113-121,123-135,160,161,163-165,167,168,209,221-224,226,271-273,277,278,280,281,283-285,300,309,315,316,318,319,334-337",0,500),
	c("v__GW_DELAY.gw________113-121,123-135,160,161,163-165,167,168,209,221-224,226,271-273,277,278,280,281,283-285,300,309,315,316,318,319,334-337",100,250),
	c("r__ALPHA_BF.gw________113-121,123-135,160,161,163-165,167,168,209,221-224,226,271-273,277,278,280,281,283-285,300,309,315,316,318,319,334-337",-0.5,0.1)
)

## Best pars
parameterization = rbind(
	## Snowmelt basin
	c("v__SMTMP.bsn",-0.162109,-0.162109),
	c("v__SFTMP.bsn",-0.197266,-0.197266),
	c("v__TIMP.bsn",0.195234,0.195234),
	c("v__SMFMN.bsn",0.787891,0.787891),
	c("v__SMFMX.bsn",3.050391,3.050391),
	c("v__SNOCOVMX.bsn",39.550781,39.550781),
	c("v__SNO50COV.bsn",0.462988,0.462988),
	## Western Coulees and Ridges
	c("r__CN2.mgt________5-29,32,33,35,40-42,44,48-51,55,57,58,137,138,179-189,192,196,227-233,237,246,274,301,310",-0.120723,-0.120723),
	c("v__ESCO.hru________5-29,32,33,35,40-42,44,48-51,55,57,58,137,138,179-189,192,196,227-233,237,246,274,301,310",0.447363,0.447363),
	c("r__SURLAG.hru________5-29,32,33,35,40-42,44,48-51,55,57,58,137,138,179-189,192,196,227-233,237,246,274,301,310",-0.597363,-0.597363),
	c("r__SOL_BD().sol________5-29,32,33,35,40-42,44,48-51,55,57,58,137,138,179-189,192,196,227-233,237,246,274,301,310",-0.161377,-0.161377),
	c("r__SOL_Z().sol________5-29,32,33,35,40-42,44,48-51,55,57,58,137,138,179-189,192,196,227-233,237,246,274,301,310",0.196289,0.196289),
	c("r__SOL_K().sol________5-29,32,33,35,40-42,44,48-51,55,57,58,137,138,179-189,192,196,227-233,237,246,274,301,310",-0.166895,-0.166895),
	c("r__SOL_AWC().sol________5-29,32,33,35,40-42,44,48-51,55,57,58,137,138,179-189,192,196,227-233,237,246,274,301,310",0.025195,0.025195),
	c("r__SLSUBBSN.hru________5-29,32,33,35,40-42,44,48-51,55,57,58,137,138,179-189,192,196,227-233,237,246,274,301,310",0.488574,0.488574),
	c("v__CH_N1.sub________5-29,32,33,35,40-42,44,48-51,55,57,58,137,138,179-189,192,196,227-233,237,246,274,301,310",0.038887,0.038887),
	c("v__CH_N2.rte________5-29,32,33,35,40-42,44,48-51,55,57,58,137,138,179-189,192,196,227-233,237,246,274,301,310",0.079941,0.079941),
	c("r__ALPHA_BF.gw________5-29,32,33,35,40-42,44,48-51,55,57,58,137,138,179-189,192,196,227-233,237,246,274,301,310",-0.871387,-0.871387),
	c("v__GW_DELAY.gw________5-29,32,33,35,40-42,44,48-51,55,57,58,137,138,179-189,192,196,227-233,237,246,274,301,310",260.839844,260.839844),
	c("v__GWQMN.gw________5-29,32,33,35,40-42,44,48-51,55,57,58,137,138,179-189,192,196,227-233,237,246,274,301,310",421.386719,421.386719),
	c("v__GW_REVAP.gw________5-29,32,33,35,40-42,44,48-51,55,57,58,137,138,179-189,192,196,227-233,237,246,274,301,310",0.036621,0.036621),
	c("v__REVAPMN.gw________5-29,32,33,35,40-42,44,48-51,55,57,58,137,138,179-189,192,196,227-233,237,246,274,301,310",198.730469,198.730469),
	## Forest Transition
	c("r__CN2.mgt________68-71,78-112,122,136,146,147,149-159,162,166,169,201,207,211-220,225,258,259,261-270,275,276,279,282,286-299,307,308,313,314,317,320-329,331,332",0.020820,0.020820),
	c("v__ESCO.hru________68-71,78-112,122,136,146,147,149-159,162,166,169,201,207,211-220,225,258,259,261-270,275,276,279,282,286-299,307,308,313,314,317,320-329,331,332",0.886562,0.886562),
	c("r__SURLAG.hru____A,B____68-71,78-112,122,136,146,147,149-159,162,166,169,201,207,211-220,225,258,259,261-270,275,276,279,282,286-299,307,308,313,314,317,320-329,331,332",-0.260352,-0.260352),
	c("r__SURLAG.hru____C,D____68-71,78-112,122,136,146,147,149-159,162,166,169,201,207,211-220,225,258,259,261-270,275,276,279,282,286-299,307,308,313,314,317,320-329,331,332",-0.086914,-0.086914),
	c("v__CH_N1.sub________68-71,78-112,122,136,146,147,149-159,162,166,169,201,207,211-220,225,258,259,261-270,275,276,279,282,286-299,307,308,313,314,317,320-329,331,332",0.073496,0.073496),
	c("v__CH_N2.rte________68-71,78-112,122,136,146,147,149-159,162,166,169,201,207,211-220,225,258,259,261-270,275,276,279,282,286-299,307,308,313,314,317,320-329,331,332",0.057090,0.057090),
	c("r__SLSUBBSN.hru________68-71,78-112,122,136,146,147,149-159,162,166,169,201,207,211-220,225,258,259,261-270,275,276,279,282,286-299,307,308,313,314,317,320-329,331,332",0.118945,0.118945),
	c("r__ALPHA_BF.gw____A,B____68-71,78-112,122,136,146,147,149-159,162,166,169,201,207,211-220,225,258,259,261-270,275,276,279,282,286-299,307,308,313,314,317,320-329,331,332",-0.916211,-0.916211),
	c("r__ALPHA_BF.gw____C,D____68-71,78-112,122,136,146,147,149-159,162,166,169,201,207,211-220,225,258,259,261-270,275,276,279,282,286-299,307,308,313,314,317,320-329,331,332",3.356445,3.356445),
	c("v__GW_DELAY.gw____A,B____68-71,78-112,122,136,146,147,149-159,162,166,169,201,207,211-220,225,258,259,261-270,275,276,279,282,286-299,307,308,313,314,317,320-329,331,332",91.816406,91.816406),
	c("v__GW_DELAY.gw____C,D____68-71,78-112,122,136,146,147,149-159,162,166,169,201,207,211-220,225,258,259,261-270,275,276,279,282,286-299,307,308,313,314,317,320-329,331,332",0.339844,0.339844),
	## Central Sand Plains
	c("r__SURLAG.hru____A,B____1-4,30,31,34,36-39,43,45-47,52-54,56,59-67,72-77,139-145,148,170-178,190,191,193-195,197-200,202-206,208,210,234-236,238-245,247-257,260,302-306,311,312,330,333",-0.857959,-0.857959),
	c("r__SURLAG.hru____C,D____1-4,30,31,34,36-39,43,45-47,52-54,56,59-67,72-77,139-145,148,170-178,190,191,193-195,197-200,202-206,208,210,234-236,238-245,247-257,260,302-306,311,312,330,333",-0.820410,-0.820410),
	c("v__ESCO.hru____A,B____1-4,30,31,34,36-39,43,45-47,52-54,56,59-67,72-77,139-145,148,170-178,190,191,193-195,197-200,202-206,208,210,234-236,238-245,247-257,260,302-306,311,312,330,333",0.517090,0.517090),
	c("v__ESCO.hru____C,D____1-4,30,31,34,36-39,43,45-47,52-54,56,59-67,72-77,139-145,148,170-178,190,191,193-195,197-200,202-206,208,210,234-236,238-245,247-257,260,302-306,311,312,330,333",0.685547,0.685547),
	c("r__CN2.mgt____A,B____1-4,30,31,34,36-39,43,45-47,52-54,56,59-67,72-77,139-145,148,170-178,190,191,193-195,197-200,202-206,208,210,234-236,238-245,247-257,260,302-306,311,312,330,333",-0.182734,-0.182734),
	c("r__CN2.mgt____C,D____1-4,30,31,34,36-39,43,45-47,52-54,56,59-67,72-77,139-145,148,170-178,190,191,193-195,197-200,202-206,208,210,234-236,238-245,247-257,260,302-306,311,312,330,333",-0.156270,-0.156270),
	c("r__SOL_Z().sol____A,B____1-4,30,31,34,36-39,43,45-47,52-54,56,59-67,72-77,139-145,148,170-178,190,191,193-195,197-200,202-206,208,210,234-236,238-245,247-257,260,302-306,311,312,330,333",-0.227441,-0.227441),
	c("r__SOL_BD().sol____A,B____1-4,30,31,34,36-39,43,45-47,52-54,56,59-67,72-77,139-145,148,170-178,190,191,193-195,197-200,202-206,208,210,234-236,238-245,247-257,260,302-306,311,312,330,333",0.185957,0.185957),
	c("v__GW_DELAY.gw____A,B____1-4,30,31,34,36-39,43,45-47,52-54,56,59-67,72-77,139-145,148,170-178,190,191,193-195,197-200,202-206,208,210,234-236,238-245,247-257,260,302-306,311,312,330,333",170.449219,170.449219),
	c("v__GW_DELAY.gw____C,D____1-4,30,31,34,36-39,43,45-47,52-54,56,59-67,72-77,139-145,148,170-178,190,191,193-195,197-200,202-206,208,210,234-236,238-245,247-257,260,302-306,311,312,330,333",2.498047,2.498047),
	c("v__GWQMN.gw____A,B____1-4,30,31,34,36-39,43,45-47,52-54,56,59-67,72-77,139-145,148,170-178,190,191,193-195,197-200,202-206,208,210,234-236,238-245,247-257,260,302-306,311,312,330,333",591.992188,591.992188),
	c("v__GWQMN.gw____C,D____1-4,30,31,34,36-39,43,45-47,52-54,56,59-67,72-77,139-145,148,170-178,190,191,193-195,197-200,202-206,208,210,234-236,238-245,247-257,260,302-306,311,312,330,333",1087.890625,1087.890625),
	c("r__ALPHA_BF.gw____A,B____1-4,30,31,34,36-39,43,45-47,52-54,56,59-67,72-77,139-145,148,170-178,190,191,193-195,197-200,202-206,208,210,234-236,238-245,247-257,260,302-306,311,312,330,333",-0.974219,-0.974219),
	c("r__ALPHA_BF.gw____C,D____1-4,30,31,34,36-39,43,45-47,52-54,56,59-67,72-77,139-145,148,170-178,190,191,193-195,197-200,202-206,208,210,234-236,238-245,247-257,260,302-306,311,312,330,333",2.232422,2.232422),
	c("v__GW_REVAP.gw________1-4,30,31,34,36-39,43,45-47,52-54,56,59-67,72-77,139-145,148,170-178,190,191,193-195,197-200,202-206,208,210,234-236,238-245,247-257,260,302-306,311,312,330,333",0.020645,0.020645),
	## Northern Highlands
	c("v__REVAPMN.gw________113-121,123-135,160,161,163-165,167,168,209,221-224,226,271-273,277,278,280,281,283-285,300,309,315,316,318,319,334-337",2092.285156,2092.285156),
	c("v__GW_REVAP.gw________113-121,123-135,160,161,163-165,167,168,209,221-224,226,271-273,277,278,280,281,283-285,300,309,315,316,318,319,334-337",0.047168,0.047168),
	c("v__GWQMN.gw________113-121,123-135,160,161,163-165,167,168,209,221-224,226,271-273,277,278,280,281,283-285,300,309,315,316,318,319,334-337",1077.148438,1077.148438),
	c("v__GW_DELAY.gw________113-121,123-135,160,161,163-165,167,168,209,221-224,226,271-273,277,278,280,281,283-285,300,309,315,316,318,319,334-337",619.238281,619.238281),
	c("r__ALPHA_BF.gw________113-121,123-135,160,161,163-165,167,168,209,221-224,226,271-273,277,278,280,281,283-285,300,309,315,316,318,319,334-337",-0.155518,-0.155518),
	c("v__RCHRG_DP.gw________113-121,123-135,160,161,163-165,167,168,209,221-224,226,271-273,277,278,280,281,283-285,300,309,315,316,318,319,334-337",0.128320,0.128320),
	c("r__CN2.mgt________113-121,123-135,160,161,163-165,167,168,209,221-224,226,271-273,277,278,280,281,283-285,300,309,315,316,318,319,334-337",-0.176367,-0.176367),
	c("r__SURLAG.hru________113-121,123-135,160,161,163-165,167,168,209,221-224,226,271-273,277,278,280,281,283-285,300,309,315,316,318,319,334-337",-0.973789,-0.973789),
	c("v__ESCO.hru________113-121,123-135,160,161,163-165,167,168,209,221-224,226,271-273,277,278,280,281,283-285,300,309,315,316,318,319,334-337",0.485742,0.485742),
	c("r__SOL_BD().sol________113-121,123-135,160,161,163-165,167,168,209,221-224,226,271-273,277,278,280,281,283-285,300,309,315,316,318,319,334-337",-0.323682,-0.323682),
	c("r__SOL_K().sol________113-121,123-135,160,161,163-165,167,168,209,221-224,226,271-273,277,278,280,281,283-285,300,309,315,316,318,319,334-337",-0.663379,-0.663379),
	c("r__SOL_Z().sol________113-121,123-135,160,161,163-165,167,168,209,221-224,226,271-273,277,278,280,281,283-285,300,309,315,316,318,319,334-337",-0.128271,-0.128271),
	c("r__SOL_AWC().sol________113-121,123-135,160,161,163-165,167,168,209,221-224,226,271-273,277,278,280,281,283-285,300,309,315,316,318,319,334-337",-0.134238,-0.134238),
	c("r__PND_EVOL.pnd________113-121,123-135,160,161,163-165,167,168,209,221-224,226,271-273,277,278,280,281,283-285,300,309,315,316,318,319,334-337",1.426758,1.426758),
	c("r__PND_FR.pnd________113-121,123-135,160,161,163-165,167,168,209,221-224,226,271-273,277,278,280,281,283-285,300,309,315,316,318,319,334-337",-0.781445,-0.781445)
)


# Don't change these
source("https://raw.githubusercontent.com/dnrwaterqualitymodeling/wisconsinRiverTMDL/master/calibration/functions_query_output.r")

# Change absolute values for ponds and wetlands
file_abs_vol = paste(projectDir, "Absolute_SWAT_Values.txt", sep="/")
abs_vol = readLines(file_abs_vol, warn=F)
abs_vol[405] = 
	"PND_PSA		        0	  50000	    	        Surface area of ponds when filled to principal spillway"
abs_vol[406] = 
	"PND_PVOL	        0	  50000		    	Volume of water needed to fill ponds to the principal spillway."
abs_vol[407] = 
	"PND_ESA		        0	  50000		    	 Surface area of ponds when filled to emergency spillway."
abs_vol[408] = 
	"PND_EVOL	        0	  50000		    	 Volume of water stored in ponds when filled to the emergency spillway."
abs_vol[409] = 
	"PND_VOL		        0	  50000		      	Initial volume of water in ponds."

abs_vol[429] = 
	"WET_NSA		        0	  50000	    	        Surface area of wetlands at normal water level ."
abs_vol[430] = 
	"WET_NVOL	        0	  50000		    	Volume of water stored in wetlands when filled to normal water level ."
abs_vol[431] = 
	"WET_MXSA	        0	  50000	    	        Surface area of wetlands at maximum water level ."
abs_vol[432] = 
	"WET_MXVOL	        0	  50000		    	Volume of water stored in wetlands when filled to maximum water level ."
abs_vol[433] = 
	"WET_VOL		        0	  50000		      	Initial volume of water in ponds."
abs_vol[215] = "R2ADJ\t0\t3"

writeLines(abs_vol, file_abs_vol)

gage_subbasin_lu = subset(gage_subbasin_lu, Keep == 1)
if (excl_ec_pine_rib) {
	gage_subbasin_lu = subset(
		gage_subbasin_lu,
		!(WRB_SubbasinID %in% c(268,155,157))
	)	
}
if (sands_east) {
	gage_subbasin_lu = subset(
		gage_subbasin_lu,
		WRB_SubbasinID %in% c(141,142)
	)
}
if (sands_west) {
	gage_subbasin_lu = subset(
		gage_subbasin_lu,
		WRB_SubbasinID %in% c(199,195)
	)
}
if (just_muskellunge) {
	gage_subbasin_lu = subset(
		gage_subbasin_lu,
		WRB_SubbasinID == 127
	)
}
gage_subbasin_lu = gage_subbasin_lu[
	c("USGS_ID", "LOAD_ID", "WRB_SubbasinID", "Notes")
]

observed_table = cbind(rep("FLOW_OUT", nrow(gage_subbasin_lu)),
    rep(7, nrow(gage_subbasin_lu)),
    gage_subbasin_lu$WRB_SubbasinID,
    paste(obsDir, "/", gage_subbasin_lu$USGS_ID, ".txt", sep="")
)
reservoirs = subset(
	gage_subbasin_lu,
	Notes == "Reservoir",
	select="WRB_SubbasinID")[[1]]
observed_table[observed_table[,3] %in% as.character(reservoirs),1] = "FLOW_IN"
observed_table[observed_table[,3] %in% as.character(reservoirs),2] = "6"

if (pollutants) {
	observed_table = rbind(
		observed_table,
		cbind(
			rep("SED_OUT", nrow(gage_subbasin_lu)),
			rep(8, nrow(gage_subbasin_lu)),
			gage_subbasin_lu$WRB_SubbasinID,
			paste(dir_pol, "/SS/calibration/", gage_subbasin_lu$LOAD_ID, ".txt", sep="")
		), cbind(
			rep("TOT_P", nrow(gage_subbasin_lu)),
			rep(9, nrow(gage_subbasin_lu)),
			gage_subbasin_lu$WRB_SubbasinID,
			paste(dir_pol, "/TP/calibration/", gage_subbasin_lu$LOAD_ID, ".txt", sep="")
		)
	)
}

obs_files = c(
	list.files(obsDir, full.names=T, pattern=".*\\.txt$"),
	list.files(paste(dir_pol, "/SS/calibration/", sep=""), full.names=T),
	list.files(paste(dir_pol, "/TP/calibration/", sep=""), full.names=T)
)

if (nrow(observed_table) > 1) {
	observed_table = observed_table[
		order(
			observed_table[,2],
			as.integer(observed_table[,3])
		)
	,]
	observed_table = observed_table[
		observed_table[,4] %in% obs_files,
	]
}


inDir = paste(projectDir,
    "/",
    toupper(strsplit(basename(projectDir), "\\.")[[1]][2]),
    ".IN",
    sep="")
file.cio = paste(projectDir,
    "/file.cio",
    sep="")
Par_inf_file = paste(inDir,
    "/par_inf.txt",
    sep="")
swEdit_file = paste(projectDir,
    "/",
    toupper(strsplit(basename(projectDir), "\\.")[[1]][2]),
    "_swEdit.def",
    sep="")
observed_rch_file = paste(inDir,
    "/observed_rch.txt",
    sep="")
extract_rch_file = paste(projectDir,
     "/",
     toupper(strsplit(basename(projectDir), "\\.")[[1]][2]),
     "_extract_rch.def",
     sep="")
observed_file = paste(inDir,
    "/Observed.txt",
    sep="")
var_file_name = paste(inDir,
    "/Var_file_name.txt",
    sep="")
var_file_rch = paste(inDir,
    "/Var_file_rch.txt",
    sep="")
if (monthly) {
    time_series = data.frame(DATE = seq(
        as.Date(paste(startYr, "-01-01", sep="")),
        as.Date(paste(endYr, "-12-31", sep="")),
        "1 months"))
    time_series = cbind(data.frame(i = 1:nrow(time_series)), time_series)
} else {
    time_series = data.frame(DATE = seq(
        as.Date(paste(startYr, "-01-01", sep="")),
        as.Date(paste(endYr, "-12-31", sep="")),
        1))
    time_series = cbind(data.frame(i = 1:nrow(time_series)), time_series)
}

# Write file.cio
file.cio.dat = readLines(file.cio, warn=F)
if (monthly) {
	file.cio.dat[59] = "               0    | IPRINT: print code (month, day, year)"
} else {
	file.cio.dat[59] = "               1    | IPRINT: print code (month, day, year)"
}
file.cio.dat[65] = "   1   2   6  44   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0"
file.cio.dat[67] = "   4   0   0   0   0   0   0   0   0   0   0   0   0   0   0"
file.cio.dat[69] = "   6   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0"
file.cio.dat[71] = "   1   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0"

writeLines(file.cio.dat, file.cio)

# Write par_inf file
l1 = paste(nrow(parameterization), "  : Number of Parameters (the program only reads the first 4 parameters or any number indicated here)", sep="")
l2 = paste(simCount, "  : number of simulations", sep="")
writeLines(paste(l1,"\n",l2,"\n\n",sep=""), Par_inf_file)
write.table(parameterization, Par_inf_file, row.name=F, col.names=F, quote=F, append=T)
# write swEdit file
l1 = "1\t: starting simulation number"
l2 = paste(simCount, ": ending simulation number", sep="\t")
writeLines(paste(l1, "\n", l2, sep=""), swEdit_file)
########################
# Write observed_rch and observed.txt file
########################
l1 = paste(nrow(observed_table), ": number of observed variables", sep="\t")
write(l1, observed_rch_file)
# write('\n', observed_rch_file, append=T)
write(paste(nrow(observed_table), ": number of observed variables", sep="\t"), observed_file)
write(paste(objFuncCode, ": Objective function type, 1=mult,2=sum,3=r2,4=chi2,5=NS,6=br2,7=ssqr,8=PBIAS,9=RSR", sep="\t"),
      observed_file, append=T)
write(
	paste(thresh,
		": min value of objective function threshold for the behavioral solutions",
		sep="\t"
	),
	observed_file,
	append=T
)
write("\n", observed_file, append=T)

for (obs_i in 1:nrow(observed_table)) {
	if (observed_table[obs_i,1] %in% c("FLOW_IN", "FLOW_OUT")) {	
	    obsData = read.table(observed_table[obs_i,4], skip=2, sep="\t", header=T)
	    obsData = obsData[-1,]
	    obsData = obsData[obsData[,5] == "A",]
	    obsData = data.frame(DATE = as.Date(as.character(obsData[,3])),
	        FLOW_OBSERVED=as.numeric(as.character(obsData[,4])))
	    if (monthly) {
	        months = as.POSIXlt(obsData$DATE)$mo + 1
	        years = as.POSIXlt(obsData$DATE)$year + 1900
	        date = paste(years, months, "01", sep="-")
	        obsMonthly = aggregate(FLOW_OBSERVED ~ date, data=obsData, mean)
			dayCount = aggregate(FLOW_OBSERVED ~ date, data=obsData, length)
			obsMonthly = obsMonthly[dayCount$FLOW_OBSERVED > 15,]
	        obsData = data.frame(DATE=as.Date(obsMonthly[,1]),
	            FLOW_OBSERVED=obsMonthly[,2])
	        obsData = merge(time_series, obsData, all.y=T, all.x=F)
	        obsData = obsData[order(obsData$i),]
	        obsData$VARNAME_DATE = paste(
	            observed_table[obs_i, 1],
	            format(obsData$DATE, "%m"),
	            format(obsData$DATE, "%Y"),
	            sep="_")
	    } else {
	        obsData = data.frame(DATE=as.Date(obsData[,1]),
	            FLOW_OBSERVED=obsData[,2])
	        obsData = merge(time_series, obsData, all.y=T, all.x=F)
	        obsData = obsData[order(obsData$i),]
	        obsData$VARNAME_DATE = paste(
	            observed_table[obs_i, 1],
	            format(obsData$DATE, "%m"),
	            format(obsData$DATE, "%d"),
	            format(obsData$DATE, "%Y"),
	            sep="_")
	    }
	    obsData = obsData[c("i","VARNAME_DATE", "FLOW_OBSERVED")]
	    #converting from cf/s to cm/s
	    obsData$FLOW_OBSERVED <- obsData$FLOW_OBSERVED * 0.0283168466
	} else if (observed_table[obs_i,1] %in% c("SED_OUT", "TOT_P")) {
		obsData = read.table(observed_table[obs_i,4], sep="\t", header=T)
		if (monthly) {
			obsData$DATE = as.Date(paste(obsData$YEAR,obsData$MO, "01", sep="-"))
			obsData = merge(time_series, obsData, all.y=T, all.x=F)
			obsData = obsData[order(obsData$i),]
			obsData$VARNAME_DATE = paste(
				observed_table[obs_i, 1],
				format(obsData$DATE, "%m"),
				format(obsData$DATE, "%Y"),
				sep="_")
		} else {
			stop("Routines not written for daily data")
		}
		poll_col_name = names(obsData)[5]
		obsData = obsData[c("i","VARNAME_DATE", poll_col_name)]
		if (poll_col_name == "SS_KG") {
			obsData$SS_KG = obsData$SS_KG / 1000
		}
	}
		
	l1 = paste(observed_table[obs_i,1], "_", observed_table[obs_i,3],
        "   : this is the name of the variable and the subbasin number to be included in the objective function",
        sep="")
    l2 = paste(nrow(obsData), "   : number of data points for this variable as it follows below. First column is a sequential number from beginning", sep="")
    l3 = "      : of the simulation, second column is variable name and date (format arbitrary), third column is variable value."
    
    write("\n", observed_rch_file, append=T)
    write(l1, observed_rch_file, append=T)
    write(l2, observed_rch_file, append=T)
    write(l3, observed_rch_file, append=T)
    write("\n", observed_rch_file, append=T)
    
    write.table(obsData, observed_rch_file, sep="\t", row.names=F, col.names=F, append=T, quote=F)
    # Observed.txt
    write(paste(observed_table[obs_i,1], "_", observed_table[obs_i,3],
                "\t: this is the name of the variable and the subbasin number to be included in the objective function",
                sep=""),
          observed_file, append=T)
	if (weight_by_sample_size) {
		wt = 1/nrow(obsData)
	} else {
		wt = 1
	}
    write(
		paste(
			wt,
			"\t: weight of the variable in the objective function\n-1    : Dynamic flow separation. Not considered if -1. If 1, then values should be added in the forth column below after observations\n-1    : constant flow separation, threshold value. (not considered if -1)\n1     : if separation of signal is considered, this is weight of the smaller values in the objective function\n1     : if separation of signal is considered, this is weight of the larger values in the objective function\n10    : percentage of measurement error",
          	sep=""
		),
		observed_file, append=T)
    write(paste(nrow(obsData), ": number of data points for this variable as it follows below. First column is a sequential number from beginning",
        sep="\t"),
        observed_file, append=T)
    write("      : of the simulation, second column is variable name and date (format arbitrary), third column is variable value.",
        observed_file, append=T)
    write("\n", observed_file, append=T)
    write.table(obsData, observed_file, sep="\t", row.names=F, col.names=F, append=T, quote=F)
    write("\n", observed_file, append=T)   
}
# write extract_rch table
write("output.rch     : swat output file name", extract_rch_file)
write(paste(length(unique(observed_table[,1])), ": number of variables to get", sep="\t"),
      extract_rch_file,
      append=T)
write(
    paste(
        paste(unique(observed_table[,2]), collapse=" "),
        ": variable column number(s) in the swat output file (as many as the above number)"
        , sep="\t"),
    extract_rch_file,
    append=T
)
write("", extract_rch_file, append=T)
write(paste(subbasinCount, ": total number of reaches (subbasins) in the project", sep="\t"),
      extract_rch_file,
      append=T)
write("", extract_rch_file, append=T)
for (variable in unique(observed_table[,2])) {
    reaches = unique(observed_table[observed_table[,2] == variable, 3]) 
    write(paste(
        length(reaches),
        ": number of reaches (subbasins) to get for variable"
        , sep="\t"),
        extract_rch_file,
        append=T)
    write(paste(paste(reaches, collapse=" "),
        ": reach (subbasin) numbers for variable (ordered)",
        sep="\t"),
        extract_rch_file,
        append=T)
}
write("", extract_rch_file, append=T)
write(paste(startYr, ": beginning year of simulation not including the warm up period", sep="\t"),
      extract_rch_file,
      append=T)
write(paste(endYr, ": end year of simulation", sep="\t"),
      extract_rch_file,
      append=T)
write("", extract_rch_file, append=T)
if (monthly) {
    write(paste(2, ": time step (1=daily, 2=monthly, 3=yearly)", sep="\t"),
          extract_rch_file,
          append=T)
} else {
    write(paste(1, ": time step (1=daily, 2=monthly, 3=yearly)", sep="\t"),
          extract_rch_file,
          append=T)
}
filenames = paste(observed_table[,1], "_", observed_table[,3], ".txt", sep="")
write(filenames, var_file_name)
write(filenames, var_file_rch)
