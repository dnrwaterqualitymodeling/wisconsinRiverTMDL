# CHANGE THESE ###########
# SWAT project

#projectDir = "D:/WRB2.Sufi2.SwatCup"
#projectDir = "D:/WRB2_driftless.Sufi2.SwatCup"
#projectDir = "D:/WRB2_sands.Sufi2.SwatCup"
#projectDir = "D:/WRB2_north_central.Sufi2.SwatCup"
#projectDir = "D:/WRB2_northern.Sufi2.SwatCup"

#projectDir = "D:/WRB_generalized/WRB.Sufi2.SwatCup"
#projectDir = "D:/WRB_generalized/WRB_driftless.Sufi2.SwatCup"
#projectDir = "D:/WRB_generalized/WRB_sands.Sufi2.SwatCup"
#projectDir = "D:/WRB_generalized/WRB_north_central.Sufi2.SwatCup"
projectDir = "D:/WRB_generalized/WRB_northern.Sufi2.SwatCup"

simCount = 128
subbasinCount = 337
startYr = 2002
endYr = 2013
objFuncCode = 5
thresh = 0
monthly = F
weight_by_sample_size = F
excl_ec_pine_rib = T
sands_east = F
sands_west = F
just_muskellunge = T

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
obsDir = "D:/usgs_raw/calibration/AMJJASO_50_pct_exc_northern"

# 4. Groundwater
#obsDir = "D:/usgs_raw/calibration/AMJJASO_driftless"
#obsDir = "D:/usgs_raw/calibration/JJAS_50_pct_exc_driftless"
#obsDir = "D:/usgs_raw/calibration/JJAS_50_pct_exc_sands"
#obsDir = "D:/usgs_raw/calibration/JJAS_50_pct_exc_north_central"
#obsDir = "D:/usgs_raw/calibration/JJAS_50_pct_exc_northern"


#gage_subbasin_lu =
#	read.csv("T:/Projects/Wisconsin_River/GIS_Datasets/observed/gauge_basin_lookup.csv",
gage_subbasin_lu = read.csv("D:/gauge_basin_lookup.csv",
	colClasses=c(rep("character", 5), "integer", "integer", "character"))
setInternet2(TRUE)

#parameterization = rbind(
#	# Western Coulees and Ridges
#	c("r__CN2.mgt________5-29,32,33,35,40-42,44,48-51,55,57,58,137,138,179-189,192,196,227-233,237,246,274,301,310",-0.13,-0.11),
#	c("v__ESCO.hru________5-29,32,33,35,40-42,44,48-51,55,57,58,137,138,179-189,192,196,227-233,237,246,274,301,310",0.4,0.5),
#	c("r__SURLAG.hru________5-29,32,33,35,40-42,44,48-51,55,57,58,137,138,179-189,192,196,227-233,237,246,274,301,310",-0.6,-0.5),
#	c("r__SLSUBBSN.hru________5-29,32,33,35,40-42,44,48-51,55,57,58,137,138,179-189,192,196,227-233,237,246,274,301,310",0.4,0.5),
#	c("r__SOL_K().sol________5-29,32,33,35,40-42,44,48-51,55,57,58,137,138,179-189,192,196,227-233,237,246,274,301,310",-0.6,-0.2),
#	c("v__CH_N1.sub________5-29,32,33,35,40-42,44,48-51,55,57,58,137,138,179-189,192,196,227-233,237,246,274,301,310",0.03,0.05),
#	c("v__CH_N2.rte________5-29,32,33,35,40-42,44,48-51,55,57,58,137,138,179-189,192,196,227-233,237,246,274,301,310",0.06,0.08),
#	c("v__GW_DELAY.gw________5-29,32,33,35,40-42,44,48-51,55,57,58,137,138,179-189,192,196,227-233,237,246,274,301,310",100,250),
#	c("v__GWQMN.gw________5-29,32,33,35,40-42,44,48-51,55,57,58,137,138,179-189,192,196,227-233,237,246,274,301,310",0,500),
#	c("v__GW_REVAP.gw________5-29,32,33,35,40-42,44,48-51,55,57,58,137,138,179-189,192,196,227-233,237,246,274,301,310",0.005,0.04),
#	c("r__ALPHA_BF.gw________5-29,32,33,35,40-42,44,48-51,55,57,58,137,138,179-189,192,196,227-233,237,246,274,301,310",-1,-0.7),
#	c("v__REVAPMN.gw________5-29,32,33,35,40-42,44,48-51,55,57,58,137,138,179-189,192,196,227-233,237,246,274,301,310",0,500),
#	# Forest Transition
#	c("r__CN2.mgt________68-71,78-112,122,136,146,147,149-159,162,166,169,201,207,211-220,225,258,259,261-270,275,276,279,282,286-299,307,308,313,314,317,320-329,331,332",0.05,0.15),
#	c("v__ESCO.hru________68-71,78-112,122,136,146,147,149-159,162,166,169,201,207,211-220,225,258,259,261-270,275,276,279,282,286-299,307,308,313,314,317,320-329,331,332",0.8,1),
#	c("r__SURLAG.hru________68-71,78-112,122,136,146,147,149-159,162,166,169,201,207,211-220,225,258,259,261-270,275,276,279,282,286-299,307,308,313,314,317,320-329,331,332",-0.8,-0.2),
#	c("v__CH_N1.sub________68-71,78-112,122,136,146,147,149-159,162,166,169,201,207,211-220,225,258,259,261-270,275,276,279,282,286-299,307,308,313,314,317,320-329,331,332",0.05,0.08),
#	c("v__CH_N2.rte________68-71,78-112,122,136,146,147,149-159,162,166,169,201,207,211-220,225,258,259,261-270,275,276,279,282,286-299,307,308,313,314,317,320-329,331,332",0.05,0.08),
#	c("r__SLSUBBSN.hru________68-71,78-112,122,136,146,147,149-159,162,166,169,201,207,211-220,225,258,259,261-270,275,276,279,282,286-299,307,308,313,314,317,320-329,331,332",-0.5,0),
#	# Central Sand Plains
#	c("v__GW_DELAY.gw________1-4,30,31,34,36-39,43,45-47,52-54,56,59-67,72-77,139-145,148,170-178,190,191,193-195,197-200,202-206,208,210,234-236,238-245,247-257,260,302-306,311,312,330,333",300,800),
#	c("v__GW_REVAP.gw________1-4,30,31,34,36-39,43,45-47,52-54,56,59-67,72-77,139-145,148,170-178,190,191,193-195,197-200,202-206,208,210,234-236,238-245,247-257,260,302-306,311,312,330,333",0,0.04),
#	c("r__ALPHA_BF.gw________1-4,30,31,34,36-39,43,45-47,52-54,56,59-67,72-77,139-145,148,170-178,190,191,193-195,197-200,202-206,208,210,234-236,238-245,247-257,260,302-306,311,312,330,333",-0.98,-0.7),
#	c("v__ESCO.hru________1-4,30,31,34,36-39,43,45-47,52-54,56,59-67,72-77,139-145,148,170-178,190,191,193-195,197-200,202-206,208,210,234-236,238-245,247-257,260,302-306,311,312,330,333",0.8,1),
#	c("r__SURLAG.hru________1-4,30,31,34,36-39,43,45-47,52-54,56,59-67,72-77,139-145,148,170-178,190,191,193-195,197-200,202-206,208,210,234-236,238-245,247-257,260,302-306,311,312,330,333",-0.99,-0.8),
#	c("r__CN2.mgt________1-4,30,31,34,36-39,43,45-47,52-54,56,59-67,72-77,139-145,148,170-178,190,191,193-195,197-200,202-206,208,210,234-236,238-245,247-257,260,302-306,311,312,330,333",0,0.15)
#)

## Driftless Area
#parameterization = rbind(
## Peak growing Driftless Area
#	c("r__CN2.mgt________1-29,31-33,35,37,40-44,48-51,54-58,137-138,170-189,196,227-233,235-241,243,246,274,301,310,312",-0.13,-0.11),
#	c("v__ESCO.hru________1-29,31-33,35,37,40-44,48-51,54-58,137-138,170-189,196,227-233,235-241,243,246,274,301,310,312",0.4,0.5),
#	c("r__SURLAG.hru________1-29,31-33,35,37,40-44,48-51,54-58,137-138,170-189,196,227-233,235-241,243,246,274,301,310,312",-0.6,-0.5),
#	c("r__SLSUBBSN.hru________1-29,31-33,35,37,40-44,48-51,54-58,137-138,170-189,196,227-233,235-241,243,246,274,301,310,312",0.4,0.5),
#	c("r__SOL_K().sol________1-29,31-33,35,37,40-44,48-51,54-58,137-138,170-189,196,227-233,235-241,243,246,274,301,310,312",-0.11,-0.9),
#	c("v__CH_N1.sub________1-29,31-33,35,37,40-44,48-51,54-58,137-138,170-189,196,227-233,235-241,243,246,274,301,310,312",0.03,0.05),
#	c("v__CH_N2.rte________1-29,31-33,35,37,40-44,48-51,54-58,137-138,170-189,196,227-233,235-241,243,246,274,301,310,312",0.06,0.08),
## Groundwater Driftless Area
#	c("r__ALPHA_BF.gw________1-29,31-33,35,37,40-44,48-51,54-58,137-138,170-189,196,227-233,235-241,243,246,274,301,310,312",-0.9999,-0.1),
#	c("v__GW_DELAY.gw________1-29,31-33,35,37,40-44,48-51,54-58,137-138,170-189,196,227-233,235-241,243,246,274,301,310,312",10,330),
#	c("v__RCHRG_DP.gw________1-29,31-33,35,37,40-44,48-51,54-58,137-138,170-189,196,227-233,235-241,243,246,274,301,310,312",0.0001,0.15),
#	c("v__GWQMN.gw________1-29,31-33,35,37,40-44,48-51,54-58,137-138,170-189,196,227-233,235-241,243,246,274,301,310,312",0,2000),
#	c("v__GW_REVAP.gw________1-29,31-33,35,37,40-44,48-51,54-58,137-138,170-189,196,227-233,235-241,243,246,274,301,310,312",0.005,0.1),
#	c("v__REVAPMN.gw________1-29,31-33,35,37,40-44,48-51,54-58,137-138,170-189,196,227-233,235-241,243,246,274,301,310,312",0,500)
#)

# North Central Hardwood Forests
#parameterization = rbind(
#	c("r__CN2.mgt________62,64-72,78-108,146,147,150-159,200,201,207,211-217,256,258,259,262-269,275,276,288-293,296,297,307,308,313,314,321-329,331,332",0.5,0.15),
#	c("v__ESCO.hru________62,64-72,78-108,146,147,150-159,200,201,207,211-217,256,258,259,262-269,275,276,288-293,296,297,307,308,313,314,321-329,331,332",0.8,1),
#	c("r__SURLAG.hru________62,64-72,78-108,146,147,150-159,200,201,207,211-217,256,258,259,262-269,275,276,288-293,296,297,307,308,313,314,321-329,331,332",-0.8,-0.2),
#	c("r__SOL_AWC().sol________62,64-72,78-108,146,147,150-159,200,201,207,211-217,256,258,259,262-269,275,276,288-293,296,297,307,308,313,314,321-329,331,332",0.5,0.15),
#	c("v__CH_N1.sub________62,64-72,78-108,146,147,150-159,200,201,207,211-217,256,258,259,262-269,275,276,288-293,296,297,307,308,313,314,321-329,331,332",0.05,0.08),
#	c("v__CH_N2.rte________62,64-72,78-108,146,147,150-159,200,201,207,211-217,256,258,259,262-269,275,276,288-293,296,297,307,308,313,314,321-329,331,332",0.05,0.08),
#	c("r__SLSUBBSN.hru________62,64-72,78-108,146,147,150-159,200,201,207,211-217,256,258,259,262-269,275,276,288-293,296,297,307,308,313,314,321-329,331,332",-0.5,0),
#	c("r__HRU_SLP.hru________62,64-72,78-108,146,147,150-159,200,201,207,211-217,256,258,259,262-269,275,276,288-293,296,297,307,308,313,314,321-329,331,332",0,0.5),
#	c("r__ALPHA_BF.gw________62,64-72,78-108,146,147,150-159,200,201,207,211-217,256,258,259,262-269,275,276,288-293,296,297,307,308,313,314,321-329,331,332",-0.9999,-0.1),
#	c("v__GW_DELAY.gw________62,64-72,78-108,146,147,150-159,200,201,207,211-217,256,258,259,262-269,275,276,288-293,296,297,307,308,313,314,321-329,331,332",10,330),
#	c("v__RCHRG_DP.gw________62,64-72,78-108,146,147,150-159,200,201,207,211-217,256,258,259,262-269,275,276,288-293,296,297,307,308,313,314,321-329,331,332",0.0001,0.15),
#	c("v__GWQMN.gw________62,64-72,78-108,146,147,150-159,200,201,207,211-217,256,258,259,262-269,275,276,288-293,296,297,307,308,313,314,321-329,331,332",0,2000),
#	c("v__GW_REVAP.gw________62,64-72,78-108,146,147,150-159,200,201,207,211-217,256,258,259,262-269,275,276,288-293,296,297,307,308,313,314,321-329,331,332",0.005,0.1),
#	c("v__REVAPMN.gw________62,64-72,78-108,146,147,150-159,200,201,207,211-217,256,258,259,262-269,275,276,288-293,296,297,307,308,313,314,321-329,331,332",0,500)
#)

## Sands and Moraines
#parameterization = rbind(
## Peak growing Northern Lakes and Forests
#	c("v__GW_DELAY.gw________30,34,36,38,39,45-47,52,53,59-61,63,73-77,139-145,148,149,190-195,197-199,202-206,208,210,234,242,244,245,247-255,257,260,261,302-306,311,330,333",200,300),
#	c("r__ALPHA_BF.gw________30,34,36,38,39,45-47,52,53,59-61,63,73-77,139-145,148,149,190-195,197-199,202-206,208,210,234,242,244,245,247-255,257,260,261,302-306,311,330,333",-0.999,-0.8),
#	c("v__RCHRG_DP.gw________30,34,36,38,39,45-47,52,53,59-61,63,73-77,139-145,148,149,190-195,197-199,202-206,208,210,234,242,244,245,247-255,257,260,261,302-306,311,330,333",0.0001,0.15),
#	c("v__GWQMN.gw________30,34,36,38,39,45-47,52,53,59-61,63,73-77,139-145,148,149,190-195,197-199,202-206,208,210,234,242,244,245,247-255,257,260,261,302-306,311,330,333",0,2000),
#	c("v__GW_REVAP.gw________30,34,36,38,39,45-47,52,53,59-61,63,73-77,139-145,148,149,190-195,197-199,202-206,208,210,234,242,244,245,247-255,257,260,261,302-306,311,330,333",0.005,0.1),
#	c("v__REVAPMN.gw________30,34,36,38,39,45-47,52,53,59-61,63,73-77,139-145,148,149,190-195,197-199,202-206,208,210,234,242,244,245,247-255,257,260,261,302-306,311,330,333",0,500),	
#	c("v__ESCO.hru________30,34,36,38,39,45-47,52,53,59-61,63,73-77,139-145,148,149,190-195,197-199,202-206,208,210,234,242,244,245,247-255,257,260,261,302-306,311,330,333",0.01,0.2),
#	c("r__PND_FR.pnd________30,34,36,38,39,45-47,52,53,59-61,63,73-77,139-145,148,149,190-195,197-199,202-206,208,210,234,242,244,245,247-255,257,260,261,302-306,311,330,333",0.5,3),
#	c("r__PND_EVOL.pnd________30,34,36,38,39,45-47,52,53,59-61,63,73-77,139-145,148,149,190-195,197-199,202-206,208,210,234,242,244,245,247-255,257,260,261,302-306,311,330,333",3,5),
#	c("r__SURLAG.hru________30,34,36,38,39,45-47,52,53,59-61,63,73-77,139-145,148,149,190-195,197-199,202-206,208,210,234,242,244,245,247-255,257,260,261,302-306,311,330,333",-0.94,-0.8),
#	c("r__CN2.mgt________30,34,36,38,39,45-47,52,53,59-61,63,73-77,139-145,148,149,190-195,197-199,202-206,208,210,234,242,244,245,247-255,257,260,261,302-306,311,330,333",-0.1,0),
#	c("r__SOL_K().sol________30,34,36,38,39,45-47,52,53,59-61,63,73-77,139-145,148,149,190-195,197-199,202-206,208,210,234,242,244,245,247-255,257,260,261,302-306,311,330,333",0,0.075),
#	c("r__SOL_AWC().sol________30,34,36,38,39,45-47,52,53,59-61,63,73-77,139-145,148,149,190-195,197-199,202-206,208,210,234,242,244,245,247-255,257,260,261,302-306,311,330,333",-0.075,0.05),
#	c("r__SOL_Z().sol________30,34,36,38,39,45-47,52,53,59-61,63,73-77,139-145,148,149,190-195,197-199,202-206,208,210,234,242,244,245,247-255,257,260,261,302-306,311,330,333",-0.075,0.05)
#)

# Northern Lakes and Forests
parameterization = rbind(
# Peak growing Northern Lakes and Forests
	c("r__CN2.mgt________109-136,160-169,209,218-226,270-273,277-287,294-295,298-300,309,315-320,334-337",-0.15,0.15),
	c("v__ESCO.hru________109-136,160-169,209,218-226,270-273,277-287,294-295,298-300,309,315-320,334-337",0.6,1),
	c("r__SURLAG.hru________109-136,160-169,209,218-226,270-273,277-287,294-295,298-300,309,315-320,334-337",-1,-0.8),
	c("r__PND_EVOL.pnd________109-136,160-169,209,218-226,270-273,277-287,294,295,298-300,309,315-320,334-337",0,3),
	c("r__PND_FR.pnd________109-136,160-169,209,218-226,270-273,277-287,294,295,298-300,309,315-320,334-337",0,3),
	c("v__PND_K.pnd________109-136,160-169,209,218-226,270-273,277-287,294,295,298-300,309,315-320,334-337",0,150),
	c("r__SOL_AWC().sol________109-136,160-169,209,218-226,270-273,277-287,294-295,298-300,309,315-320,334-337",-0.1,0.1),
	c("r__SOL_BD().sol________109-136,160-169,209,218-226,270-273,277-287,294-295,298-300,309,315-320,334-337",-0.1,0.1),
	c("r__SOL_K().sol________109-136,160-169,209,218-226,270-273,277-287,294-295,298-300,309,315-320,334-337",-0.1,0.1)
#	c("v__GW_DELAY.gw________109-136,160-169,209,218-226,270-273,277-287,294-295,298-300,309,315-320,334-337",200,300),
#	c("r__ALPHA_BF.gw________109-136,160-169,209,218-226,270-273,277-287,294-295,298-300,309,315-320,334-337",-0.999,-0.8),
#	c("v__RCHRG_DP.gw________109-136,160-169,209,218-226,270-273,277-287,294-295,298-300,309,315-320,334-337",0.0001,0.15),
#	c("v__GWQMN.gw________109-136,160-169,209,218-226,270-273,277-287,294-295,298-300,309,315-320,334-337",0,2000),
#	c("v__GW_REVAP.gw________109-136,160-169,209,218-226,270-273,277-287,294-295,298-300,309,315-320,334-337",0.005,0.1),
#	c("v__REVAPMN.gw________109-136,160-169,209,218-226,270-273,277-287,294-295,298-300,309,315-320,334-337",0,500),	
	
)

##### GROUNDWATER #####

## Driftless Area
#parameterization = rbind(
## Snowmelt basin
#	c("r__SMTMP.bsn",-0.000171,0.000171),
#	c("r__SFTMP.bsn",-0.000179,0.000179),
#	c("r__TIMP.bsn",-0.000051,0.000051),
#	c("r__SMFMN.bsn",-0.0003,0.0003),
#	c("r__SMFMX.bsn",-0.000312,0.000312),
#	c("r__SNOCOVMX.bsn",-0.002234,0.002234),
#	c("r__SNO50COV.bsn",-0.00002,0.00002),
#	c("r__SURLAG.hru",-0.000704,0.000704),
## Snowmelt Driftless Area
#	c("r__CANMX.hru________1-29,31-33,35,37,40-44,48-51,54-58,137-138,170-189,196,227-233,235-241,243,246,274,301,310,312",-0.000348,0.000348),
#	c("r__PND_EVOL.pnd________1-29,31-33,35,37,40-44,48-51,54-58,137-138,170-189,196,227-233,235-241,243,246,274,301,310,312",-0.000148,0.000148),
#	c("r__PND_FR.pnd________1-29,31-33,35,37,40-44,48-51,54-58,137-138,170-189,196,227-233,235-241,243,246,274,301,310,312",-0.000147,0.000147),
## Growing season runoff Driftless Area
#	c("r__CN2.mgt________1-29,31-33,35,37,40-44,48-51,54-58,137-138,170-189,196,227-233,235-241,243,246,274,301,310,312",-0.000023,0.000023),
#	c("r__ESCO.hru________1-29,31-33,35,37,40-44,48-51,54-58,137-138,170-189,196,227-233,235-241,243,246,274,301,310,312",-0.000717,0.000717),
## Groundwater Driftless Area
#	c("r__ALPHA_BF.gw________1-29,31-33,35,37,40-44,48-51,54-58,137-138,170-189,196,227-233,235-241,243,246,274,301,310,312",-0.9999,-0.1),
#	c("v__GW_DELAY.gw________1-29,31-33,35,37,40-44,48-51,54-58,137-138,170-189,196,227-233,235-241,243,246,274,301,310,312",10,330),
#	c("v__RCHRG_DP.gw________1-29,31-33,35,37,40-44,48-51,54-58,137-138,170-189,196,227-233,235-241,243,246,274,301,310,312",0.0001,0.15),
#	c("v__GWQMN.gw________1-29,31-33,35,37,40-44,48-51,54-58,137-138,170-189,196,227-233,235-241,243,246,274,301,310,312",0,2000),
#	c("v__GW_REVAP.gw________1-29,31-33,35,37,40-44,48-51,54-58,137-138,170-189,196,227-233,235-241,243,246,274,301,310,312",0.005,0.1),
#	c("v__REVAPMN.gw________1-29,31-33,35,37,40-44,48-51,54-58,137-138,170-189,196,227-233,235-241,243,246,274,301,310,312",0,500),
#	c("r__SOL_AWC().sol________1-29,31-33,35,37,40-44,48-51,54-58,137-138,170-189,196,227-233,235-241,243,246,274,301,310,312",-0.1,0.1),
#	c("r__SOL_BD().sol________1-29,31-33,35,37,40-44,48-51,54-58,137-138,170-189,196,227-233,235-241,243,246,274,301,310,312",-0.1,0.1),
#	c("r__SOL_K().sol________1-29,31-33,35,37,40-44,48-51,54-58,137-138,170-189,196,227-233,235-241,243,246,274,301,310,312",-0.1,0.1)
#)

## North Central Hardwood Forests
#parameterization = rbind(
## Snowmelt basin
#	c("r__SMTMP.bsn",-0.000171,0.000171),
#	c("r__SFTMP.bsn",-0.000179,0.000179),
#	c("r__TIMP.bsn",-0.000051,0.000051),
#	c("r__SMFMN.bsn",-0.0003,0.0003),
#	c("r__SMFMX.bsn",-0.000312,0.000312),
#	c("r__SNOCOVMX.bsn",-0.002234,0.002234),
#	c("r__SNO50COV.bsn",-0.00002,0.00002),
#	c("r__SURLAG.hru",-0.000704,0.000704),
## Snowmelt North Central Hardwood Forests
#	c("r__CANMX.hru________62,64-72,78-108,146,147,150-159,200,201,207,211-217,256,258,259,262-269,275,276,288-293,296,297,307,308,313,314,321-329,331,332",-0.000206,0.000206),
#	c("r__PND_EVOL.pnd________62,64-72,78-108,146,147,150-159,200,201,207,211-217,256,258,259,262-269,275,276,288-293,296,297,307,308,313,314,321-329,331,332",-0.000275,0.000275),
#	c("r__PND_FR.pnd________62,64-72,78-108,146,147,150-159,200,201,207,211-217,256,258,259,262-269,275,276,288-293,296,297,307,308,313,314,321-329,331,332",-0.000241,0.000241),
## Growing season runoff North Central Hardwood Forests
#	c("r__CN2.mgt________62,64-72,78-108,146,147,150-159,200,201,207,211-217,256,258,259,262-269,275,276,288-293,296,297,307,308,313,314,321-329,331,332",-0.000042,0.000042),
#	c("r__ESCO.hru________62,64-72,78-108,146,147,150-159,200,201,207,211-217,256,258,259,262-269,275,276,288-293,296,297,307,308,313,314,321-329,331,332",-0.00008,0.00008),
## Groundwater North Central Hardwood Forests
#	c("r__ALPHA_BF.gw________62,64-72,78-108,146,147,150-159,200,201,207,211-217,256,258,259,262-269,275,276,288-293,296,297,307,308,313,314,321-329,331,332",-0.9999,-0.1),
#	c("v__GW_DELAY.gw________62,64-72,78-108,146,147,150-159,200,201,207,211-217,256,258,259,262-269,275,276,288-293,296,297,307,308,313,314,321-329,331,332",10,330),
#	c("v__RCHRG_DP.gw________62,64-72,78-108,146,147,150-159,200,201,207,211-217,256,258,259,262-269,275,276,288-293,296,297,307,308,313,314,321-329,331,332",0.0001,0.15),
#	c("v__GWQMN.gw________62,64-72,78-108,146,147,150-159,200,201,207,211-217,256,258,259,262-269,275,276,288-293,296,297,307,308,313,314,321-329,331,332",0,2000),
#	c("v__GW_REVAP.gw________62,64-72,78-108,146,147,150-159,200,201,207,211-217,256,258,259,262-269,275,276,288-293,296,297,307,308,313,314,321-329,331,332",0.005,0.1),
#	c("v__REVAPMN.gw________62,64-72,78-108,146,147,150-159,200,201,207,211-217,256,258,259,262-269,275,276,288-293,296,297,307,308,313,314,321-329,331,332",0,500),
#	c("r__SOL_AWC().sol________62,64-72,78-108,146,147,150-159,200,201,207,211-217,256,258,259,262-269,275,276,288-293,296,297,307,308,313,314,321-329,331,332",-0.1,0.1),
#	c("r__SOL_BD().sol________62,64-72,78-108,146,147,150-159,200,201,207,211-217,256,258,259,262-269,275,276,288-293,296,297,307,308,313,314,321-329,331,332",-0.1,0.1),
#	c("r__SOL_K().sol________62,64-72,78-108,146,147,150-159,200,201,207,211-217,256,258,259,262-269,275,276,288-293,296,297,307,308,313,314,321-329,331,332",-0.1,0.1)
#)

## Central Sands and Moraines
#parameterization = rbind(
## Snowmelt basin
#	c("r__SMTMP.bsn",-0.000171,0.000171),
#	c("r__SFTMP.bsn",-0.000179,0.000179),
#	c("r__TIMP.bsn",-0.000051,0.000051),
#	c("r__SMFMN.bsn",-0.0003,0.0003),
#	c("r__SMFMX.bsn",-0.000312,0.000312),
#	c("r__SNOCOVMX.bsn",-0.002234,0.002234),
#	c("r__SNO50COV.bsn",-0.00002,0.00002),
#	c("r__SURLAG.hru",-0.000704,0.000704),
## Snowmelt Central Sands and Moraines
#	c("r__PND_EVOL.pnd________30,34,36,38,39,45-47,52,53,59-61,63,73-77,139-145,148,149,190-195,197-199,202-206,208,210,234,242,244,245,247-255,257,260,261,302-306,311,330,333",-0.000240,0.000240),
#	c("r__PND_FR.pnd________30,34,36,38,39,45-47,52,53,59-61,63,73-77,139-145,148,149,190-195,197-199,202-206,208,210,234,242,244,245,247-255,257,260,261,302-306,311,330,333",-0.000942,0.000942),
## Growing season runoff Central Sands and Moraines
#	c("r__CN2.mgt________30,34,36,38,39,45-47,52,53,59-61,63,73-77,139-145,148,149,190-195,197-199,202-206,208,210,234,242,244,245,247-255,257,260,261,302-306,311,330,333",-0.000029,0.000029),
#	c("r__ESCO.hru________30,34,36,38,39,45-47,52,53,59-61,63,73-77,139-145,148,149,190-195,197-199,202-206,208,210,234,242,244,245,247-255,257,260,261,302-306,311,330,333",-0.000103,0.000103),
## Groundwater Central Sands and Moraines
#	c("r__ALPHA_BF.gw________30,34,36,38,39,45-47,52,53,59-61,63,73-77,139-145,148,149,190-195,197-199,202-206,208,210,234,242,244,245,247-255,257,260,261,302-306,311,330,333",-0.9999,-0.1),
#	c("v__GW_DELAY.gw________30,34,36,38,39,45-47,52,53,59-61,63,73-77,139-145,148,149,190-195,197-199,202-206,208,210,234,242,244,245,247-255,257,260,261,302-306,311,330,333",10,330),
#	c("v__RCHRG_DP.gw________30,34,36,38,39,45-47,52,53,59-61,63,73-77,139-145,148,149,190-195,197-199,202-206,208,210,234,242,244,245,247-255,257,260,261,302-306,311,330,333",0.0001,0.15),
#	c("v__GWQMN.gw________30,34,36,38,39,45-47,52,53,59-61,63,73-77,139-145,148,149,190-195,197-199,202-206,208,210,234,242,244,245,247-255,257,260,261,302-306,311,330,333",0,2000),
#	c("v__GW_REVAP.gw________30,34,36,38,39,45-47,52,53,59-61,63,73-77,139-145,148,149,190-195,197-199,202-206,208,210,234,242,244,245,247-255,257,260,261,302-306,311,330,333",0.005,0.1),
#	c("v__REVAPMN.gw________30,34,36,38,39,45-47,52,53,59-61,63,73-77,139-145,148,149,190-195,197-199,202-206,208,210,234,242,244,245,247-255,257,260,261,302-306,311,330,333",0,500),
#	c("r__SOL_AWC().sol________30,34,36,38,39,45-47,52,53,59-61,63,73-77,139-145,148,149,190-195,197-199,202-206,208,210,234,242,244,245,247-255,257,260,261,302-306,311,330,333",-0.1,0.1),
#	c("r__SOL_BD().sol________30,34,36,38,39,45-47,52,53,59-61,63,73-77,139-145,148,149,190-195,197-199,202-206,208,210,234,242,244,245,247-255,257,260,261,302-306,311,330,333",-0.1,0.1),
#	c("r__SOL_K().sol________30,34,36,38,39,45-47,52,53,59-61,63,73-77,139-145,148,149,190-195,197-199,202-206,208,210,234,242,244,245,247-255,257,260,261,302-306,311,330,333",-0.1,0.1)
#)

# Northern Lakes and Forests
#parameterization = rbind(
## Snowmelt basin
#	c("r__SMTMP.bsn",-0.000171,0.000171),
#	c("r__SFTMP.bsn",-0.000179,0.000179),
#	c("r__TIMP.bsn",-0.000051,0.000051),
#	c("r__SMFMN.bsn",-0.0003,0.0003),
#	c("r__SMFMX.bsn",-0.000312,0.000312),
#	c("r__SNOCOVMX.bsn",-0.002234,0.002234),
#	c("r__SNO50COV.bsn",-0.00002,0.00002),
#	c("r__SURLAG.hru",-0.000704,0.000704),
## Snowmelt Northern Lakes and Forests
#	c("r__CANMX.hru________109-136,160-169,209,218-226,270-273,277-287,294-295,298-300,309,315-320,334-337",-0.000098,0.000098),
#	c("r__PND_EVOL.pnd________109-136,160-169,209,218-226,270-273,277-287,294-295,298-300,309,315-320,334-337",-0.000148,0.000148),
#	c("r__PND_FR.pnd________109-136,160-169,209,218-226,270-273,277-287,294-295,298-300,309,315-320,334-337",-0.000627,0.000627),
## Growing season runoff Northern Lakes and Forests
#	c("r__CN2.mgt________109-136,160-169,209,218-226,270-273,277-287,294-295,298-300,309,315-320,334-337",-0.000024,0.000024),
#	c("r__ESCO.hru________109-136,160-169,209,218-226,270-273,277-287,294-295,298-300,309,315-320,334-337",-0.000703,0.000703),
## Groundwater Northern Lakes and Forests
#	c("r__ALPHA_BF.gw________109-136,160-169,209,218-226,270-273,277-287,294-295,298-300,309,315-320,334-337",-0.9999,-0.1),
#	c("v__GW_DELAY.gw________109-136,160-169,209,218-226,270-273,277-287,294-295,298-300,309,315-320,334-337",10,330),
#	c("v__RCHRG_DP.gw________109-136,160-169,209,218-226,270-273,277-287,294-295,298-300,309,315-320,334-337",0.0001,0.15),
#	c("v__GWQMN.gw________109-136,160-169,209,218-226,270-273,277-287,294-295,298-300,309,315-320,334-337",0,2000),
#	c("v__GW_REVAP.gw________109-136,160-169,209,218-226,270-273,277-287,294-295,298-300,309,315-320,334-337",0.005,0.1),
#	c("v__REVAPMN.gw________109-136,160-169,209,218-226,270-273,277-287,294-295,298-300,309,315-320,334-337",0,500),
#	c("r__SOL_AWC().sol________109-136,160-169,209,218-226,270-273,277-287,294-295,298-300,309,315-320,334-337",-0.1,0.1),
#	c("r__SOL_BD().sol________109-136,160-169,209,218-226,270-273,277-287,294-295,298-300,309,315-320,334-337",-0.1,0.1),
#	c("r__SOL_K().sol________109-136,160-169,209,218-226,270-273,277-287,294-295,298-300,309,315-320,334-337",-0.1,0.1)
#)


# Don't change these
source("https://raw.githubusercontent.com/dnrwaterqualitymodeling/wisconsinRiverTMDL/master/calibration/functions_query_output.r")

# Change absolute values for ponds and wetlands
file_abs_vol = paste(projectDir, "Absolute_SWAT_Values.txt", sep="/")
abs_vol = readLines(file_abs_vol)
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
gage_subbasin_lu = gage_subbasin_lu[c("USGS_ID", "WRB_SubbasinID")]
observed_table = cbind(rep("FLOW_OUT", nrow(gage_subbasin_lu)),
    rep(6, nrow(gage_subbasin_lu)),
    gage_subbasin_lu$WRB_SubbasinID,
    paste(obsDir, "/", gage_subbasin_lu$USGS_ID, ".txt", sep="")
)
if (nrow(observed_table) > 1) {
	observed_table = observed_table[order(as.integer(observed_table[,3])),]
	observed_table = observed_table[
		observed_table[,4] %in% list.files(obsDir, full.names=T),
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
file.cio.dat = readLines(file.cio)
if (monthly) {
	file.cio.dat[59] = "               0    | IPRINT: print code (month, day, year)"
} else {
	file.cio.dat[59] = "               1    | IPRINT: print code (month, day, year)"
}
file.cio.dat[65] = "   2   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0"
file.cio.dat[67] = "   4   0   0   0   0   0   0   0   0   0   0   0   0   0   0"
file.cio.dat[69] = "   6   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0"
file.cio.dat[71] = "   1   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0"
# SEDOUT = 10
# TP = 49
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
        paste(unique(observed_table[,2]), sep=" "),
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
