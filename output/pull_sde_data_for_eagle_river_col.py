import os, arcpy, cx_Oracle, tempfile, csv
from arcpy import env

access_vars = "W:/WQWT_PROJECTS/WY_CQ_MS_AU_INTERSECTION/access.pickle"
with open('objs.pickle') as f:
    obj0, obj1, obj2 = pickle.load(f)
    
appDir = os.environ['APPDATA']
sdeCon = appDir + '/ESRI/Desktop10.1/ArcCatalog/' + schemaOut + '_' + pwOut + '_' + sde + '.sde'
catch_fc = sdeCon + '/' + schemaOut + '.WD_HYDRO_VA_CATCHMENT_AR_24K'
lulc_tbl_name = 'WD_HYDRO_VA_WATERSHED_TR_REF'
lulc_tbl = sdeCon + '/' + schemaOut + '.' + lulc_tbl_name
yield_tbl = r'T:\Projects\Wisconsin_River\Model_Outputs\eagle_river_swat\eagle_river_yields.txt'
out_tbl = r'T:\Projects\Wisconsin_River\Model_Outputs\eagle_river_swat\eagle_river_lulc_composition.txt'

lulc_tble = sdeCon + '/' + schemaOut + '.' + lulc_tbl_name
topology_tbl = sdeCon + '/' + schemaOut + '.WD_HYDRO_VA_UPSTR_TOPOLOGY_REF'
outfall = 200185815

nlcd_codes = {
	'11' : 'open_water',
	'21' : 'developed_open_space',
	'22' : 'developed_low_intensity',
	'23' : 'developed_medium_intensity',
	'24' : 'developed_high_intensity',
	'31' : 'barren_land',
	'41' : 'deciduous_forest',
	'42' : 'evergreen_forest',
	'43' : 'mixed_forest',
	'52' : 'shrub_scrub',
	'71' : 'grassland_herbaceous',
	'81' : 'pasture_hay',
	'82' : 'cultivated_crops',
	'90' : 'woody_wetlands',
	'95' : 'emergent_herbaceous_wetlands'
}

td = tempfile.mkdtemp()
env.scratchWorkspace = td

con = cx_Oracle.connect(schemaOut + '/' + pwOut + '@' + sde)
cur = con.cursor()
sql = "SELECT CATCHID from WD_HYDRO_VA_UPSTR_TOPOLOGY_REF where TOCATCHID = " + str(outfall)
ids = cur.execute(sql).fetchall()
ids = tuple([i[0] for i in ids])
cur.close()
con.close()

arcpy.MakeFeatureLayer_management(catch_fc, "catch_layer")
arcpy.SelectLayerByAttribute_management("catch_layer", "NEW_SELECTION", 'CATCHID IN ' + str(ids))
arcpy.AddJoin_management("catch_layer", "CATCHID", lulc_tbl, "REACHID")

fms = arcpy.FieldMappings()

field_list = ['TRW_LU06_' + c for c in nlcd_codes.keys()]
field_list = ['CATCHID'] + field_list
for f in field_list:
	fld_map = arcpy.FieldMap()
	if f[2:6] == 'LU06' or f[4:8] == 'LU06':
		fld_map.addInputField('catch_layer', schemaOut + "." + lulc_tbl_name + '.' + f)
		out_field = fld_map.outputField
		out_field.name = nlcd_codes[f[9:11]]
		out_field.aliasName = nlcd_codes[f[9:11]]
	else:
		fld_map.addInputField('catch_layer', 'W23324.WD_HYDRO_VA_CATCHMENT_AR_24K.' + f)
		out_field = fld_map.outputField
		out_field.name = f
		out_field.aliasName = f
	fld_map.outputField = out_field
	fms.addFieldMap(fld_map)
fld_map = arcpy.FieldMap()
fld_map.addInputField('catch_layer', 'W23324.WD_HYDRO_VA_WATERSHED_TR_REF.TRW_AREA')
out_field = fld_map.outputField
out_field.name = 'WATERSHED_AREA_SQ_KM'
out_field.aliasName = 'WATERSHED_AREA_SQ_KM'
fld_map.outputField = out_field
fms.addFieldMap(fld_map)
del fld_map

arcpy.TableToTable_conversion('catch_layer', env.scratchGDB, 'nlcd_comp', "", fms)

swat_fields = ['WATR', 'URML', 'PAST', 'FRSD', 'FRSE', 'FRST', 'HAY', 'AGRL', 'WETN']

for sf in swat_fields:
	arcpy.AddField_management(env.scratchGDB + '/nlcd_comp', sf, 'FLOAT')

fs = [v[1] for v in sorted(nlcd_codes.items())]
fs = fs + swat_fields

with arcpy.da.UpdateCursor(env.scratchGDB + '/nlcd_comp', fs) as cursor:
	for row in cursor:
		row[15] = row[0]
		row[16] = row[1] + row[2] + row[3] + row[4]
		row[17] = row[5] + row[9] + row[10]
		row[18] = row[6]
		row[19] = row[7]
		row[20] = row[8]
		row[21] = row[11]
		row[22] = row[12]
		row[23] = row[13] + row[14]
		cursor.updateRow(row)

for delete_field in nlcd_codes.values():
	arcpy.DeleteField_management(env.scratchGDB + '/nlcd_comp', delete_field)

# yield_tbl_d = []
# with open(yield_tbl,'r') as f:
	# next(f)
	# reader = csv.reader(f,delimiter='\t')
	# for lulc,wy,sedy,orgpy,sedpy,solpy in reader:
		# yield_tbl_d.append([lulc,wy,sedy,orgpy,sedpy,solpy])

f = open(out_tbl, 'w')
with arcpy.da.SearchCursor(env.scratchGDB + '/nlcd_comp', ['CATCHID', 'WATERSHED_AREA_SQ_KM'] + swat_fields) as cursor:
	f.write('\t'.join(['REACHID', 'WATERSHED_AREA_SQ_KM'] + swat_fields) + '\n')
	for row in cursor:
		f.write('\t'.join(map(str, row)) + '\n')
f.close()


