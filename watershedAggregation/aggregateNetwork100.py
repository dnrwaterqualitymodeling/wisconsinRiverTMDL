import arcgisscripting, numpy, os, sys
from numpy import *

gp = arcgisscripting.create()

gp.OverWriteOutput = 1

gp.AddMessage("Reading Input Parameters")
breakFile = gp.GetParameterAsText(0)
breakField = gp.GetParameterAsText(1)
subshedFile = gp.GetParameterAsText(2)
hydroLineFile = gp.GetParameterAsText(3)
outAggSubshed = gp.GetParameterAsText(4)
outAggHydroline = gp.GetParameterAsText(5)
outLookupTable = gp.GetParameterAsText(6)

# breakFile = r"T:\Projects\Wisconsin_River\GIS_Datasets\Watersheds\firstDraftSubbasinDelineation\Subwatershed_breaks_v5.shp"
# breakField = 'OBJECTID'
# subshedFile = r'K:\temp\temp.gdb\WRB_HUC16_WTM'
# hydroLineFile = r"T:\Projects\Wisconsin_River\Model_Inputs\SWAT_Inputs\hydro\aggregate_hydro.shp"
# outAggSubshed = r'K:\temp\test_new_aggregate_network_subshed.shp'
# outAggHydroline = r'K:\temp\test_new_aggregate_network_hydro.shp'
# outLookupTable = r'K:\temp\test_new_aggregate_network_lookupTable.txt'

npVersion = float(numpy.version.version[0:3])
if npVersion < 1.7:
	arcpy.AddError("Numpy Version out of date. \
	Please install version >= 1.7")

tempDir = os.path.dirname(outAggSubshed)
if not os.path.exists(tempDir):
	os.path.makedirs(tempDir)

def getColVals(dataset, column, expr):
	rows = gp.searchcursor(dataset, expr,"",column,"")
	vals = []
	row = rows.next()
	while row:
		val = int(row.GetValue(column))
		vals.append(val)
		row = rows.next()
	del row, rows
	return array(vals)

def findUpstream(subshedLayer, breakCatchids, breakIds, topologyTable, unsortedAggShed):
	gp.AddField_management(subshedLayer, "GRIDCODE", "LONG", "", "", 12, "", "", "")
	gp.AddField_management(subshedLayer, "Subbasin", "LONG", "", "", 12, "", "", "")
	i = 0
	for breakCatchid in breakCatchids:
		breakId = breakIds[i]
		gp.AddMessage("     " + str(breakId))
		i += 1
		upstreamCatchids = array([], dtype='i4')
		# "tos" are to-features. Start at from-feature of initial reach
		tos = array(breakCatchid)
		end = False
		while not end:
			# Find all tocatchids of "tos"
			froms = topologyTable['CATCHID'][in1d(topologyTable['TOCATCHID'], tos)]
			froms = delete(froms, where(in1d(froms, breakCatchids)))
			# Cut off catchids associated with breaks
			upstreamCatchids = append(upstreamCatchids, froms)
			tos = froms
			if len(froms) == 0:
				end = True
				upstreamCatchids = append(upstreamCatchids, breakCatchid)
		strFields = gp.ListFields(subshedLayer, "CATCHID", "String")
		if strFields.Next() <> None:
			upstreamCatchids = map(int, upstreamCatchids)
			if len(upstreamCatchids) == 1:
				expr = '"CATCHID" = ' + str(upstreamCatchids[0])
			else:
				expr = '"CATCHID" IN ' + str(tuple(upstreamCatchids))
		else:
			upstreamCatchids = map(int, upstreamCatchids)
			if len(upstreamCatchids) == 1:
				expr = '"CATCHID" = ' + str(upstreamCatchids[0])
			else:
				expr = '"CATCHID" IN ' + str(tuple(upstreamCatchids))
		rows = gp.UpdateCursor("subsheds", expr)
		row = rows.Next()
		while row:
			row.GRIDCODE = int(breakId)
			row.Subbasin = int(breakId)
			rows.UpdateRow(row)
			row = rows.Next()
		del row, rows
		topologyTable['AGGID'][in1d(topologyTable['CATCHID'], upstreamCatchids)] = breakId
	gp.SelectLayerByAttribute_management("subsheds", "NEW_SELECTION", 'NOT "GRIDCODE" = 0')
	gp.Dissolve_management("subsheds", unsortedAggShed, ["GRIDCODE", "Subbasin"])
	gp.AddMessage("Upstream watersheds aggregated")
	return topologyTable


def buildAggTopology(outAggSubshed, hydroLineFile, unsortedAggLine, breakCatchids, topologyTable):
	gp.AddMessage("Intersecting Hydro Lines")
	gp.MakeFeatureLayer_management(hydroLineFile, "hydroLine")
	gp.MakeFeatureLayer_management(outAggSubshed, "outAggSubshed")
	hydroIntersect = os.path.dirname(unsortedAggLine) + "/hydroIntersect.shp"
	gp.Intersect_analysis(["hydroLine", "outAggSubshed"], hydroIntersect, "NO_FID", "", "LINE")
	gp.Dissolve_management(hydroIntersect, unsortedAggLine, "GRIDCODE")
	for field in ["ARCID", "GRID_CODE", "FROM_NODE", "TO_NODE", "Subbasin", "SubbasinR"]:
		gp.AddField_management(unsortedAggLine, field, "LONG", "", "", 12, "", "", "")
	for breakCatchid in breakCatchids:
		from_ = topologyTable['AGGID'][topologyTable['CATCHID'] == breakCatchid]
		tocatchid = topologyTable['TOCATCHID'][topologyTable['CATCHID'] == breakCatchid]
		to_ = topologyTable['AGGID'][topologyTable['CATCHID'] == tocatchid]
		rows = gp.UpdateCursor(unsortedAggLine, '"GRIDCODE" = ' + str(from_[0]))
		row = rows.next()
		while row:
			row.ARCID = int(from_[0])
			row.GRID_CODE = int(from_[0])
			row.FROM_NODE = int(from_[0])
			row.Subbasin = int(from_[0])
			if len(to_) == 0 or to_[0] == '':
				row.TO_NODE = 0
				row.SubbasinR = 0
			else:
				row.TO_NODE = int(to_[0])
				row.SubbasinR = int(to_[0])
			rows.updateRow(row)
			row = rows.next()
		del row, rows
	gp.DeleteField_management(unsortedAggLine, ["GRIDCODE"])
	gp.AddMessage("Topology built")

def aggregateByBreaks(breakFile, breakField, subshedFile, outAggSubshed):
	gp.AddMessage("Intersecting point breaks with subsheds")
	breakIntersect = tempDir + "/breakIntersect.shp"
	gp.Intersect_analysis([breakFile, subshedFile], breakIntersect, "ALL", "#", "POINT")
	breakIds = getColVals(breakIntersect, breakField, "")
	breakCatchids = getColVals(breakIntersect, "CATCHID", "")
	if len(breakIds) <> len(breakCatchids):
		gp.AddWarning("Multiple break points in single subshed")
	# create topology table
	gp.AddMessage("Creating topology table as numpy structured array")
	gp.MakeFeatureLayer_management(subshedFile, "subsheds")
	catchid = getColVals("subsheds", "CATCHID", "")
	tocatchid = getColVals("subsheds", "TOCATCHID", "")
	topologyTable = zeros((len(catchid),), dtype=[('CATCHID', 'i4'),('TOCATCHID', 'i4'), ('AGGID', 'a20')])
	topologyTable['CATCHID'] = array(catchid)
	topologyTable['TOCATCHID'] = array(tocatchid)
	del catchid, tocatchid
	gp.AddMessage("Aggregating upstream watersheds for each point break")
	unsortedAggShed = tempDir + "/unsortedAggShed.shp"
	unsortedAggLine = tempDir + "/unsortedAggLine.shp"
	################################################################
	topologyTable = findUpstream("subsheds", breakCatchids, breakIds, topologyTable, unsortedAggShed)
	nonNas = where(topologyTable['AGGID'] != '')
	outTopologyTable = topologyTable[nonNas]
	outTopologyTable = outTopologyTable[['CATCHID','AGGID']]
	outTopologyTable = outTopologyTable.astype([('CATCHID', 'S20'), ('AGGID', 'S20')])
	outTopologyTable = asarray([
		outTopologyTable['CATCHID'],
		outTopologyTable['AGGID']
	
	])
	outTopologyTable = transpose(outTopologyTable)
	# savetxt(outLookupTable, asarray([[1,2],[3,4]]), delimiter=',')
	savetxt(outLookupTable, outTopologyTable, fmt='%-9s,%-4s', header='CATCHID,SUBBASIN', comments='')
	################################################################
	gp.AddMessage("Building topology for aggregated watersheds")
	buildAggTopology(unsortedAggShed, hydroLineFile, unsortedAggLine, breakCatchids, topologyTable)
	################################################################
	gp.AddMessage("Writing output, sorting, and cleaning up")
	gp.DeleteField_management(unsortedAggShed, ["Shape_Leng", "Shape_Area"])
	gp.CreateFeatureclass_management(os.path.dirname(outAggSubshed), os.path.basename(outAggSubshed), "POLYGON", unsortedAggShed, "DISABLED", "DISABLED", unsortedAggShed)
	gp.CreateFeatureclass_management(os.path.dirname(outAggHydroline), os.path.basename(outAggHydroline), "POLYLINE", unsortedAggLine, "DISABLED", "DISABLED", unsortedAggLine)
	rows = gp.searchcursor(unsortedAggShed, "", "", "", "GRIDCODE A")
	row = rows.next()
	insertRows = gp.insertcursor(outAggSubshed)
	while row:
		insertRow = insertRows.newrow()
		insertRow.shape = row.shape
		for field in ["GRIDCODE", "Subbasin"]:
			insertRow.setvalue(field, row.getValue(field))
		insertRows.insertrow(insertRow)
		row = rows.next()
	del row, rows, insertRow, insertRows
	rows = gp.searchcursor(unsortedAggLine, "", "", "", "ARCID A")
	row = rows.next()
	insertRows = gp.insertcursor(outAggHydroline)
	while row:
		insertRow = insertRows.newrow()
		insertRow.shape = row.shape
		for field in ["ARCID", "GRID_CODE", "FROM_NODE", "TO_NODE", "Subbasin", "SubbasinR"]:
			insertRow.setvalue(field, row.getValue(field))
		insertRows.insertrow(insertRow)
		row = rows.next()
	del row, rows, insertRow, insertRows
	for dataset in ["subsheds", "hydroLine", os.path.dirname(unsortedAggLine) + "/hydroIntersect.shp", breakIntersect, unsortedAggShed, unsortedAggLine]:
		gp.Delete_management(dataset)

if __name__ == '__main__':
	aggregateByBreaks(breakFile, breakField, subshedFile, outAggSubshed)
