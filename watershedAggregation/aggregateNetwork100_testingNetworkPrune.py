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
		# val = int(row.GetValue(column))
		val = row.GetValue(column)
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

def findUpstream_hydro(hydroLayer, breakCatchids, breakIds, topologyTable, unsortedAggLine):
	gp.AddField_management(hydroLayer, "GRIDCODE", "LONG", "", "", 12, "", "", "")
	i = 0
	for breakCatchid in breakCatchids:
		breakId = breakIds[i]
		gp.AddMessage("     " + str(breakId))
		i += 1
		upstreamCatchids = array([], dtype='i4')
		# "tos" are to-features. Start at from-feature of initial reach
		tos = topologyTable['TRACEID'][in1d(topologyTable['REACHID'], array(breakCatchid))]
		end = False
		j = 0
		while not end:
			j += 1
			# Find all tocatchids of "tos"
			fromTable = topologyTable[in1d(topologyTable['TO_TRACEID'], tos)]
			# Select the TRACEID with the largest drainage area
			if len(fromTable) > 0:
				froms = fromTable['TRACEID'][fromTable['DA'] == max(fromTable['DA'])]
				fromReachid = fromTable['REACHID'][fromTable['DA'] == max(fromTable['DA'])]
			froms = delete(froms, where(in1d(fromReachid, breakCatchids)))
			froms = delete(froms, where(in1d(fromReachid, upstreamCatchids)))
			fromReachid = delete(fromReachid, where(in1d(fromReachid, breakCatchids)))
			fromReachid = delete(fromReachid, where(in1d(fromReachid, upstreamCatchids)))
			# Cut off catchids associated with breaks
			upstreamCatchids = append(upstreamCatchids, fromReachid)
			tos = array(froms)
			if len(fromReachid) == 0:
				end = True
				upstreamCatchids = append(upstreamCatchids, breakCatchid)
			if j > 10000:
				raise RuntimeError('Infinite while loop')
		strFields = gp.ListFields(hydroLayer, "REACHID", "String")
		if strFields.Next() <> None:
			upstreamCatchids = map(int, upstreamCatchids)
			if len(upstreamCatchids) == 1:
				expr = '"REACHID" = ' + str(upstreamCatchids[0])
			else:
				expr = '"REACHID" IN ' + str(tuple(upstreamCatchids))
		else:
			upstreamCatchids = map(int, upstreamCatchids)
			if len(upstreamCatchids) == 1:
				expr = '"REACHID" = ' + str(upstreamCatchids[0])
			else:
				expr = '"REACHID" IN ' + str(tuple(upstreamCatchids))
		rows = gp.UpdateCursor("hydroLine", expr)
		row = rows.Next()
		while row:
			row.GRIDCODE = int(breakId)
			rows.UpdateRow(row)
			row = rows.Next()
		del row, rows
		topologyTable['AGGID'][in1d(topologyTable['REACHID'], upstreamCatchids)] = breakId
	gp.SelectLayerByAttribute_management("hydroLine", "NEW_SELECTION", 'NOT "GRIDCODE" = 0')
	gp.Dissolve_management("hydroLine", unsortedAggLine, "GRIDCODE")
	gp.AddMessage("Upstream watersheds aggregated")

def buildAggTopology(outAggSubshed, unsortedAggLine, breakCatchids, topologyTable):
	gp.AddMessage("Intersecting Hydro Lines")
	gp.MakeFeatureLayer_management(unsortedAggLine, "hydroLine")
	gp.MakeFeatureLayer_management(outAggSubshed, "outAggSubshed")
	# hydroIntersect = os.path.dirname(unsortedAggLine) + "/hydroIntersect.shp"
	# gp.Intersect_analysis(["hydroLine", "outAggSubshed"], hydroIntersect, "NO_FID", "", "LINE")
	# gp.Dissolve_management(hydroIntersect, unsortedAggLine, "GRIDCODE")
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
	
	gp.AddMessage("Creating topology tables as numpy structured array: watersheds")
	gp.MakeFeatureLayer_management(subshedFile, "subsheds")
	catchid = getColVals("subsheds", "CATCHID", "")
	tocatchid = getColVals("subsheds", "TOCATCHID", "")
	topologyTable_sheds = zeros((len(catchid),),
		dtype=[('CATCHID', 'i4'),('TOCATCHID', 'i4'), ('AGGID', 'a20')])
	topologyTable_sheds['CATCHID'] = array(catchid)
	topologyTable_sheds['TOCATCHID'] = array(tocatchid)
	del catchid, tocatchid
	
	gp.AddMessage("Creating topology tables as numpy structured array: hydrolines")
	gp.MakeFeatureLayer_management(hydroLineFile, "hydroLine")
	reachid = getColVals("hydroLine", "REACHID", "")
	traceid = getColVals("hydroLine", "TRACEID", "")
	totraceid = getColVals("hydroLine", "TO_TRACEID", "")
	da = getColVals("hydroLine", "TRW_AREA", "")
	topologyTable_hydro = zeros((len(traceid),),
		dtype=[('REACHID','i4'),
			('TRACEID', 'i4'),
			('TO_TRACEID', 'i4'),
			('DA', 'f4'),
			('AGGID', 'a20')])
	topologyTable_hydro['REACHID'] = array(reachid)
	topologyTable_hydro['TRACEID'] = array(traceid)
	topologyTable_hydro['TO_TRACEID'] = array(totraceid)
	topologyTable_hydro['DA'] = array(da)
	del reachid, traceid, totraceid, da

	gp.AddMessage("Aggregating upstream watersheds for each point break")
	unsortedAggShed = tempDir + "/unsortedAggShed.shp"
	unsortedAggLine = tempDir + "/unsortedAggLine.shp"
	################################################################
	topologyTable = findUpstream("subsheds", breakCatchids, breakIds, topologyTable_sheds, unsortedAggShed)
	findUpstream_hydro("hydroLine", breakCatchids, breakIds, topologyTable_hydro, unsortedAggLine)
	################################################################
	gp.AddMessage("Building topology for aggregated watersheds")
	buildAggTopology(unsortedAggShed, unsortedAggLine, breakCatchids, topologyTable)
	################################################################
	gp.AddMessage("Writing output, sorting, and cleaning up")
	gp.DeleteField_management(unsortedAggShed, ["Shape_Leng", "Shape_Area"])
	gp.CreateFeatureclass_management(os.path.dirname(outAggSubshed), os.path.basename(outAggSubshed), "POLYGON", unsortedAggShed, "DISABLED", "DISABLED", unsortedAggShed)
	gp.CreateFeatureclass_management(os.path.dirname(outAggHydroline),\
		os.path.basename(outAggHydroline), "POLYLINE", unsortedAggLine, "DISABLED", "DISABLED",\
		unsortedAggLine)
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
	for dataset in ["subsheds", "hydroLine", breakIntersect, unsortedAggShed, unsortedAggLine]:
		gp.Delete_management(dataset)

if __name__ == '__main__':
	aggregateByBreaks(breakFile, breakField, subshedFile, outAggSubshed)
