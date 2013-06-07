How to Run the Watershed Aggregation Tool
======================================

1. [HUC16 watersheds can be downloaded here.](https://dl.dropboxusercontent.com/u/17521862/huc16s/huc16s.zip)

2. [Download the tools as a compressed package](https://github.com/asruesch/watershedAggregation/archive/master.zip), and unzip the contents to a location on your computer.

3. Open an instance of ArcMap. Right-click on **ArcToolbox** and **Add Toolbox**, and browse to the file **aggregateNetwork.tbx** within the directory you just unzipped. Within the toolbox in ArcGIS, click on the script called **Aggregate Network**.

4. Add the [HUC16 polygon shapefile](https://dl.dropboxusercontent.com/u/17521862/huc16s/huc16s.zip) to your ArcMap instance. Add a hydroline layer of your choice. Create an empty point shapefile. Populate the shapefile with points where you want to “break” your watershed network. Each point must fall within the sub-watershed polygon associated with the stream reach where you want the break to occur (green point in image below). The green point break will delineate the upstream watershed at the outfall of the sub-watershed (red point in image below).

![Positioning outfalls](https://dl.dropboxusercontent.com/u/17521862/etc/positioningOutfalls.png "Positioning outfalls")

4. Add a field to the point shapefile where a unique ID will be stored. The IDs of the aggregated watershed delineations will conform to this ID. Populate each field with a unique ID.

5. Run the Aggregate Network tool. The argument **Subsheds** must be the HUC16s. The **Hydro lines** argument should be the underlying hydrolines you will use to run SWAT. Define output shapefiles for watersheds and hydrolines and click OK.
