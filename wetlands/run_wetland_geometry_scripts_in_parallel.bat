set r_exe="C:\Program Files\R\R-3.0.1\bin\x64\Rscript.exe"
set r_script=T:\Projects\Wisconsin_River\Code\wetlands\calculate_wetland_parameters.r
set out_raster_dir=K:/temp/wetlands_rasters
set out_geometry_file=T:/Projects/Wisconsin_River/GIS_datasets/wetlands/wetland_geometry_

start "wetland_1" %r_exe% %r_script% 1 85 %out_raster_dir% %out_geometry_file%1.csv
REM start "wetland_1" %r_exe% %r_script% 1 2 %out_raster_dir% %out_geometry_file%1.csv
start "wetland_2" %r_exe% %r_script% 86 170 %out_raster_dir% %out_geometry_file%2.csv
start "wetland_3" %r_exe% %r_script% 171 255 %out_raster_dir% %out_geometry_file%3.csv
start "wetland_4" %r_exe% %r_script% 256 338 %out_raster_dir% %out_geometry_file%4.csv

start "wetland_3" %r_exe% %r_script% 251 251 %out_raster_dir% %out_geometry_file%3.csv
start "wetland_4" %r_exe% %r_script% 284 284 %out_raster_dir% %out_geometry_file%4.csv
