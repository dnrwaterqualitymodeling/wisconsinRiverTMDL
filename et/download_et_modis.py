import urllib
import os
import time

dst_dir = "H:/et_data/"
tmp_dir = "H:/temp_directory"

if not os.path.exists(tmp_dir):
	os.mkdir(tmp_dir)

base_ftp = "ftp://ftp.ntsg.umt.edu/pub/MODIS/NTSG_Products/MOD16/MOD16A2_MONTHLY.MERRA_GMAO_1kmALB/GEOTIFF_0.05degree/"
base_file_name = "MOD16A2_ET_0.05deg_GEO_"
base_out_file = "MODIS_ET_"
yrs = [2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014]
mos = ["01","02","03","04","05","06","07","08","09","10","11","12"]

for yr in yrs:
	print("Beginning year", str(yr))
	for mo in mos:
		print("starting month", mo)
		yr_mo = str(yr) + "M" + mo + ".tif"
		url = base_ftp + base_file_name + yr_mo
		out_file = base_out_file + yr_mo
		if os.path.exists(dst_dir + out_file):
			print("Already got it")
			continue
		try:
			urllib.urlretrieve(url, dst_dir + out_file)
			print("Download seemingly successful, waiting for a minute")
			time.sleep(60)
		except:
			print "Downloand for", yr_mo, "failed."
		