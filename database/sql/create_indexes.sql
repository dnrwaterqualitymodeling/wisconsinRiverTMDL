CREATE INDEX output_hru_sub on output_hru_monthly (sub);
CREATE INDEX output_hru_yr on output_hru_monthly (yr);
CREATE INDEX output_hru_mon on output_hru_monthly (mon);
CREATE INDEX output_hru_sub_yr_mon on output_hru_monthly (sub, yr, mon);

CREATE INDEX output_sub_sub on output_sub_monthly (sub);
CREATE INDEX output_sub_yr on output_sub_monthly (yr);
CREATE INDEX output_sub_mon on output_sub_monthly (mon);
CREATE INDEX output_sub_sub_yr_mon on output_sub_monthly (sub, yr, mon);

CREATE INDEX urban_daily_muni on urban_daily (muni);
CREATE INDEX urban_daily_subbasin on urban_daily (subbasin);
CREATE INDEX urban_daily_yr on urban_daily (yr);
CREATE INDEX urban_daily_mo on urban_daily (mo);
CREATE INDEX urban_daily_subbasin_yr_mo on urban_daily (subbasin, yr, mo);