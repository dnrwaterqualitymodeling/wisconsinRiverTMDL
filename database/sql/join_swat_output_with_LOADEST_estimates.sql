CREATE VIEW observed_vs_simulated AS
	SELECT
		s.rch AS rch,
		station_name,
		s.mon AS mon,
		s.yr AS yr,
		flow_out AS flow_sim,
		fm.flow / 35.3147 AS flow_obs,
		(flow_out - (fm.flow / 35.3147)) / (fm.flow / 35.3147) AS flow_pbias,
		sed_out AS sed_sim,
		o.sed_obs / 1000 AS sed_obs,
		(sed_out - (o.sed_obs / 1000)) / (o.sed_obs / 1000) AS sed_pbias,
		tot_p AS tp_sim,
		o.tp_obs AS tp_obs,
		(tot_p - o.tp_obs) / o.tp_obs AS tp_pbias,
		(tot_p / flow_out) * (1/n_days) * 11.57407 AS tp_fwmc_sim,
		(o.tp_obs / fm.flow) * (1/n_days) * 408.7352 AS tp_fwmc_obs,
		(((tot_p / flow_out) * (1/n_days) * 11.57407) - ((o.tp_obs / fm.flow) * (1/n_days) * 408.7352)) / ((o.tp_obs / fm.flow) * (1/n_days) * 408.7352) AS tp_fwmc_pbias
	FROM
		output_rch_monthly s INNER JOIN station_id_rch_xref id_rch
		ON s.rch = id_rch.rch
		INNER JOIN usgs_station_xref nm
		ON id_rch.station_id = nm.station_id
		LEFT JOIN flow_monthly fm
		ON id_rch.station_id = fm.station_id AND s.mon = fm.mon AND s.yr = fm.yr
		LEFT JOIN days_in_mo dim
		ON s.mon = dim.mon AND s.yr = dim.yr
		LEFT JOIN 
			(SELECT
				station_id,
				mon,
				yr,
				MAX(CASE WHEN parameter = "SS_00530" THEN load_monthly ELSE NULL END) as sed_obs,
				MAX(CASE WHEN parameter = "TP_00665" THEN load_monthly ELSE NULL END) as tp_obs
			FROM load_estimates_monthly_sampling_window 
			GROUP BY
				station_id,
				mon,
				yr) o
		ON id_rch.station_id = o.station_id AND s.mon = o.mon AND s.yr = o.yr;