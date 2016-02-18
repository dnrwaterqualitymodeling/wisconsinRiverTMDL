CREATE VIEW load_estimates_monthly_sampling_window AS
	SELECT
		lem.station_id,
		lem.parameter,
		lem.mon,
		lem.yr,
		CAST(load_monthly AS REAL) AS load_monthly
	FROM load_estimates_monthly lem LEFT JOIN load_estimate_summary les
	ON lem.station_id = les.station_id AND lem.parameter = les.parameter
	WHERE
		(lem.yr > les.wq_start_yr AND lem.yr < les.wq_end_yr) OR
		(lem.yr = les.wq_start_yr AND lem.mon > les.wq_start_yr) OR
		(lem.yr = les.wq_end_yr AND lem.mon < les.wq_end_yr)