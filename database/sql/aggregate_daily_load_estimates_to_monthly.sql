CREATE VIEW load_estimates_monthly AS
	SELECT
	station_id,
	parameter,
	mon,
	yr,
	CAST(SUM(dload) AS REAL) AS load_monthly
	FROM load_estimates
	GROUP BY
		station_id,
		parameter,
		mon,
		yr
