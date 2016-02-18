CREATE VIEW flow_monthly AS
SELECT station_id, mon, yr, AVG(flow) as flow
FROM
	(SELECT station_id, mon,day, yr, AVG(dflow) as flow
	FROM load_estimates
	GROUP BY station_id, mon, yr)
GROUP BY station_id, mon, yr