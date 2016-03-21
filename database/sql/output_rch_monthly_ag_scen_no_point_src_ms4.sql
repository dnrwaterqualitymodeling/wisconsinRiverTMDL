CREATE VIEW output_rch_monthly_ag_scen_no_point_src_ms4 AS
	SELECT
		o.RCH AS RCH,
		o.MON AS MON,
		o.YR AS YR,
		o.FLOW_OUT AS FLOW_OUT,
		o.SED_OUT - IFNULL(p.SED_OUT, 0) AS SED_OUT,
		o.ORGP_OUT - IFNULL(p.ORGP_OUT, 0) AS ORGP_OUT,
		o.MINP_OUT - IFNULL(p.MINP_OUT, 0) AS MINP_OUT,
		o.TOT_P - IFNULL(p.TOT_P, 0) AS TOT_P
	FROM output_rch_monthly_ag_scen o LEFT JOIN point_source_ms4_monthly p
	ON o.RCH = p.RCH AND o.MON = p.MON AND o.YR = p.YR