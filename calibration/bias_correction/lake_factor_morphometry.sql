SELECT
	wbic,
	CASE WHEN official_name = 'Unnamed' THEN NULL ELSE official_name END AS official_name,
	CASE WHEN local_name = 'Unnamed' THEN NULL ELSE local_name END AS local_name,
	official_size,
	MAX(CASE
		WHEN waterbody_meas_unit_code = 'FT' AND waterbody_meas_type_code = 'ND' THEN waterbody_meas_amt
		ELSE NULL END)
		AS mean_depth_ft,
	CAST(MAX(CASE
		WHEN waterbody_meas_unit_code = 'AF' AND waterbody_meas_type_code = 'VO' THEN waterbody_meas_amt
		ELSE NULL END) AS INTEGER)
		AS volume_acre_ft
FROM
	w23323.wl_row_waterbody_v
	LEFT JOIN w23323.wl_row_waterbody_hydro
	ON w23323.wl_row_waterbody_v.waterbody_seq_no = w23323.wl_row_waterbody_hydro.waterbody_seq_no
	LEFT JOIN w23323.wl_row_waterbody_meas
	ON w23323.wl_row_waterbody_v.waterbody_seq_no = w23323.wl_row_waterbody_meas.waterbody_seq_no
WHERE
	((official_name IS NOT NULL AND official_name NOT LIKE '%Unnamed%' AND official_name NOT LIKE 'Un Spring%') OR
		(local_name IS NOT NULL AND local_name NOT LIKE '%Unnamed%' AND local_name NOT LIKE 'Un Spring%')) AND
		waterbody_type_code IN ('RF', 'CB', 'BK', 'DP', 'GP', 'IW', 'TP', 'LP') AND
		(waterbody_meas_type_code IS NULL OR waterbody_meas_type_code IN ('SZ', 'XD', 'ND', 'VO', 'RL', 'RM', 'RH'))
GROUP BY
	wbic,
	official_name,
	local_name,
	official_size;