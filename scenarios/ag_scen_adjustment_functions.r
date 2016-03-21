library(stringr)
library(dplyr)

read_output.rch = function(file_output.rch) {
	colnames=c("REACH","RCH","GIS","MON","AREA","FLOW_IN","FLOW_OUT","EVAP","TLOSS",
		"SED_IN","SED_OUT","SEDCONC","ORGN_IN","ORGN_OUT","ORGP_IN",
		"ORGP_OUT","NO3_IN","NO3_OUT","NH4_IN","NH4_OUT","NO2_IN","NO2_OUT",
		"MINP_IN","MINP_OUT","CHLA_IN","CHLA_OUT","CBOD_IN","CBOD_OUT",
		"DISOX_IN","DISOX_OUT","SOLPST_IN","SOLPST_OUT","SORPST_IN",
		"SORPST_OUT","REACTPST","VOLPST","SETTLPST","RESUSP_PST",
		"DIFFUSEPST","REACBEDPST","BURYPST","BED_PST","BACTP_OUT",
		"BACTLP_OUT","CMETAL_1","CMETAL_2","CMETAL_3","TOT_N","TOT_P",
		"NO3CONC","WTMP")
	colclasses=c("character","integer","character","character",rep("numeric", 47))
	d = read.table(
		file_output.rch,
		skip=9,
		colClasses=colclasses
	)
	names(d) = colnames
	d = d[,-1]
	d = d %>%
		filter(!grepl('(^20)|(^12\\.0)', MON)) %>%
		mutate(MON = as.integer(MON))
	yrs = NULL
	for (y in 2002:2013) {
		yrs = c(yrs, rep(y, dim(d)[1] / 12))
	}
	d = mutate(d, YR=yrs)
	d = d[
		c("RCH","GIS","MON","YR","AREA","FLOW_IN","FLOW_OUT","EVAP","TLOSS",
			"SED_IN","SED_OUT","SEDCONC","ORGN_IN","ORGN_OUT","ORGP_IN",
			"ORGP_OUT","NO3_IN","NO3_OUT","NH4_IN","NH4_OUT","NO2_IN","NO2_OUT",
			"MINP_IN","MINP_OUT","CHLA_IN","CHLA_OUT","CBOD_IN","CBOD_OUT",
			"DISOX_IN","DISOX_OUT","SOLPST_IN","SOLPST_OUT","SORPST_IN",
			"SORPST_OUT","REACTPST","VOLPST","SETTLPST","RESUSP_PST",
			"DIFFUSEPST","REACBEDPST","BURYPST","BED_PST","BACTP_OUT",
			"BACTLP_OUT","CMETAL_1","CMETAL_2","CMETAL_3","TOT_N","TOT_P",
			"NO3CONC","WTMP")
	]
	return(d)
}

adj_usle_p = function(mgt_list, adj_factor) {
	mgt_adj = lapply(
		mgt_list,
		function (mgt) {
			v = as.numeric(str_sub(mgt[12], 9, 16))
			v = v * (1 + adj_factor)
			v = sprintf("%8f", v)
			str_sub(mgt[12], 9, 16) = v
			return(mgt)
		}
	)
	return(mgt_adj)
}

adj_filterw = function(mgt_list, adj_factor) {
	mgt_adj = lapply(
		mgt_list,
		function (mgt) {
			v = as.numeric(str_sub(mgt[14], 7, 16))
			v = v * (1 + adj_factor)
			v = sprintf("%10.6f", v)
			str_sub(mgt[14], 7, 16) = v
			return(mgt)
		}
	)
	return(mgt_adj)
}	

adj_cnop = function(mgt_list, adj_factor, op_codes) {
	mgt_adj = lapply(
		mgt_list,
		function (mgt) {
			ops = mgt[-(1:30)]
			for (op in op_codes) {
				cn_lns = as.integer(str_sub(ops, 17, 18)) == op
				if (op == 1) {
					pos = c(76,80)
					fmt = "%4.2f"
				} else if (op == 6) {
					pos=c(35,43)
					fmt = "%9.6f"
				}
				cn = as.numeric(str_sub(ops[cn_lns], pos[1], pos[2]))
				cn_adj = cn * (1 + adj_factor)
				cn_adj = sprintf(fmt, cn_adj)
				str_sub(ops[cn_lns], pos[1], pos[2]) = cn_adj
				mgt = c(mgt[1:30], ops)
			}
			return(mgt)
		}
	)
	return(mgt_adj)
}	

adj_manure_amt = function(mgt_list, adj_factor) {
	mgt_adj = lapply(
		mgt_list,
		function (mgt) {
			ops = mgt[-(1:30)]
			m_lns = str_sub(ops, 18, 23) == "3   44"
			m_amt = as.numeric(str_sub(ops[m_lns], 33, 43))
			m_adj = m_amt * (1 + adj_factor)
			m_adj = sprintf("%11.5f", m_adj)
			str_sub(ops[m_lns], 33, 43) = m_adj
			mgt = c(mgt[1:30], ops)
			return(mgt)
		}
	)
	return(mgt_adj)
}	

