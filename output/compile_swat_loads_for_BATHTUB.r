library(dplyr)

file_out_bep = "T:/Projects/Wisconsin_River/Model_Outputs/bathtub_swat/bep.txt"
file_out_dubay = "T:/Projects/Wisconsin_River/Model_Outputs/bathtub_swat/dubay.txt"
file_out_lw = "T:/Projects/Wisconsin_River/Model_Outputs/bathtub_swat/lw.txt"

########################
## Read in monthly output.rch data
file_output.rch = "C:/TEMP/swat_output_for_bathtub_models/WRB.Sufi2.SwatCup/output.rch"
file_output.sub = "C:/TEMP/swat_output_for_bathtub_models/WRB.Sufi2.SwatCup/output.sub"
output.rch = read.table(
	file_output.rch,
	skip=9,
	colClasses = c(
		"NULL",
		"integer",
		"NULL",
		"character",
		"NULL",
		"NULL",
		"numeric",
		"numeric",
		"numeric"
	)
)
mons = seq(as.Date("2002-01-01"), as.Date("2013-12-01"), "1 month")
mons = data.frame(
	yr=as.integer(format(mons, "%Y")),
	mo=as.integer(format(mons, "%m"))
)
sec_in_mo = aggregate(
	dates ~ format(dates, "%Y") + format(dates, "%m"),
	data=data.frame(
		dates = seq(as.Date("2002-01-01"), as.Date("2013-12-31"), "1 day")),
	function(x) { length(x) * 86400 }
)$dates
output.rch = output.rch %>%
	rename(reach=V2, mo=V4, flow=V7, sed=V8, tp=V9) %>%
	filter(mo != "12.0") %>%
	mutate(mo=as.integer(mo)) %>%
	filter(mo <= 12) %>%
	arrange(reach) %>%
	mutate(yr=rep(mons$yr, 337), mo=rep(mons$mo, 337)) %>%
	filter(yr >= 2009)
output.sub = read.fwf(
	file_output.sub,
	widths=c(-6,4,-9,5,rep(10,6)),
	skip=9,
	colClasses = c(
		"integer",
		"character",
		rep("numeric", 6)
	),
	strip.white=T
)
output.sub = output.sub %>%
	rename(reach=V1, mo=V2, area=V3, flow=V4, sed=V5, orgp=V6, solp=V7, sedp=V8) %>%
	filter(mo != "12.0") %>%
	mutate(mo=as.integer(mo)) %>%
	filter(mo <= 12) %>%
	mutate(
		flow=flow*area*1000,
		sed=sed*area*100,
		tp=(orgp + solp + sedp)*area*100) %>%
	arrange(reach) %>%
	mutate(
		yr=rep(mons$yr, 337),
		mo=rep(mons$mo, 337),
		sec=rep(sec_in_mo, 337)) %>%
	mutate(flow=flow/sec) %>%
	filter(yr >= 2009) %>%
	select(reach,yr,mo,flow,sed,tp)

mons = filter(mons, yr >= 2009)

#########################
## ungaged to BEP
up = 152
down = 327

up.df = filter(output.rch, reach == up)
down.df = filter(output.rch, reach == down)

between = down.df[c("flow","sed","tp")] - up.df[c("flow","sed","tp")]
between = cbind(mons, between)
## Direct drainages
subs = c(87,88,89)
subs.df = output.sub %>%
	filter(reach %in% subs) %>%
	group_by(yr, mo) %>%
	summarise(flow=sum(flow), sed=sum(sed), tp=sum(tp))

between[c("flow","sed","tp")] = between[c("flow","sed","tp")] + subs.df[c("flow","sed","tp")]
write.table(between, file_out_bep, sep="\t", row.names=F)

#########################
## ungaged to Dubay
up = 154
down = 262

up.df = filter(output.rch, reach == up)
down.df = filter(output.rch, reach == down)

between.1 = down.df[c("flow","sed","tp")] - up.df[c("flow","sed","tp")]
between.1 = cbind(mons, between.1)

up = 150
down = 80

up.df = filter(output.rch, reach == up)
down.df = filter(output.rch, reach == down)

between.2 = down.df[c("flow","sed","tp")] - up.df[c("flow","sed","tp")]
between.2 = cbind(mons, between.2)

direct = output.rch %>%
	filter(reach == 329) %>%
	select(yr,mo,flow,sed,tp)

## Direct drainages
subs = 81
subs.df = output.sub %>%
	filter(reach %in% subs) %>%
	group_by(yr, mo) %>%
	summarise(flow=sum(flow), sed=sum(sed), tp=sum(tp))

between = cbind(mons,
	between.1[c("flow","sed","tp")] + 
	between.2[c("flow","sed","tp")] +
	direct[c("flow","sed","tp")] +
	subs.df[c("flow","sed","tp")]
)
write.table(between, file_out_dubay, sep="\t", row.names=F)

#########################
## ungaged to Lake Wisconsin
ups = c(191, 137)
down = 235

ups.df = output.rch %>%
	filter(reach %in% ups) %>%
	group_by(yr, mo) %>%
	summarise(
		flow=sum(flow),
		sed=sum(sed),
		tp=sum(tp))
down.df = filter(output.rch, reach == down)

between.1 = down.df[c("flow","sed","tp")] - ups.df[c("flow","sed","tp")]
between.1 = cbind(mons, between.1)

## Direct reaches
rchs = 236:240
rchs.df = output.rch %>%
	filter(reach %in% rchs) %>%
	group_by(yr, mo) %>%
	summarise(flow=sum(flow), sed=sum(sed), tp=sum(tp))

## Direct drainages
subs = 1
subs.df = output.sub %>%
	filter(reach %in% subs) %>%
	group_by(yr, mo) %>%
	summarise(flow=sum(flow), sed=sum(sed), tp=sum(tp))

between = cbind(mons,
	between.1[c("flow","sed","tp")] + 
	rchs.df[c("flow","sed","tp")] +
	subs.df[c("flow","sed","tp")]
)
write.table(between, file_out_lw, sep="\t", row.names=F)
	