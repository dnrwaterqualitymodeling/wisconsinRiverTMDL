wd = "H:/WRB/Scenarios/Default/TxtInOut"

files_pnds = list.files(wd, "*.pnd")

dat = data.frame()

for (fl in files_pnds){
	lnes = readLines(paste(wd, fl, sep="/"))
	sb = gsub("0", "", substr(fl, 1, 5))
	print(paste("working on Subabsin", sb))
	PND_FR = substr(lnes[3], 9, 16)
	PND_PSA = substr(lnes[4], 9, 16)
	PND_PVOL = substr(lnes[5], 9, 16)
	PND_ESA = substr(lnes[6], 9, 16)
	PND_EVOL = substr(lnes[7], 9, 16)

	WET_FR = substr(lnes[29], 9, 16)
	WET_NSA = substr(lnes[30], 9, 16)
	WET_NVOL = substr(lnes[31], 9, 16)
	WET_MXSA = substr(lnes[32], 9, 16)
	WET_MXVOL = substr(lnes[33], 9, 16)
	
	rw = c(sb,
		PND_FR,
		PND_PSA,
		PND_PVOL,
		PND_ESA,
		PND_EVOL,
		WET_FR,
		WET_NSA,
		WET_NVOL,
		WET_MXSA,
		WET_MXVOL)
	dat = rbind(dat, as.numeric(rw))
}
names(dat) = c("Subbasin",
	"PND_FR",
	"PND_PSA",
	"PND_PVOL",
	"PND_ESA",
	"PND_EVOL",
	"WET_FR",
	"WET_NSA",
	"WET_NVOL",
	"WET_MXSA",
	"WET_MXVOL")

# looks good...
plot(PND_FR ~ WET_FR, data=dat)

#####
hist(dat$PND_PVOL/dat$PND_PSA)
dat[which((dat$PND_PVOL/dat$PND_PSA)>100),]

hist(dat$PND_EVOL/dat$PND_ESA)
dat[which((dat$PND_EVOL/dat$PND_ESA)>100),]

hist(dat$WET_NVOL/dat$WET_NSA)
dat[which((dat$WET_NVOL/dat$WET_NSA)>10),]

### There are some anomalus, very high values of max volume.
### 	Got it: ponds were not masking sinks in wet script.
hist(dat$WET_MXVOL/dat$WET_MXSA)
dat[which((dat$WET_MXVOL/dat$WET_MXSA)>10),]

### how many have a larger normal than max? a few
dat[which(dat$PND_PVOL > dat$PND_EVOL),c('Subbasin', 'PND_PVOL', 'PND_EVOL')]

#### Issue here: why do a couple of these subs have greater emergency than principle surface areas?
dat[which(dat$PND_PSA > dat$PND_ESA),c('Subbasin', 'PND_PSA', 'PND_ESA')]
dat[which(dat$WET_NVOL > dat$WET_MXVOL),c('Subbasin', 'WET_NSA', 'WET_MXSA')]
dat[which(dat$WET_NSA > dat$WET_MXSA),c('Subbasin', 'WET_NVOL', 'WET_MXVOL')]

### How many have a calc'd volume even though fraction = 0
dat[which(dat$PND_PVOL > 0 & dat$PND_FR == 0),]
dat[which(dat$WET_NVOL > 0 & dat$WET_FR == 0),]



