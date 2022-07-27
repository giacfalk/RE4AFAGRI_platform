# 1) irrigation needs (total and crop specific)

crops_list <- read.csv("crops_list.csv")

months <- 1:12
rcps <- c("rcp45", "rcp60")
ssps <- c("ssp2", "ssp5")
year <- seq(2020, 2060, 10)
crop <- c(crops_list)

l <- expand.grid(months, rcps, ssps, year, crop, stringsAsFactors = F)

out <- paste0('ELSEIF
[month]=', l$Var1,
'\nAND
[RCP]=="', l$Var2,'"
AND
[SSP]=="', l$Var3,'"
THEN',
'\n[m_IRREQ_', l$Var1, '_', l$Var3, '_', l$Var2, ']\n')

out[1] <- gsub("ELSE", "", out[1])
out[length(out)+1] <- "END"

write(out, "D:/OneDrive - IIASA/RE4AFAGRI_platform/online_platform/field_calculators/irrigation_M.txt")

####

# 2) yield growth potential (total and crop specific)

rcps <- c("rcp45", "rcp60")
ssps <- c("ssp2", "ssp5")
year <- seq(2020, 2060, 10)
crop <- c(crops_list)

l <- expand.grid(rcps, ssps, year, crop, stringsAsFactors = F)


# 3) ely demand (sector, month, year, ssp, rcp)

months <- 1:12
rcps <- c("rcp45", "rcp60")
ssps <- c("ssp2", "ssp5")
year <- seq(2020, 2060, 10)
sector <- c(sectors_list)

l <- expand.grid(months, rcps, ssps, year, sectors_list, stringsAsFactors = F)


# 4) crop processing - machineries requirements and power needs (crop, rcp)

rcps <- c("rcp45", "rcp60")
ssps <- c("ssp2", "ssp5")
year <- seq(2020, 2060, 10)
crop <- c(crops_list)

l <- expand.grid(rcps, ssps, year, crop, stringsAsFactors = F)



# 5) solar pumps -> 

rcps <- c("rcp45", "rcp60")
ssps <- c("ssp2", "ssp5")
year <- seq(2020, 2060, 10)

l <- expand.grid(rcps, ssps, year, crop, stringsAsFactors = F)


# 6) onsset -> technology; investment requirement; capacity; LCOE (year, ssp, rcp, other parameter)

rcps <- c("rcp45", "rcp60")
ssps <- c("ssp2", "ssp5")
year <- seq(2020, 2060, 10)
other_par <- c(other_par_list)

l <- expand.grid(rcps, ssps, year, other_par, stringsAsFactors = F)

# 7) nest -> (year, ssp, rcp, other parameter)

rcps <- c("rcp45", "rcp60")
ssps <- c("ssp2", "ssp5")
year <- seq(2020, 2060, 10)
other_par <- c(other_par_list)

l <- expand.grid(rcps, ssps, year, other_par, stringsAsFactors = F)
