# Pre process dati ARPA: PM10 (SM2005), period 13/12/2021 -> 13/01/22

rm(list = ls())

stazioni_raw = read.csv(paste0(getwd(), "/dati_ARPA/raw/stazioni.csv"), header=TRUE)

# Check Lombardy provinces
province = c("BG", "BS", "CO", "CR", "LC", "LO", "MN", "MB", "MI", "PV", "SO", "VA")

id_rm_prov = c()
for(i in 1:nrow(stazioni_raw)){
  if(!stazioni_raw$Provincia[i] %in% province)
    id_rm_prov = c(id_rm_prov, i)
}

stazioni = stazioni_raw[-id_rm_prov, ]  # 92


# Filter for date: -> remove if there are no measurements in the whole period
data_stop_vec <- as.Date(stazioni$DataStop, format = "%d/%m/%Y")
data_start_vec <- as.Date(stazioni$DataStart , format = "%d/%m/%Y")
data_inf = as.Date("12/12/2021", format = "%d/%m/%Y")
data_sup = as.Date("13/01/2022", format = "%d/%m/%Y")
  
id_rm_data = c()
for(i in 1:length(data_stop_vec)){
  if(length(data_stop_vec[i]) > 1)
  if(data_stop_vec[i] < data_inf)
    id_rm_data = c(id_rm_data, i)
}
# -> none

id_rm_data = c()
for(i in 1:length(data_start_vec)){
  if(length(data_start_vec[i]) > 1)
    if(data_start_vec[i] > data_sup)
      id_rm_data = c(id_rm_data, i)
}
# -> none


# extract IdSensors
id_sensori = stazioni$IdSensore # unique
n_sensors = length(id_sensori)

# write.csv(id_sensori, file=paste0(getwd(), "/dati_ARPA/idsensori.csv"))


##---- Dati PM10--------
dati_raw = read.csv(paste0(getwd(), "/dati_ARPA/raw/inquinanti.csv"), header=TRUE)

# remove not valid data
id_nv = c()
for(i in 1:nrow(dati_raw)){
  if(dati_raw$Valore[i] == "-9,999"){
    id_nv = c(id_nv, i)
  }else{
    if(!(dati_raw$Stato[i] == "VA"))
      id_nv = c(id_nv, i)
  }
}

dati = dati_raw[-id_nv, ]


# Extract only PM10
rows_pm10 = c()
for(i in 1:nrow(dati)){
  if(dati$idSensore[i] %in% id_sensori)
    rows_pm10 = c(rows_pm10, i)
}

dati_pm10 = dati[rows_pm10, ]

# Check
id_na = c()
for(i in 1:nrow(dati_pm10)){
  if(dati_pm10$Valore[i] == "-9,999"){
    id_na = c(id_na, i)
  }else{
    if(!(dati_pm10$Stato[i] == "VA"))
      id_na = c(id_na, i)
  }
}
# none 

# Add "Day" column
dati_pm10$Day = substr(dati_pm10$Data, 1, 10)

# Extract data in the period 
n_days = length(unique(dati_pm10$Day))  # 32
observations = matrix(nrow = n_sensors, ncol = n_days)

for(i in 1:n_sensors){
  count_day = 1
  for(day in unique(dati_pm10$Day)){
    
    if(dim(dati_pm10[which((dati_pm10$idSensore == id_sensori[i]) & (dati_pm10$Day == day)), ])[1] > 0){
      observations[i, count_day] = dati_pm10[which((dati_pm10$idSensore == id_sensori[i]) & (dati_pm10$Day == day)), ]$Valore
    }else{
      observations[i, count_day] = NA
    }
    count_day = count_day + 1
  }
}


# Check percentage of NA by sensors 
perc_na = c()
for(i in 1:n_sensors){
  perc_na = c(perc_na, sum(is.na(observations[i, ]))/n_days*100)
}
perc_na 


id_rm = which(perc_na > 90 )

# Clean data 
id_sensori_pm10 = id_sensori[-id_rm]
n_sensors_pm10 = length(id_sensori_pm10)   # 66
observations_PM10 = matrix(as.numeric(observations[-id_rm, ]), 
                             nrow = nrow(observations[-id_rm, ]), ncol=ncol(observations[-id_rm, ]))


# Extract longitude and latitude  
lon_vec = c()
lat_vec = c()

for(id in id_sensori_pm10){
  lon_vec = c(lon_vec, stazioni[which(stazioni$IdSensore == id), ]$lng)
  lat_vec = c(lat_vec, stazioni[which(stazioni$IdSensore == id), ]$lat)
}

space_locs = cbind(lon_vec, lat_vec)

time_locs = seq(1, ncol(dati))

# Save data 
write.csv(format(space_locs, digits=16), file=paste0(getwd(), "/smoothing/space_locs.csv"))
write.csv(format(time_locs, digits=16), file=paste0(getwd(), "/smoothing/time_locs.csv"))
write.csv(format(observations_PM10, digits=16), file=paste0(getwd(), "/smoothing/y.csv"))





