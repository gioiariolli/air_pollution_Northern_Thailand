install.packages("ggpubr")
library(ggpubr)
library(dplyr)
library(ggplot2)

# To make scatterplot and look at the correlation between variables and OP
PMF_OP_2020 <- read.csv2("~/Desktop/Progetto Thailandia/PMF_OP_2020.csv")
PMF_OP_2021 <- read.csv2("~/Desktop/Progetto Thailandia/PMF_OP_2021.csv")
PMF_OP <- read.csv2("~/Desktop/Progetto Thailandia/PMF_OP.csv")
head(PMF_OP_2020)
head(PMF_OP_2021)

# Defining two seasons in the two years: Smoke-haze and Non-smoke haze (by looking at PM2.5 conc. levels)
# Non-smoke haze season
PMF_OP_nsm <- rbind(PMF_OP[27:77,], PMF_OP [130:140,])
# Smoke-haze season
PMF_OP_sm <- rbind(PMF_OP[1:26,], PMF_OP [78:130,])

# GENERAL CORRELATION OP and PM2.5
# OPv: Full year, R2= 0.732
ggplot(data = PMF_OP, aes(x = PM2.5, y = Opv)) +
  geom_smooth(method = "lm") +
  geom_point() +
  stat_regline_equation(label.y = 1.2) +  
  stat_cor(aes(label = ..rr.label..), label.y = 1, digits = 3)

# OPv: smoke-haze, R2= 0.564
ggplot(data = PMF_OP_sm, aes(x = PM2.5, y = Opv)) +
  geom_smooth(method = "lm") +
  geom_point() +
  stat_regline_equation(label.y = 1.2) +  
  stat_cor(aes(label = ..rr.label..), label.y = 1, digits = 3)

#OPv: non-smoke haze, R2= 0.258
ggplot(data = PMF_OP_nsm, aes(x = PM2.5, y = Opv)) +
  geom_smooth(method = "lm") +
  geom_point() +
  stat_regline_equation(label.y = 1.2) +  
  stat_cor(aes(label = ..rr.label..), label.y = 1, digits = 3)

#OPm: full year, R2= 0.0444
ggplot(data = PMF_OP, aes(x = PM2.5, y = Opm)) +
  geom_smooth(method = "lm") +
  geom_point() +
  stat_regline_equation(label.y = 1.4) +  
  stat_cor(aes(label = ..rr.label..), label.y = 1, digits = 3)

#OPm: smoke-haze, R2= 0.0002
ggplot(data = PMF_OP_sm, aes(x = PM2.5, y = Opm)) +
  geom_smooth(method = "lm") +
  geom_point() +
  stat_regline_equation(label.y = 1.4) +  
  stat_cor(aes(label = ..rr.label..), label.y = 1, digits = 3)

#OPm: non-smoke haze, R2=0.0121
ggplot(data = PMF_OP_nsm, aes(x = PM2.5, y = Opm)) +
  geom_smooth(method = "lm") +
  geom_point() +
  stat_regline_equation(label.y = 1.4) +  
  stat_cor(aes(label = ..rr.label..), label.y = 1, digits = 3)

# STARTING CORRELATION BETWEEN OPv 2020 AND EACH PMF FACTOR
# Burning Biomass R2= 0.6, good correlation
ggplot(data = PMF_OP_2020, aes(x = BB, y = Opv)) +
  geom_smooth(method = "lm") +
  geom_point() +
  stat_regline_equation(label.y = 1.2) +  
  stat_cor(aes(label = ..rr.label..), label.y = 1, digits = 3)

# Soil Dust, R2= 0.304, not correlated
ggplot(data = PMF_OP_2020, aes(x = Soil.dust, y = Opv)) +
  geom_smooth(method = "lm") +
  geom_point() +
  stat_regline_equation(label.y = 1.2) +  
  stat_cor(aes(label = ..rr.label..), label.y = 1, digits = 3)

# Secondary Aerosol, R2= 0.239, not correlated
ggplot(data = PMF_OP_2020, aes(x = Sec.aero, y = Opv)) +
  geom_smooth(method = "lm") +
  geom_point() +
  stat_regline_equation(label.y = 1.2) +  
  stat_cor(aes(label = ..rr.label..), label.y = 1, digits = 3)

# Cu-Mg, R2= 0.722, very good correlation
ggplot(data = PMF_OP_2020, aes(x = Cu.Mg, y = Opv)) +
  geom_smooth(method = "lm") +
  geom_point() +
  stat_regline_equation(label.y = 1.2) +  
  stat_cor(aes(label = ..rr.label..), label.y = 1, digits = 3)

# Traffic, R2= 0.388, not correlated
ggplot(data = PMF_OP_2020, aes(x = Traffic, y = Opv)) +
  geom_smooth(method = "lm") +
  geom_point() +
  stat_regline_equation(label.y = 1.2) +  
  stat_cor(aes(label = ..rr.label..), label.y = 1, digits = 3)

# CORRELATION TEST OPv 2021 AND PMF FACTORS
# Biomass Burning,R2= 0.347
ggplot(data = PMF_OP_2021, aes(x = BB, y = Opv)) +
  geom_smooth(method = "lm") +
  geom_point() +
  stat_regline_equation(label.y = 1.2) +  
  stat_cor(aes(label = ..rr.label..), label.y = 1, digits = 3)

# Soil Dust,R2= 0.124
ggplot(data = PMF_OP_2021, aes(x = Soil.dust, y = Opv)) +
  geom_smooth(method = "lm") +
  geom_point() +
  stat_regline_equation(label.y = 1.2) + 
  stat_cor(aes(label = ..rr.label..), label.y = 1, digits = 3)

# Secondary aerosol, R2= 0.432
ggplot(data = PMF_OP_2021, aes(x = Sec.aero, y = Opv)) +
  geom_smooth(method = "lm") +
  geom_point() +
  stat_regline_equation(label.y = 1.2) + 
  stat_cor(aes(label = ..rr.label..), label.y = 1, digits = 3)

# Cu-Mg, R2= 0.345
ggplot(data = PMF_OP_2021, aes(x = Cu.Mg, y = Opv)) +
  geom_smooth(method = "lm") +
  geom_point() +
  stat_regline_equation(label.y = 1.2) + 
  stat_cor(aes(label = ..rr.label..), label.y = 1, digits = 3)

# Traffic, R2= 0.463
ggplot(data = PMF_OP_2021, aes(x = Traffic, y = Opv)) +
  geom_smooth(method = "lm") +
  geom_point() +
  stat_regline_equation(label.y = 1.2) + 
  stat_cor(aes(label = ..rr.label..), label.y = 1, digits = 3)

# WE FILTER THE 2020 DATASET FOR SEASONS, using PM 2.5 values as our main criteria:
# Non-smoke haze season
PMF_OP_2020_nsm <- PMF_OP_2020[27:77,]
# Smoke-haze season
PMF_OP_2020_sm <- PMF_OP_2020[1:26,] 

# RUNNING CORRELATION TEST FULL YEARS
# Traffic
ggplot(data = PMF_OP_sm, aes(x = Traffic, y = Opv)) +
  geom_smooth(method = "lm") +
  geom_point() +
  stat_regline_equation(label.y = 1.2) + 
  stat_cor(aes(label = ..rr.label..), label.y = 1, digits = 3) # 0.187

ggplot(data = PMF_OP_nsm, aes(x = Traffic, y = Opv)) +
  geom_smooth(method = "lm") +
  geom_point() +
  stat_regline_equation(label.y = 1.2) + 
  stat_cor(aes(label = ..rr.label..), label.y = 1, digits = 3) # 0.117

# Soil dust
ggplot(data = PMF_OP_sm, aes(x = Soil.dust, y = Opv)) +
  geom_smooth(method = "lm") +
  geom_point() +
  stat_regline_equation(label.y = 1.2) + 
  stat_cor(aes(label = ..rr.label..), label.y = 1, digits = 3) #0.0385

ggplot(data = PMF_OP_nsm, aes(x = Soil.dust, y = Opv)) +
  geom_smooth(method = "lm") +
  geom_point() +
  stat_regline_equation(label.y = 1.2) + 
  stat_cor(aes(label = ..rr.label..), label.y = 1, digits = 3) #0.0636

# Biomass burning
ggplot(data = PMF_OP_nsm, aes(x = BB, y = Opv)) +
  geom_smooth(method = "lm") +
  geom_point() +
  stat_regline_equation(label.y = 1.2) + 
  stat_cor(aes(label = ..rr.label..), label.y = 1, digits = 3) #0.256

ggplot(data = PMF_OP_sm, aes(x = BB, y = Opv)) +
  geom_smooth(method = "lm") +
  geom_point() +
  stat_regline_equation(label.y = 1.2) + 
  stat_cor(aes(label = ..rr.label..), label.y = 1, digits = 3) # 0.178

# Cu-Mg, significant difference between the seasons
ggplot(data = PMF_OP_nsm, aes(x = Cu.Mg, y = Opv)) +
  geom_smooth(method = "lm") +
  geom_point() +
  stat_regline_equation(label.y = 1.2) + 
  stat_cor(aes(label = ..rr.label..), label.y = 1, digits = 3) #0.746

ggplot(data = PMF_OP_sm, aes(x = Cu.Mg, y = Opv)) +
  geom_smooth(method = "lm") +
  geom_point() +
  stat_regline_equation(label.y = 1.2) + 
  stat_cor(aes(label = ..rr.label..), label.y = 1, digits = 3) # 0.311

# Secondary aerosol
ggplot(data = PMF_OP_nsm, aes(x = Sec.aero, y = Opv)) +
  geom_smooth(method = "lm") +
  geom_point() +
  stat_regline_equation(label.y = 1.2) + 
  stat_cor(aes(label = ..rr.label..), label.y = 1, digits = 3) #0.247

ggplot(data = PMF_OP_sm, aes(x = Sec.aero, y = Opv)) +
  geom_smooth(method = "lm") +
  geom_point() +
  stat_regline_equation(label.y = 1.2) + 
  stat_cor(aes(label = ..rr.label..), label.y = 1, digits = 3) #0.108



# STARTING CORRELATION BETWEEN OPv 2020 AND EACH PMF FACTOR
# Burning Biomass R2= 0.6, good correlation
ggplot(data = PMF_OP_2020, aes(x = BB, y = Opv)) +
  geom_smooth(method = "lm") +
  geom_point() +
  stat_regline_equation(label.y = 1.2) +  
  stat_cor(aes(label = ..rr.label..), label.y = 1, digits = 3)

# Soil Dust, R2= 0.304, not correlated
ggplot(data = PMF_OP_2020, aes(x = Soil.dust, y = Opv)) +
  geom_smooth(method = "lm") +
  geom_point() +
  stat_regline_equation(label.y = 1.2) +  
  stat_cor(aes(label = ..rr.label..), label.y = 1, digits = 3)

# Secondary Aerosol, R2= 0.239, not correlated
ggplot(data = PMF_OP_2020, aes(x = Sec.aero, y = Opv)) +
  geom_smooth(method = "lm") +
  geom_point() +
  stat_regline_equation(label.y = 1.2) +  
  stat_cor(aes(label = ..rr.label..), label.y = 1, digits = 3)

# Cu-Mg, R2= 0.722, very good correlation
ggplot(data = PMF_OP_2020, aes(x = Cu.Mg, y = Opv)) +
  geom_smooth(method = "lm") +
  geom_point() +
  stat_regline_equation(label.y = 1.2) +  
  stat_cor(aes(label = ..rr.label..), label.y = 1, digits = 3)

# Traffic, R2= 0.388, not correlated
ggplot(data = PMF_OP_2020, aes(x = Traffic, y = Opv)) +
  geom_smooth(method = "lm") +
  geom_point() +
  stat_regline_equation(label.y = 1.2) +  
  stat_cor(aes(label = ..rr.label..), label.y = 1, digits = 3)

# CORRELATION TEST OPv 2021 AND PMF FACTORS
# Biomass Burning,R2= 0.347
ggplot(data = PMF_OP_2021, aes(x = BB, y = Opv)) +
  geom_smooth(method = "lm") +
  geom_point() +
  stat_regline_equation(label.y = 1.2) +  
  stat_cor(aes(label = ..rr.label..), label.y = 1, digits = 3)

# Soil Dust,R2= 0.124
ggplot(data = PMF_OP_2021, aes(x = Soil.dust, y = Opv)) +
  geom_smooth(method = "lm") +
  geom_point() +
  stat_regline_equation(label.y = 1.2) + 
  stat_cor(aes(label = ..rr.label..), label.y = 1, digits = 3)

# Secondary aerosol, R2= 0.432
ggplot(data = PMF_OP_2021, aes(x = Sec.aero, y = Opv)) +
  geom_smooth(method = "lm") +
  geom_point() +
  stat_regline_equation(label.y = 1.2) + 
  stat_cor(aes(label = ..rr.label..), label.y = 1, digits = 3)

# Cu-Mg, R2= 0.345
ggplot(data = PMF_OP_2021, aes(x = Cu.Mg, y = Opv)) +
  geom_smooth(method = "lm") +
  geom_point() +
  stat_regline_equation(label.y = 1.2) + 
  stat_cor(aes(label = ..rr.label..), label.y = 1, digits = 3)

# Traffic, R2= 0.463
ggplot(data = PMF_OP_2021, aes(x = Traffic, y = Opv)) +
  geom_smooth(method = "lm") +
  geom_point() +
  stat_regline_equation(label.y = 1.2) + 
  stat_cor(aes(label = ..rr.label..), label.y = 1, digits = 3)

# WE FILTER THE FULL DATASET FOR SEASONS, using PM 2.5 values as our main criteria:
# Non-smoke haze season
PMF_OP_2021_nsm <- PMF_OP_2020[27:77,]
# Smoke-haze season
PMF_OP_2020_sm <- PMF_OP_2020[1:26,] 

# RUNNING CORRELATION TEST PER SEASON 2020 PER FACTOR WITH OPv
# Traffic
ggplot(data = PMF_OP_2020_nsm, aes(x = Traffic, y = Opv)) +
  geom_smooth(method = "lm") +
  geom_point() +
  stat_regline_equation(label.y = 1.2) + 
  stat_cor(aes(label = ..rr.label..), label.y = 1, digits = 3) # 0.305

ggplot(data = PMF_OP_2020_sm, aes(x = Traffic, y = Opv)) +
  geom_smooth(method = "lm") +
  geom_point() +
  stat_regline_equation(label.y = 1.2) + 
  stat_cor(aes(label = ..rr.label..), label.y = 1, digits = 3) # 0.0265

# Soil dust
ggplot(data = PMF_OP_2020_nsm, aes(x = Soil.dust, y = Opv)) +
  geom_smooth(method = "lm") +
  geom_point() +
  stat_regline_equation(label.y = 1.2) + 
  stat_cor(aes(label = ..rr.label..), label.y = 1, digits = 3) #0.0967

ggplot(data = PMF_OP_2020_sm, aes(x = Soil.dust, y = Opv)) +
  geom_smooth(method = "lm") +
  geom_point() +
  stat_regline_equation(label.y = 1.2) + 
  stat_cor(aes(label = ..rr.label..), label.y = 1, digits = 3) #0.0604

# Biomass burning
ggplot(data = PMF_OP_2020_nsm, aes(x = BB, y = Opv)) +
  geom_smooth(method = "lm") +
  geom_point() +
  stat_regline_equation(label.y = 1.2) + 
  stat_cor(aes(label = ..rr.label..), label.y = 1, digits = 3) #0.239

ggplot(data = PMF_OP_2020_sm, aes(x = BB, y = Opv)) +
  geom_smooth(method = "lm") +
  geom_point() +
  stat_regline_equation(label.y = 1.2) + 
  stat_cor(aes(label = ..rr.label..), label.y = 1, digits = 3) # 0.12

# Cu-Mg, significant difference between the seasons
ggplot(data = PMF_OP_202_nsm, aes(x = Cu.Mg, y = Opv)) +
  geom_smooth(method = "lm") +
  geom_point() +
  stat_regline_equation(label.y = 1.2) + 
  stat_cor(aes(label = ..rr.label..), label.y = 1, digits = 3) #0.12

ggplot(data = PMF_OP_2020_sm, aes(x = Cu.Mg, y = Opv)) +
  geom_smooth(method = "lm") +
  geom_point() +
  stat_regline_equation(label.y = 1.2) + 
  stat_cor(aes(label = ..rr.label..), label.y = 1, digits = 3) # important 0.6

# Secondary aerosol
ggplot(data = PMF_OP_2020_nsm, aes(x = Sec.aero, y = Opv)) +
  geom_smooth(method = "lm") +
  geom_point() +
  stat_regline_equation(label.y = 1.2) + 
  stat_cor(aes(label = ..rr.label..), label.y = 1, digits = 3) #0.315

ggplot(data = PMF_OP_2020_sm, aes(x = Sec.aero, y = Opv)) +
  geom_smooth(method = "lm") +
  geom_point() +
  stat_regline_equation(label.y = 1.2) + 
  stat_cor(aes(label = ..rr.label..), label.y = 1, digits = 3) #0.00916
