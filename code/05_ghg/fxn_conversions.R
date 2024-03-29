
#packageDetails("measurements")

# I can't get tehe NIST one to work. It has some weird units. 

# conversions -------------------------------------------------------------

library(measurements)


# density -----------------------------------------------------------------

den_dies_kg_per_l <- 0.8375
metribuzin_g_cm3 <- 1.31

# weight -------------------------------------------------------------------

kg_per_lb <- conv_unit(1, "lbs", "kg")
lb_per_kg <- 1 / kg_per_lb

lbs_per_ton <- 2000

g_per_kg <- 1000
kg_per_g <- 1/g_per_kg

# length -------------------------------------------------------------------


in_per_m <- conv_unit(1, "m", "inch")
in_per_ft <- 12
ft_per_in <- 1/12
m_per_in <- 1 / in_per_m

mm_per_m <- conv_unit(1, "m", "mm")
mm_per_in <- m_per_in * mm_per_m

m_per_ft <- conv_unit(1, "feet", "m")
ft_per_m <- 1/m_per_ft



# area --------------------------------------------------------------------

ac_per_ha <- conv_unit(1, "hectare", "acre")
ha_per_ac <- 1/ac_per_ha
m2_per_ha <- 10000


# energy ------------------------------------------------------------------


btu_per_mj <- conv_unit(1, "J", "BTU") * 1000 * 1000
mj_per_btu <- 1 / btu_per_mj
btu_per_ftlb <- 0.00128507
kwh_per_mwh <- 1000
mwh_per_kwh <- 1/1000
btu_per_kwh <- 3412.14
kwh_per_btu <- 1/btu_per_kwh
btu_per_mmbtu <- 1000000
mmbtu_per_btu <- 1/btu_per_mmbtu

mj_per_tj <- 1000000
tj_per_mj <- 1/mj_per_tj

# volume ------------------------------------------------------------------

cm3_per_m3 <- conv_unit(1, "m3", "cm3")
cuft_per_m3 <- conv_unit(1, "m3", "ft3")
m3_per_cuft <- 1/cuft_per_m3

cm3_per_gal <- 3785.41
gal_per_cm3 <- 1/cm3_per_gal

pints_per_gal <- conv_unit(1, "us_gal", "us_pint")
l_per_gal <- conv_unit(1, "us_gal", "l")
gal_per_l <- 1/l_per_gal
gal_per_pint <- 1/pints_per_gal

oz_per_gal <- conv_unit(1, "us_gal", "us_oz")
gal_per_oz <- 1/oz_per_gal

l_per_ml <- conv_unit(1, "ml", "l")
ml_per_l <- 1/l_per_ml

# water -------------------------------------------------------------------

g_per_gal_water <- 3785.41
lb_per_gal_water <- 8.3
gal_per_acft <- 325851
l_water_per_m3 <- 1000



# head --------------------------------------------------------------------

psi_per_mhead <- 1.42 #--psi per 1 meter of head
mhead_per_psi <- 1/psi_per_mhead

fthead_per_psi <- 2.31
psi_per_fthead <- 1/fthead_per_psi

# pressure ----------------------------------------------------------------

psi_per_kpa <- conv_unit(1, "kPa", "psi")
kpa_per_psi <- 1/psi_per_kpa


# ghg ---------------------------------------------------------------------

n_to_n2o <- 44/28


# density -----------------------------------------------------------------

dies_dens_g_per_ml <- 0.8375
gas_dens_g_per_ml <- 0.7429


