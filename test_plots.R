# LUCAS plots

library(readxl)
library(tidyverse)
library(tidybayes)
library(brms)

LUCAS_SOIL_2018 = read_excel("C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/LUCAS_data/Data_unzipped/LUCAS-SOIL-2018.xls")
texture_data    = read.csv("C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/LUCAS_data/Data_unzipped/LUCAS_Text_All_10032025.csv")
LUCAS_data_texture = merge(LUCAS_SOIL_2018,texture_data,by="POINTID")
bulk_density = read.csv("C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/LUCAS_data/Data_unzipped/BD2018_data_for_paper.csv")
colnames(bulk_density)[1] = "POINTID"

# Some filters ----

# 0-20 cm
LUCAS_data_texture_0_20 = LUCAS_data_texture %>% filter(Depth == "0-20 cm")
# Coarse land uses
# Agriculture
agriculture     = LUCAS_data_texture_0_20 %>% filter(LU1_Desc == "Agriculture (excluding fallow land and kitchen gardens)")
agriculture     = agriculture %>% filter(USDA != "") # no texture information
agriculture     = agriculture %>% filter(LC0_Desc %in% c("Grassland", "Cropland"))
agriculture     = agriculture %>% filter(USDA %in% c("clay","clay loam","loam",
                                                     "loamy sand","sand","sandy clay loam",
                                                     "sandy loam","silt loam","silty clay",
                                                     "silty clay loam"))
# Forest
forest          = LUCAS_data_texture_0_20 %>% filter(LU1_Desc == "Forestry")
forest          = forest %>% filter(USDA != "") # no texture information

# Exploratory plots

# AGRICULTURE----
table(agriculture$LC0_Desc,agriculture$USDA)

# pH
pH_agriculture = ggplot(agriculture,aes(x=as.numeric(pH_H2O),fill=LC0_Desc)) + geom_density(alpha=.4) + 
  facet_wrap(vars(USDA)) + labs(x="pH") + theme(legend.title = element_blank())

png("C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/LUCAS_data/Figures/pH_agriculture_data.png",
    width=3600,height=2700,res=300)
print(pH_agriculture)
dev.off()

# Organic matter
agriculture_1 = agriculture %>% filter(!OC %in% c("<0.0", "< LOD"))
OM_agriculture = ggplot(agriculture_1,aes(x=as.numeric(OC),fill=LC0_Desc)) + geom_density(alpha=.4) + 
  facet_wrap(vars(USDA)) + labs(x="Organic carbon content g/Kg") + theme(legend.title = element_blank())

png("C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/LUCAS_data/Figures/OM_agriculture_data.png",
    width=3600,height=2700,res=300)
print(OM_agriculture)
dev.off()

OM_agriculture_2 = ggplot(agriculture_1,aes(x=log(as.numeric(OC)),fill=LC0_Desc)) + geom_density(alpha=.4) + 
  facet_wrap(vars(USDA)) + labs(x="Log (Organic carbon content g/Kg)") + theme(legend.title = element_blank())

png("C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/LUCAS_data/Figures/OM_agriculture_data_2.png",
    width=3600,height=2700,res=300)
print(OM_agriculture_2)
dev.off()

# Bulk density
agriculture_bulk = merge(agriculture,bulk_density,by="POINTID")
BD_agriculture = ggplot(agriculture_bulk,aes(x=as.numeric(BDsample_0),fill=LC0_Desc.x)) + geom_density(alpha=.4) + 
  facet_wrap(vars(USDA)) + labs(x="Bulk Density") + theme(legend.title = element_blank())

png("C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/LUCAS_data/Figures/BD_agriculture_data.png",
    width=3600,height=2700,res=300)
print(BD_agriculture)
dev.off()

# Test density functions ----

# Bulk density ----
# Varying land‑use effect (random slopes):
# Each texture has its own cropland–grassland shift, partially pooled.
# Ecologically: “Cropland compacts sandy soils more than clay soils, but estimates borrow strength across textures.”

fit_density = brm(
  BDsample_0 ~ 1 + LC0_Desc.x + (1 + LC0_Desc.x| USDA),
  data = agriculture_bulk,
  family = gaussian(),
  prior = c(
    prior(normal(1.3, 0.3), class = Intercept),
    prior(normal(0, 0.2), class = b),          # land-use effect
    prior(exponential(1), class = sd),         # texture variation
    prior(exponential(1), class = sigma)       # residual SD
  ),
  cores = 4, chains = 4, iter = 4000
)

# Summary plots
summary(fit_density)
plot(fit_density)

# Fitted distributions

conditions = expand.grid(
  USDA = unique(agriculture_bulk$USDA),
  LC0_Desc.x = c("Cropland", "Grassland")
)
pred_draws = fit_density %>%
  add_predicted_draws(newdata = conditions, re_formula = NULL)

write.csv(pred_draws,"C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/LUCAS_data/Figures/posterior_BD.csv", row.names = FALSE)

# Plotting

posterior_DB = ggplot(pred_draws, aes(x = .prediction, fill = LC0_Desc.x)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ USDA, scales = "free") +
  labs(
    x = "Predicted Bulk Density",
    y = "Posterior density",
    fill = "Land use",
    title = "Posterior predictive distributions per USDA × land-use"
  ) +
  theme_bw()

png("C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/LUCAS_data/Figures/posterior_DB_agriculture_data.png",
    width=3600,height=2700,res=300)
print(posterior_DB)
dev.off()

posterior_DB_2 = ggplot(pred_draws, aes(x = LC0_Desc.x, y = .prediction, fill = LC0_Desc.x)) +
  geom_violin(alpha = 0.6) +
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  facet_wrap(~ USDA, scales = "free_y") +
  theme_bw() +
  labs(
    y = "Predicted Bulk Density",
    x = "Land use",
    title = "Posterior predictive distributions per USDA × land-use"
  )

png("C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/LUCAS_data/Figures/posterior_DB_2_agriculture_data.png",
    width=3600,height=2700,res=300)
print(posterior_DB_2)
dev.off()

# CDF

# Expected mean 
mu_draws = fit_density %>%
  add_epred_draws(newdata = conditions, re_formula = NULL)
# Expected sd
sigma_draws = fit_density %>% spread_draws(sigma)
mu_sigma = mu_draws %>%
  left_join(sigma_draws, by = ".draw")
# Grid
x_grid = seq(
  min(agriculture_bulk$BDsample_0),
  max(agriculture_bulk$BDsample_0),
  length.out = 200
)
# 1 - CDF for each posterior draw
cdf_draws = mu_sigma %>%
  tidyr::crossing(x = x_grid) %>%
  mutate(cdf = pnorm(x, mean = .epred, sd = sigma))
cdf_draws = cdf_draws %>%
  mutate(survival = 1 - cdf)
write.csv(cdf_draws,"C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/LUCAS_data/Figures/cdf_draws_BD.csv", row.names = FALSE)

# Plot CDF
cdf_draws = read.csv("C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/LUCAS_data/Figures/cdf_draws_BD.csv")

survival_summary = cdf_draws %>%
  group_by(USDA, LC0_Desc.x, x) %>%
  summarise(
    survival_mean = mean(survival),
    survival_low  = quantile(survival, 0.025),
    survival_high = quantile(survival, 0.975),
    .groups = "drop"
  )

write.csv(survival_summary,"C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/LUCAS_data/Figures/survival_summary_BD.csv", row.names = FALSE)

cdf_DB = ggplot(survival_summary,
                aes(x = x, y = survival_mean, color = LC0_Desc.x, fill = LC0_Desc.x)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = survival_low, ymax = survival_high),
              alpha = 0.2, color = NA) +
  facet_wrap(~ USDA, scales = "free") +
  theme_bw() +
  labs(
    x = "Bulk Density",
    y = "1 - CDF",
    color = "Land use",
    fill = "Land use",
    title = "Posterior survival curves with 95% credible intervals"
  )

png("C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/LUCAS_data/Figures/cdf_DB.png",
    width=3600,height=2700,res=300)
print(cdf_DB)
dev.off()

# Organic matter ----

fit_om = brm(
  log(as.numeric(OC)) ~ 1 + LC0_Desc + (1 + LC0_Desc | USDA),
  data = agriculture_1,
  family = gaussian(),
  cores = 4,
  seed = 1234
)

# Summary plots
summary(fit_om)
plot(fit_om)

# Fitted distributions

conditions = expand.grid(
  USDA = unique(agriculture_1$USDA),
  LC0_Desc = c("Cropland", "Grassland")
)
pred_draws = fit_om %>%
  add_predicted_draws(newdata = conditions, re_formula = NULL)

write.csv(pred_draws,"C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/LUCAS_data/Figures/posterior_OC.csv", row.names = FALSE)

# Plotting

posterior_OC = ggplot(pred_draws, aes(x = .prediction, fill = LC0_Desc)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ USDA, scales = "free") +
  labs(
    x = "log(Organic Carbon)",
    y = "Posterior density",
    fill = "Land use",
    title = "Posterior predictive distributions per USDA × land-use"
  ) +
  theme_bw()

png("C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/LUCAS_data/Figures/posterior_OC_agriculture_data.png",
    width=3600,height=2700,res=300)
print(posterior_OC)
dev.off()

posterior_OC_2 = ggplot(pred_draws, aes(x = LC0_Desc, y = .prediction, fill = LC0_Desc)) +
  geom_violin(alpha = 0.6) +
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  facet_wrap(~ USDA, scales = "free_y") +
  theme_bw() +
  labs(
    y = "log(Organic Carbon)",
    x = "Land use",
    title = "Posterior predictive distributions per USDA × land-use"
  )

png("C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/LUCAS_data/Figures/posterior_OC_2_agriculture_data.png",
    width=3600,height=2700,res=300)
print(posterior_OC_2)
dev.off()

# CDF

# Expected mean 
mu_draws = fit_om %>%
  add_epred_draws(newdata = conditions, re_formula = NULL)
# Expected sd
sigma_draws = fit_om %>% spread_draws(sigma)
mu_sigma = mu_draws %>%
  left_join(sigma_draws, by = ".draw")
# Grid
x_grid = seq(
  min(log(as.numeric((agriculture_1$OC)))),
  max(log(as.numeric((agriculture_1$OC)))),
  length.out = 200
)

# CDF for each posterior draw
cdf_draws = mu_sigma %>%
  tidyr::crossing(x = x_grid) %>%
  mutate(cdf = pnorm(x, mean = .epred, sd = sigma))

write.csv(cdf_draws,"C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/LUCAS_data/Figures/cdf_draws_OC.csv", row.names = FALSE)

# Plot CDF

cdf_draws = cdf_draws %>%
  group_by(USDA, LC0_Desc, x) %>%
  summarise(
    cdf_mean = mean(cdf),
    cdf_low  = quantile(cdf, 0.025),
    cdf_high = quantile(cdf, 0.975),
    .groups = "drop"
  )

write.csv(cdf_draws,"C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/LUCAS_data/Figures/cdf_summary_OC.csv", row.names = FALSE)

cdf_OC = ggplot(cdf_draws,
                aes(x = x, y = cdf_mean, color = LC0_Desc, fill = LC0_Desc)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = cdf_low, ymax = cdf_high),
              alpha = 0.2, color = NA) +
  facet_wrap(~ USDA, scales = "free") +
  theme_bw() +
  labs(
    x = "log(Organic Carbon)",
    y = "CDF",
    color = "Land use",
    fill = "Land use",
    title = "Posterior survival curves with 95% credible intervals"
  )

png("C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/LUCAS_data/Figures/cdf_OC.png",
    width=3600,height=2700,res=300)
print(cdf_OC)
dev.off()

# pH ----
# Gaussian first

fit_ph = brm(
  as.numeric(pH_H2O) ~ 1 + LC0_Desc + (1 + LC0_Desc | USDA),
  data = agriculture,
  family = gaussian(),
  seed = 1234,
  cores = 4, chains = 4, iter = 5000
)

# Summary plots
summary(fit_ph)
plot(fit_ph)

# Fitted distributions

conditions = expand.grid(
  USDA = unique(agriculture$USDA),
  LC0_Desc = c("Cropland", "Grassland")
)
pred_draws = fit_ph %>%
  add_predicted_draws(newdata = conditions, re_formula = NULL)

write.csv(pred_draws,"C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/LUCAS_data/Figures/posterior_pH.csv", row.names = FALSE)

# Plotting

posterior_pH = ggplot(pred_draws, aes(x = .prediction, fill = LC0_Desc)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ USDA, scales = "free") +
  labs(
    x = "pH",
    y = "Posterior density",
    fill = "Land use",
    title = "Posterior predictive distributions per USDA × land-use"
  ) +
  theme_bw()

png("C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/LUCAS_data/Figures/posterior_pH_agriculture_data.png",
    width=3600,height=2700,res=300)
print(posterior_pH)
dev.off()

posterior_pH_2 = ggplot(pred_draws, aes(x = LC0_Desc, y = .prediction, fill = LC0_Desc)) +
  geom_violin(alpha = 0.6) +
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  facet_wrap(~ USDA, scales = "free_y") +
  theme_bw() +
  labs(
    y = "pH)",
    x = "Land use",
    title = "Posterior predictive distributions per USDA × land-use"
  )

png("C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/LUCAS_data/Figures/posterior_pH_2_agriculture_data.png",
    width=3600,height=2700,res=300)
print(posterior_pH_2)
dev.off()

# Expected mean 
mu_draws = fit_ph %>%
  add_epred_draws(newdata = conditions, re_formula = NULL)
# Expected sd
sigma_draws = fit_ph %>% spread_draws(sigma)
mu_sigma = mu_draws %>%
  left_join(sigma_draws, by = ".draw")
# Grid
x_grid = seq(
  min((as.numeric((agriculture$pH_H2O)))),
  max((as.numeric((agriculture$pH_H2O)))),
  length.out = 200
)

# CDF for each posterior draw
optimum   = 6.5
tolerance = 0.5

cdf_draws = mu_sigma %>%
  tidyr::crossing(x = x_grid) %>%
  mutate(density = pnorm(x, mean = .epred, sd = sigma),
         weight = exp(-(x - optimum)^2 / (2 * tolerance^2)), # Multiply by a smooth weight around 6–7
         goodness = density * weight) 

cdf_draws2 = cdf_draws %>%
  group_by(USDA, LC0_Desc, x) %>%
  summarise(
    cdf_mean = mean(goodness),
    cdf_low  = quantile(goodness, 0.025),
    cdf_high = quantile(goodness, 0.975),
    .groups = "drop"
  )

test_pH = ggplot(cdf_draws2,
                 aes(x = x, y = cdf_mean, color = LC0_Desc, fill = LC0_Desc)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = cdf_low, ymax = cdf_high),
              alpha = 0.2, color = NA) +
  facet_wrap(~ USDA, scales = "free") +
  theme_bw() +
  labs(
    x = "pH",
    y = "Distance",
    color = "Land use",
    fill = "Land use",
  )

png("C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/LUCAS_data/Figures/test_pH.png",
    width=3600,height=2700,res=300)
print(test_pH)
dev.off()