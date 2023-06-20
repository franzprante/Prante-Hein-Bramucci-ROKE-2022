rm(list = ls())

library(openxlsx)
library(dplyr)

load(here::here("R/simulated_series_for_plots"))
load(here::here("R/simulated_series_for_plots_follow_up_paper"))

d4_mod <- d4 %>% 
  select(-c(gr_Y, gr_Yx, L_Yh2x)) %>% 
  mutate(period = 1:nrow(d4)) %>% 
  mutate(baseline_u = rep(tail(d1$u, 1), nrow(d4)), .before = u) %>% 
  mutate(baseline_C_Y = rep(tail(d1$C_Y, 1), nrow(d4)), .before = C_Y) %>% 
  mutate(baseline_I_Y = rep(tail(d1$I_Y, 1), nrow(d4)), .before = I_Y) %>% 
  mutate(baseline_G_Y = rep(tail(d1$G_Y, 1), nrow(d4)), .before = G_Y) %>% 
  mutate(baseline_NX_Y = rep(tail(d1$NX_Y, 1), nrow(d4)), .before = NX_Y) %>% 
  mutate(baseline_NIIP_Y = rep(tail(d1$NIIP_Y, 1), nrow(d4)), .before = NIIP_Y) %>% 
  mutate(baseline_S_g = rep(tail(d1$S_g, 1), nrow(d4)), .before = S_g) %>% 
  mutate(baseline_L_Yh2 = 0, .before = L_Yh2) %>% 
  mutate(max_leverage_ratio = 0.375, .before = baseline_L_Yh2) %>% 
  select(period, everything())

d5_mod <- d5 %>% 
  select(-c(gr_Y, gr_Yx, L_Yh2x)) %>% 
  mutate(period = 1:nrow(d5)) %>% 
  mutate(baseline_u = rep(tail(d1$u, 1), nrow(d5)), .before = u) %>% 
  mutate(baseline_C_Y = rep(tail(d1$C_Y, 1), nrow(d5)), .before = C_Y) %>% 
  mutate(baseline_I_Y = rep(tail(d1$I_Y, 1), nrow(d5)), .before = I_Y) %>% 
  mutate(baseline_G_Y = rep(tail(d1$G_Y, 1), nrow(d5)), .before = G_Y) %>% 
  mutate(baseline_NX_Y = rep(tail(d1$NX_Y, 1), nrow(d5)), .before = NX_Y) %>% 
  mutate(baseline_NIIP_Y = rep(tail(d1$NIIP_Y, 1), nrow(d5)), .before = NIIP_Y) %>% 
  mutate(baseline_S_g = rep(tail(d1$S_g, 1), nrow(d5)), .before = S_g) %>% 
  mutate(baseline_L_Yh2 = 0, .before = L_Yh2) %>% 
  mutate(max_leverage_ratio = 0.375, .before = baseline_L_Yh2) %>% 
  select(period, everything())

d_new_scen_mod <- d_new_scen %>% 
  select(-c(gr_Y, 
            gr_Yx,
            L_Yh2x,
            B_Y_ratio_g, 
            B_Y_ratio_gx, 
            B_Y_ratio_f, 
            B_Y_ratio_fx)) %>% 
  mutate(period = 1:nrow(d_new_scen)) %>% 
  mutate(baseline_u = rep(tail(d1$u, 1), nrow(d_new_scen)), .before = u) %>% 
  mutate(baseline_C_Y = rep(tail(d1$C_Y, 1), nrow(d_new_scen)), .before = C_Y) %>% 
  mutate(baseline_I_Y = rep(tail(d1$I_Y, 1), nrow(d_new_scen)), .before = I_Y) %>% 
  mutate(baseline_G_Y = rep(tail(d1$G_Y, 1), nrow(d_new_scen)), .before = G_Y) %>% 
  mutate(baseline_NX_Y = rep(tail(d1$NX_Y, 1), nrow(d_new_scen)), .before = NX_Y) %>% 
  mutate(baseline_NIIP_Y = rep(tail(d1$NIIP_Y, 1), nrow(d_new_scen)), .before = NIIP_Y) %>% 
  mutate(baseline_S_g = rep(tail(d1$S_g, 1), nrow(d_new_scen)), .before = S_g) %>% 
  mutate(baseline_L_Yh2 = 0, .before = L_Yh2) %>% 
  mutate(max_leverage_ratio = 0.375, .before = baseline_L_Yh2) %>%
  select(period, everything())

wb <- createWorkbook()
addWorksheet(wb, sheetName = "Figure 1") 
addWorksheet(wb, sheetName = "Figure 2") 
addWorksheet(wb, sheetName = "Figure 3")

writeData(wb, 1, x = d4_mod)
writeData(wb, 2, x = d5_mod)
writeData(wb, 3, x = d_new_scen_mod)

saveWorkbook(wb, file = "R/plots/Hein-Prante-Bramucci-PSLQR.xlsx", overwrite = TRUE)
