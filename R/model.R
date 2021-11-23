library(sfcr)
source(here::here("R/simplot.R"))
#TODO Delete the above line and the file "simplot.R" when Plotly figures are done.

# Model equations

eqs <- sfcr_set(
  
  # GDP domestic economy
  Y ~ C + I + G + X - M,
  
  # GDP external economy X
  Yx ~ Cx + Ix + Gx + M - X,
  
  # Income domestic economy
  W ~ ws * Y,
  P ~ Y - W,
  Tax ~ tax * Y,
  T_W_1 ~ tax * W * ws_1,
  T_W_2 ~ tax * W - T_W_1,
  W_1_gross ~ ws_1 * W,
  W_2_gross ~ W - W_1_gross,
  W_1 ~ (1 - tax) * W * ws_1,
  W_2 ~ (1 - tax) * W - W_1,
  T_P ~ tax * P,
  P_net ~ (1 - tax) * P - r[-1] * L_f[-1] + r[-1] * D_f[-1],
  P_d ~ max( ( 1 - s_f ) * P_net , 0 ),
  Y_d_1 ~ W_1 + P_d + r[-1] * V_h_1[-1],
  Y_d_2 ~ W_2 + r[-1] * V_h_2[-1],
  P_f ~ P_net - P_d,
  
  # Income external economy X
  Wx ~ wsx * Yx,
  Px ~ Yx - Wx,
  Taxx ~ taxx * Yx,
  T_W_1x ~ taxx * Wx * ws_1x,
  T_W_2x ~ taxx * Wx - T_W_1x,
  W_1_grossx ~ ws_1x * Wx,
  W_2_grossx ~ Wx - W_1_grossx,
  W_1x ~ (1 - taxx) * Wx * ws_1x,
  W_2x ~ (1 - taxx) * Wx - W_1x,
  T_Px ~ taxx * Px,
  P_netx ~ (1 - taxx) * Px - r[-1] * L_fx[-1] + r[-1] * D_fx[-1],
  P_dx ~ max( ( 1 - s_fx ) * P_netx , 0 ),
  Y_d_1x ~ W_1x + P_dx + r[-1] * V_h_1x[-1],
  Y_d_2x ~ W_2x + r[-1] * V_h_2x[-1],
  P_fx ~ P_netx - P_dx,
  
  # Households domestic economy
  C ~ C_1 + C_2,
  c_0_1 ~ c_0_1[-1] * (1 + gr_c_0_1),
  C_1 ~ c_0_1 + c_Y_d_1 * Y_d_1 + c_D_1 * D_h_1[-1],
  constraint ~ if ( L_Y_ratio_h_2 < L_Y_ratio_banks ) {1} else {0},
  c_0_2 ~ c_0_2[-1] * (1 + gr_c_0_2),
  C_2 ~ c_0_2 + c_Y_d_2 * Y_d_2 + c_D_2 * D_h_2[-1] + constraint * alpha * C_1,
  S_h_1 ~ Y_d_1 - C_1,
  S_h_2 ~ Y_d_2 - C_2,
  V_h_1 ~ V_h_1[-1] + S_h_1,
  D_h_1 ~ max( 0 , V_h_1 ),
  L_h_1 ~ -1 * min( 0 , V_h_1 ),
  V_h_2 ~ V_h_2[-1] + S_h_2,
  D_h_2 ~ max( 0, V_h_2 ),
  L_h_2 ~ -1 * min( 0 , V_h_2 ),
  L_Y_ratio_h_2 ~ if ( Y_d_2[-1]==0) {0} else {L_h_2[-1] / Y_d_2[-1]},
  
  # Households external economy X
  Cx ~ C_1x + C_2x,
  c_0_1x ~ c_0_1x[-1] * (1 + gr_c_0_1x),
  C_1x ~ c_0_1x + c_Y_d_1x * Y_d_1x + c_D_1x * D_h_1x[-1],
  constraintx ~ if ( L_Y_ratio_h_2x < L_Y_ratio_banksx ) {1} else {0},
  c_0_2x ~ c_0_2x[-1] * (1 + gr_c_0_2x),
  C_2x ~ c_0_2x + c_Y_d_2x * Y_d_2x + c_D_2x * D_h_2x[-1] + constraintx * alphax * C_1x,
  S_h_1x ~ Y_d_1x - C_1x,
  S_h_2x ~ Y_d_2x - C_2x,
  V_h_1x ~ V_h_1x[-1] + S_h_1x,
  D_h_1x ~ max( 0 , V_h_1x ),
  L_h_1x ~ -1 * min( 0 , V_h_1x ),
  V_h_2x ~ V_h_2x[-1] + S_h_2x,
  D_h_2x ~ max( 0, V_h_2x ),
  L_h_2x ~ -1 * min( 0 , V_h_2x ),
  L_Y_ratio_h_2x ~ if ( Y_d_2x[-1]==0) {0} else {L_h_2x[-1] / Y_d_2x[-1]},
  
  # Firms domestic economy
  I ~ a_a * K[-1] + a_Y * v * Y,
  S_f ~ P_f - I,
  K ~ K[-1] - delta * K[-1] + I,
  u ~ Y / Y_fc,
  Y_fc ~ K[-1] / v,
  V_f_fin ~ V_f_fin[-1] + S_f,
  D_f ~ max( 0 , V_f_fin ),
  L_f ~ -1 * min( 0 , V_f_fin ),
  V_f ~ V_f_fin + K,
  
  # Firms external economy X
  Ix ~ a_ax * Kx[-1] + a_Yx * vx * Yx,
  S_fx ~ P_fx - Ix,
  Kx ~ Kx[-1] - deltax * Kx[-1] + Ix,
  ux ~ Yx / Y_fcx,
  Y_fcx ~ Kx[-1] / vx,
  V_f_finx ~ V_f_finx[-1] + S_fx,
  D_fx ~ max( 0 , V_f_finx ),
  L_fx ~ -1 * min( 0 , V_f_finx ),
  V_fx ~ V_f_finx + Kx,
  
  # Government domestic economy
  G_A ~ G_A[-1] * (1 + grG_A),
  G ~ G_A + sigma * Tax,
  S_g ~ Tax - G + r[-1] * V_g[-1],
  V_g ~ V_g[-1] + S_g,
  D_g ~ max( 0 , V_g ),
  L_g ~ -1 * min( 0 , V_g ),
  
  # Government external economy X
  G_Ax ~ G_Ax[-1] * (1 + grG_Ax),
  Gx ~ G_Ax + sigmax * Taxx,
  S_gx ~ Taxx - Gx + r[-1] * V_gx[-1],
  V_gx ~ V_gx[-1] + S_gx,
  D_gx ~ max( 0 , V_gx ),
  L_gx ~ -1 * min( 0 , V_gx ),
  
  # Trade, current account and NIIP
  M ~ (phi * u - psi * er) * K[-1],
  X ~ (phix * ux - psix * 1 / er) * Kx[-1],
  NX ~ X - M,
  CA ~ NX + r[-1] * NIIP[-1],
  R_CA ~ r[-1] * NIIP[-1],
  NIIP ~ NIIP[-1] + CA,
  
  # Banks domestic economy
  L ~ L_h_1 + L_h_2 + L_f + L_g + NIIP,
  R_L ~ r[-1] * L[-1],
  D ~ D_h_1 + D_h_2 + D_f + D_g,
  R_D ~ r[-1] * D[-1],
  R ~ R_L - R_D,
  V_b ~ L - D,
  
  # Banks external economy
  Lx ~ L_h_1x + L_h_2x + L_fx + L_gx - NIIP,
  R_Lx ~ r[-1] * Lx[-1],
  Dx ~ D_h_1x + D_h_2x + D_fx + D_gx,
  R_Dx ~ r[-1] * Dx[-1],
  Rx ~ R_Lx - R_Dx,
  V_bx ~ Lx - Dx,
  
  # Auxiliary calculations domestic economy
  # Objects for SFC-Checks
  sum_S ~ S_h_1 + S_h_2 + S_f + S_g - CA,
  V ~ V_h_1 + V_h_2 + V_g + V_b + NIIP + V_f_fin + K,
  V_fin ~ V_h_1 + V_h_2 + V_g + V_b - NIIP + V_f_fin,
  L_nb ~ L_h_1 + L_h_2 + L_f + L_g,
  # Shares in output
  C_Y_share ~ C / Y,
  I_Y_share ~ I / Y,
  G_Y_share ~ G / Y,
  NX_Y_share ~ NX / Y, 
  # Trade stock-flow ratios
  CA_Y_ratio ~ (NX + R_CA) / Y, 
  NIIP_Y_ratio ~ NIIP / Y,
  # Growth rates
  gr_Y ~ (Y - Y[-1]) / Y[-1],
  gr_C ~ (C - C[-1]) / C[-1],
  gr_I ~ (I - I[-1]) / I[-1],
  gr_G ~ (G - G[-1]) / G[-1],
  gr_NX ~ (NX - NX[-1]) / NX[-1],
  gr_K ~ (K - K[-1]) / K[-1],
  
  # Auxiliary calculations external economy
  # Objects for SFC-Checks
  sum_Sx ~ S_h_1x + S_h_2x + S_fx + S_gx + CA,
  Vx ~ V_h_1x + V_h_2x + V_gx + V_bx - NIIP + V_f_finx + Kx,
  V_finx ~ V_h_1x + V_h_2x + V_gx + V_bx - NIIP + V_f_finx,
  L_nbx ~ L_h_1x + L_h_2x + L_fx + L_gx,
  # Shares in output
  C_Y_sharex ~ Cx / Yx,
  I_Y_sharex ~ Ix / Yx,
  G_Y_sharex ~ Gx / Yx,
  NX_Y_sharex ~ - NX / Yx,
  # Trade stock-flow ratios
  CA_Y_ratiox ~ ( - NX - R_CA) / Yx,
  NIIP_Y_ratiox ~ - NIIP / Yx,
  # Growth rates
  gr_Yx ~ (Yx - Yx[-1]) / Yx[-1],
  gr_Cx ~ (Cx - Cx[-1]) / Cx[-1],
  gr_Ix ~ (Ix - Ix[-1]) / Ix[-1],
  gr_Gx ~ (Gx - Gx[-1]) / Gx[-1],
  gr_NXx ~ (- NX - (-NX[-1])) / (-NX[-1]),
  gr_Kx ~ (Kx - Kx[-1]) / Kx[-1],
  
  # Global capital stock
  K_glob ~ K + Kx,
  # Global wealth
  V_glob ~ V + Vx
)

# Baseline

external <- sfcr_set(
  # Domestic economy
  ws ~ 0.6,
  ws_1 ~ 0.2,
  tax ~ 0.2,
  sigma ~ 1,
  r ~ 0.01,
  s_f ~ 0.3,
  gr_c_0_1 ~ 0,
  gr_c_0_2 ~ 0,
  c_Y_d_1 ~ 0.4,
  c_D_1 ~ 0.05,
  c_Y_d_2 ~ 0.7,
  c_D_2 ~ 0.015,
  alpha ~ 0,
  a_a ~ 0.015,
  a_Y ~ 0.016,
  a_r ~ 0,
  delta ~ 0.01,
  grG_A ~ 0.01,
  L_Y_ratio_banks ~ 0,
  v ~ 5,
  
  # Trade
  er ~ 1,
  # Domestic economy
  psi ~ 0.01,
  phi ~ 0.05,
  # External economy
  psix ~ 0.01,
  phix ~ 0.05,
  
  # External economy
  wsx ~ 0.6,
  ws_1x ~ 0.2,
  taxx ~ 0.2,
  sigmax ~ 1,
  s_fx ~ 0.3,
  gr_c_0_1x ~ 0,
  gr_c_0_2x ~ 0,
  c_Y_d_1x ~ 0.4,
  c_D_1x ~ 0.05,
  c_Y_d_2x ~ 0.7,
  c_D_2x ~ 0.015,
  alphax ~ 0,
  a_ax ~ 0.015,
  a_Yx ~ 0.016,
  a_rx ~ 0,
  deltax ~ 0.01,
  grG_Ax ~ 0.01,
  L_Y_ratio_banksx ~ 0,
  vx ~ 5
)

initial <- sfcr_set(
  # Domestic economy
  c_0_1 ~ 0.2,
  c_0_2 ~ 0.2,
  G_A ~ 0.5,
  r ~ 0.01,
  S_h_1 ~ 0,
  S_h_2 ~ 0,
  S_f ~ 0,
  S_g ~ 0,
  sum_S ~ 0,
  R ~ 0,
  CA ~ 0,
  L_h_1 ~ 0,
  L_h_2 ~ 0,
  V_h_1 ~ 0,
  D_h_1 ~ 0,
  D_h_2 ~ 0,
  V_h_2 ~ 0,
  L_f ~ 0,
  D_f ~ 0,
  V_f_fin ~ 0,
  D_g ~ 0,
  L_g ~ 0,
  V_g ~ 0,
  V_b ~ 0,
  NIIP ~ 0,
  V_fin ~ 0,
  K ~ 40,
  V_f ~ 40,
  V ~ 40,
  
  # External economy
  c_0_1x ~ 0.2,
  c_0_2x ~ 0.2,
  G_Ax ~ 0.5,
  S_h_1x ~ 0,
  S_h_2x ~ 0,
  S_fx ~ 0,
  S_gx ~ 0,
  sum_Sx ~ 0,
  Rx ~ 0,
  L_h_1x ~ 0,
  L_h_2x ~ 0,
  V_h_1x ~ 0,
  D_h_1x ~ 0,
  D_h_2x ~ 0,
  V_h_2x ~ 0,
  L_fx ~ 0,
  D_fx ~ 0,
  V_f_finx ~ 0,
  D_gx ~ 0,
  L_gx ~ 0,
  V_gx ~ 0,
  V_bx ~ 0,
  V_finx ~ 0,
  Kx ~ 40,
  V_fx ~ 40,
  Vx ~ 40
)

bl <- sfcr_baseline(
  equations = eqs,
  external = external,
  initial = initial,
  periods = 1500,
  method = 'Broyden',
  hidden = c("K_glob" = "V_glob"),
  max_iter = 350,
  .hidden_tol = 1e-08,
  tol = 1e-08,
  rhtol = TRUE,
)

par(mfrow = c(1,1))
plot(bl$V_glob, type = "l", lty = 14, lwd = 3, col = 2)
lines(bl$K_glob)
mtext(latex2exp::TeX("SFC-check: V_{global} vs. K_{global}"))

# Base R figure 1
layout(matrix(c(1, 2, 3,
                4, 5, 6,
                7, 7, 7), nrow=3, byrow=TRUE), heights = c(0.45,0.45,0.1))
par(mar=c(3,2,3,2))
simplot(var = "u", scen = bl, tc = T)
hr <- 0.0163
simplot(var = "gr_Y", tex = "Growth rate of Y", scen = bl, start = 10, tc = T, autscal = F, h = hr)
simplot(var = "C_Y_share", scen = bl, tc = T, tex = "C / Y")
simplot(var = "I_Y_share", scen = bl, tc = T, tex = "I / Y")
simplot(var = "G_Y_share", scen = bl, tc = T, tex = "G / Y")
simplot(var = "NX_Y_share", scen = bl, tc = T, h = 0, tex = "NX / Y")
par(mar=c(0,0,0,0)) 
plot(1, type = "n", axes=F, xlab="", ylab="") 
legend("center", 
       legend=c("Domestic economy",
                "External economy"),
       col=c("black", "red", "grey"), lty = c(1,3), horiz = T, lwd = 2)

# Dataframe for figure 1
d1 <- data.frame(
  u = bl$u, 
  ux = bl$ux, 
  gr_Y = bl$gr_Y, 
  gr_Yx = bl$gr_Yx,
  C_Y = bl$C_Y_share,
  C_Yx = bl$C_Y_sharex,
  I_Y = bl$I_Y_share,
  I_Yx = bl$I_Y_sharex,
  G_Y = bl$G_Y_share,
  G_Yx = bl$G_Y_sharex,
  NX_Y = bl$NX_Y_share,
  NX_Yx = bl$NX_Y_sharex
)

# Extending the baseline for scenario time range
blext <- sfcr_scenario(
  baseline = bl,
  scenario = NULL,
  periods = 2500
)

# Dataframe for extended baseline scenario
dbl <- data.frame(
  u = blext$u, 
  ux = blext$ux, 
  gr_Y = blext$gr_Y, 
  gr_Yx = blext$gr_Yx,
  C_Y = blext$C_Y_share,
  C_Yx = blext$C_Y_sharex,
  I_Y = blext$I_Y_share,
  I_Yx = blext$I_Y_sharex,
  G_Y = blext$G_Y_share,
  G_Yx = blext$G_Y_sharex,
  NX_Y = blext$NX_Y_share,
  NX_Yx = blext$NX_Y_sharex
)

# Scenarios

# DLPD and ELM before crisis
shock_1 <- sfcr_shock(
  variables = sfcr_set(
    ws ~ 0.55,
    ws_1 ~ 0.3,
    wsx ~ 0.5,
    L_Y_ratio_banks ~ 0.375,
    alpha ~ 0.29,
    er ~ 0.9
  ),
  start = 100,
  end = 2500
)

DLPD_ELM <- sfcr_scenario(
  baseline = bl,
  scenario = list(
    shock_1
  ),
  periods = 2500
)

par(mfrow = c(1,1))
plot(DLPD_ELM$V_glob, type = "l", lty = 14, lwd = 3, col = 2)
lines(DLPD_ELM$K_glob)
mtext(latex2exp::TeX("SFC-check: V_{global} vs. K_{global}"))

# Base R figure 2
layout(matrix(c(1, 2, 3,
                4, 5, 6,
                7, 7, 7), nrow=3, byrow=TRUE), heights = c(0.45,0.45,0.1))
par(mar=c(3,2,3,2))
simplot(var = "u", scen = DLPD_ELM, tc = T)
lines(blext$u, col = "grey", lwd = 3)
plot(DLPD_ELM$gr_Y, type = "l", ylim = c(0.016,0.018), lwd = 3)
lines(DLPD_ELM$gr_Yx, col = "red", lwd = 3, lty = 3)
lines(blext$gr_Yx, col = "grey", lwd = 3)
mtext("Growth Y")
simplot("L_Y_ratio_h_2", scen = DLPD_ELM, tex = "debt / income non-rich", end = 2500, tc = T)
lines(blext$L_Y_ratio_h_2, col = "grey", lwd = 3)
simplot(var = "C_Y_share", scen = DLPD_ELM, tc = T, tex = "C / Y")
lines(blext$C_Y_share, col = "grey", lwd = 3)
simplot(var = "NX_Y_share", scen = DLPD_ELM, tc = T, h = 0, tex = "NX / Y")
lines(blext$NX_Y_share, col = "grey", lwd = 3)
simplot("NIIP_Y_ratio", scen = DLPD_ELM, tc =T, h = 0, tex = "NIIP / Y")
lines(blext$NIIP_Y_ratio, col = "grey", lwd = 3)
par(mar=c(0,0,0,0)) 
plot(1, type = "n", axes=F, xlab="", ylab="") 
legend("center", 
       legend=c("Domestic economy",
                "External economy",
                "Baseline"),
       col=c("black", "red", "grey"), lty = c(1,3), horiz = T, lwd = 2)

# Dataframe for figure 2
d2 <- data.frame(
  u = DLPD_ELM$u, 
  ux = DLPD_ELM$ux, 
  gr_Y = DLPD_ELM$gr_Y, 
  gr_Yx = DLPD_ELM$gr_Yx,
  C_Y = DLPD_ELM$C_Y_share,
  C_Yx = DLPD_ELM$C_Y_sharex,
  I_Y = DLPD_ELM$I_Y_share,
  I_Yx = DLPD_ELM$I_Y_sharex,
  G_Y = DLPD_ELM$G_Y_share,
  G_Yx = DLPD_ELM$G_Y_sharex,
  NX_Y = DLPD_ELM$NX_Y_share,
  NX_Yx = DLPD_ELM$NX_Y_sharex
)

# Crisis
shock_2 <- sfcr_shock(
  variables = sfcr_set(
    ws ~ seq(0.55, 0.547, length.out = 30),
    ws_1 ~ seq(0.3, 0.303, length.out = 30)
  ),
  start = 1000,
  end = 1029
)

shock_3 <- sfcr_shock(
  variables = sfcr_set(
    L_Y_ratio_banks ~ 0,
    alpha ~ 0
    
  ),
  start = 1030,
  end = 2500
)

crisis <- sfcr_scenario(
  baseline = bl,
  scenario = list(
    shock_1,
    shock_2,
    shock_3
  ),
  periods = 2500
)

par(mfrow = c(1,1))
plot(crisis$V_glob, type = "l", lty = 14, lwd = 3, col = 2)
lines(crisis$K_glob)
mtext(latex2exp::TeX("SFC-check: V_{global} vs. K_{global}"))

# Base R figure 3
layout(matrix(c(1, 2, 3,
                4, 5, 6,
                7, 7, 7), nrow=3, byrow=TRUE), heights = c(0.45,0.45,0.1))
par(mar=c(3,2,3,2))
simplot(var = "u", scen = crisis, tc = T)
lines(blext$u, col = "grey", lwd = 3)
plot(crisis$gr_Y, type = "l", ylim = c(0.014,0.018), lwd = 3)
lines(crisis$gr_Yx, col = "red", lwd = 3, lty = 3)
lines(blext$gr_Yx, col = "grey", lwd = 3)
mtext("Growth Y")
simplot("L_Y_ratio_h_2", scen = crisis, tex = "debt / income non-rich", end = 2500, tc = T)
lines(blext$L_Y_ratio_h_2, col = "grey", lwd = 3)
simplot(var = "C_Y_share", scen = crisis, tc = T, tex = "C / Y")
lines(blext$C_Y_share, col = "grey", lwd = 3)
simplot(var = "NX_Y_share", scen = crisis, tc = T, h = 0, tex = "NX / Y")
lines(blext$NX_Y_share, col = "grey", lwd = 3)
simplot("NIIP_Y_ratio", scen = crisis, tc =T, h = 0, tex = "NIIP / Y")
lines(blext$NIIP_Y_ratio, col = "grey", lwd = 3)
par(mar=c(0,0,0,0)) 
plot(1, type = "n", axes=F, xlab="", ylab="") 
legend("center", 
       legend=c("Domestic economy",
                "External economy",
                "Baseline"),
       col=c("black", "red", "grey"), lty = c(1,3), horiz = T, lwd = 2)

# Dataframe for figure 3
d3 <- data.frame(
  u = crisis$u, 
  ux = crisis$ux, 
  gr_Y = crisis$gr_Y, 
  gr_Yx = crisis$gr_Yx,
  C_Y = crisis$C_Y_share,
  C_Yx = crisis$C_Y_sharex,
  I_Y = crisis$I_Y_share,
  I_Yx = crisis$I_Y_sharex,
  G_Y = crisis$G_Y_share,
  G_Yx = crisis$G_Y_sharex,
  NX_Y = crisis$NX_Y_share,
  NX_Yx = crisis$NX_Y_sharex
)

# DLPD to DDL
shock_4 <- sfcr_shock(
  variables = sfcr_set(
    sigma ~ 1.05,
    ws ~ 0.6,
    ws_1 ~ 0.2
  ),
  start = 1031,
  end = 2500
)
DDL <- sfcr_scenario(
  baseline = bl,
  scenario = list(
    shock_1,
    shock_2,
    shock_3,
    shock_4
  ),
  periods = 2500
)

par(mfrow = c(1,1))
plot(DDL$V_glob, type = "l", lty = 14, lwd = 3, col = 2)
lines(DDL$K_glob)
mtext(latex2exp::TeX("SFC-check: V_{global} vs. K_{global}"))

# Base R figure 4
layout(matrix(c(1, 2, 
                3, 4,
                5, 5), nrow=3, byrow=TRUE), heights = c(0.45,0.45,0.1))
par(mar=c(3,2,3,2))
simplot(var = "u", scen = DDL, tc = T)
lines(blext$u, col = "grey", lwd = 3)
plot(DDL$gr_Y, type = "l", ylim = c(0.01,0.018), lwd = 3)
lines(DDL$gr_Yx, col = "red", lwd = 3, lty = 3)
lines(blext$gr_Yx, col = "grey", lwd = 3)
mtext("Growth Y")
plot(DDL$S_g / DDL$Y, type = "l", lwd = 3, ylab = "", xlab = "")
abline(h = 0 ,lty = 2)
mtext(latex2exp::TeX("S_g / Y"))
simplot(var = "NX_Y_share", scen = DDL, tc = T, h = 0, tex = "NX / Y")
lines(blext$NX_Y_share, col = "grey", lwd = 3)
par(mar=c(0,0,0,0)) 
plot(1, type = "n", axes=F, xlab="", ylab="") 
legend("center", 
       legend=c("Domestic economy",
                "External economy",
                "Baseline"),
       col=c("black", "red", "grey"), lty = c(1,3), horiz = T, lwd = 2)

# Dataframe for figure 4
d4 <- data.frame(
  u = DDL$u, 
  ux = DDL$ux, 
  gr_Y = DDL$gr_Y, 
  gr_Yx = DDL$gr_Yx,
  C_Y = DDL$C_Y_share,
  C_Yx = DDL$C_Y_sharex,
  I_Y = DDL$I_Y_share,
  I_Yx = DDL$I_Y_sharex,
  G_Y = DDL$G_Y_share,
  G_Yx = DDL$G_Y_sharex,
  NX_Y = DDL$NX_Y_share,
  NX_Yx = DDL$NX_Y_sharex
)

# DLPD to ELM
shock_5 <- sfcr_shock(
  variables = sfcr_set(
    sigma ~ 0.95,
    psi ~ 0.015,
    er ~ 1,
    phi ~ 0.04,
    phix ~ 0.06, 
    sigmax ~ 1.15,
    ws ~ 0.5,
    ws_1 ~ 0.25
  ),
  start = 1031,
  end = 2500
)

ELM <- sfcr_scenario(
  baseline = bl,
  scenario = list(
    shock_1,
    shock_2,
    shock_3,
    shock_5
  ),
  periods = 2500
)

par(mfrow = c(1,1))
plot(ELM$V_glob, type = "l", lty = 14, lwd = 3, col = 2)
lines(ELM$K_glob)
mtext(latex2exp::TeX("SFC-check: V_{global} vs. K_{global}"))

# Base R figure 5
layout(matrix(c(1, 2, 
                3, 4,
                5, 5), nrow=3, byrow=TRUE), heights = c(0.45,0.45,0.1))
par(mar=c(3,2,3,2))
simplot(var = "u", scen = ELM, tc = T)
lines(blext$u, col = "grey", lwd = 3)
plot(ELM$gr_Y, type = "l", ylim = c(0.01,0.018), lwd = 3)
lines(ELM$gr_Yx, col = "red", lwd = 3, lty = 3)
lines(blext$gr_Yx, col = "grey", lwd = 3)
mtext("Growth Y")
simplot(var = "NX_Y_share", scen = ELM, tc = T, h = 0, tex = "NX / Y")
lines(blext$NX_Y_share, col = "grey", lwd = 3)
simplot("NIIP_Y_ratio", scen = ELM, tc =T, h = 0, tex = "NIIP / Y")
lines(blext$NIIP_Y_ratio, col = "grey", lwd = 3)
par(mar=c(0,0,0,0)) 
plot(1, type = "n", axes=F, xlab="", ylab="") 
legend("center", 
       legend=c("Domestic economy",
                "External economy",
                "Baseline"),
       col=c("black", "red", "grey"), lty = c(1,3), horiz = T, lwd = 2)

# Dataframe for figure 5
d5 <- data.frame(
  u = ELM$u, 
  ux = ELM$ux, 
  gr_Y = ELM$gr_Y, 
  gr_Yx = ELM$gr_Yx,
  C_Y = ELM$C_Y_share,
  C_Yx = ELM$C_Y_sharex,
  I_Y = ELM$I_Y_share,
  I_Yx = ELM$I_Y_sharex,
  G_Y = ELM$G_Y_share,
  G_Yx = ELM$G_Y_sharex,
  NX_Y = ELM$NX_Y_share,
  NX_Yx = ELM$NX_Y_sharex
)

save(d1, dbl, d2, d3, d4, d5, file = here::here("R/simulated_series_for_plots"))
