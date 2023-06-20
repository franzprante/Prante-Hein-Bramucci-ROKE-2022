library(plotly)

# Load the data
load(here::here("R/simulated_series_for_plots_follow_up_paper"))

t <- list(family = "cambria")

# Annotations for subplots
u <- list(
  text = "DLPD                 u                  PES-DDL",
  font = list(size = 12),
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE)

C_Y <- list(
  text = "DLPD                 C/Y                  PES-DDL",
  font = list(size = 12),
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE)

I_Y <- list(
  text = "I/Y",
  font = list(size = 12),
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE)

G_Y <- list(
  text = "G/Y",
  font = list(size = 12),
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE)

NX_Y <- list(
  text = "NX/Y",
  font = list(size = 12),
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE)

S_g_Y <- list(
  text = "S<sub>g</sub>/Y",
  font = list(size = 12),
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE)

L_Yh2 <- list(
  text = "L<sub>h2</sub>/Y<sub>dh2</sub>",
  font = list(size = 12),
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE)

NIIP_Y <- list(
  text = "NIIP/Y",
  font = list(size = 12),
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE)

# Utilization rate
p_u_before <- plot_ly() %>% 
  add_trace(x = 0:(nrow(d_new_scen)-3),
            y = tail(d1$u, 1),
            type = "scatter",
            mode = "lines",
            name = "u<sub>baseline</sub>",
            line = list(color = "lightgrey")) %>% 
  add_trace(x = 1:950,#0:(nrow(d_new_scen)-1),
            y = d_new_scen$u[1:950],
            type = "scatter",
            mode = "lines",
            line = list(color = "black"),
            name = "u") %>% 
  add_trace(x = 1:950,#0:(nrow(d_new_scen)-1),
            y = d_new_scen$ux[1:950],
            type = "scatter",
            mode = "lines",
            line = list(color = "rgb(144,144,144)",
                        dash = "dash"),
            name = "u<sub>x</sub>") %>%
  layout(xaxis = list(range = c(-10, 950),
                      tickfont = list(size = 7)),
         yaxis = list(range = c(0.55, 0.8),
                      tickfont = list(size = 7),
                      dtick = 0.05),
         showlegend = TRUE,
         hovermode = "compare")

p_u_after <- plot_ly() %>% 
  add_trace(x = 0:(nrow(d_new_scen)-3),
            y = tail(d1$u, 1),
            type = "scatter",
            mode = "lines",
            name = "u<sub>baseline</sub>",
            line = list(color = "lightgrey")) %>% 
  add_trace(x = 950:2000,#0:(nrow(d_new_scen)-1),
            y = d_new_scen$u[950:2000],
            type = "scatter",
            mode = "lines",
            line = list(color = "black"),
            name = "u") %>% 
  add_trace(x = 950:2000,#0:(nrow(d_new_scen)-1),
            y = d_new_scen$ux[950:2000],
            type = "scatter",
            mode = "lines",
            line = list(color = "rgb(144,144,144)",
                        dash = "dash"),
            name = "u<sub>x</sub>") %>%
  layout(xaxis = list(range = c(945, 2000),
                      dtick = 200,
                      tickfont = list(size = 7)),
         yaxis = list(range = c(0.55, 0.8),
                      tickfont = list(size = 7),
                      showticklabels = FALSE,
                      dtick = 0.05),
         showlegend = TRUE,
         hovermode = "compare")

sub_plots_p_u <- subplot(style(p_u_before, showlegend = FALSE),
                         p_u_after,
                         nrows = 1, margin = 0.000, titleY = T, titleX = T) %>% 
  layout(annotations = u,
         font = t)

# Consumption share
p_C_before <- plot_ly() %>% 
  add_trace(x = 0:(nrow(d_new_scen)-3),
            y = tail(d1$C_Y, 1),
            type = "scatter",
            mode = "lines",
            name = "Baseline",
            line = list(color = "lightgrey")) %>% 
  add_trace(x = 1:950,#0:(nrow(d_new_scen)-1),
            y = d_new_scen$C_Y[1:950],
            type = "scatter",
            mode = "lines",
            line = list(color = "black"),
            name = "C/Y") %>% 
  add_trace(x = 1:950,#0:(nrow(d_new_scen)-1),
            y = d_new_scen$C_Yx[1:950],
            type = "scatter",
            mode = "lines",
            line = list(color = "rgb(144,144,144)",
                        dash = "dash"),
            name = "C/Y<sub>x</sub>") %>%
  layout(xaxis = list(range = c(-10, 950),
                      tickfont = list(size = 7)),
         yaxis = list(range = c(0.55, 0.7),
                      tickfont = list(size = 7)),
         showlegend = TRUE,
         hovermode = "compare")

p_C_after <- plot_ly() %>% 
  add_trace(x = 0:(nrow(d_new_scen)-3),
            y = tail(d1$C_Y, 1),
            type = "scatter",
            mode = "lines",
            name = "Baseline",
            line = list(color = "lightgrey")) %>% 
  add_trace(x = 950:2000,#0:(nrow(d_new_scen)-1),
            y = d_new_scen$C_Y[950:2000],
            type = "scatter",
            mode = "lines",
            line = list(color = "black"),
            name = "C/Y") %>% 
  add_trace(x = 950:2000,#0:(nrow(d_new_scen)-1),
            y = d_new_scen$C_Yx[950:2000],
            type = "scatter",
            mode = "lines",
            line = list(color = "rgb(144,144,144)",
                        dash = "dash"),
            name = "C/Y<sub>x</sub>") %>%
  layout(xaxis = list(range = c(945, 2000),
                      dtick = 200,
                      tickfont = list(size = 7)),
         yaxis = list(range = c(0.55, 0.7),
                      showticklabels = FALSE,
                      tickfont = list(size = 7)),
         showlegend = TRUE,
         hovermode = "compare")

sub_plots_p_C <- subplot(style(p_C_before, showlegend = FALSE),
                         p_C_after,
                         nrows = 1, margin = 0.000, titleY = T, titleX = T) %>% 
  layout(annotations = C_Y)

# Investment share
p_I_before <- plot_ly() %>%
  add_trace(x = 0:(nrow(d_new_scen)-3),
            y = tail(d1$I_Y, 1),
            type = "scatter",
            mode = "lines",
            name = "I/Y<sub>baseline</sub>",
            line = list(color = "lightgrey")) %>% 
  add_trace(x = 1:950,#0:(nrow(d_new_scen)-1),
            y = d_new_scen$I_Y[1:950],
            type = "scatter",
            mode = "lines",
            line = list(color = "black"),
            name = "I/Y") %>% 
  add_trace(x = 1:950,#0:(nrow(d_new_scen)-1),
            y = d_new_scen$I_Yx[1:950],
            type = "scatter",
            mode = "lines",
            line = list(color = "rgb(144,144,144)",
                        dash = "dash"),
            name = "I/Y<sub>x</sub>") %>%
  layout(xaxis = list(range = c(-10, 950),
                      tickfont = list(size = 7)),
         yaxis = list(range = c(0.17, 0.22),
                      tickfont = list(size = 7)),
         showlegend = TRUE,
         hovermode = "compare")

# Investment share
p_I_after <- plot_ly() %>%
  add_trace(x = 0:(nrow(d_new_scen)-3),
            y = tail(d1$I_Y, 1),
            type = "scatter",
            mode = "lines",
            name = "I/Y<sub>baseline</sub>",
            line = list(color = "lightgrey")) %>% 
  add_trace(x = 950:2000,#0:(nrow(d_new_scen)-1),
            y = d_new_scen$I_Y[950:2000],
            type = "scatter",
            mode = "lines",
            line = list(color = "black"),
            name = "I/Y") %>% 
  add_trace(x = 950:2000,#0:(nrow(d_new_scen)-1),
            y = d_new_scen$I_Yx[950:2000],
            type = "scatter",
            mode = "lines",
            line = list(color = "rgb(144,144,144)",
                        dash = "dash"),
            name = "I/Y<sub>x</sub>") %>%
  layout(xaxis = list(range = c(945, 2000),
                      dtick = 200,
                      tickfont = list(size = 7)),
         yaxis = list(range = c(0.17, 0.22),
                      showticklabels = FALSE,
                      tickfont = list(size = 7)),
         showlegend = TRUE,
         hovermode = "compare")

sub_plots_p_I <- subplot(style(p_I_before, showlegend = FALSE),
                         p_I_after,
                         nrows = 1, margin = 0.000, titleY = T, titleX = T) %>% 
  layout(annotations = I_Y)

# Government share
p_G_before <- plot_ly() %>% 
  add_trace(x = 1:950,#0:(nrow(d_new_scen)-1),
            y = 0.2,
            type = "scatter",
            mode = "lines",
            name = "Baseline",
            line = list(color = "lightgrey")) %>% 
  add_trace(x = 1:950,#0:(nrow(d_new_scen)-1),
            y = round(d_new_scen$G_Y[1:950], digits = 2),
            type = "scatter",
            mode = "lines",
            line = list(color = "black"),
            name = "G/Y") %>% 
  add_trace(x = 1:950,#0:(nrow(d_new_scen)-1),
            y = round(d_new_scen$G_Yx[1:950], digits = 2),
            type = "scatter",
            mode = "lines",
            line = list(color = "rgb(144,144,144)",
                        dash = "dash"),
            name = "G/Y<sub>x</sub>") %>%
  layout(xaxis = list(range = c(-10, 950),
                      tickfont = list(size = 7)),
         yaxis = list(range = c(0.15, 0.25), 
                      tickfont = list(size = 7)),
         showlegend = TRUE,
         hovermode = "compare")

p_G_after <- plot_ly() %>%
  add_trace(x = 950:2000,#0:(nrow(d_new_scen)-1),
            y = 0.2,
            type = "scatter",
            mode = "lines",
            name = "Baseline",
            line = list(color = "lightgrey")) %>% 
  add_trace(x = 950:2000,#0:(nrow(d_new_scen)-1),
            y = round(d_new_scen$G_Y[950:2000], digits = 2),
            type = "scatter",
            mode = "lines",
            line = list(color = "black"),
            name = "G/Y") %>% 
  add_trace(x = 950:2000,#0:(nrow(d_new_scen)-1),
            y = round(d_new_scen$G_Yx[950:2000], digits = 2),
            type = "scatter",
            mode = "lines",
            line = list(color = "rgb(144,144,144)",
                        dash = "dash"),
            name = "G/Y<sub>x</sub>") %>%
  layout(xaxis = list(range = c(945, 2000),
                      dtick = 200,
                      tickfont = list(size = 7)),
         yaxis = list(range = c(0.15, 0.25),
                      showticklabels = FALSE,
                      tickfont = list(size = 7)),
         showlegend = TRUE,
         hovermode = "compare")

sub_plots_p_G <- subplot(style(p_G_before, showlegend = FALSE),
                         p_G_after,
                         nrows = 1, margin = 0.000, titleY = T, titleX = T) %>% 
  layout(annotations = G_Y)

# Net export share
p_NX_before <- plot_ly() %>% 
  add_trace(x = 0:(nrow(d_new_scen)-3),
            y = tail(d1$NX_Y, 1),
            type = "scatter",
            mode = "lines",
            name = "Baseline",
            line = list(color = "lightgrey")) %>% 
  add_trace(x = 1:950,#0:(nrow(d_new_scen)-1),
            y = d_new_scen$NX_Y[1:950],
            type = "scatter",
            mode = "lines",
            line = list(color = "black"),
            name = "Domestic economy") %>% # NX/Y 
  add_trace(x = 1:950,#0:(nrow(d_new_scen)-1),
            y = d_new_scen$NX_Yx[1:950],
            type = "scatter",
            mode = "lines",
            line = list(color = "rgb(144,144,144)",
                        dash = "dash"),
            name = "External economy") %>% # NX/Y<sub>x</sub>
  layout(xaxis = list(range = c(-10, 950),
                      tickfont = list(size = 7)),
         yaxis = list(range = c(-0.05, 0.05),
                      tickfont = list(size = 7),
                      dtick = 0.05),
         showlegend = TRUE,
         hovermode = "compare")

p_NX_after <- plot_ly() %>% 
  add_trace(x = 0:(nrow(d_new_scen)-3),
            y = tail(d1$NX_Y, 1),
            type = "scatter",
            mode = "lines",
            name = "Baseline",
            line = list(color = "lightgrey")) %>% 
  add_trace(x = 950:2000,#0:(nrow(d_new_scen)-1),
            y = d_new_scen$NX_Y[950:2000],
            type = "scatter",
            mode = "lines",
            line = list(color = "black"),
            name = "Domestic economy") %>% # NX/Y 
  add_trace(x = 950:2000,#0:(nrow(d_new_scen)-1),
            y = d_new_scen$NX_Yx[950:2000],
            type = "scatter",
            mode = "lines",
            line = list(color = "rgb(144,144,144)",
                        dash = "dash"),
            name = "External economy") %>% # NX/Y<sub>x</sub>
  layout(xaxis = list(range = c(945, 2000),
                      dtick = 200,
                      tickfont = list(size = 7)),
         yaxis = list(range = c(-0.05, 0.05),
                      tickfont = list(size = 7),
                      showticklabels = FALSE,
                      dtick = 0.05),
         showlegend = TRUE,
         hovermode = "compare")

sub_plots_p_NX <- subplot(style(p_NX_before, showlegend = FALSE),
                         p_NX_after,
                         nrows = 1, margin = 0.000, titleY = T, titleX = T) %>% 
  layout(annotations = NX_Y)

# NIIP share
p_NIIP_before <- plot_ly() %>% 
  add_trace(x = 0:(nrow(d_new_scen)-3),
            y = 0,
            type = "scatter",
            mode = "lines",
            name = "Baseline",
            line = list(color = "lightgrey")) %>% 
  add_trace(x = 1:950,#0:(nrow(d_new_scen)-1),
            y = d_new_scen$NIIP_Y[1:950],
            type = "scatter",
            mode = "lines",
            line = list(color = "black"),
            name = "Domestic economy") %>% 
  add_trace(x = 1:950,#0:(nrow(d_new_scen)-1),
            y = d_new_scen$NIIP_Yx[1:950],
            type = "scatter",
            mode = "lines",
            line = list(color = "rgb(144,144,144)",
                        dash = "dash"),
            name = "External economy") %>% 
  layout(xaxis = list(range = c(-10, 950),
                      tickfont = list(size = 7)),
         yaxis = list(range = c(-3, 3),
                      tickfont = list(size = 7),
                      dtick = 1),
         showlegend = TRUE,
         hovermode = "compare")

p_NIIP_after <- plot_ly() %>% 
  add_trace(x = 0:(nrow(d_new_scen)-3),
            y = 0,
            type = "scatter",
            mode = "lines",
            name = "Baseline",
            line = list(color = "lightgrey")) %>% 
  add_trace(x = 950:2000,#0:(nrow(d_new_scen)-1),
            y = d_new_scen$NIIP_Y[950:2000],
            type = "scatter",
            mode = "lines",
            line = list(color = "black"),
            name = "Domestic economy") %>% 
  add_trace(x = 950:2000,#0:(nrow(d_new_scen)-1),
            y = d_new_scen$NIIP_Yx[950:2000],
            type = "scatter",
            mode = "lines",
            line = list(color = "rgb(144,144,144)",
                        dash = "dash"),
            name = "External economy") %>% 
  layout(xaxis = list(range = c(945, 2000),
                      dtick = 200,
                      tickfont = list(size = 7)),
         yaxis = list(range = c(-3, 3),
                      showticklabels = FALSE,
                      tickfont = list(size = 7),
                      dtick = 1),
         showlegend = TRUE,
         hovermode = "compare")

sub_plots_p_NIIP <- subplot(style(p_NIIP_before, showlegend = FALSE),
                          p_NIIP_after,
                          nrows = 1, margin = 0.000, titleY = T, titleX = T) %>% 
  layout(annotations = NIIP_Y)

# Deficit
p_S_g_Y_before <- plot_ly() %>% 
  add_trace(x = 0:(nrow(d_new_scen)-3),
            y = 0,
            type = "scatter",
            mode = "lines",
            name = "Baseline",
            line = list(color = "lightgrey")) %>% 
  add_trace(x = 1:950,#0:(nrow(d_new_scen)-1),
            y = d_new_scen$S_g[1:950],
            type = "scatter",
            mode = "lines",
            line = list(color = "black"),
            name = "S_g / Y") %>% 
  add_trace(x = 1:950,#0:(nrow(d_new_scen)-1),
            y = d_new_scen$S_gx[1:950],
            type = "scatter",
            mode = "lines",
            line = list(color = "rgb(144,144,144)",
                        dash = "dash"),
            name = "S_g / Y") %>% 
  layout(xaxis = list(range = c(-10, 950),
                      tickfont = list(size = 7)),
         yaxis = list(range = c(-0.03, 0.03),
                      tickfont = list(size = 7)),
         showlegend = TRUE,
         hovermode = "compare")

p_S_g_Y_after <- plot_ly() %>% 
  add_trace(x = 0:(nrow(d_new_scen)-3),
            y = 0,
            type = "scatter",
            mode = "lines",
            name = "Baseline",
            line = list(color = "lightgrey")) %>% 
  add_trace(x = 950:2000,#0:(nrow(d_new_scen)-1),
            y = d_new_scen$S_g[950:2000],
            type = "scatter",
            mode = "lines",
            line = list(color = "black"),
            name = "S_g / Y") %>% 
  add_trace(x = 950:2000,#0:(nrow(d_new_scen)-1),
            y = d_new_scen$S_gx[950:2000],
            type = "scatter",
            mode = "lines",
            line = list(color = "rgb(144,144,144)",
                        dash = "dash"),
            name = "S_g / Y") %>% 
  layout(xaxis = list(range = c(945, 2000),
                      dtick = 200,
                      tickfont = list(size = 7)),
         yaxis = list(range = c(-0.03, 0.03),
                      showticklabels = FALSE,
                      tickfont = list(size = 7)),
         showlegend = TRUE,
         hovermode = "compare")

sub_plots_p_S_g_Y <- subplot(style(p_S_g_Y_before, showlegend = FALSE),
                             p_S_g_Y_after,
                            nrows = 1, margin = 0.000, titleY = T, titleX = T) %>% 
  layout(annotations = S_g_Y)

# Debt to income ratio
p_L_Yh2_before <- plot_ly() %>% 
  add_trace(x = 0:(nrow(d_new_scen)-1),
            y = 0.375,
            type = "scatter",
            mode = "lines",
            name = "Max leverage ratio",
            showlegend = TRUE,
            line = list(color = "lightgrey", 
                        dash = "dot")) %>% 
  add_trace(x = 1:950,#0:(nrow(d_new_scen)-1),
            y = d_new_scen$L_Yh2[1:950],
            type = "scatter",
            mode = "lines",
            showlegend = FALSE,
            line = list(color = "black"),
            name = "(L/Y)<sub>h2</sub>") %>% 
  add_trace(x = 0:(nrow(d_new_scen)-3),
            y = 0,
            type = "scatter",
            mode = "lines",
            name = "Baseline",
            showlegend = FALSE,
            line = list(color = "lightgrey")) %>% 
  layout(xaxis = list(range = c(-10, 950),
                      tickfont = list(size = 7)),
         yaxis = list(range = c(-0.01, 0.4), 
                      tickfont = list(size = 7)),
         showlegend = FALSE,
         hovermode = "compare")

p_L_Yh2_after <- plot_ly() %>% 
  add_trace(x = 0:(nrow(d_new_scen)-1),
            y = 0.375,
            type = "scatter",
            mode = "lines",
            name = "Max leverage ratio",
            showlegend = TRUE,
            line = list(color = "lightgrey", 
                        dash = "dot")) %>% 
  add_trace(x = 950:2000,#0:(nrow(d_new_scen)-1),
            y = d_new_scen$L_Yh2[950:2000],
            type = "scatter",
            mode = "lines",
            showlegend = FALSE,
            line = list(color = "black"),
            name = "(L/Y)<sub>h2</sub>") %>% 
  add_trace(x = 0:(nrow(d_new_scen)-3),
            y = 0,
            type = "scatter",
            mode = "lines",
            name = "Baseline",
            showlegend = FALSE,
            line = list(color = "lightgrey")) %>% 
  layout(xaxis = list(range = c(945, 2000),
                      dtick = 200,
                      tickfont = list(size = 7)),
         yaxis = list(range = c(-0.01, 0.4), 
                      showticklabels = FALSE,
                      tickfont = list(size = 7)),
         showlegend = FALSE,
         hovermode = "compare")

sub_plots_p_L_Yh2 <- subplot(style(p_L_Yh2_before, showlegend = FALSE),
                             p_L_Yh2_after,
                             nrows = 1, margin = 0.000, titleY = T, titleX = T) %>% 
  layout(annotations = L_Yh2)

sub_plots <- subplot(style(sub_plots_p_u, showlegend = FALSE),
                     style(sub_plots_p_C, showlegend = FALSE),
                     sub_plots_p_L_Yh2,
                     style(sub_plots_p_I, showlegend = FALSE),
                     style(sub_plots_p_S_g_Y, showlegend = FALSE),
                     style(sub_plots_p_G, showlegend = FALSE),
                     sub_plots_p_NIIP,
                     style(sub_plots_p_NX, showlegend = FALSE),
                     nrows = 4, 
                     margin = 0.04, 
                     titleY = T, 
                     titleX = T) %>%
                       layout(legend = list(x = 0.5,
                                            orientation = "h",
                                            xanchor = "center",
                                            font = list(size = 12))) %>%
                       config(displayModeBar = F)

sub_plots

# orca(
#  sub_plots,
#  file = "R/plots/figure_new_scenario.pdf"
# )
