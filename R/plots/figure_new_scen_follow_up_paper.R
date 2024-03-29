library(plotly)

# Load the data
load(here::here("R/simulated_series_for_plots_follow_up_paper"))

# Annotations for subplots
u <- list(
  text = "u",
  font = list(size = 14),
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 0.95,
  showarrow = FALSE)

gr_Y <- list(
  text = "Y&#770;",
  font = list(size = 14),
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 0.95,
  showarrow = FALSE)

C_Y <- list(
  text = "C/Y",
  font = list(size = 14),
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 0.95,
  showarrow = FALSE)

I_Y <- list(
  text = "I/Y",
  font = list(size = 14),
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 0.95,
  showarrow = FALSE)

G_Y <- list(
  text = "G/Y",
  font = list(size = 14),
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 0.95,
  showarrow = FALSE)

NX_Y <- list(
  text = "NX/Y",
  font = list(size = 14),
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 0.95,
  showarrow = FALSE)

S_g_Y <- list(
  text = "S<sub>g</sub>/Y",
  font = list(size = 14),
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 0.95,
  showarrow = FALSE)

L_Yh2 <- list(
  text = "L<sub>h2</sub>/Y<sub>dh2</sub>",
  font = list(size = 14),
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 0.95,
  showarrow = FALSE)

NIIP_Y <- list(
  text = "NIIP/Y",
  font = list(size = 14),
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 0.95,
  showarrow = FALSE)

# Utilization rate
p_u <- plot_ly() %>% 
  add_trace(x = 0:(nrow(d_new_scen)-3),
            y = tail(d1$u, 1),
            type = "scatter",
            mode = "lines",
            name = "u<sub>baseline</sub>",
            line = list(color = "grey")) %>% 
  add_trace(x = 0:(nrow(d_new_scen)-1),
            y = d_new_scen$u,
            type = "scatter",
            mode = "lines",
            line = list(color = "black"),
            name = "u") %>% 
  add_trace(x = 0:(nrow(d_new_scen)-1),
            y = d_new_scen$ux,
            type = "scatter",
            mode = "lines",
            line = list(color = "red",
                        dash = "dash"),
            name = "u<sub>x</sub>") %>%
  layout(xaxis = list(range = c(-10, 2000),
                      tickfont = list(size = 8)),
         yaxis = list(range = c(0.5, 0.8),
                      tickfont = list(size = 8),
                      dtick = 0.05),
         annotations = u,
         showlegend = TRUE,
         hovermode = "compare")

#p_u

# Growth rate of output
p_gr_Y <- plot_ly() %>% 
  # add_trace(x = 0:(nrow(d_new_scen)-3),
  #           y = tail(d_new_scen$gr_Y, 1),
  #           type = "scatter",
  #           mode = "lines",
  #           line = list(color = "black",
  #                       dash = "dot",
  #                       width = 1),
  #           hoverinfo = "none") %>% 
  add_trace(x = 0:(nrow(d_new_scen)-3),
            y = tail(d1$gr_Y, 1),
            type = "scatter",
            mode = "lines",
            line = list(color = "grey"),
            name = "gr<sub>Y&#770;<sub>baseline</sub></sub>") %>%
  add_trace(x = 0:(nrow(d_new_scen)-3),
            y = d_new_scen$gr_Y[3:nrow(d_new_scen)],
            type = "scatter",
            mode = "lines",
            line = list(color = "black"),
            name = "gr<sub>Y&#770;<sub>") %>% 
  add_trace(x = 0:(nrow(d_new_scen)-3),
            y = d_new_scen$gr_Yx[3:nrow(d_new_scen)],
            type = "scatter",
            mode = "lines",
            name = "gr<sub>Y&#770;<sub>x</sub></sub>",
            line = list(color = "red",
                        dash = "dash")) %>%
  layout(xaxis = list(range = c(-10, 2000),
                      tickfont = list(size = 8)),
         yaxis = list(range = c(0.014, 0.018),
                      tickfont = list(size = 8)),
         annotations = gr_Y,
         showlegend = TRUE,
         hovermode = "compare")

#p_gr_Y

# Consumption share
p_C <- plot_ly() %>% 
  add_trace(x = 0:(nrow(d_new_scen)-3),
            y = tail(d1$C_Y, 1),
            type = "scatter",
            mode = "lines",
            name = "Baseline",
            line = list(color = "grey")) %>% 
  add_trace(x = 0:(nrow(d_new_scen)-1),
            y = d_new_scen$C_Y,
            type = "scatter",
            mode = "lines",
            line = list(color = "black"),
            name = "C/Y") %>% 
  add_trace(x = 0:(nrow(d_new_scen)-1),
            y = d_new_scen$C_Yx,
            type = "scatter",
            mode = "lines",
            line = list(color = "red",
                        dash = "dash"),
            name = "C/Y<sub>x</sub>") %>%
  layout(xaxis = list(range = c(-10, 2000),
                      tickfont = list(size = 8)),
         yaxis = list(range = c(0.5, 0.7),
                      tickfont = list(size = 8)),
         annotations = C_Y,
         showlegend = TRUE,
         hovermode = "compare")
#p_C

# Investment share
p_I <- plot_ly() %>%
  # add_trace(x = 0:(nrow(d_new_scen)-3),
  #           y = tail(d_new_scen$I_Y, 1),
  #           type = "scatter",
  #           mode = "lines",
  #           hoverinfo = "none",
  #           line = list(color = "black",
  #                       dash = "dot",
  #                       width = 1)) %>% 
  add_trace(x = 0:(nrow(d_new_scen)-3),
            y = tail(d1$I_Y, 1),
            type = "scatter",
            mode = "lines",
            name = "I/Y<sub>baseline</sub>",
            line = list(color = "grey")) %>% 
  add_trace(x = 0:(nrow(d_new_scen)-1),
            y = d_new_scen$I_Y,
            type = "scatter",
            mode = "lines",
            line = list(color = "black"),
            name = "I/Y") %>% 
  add_trace(x = 0:(nrow(d_new_scen)-1),
            y = d_new_scen$I_Yx,
            type = "scatter",
            mode = "lines",
            line = list(color = "red",
                        dash = "dash"),
            name = "I/Y<sub>x</sub>") %>%
  layout(xaxis = list(range = c(-10, 2000),
                      tickfont = list(size = 8)),
         yaxis = list(range = c(0.17, 0.22),
                      tickfont = list(size = 8)),
         annotations = I_Y,
         showlegend = TRUE,
         hovermode = "compare")

#p_I

# Government share
p_G <- plot_ly() %>% 
  add_trace(x = 0:(nrow(d_new_scen)-1),
            y = round(d_new_scen$G_Y, digits = 2),
            type = "scatter",
            mode = "lines",
            line = list(color = "black"),
            name = "G/Y") %>% 
  add_trace(x = 0:(nrow(d_new_scen)-1),
            y = round(d_new_scen$G_Yx, digits = 2),
            type = "scatter",
            mode = "lines",
            line = list(color = "red",
                        dash = "dash"),
            name = "G/Y<sub>x</sub>") %>%
  layout(xaxis = list(range = c(-10, 2000),
                      tickfont = list(size = 8)),
         yaxis = list(range = c(0, 0.3), 
                      tickfont = list(size = 8)),
         annotations = G_Y,
         showlegend = TRUE,
         hovermode = "compare")

#p_G

# Net export share
p_NX <- plot_ly() %>% 
  add_trace(x = 0:(nrow(d_new_scen)-3),
            y = tail(d1$NX_Y, 1),
            type = "scatter",
            mode = "lines",
            name = "Baseline",
            line = list(color = "grey")) %>% 
  add_trace(x = 0:(nrow(d_new_scen)-1),
            y = d_new_scen$NX_Y,
            type = "scatter",
            mode = "lines",
            line = list(color = "black"),
            name = "Domestic economy") %>% # NX/Y 
  add_trace(x = 0:(nrow(d_new_scen)-1),
            y = d_new_scen$NX_Yx,
            type = "scatter",
            mode = "lines",
            line = list(color = "red",
                        dash = "dash"),
            name = "External economy") %>% # NX/Y<sub>x</sub>
  layout(xaxis = list(range = c(-10, 2000),
                      tickfont = list(size = 8)),
         yaxis = list(range = c(-0.12, 0.12),
                      tickfont = list(size = 8),
                      dtick = 0.05),
         annotations = NX_Y,
         showlegend = TRUE,
         hovermode = "compare")

#p_NX

# NIIP share
p_NIIP <- plot_ly() %>% 
  add_trace(x = 0:(nrow(d_new_scen)-3),
            y = 0,
            type = "scatter",
            mode = "lines",
            name = "Baseline",
            line = list(color = "grey")) %>% 
  add_trace(x = 0:(nrow(d_new_scen)-1),
            y = d_new_scen$NIIP_Y,
            type = "scatter",
            mode = "lines",
            line = list(color = "black"),
            name = "Domestic economy") %>% 
  add_trace(x = 0:(nrow(d_new_scen)-1),
            y = d_new_scen$NIIP_Yx,
            type = "scatter",
            mode = "lines",
            line = list(color = "red",
                        dash = "dash"),
            name = "External economy") %>% 
  layout(xaxis = list(range = c(-10, 2000),
                      tickfont = list(size = 8)),
         yaxis = list(range = c(-6, 6),
                      tickfont = list(size = 8),
                      dtick = 1),
         annotations = NIIP_Y,
         showlegend = TRUE,
         hovermode = "compare")

# Deficit
p_S_g_Y <- plot_ly() %>% 
  add_trace(x = 0:(nrow(d_new_scen)-3),
            y = 0,
            type = "scatter",
            mode = "lines",
            name = "Baseline",
            line = list(color = "grey")) %>% 
  add_trace(x = 0:(nrow(d_new_scen)-1),
            y = d_new_scen$S_g,
            type = "scatter",
            mode = "lines",
            line = list(color = "black"),
            name = "S_g / Y") %>% 
  add_trace(x = 0:(nrow(d_new_scen)-1),
            y = d_new_scen$S_gx,
            type = "scatter",
            mode = "lines",
            line = list(color = "red",
                        dash = "dash"),
            name = "S_g / Y") %>% 
  layout(xaxis = list(range = c(-10, 2000),
                      tickfont = list(size = 8)),
         yaxis = list(range = c(-0.1, 0.02), 
                      tickfont = list(size = 8)),
         annotations = S_g_Y,
         showlegend = TRUE,
         hovermode = "compare")

# Debt to income ratio
p_L_Yh2 <- plot_ly() %>% 
  add_trace(x = 0:(nrow(d_new_scen)-1),
            y = 0.375,
            type = "scatter",
            mode = "lines",
            name = "Max leverage ratio",
            showlegend = TRUE,
            line = list(color = "grey", 
                        dash = "dot")) %>% 
  add_trace(x = 0:(nrow(d_new_scen)-1),
            y = d_new_scen$L_Yh2,
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
            line = list(color = "grey")) %>% 
  layout(xaxis = list(range = c(-10, 2000),
                      tickfont = list(size = 8)),
         yaxis = list(range = c(-0.01, 0.4), 
                      tickfont = list(size = 8)),
         annotations = L_Yh2,
         showlegend = FALSE,
         hovermode = "compare")

sub_plots <- subplot(style(p_u, showlegend = FALSE),
                     p_L_Yh2,
                     style(p_S_g_Y, showlegend = FALSE),
                     p_NIIP,
                     style(p_C, showlegend = FALSE),
                     style(p_I, showlegend = FALSE),
                     style(p_G, showlegend = FALSE),
                     style(p_NX, showlegend = FALSE),
                     nrows = 2, margin = 0.045, titleY = T, titleX = T) %>%
  layout(legend = list(x = 0.5,
                       orientation = "h",
                       xanchor = "center",
                       font = list(size = 12))) %>% 
  config(displayModeBar = F)

sub_plots

# orca(
#   sub_plots,
#   file = "R/plots/figure5.pdf"
# )
