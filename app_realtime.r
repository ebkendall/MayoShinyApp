## this is for illustration purposes with randomly chosen defaults
## Taken from https://github.com/danyaalmohamed/R/blob/master/animatedshiny.r

# ------------------------------------------------------------------------------
# The actual shiny application # -----------------------------------------------
# ------------------------------------------------------------------------------
load('www/clean_meds.rda')
load("www/data_format_FULL_48hr_update_RBC_sub.rda")
pace_id = c(18075, 108825, 110750, 125025, 173750, 260100, 304700, 307225, 310100,
            382450, 429375, 516150, 533075, 666750, 677225, 732525, 763050, 767500, 
            769025, 777175, 794900, 799125, 819225)
data_format = data_format[!(data_format[,'EID'] %in% pace_id), ]
focus_ind = which(data_format[,"EID"] == 61700)

rbc_times = as.numeric(data_format[focus_ind, 'RBC_ordered'] != 0)
rbc_admin_times = rep(0, length(focus_ind))
rbc_admin_times[which(diff(data_format[focus_ind, 'n_RBC_admin']) > 0) + 1] = 1
time = data_format[focus_ind, "time"]

load(paste0('www/mcmc_out_interm_5_4it5.rda'))
B_chain = mcmc_out_temp$B_chain[300:1000, focus_ind]
hr_chain = colMeans(mcmc_out_temp$hr_chain[300:1000, focus_ind])
bp_chain = colMeans(mcmc_out_temp$bp_chain[300:1000, focus_ind])
hc_chain = colMeans(mcmc_out_temp$hc_chain[300:1000, focus_ind])
la_chain = colMeans(mcmc_out_temp$la_chain[300:1000, focus_ind])
prob_chain = rbind( colMeans(B_chain == 1),
                    colMeans(B_chain == 2),
                    colMeans(B_chain == 3))

cumulative_post_prob = NULL
for(w in 1:length(focus_ind)) {
	start_index = focus_ind[1]
	end_index = focus_ind[w]
	if(w - 24 > 0) start_index = focus_ind[w - 12]

	y_or_n_2 = apply(mcmc_out_temp$B_chain[300:1000, start_index:end_index, drop = F],
					 1, function(x) (2 %in% x))
	prob_2 = mean(y_or_n_2)

	cumulative_post_prob = c(cumulative_post_prob, prob_2)
}
rm(mcmc_out_temp)

hr_obs_imp = as.numeric(is.na(data_format[focus_ind,"hr"]))
bp_obs_imp = as.numeric(is.na(data_format[focus_ind,"map"]))
hc_obs_imp = as.numeric(is.na(data_format[focus_ind,"hemo"]))
la_obs_imp = as.numeric(is.na(data_format[focus_ind,"lactate"]))

library(shiny)
library(flexdashboard)

state_names = c("Stable", "Bleed", "Recovery")

ylim_hr   = c(min(hr_chain), max(hr_chain))
ylim_map  = c(min(bp_chain), max(bp_chain))
ylim_hemo = c(0, max(hc_chain))
ylim_lact = c(0, max(la_chain))
ylim_prob = c(0,1)

xlim_main = c(1,50)

ui<-fluidPage(
  # App title ----
  titlePanel("Real time updates"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      fluidRow(actionButton("play","Play")),
      fluidRow(actionButton("stop","Stop")),
      fluidRow(gaugeOutput("gauge")),
      fluidRow(verbatimTextOutput("text")),
      width = 2
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      fluidRow(
        splitLayout(cellWidths = c("50%", "50%"), plotOutput("hr"), plotOutput("map"))
      ),
      fluidRow(
        splitLayout(cellWidths = c("50%", "50%"), plotOutput("hemo"), plotOutput("prob"))
      ),
      fluidRow(
        splitLayout(cellWidths = c("50%", "50%"), plotOutput("lact"))
      )
      
    )
  )
  
)


server<-function(input,output){
  
  prob_vals = reactiveValues()
  prob_vals$prob_dist_indicator = 1
  prob_vals$prob_dist_change_time = 1
  prob_vals$post_bleed = 0
  
  waits <- reactiveValues()
  waits$hr_obs   = hr_chain[1]
  waits$map_obs  = bp_chain[1]
  waits$hemo_obs = hc_chain[1]
  waits$lact_obs = la_chain[1]
  
  waits$hr_impute   = hr_obs_imp[1]
  waits$map_impute  = bp_obs_imp[1]
  waits$hemo_impute = hc_obs_imp[1]
  waits$lact_impute = la_obs_imp[1]

  waits$prob_obs = matrix(0, nrow = 3, ncol = 50)
  
  waits$rbc_update = paste("Patient Chart:", '\n')
  waits$rbc_admin  = rbc_admin_times[1]
  waits$rbc_order  = rbc_times[1]
  waits$abline_order = NULL
  waits$abline_admin = NULL
  
  waits$bleed_gauge = cumulative_post_prob[1]
  
  waits$iter = 1
  
  forward<-function(){
    waits$hr_obs   = tail(c(waits$hr_obs,   hr_chain[waits$iter]),50)
    waits$map_obs  = tail(c(waits$map_obs,  bp_chain[waits$iter]),50)
    waits$hemo_obs = tail(c(waits$hemo_obs, hc_chain[waits$iter]),50)
    waits$lact_obs = tail(c(waits$lact_obs, la_chain[waits$iter]),50)
    
    waits$hr_impute   = tail(c(waits$hr_impute,   hr_obs_imp[waits$iter]),50)
    waits$map_impute  = tail(c(waits$map_impute,  bp_obs_imp[waits$iter]),50)
    waits$hemo_impute = tail(c(waits$hemo_impute, hc_obs_imp[waits$iter]),50)
    waits$lact_impute = tail(c(waits$lact_impute, la_obs_imp[waits$iter]),50)
    
    waits$rbc_order = rbc_times[waits$iter]
    waits$rbc_admin = rbc_admin_times[waits$iter]
    
    waits$bleed_gauge = cumulative_post_prob[waits$iter]
    
    if(waits$rbc_order == 1) {
      temp = paste0("RBC Ordered at ", time[waits$iter], " min")
      curr = waits$rbc_update
      curr = paste(curr, temp, sep = "\n")
      waits$rbc_update = curr
      
      waits$abline_order = c(waits$abline_order, waits$iter)
    }
    if(waits$rbc_admin == 1) {
      temp = paste0("RBC Admin.  at ", time[waits$iter], " min")
      curr = waits$rbc_update
      curr = paste(curr, temp, sep = "\n")
      waits$rbc_update = curr
      
      waits$abline_admin = c(waits$abline_admin, waits$iter)
    }
    
    if(waits$bleed_gauge > 0.85) {
      temp = paste0("ALERT: Possible bleed")
      curr = waits$rbc_update
      curr = paste(curr, temp, sep = "\n")
      waits$rbc_update = curr
    }
    
    if(waits$iter > 50) {
      update_mat = matrix(0, nrow = 3, ncol = 50)
      update_mat[, 1:49] = waits$prob_obs[,2:50]
      update_mat[,50] = prob_chain[,waits$iter]
      
      waits$prob_obs = update_mat
      
      if(!is.null(waits$abline_order)) {
        waits$abline_order = waits$abline_order - 1
      }
      if(!is.null(waits$abline_admin)) {
        waits$abline_admin = waits$abline_admin - 1
      }
      
      xlim_main = c(head(xlim_main, 1) + 1, tail(xlim_main, 1) + 1)
    } else {
      waits$prob_obs[,waits$iter] = prob_chain[,waits$iter]
    }
    
    waits$iter = waits$iter + 1
  }
  
  session<-reactiveValues()
  session$timer<-reactiveTimer(Inf)
  
  observeEvent(input$play,{
    session$timer<-reactiveTimer(600)
    observeEvent(session$timer(),{
      forward()
    })
  })
  
  
  observeEvent(input$stop,{
    session$timer<-reactiveTimer(Inf)
  })
  
  observeEvent(input$bleed,{
    prob_vals$prob_dist_indicator <- 2
  })
  
  output$hr <- renderPlot({
    par(bg='black', fg='green')
    plot( x=waits$hr_obs, type = 'l', ylim = ylim_hr, xlim = xlim_main,
          main=paste0('heart rate'), xlab='time', ylab=NA, xaxt='n', col.main='green',
          col.axis='green', pch=20, cex=1)
    grid( nx=20, NULL, col='white')
    if(!is.null(waits$abline_order)) {
      abline(v = waits$abline_order, col = 'red', lwd = 2)
    }
    if(!is.null(waits$abline_admin)) {
      abline(v = waits$abline_admin, col = 'yellow', lwd = 2)
    }
  })
  
  output$map <- renderPlot({
    par(bg='black', fg='green')
    plot( x=waits$map_obs, type = 'l', ylim = ylim_map, xlim = xlim_main,
          main=paste0('mean arterial pressure'), xlab='time', ylab=NA, xaxt='n', 
          col.main='green', col.axis='green', pch=20, cex=1)
    grid( nx=20, NULL, col='white')
    if(!is.null(waits$abline_order)) {
      abline(v = waits$abline_order, col = 'red', lwd = 2)
    }
    if(!is.null(waits$abline_admin)) {
      abline(v = waits$abline_admin, col = 'yellow', lwd = 2)
    }
  })
  
  output$hemo <- renderPlot({
    par(bg='black', fg='green')
    plot( x=waits$hemo_obs, type = 'l', ylim = ylim_hemo, xlim = xlim_main,
          main=paste0('hemoglobin concentration'), xlab='time', ylab=NA, xaxt='n', 
          col.main='green', col.axis='green', pch=20, cex=1)
    grid( nx=20, NULL, col='white')
    if(!is.null(waits$abline_order)) {
      abline(v = waits$abline_order, col = 'red', lwd = 2)
    }
    if(!is.null(waits$abline_admin)) {
      abline(v = waits$abline_admin, col = 'yellow', lwd = 2)
    }
  })
  
  output$lact <- renderPlot({
    par(bg='black', fg='green')
    plot( x=waits$lact_obs, type = 'l', ylim = ylim_lact, xlim = xlim_main,
          main=paste0('lactate levels'), xlab='time', ylab=NA, xaxt='n', 
          col.main='green', col.axis='green', pch=20, cex=1)
    grid( nx=20, NULL, col='white')
    if(!is.null(waits$abline_order)) {
      abline(v = waits$abline_order, col = 'red', lwd = 2)
    }
    if(!is.null(waits$abline_admin)) {
      abline(v = waits$abline_admin, col = 'yellow', lwd = 2)
    }
  })
  
  output$prob <- renderPlot({
    par(bg='black', fg='green')
    
    barplot( waits$prob_obs,
             col=c( 'dodgerblue', 'firebrick1', 'yellow2'),
             space=0, main = "posterior probability", ylim = ylim_prob,
             col.main='green',  col.axis='green', border=NA)
    grid( nx=20, NULL, col='white')
  })
  
  output$gauge = renderGauge({
    gauge(waits$bleed_gauge, 
          min = 0, 
          max = 1, 
          sectors = gaugeSectors(success = c(0, 0.6), 
                                 warning = c(0.6, 0.8),
                                 danger = c(0.8, 1)))
  })
  
  output$text <- renderText({
    waits$rbc_update
  })
  
}

# runApp(shinyApp(ui,server),launch.browser = TRUE)
shinyApp(ui = ui, server = server)