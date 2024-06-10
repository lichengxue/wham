#' Plot performance metrics for one realization using different management strategies
#' 
#' a function to compare performance metrics of different assessment models 
#' 
#' @param mods A list of models 
#' @param main.dir Path to save the final report
#' @param use.n.years Number of years that is used to summarize statistics   
#' @param base.mod Base model used to calculate relative difference in model performance (default = 4)
#' @param do.short.term A list of information used to summarize performance over the fist n years
#'   \describe{
#'     \item{\code{$first.n.years}}{integer, number of first years to include in results. Default = 3.}
#'     \item{\code{$year_start}}{integer, first year of the burn-in period. Default = 1973.}
#'     \item{\code{$year_end}}{integer, last year of the burn-in period. Default = 2022.}
#'   }
#' @param dpi Resolution (number of dots per inches)
#' 
#' @return a report
#'   
#' @export
#'
plot_mse_output <- function(mods, main.dir = getwd(), 
                            use.n.years = 10, 
                            base.mod = 4, 
                            do.short.term = list(first.n.years = 3, year_start = 1973, year_end = 2022), 
                            dpi = 150) {
  
  require(ggplot2)
  require(dplyr)
  require(tidyr)
  require(fmsb)
  require(ggpubr)
  
  sub.dir <- 'Report'
  if (file.exists(sub.dir)){
  } else {
    dir.create(file.path(main.dir, sub.dir))
  }
  
  # Compare models for 1 realization
  if(!is.list(mods[[1]][[1]][[1]])){ 
    tmp1 = mods[[1]][[1]]; tmp2 = mods[[2]][[1]]
    if (sum(tmp1$input$data$agg_catch[1:3]-tmp2$input$data$agg_catch[1:3]) != 0) {
      warning("Results are not comparable because the realization is different.\nPlease check the seed!")
    } else {
      # ------------------------------------------------------------
      # ----------------------one realization-----------------------
      # ------------------------------------------------------------
      cat("Results are a model comparison for one realization!\n")
      
      # ------------------------------------------------------------
      # ------------------------------------------------------------
      # ------------------------- Line Plot ------------------------
      # ----------------- SSB, F, Catch_r, Catch_s -----------------
      # ------------------------------------------------------------
      # ------------------------------------------------------------
      
      Years = mods[[1]]$om$years
      
      if(is.null(use.n.years)) use.n.years = 10
      
      extract_var <- function(mod, var){
        if (var != "Catch_r") {
          data <- data.frame(mod$om$rep[var])
          data[paste0(var,"_total")] <- rowSums(data)
          if (var == "Catch_s"){
            var = "pred_catch"
            data <- data.frame(mod$om$rep[var])
            data[paste0(var,"_total")] <- rowSums(data)
            names <- gsub("pred_catch","Catch_s",colnames(data))
            colnames(data) <- names
          }
        } else {
          var = "pred_stock_catch"
          data <- mod$om$rep$pred_stock_catch
          Catch_list = list()
          for (i in 1:nrow(data)) {
            Catch_list[[paste0("Catch_r.",i)]] <- apply(data, MARGIN = 3, FUN = colSums)[i,]
          }
          Catch_r.total = apply(data, MARGIN = 3, FUN = sum)
          data <- data.frame(Catch_list,Catch_r.total)
        }
        return(data)
      }
      
      var = "SSB"
      res = NULL
      for (i in 1:length(mods)){
        tmp <- extract_var(mods[[i]],var)
        tmp$Model <- paste("Model",i)
        tmp$Year <- Years
        res <- rbind(res, tmp)
      }
      res <- pivot_longer(res,cols = starts_with(var),names_to = "Label", values_to = var)
      
      p1 <- ggplot(res, aes(x = Year, y=!! rlang::sym(var),col = Model)) +
        geom_line(size = 0.8, alpha = 0.8) +
        facet_grid(Label ~., scales = "free") + 
        scale_colour_brewer(palette = "Set2") + 
        ggtitle("SSB") +
        ylab("SSB") +
        theme_bw()
      ggsave(file.path(main.dir,sub.dir,paste0(var,".PNG")), p1, width = 10, height = 7, dpi = dpi)
      
      var = "Fbar"
      res = NULL
      for (i in 1:length(mods)){
        tmp <- extract_var(mods[[i]],var)
        tmp$Model <- paste("Model",i)
        tmp$Year <- Years
        res <- rbind(res, tmp)
      }
      res <- pivot_longer(res,cols = starts_with(var),names_to = "Label", values_to = var)
      
      p2 <- ggplot(res, aes(x = Year, y=!! rlang::sym(var),col = Model)) +
        geom_line(size = 0.8, alpha = 0.8) +
        facet_grid(Label ~., scales = "free") + 
        scale_colour_brewer(palette = "Set2") + 
        ggtitle("Fleet-specific F") +
        ylab("F") +
        theme_bw()
      ggsave(file.path(main.dir,sub.dir,paste0(var,".PNG")), p2, width = 10, height = 7, dpi = dpi)
      
      var = "Catch_s"
      res = NULL
      for (i in 1:length(mods)){
        tmp <- extract_var(mods[[i]],var)
        tmp$Model <- paste("Model",i)
        tmp$Year <- Years
        res <- rbind(res, tmp)
      }
      res <- pivot_longer(res,cols = starts_with(var),names_to = "Label", values_to = var)
      
      p3 <- ggplot(res, aes(x = Year, y=!! rlang::sym(var),col = Model)) +
        geom_line(size = 0.8, alpha = 0.8) +
        facet_grid(Label ~., scales = "free") + 
        scale_colour_brewer(palette = "Set2") + 
        ggtitle("Stock-specific Catch") +
        ylab("Catch") +
        theme_bw()
      ggsave(file.path(main.dir,sub.dir,paste0(var,".PNG")), p3, width = 10, height = 7, dpi = dpi)
      
      var = "Catch_r"
      res = NULL
      for (i in 1:length(mods)){
        tmp <- extract_var(mods[[i]],var)
        tmp$Model <- paste("Model",i)
        tmp$Year <- Years
        res <- rbind(res, tmp)
      }
      res <- pivot_longer(res,cols = starts_with(var),names_to = "Label", values_to = var)
      
      p4 <- ggplot(res, aes(x = Year, y=!! rlang::sym(var),col = Model)) +
        geom_line(size = 0.8, alpha = 0.8) +
        facet_grid(Label ~., scales = "free") + 
        scale_colour_brewer(palette = "Set2") + 
        ggtitle("Region-specific Catch") +
        ylab("Catch") +
        theme_bw()
      ggsave(file.path(main.dir,sub.dir,paste0(var,".PNG")), p4, width = 10, height = 7, dpi = dpi)
      
      # Plot them together
      p <- ggarrange(p1,p2,p3,p4,common.legend = TRUE,legend = "right")
      ggsave(file.path(main.dir,sub.dir,"Performance_Metrics.PNG"), p, width = 15, height = 10, dpi = dpi)
      
      
      # ------------------------------------------------------------
      # ------------------------------------------------------------
      # ------------------------- Box Plot ------------------------
      # ----------------- SSB, F, Catch_r, Catch_s -----------------
      # ------------------------------------------------------------
      # ------------------------------------------------------------
      
      # Performance metrics summarized over the last n years
      var = "SSB"
      res = NULL
      for (i in 1:length(mods)){
        tmp <- extract_var(mods[[i]],var)
        if (is.null(use.n.years)) {
          tmp$Model <- paste("Model",i)
          tmp$Year <- Years
          tmp <- tail(tmp,5)
          res <- rbind(res, tmp) 
        } else {
          tmp$Model <- paste("Model",i)
          tmp$Year <- Years
          tmp <- tail(tmp,use.n.years)
          res <- rbind(res, tmp) 
        }
      }
      
      res <- pivot_longer(res,cols = starts_with(var),names_to = "Label", values_to = var)
      
      p1 <- ggplot(res, aes(x = Model, y=!! rlang::sym(var),col = Model)) +
        geom_boxplot(outlier.shape = NA) +
        facet_grid(Label ~., scales = "free") + 
        scale_colour_brewer(palette = "Set2") + 
        ggtitle(paste0("SSB"," (over last ",use.n.years," years)")) +
        ylab("SSB") +
        coord_cartesian(ylim = quantile(res[[var]],c(0.05,0.95))) + 
        theme_bw()
      ggsave(file.path(main.dir,sub.dir,paste0(var,"_last_",use.n.years,"years.PNG")), p1, width = 10, height = 7, dpi = dpi)
      
      var = "Fbar"
      res = NULL
      for (i in 1:length(mods)){
        tmp <- extract_var(mods[[i]],var)
        if (is.null(use.n.years)) {
          tmp$Model <- paste("Model",i)
          tmp$Year <- Years
          tmp <- tail(tmp,5)
          res <- rbind(res, tmp) 
        } else {
          tmp$Model <- paste("Model",i)
          tmp$Year <- Years
          tmp <- tail(tmp,use.n.years)
          res <- rbind(res, tmp) 
        }
      }
      
      res <- pivot_longer(res,cols = starts_with(var),names_to = "Label", values_to = var)
      
      p2 <- ggplot(res, aes(x = Model, y=!! rlang::sym(var),col = Model)) +
        geom_boxplot(outlier.shape = NA) +
        facet_grid(Label ~., scales = "free") + 
        scale_colour_brewer(palette = "Set2") + 
        ggtitle(paste0("Fleet-specific F"," (over last ",use.n.years," years)")) +
        ylab("F") +
        coord_cartesian(ylim = quantile(res[[var]],c(0.05,0.95))) + 
        theme_bw()
      ggsave(file.path(main.dir,sub.dir,paste0(var,"_last_",use.n.years,"years.PNG")), p2, width = 10, height = 7, dpi = dpi)
      
      var = "Catch_s"
      res = NULL
      for (i in 1:length(mods)){
        tmp <- extract_var(mods[[i]],var)
        if (is.null(use.n.years)) {
          tmp$Model <- paste("Model",i)
          tmp$Year <- Years
          tmp <- tail(tmp,5)
          res <- rbind(res, tmp) 
        } else {
          tmp$Model <- paste("Model",i)
          tmp$Year <- Years
          tmp <- tail(tmp,use.n.years)
          res <- rbind(res, tmp) 
        }
      }
      
      res <- pivot_longer(res,cols = starts_with(var),names_to = "Label", values_to = var)
      
      p3 <- ggplot(res, aes(x = Model, y=!! rlang::sym(var),col = Model)) +
        geom_boxplot(outlier.shape = NA) +
        facet_grid(Label ~., scales = "free") + 
        scale_colour_brewer(palette = "Set2") + 
        ggtitle(paste0("Stock-specific Catch"," (over last ",use.n.years," years)")) +
        ylab("Catch") +
        coord_cartesian(ylim = quantile(res[[var]],c(0.05,0.95))) + 
        theme_bw()
      ggsave(file.path(main.dir,sub.dir,paste0(var,"_last_",use.n.years,"years.PNG")), p3, width = 10, height = 7, dpi = dpi)
      
      var = "Catch_r"
      res = NULL
      for (i in 1:length(mods)){
        tmp <- extract_var(mods[[i]],var)
        if (is.null(use.n.years)) {
          tmp$Model <- paste("Model",i)
          tmp$Year <- Years
          tmp <- tail(tmp,5)
          res <- rbind(res, tmp) 
        } else {
          tmp$Model <- paste("Model",i)
          tmp$Year <- Years
          tmp <- tail(tmp,use.n.years)
          res <- rbind(res, tmp) 
        }
      }
      res <- pivot_longer(res,cols = starts_with(var),names_to = "Label", values_to = var)
      
      p4 <- ggplot(res, aes(x = Model, y=!! rlang::sym(var),col = Model)) +
        geom_boxplot(outlier.shape = NA) +
        facet_grid(Label ~., scales = "free") + 
        scale_colour_brewer(palette = "Set2") + 
        ggtitle(paste0("Region-specific Catch"," (over last ",use.n.years," years)")) +
        ylab("Catch") +
        coord_cartesian(ylim = quantile(res[[var]],c(0.05,0.95))) + 
        theme_bw()
      
      ggsave(file.path(main.dir,sub.dir,paste0(var,"_last_",use.n.years,"years.PNG")), p4, width = 10, height = 7, dpi = dpi)
      
      # Plot them together
      p <- ggarrange(p1,p2,p3,p4,common.legend = TRUE,legend = "right")
      ggsave(file.path(main.dir,sub.dir,paste0("Performance_last_",use.n.years,"years.PNG")), p, width = 15, height = 10, dpi = dpi)
    }
  }
  
  # ------------------------------------------------------------
  # ----------------------one realization-----------------------
  # ------------------------------------------------------------
  
  # Compare models over n realizations
  if (is.list(mods[[1]][[1]][[1]])) { 
    
    tmp1 = mods[[1]][[1]][[1]]; tmp2 = mods[[1]][[2]][[1]]
    
    if (sum(tmp1$input$data$agg_catch[1:3]-tmp2$input$data$agg_catch[1:3]) != 0) {
      warning("Results are not comparable because the realization is different.\nPlease check the seed!")
    } else {
      cat("Results are model comparisons for n realizations!\n")
      
      # ------------------------------------------------------------
      # ------------------------------------------------------------
      # ------------------------- Box Plot -------------------------
      # ----------------- SSB, F, Catch_r, Catch_s -----------------
      # ------------------------------------------------------------
      # ------------------------------------------------------------
      
      Years = mods[[1]][[1]]$om$years
      
      if(is.null(use.n.years)) use.n.years = 10
      
      extract_var <- function(mod, var){
        if (var != "Catch_r") {
          data <- data.frame(mod$om$rep[var])
          data[paste0(var,"_total")] <- rowSums(data)
          if (var == "Catch_s"){
            var = "pred_catch"
            data <- data.frame(mod$om$rep[var])
            data[paste0(var,"_total")] <- rowSums(data)
            names <- gsub("pred_catch","Catch_s",colnames(data))
            colnames(data) <- names
          }
        } else {
          var = "pred_stock_catch"
          data <- mod$om$rep$pred_stock_catch
          Catch_list = list()
          for (i in 1:nrow(data)) {
            Catch_list[[paste0("Catch_r.",i)]] <- apply(data, MARGIN = 3, FUN = colSums)[i,]
          }
          Catch_r.total = apply(data, MARGIN = 3, FUN = sum)
          data <- data.frame(Catch_list,Catch_r.total)
        }
        return(data)
      }
      
      var = "SSB"
      res = NULL
      
      for (i in 1:length(mods)){
        for (j in 1:length(mods[[1]])){
          tmp <- extract_var(mods[[i]][[j]],var)
          if (is.null(use.n.years)) {
            tmp$nsim <- i
            tmp$Model <- paste("Model",j)
            tmp$Year <- Years
            tmp <- tail(tmp,5)
            res <- rbind(res, tmp)
          } else {
            tmp$nsim <- i
            tmp$Model <- paste("Model",j)
            tmp$Year <- Years
            tmp <- tail(tmp,use.n.years)
            res <- rbind(res, tmp)
          }
        }
      }
      res <- pivot_longer(res,cols = starts_with(var),names_to = "Label", values_to = var)
      
      p1 <- ggplot(res, aes(x = Model, y=!! rlang::sym(var),col = Model)) +
        geom_boxplot(outlier.shape = NA) +
        facet_grid(Label ~., scales = "free") + 
        scale_colour_brewer(palette = "Set2") + 
        ggtitle(paste0("SSB"," (over last ",use.n.years," years)")) +
        ylab("SSB") +
        coord_cartesian(ylim = quantile(res[[var]],c(0.05,0.95))) + 
        theme_bw()
      ggsave(file.path(main.dir,sub.dir,paste0(var,"_last_",use.n.years,"years.PNG")), p1, width = 10, height = 7, dpi = dpi)
      
      var = "Fbar"
      res = NULL
      for (i in 1:length(mods)){
        for (j in 1:length(mods[[1]])){
          tmp <- extract_var(mods[[i]][[j]],var)
          if (is.null(use.n.years)) {
            tmp$nsim <- i
            tmp$Model <- paste("Model",j)
            tmp$Year <- Years
            tmp <- tail(tmp,5)
            res <- rbind(res, tmp)
          } else {
            tmp$nsim <- i
            tmp$Model <- paste("Model",j)
            tmp$Year <- Years
            tmp <- tail(tmp,use.n.years)
            res <- rbind(res, tmp)
          }
        }
      }
      res <- pivot_longer(res,cols = starts_with(var),names_to = "Label", values_to = var)
      
      p2 <- ggplot(res, aes(x = Model, y=!! rlang::sym(var),col = Model)) +
        geom_boxplot(outlier.shape = NA) +
        facet_grid(Label ~., scales = "free") + 
        scale_colour_brewer(palette = "Set2") + 
        ggtitle(paste0("Fleet-specific F"," (over last ",use.n.years," years)")) +
        ylab("F") +
        coord_cartesian(ylim = quantile(res[[var]],c(0.05,0.95))) + 
        theme_bw()
      ggsave(file.path(main.dir,sub.dir,paste0(var,"_last_",use.n.years,"years.PNG")), p2, width = 10, height = 7, dpi = dpi)
      
      var = "Catch_s"
      res = NULL
      for (i in 1:length(mods)){
        for (j in 1:length(mods[[1]])){
          tmp <- extract_var(mods[[i]][[j]],var)
          if (is.null(use.n.years)) {
            tmp$nsim <- i
            tmp$Model <- paste("Model",j)
            tmp$Year <- Years
            tmp <- tail(tmp,5)
            res <- rbind(res, tmp)
          } else {
            tmp$nsim <- i
            tmp$Model <- paste("Model",j)
            tmp$Year <- Years
            tmp <- tail(tmp,use.n.years)
            res <- rbind(res, tmp)
          }
        }
      }
      res <- pivot_longer(res,cols = starts_with(var),names_to = "Label", values_to = var)
      
      p3 <- ggplot(res, aes(x = Model, y=!! rlang::sym(var),col = Model)) +
        geom_boxplot(outlier.shape = NA) +
        facet_grid(Label ~., scales = "free") + 
        scale_colour_brewer(palette = "Set2") + 
        ggtitle(paste0("Stock-specific Catch"," (over last ",use.n.years," years)")) +
        ylab("Catch") +
        coord_cartesian(ylim = quantile(res[[var]],c(0.05,0.95))) + 
        theme_bw()
      ggsave(file.path(main.dir,sub.dir,paste0(var,"_last_",use.n.years,"years.PNG")), p3, width = 10, height = 7, dpi = dpi)
      
      var = "Catch_r"
      res = NULL
      for (i in 1:length(mods)){
        for (j in 1:length(mods[[1]])){
          tmp <- extract_var(mods[[i]][[j]],var)
          if (is.null(use.n.years)) {
            tmp$nsim <- i
            tmp$Model <- paste("Model",j)
            tmp$Year <- Years
            tmp <- tail(tmp,5)
            res <- rbind(res, tmp)
          } else {
            tmp$nsim <- i
            tmp$Model <- paste("Model",j)
            tmp$Year <- Years
            tmp <- tail(tmp,use.n.years)
            res <- rbind(res, tmp)
          }
        }
      }
      res <- pivot_longer(res,cols = starts_with(var),names_to = "Label", values_to = var)
      
      p4 <- ggplot(res, aes(x = Model, y=!! rlang::sym(var),col = Model)) +
        geom_boxplot(outlier.shape = NA) +
        facet_grid(Label ~., scales = "free") + 
        scale_colour_brewer(palette = "Set2") + 
        ggtitle(paste0("Region-specific Catch"," (over last ",use.n.years," years)")) +
        ylab("Catch") +
        coord_cartesian(ylim = quantile(res[[var]],c(0.05,0.95))) + 
        theme_bw()
      
      ggsave(file.path(main.dir,sub.dir,paste0(var,"_last_",use.n.years,"years.PNG")), p4, width = 10, height = 7, dpi = dpi)
      
      p <- ggarrange(p1,p2,p3,p4,common.legend = TRUE,legend = "right")
      ggsave(file.path(main.dir,sub.dir,paste0("Performance_last_",use.n.years,"years.PNG")), p, width = 15, height = 10, dpi = dpi)
      
      var = "Catch_s"
      res = NULL
      for (i in 1:length(mods)){
        for (j in 1:length(mods[[1]])){
          tmp <- extract_var(mods[[i]][[j]],var)
          if (is.null(use.n.years)) {
            tmp <- tail(tmp,5)
          } else {
            tmp <- tail(tmp,use.n.years)
          }
          names <- names(tmp)
          tmp <- colSums(tmp)
          tmp <- data.frame(t(tmp))
          tmp$nsim <- i
          tmp$Model <- paste("Model",j)
          res <- rbind(res, tmp)
        }
      }
      res <- pivot_longer(res,cols = starts_with(var),names_to = "Label", values_to = var)
      p <- ggplot(res, aes(x = Model, y=!! rlang::sym(var),col = Model)) +
        geom_boxplot(outlier.shape = NA) +
        facet_grid(Label ~., scales = "free") + 
        scale_colour_brewer(palette = "Set2") + 
        ggtitle(paste0("Stock-specific Total Catch"," (over last ",use.n.years," years)")) +
        ylab("Catch") +
        coord_cartesian(ylim = quantile(res[[var]],c(0.05,0.95))) + 
        theme_bw()
      ggsave(file.path(main.dir,sub.dir,paste0("Total_",var,"_last_",use.n.years,"years.PNG")), p, width = 10, height = 7, dpi = dpi)
      
      var = "Catch_r"
      res = NULL
      for (i in 1:length(mods)){
        for (j in 1:length(mods[[1]])){
          tmp <- extract_var(mods[[i]][[j]],var)
          if (is.null(use.n.years)) {
            tmp <- tail(tmp,5)
          } else {
            tmp <- tail(tmp,use.n.years)
          }
          names <- names(tmp)
          tmp <- colSums(tmp)
          tmp <- data.frame(t(tmp))
          tmp$nsim <- i
          tmp$Model <- paste("Model",j)
          res <- rbind(res, tmp)
        }
      }
      res <- pivot_longer(res,cols = starts_with(var),names_to = "Label", values_to = var)
      p <- ggplot(res, aes(x = Model, y=!! rlang::sym(var),col = Model)) +
        geom_boxplot(outlier.shape = NA) +
        facet_grid(Label ~., scales = "free") + 
        scale_colour_brewer(palette = "Set2") + 
        ggtitle(paste0("Region-specific Total Catch"," (over last ",use.n.years," years)")) +
        ylab("Catch") +
        coord_cartesian(ylim = quantile(res[[var]],c(0.05,0.95))) + 
        theme_bw()
      
      ggsave(file.path(main.dir,sub.dir,paste0("Total_",var,"_last_",use.n.years,"years.PNG")), p, width = 10, height = 7, dpi = dpi)
      
      # ------------------------------------------------------------
      # ------------------------------------------------------------
      # ------------------------- Box Plot -------------------------
      # ----------------------- Stock Status -----------------------
      # ------------------------------------------------------------
      
      # Reference points and stock status (SSB)
      res = NULL
      for (i in 1:length(mods)){
        for (j in 1:length(mods[[1]])){
          tmp <- mods[[i]][[j]]$om$rep$SSB
          tmp <- cbind(tmp,rowSums(tmp))
          tmp <- tmp/exp(mods[[i]][[j]]$om$rep$log_SSB_FXSPR)
          tmp <- as.data.frame(tmp)
          names(tmp) <- paste0("SSB/SSB40%.s",1:ncol(tmp))
          names(tmp)[ncol(tmp)] <- "SSB/SSB40%_Global"
          if (is.null(use.n.years)) {
            tmp$nsim <- i
            tmp$Model <- paste("Model",j)
            tmp$Year <- Years
            tmp <- tail(tmp,5)
            res <- rbind(res, tmp)
          } else {
            tmp$nsim <- i
            tmp$Model <- paste("Model",j)
            tmp$Year <- Years
            tmp <- tail(tmp,use.n.years)
            res <- rbind(res, tmp)
          }
        }
      }
      var = "SSB/SSB40%"
      res <- pivot_longer(res,cols = starts_with(var),names_to = "Label", values_to = var)
      p1 <- ggplot(res, aes(x = Model, y=!! rlang::sym(var),col = Model)) +
        geom_boxplot(outlier.shape = NA) +
        facet_grid(Label ~., scales = "free") + 
        scale_colour_brewer(palette = "Set2") + 
        ggtitle(paste0("SSB/SSB40%"," (over last ",use.n.years," years)")) +
        ylab("SSB/SSB40%") +
        coord_cartesian(ylim = quantile(res[[var]],c(0.05,0.95))) + 
        theme_bw()
      
      ggsave(file.path(main.dir,sub.dir,paste0("SSBoverSSB40%_last_",use.n.years,"years.PNG")), p1, width = 10, height = 7, dpi = dpi)
      
      # Reference points and stock status
      res = NULL
      for (i in 1:length(mods)){
        for (j in 1:length(mods[[1]])){
          tmp <- mods[[i]][[j]]$om$rep$SSB
          tmp <- cbind(tmp,rowSums(tmp))
          tmp <- tmp/exp(mods[[i]][[j]]$om$rep$log_SSB_FXSPR)
          if (is.null(use.n.years)) {
            tmp <- tail(tmp,5)
            tmp <- apply(tmp,2,function(x) sum(x < 0.5)/5)
          } else {
            tmp <- tail(tmp,use.n.years)
            tmp <- apply(tmp,2,function(x) sum(x < 0.5)/use.n.years)
          }
          tmp <- data.frame(t(tmp))
          names(tmp) <- paste0("SSB/SSB40%.s",1:ncol(tmp))
          names(tmp)[ncol(tmp)] <- "SSB/SSB40%_Global"
          tmp$nsim <- i
          tmp$Model <- paste("Model",j)
          res <- rbind(res, tmp)
        }
      }
      var = "SSB/SSB40%"
      res <- pivot_longer(res,cols = starts_with(var),names_to = "Label", values_to = var)
      p2 <- ggplot(res, aes(x = Model, y=!! rlang::sym(var),col = Model)) +
        geom_boxplot(outlier.shape = NA) +
        facet_grid(Label ~., scales = "free") + 
        scale_colour_brewer(palette = "Set2") + 
        ggtitle(paste0("Probability of SSB/SSB40% < 0.5"," (over last ",use.n.years," years)")) +
        ylab("Probability") +
        coord_cartesian(ylim = quantile(res[[var]],c(0.05,0.95))) + 
        theme_bw()
      
      ggsave(file.path(main.dir,sub.dir,paste0("Overfished_last_",use.n.years,"years.PNG")), p2, width = 10, height = 7, dpi = dpi)
      
      # Reference points and stock status (F)
      res = NULL
      for (i in 1:length(mods)){
        for (j in 1:length(mods[[1]])){
          Fbar <- mods[[i]][[j]]$om$rep$Fbar
          Fbar <- cbind(Fbar,rowSums(Fbar))
          n_fleets = mods[[i]][[j]]$om$input$data$n_fleets
          n_regions = mods[[i]][[j]]$om$input$data$n_regions
          Fbar_age = max(mods[[i]][[j]]$om$input$data$Fbar_ages)
          Fbar_XSPR = exp(mods[[i]][[j]]$om$rep$log_FAA_XSPR)[,,Fbar_age]
          Fbar_XSPR = t(rbind(Fbar_XSPR[1:n_fleets,],Fbar_XSPR[n_fleets+n_regions+1,]))
          tmp <- as.data.frame(Fbar/Fbar_XSPR)
          names(tmp) <- paste0("F/F40%.",1:ncol(tmp))
          names(tmp)[ncol(tmp)] <- "F/F40%_Global"
          if (is.null(use.n.years)) {
            tmp <- tail(tmp,5)
          } else {
            tmp <- tail(tmp,use.n.years)
          }
          tmp$nsim <- i
          tmp$Model <- paste("Model",j)
          res <- rbind(res, tmp)
        }
      }
      var = "F/F40%"
      res <- pivot_longer(res,cols = starts_with(var),names_to = "Label", values_to = var)
      p3 <- ggplot(res, aes(x = Model, y=!! rlang::sym(var),col = Model)) +
        geom_boxplot(outlier.shape = NA) +
        facet_grid(Label ~., scales = "free") + 
        scale_colour_brewer(palette = "Set2") + 
        ggtitle(paste0("F/F40%"," (over last ",use.n.years," years)")) +
        ylab("F/F40%") +
        coord_cartesian(ylim = quantile(res[[var]],c(0.05,0.95))) + 
        theme_bw()
      ggsave(file.path(main.dir,sub.dir,paste0("FoverF40_last_",use.n.years,"years.PNG")), p3, width = 10, height = 7, dpi = dpi)
      
      # Reference points and stock status (F)
      res = NULL
      for (i in 1:length(mods)){
        for (j in 1:length(mods[[1]])){
          Fbar <- mods[[i]][[j]]$om$rep$Fbar
          Fbar <- cbind(Fbar,rowSums(Fbar))
          n_fleets = mods[[i]][[j]]$om$input$data$n_fleets
          n_regions = mods[[i]][[j]]$om$input$data$n_regions
          Fbar_age = max(mods[[i]][[j]]$om$input$data$Fbar_ages)
          Fbar_XSPR = exp(mods[[i]][[j]]$om$rep$log_FAA_XSPR)[,,Fbar_age]
          Fbar_XSPR = t(rbind(Fbar_XSPR[1:n_fleets,],Fbar_XSPR[n_fleets+n_regions+1,]))
          tmp <- as.data.frame(Fbar/Fbar_XSPR)
          names(tmp) <- paste0("F/F40%.",1:ncol(tmp))
          names(tmp)[ncol(tmp)] <- "F/F40%_Global"
          if (is.null(use.n.years)) {
            tmp <- tail(tmp,5)
            tmp <- apply(tmp,2,function(x) sum(x > 1)/5)
          } else {
            tmp <- tail(tmp,use.n.years)
            tmp <- apply(tmp,2,function(x) sum(x > 1)/use.n.years)
          }
          tmp <- data.frame(t(tmp))
          names(tmp) <- paste0("F/F40%.",1:ncol(tmp))
          names(tmp)[ncol(tmp)] <- "F/F40%_Global"
          tmp$nsim <- i
          tmp$Model <- paste("Model",j)
          res <- rbind(res, tmp)
        }
      }
      var = "F/F40%"
      res <- pivot_longer(res,cols = starts_with(var),names_to = "Label", values_to = var)
      p4 <- ggplot(res, aes(x = Model, y=!! rlang::sym(var),col = Model)) +
        geom_boxplot(outlier.shape = NA) +
        facet_grid(Label ~., scales = "free") + 
        scale_colour_brewer(palette = "Set2") + 
        ggtitle(paste0("Probability of F/F40% > 1"," (over last ",use.n.years," years)")) +
        ylab("Probability") +
        coord_cartesian(ylim = quantile(res[[var]],c(0.05,0.95))) + 
        theme_bw()
      ggsave(file.path(main.dir,sub.dir,paste0("Overfishing_last_",use.n.years,"years.PNG")), p4, width = 10, height = 7, dpi = dpi)
      
      p <- ggarrange(p1,p2,p3,p4,common.legend = TRUE,legend = "right")
      ggsave(file.path(main.dir,sub.dir,paste0("Stock_Status_Performance_last_",use.n.years,"years.PNG")), p, width = 15, height = 10, dpi = dpi)
      
      # ------------------------------------------------------------
      # ------------------------------------------------------------
      # ------------------------ Radar Chart -----------------------
      # ------------------------------------------------------------
      # ------------------------------------------------------------
      
      extract_var <- function(mod, var){
        if (var != "Catch_r") {
          data <- data.frame(mod$om$rep[var])
          data[paste0(var,"_total")] <- rowSums(data)
          if (var == "Catch_s"){
            var = "pred_catch"
            data <- data.frame(mod$om$rep[var])
            data[paste0(var,"_total")] <- rowSums(data)
            names <- gsub("pred_catch","Catch_s",colnames(data))
            colnames(data) <- names
          }
        } else {
          var = "pred_stock_catch"
          data <- mod$om$rep$pred_stock_catch
          Catch_list = list()
          for (i in 1:nrow(data)) {
            Catch_list[[paste0("Catch_r",i)]] <- apply(data, MARGIN = 3, FUN = colSums)[i,]
          }
          Catch_r_total = apply(data, MARGIN = 3, FUN = sum)
          data <- data.frame(Catch_list,Catch_r_total)
        }
        return(data)
      }
      
      var = "SSB"
      res = NULL
      for (i in 1:length(mods)){
        for (j in 1:length(mods[[1]])){
          tmp <- extract_var(mods[[i]][[j]],var)
          if (is.null(use.n.years)) {
            tmp <- tail(tmp,5)
          } else {
            tmp <- tail(tmp,use.n.years)
          }
          tmp <- data.frame(t(colSums(tmp)))
          tmp$nsim <- i
          tmp$Model <- paste("Model",j)
          res <- rbind(res, tmp)
        }
      }
      res1 <- pivot_longer(res,cols = starts_with(var),names_to = "Index", values_to = "Value")
      
      var = "Catch_r"
      res = NULL
      for (i in 1:length(mods)){
        for (j in 1:length(mods[[1]])){
          tmp <- extract_var(mods[[i]][[j]],var)
          if (is.null(use.n.years)) {
            tmp <- tail(tmp,5)
          } else {
            tmp <- tail(tmp,use.n.years)
          }
          tmp <- data.frame(t(colSums(tmp)))
          tmp$nsim <- i
          tmp$Model <- paste("Model",j)
          res <- rbind(res, tmp)
        }
      }
      res2 <- pivot_longer(res,cols = starts_with(var),names_to = "Index", values_to = "Value")
      
      # Reference points and stock status
      res = NULL
      for (i in 1:length(mods)){
        for (j in 1:length(mods[[1]])){
          tmp <- mods[[i]][[j]]$om$rep$SSB
          tmp <- cbind(tmp,rowSums(tmp))
          tmp <- tmp/exp(mods[[i]][[j]]$om$rep$log_SSB_FXSPR)
          if (is.null(use.n.years)) {
            tmp <- tail(tmp,5)
            tmp <- apply(tmp,2,function(x) sum(x > 0.5)/5)
          } else {
            tmp <- tail(tmp,use.n.years)
            tmp <- apply(tmp,2,function(x) sum(x > 0.5)/use.n.years)
          }
          # tmp[which(tmp == 0)] = 0.01
          tmp <- data.frame(t(tmp))
          names(tmp) <- paste0("PNO_OFED_S",1:ncol(tmp))
          names(tmp)[ncol(tmp)] <- "PNO_OFED_G"
          tmp$nsim <- i
          tmp$Model <- paste("Model",j)
          res <- rbind(res, tmp)
        }
      }
      var = "PNO_OFED"
      res3 <- pivot_longer(res,cols = starts_with(var),names_to = "Index", values_to = "Value")
      
      # Reference points and stock status (F)
      res = NULL
      for (i in 1:length(mods)){
        for (j in 1:length(mods[[1]])){
          Fbar <- mods[[i]][[j]]$om$rep$Fbar
          Fbar <- cbind(Fbar,rowSums(Fbar))
          n_fleets = mods[[i]][[j]]$om$input$data$n_fleets
          n_regions = mods[[i]][[j]]$om$input$data$n_regions
          Fbar_age = max(mods[[i]][[j]]$om$input$data$Fbar_ages)
          Fbar_XSPR = exp(mods[[i]][[j]]$om$rep$log_FAA_XSPR)[,,Fbar_age]
          Fbar_XSPR = t(rbind(Fbar_XSPR[1:n_fleets,],Fbar_XSPR[n_fleets+n_regions+1,]))
          tmp <- as.data.frame(Fbar/Fbar_XSPR)
          names(tmp) <- paste0("PNO_OFING_S",1:ncol(tmp))
          names(tmp)[ncol(tmp)] <- "PNO_OFING_G"
          if (is.null(use.n.years)) {
            tmp <- tail(tmp,5)
            tmp <- apply(tmp,2,function(x) sum(x < 1)/5)
          } else {
            tmp <- tail(tmp,use.n.years)
            tmp <- apply(tmp,2,function(x) sum(x < 1)/use.n.years)
          }
          # tmp[which(tmp == 0)] = 0.01
          tmp <- data.frame(t(tmp))
          names(tmp) <- paste0("PNO_OFING_S",1:ncol(tmp))
          names(tmp)[ncol(tmp)] <- "PNO_OFING_G"
          tmp$nsim <- i
          tmp$Model <- paste("Model",j)
          res <- rbind(res, tmp)
        }
      }
      var = "PNO_OFING"
      res4 <- pivot_longer(res,cols = starts_with(var),names_to = "Index", values_to = "Value")
      
      res = rbind(res1,res2,res3,res4)
      
      df_agg <- res %>% 
        group_by(Model, Index) %>% 
        summarise(Value = mean(Value))
      # Reshape data to wide format
      df <- data.frame(spread(df_agg, key = Index, value = "Value"))
      
      rownames(df) <- df$Model
      row_names <- rownames(df)
      
      normalize <- function(x) {
        return((x-min(x))/(max(x)-min(x)))
      }
      
      df.tmp <- as.data.frame(lapply(df[,2:ncol(df)],normalize))
      
      for (i in 1:ncol(df.tmp)){
        tmp <- df.tmp[,i]
        if (max(tmp) == "NaN" & min(tmp) == "NaN") {
          df.tmp[,i] = 1
        }
      }
      
      max_values <- rep(1,ncol(df))
      min_values <- rep(0,ncol(df))
      
      df <- data.frame(Model = row_names,df.tmp)
      
      df <- rbind(max_values, min_values, df)
      
      row.names(df)= c("Maximum","Minimum",row_names)
      
      my_colors <- colorRampPalette(c("red","blue","green","purple","orange"))
      colors_border <- c(rgb(1,1,1,0),rgb(1,1,1,0),my_colors(length(row_names)))
      
      png(file.path(main.dir,sub.dir,paste0("Radar_chart_last_",use.n.years,"years.png")),width = 7, height = 7, units = "in", res = dpi)
      radarchart(
        df[,-1], axistype=0 , maxmin=F,
        #custom polygon
        plwd=4 , plty=1, 
        pcol = colors_border,
        #custom the grid
        cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8, 
        #custom labels
        vlcex=0.8 )
      legend(x=-1.4, y=1.25, legend = rownames(df)[-c(1:2)], col = my_colors(length(row_names)),
             pch=20, text.col = "grey", cex=0.8, pt.cex=2)
      dev.off()
      
      # ------------------------------------------------------------
      # ------------------------------------------------------------
      # --------------- Calculate Relative Difference --------------
      # ----------------- SSB, F, Catch_r, Catch_s -----------------
      # ------------------------------------------------------------
      # ------------------------------------------------------------
      
      extract_var <- function(mod, var){
        if (var != "Catch_r") {
          data <- data.frame(mod$om$rep[var])
          data[paste0(var,"_total")] <- rowSums(data)
          if (var == "Catch_s"){
            var = "pred_catch"
            data <- data.frame(mod$om$rep[var])
            data[paste0(var,"_total")] <- rowSums(data)
            names <- gsub("pred_catch","Catch_s",colnames(data))
            colnames(data) <- names
          }
        } else {
          var = "pred_stock_catch"
          data <- mod$om$rep$pred_stock_catch
          Catch_list = list()
          for (i in 1:nrow(data)) {
            Catch_list[[paste0("Catch_r.",i)]] <- apply(data, MARGIN = 3, FUN = colSums)[i,]
          }
          Catch_r.total = apply(data, MARGIN = 3, FUN = sum)
          data <- data.frame(Catch_list,Catch_r.total)
        }
        return(data)
      }
      
      # correct model ID (base.model)
      cat(paste0("\nBase model is Model ",base.mod,"!\n"))
      k = base.mod 
      
      var = "SSB"
      res = NULL
      
      for (i in 1:length(mods)){
        for (j in 1:length(mods[[1]])){
          tmp  <- extract_var(mods[[i]][[j]],var)
          true <- extract_var(mods[[i]][[k]],var)
          tmp  <- tmp/true - 1
          if (is.null(use.n.years)) {
            tmp$nsim <- i
            tmp$Model <- paste("Model",j)
            tmp$Year <- Years
            tmp <- tail(tmp,5)
            res <- rbind(res, tmp)
          } else {
            tmp$nsim <- i
            tmp$Model <- paste("Model",j)
            tmp$Year <- Years
            tmp <- tail(tmp,use.n.years)
            res <- rbind(res, tmp)
          }
        }
      }
      res <- pivot_longer(res,cols = starts_with(var),names_to = "Label", values_to = var)
      
      p1 <- ggplot(res, aes(x = Model, y=!! rlang::sym(var),col = Model)) +
        geom_boxplot(outlier.shape = NA) +
        facet_grid(Label ~., scales = "free") + 
        scale_colour_brewer(palette = "Set2") + 
        ggtitle(paste0("SSB"," (over last ",use.n.years," years)")) +
        ylab("Relative Difference") +
        coord_cartesian(ylim = quantile(res[[var]],c(0.05,0.95))) + 
        theme_bw()
      ggsave(file.path(main.dir,sub.dir,paste0(var,"_last_",use.n.years,"years_bias.PNG")), p1, width = 10, height = 7, dpi = dpi)
      
      var = "Fbar"
      res = NULL
      for (i in 1:length(mods)){
        for (j in 1:length(mods[[1]])){
          tmp  <- extract_var(mods[[i]][[j]],var)
          true <- extract_var(mods[[i]][[k]],var)
          tmp  <- tmp/true - 1
          if (is.null(use.n.years)) {
            tmp$nsim <- i
            tmp$Model <- paste("Model",j)
            tmp$Year <- Years
            tmp <- tail(tmp,5)
            res <- rbind(res, tmp)
          } else {
            tmp$nsim <- i
            tmp$Model <- paste("Model",j)
            tmp$Year <- Years
            tmp <- tail(tmp,use.n.years)
            res <- rbind(res, tmp)
          }
        }
      }
      res <- pivot_longer(res,cols = starts_with(var),names_to = "Label", values_to = var)
      
      p2 <- ggplot(res, aes(x = Model, y=!! rlang::sym(var),col = Model)) +
        geom_boxplot(outlier.shape = NA) +
        facet_grid(Label ~., scales = "free") + 
        scale_colour_brewer(palette = "Set2") + 
        ggtitle(paste0("Fleet-specific F"," (over last ",use.n.years," years)")) +
        ylab("Relative Difference") +
        coord_cartesian(ylim = quantile(res[[var]],c(0.05,0.95))) + 
        theme_bw()
      ggsave(file.path(main.dir,sub.dir,paste0(var,"_last_",use.n.years,"years_bias.PNG")), p2, width = 10, height = 7, dpi = dpi)
      
      var = "Catch_s"
      res = NULL
      for (i in 1:length(mods)){
        for (j in 1:length(mods[[1]])){
          tmp  <- extract_var(mods[[i]][[j]],var)
          true <- extract_var(mods[[i]][[k]],var)
          tmp  <- tmp/true - 1
          if (is.null(use.n.years)) {
            tmp$nsim <- i
            tmp$Model <- paste("Model",j)
            tmp$Year <- Years
            tmp <- tail(tmp,5)
            res <- rbind(res, tmp)
          } else {
            tmp$nsim <- i
            tmp$Model <- paste("Model",j)
            tmp$Year <- Years
            tmp <- tail(tmp,use.n.years)
            res <- rbind(res, tmp)
          }
        }
      }
      res <- pivot_longer(res,cols = starts_with(var),names_to = "Label", values_to = var)
      
      p3 <- ggplot(res, aes(x = Model, y=!! rlang::sym(var),col = Model)) +
        geom_boxplot(outlier.shape = NA) +
        facet_grid(Label ~., scales = "free") + 
        scale_colour_brewer(palette = "Set2") + 
        ggtitle(paste0("Stock-specific Catch"," (over last ",use.n.years," years)")) +
        ylab("Relative Difference") +
        coord_cartesian(ylim = quantile(res[[var]],c(0.05,0.95))) + 
        theme_bw()
      ggsave(file.path(main.dir,sub.dir,paste0(var,"_last_",use.n.years,"years_bias.PNG")), p3, width = 10, height = 7, dpi = dpi)
      
      var = "Catch_r"
      res = NULL
      for (i in 1:length(mods)){
        for (j in 1:length(mods[[1]])){
          tmp  <- extract_var(mods[[i]][[j]],var)
          true <- extract_var(mods[[i]][[k]],var)
          tmp  <- tmp/true - 1
          if (is.null(use.n.years)) {
            tmp$nsim <- i
            tmp$Model <- paste("Model",j)
            tmp$Year <- Years
            tmp <- tail(tmp,5)
            res <- rbind(res, tmp)
          } else {
            tmp$nsim <- i
            tmp$Model <- paste("Model",j)
            tmp$Year <- Years
            tmp <- tail(tmp,use.n.years)
            res <- rbind(res, tmp)
          }
        }
      }
      res <- pivot_longer(res,cols = starts_with(var),names_to = "Label", values_to = var)
      
      p4 <- ggplot(res, aes(x = Model, y=!! rlang::sym(var),col = Model)) +
        geom_boxplot(outlier.shape = NA) +
        facet_grid(Label ~., scales = "free") + 
        scale_colour_brewer(palette = "Set2") + 
        ggtitle(paste0("Region-specific Catch"," (over last ",use.n.years," years)")) +
        ylab("Relative Difference") +
        coord_cartesian(ylim = quantile(res[[var]],c(0.05,0.95))) + 
        theme_bw()
      
      ggsave(file.path(main.dir,sub.dir,paste0(var,"_last_",use.n.years,"years_bias.PNG")), p4, width = 10, height = 7, dpi = dpi)
      
      p <- ggarrange(p1,p2,p3,p4,common.legend = TRUE,legend = "right")
      ggsave(file.path(main.dir,sub.dir,paste0("Performance_last_",use.n.years,"years_bias.PNG")), p, width = 15, height = 10, dpi = dpi)
      
      # ------------------------------------------------------------
      # ------------------------------------------------------------
      # ------------------------- KOBE Plot ------------------------
      # ------------------------------------------------------------
      # ------------------------------------------------------------
      
      res = NULL
      for (i in 1:length(mods)){
        for (j in 1:length(mods[[1]])){
          tmp <- mods[[i]][[j]]$om$rep$SSB
          tmp <- cbind(tmp,rowSums(tmp))
          tmp <- as.data.frame(tmp/exp(mods[[i]][[j]]$om$rep$log_SSB_FXSPR))
          tmp <- tail(tmp,use.n.years)
          names(tmp) <- paste0("S",1:ncol(tmp))
          names(tmp)[ncol(tmp)] <- "GB"
          tmp$nsim <- i
          tmp$Model <- paste("Model",j)
          res <- rbind(res, tmp)
        }
      }
      res1 = res
      
      temp1 <- NULL
      for (i in 1:(ncol(res1)-2)) {
        tmp <- data.frame(res1[,i],res1[,'nsim'],res1[,'Model'])
        names(tmp) = c('Overfished',tail(names(res1),2))
        tmp$Index <- names(res1)[i]
        temp1 <- rbind(temp1,tmp)
      }
      
      res = NULL
      for (i in 1:length(mods)){
        for (j in 1:length(mods[[1]])){
          Fbar <- mods[[i]][[j]]$om$rep$Fbar
          Fbar <- cbind(Fbar,rowSums(Fbar))
          n_fleets = mods[[i]][[j]]$om$input$data$n_fleets
          n_regions = mods[[i]][[j]]$om$input$data$n_regions
          Fbar_age = max(mods[[i]][[j]]$om$input$data$Fbar_ages)
          Fbar_XSPR = exp(mods[[i]][[j]]$om$rep$log_FAA_XSPR)[,,Fbar_age]
          Fbar_XSPR = t(rbind(Fbar_XSPR[1:n_fleets,],Fbar_XSPR[n_fleets+n_regions+1,]))
          tmp <- as.data.frame(Fbar/Fbar_XSPR)
          tmp <- tail(tmp,use.n.years)
          names(tmp) <- paste0("S",1:ncol(tmp))
          names(tmp)[ncol(tmp)] <- "GB"
          tmp$nsim <- i
          tmp$Model <- paste("Model",j)
          res <- rbind(res, tmp)
        }
      }
      res2 = res
      
      temp2 <- NULL
      for (i in 1:(ncol(res2)-2)) {
        tmp <- data.frame(res2[,i],res2[,'nsim'],res2[,'Model'])
        names(tmp) = c('Overfishing',tail(names(res2),2))
        tmp$Index <- names(res2)[i]
        temp2 <- rbind(temp2,tmp)
      }
      
      temp <- cbind(temp1,temp2)
      temp <- temp %>% 
        select(unique(names(.))) 
      
      df2 <- temp[temp$Model == paste0("Model ",k),]
      
      n.col = length(unique(df$Model))
      my_colors <- colorRampPalette(c("red","blue","green","purple","orange"))(n.col)
      
      p <- ggplot(data = temp,aes(x = Overfished ,y = Overfishing)) + 
        facet_wrap(~ Index) +
        geom_point(aes(color = Model),size = 3, alpha = 0.3) +
        # scale_color_manual(values=viridis::viridis(n.col)) +
        scale_color_manual(values=my_colors) +
        annotate('rect',xmin = 0.5, xmax = 100, ymin = -100, ymax = 1, alpha = 0.2, fill = "yellow") +
        theme_bw() +
        xlab(bquote(paste("SSB/", SSB[paste(.(40),"%")]))) +
        ylab(bquote(paste("F/", F[paste(.(40),"%")]))) +
        ggtitle("Stock Status") +
        theme(axis.text=element_text(size=12,face="bold"),
              axis.title=element_text(size=20,face="bold"),
              plot.title=element_text(size=15,face="bold"),
              strip.text = element_text(size=12,face="bold"),
              legend.text = element_text(size=12,face="bold"),
              legend.title = element_text(size=12,face="bold")) +
        theme(strip.text.x = element_text(size=12, color="black",
                                          face="bold.italic")) +
        geom_point(data = df2, aes(x=Overfished,y=Overfishing),colour="black",size = 2.5, shape = 21, stroke = 2, alpha = .1) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black")) +
        geom_vline(xintercept = 0.5, linetype = "dashed", color = "red",size = 1) + 
        geom_hline(yintercept = 1, linetype = "dashed", color = "red",size = 1) +
        coord_cartesian(ylim = quantile(temp[['Overfishing']],c(0.05,0.95)),xlim = quantile(temp[['Overfished']],c(0.05,0.95)))
      ggsave(file.path(main.dir,sub.dir,paste0("Population_Status_last_",use.n.years,"years_all.PNG")), p, width = 15, height = 5, dpi = dpi)
      
      temp <- temp %>% 
        select(unique(names(.))) %>%
        select(-nsim)
      
      df <- aggregate(. ~ Model + Index, temp, function(x) quantile(x))
      df <- do.call("data.frame", df)
      df2 <- df[df$Model == paste0("Model ",k),] 
      
      n.col = length(unique(df$Model))
      my_colors <- colorRampPalette(c("red","blue","green","purple","orange"))(n.col)
      
      p <- ggplot(data = df,aes(x = Overfished.50.,y = Overfishing.50.)) + 
        facet_wrap(~ Index) +
        annotate('rect',xmin = 0.5, xmax = 100, ymin = -100, ymax = 1, alpha = 0.4, fill = "yellow") +
        geom_errorbar(aes(ymin = Overfishing.25.,
                          ymax = Overfishing.75.,
                          color = Model), size = 1) +
        geom_errorbarh(aes(xmin = Overfished.25.,
                           xmax = Overfished.75.,
                           color = Model), size = 1) +
        geom_point(aes(color = Model),size = 5, alpha = 0.8) +
        # scale_color_manual(values=viridis::viridis(n.col)) + 
        scale_color_manual(values=my_colors) +
        theme_bw() +
        xlab(bquote(paste("SSB/", SSB[paste(.(40),"%")]))) +
        ylab(bquote(paste("F/", F[paste(.(40),"%")]))) +
        ggtitle("Stock Status") +
        theme(axis.text=element_text(size=12,face="bold"),
              axis.title=element_text(size=20,face="bold"),
              plot.title=element_text(size=15,face="bold"),
              strip.text = element_text(size=12,face="bold"),
              legend.text = element_text(size=12,face="bold"),
              legend.title = element_text(size=12,face="bold")) +
        theme(strip.text.x = element_text(size=12, color="black",
                                          face="bold.italic")) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black")) +
        geom_vline(xintercept = 0.5, linetype = "dashed", color = "red",size = 1) + 
        geom_hline(yintercept = 1, linetype = "dashed", color = "red",size = 1) +
        coord_cartesian(ylim = quantile(temp[['Overfishing']],c(0.05,0.95)),xlim = quantile(temp[['Overfished']],c(0.05,0.95)))
      ggsave(file.path(main.dir,sub.dir,paste0("Population_Status_last_",use.n.years,"years_median.PNG")), p, width = 15, height = 5, dpi = dpi)
      
      
      # ------------------------------------------------------------
      # ------------------------------------------------------------
      # ------------------ Calculate First N years -----------------
      # ----------------- SSB, F, Catch_r, Catch_s -----------------
      # ------------------------------------------------------------
      # ------------------------------------------------------------
      
      if(!is.null(do.short.term)) {
        cat(paste0("\nPerformance metrics are summarized over a short term!\n"))
        cat(paste0("\nMake sure |first.n.years|, |year_start|, and |year_end| are specified correctly!\n")) 
        cat(paste0("\nfirst.n.years = ", do.short.term$first.n.years,"\n"))
        cat(paste0("\nyear_start = ", do.short.term$year_start,"\n"))
        cat(paste0("\nyear_end = ", do.short.term$year_end,"\n"))
        first.n.years = do.short.term$first.n.years
        year_start = do.short.term$year_start
        year_end = do.short.term$year_end
      }
      
      base.years = year_end-year_start+1
      first.mse.year = base.years+1
      
      extract_var <- function(mod, var){
        if (var != "Catch_r") {
          data <- data.frame(mod$om$rep[var])
          data[paste0(var,"_total")] <- rowSums(data)
          if (var == "Catch_s"){
            var = "pred_catch"
            data <- data.frame(mod$om$rep[var])
            data[paste0(var,"_total")] <- rowSums(data)
            names <- gsub("pred_catch","Catch_s",colnames(data))
            colnames(data) <- names
          }
        } else {
          var = "pred_stock_catch"
          data <- mod$om$rep$pred_stock_catch
          Catch_list = list()
          for (i in 1:nrow(data)) {
            Catch_list[[paste0("Catch_r.",i)]] <- apply(data, MARGIN = 3, FUN = colSums)[i,]
          }
          Catch_r.total = apply(data, MARGIN = 3, FUN = sum)
          data <- data.frame(Catch_list,Catch_r.total)
        }
        return(data)
      }
      
      # correct model ID
      k = base.mod
      
      var = "SSB"
      res = NULL
      
      for (i in 1:length(mods)){
        for (j in 1:length(mods[[1]])){
          tmp  <- extract_var(mods[[i]][[j]],var)
          true <- extract_var(mods[[i]][[k]],var)
          tmp  <- tmp/true - 1
          if (is.null(first.n.years)) {
            tmp$nsim <- i
            tmp$Model <- paste("Model",j)
            tmp$Year <- Years
            tmp <- tmp[first.mse.year:(first.mse.year+2),]
            res <- rbind(res, tmp)
          } else {
            tmp$nsim <- i
            tmp$Model <- paste("Model",j)
            tmp$Year <- Years
            tmp <- tmp[first.mse.year:(first.mse.year+first.n.years),]
            res <- rbind(res, tmp)
          }
        }
      }
      res <- pivot_longer(res,cols = starts_with(var),names_to = "Label", values_to = var)
      
      p1 <- ggplot(res, aes(x = Model, y=!! rlang::sym(var),col = Model)) +
        geom_boxplot(outlier.shape = NA) +
        facet_grid(Label ~., scales = "free") + 
        scale_colour_brewer(palette = "Set2") + 
        ggtitle(paste0("SSB"," (over first ",first.n.years," years)")) +
        ylab("Relative Difference") +
        coord_cartesian(ylim = quantile(res[[var]],c(0.05,0.95))) + 
        theme_bw()
      ggsave(file.path(main.dir,sub.dir,paste0(var,"_first_",first.n.years,"years_bias.PNG")), p1, width = 10, height = 7, dpi = dpi)
      
      var = "Fbar"
      res = NULL
      for (i in 1:length(mods)){
        for (j in 1:length(mods[[1]])){
          tmp  <- extract_var(mods[[i]][[j]],var)
          true <- extract_var(mods[[i]][[k]],var)
          tmp  <- tmp/true - 1
          if (is.null(first.n.years)) {
            tmp$nsim <- i
            tmp$Model <- paste("Model",j)
            tmp$Year <- Years
            tmp <- tmp[first.mse.year:(first.mse.year+2),]
            res <- rbind(res, tmp)
          } else {
            tmp$nsim <- i
            tmp$Model <- paste("Model",j)
            tmp$Year <- Years
            tmp <- tmp[first.mse.year:(first.mse.year+first.n.years),]
            res <- rbind(res, tmp)
          }
        }
      }
      res <- pivot_longer(res,cols = starts_with(var),names_to = "Label", values_to = var)
      
      p2 <- ggplot(res, aes(x = Model, y=!! rlang::sym(var),col = Model)) +
        geom_boxplot(outlier.shape = NA) +
        facet_grid(Label ~., scales = "free") + 
        scale_colour_brewer(palette = "Set2") + 
        ggtitle(paste0("Fleet-specific F"," (over first ",first.n.years," years)")) +
        ylab("Relative Difference") +
        coord_cartesian(ylim = quantile(res[[var]],c(0.05,0.95))) + 
        theme_bw()
      ggsave(file.path(main.dir,sub.dir,paste0(var,"_first_",first.n.years,"years_bias.PNG")), p2, width = 10, height = 7, dpi = dpi)
      
      var = "Catch_s"
      res = NULL
      for (i in 1:length(mods)){
        for (j in 1:length(mods[[1]])){
          tmp  <- extract_var(mods[[i]][[j]],var)
          true <- extract_var(mods[[i]][[k]],var)
          tmp  <- tmp/true - 1
          if (is.null(first.n.years)) {
            tmp$nsim <- i
            tmp$Model <- paste("Model",j)
            tmp$Year <- Years
            tmp <- tmp[first.mse.year:(first.mse.year+2),]
            res <- rbind(res, tmp)
          } else {
            tmp$nsim <- i
            tmp$Model <- paste("Model",j)
            tmp$Year <- Years
            tmp <- tmp[first.mse.year:(first.mse.year+first.n.years),]
            res <- rbind(res, tmp)
          }
        }
      }
      res <- pivot_longer(res,cols = starts_with(var),names_to = "Label", values_to = var)
      
      p3 <- ggplot(res, aes(x = Model, y=!! rlang::sym(var),col = Model)) +
        geom_boxplot(outlier.shape = NA) +
        facet_grid(Label ~., scales = "free") + 
        scale_colour_brewer(palette = "Set2") + 
        ggtitle(paste0("Stock-specific Catch"," (over first ",first.n.years," years)")) +
        ylab("Relative Difference") +
        coord_cartesian(ylim = quantile(res[[var]],c(0.05,0.95))) + 
        theme_bw()
      ggsave(file.path(main.dir,sub.dir,paste0(var,"_first_",first.n.years,"years_bias.PNG")), p3, width = 10, height = 7, dpi = dpi)
      
      var = "Catch_r"
      res = NULL
      for (i in 1:length(mods)){
        for (j in 1:length(mods[[1]])){
          tmp  <- extract_var(mods[[i]][[j]],var)
          true <- extract_var(mods[[i]][[k]],var)
          tmp  <- tmp/true - 1
          if (is.null(first.n.years)) {
            tmp$nsim <- i
            tmp$Model <- paste("Model",j)
            tmp$Year <- Years
            tmp <- tmp[first.mse.year:(first.mse.year+2),]
            res <- rbind(res, tmp)
          } else {
            tmp$nsim <- i
            tmp$Model <- paste("Model",j)
            tmp$Year <- Years
            tmp <- tmp[first.mse.year:(first.mse.year+first.n.years),]
            res <- rbind(res, tmp)
          }
        }
      }
      res <- pivot_longer(res,cols = starts_with(var),names_to = "Label", values_to = var)
      
      p4 <- ggplot(res, aes(x = Model, y=!! rlang::sym(var),col = Model)) +
        geom_boxplot(outlier.shape = NA) +
        facet_grid(Label ~., scales = "free") + 
        scale_colour_brewer(palette = "Set2") + 
        ggtitle(paste0("Region-specific Catch"," (over first ",first.n.years," years)")) +
        ylab("Relative Difference") +
        coord_cartesian(ylim = quantile(res[[var]],c(0.05,0.95))) + 
        theme_bw()
      
      ggsave(file.path(main.dir,sub.dir,paste0(var,"_first_",first.n.years,"years_bias.PNG")), p4, width = 10, height = 7, dpi = dpi)
      
      p <- ggarrange(p1,p2,p3,p4,common.legend = TRUE,legend = "right")
      ggsave(file.path(main.dir,sub.dir,paste0("Performance_first_",first.n.years,"years_bias.PNG")), p, width = 15, height = 10, dpi = dpi)
    }
    
    # ------------------------------------------------------------
    # ------------------------------------------------------------
    # ------------------ Simulation-Estimation -------------------
    # ------------ Mean_rec, Sigma, Pearson_resid. ---------------
    # ------------------------------------------------------------
    # ------------------------------------------------------------
    res = NULL
    for (i in 1:length(mods)){ # ith realization
      for (j in 1:length(mods[[1]])){ #jth EM
        tmp <- mods[[i]][[j]]
        k = length(tmp$par.est) # kth assessment 
        if(any(names(tmp$par.est[[k]]) == "mean_rec_pars")){
          temp = exp(tmp$par.est[[k]]$mean_rec_pars[,1])
        } else {
          m = length(tmp$par.est[[k]])
          temp <- NULL
          for (n in 1:m) {
            temp1 = exp(tmp$par.est[[k]][[n]]$mean_rec_pars[,1])
            temp <- c(temp,temp1)
          }
        }
        nsim  = i
        Model = j
        res1  = data.frame(Model,nsim,Value = temp)
        res1$Var = paste0("Mean_Rec_",1:length(temp))
        if(length(temp) == 1) res1$Var = paste0("Mean_Rec")
        res = rbind(res,res1)
      }
    }
    
    res$Model = as.factor(res$Model)
    
    p1 <- ggplot(res, aes(x = Model, y=Value,col = Model)) +
      geom_boxplot(outlier.shape = NA) +
      facet_grid(Var ~., scales = "free") + 
      scale_colour_brewer(palette = "Set2") + 
      ggtitle(paste0("Mean Recruitment\nEstimated from the last assessment model")) +
      ylab("") +
      theme_bw() +
      theme(axis.text=element_text(size=12,face="bold"),
            axis.title=element_text(size=20,face="bold"),
            plot.title=element_text(size=15,face="bold"),
            strip.text = element_text(size=12,face="bold"),
            legend.text = element_text(size=12,face="bold"),
            legend.title = element_text(size=12,face="bold")) +
      theme(strip.text.x = element_text(size=12, color="black",
                                        face="bold.italic"))
    ggsave(file.path(main.dir,sub.dir,paste0("Mean_rec_par.PNG")), p1, width = 10, height = 10, dpi = dpi)
    
    
    res = NULL
    for (i in 1:length(mods)){ # ith realization
      for (j in 1:length(mods[[1]])){ #jth EM
        tmp <- mods[[i]][[j]]
        k = length(tmp$par.est) # kth assessment 
        if(any(names(tmp$par.est[[k]]) == "log_NAA_sigma")){
          rec_sig = exp(tmp$par.est[[k]]$log_NAA_sigma[,1,1])
          naa_sig = exp(tmp$par.est[[k]]$log_NAA_sigma[,1,2])
          temp <- c(rec_sig,naa_sig)
        } else {
          m = length(tmp$par.est[[k]])
          temp <- NULL
          for (n in 1:m) {
            rec_sig = exp(tmp$par.est[[k]][[n]]$log_NAA_sigma[,1,1])
            naa_sig = exp(tmp$par.est[[k]][[n]]$log_NAA_sigma[,1,2])
            temp <- c(rec_sig,naa_sig)
          }
        }
        
        nsim  = i
        Model = j
        
        res1  = data.frame(Model,nsim,Value = temp)
        if (length(temp) == 2) {
          res1$Var = c("Rec_sigma","NAA_sigma")
        } else{
          res1$Var = c(paste0("Rec_sigma",1:length(rec_sig)),paste0("NAA_sigma",1:length(naa_sig)))
        }
        res = rbind(res,res1)
      }
    }
    
    res$Model = as.factor(res$Model)
    
    p2 <- ggplot(res, aes(x = Model, y=Value,col = Model)) +
      geom_boxplot(outlier.shape = NA) +
      facet_grid(Var ~., scales = "free") + 
      scale_colour_brewer(palette = "Set2") + 
      ggtitle(paste0("Variance para. of NAA deviations\nEstimated from the last assessment model")) +
      ylab("") +
      theme_bw() +
      theme(axis.text=element_text(size=12,face="bold"),
            axis.title=element_text(size=20,face="bold"),
            plot.title=element_text(size=15,face="bold"),
            strip.text = element_text(size=12,face="bold"),
            legend.text = element_text(size=12,face="bold"),
            legend.title = element_text(size=12,face="bold")) +
      theme(strip.text.x = element_text(size=12, color="black",
                                        face="bold.italic"))
    ggsave(file.path(main.dir,sub.dir,paste0("Variance_Para_NAA.PNG")), p2, width = 10, height = 10, dpi = dpi)
    
    # ------------------------------------------------------------
    # ------------------------------------------------------------
    # ------------------- AIC, Pearson Resid. --------------------
    # ------------ Mean_rec, Sigma, Pearson_resid. ---------------
    # ------------------------------------------------------------
    # ------------------------------------------------------------

    # aic = -2*(tmp$opt_list[[k]]$obj + length(tmp$opt_list[[k]]$par))
    res = NULL
    for (i in 1:length(mods)){ # ith realization
      for (j in 1:length(mods[[1]])){ #jth EM
        tmp <- mods[[i]][[j]]
        k = length(tmp$par.est) # kth assessment 
        if(any(names(tmp$par.est[[k]]) == "mean_rec_pars")){
          temp = -2*(tmp$opt_list[[k]]$obj + length(tmp$opt_list[[k]]$par))
        } else {
          m = length(tmp$par.est[[k]])
          temp <- NULL
          for (n in 1:m) {
            temp1 = -2*(tmp$opt_list[[k]][[n]]$obj + length(tmp$opt_list[[k]][[n]]$par))
            temp <- sum(temp,temp1)
          }
        }
        nsim  = i
        Model = j
        res1  = data.frame(Model,nsim,Value = temp)
        res1$Var = paste0("AIC_",1:length(temp))
        if(length(temp) == 1) res1$Var = paste0("AIC")
        res = rbind(res,res1)
      }
    }
    
    res$Model = as.factor(res$Model)
    
    p3 <- ggplot(res, aes(x = Model, y=Value,col = Model)) +
      geom_boxplot(outlier.shape = NA) +
      facet_grid(Var ~., scales = "free") + 
      scale_colour_brewer(palette = "Set2") + 
      ggtitle(paste0("AIC")) +
      ylab("") +
      theme_bw() +
      theme(axis.text=element_text(size=12,face="bold"),
            axis.title=element_text(size=20,face="bold"),
            plot.title=element_text(size=15,face="bold"),
            strip.text = element_text(size=12,face="bold"),
            legend.text = element_text(size=12,face="bold"),
            legend.title = element_text(size=12,face="bold")) +
      theme(strip.text.x = element_text(size=12, color="black",
                                        face="bold.italic"))
    ggsave(file.path(main.dir,sub.dir,paste0("AIC.PNG")), p3, width = 5, height = 5, dpi = dpi)

    cat("\n----------------------------------\n| Congrats! Your report is done! |\n----------------------------------\n")
    cat(paste0("\nReport has been saved in ",file.path(main.dir,sub.dir)))
  }
}
