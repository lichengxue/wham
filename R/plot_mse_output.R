#' Plot performance metrics for one realization using different management strategies
#' 
#' a function to compare performance metrics of different assessment models 
#' 
#' @param mods A list of models 
#' @param main.dir Path to save the final report
#' @param use.n.years Number of years that is used to summarize statistics   
#' @param dpi Resolution (number of dots per inches)
#' 
#' @return a report
#'   
#' @export
#'
plot_mse_output <- function(mods, main.dir = getwd(), use.n.years = 10, dpi = 150) {
  # main.dir = getwd()
  
  require(ggplot2)
  require(dplyr)
  require(tidyr)
  require(fmsb)
  sub.dir <- 'Report'
  if (file.exists(sub.dir)){
  } else {
    dir.create(file.path(main.dir, sub.dir))
  }
  
  if(!is.list(mods[[1]][[1]][[1]])){ # Compare models over 1 realization
    Years = mods[[1]]$om$years
    
    if(is.null(use.n.years)) use.n.years = 10
    
    require(ggplot2)
    
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
    res <- tidyr::pivot_longer(res,cols = starts_with(var),names_to = "Label", values_to = var)
    
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
    res <- tidyr::pivot_longer(res,cols = starts_with(var),names_to = "Label", values_to = var)
    
    p2 <- ggplot(res, aes(x = Year, y=!! rlang::sym(var),col = Model)) +
      geom_line(size = 0.8, alpha = 0.8) +
      facet_grid(Label ~., scales = "free") + 
      scale_colour_brewer(palette = "Set2") + 
      ggtitle("Fleet-specific F") +
      ylab("F") +
      theme_bw()
    ggsave(file.path(main.dir,sub.dir,paste0(var,".PNG")), p2, width = 10, height = 7, dpi = dpi)
    
    # ----------------------------------------------------------------------------
    var = "Catch_s"
    res = NULL
    for (i in 1:length(mods)){
      tmp <- extract_var(mods[[i]],var)
      tmp$Model <- paste("Model",i)
      tmp$Year <- Years
      res <- rbind(res, tmp)
    }
    res <- tidyr::pivot_longer(res,cols = starts_with(var),names_to = "Label", values_to = var)
    
    p3 <- ggplot(res, aes(x = Year, y=!! rlang::sym(var),col = Model)) +
      geom_line(size = 0.8, alpha = 0.8) +
      facet_grid(Label ~., scales = "free") + 
      scale_colour_brewer(palette = "Set2") + 
      ggtitle("Stock-specific Catch") +
      ylab("Catch") +
      theme_bw()
    ggsave(file.path(main.dir,sub.dir,paste0(var,".PNG")), p3, width = 10, height = 7, dpi = dpi)
    
    #--------------------------------------------------------------------------------
    var = "Catch_r"
    res = NULL
    for (i in 1:length(mods)){
      tmp <- extract_var(mods[[i]],var)
      tmp$Model <- paste("Model",i)
      tmp$Year <- Years
      res <- rbind(res, tmp)
    }
    res <- tidyr::pivot_longer(res,cols = starts_with(var),names_to = "Label", values_to = var)
    
    p4 <- ggplot(res, aes(x = Year, y=!! rlang::sym(var),col = Model)) +
      geom_line(size = 0.8, alpha = 0.8) +
      facet_grid(Label ~., scales = "free") + 
      scale_colour_brewer(palette = "Set2") + 
      ggtitle("Region-specific Catch") +
      ylab("Catch") +
      theme_bw()
    ggsave(file.path(main.dir,sub.dir,paste0(var,".PNG")), p4, width = 10, height = 7, dpi = dpi)
    
    p <- ggpubr::ggarrange(p1,p2,p3,p4,common.legend = TRUE,legend = "right")
    ggsave(file.path(main.dir,sub.dir,"Performance_Metrics.PNG"), p, width = 15, height = 10, dpi = dpi)
    
    #-------------------------------------------------------------------------
    #--------------------------------Boxplot----------------------------------
    #-------------------------------------------------------------------------
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
    
    res <- tidyr::pivot_longer(res,cols = starts_with(var),names_to = "Label", values_to = var)
    
    p1 <- ggplot(res, aes(x = Model, y=!! rlang::sym(var),col = Model)) +
      geom_boxplot() +
      facet_grid(Label ~., scales = "free") + 
      scale_colour_brewer(palette = "Set2") + 
      ggtitle(paste0("SSB"," (over last ",use.n.years," years)")) +
      ylab("SSB") +
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
    
    res <- tidyr::pivot_longer(res,cols = starts_with(var),names_to = "Label", values_to = var)
    
    p2 <- ggplot(res, aes(x = Model, y=!! rlang::sym(var),col = Model)) +
      geom_boxplot() +
      facet_grid(Label ~., scales = "free") + 
      scale_colour_brewer(palette = "Set2") + 
      ggtitle(paste0("Fleet-specific F"," (over last ",use.n.years," years)")) +
      ylab("F") +
      theme_bw()
    ggsave(file.path(main.dir,sub.dir,paste0(var,"_last_",use.n.years,"years.PNG")), p2, width = 10, height = 7, dpi = dpi)
    
    # ----------------------------------------------------------------------------
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
    
    res <- tidyr::pivot_longer(res,cols = starts_with(var),names_to = "Label", values_to = var)
    
    p3 <- ggplot(res, aes(x = Model, y=!! rlang::sym(var),col = Model)) +
      geom_boxplot() +
      facet_grid(Label ~., scales = "free") + 
      scale_colour_brewer(palette = "Set2") + 
      ggtitle(paste0("Stock-specific Catch"," (over last ",use.n.years," years)")) +
      ylab("Catch") +
      theme_bw()
    ggsave(file.path(main.dir,sub.dir,paste0(var,"_last_",use.n.years,"years.PNG")), p3, width = 10, height = 7, dpi = dpi)
    
    #--------------------------------------------------------------------------------
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
    res <- tidyr::pivot_longer(res,cols = starts_with(var),names_to = "Label", values_to = var)
    
    p4 <- ggplot(res, aes(x = Model, y=!! rlang::sym(var),col = Model)) +
      geom_boxplot() +
      facet_grid(Label ~., scales = "free") + 
      scale_colour_brewer(palette = "Set2") + 
      ggtitle(paste0("Region-specific Catch"," (over last ",use.n.years," years)")) +
      ylab("Catch") +
      theme_bw()
    
    ggsave(file.path(main.dir,sub.dir,paste0(var,"_last_",use.n.years,"years.PNG")), p4, width = 10, height = 7, dpi = dpi)
    
    p <- ggpubr::ggarrange(p1,p2,p3,p4,common.legend = TRUE,legend = "right")
    ggsave(file.path(main.dir,sub.dir,paste0("Performance_last_",use.n.years,"years.PNG")), p, width = 15, height = 10, dpi = dpi)
    
  } else { # Compare models over x realizations
    # --------------------------------------------------------------------------
    # --------------------------------------------------------------------------
    # --------------------------------------------------------------------------
    Years = mods[[1]][[1]]$om$years
    
    if(is.null(use.n.years)) use.n.years = 10
    
    require(ggplot2)
    
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
    
    #-------------------------------------------------------------------------
    #--------------------------------Boxplot----------------------------------
    #-------------------------------------------------------------------------
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
    res <- tidyr::pivot_longer(res,cols = starts_with(var),names_to = "Label", values_to = var)
    
    p1 <- ggplot(res, aes(x = Model, y=!! rlang::sym(var),col = Model)) +
      geom_boxplot() +
      facet_grid(Label ~., scales = "free") + 
      scale_colour_brewer(palette = "Set2") + 
      ggtitle(paste0("SSB"," (over last ",use.n.years," years)")) +
      ylab("SSB") +
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
    res <- tidyr::pivot_longer(res,cols = starts_with(var),names_to = "Label", values_to = var)
    
    p2 <- ggplot(res, aes(x = Model, y=!! rlang::sym(var),col = Model)) +
      geom_boxplot() +
      facet_grid(Label ~., scales = "free") + 
      scale_colour_brewer(palette = "Set2") + 
      ggtitle(paste0("Fleet-specific F"," (over last ",use.n.years," years)")) +
      ylab("F") +
      theme_bw()
    ggsave(file.path(main.dir,sub.dir,paste0(var,"_last_",use.n.years,"years.PNG")), p2, width = 10, height = 7, dpi = dpi)
    
    # ----------------------------------------------------------------------------
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
    res <- tidyr::pivot_longer(res,cols = starts_with(var),names_to = "Label", values_to = var)
    
    p3 <- ggplot(res, aes(x = Model, y=!! rlang::sym(var),col = Model)) +
      geom_boxplot() +
      facet_grid(Label ~., scales = "free") + 
      scale_colour_brewer(palette = "Set2") + 
      ggtitle(paste0("Stock-specific Catch"," (over last ",use.n.years," years)")) +
      ylab("Catch") +
      theme_bw()
    ggsave(file.path(main.dir,sub.dir,paste0(var,"_last_",use.n.years,"years.PNG")), p3, width = 10, height = 7, dpi = dpi)
    
    #--------------------------------------------------------------------------------
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
    res <- tidyr::pivot_longer(res,cols = starts_with(var),names_to = "Label", values_to = var)
    
    p4 <- ggplot(res, aes(x = Model, y=!! rlang::sym(var),col = Model)) +
      geom_boxplot() +
      facet_grid(Label ~., scales = "free") + 
      scale_colour_brewer(palette = "Set2") + 
      ggtitle(paste0("Region-specific Catch"," (over last ",use.n.years," years)")) +
      ylab("Catch") +
      theme_bw()
    
    ggsave(file.path(main.dir,sub.dir,paste0(var,"_last_",use.n.years,"years.PNG")), p4, width = 10, height = 7, dpi = dpi)
    
    p <- ggpubr::ggarrange(p1,p2,p3,p4,common.legend = TRUE,legend = "right")
    ggsave(file.path(main.dir,sub.dir,paste0("Performance_last_",use.n.years,"years.PNG")), p, width = 15, height = 10, dpi = dpi)
    
    # --------------------------------------------------------------------------
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
    res <- tidyr::pivot_longer(res,cols = starts_with(var),names_to = "Label", values_to = var)
    p <- ggplot(res, aes(x = Model, y=!! rlang::sym(var),col = Model)) +
      geom_boxplot() +
      facet_grid(Label ~., scales = "free") + 
      scale_colour_brewer(palette = "Set2") + 
      ggtitle(paste0("Stock-specific Total Catch"," (over last ",use.n.years," years)")) +
      ylab("Catch") +
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
    res <- tidyr::pivot_longer(res,cols = starts_with(var),names_to = "Label", values_to = var)
    p <- ggplot(res, aes(x = Model, y=!! rlang::sym(var),col = Model)) +
      geom_boxplot() +
      facet_grid(Label ~., scales = "free") + 
      scale_colour_brewer(palette = "Set2") + 
      ggtitle(paste0("Region-specific Total Catch"," (over last ",use.n.years," years)")) +
      ylab("Catch") +
      theme_bw()
    
    ggsave(file.path(main.dir,sub.dir,paste0("Total_",var,"_last_",use.n.years,"years.PNG")), p, width = 10, height = 7, dpi = dpi)
    
    # -------------------------------------------------------------------------
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
    res <- tidyr::pivot_longer(res,cols = starts_with(var),names_to = "Label", values_to = var)
    p1 <- ggplot(res, aes(x = Model, y=!! rlang::sym(var),col = Model)) +
      geom_boxplot() +
      facet_grid(Label ~., scales = "free") + 
      scale_colour_brewer(palette = "Set2") + 
      ggtitle(paste0("SSB/SSB40%"," (over last ",use.n.years," years)")) +
      ylab("SSB/SSB40%") +
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
          tmp <- apply(tmp,2,function(x) sum(x > 1)/5)
        } else {
          tmp <- tail(tmp,use.n.years)
          tmp <- apply(tmp,2,function(x) sum(x > 1)/use.n.years)
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
    res <- tidyr::pivot_longer(res,cols = starts_with(var),names_to = "Label", values_to = var)
    p2 <- ggplot(res, aes(x = Model, y=!! rlang::sym(var),col = Model)) +
      geom_boxplot() +
      facet_grid(Label ~., scales = "free") + 
      scale_colour_brewer(palette = "Set2") + 
      ggtitle(paste0("Probability of SSB/SSB40% > 0.5"," (over last ",use.n.years," years)")) +
      ylab("Probability") +
      theme_bw()
    
    ggsave(file.path(main.dir,sub.dir,paste0("Overfished_last_",use.n.years,"years.PNG")), p2, width = 10, height = 7, dpi = dpi)
    
    # ------------------------------------------------------------------------
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
    res <- tidyr::pivot_longer(res,cols = starts_with(var),names_to = "Label", values_to = var)
    p3 <- ggplot(res, aes(x = Model, y=!! rlang::sym(var),col = Model)) +
      geom_boxplot() +
      facet_grid(Label ~., scales = "free") + 
      scale_colour_brewer(palette = "Set2") + 
      ggtitle(paste0("F/F40%"," (over last ",use.n.years," years)")) +
      ylab("F/F40%") +
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
    res <- tidyr::pivot_longer(res,cols = starts_with(var),names_to = "Label", values_to = var)
    p4 <- ggplot(res, aes(x = Model, y=!! rlang::sym(var),col = Model)) +
      geom_boxplot() +
      facet_grid(Label ~., scales = "free") + 
      scale_colour_brewer(palette = "Set2") + 
      ggtitle(paste0("Probability of F/F40% > 1"," (over last ",use.n.years," years)")) +
      ylab("Probability") +
      theme_bw()
    ggsave(file.path(main.dir,sub.dir,paste0("Overfishing_last_",use.n.years,"years.PNG")), p4, width = 10, height = 7, dpi = dpi)
    
  }
  
  p <- ggpubr::ggarrange(p1,p2,p3,p4,common.legend = TRUE,legend = "right")
  ggsave(file.path(main.dir,sub.dir,paste0("Stock_Status_Performance_last_",use.n.years,"years.PNG")), p, width = 15, height = 10, dpi = dpi)
  
  # ----------------------------------------------------------------------------
  # -------------------------------Radar Chart ---------------------------------
  # ----------------------------------------------------------------------------
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
  
  # var = "SSB"
  # res = NULL
  # for (i in 1:length(mods)){
  #   tmp <- extract_var(mods[[i]],var)
  #   if (is.null(use.n.years)) {
  #     tmp <- tail(tmp,5)
  #   } else {
  #     tmp <- tail(tmp,use.n.years)
  #   }
  #   tmp <- data.frame(t(colSums(tmp)))
  #   tmp$nsim <- i
  #   tmp$Model <- paste("Model",j)
  #   res <- rbind(res, tmp)
  # }
  # res1 <- tidyr::pivot_longer(res,cols = starts_with(var),names_to = "Index", values_to = "Value")
  
  
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
  res1 <- tidyr::pivot_longer(res,cols = starts_with(var),names_to = "Index", values_to = "Value")
  
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
  res2 <- tidyr::pivot_longer(res,cols = starts_with(var),names_to = "Index", values_to = "Value")
  
  # res2 <- res2 %>%
  #   # Specify group indicator, column, function
  #   group_by(Model) %>%
  #   # Calculate the mean of the "Frequency" column for each group
  #   summarise_at(vars(Catch_r),
  #                list(Catch = mean))
  
  # Reference points and stock status
  res = NULL
  for (i in 1:length(mods)){
    for (j in 1:length(mods[[1]])){
      tmp <- mods[[i]][[j]]$om$rep$SSB
      tmp <- cbind(tmp,rowSums(tmp))
      tmp <- tmp/exp(mods[[i]][[j]]$om$rep$log_SSB_FXSPR)
      if (is.null(use.n.years)) {
        tmp <- tail(tmp,5)
        tmp <- apply(tmp,2,function(x) sum(x < 1)/5)
      } else {
        tmp <- tail(tmp,use.n.years)
        tmp <- apply(tmp,2,function(x) sum(x < 1)/use.n.years)
      }
      # tmp[which(tmp == 0)] = 0.01
      tmp <- data.frame(t(tmp))
      names(tmp) <- paste0("Not_Overfished_s",1:ncol(tmp))
      names(tmp)[ncol(tmp)] <- "Not_Overfished_Global"
      tmp$nsim <- i
      tmp$Model <- paste("Model",j)
      res <- rbind(res, tmp)
    }
  }
  var = "Not_Overfished"
  res3 <- tidyr::pivot_longer(res,cols = starts_with(var),names_to = "Index", values_to = "Value")
  # names = unique(res3$Label)
  # for (name in names) {
  #   res <- filter(res3,Label == name) %>%
  #     # Specify group indicator, column, function
  #     group_by(Model) %>%
  #     # Calculate the mean of the "Frequency" column for each group
  #     summarise_at(vars(`SSB/SSB40%`),
  #                  list(Index = mean)) %>% 
  #     data.frame()
  #   dat = cbind(dat,res)
  # }
  # res3 <- res3 %>%
  #   # Specify group indicator, column, function
  #   group_by(Model) %>%
  #   # Calculate the mean of the "Frequency" column for each group
  #   summarise_at(vars(`SSB/SSB40%`),
  #                list(Prob.not.Overfished = mean))
  
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
      names(tmp) <- paste0("Not_Overfishing_r",1:ncol(tmp))
      names(tmp)[ncol(tmp)] <- "Not_Overfishing_Global"
      if (is.null(use.n.years)) {
        tmp <- tail(tmp,5)
        tmp <- apply(tmp,2,function(x) sum(x < 1)/5)
      } else {
        tmp <- tail(tmp,use.n.years)
        tmp <- apply(tmp,2,function(x) sum(x < 1)/use.n.years)
      }
      # tmp[which(tmp == 0)] = 0.01
      tmp <- data.frame(t(tmp))
      names(tmp) <- paste0("Not_Overfishing_r",1:ncol(tmp))
      names(tmp)[ncol(tmp)] <- "Not_Overfishing_Global"
      tmp$nsim <- i
      tmp$Model <- paste("Model",j)
      res <- rbind(res, tmp)
    }
  }
  var = "Not_Overfishing"
  res4 <- tidyr::pivot_longer(res,cols = starts_with(var),names_to = "Index", values_to = "Value")
  # res4 <- res4 %>%
  #   # Specify group indicator, column, function
  #   group_by(Model) %>%
  #   # Calculate the mean of the "Frequency" column for each group
  #   summarise_at(vars(`F/F40%`),
  #                list(Prob.not.Overfishing = mean))
  
  # define the "true" model!
  res = rbind(res1,res2,res3,res4)
  
  df_agg <- res %>% 
    group_by(Model, Index) %>% 
    summarise(Value = mean(Value))
  # Reshape data to wide format
  df <- data.frame(spread(df_agg, key = Index, value = "Value"))
  
  rownames(df) <- df$Model
  
  for (i in 2:ncol(df)){
    tmp <- df[,i]
    if (max(tmp) == min(tmp)) {
      df[,i] = NA
    }
  }
  
  # max_values = apply(df[,-1],2,max)
  # min_values = apply(df[,-1],2,min)
  # Plot radar plots for each model and combine them
  tiff(file.path(main.dir,sub.dir,paste0("Radar_chart_last_",use.n.years,"years.tiff")),width = 7, height = 7, units = "in", res = dpi)
  radarchart(
    df[,-1], axistype=0 , maxmin=F,
    #custom polygon
    plwd=4 , plty=1, 
    #custom the grid
    cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8, 
    #custom labels
    vlcex=0.8 )
  legend(x=-1.4, y=1.2, legend = rownames(df), bty = "n", pch=20, col=1:8, text.col = "grey", cex=0.8, pt.cex=2)
  dev.off()
  
  cat("Report is done! \n")
  cat(paste0("Report has been save in ",file.path(main.dir,sub.dir)))
}
