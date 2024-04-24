# -----------------------------------------------------------------------
plot_mse_output <- function(mods, main.dir = getwd(), use.n.years = 10, dpi = 72) {
  # main.dir = getwd()
  sub.dir <- 'Report'
  if (file.exists(sub.dir)){
  } else {
    dir.create(file.path(main.dir, sub.dir))
  }
  
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
  ggsave(file.path(main.dir,sub.dir,paste0(var,".PNG")), p1, width = 7, height = 5, dpi = dpi)
  
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
    ggtitle("Stock-specific F") +
    ylab("F") +
    theme_bw()
  ggsave(file.path(main.dir,sub.dir,paste0(var,".PNG")), p2, width = 7, height = 5, dpi = dpi)
  
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
  ggsave(file.path(main.dir,sub.dir,paste0(var,".PNG")), p3, width = 7, height = 5, dpi = dpi)
  
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
  ggsave(file.path(main.dir,sub.dir,paste0(var,".PNG")), p4, width = 7, height = 5, dpi = dpi)
  
  p <- ggpubr::ggarrange(p1,p2,p3,p4,common.legend = TRUE,legend = "right")
  ggsave(file.path(main.dir,sub.dir,"Performance_Metrics.PNG"), p, width = 10, height = 7, dpi = dpi)
  
  #-------------------------------------------------------------------------
  var = "SSB"
  res = NULL
  for (i in 1:length(mods)){
    tmp <- extract_var(mods[[i]],var)
    tmp$Model <- paste("Model",i)
    tmp$Year <- Years
    res <- rbind(res, tmp)
  }
  res <- tidyr::pivot_longer(res,cols = starts_with(var),names_to = "Label", values_to = var)
  
  p1 <- ggplot(res, aes(x = Model, y=!! rlang::sym(var),col = Model)) +
    geom_boxplot() +
    facet_grid(Label ~., scales = "free") + 
    scale_colour_brewer(palette = "Set2") + 
    ggtitle("SSB") +
    ylab("SSB") +
    theme_bw()
  ggsave(file.path(main.dir,sub.dir,paste0(var,"_boxplot.PNG")), p1, width = 7, height = 5, dpi = dpi)
  
  var = "Fbar"
  res = NULL
  for (i in 1:length(mods)){
    tmp <- extract_var(mods[[i]],var)
    tmp$Model <- paste("Model",i)
    tmp$Year <- Years
    res <- rbind(res, tmp)
  }
  res <- tidyr::pivot_longer(res,cols = starts_with(var),names_to = "Label", values_to = var)
  
  p2 <- ggplot(res, aes(x = Model, y=!! rlang::sym(var),col = Model)) +
    geom_boxplot() +
    facet_grid(Label ~., scales = "free") + 
    scale_colour_brewer(palette = "Set2") + 
    ggtitle("Stock-specific F") +
    ylab("F") +
    theme_bw()
  ggsave(file.path(main.dir,sub.dir,paste0(var,"_boxplot.PNG")), p2, width = 7, height = 5, dpi = dpi)
  
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
  
  p3 <- ggplot(res, aes(x = Model, y=!! rlang::sym(var),col = Model)) +
    geom_boxplot() +
    facet_grid(Label ~., scales = "free") + 
    scale_colour_brewer(palette = "Set2") + 
    ggtitle("Stock-specific Catch") +
    ylab("Catch") +
    theme_bw()
  ggsave(file.path(main.dir,sub.dir,paste0(var,"_boxplot.PNG")), p3, width = 7, height = 5, dpi = dpi)
  
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
  
  p4 <- ggplot(res, aes(x = Model, y=!! rlang::sym(var),col = Model)) +
    geom_boxplot() +
    facet_grid(Label ~., scales = "free") + 
    scale_colour_brewer(palette = "Set2") + 
    ggtitle("Region-specific Catch") +
    ylab("Catch") +
    theme_bw()
  
  ggsave(file.path(main.dir,sub.dir,paste0(var,"_boxplot.PNG")), p4, width = 7, height = 5, dpi = dpi)
  
  p <- ggpubr::ggarrange(p1,p2,p3,p4,common.legend = TRUE,legend = "right")
  ggsave(file.path(main.dir,sub.dir,"Performance_Metrics_boxplot.PNG"), p, width = 10, height = 7, dpi = dpi)
  
  #-------------------------------------------------------------------------
  var = "SSB"
  res = NULL
  for (i in 1:length(mods)){
    tmp <- extract_var(mods[[i]],var)
    if (is.null(use.n.years)) {
      tmp$Model <- paste("Model",i)
      tmp$Year <- Years
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
    ggtitle("SSB") +
    ylab("SSB") +
    theme_bw()
  ggsave(file.path(main.dir,sub.dir,paste0(var,"_last_",use.n.years,"years.PNG")), p1, width = 7, height = 5, dpi = dpi)
  
  var = "Fbar"
  res = NULL
  for (i in 1:length(mods)){
    tmp <- extract_var(mods[[i]],var)
    if (is.null(use.n.years)) {
      tmp$Model <- paste("Model",i)
      tmp$Year <- Years
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
    ggtitle("Stock-specific F") +
    ylab("F") +
    theme_bw()
  ggsave(file.path(main.dir,sub.dir,paste0(var,"_last_",use.n.years,"years.PNG")), p2, width = 7, height = 5, dpi = dpi)
  
  # ----------------------------------------------------------------------------
  var = "Catch_s"
  res = NULL
  for (i in 1:length(mods)){
    tmp <- extract_var(mods[[i]],var)
    if (is.null(use.n.years)) {
      tmp$Model <- paste("Model",i)
      tmp$Year <- Years
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
    ggtitle("Stock-specific Catch") +
    ylab("Catch") +
    theme_bw()
  ggsave(file.path(main.dir,sub.dir,paste0(var,"_last_",use.n.years,"years.PNG")), p3, width = 7, height = 5, dpi = dpi)
  
  #--------------------------------------------------------------------------------
  var = "Catch_r"
  res = NULL
  for (i in 1:length(mods)){
    tmp <- extract_var(mods[[i]],var)
    if (is.null(use.n.years)) {
      tmp$Model <- paste("Model",i)
      tmp$Year <- Years
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
    ggtitle("Region-specific Catch") +
    ylab("Catch") +
    theme_bw()
  
  ggsave(file.path(main.dir,sub.dir,paste0(var,"_last_",use.n.years,"years.PNG")), p4, width = 7, height = 5, dpi = dpi)
  
  p <- ggpubr::ggarrange(p1,p2,p3,p4,common.legend = TRUE,legend = "right")
  ggsave(file.path(main.dir,sub.dir,paste0("Performance_last_",use.n.years,"years.PNG")), p, width = 10, height = 7, dpi = dpi)
  
  # ---------------------------------------------------------------------------
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
  ggsave(file.path(main.dir,sub.dir,paste0(var,".PNG")), p1, width = 7, height = 5, dpi = dpi)
  
  
  cat("Report is done!")
}

