getMAPE <- function(pred){
  MeanAPE <- mean(abs(pred$quantity - pred$estimated_quantity)/pred$quantity)*100
  MedianAPE <- median(abs(pred$quantity - pred$estimated_quantity)/pred$quantity)*100
  nbr <- nrow(unique(pred[attribute]))
  return(c(MeanAPE, MedianAPE, nbr))
}
weightedMAPE <- function(MAPEdf){
  MedianAPE <- sum(MAPEdf$n * MAPEdf$MedianAPE)/sum(MAPEdf$n)
  MeanAPE <- sum(MAPEdf$n * MAPEdf$MeanAPE)/sum(MAPEdf$n)
  return(c(MeanAPE, MedianAPE))
}

getMapeDensity <- function(mape, segmentedMAPE = TRUE, segmentedModel = FALSE){
  if(segmentedModel){
    MAPEdf <- mclapply(mape, function(x) {
      x %<>% 
        do.call('rbind', .) %>% 
        as.data.frame() %>% 
        mutate(segment = rownames(.))
      colnames(x)[1:3] <- c('MeanAPE', 'MedianAPE', 'n')
      x$segment <- as.factor(x$segment)
      return(x)}, mc.cores = 4)
  
    
    MAPE_seg <-  do.call('rbind', MAPEdf)
    
    
    MAPE_unseg <- mclapply(MAPEdf, function(x) weightedMAPE(x), mc.cores = 8)
    MAPE_unseg %<>% do.call('rbind',.) %>% as.data.frame()
    colnames(MAPE_unseg) <- c('MeanAPE', 'MedianAPE')
      
    if(segmentedMAPE){
      plot <- ggplot(MAPE_seg, aes(x = MedianAPE)) +
        geom_density(aes(group = segment, colour = segment, fill = segment), alpha = 0.2) +
        geom_density(data = MAPE_unseg, aes(x = MedianAPE), alpha = 0.3, show.legend = FALSE)
    }else{
      plot <- ggplot(stack(MAPE_unseg), aes(x=values)) + 
        geom_density(aes(group=ind, colour=ind, fill=ind), alpha=0.3)
    }
  }else{
    MAPEdf <- mape %>% 
      do.call('rbind', .) %>% 
      as.data.frame()
    colnames(MAPEdf)[1:3] <- c('MeanAPE', 'MedianAPE', 'n')
    plot <- ggplot(stack(MAPEdf[,1:2]), aes(x=values)) + 
      geom_density(aes(group=ind, colour=ind, fill=ind), alpha=0.3)
  }
  return(plot)
}