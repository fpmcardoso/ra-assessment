plot_pred_type_distribution <- function(df, threshold) {
        v <- rep(NA, nrow(df))
        v <- ifelse(df$pr >= threshold & df$y == "Fraud", "True Positive", v)
        v <- ifelse(df$pr >= threshold & df$y == "No Fraud", "False Positive", v)
        v <- ifelse(df$pr < threshold & df$y == "Fraud", "False Negative", v)
        v <- ifelse(df$pr < threshold & df$y == "No Fraud", "True Negative", v)
        
        df$pred_type <- v
        colors <- c("#ca0020", "#f4a582", "#bababa", "#404040")
        ggplot(data=df, aes(x=y, y=pr)) + 
                geom_violin(fill=rgb(1,1,1,alpha=0.6), color=NA) + 
                geom_jitter(aes(color=pred_type, size = 3), alpha=0.6) +
                geom_hline(yintercept=threshold, color="black", alpha=0.6) +
                scale_color_manual(values = colors, name = "Prediction\nType") +
                labs(title=sprintf("Threshold at %.2f", threshold),
                     y = "Fraud Probability Rate",
                     x = "Inspection Result")+
                guides(size = FALSE)
}

plot_all_predictors <- function(df, title){
         ggplot(df,
                aes(x = location, fill = class)) +
                geom_bar() +
                facet_grid(phase ~ y ) +
                labs(title = title,
                     y = "Total",
                     x = "Location")
}