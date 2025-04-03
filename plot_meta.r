plot_meta <- function(data = data,
                      shape_range = c(1, 4),
                      #para for plot_1
                      size_plot_1 = 4,
                      size_plot_1_title = 5,
                      pos = c(3, 3.5, 4, 4.5),
                      sub_title = c("Int +", "Int -", "Int +", "Int -"),
                      #par for plot 2
                      diamonds_high = 0.2,
                      legend_pos = c(0.8, 0.5),
                      legend_key_size = unit(0.3, 'cm'),
                      x_lim = NULL,
                      x_break = NULL,
                      dodge_width = 0.9,
                      #trunc--------
                      expand = c(0.0001, 0.0001),
                      arrow_adj = 0.05, #arrow cover the line
                      arrow_add = arrow(length=unit(0.25,"cm"),
                                    ends="last",
                                    type = "closed"), 
                      y_lim_low = 0.2, 
                      up_gap = 0.4,
                      #par for arrage
                      width = c(4, 3, 1.5, 1.5),
                      figure = FALSE,
                      figure_width = 14.65,
                      figure_hight = 8.11,
                      path = NULL
){
  print(unique(data$outcome))
  print(unique(data$cat))
  print(unique(data$gender))
  
  #para----
  size_plot_1_comb <- size_plot_1/1.6
  #data for plot------------------
  #extraxt the eff-------
  dat_plot <- bind_rows(data$res[data$adjusted == FALSE][[1]]$meta_table, 
                        data$res[data$adjusted == TRUE][[1]]$meta_table)
  dat_plot <- dat_plot %>% arrange(country)
  country_order <- rev(unique(dat_plot$country))
  
  dat_plot[c(nrow(dat_plot) + 1, nrow(dat_plot) + 2), ] <- NA
  dat_plot$country[which(is.na(dat_plot$country))] <- c(" ", "  ")
  dat_plot$adjusted[which(is.na(dat_plot$adjusted))] <- c(FALSE, TRUE)
  
  dat_plot <- dat_plot %>% mutate(
    value = as.numeric(value),
    value_low = as.numeric(value_low),
    value_high = as.numeric(value_high),
    shape_size = scales::rescale(1/sei, to = shape_range)
  )
  
  x_name <- na.omit(unique(dat_plot$cat))
  x_name <- ifelse(str_fun(x_name, "ratio"), "Risk ratio", "Risk difference")
  
  #extract the meta results------
  dat_plot_2 <- bind_rows(data$res[data$adjusted == FALSE][[1]]$res_meta, 
                          data$res[data$adjusted == TRUE][[1]]$res_meta)
  
 if(x_name == "Risk ratio"){
   dat_plot_2 <- dat_plot_2 %>% mutate(
     overall = exp(as.numeric(overall)),
     overall_low = exp(as.numeric(overall_low)),
     overall_high = exp(as.numeric(overall_high))
   )
 }
  
  if(x_name == "Risk difference"){
    dat_plot_2 <- dat_plot_2 %>% mutate(
      overall = as.numeric(overall)*100,
      overall_low = as.numeric(overall_low)*100,
      overall_high = as.numeric(overall_high)*100
    )
  }
  dat_plot_2$adjusted <- c(FALSE, TRUE)
  
  #add the meta reuslt to dat_plot-----
  dat_plot <- dat_plot %>% mutate(
    comb = case_when(
      is.na(comb) & adjusted == FALSE ~ dat_plot_2$overall_comb[dat_plot_2$adjusted == FALSE],
      is.na(comb) & adjusted == TRUE ~ dat_plot_2$overall_comb[dat_plot_2$adjusted == TRUE],
      TRUE ~ comb
    )
  )
  
  dat_plot <- dat_plot %>% mutate(
    country = factor(country, levels = c("  ", " ", country_order))
  )
  
  #comb the descri infor------
  dat_plot_2 <- dat_plot_2 %>% mutate(
    adj = ifelse(adjusted == TRUE, "adjusted", "unadjusted")
  )
  dat_plot_2 <- dat_plot_2 %>% mutate(
    
    lab = str_glue("Test for heterogeneity: tau^2{tau_square}; chi^2={chi_square}, df={df}, P{p_value_1}; I^2{i_square}
                 Test for overall {adj} {str_to_lower(method)}: Z={z_value}, P{p_value_2}")
  )
  #browser()
  truncated <- !is.null(x_lim) 
  if(truncated == TRUE){
    truncated <- x_lim[2] > max(mdat$value_high, na.rm = TRUE)
  }
  
  
  if(truncated == FALSE){
    #plot_1------------------
    plot_1 <- ggplot() + 
      geom_text(data = dat_plot, aes(x = 1, y = country, label = country), 
                hjust = 0, size = size_plot_1) + 
      geom_text(data = dat_plot, aes(x = pos[1], y = country, label = n_EXP),
                size = size_plot_1) + 
      geom_text(data = dat_plot, aes(x = pos[2], y = country, label = N_EXP - n_EXP), 
                size = size_plot_1) + 
      geom_text(data = dat_plot, aes(x = pos[3], y = country, label = n_noEXP), 
                size = size_plot_1) + 
      geom_text(data = dat_plot, aes(x = pos[4], y = country, label = N_noEXP - n_noEXP), 
                size = size_plot_1) + 
      geom_hline(yintercept = 2.5) + 
      geom_hline(yintercept = 1.5, linetype = 2) + 
      geom_text(data = dat_plot_2[dat_plot_2$adjusted == FALSE, ], 
                aes(x = 1, y = 2, label = lab), hjust = 0, size = size_plot_1_comb) +
      geom_text(data = dat_plot_2[dat_plot_2$adjusted == TRUE, ], 
                aes(x = 1, y = 1, label = lab), hjust = 0, size = size_plot_1_comb) +
      scale_x_continuous(breaks = c(1, pos),
                         label = c("", "Int +", "Int -", "Int +", "Int -")) + 
      theme_classic() + 
      theme(axis.line.y = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.x = element_text(colour = "white"),
            axis.title.x = element_text(colour = "white"),
            axis.ticks.x = element_line(colour = "white"),
            plot.margin = unit(c(0, 0, 0, 0), "null"),
            panel.margin = unit(c(0, 0, 0, 0), "null"))
    
    plot_1_title <- ggplot() + 
      geom_text(data = dat_plot, aes(x = 1, y = 1, label = "Country"), 
                hjust = 0, size = size_plot_1_title, vjust = -1, fontface = "bold") + 
      geom_text(data = dat_plot, aes(x = pos[1], y = 1, label = sub_title[1]), vjust = -1) + 
      geom_text(data = dat_plot, aes(x = pos[2], y = 1, label = sub_title[2]), vjust = -1) + 
      geom_text(data = dat_plot, aes(x = sum(pos[c(1, 2)])/2, y = 1.5, label = "Disabled"), 
                size = size_plot_1_title, vjust = 1, fontface = "bold") + 
      geom_text(data = dat_plot, aes(x = pos[3], y = 1, label = sub_title[3]), vjust = -1) + 
      geom_text(data = dat_plot, aes(x = pos[4], y = 1, label = sub_title[4]), vjust = -1) + 
      geom_text(data = dat_plot, aes(x = sum(pos[c(3, 4)])/2, y = 1.5, label = "Non-disabled"), 
                size = size_plot_1_title, vjust = 1, fontface = "bold") + 
      theme_classic() + 
      theme(axis.line.y = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            plot.margin = unit(c(0, 0, 0, 0), "null"),
            panel.margin = unit(c(0, 0, 0, 0), "null"))
    
    #plot_2------------------
    dat_dia_false <- data.table(
      x = c(dat_plot_2$overall_low[dat_plot_2$adjusted == FALSE], 
            dat_plot_2$overall[dat_plot_2$adjusted == FALSE], 
            dat_plot_2$overall_high[dat_plot_2$adjusted == FALSE], 
            dat_plot_2$overall[dat_plot_2$adjusted == FALSE]), 
      y = c(2, 2+ diamonds_high, 2, 2-diamonds_high),
      adjusted = "Unadjusted"
    )
    
    dat_dia_true <- data.table(
      x = c(dat_plot_2$overall_low[dat_plot_2$adjusted == TRUE], 
            dat_plot_2$overall[dat_plot_2$adjusted == TRUE], 
            dat_plot_2$overall_high[dat_plot_2$adjusted == TRUE], 
            dat_plot_2$overall[dat_plot_2$adjusted == TRUE]), 
      y = c(1, 1+ diamonds_high, 1, 1-diamonds_high),
      adjusted = "Adjusted"
    )
    
    mdat <- dat_plot %>% mutate(
      adjusted = factor(adjusted, levels = c(TRUE, FALSE), labels = c("Adjusted", "Unadjusted"))
    )
    
    xintercept <- ifelse(x_name == "Risk ratio", 1, 0)
    
    message(str_glue("Range of effect value: {str_c(range(mdat$value, na.rm = TRUE), collapse = '-')}"))
    message(str_glue("Range of 95%CI of effect value: {str_c(range(c(mdat$value_low, mdat$value_high), na.rm = TRUE), collapse = '-')}"))
    
    # cat(str_c(str_c(rep("-", times = level + 1), collapse = ""),
    #           glue_col("{blue '{res}' not in the {dataset}}\n\n")))
    
    
      plot_2 <- ggplot(data = mdat, aes(x = value, 
                                        y = country, 
                                        colour = adjusted, 
                                        fill = adjusted)) + 
        geom_vline(xintercept = xintercept, linetype = 2, colour= "grey50") +
        geom_point(size = mdat$shape_size,
                   position = position_dodge(width = dodge_width), shape = 15) +
        # geom_linerange(aes(xmin = value_low, xmax = value_high,
        #                  y = country),
        #              position = position_dodge(width = 0.90), linetyple = 2) +
        # geom_segment(aes(x = value_low, xend = value_high,
        #                    y = country, yend = country),
        #                position = position_dodge(width = 0.90), linetyple = 2) +
        geom_errorbarh( aes(xmin = value_low, xmax = value_high),
                        height = 0.3,
                        position = position_dodge(width = dodge_width)) +
        geom_polygon(data = dat_dia_false, aes(x = x, y = y)) + 
        geom_polygon(data = dat_dia_true, aes(x = x, y = y)) + 
        geom_hline(yintercept = 2.5) + 
        scale_x_continuous(limits = x_lim, 
                           breaks = x_break,
                           #oob = scales::oob_squish,
                           name = x_name) + 
        scale_colour_manual(values = pal_nejm("default")(8)[c(1, 2)], name = "") + 
        scale_fill_manual(values = pal_nejm("default")(8)[c(1, 2)], name = "") + 
        theme_classic() + 
        theme(axis.line.y = element_blank(),
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              legend.position = legend_pos,
              legend.key.size = legend_key_size,
              #legend.background = 
              plot.margin = unit(c(0, 0, 0, 0), "null"),
              panel.margin = unit(c(0, 0, 0, 0), "null"))
    
    plot_2_title <- ggplot() + 
      geom_text(data = dat_plot, aes(x = 1, y = 1, label = " ")) + 
      theme_classic() + 
      theme(axis.line.y = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            plot.margin = unit(c(0, 0, 0, 0), "null"),
            panel.margin = unit(c(0, 0, 0, 0), "null"))
    
    #plot_3------------------
    mdat <- dat_plot %>% filter(adjusted == FALSE) %>% select(country, comb, adjusted)
    mdat <- rbind(mdat, data.frame(country = "  ", comb = "-", adjusted = FALSE))
    plot_3 <- ggplot() + 
      geom_text(data = mdat, 
                aes(x = 1, y = country, label = comb), size = size_plot_1) + 
      geom_hline(yintercept = 2.5) + 
      theme_classic() + 
      theme(axis.line.y = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.x = element_text(colour = "white"),
            axis.title.x = element_text(colour = "white"),
            axis.ticks.x = element_line(colour = "white"),
            plot.margin = unit(c(0, 0, 0, 0), "null"),
            panel.margin = unit(c(0, 0, 0, 0), "null"))
    
    label_3 <- ifelse(x_name == "Risk ratio", 
                      "Unadjusted risk ratio\n[95% CI]", 
                      "Unadjusted risk difference\n[95% CI]")
    
    plot_3_title <- ggplot() + 
      geom_text(data = dat_plot, aes(x = 1, y = 1, label = label_3), 
                size = size_plot_1_title, fontface = "bold") + 
      theme_classic() + 
      theme(axis.line.y = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            plot.margin = unit(c(0, 0, 0, 0), "null"),
            panel.margin = unit(c(0, 0, 0, 0), "null"))
    
    #plot_4------------------
    mdat <- dat_plot %>% filter(adjusted == TRUE) %>% select(country, comb, adjusted)
    mdat <- rbind(mdat, data.frame(country = " ", comb = "-", adjusted = TRUE))
    plot_4 <- ggplot() + 
      geom_text(data = mdat, 
                aes(x = 1, y = country, label = comb), size = size_plot_1) + 
      geom_hline(yintercept = 2.5) + 
      theme_classic() + 
      theme(axis.line.y = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.x = element_text(colour = "white"),
            axis.title.x = element_text(colour = "white"),
            axis.ticks.x = element_line(colour = "white"),
            plot.margin = unit(c(0, 0, 0, 0), "null"),
            panel.margin = unit(c(0, 0, 0, 0), "null"))
    
    label_4 <- ifelse(x_name == "Risk ratio", 
                      "Adjusted risk ratio\n[95% CI]", 
                      "Adjusted risk difference\n[95% CI]")
    
    plot_4_title <- ggplot() + 
      geom_text(data = dat_plot, aes(x = 1, y = 1, label = label_4), 
                size = size_plot_1_title, fontface = "bold") + 
      theme_classic() + 
      theme(axis.line.y = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            plot.margin = unit(c(0, 0, 0, 0), "null"),
            panel.margin = unit(c(0, 0, 0, 0), "null"))
    
  }
  
  if(truncated == TRUE){
    dat_plot <- dat_plot %>% mutate(
      trunc = ifelse(value_high > x_lim[2], TRUE, FALSE),
      value_high2 = ifelse(value_high > x_lim[2], x_lim[2], value_high),
      y = rev(c(1, 2, rep(3:100, each = 2))[1:nrow(dat_plot)]),
      y_a = ifelse(adjusted == FALSE, y + dodge_width/4, y - dodge_width/4),
      y_a = ifelse(y %in% c(1, 2), y, y_a)
    )
    
    
    #plot_1------------------
    plot_1 <- ggplot() + 
      geom_text(data = dat_plot, aes(x = 1, y = y, label = country), 
                hjust = 0, size = size_plot_1) + 
      geom_text(data = dat_plot, aes(x = pos[1], y = y, label = n_EXP),
                size = size_plot_1) + 
      geom_text(data = dat_plot, aes(x = pos[2], y = y, label = N_EXP - n_EXP), 
                size = size_plot_1) + 
      geom_text(data = dat_plot, aes(x = pos[3], y = y, label = n_noEXP), 
                size = size_plot_1) + 
      geom_text(data = dat_plot, aes(x = pos[4], y = y, label = N_noEXP - n_noEXP), 
                size = size_plot_1) + 
      geom_hline(yintercept = 2.5) + 
      geom_hline(yintercept = 1.5, linetype = 2) + 
      geom_text(data = dat_plot_2[dat_plot_2$adjusted == FALSE, ], 
                aes(x = 1, y = 2, label = lab), hjust = 0, size = size_plot_1_comb) +
      geom_text(data = dat_plot_2[dat_plot_2$adjusted == TRUE, ], 
                aes(x = 1, y = 1, label = lab), hjust = 0, size = size_plot_1_comb) +
      scale_x_continuous(breaks = c(1, pos),
                         label = c("", "Int +", "Int -", "Int +", "Int -")) + 
      scale_y_continuous(expand = expand, 
                         limits = c(y_lim_low, max(dat_plot$y)+up_gap)) + 
      theme_classic() + 
      theme(axis.line.y = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.x = element_text(colour = "white"),
            axis.title.x = element_text(colour = "white"),
            axis.ticks.x = element_line(colour = "white"),
            plot.margin = unit(c(0, 0, 0, 0), "null"),
            panel.margin = unit(c(0, 0, 0, 0), "null"))
    
    plot_1_title <- ggplot() + 
      geom_text(data = dat_plot, aes(x = 1, y = 1, label = "Country"), 
                hjust = 0, size = size_plot_1_title, vjust = -1, fontface = "bold") + 
      geom_text(data = dat_plot, aes(x = pos[1], y = 1, label = sub_title[1]), vjust = -1) + 
      geom_text(data = dat_plot, aes(x = pos[2], y = 1, label = sub_title[2]), vjust = -1) + 
      geom_text(data = dat_plot, aes(x = sum(pos[c(1, 2)])/2, y = 1.5, label = "Disabled"), 
                size = size_plot_1_title, vjust = 1, fontface = "bold") + 
      geom_text(data = dat_plot, aes(x = pos[3], y = 1, label = sub_title[3]), vjust = -1) + 
      geom_text(data = dat_plot, aes(x = pos[4], y = 1, label = sub_title[4]), vjust = -1) + 
      geom_text(data = dat_plot, aes(x = sum(pos[c(3, 4)])/2, y = 1.5, label = "Non-disabled"), 
                size = size_plot_1_title, vjust = 1, fontface = "bold") + 
      theme_classic() + 
      theme(axis.line.y = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            plot.margin = unit(c(0, 0, 0, 0), "null"),
            panel.margin = unit(c(0, 0, 0, 0), "null"))
    
    #plot_2------------------
    dat_dia_false <- data.table(
      x = c(dat_plot_2$overall_low[dat_plot_2$adjusted == FALSE], 
            dat_plot_2$overall[dat_plot_2$adjusted == FALSE], 
            dat_plot_2$overall_high[dat_plot_2$adjusted == FALSE], 
            dat_plot_2$overall[dat_plot_2$adjusted == FALSE]), 
      y = c(2, 2+ diamonds_high, 2, 2-diamonds_high),
      adjusted = "Unadjusted"
    )
    
    dat_dia_true <- data.table(
      x = c(dat_plot_2$overall_low[dat_plot_2$adjusted == TRUE], 
            dat_plot_2$overall[dat_plot_2$adjusted == TRUE], 
            dat_plot_2$overall_high[dat_plot_2$adjusted == TRUE], 
            dat_plot_2$overall[dat_plot_2$adjusted == TRUE]), 
      y = c(1, 1+ diamonds_high, 1, 1-diamonds_high),
      adjusted = "Adjusted"
    )
    
    mdat <- dat_plot %>% mutate(
      adjusted = factor(adjusted, levels = c(TRUE, FALSE), labels = c("Adjusted", "Unadjusted"))
    )
    
    xintercept <- ifelse(x_name == "Risk ratio", 1, 0)
    
    message(str_glue("Range of effect value: {str_c(range(mdat$value, na.rm = TRUE), collapse = '-')}"))
    message(str_glue("Range of 95%CI of effect value: {str_c(range(c(mdat$value_low, mdat$value_high), na.rm = TRUE), collapse = '-')}"))
    
    # cat(str_c(str_c(rep("-", times = level + 1), collapse = ""),
    #           glue_col("{blue '{res}' not in the {dataset}}\n\n")))
    
    dat_arrow <- mdat %>% filter(trunc == TRUE) %>% mutate(
      value_high2 = value_high2 + arrow_adj
    )
   #browser()
    plot_2 <- ggplot(data = mdat, aes(x = value, 
                                      y = y_a, 
                                      colour = adjusted, 
                                      fill = adjusted)) + 
      geom_vline(xintercept = xintercept, linetype = 2, colour= "grey50") +
      geom_point(size = mdat$shape_size,
                 #position = position_dodge(width = dodge_width), 
                 shape = 15) +
      
      geom_segment(data = dat_arrow, aes(x = value, xend = value_high2,
                                         y = y_a, yend = y_a),
                   #position = position_dodge(width = 0.90), 
                   #linetyple = 2,
                   arrow = arrow_add) +
      
      geom_errorbarh(aes(xmin = value_low, xmax = value_high2),
                     #position = position_dodge(width = dodge_width)
                     height = 0.3) +
      geom_polygon(data = dat_dia_false, aes(x = x, y = y)) + 
      geom_polygon(data = dat_dia_true, aes(x = x, y = y)) + 
      geom_hline(yintercept = 2.5) + 
      scale_x_continuous(limits = c(x_lim[1], x_lim[2] + arrow_adj), 
                         breaks = x_break,
                         #oob = scales::oob_squish,
                         name = x_name) + 
      scale_y_continuous(expand = expand, #breaks = c(0:100), 
                         limits = c(y_lim_low, max(mdat$y)+up_gap)) + 
      scale_colour_manual(values = pal_nejm("default")(8)[c(1, 2)], name = "") + 
      scale_fill_manual(values = pal_nejm("default")(8)[c(1, 2)], name = "") + 
      theme_classic() + 
      theme(axis.line.y = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            legend.position = legend_pos,
            legend.key.size = legend_key_size,
            #legend.background = 
            plot.margin = unit(c(0, 0, 0, 0), "null"),
            panel.margin = unit(c(0, 0, 0, 0), "null"))
    
    
    plot_2_title <- ggplot() + 
      geom_text(data = dat_plot, aes(x = 1, y = 1, label = " ")) + 
      theme_classic() + 
      theme(axis.line.y = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            plot.margin = unit(c(0, 0, 0, 0), "null"),
            panel.margin = unit(c(0, 0, 0, 0), "null"))
    
    #plot_3------------------
    mdat <- dat_plot %>% filter(adjusted == FALSE) %>% select(y, comb, adjusted)
    mdat <- rbind(mdat, data.frame(y = 1, comb = "-", adjusted = FALSE))
    plot_3 <- ggplot() + 
      geom_text(data = mdat, 
                aes(x = 1, y = y, label = comb), size = size_plot_1) + 
      geom_hline(yintercept = 2.5) + 
      scale_y_continuous(expand = expand, #breaks = c(0:100), 
                         limits = c(y_lim_low, max(mdat$y)+up_gap)) + 
      theme_classic() + 
      theme(axis.line.y = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.x = element_text(colour = "white"),
            axis.title.x = element_text(colour = "white"),
            axis.ticks.x = element_line(colour = "white"),
            plot.margin = unit(c(0, 0, 0, 0), "null"),
            panel.margin = unit(c(0, 0, 0, 0), "null"))
    
    label_3 <- ifelse(x_name == "Risk ratio", 
                      "Unadjusted risk ratio\n[95% CI]", 
                      "Unadjusted risk difference\n[95% CI]")
    
    plot_3_title <- ggplot() + 
      geom_text(data = dat_plot, aes(x = 1, y = 1, label = label_3), 
                size = size_plot_1_title, fontface = "bold") + 
      theme_classic() + 
      theme(axis.line.y = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            plot.margin = unit(c(0, 0, 0, 0), "null"),
            panel.margin = unit(c(0, 0, 0, 0), "null"))
    
    #plot_4------------------
    mdat <- dat_plot %>% filter(adjusted == TRUE) %>% select(y, comb, adjusted)
    mdat <- rbind(mdat, data.frame(y = 2, comb = "-", adjusted = TRUE))
    plot_4 <- ggplot() + 
      geom_text(data = mdat, 
                aes(x = 1, y = y, label = comb), size = size_plot_1) + 
      geom_hline(yintercept = 2.5) + 
      scale_y_continuous(expand = expand, 
                         limits = c(y_lim_low, max(mdat$y)+up_gap)) + 
      theme_classic() + 
      theme(axis.line.y = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.x = element_text(colour = "white"),
            axis.title.x = element_text(colour = "white"),
            axis.ticks.x = element_line(colour = "white"),
            plot.margin = unit(c(0, 0, 0, 0), "null"),
            panel.margin = unit(c(0, 0, 0, 0), "null"))
    
    label_4 <- ifelse(x_name == "Risk ratio", 
                      "Adjusted risk ratio\n[95% CI]", 
                      "Adjusted risk difference\n[95% CI]")
    
    plot_4_title <- ggplot() + 
      geom_text(data = dat_plot, aes(x = 1, y = 1, label = label_4), 
                size = size_plot_1_title, fontface = "bold") + 
      theme_classic() + 
      theme(axis.line.y = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            plot.margin = unit(c(0, 0, 0, 0), "null"),
            panel.margin = unit(c(0, 0, 0, 0), "null"))
    
  }

  #arrange--------------
  n_1 <- 2
  n_2 <- nrow(dat_plot %>% filter(adjusted == FALSE)) + 3
  gridExtra::grid.arrange(plot_1_title, plot_2_title, plot_3_title, plot_4_title, 
                          plot_1, plot_2, plot_3, plot_4, 
                          widths = width/sum(width),
                          heights = c(n_1, n_2)/(n_1 + n_2),
                          nrow = 2)
  g <- gridExtra::arrangeGrob(plot_1_title, plot_2_title, plot_3_title, plot_4_title, 
                               plot_1, plot_2, plot_3, plot_4, 
                               widths = width/sum(width),
                               heights = c(n_1, n_2)/(n_1 + n_2),
                               nrow = 2)
  figure_hight = sum(n_1, n_2)/26 * figure_hight
  
  if(figure == TRUE){
    outc <- na.omit(unique(dat_plot$outcome))
    if ("gender" %in% names(dat_plot)) {
      gender <- na.omit(unique(dat_plot$gender))
      ggsave(g, file = str_glue("{outc}_{gender}_{x_name}.jpg"), 
             width = figure_width, 
             height = figure_hight, dpi = 300, path = path)
    }
    
    if (!"gender" %in% names(dat_plot)) {
      gender <- na.omit(unique(dat_plot$gender))
      ggsave(g, file = str_glue("{outc}_all_{x_name}.jpg"), 
             width = figure_width, 
             height = figure_hight, dpi = 300, path = path)
    }
  }
  
}