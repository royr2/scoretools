#' Print method for "ks" class
#' @export

print.ks <- function(tab){

  max_ks <- attr(tab, "max_ks")
  bin <- attr(tab, "decile")

  cat("\n")

  tab %>%
    mutate(S.No = row_number()) %>%
    select(S.No, Bins, Total, Events, Pop_Pct, Event.Rate, KS,
           Capture.Rate, Cum.Event.Rate) %>%
    rename("Total\n(#)" = Total,
           "Events\n(#)" = Events,
           "Pop\n(%)" = "Pop_Pct",
           "Event Rate\n(%)" = Event.Rate,
           "Cum. Event Rate\n(%)" = "Cum.Event.Rate",
           "Capture Rate\n(%)" = "Capture.Rate") %>%

    # Hux table
    as_hux() %>%
    set_all_padding(10) %>%
    set_top_border(row = 1, value = 1) %>%
    set_top_border(row = 2, value = 1) %>%
    set_col_width(c(0.1, 0.2, rep(0.15, 7))) %>%
    set_position("center") %>%
    set_cell_properties(1, 1, align = "center") %>%
    set_cell_properties(bin + 1, 7, background_color = "yellow") %>%
    set_cell_properties(bin + 1, 7, text_color = "black") %>%
    set_cell_properties(bin + 1, 7, bold = T) %>%
    set_caption(paste0("Gains Table (modeling for ", attr(tab, "type"), ")")) %>%
    set_caption_pos("top") %>%

    add_footnote(paste0(
      "Max KS: ", max_ks, " (bin #", bin, ")",
      "   AUROC: ", attr(tab, "roc"),
      "   Gini: ", attr(tab, "gini")
    )) %>%
    print(colnames = F)
}

#' Plot method for class 'ks'
#' @export

plot.ks <- function(obj, font_size = 14){
  gains_chart(obj, font_size = font_size)
}


