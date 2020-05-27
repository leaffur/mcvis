shinyServer(function(input, output, session) {

  output$variableTables = shiny::renderDataTable(
    cleanDigits(psych::describe(shiny_mcvis_result$X))

    )


  # highlight selected rows in the scatterplot
  output$ggplotOutput = renderPlot({
    s = input$variableTables_rows_selected
    # if(is.null(s)) {return(ggplot(shiny_mcvis_result))}

    g = shiny_mcvis_result$MC
    eig.max = ncol(g)
    vol.max = ncol(g)

    col.names = shiny_mcvis_result$col.names
    p = ncol(g)

    eig.max = min(p, eig.max)
    vol.max = min(p, vol.max)
    or = order(g[p,]) ## Order the columns of g by the smallest eigen value
    or = or[1:vol.max]
    g.or = g[,or]
    if (vol.max > 1) {g.or = g.or[p:(p-eig.max+1),]} else {g.or = as.matrix(g.or[p:(p-eig.max+1)])}
    if (eig.max == 1) {g.or = t(g.or)}
    g.or.subset = g.or

    matchingCols = match(colnames(g[,s, drop = FALSE]), colnames(g.or.subset))
    g.or.subset[, -matchingCols] = 1
    # ###############  ggplot #######################
    preDat = reshape2::melt(g.or.subset,
                            varnames = c("X2", "X1"),
                            value.name = "weights")

    thickness = 1 - preDat$weights


    ## Size category is the scale of the g-matrix.
    # sizeCategory5 = 1
    # sizeCategory5 = input$sizeCategory5
    sizeCategory5 = max(thickness)
    # sizeCategory5 = thickness[1]
    sizeCategory1 = 0.01*sizeCategory5
    sizeCategory2 = 0.5*sizeCategory5
    sizeCategory3 = 0.7*sizeCategory5
    sizeCategory4 = 0.9*sizeCategory5

    ggplotSizeManual = c(0, 0.1, 0.5, 1, 2)

    ggplotSizeCategory = dplyr::case_when(
      thickness <= sizeCategory1 ~ "category1",
      thickness <= sizeCategory2 ~ "category2",
      thickness <= sizeCategory3 ~ "category3",
      thickness <= sizeCategory4 ~ "category4",
      thickness <= sizeCategory5 ~ "category5",
      thickness > sizeCategory5 ~ "category5"
    )

    ggplotSizeCategory = factor(ggplotSizeCategory,
                                levels = paste0("category", 1:5))

    ggplotAlphaManual = c(0, 0.5, 0.5, 0.5, 1.0)

    dat = dplyr::mutate(
        preDat,
        thickness,
        ggplotSizeCategory
      )


    dat$x1_norm = rangeTransform(as.integer(dat$X1))
    dat$x2_norm = rangeTransform(as.integer(dat$X2))

    dat$y1 = 0
    dat$y2 = 1


    axis_1 = data.frame(x=rangeTransform(as.integer(unique(dat$X1))),
                        y=0, label=as.character(unique(dat$X1)))

    axis_2 = data.frame(x=rangeTransform(as.integer(unique(dat$X2))),
                        y=1, label=as.character(unique(dat$X2)))

    gg = ggplot2::ggplot(data=dat) +
      theme_bw() +
      theme(axis.title=element_blank()) +
      theme(axis.text=element_blank()) +
      theme(axis.ticks=element_blank()) +
      theme(panel.grid=element_blank()) +
      geom_segment(aes(x=x1_norm, xend=x2_norm, y=y1, yend=y2, colour = X2,
                       size = ggplotSizeCategory, alpha = ggplotSizeCategory)) +
      scale_size_manual(values = ggplotSizeManual, drop = FALSE) +
      # scale_alpha_continuous(range = c(0, 1)) +
      scale_alpha_manual(values = ggplotAlphaManual, drop = FALSE) +
      geom_segment(x=0, xend=1, y=0, yend=0, size=0.7) +
      geom_segment(x=0, xend=1, y=1, yend=1, size=0.7) +
      # scale_colour_grey() +
      # scale_colour_brewer(palette="Set1") +
      scale_y_continuous(limits=c(-0.2, 1.2), expand=c(0, 0)) +
      geom_segment(data=axis_1, aes(x=x, xend=x, y=y, yend=y-0.025), size=0.7) +
      geom_segment(data=axis_2, aes(x=x, xend=x, y=y, yend=y+0.025), size=0.7) +
      geom_text(data=axis_1, aes(label=label, x=x, y=y - 0.075)) +
      geom_text(data=axis_2, aes(label=label, x=x, y=y + 0.075)) +
      labs(caption = "Largest Eigen = smallest Eigenvalue") +
      guides(
        colour = FALSE,
        size = guide_legend(title = ""),
        alpha = guide_legend(title = "")
      )

    gg

  })

})
