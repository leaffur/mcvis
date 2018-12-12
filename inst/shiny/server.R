library(shiny)
library(DT)
library(mplot)
library(psych)

data("artificialeg")
p = dim(artificialeg)[2]-1
X = artificialeg[,1:p]
mcvis_result = mcvis2(X)

shinyServer(function(input, output, session) {

  output$x1 = DT::renderDataTable(psych::describe(X), server = FALSE)

  # highlight selected rows in the scatterplot
  output$x2 = renderPlot({
    s = input$x1_rows_selected
    eig.max = ncol(mcvis_result$g)
    vol.max = ncol(mcvis_result$g)
    g = mcvis_result$g
    col.names = mcvis_result$col.names
    p = ncol(g)

    eig.max = min(p, eig.max)
    vol.max = min(p, vol.max)
    or = order(g[p,]) ## Order the columns of g by the smallest eigen value
    or = or[1:vol.max]
    g.or = g[,or]
    if (vol.max > 1) {g.or = g.or[p:(p-eig.max+1),]} else {g.or = as.matrix(g.or[p:(p-eig.max+1)])}
    if (eig.max == 1) {g.or = t(g.or)}
    g.or.subset = g.or

    matchingCols = match(colnames(g[,s]), colnames(g.or.subset))
    g.or.subset[, -matchingCols] = 1
    # ###############  ggplot #######################
    preDat = reshape2::melt(g.or.subset,
                            varnames = c("X2", "X1"),
                            value.name = "weights")

    thickness = 1 - preDat$weights

    # input$sizeCategory1 = 0.0
    # input$sizeCategory2 = 0.1
    # input$sizeCategory3 = 0.2
    # input$sizeCategory4 = 0.7
    # input$sizeCategory5 = 1.5
    #
    # ggplotSizeCategory = dplyr::case_when(
    #   thickness < input$sizeCategory1 ~ "category1",
    #   thickness < input$sizeCategory2 ~ "category2",
    #   thickness < input$sizeCategory3 ~ "category3",
    #   thickness < input$sizeCategory4 ~ "category4",
    #   thickness < input$sizeCategory5 ~ "category5"
    # )
    #
    #
    # input$alphaCategory1 = 0.0
    # input$alphaCategory2 = 0.1
    # input$alphaCategory3 = 0.2
    # input$alphaCategory4 = 0.5
    # input$alphaCategory5 = 1.0
    #
    # ggplotAlphaManual = c(
    #   input$alphaCategory1,
    #   input$alphaCategory2,
    #   input$alphaCategory3,
    #   input$alphaCategory4,
    #   input$alphaCategory5
    # )
    temp = list()
    temp$sizeCategory1 = 0.0
    temp$sizeCategory2 = 0.1
    temp$sizeCategory3 = 0.2
    temp$sizeCategory4 = 0.3
    temp$sizeCategory5 = 1

    ggplotSizeManual = c(0, 0.1, 0.3, 0.8, 1.5)

    ggplotSizeCategory = dplyr::case_when(
      thickness < temp$sizeCategory1 ~ "category1",
      thickness < temp$sizeCategory2 ~ "category2",
      thickness < temp$sizeCategory3 ~ "category3",
      thickness < temp$sizeCategory4 ~ "category4",
      thickness < temp$sizeCategory5 ~ "category5"
    )
    ggplotAlphaManual = c(0, 0.5, 0.5, 0.5, 1.0)
    ggplotAlphaCategory = ggplotSizeCategory


    # temp$alphaCategory1 = 0.0
    # temp$alphaCategory2 = 0.5
    # temp$alphaCategory3 = 0.6
    # temp$alphaCategory4 = 0.7
    # temp$alphaCategory5 = 1.0
    #
    # ggplotAlphaManual = c(0, 0.2, 0.4, 0.5, 1.0)
    #
    # ggplotAlphaCategory = dplyr::case_when(
    #   thickness < temp$alphaCategory1 ~ "category1",
    #   thickness < temp$alphaCategory2 ~ "category2",
    #   thickness < temp$alphaCategory3 ~ "category3",
    #   thickness < temp$alphaCategory4 ~ "category4",
    #   thickness < temp$alphaCategory5 ~ "category5"
    # )



    # breaks = quantile(thickness, probs = seq(0, 1, by = 0.2), na.rm = TRUE) %>% unique
    # thicknessCategory = cut(thickness,
    #                         breaks = breaks,
    #                         labels = paste0("category", 1:5)[1:(length(breaks)-1)],
    #                         include.lowest = T,
    #                         ordered_result = T)
    #
    # alphaBreaks = quantile(thickness, probs = seq(0, 1, length.out = 4), na.rm = TRUE) %>% unique
    # alphaCategory = cut(thickness,
    #                     breaks = alphaBreaks,
    #                     labels = paste0("category", 1:5)[1:(length(alphaBreaks)-1)],
    #                     include.lowest = T,
    #                     ordered_result = T)
    #
    # ggplotSizeManual = c(0, 0.1, 0.5, 1, 2)[length(levels(thicknessCategory)):5]
    # ggplotAlphaManual = c(0, 0, 0.1, 0.4, 1)[(6 - length(levels(alphaCategory))):5]

    dat = preDat %>%
      dplyr::mutate(
        thickness,
        ggplotSizeCategory,
        ggplotAlphaCategory
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
                       size = ggplotSizeCategory, alpha = ggplotAlphaCategory)) +
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
      labs(caption = "Largest Eigen = smallest Eigenvalue")
    gg

  })

})
