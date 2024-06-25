# VISUALIZACION DE DATOS --------------------------------------------------

library (magrittr)

# PARTE 1
mtcars %>% tidyr::pivot_longer (
    cols = c ("disp","hp"),
    names_to = "variables",
    values_to = "values"
) %>% ggplot2::ggplot (
    data = .,mapping = ggplot2::aes (x = mpg,y = values)
) + ggplot2::geom_point () + ggplot2::facet_wrap (
    facets = ~ variables
)
mtcars2 <- mtcars %>% tidyr::pivot_longer (
    cols = c ("disp","hp"),
    names_to = "variables",
    values_to = "values"
)
plot_mtcars <- mtcars %>% tidyr::pivot_longer (
    cols = c ("disp","hp"),
    names_to = "variables",values_to = "values"
) %>% ggplot2::ggplot (
    mapping = ggplot2::aes (x = mpg)
) + ggplot2::geom_point (
    mapping = ggplot2::aes (y = values,colour = variables)
) + ggplot2::facet_wrap (
    facet = ~ "GRAFICO DE 2 VARIABLES"
)
plot_mtcars + ggplot2::labs (
    title = "Mi primera imagen",caption = "este es un pie de pagina",
    x = "Cantidad",y = "Valores",colour = "Variables"
) + ggplot2::scale_colour_discrete (
    labels = c ("Pizza","Coca-Cola")
) + ggplot2::theme (
    legend.position = "bottom",
    text = ggplot2::element_text (size = 12),
    plot.title = ggplot2::element_text (face = "bold")
)
plot_mtcars + ggplot2::labs (
    x = "Cantidad",y = "Valores",colour = "Variables"
) + ggplot2::scale_colour_discrete (
    labels = c ("Día","Noche")
) + ggplot2::theme (
    legend.position = "bottom"
)
mtcars %>% dplyr::mutate (
    marcas = rownames (mtcars)
) %>% ggplot2::ggplot (
    mapping = ggplot2::aes (x = mpg)
) + ggplot2::geom_point (
    mapping = ggplot2::aes (y = hp)
) + ggplot2::geom_text (
    mapping = ggplot2::aes (y = hp,label = marcas),
    check_overlap = TRUE
)
data_nerlove <- haven::read_dta (
    file = "data_1.1/nerlove63.dta"
) %>% dplyr::relocate (
    pkap,.before = pfuel
) %>% dplyr::rename_with (
    .cols = dplyr::everything (),
    .fn = ~ c ("TC","Q","p1","p2","p3")
) %>% dplyr::arrange (Q) %>% dplyr::mutate (
    group = base::cut (
        x = Q,breaks = c (
            -Inf,196,719,1474,3286,Inf
        ),labels = c ("A","B","C","D","E")
    )
)
grafico_st1 <- forecast::autoplot (object = AirPassengers)
grafico_st2 <- forecast::autoplot (object = AirPassengers-50)

cowplot::plot_grid (grafico_st1,grafico_st2,ncol = 1)

plotly::ggplotly (grafico_st1)

## PARTE 2
data_dta <- haven::read_dta (
    file = "data_1.1/nerlove63.dta"
) %>% dplyr::relocate (
    pkap,.before = pfuel
) %>% dplyr::rename_with (
    .cols = dplyr::everything (),
    .fn = ~ c ("TC","Q","p1","p2","p3")
) %>% dplyr::arrange (Q) %>% dplyr::mutate (
    group = base::cut (
        x = Q,breaks = c (-Inf,150,600,1800,3600,Inf),
        labels = c ("A","B","C","D","E")
    )
)
## TABULACION DE VARIABLES Y DISTRIBUCION DE FRECUENCIAS
data_fc <- data_dta %>% dplyr::count (
    group,name = "freq_abs"
) %>% dplyr::mutate (
    values = base::seq (from = 1,length.out = base::length (group)),
    freq_rel = freq_abs/base::sum (freq_abs),
    freq_per = base::round (x = freq_rel*100,digits = 2),
    cumu_abs = base::cumsum (freq_abs),
    cumu_rel = base::cumsum (freq_rel),
    cumu_per = base::cumsum (freq_per)
) %>% dplyr::relocate (
    values,.before = "freq_abs"
)
data_fc %>% base::as.data.frame () %>% stargazer::stargazer (
    type = "text",summary = FALSE,rownames = FALSE,
    title = "Distribución de frecuencias"
)
## BAR PLOT
plot_bar <- data_fc %>% ggplot2::ggplot (
    mapping = ggplot2::aes (x = group)
) + ggplot2::geom_bar (
    mapping = ggplot2::aes (
        y = freq_abs,fill = group
    ),stat = "identity"
) + ggplot2::labs (
    y = "Frequency",fill = "Group"
) + ggplot2::theme (
    axis.ticks.x = ggplot2::element_blank (),
    axis.text.x = ggplot2::element_blank (),
    axis.title.x = ggplot2::element_blank (),
    legend.position = "bottom",
    legend.key.size = ggplot2::unit (x = 0.25,units = "cm")
);plot_bar
## BAR PLOT - ALTERNATIVA
data_fc %>% ggplot2::ggplot () + ggplot2::geom_segment (
    mapping = ggplot2::aes (x = values,y = 0,yend = freq_abs)
)
## PIE PLOT
plot_pie <- data_fc %>% ggplot2::ggplot (
    mapping = ggplot2::aes (x = "",y = freq_rel)
) + ggplot2::geom_col (
    mapping = ggplot2::aes (fill = group)
) + ggplot2::geom_text (
    mapping = ggplot2::aes (label = freq_per),size = 2.5,
    position = ggplot2::position_stack (vjust = 0.5)
) + ggplot2::labs (
    fill = "Group"
) + ggplot2::coord_polar (theta = "y") + ggplot2::theme (
    axis.title = ggplot2::element_blank (),
    legend.position = "bottom",
    legend.key.size = ggplot2::unit (x = 0.25,units = "cm")
);plot_pie
## STEP PLOT
plot_step <- data_fc %>% ggplot2::ggplot (
    mapping = ggplot2::aes (x = values,y = cumu_abs)
) + ggplot2::geom_step () + ggplot2::geom_point () + ggplot2::labs (
    x = "Values",y = "Cumulative Absolute"
);plot_step
## OGIVE PLOT
plot_ogive <- data_fc %>% ggplot2::ggplot (
    mapping = ggplot2::aes (x = values,y = cumu_per)
) + ggplot2::geom_line () + ggplot2::geom_point () + ggplot2::labs (
    x = "Values",y = "Cumulative Percent"
);plot_ogive
## HISTOGRAM
plot_hist <- data_dta %>% ggplot2::ggplot (
    mapping = ggplot2::aes (x = p2)
) + ggplot2::geom_histogram (bins = 20) + ggplot2::labs (
    x = "Price of Capital",y = "Frequency"
);plot_hist
## DENSITY
plot_density <- data_dta %>% ggplot2::ggplot (
    mapping = ggplot2::aes (x = p2)
) + ggplot2::geom_density () + ggplot2::labs (
    x = "Price of Capital",y = "Density"
);plot_density
## SCATTER PLOT
plot_point <- data_dta %>% dplyr::mutate (
    Q = Q/1000
) %>% ggplot2::ggplot (
    mapping = ggplot2::aes (x = Q,y = TC)
) + ggplot2::geom_point () + ggplot2::labs (
    x = "Output (TN KwH)",y = "Total Cost (MM USD)"
);plot_point
## LINE PLOT
plot_line <- AirPassengers %>% tsibble::as_tsibble () %>% tibble::as_tibble () %>% dplyr::mutate (
    index = base::as.Date (index)
) %>% ggplot2::ggplot (
    mapping = ggplot2::aes (x = index,y = value)
) + ggplot2::geom_line () + ggplot2::labs (
    x = "Time (Months)",y = "Passangers"
)
## GUARDAR GRAFICOS
### Esta seccion no es necesario que lo corran porque solo es para
### generar los graficos para mis diapositivas PDF en LaTeX.
### Por lo tanto, solo necesitan visualizarlo aqui en RStudio
### list (
###     plot_bar,plot_pie,plot_step,plot_ogive,
###     plot_hist,plot_density,plot_line,plot_point
### ) %>% purrr::map2 (
###     .y = c (
###         "plot_bar.png","plot_pie.png","plot_step.png","plot_ogive.png",
###         "plot_hist.png","plot_density.png","plot_line.png","plot_point.png"
###     ),
###     .f = function (m,n) return (
###         ggplot2::ggsave (
###             plot = m,filename = n,width = 7.75,height = 6,scale = 0.8,
###             units = "cm",path = "../Diapositiva/figures/"
###         )
###     )
### )
