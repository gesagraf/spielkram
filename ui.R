# defininig UI
ui <- fluidPage(

  # Application title

  # define sidebars

fluidRow(
#     # #### Organiszing Plots ####
# # # specific sample
    column(2, checkboxInput("p_samp", "konkrete Stichprobe", value = T)),
    # forest plot
column(2,     checkboxInput("p_forest", "Forestplot", value = F)),
    # SKV Mean
    column(2,     checkboxInput("p_mean", "SKV Arithmetisches Mittel", value = F)),
    # SKV Minmax
    column(2,   checkboxInput("p_minmax", "SKV Alternativer Schätzer", value = F)),
    # SKV Bayes uni
    column(2,   checkboxInput("p_bayes_uni", "SKV gleichverteilter Bayes Schätzer", value = F)),
    # SKV Bayes nv
    column(2,  checkboxInput("p_bayes_nv", "SKV normalverteilter Bayes Schätzer", value = F)),
),
    # # action Button
    #      actionButton("go", "Go"),

  sidebarPanel(width = 3,
               # selectInput(
               #   "anzeigen", "Welche Plots möchtest du gerne angezeigt bekommen?", possibilities,
               # multiple = TRUE
               # ),

    #### Population ####
    # Population mu
    sliderInput(inputId = "mu",
                paste0("Populations-", expression(mu)),
                min = -100,
                max = 100,
                value = 0),

    # Population sd
    sliderInput(inputId = "std",
                "Populations-Standardabweichung",
                min = 0,
                max = 50,
                value = 1),


    #### Samples ####
    # Samplesize
    sliderInput(inputId = "n",
                "Größe der einzelnen Stichproben",
                min = 0,
                max = 1000,
                value = 20),

    # Number of Samples
    sliderInput(inputId = "number",
                "Gesamtanzahl der Stichproben",
                min = 0,
                max = 1000,
                value = 10),

    # Specific Sample: Dynamic Max depending on "number" (see server.R)
    sliderInput(inputId = "specific",
                label = "Spezifische Stichprobe",
                max = 10,
                min = 1,
                step = 1,
                value = 3),

    #### Priors ####
    # Gleichverteiler Prior
    # Minimum
    sliderInput(inputId = "min_uni_priori",
                "Minimum der Gleichverteilten Priori",
                min = -100,
                max = 100,
# !!! An Maximum anpassen
                value = -5),

    # Maximum
    sliderInput(inputId = "max_uni_priori",
                "Maximum der Gleichverteilten Priori",
                min = -100,
                max = 100,
  # !!! An Minimum anpassen
                value = 0),


    # Normalverteilter Prior
    # Prior Mean
    sliderInput(inputId = "mu_prior",
                label = "Prior Mean",
                min = -100,
                max = 100,
                value = 50),

    # Sidebar with a slider input for Prior Tau
    sliderInput(inputId = "tau_prior",
                label = "Prior Tau",
                min = 1,
                max = 100,
                value = 10)

  ),

  # Show a plot of the generated distribution
  mainPanel(
        tabPanel("some title",
                 fluidRow(
                          column(5, plotOutput("plot_samp")),
                          column(5, plotOutput("plot_forest")),
                      #    column(2, plotOutput("legende"))
                 ),
                fluidRow(
                         column(5, plotOutput("plot_mean")),
                         column(5, plotOutput("plot_minmax")),
                       #  column(2, plotOutput("legende"))

                ),
                fluidRow(
                         column(5, plotOutput("plot_bayes_uni")),
                         column(5, plotOutput("plot_bayes_nv")),
                        # column(2, plotOutput("legende"))


        )
      )
  )
)









