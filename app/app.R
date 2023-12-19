#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggtext)
library(tidyverse)
library(data.table)
library(datasets)
library(maps)
# library(plotly)
library(bslib)


import <- fread("LinkedInJobs_MLDataset_cleaned.csv") |> 
  filter(sal < 500000) |> 
  mutate(full = tolower(full))

state_pop <- as.data.frame(datasets::state.x77) |> 
  janitor::clean_names() |> 
  rownames_to_column(var = "state_full") |> 
  mutate(state_full = tolower(state_full)) |> 
  left_join(tibble(state = state.abb, 
                   state_full = tolower(state.name)), 
            by = c( "state_full"= "state_full"))


state_borders <- map_data("state")




##Outside cleaning
  # janitor::clean_names() |> 
  # mutate(id = row_number()) |> 
  # filter(med_sal != 0) |> 
  # mutate(sal = case_when(
  #   py_prd == "HOURLY" ~ med_sal*40*52, 
  #   py_prd == "MONTHLY" ~ med_sal*12, 
  #   py_prd == "WEEKLY" ~ med_sal*52, 
  #   T ~ med_sal
  # )) |> 
  # mutate(sal = ifelse(sal <=100, sal *1000, sal)) |> 
  # filter(sal < 500000) |> 
  # filter(py_prd != "ONCE") |> 
  # mutate(id = row_number())
# + added in the state abbreviations


js_enter <- '$(document).on("keyup", function(e) {
  if(e.keyCode == 13){
    Shiny.onInputChange("keyPressed", Math.random());
  }
});
'




plot2 <-  import |> 
  ggplot(aes(y = views, x = sal )) +
  geom_point(alpha = 0.25, color = "#BF0F61") +
  geom_smooth(method = lm, se = FALSE, color = "#04BFAD") +
  ggpubr::stat_regline_equation( aes(label = ..eq.label..),
                                 label.x.npc = "right", label.y.npc = "top", hjust = 1) +
  # stat_summary_bin(fun = "median", colour = "red", size = 4,
  #                  bins = 1,
  #                  geom = "text", aes(label = after_stat(y)),
  #                  position = position_nudge(x = 0.25)) +
  scale_x_continuous(labels = scales::dollar_format( scale = 0.001, accuracy = 1, suffix = "K")) +
  labs(x = "Salary", 
       y = "Posting views") +
  theme_minimal(base_size = 14)


## ----------------------------------------------------------------------------------------
## Page Desgin


# Define UI for application that draws a histogram
ui <- fluidPage(
  
    theme = bs_theme(version = 4, bootswatch = "lux"),

    # Application title
    titlePanel("2023 LinkedIn Data"),

    # Sidebar with a slider input for number of bins 
    tags$script(js_enter),
    tags$head(tags$style(
      HTML('
                     .main-sidebar {
                          background-color: rgb(255,125,125);
                          }
           ')
    )),
    
    # fluidRow("Text on row 1"),
    wellPanel( fluidRow("Search by keyword(s) in 2023 job title postings on linkedin. Top keywords: manager, sales, engineer, 
                        director, engineer, nurse, coordinator, remote") ),
    
    sidebarLayout(
      
 
        
        #previously 'mainPanel()'
        mainPanel(
          # h4("Search keywords in 2023 job title postings on linkedin"),
           #plotOutput("distPlot"),
           #plotOutput("distPlot2"),
           # plotOutput("distPlot3"),
           # htmlOutput("idk"),
           #plotOutput("distPlot4"),
          h2("Types of jobs posted"),
           plotOutput("distPlot5"),
          h2("Experience jobs posted"),
          plotOutput("distPlot5.5"),
          h2("Distribution of salaries"),
           plotOutput("distPlot6"),
           h2("Percentage of posts"),
           p("This takes the percentage of posts in each state that contain the keyword, Previously,
             the percentage was based of state population but that only provided a map of states which utilized LI the most thus
             there was minimal difference between keywords. Mapping # number of posts by state just showed a map of the US population"),
           plotOutput("distPlot7"),
          h2("Salary vs. views of post"),
          p("Largely uninteresting scatter plot with an attempt at linear regression"),
           #plotlyOutput("distPlot9")
           plotOutput("distPlot8"),
          h2("Remote work?"),
          plotOutput("distPlot10"),
          h2("LinkedinIn Easy Apply"),
          p("I'm not sure the defintion of the variable, but related to if you can apply on the LinkedIn"),
          plotOutput("distPlot11"),
          htmlOutput("bulleted_list")
                    
        ),

      
        
      # 
      # tabsetPanel(type = "tabs",
      #             tabPanel("Plot",  plotOutput("distPlot5")),
      #             tabPanel("Summary",  plotOutput("distPlot5.5")))
      # ),
        sidebarPanel(
          htmlOutput("html1"),
          textInput("searchBar1", label = "Comma-seperated key words:", value = "", 
                    placeholder = "(ex: analyst)")
        )
        
        
        
    )
    
    
    
)




# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
  output$html1 <- renderUI({
    HTML( "<br>")
  })

  output$bulleted_list <- renderText({
    
    "Source: <a href='https://www.kaggle.com/datasets/adampq/linkedin-jobs-machine-learning-data-set'>Kaggle</a><br>
    <b>Limtations include: </b>
    <li>Removed job postings that did not have salary</li>
    <li>Currently, key words are as 'OR' search. Future iterations should have an 'and' search feature or option for further breakdown</li>
    <li>Does not differentiate between industries thus not entirely comparable.</li>
    <li>Key word search of job titles not job descriptions, as the job descriptions need further cleaning to be useable</li>"
  })

    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white',
    #          xlab = 'Waiting time to next eruption (in mins)',
    #          main = 'Histogram of waiting times')
    # })
    
    # output$distPlot2 <- renderPlot({
    #     ggplot(mtcars, aes(wt, mpg)) + 
    #       geom_point() +
    #       scale_y_continuous(limits = c(0, input$bins))
    # })
    
    
    
    ## Works for getting baseline plot -------------------------------------------------
    # output$distPlot3 <- renderPlot({
    #   
    #   if (input$dropDown1 == "everyone") {
    #     filter2 <- "*"
    #   } else {
    #     filter2 <- input$dropDown1
    #   }
    #   
    #   
    #   lim2 <- max(table(analyst$wrk_typ))/2
    #   
    #   newTable <- import |> 
    #     filter(grepl(filter2, job_desc, ignore.case = T)) |> 
    #     count(wrk_typ) |> 
    #     mutate(perc = n/sum(n), 
    #            wrk_typ = fct_reorder(wrk_typ, n), 
    #            label = paste0("n = ", n, "<br>", scales::percent(perc))) 
    #   
    #   lim2 <- max(newTable$n)/2
    #     
    #   ggplot(newTable, aes(y = wrk_typ, x =n)) +
    #     geom_bar(stat="identity") + 
    #     geom_richtext(aes(label = label), hjust = 0, 
    #                 label.margin = unit(c(0, 2, 0, 2), "mm"),
    #                 fill = NA, label.colour = NA) +
    #     scale_x_continuous(expand = expand_scale(add = c(0, lim2) )) +
    #     theme_minimal()
    # })
    
    ##---------------------------------------------------------------
    ## WORKS for getting reactive plot ------------------------------------------------- 
    
    
    #gets entire new dataset 
    # calc_newData <- reactive({
    # 
    #   if (input$dropDown1 == "everyone") {
    #     filter2 <- "*"
    #   } else {
    #     filter2 <- input$dropDown1
    #   }
    # 
    #   import[grepl(filter2, job_ttl, ignore.case = T)]
  
    # })
    # 
    # 
    # 
    # 
    # 
    # 
    # output$distPlot4 <- renderPlot({
    # 
    #   calc_newData() |>
    #     count(wrk_typ) |>
    #     mutate(perc = n/sum(n),
    #            wrk_typ = fct_reorder(wrk_typ, n),
    #            label = paste0("n = ", n, "<br>", scales::percent(perc)))  |>
    #     ggplot( aes(y = wrk_typ, x =n)) +
    #     geom_bar(stat="identity") +
    #     geom_richtext(aes(label = label), hjust = 0,
    #                   label.margin = unit(c(0, 2, 0, 2), "mm"),
    #                   fill = NA, label.colour = NA) +
    #     # scale_x_continuous(expand = expand_scale(add = c(0, lim2) )) +
    #     theme_minimal()
    # 
    # })
    
    
    
    ## Doesn't work -------------------------------------------------------------------
    
    
   # rct_ggplot_fun <- reactive({
   #    import |> 
   #      
   #      count(wrk_typ) |> 
   #      mutate(perc = n/sum(n), 
   #             wrk_typ = fct_reorder(wrk_typ, n), 
   #             label = paste0("n = ", n, "<br>", scales::percent(perc))) |> 
   #      ggplot(aes(y = wrk_typ, x =n)) +
   #      geom_bar(stat="identity") + 
   #      geom_richtext(aes(label = label), hjust = 0, 
   #                    label.margin = unit(c(0, 2, 0, 2), "mm"),
   #                    fill = NA, label.colour = NA) +
   #      scale_x_continuous(expand = expand_scale(add = c(0, lim2) )) +
   #      theme_minimal()
   # 
   #  })
   #  
   #  
   #  
   #  output$distPlot5 <- renderPlot({
   #    rct_ggplot_fun() +
   #      geom_vline(xintercept = 50, color = "red")
   #    })
    
    
    
    ## Works by "ENTER KEY" --------------------------------------------------------------------------


    entered <- reactiveVal(value = "*")
    
    
    clicked <- observeEvent(input[["keyPressed"]], {
      entered(input[["searchBar1"]])
    })
    
    
    #OG search -- works as combined word search
    srch_clean <- reactive({
      if (entered() != "*") {
        filter3 <- str_squish(gsub(",", "|", str_squish(entered())))
      } else {
        filter3 <- "*"
      }
    })
    
    
    
    # srch_clean <- reactive({
    #   
    #   
    #   if (entered() %in% c("*","")) {
    #     filter3 <- "*"
    #     
    #   }
    #   
    #   else if (input$srchType == "andz") {
    #     txt <- gsub("\\s", "", entered())
    #     
    #     txt <- unlist(strsplit(txt, ","))
    #     filter3 <- paste0(paste0("(?=.*", txt, ")"), collapse = "")
    #   }
    #   
    #   
    #   else if (input$srchType == "norm") {
    #     filter3 <- str_squish(gsub(",", "|", str_squish(entered())))
    #   }
    # 
    #   
    # })
    
    
    # output$idk <- renderText(srch_clean())
    
    
    get_data <- reactive({
      #need to add perl for AND
      import[grepl(srch_clean(), job_ttl, ignore.case = T)]
      
    })
    
    
    ## Gets plot1-----------------------------------------------
    
    
    output$distPlot5 <- renderPlot({
      
      lim2 <- max(table(get_data()$wrk_typ))/2
      
      
      get_data() |> 
          count(wrk_typ) |> 
          mutate(perc = n/sum(n), 
                 wrk_typ = fct_reorder(wrk_typ, n), 
                 label = paste0("n = ", scales::comma(n), "<br>", scales::percent(perc)))  |> 
          ggplot( aes(y = wrk_typ, x =n)) +
          geom_bar(stat="identity", fill = "#04BFAD") +
          geom_richtext(aes(label = label), hjust = 0,
                        label.margin = unit(c(0, 2, 0, 2), "mm"),
                        fill = NA, label.colour = NA) +
          labs(y = "Type of position", 
               x = "# of posts") +
          scale_x_continuous(expand = expand_scale(add = c(0, lim2)), 
                             labels = scales::comma_format()) +
          theme_minimal(base_size = 14)
        

    })
    
    
    
    output$distPlot5.5 <- renderPlot({
      
      lim3 <- max(table(get_data()$xp_lvl))/2
      

      
      get_data() |> 
        count(xp_lvl) |> 
        mutate(perc = n/sum(n), 
               xp_lvl = factor(xp_lvl, levels = c("Not Listed", 
                                                  "Internship", 
                                                  "Entry level", 
                                                  "Associate", 
                                                  "Mid-Senior level", 
                                                  "Director" , 
                                                  "Executive" 
                                                  
                                                  )), 
               label = paste0("n = ", scales::comma(n), "<br>", scales::percent(perc)))  |> 
        ggplot( aes(y = xp_lvl, x =n)) +
        geom_bar(stat="identity", fill = "#04BFAD") +
        geom_richtext(aes(label = label), hjust = 0,
                      label.margin = unit(c(0, 2, 0, 2), "mm"),
                      fill = NA, label.colour = NA) +
        labs(y = "Experience level", 
             x = "# of posts") +
        scale_x_continuous(expand = expand_scale(add = c(0, lim3)), 
                           labels = scales::comma_format()) +
        theme_minimal(base_size = 14)
      
      
    })
    
    
    
  ## -----------------------------------------------------------------------
    
    
    
    output$distPlot6 <- renderPlot({
      
      
      plot3 <-  import |> 
        mutate(groupz = case_when(
          #need to add perl for AND
          grepl(srch_clean(), job_ttl, ignore.case =T) ~ srch_clean(), 
          T ~ "Everything else"
        ), 
        color = ifelse(groupz == "Everything else", "#038C7F", "#BF0F61")) |> 
        
        ggplot(aes( x = sal, color = color)) +
        geom_density(linewidth = 1) +
        scale_x_continuous(
          labels = scales::dollar_format()) +
        scale_color_identity() +
        theme_minimal(base_size = 18) +
        theme(axis.text.y = element_blank(), 
              axis.title.y = element_blank(), 
              # panel.grid.minor.x = element_blank(),
              # panel.grid.major.x = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              legend.position = "none")
      
      
      spec_label <- plot3$data |> 
        summarise(med = median(sal), 
                  liqr = quantile(sal, 0.25), 
                  uiqr = quantile(sal, 0.75),
                  .by = groupz) |> 
        arrange(-med) |> 
        mutate(color = ifelse(groupz == "Everything else", "#038C7F", "#BF0F61"),
               label = paste0("<span style = 'color: ", 
                              color,
                              ";'>",
                              str_to_title(groupz), ": ", 
                              scales::dollar(med, scale = 0.001, accuracy = 0.1, suffix = "K"),
                              " (IQR ", 
                              scales::dollar(liqr, scale = 0.001, accuracy = 0.1, suffix = "K"), 
                              "-", 
                              scales::dollar(uiqr, scale = 0.001, accuracy = 0.1, suffix = "K"), 
                              ")</span>"
               ))
      
      
      
      
      max_y <- max( ggplot_build(plot3)$data[[1]]$y)
      
      if (nrow(import) != nrow(get_data())) {
        
        
        plot3 +
          annotate(geom='richtext', x=c(500000, 500000), 
                   y=  c(max_y - max_y/5, max_y - max_y/3.5), 
                   color = NA,
                   hjust = 1,
                   label=  spec_label$label) 
        
      } else {
        
        
        plot3 +
          annotate(geom='richtext', x=500000, 
                   y=  max_y - max_y/5, 
                   color = NA,
                   hjust = 1,
                   label=  spec_label$label[1])
        
        
      }
      
      

      
      
      
      
    })
    
    
    ##-----------------------------------------------------
    ## Map of the USA
    
    output$distPlot7 <- renderPlot({
    
    #% of state population
      # linkedin_per <- get_data() |>
      #   summarize(n = n(),
      #             med = median(sal), .by = "full") |>
      #   left_join(select(state_pop, population, income, state, state_full),
      #             by = c("full" = "state_full")) |>
      #   filter(!is.na(population)) |>
      #   mutate(per = n/ population) |>
      #   arrange(per)
    
      
      
      
      linkedin_per <-  get_data() |>
        count(full) |>
        rename(n_keyword = n) |>
        left_join(count(import, full), by = "full") |>
        mutate(per = n_keyword/n)
        

    # 
    # 
    state_borders |>
      left_join(linkedin_per, by = c("region"="full")) |>
      ggplot() +
      geom_polygon(aes(x = long, y = lat, fill = per, group = group), color = "white") +
      scale_fill_gradient(low = "lightblue", high = "darkblue") +
      coord_fixed(1.3) +
      guides(fill=FALSE) +
      theme_void(base_size = 14)# do this to leave off the color legend
    
    })
    
    ##-----------------------------------------------------
    ## Scatter plots
    
    output$distPlot8 <- renderPlot({
      
      plot2 %+%
        subset(import, grepl(srch_clean(), job_ttl, ignore.case = T))
    
      
    })
    
    ##-----------------------------------------------------
    
    
    output$distPlot10 <- renderPlot({
      
      lim4 <- max(table(get_data()$is_remote))/2
      
      
      get_data() |> 
        mutate(is_remote = ifelse(is_remote == 1, "Remote", "Not remoted")) |> 
        count(is_remote) |> 
        mutate(perc = n/sum(n), 
               is_remote = factor(is_remote, levels = rev(c("Remote", "Not remoted"))), 
               label = paste0("n = ", scales::comma(n), "<br>", scales::percent(perc)))  |> 
        ggplot( aes(y = is_remote, x =n)) +
        geom_bar(stat="identity", fill = "#04BFAD") +
        geom_richtext(aes(label = label), hjust = 0,
                      label.margin = unit(c(0, 2, 0, 2), "mm"),
                      fill = NA, label.colour = NA) +
        labs(y = "", 
             x = "# of posts") +
        scale_x_continuous(expand = expand_scale(add = c(0, lim4)),
                           labels = scales::comma_format()) +
        theme_minimal(base_size = 14)
      
      
    })
    
    ##------------------------------------------------------------
    
    output$distPlot11 <- renderPlot({
      
      lim5 <- max(table(get_data()$is_remote))/2
      
      
      # get_data() |> 
      import |> 
        mutate(app_is_off = ifelse(app_is_off, "Not LinkedIn Easy Apply", "LinkedIn Easy Apply")) |> 
        count(app_is_off) |> 
        mutate(perc = n/sum(n), 
               app_is_off= factor(app_is_off, levels = c("Not LinkedIn Easy Apply", "LinkedIn Easy Apply")), 
               label = paste0("n = ", scales::comma(n), "<br>", scales::percent(perc)))  |> 
        ggplot( aes(y = app_is_off, x =n)) +
        geom_bar(stat="identity", fill = "#04BFAD") +
        geom_richtext(aes(label = label), hjust = 0,
                      label.margin = unit(c(0, 2, 0, 2), "mm"),
                      fill = NA, label.colour = NA) +
        labs(y = "", 
             x = "# of posts") +
        scale_x_continuous(expand = expand_scale(add = c(0, lim5)),
                           labels = scales::comma_format()) +
        theme_minimal(base_size = 14)
      
      
    })
    
    
    # output$distPlot9 <- renderPlot({
    #   
    #   plot4 <- plot2 %+%
    #     subset(import, grepl(srch_clean(), job_ttl, ignore.case = T))
    #   
    #   ggplotly(plot4)
    #   
    # })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)







# library(plotly)
# 
# ui <- fluidPage(
#   selectInput("choice", "Choose", choices = names(iris), selected = NULL),
#   plotlyOutput("graph")
# )
# 
# server <- function(input, output, session){
#   
#   output$graph <- renderPlotly({
#     plot_ly(iris, x = ~get(input$choice), y = ~Sepal.Length, type = 'scatter', mode = 'markers')
#   })
# }
# 
# shinyApp(ui, server)












