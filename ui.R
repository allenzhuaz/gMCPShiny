
#-----User Interface for Shiny App-----

ui<- function(request){
  fluidPage(
  
    # tags$head(
    #   tags$link(rel = "stylesheet", type = "text/css", href = "mystyle.css")
    # ),
    # tags$head(
    #   tags$link(rel = "stylesheet", href="https://fonts.googleapis.com/css?family=IBM+Plex+Mono|IBM+Plex+Serif|IBM+Plex+Sans:400,700i")
    # ),
    # 
    # HTML(navbar),
    
    # Custom JS to enable downloading png from DOM
    tags$head(
      HTML('<script type="text/javascript">
             function prepHref(linkElement) {
               var myDiv = document.getElementById("thePlot");
               var myImage = myDiv.children[0];
               linkElement.href = myImage.src;
             }
           </script>')
    ),
    
    #-----Application Title-----
    
    titlePanel(" Multiplicity Graphs for Hypothesis Testing"),
    
    sidebarLayout(
      
      #-----User input sidebar panel-----  
      
      sidebarPanel(
        tabsetPanel(type="tabs",
          
          #-----Hypotheses Tab-----     
          
          tabPanel("Hypotheses",
            h3("Set the # of Hypotheses"),
            p("(Both name and group must be updated.)"),
            HTML('<p>The text input uses the plotmath syntax. See here for additional details: <a href="https://stat.ethz.ch/R-manual/R-devel/library/grDevices/html/plotmath.html", target = "_blank">Plot Math Syntax </a>. Use "\\n" to add a carriage return.</p>'),
            br(),
            rHandsontableOutput("hotHypotheses"),
            

          
            
            br(),
            rHandsontableOutput("hotGroups"),
            br(),
            actionButton("update", "Update Nodes"),
            #bookmarkButton(),
            br(),
            h3("Save or load table data"),
            p("When loading data, only the tables (hypotheses, groups, transitions, and positions) will be updated. User must press the each update button to change the graph."),
            downloadButton("save_inputs", "Save Tables"),
            br(),
            fileInput("load_inputs", "Load Table Inputs from .rda or .rdata file", accept = c(".rda", ".rdata"))
          ), # end Hypotheses Tab
          
          #-----Transitions Tab-----  
          
          tabPanel("Transitions",
            h3("Input Data Frame"),
            p("Transitions between non-existing hypotheses will not influence graph output."),
            rHandsontableOutput("hotTransitions"),
            br(),
            actionButton("updateEdges", "Update Edges")
          ), # end Transitions Tab
          
          #-----Format Tab-----  
          
          tabPanel("Format",
            tabsetPanel(type = "tabs",
                        
              tabPanel("Ellipses",
                h3("Change the Shape and Text Format"),
                sliderInput("width", "Set the ellipsis width:", 0, 1, .75, .01),
                sliderInput("height", "Set the ellipsis height:", 0, 1, .5, .01),
                numericInput("size", "Hypothesis Text Size", 8, 1, 10, 1),
                numericInput("digits", "# Digits for weight/alpha",3,1,6,1),
                sliderInput("rotation", "Set the rotation", -1, 1, 0, .005),
                br(),
                h3("Set the Positions"),
                p("By default, any custom positioning data will be lost if you add hypotheses or set the rotation (hypotheses will become equally spaced again).
                  Check the box below to keep custom positions, place all new hypotheses at position (0,0), and maintain current spacing when rotating."),
                checkboxInput("chkCustomPositions", label = "Keep Custom Positions", value = FALSE),
                rHandsontableOutput("hotPositions"),
                actionButton("updatePositions", "Update Custom Positions")
              ), # end Ellipses subtab 
              
              tabPanel("Connections",
                br(),
                numericInput("boxtextsize", "Transition Text Size", 6, 1, 10, 1),
                numericInput("trhw", "Transition Box Width", .13, .05, .3, .01),
                numericInput("trhh", "Transition Box Height", .1, .05, .3, .01),
                numericInput("trdigits", "# Digits for transitions",2,1,6,1),
                sliderInput("trprop", "Transition positioning",.05,.95,.33,.01),
                numericInput("arrowsize", "Arrow Size", .035, .005, .6, .005)
              ), # end Connections subtab
              
              tabPanel("Colors",
                checkboxInput("chkAddColors", label = "Add Colors", value = FALSE),
                
                conditionalPanel(
                    condition = "input.chkAddColors == true",
                    h3("Set the Colors"),
                    selectInput("palette", "Select the Color Palette:", c("Dark2", "Greyscale", "Color Blind","All Colors")),
                    uiOutput("colorSet")
                ), # end Add Colors conditional panel
                
                checkboxInput("chkLegend", label = "Show Legend", value = FALSE),
                
                conditionalPanel(
                    condition = "input.chkLegend == true",
                    textInput("txtLegendName", label = "Legend Name:", value = "Group Name"),
                    numericInput("legendtextsize","Legend text size",20,6,14,1)
                ) # end Show Legend conditional panel
                
              ) # end Colors subtab
            ) # end tabsetPanel (subtabs)
          ) # end Format tab
        
        ) # end tabsetPanel (maintabs)
      ), # end sidebarPanel
      
      
      #-----Plot and Code Main Panel-----  
      mainPanel(
        # Custom HTML to trigger JS which downloads image from DOM
        #HTML('<a href="#" role="button" class="btn btn-default" onclick="prepHref(this)" download><i class="fa fa-download"></i>Download Image</a>'),
        
        tabsetPanel(
          
          #-----Graph Tab-----  
          
          tabPanel("Graph",
            plotOutput("thePlot")
          ), # end Graph tab 
          
          #-----Code Tab-----  
          tabPanel("Code",
            br(),
            p("Code from both the below tabs is needed in order to replicate this hGraph."),
            downloadButton("downloadCode", "Download Code to an R File"),
            br(),
            br(),
            tabsetPanel(
              
              tabPanel("Function call for creating the hGraph",
                       br(),
                       p("This code can be updated when changes are made to the graph."),
                       br(),
                       actionButton("updateCode", "Update Code"),
                       verbatimTextOutput("changingCode")
              ), # end Changing Code subtab
              
              tabPanel("Supporting code for creating the hGraph",
                       br(),
                       p("This code stays constant throughout graph changes."),
                       br(),
                       verbatimTextOutput("fixedCode")
              ) # end Fixed Code subtab
              
            )
          ) # end Code tab
        ) # end tabset panel
      ) # end main panel
    ) # end sidebarlayout 
  ) # end fluid page 
} # end ui 

shinyUI(ui)


