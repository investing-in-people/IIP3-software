library(shiny)
library(shinydashboard)
library(mvtnorm)
library(stringr)
library(shinyjs)

#turn off scientific notation
options(scipen=999)

#This work is licensed under the GNU General Public License v3.0. To view a copy of this license, visit 
#https://www.gnu.org/licenses/gpl-3.0.html

#Cost of employee separations module. Support for the R Shiny modules in this book was provided by the Society for Human Resource 
#Management (SHRM) Chief Knowledge Officer, Alex Alonso, and the SHRM Knowledge Division. Module designers and programmers are Leo 
#Alexander III, Evan Mulfinger, and Frederick L. Oswald at Rice University. Modules are based on formulas and context provided in the 
#book: Cascio, W., Boudreau, J., & Fink. A. (year). Investing in people: The financial impact of human resource initiatives (3rd Ed.). 
#Upper Saddle River, NJ: FT Press. is licensed under a Creative Commons Attribution 4.0 International License.

#set characters for naylor-shine radio buttons
caseNames <- list(1,2,3)
caseText <- list("Case 1: &Phi;<sub>i</sub> known, solve for mean criterion score Z<sub>yi</sub>", "Case 2: Z<sub>xi</sub> Known, solve for mean criterion score Z<sub>yi</sub>","Case 3: Mean criterion score Z<sub>yi</sub> known, solve for &Phi;<sub>i</sub> and Z<sub>xi</sub>")
caseHtml <- lapply(caseText, HTML)

#download/extract exchange rates
rawText <- readLines('https://www.ecb.europa.eu/stats/eurofxref/eurofxref-daily.xml')[9:40]

country <- lapply(rawText, function (x) {
res <- str_match(x, "currency='(.*?)' rate")
res[,2]
})

rate <- lapply(rawText, function (x) {
  res <- str_match(x, "rate='(.*?)'/>")
  res[,2]
})

#create list of exchange rates (base US Dollars)
currRat <- as.numeric(c(unlist(rate)))/as.numeric(c(unlist(rate))[1])
names(currRat) <- c('US Dollars',
                  'Yen',
                  'Bulgarian Levs',
                  'Czech Korunas',
                  'Danish Kroner',
                  'Pounds Sterling',
                  'Forints',
                  'Zloty',
                  'Romanian Lei',
                  'Swedish Kronor',
                  'Swiss Francs',
                  'Iceland Kronur',
                  'Norwegian Kroner',
                  'Kunas',
                  'Russian Rubles',
                  'Turkish Lire',
                  'Australian Dollars',
                  'Brazilian Reais',
                  'Canadian Dollars',
                  'Yuan Renminbi',
                  'Hong Kong Dollars',
                  'Rupiahs',
                  'New Israeli Sheqalim',
                  'Indian Rupees',
                  'Won',
                  'Mexican Pesos',
                  'Malaysian Ringgit',
                  'New Zealand Dollars',
                  'Philippine Pesos',
                  'Singapore Dollars',
                  'Thai Baht',
                  'Rand')

#create list of currency symbols
currSym <- c("\U00024",
            "\U000A5",
            "\U0043B\U00432",
            "\U0004B\U0010D",
            "\U0006B\U00072",
            "\U000A3",
            "\U00046\U00074",
            "\U0007A\U00142",
            "\U0006C\U00065\U00069",
            "\U0006B\U00072",
            "\U00043\U00048\U00046",
            "\U0006B\U00072",
            "\U0006B\U00072",
            "\U0006B\U0006E",
            "\U00440\U00443\U00431",
            "\U020BA",
            "\U00024",
            "\U00052\U00024",
            "\U00024",
            "\U000A5",
            "\U00024",
            "\U00052\U00070",
            "\U020AA",
            "\U020B9",
            "\U020A9",
            "\U00024",
            "\U00052\U0004D",
            "\U00024",
            "\U020B1",
            "\U00024",
            "\U00E3F",
            "\U00052")
names(currSym) <- names(currRat)

currSymHTML <- c("&#36;",
                "&#165;",
                "&#1083;&#1074;",
                "&#75;&#269;",
                "&#107;&#114;",
                "&#163;",
                "&#70;&#116;",
                "&#122;&#322;",
                "&#108;&#101;&#105;",
                "&#107;&#114;",
                "&#67;&#72;&#70;",
                "&#107;&#114;",
                "&#107;&#114;",
                "&#107;&#110;",
                "&#1088;&#1091;&#1073;",
                "&#8378;",
                "&#36;",
                "&#82;&#36;",
                "&#36;",
                "&#165;",
                "&#36;",
                "&#82;&#112;",
                "&#8362;",
                "&#8377;",
                "&#8361;",
                "&#36;",
                "&#82;&#77;",
                "&#36;",
                "&#8369;",
                "&#36;",
                "&#3647;",
                "&#82;")
names(currSymHTML) <- names(currRat)


######################################################

#user interface
ui <- dashboardPage(
  
  #title in header
  dashboardHeader(title = "Investing in People Online", disable = T),
  
  #create sidebar containing user input interface
  dashboardSidebar(
    
    #adjust the sidebar top margin
    tags$style(".left-side, .main-sidebar {padding-top:0px;padding-bottom:0px;position:fixed;}"),
    
    #sidebar menu items
    sidebarMenu(
      
      id = "sidebarmenu",
      
      br(),
      
      div(style = "text-align:center", h4("Investing in People")),
      
      br(),
      
      menuItem("Home", tabName = "home", icon = icon("home", lib = "font-awesome"), selected = TRUE),
            
      menuItem("Absenteeism", tabName = "absenteeism", icon = icon("clock", lib = "font-awesome")),
      
      menuItem("Employee Separations", tabName = "separation", icon = icon("exchange", lib = "font-awesome")),
      
      menuItem("Health, Wellness, & Welfare", tabName = "welfare", icon = icon("heartbeat", lib = "font-awesome")),
      
      menuItem("Attitudes & Engagement", tabName = "engagement", icon = icon("smile")),
      
      menuItem("Workplace Flexibility", tabName = "worklife", icon = icon("balance-scale", lib = "font-awesome")),
      
      menuItem("Staffing Utility", tabName = "utilityMenu", icon = icon("users", lib = "font-awesome"),
      
        menuSubItem("Utility Models", tabName = "staffing"),

        menuSubItem("Job Performance", tabName = "performance")),
      
      menuItem("Enhanced Selection", tabName = "enhancedBCG", icon = icon("chart-line", lib = "font-awesome")),

      menuItem("HR Development", tabName = "development", icon = icon("chart-pie", lib = "font-awesome")),
      
      br(),
      
      br(),

      menuItem("Manage Data", tabName = "data", icon = icon("file-import", lib = "font-awesome")),
      
      menuItem("Settings", tabName = "settings", icon = icon("cog", lib = "font-awesome")),
      
      menuItem("Tutorial", tabName = "tutorial", icon = icon("book-open", lib = "font-awesome")),
      
      menuItem("FAQs", tabName = "faqs", icon = icon("question-circle", lib = "font-awesome")),
      
      menuItem("Contact Us", tabName = "contact", icon = icon("envelope", lib = "font-awesome"))
      
    )
  
  ),
  
  #output panels
  dashboardBody(class="dashBody",
    
    #CSS tag to fix sidebar
    tags$head(tags$script(type="text/javascript",'$(document).ready(function(){
                           $(".main-sidebar").css("height","100%");
                           $(".main-sidebar .sidebar").css({"position":"relative","max-height": "100%","overflow": "auto"})
                           })')),

    #CSS tag to make sure page extends to the bottom of the browser when header is disabled
    tags$style(".dashBody {min-height: 100vh !important};"),

    #CSS tag to remove headers from tables
    tags$head(tags$style(type = "text/css", "#absTab th {display:none;}")),
    tags$head(tags$style(type = "text/css", "#sepTab th {display:none;}")),
    tags$head(tags$style(type = "text/css", "#repTab th {display:none;}")),
    tags$head(tags$style(type = "text/css", "#traTab th {display:none;}")),
    tags$head(tags$style(type = "text/css", "#perfTab th {display:none;}")),
    tags$head(tags$style(type = "text/css", "#sepTotTab th {display:none;}")),
    tags$head(tags$style(type = "text/css", "#alcTab th {display:none;}")),
    tags$head(tags$style(type = "text/css", "#eAPTab th {display:none;}")),
    tags$head(tags$style(type = "text/css", "#engTab3 th {display:none;}")),
    tags$head(tags$style(type = "text/css", "#wLifTab th {display:none;}")),
    tags$head(tags$style(type = "text/css", "#tayRusTab th {display:none;}")),
    tags$head(tags$style(type = "text/css", "#nayShiTab th {display:none;}")),
    tags$head(tags$style(type = "text/css", "#brEvTab1 th {display:none;}")),
    tags$head(tags$style(type = "text/css", "#brEvTab2 th {display:none;}")),
    tags$head(tags$style(type = "text/css", "#traUtTab th {display:none;}")),
    tags$head(tags$style(type = "text/css", "#meetTab th {display:none;}")),
    tags$head(tags$style(type = "text/css", "#oneCohUtTab th {display:none;}")),
    tags$head(tags$style(type = "text/css", "#multCohUtTab th {display:none;}")),
    tags$head(tags$style(type = "text/css", "#eBCGTab th {display:none;}")),
    tags$head(tags$style(type = "text/css", "#CREPIDTab th {display:none;}")),
    tags$head(tags$style(type = "text/css", "#globEstTab th {display:none;}")),

    #CSS tag to format drop down menus
    tags$style(type='text/css', ".selectize-input {font-size: 12px; line-height: 21px; padding: 5px;}
      .selectize-dropdown {font-size: 12px; line-height: 20px; padding: 0px;}"),

    #CSS tags to format boxes and inputs
    tags$head(tags$style(HTML('
      .box-body {
        margin-left: 15px;
        margin-right: 15px;
      }
      .nav-tabs-custom .tab-content{
        margin-left: 15px;
        margin-right: 15px;
      }
      .nav-tabs-custom .nav-tabs li.active {
        border-top-color: #558E3F;
      }
      .skin-blue .main-header .logo {
        background-color: #558E3F;
      }
      .skin-blue .main-header .logo:hover {
        background-color: #558E3F;
      }
      .skin-blue .main-header .navbar {
        background-color: #558E3F;
      }
      .skin-blue .main-header .navbar .sidebar-toggle:hover {
        background-color: #467534;
      }
      .skin-blue .sidebar-menu li.active a {
        border-left-color: #558E3F;
      }
      .skin-blue .sidebar-menu li:hover a {
        border-left-color: #558E3F;
      }
      .skin-blue .sidebar-menu .treeview-menu li.active a {
        border-left-color: #558E3F;
      }
      .skin-blue .sidebar-menu .treeview-menu li:hover a {
        border-left-color: #558E3F;
      }
      .skin-blue .sidebar-menu li a {
        padding-bottom:7px;
        padding-top:7px;
      }
      .box{
        border-top-color: #558E3F;
      }
      .form-control {
        font-size:12px;
        height: 21px;
        width: 60px;
        padding: 1px;
      }
      label {
        font-weight: 400;
      }
      .input-group .form-control:last-child {
        font-weight:normal;
        height: 34px;
      }')),

      #CSS tag to suppress
      tags$style(type="text/css",
         ".shiny-output-error { visibility: hidden; }",
         ".shiny-output-error:before { visibility: hidden; }"
      )
    ),

    #script to allow links to menu items
    tags$script(HTML("
      var openTab = function(tabName){
        $('a', $('.sidebar')).each(function() {
          if(this.getAttribute('data-value') == tabName) {
            this.click()
          };
        });
      }
    ")),

    #tabset container
    tabItems(
      
      #first menu item
      tabItem(tabName = "home",

        HTML("<h3>&nbsp;&nbsp;&nbsp;Welcome to Investing in People Online</h3>"),

        fluidRow(

          column(9,

            br(),

            box(width = 12, style = "font-size:14px",
  
              br(),
              
              fluidRow(
                
                div(class = "col-sm-12 col-md-12 col-lg-12", style="text-align:left",
                
                  column(12,
                    
                    fluidRow(
                      
                      column(12,
                        
                        HTML("<a href='https://store.shrm.org/investing-in-people-financial-impact-of-human-resource-initiatives-third-edition.html' target='_blank'><img style='padding: 20px; padding-top: 0px; float: right;' src='cover.jpg', width='250'></a>"),
                    
                        p("The demand for accountability among all business functions has never been greater. A key responsibility of human resource (HR) leaders and consultants is to articulate the logical connections between progressive HR practices and firm performance, and the need to demonstrate those connections with data."),
                        
                        tags$p(HTML("<i>Investing in People</i> (Third Edition) provides specific formulas and calculations that you can use to evaluate the impact of your own talent decisions. To make the formulas easier to use, <a href='http://www.shrm.org' target='_blank'>The Society for Human Resource Management (SHRM)</a> sponsored the development of the updated software to accompany the chapters on the following topics: <ul><li>employee separations (turnover)</li><li>absenteeism</li><li>health and welfare</li><li>attitudes and engagement</li><li>workplace flexibility programs</li><li>staffing utility</li><li>payoffs from improved selection</li><li>payoffs from training and development.</li></ul>")),
                        
                        p("This software is designed to help you quickly implement the logic and technology in this book to look inside the 'black box' between HR practices and financial/business performance."),
                        
                        hr(),
                        
                        p("To get started, we recommend reviewing the following tutorials:"),
                        
                        HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"),a("General Overview", onclick = "openTab('tutorial')", href="#overview"),
                        
                        br(),
                        
                        HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"),a("Supported Currencies", onclick = "openTab('tutorial')", href="#currencies"),
                        
                        br(),
                        
                        HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"),a("Entering Global Data", onclick = "openTab('tutorial')", href="#global"),
                        
                        br(),
                        
                        HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"),a("Saving My Data", onclick = "openTab('tutorial')", href="#saving"),
                        
                        br(),
                        
                        HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"),a("Privacy & Security", onclick = "openTab('tutorial')", href="#privacy"),
                        
                        br(),
                        
                        hr(),
                        
                        HTML("<a href='https://store.shrm.org' target='_blank'><img style='padding: 20px; padding-top: 0px; float: right;' src='SHRMLogo.png', width='150'></a>"),
                        
                        p(HTML("<i>Investing in People Online</i> was developed thanks to the generous support provided by the Society for Human Resource Management (SHRM). This open-source software was created using R Shiny and is made available under the "), 
                        a("GNU General Public License v3.0,", href = "https://www.gnu.org/licenses/gpl-3.0.html"),
                        "allowing users the freedom to run, study, share and modify the software as long as they track changes and credit the source code appropriately. See the previous link to the GNU GPL license for information on how to do this. The software may be used here online or downloaded from ",
                        a("this", href = "https://github.com/investing-in-people/IIP3-software"),
                        " GitHub repository to be run on a local computer using R Studio.",
                        br(),
                        div(style="margin-bottom:5px;","To cite this software in publications, please use:"),
                        HTML(" Alexander III, L., Mulfinger, E., & Oswald, F. L. (2019). <i>Investing in People Online</i> (Version 2.0) [Software], Rice University, Houston, Texas. Available from https://orgtools.shinyapps.io/IIP3/"),
                        br(),
                        br(),
                        " Please see SHRM's privacy ", 
                        a("statement ",  href = "https://www.shrm.org/about-shrm/pages/privacy-policy.aspx"),
                        " to learn more about your privacy when you use this or other online resources provided by SHRM.")

                      )
                      
                    )
                    
                  )
                  
                )
                
              )

            )
            
          )

        )

      ),
    
      #second menu item
      tabItem(tabName = "absenteeism",

        fluidRow(
          
          column(9,
            
            HTML("<h3>&nbsp;&nbsp;&nbsp;Cost of Absenteeism</h3>"),
            
            br(),
            
            box(width = 12,
                      
              uiOutput("absUI"),
              
              hr(),
                    
              br(),
              
              tags$h4("Estimated Cost of Absenteeism"),
              
              br(),
              
              tableOutput("absTab"),
              
              br(),
              
              plotOutput("absPie")
              
            )
    
          )
            
        )
          
      ),   
      
      #third menu item
      tabItem(tabName = "separation",
        
        fluidRow(
          
          column(9,
        
            HTML("<h3>&nbsp;&nbsp;&nbsp;Cost of Employee Separations</h3>"),
            
            br(),
            
            tabBox(width = 12,
              
              #first separation tab
              tabPanel(title = "Separation Costs",
                
                uiOutput("sepUI"),
                
                uiOutput("unempTaxInputs"),

                hr(),                    
    
                br(),
      
                tags$h4("Estimated Cost of Employee Separations"),
      
                br(),
      
                tableOutput("sepTab"),
                
                br(),
      
                plotOutput("sepPie")

              ),
              
              #second separation tab
              tabPanel(title = "Replacement Costs",
                
                fluidRow(
                  
                  column(12,
                
                    uiOutput("repUI"),
                    
                    uiOutput("medExamInputs")
                    
                  )
                  
                ),
                
                fluidRow(
                  
                  column(12,
                    
                    hr(),
                    
                    br(),
                    
                    tags$h4("Estimated Cost of Employee Replacements"),
                    
                    br(),
                             
                    tableOutput("repTab"),
                    
                    br(),
                    
                    plotOutput("repPie")
                    
                  )
                  
                )
                
              ),
              
              #third separation tab
              tabPanel(title = "Training Costs",
                
                fluidRow(
                  
                  column(12,
          
                    uiOutput("traUI"),
                    
                    hr(),
                    
                    br(),
                    
                    tags$h4("Estimated Cost of Employee Training"),
                    
                    br(),
                             
                    tableOutput("traTab"),
                    
                    br(),
                    
                    plotOutput("traPie")
                    
                  )
                  
                )
                
              ),
              
              #fourth separation tab
              tabPanel(title = "Performance Costs",
                
                fluidRow(
                  
                  column(12,
                
                    uiOutput("perfUI"),
                    
                    uiOutput("diffPerfInputs")
                    
                  )
                  
                ),
                
                fluidRow(
                  
                  column(12,
                
                    hr(),
                    
                    br(),
                    
                    tags$h4("Estimated Performance Costs"),
                    
                    br(),
                             
                    tableOutput("perfTab")
                    
                  )
                  
                )
                
              ),
              
              #fifth separation tab
              tabPanel(title = "Total Costs",
                
              uiOutput("totSepUI"),
                    
              tags$h4("Total Estimated Costs of Employee Separations"),
                  
              br(),
                           
              tableOutput("sepTotTab"),
                
              br(),
              
              plotOutput("sepTotPie")
              
              )
              
            )
            
          )
          
        )
        
      ),
      
      #fourth menu item
      tabItem(tabName = "welfare",

        fluidRow(

          column(9,

            HTML("<h3>&nbsp;&nbsp;&nbsp;Employee Health, Wellness, and Welfare</h3>"),

            br(),

            tabBox(width = 12,

              #first welfare tab
              tabPanel(title = "Cost of Alcohol Abuse", 

                uiOutput("alcUI"),

                hr(),

                br(),

                tags$h4("Estimated Cost of Alcohol Abuse"),

                br(),

                tableOutput("alcTab")

              ),

              #second welfare tab
              tabPanel(title = "Return on EAP Investment",

                uiOutput("eAPUI"),

                hr(),

                br(),

                tags$h4("Estimated Return on EAP Investment"),

                br(),

                tableOutput("eAPTab")

              )

            )

          )

        )

      ),
      
      #fifth menu item
      tabItem(tabName = "engagement",
        
        fluidRow(
          
          column(9,
            
            HTML("<h3>&nbsp;&nbsp;&nbsp;Employee Attitudes and Engagement</h3>"),
            
            br(),
            
            box(width = 12,
              
              uiOutput("engUI"),
              
              hr(),
    
              br(),
            
              tags$h4("Estimated Benefit of Engagement"),
              
              br(),
              
              tags$h5("Outcome results:"),
              
              br(),
                       
              tableOutput("engTab1"),
              
              br(),
              
              br(),
              
              tags$h5("Outcome improvement from Period 1 to Period 2 between the two quartiles:"),
              
              br(),
                       
              tableOutput("engTab2"),
              
              br(),
              
              br(),
              
              tags$h5("Engagement differences:"),
              
              br(),
                       
              tableOutput("engTab3")
              
            )
              
          )
            
        )

      ),
      
      #sixth menu item
      tabItem(tabName = "worklife",
          
        fluidRow(
        
          column(9,
        
            HTML("<h3>&nbsp;&nbsp;&nbsp;Workplace Flexibility Programs</h3>"),
            
            br(),
            
            box(width = 12,
            
              uiOutput("wLifUI"),
              
              hr(),
                
              br(),
            
              tags$h4("Estimated Benefit of Workplace Flexibility Programs"),
              
              br(),              
                       
              tableOutput("wLifTab")
              
            )
          
          )
          
        )
        
      ),

      #seventh menu item
      tabItem(tabName = "staffing",
        
        fluidRow(
          
          column(9,
        
            HTML("<h3>&nbsp;&nbsp;&nbsp;Staffing Utility</h3>"),
            
            br(),
            
            tabBox(width = 12,
              
              #first staffing tab
              tabPanel(title = "Taylor-Russell Model",
                
                fluidRow(
                  
                  column(12,
                
                    uiOutput("tayRusUI"),
    
                    hr(),                    
        
                    br(),
          
                    tags$h4("Taylor-Russell Results"),
          
                    br(),
          
                    tableOutput("tayRusTab"),
                    
                    br(),
          
                    plotOutput("tayRusChart")
                    
                  )
                  
                )
  
              ),
              
              #second staffing tab
              tabPanel(title = "Naylor-Shine Model",
                
                fluidRow(
                  
                  column(12,
                
                    uiOutput("nayShiUI"),
                    
                    uiOutput("nayShiInputs"),
                    
                    uiOutput("calcSelInputs")
                    
                  )
                  
                ),
                  
                fluidRow(
                  
                  column(12,
                
                    hr(),
                    
                    br(),
                    
                    tags$h4("Naylor-Shine Results"),
                    
                    br(),
                             
                    tableOutput("nayShiTab"),
                    
                    br(),
                    
                    plotOutput("nayShiChart")
                    
                  )
                  
                )
                
              ),
              
              #third separation tab
              tabPanel(title = "Brogden-Cronbach-Gleser Model",
                
                uiOutput("bCGUI"),
                
                hr(),
                
                br(),
                
                tags$h4("Brogden-Cronbach-Gleser Model Results"),
                
                br(),
                         
                tableOutput("bCGTab"),
                
                br(),
                
                plotOutput("bCGChart")
                
              )
              
            )
            
          )
          
        )
        
      ),
      
      #eighth menu item
      tabItem(tabName = "enhancedBCG",
          
        fluidRow(
        
          column(9,
        
            HTML("<h3>&nbsp;&nbsp;&nbsp;Brogden-Cronbach-Gleser Utility</h3>"),
            
            br(),
            
            box(width = 12,
            
              uiOutput("eBCGUI1"),

              uiOutput("eBCGUI2"),

              uiOutput("eBCGUI3"),

              uiOutput("eBCGUI4"),
                            
              hr(),
                
              br(),
            
              tags$h4("Brogden-Cronbach-Gleser Utility Results"),
              
              br(),              
                       
              tableOutput("eBCGTab"),
              
              plotOutput("eBCGPlot")
              
            )
          
          )
          
        )
        
      ),
      
      #ninth menu item
      tabItem(tabName = "performance",
          
        fluidRow(
        
          column(9,
        
            HTML("<h3>&nbsp;&nbsp;&nbsp;Economic Value of Job Performance</h3>"),
            
            br(),
            
            tabBox(width = 12,
              
              #first job performance tab
              tabPanel(title = "CREPID Model",
                
                fluidRow(
                
                  column(12,
                    
                    uiOutput("CREPIDUI"),
                    
                    hr(),
                      
                    br(),
                  
                    tags$h4("Overall Economic Value using CREPID"),
                    
                    br(),              
                             
                    tableOutput("CREPIDTab")
                    
                  )
                  
                )
                
              ),
              
              #second job performance tab
              tabPanel(title = "Global Estimation Procedure",
                
                fluidRow(
                
                  column(12,
                    
                    uiOutput("globEstUI"),
                    
                    hr(),
                      
                    br(),
                  
                    tags$h4("Global Estimation Procedure Results"),
                    
                    br(),              
                             
                    tableOutput("globEstTab")
                    
                  )
                  
                )
                
              )
              
            )
          
          )
          
        )
        
      ),
      
      #tenth menu item
      tabItem(tabName = "development",
        
        fluidRow(
        
          column(9,
        
            HTML("<h3>&nbsp;&nbsp;&nbsp;Costs/Benefits of HR Development</h3>"),
            
            br(),
            
            tabBox(width = 12,

              #first development tab
              tabPanel(title = "Break-even Analysis",
                
                fluidRow(
                  
                  column(12,
                
                    uiOutput("brEvUI1"),
                    
                    plotOutput("brEvPlot1"),
                    
                    HTML("Note: <SPAN STYLE='text-decoration:overline'><i>Z</i></SPAN><sub>u</sub> and <SPAN STYLE='text-decoration:overline'><i>Z</i></SPAN><sub>t</sub> are the mean standardized performance scores of untrained and trained employees, resepectively."),
    
                    hr(),                    
        
                    br(),
          
                    tags$h4("Break-even Analysis Results"),
          
                    br(),
          
                    tableOutput("brEvTab1"),
                    
                    br(),

                    uiOutput("brEvUI2"),
                    
                    plotOutput("brEvPlot2"),
                    
                    HTML("Note: <SPAN STYLE='text-decoration:overline'><i>Z</i></SPAN><sub>u</sub> and <SPAN STYLE='text-decoration:overline'><i>Z</i></SPAN><sub>t</sub> are the mean standardized performance scores of untrained and trained employees, resepectively."),

                    hr(),                    
        
                    br(),
          
                    tags$h4("Break-even Analysis Results"),
          
                    br(),
          
                    tableOutput("brEvTab2")
                    
                  )
                  
                )
  
              ),
              
              #second development tab
              tabPanel(title = "Training Utility",
                
                uiOutput("traUtUI"),
                
                hr(),
                
                br(),
                
                tags$h4("Estimated Utility of a Training Program"),
                
                br(),
                         
                tableOutput("traUtTab")
                
              ),
              
              #third development tab
              tabPanel(title = "Off-site Meetings",
                
                uiOutput("meet1UI"),
                
                uiOutput("meet2UI"),
                
                uiOutput("meet3UI"),
                
                uiOutput("meet4UI"),
                
                uiOutput("meet5UI"),
                
                uiOutput("meet6UI"),
                
                hr(),
                
                br(),
                
                tags$h4("Estimated Cost of Off-site Meetings"),
                
                br(),
                         
                tableOutput("meetTab")
                
              ),
              
              #fourth development tab
              tabPanel(title = "One Cohort Utility",
                
                uiOutput("oneCohUtUI"),
                
                hr(),
                
                br(),
                
                tags$h4("Estimated Training Utility for One Cohort"),
                
                br(),
                         
                tableOutput("oneCohUtTab")
                
              ),
              
              #fifth development tab
              tabPanel(title = "Multiple Cohorts Utility",
                
              uiOutput("multCohUtUI"),
                
              hr(),
                
              br(),
                    
              tags$h4("Change in Utility with Economic Corrections (multiple periods)"),
                  
              br(),
                           
              tableOutput("multCohUtTab")
              
              )
              
            )
          
          )
          
        )
        
      ),
                  
      #twelth menu item
      tabItem(tabName = "data",
                      
        fluidRow(
          
          column(9,
                      
            HTML("<h3>&nbsp;&nbsp;&nbsp;Manage Your Data</h3>"),
            
            br(),
            
            box(width = 12,
              
              br(),              
                  
              tags$h4("Save Data"),
              
              #create save button
              downloadButton("saveData", "Save"),
                                          
              br(),
              
              br(),
              
              tags$h4("Load Data"),
              
              #create load button
              fileInput("uploadFile",
                label = NULL,
                accept = c("text/csv",
                          "text/comma-separated-values,text/plain",
                          ".csv"),
                multiple = FALSE,
                width = '350px'
                
              )
                
            )
          
          )
          
        )    
        
      ),
      
      #thirteenth menu item
      tabItem(tabName = "settings",
                      
        fluidRow(
          
          column(9,
            
            HTML("<h3>&nbsp;&nbsp;&nbsp;Settings</h3>"),
            
            br(),
            
            box(width = 12,
              
              br(),
                
              tags$h4("Currency Settings"),
              
              br(),
              
              uiOutput("currUI"),

              br(),
              
              uiOutput("currencyUI"),
              
              actionButton("convert", "Update"),
              
              br(),
              
              hr(),
              
              br(),
              
              tags$h4("Global Variable Settings"),
              
              br(),
              
              uiOutput("settingsUI")
              
            )
          
          )
          
        )
        
      ),
      
      #fourteenth menu item
      tabItem(tabName = "tutorial",
                      
        fluidRow(
          
          column(9,
            
            HTML("<h3>&nbsp;&nbsp;&nbsp;Tutorial</h3>"),
            
            br(),
            
            box(width = 12,
            
              br(),
              
              tags$a(id="overview",""),
                  
              tags$h4(tags$b("General Overview")),
              
              br(),

              p(HTML('The updated software that accompanies <i>Investing in People</i> (Third Edition) includes 
              nine modules designed to help you calculate common, yet often overlooked, HR costs and benefits. To 
              perform a calculation for a particular module, click on that module in the sidebar menu on the left 
              side of the screen, fill out the presented forms, and as information is entered, the calculations will 
              be performed automatically and the appropriate output (e.g., tables, figures) will be populated or created. 
              This allows you to adjust the values entered on the form interactively and entertain "what if" 
              scenarios on the fly. Some modules will produce output before the form is completely filled out. 
              This is because the default value of zero is a valid value for some fields and allows the module 
              to carry out the necessary calculations. However, please <b>fill out all fields with the 
              appropriate values to ensure accurate calculations</b>. When entering a numerical value, you may use
              thousands or decimal separators; but, <b>you must enter them using the following format: ###,###,###.## 
              (e.g., 1,000,000.00)</b>. Note: currency symbols and thousands separators are optional.')),
              
              br(),

              p('In some cases, the modules span several tabs. Each tab represents either a subtotal for a larger 
              calculation (as is the case for the "Cost of Employee Separations" module) or a separate but related 
              module in group of modules (as is the case for the "Staffing Utility" modules), which facilitate three different
              methods for calculating the utility of employee selection procedures.'),
              
              br(),

              p('Let us walk through the most complex module: Cost of Employee Separations. Start by clicking 
              on "Employee Separations" in the sidebar menu on the left side of the screen. Fill out all of the fields 
              in the form under the first tab labelled "Separation costs." Once the form is filled out completely, the 
              appropriate output will be displayed in the results table and a pie chart will appear at the bottom of the 
              form. Continue filling out the forms by clicking on the tabs labeled "Replacement Costs,"  "Training Costs," 
              and "Performance Costs." Eventually, you will arrive at the last tab labelled "Total Costs," where you will 
              see the totals of all your calculations.'),
              
              br(),
              
              tags$a(id="currencies",""),
                  
              tags$h4(tags$b("Currency Conversion")),
              
              br(),

              p(HTML('The software supports many different currencies. To change the currency displayed in the both the inputs and 
              outputs of the software, click on the "Settings" button on the sidebar menu on the left side of the screen, then 
              select your desired currency in the drop-down menu labelled "Select Currency:" and click the "Update" button. All 
              currency inputs and outputs will be converted at the current exchange rate (updated daily) to the currency that you 
              selected. You may change from one currency to another at any time. We recommended that you save your data before 
              changing currencies.')),
              
              br(),
              
              tags$a(id="global",""),
                  
              tags$h4(tags$b("Global Data")),
              
              br(),

              p(HTML('Some values are required for calculations in more than one module. Conveniently, you can enter those values in the
              "Global Data Editor." To change these global data, click on the "Settings" button on the sidebar menu on the left side of 
              the screen, then enter these data in the input fields in the "Global Variable Settings" section and click the "Update 
              Global Settings" button. The data entered in these fields (including zero values) will be entered in all the places they 
              occur in all of the modules. It is important to note that these data will overwrite any data that you have entered in the 
              modules previously. We recommended that you save your data before updating these data.')),
              
              br(),              
              
              tags$a(id="saving",""),
              
              tags$h4(tags$b("Saving My Data")),
              
              br(),

              p(HTML('The data you enter do not persist on the web server once your session is completed. So, <b>it is very important not 
              to click back or refresh your web browser</b>. Only navigate through the software using the menus, tabs, and
              links presented in the software. And save your data often so if you accidentally close your browser or your 
              connection is interrupted, you may resume from your last "Save File" command. Although this may seem inconvenient, it is 
              very secure as your data are completely under your control and are only stored locally on your computer.')),
              
              br(),

              p('To save data, click on the "Manage Data" button on the sidebar menu on the left side of the screen, then click
              on the "Save" button. The system will store your data in a .csv file that will be downloaded to your computer. Some 
              browsers will download the file automatically to your default-download location, while others will open a "Save File" 
              interface allowing you to change the filename and the location of the desired save file. You may name the file anything you 
              wish and store it anywhere you like as long as it is saved as a .csv file and you can find it when you wish to restore 
              your data. This file will contain everything you have entered and can be uploaded at any time to restore your data as 
              it existed at the time of your last save.'),
              
              br(),

              tags$h4(tags$b("Loading Your Data")),
              
              br(),

              p(HTML('To restore your data from a saved file, click on the "Manage Data" button on the sidebar menu on the left side of 
              the screen, then click on the "Browse" button. When the "Open File" dialog appears, direct your browser to the location of
              the desired saved file. When you have found the correct .csv file, click "Open." After it is finished uploading, the progress 
              bar will indicate the status of the upload. Remember: <b>Restoring information from a data file will clear out any data you have 
              entered since the last time you saved!</b>')),

              br(),
              
              tags$a(id="privacy",""),

              tags$h4(tags$b("Privacy & Security")),
              
              br(),

              p(HTML('Your privacy is of the utmost importance to us. The data you transmit to the system are transmitted over an encrypted 
              connection and is not retained after your browsing session ends. The apps are currently hosted by shinyapps.io.
              Click <a href="https://docs.rstudio.com/shinyapps.io/security-and-compliance.html" target="_blank">here</a> for more information 
              about the security of shinyapps.io services. There are not any secret backups or log files that 
              retain your information. This will guarantee maximum security for your data. However, be sure to <b>save early and
              save often</b>, especially when using long modules or multiple modules.'))

            )
            
          )
          
        )
        
      ),
      
      #fifteenth menu item
      tabItem(tabName = "faqs",
                      
        fluidRow(
          
          column(9,
            
            HTML("<h3>&nbsp;&nbsp;&nbsp;Frequently Asked Questions</h3>"),
            
            br(),
            
            box(width = 12,

              br(),

              tags$h4("Who should I contact if something isn't working as expected?"),
              
              br(),

              p('For any questions regarding content or technical issues (including troubles with browser  
              compatibility, incorrect calculations, or printing problems), click on the "Contact Us" button on the 
              sidebar menu on the left side of the screen, then complete all fields in the form,
              then click "Submit."'),
              
              br(),

              tags$h4("How can I ensure that my data are secure?"),
              
              br(),
              
              'Your privacy is of the utmost importance to us. The data you transmit to the system are transmitted over an 
              encrypted connection and is not retained after your browsing session ends. The apps are currently hosted by shinyapps.io.
              Click ',HTML("<a href='http://www.shrm.org' target='_blank'>here</a>"),' for more information about the security of 
              shinyapps.io services. Data will only be stored on your local 
              computer and only when and if you choose to do so (we recommend doing so frequently to avoid data loss). We also 
              recommend using the same security precautions with your save files on your personal computer that you would take 
              with any files containing sensitive data.',

              br(),
              
              br(),
              
              tags$a(id="citation",""),
              
              tags$h4("How do I cite this software?"),
              
              br(),

              HTML("Alexander III, L., Mulfinger, E., & Oswald, F. L. (2019). <i>Investing in People Online</i> (Version 2.0) [Software], Rice University, Houston, Texas. Available from https://orgtools.shinyapps.io/IIP3/"),
              
              br(),
              
              br()

            )
            
          )
          
        )
        
      ),
      
      #sixteenth menu item
      tabItem(tabName = "contact",
                      
        fluidRow(
          
          column(9,
            
            HTML("<h3>&nbsp;&nbsp;&nbsp;Contact Us</h3>"),
            
            br(),
            
            box(width = 12,

              
              br(),

              textAreaInput("name", label = "Enter your name:", width = 200, height = 22),
              
              br(),
              
              textAreaInput("email", label = "Enter your email address:", width = 200, height = 22),
              
              br(),
              
              selectInput("reason", label = "Reason for message:", choices = c("Select an Option","Content Issue","Technical Issue","General Comment","Other"), selected = "Select an Option", width = 400),
                
              br(),
              
              textAreaInput("body", label = "Enter your message:", width = 400, height = 120),
              
              br(),
              
              actionButton("send", label = "Send Message"),
              
              br()

            )
            
          )
          
        )
        
      )

    
    ),
    
  useShinyjs())
  
)

server <- function(input, output, session) {
  
  runjs('
    var el2 = document.querySelector(".skin-blue");
    el2.className = "skin-blue sidebar-mini";
  ')
  
  runjs({'
    var el2 = document.querySelector(".skin-blue");
    el2.className = "skin-blue sidebar-mini";
    var clicker = document.querySelector(".sidebar-toggle");
    clicker.id = "switchState";
  '})
  
  onclick('switchState', runjs({'
    var title = document.querySelector(".logo")
    if (title.style.visibility == "hidden") {
      title.style.visibility = "visible";
    } else {
      title.style.visibility = "hidden";
    }
  '}))

  ###all apps###
    
  #reactive values to store user data
  data <- reactiveValues(absentWorkersArePaid = "Yes",
                        currency = "US Dollars",
                        currOld = "US Dollars",
                        diffPerfMethod = "Calculated",
                        eBCGSelEcFac = "Yes",
                        eBCGSelMultCoh = "Yes",
                        medExamType = "In-house",
                        meetChargeMethod = "Itemized costs",
                        meetDevCostMethod = "Itemized costs",
                        meetFacCostMethod = "Itemized costs",
                        nayShiCalcSel = 1,
                        orgSubjectToUnempTax = "Yes",
                        adminPreEmpHRTime = 0,
                        agencyFee = 0,
                        alcNumIndF1 = 0,
                        alcNumIndF2 = 0,
                        alcNumIndF3 = 0,
                        alcNumIndM1 = 0,
                        alcNumIndM2 = 0,
                        alcNumIndM3 = 0,
                        aveAnnCohEarnM1 = 0,
                        aveAnnCohEarnM2 = 0,
                        aveAnnCohEarnM3 = 0,
                        aveAnnCohEarnF1 = 0,
                        aveAnnCohEarnF2 = 0,
                        aveAnnCohEarnF3 = 0,
                        avgHrlyWageBC = 0,
                        avgHrlyWageBC1 = 0,
                        avgHrlyWageBC2 = 0,
                        avgHrlyWageCL = 0,
                        avgHrlyWageCL1 = 0,
                        avgHrlyWageCL2 = 0,
                        avgHrlyWageMGMT = 0,
                        avgHrlyWageMGMT1 = 0,
                        avgHrlyWageMGMT2 = 0,
                        avgHrlyWageSup = 0,
                        avgMentorHrlyPay = 0,
                        avgTraineeHrlyPay = 0,
                        avgTrainerHrlyPay = 0,
                        baseTaxRate = 0,
                        bCGSDy1 = 0,
                        bCGSDy2 = 0,
                        bCGSelQuota1 = 0,
                        bCGSelQuota2 = 0,
                        bCGSR1 = 0,
                        bCGSR2 = 0,
                        bCGTestCostPer1 = 0,
                        bCGTestCostPer2 = 0,
                        bCGVal1 = 0,
                        bCGVal2 = 0,
                        brEvBenDuration = 0,
                        brEvDevYears = 0,
                        brEvDt = 0,
                        brEvNumTrainee1 = 0,
                        brEvNumTrainee2 = 0,
                        brEvSDy = 0,
                        brEvTrainCostPer1 = 0,
                        brEvTrainCostPer2 = 0,
                        compaRatioLeave = 0,
                        compaRatioReplace = 0,
                        contractedExamCost = 0,
                        costMedExamSupplies = 0,
                        costNewEquip = 0,
                        costReducedOutput = 0,
                        costSubEmps = 0,
                        CREPIDAnnSalary = 0,
                        CREPIDTF1 = 0,
                        CREPIDTF2 = 0,
                        CREPIDTF3 = 0,
                        CREPIDTF4 = 0,
                        CREPIDTF5 = 0,
                        CREPIDTF6 = 0,
                        CREPIDTF7 = 0,
                        CREPIDTF8 = 0,
                        CREPIDImp1 = 0,
                        CREPIDImp2 = 0,
                        CREPIDImp3 = 0,
                        CREPIDImp4 = 0,
                        CREPIDImp5 = 0,
                        CREPIDImp6 = 0,
                        CREPIDImp7 = 0,
                        CREPIDImp8 = 0,
                        CREPIDPerf1 = 0,
                        CREPIDPerf2 = 0,
                        CREPIDPerf3 = 0,
                        CREPIDPerf4 = 0,
                        CREPIDPerf5 = 0,
                        CREPIDPerf6 = 0,
                        CREPIDPerf7 = 0,
                        CREPIDPerf8 = 0,
                        deptRepPayrate = 0,
                        eAPCostRepCler = 0,
                        eAPCostRepMgmt = 0,
                        eAPCostRepProd = 0,
                        eAPHospCost = 0,
                        eAPNumEmpCler = 0,
                        eAPNumEmpMgmt = 0,
                        eAPNumEmpProd = 0,
                        eAPNumQuitCler = 0,
                        eAPNumQuitMgmt = 0,
                        eAPNumQuitProd = 0,
                        eAPPctBudgHosp = 0,
                        eBCGAveTenure1 = 0,
                        eBCGAveTenure2 = 0,
                        eBCGDiscRate = 0,
                        eBCGNumEmpsAddPerYear = 0,
                        eBCGNumEmpsSubPerYear = 0,
                        eBCGNumSelInYear = 0,
                        eBCGProgYears = 0,        
                        eBCGSDy = 0,    
                        eBCGSR1 = 0,
                        eBCGSR2 = 0,
                        eBCGTax = 0,
                        eBCGTestCostPer1 = 0,
                        eBCGTestCostPer2 = 0,
                        eBCGVal1 = 0,
                        eBCGVal2 = 0,
                        eBCGVarCosts = 0,
                        empHourLost = 0,
                        engAveScoreBottom = 0,
                        engAveScoreTop = 0,
                        engRevBottom1 = 0,
                        engRevBottom2 = 0,
                        engRevTop1 = 0,
                        engRevTop2 = 0,
                        entInterviewerHrlyPay = 0,
                        extInterviewerHrlyPay = 0,
                        entranceInterviewTime = 0,
                        examinerPayRate = 0,
                        exitInterviewTime = 0,
                        globEst15thPct1 = 0,
                        globEst15thPct2 = 0,
                        globEst15thPct3 = 0,
                        globEst15thPct4 = 0,
                        globEst15thPct5 = 0,
                        globEst15thPct6 = 0,
                        globEst15thPct7 = 0,
                        globEst15thPct8 = 0,
                        globEst15thPct9 = 0,
                        globEst15thPct10 = 0,
                        globEst15thPct11 = 0,
                        globEst15thPct12 = 0,
                        globEst15thPct13 = 0,
                        globEst15thPct14 = 0,
                        globEst15thPct15 = 0,
                        globEst15thPct16 = 0,
                        globEst15thPct17 = 0,
                        globEst15thPct18 = 0,
                        globEst15thPct19 = 0,
                        globEst15thPct20 = 0,
                        globEst50thPct1 = 0,
                        globEst50thPct2 = 0,
                        globEst50thPct3 = 0,
                        globEst50thPct4 = 0,
                        globEst50thPct5 = 0,
                        globEst50thPct6 = 0,
                        globEst50thPct7 = 0,
                        globEst50thPct8 = 0,
                        globEst50thPct9 = 0,
                        globEst50thPct10 = 0,
                        globEst50thPct11 = 0,
                        globEst50thPct12 = 0,
                        globEst50thPct13 = 0,
                        globEst50thPct14 = 0,
                        globEst50thPct15 = 0,
                        globEst50thPct16 = 0,
                        globEst50thPct17 = 0,
                        globEst50thPct18 = 0,
                        globEst50thPct19 = 0,
                        globEst50thPct20 = 0,
                        globEst85thPct1 = 0,
                        globEst85thPct2 = 0,
                        globEst85thPct3 = 0,
                        globEst85thPct4 = 0,
                        globEst85thPct5 = 0,
                        globEst85thPct6 = 0,
                        globEst85thPct7 = 0,
                        globEst85thPct8 = 0,
                        globEst85thPct9 = 0,
                        globEst85thPct10 = 0,
                        globEst85thPct11 = 0,
                        globEst85thPct12 = 0,
                        globEst85thPct13 = 0,
                        globEst85thPct14 = 0,
                        globEst85thPct15 = 0,
                        globEst85thPct16 = 0,
                        globEst85thPct17 = 0,
                        globEst85thPct18 = 0,
                        globEst85thPct19 = 0,
                        globEst85thPct20 = 0,
                        hRPayrate = 0,
                        hRPayrate1 = 0,
                        hRPayrate2 = 0,
                        lumpSum = 0,
                        meetAVEquip = 0,
                        meetAvgAttendee = 0,
                        meetBusSvcs = 0,
                        meetCoffeeBreaks = 0,
                        meetDevCostPrev = 0,
                        meetDuration = 0,
                        meetEquipMaterial = 0,
                        meetFacLumpsum = 0,    
                        meetNumManagers = 0,
                        meetNumMgrAttend = 0,
                        meetNumOffsite = 0,
                        meetOtherCharges = 0,
                        meetOutsideConsult = 0,
                        meetReception = 0,
                        meetRoomRental = 0,
                        meetSleepRooms = 0,
                        meetThreeMeals = 0,
                        meetTraDeptOverhead = 0,
                        meetTransport = 0,
                        meetTraStaff = 0,
                        midpointPay = 0,
                        movingCost = 0,
                        multCohUtAltProcVal = 0,
                        multCohUtAltSelProcCost = 0,
                        multCohUtAveTenure = 0,
                        multCohUtCorpTax = 0,
                        multCohUtDiscRate = 0,
                        multCohUtOrdProcVal = 0,
                        multCohUtOrdSelProcCost = 0,
                        multCohUtProgDur = 0,
                        multCohUtSDy = 0,
                        multCohUtSR = 0,
                        multCohUtVarCosts = 0,
                        multCohUtYrlyAddEmps = 0,
                        multCohUtYrlySubEmps = 0,
                        nayShiSR = 0,
                        nayShiVal = 0,
                        nayShiZxi = 0,
                        nayShiZyi = 0,
                        newHireHrlyPay = 0,
                        numApplicants = 0,
                        numberOfTests = 0,
                        numDaysPerWorkYear = 0,
                        numDaysPerWorkYear1 = 0,
                        numDaysPerWorkYear2 = 0,
                        numEmpOver7k = 0,
                        numEmpsInCompany = 0,
                        numEmpsInCompany1 = 0,
                        numEmpsInCompany2 = 0,
                        numEmpsInCompany3 = 0,
                        numEntranceInterviews = 0,
                        numHrsPerWorkWeek = 0,
                        numHrsPerWorkWeek1 = 0,
                        numHrsPerWorkWeek2 = 0,
                        numMenteesPerMentor = 0,
                        numNewHires = 0,
                        numOTJTrainingHrs = 0,
                        numOTJTrainings = 0,
                        numPositions = 0,
                        numPrograms = 0,
                        numStaffingMeeting = 0,
                        numTrainees = 0,
                        numTurnover = 0,
                        oneCohUtCorpTax = 0,
                        oneCohUtDiscRate = 0,
                        oneCohUtDt = 0,
                        oneCohUtDuration = 0,
                        oneCohUtNumTrainees = 0,
                        oneCohUtSDy = 0,
                        oneCohUtTrainCostPer = 0,
                        oneCohUtVarCosts = 0,
                        orientationHrlyCost = 0,
                        orientationLength = 0,
                        pctAbsentBC = 0,
                        pctAbsentCL = 0,
                        pctAbsentMGMT = 0,
                        pctAlcProbM1 = 0,
                        pctAlcProbM2 = 0,
                        pctAlcProbM3 = 0,
                        pctAlcProbF1 = 0,
                        pctAlcProbF2 = 0,
                        pctAlcProbF3 = 0,
                        pctAlcProdDecrease = 0,
                        pctEmpBenefits = 0,
                        pctReduceAbs = 0,
                        pctReduceSep =0,
                        pctTrainingCostRep = 0,
                        pctTurnoverBC = 0,
                        pctTurnoverCL = 0,
                        pctTurnoverMGMT = 0,
                        propRedTrainingProd = 0,
                        scoringCosts = 0,
                        sepAdminHRTime = 0,
                        sevPayWeeks = 0,
                        staffingMeetingTime = 0,
                        supHrsLostPerDay = 0,
                        supsDealWithAbsence = 0,
                        tayRusBR = 0,    
                        tayRusSR = 0,
                        tayRusVal = 0,
                        testMaterialCosts = 0,
                        timeComm = 0,
                        timeMedExam = 0,
                        timePriorExitInterview = 0,
                        timeTransferInfo = 0,
                        totCostOfAbs = 0,
                        totCostOfSep = 0,
                        trainingLength = 0,
                        traUtNumTrainee = 0,
                        traUtBenDuration = 0,
                        traUtDt = 0,
                        traUtSDy = 0,
                        traUtTrainCostPer = 0,
                        travelCost = 0,
                        unempTaxRate = 0,
                        weightedAveEarnings = 0)
  
  store <- reactiveValues(globSet = NULL,
                          pctSet = NULL,
                          currSet = NULL,
                          xRate = NULL,
                          absCalc = NULL,
                          sepCalc = NULL, 
                          repCalc = NULL, 
                          traCalc = NULL, 
                          perfCalc = NULL,
                          alcCalc = NULL,
                          eAPCalc = NULL,
                          engCalc = NULL,
                          wLifCalc = NULL,
                          bCGCalc = NULL,
                          brEvCalc = NULL,
                          traUtCalc = NULL,
                          meetCalc = NULL,
                          oneCohUtCalc = NULL,
                          multCohUtCalc = NULL,
                          eBCGCalc = NULL,
                          eBCGCalcGraph = NULL,
                          CREPIDCalc = NULL,
                          globEstCalc = NULL)
  
  store$globSet <- c("numEmpsInCompany",
                    "numEmpsInCompany1",
                    "numEmpsInCompany2",
                    "numEmpsInCompany3",
                    "numHrsPerWorkWeek",
                    "numHrsPerWorkWeek1",
                    "numHrsPerWorkWeek2",
                    "numDaysPerWorkYear",
                    "numDaysPerWorkYear1",
                    "numDaysPerWorkYear2",
                    "avgHrlyWageBC",
                    "avgHrlyWageBC1",
                    "avgHrlyWageBC2",
                    "avgHrlyWageCL",
                    "avgHrlyWageCL1",
                    "avgHrlyWageCL2",
                    "avgHrlyWageMGMT",
                    "avgHrlyWageMGMT1",
                    "avgHrlyWageMGMT2")
  
  store$pctSet <- c("pctAbsentBC",
                    "pctAbsentCL",
                    "pctAbsentMGMT",
                    "pctEmpBenefits",
                    "pctTurnoverBC",
                    "pctTurnoverCL",
                    "pctTurnoverMGMT",
                    "unempTaxRate",
                    "baseTaxRate",
                    "pctTrainingCostRep",
                    "propRedTrainingProd",
                    "pctAlcProdDecrease",
                    "pctAlcProbM1",
                    "pctAlcProbM2",
                    "pctAlcProbM3",
                    "pctAlcProbF1",
                    "pctAlcProbF2",
                    "pctAlcProbF3",
                    "eAPPctBudgHosp",
                    "pctReduceAbs",
                    "pctReduceSep",
                    "eBCGTax",
                    "eBCGDiscRate",
                    "oneCohUtCorpTax",
                    "oneCohUtDiscRate",
                    "multCohUtCorpTax",
                    "multCohUtDiscRate")
  
    store$currSet <- c("agencyFee",
              				"aveAnnCohEarnM1",
              				"aveAnnCohEarnM2",
              				"aveAnnCohEarnM3",
              				"aveAnnCohEarnF1",
              				"aveAnnCohEarnF2",
              				"aveAnnCohEarnF1",
                      "avgHrlyWageBC",
                      "avgHrlyWageBC1",
                      "avgHrlyWageBC2",
                      "avgHrlyWageCL",
                      "avgHrlyWageCL1",
                      "avgHrlyWageCL2",
                      "avgHrlyWageMGMT",
                      "avgHrlyWageMGMT1",
                      "avgHrlyWageMGMT2",
                      "avgHrlyWageSup",
                      "avgMentorHrlyPay",
                      "avgTraineeHrlyPay",
                      "avgTrainerHrlyPay",
              				"bCGSDy1",
              				"bCGSDy2",
              				"bCGTestCostPer1",
              				"bCGTestCostPer1",
                      "brEvSDy",
              				"brEvTrainCostPer1",
              				"brEvTrainCostPer2",
                      "contractedExamCost",
                      "costMedExamSupplies",
                      "costNewEquip",
                      "costReducedOutput",
                      "costSubEmps",
                      "CREPIDAnnSalary",
                      "deptRepPayrate",
              				"eAPCostRepCler",
              				"eAPCostRepMgmt",
              				"eAPCostRepProd",
              				"eAPHospCost",
                      "eBCGSDy",
                      "eBCGTestCostPer1",
                      "eBCGTestCostPer2",
              				"engRevBottom1",
              				"engRevBottom2",
              				"engRevTop1",
              				"engRevTop2",
                      "examinerPayRate",
                      "entInterviewerHrlyPay",
                      "extInterviewerHrlyPay",
                      "hRPayrate",
                      "hRPayrate1",
                      "hRPayrate2",
                      "lumpSum",
              				"meetAVEquip",
              				"meetAvgAttendee",
              				"meetBusSvcs",
              				"meetCoffeeBreaks",
              				"meetDevCostPrev",
              				"meetEquipMaterial",
              				"meetFacLumpsum",
              				"meetOtherCharges",
              				"meetOutsideConsult",
              				"meetReception",
              				"meetRoomRental",
              				"meetSleepRooms",
              				"meetThreeMeals",
              				"meetTraDeptOverhead",
              				"meetTransport",
              				"meetTraStaff",
                      "midpointPay",
                      "movingCost",
              				"multCohUtAltSelProcCost",
              				"multCohUtOrdSelProcCost",
              				"multCohUtSDy",
                      "newHireHrlyPay",
                      "oneCohUtSDy",
              				"oneCohUtTrainCostPer",
                      "orientationHrlyCost",
                      "scoringCosts",
                      "testMaterialCosts",
                      "totCostOfAbs",
                      "totCostOfSep",
                      "traUtSDy",
              				"traUtTrainCostPer",
                      "travelCost",
                      "weightedAveEarnings")

  ###settings###
  
  #settings UI
  output$settingsUI <- renderUI({
    
    print("settingsUI")

    tagList(

      fluidRow(

        column(12,

          textInput("numEmpsInCompany", "Number of employees in organization:", isolate(data$numEmpsInCompany)),

          textInput("numHrsPerWorkWeek", "Number of hours worked per week:", isolate(data$numHrsPerWorkWeek)),

          textInput("numDaysPerWorkYear", "Number of working days per year:", isolate(data$numDaysPerWorkYear)),

          textInput("avgHrlyWageBC", "Average front-line hourly pay:", paste0(currSym[data$currency],isolate(data$avgHrlyWageBC))),

          textInput("avgHrlyWageCL", "Average administrative/support hourly pay:", paste0(currSym[data$currency],isolate(data$avgHrlyWageCL))),

          textInput("avgHrlyWageMGMT", "Average management and professional hourly pay:", paste0(currSym[data$currency],isolate(data$avgHrlyWageMGMT))),
          
          actionButton("updtGlobal", "Update Global Settings")

        )

      )

    )

  })
  
  #copy global settings to all inputs
  observeEvent(input$updtGlobal, {

    print("globUpdt")

    data$numEmpsInCompany <- data$numEmpsInCompany1 <- data$numEmpsInCompany2 <- data$numEmpsInCompany3 <- as.numeric(input$numEmpsInCompany)
    data$numHrsPerWorkWeek <- data$numHrsPerWorkWeek1 <- data$numHrsPerWorkWeek2 <- as.numeric(input$numHrsPerWorkWeek)
    data$numDaysPerWorkYear <- data$numDaysPerWorkYear1 <- data$numDaysPerWorkYear2 <- as.numeric(input$numDaysPerWorkYear)
    data$avgHrlyWageBC <- data$avgHrlyWageBC1 <- data$avgHrlyWageBC2 <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$avgHrlyWageBC)))
    data$avgHrlyWageCL <- data$avgHrlyWageCL1 <- data$avgHrlyWageCL2 <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$avgHrlyWageCL)))
    data$avgHrlyWageMGMT <- data$avgHrlyWageMGMT1 <- data$avgHrlyWageMGMT2 <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$avgHrlyWageMGMT)))

    updtGlob(c("numEmpsInCompany","numHrsPerWorkWeek","numDaysPerWorkYear","avgHrlyWageBC","avgHrlyWageCL","avgHrlyWageMGMT"))

  })
  
  ###absenteeism###
  
  #absenteeism UI
  output$absUI <- renderUI({
    
    print("absUI")
    
    tagList({
      
      fluidRow(
        
        column(12,
            
          fluidRow(
            
            column(12,
              
              "Note: Currency is set to ",data$currency,
  
              br(),
              
              br(),
      
              textInput("empHourLost", "Total employee hours lost to absenteeism for the period:", isolate(data$empHourLost)),
              textInput("numHrsPerWorkWeek1", "Number of hours worked per week:", isolate(data$numHrsPerWorkWeek)),
              
              "Are absent workers paid?",
              
              div(style = "width:60px;margin-top:5px;max-width:200%;",
                
                selectInput("absentWorkersArePaid", label = NULL, c("Yes", "No"), isolate(data$absentWorkersArePaid))
              
              ),
              
              hr()
              
            )
            
          ),
          
          fluidRow(
          
            column(4,
              
              ""
              
            ),
            
            column(4,
              
              "Percent of total absenteeism"
              
            ),
            
            column(4,
              
              "Average hourly wage"
              
            )
            
          ),
          
          fluidRow(
            
            br(),

            column(4,
              
              "Front-line"
              
            ),
            
            column(4,
              
              textInput("pctAbsentBC", label = NULL, paste0(isolate(data$pctAbsentBC),"%"))
              
            ),
            
            column(4,
              
              textInput("avgHrlyWageBC1", label = NULL, paste0(currSym[data$currency],isolate(data$avgHrlyWageBC)))
              
            )
            
          ),
          
          fluidRow(

            column(4,
              
              "Administrative/support"
              
            ),
            
            column(4,
              
              textInput("pctAbsentCL", label = NULL, paste0(isolate(data$pctAbsentCL),"%"))
              
            ),
            
            column(4,
              
              textInput("avgHrlyWageCL1", label = NULL, paste0(currSym[data$currency],isolate(data$avgHrlyWageCL)))
              
            )
            
          ),
          
          fluidRow(

            column(4,
              
              "Management and professional"
              
            ),
            
            column(4,
              
              textInput("pctAbsentMGMT", label = NULL, paste0(isolate(data$pctAbsentMGMT),"%"))
              
            ),
            
            column(4,
              
              textInput("avgHrlyWageMGMT1", label = NULL, paste0(currSym[data$currency],isolate(data$avgHrlyWageMGMT)))
              
            )
          
          ),
        
          fluidRow(
            
            column(12,
              
              hr(),
          
              textInput("pctEmpBenefits", "Additional cost of employee benefits (percentage):", paste0(isolate(data$pctEmpBenefits),"%")),
              textInput("supHrsLostPerDay", "Supervisory hours lost per day (per supervisor):", isolate(data$supHrsLostPerDay)),
              textInput("supsDealWithAbsence", "Number of supervisors who deal with absence problems:", isolate(data$supsDealWithAbsence)),
              textInput("numDaysPerWorkYear1", "Number of working days per year:", isolate(data$numDaysPerWorkYear)),
              textInput("avgHrlyWageSup", "Average hourly supervisory wage (not including benefits):", paste0(currSym[data$currency],isolate(data$avgHrlyWageSup))),
              textInput("costSubEmps", "Cost of substitute employees (total):", paste0(currSym[data$currency],isolate(data$costSubEmps))),
              textInput("costReducedOutput", "Cost of reduced quantity or quality of work outputs (total):", paste0(currSym[data$currency],isolate(data$costReducedOutput))),
              textInput("numEmpsInCompany1", "Number of employees in organization:", isolate(data$numEmpsInCompany))
              
            )
            
          )
          
        )
        
      )
      
    })
    
  })
  
  #absenteeism inputs
  absData <- reactive({
    
    print("absData")
    
    #convert text inputs to numbers and strip symbols
    data$empHourLost <- as.numeric(gsub(",", "", input$empHourLost))
    data$numHrsPerWorkWeek <- data$numHrsPerWorkWeek1 <- data$numHrsPerWorkWeek2 <- as.numeric(input$numHrsPerWorkWeek1)
    data$pctAbsentBC <- as.numeric(gsub("[%]", "", input$pctAbsentBC))
    data$pctAbsentCL <- as.numeric(gsub("[%]", "", input$pctAbsentCL))
    data$pctAbsentMGMT <- as.numeric(gsub("[%]", "", input$pctAbsentMGMT))
    data$avgHrlyWageBC <- data$avgHrlyWageBC1 <- data$avgHrlyWageBC2 <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$avgHrlyWageBC1)))
    data$avgHrlyWageCL <- data$avgHrlyWageCL1 <- data$avgHrlyWageCL2 <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$avgHrlyWageCL1)))
    data$avgHrlyWageMGMT <- data$avgHrlyWageMGMT1 <- data$avgHrlyWageMGMT2 <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$avgHrlyWageMGMT1)))
    data$pctEmpBenefits <- as.numeric(gsub("[%]", "", input$pctEmpBenefits))
    data$supHrsLostPerDay <- as.numeric(input$supHrsLostPerDay)
    data$supsDealWithAbsence <- as.numeric(gsub(",", "", input$supsDealWithAbsence))
    data$numDaysPerWorkYear <- data$numDaysPerWorkYear1 <- data$numDaysPerWorkYear2 <- as.numeric(input$numDaysPerWorkYear1)
    data$avgHrlyWageSup <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$avgHrlyWageSup)))
    data$costSubEmps <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$costSubEmps)))
    data$costReducedOutput <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$costReducedOutput)))
    data$numEmpsInCompany <- data$numEmpsInCompany1 <- data$numEmpsInCompany2 <- data$numEmpsInCompany2 <- as.numeric(gsub(",", "", input$numEmpsInCompany1))

    if (req(input$absentWorkersArePaid) == "No") {
      
      #update settings
      data$absentWorkersArePaid <- "No"
      
     } else {
      
      #update settings
      data$absentWorkersArePaid <- "Yes"
      
    }

    #calculations
    absCalc()
    
    updtGlob(c("numHrsPerWorkWeek1","avgHrlyWageBC1","avgHrlyWageCL1","avgHrlyWageMGMT1","numDaysPerWorkYear1","numEmpsInCompany1"))
        
    #format output
    col1 <- c("1. Total employee-hours lost to absenteeism for the period",
            "2. Average wage/salary per hour per absent employee",
            "3. Cost of employee benefits per hour per absent employee",
            "4. Total compensation lost per hour per absent employee",
            "5. Total compensation lost to absent employees",
            "6. Total supervisory hours lost on employee absenteeism",
            "7. Average hourly supervisory wage, including benefits",
            "8. Total supervisory salaries lost to managing problems of absenteeism",
            "9. Cost of substitute employees",
            "10. Cost of reduced quantity and quality of work",
            "11. Total estimated cost of absenteeism",
            "12. Total estimated cost of absenteeism per employee")
    
    col2 <- c(formatC(store$absCalc$empHourLost, format = "f", digits = 1, big.mark = "," , drop0trailing = TRUE),
            paste0(currSymHTML[data$currency], formatC(store$absCalc$avgPayAbs, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)),
            paste0(currSymHTML[data$currency], formatC(store$absCalc$avgHourlyBenefits, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)),
            paste0(currSymHTML[data$currency], formatC(store$absCalc$totalAvgCompensationPerEmployee, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)),
            paste0(currSymHTML[data$currency], formatC(store$absCalc$totalAvgCompensationAllEmployees, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)),
            formatC(store$absCalc$supTimeSpentPerYear, format = "f", digits = 1, big.mark = "," , drop0trailing = TRUE),
            paste0(currSymHTML[data$currency], formatC(store$absCalc$supCompPerHour, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)),
            paste0(currSymHTML[data$currency], formatC(store$absCalc$supCompPerYear, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)),
            paste0(currSymHTML[data$currency], formatC(store$absCalc$costSubEmps, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)),
            paste0(currSymHTML[data$currency], formatC(store$absCalc$costReducedOutput, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)),
            paste0(currSymHTML[data$currency], formatC(store$absCalc$totalCostOfAbsenteeism, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)),
            paste0(currSymHTML[data$currency], formatC(store$absCalc$totalCostOfAbsenteeismPerEmployee, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)))
    
    #check if all the calculations result in a valid value and render appropriate results column
    if (all(as.logical(lapply(req(store$absCalc), is.finite)))){
      
      return(cbind(col1, col2))
      
    } else {

      return(cbind(col1, rep("-", 12)))
      
    }
    
  })
  
  #absenteeism calculations
  absCalc <- function() {
    
    print("absCalc")
    
    store$absCalc$empHourLost <- data$empHourLost
    store$absCalc$avgPayAbs <- (((data$pctAbsentBC/100) * data$avgHrlyWageBC) + ((data$pctAbsentCL/100) * data$avgHrlyWageCL) + ((data$pctAbsentMGMT/100) * data$avgHrlyWageMGMT))
    avgAnnualSalary <- store$absCalc$avgPayAbs * (52 * data$numHrsPerWorkWeek)
    avgAnnualBenefits <- avgAnnualSalary * (data$pctEmpBenefits/100)
    store$absCalc$avgHourlyBenefits <- avgAnnualBenefits / (52 * data$numHrsPerWorkWeek)
    
    if (req(data$absentWorkersArePaid) == "No") {
      
      store$absCalc$totalAvgCompensationPerEmployee <- store$absCalc$avgHourlyBenefits
      
     } else {
       
      store$absCalc$totalAvgCompensationPerEmployee <- store$absCalc$avgPayAbs + store$absCalc$avgHourlyBenefits
      
    }
    
    store$absCalc$totalAvgCompensationAllEmployees <- store$absCalc$totalAvgCompensationPerEmployee * data$empHourLost 
    store$absCalc$supTimeSpentPerYear <-  data$supHrsLostPerDay * data$supsDealWithAbsence * data$numDaysPerWorkYear
    store$absCalc$supCompPerHour <-  ((data$avgHrlyWageSup * (data$pctEmpBenefits/100)) + data$avgHrlyWageSup)
    store$absCalc$supCompPerYear <-  store$absCalc$supCompPerHour * store$absCalc$supTimeSpentPerYear
    store$absCalc$costSubEmps <- data$costSubEmps
    store$absCalc$costReducedOutput <- data$costReducedOutput
    store$absCalc$totalCostOfAbsenteeism <- store$absCalc$totalAvgCompensationAllEmployees + store$absCalc$supCompPerYear + data$costSubEmps + data$costReducedOutput
    store$absCalc$totalCostOfAbsenteeismPerEmployee <- store$absCalc$totalCostOfAbsenteeism / data$numEmpsInCompany
    
  }
  
  #absenteeism table
  output$absTab <- renderTable({
    
    req(absData())
    
  }, align = 'lr', sanitize.text.function = function(x) x)
  
  #absenteeism pie chart
  output$absPie <- renderPlot({
    
    req(all(as.logical(lapply(req(store$absCalc), is.finite))) & any(req(store$absCalc) > 0))

    #assign values for "slices" of the pie
    slices <- c(store$absCalc$totalAvgCompensationAllEmployees,
              store$absCalc$supCompPerYear,
              store$absCalc$costSubEmps,
              store$absCalc$costReducedOutput)

    #name slices
    names(slices) <- c("Employee Compensation", "Supervisor Salary", "Substitute Employees", "Reduced Productivity")
    
    slices <- sort(slices, decreasing = TRUE) 
    
    pct <- lapply(slices, function (x) round(x/sum(slices)*100))
     
    pctlbls <- paste(pct,"%",sep="") #add % to labels

    par(mar = c(1,1,1,1))
    
    pie(
      slices, 
      edges = 400, 
      radius = 0.8,
      clockwise = TRUE,
      angle = 45,
      col = rainbow(length(slices)),
      labels = head(pctlbls, ((length(pct))-(sum(pct<2)))),
      cex = 0.8
    )
    
    legend(
      x = -2.0,
      y = 0.7,
      inset = .05,
      title = "Costs", 
      legend = names(slices),
      fill = rainbow(length(slices)),
      horiz = FALSE,
      cex = 0.8,
      text.width = 0.8
    )

  })
  
  ###separation###
  
  #separation tab#
  
  #separation UI
  output$sepUI <- renderUI({
    
    print("sepUI")
    
    tagList(

      fluidRow(

        column(12,
          
          "Note: Currency is set to ",data$currency,
          
          br(),

          br(),

          tags$h4("1. Exit Interview"),

          br(),

          textInput("timePriorExitInterview", "Time required prior to interview (minutes):", isolate(data$timePriorExitInterview)),
          textInput("exitInterviewTime", "Time required for interview (minutes):", isolate(data$exitInterviewTime)),
          textInput("extInterviewerHrlyPay", "Interviewer's pay rate (hourly):", paste0(currSym[data$currency],isolate(data$extInterviewerHrlyPay))),
          textInput("numTurnover", "Number of turnovers during period:", isolate(data$numTurnover))

        )

      ),

      fluidRow(
        
        hr(),
      
        column(4,
          
          ""
          
        ),
        
        column(4,
          
          "Percent of total separations"
          
        ),
        
        column(4,
          
          "Average hourly wage"
          
        )
        
      ),
      
      fluidRow(
        
        br(),

        column(4,
          
          "Front-line"
          
        ),
        
        column(4,
          
          textInput("pctTurnoverBC", label = NULL, paste0(isolate(data$pctTurnoverBC),"%"))
          
        ),
        
        column(4,
          
          textInput("avgHrlyWageBC2", label = NULL, paste0(currSym[data$currency],isolate(data$avgHrlyWageBC)))
          
        )
        
      ),
      
      fluidRow(

        column(4,
          
          "Administrative/support"
          
        ),
        
        column(4,
          
          textInput("pctTurnoverCL", label = NULL, paste0(isolate(data$pctTurnoverCL),"%"))
          
        ),
        
        column(4,
          
          textInput("avgHrlyWageCL2", label = NULL, paste0(currSym[data$currency],isolate(data$avgHrlyWageCL)))
          
        )
        
      ),
      
      fluidRow(

        column(4,
          
          "Management and professional"
          
        ),
        
        column(4,
          
          textInput("pctTurnoverMGMT", label = NULL, paste0(isolate(data$pctTurnoverMGMT),"%"))
          
        ),
        
        column(4,
          
          textInput("avgHrlyWageMGMT2", label = NULL, paste0(currSym[data$currency],isolate(data$avgHrlyWageMGMT)))
          
        )
      
      ),

      fluidRow(
        
        hr(),

        column(12,

          tags$h4("2. Separation pay"),

          br(),

          textInput("numHrsPerWorkWeek2", "Number of hours worked per week:", isolate(data$numHrsPerWorkWeek)),
          textInput("sevPayWeeks", "Number of weeks of severance pay:", isolate(data$sevPayWeeks))
          
        )
        
      ),
      
      fluidRow(

        hr(),
      
        column(12,

            tags$h4("3. Administrative Functions"),
  
            br(),
          
            div(style="margin-bottom:5px;","Time required by HR department for administrative functions related to termination (minutes):"),
            textInput("sepAdminHRTime", label = NULL, isolate(data$sepAdminHRTime)),
            div(style="margin-bottom:5px;","Average HR department employee pay rate (hourly):"),
            textInput("hRPayrate1", label = NULL, paste0(currSym[data$currency],isolate(data$hRPayrate)))

        )
        
      ),
      
      fluidRow(

        hr(),
      
        column(12,

          tags$h4("4. Unemployment Tax"),

          br(),

          "My organization is subject to unemployment tax:",

          div(style = "width:60px;margin-top:5px;",
            
            selectInput("orgSubjectToUnempTax", label = NULL, c("Yes", "No"), isolate(data$orgSubjectToUnempTax))
          
          )

        )

      )
      
    )

  })
  
  #separation unemployment tax UI
  output$unempTaxInputs <- renderUI({
    
    print("sepUnempTaxUI")
    
    #check if organization is subject to unemployment tax
    if (req(input$orgSubjectToUnempTax) == "Yes") {
    
      #inputs if "Yes" is selected
      tagList(
      
        textInput("numEmpsInCompany2", "Number of employees in organization:", isolate(data$numEmpsInCompany2)),
        textInput("unempTaxRate", "Unemployment tax rate (percentage):", paste0(isolate(data$unempTaxRate), "%")),
        textInput("baseTaxRate", "Base rate (percentage):", paste0(isolate(data$baseTaxRate), "%")),
        div(style="margin-bottom:5px;",paste0("Number of employees earning more than $7,000 USD",if (!is.null(store$xRate) && (!data$currency=="US Dollars")) {paste0(" (approx. ",currSym[data$currency],formatC(store$xRate*7000,format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE),")")}," annually:")),
        textInput("numEmpOver7k", label = NULL, isolate(data$numEmpOver7k)),
        div(style="margin-bottom:5px;","Weighted average earnings if less than $7,000 USD",if (!is.null(store$xRate) && (!data$currency=="US Dollars")) {paste0(" (approx. ",currSym[data$currency],formatC(store$xRate*7000,format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE),")")}," annually:"),
        textInput("weightedAveEarnings", label = NULL, paste0(currSym[data$currency],isolate(data$weightedAveEarnings)))
        
      )
      
    } else {
      
      #no inputs if "No" is selected
      return(NULL)
      
    }
    
  })

  #separation inputs
  sepData <- reactive({
    
    print("sepData")
    
    #convert text inputs to numbers and strip symbols
    #exit interview inputs
    data$timePriorExitInterview <- as.numeric(input$timePriorExitInterview)
    data$exitInterviewTime <- as.numeric(input$exitInterviewTime)
    data$extInterviewerHrlyPay <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$extInterviewerHrlyPay)))
    data$numTurnover <- as.numeric(gsub(",", "", input$numTurnover))
    data$pctTurnoverBC <- as.numeric(gsub("[%]", "", input$pctTurnoverBC))
    data$pctTurnoverCL <- as.numeric(gsub("[%]", "", input$pctTurnoverCL))
    data$pctTurnoverMGMT <- as.numeric(gsub("[%]", "", input$pctTurnoverMGMT))
    data$avgHrlyWageBC <- data$avgHrlyWageBC1 <- data$avgHrlyWageBC2 <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$avgHrlyWageBC2)))
    data$avgHrlyWageCL <- data$avgHrlyWageCL1 <- data$avgHrlyWageCL2 <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$avgHrlyWageCL2)))
    data$avgHrlyWageMGMT <- data$avgHrlyWageMGMT1 <- data$avgHrlyWageMGMT2 <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$avgHrlyWageMGMT2)))
    
    #separation pay inputs
    data$numHrsPerWorkWeek <- data$numHrsPerWorkWeek1 <- data$numHrsPerWorkWeek2 <- as.numeric(input$numHrsPerWorkWeek2)
    data$sevPayWeeks <- as.numeric(input$sevPayWeeks)
    
    #administrative functions inputs
    data$sepAdminHRTime <- as.numeric(input$sepAdminHRTime)
    data$hRPayrate <- data$hRPayrate1 <- data$hRPayrate2 <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$hRPayrate1)))
    
    #check if organization is subject to unemployment tax
    if (req(input$orgSubjectToUnempTax) == "Yes") { 
      
      #update data
      data$orgSubjectToUnempTax <- "Yes"
      
      #load values from these inputs if "Yes" is selected
      data$numEmpsInCompany <- data$numEmpsInCompany1 <- data$numEmpsInCompany2 <- data$numEmpsInCompany3 <- as.numeric(gsub(",", "", input$numEmpsInCompany2))
      data$unempTaxRate <- as.numeric(gsub("[%]", "", input$unempTaxRate))
      data$baseTaxRate <- as.numeric(gsub("[%]", "", input$baseTaxRate))
      data$numEmpOver7k <- as.numeric(gsub(",", "", input$numEmpOver7k))
      data$weightedAveEarnings <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$weightedAveEarnings)))
      
    } else {
      
      #update data
      data$orgSubjectToUnempTax <- "No"
      
      #no inputs necessary if "No" is selected
      
    }
    
    #calculations
    sepCalc()
		
    updtGlob(c("avgHrlyWageBC2","avgHrlyWageCL2","avgHrlyWageMGMT2","numHrsPerWorkWeek2","hRPayrate1","numEmpsInCompany2"))
    
		#output a results table
	  col1 <- c("1. Cost of exit interview",
	          "2. Cost of separation pay",
            "3. Cost of administrative functions",
            "4. Cost of unemployment tax",
	          "Total")
  
    col2 <- c(paste0(currSymHTML[data$currency], formatC(store$sepCalc$totExitInterview, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)),
            paste0(currSymHTML[data$currency], formatC(store$sepCalc$severance, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)),
            paste0(currSymHTML[data$currency], formatC(store$sepCalc$adminFunc, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)),
            paste0(currSymHTML[data$currency], formatC(store$sepCalc$unempTax, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)),
            paste0(currSymHTML[data$currency], formatC(sum(as.numeric(store$sepCalc)), format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)))
    
    #check if all the calculations result in a valid value and render appropriate results column
    if (all(as.logical(lapply(req(store$sepCalc), is.finite)))){
      
      return(cbind(col1, col2))
      
    } else {
      
      return(cbind(col1, rep("-", 5)))
      
    }
      
  })
    
  #separation calculations
  sepCalc <- function() {
    
    print("sepCalc")
    
    #calculate exit interview costs
    costExitInterviewer <- ((data$timePriorExitInterview / 60) + (data$exitInterviewTime / 60)) * data$extInterviewerHrlyPay * data$numTurnover
    avgPayExitInt <- (((data$pctTurnoverBC/100) * data$avgHrlyWageBC) + ((data$pctTurnoverCL/100) * data$avgHrlyWageCL) + ((data$pctTurnoverMGMT/100) * data$avgHrlyWageMGMT))
    costExitEmpMgmt <- (data$exitInterviewTime / 60) * avgPayExitInt * data$numTurnover
    store$sepCalc$totExitInterview <- costExitInterviewer + costExitEmpMgmt
    
    #calculate severance pay
		store$sepCalc$severance <- (data$numHrsPerWorkWeek * data$sevPayWeeks * avgPayExitInt) * data$numTurnover
		
		#calculate costs of administrative functions
		store$sepCalc$adminFunc <- (data$sepAdminHRTime / 60) * data$hRPayrate * data$numTurnover
    
    #calculate unemployment tax
		if (req(input$orgSubjectToUnempTax) == "Yes") {
		  
		  #check if weighted average earnings is greater than 7000
		  if (as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", req(isolate(input$weightedAveEarnings))))) > 7000) {
      
        #if yes, set weightAve equal to weightedAveEarnings
        weightAve <- data$weightedAveEarnings

      } else {
        
        #if no, set weightAve equal to
        weightAve <- 7000
        
      }
      
      #conduct unemployment tax calculations if "Yes" is selected
  		store$sepCalc$unempTax <- ((data$unempTaxRate - data$baseTaxRate) / 100) * ((7000 * (data$numEmpsInCompany + data$numTurnover)) + (data$weightedAveEarnings * data$numEmpOver7k)) + ((data$unempTaxRate / 100) * (weightAve * data$numTurnover)) - ((data$unempTaxRate - data$baseTaxRate) * (data$weightedAveEarnings * data$numTurnover))
      
    } else {
      
      #if "No" is selected
      store$sepCalc$unempTax <- 0
      
    }
    
  }
  
  #separation table
  output$sepTab <- renderTable({
    
    req(sepData())
    
  }, align = 'lr', sanitize.text.function = function(x) x)
  
  #separation pie chart
  output$sepPie <- renderPlot({
    
    req(all(as.logical(lapply(req(store$sepCalc), is.finite))) & any(req(store$sepCalc) > 0))

    #assign values for "slices" of the pie
    slices <- c(store$sepCalc$severance,store$sepCalc$adminFunc,store$sepCalc$totExitInterview,store$sepCalc$unempTax)
        
    #name slices
    names(slices) <- c("Separation Pay", "Admin Functions", "Exit Interview", "Unemployment Tax")
    
    slices <- sort(slices, decreasing = TRUE) 
    
    pct <- lapply(slices, function (x) round(x/sum(slices)*100))
     
    pctlbls <- paste(pct,"%",sep="") #add % to labels

    par(mar = c(1,1,1,1))
    
    pie(
      slices, 
      edges = 400, 
      radius = 0.8,
      clockwise = TRUE,
      angle = 45,
      col = rainbow(length(slices)),
      labels = head(pctlbls, ((length(pct))-(sum(pct<2)))),
      cex = 0.8
    )
    
    legend(
      x = -2.0,
      y = 0.7,
      inset = .05,
      title = "Costs", 
      legend = names(slices),
      fill = rainbow(length(slices)),
      horiz = FALSE,
      cex = 0.8,
      text.width = 0.8
    )

  })
  
  #replacement#
  
  #replacement UI
  output$repUI <- renderUI({
    
    print("repUI")
    
    tagList({
      
      fluidRow(
        
        column(12,
      
          fluidRow(
            
            column(12,
              
              "Note: Currency is set to ",data$currency,
              
              br(),
            
              br(),
                
              tags$h4("1. Communicate Job Availability"),
              
              br(),
              
              div(style="margin-bottom:5px;","Advertising and employment agency fees per termination:"),
              textInput("agencyFee", label = NULL, paste0(currSym[data$currency],isolate(data$agencyFee))),
              textInput("timeComm", "Time to communicate job availability (hours):", isolate(data$timeComm))
              
            )
            
          ),
          
          fluidRow(
            
            column(12,
              
              hr(),
             
              tags$h4("2. Pre-Employment Admin Functions"),
              
              br(),
              
              div(style="margin-bottom:5px;","Administrative time required per position to be filled (hours):"),        
              textInput("adminPreEmpHRTime", label = NULL, isolate(data$adminPreEmpHRTime)),
              textInput("numPositions", "Number of positions to be filled:", isolate(data$numPositions))
              
            )
            
          ),
          
          fluidRow(
            
            column(12,
              
              hr(),                
              
              tags$h4("3. Entrance Interview"),
              
              br(),
                       
              textInput("entranceInterviewTime", "Time required for interview (minutes):", isolate(data$entranceInterviewTime)),
              textInput("entInterviewerHrlyPay", "Interviewer's pay rate (hourly):", paste0(currSym[data$currency],isolate(data$entInterviewerHrlyPay))),
              textInput("numEntranceInterviews", "Number of interviews during period:", isolate(data$numEntranceInterviews))
              
            )
            
          ),
          
          fluidRow(
            
            column(12,  
              
              hr(),                
            
              tags$h4("4. Testing"),
              
              br(),
                       
              textInput("testMaterialCosts", "Cost of materials per person:", paste0(currSym[data$currency],isolate(data$testMaterialCosts))),
              textInput("scoringCosts", "Cost of scoring per person:", paste0(currSym[data$currency],isolate(data$scoringCosts))),
              textInput("numberOfTests", "Number of candidates tested:", isolate(data$numberOfTests))
              
            )
            
          ),
          
          fluidRow(
            
            column(12,
              
              hr(),                
            
              tags$h4("5. Staffing Meeting"),
              
              br(),
              
              textInput("staffingMeetingTime", "Time required for meeting (minutes):", isolate(data$staffingMeetingTime)),
              div(style="margin-bottom:5px;","Department representative average pay rate (hourly):"),
              textInput("deptRepPayrate", label = NULL, paste0(currSym[data$currency],isolate(data$deptRepPayrate))),
              textInput("numStaffingMeeting", "Number of meetings during period:", isolate(data$numStaffingMeeting))
              
            )
            
          ),
          
          fluidRow(
            
            column(12,
              
              hr(),                
             
              tags$h4("6. Travel/Moving Expenses"),
              
              br(),
                      
              textInput("travelCost", "Average travel costs per applicant:", paste0(currSym[data$currency],isolate(data$travelCost))),
              textInput("movingCost", "Average moving cost per new hire:", paste0(currSym[data$currency],isolate(data$movingCost))),
              textInput("numApplicants", "Number of applicants during period:", isolate(data$numApplicants)),
              textInput("numNewHires", "Number of new hires during period:", isolate(data$numNewHires))
              
            )
            
          ),
          
          fluidRow(
            
            column(12,
              
              hr(),                
              
              tags$h4("7. Post-Employment"),
              
              br(),
              
              div(style="margin-bottom:5px;","Time required for acquiring and disseminating information (minutes):"),
                       
              textInput("timeTransferInfo", label = NULL, isolate(data$timeTransferInfo))
              
            )
            
          ),
          
          fluidRow(
            
            column(12,
              
              hr(),                
            
              tags$h4("8. Medical Exams"),
              
              br(),
              
              "Examination type:",
              
              div(style = "width:100px;margin-top:5px;",
                
                selectInput("medExamType", label = NULL, c("In-house", "Contracted"), isolate(data$medExamType))
              
              )
          
            )
        
          )
          
        )
      
      )
    
    })
    
  })
  
  #replacement medical exam UI
  output$medExamInputs <- renderUI({
    
    print("repMedExamUI")
    
    #check if exams are conducted in-house or contracted
    if (req(input$medExamType) == "In-house") {
      
      #inputs if "In-house" is selected
      tagList(
        
        div(style="margin-bottom:5px;","Time required for in-house examination (minutes):"),
        textInput("timeMedExam", label = NULL, isolate(data$timeMedExam)),
        textInput("examinerPayRate", "In-house examiner's payrate (hourly):", paste0(currSym[data$currency],isolate(data$examinerPayRate))),
        textInput("costMedExamSupplies", "Cost of supplies used for in-house exam:", paste0(currSym[data$currency],isolate(data$costMedExamSupplies)))
        
      )
      
    } else {
      
      #inputs if "Contracted" is selected
      tagList(
      
        textInput("contractedExamCost", "Contracted rate per examination:", paste0(currSym[data$currency],isolate(data$contractedExamCost)))
        
      )
      
    }
    
  })
  
  #replacement inputs
  repData <- reactive({
    
    print("repData")
    
    #convert text inputs to numbers and strip symbols
    #job availability inputs
    data$agencyFee <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$agencyFee)))
    data$timeComm <- as.numeric(input$timeComm)
    
    #pre-employment inputs
    data$adminPreEmpHRTime <- as.numeric(input$adminPreEmpHRTime)
    data$numPositions <- as.numeric(gsub(",", "", input$numPositions))
    
    #entrance interview inputs
    data$entranceInterviewTime <- as.numeric(input$entranceInterviewTime)
    data$entInterviewerHrlyPay <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$entInterviewerHrlyPay)))
    data$numEntranceInterviews <- as.numeric(gsub(",", "", input$numEntranceInterviews))
    
    #testing inputs
    data$testMaterialCosts <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$testMaterialCosts)))
    data$scoringCosts <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$scoringCosts)))
    data$numberOfTests <- as.numeric(gsub(",", "", input$numberOfTests))
    
    #staff meeting inputs
    data$staffingMeetingTime <- as.numeric(input$staffingMeetingTime)
    data$deptRepPayrate <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$deptRepPayrate)))
    data$numStaffingMeeting <- as.numeric(gsub(",", "", input$numStaffingMeeting))

    #travel/moving inputs
    data$travelCost <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$travelCost)))
    data$movingCost <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$movingCost)))
    data$numApplicants <- as.numeric(gsub(",", "", input$numApplicants))
    data$numNewHires <- as.numeric(gsub(",", "", input$numNewHires))
    
    #post-employment inputs
    data$timeTransferInfo <- as.numeric(input$timeTransferInfo)
    
    #medical exam inputs
    #check if medical exams are conducted "In-house" or "Contracted"
    if (req(input$medExamType) == "In-house") {
      
      #update data
      data$medExamType <- "In-house"
      
      #load values from these inputs if "In-house" is selected
      data$timeMedExam <- as.numeric(input$timeMedExam)
      data$examinerPayRate <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$examinerPayRate)))
      data$costMedExamSupplies <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$costMedExamSupplies)))
      
    } else {
      
      #update data
      data$medExamType <- "Contracted"
      
      #if "Contracted" is selected
      data$contractedExamCost <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$contractedExamCost)))
      
    }
    
    #calculations
    repCalc()
    
    #format output
    col1 <- c("1. Cost of communicating job availability",
            "2. Cost of pre-employment administrative functions",
            "3. Cost of entrance interview",
            "4. Cost of testing",
            "5. Cost of staff meetings",
            "6. Cost of travel and moving",
            "7. Post-employment costs",
            "8. Cost of medical exam",
            "Total")
    
    col2 <- c(paste0(currSymHTML[data$currency], formatC(store$repCalc$commJobAvailability, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)),
            paste0(currSymHTML[data$currency], formatC(store$repCalc$preempAdminCost, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)),
            paste0(currSymHTML[data$currency], formatC(store$repCalc$entInterviewCost, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)),
            paste0(currSymHTML[data$currency], formatC(store$repCalc$testingCost, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)),
            paste0(currSymHTML[data$currency], formatC(store$repCalc$staffMeetingCost, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)),
            paste0(currSymHTML[data$currency], formatC(store$repCalc$travelExpenses, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)),
            paste0(currSymHTML[data$currency], formatC(store$repCalc$postEmpCosts, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)),
            paste0(currSymHTML[data$currency], formatC(store$repCalc$totMedExamCost, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)),
            paste0(currSymHTML[data$currency], formatC(sum(as.numeric(store$repCalc)), format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)))
    
    #check if all the calculations result in a valid value and render appropriate results column
    if (all(as.logical(lapply(req(store$repCalc), is.finite)))){
      
      return(cbind(col1, col2))
      
    } else {
      
      return(cbind(col1, rep("-", 9)))
      
    }
    
  })
  
  #replacement calculations
  repCalc <- function() {
    
    print("repCalc")
    
    #calculate cost of communicating job availability
    store$repCalc$commJobAvailability <- (data$agencyFee + (data$timeComm * data$hRPayrate)) * data$numTurnover
    
    #calculate cost of pre-employment admin functions
    store$repCalc$preempAdminCost <- (data$adminPreEmpHRTime) * data$hRPayrate * data$numPositions
    
    #calculate cost of entrance interview
    store$repCalc$entInterviewCost <- (data$entranceInterviewTime / 60) * data$entInterviewerHrlyPay * data$numEntranceInterviews
    
    #calculate cost of testing
    store$repCalc$testingCost <- (data$testMaterialCosts + data$scoringCosts) * data$numberOfTests
    
    #calculate cost of staff meetings
    store$repCalc$staffMeetingCost <- (data$staffingMeetingTime / 60) * (data$hRPayrate + data$deptRepPayrate) * data$numStaffingMeeting
    
    #calculate cost of travel/moving
    store$repCalc$travelExpenses <- (data$travelCost * (data$numApplicants)) + (data$movingCost * (data$numNewHires / 10))
    
    #calculate post-employment costs
    store$repCalc$postEmpCosts <- (data$timeTransferInfo / 60) * data$hRPayrate * data$numTurnover
    
    #calculate cost of medical exam
    #check if medical exams are conducted "In-house" or "Contracted"
    if (req(input$medExamType) == "In-house") {
      
      #if "In-house" is selected
      store$repCalc$totMedExamCost <- (((data$timeMedExam / 60) * data$examinerPayRate) + data$costMedExamSupplies) * data$numTurnover
      
    } else {
      
      #if "Contracted" is selected
      store$repCalc$totMedExamCost <- (data$contractedExamCost * data$numTurnover)
      
    }    
    
  }
  
  #replacement table
  output$repTab <- renderTable({
    
    req(repData())
    
  }, align = 'lr', sanitize.text.function = function(x) x)
  
  #replacement pie chart
  output$repPie <- renderPlot({
    
    req(all(as.logical(lapply(req(store$repCalc), is.finite))) & any(req(store$repCalc) > 0))
    
    #assign values for "slices" of the pie
    slices <- c(store$repCalc$commJobAvailability, 
              store$repCalc$preempAdminCost, 
              store$repCalc$entInterviewCost, 
              store$repCalc$testingCost, 
              store$repCalc$staffMeetingCost, 
              store$repCalc$travelExpenses, 
              store$repCalc$postEmpCosts, 
              store$repCalc$totMedExamCost)
    
    #name slices
    names(slices) <- c("Communicating Availability",
                    "Pre-Employment Admin", 
                    "Entrance Interview", 
                    "Testing", 
                    "Staff Meetings", 
                    "Travel & Moving", 
                    "Post-Employment Costs", 
                    "Medical Exam")
    
    slices <- sort(slices, decreasing = TRUE) 
    
    pct <- lapply(slices, function (x) round(x/sum(slices)*100))
     
    pctlbls <- paste(pct,"%",sep="") #add % to labels

    par(mar = c(1,1,1,1))
    
    pie(
      slices, 
      edges = 400, 
      radius = 0.8,
      clockwise = TRUE,
      angle = 45,
      col = rainbow(length(slices)),
      labels = head(pctlbls, ((length(pct))-(sum(pct<2)))),
      cex = 0.8
    )
    
    legend(
      x = -2.0,
      y = 0.7,
      inset = .05,
      title = "Costs", 
      legend = names(slices),
      fill = rainbow(length(slices)),
      horiz = FALSE,
      cex = 0.8,
      text.width = 0.8
    )
    
  })
  
  #training#
  
  #training UI
  output$traUI <- renderUI({
    
    print("traUI")
    
    tagList({
               
      fluidRow(
        
        column(12,
          
          fluidRow(
            
            column(12,
              
              "Note: Currency is set to ",data$currency,
              
              br(),
          
              br(),
            
              tags$h4("1. Formal Training Program"),
                
              br(),
              
              textInput("trainingLength", "Length of training program (hours):", isolate(data$trainingLength)),
              textInput("avgTrainerHrlyPay", "Average hourly rate of trainer or trainers:", paste0(currSym[data$currency],isolate(data$avgTrainerHrlyPay))),
              textInput("numPrograms", "Number of programs conducted:", isolate(data$numPrograms)),
              div(style="margin-bottom:5px;","Percentage of training costs attributed to replacements:"),
              textInput("pctTrainingCostRep", label = NULL, paste0(isolate(data$pctTrainingCostRep),"%")),
              textInput("avgTraineeHrlyPay", "Average hourly pay per trainee:", paste0(currSym[data$currency],isolate(data$avgTraineeHrlyPay))),
              div(style="margin-bottom:5px;","Total number of replacements trained during period:"),
              textInput("numTrainees", label = NULL, isolate(data$numTrainees))
              
            )
            
          ),
              
          fluidRow(
            
            column(12,
          
              hr(),                
             
              tags$h4("2. On-the-Job Training (OJT)"),
              
              br(),
              
              textInput("numOTJTrainingHrs", "Number of hours required for instruction:", isolate(data$numOTJTrainingHrs)),
              div(style="margin-bottom:5px;","Average hourly rate of experienced employee (mentor):"),
              textInput("avgMentorHrlyPay", label = NULL, paste0(currSym[data$currency],isolate(data$avgMentorHrlyPay))),
              div(style="margin-bottom:5px;","Proportional reduction in productivity due to training:"),
              textInput("propRedTrainingProd", label = NULL, paste0(isolate(data$propRedTrainingProd),"%")),
              div(style="margin-bottom:5px;","Number of new employees assigned to each experienced employee for OJT :"),
              textInput("numMenteesPerMentor", label = NULL, isolate(data$numMenteesPerMentor)),
              textInput("newHireHrlyPay", "Average hourly rate per new employee:", paste0(currSym[data$currency],isolate(data$newHireHrlyPay))),
              textInput("numOTJTrainings", "Total number of instructions during period:", isolate(data$numOTJTrainings))
              
            )
            
          ),
          
          fluidRow(
            
            column(12,
          
              hr(),                
              
              tags$h4("3. New Equipment"),
              
              br(),
              
              textInput("costNewEquip", "Cost of new equipment:", paste0(currSym[data$currency],isolate(data$costNewEquip)))
              
            )
            
          ),
          
          fluidRow(
            
            column(12,
          
              hr(),                
            
              tags$h4("4. Orientation Costs"),
              
              br(),
                       
              textInput("orientationLength", "Length of orientation (hours):", isolate(data$orientationLength)),
              textInput("orientationHrlyCost", "Average hourly cost to deliver:", paste0(currSym[data$currency],isolate(data$orientationHrlyCost)))
              
            )
            
          )
          
        )
        
      )
      
    })
      
  })
  
  #training inputs
  traData <- reactive({
    
    print("traData")
    
    #convert text inputs to numbers and strip symbols
    #formal training program inputs
    data$trainingLength <- as.numeric(input$trainingLength)
    data$avgTrainerHrlyPay <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$avgTrainerHrlyPay)))
    data$numPrograms <- as.numeric(gsub(",", "", input$numPrograms))
    data$pctTrainingCostRep <- as.numeric(gsub("[%]", "", input$pctTrainingCostRep))
    data$avgTraineeHrlyPay <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$avgTraineeHrlyPay)))
    data$numTrainees <- as.numeric(gsub(",", "", input$numTrainees))
    
    #on-the-job training inputs
    data$numOTJTrainingHrs <- as.numeric(gsub(",", "", input$numOTJTrainingHrs))
    data$avgMentorHrlyPay <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$avgMentorHrlyPay)))
    data$propRedTrainingProd <- as.numeric(gsub("[%]", "", input$propRedTrainingProd))
    data$numMenteesPerMentor <- as.numeric(input$numMenteesPerMentor)
    data$newHireHrlyPay <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$newHireHrlyPay)))
    data$numOTJTrainings <- as.numeric(gsub(",", "", input$numOTJTrainings))
    
    #new equipment inputs
    data$costNewEquip <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$costNewEquip)))
    
    #orientation inputs
    data$orientationLength <- as.numeric(input$orientationLength)
    data$orientationHrlyCost <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$orientationHrlyCost)))
    
    #calculations
    traCalc()
    
    #output a results table
    col1 <- c("1. Cost of formal training",
            "2. Cost of on-the-job training",
            "3. Cost of training literature",
            "4. Cost of orientation",
            "Total")
    
    col2 <- c(paste0(currSymHTML[data$currency], formatC(store$traCalc$formalTrainingCosts, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)),
            paste0(currSymHTML[data$currency], formatC(store$traCalc$costOTJTraining, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)),
            paste0(currSymHTML[data$currency], formatC(store$traCalc$trainingNewEquip, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)),
            paste0(currSymHTML[data$currency], formatC(store$traCalc$orientationCost, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)),
            paste0(currSymHTML[data$currency], formatC(sum(as.numeric(store$traCalc)), format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)))

    #check if all the calculations result in a valid value and render appropriate results column
    if (all(as.logical(lapply(req(store$traCalc), is.finite)))){
      
      return(cbind(col1, col2))
  
    } else {
  
      return(cbind(col1, rep("-", 5)))
      
    }
    
  })
  
  #training calculations
  traCalc <- function() {
    
    print("traCalc")
    
    #calculate cost of formal training program
    store$traCalc$formalTrainingCosts <- (data$trainingLength * data$avgTrainerHrlyPay * data$numPrograms * (data$pctTrainingCostRep / 100)) + (data$avgTraineeHrlyPay * data$numTrainees * data$trainingLength)
    
    #calculate cost of on-the-job training
    store$traCalc$costOTJTraining <- (data$numOTJTrainingHrs) * ((data$avgMentorHrlyPay * (data$propRedTrainingProd / 100) * (data$numOTJTrainings / data$numMenteesPerMentor)) + (data$newHireHrlyPay * data$numOTJTrainings))
    
    #calculate cost of new equipment
    store$traCalc$trainingNewEquip <- data$costNewEquip
    
    #calculate cost of orientation
    store$traCalc$orientationCost <- (data$orientationHrlyCost * data$orientationLength)
    
  }
  
  #training table
  output$traTab <- renderTable({
    
    req(traData())
    
  }, align = 'lr', sanitize.text.function = function(x) x)
  
  #training pie chart
  output$traPie <- renderPlot({
    
    req(all(as.logical(lapply(req(store$traCalc), is.finite))) & any(req(store$traCalc) > 0))
    
    #assign values for "slices" of the pie
    slices <- c(store$traCalc$formalTrainingCosts, 
              store$traCalc$costOTJTraining, 
              store$traCalc$trainingNewEquip, 
              store$traCalc$orientationCost)
    
    #name slices
    names(slices) <- c("Formal Training", "On-The-Job Training", "New Equipment", "Orientation")
    
    slices <- sort(slices, decreasing = TRUE) 
    
    pct <- lapply(slices, function (x) round(x/sum(slices)*100))
     
    pctlbls <- paste(pct,"%",sep="") #add % to labels

    par(mar = c(1,1,1,1))
    
    pie(
      slices, 
      edges = 400, 
      radius = 0.8,
      clockwise = TRUE,
      angle = 45,
      col = rainbow(length(slices)),
      labels = head(pctlbls, ((length(pct))-(sum(pct<2)))),
      cex = 0.8
    )
    
    legend(
      x = -2.0,
      y = 0.7,
      inset = .05,
      title = "Costs", 
      legend = names(slices),
      fill = rainbow(length(slices)),
      horiz = FALSE,
      cex = 0.8,
      text.width = 0.8
    )
    
  })
  
  #performance#
  
  #performance UI
  output$perfUI <- renderUI({
    
    print("perfUI")
    
    tagList({
              
      fluidRow(
        
        column(12,
          
          "Note: Currency is set to ",data$currency,
          
          br(),
        
          br(),
          
          tags$h4("1. Difference in Performance"),
          
          br(),
          
          "Difference will be :",
          
          div(style = "width:170px;margin-top:5px;",
            
            selectInput("diffPerfMethod", label = NULL, c("Calculated", "Entered as a lump sum"), isolate(data$diffPerfMethod))
          
          )
          
        )
        
      )               
      
    })
    
  })
  
  #performance method UI  
  output$diffPerfInputs <- renderUI({
    
    print("diffPerfUI")
    
    #check if difference in performance is calculated or entered as a lump sum
    if (req(input$diffPerfMethod) == "Calculated") {
      
      #update settings
      data$diffPerfMethod <- "Calculated"
    
      #inputs if "Calculated" is selected  
      tagList(
      
        div(style="margin-bottom:5px;","Compa-ratio of the leaving employee or group (between 0.8 and 1.2):"),
        textInput("compaRatioLeave", label = NULL, isolate(data$compaRatioLeave)),
        div(style="margin-bottom:5px;","Compa-ratio of the replacement employee or group (between 0.8 and 1.2):"),
        textInput("compaRatioReplace", label = NULL, isolate(data$compaRatioReplace)),
        div(style="margin-bottom:5px;","Annual rate of pay at the midpoint of the pay grade:"),
        textInput("midpointPay", label = NULL, paste0(currSym[data$currency],isolate(data$midpointPay)))
        
      )
      
    } else {
      
      #update settings
      data$diffPerfMethod <- "Entered as a lump sum"
      
      #inputs if "Entered as a lump sum" is selected
      tagList(
      
        textInput("lumpSum", "Lump sum amount:", paste0(currSym[data$currency],isolate(data$lumpSum)))
        
      )
      
    }
    
  })

  #performance inputs
  perfData <- reactive({
    
    print("perfData")
    
    #convert text inputs to numbers and strip symbols
    #performance cost inputs
    #check if performance costs are "Calculated" or "Entered as a lump sum"
    if (req(input$diffPerfMethod) == "Calculated") {
      
      #load values from these inputs if "Calculated" is selected
      data$compaRatioLeave <- as.numeric(input$compaRatioLeave)
      data$compaRatioReplace <- as.numeric(input$compaRatioReplace)
      data$midpointPay <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$midpointPay)))
      
    } else {
      
      #if "Entered as a lump sum" is selected
      data$lumpSum <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$lumpSum)))
      
    }
    
    #calculations
    perfCalc()
    
    #output a results table
    col1 <- c("1. Cost of difference in performance")
    
    col2 <- c(paste0(currSymHTML[data$currency], formatC(store$perfCalc$diffPerfCost, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)))

    #check if all the calculations result in a valid value and render appropriate results column
    if (all(as.logical(lapply(req(store$perfCalc), is.finite)))){
      
      return(cbind(col1, col2))
  
    } else {
    
      return(cbind(col1, "-"))
      
    }
    
  })
  
  #performance calculations
  perfCalc <- function() {
    
    print("perfCalc")
    
    #check if performance costs are conducted "Calculated" or "Entered as a lump sum"
    if (req(input$diffPerfMethod) == "Calculated") {
      
      #if "Calculated" is selected
      store$perfCalc$diffPerfCost = (data$compaRatioLeave - data$compaRatioReplace) * data$midpointPay
      
    } else {
      
      #if "Entered as a lump sum" is selected
      store$perfCalc$diffPerfCost = data$lumpSum
      
    }
    
  }
  
  #performance table
  output$perfTab <- renderTable({
    
    req(perfData())
    
  }, align = 'lr', sanitize.text.function = function(x) x)
  
  #separation-totals#
  
  #separation-totals UI
  output$totSepUI <- renderUI({
    
    print("sepTotUI")
    
    tagList({
              
      fluidRow(
        
        column(12,
          
          "Note: Currency is set to ",data$currency,
          
          br(),
        
          br()
          
        )
            
      )

    })
    
  })
  
  #separation-totals inputs
  sepTotData <- reactive({
    
    print("sepTotData")
    
    #output a results table
    col1 <- c("1. Separation costs", 
            "2. Replacement costs", 
            "3. Training costs", 
            "4. Performance costs",
            "Total")
    
    col2 <- c(paste0(currSymHTML[data$currency], formatC(sum(as.numeric(store$sepCalc)), format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)),
            paste0(currSymHTML[data$currency], formatC(sum(as.numeric(store$repCalc)), format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)),
            paste0(currSymHTML[data$currency], formatC(sum(as.numeric(store$traCalc)), format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)),
            paste0(currSymHTML[data$currency], formatC(sum(as.numeric(store$perfCalc)), format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)),
            paste0(currSymHTML[data$currency], formatC(sum(as.numeric(c(store$sepCalc,store$repCalc,store$traCalc,store$perfCalc))), format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)))
  
    #check if all the calculations result in a valid value and render appropriate results column
    if (all(as.logical(lapply(c(req(store$sepCalc),req(store$repCalc),req(store$traCalc),req(store$perfCalc)), is.finite)))){
      
      return(cbind(col1, col2))
  
    } else {
    
      return(cbind(col1, rep("-", 5)))
      
    }
  
  })
  
  #separation-totals table
  output$sepTotTab <- renderTable({
    
    req(sepTotData())
    
  }, align = 'lr', sanitize.text.function = function(x) x)
  
  #separation-totals pie chart
  output$sepTotPie <- renderPlot({
    
    req(all(as.logical(lapply(c(req(store$sepCalc),req(store$repCalc),req(store$traCalc),req(store$perfCalc)), is.finite))) & any(as.logical(c(req(store$sepCalc),req(store$repCalc),req(store$traCalc),req(store$perfCalc)))) > 0)
    
    #assign values for "slices" of the pie
    slices <- c(sum(as.numeric(store$sepCalc)),sum(as.numeric(store$repCalc)),sum(as.numeric(store$traCalc)),sum(as.numeric(store$perfCalc)))

    #name slices
    names(slices) <- c("Separation costs", 
                    "Replacement costs", 
                    "Training costs", 
                    "Performance costs")
    
    slices <- sort(slices, decreasing = TRUE) 
    
    pct <- lapply(slices, function (x) round(x/sum(slices)*100))
     
    pctlbls <- paste(pct,"%",sep="") #add % to labels

    par(mar = c(1,1,1,1))
    
    pie(
      slices, 
      edges = 400, 
      radius = 0.8,
      clockwise = TRUE,
      angle = 45,
      col = rainbow(length(slices)),
      labels = head(pctlbls, ((length(pct))-(sum(pct<2)))),
      cex = 0.8
    )
    
    legend(
      x = -2.0,
      y = 0.7,
      inset = .05,
      title = "Costs", 
      legend = names(slices),
      fill = rainbow(length(slices)),
      horiz = FALSE,
      cex = 0.8,
      text.width = 0.8
    )
    
  })

  ###welfare###
  
  #alcohol tab#
  
  #alcohol UI
  output$alcUI <- renderUI({
    
    print("alcUI")

    tagList({
      
      fluidRow(
        
        column(12,

          fluidRow(

            column(12,
              
              "Note: Currency is set to ",data$currency,
              
              br(),
              
              br(),
              
              div(style="margin-bottom:5px;","The productivity decrease attributable to alcohol (percentage):"),
                        
              textInput("pctAlcProdDecrease", label = NULL, paste0(isolate(data$pctAlcProdDecrease),"%"))
              
            )
            
          ),
            
          fluidRow(
            
            br(),

            column(3,
              
              "Gender-age cohort"
              
            ),
            
            column(3,
              
              "Number of individuals"
              
            ),            
            
            column(3,
              
              "% with alcohol abuse problems"
              
            ),
            
            column(3,
              
              "Average annual earnings of cohort"
              
            )

          ),
          
          fluidRow(
            
            br(),

            column(3,
              
              "Males, 25 & under"
              
            ),
            
            column(3,
              
              textInput("alcNumIndM1", label = NULL, isolate(data$alcNumIndM1))
              
            ),            
            
            column(3,
              
              textInput("pctAlcProbM1", label = NULL, paste0(isolate(data$pctAlcProbM1),"%"))
              
            ),
            
            column(3,
              
              textInput("aveAnnCohEarnM1", label = NULL, paste0(currSym[data$currency],isolate(data$aveAnnCohEarnM1)))
              
            )

          ),
          
          fluidRow(

            column(3,
              
              "Males, 26 to 44"
              
            ),
            
            column(3,
              
              textInput("alcNumIndM2", label = NULL, isolate(data$alcNumIndM2))
              
            ),            
            
            column(3,
              
              textInput("pctAlcProbM2", label = NULL, paste0(isolate(data$pctAlcProbM2),"%"))
              
            ),
            
            column(3,
              
              textInput("aveAnnCohEarnM2", label = NULL, paste0(currSym[data$currency],isolate(data$aveAnnCohEarnM2)))
              
            )

          ),
          
          fluidRow(

            column(3,
              
              "Males, 45 & over"
              
            ),
            
            column(3,
              
              textInput("alcNumIndM3", label = NULL, isolate(data$alcNumIndM3))
              
            ),            
            
            column(3,
              
              textInput("pctAlcProbM3", label = NULL, paste0(isolate(data$pctAlcProbM3),"%"))
              
            ),
            
            column(3,
              
              textInput("aveAnnCohEarnM3", label = NULL, paste0(currSym[data$currency],isolate(data$aveAnnCohEarnM3)))
              
            )

          ),
          
          fluidRow(

            column(3,
              
              "Females, 25 & under"
              
            ),
            
            column(3,
              
              textInput("alcNumIndF1", label = NULL, isolate(data$alcNumIndF1))
              
            ),            
            
            column(3,
              
              textInput("pctAlcProbF1", label = NULL, paste0(isolate(data$pctAlcProbF1),"%"))
              
            ),
            
            column(3,
              
              textInput("aveAnnCohEarnF1", label = NULL, paste0(currSym[data$currency],isolate(data$aveAnnCohEarnF1)))
              
            )

          ),
          
          fluidRow(

            column(3,
              
              "Females, 26 to 44"
              
            ),
            
            column(3,
              
              textInput("alcNumIndF2", label = NULL, isolate(data$alcNumIndF2))
              
            ),            
            
            column(3,
              
              textInput("pctAlcProbF2", label = NULL, paste0(isolate(data$pctAlcProbF2),"%"))
              
            ),
            
            column(3,
              
              textInput("aveAnnCohEarnF2", label = NULL, paste0(currSym[data$currency],isolate(data$aveAnnCohEarnF2)))
              
            )

          ),
          
          fluidRow(

            column(3,
              
              "Females, 45 & over"
              
            ),
            
            column(3,
              
              textInput("alcNumIndF3", label = NULL, isolate(data$alcNumIndF3))
              
            ),            
            
            column(3,
              
              textInput("pctAlcProbF3", label = NULL, paste0(isolate(data$pctAlcProbF3),"%"))
              
            ),
            
            column(3,
              
              textInput("aveAnnCohEarnF3", label = NULL, paste0(currSym[data$currency],isolate(data$aveAnnCohEarnF3)))
              
            )

          )
        
        )
        
      )

    })

  })
  
  #alcohol inputs
  alcData <- reactive({
    
    print("alcData")
    
    #convert text inputs to numbers and strip symbols
    #alcohol abuse cost inputs
    data$pctAlcProdDecrease <- as.numeric(gsub("[%]", "", input$pctAlcProdDecrease))
    data$alcNumIndM1 <- as.numeric(input$alcNumIndM1)
    data$alcNumIndM2 <- as.numeric(input$alcNumIndM2)
    data$alcNumIndM3 <- as.numeric(input$alcNumIndM3)
    data$alcNumIndF1 <- as.numeric(input$alcNumIndF1)
    data$alcNumIndF2 <- as.numeric(input$alcNumIndF2)
    data$alcNumIndF3 <- as.numeric(input$alcNumIndF3)
    data$pctAlcProbM1 <- as.numeric(gsub("[%]", "", input$pctAlcProbM1))
    data$pctAlcProbM2 <- as.numeric(gsub("[%]", "", input$pctAlcProbM2))
    data$pctAlcProbM3 <- as.numeric(gsub("[%]", "", input$pctAlcProbM3))
    data$pctAlcProbF1 <- as.numeric(gsub("[%]", "", input$pctAlcProbF1))
    data$pctAlcProbF2 <- as.numeric(gsub("[%]", "", input$pctAlcProbF2))
    data$pctAlcProbF3 <- as.numeric(gsub("[%]", "", input$pctAlcProbF3))
    data$aveAnnCohEarnM1 <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$aveAnnCohEarnM1)))
    data$aveAnnCohEarnM2 <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$aveAnnCohEarnM2)))
    data$aveAnnCohEarnM3 <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$aveAnnCohEarnM3)))
    data$aveAnnCohEarnF1 <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$aveAnnCohEarnF1)))
    data$aveAnnCohEarnF2 <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$aveAnnCohEarnF2)))
    data$aveAnnCohEarnF3 <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$aveAnnCohEarnF3)))

    #calculations
    alcCalc()
    
    #output a results table
    col1 <- c("1. Total Cost of Males 25 & Under", 
            "2. Total Cost of Males 26 to 44", 
            "3. Total Cost of Males 45 & Over",
            "4. Total Cost of Females 25 & Under", 
            "5. Total Cost of Females 26 to 44", 
            "6. Total Cost of Females 45 & Over",
            "7. Total estimated productivity loss",
            "8. Total estimated productivity loss due to alcohol")
    
    col2 <- c(paste0(currSymHTML[data$currency], formatC(store$alcCalc$alcCostM1, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)),
            paste0(currSymHTML[data$currency], formatC(store$alcCalc$alcCostM2, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)),
            paste0(currSymHTML[data$currency], formatC(store$alcCalc$alcCostM3, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)),
            paste0(currSymHTML[data$currency], formatC(store$alcCalc$alcCostF1, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)),
            paste0(currSymHTML[data$currency], formatC(store$alcCalc$alcCostF2, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)),
            paste0(currSymHTML[data$currency], formatC(store$alcCalc$alcCostF3, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)),
            paste0(currSymHTML[data$currency], formatC(store$alcCalc$totProdLoss, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)),
            paste0(currSymHTML[data$currency], formatC(store$alcCalc$alcTotProdLoss, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)))
    
    #check if all the calculations result in a valid value and render appropriate results column
    if (!any(as.logical(store$alcCalc) == logical(0))){
      
      if (all(as.logical(lapply(req(store$alcCalc), is.finite)))){
    
        return(cbind(col1, col2))
    
      } else {
      
        return(cbind(col1, rep("-", 8)))
        
      }
      
    } else {
      
      return(cbind(col1, rep("-", 8)))
      
    }
    
  })
  
  #alcohol calculations
  alcCalc <- function() {
    
    print("alcCalc")
    
    #calculate work-life program benefits
    store$alcCalc$alcCostM1 <- data$alcNumIndM1 * (data$aveAnnCohEarnM1 * (data$pctAlcProbM1/100) * (data$pctAlcProdDecrease/100))
    store$alcCalc$alcCostM2 <- data$alcNumIndM2 * (data$aveAnnCohEarnM2 * (data$pctAlcProbM2/100) * (data$pctAlcProdDecrease/100))
    store$alcCalc$alcCostM3 <- data$alcNumIndM3 * (data$aveAnnCohEarnM3 * (data$pctAlcProbM3/100) * (data$pctAlcProdDecrease/100))
    store$alcCalc$alcCostF1 <- data$alcNumIndF1 * (data$aveAnnCohEarnF1 * (data$pctAlcProbF1/100) * (data$pctAlcProdDecrease/100))
    store$alcCalc$alcCostF2 <- data$alcNumIndF2 * (data$aveAnnCohEarnF2 * (data$pctAlcProbF2/100) * (data$pctAlcProdDecrease/100))
    store$alcCalc$alcCostF3 <- data$alcNumIndF3 * (data$aveAnnCohEarnF3 * (data$pctAlcProbF3/100) * (data$pctAlcProdDecrease/100))
    store$alcCalc$alcTotProdLoss <- sum(store$alcCalc$alcCostM1,
                                        store$alcCalc$alcCostM2,
                                        store$alcCalc$alcCostM3,
                                        store$alcCalc$alcCostF1,
                                        store$alcCalc$alcCostF2,
                                        store$alcCalc$alcCostF3)
    store$alcCalc$totProdLoss <- (store$alcCalc$alcTotProdLoss/(data$pctAlcProdDecrease/100))
    
  }
  
  #show the values in a table
  output$alcTab <- renderTable({
    
    req(alcData())
    
  }, align = 'lr', sanitize.text.function = function(x) x)
  
  #EAP tab#
  
  #EAP UI
  output$eAPUI <- renderUI({
    
    print("eAPUI")

    tagList({
      
      fluidRow(
        
        column(12,
          
          "Note: Currency is set to ",data$currency,
          
          br(),
          
          br(),
          
          fluidRow(
            
            column(3,
              
              ""
              
            ),
            
            column(3,
              
              "Number of employees"
              
            ),            
            
            column(3,
              
              "Individual cost of replacement and training"
              
            ),
            
            column(3,
              
              "Number of employees who quit after EAP"
              
            )

          ),
          
          fluidRow(
            
            br(),
            
            column(3,
              
              "Production"
              
            ),
            
            column(3,
              
              textInput("eAPNumEmpProd", label = NULL, isolate(data$eAPNumEmpProd))
              
            ),            
            
            column(3,
              
              textInput("eAPCostRepProd", label = NULL, paste0(currSym[data$currency],isolate(data$eAPCostRepProd)))
              
            ),
            
            column(3,
              
              textInput("eAPNumQuitProd", label = NULL, isolate(data$eAPNumQuitProd))
              
            )

          ),
          
          fluidRow(
            
            column(3,
              
              "Administrative/support"
              
            ),
            
            column(3,
              
              textInput("eAPNumEmpCler", label = NULL, isolate(data$eAPNumEmpCler))
              
            ),            
            
            column(3,
              
              textInput("eAPCostRepCler", label = NULL, paste0(currSym[data$currency],isolate(data$eAPCostRepCler)))
              
            ),
            
            column(3,
              
              textInput("eAPNumQuitCler", label = NULL, isolate(data$eAPNumQuitCler))
              
            )

          ),
          
          fluidRow(
            
            column(3,
              
              "Management"
              
            ),
            
            column(3,
              
              textInput("eAPNumEmpMgmt", label = NULL, isolate(data$eAPNumEmpMgmt))
              
            ),            
            
            column(3,
              
              textInput("eAPCostRepMgmt", label = NULL, paste0(currSym[data$currency],isolate(data$eAPCostRepMgmt)))
              
            ),
            
            column(3,
              
              textInput("eAPNumQuitMgmt", label = NULL, isolate(data$eAPNumQuitMgmt))
              
            ),
            
            hr()

          ),
          
          fluidRow(
              
            br(),
            
            column(3,
              
              ""
              
            ),
            
            column(9,
              
              div(style="margin-bottom:5px;","Percentage of total annual EAP budget used for hospitalization:"),
                        
              textInput("eAPPctBudgHosp", label = NULL, paste0(isolate(data$eAPPctBudgHosp),"%"))
              
            )

          ),
          
          fluidRow(
            
            column(3,
              
              ""
              
            ),
            
            column(9,
              
              div(style="margin-bottom:5px;","Hospitalization cost:"),
                        
              textInput("eAPHospCost", label = NULL, paste0(currSym[data$currency],isolate(data$eAPHospCost)))
              
            )
            
          )
        
        )
        
      )

    })

  })

  #EAP inputs
  eAPData <- reactive({

    print("eAPData")

    #convert text inputs to numbers and strip symbols
    #EAP program benefit inputs
    data$eAPNumEmpProd <- as.numeric(input$eAPNumEmpProd)
    data$eAPNumEmpCler <- as.numeric(input$eAPNumEmpCler)
    data$eAPNumEmpMgmt <- as.numeric(input$eAPNumEmpMgmt)
    data$eAPCostRepProd <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$eAPCostRepProd)))
    data$eAPCostRepCler <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$eAPCostRepCler)))
    data$eAPCostRepMgmt <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$eAPCostRepMgmt)))
    data$eAPNumQuitProd <- as.numeric(input$eAPNumQuitProd)
    data$eAPNumQuitCler <- as.numeric(input$eAPNumQuitCler)
    data$eAPNumQuitMgmt <- as.numeric(input$eAPNumQuitMgmt)
    data$eAPPctBudgHosp <- as.numeric(gsub("[%]", "", input$eAPPctBudgHosp))
    data$eAPHospCost <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$eAPHospCost)))

    #calculations
    eAPCalc()

    #output a results table
    col1 <- c("1. Total potential cost",
            "2. Total cost of terminations",
            "3. Total budget",
            "4. Return-on-Investment")

    col2 <- c(paste0(currSymHTML[data$currency], formatC(store$eAPCalc$eAPTotPotCost, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)),
            paste0(currSymHTML[data$currency], formatC(store$eAPCalc$eAPCostOfTerm, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)),
            paste0(currSymHTML[data$currency], formatC(store$eAPCalc$eAPTotBudget, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)),
            paste0(formatC(store$eAPCalc$eAPROI, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE),"%"))

    #check if all the calculations result in a valid value and render appropriate results column
    if (!any(as.logical(store$eAPCalc) == logical(0))){
      
      if (all(as.logical(lapply(req(store$eAPCalc), is.finite)))){
    
        return(cbind(col1, col2))
    
      } else {
      
        return(cbind(col1, rep("-", 4)))
        
      }
      
    } else {
      
      return(cbind(col1, rep("-", 4)))
      
    }
    
  })
  
  #calculations
  eAPCalc <- function() {
    
    print("eAPCalc")
    
    #calculate EAP program benefits
    store$eAPCalc$eAPTotPotCost <- ((data$eAPNumEmpProd * data$eAPCostRepProd) + (data$eAPNumEmpCler * data$eAPCostRepCler) + (data$eAPNumEmpMgmt * data$eAPCostRepMgmt))
    store$eAPCalc$eAPCostOfTerm <- ((data$eAPNumQuitProd * data$eAPCostRepProd) + (data$eAPNumQuitCler * data$eAPCostRepCler) + (data$eAPNumQuitMgmt * data$eAPCostRepMgmt))
    store$eAPCalc$eAPTotBudget <- ((data$eAPHospCost * 100) / data$eAPPctBudgHosp)
    store$eAPCalc$eAPROI <- (((store$eAPCalc$eAPTotPotCost - (store$eAPCalc$eAPTotBudget + store$eAPCalc$eAPCostOfTerm)) / (store$eAPCalc$eAPTotBudget + store$eAPCalc$eAPCostOfTerm)) * 100)
    
  }
  
  #show the values in a table
  output$eAPTab <- renderTable({
    
    req(eAPData())
    
  }, align = 'lr', sanitize.text.function = function(x) x)
  
  ###engagement###
  
  #engagement UI
  output$engUI <- renderUI({
    
    print("engUI")

    fluidRow(
      
      column(12,
        
        "Note: Currency is set to ",data$currency,

        br(),
        
        br(),
        
        fluidRow(
          
          column(6,
            
            " "
            
          ),
          
          column(6,
          
            HTML("Average score on overall engagment") 
            
          )
          
        ),

        fluidRow(
         
          br(),
          
          column(6,
            
            HTML("Top quartile performers:")
            
          ),
          
          column(6,
          
            textInput("engAveScoreTop", label = NULL, isolate(data$engAveScoreTop)) 
            
          )
          
        ),
        
        fluidRow(
         
          br(),
          
          column(6,
            
            HTML("Bottom quartile performers:")
            
          ),
          
          column(6,
          
            textInput("engAveScoreBottom", label = NULL, isolate(data$engAveScoreBottom)) 
            
          )
          
        ),
        
        fluidRow(
          
          br(),
          
          br(),
          
          column(6,
            
            " "
            
          ),
          
          column(3,
            
            "Period 1"
            
          ),
          
          column(3,
          
            "Period 2"
            
          )
          
        ),
        
        fluidRow(
         
          br(),
          
          column(6,
            
            HTML("Value of outcome (e.g., revenue) for units in the TOP quartile of overall performance:")
            
          ),
          
          column(3,
            
            textInput("engRevTop1", label = NULL, paste0(currSym[data$currency],isolate(data$engRevTop1)))
            
          ),
          
          column(3,
          
            textInput("engRevTop2", label = NULL, paste0(currSym[data$currency],isolate(data$engRevTop2))) 
            
          )
          
        ),
        
        fluidRow(
          
          br(),
          
          column(6,
            
            HTML("Value of outcome (e.g., revenue) for units in the BOTTOM quartile of overall performance:")
            
          ),
          
          column(3,
            
            textInput("engRevBottom1", label = NULL, paste0(currSym[data$currency],isolate(data$engRevBottom1)))
            
          ),
          
          column(3,
          
            textInput("engRevBottom2", label = NULL, paste0(currSym[data$currency],isolate(data$engRevBottom2)))
            
          )
          
        )
      
      )
      
    )
    
  })
  
  #engagement inputs
  engData1 <- reactive({

    print("engData1")

    #convert text inputs to numbers and strip symbols
    #engagement inputs
    data$engRevTop1 <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$engRevTop1)))
    data$engRevTop2 <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$engRevTop2)))
    data$engAveScoreTop <- as.numeric(input$engAveScoreTop)
    data$engRevBottom1 <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$engRevBottom1)))
    data$engRevBottom2 <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$engRevBottom2)))
    data$engAveScoreBottom <- as.numeric(input$engAveScoreBottom)

    #calculations
    engCalc()

    #output a results table
    col1 <- "Performance difference between top and bottom quartiles"

    col2 <- paste0(currSymHTML[data$currency], formatC(store$engCalc$revDiff1, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE))
    
    col3 <- paste0(currSymHTML[data$currency], formatC(store$engCalc$revDiff2, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE))

    #check if all the calculations result in a valid value and render appropriate results column
    if (!any(as.logical(store$engCalc) == logical(0))){
      
      if (all(as.logical(lapply(req(store$engCalc), is.finite)))){
    
        tab <- cbind(col1, col2, col3)
        
        colnames(tab) <- c(" ","Period 1","Period 2")
        
        return(tab)
    
      } else {
        
        tab <- cbind(col1, "-", "-")
        
        colnames(tab) <- c(" ","Period 1","Period 2")
        
        return(tab)
        
      }
      
    } else {
      
      tab <- cbind(col1, "-", "-")
      
      colnames(tab) <- c(" ","Period 1","Period 2")
      
      return(tab)
      
    }
    
  })
  
  #engagement inputs
  engData2 <- reactive({

    print("engData2")

    #convert text inputs to numbers and strip symbols
    #engagement inputs
    data$engRevTop1 <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$engRevTop1)))
    data$engRevTop2 <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$engRevTop2)))
    data$engAveScoreTop <- as.numeric(input$engAveScoreTop)
    data$engRevBottom1 <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$engRevBottom1)))
    data$engRevBottom2 <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$engRevBottom2)))
    data$engAveScoreBottom <- as.numeric(input$engAveScoreBottom)

    #calculations
    engCalc()

    #output a results table
    col1 <- c("Top quartile",
            "Bottom quartile",
            "Difference in outcome improvements (Top quartile vs. bottom quartile)")

    col2 <- c(paste0(currSymHTML[data$currency], formatC(store$engCalc$revTopChange, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)),
            paste0(currSymHTML[data$currency], formatC(store$engCalc$revBottomChange, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)),
            paste0(currSymHTML[data$currency], formatC(store$engCalc$diffImprove, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)))
    
    col3 <- c(paste0(formatC(store$engCalc$revTopChangePct, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE),"%"),
            paste0(formatC(store$engCalc$revBottomChangePct, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE),"%"),
            paste0(formatC(store$engCalc$diffImprovePct, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE),"%"))

    #check if all the calculations result in a valid value and render appropriate results column
    if (!any(as.logical(store$engCalc) == logical(0))){
      
      if (all(as.logical(lapply(req(store$engCalc), is.finite)))){
    
        tab <- cbind(col1, col2, col3)
        
        colnames(tab) <- c(" ","Amount change","Percent change")
        
        return(tab)
    
      } else {
        
        tab <- cbind(col1, rep("-",3), rep("-",3))
        
        colnames(tab) <- c(" ","Amount change","Percent change")
        
        return(tab)
        
      }
      
    } else {
      
      tab <- cbind(col1, rep("-",3), rep("-",3))
      
      colnames(tab) <- c(" ","Amount change","Percent change")
      
      return(tab)
      
    }

  })
  
  #engagement inputs
  engData3 <- reactive({

    print("engData3")

    #convert text inputs to numbers and strip symbols
    #engagement inputs
    data$engRevTop1 <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$engRevTop1)))
    data$engRevTop2 <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$engRevTop2)))
    data$engAveScoreTop <- as.numeric(input$engAveScoreTop)
    data$engRevBottom1 <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$engRevBottom1)))
    data$engRevBottom2 <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$engRevBottom2)))
    data$engAveScoreBottom <- as.numeric(input$engAveScoreBottom)

    #calculations
    engCalc()

    #output a results table
    col1 <- c("Gap in overall engagement between top and bottom quartiles",
            "Difference in improvement associated with each additional point in the overall engagement score",
            "Difference in total outcome at period 2 associated with each additional point in the overall engagement score at period 1")

    col2 <- c(formatC(store$engCalc$engGapTopBottom, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE),
            paste0(currSymHTML[data$currency], formatC(store$engCalc$diffImpPerEngPoint, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)),
            paste0(currSymHTML[data$currency], formatC(store$engCalc$diffP2PerEngPoint, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)))

    #check if all the calculations result in a valid value and render appropriate results column
    if (!any(as.logical(store$engCalc) == logical(0))){
      
      if (all(as.logical(lapply(req(store$engCalc), is.finite)))){
        
        return(cbind(col1, col2))
    
      } else {
        
        return(cbind(col1, rep("-",3)))
        
      }
      
    } else {
      
      return(cbind(col1, rep("-",3)))
      
    }

    
  })
  
  #calculations
  engCalc <- function() {
    
    print("engCalc")
    
    #calculate engagement benefits
    store$engCalc$revDiff1 <- data$engRevTop1 - data$engRevBottom1
    store$engCalc$revDiff2 <- data$engRevTop2 - data$engRevBottom2
    store$engCalc$revTopChange <- data$engRevTop2 - data$engRevTop1
    store$engCalc$revBottomChange <- data$engRevBottom2 - data$engRevBottom1
    store$engCalc$revTopChangePct <- 100 * ((data$engRevTop2 / data$engRevTop1) - 1)
    store$engCalc$revBottomChangePct <- 100 * ((data$engRevBottom2 / data$engRevBottom1) - 1)
    store$engCalc$diffImprove <- store$engCalc$revTopChange - store$engCalc$revBottomChange
    store$engCalc$diffImprovePct <- 100 * ((store$engCalc$revTopChangePct / store$engCalc$revBottomChangePct) - 1)
    store$engCalc$engGapTopBottom <- data$engAveScoreTop - data$engAveScoreBottom
    store$engCalc$diffImpPerEngPoint <- store$engCalc$diffImprove / store$engCalc$engGapTopBottom 
    store$engCalc$diffP2PerEngPoint <- store$engCalc$revDiff2 / store$engCalc$engGapTopBottom
    
    print(data$engRevTop1)
    print(data$engRevTop2)
    print(data$engRevBottom1)
    print(data$engRevBottom2)
    
    
    
    print(store$engCalc$revDiff1)
    print(store$engCalc$revDiff2)
    print(store$engCalc$revTopChange)
    print(store$engCalc$revBottomChange)
    print(store$engCalc$revTopChangePct)
    print(store$engCalc$revBottomChangePct)
    print(store$engCalc$diffImprove)
    print(store$engCalc$diffImprovePct)
    print(store$engCalc$engGapTopBottom)
    print(store$engCalc$diffImpPerEngPoint)
    print(store$engCalc$diffP2PerEngPoint)
    
  }
  
  #show the values in a table
  output$engTab1 <- renderTable({
    
    req(engData1())
    
  }, align = 'lrr', sanitize.text.function = function(x) x)
  
  #show the values in a table
  output$engTab2 <- renderTable({
    
    req(engData2())
    
  }, align = 'lrr', sanitize.text.function = function(x) x)
  
  #show the values in a table
  output$engTab3 <- renderTable({
    
    req(engData3())
    
  }, align = 'lr', sanitize.text.function = function(x) x)
  
  ###work-life###
  
  #work-life UI
  output$wLifUI <- renderUI({
    
    print("wLifUI")
    
    fluidRow(
      
      column(12,
        
        "Note: Currency is set to ",data$currency,
        
        br(),

        br(),

        textInput("totCostOfAbs", "Total cost of absenteeism:", paste0(currSym[data$currency],isolate(data$totCostOfAbs))),
        textInput("pctReduceAbs", "My organization is planning to implement a workplace flexibility program that I estimate will reduce absenteeism by:", paste0(isolate(data$pctReduceAbs),"%")),
        textInput("totCostOfSep", "Total cost of turnover:", paste0(currSym[data$currency],isolate(data$totCostOfSep))),
        textInput("pctReduceSep", "My organization is planning to implement a workplace flexibility program that I estimate will reduce my organization's overall turnover by:", paste0(isolate(data$pctReduceSep),"%"))
      
      )
      
    )
    
  })
  
  #work-life inputs
  wLifData <- reactive({
    
    print("wLifData")
    
    #convert text inputs to numbers and strip symbols
    #work-life program benefit inputs
    data$totCostOfAbs <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$totCostOfAbs)))
    data$pctReduceAbs <- as.numeric(gsub("[%]", "", input$pctReduceAbs))
    data$totCostOfSep <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$totCostOfSep)))
    data$pctReduceSep <- as.numeric(gsub("[%]", "", input$pctReduceSep))
    
    #calculations
    wLifCalc()
    
    #output a results table
    col1 <- c("1. Original cost of absenteeism", 
            "2. Cost of absenteeism after workplace flexibility program", 
            "3. Original cost of turnover", 
            "4. Cost of turnover after workplace flexibility program",
            "5. Total savings from workplace flexibility programs")
    
    col2 <- c(paste0(currSymHTML[data$currency], formatC(data$totCostOfAbs, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)),
            paste0(currSymHTML[data$currency], formatC(store$wLifCalc$newCostOfAbs, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)),
            paste0(currSymHTML[data$currency], formatC(data$totCostOfSep, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)),
            paste0(currSymHTML[data$currency], formatC(store$wLifCalc$newCostOfSep, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)),
            paste0(currSymHTML[data$currency], formatC(store$wLifCalc$redTotCost, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)))
    
    #check if all the calculations result in a valid value and render appropriate results column
    if (all(as.logical(lapply(req(store$wLifCalc), is.finite)))){
  
      return(cbind(col1, col2))
  
    } else {
    
      return(cbind(col1, rep("-", 5)))
      
    }
    
  })
  
  #work-life calculations
  wLifCalc <- function() {
    
    print("wLifCalc")
    
    #calculate work-life program benefits
    store$wLifCalc$redCostOfAbs <- (data$totCostOfAbs * (data$pctReduceAbs/100))
    store$wLifCalc$redCostOfSep <- (data$totCostOfSep * (data$pctReduceSep/100))
    store$wLifCalc$redTotCost <- (store$wLifCalc$redCostOfAbs + store$wLifCalc$redCostOfSep)
    store$wLifCalc$newCostOfAbs <- (data$totCostOfAbs - store$wLifCalc$redCostOfAbs)
    store$wLifCalc$newCostOfSep <- (data$totCostOfSep - store$wLifCalc$redCostOfSep)
    
  }
  
  #show the values in a table
  output$wLifTab <- renderTable({
    
    req(wLifData())
    
  }, align = 'lr', sanitize.text.function = function(x) x)
  
  ###staffing###
  
  #taylor-russell tab#
  
  #taylor-russell UI
  output$tayRusUI <- renderUI({
    
    print("tayRusUI")
    
    fluidRow(
      
      column(12,
        
        "Note: Currency is set to ",data$currency,
        
        br(),

        br(),

        textInput("tayRusVal", "Validity (between 0 and 1):", isolate(data$tayRusVal)),
        textInput("tayRusSR", "Selection ratio (between 0 and 0.95):", isolate(data$tayRusSR)),
        textInput("tayRusBR", "Base rate (between 0.05 and 0.90):", isolate(data$tayRusBR))
      
      )
      
    )
    
  })
  
  #taylor-russell inputs
  tayRusData <- reactive({
    
    print("tayRusData")
    
    #convert text inputs to numbers and strip symbols
    #taylor-russell utility inputs
    data$tayRusVal <- as.numeric(input$tayRusVal)
    data$tayRusSR <- as.numeric(input$tayRusSR)
    data$tayRusBR <- as.numeric(input$tayRusBR)

    
    #calculations
    tayRusCalc()
    
    #output a results table
    col1 <- c("1. Success ratio", 
            "2. Percentage gain")
    
    col2 <- c(formatC(store$tayRusCalc$tayRusSuccessRat, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE),
            paste0(formatC(store$tayRusCalc$tayRusPctGain, format = "f", digits = 1, big.mark = "," , drop0trailing = FALSE),"%"))
    
    #check if all the calculations result in a valid value and render appropriate results column
    if (all(as.logical(lapply(req(store$tayRusCalc), is.finite)))){
  
      return(cbind(col1, col2))
  
    } else {
    
      return(cbind(col1, rep("-", 2)))
      
    }
    
  })
  
  #taylor-russell calculations
  tayRusCalc <- function() {
    
    print("tayRusCalc")
    
    SR <- data$tayRusSR
    BR <- data$tayRusBR
    rxy <- data$tayRusVal
    
    #calculate taylor-russell utility
    #get z-score cutoffs corresponding to selection ratio and base rate
    Zsr <- qnorm(1-SR)
    Zbr <- qnorm(1-BR)

    #create correlation matrix, sigma
    sigma <- diag(2)
    sigma[1,2] <- sigma[2,1] <- rxy
    
    #normal area - quadrant A - assumes bivariate normal
    p <- (pmvnorm(mean=rep(0, 2), sigma, lower=c(Zsr,Zbr), upper=c(Inf,Inf)))[1]
    
    #Taylor-Russell - A divided by (A+B), the selection ratio 
    store$tayRusCalc$tayRusSuccessRat <- p/SR
    
    #percentage gain
    store$tayRusCalc$tayRusPctGain <- (store$tayRusCalc$tayRusSuccessRat - BR) * 100
   
  }
  
  #show the values in a table
  output$tayRusTab <- renderTable({
    
    req(tayRusData())
    
  }, align = 'lr', sanitize.text.function = function(x) x)
  
  #naylor-shine tab#

  #naylor-shine UI
  output$nayShiUI <- renderUI({
    
    print("nayShiUI")
    
    fluidRow(
      
      column(12,
        
        "Note: Currency is set to ",data$currency,
        
        br(),

        br(),
        
        HTML("Validity coefficient (r<sub>xy</sub>) (between -1 and +1):"),

        div(style = "margin-top:5px;white-space:nowrap;",
        
          textInput("nayShiVal", label = NULL, isolate(data$nayShiVal)),
  
          br(),
          
          "Please select one:",
          
          br(),
          
          radioButtons("nayShiCalcSel", label = NULL, selected = isolate(data$nayShiCalcSel), choiceNames = caseHtml, choiceValues = caseNames)
        
        )
        
      )
      
    )
    
  })

  #calculation select UI  
  output$calcSelInputs <- renderUI({
    
    print("calcSelUI")
    
    #check which calculation type is selected
    if (req(input$nayShiCalcSel) == 1) {
      
      #update settings
      data$nayShiCalcSel <- 1
    
      #inputs if 1 is selected  
      tagList(
        
        HTML("Selection ratio &Phi;<sub>i</sub> (between 0 and 1):"),
        
        div(style = "margin-top:5px;",
          textInput("nayShiSR", label = NULL, isolate(data$nayShiSR))
        )
        
      )
      
    } else {

      if (req(input$nayShiCalcSel) == 2) {
        
        #update settings
        data$nayShiCalcSel <- 2
      
        #inputs if 2 is selected  
        tagList(
        
          HTML("Z<sub>xi</sub>:"),
          
          div(style = "margin-top:5px;",
            textInput("nayShiZxi", label = NULL, isolate(data$nayShiZxi))
          )
          
        )
        
      } else {
        
        #update settings
        data$nayShiCalcSel <- 3
        
        #inputs if 3 is selected
        tagList(
          
          HTML("Mean criterion score Z<sub>yi</sub> (number of standard deviations):"),
          
          div(style = "margin-top:5px;",
            textInput("nayShiZyi", label = NULL, isolate(data$nayShiZyi))
          )
          
        )
        
      }
      
    }
    
  })

  #naylor-shine inputs
  nayShiData <- reactive({
    
    print("nayShiData")
    
    #convert text inputs to numbers and strip symbols
    #naylor-shine utility inputs
    data$nayShiVal <- as.numeric(input$nayShiVal)
    
    #check which calculation type is selected
    if (req(input$nayShiCalcSel) == 1) {
      
      #update data
      data$nayShiCalcSel <- 1
      
      #load values from these inputs if 1 is selected
      store$nayShiCalc$nayShiSR <- data$nayShiSR <- as.numeric(input$nayShiSR)
      
    } else {

      if (req(input$nayShiCalcSel) == 2) {
        
        #update data
        data$nayShiCalcSel <- 2
        
        #load values from these inputs if 2 is selected
        store$nayShiCalc$nayShiZxi <- data$nayShiZxi <- as.numeric(input$nayShiZxi)
      
      } else {
        
        #update data
        data$nayShiCalcSel <- 3
        
        #load values from these inputs if 3 is selected
        store$nayShiCalc$nayShiZyi <- data$nayShiZyi <- as.numeric(input$nayShiZyi)
      
      }
      
    }
    
    #calculations
    nayShiCalc()
    
    #output a results table
    col1 <- c("1. Selection ratio &Phi;<sub>i</sub>", 
            "2. Z<sub>xi</sub>",
            "3. &lambda;<sub>i</>",
            "4. Mean criterion score Z<sub>yi</sub>")
    
    col2 <- c(formatC(store$nayShiCalc$nayShiSR, format = "f", digits = 4, big.mark = "," , drop0trailing = FALSE),
            formatC(store$nayShiCalc$nayShiZxi, format = "f", digits = 4, big.mark = "," , drop0trailing = FALSE),
            formatC(store$nayShiCalc$nayShiLambda, format = "f", digits = 4, big.mark = "," , drop0trailing = FALSE),
            formatC(store$nayShiCalc$nayShiZyi, format = "f", digits = 4, big.mark = "," , drop0trailing = FALSE))
    
    #check if all the calculations result in a valid value and render appropriate results column
    if (all(as.logical(lapply(req(store$nayShiCalc), is.finite)))){
  
      return(cbind(col1, col2))
  
    } else {
    
      return(cbind(col1, rep("-", 4)))
      
    }
    
  })
  
  #naylor-shine calculations
  nayShiCalc <- function() {
    
    print("nayShiCalc")
    
    #check which calculation type is selected
    if (req(input$nayShiCalcSel) == 1) {
      
      #load inputs
      rxy <- data$nayShiVal
      SR <- data$nayShiSR
      
      #calculate naylor-shine utility
      #get z-score cutoff corresponding to selection ratio
      Zsr <- qnorm(1-SR)
  
      #use dnorm to get the height of the normal ordinate
      Lambda <- dnorm(Zsr,0,1)
      
      #calculate mean criterion (Z score)
      Zyi <- rxy* Lambda/SR
      
      #store output
      store$nayShiCalc$nayShiZxi <- Zsr
      store$nayShiCalc$nayShiLambda <- Lambda
      store$nayShiCalc$nayShiZyi <- Zyi
      
    } else {

      if (req(input$nayShiCalcSel) == 2) {
        
        #store results
        rxy <- data$nayShiVal
        Zsr <- data$nayShiZxi
  
        #calculate naylor-shine utility
        #get selection ratio corresponding to z-score cutoff 
        SR <- 1 - pnorm(Zsr)
    
        #use dnorm to get the height of the normal ordinate
        Lambda <- dnorm(Zsr,0,1)
        
        #caculate Lambda
        Zyi <- rxy* Lambda/SR
      
        #store output
        store$nayShiCalc$nayShiSR <- SR
        store$nayShiCalc$nayShiLambda <- Lambda
        store$nayShiCalc$nayShiZyi <- Zyi
        
      } else {
        
        #store results
        rxy <- data$nayShiVal
        Zyi <- data$nayShiZyi
        
        #recover selection ratio (SR)
        #given mean performance (Zyi) and validity (r)
        rat <- Zyi/rxy # ratio needed 
        
        #find SR given rat 
        Zs <- seq(-3,3,.01) #possible Z on X
        a <- dnorm(Zs,0,1)  #corresponding normal ordinates of X
        b <- pnorm(-Zs)     #corresponding selection ratios of X
        z <- a/b            #ratio of a ot b
        dif <- rat - z      #the ratio z should equal the ratio in line 10
                            #meaning the difference should be near-zero
        ind <- which(dif==min(abs(dif))) #find where the smallest difference is...
        Zsr <- Zs[ind]   #z-score cutoff corresponding to selection ratio
        SR <- pnorm(-Zs[ind]) #corresponding selection ratio
        Lambda <- dnorm(Zsr,0,1) #height of the normal ordinate
        
        #store output
        store$nayShiCalc$nayShiSR <- SR
        store$nayShiCalc$nayShiZxi <- Zsr
        store$nayShiCalc$nayShiLambda <- Lambda
        
      }
      
    }
   
  }
  
  #show the values in a table
  output$nayShiTab <- renderTable({
    
    req(nayShiData())
    
  }, align = 'lr', sanitize.text.function = function(x) x)
  
  #brogden-cronbach-gleser tab#
  
  #brogden-cronbach-gleser UI
  output$bCGUI<- renderUI({
    
    print("bCGUI")
    
    fluidRow(
      
      column(12,
        
        "Note: Currency is set to ",data$currency,
        
        br(),

        br(),
        
        fluidRow(
          
          column(6,
            
            " "
            
          ),
          
          column(3,
            
            "Existing procedure"
            
          ),
          
          column(3,
          
            "Replacement procedure"
            
          )
          
        ),
        
        fluidRow(
          
          br(),
          
          column(6,
            
            "Quota for selection:"
            
          ),
          
          column(3,
            
            textInput("bCGSelQuota1", label = NULL, isolate(data$bCGSelQuota1))
            
          ),
          
          column(3,
          
            textInput("bCGSelQuota2", label = NULL, isolate(data$bCGSelQuota2)) 
            
          )
          
        ),
        
        fluidRow(
          
          column(6,
            
            "Selection ratio:"
            
          ),
          
          column(3,
            
            textInput("bCGSR1", label = NULL, isolate(data$bCGSR1))
            
          ),
          
          column(3,
          
            textInput("bCGSR2", label = NULL, isolate(data$bCGSR2)) 
            
          )
          
        ),
        
        fluidRow(
          
          column(6,
            
            "Standard deviation of job performance:"
            
          ),
          
          column(3,
            
            textInput("bCGSDy1", label = NULL, paste0(currSym[data$currency],isolate(data$bCGSDy1)))
            
          ),
          
          column(3,
          
            textInput("bCGSDy2", label = NULL, paste0(currSym[data$currency],isolate(data$bCGSDy2)))
            
          )
          
        ),
        
        fluidRow(
          
          column(6,
            
            HTML("Validity of the selection procedure (r<sub>xy</sub>):")
            
          ),
          
          column(3,
            
            textInput("bCGVal1", label = NULL, isolate(data$bCGVal1))
            
          ),
          
          column(3,
          
            textInput("bCGVal2", label = NULL, isolate(data$bCGVal2)) 
            
          )
          
        ),
        
        fluidRow(
          
          column(6,
            
            "Cost of testing one person:"
            
          ),
          
          column(3,
            
            textInput("bCGTestCostPer1", label = NULL, paste0(currSym[data$currency],isolate(data$bCGTestCostPer1)))
            
          ),
          
          column(3,
          
            textInput("bCGTestCostPer2", label = NULL, paste0(currSym[data$currency],isolate(data$bCGTestCostPer2)))
            
          )
          
        )
        
      )
      
    )
    
  })

  #brogden-cronbach-gleser inputs
  bCGData <- reactive({
    
    print("bCGData")
    
    #convert text inputs to numbers and strip symbols
    #brogden-cronbach-gleser utility inputs
    data$bCGSelQuota1 <- as.numeric(input$bCGSelQuota1)
    data$bCGSelQuota2 <- as.numeric(input$bCGSelQuota2)
    data$bCGSR1 <- as.numeric(input$bCGSR1)
    data$bCGSR2 <- as.numeric(input$bCGSR2)
    data$bCGSDy1 <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$bCGSDy1)))
    data$bCGSDy2 <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$bCGSDy2)))
    data$bCGVal1 <- as.numeric(input$bCGVal1)
    data$bCGVal2 <- as.numeric(input$bCGVal2)
    data$bCGTestCostPer1 <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$bCGTestCostPer1)))
    data$bCGTestCostPer2 <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$bCGTestCostPer2)))

    #calculations
    bCGCalc()
    
    #output a results table
    col1 <- c("1. The number recruited (N)", 
            "2. Net gain over random selection (overall)",
            "3. Net gain over random selection (per selectee)")
    
    col2 <- c(formatC(store$bCGCalc$bCGNumRec1, format = "f", digits = 0, big.mark = "," , drop0trailing = FALSE),
            paste0(currSymHTML[data$currency], formatC(store$bCGCalc$bCGNetGain1, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)),
            paste0(currSymHTML[data$currency], formatC(store$bCGCalc$bCGNetGainPer1, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)))
    
    col3 <- c(formatC(store$bCGCalc$bCGNumRec2, format = "f", digits = 0, big.mark = "," , drop0trailing = FALSE),
            paste0(currSymHTML[data$currency], formatC(store$bCGCalc$bCGNetGain2, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)),
            paste0(currSymHTML[data$currency], formatC(store$bCGCalc$bCGNetGainPer2, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)))
    
    col4 <- c(formatC(store$bCGCalc$bCGNumRecDiff, format = "f", digits = 0, big.mark = "," , drop0trailing = FALSE),
            paste0(currSymHTML[data$currency], formatC(store$bCGCalc$bCGNetGainDiff, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)),
            paste0(currSymHTML[data$currency], formatC(store$bCGCalc$bCGNetGainDiffPer, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)))
    
    #check if all the calculations result in a valid value and render appropriate results column
    if (all(as.logical(lapply(req(store$bCGCalc), is.finite)))){
  
      tab <- cbind(col1, col2, col3, col4)
      
      colnames(tab) <- c(" ","Procedure 1","Procedure 2", "Difference")
      
      return(tab)
  
    } else {
    
      tab <- cbind(col1, rep("-", 3), rep("-", 3), rep("-", 3))
      
      colnames(tab) <- c(" ","Procedure 1","Procedure 2", "Difference")
      
      return(tab)
      
    }
    
  })

  #brogden-cronbach-gleser calculations
  bCGCalc <- function() {
    
    print("bCGCalc")
    
    bcg <- function(SR,rxy,SDy,Cy){
      
      #get z-score cutoffs corresponding to selection ratio and base rate
      Zsr <- qnorm(1-SR)
          
      #use dnorm to get the height of the normal ordinate
      Zxi <- dnorm(Zsr,0,1)/SR
      
      #Brogden-Cronbach-Gleser
      U <- rxy*Zxi*SDy - Cy/SR
      
      return(U)
      
    }
    
    store$bCGCalc$bCGNumRec1 <- round(data$bCGSelQuota1/data$bCGSR1, 0)
    store$bCGCalc$bCGNetGainPer1 <- bcg(data$bCGSR1,data$bCGVal1,data$bCGSDy1,data$bCGTestCostPer1)
    store$bCGCalc$bCGNetGain1 <- store$bCGCalc$bCGNetGainPer1*data$bCGSelQuota1
    store$bCGCalc$bCGNumRec2 <- round(data$bCGSelQuota2/data$bCGSR2, 0)
    store$bCGCalc$bCGNetGainPer2 <- bcg(data$bCGSR2,data$bCGVal2,data$bCGSDy2,data$bCGTestCostPer2)
    store$bCGCalc$bCGNetGain2 <- store$bCGCalc$bCGNetGainPer2*data$bCGSelQuota2
    store$bCGCalc$bCGNumRecDiff <- store$bCGCalc$bCGNumRec2 - store$bCGCalc$bCGNumRec1
    store$bCGCalc$bCGNetGainDiff <- store$bCGCalc$bCGNetGain2 - store$bCGCalc$bCGNetGain1
    store$bCGCalc$bCGNetGainDiffPer <- store$bCGCalc$bCGNetGainPer2 - store$bCGCalc$bCGNetGainPer1

  }

  #show the values in a table
  output$bCGTab <- renderTable({
    
    req(bCGData())
    
  }, align = 'lrrr', sanitize.text.function = function(x) x)
  
  ###enhanced selection###
  
  #first enhanced selection UI
  output$eBCGUI1 <- renderUI({
    
    print("eBCGUI1")
    
    fluidRow(
      
      column(12,
        
        "Note: Currency is set to ",data$currency,
        
        br(),

        br(),
        
        fluidRow(
          
          column(6,
            
            " "
            
          ),
          
          column(3,
            
            "Existing procedure"
            
          ),
          
          column(3,
          
            "Replacement procedure"
            
          )
          
        ),
        
        fluidRow(
         
          br(),
          
          column(6,
            
            HTML("Validity of the procedure:")
            
          ),
          
          column(3,
            
            textInput("eBCGVal1", label = NULL, isolate(data$eBCGVal1))
            
          ),
          
          column(3,
          
            textInput("eBCGVal2", label = NULL, isolate(data$eBCGVal2)) 
            
          )
          
        ),
        
        fluidRow(
          
          column(6,
            
            "Cost per applicant of the procedure:"
            
          ),
          
          column(3,
            
            textInput("eBCGTestCostPer1", label = NULL, paste0(currSym[data$currency],isolate(data$eBCGTestCostPer1)))
            
          ),
          
          column(3,
          
            textInput("eBCGTestCostPer2", label = NULL, paste0(currSym[data$currency],isolate(data$eBCGTestCostPer2)))
            
          )
          
        ),
        
        fluidRow(
          
          column(6,
            
            "Selection ratio:"
            
          ),
          
          column(3,
            
            textInput("eBCGSR1", label = NULL, isolate(data$eBCGSR1))
            
          ),
          
          column(3,
          
            textInput("eBCGSR2", label = NULL, isolate(data$eBCGSR2)) 
            
          )
          
        ),
        
        fluidRow(
          
          column(6,
            
            "The tenure in years of the average selectee:"
            
          ),
          
          column(3,
            
            textInput("eBCGAveTenure1", label = NULL, isolate(data$eBCGAveTenure1))
            
          ),
          
          column(3,
          
            textInput("eBCGAveTenure2", label = NULL, isolate(data$eBCGAveTenure2)) 
            
          )
          
        ),
        
        fluidRow(
          
          hr(),
          
          column(9,
            
            " "
            
          ),
          
          column(3,
            
            "Both procedures"
            
          )
          
        ),

        fluidRow(
          
          br(),
          
          column(9,
            
            "Standard deviation of job performance:"
            
          ),
          
          column(3,
            
            textInput("eBCGSDy", label = NULL, paste0(currSym[data$currency],isolate(data$eBCGSDy)))
            
          )
          
        ),
        
        fluidRow(
          
          column(9,
            
            "The number selected in a given year:"
            
          ),
          
          column(3,
            
            textInput("eBCGNumSelInYear", label = NULL, isolate(data$eBCGNumSelInYear))
            
          )
          
        ),
        
        fluidRow(
          
          column(9,
            
            "Account for economic factors?"
            
          ),
          
          column(3,
            
            div(style = "width:80px",

              selectInput("eBCGSelEcFac", label = NULL, c("Yes", "No"), isolate(data$eBCGSelEcFac))
            
            )
            
          )
          
        )
        
      )
      
    )
    
  })
        
  #second enhanced brogden-cronbach-gleser UI
  output$eBCGUI2 <- renderUI({
    
    print("eBCGUI2")
    
    #check if economic factors are accounted for
    if (req(input$eBCGSelEcFac) == "Yes") {
    
      #inputs if "Yes" is selected
      tagList(
    
        fluidRow(
          
          column(12,
            
            fluidRow(
              
              column(9,
                
                HTML("Variable costs (enter as a negative proportion):")
                
              ),
              
              column(3,
                
                textInput("eBCGVarCosts", label = NULL, isolate(data$eBCGVarCosts))
                
              )
              
            ),
            
            fluidRow(
              
              column(9,
                
                "Taxes (percentage):"
                
              ),
              
              column(3,
    
                textInput("eBCGTax", label = NULL, paste0(isolate(data$eBCGTax),"%"))
                
              )
              
            ),
            
            fluidRow(
              
              column(9,
                
                HTML("Discount rate (percentage):")
                
              ),
              
              column(3,
    
                textInput("eBCGDiscRate", label = NULL, paste0(isolate(data$eBCGDiscRate),"%"))
                
              )
              
            )
            
          )
          
        )
        
      )
      
    } else {
      
      #no inputs if "No" is selected
      return(NULL)     
      
    }
    
  })
          
  #third enhanced brogden-cronbach-gleser UI
  output$eBCGUI3 <- renderUI({
    
    print("eBCGUI3")
    
    fluidRow(
      
      column(12,                  
        
        fluidRow(
          
          column(9,
            
            "Account for mulitple cohorts?"
            
          ),
          
          column(3,
            
            div(style = "width:80px",
              
              selectInput("eBCGSelMultCoh", label = NULL, c("Yes", "No"), isolate(data$eBCGSelMultCoh))
            
            )
            
          )
          
        )
        
      )
      
    )
    
  })        
        
  #fourth enhanced brogden-cronbach-gleser UI
  output$eBCGUI4 <- renderUI({
    
    print("eBCGUI4")
    
    #check if multiple cohorts are accounted for
    if (req(input$eBCGSelMultCoh) == "Yes") {
    
      #inputs if "Yes" is selected
      tagList(
    
        fluidRow(
          
          column(12,        
            
            fluidRow(
              
              column(9,
                
                "Program designed to last (years):"
                
              ),
              
              column(3,
                
                textInput("eBCGProgYears", label = NULL, isolate(data$eBCGProgYears))
                
              )
              
            ),
            
            fluidRow(
              
              column(9,
                
                "Number of employees added each year:"
                
              ),
              
              column(3,
                
                textInput("eBCGNumEmpsAddPerYear", label = NULL, isolate(data$eBCGNumEmpsAddPerYear))
                
              )
              
            ),
            
            fluidRow(
              
              column(9,
                
                "Number of employees subtracted each year (after average tenure):"
                
              ),
              
              column(3,
                
                textInput("eBCGNumEmpsSubPerYear", label = NULL, isolate(data$eBCGNumEmpsSubPerYear))
                
              )
              
            )
    
          )
          
        )
        
      )
      
    } else {
      
      #no inputs if "No" is selected
      return(NULL)       
      
    }
    
  })

  #enhanced selection costs inputs
  eBCGData <- reactive({
    
    print("eBCGData")
    
    #convert text inputs to numbers and strip symbols
    #enhanced BCG inputs
    data$eBCGVal1 <- as.numeric(input$eBCGVal1)
    data$eBCGVal2 <- as.numeric(input$eBCGVal2)
    data$eBCGTestCostPer1 <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$eBCGTestCostPer1)))
    data$eBCGTestCostPer2 <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$eBCGTestCostPer2)))
    data$eBCGSR1 <- as.numeric(input$eBCGSR1)
    data$eBCGSR2 <- as.numeric(input$eBCGSR2)
    data$eBCGAveTenure1 <- as.numeric(input$eBCGAveTenure1)
    data$eBCGAveTenure2 <- as.numeric(input$eBCGAveTenure2)
    data$eBCGSDy <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$eBCGSDy)))
    data$eBCGNumSelInYear <- as.numeric(input$eBCGNumSelInYear)

    #account for economic factors inputs
    if (req(input$eBCGSelEcFac) == "Yes") {

      #update settings
      data$eBCGSelEcFac <- "Yes"

      #inputs if "Yes" selected
      data$eBCGVarCosts <- as.numeric(input$eBCGVarCosts)
      data$eBCGTax <- as.numeric(gsub("[%]", "", req(input$eBCGTax)))
      data$eBCGDiscRate <- as.numeric(gsub("[%]", "", req(input$eBCGDiscRate)))

    } else {

      #update settings
      data$eBCGSelEcFac <- "No"

      #no inputs if "No" selected

    }

    #account for economic factors inputs
    if (req(input$eBCGSelMultCoh) == "Yes") {

      #update settings
      data$eBCGSelMultCoh <- "Yes"

      #inputs if "Yes" selected
      data$eBCGProgYears <- as.numeric(req(input$eBCGProgYears))
      data$eBCGNumEmpsAddPerYear <- as.numeric(req(input$eBCGNumEmpsAddPerYear))
      data$eBCGNumEmpsSubPerYear <- as.numeric(req(input$eBCGNumEmpsSubPerYear))

    } else {

      #update settings
      data$eBCGSelMultCoh <- "No"

      #no inputs if "No" selected

    }

    #calculations
    eBCGCalc()

    #output a results table
    col1 <- c("1. Total utility",
            "2. Total utility per selectee",
            "3. The per-year gain in utility per selectee")

    col2 <- c(paste0(currSymHTML[data$currency], formatC(store$eBCGCalc$eBCGTotUt, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)),
            paste0(currSymHTML[data$currency], formatC(store$eBCGCalc$eBCGTotUtPer, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)),
            paste0(currSymHTML[data$currency], formatC(store$eBCGCalc$eBCGTotUtPerPerYr, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)))

    #check if all the calculations result in a valid value and render appropriate results column
    if (!any(as.logical(store$eBCGCalc) == logical(0))){
      
      if (all(as.logical(lapply(req(store$eBCGCalc), is.finite)))){
    
        return(cbind(col1, col2))
    
      } else {
      
        return(cbind(col1, rep("-", 3)))
        
      }
      
    } else {
      
      return(cbind(col1, rep("-", 3)))
      
    }

  })
  
  #enhanced selection calculations
  eBCGCalc <- function() {
    
    print("eBCGCalc")
    
    #calculate enhanced selection utility
    if (req(input$eBCGSelMultCoh) == "No") {
      
    #single cohort
    tenure1 <- data$eBCGAveTenure1 
    tenure2 <- data$eBCGAveTenure2
    numSel <- data$eBCGNumSelInYear
    oldVal <- data$eBCGVal1
    newVal <- data$eBCGVal2
    oldCost <- data$eBCGTestCostPer1    
    newCost <- data$eBCGTestCostPer2
    SDjp <- data$eBCGSDy
    sr1  <- data$eBCGSR1
    
    ordsr1 <- dnorm(qnorm(1-sr1),0,1)
      
    #compare procedure 1 and procedure 2
    varCosts <- data$eBCGVarCosts
    tax <- data$eBCGTax
    disc <- data$eBCGDiscRate
    
    if (req(input$eBCGSelEcFac) == "No") {
      
      varCosts <- 0
      tax <- 0
      disc <- 0
      
    }
    
    tenureLow <- floor(tenure1)
    tenureHigh <- tenureLow + 1
    
    totDelta <- 0
    sumone <- 0
    sumtwo <- 0
    
    for (i in 1:tenureLow) {
      sumone <- sumone + 1 / ((1+disc / 100)^i)
    }
    
    for (i in 1:tenureHigh) {
      sumtwo <- sumtwo + 1 / ((1+disc / 100)^i)
    }
    
    sumDiff <- sumtwo - sumone
    mult <- tenureHigh - tenureLow
    sumthree <- sumone + sumDiff * mult
    
    delta <-  numSel * sumthree * SDjp * (1 + varCosts) * (1 - tax / 100) * (newVal - oldVal) * (ordsr1 / sr1) - (newCost - oldCost) * (numSel / sr1) * (1 - tax / 100)
    
    #store results
    store$eBCGCalc$eBCGTotUt <- delta
    store$eBCGCalc$eBCGTotUtPer <- delta / numSel
    store$eBCGCalc$eBCGTotUtPerPerYr <- (delta / numSel) / tenure1
    
    } #end single cohorts section
    
    if (req(input$eBCGSelMultCoh) == "Yes") {
      
    #multiple cohorts
    varCosts <- data$eBCGVarCosts
    tax <- data$eBCGTax
    disc <- data$eBCGDiscRate
    
    if (req(input$eBCGSelEcFac) == "No") {
      
      varCosts <- 0
      tax <- 0
      disc <- 0
      
    }
    
    costOrd <- data$eBCGTestCostPer1 
    costAc <- data$eBCGTestCostPer2
    validOrd <- data$eBCGVal1
    validAc <- data$eBCGVal2
    SDjp <- data$eBCGSDy
    sr1  <- data$eBCGSR1
    ordsr1 <- dnorm(qnorm(1-sr1),0,1)
    tenure1 <- data$eBCGAveTenure1
    tenure2 <- data$eBCGAveTenure2
    
    last <- data$eBCGProgYears
    add <- data$eBCGNumEmpsAddPerYear
    subt <- data$eBCGNumEmpsSubPerYear
    
    nk <- 0
    paytot <- 0
    paysel <- 0
    payselfir <- 0
    
    discProp <- disc / 100
    
    valid <- validAc - validOrd
    
    ord <- ordsr1 / sr1
    ck <- add * ((costAc - costOrd) / sr1)
    taxProp <- tax / 100
    discRat <- 1 / (1 + discProp)
      
    numyr <- tenure1 + last
    
    totDelta <- 0
    
    for (i in 1:numyr) {
      if (i > tenure1) {nk <- nk - subt}
      if (i <= last) {nk <- nk + add}
      if (i > last) {ck <- 0}
      if (nk >= 0) {
        delta1 <- nk * ((discRat^i) * valid * ord * SDjp * (1 + varCosts) * (1 - taxProp))
      }
      delta2 <- ck * (1 - taxProp) * (discRat^(i - 1))
      delta <- delta1 - delta2
      totDelta <- totDelta + delta
    }
    
    #store results
    store$eBCGCalc$eBCGTotUt <- totDelta
    store$eBCGCalc$eBCGTotUtPer <- totDelta / (last * add)
    store$eBCGCalc$eBCGTotUtPerPerYr <- (totDelta / (last * add)) / numyr
    
    }
   
  }
  
  #enhanced selection calculations
  eBCGCalcGraph <- function() {
    
    print("eBCGCalcGraph")
    
    #calculate enhanced selection utility
      
    #single cohort with economic factors
    tenure1 <- data$eBCGAveTenure1 
    tenure2 <- data$eBCGAveTenure2
    numSel <- data$eBCGNumSelInYear
    oldVal <- data$eBCGVal1
    newVal <- data$eBCGVal2
    oldCost <- data$eBCGTestCostPer1    
    newCost <- data$eBCGTestCostPer2
    SDjp <- data$eBCGSDy
    sr1  <- data$eBCGSR1
    
    ordsr1 <- dnorm(qnorm(1-sr1),0,1)
    
    #economic factors
    varCosts <- data$eBCGVarCosts
    tax <- data$eBCGTax
    disc <- data$eBCGDiscRate

    tenureLow <- floor(tenure1)
    tenureHigh <- tenureLow + 1
    
    totDelta <- 0
    sumone <- 0
    sumtwo <- 0
    
    for (i in 1:tenureLow) {
      sumone <- sumone + 1 / ((1+disc / 100)^i)
    }
    
    for (i in 1:tenureHigh) {
      sumtwo <- sumtwo + 1 / ((1+disc / 100)^i)
    }
    
    sumDiff <- sumtwo - sumone
    mult <- tenureHigh - tenureLow
    sumthree <- sumone + sumDiff * mult
    
    delta <-  numSel * sumthree * SDjp * (1 + varCosts) * (1 - tax / 100) * (newVal - oldVal) * (ordsr1 / sr1) - (newCost - oldCost) * (numSel / sr1) * (1 - tax / 100)
    
    #store results
    store$eBCGCalcGraph$eBCGTotUt1 <- delta
    store$eBCGCalcGraph$eBCGTotUtPerSingle1 <- delta / numSel
    store$eBCGCalcGraph$eBCGTotUtPerPerYrSingle1 <- (delta / numSel) / tenure1
    
    #single cohort without economic factors
    tenure1 <- data$eBCGAveTenure1 
    tenure2 <- data$eBCGAveTenure2
    numSel <- data$eBCGNumSelInYear
    oldVal <- data$eBCGVal1
    newVal <- data$eBCGVal2
    oldCost <- data$eBCGTestCostPer1    
    newCost <- data$eBCGTestCostPer2
    SDjp <- data$eBCGSDy
    sr1  <- data$eBCGSR1
    
    ordsr1 <- dnorm(qnorm(1-sr1),0,1)
    
    #no economic factors
    varCosts <- 0
    tax <- 0
    disc <- 0

    tenureLow <- floor(tenure1)
    tenureHigh <- tenureLow + 1
    
    totDelta <- 0
    sumone <- 0
    sumtwo <- 0
    
    for (i in 1:tenureLow) {
      sumone <- sumone + 1 / ((1+disc / 100)^i)
    }
    
    for (i in 1:tenureHigh) {
      sumtwo <- sumtwo + 1 / ((1+disc / 100)^i)
    }
    
    sumDiff <- sumtwo - sumone
    mult <- tenureHigh - tenureLow
    sumthree <- sumone + sumDiff * mult
    
    delta <-  numSel * sumthree * SDjp * (1 + varCosts) * (1 - tax / 100) * (newVal - oldVal) * (ordsr1 / sr1) - (newCost - oldCost) * (numSel / sr1) * (1 - tax / 100)
    
    #store results
    store$eBCGCalcGraph$eBCGTotUt0 <- delta
    store$eBCGCalcGraph$eBCGTotUtPerSingle0 <- delta / numSel
    store$eBCGCalcGraph$eBCGTotUtPerPerYrSingle0 <- (delta / numSel) / tenure1


    #multiple cohorts with economic factors
    varCosts <- data$eBCGVarCosts
    tax <- data$eBCGTax
    disc <- data$eBCGDiscRate
    
    costOrd <- data$eBCGTestCostPer1 
    costAc <- data$eBCGTestCostPer2
    validOrd <- data$eBCGVal1
    validAc <- data$eBCGVal2
    SDjp <- data$eBCGSDy
    sr1  <- data$eBCGSR1
    ordsr1 <- dnorm(qnorm(1-sr1),0,1)
    tenure1 <- data$eBCGAveTenure1
    tenure2 <- data$eBCGAveTenure2
    
    last <- data$eBCGProgYears
    add <- data$eBCGNumEmpsAddPerYear
    subt <- data$eBCGNumEmpsSubPerYear
    
    nk <- 0
    paytot <- 0
    paysel <- 0
    payselfir <- 0
    
    discProp <- disc / 100
    
    valid <- validAc - validOrd
    
    ord <- ordsr1 / sr1
    ck <- add * ((costAc - costOrd) / sr1)
    taxProp <- tax / 100
    discRat <- 1 / (1 + discProp)
      
    numyr <- tenure1 + last
    
    totDelta <- 0
    
    for (i in 1:numyr) {
      if (i > tenure1) {nk <- nk - subt}
      if (i <= last) {nk <- nk + add}
      if (i > last) {ck <- 0}
      if (nk >= 0) {
        delta1 <- nk * ((discRat^i) * valid * ord * SDjp * (1 + varCosts) * (1 - taxProp))
      }
      delta2 <- ck * (1 - taxProp) * (discRat^(i - 1))
      delta <- delta1 - delta2
      totDelta <- totDelta + delta
    }
    
    #store results
    store$eBCGCalcGraph$eBCGTotUt3 <- totDelta
    store$eBCGCalcGraph$eBCGTotUtPer3 <- totDelta / (last * add)
    store$eBCGCalcGraph$eBCGTotUtPerPerYr3 <- (totDelta / (last * add)) / numyr
    
    #multiple cohorts without economic factors
    varCosts <- 0
    tax <- 0
    disc <- 0
    
    costOrd <- data$eBCGTestCostPer1 
    costAc <- data$eBCGTestCostPer2
    validOrd <- data$eBCGVal1
    validAc <- data$eBCGVal2
    SDjp <- data$eBCGSDy
    sr1  <- data$eBCGSR1
    ordsr1 <- dnorm(qnorm(1-sr1),0,1)
    tenure1 <- data$eBCGAveTenure1
    tenure2 <- data$eBCGAveTenure2
    
    last <- data$eBCGProgYears
    add <- data$eBCGNumEmpsAddPerYear
    subt <- data$eBCGNumEmpsSubPerYear
    
    nk <- 0
    paytot <- 0
    paysel <- 0
    payselfir <- 0
    
    discProp <- disc / 100
    
    valid <- validAc - validOrd
    
    ord <- ordsr1 / sr1
    ck <- add * ((costAc - costOrd) / sr1)
    taxProp <- tax / 100
    discRat <- 1 / (1 + discProp)
      
    numyr <- tenure1 + last
    
    totDelta <- 0
    
    for (i in 1:numyr) {
      if (i > tenure1) {nk <- nk - subt}
      if (i <= last) {nk <- nk + add}
      if (i > last) {ck <- 0}
      if (nk >= 0) {
        delta1 <- nk * ((discRat^i) * valid * ord * SDjp * (1 + varCosts) * (1 - taxProp))
      }
      delta2 <- ck * (1 - taxProp) * (discRat^(i - 1))
      delta <- delta1 - delta2
      totDelta <- totDelta + delta
    }
    
    #store results
    store$eBCGCalcGraph$eBCGTotUt2 <- totDelta
    store$eBCGCalcGraph$eBCGTotUtPer2 <- totDelta / (last * add)
    store$eBCGCalcGraph$eBCGTotUtPerPerYr2 <- (totDelta / (last * add)) / numyr
   
  }
  
  #eBCGPlot
  output$eBCGPlot <- renderPlot({
    
    eBCGCalcGraph()

    if (req(input$eBCGSelMultCoh) == "No" && req(input$eBCGSelEcFac) == "Yes") {
      
      dat <- c(store$eBCGCalcGraph$eBCGTotUt0,store$eBCGCalcGraph$eBCGTotUt1,0,0)
      label <- c("No Corrections","Economic Factors","Multiple Cohorts","Both")
      
    } else {
      
      if (req(input$eBCGSelMultCoh) == "Yes" && req(input$eBCGSelEcFac) == "No") {
        
        dat <- c(store$eBCGCalcGraph$eBCGTotUt0,0,store$eBCGCalcGraph$eBCGTotUt2,0)
        label <- c("No Corrections","Economic Factors","Multiple Cohorts","Both")
        
      } else {
        
        if (req(input$eBCGSelMultCoh) == "Yes" && req(input$eBCGSelEcFac) == "Yes") {

          dat <- c(store$eBCGCalcGraph$eBCGTotUt0,store$eBCGCalcGraph$eBCGTotUt1,store$eBCGCalcGraph$eBCGTotUt2,store$eBCGCalcGraph$eBCGTotUt3)
          label <- c("No Corrections","Economic Factors","Multiple Cohorts","Both") 
          
        } else {
          
          dat <- c(store$eBCGCalcGraph$eBCGTotUt0,0,0,0)
          label <- c("No Corrections","Economic Factors","Multiple Cohorts","Both")
          
        }
        
      }
      
    }
    
    if (min(dat)<0 && max(dat)<=0) {
      lo=0
      hi=1.25*min(dat)
    }
    
    if (min(dat)<0 && max(dat)>0) {
      lo=1.25*min(dat)
      hi=1.25*max(dat)
    }

    if (min(dat)>=0) {
      lo=0
      hi=1.25*max(dat)
    }
    
    barplot(dat,names.arg=label,ylim=c(lo,hi),xlab="Corrections",ylab=paste0("Utility (",data$currency,")"),col="blue",main="Comparison of Total Utility Estimates")
    
  })
  
  #show the values in a table
  output$eBCGTab <- renderTable({
    
    req(eBCGData())
    
  }, align = 'lr', sanitize.text.function = function(x) x)
  
  ###performance###
  
  #CREPID tab
  
  #CREPID UI
  output$CREPIDUI <- renderUI({
    
    print("CREPIDUI")

    tagList({
      
      fluidRow(
        
        column(12,

          fluidRow(

            column(12,
              
              "Note: Currency is set to ",data$currency,
              
              br()
              
            )
            
          ),
            
          fluidRow(
            
            br(),

            column(3,
              
              "Principal activity"
              
            ),
            
            column(3,
              
              "Time/frequency"
              
            ),            
            
            column(3,
              
              "Importance"
              
            ),
            
            column(3,
              
              "Performance rating"
              
            )

          ),
          
          fluidRow(
            
            br(),

            column(3,
              
              "1"
              
            ),
            
            column(3,
              
              textInput("CREPIDTF1", label = NULL, isolate(data$CREPIDTF1))
              
            ),            
            
            column(3,
              
              textInput("CREPIDImp1", label = NULL, isolate(data$CREPIDImp1))
              
            ),
            
            column(3,
              
              textInput("CREPIDPerf1", label = NULL, isolate(data$CREPIDPerf1))
              
            )

          ),
          
          fluidRow(

            column(3,
              
              "2"
              
            ),
            
            column(3,
              
              textInput("CREPIDTF2", label = NULL, isolate(data$CREPIDTF2))
              
            ),            
            
            column(3,
              
              textInput("CREPIDImp2", label = NULL, isolate(data$CREPIDImp2))
              
            ),
            
            column(3,
              
              textInput("CREPIDPerf2", label = NULL, isolate(data$CREPIDPerf2))
              
            )

          ),
          
          fluidRow(

            column(3,
              
              "3"
              
            ),
            
            column(3,
              
              textInput("CREPIDTF3", label = NULL, isolate(data$CREPIDTF3))
              
            ),            
            
            column(3,
              
              textInput("CREPIDImp3", label = NULL, isolate(data$CREPIDImp3))
              
            ),
            
            column(3,
              
              textInput("CREPIDPerf3", label = NULL, isolate(data$CREPIDPerf3))
              
            )

          ),
          
          fluidRow(

            column(3,
              
              "4"
              
            ),
            
            column(3,
              
              textInput("CREPIDTF4", label = NULL, isolate(data$CREPIDTF4))
              
            ),            
            
            column(3,
              
              textInput("CREPIDImp4", label = NULL, isolate(data$CREPIDImp4))
              
            ),
            
            column(3,
              
              textInput("CREPIDPerf4", label = NULL, isolate(data$CREPIDPerf4))
              
            )

          ),
          
          fluidRow(

            column(3,
              
              "5"
              
            ),
            
            column(3,
              
              textInput("CREPIDTF5", label = NULL, isolate(data$CREPIDTF5))
              
            ),            
            
            column(3,
              
              textInput("CREPIDImp5", label = NULL, isolate(data$CREPIDImp5))
              
            ),
            
            column(3,
              
              textInput("CREPIDPerf5", label = NULL, isolate(data$CREPIDPerf5))
              
            )

          ),
          
          fluidRow(

            column(3,
              
              "6"
              
            ),
            
            column(3,
              
              textInput("CREPIDTF6", label = NULL, isolate(data$CREPIDTF6))
              
            ),            
            
            column(3,
              
              textInput("CREPIDImp6", label = NULL, isolate(data$CREPIDImp6))
              
            ),
            
            column(3,
              
              textInput("CREPIDPerf6", label = NULL, isolate(data$CREPIDPerf6))
              
            )

          ),
          
          fluidRow(

            column(3,
              
              "7"
              
            ),
            
            column(3,
              
              textInput("CREPIDTF7", label = NULL, isolate(data$CREPIDTF7))
              
            ),            
            
            column(3,
              
              textInput("CREPIDImp7", label = NULL, isolate(data$CREPIDImp7))
              
            ),
            
            column(3,
              
              textInput("CREPIDPerf7", label = NULL, isolate(data$CREPIDPerf7))
              
            )

          ),
          
          fluidRow(

            column(3,
              
              "8"
              
            ),
            
            column(3,
              
              textInput("CREPIDTF8", label = NULL, isolate(data$CREPIDTF8))
              
            ),            
            
            column(3,
              
              textInput("CREPIDImp8", label = NULL, isolate(data$CREPIDImp8))
              
            ),
            
            column(3,
              
              textInput("CREPIDPerf8", label = NULL, isolate(data$CREPIDPerf8))
              
            )
            
          ),
          fluidRow(
            
            br(),
            
            br(),

            column(3,
              
              ""
              
            ),
            
            column(3,
              
              ""
              
            ),            
            
            column(3,
              
              "Annual salary:"
              
            ),
            
            column(3,
              
              textInput("CREPIDAnnSalary", label = NULL, paste0(currSym[data$currency],isolate(data$CREPIDAnnSalary)))
              
            )
            
          )
        
        )
        
      )

    })

  })
  
  #CREPID inputs
  CREPIDData <- reactive({
    
    print("CREPIDData")
    
    #convert text inputs to numbers and strip symbols
    #CREPID inputs
    data$CREPIDTF1 <- as.numeric(input$CREPIDTF1)
    data$CREPIDTF2 <- as.numeric(input$CREPIDTF2)
    data$CREPIDTF3 <- as.numeric(input$CREPIDTF3)
    data$CREPIDTF4 <- as.numeric(input$CREPIDTF4)
    data$CREPIDTF5 <- as.numeric(input$CREPIDTF5)
    data$CREPIDTF6 <- as.numeric(input$CREPIDTF6)
    data$CREPIDTF7 <- as.numeric(input$CREPIDTF7)
    data$CREPIDTF8 <- as.numeric(input$CREPIDTF8)
    data$CREPIDImp1 <- as.numeric(input$CREPIDImp1)
    data$CREPIDImp2 <- as.numeric(input$CREPIDImp2)
    data$CREPIDImp3 <- as.numeric(input$CREPIDImp3)
    data$CREPIDImp4 <- as.numeric(input$CREPIDImp4)
    data$CREPIDImp5 <- as.numeric(input$CREPIDImp5)
    data$CREPIDImp6 <- as.numeric(input$CREPIDImp6)
    data$CREPIDImp7 <- as.numeric(input$CREPIDImp7)
    data$CREPIDImp8 <- as.numeric(input$CREPIDImp8)
    data$CREPIDPerf1 <- as.numeric(input$CREPIDPerf1)
    data$CREPIDPerf2 <- as.numeric(input$CREPIDPerf2)
    data$CREPIDPerf3 <- as.numeric(input$CREPIDPerf3)
    data$CREPIDPerf4 <- as.numeric(input$CREPIDPerf4)
    data$CREPIDPerf5 <- as.numeric(input$CREPIDPerf5)
    data$CREPIDPerf6 <- as.numeric(input$CREPIDPerf6)
    data$CREPIDPerf7 <- as.numeric(input$CREPIDPerf7)
    data$CREPIDPerf8 <- as.numeric(input$CREPIDPerf8)
    data$CREPIDAnnSalary <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$CREPIDAnnSalary)))

    #calculations
    CREPIDCalc()

    #output a results table
    col1 <- c("1. Total relative weights",
            "2. Overall economic value")

    col2 <- c(formatC(store$CREPIDCalc$CREPIDTotRW, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE),
            paste0(currSymHTML[data$currency], formatC(store$CREPIDCalc$CREPIDOverallEcVal, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)))

    #check if all the calculations result in a valid value and render appropriate results column
    if (!any(as.logical(store$CREPIDCalc) == logical(0))){
      
      if (all(as.logical(lapply(req(store$CREPIDCalc), is.finite)))){
    
        return(cbind(col1, col2))
    
      } else {
      
        return(cbind(col1, rep("-", 2)))
        
      }
      
    } else {
      
      return(cbind(col1, rep("-", 2)))
      
    }

  })
  
  #CREPID calculations
  CREPIDCalc <- function() {
    
    print("CREPIDCalc")
    
    #calculate economic value using CREPID method
    
    #CREPID time 1-8 
    cTime <- c(data$CREPIDTF1,data$CREPIDTF2,data$CREPIDTF3,data$CREPIDTF4,data$CREPIDTF5,data$CREPIDTF6,data$CREPIDTF7,data$CREPIDTF8)
    
    #CREPID importance 1-8 
    cImp <- c(data$CREPIDImp1,data$CREPIDImp2,data$CREPIDImp3,data$CREPIDImp4,data$CREPIDImp5,data$CREPIDImp6,data$CREPIDImp7,data$CREPIDImp8)
    
    rwTot <- as.numeric(cTime%*%cImp) # total of the relative weights
    rw <- (cTime*cImp) / rwTot # each relative weight
    
    #performance ratings 
    cPerfRat <- c(data$CREPIDPerf1,data$CREPIDPerf2,data$CREPIDPerf3,data$CREPIDPerf4,data$CREPIDPerf5,data$CREPIDPerf6,data$CREPIDPerf7,data$CREPIDPerf8)
    
    #annual pay
    ap <- data$CREPIDAnnSalary
    
    #dollar value
    cDollar <- rw*ap
    
    #net dollar value
    cNetDollar <- cPerfRat*cDollar
    
    #crepid results
    crepid <- sum(cNetDollar)

    #store results
    store$CREPIDCalc$CREPIDTotRW <- rwTot
    store$CREPIDCalc$CREPIDOverallEcVal <- crepid
   
  }
  
  #show the values in a table
  output$CREPIDTab <- renderTable({
    
    req(CREPIDData())
    
  }, align = 'lr', sanitize.text.function = function(x) x)
  
  #global estimation tab
  
  #global estimation UI
  output$globEstUI <- renderUI({
    
    print("globEstUI")

    tagList({
      
      fluidRow(
        
        column(12,

          fluidRow(

            column(12,
              
              "Note: Currency is set to ",data$currency,
              
              br()
              
            )
            
          ),
            
          fluidRow(
            
            br(),

            column(3,
              
              "Expert"
              
            ),
            
            column(3,
              
              "15th percentile"
              
            ),            
            
            column(3,
              
              "50th percentile"
              
            ),
            
            column(3,
              
              "85th percentile"
              
            )

          ),
          
          fluidRow(
            
            br(),

            column(3,
              
              "1"
              
            ),
            
            column(3,
              
              textInput("globEst15thPct1", label = NULL, isolate(data$globEst15thPct1))
              
            ),            
            
            column(3,
              
              textInput("globEst50thPct1", label = NULL, isolate(data$globEst50thPct1))
              
            ),
            
            column(3,
              
              textInput("globEst85thPct1", label = NULL, isolate(data$globEst85thPct1))
              
            )

          ),
          
          fluidRow(

            column(3,
              
              "2"
              
            ),
            
            column(3,
              
              textInput("globEst15thPct2", label = NULL, isolate(data$globEst15thPct2))
              
            ),            
            
            column(3,
              
              textInput("globEst50thPct2", label = NULL, isolate(data$globEst50thPct2))
              
            ),
            
            column(3,
              
              textInput("globEst85thPct2", label = NULL, isolate(data$globEst85thPct2))
              
            )

          ),
          
          fluidRow(

            column(3,
              
              "3"
              
            ),
            
            column(3,
              
              textInput("globEst15thPct3", label = NULL, isolate(data$globEst15thPct3))
              
            ),            
            
            column(3,
              
              textInput("globEst50thPct3", label = NULL, isolate(data$globEst50thPct3))
              
            ),
            
            column(3,
              
              textInput("globEst85thPct3", label = NULL, isolate(data$globEst85thPct3))
              
            )

          ),
          
          fluidRow(

            column(3,
              
              "4"
              
            ),
            
            column(3,
              
              textInput("globEst15thPct4", label = NULL, isolate(data$globEst15thPct4))
              
            ),            
            
            column(3,
              
              textInput("globEst50thPct4", label = NULL, isolate(data$globEst50thPct4))
              
            ),
            
            column(3,
              
              textInput("globEst85thPct4", label = NULL, isolate(data$globEst85thPct4))
              
            )

          ),
          
          fluidRow(

            column(3,
              
              "5"
              
            ),
            
            column(3,
              
              textInput("globEst15thPct5", label = NULL, isolate(data$globEst15thPct5))
              
            ),            
            
            column(3,
              
              textInput("globEst50thPct5", label = NULL, isolate(data$globEst50thPct5))
              
            ),
            
            column(3,
              
              textInput("globEst85thPct5", label = NULL, isolate(data$globEst85thPct5))
              
            )

          ),
          
          fluidRow(

            column(3,
              
              "6"
              
            ),
            
            column(3,
              
              textInput("globEst15thPct6", label = NULL, isolate(data$globEst15thPct6))
              
            ),            
            
            column(3,
              
              textInput("globEst50thPct6", label = NULL, isolate(data$globEst50thPct6))
              
            ),
            
            column(3,
              
              textInput("globEst85thPct6", label = NULL, isolate(data$globEst85thPct6))
              
            )

          ),
          
          fluidRow(

            column(3,
              
              "7"
              
            ),
            
            column(3,
              
              textInput("globEst15thPct7", label = NULL, isolate(data$globEst15thPct7))
              
            ),            
            
            column(3,
              
              textInput("globEst50thPct7", label = NULL, isolate(data$globEst50thPct7))
              
            ),
            
            column(3,
              
              textInput("globEst85thPct7", label = NULL, isolate(data$globEst85thPct7))
              
            )

          ),
          
          fluidRow(

            column(3,
              
              "8"
              
            ),
            
            column(3,
              
              textInput("globEst15thPct8", label = NULL, isolate(data$globEst15thPct8))
              
            ),            
            
            column(3,
              
              textInput("globEst50thPct8", label = NULL, isolate(data$globEst50thPct8))
              
            ),
            
            column(3,
              
              textInput("globEst85thPct8", label = NULL, isolate(data$globEst85thPct8))
              
            )

          ),
          
          fluidRow(

            column(3,
              
              "9"
              
            ),
            
            column(3,
              
              textInput("globEst15thPct9", label = NULL, isolate(data$globEst15thPct9))
              
            ),            
            
            column(3,
              
              textInput("globEst50thPct9", label = NULL, isolate(data$globEst50thPct9))
              
            ),
            
            column(3,
              
              textInput("globEst85thPct9", label = NULL, isolate(data$globEst85thPct9))
              
            )

          ),
          
          fluidRow(

            column(3,
              
              "10"
              
            ),
            
            column(3,
              
              textInput("globEst15thPct10", label = NULL, isolate(data$globEst15thPct10))
              
            ),            
            
            column(3,
              
              textInput("globEst50thPct10", label = NULL, isolate(data$globEst50thPct10))
              
            ),
            
            column(3,
              
              textInput("globEst85thPct10", label = NULL, isolate(data$globEst85thPct10))
              
            )

          ),
          
          fluidRow(

            column(3,
              
              "11"
              
            ),
            
            column(3,
              
              textInput("globEst15thPct11", label = NULL, isolate(data$globEst15thPct11))
              
            ),            
            
            column(3,
              
              textInput("globEst50thPct11", label = NULL, isolate(data$globEst50thPct11))
              
            ),
            
            column(3,
              
              textInput("globEst85thPct11", label = NULL, isolate(data$globEst85thPct11))
              
            )

          ),
          
          fluidRow(

            column(3,
              
              "12"
              
            ),
            
            column(3,
              
              textInput("globEst15thPct12", label = NULL, isolate(data$globEst15thPct12))
              
            ),            
            
            column(3,
              
              textInput("globEst50thPct12", label = NULL, isolate(data$globEst50thPct12))
              
            ),
            
            column(3,
              
              textInput("globEst85thPct12", label = NULL, isolate(data$globEst85thPct12))
              
            )

          ),
          
          fluidRow(

            column(3,
              
              "13"
              
            ),
            
            column(3,
              
              textInput("globEst15thPct13", label = NULL, isolate(data$globEst15thPct13))
              
            ),            
            
            column(3,
              
              textInput("globEst50thPct13", label = NULL, isolate(data$globEst50thPct13))
              
            ),
            
            column(3,
              
              textInput("globEst85thPct13", label = NULL, isolate(data$globEst85thPct13))
              
            )

          ),
          
          fluidRow(

            column(3,
              
              "14"
              
            ),
            
            column(3,
              
              textInput("globEst15thPct14", label = NULL, isolate(data$globEst15thPct14))
              
            ),            
            
            column(3,
              
              textInput("globEst50thPct14", label = NULL, isolate(data$globEst50thPct14))
              
            ),
            
            column(3,
              
              textInput("globEst85thPct14", label = NULL, isolate(data$globEst85thPct14))
              
            )

          ),
          
          fluidRow(

            column(3,
              
              "15"
              
            ),
            
            column(3,
              
              textInput("globEst15thPct15", label = NULL, isolate(data$globEst15thPct15))
              
            ),            
            
            column(3,
              
              textInput("globEst50thPct15", label = NULL, isolate(data$globEst50thPct15))
              
            ),
            
            column(3,
              
              textInput("globEst85thPct15", label = NULL, isolate(data$globEst85thPct15))
              
            )

          ),
          
          fluidRow(

            column(3,
              
              "16"
              
            ),
            
            column(3,
              
              textInput("globEst15thPct16", label = NULL, isolate(data$globEst15thPct16))
              
            ),            
            
            column(3,
              
              textInput("globEst50thPct16", label = NULL, isolate(data$globEst50thPct16))
              
            ),
            
            column(3,
              
              textInput("globEst85thPct16", label = NULL, isolate(data$globEst85thPct16))
              
            )

          ),
          
          fluidRow(

            column(3,
              
              "17"
              
            ),
            
            column(3,
              
              textInput("globEst15thPct17", label = NULL, isolate(data$globEst15thPct17))
              
            ),            
            
            column(3,
              
              textInput("globEst50thPct17", label = NULL, isolate(data$globEst50thPct17))
              
            ),
            
            column(3,
              
              textInput("globEst85thPct17", label = NULL, isolate(data$globEst85thPct17))
              
            )

          ),
          
          fluidRow(

            column(3,
              
              "18"
              
            ),
            
            column(3,
              
              textInput("globEst15thPct18", label = NULL, isolate(data$globEst15thPct18))
              
            ),            
            
            column(3,
              
              textInput("globEst50thPct18", label = NULL, isolate(data$globEst50thPct18))
              
            ),
            
            column(3,
              
              textInput("globEst85thPct18", label = NULL, isolate(data$globEst85thPct18))
              
            )

          ),
          
          fluidRow(

            column(3,
              
              "19"
              
            ),
            
            column(3,
              
              textInput("globEst15thPct19", label = NULL, isolate(data$globEst15thPct19))
              
            ),            
            
            column(3,
              
              textInput("globEst50thPct19", label = NULL, isolate(data$globEst50thPct19))
              
            ),
            
            column(3,
              
              textInput("globEst85thPct19", label = NULL, isolate(data$globEst85thPct19))
              
            )

          ),
          
          fluidRow(

            column(3,
              
              "20"
              
            ),
            
            column(3,
              
              textInput("globEst15thPct20", label = NULL, isolate(data$globEst15thPct20))
              
            ),            
            
            column(3,
              
              textInput("globEst50thPct20", label = NULL, isolate(data$globEst50thPct20))
              
            ),
            
            column(3,
              
              textInput("globEst85thPct20", label = NULL, isolate(data$globEst85thPct20))
              
            )

          )
          
        )
        
      )
      
    })
    
  })
  
  #global estimation inputs
  globEstData <- reactive({
    
    print("globEstData")
    
    #convert text inputs to numbers and strip symbols
    #global estimation inputs
    data$globEst15thPct1 <- as.numeric(input$globEst15thPct1)
    data$globEst15thPct2 <- as.numeric(input$globEst15thPct2)
    data$globEst15thPct3 <- as.numeric(input$globEst15thPct3)
    data$globEst15thPct4 <- as.numeric(input$globEst15thPct4)
    data$globEst15thPct5 <- as.numeric(input$globEst15thPct5)
    data$globEst15thPct6 <- as.numeric(input$globEst15thPct6)
    data$globEst15thPct7 <- as.numeric(input$globEst15thPct7)
    data$globEst15thPct8 <- as.numeric(input$globEst15thPct8)
    data$globEst15thPct9 <- as.numeric(input$globEst15thPct9)
    data$globEst15thPct10 <- as.numeric(input$globEst15thPct10)
    data$globEst15thPct11 <- as.numeric(input$globEst15thPct11)
    data$globEst15thPct12 <- as.numeric(input$globEst15thPct12)
    data$globEst15thPct13 <- as.numeric(input$globEst15thPct13)
    data$globEst15thPct14 <- as.numeric(input$globEst15thPct14)
    data$globEst15thPct15 <- as.numeric(input$globEst15thPct15)
    data$globEst15thPct16 <- as.numeric(input$globEst15thPct16)
    data$globEst15thPct17 <- as.numeric(input$globEst15thPct17)
    data$globEst15thPct18 <- as.numeric(input$globEst15thPct18)
    data$globEst15thPct19 <- as.numeric(input$globEst15thPct19)
    data$globEst15thPct20 <- as.numeric(input$globEst15thPct20)
    data$globEst50thPct1 <- as.numeric(input$globEst50thPct1)
    data$globEst50thPct2 <- as.numeric(input$globEst50thPct2)
    data$globEst50thPct3 <- as.numeric(input$globEst50thPct3)
    data$globEst50thPct4 <- as.numeric(input$globEst50thPct4)
    data$globEst50thPct5 <- as.numeric(input$globEst50thPct5)
    data$globEst50thPct6 <- as.numeric(input$globEst50thPct6)
    data$globEst50thPct7 <- as.numeric(input$globEst50thPct7)
    data$globEst50thPct8 <- as.numeric(input$globEst50thPct8)
    data$globEst50thPct9 <- as.numeric(input$globEst50thPct9)
    data$globEst50thPct10 <- as.numeric(input$globEst50thPct10)
    data$globEst50thPct11 <- as.numeric(input$globEst50thPct11)
    data$globEst50thPct12 <- as.numeric(input$globEst50thPct12)
    data$globEst50thPct13 <- as.numeric(input$globEst50thPct13)
    data$globEst50thPct14 <- as.numeric(input$globEst50thPct14)
    data$globEst50thPct15 <- as.numeric(input$globEst50thPct15)
    data$globEst50thPct16 <- as.numeric(input$globEst50thPct16)
    data$globEst50thPct17 <- as.numeric(input$globEst50thPct17)
    data$globEst50thPct18 <- as.numeric(input$globEst50thPct18)
    data$globEst50thPct19 <- as.numeric(input$globEst50thPct19)
    data$globEst50thPct20 <- as.numeric(input$globEst50thPct20)
    data$globEst85thPct1 <- as.numeric(input$globEst85thPct1)
    data$globEst85thPct2 <- as.numeric(input$globEst85thPct2)
    data$globEst85thPct3 <- as.numeric(input$globEst85thPct3)
    data$globEst85thPct4 <- as.numeric(input$globEst85thPct4)
    data$globEst85thPct5 <- as.numeric(input$globEst85thPct5)
    data$globEst85thPct6 <- as.numeric(input$globEst85thPct6)
    data$globEst85thPct7 <- as.numeric(input$globEst85thPct7)
    data$globEst85thPct8 <- as.numeric(input$globEst85thPct8)
    data$globEst85thPct9 <- as.numeric(input$globEst85thPct9)
    data$globEst85thPct10 <- as.numeric(input$globEst85thPct10)
    data$globEst85thPct11 <- as.numeric(input$globEst85thPct11)
    data$globEst85thPct12 <- as.numeric(input$globEst85thPct12)
    data$globEst85thPct13 <- as.numeric(input$globEst85thPct13)
    data$globEst85thPct14 <- as.numeric(input$globEst85thPct14)
    data$globEst85thPct15 <- as.numeric(input$globEst85thPct15)
    data$globEst85thPct16 <- as.numeric(input$globEst85thPct16)
    data$globEst85thPct17 <- as.numeric(input$globEst85thPct17)
    data$globEst85thPct18 <- as.numeric(input$globEst85thPct18)
    data$globEst85thPct19 <- as.numeric(input$globEst85thPct19)
    data$globEst85thPct20 <- as.numeric(input$globEst85thPct20)

    #calculations
    globEstCalc()

    #output a results table
    col1 <- c("1. Overall average",
            "2. Average difference between 15th and 50th perentile",
            "3. Average difference between 50th and 85th perentile",
            "4. Standard error of the mean",
            "5. 90% confidence interval")

    col2 <- c(formatC(store$globEstCalc$globEstOverallAvg, format = "f", digits = 4, big.mark = "," , drop0trailing = FALSE),
            formatC(store$globEstCalc$globEstAvgDiff15th50th, format = "f", digits = 4, big.mark = "," , drop0trailing = FALSE),
            formatC(store$globEstCalc$globEstAvgDiff50th85th, format = "f", digits = 4, big.mark = "," , drop0trailing = FALSE),
            formatC(store$globEstCalc$globEstStdError, format = "f", digits = 4, big.mark = "," , drop0trailing = FALSE),
            paste0("[",formatC(store$globEstCalc$globEstConfIntLo, format = "f", digits = 4, big.mark = "," , drop0trailing = FALSE),",",formatC(store$globEstCalc$globEstConfIntHi, format = "f", digits = 4, big.mark = "," , drop0trailing = FALSE),"]"))

    #check if all the calculations result in a valid value and render appropriate results column
    if (!any(as.logical(store$globEstCalc) == logical(0))){
      
      if (all(as.logical(lapply(req(store$globEstCalc), is.finite)))){
    
        return(cbind(col1, col2))
    
      } else {
      
        return(cbind(col1, rep("-", 5)))
        
      }
      
    } else {
      
      return(cbind(col1, rep("-", 5)))
      
    }

  })
  
  #global estimation calculations
  globEstCalc <- function() {
    
    print("globEstCalc")
    
    #percentile
    pct15Est <- c(data$globEst15thPct1,
                data$globEst15thPct2,
                data$globEst15thPct3,
                data$globEst15thPct4,
                data$globEst15thPct5,
                data$globEst15thPct6,
                data$globEst15thPct7,
                data$globEst15thPct8,
                data$globEst15thPct9,
                data$globEst15thPct10,
                data$globEst15thPct11,
                data$globEst15thPct12,
                data$globEst15thPct13,
                data$globEst15thPct14,
                data$globEst15thPct15,
                data$globEst15thPct16,
                data$globEst15thPct17,
                data$globEst15thPct18,
                data$globEst15thPct19,
                data$globEst15thPct20)

    pct50Est <- c(data$globEst50thPct1,
                data$globEst50thPct2,
                data$globEst50thPct3,
                data$globEst50thPct4,
                data$globEst50thPct5,
                data$globEst50thPct6,
                data$globEst50thPct7,
                data$globEst50thPct8,
                data$globEst50thPct9,
                data$globEst50thPct10,
                data$globEst50thPct11,
                data$globEst50thPct12,
                data$globEst50thPct13,
                data$globEst50thPct14,
                data$globEst50thPct15,
                data$globEst50thPct16,
                data$globEst50thPct17,
                data$globEst50thPct18,
                data$globEst50thPct19,
                data$globEst50thPct20)
    
    pct85Est <- c(data$globEst85thPct1,
                data$globEst85thPct2,
                data$globEst85thPct3,
                data$globEst85thPct4,
                data$globEst85thPct5,
                data$globEst85thPct6,
                data$globEst85thPct7,
                data$globEst85thPct8,
                data$globEst85thPct9,
                data$globEst85thPct10,
                data$globEst85thPct11,
                data$globEst85thPct12,
                data$globEst85thPct13,
                data$globEst85thPct14,
                data$globEst85thPct15,
                data$globEst85thPct16,
                data$globEst85thPct17,
                data$globEst85thPct18,
                data$globEst85thPct19,
                data$globEst85thPct20)
                    
                dif50and15 <- as.numeric(pct50Est) - as.numeric(pct15Est)
                dif85and50 <- as.numeric(pct85Est) - as.numeric(pct50Est)
                ct <- sum(as.numeric(pct50Est)!=0)
                
                #calculate  outputs
                pct50total <- sum(pct50Est) / ct
                dif50th15total <- sum(dif50and15) / ct
                dif85th50total <- sum(dif85and50) / ct
                meanOverall <- (dif50th15total+dif85th50total) / 2
                stdErrorMean <- meanOverall / sqrt(ct)
                confInt90hi <- pct50total + 1.65 * stdErrorMean
                confInt90lo <- pct50total - 1.65 * stdErrorMean

                #store results
                store$globEstCalc$globEstOverallAvg <- meanOverall
                store$globEstCalc$globEstAvgDiff15th50th <- dif50th15total
                store$globEstCalc$globEstAvgDiff50th85th <- dif85th50total
                store$globEstCalc$globEstStdError <- stdErrorMean
                store$globEstCalc$globEstConfIntLo <- confInt90lo
                store$globEstCalc$globEstConfIntHi <- confInt90hi
   
  }
  
  #show the values in a table
  output$globEstTab <- renderTable({
    
    req(globEstData())
    
  }, align = 'lr', sanitize.text.function = function(x) x)
  
  ###development###
  
  #break-even tab#
  
  #break-even UI
  output$brEvUI1 <- renderUI({
    
    fluidRow(
      
      column(12,
        
        fluidRow(
          
          column(12,
            
            "Note: Currency is set to ",data$currency,
            
            br(),
            
            br(),
            
            HTML("<h4>1. Break-even - Given d<sub>t</sub></h4>"),
            
            br(),
            
            div(style="margin-bottom:5px;",HTML("Effect size (standard deviation difference between job performance of trained and untrained employees, d<sub>t</sub>):")),
            
            textInput("brEvDt", label = NULL, isolate(data$brEvDt)),
            
            textInput("brEvNumTrainee1", "Number of employees being trained:", isolate(data$brEvNumTrainee1)),
            
            textInput("brEvTrainCostPer1", "Cost of training each employee:", paste0(currSym[data$currency],isolate(data$brEvTrainCostPer1))),
            
            div(style="margin-bottom:5px;","Expected duration of benefits in the trained group (in years):"),
            
            textInput("brEvBenDuration", label = NULL, isolate(data$brEvBenDuration))
          
          )
        
        )
        
      )
      
    )
    
  })

  #break-even UI
  output$brEvUI2 <- renderUI({
    
    fluidRow(
      
      column(12,
        
        fluidRow(
          
          column(12,
            
            hr(),
            
            br(),
            
            HTML("<h4>2. Break-even - Given SD<sub>y</sub></h4>"),
            
            br(),
            
            div(style="margin-bottom:5px;",HTML("Standard deviation of dollar-valued job performance among untrained employees (SD<sub>y</sub>):")),
            
            textInput("brEvSDy", label = NULL, paste0(currSym[data$currency],isolate(data$brEvSDy))),
            
            textInput("brEvNumTrainee2", "Number of employees being trained:", isolate(data$brEvNumTrainee2)),
            
            textInput("brEvTrainCostPer2", "Cost of training each employee:", paste0(currSym[data$currency],isolate(data$brEvTrainCostPer2))),
            
            div(style="margin-bottom:5px;","Number of years the organizational development effort will last:"),
            
            textInput("brEvDevYears", label = NULL, isolate(data$brEvDevYears))
          
          )
        
        )
        
      )
      
    )
  
  })
  
  #break-even inputs
  brEvData1 <- reactive({
    
    print("brEvData1")
    
    #convert text inputs to numbers and strip symbols
    #given Dt inputs
    data$brEvDt <- as.numeric(input$brEvDt)
    data$brEvNumTrainee1 <- as.numeric(input$brEvNumTrainee1)
    data$brEvTrainCostPer1 <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$brEvTrainCostPer1)))
    data$brEvBenDuration <- as.numeric(input$brEvBenDuration)

    #given SDy inputs
    data$brEvSDy <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$brEvSDy)))
    data$brEvNumTrainee2 <- as.numeric(input$brEvNumTrainee2)
    data$brEvTrainCostPer2 <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$brEvTrainCostPer2)))
    data$brEvDevYears <- as.numeric(input$brEvDevYears)
    
    #calculations
    brEvCalc()
    
    #output a results table
    col1 <- c("1. Standard deviation break-even dollar value")
    
    col2 <- c(paste0(currSym[data$currency],formatC(store$brEvCalc$SDBrEvVal, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)))
    
    #check if all the calculations result in a valid value and render appropriate results column
    if(!is.null(store$brEvCalc$SDBrEvVal)) {
      
      if(!is.finite(store$brEvCalc$SDBrEvVal)){col2[1] <- "-"}
      
    } else {
      
      col2[1] <- "-"
      
    }

    return(cbind(col1, col2))

  })
  
  #break-even inputs
  brEvData2 <- reactive({
    
    print("brEvData2")
    
    #convert text inputs to numbers and strip symbols
    #given Dt inputs
    data$brEvDt <- as.numeric(input$brEvDt)
    data$brEvNumTrainee1 <- as.numeric(input$brEvNumTrainee1)
    data$brEvTrainCostPer1 <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$brEvTrainCostPer1)))
    data$brEvBenDuration <- as.numeric(input$brEvBenDuration)

    #given SDy inputs
    data$brEvSDy <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$brEvSDy)))
    data$brEvNumTrainee2 <- as.numeric(input$brEvNumTrainee2)
    data$brEvTrainCostPer2 <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$brEvTrainCostPer2)))
    data$brEvDevYears <- as.numeric(input$brEvDevYears)
    
    #calculations
    brEvCalc()
    
    #output a results table
    col1 <- c("2. Break-even value of the true difference in job performance")
    
    col2 <- c(formatC(store$brEvCalc$dtBrEvVal, format = "f", digits = 3, big.mark = "," , drop0trailing = FALSE))
    
    #check if all the calculations result in a valid value and render appropriate results column
    if(!is.null(store$brEvCalc$dtBrEvVal)) {
      
      if(!is.finite(store$brEvCalc$dtBrEvVal)){col2[1] <- "-"}
      
    } else {
      
      col2[1] <- "-"
      
    }
    
    return(cbind(col1, col2))

  })
  
  #break-even calculations
  brEvCalc <- function() {
    
    print("brEvCalc")
    
    store$brEvCalc$SDBrEvVal <- (data$brEvNumTrainee1 * data$brEvTrainCostPer1) / (data$brEvBenDuration * data$brEvNumTrainee1 * data$brEvDt)
    
    store$brEvCalc$dtBrEvVal <- (data$brEvNumTrainee2 * data$brEvTrainCostPer2) / (data$brEvDevYears * data$brEvNumTrainee2 * data$brEvSDy)

  }
  
  #show the values in a table
  output$brEvTab1 <- renderTable({
    
    req(brEvData1())
    
  }, align = 'lr', sanitize.text.function = function(x) x)
  
  #show the values in a table
  output$brEvTab2 <- renderTable({
    
    req(brEvData2())
    
  }, align = 'lr', sanitize.text.function = function(x) x)

  #break-even plot dt
  output$brEvPlot1 <- renderPlot({
    
    dt <- data$brEvDt
    halfDt <- dt/2
    z1 <- 0 - halfDt
    z2 <- 0 + halfDt
    
    x <- seq(-4,4,length=500)
    y1 <- dnorm(x,mean=z1,sd=1)
    y2 <- dnorm(x,mean=z2,sd=1)
    plot(x,y1,type = "l",lty=1,col="red",main="Distributions of Performance Among Trained and Untrained Employees",ylim=c(0,0.5),xlim=c(-4,4),ylab="Probability Density",xlab="Standardized Job Performance",xaxt="n")
    axis(side=1,labels=F,at=-4:4) 
    curve((dnorm(x,mean=z1,sd=1)+.001),add=TRUE,lty=1,col="red")
    curve(dnorm(x,mean=z2,sd=1),add=TRUE,lty=1,col="blue")
    abline(v=z1, col='red')
    abline(v=z2, col='blue')
    arrows(x0=z1, y0=0.2, x1=z2, y1=0.2, length=0.05, code=3, col='black')
    text(x=0, y=0.17, cex=1, labels=bquote(paste(italic(d)['t']*' = ', .(z2-z1))), col='black')
    text(x=(z1-0.3), y=0.45, cex=1, labels=bquote(paste(bar(italic(Z))['u'])), col='red')
    text(x=(z2+0.3), y=0.45, cex=1, labels=bquote(paste(bar(italic(Z))['t'])), col='blue')
    legend("topright", legend=c("Untrained Employees", "Trained Employees"), col=c("red", "blue"), lty=c(1,1), cex=0.8)
    
  })
  
  #break-even plot dt
  output$brEvPlot2 <- renderPlot({
    
    if (!is.finite(store$brEvCalc$dtBrEvVal)) {
      dt <- 0
    } else {
      dt <- store$brEvCalc$dtBrEvVal
    }
    
    halfDt <- dt/2
    z1 <- 0 - halfDt
    z2 <- 0 + halfDt
    
    x <- seq(-4,4,length=500)
    y1 <- dnorm(x,mean=z1,sd=1)
    y2 <- dnorm(x,mean=z2,sd=1)
    plot(x,y1,type = "l",lty=1,col="red",main="Distributions of Performance Among Trained and Untrained Employees",ylim=c(0,0.5),xlim=c(-4,4),ylab="Probability Density",xlab="Standardized Job Performance",xaxt="n")
    axis(side=1,labels=F,at=-4:4) 
    curve((dnorm(x,mean=z1,sd=1)+.001),add=TRUE,lty=1,col="red")
    curve(dnorm(x,mean=z2,sd=1),add=TRUE,lty=1,col="blue")
    abline(v=z1, col='red')
    abline(v=z2, col='blue')
    arrows(x0=z1, y0=0.2, x1=z2, y1=0.2, length=0.05, code=3, col='black')
    text(x=0, y=0.17, cex=1, labels=bquote(paste(italic(d)['t']*' = ', .(z2-z1))), col='black')
    text(x=(z1-0.3), y=0.45, cex=1, labels=bquote(paste(bar(italic(Z))['u'])), col='red')
    text(x=(z2+0.3), y=0.45, cex=1, labels=bquote(paste(bar(italic(Z))['t'])), col='blue')
    legend("topright", legend=c("Untrained Employees", "Trained Employees"), col=c("red", "blue"), lty=c(1,1), cex=0.8)
    
  })
  
  #training utility tab#
  
  #training utility UI
  output$traUtUI <- renderUI({
        
    fluidRow(
      
      column(12,
        
        "Note: Currency is set to ",data$currency,
        
        br(),
        
        br(),
        
        textInput("traUtNumTrainee", "Number of employees being trained:", isolate(data$traUtNumTrainee)),
        
        div(style="margin-bottom:5px;","Expected duration of benefits in the trained group (in years):"),
  
        textInput("traUtBenDuration", label = NULL, isolate(data$traUtBenDuration)),
        
        div(style="margin-bottom:5px;",HTML("Effect size (standard deviation difference between job performance of trained and untrained employees, d<sub>t</sub>):")),
        
        textInput("traUtDt", label = NULL, isolate(data$traUtDt)),
  
        div(style="margin-bottom:5px;",HTML("Standard deviation of dollar-valued job performance among untrained employees (SD<sub>y</sub>):")),
        
        textInput("traUtSDy", label = NULL, paste0(currSym[data$currency],isolate(data$traUtSDy))),
        
        textInput("traUtTrainCostPer", "Cost of training each employee:", paste0(currSym[data$currency],isolate(data$traUtTrainCostPer)))
  
      )
    
    )
  
  })
  
  #training utility inputs
  traUtData <- reactive({
    
    print("traUtData")
    
    #convert text inputs to numbers and strip symbols
    #training utility inputs
    data$traUtNumTrainee <- as.numeric(input$traUtNumTrainee)
    data$traUtBenDuration <- as.numeric(input$traUtBenDuration)
    data$traUtDt <- as.numeric(input$traUtDt)
    data$traUtSDy <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$traUtSDy)))
    data$traUtTrainCostPer <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$traUtTrainCostPer)))
    
    #calculations
    traUtCalc()
    
    #output a results table
    col1 <- "1. Estimated utility of the training program"
    
    col2 <- paste0(currSymHTML[data$currency], formatC(store$traUtCalc$traUtEst, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE))
    
    #check if all the calculations result in a valid value and render appropriate results column
    if(!is.null(store$traUtCalc$traUtEst)) {
      
      if(!is.finite(store$traUtCalc$traUtEst)){col2 <- "-"}
      
    } else {
      
      col2 <- "-"
      
    }
    
    return(cbind(col1, col2))
    
  })

  #training utility calculations
  traUtCalc <- function() {
    
    print("traUtCalc")
    
    store$traUtCalc$traUtEst <- (data$traUtNumTrainee * data$traUtBenDuration * data$traUtDt * data$traUtSDy) - (data$traUtTrainCostPer * data$traUtNumTrainee)

  }
  
  #show the values in a table
  output$traUtTab <- renderTable({
    
    req(traUtData())
    
  }, align = 'lr', sanitize.text.function = function(x) x)
  
  #meeting costs tab#

  #first meeting costs UI
  output$meet1UI <- renderUI({
    
    print("meet1UI")
    
    tagList({
      
      fluidRow(
        
        column(12,
          
          fluidRow(
            
            column(12,
              
              "Note: Currency is set to ",data$currency,
              
              br(),
            
              br(),
                
              tags$h4("1. Company details"),
              
              br(),
              
              textInput("numEmpsInCompany3", "Total number of employees:", isolate(data$numEmpsInCompany3)),
              textInput("meetNumManagers", "Total number of managers:", isolate(data$meetNumManagers)),
              textInput("meetNumOffsite", "Number of off-site meetings held per year:", isolate(data$meetNumOffsite)),
              textInput("meetNumMgrAttend", "Number of managers attend each meeting:", isolate(data$meetNumMgrAttend)),
              textInput("meetDuration", "Duration of the meeting (in days):", isolate(data$meetDuration))
              
            )
            
          ),
              
          fluidRow(
    
            column(12,
    
              hr(),
              
              br(),
    
              tags$h4("2. Development Costs"),
    
              br(),
    
              "Enter cost of development as:",
    
              div(style = "width:170px;margin-top:5px;",
    
                selectInput("meetDevCostMethod", label = NULL, c("Itemized costs", "A lump sum"), isolate(data$meetDevCostMethod))
    
              )
    
            )
            
          )
          
        )

      )
  
    })
    
  })
  
  #second meeting costs UI
  output$meet2UI <- renderUI({
    
    print("meet2UI")
    
    #check if meeting development costs are itemized
    if (req(input$meetDevCostMethod) == "Itemized costs") {
    
      #inputs if "Itemized costs" is selected
      tagList(
          
        fluidRow(
          
          column(12,
          
            textInput("meetTraDeptOverhead", "Training department overhead:", paste0(currSym[data$currency],isolate(data$meetTraDeptOverhead))),
            textInput("meetTraStaff", "Training staff salaries:", paste0(currSym[data$currency],isolate(data$meetTraStaff))),
            textInput("meetOutsideConsult", "Outside consultants:", paste0(currSym[data$currency],isolate(data$meetOutsideConsult))),
            textInput("meetEquipMaterial", "Equipment and meeting materials:", paste0(currSym[data$currency],isolate(data$meetEquipMaterial)))

          )
          
        )
        
      )
      
    } else {
      
      #inputs if "A lump sum" is selected
      tagList(
        
        fluidRow(
          
          column(12,
            
            div(style="margin-bottom:5px;","Total development cost of the program (Previous Year):"),
      
            textInput("meetDevCostPrev", label = NULL, paste0(currSym[data$currency],isolate(data$meetDevCostPrev)))        

          )
          
        )
        
      )
      
    }
    
  })
  
  #third meeting costs UI
  output$meet3UI <- renderUI({
    
    print("meet3UI")
    
    tagList({
      
      fluidRow(
        
        column(12,
          
          fluidRow(
            
            column(12,
              
              hr(),
              
              br(),
                
              tags$h4("3. Participant Costs"),
              
              br(),
              
              textInput("meetAvgAttendee", "Attendee's average salary (including benefits):", paste0(currSym[data$currency],isolate(data$meetAvgAttendee)))
              
            )
            
          ),
              
          fluidRow(
    
            column(12,
    
              hr(),
              
              br(),
    
              tags$h4("4. Delivery Costs"),
    
              br(),
    
              "Enter facility costs as :",
    
              div(style = "width:170px;margin-top:5px;",
    
                selectInput("meetFacCostMethod", label = NULL, c("Itemized costs", "A lump sum"), isolate(data$meetFacCostMethod))
    
              )
    
            )
            
          )
          
        )

      )
  
    })
    
  })
  
  #fourth meeting costs UI
  output$meet4UI <- renderUI({
    
    print("meet4UI")
    
    #check if meeting facility costs are itemized
    if (req(input$meetFacCostMethod) == "Itemized costs") {
    
      #inputs if "Itemized costs" is selected
      tagList(
          
        fluidRow(
          
          column(12,
          
            textInput("meetSleepRooms", "Sleeping rooms (total):", paste0(currSym[data$currency],isolate(data$meetSleepRooms))),
            textInput("meetThreeMeals", "Three meals per day (total):", paste0(currSym[data$currency],isolate(data$meetThreeMeals))),
            textInput("meetCoffeeBreaks", "Coffee breaks (total):", paste0(currSym[data$currency],isolate(data$meetCoffeeBreaks))),
            textInput("meetReception", "Reception (total):", paste0(currSym[data$currency],isolate(data$meetReception)))

          )
          
        )
        
      )
      
    } else {
      
      #inputs if "A lump sum" is selected
      tagList(
        
        fluidRow(
          
          column(12,
      
            textInput("meetFacLumpsum", "Total facility costs:", paste0(currSym[data$currency],isolate(data$meetFacLumpsum)))        

          )
          
        )
        
      )
      
    }
    
  })
  
  #fifth meeting costs UI
  output$meet5UI <- renderUI({
    
    print("meet5UI")
    
    tagList({
      
      fluidRow(
        
        column(12,
          
          fluidRow(
            
            column(12,
            
              br(),
                
              "Enter meeting charges as:",
    
              div(style = "width:170px;margin-top:5px;",
    
                selectInput("meetChargeMethod", label = NULL, c("Itemized costs", "A lump sum"), isolate(data$meetChargeMethod))
    
              )
              
            )
            
          )
          
        )
        
      )
        
    })
      
  })

  #sixth meeting costs UI
  output$meet6UI <- renderUI({
    
    print("meet6UI")
    
    #check if meeting charges are itemized
    if (req(input$meetChargeMethod) == "Itemized costs") {
    
      #inputs if "Itemized costs" is selected
      tagList(
          
        fluidRow(
          
          column(12,
          
            textInput("meetRoomRental", "Room rental (total):", paste0(currSym[data$currency],isolate(data$meetRoomRental))),
            textInput("meetAVEquip", "Audiovisual equipment rental (total):", paste0(currSym[data$currency],isolate(data$meetAVEquip))),
            textInput("meetBusSvcs", "Business services (total):", paste0(currSym[data$currency],isolate(data$meetBusSvcs))),
            textInput("meetTransport", "Transportation to the meeting:", paste0(currSym[data$currency],isolate(data$meetTransport)))

          )
          
        )
        
      )
      
    } else {
      
      #inputs if "A lump sum" is selected
      tagList(
        
        fluidRow(
          
          column(12,
      
            textInput("meetOtherCharges", "Other meeting charges:", paste0(currSym[data$currency],isolate(data$meetOtherCharges))),
            textInput("meetTransport", "Transportation to the meeting:", paste0(currSym[data$currency],isolate(data$meetTransport)))        

          )
          
        )
        
      )
      
    }
    
  })
  
  #meeting costs inputs
  meetData <- reactive({
    
    print("meetData")
    
    #convert text inputs to numbers and strip symbols
    #company details inputs
    data$numEmpsInCompany <- data$numEmpsInCompany1 <- data$numEmpsInCompany2 <- data$numEmpsInCompany3 <- as.numeric(input$numEmpsInCompany3)
    data$meetNumManagers <- as.numeric(input$meetNumManagers)
    data$meetNumOffsite <- as.numeric(input$meetNumOffsite)
    data$meetNumMgrAttend <- as.numeric(input$meetNumMgrAttend)
    data$meetDuration <- as.numeric(input$meetDuration)
    
    #development costs inputs
    if (req(input$meetDevCostMethod) == "Itemized costs") {
      
      #update settings
      data$meetDevCostMethod <- "Itemized costs"
      
      #development costs itemized inputs
      data$meetTraDeptOverhead <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$meetTraDeptOverhead)))
      data$meetTraStaff <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$meetTraStaff)))
      data$meetOutsideConsult <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$meetOutsideConsult)))
      data$meetEquipMaterial <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$meetEquipMaterial)))
      
    } else {
      
      #update settings
      data$meetDevCostMethod <- "A lump sum"
      
      #development costs lump sum input
      data$meetDevCostPrev <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$meetDevCostPrev)))
      
    }
    
    #participant costs inputs
    data$meetAvgAttendee <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$meetAvgAttendee)))
    
    #delivery costs - facility costs inputs
    if (req(input$meetFacCostMethod) == "Itemized costs") {
      
      #update settings
      data$meetFacCostMethod <- "Itemized costs"
      
      #facility costs itemized inputs
      data$meetSleepRooms <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$meetSleepRooms)))
      data$meetThreeMeals <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$meetThreeMeals)))
      data$meetCoffeeBreaks <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$meetCoffeeBreaks)))
      data$meetReception <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$meetReception)))
      
    } else {
      
      #update settings
      data$meetFacCostMethod <- "A lump sum"
      
      #facility costs lump sum input
      data$meetFacLumpsum <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$meetFacLumpsum)))
      
    }
    
    #delivery costs - meeting charges inputs
    if (req(input$meetChargeMethod) == "Itemized costs") {
      
      #update settings
      data$meetChargeMethod <- "Itemized costs"
      
      #meeting charges itemized inputs
      data$meetRoomRental <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$meetRoomRental)))
      data$meetAVEquip <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$meetAVEquip)))
      data$meetBusSvcs <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$meetBusSvcs)))
      
    } else {
      
      #update settings
      data$meetChargeMethod <- "A lump sum"
      
      #meeting charges lump sum input
      data$meetOtherCharges <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$meetOtherCharges)))
      
    }
    
    data$meetTransport <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$meetTransport)))

    #calculations
    meetCalc()
    
    updtGlob(c("numEmpsInCompany3"))

    
    #output a results table
    col1 <- "1. Total per-day, per-person cost of one off-site meeting"
    
    col2 <- paste0(currSymHTML[data$currency], formatC(store$meetCalc$meetCost, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE))
    
    #check if all the calculations result in a valid value and render appropriate results column
    if(!is.null(store$meetCalc$meetCost)) {
      
      if(!is.finite(store$meetCalc$meetCost)){col2 <- "-"}
      
    } else {
      
      col2 <- "-"
      
    }
    
    return(cbind(col1, col2))
    
  })
  
  #meeting costs calculations
  meetCalc <- function() {
    
    print("meetCalc")
    
    #company details
    nEmp <- data$numEmpsInCompany3
    nMgrTotal <- data$meetNumManagers
    nOffSiteMeet <- data$meetNumOffsite
    nMgrAttend <- data$meetNumMgrAttend
    durDays <- data$meetDuration

    #development costs
    if (req(input$meetDevCostMethod) == "Itemized costs") {
    
      devCost <- data$meetTraDeptOverhead + data$meetTraStaff + data$meetOutsideConsult + data$meetEquipMaterial
      
    } else {
      
      devCost <- data$meetDevCostPrev 
    
    }
    
    #participant costs   
    attendSalaryAvg <- data$meetAvgAttendee
    
    #delivery costs - facility costs
    if (req(input$meetFacCostMethod) == "Itemized costs") {
    
      facilityCost <- data$meetSleepRooms + data$meetThreeMeals + data$meetCoffeeBreaks + data$meetReception
      
    } else {
      
      facilityCost <- data$meetFacLumpsum
    
    }
    
    #delivery costs - meeting charges
    if (req(input$meetChargeMethod) == "Itemized costs") {
    
      meetingCost <- data$meetRoomRental + data$meetAVEquip + data$meetBusSvcs
      
    } else {
      
      meetingCost <- data$meetOtherCharges
    
    }
    
    transportCost <- data$meetTransport
    
    #calculate final meeting costs totals
    v1 <- (devCost / nOffSiteMeet) / nMgrTotal
    v2 <- attendSalaryAvg / 236 # salary per working day
    v3 <- (meetingCost / nMgrAttend) / durDays
    v4 <- (facilityCost / nMgrAttend) / durDays
    v5 <- (transportCost / nMgrAttend) / durDays
    
    #final cost of offsite meetings per person per day
    store$meetCalc$meetCost <- v1 + v2 + v3 + v4 + v5

  }
  
  #show the values in a table
  output$meetTab <- renderTable({
    
    req(meetData())
    
  }, align = 'lr', sanitize.text.function = function(x) x)

  #one cohort utility tab#
  
  #one cohort utility UI
  output$oneCohUtUI <- renderUI({
    
    print("oneCohUtUI")
    
    fluidRow(
      
      column(12,
        
        "Note: Currency is set to ",data$currency,
        
        br(),

        br(),

        textInput("oneCohUtVarCosts", "Variable costs (enter as a negative proportion):", isolate(data$oneCohUtVarCosts)),
        textInput("oneCohUtCorpTax", "Corporate tax (percentage):", paste0(isolate(data$oneCohUtCorpTax),"%")),
        textInput("oneCohUtDiscRate", "Discount rate (percentage):", paste0(isolate(data$oneCohUtDiscRate),"%")),
        textInput("oneCohUtTrainCostPer", "Cost to train one employee:", paste0(currSym[data$currency],isolate(data$oneCohUtTrainCostPer))),
        textInput("oneCohUtNumTrainees", "Number of employees trained:", isolate(data$oneCohUtNumTrainees)),
        
        div(style="margin-bottom:5px;",HTML("Effect size (standard deviation difference between job performance of trained and untrained employees, d<sub>t</sub>):")),
        textInput("oneCohUtDt", label = NULL, isolate(data$oneCohUtDt)),
        
        div(style="margin-bottom:5px;",HTML("Standard deviation of dollar-valued job performance among untrained employees (SD<sub>y</sub>):")),
        textInput("oneCohUtSDy", label = NULL, paste0(currSym[data$currency],isolate(data$oneCohUtSDy))),
        div(style="margin-bottom:5px;","Expected duration of benefits in the trained group (in years):"),
        textInput("oneCohUtDuration", label = NULL, isolate(data$oneCohUtDuration))
      
      )
      
    )
    
  })
  
  #one cohort utility inputs
  oneCohUtData <- reactive({
    
    print("oneCohUtData")
    
    #convert text inputs to numbers and strip symbols
    #one cohort utility inputs
    data$oneCohUtVarCosts <- as.numeric(input$oneCohUtVarCosts)
    data$oneCohUtCorpTax <- as.numeric(gsub("[%]", "", input$oneCohUtCorpTax))
    data$oneCohUtDiscRate <- as.numeric(gsub("[%]", "", input$oneCohUtDiscRate))
    data$oneCohUtTrainCostPer <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$oneCohUtTrainCostPer)))
    data$oneCohUtNumTrainees <- as.numeric(gsub("[%]", "", input$oneCohUtNumTrainees))
    data$oneCohUtDt <- as.numeric(input$oneCohUtDt)
    data$oneCohUtSDy <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$oneCohUtSDy)))
    data$oneCohUtDuration <- as.numeric(input$oneCohUtDuration)
    
    #calculations
    oneCohUtCalc()
    
    #output a results table
    col1 <- "1. Estimated training utility for one cohort"
    
    col2 <- paste0(currSymHTML[data$currency], formatC(store$oneCohUtCalc$oneCohEstUt, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE))
    
    #check if all the calculations result in a valid value and render appropriate results column
    if(!is.null(store$oneCohUtCalc$oneCohEstUt)) {
      
      if(!is.finite(store$oneCohUtCalc$oneCohEstUt)){col2 <- "-"}
      
    } else {
      
      col2 <- "-"
      
    }
    
    return(cbind(col1, col2))
    
  })
  
  #one cohort utility calculations
  oneCohUtCalc <- function() {
    
    print("oneCohUtCalc")
    
    #calculate one cohort utility
    Tax <- data$oneCohUtCorpTax / 100
    delta1 <- 0
    delta2 <- 0
    delta3 <- 0
    tdelta <- 0
    count <- 1
    
    disc <- 1 / (1 + data$oneCohUtDiscRate / 100)
    
    for (i in count:data$oneCohUtDuration)
    {
      delta1 <- (disc^i) * data$oneCohUtDt * data$oneCohUtSDy * (1 + data$oneCohUtVarCosts) + (1 - Tax)
      if (i == 1) {
      delta2 <- data$oneCohUtTrainCostPer * data$oneCohUtNumTrainees * (1 - Tax) * (disc ^ (i - 1))
      # note that the above statement is a little strange because (disc ^ (i - 1)) = disc when i=1...?
      } else {
      delta2 <- 0
      }
      delta3 <- data$oneCohUtNumTrainees * delta1 - delta2
      tdelta <- tdelta + delta3
    }
    #training utility for one cohort
    store$oneCohUtCalc$oneCohEstUt <- tdelta

  }
  
  #show the values in a table
  output$oneCohUtTab <- renderTable({
    
    req(oneCohUtData())
    
  }, align = 'lr', sanitize.text.function = function(x) x)
  
  #multiple cohort utility tab#
  
  #multiple cohort utility UI
  output$multCohUtUI <- renderUI({
    
    print("multCohUtUI")
    
    fluidRow(
      
      column(12,
        
        "Note: Currency is set to ",data$currency,
        
        br(),

        br(),

        textInput("multCohUtVarCosts", "Variable costs (enter as a negative proportion):", isolate(data$multCohUtVarCosts)),
        textInput("multCohUtCorpTax", "Corporate tax (percentage):", paste0(isolate(data$multCohUtCorpTax),"%")),
        textInput("multCohUtDiscRate", "Discount rate (percentage):", paste0(isolate(data$multCohUtDiscRate),"%")),
        textInput("multCohUtOrdSelProcCost", "Ordinary select procedure cost:", paste0(currSym[data$currency],isolate(data$multCohUtOrdSelProcCost))),        
        textInput("multCohUtAltSelProcCost", "Alternate select procedure cost:", paste0(currSym[data$currency],isolate(data$multCohUtAltSelProcCost))),
        textInput("multCohUtSDy", "Standard deviation of job performance:", paste0(currSym[data$currency],isolate(data$multCohUtSDy))),
        textInput("multCohUtOrdProcVal", "Validity of the ordinary procedure:", isolate(data$multCohUtOrdProcVal)),
        textInput("multCohUtAltProcVal", "Validity of the alternative procedure:", isolate(data$multCohUtAltProcVal)),
        textInput("multCohUtSR", "Selection ratio:", isolate(data$multCohUtSR)),
        textInput("multCohUtAveTenure", "The tenure in years of the average selectee:", isolate(data$multCohUtAveTenure)),
        textInput("multCohUtProgDur", "Program designed to last (years):", isolate(data$multCohUtProgDur)),
        textInput("multCohUtYrlyAddEmps", "Number of employees added each year:", isolate(data$multCohUtYrlyAddEmps)),
        div(style="margin-bottom:5px;","Number of employees subtracted each year (after average tenure):"),
        textInput("multCohUtYrlySubEmps", label =  NULL, isolate(data$multCohUtYrlySubEmps))
      
      )
      
    )
    
  })
  
  #one cohort utility inputs
  multCohUtData <- reactive({
    
    print("multCohUtData")
    
    #convert text inputs to numbers and strip symbols
    #multiple cohort utility inputs
    data$multCohUtVarCosts <- as.numeric(input$multCohUtVarCosts)
    data$multCohUtCorpTax <- as.numeric(gsub("[%]", "", input$multCohUtCorpTax))
    data$multCohUtDiscRate <- as.numeric(gsub("[%]", "", input$multCohUtDiscRate))
    data$multCohUtOrdSelProcCost <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$multCohUtOrdSelProcCost)))
    data$multCohUtAltSelProcCost <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$multCohUtAltSelProcCost)))
    data$multCohUtSDy <- as.numeric(gsub(",", "", gsub(paste0("[",currSym[data$currency],"]"), "", input$multCohUtSDy)))
    data$multCohUtOrdProcVal <- as.numeric(input$multCohUtOrdProcVal)
    data$multCohUtAltProcVal <- as.numeric(input$multCohUtAltProcVal)
    data$multCohUtSR <- as.numeric(input$multCohUtSR)
    data$multCohUtAveTenure <- as.numeric(input$multCohUtAveTenure)
    data$multCohUtProgDur <- as.numeric(input$multCohUtProgDur)
    data$multCohUtYrlyAddEmps <- as.numeric(input$multCohUtYrlyAddEmps)
    data$multCohUtYrlySubEmps <- as.numeric(input$multCohUtYrlySubEmps)
    
    #calculations
    multCohUtCalc()
    
    #output a results table
    col1 <- c("1. Overall utility per selectee", 
            "2. Overall utility of the program")
    
    col2 <- c(paste0(currSym[data$currency],formatC(store$multCohUtCalc$multCohUtPer, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)),
            paste0(currSym[data$currency],formatC(store$multCohUtCalc$multCohUtTot, format = "f", digits = 2, big.mark = "," , drop0trailing = FALSE)))
    
    #check if all the calculations result in a valid value and render appropriate results column
    if (all(as.logical(lapply(req(store$multCohUtCalc), is.finite)))){
  
      return(cbind(col1, col2))
  
    } else {
    
      return(cbind(col1, rep("-", 2)))
      
    }
    
  })
  
  #multiple cohort utility calculations
  multCohUtCalc <- function() {
    
    print("multCohUtCalc")
    
    #calculate multiple cohort utility
    nk <- 0
    paytot <- 0
    paysel <- 0
    payselfir <- 0
    number <- 1
    disc <- data$multCohUtDiscRate / 100
    val <- data$multCohUtAltProcVal - data$multCohUtOrdProcVal
    Zsr <- qnorm(1-data$multCohUtSR)
    ord <- dnorm(Zsr,0,1) / data$multCohUtSR
    ck <- data$multCohUtYrlyAddEmps * ((data$multCohUtAltSelProcCost - data$multCohUtOrdSelProcCost)/data$multCohUtSR)
    tax <- data$multCohUtCorpTax / 100
    discRatio <- 1 / (1 + disc)
    
    numyr <- data$multCohUtAveTenure + data$multCohUtProgDur
    
    if (req(input$multCohUtAveTenure) >= req(input$multCohUtProgDur)) {
      pick <- 1
    } else {
      pick <- 2
    }
    
    totdelta <- 0
    for (i in 1:numyr) {
      if (i > req(input$multCohUtAveTenure)) nk <- nk - data$multCohUtYrlySubEmps
      if (i <= req(input$multCohUtProgDur)) nk <- nk + data$multCohUtYrlyAddEmps
      if (i > req(input$multCohUtProgDur)) ck <- 0
      if (nk >= 0) delta1 <- nk * ((discRatio ^ i) * val * ord * (1 + data$multCohUtVarCosts) * (1 - tax))
      delta2 <- ck * (1 - tax) * (discRatio ^ (i - 1))
      delta <- delta1 - delta2
      totdelta <- totdelta + delta
    }

    #training utility for multiple cohorts
    store$multCohUtCalc$multCohUtTot <- totdelta
    store$multCohUtCalc$multCohUtPer <- store$multCohUtCalc$multCohUtTot / (data$multCohUtProgDur*data$multCohUtYrlyAddEmps)

  }
  
  #show the values in a table
  output$multCohUtTab <- renderTable({
    
    req(multCohUtData())
    
  }, align = 'lr', sanitize.text.function = function(x) x)
  
  ###manage data###
  
  #currency select UI
  output$currencyUI <- renderUI({
    
    print("currencyUI")
    
    fluidRow(
      
      column(12,
        
        div(style = "width:250px",
        
          selectInput(
            inputId = "currency",
            label = "Select Currency:", 
            selected = data$currency, 
            multiple = FALSE, 
            choices = c(names(currRat))
            
          )
          
        )

      )
      
    )
    
  })

  #input update function
  updtUI <- function() {
    
    list <- reactiveValuesToList(data)
    
    updateSel1st <- c("absentWorkersArePaid","currency","diffPerfMethod","eBCGSelEcFac","eBCGSelMultCoh","medExamType","meetChargeMethod","meetDevCostMethod","meetFacCostMethod","nayShiCalcSel","orgSubjectToUnempTax")

    updtSel <- function(sel,nam) {updateSelectInput(session, inputId = nam, selected = sel)}
    
    listSel <- lapply(list[names(list) %in% updateSel1st], as.character)
    
    mapply(updtSel,listSel,names(listSel))
    
    currListTxt <- list[store$currSet]
    
    pctListTxt <- list[store$pctSet]
    
    updtPctTxt <- function(val,nam) {updateTextInput(session, inputId = nam, value = paste0(val,"%"))}
    
    mapply(updtPctTxt,pctListTxt,names(pctListTxt))
    
    updtCurrTxt <- function(val,nam) {updateTextInput(session, inputId = nam, value = paste0(currSym[data$currency],val))}
    
    mapply(updtCurrTxt,currListTxt,names(currListTxt))
    
  }
  
  #input global data function
  updtGlob <- function(skipList) {
    
    list <- reactiveValuesToList(data)
    
    globSettingsList <- store$globSet
    
    keepList <- list[names(list) %in% globSettingsList]
    
    keepListFinal <- keepList[! names(keepList) %in% skipList]
    
    updtTxt <- function(val,nam) {updateTextInput(session, inputId = nam, value = val)}  #paste0(currSym[data$currency],val))}

    mapply(updtTxt,keepListFinal,names(keepListFinal))
    
  }

  #download .csv
  output$saveData <- downloadHandler(

    filename = function() {
      paste('IIP', Sys.Date(), '.csv', sep='')
    },
    
    content = function(file) {
      write.csv(as.data.frame(reactiveValuesToList(data)), file, row.names = FALSE)
    }

  )
  
  #upload .csv
  observeEvent(input$uploadFile, {
    
    inFile <- input$uploadFile

    inTable <- read.csv(inFile$datapath, header = TRUE)
    
    lst <- as.list(inTable)
    
    updtDat <- function (val,nam) {data[[nam]] <- val}
    
    mapply(updtDat,lst,names(lst))
    
    updtUI()
    
    absCalc()
    sepCalc() 
    repCalc() 
    traCalc() 
    perfCalc()
    alcCalc()
    eAPCalc()
    engCalc()
    wLifCalc()
    bCGCalc()
    brEvCalc()
    traUtCalc()
    meetCalc()
    oneCohUtCalc()
    multCohUtCalc()
    eBCGCalc()
    CREPIDCalc()
    globEstCalc()
    globEstCalcGraph()
    
    showNotification(paste0("Data file (",inFile$datapath,") loaded succesfully!"))
    
  })
  
  output$currUI <- renderUI({
    
    tagList(
    
      "Note: Currency is set to ",data$currency
      
    )
    
  })
  
  #convert currency on button click
  observeEvent(input$convert, {
    
    data$currOld <- data$currency
    
    data$currency <- input$currency
    
    rates <- currRat

    list <- reactiveValuesToList(data)
  
    cinputs <- store$currSet
  
    list <- list[names(list) %in% cinputs]
    
    names <- names(list)
    
    store$xRate <- rates[data$currency]
    
    converted <- round((rates[data$currency] * (1 / rates[data$currOld]) * as.numeric(list)), 2)
    
    updtCurrency <- function(val,nam) {updateTextInput(session, inputId = nam, value = val)}
    
    updtDat <- function (val,nam) {data[[nam]] <- val}

    mapply(updtCurrency,paste0(currSym[data$currency],converted),names)

    mapply(updtDat,converted,names) 
    
    showNotification(paste0("Currency converted from ",data$currOld," to ",data$currency,"!"))
    
  })
  
  #email sender
  sendEmail <- function(email = "leo.alexander@rice.edu",
                        mail_message = "Hello"){
  
    url <- "https://api.mailgun.net/v3/sandbox664e13975c6a4f5bac7c5db198789ae1.mailgun.org/messages"
    ## username:password so api_key is all after the api:
    api_key <- "c2764d964fa8ac32848072eae59b81fb-2b778fc3-512dd818"
    the_body <-
      list(
        from="Mailgun Sandbox <postmaster@sandbox664e13975c6a4f5bac7c5db198789ae1.mailgun.org>",
        to=email,
        subject="Mailgun from R",
        text=mail_message
      )
  
    req <- httr::POST(url,
                      httr::authenticate("api", api_key),
                      encode = "form",
                      body = the_body)
  
    httr::stop_for_status(req)
    
    TRUE
  
  }
  
  #email send
  observeEvent(input$send, {
    
    if (is.null(input$name)||is.null(input$email)||is.null(input$body)){
    
      showNotification("Message send failed. Please make sure you filled out all form fields and try again.") 
      return()
      
    }
    
    msgContent <- paste("Name: ",as.character(input$name),"Email: ",as.character(input$email),"Reason: ",as.character(input$reason),"Message:\n",as.character(input$body), sep = "\n\n")
    
    x <- sendEmail(mail_message = msgContent)
    
    if (x == TRUE ) {
      
      showNotification("Your message has been sent. Please allow 24 to 48 hours for a response.")
      
    } else {
      
      showNotification("Message send failed. Please check your message and try again.")
      
    }
    
  })
    
}

shinyApp(ui, server)