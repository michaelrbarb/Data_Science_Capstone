#*************************************************************************
#   UI for the Data Science Capstone App
#*************************************************************************

shinyUI(navbarPage(inverse=TRUE,"Coursera Data Science Capstone",
                  tabPanel("Word Prediction App",
                           fluidPage(
                            absolutePanel(top="0%",left="0%",
                            img(src="data_banner7.jpg", width=1700, height=160)),
                            
                            absolutePanel(top="30%",#left="31%",
                            h4("Enter your text below"),
                            p(textInput("word1",label=NULL,value="Data analysis")),
                            br(),
                            h4("Predicted words"),
                            p(verbatimTextOutput("pred"))))),
                            
                   tabPanel("About",
                            fluidPage(
                            absolutePanel(top="29%",left="10%",
                            img(src="blog.jpg",width="28%",height="28%")),
                            absolutePanel(top="29%",left="42%",
                            img(src="newspaper.jpg",width="28%",height="28%")),
                            absolutePanel(top="29%",left="70%",
                            img(src="twitter.jpg",width="50%",height="50%")),
                            absolutePanel(top="50%", left="5%",right="5%",
                            p("This application was created for the Coursera Johns Hopkins Data 
                              Science Capstone Course. The data consists of text files from blogs, 
                              news sources and twitter.  The data is collected from publicly 
                              available sources by a web crawler. The crawler checks for language, 
                              so as to mainly get texts consisting of the desired language (in this                               case Enlish). The application uses 2-gram relationships derived from                               a sample of the forementioned texts to predict the next word. I                                    enjoyed all the JHU Data Science classes and would recommend them to                               anyone interested in the subject. I hope you like the app!"))))))
