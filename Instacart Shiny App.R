answer = function(data) {
  data2 = data[1:5,]
  return(data2) 
}


df<-read.csv("Output (9).csv")#,sep="|")
final_prod_df<-read.csv("Final_Products (3).csv")#, sep = "|")
df = na.omit(df)
final_prod_df$product_name = gsub('"','',final_prod_df$product_name)
#df = df[3:nrow(df),]
#df = order(df,decreasing = TRUE,c("support","confidence"))
#?order


library(shiny)

ui <- navbarPage(
  title = img(src='Instacart.jpg', width = "250px", height = "48px", style="margin-top: -14px; margin-right:-14px;margin-left:-14px", height = 50),
  tabPanel(
    title="Product Recommender for Instacart",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          selectizeInput("select",label = "Select previously purchased items",choices = sort(final_prod_df$product_name)),
          actionButton("submit", "Let's Go!")
        ),#SideBArPanel
        mainPanel(tableOutput("table1")
                  #verbatimTextOutput("prod_name")
        )
      )#SideBarLayout
    )#FluidPage
  ), 
  tabPanel(
    title = "Visualization of Association Rules",   fluidRow(
      column(2),
      column(3, img(src='scatterplot.jpeg', width="300%", align="center")),
      column(10, img(src='twokeyplot.jpeg', width="60%", align="center")))),# tab 1 panel
  tabPanel(title="About", # panel 2 
           fluidRow(
             column(5),
             column(2,img(src='KrannertPurdue.png', width="100%", align="center")),
             column(5)
           ), # first row
           fluidRow(
             h3("This project is developed by 2020 MS BA-IM & MS MKT students from Krannert School of Management at Purdue University.", align="center")
           ), # second row
           fluidRow(
             column(1),
             column(2, img(src='profile.jpg', width="100%", align="center")),
             column(2, img(src='karan.jpg', width="100%", align="center")),
             column(2, img(src='blair.jpg', width="100%", align="center")),
             column(2, img(src='bryan.jpg', width="100%", align="center")),
             column(2, img(src='jason.jpg', width="100%", align="center")),
             column(1)
           ), # third row
           fluidRow(
             column(1),
             column(2, h5("Joeary Long is a 2020 Purdue MS- Marketing student having interests in Marketing Analytics and Digital Marketing domains. Her expertise is in digital marketing analytics and she is also interested to work in consumer good and digital marketing industry.")),
             column(2, h5("Karan Doshi is a 2020 Purdue MS- Marketing student who had two years of full-time experience as a Business development associate with Life Insurance Corporation of India. His Short term goal is to work as a Marketing and Consumer analyst.")),
             column(2, h5("Blair Banker is a 2020 Masters Student in Business Analytics and Information Management interested in technology consulting. He has a background in financial advising and cybersecurity consulting. He is currently open to various opportunities.")),
             column(2, h5("Bryan Winovich is a 2020 Masters Student in Business Analytics at Krannert School of Management at Purdue University with an interest in Data Science, and a background in Math, Statistics, and Economics via an Actuarial Science degree at The Ohio State University.")),
             column(2, h5("Jason Pastoor is a 2020 MS BAIM student at the Krannert School of Management at Purdue University. He graduated from Transylvania University in 2019 with a double major in mathematics and economics. He is primarily interested in sports analytics with a focus on baseball and football.")),
             column(1)
           ), # forth row
           fluidRow(
             column(1),
             column(2, h5("Get to know more about her:",
                          uiOutput("herejoeary",align="center"))),
             column(2, h5("Get to know more about him:",
                          uiOutput("herekaran",align="center"))),
             column(2, h5("Get to know more about him:",
                          uiOutput("hereblair",align="center"))),
             column(2, h5("Get to know more about him:",
                          uiOutput("herebyran",align="center"))),
             column(2, h5("Get to know more about him:",
                          uiOutput("herejason",align="center"))),
             column(1)
           ) # fifth row
           
  ) # tab 2 panel
  
  
)#NavBarPage

server<-function(input,output,server){
  observeEvent(input$submit,{
    prod_name<-input$select
    library(data.table)
    #library(DT)
    df_subset<-df[grepl(prod_name,df$LHS),"RHS"]
    df_subset2 = data.frame(head(df_subset,5))
    #df_subset2 = datatable(df_subset2,colnames="Answer")
    #df_subset2 = answer(df_subset)
    
    #df_subset = arrange(df_subset, desc(confidence),desc(support) )
    #df_subset = df_subset[1:5,]
    #df_subset2 
    #df_subset = order(df_subset)
    #names(df_subset)<-"Things you might be interested in"
    #df_subset<-df[df$LHS==input$select,"RHS"]
    names(df_subset2)[1] = "Recommendations"
    output$table1<-renderTable({df_subset2})
    
    
  })
  url_joeary <- a("here",href="https://www.linkedin.com/in/zhongrui-long-2b3849153/")
  output$herejoeary <- renderUI({
    tagList(url_joeary)
  })
  url_karan <- a("here",href="https://www.linkedin.com/in/karandoshi96/")
  output$herekaran <- renderUI({
    tagList(url_karan)
  })
  url_blair <- a("here",href="https://www.linkedin.com/in/blair-banker-b68a7151/")
  output$hereblair <- renderUI({
    tagList(url_blair)
  })
  url_bryan <- a("here",href="https://www.linkedin.com/in/bryan-winovich-b90a8610b/")
  output$herebyran <- renderUI({
    tagList(url_bryan)
  })
  url_jason <- a("here",href="https://www.linkedin.com/in/jason-pastoor-6329a815b/")
  output$herejason <- renderUI({
    tagList(url_jason)
  })#ObserveEvent
  
  
  
}

shinyApp(ui = ui,server = server)