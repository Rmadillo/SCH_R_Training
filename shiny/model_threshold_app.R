#######################################################
# Threshold Explorer Shiny App
# Dwight Barry
# 3 November 2016
# Version 0.9.1 
# setwd("C:/Users/DBARR1/Documents/Rcode/shiny")
#######################################################

##### Global #####

# Load Packages
require(shiny)
require(ggplot2)
require(dplyr)
require(ROCR)
require(htmlTable)

# Load Data
#data("ROCR.simple")
#dataset = data.frame(Prediction = ROCR.simple$predictions, Outcome = ROCR.simple$labels)

# ED 2 hour model -- random forest
#dataset = read.csv("edv2.csv", header=T)

# ED 4 hour model -- adaBoost
dataset = read.csv("edv4.csv", header=T)

# Make labels a factor and add a Noise variable for comparison
#dataset$Outcome_Factor = ordered(ifelse(dataset$Outcome == 1, "Yes", "No"))
#set.seed(24)
#dataset$Noise = rbinom(length(ROCR.simple$labels), 1, 0.1)
#dataset$Noise_Factor = ordered(ifelse(dataset$Noise == 1, "Yes", "No"))

#######################################################

##### User Interface #####

ui = shinyUI(fluidPage(
    
    title = "Threshold Explorer",
    br(),
    
    # Top Row
    fluidRow(
        
        # User Input Section
        column(2, style = "background-color:#FFF8E7;", # cosmic latte!
               HTML("<h4>Classification Model<br>Threshold Explorer</h4>"),
               #br(),
               hr(),
               selectInput('x', 'Predictor', names(dataset), 
                           names(dataset)[[2]]),
               selectInput('y', 'Outcome', names(Filter(is.factor, dataset)), 
                           names(dataset)[[1]]),

               br(),
               HTML("placeholder for file upload"),
               br(),
               hr()
        ),
        
        # Comparative Density Plot
        column(3,
               HTML('<b>Comparative Density Plot</b>'),
               plotOutput('cd_plot', height = 350)
        ),
        
        
        # ROC Plot
        column(3, 
               HTML('<b>ROC Plot</b>'),
               plotOutput('roc_plot', height = 350)
        ),
        
        
        # Precision-Recall Plot
        column(3,
               HTML('<b>Precision-Recall Plot</b>'),
               plotOutput('pr_plot', height = 350)
        )

    ),       
    
    # Bottom Row
    
    fluidRow(
        
        # Logo column
        column(2, style = "background-color:#FFF8E7;", 
               numericInput('threshold', 'Select Threshold',
                            value = 0.50,
                            min = 0.00,
                            step = 0.01,
                            max = 1.00),
               numericInput('fp', 'Select False Positive Cost',
                            value = 1,
                            min = 0,
                            step = 1),
               numericInput('fn', 'Select False Negative Cost',
                            value = 1,
                            min = 0,
                            step = 1),
               hr(),
               br(),
               HTML('<b>Seattle Children\'s<br>Enterprise Anaytics</b>'),
               HTML('<br><br>Questions? <br>Contact <a href="mailto:dwight.barry@seattlechildrens.org?Subject=Threshold Explorer App">Dwight Barry</a></p>'),
               br(),
               HTML('<p><a href="https://en.wikipedia.org/wiki/Confusion_matrix" target="_blank">Overview of Classification Metrics</a></p>')
        ),

        # Cost Curve
        column(3,
               br(),
               HTML('<b>Cost Curve</b>'),
               plotOutput("cost_curve", height = 300)
               
        ),  
                
        # Decision Metrics
        column(3, 
               br(),
               HTML("<b>Threshold Results</b>"),
               htmlOutput("thresh_summary")
        ),
        
        # Confusion Matrices
        column(3,
               br(),
               HTML("<b>Confusion Matrix</b><br>"),
               HTML("<br>Proportions:"),
               tableOutput("confu_tab2"),
               HTML("<br>Counts:"),
               tableOutput("confu_tab1")
        )
        
        
    )
)
)

#######################################################

##### Server #####

server = shinyServer(function(input, output) {
    
    # ROC calcs and plot
    output$roc_plot = renderPlot({
        
        eval = prediction(dataset[,input$x],dataset[,input$y])
        
        sens_perf = performance(eval, "sens", "fpr")
        thresh = sens_perf@alpha.values[[1]]
        sens = sens_perf@y.values[[1]]
        fpr = sens_perf@x.values[[1]]
        
        roc_frame = round(data.frame(thresh, sens, fpr), 2)
        roc_sub = roc_frame[seq(2, nrow(roc_frame), by = 50), ]
        
        auc_perf = performance(eval, measure = "auc")
        
        rp = ggplot(roc_frame, aes(x = fpr, y = sens)) + 
            geom_abline(intercept = 0, slope = 1, color = "lightgray") +
            geom_line() +
            xlab("False Positive Rate (1-Specificity)") +
            ylab("True Positive Rate (Sensitivity)") +
            geom_label(data = roc_sub, aes(x = fpr, y = sens, label = thresh), 
                       alpha = 0.8) +
            geom_label(x = 0.8, y = 0.01, alpha = 0.8, label.size = 0,
                       label = paste0("AUC value: ", round(auc_perf@y.values[[1]], 2))) +
            geom_label(data = head(filter(roc_frame, thresh == input$threshold), 1),
                       aes(x = fpr, y = sens, label = thresh), color = "blue", size = 5,
                       fontface = "bold") +
            theme_bw()
        
        print(rp)
        
    })
    
    # Precision-recall calcs and plot
    output$pr_plot = renderPlot({
        
        eval = prediction(dataset[,input$x],dataset[,input$y])
        
        prec_perf = performance(eval, "prec", "rec")
        
        thresh = prec_perf@alpha.values[[1]]
        prec = prec_perf@y.values[[1]]
        rec = prec_perf@x.values[[1]]
        
        pr_frame = round(data.frame(thresh, prec, rec), 2)
        pr_sub = pr_frame[seq(2, nrow(pr_frame), by = 50), ]
        
        pr = ggplot(pr_frame, aes(x = rec)) +
            labs(x = "True Positive Rate (Sensitivity)", y = "Precision (PPV)") +
            geom_line(aes(y = prec), na.rm = T) +
            geom_label(data = pr_sub, aes(x = rec, y = prec, label = thresh), 
                      alpha = 0.8) +
            geom_label(data=head(filter(pr_frame, thresh == input$threshold), 1),
                       aes(x = rec, y = prec, label = thresh), color = "blue", size = 5,
                       fontface = "bold") +
            theme_bw()
        
        print(pr)
        
    })
    
    # Classification metrics calcs
    output$thresh_summary = renderText({
        
        eval = prediction(dataset[,input$x],dataset[,input$y])
        
        sens_perf = performance(eval, measure="sens")
        fpr_perf = performance(eval, measure="fpr")
        fnr_perf = performance(eval, measure="fnr")
        spec_perf = performance(eval, measure="spec")
        prec_perf = performance(eval, measure="prec")
        npv_perf = performance(eval, measure="npv")
        acc_perf = performance(eval, measure="acc")
        odds_perf = performance(eval, measure="odds")
        lift_perf = performance(eval, measure="lift")
        
        thresh = round(sens_perf@x.values[[1]], 2)
        sens = sens_perf@y.values[[1]]
        fpr = fpr_perf@y.values[[1]]
        fnr = fnr_perf@y.values[[1]]
        spec = spec_perf@y.values[[1]]
        prec = prec_perf@y.values[[1]]
        npv = npv_perf@y.values[[1]]
        acc = acc_perf@y.values[[1]]
        odds = odds_perf@y.values[[1]]
        lift = lift_perf@y.values[[1]]
        
        thresh_frame = data.frame(thresh, sens, fpr, fnr, spec, prec, npv, acc, odds, lift)
        outy = head(thresh_frame[which(thresh_frame$thres == input$threshold),], 1)
        
        prbe_perf = performance(eval, measure="prbe")
        prbe = round(data.frame(prbe_perf@x.values[[1]], prbe_perf@y.values[[1]]), 2)
        
        ssbe = performance(eval, "sens", "spec")
        
        # Balance accuracy
        pred_val = as.factor(ifelse(dataset[,input$x] < input$threshold,
                                    "Predict: Green/Yellow", "Predict: Orange/Red"))
        pred_val = ordered(pred_val, levels = c("Predict: Orange/Red", 
                                                "Predict: Green/Yellow"))
        real_val = dataset[,input$y]
        real_val = ordered(real_val, levels = c("Orange/Red", "Green/Yellow"))
        real_val_table = table(pred_val, real_val)
        first_row = real_val_table[1,1] / (real_val_table[1,1] + real_val_table[1,2])  
        second_row = real_val_table[2,2] / (real_val_table[2,1] + real_val_table[2,2])  
        bal_acc = (first_row + second_row) / 2
        
        # Return values
        paste0("<br>Sensitivity and specificity are maximized at a threshold of ",
               round(ssbe@alpha.values[[1]][which.max(ssbe@x.values[[1]]+ssbe@y.values[[1]])], 2), ".<br>",
               "<br>Precision and recall break-even (", round(max(prbe[2]), 2),
                    ") occurs at a threshold of ", round(min(prbe[1]), 2), ".<br>", 
               
               "<br><i>Given a threshold of ", outy$thresh, 
                    ", the classification metrics are:</i>",
               "<br><br>True Positive Rate (Sensitivity): ", round(outy$sens,2),
               "<br>False Negative Rate: ", round(outy$fnr, 2), 
               "<br>False Positive Rate: ", round(outy$fpr, 2), 
               "<br>True Negative Rate (Specificity): ", round(outy$spec,2),
               "<br>Positive Predictive Value (Precision): ", round(outy$prec,2),
               "<br>Negative Predictive Value: ", round(outy$npv,2),
               "<br>Raw Accuracy: ", round(outy$acc,2),
               "<br>Balanced Accuracy: ", round(bal_acc,2),
               "<br>Odds Ratio: ", round(outy$odds,2),
               "<br>Lift: ", round(outy$lift,1)
               )
        
    })
    
    # Comparative density plot
    output$cd_plot = renderPlot({

        cd = ggplot(data = dataset, aes_string(x = input$x)) + 
            #geom_histogram(aes_string(y="..density..", color = input$y,
            #     fill = input$y), position="identity", alpha=0.1, 
            #     binwidth=0.05, na.rm = T) + 
            geom_density(aes_string(color = input$y,
                linetype = input$y), na.rm = T) + 
            geom_vline(aes(xintercept = input$threshold), linetype="dashed",
                color = "blue") +
            theme_bw() +
            theme(legend.position = "top", 
                  axis.ticks.y = element_blank(),
                  axis.text.y = element_blank())
        
        print(cd)
        
    })
    
    # Confusion matrix table for counts
    output$confu_tab1 = renderTable({
        pred_val = as.factor(ifelse(dataset[,input$x] < input$threshold,
            "Predict: Green/Yellow", "Predict: Orange/Red"))
        pred_val = ordered(pred_val, levels = c("Predict: Orange/Red", 
                                                "Predict: Green/Yellow"))
        real_val = dataset[,input$y]
        real_val = ordered(real_val, levels = c("Orange/Red", "Green/Yellow"))
        txtRound(addmargins(table(pred_val, real_val)), 0)
    }, align='rrrr')
    
    # Confusion matrix table for proportions
    output$confu_tab2 = renderTable({
        pred_val = as.factor(ifelse(dataset[,input$x] < input$threshold,
            "Predict: Green/Yellow", "Predict: Orange/Red"))
        pred_val = ordered(pred_val, levels = c("Predict: Orange/Red", 
                                        "Predict: Green/Yellow"))
        real_val = dataset[,input$y]
        real_val = ordered(real_val, levels = c("Orange/Red", "Green/Yellow"))
        addmargins(prop.table(table(pred_val, real_val)))
    })
    
    # Cost curve calcs and plot
    output$cost_curve = renderPlot({
        
        eval = prediction(dataset[,input$x], dataset[,input$y])
        
        cost_perf = performance(eval, measure = "cost", cost.fp = input$fp,
             cost.fn = input$fn)
        
        thresh = cost_perf@x.values[[1]]
        cost = cost_perf@y.values[[1]]
        
        cost_frame = data.frame(thresh, cost)
        
        cp = ggplot(data = cost_frame, aes(x = thresh, y = cost)) + 
            geom_line(na.rm = T) + 
            xlab(input$x) +
            ylab("Cost") +
            geom_vline(aes(xintercept = input$threshold), linetype = "dashed",
                color="blue") +
            theme_bw()
        
        print(cp)
        
    })
    
})

#######################################################

# Return the Shiny app
shinyApp(ui = ui, server = server)

#######################################################
