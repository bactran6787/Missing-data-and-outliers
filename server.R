#Author: Bac Tran
#This is the user-interface definition of a Shiny web application. You can
#run the application by clicking 'Run App' above.
#

shinyServer(function(input, output, session) {
  
  pMiss <- function(x){ sum(is.na(x)) / length(x) * 100 }
  
  getData <- shiny::reactive({
    read.csv("CovidData.csv", header = TRUE, stringsAsFactors = TRUE)
  })
  
  getRecipe <- reactive({
    dat <- getclean()
    dat_train <- dat[dat$OBS_TYPE == "Train",]
    rec <- recipes::recipe(DEATH_RATE ~., data = dat_train ) %>%
      update_role("CODE", new_role = "id") %>%   #id is not a predictor
      update_role("OBS_TYPE", new_role = "split") %>%  #obs_type is not a predictor
      step_impute_knn(all_predictors(), neighbors = input$knnnumber, skip = !input$knn) %>%
      step_center(all_numeric(), -has_role("outcome"), skip = !input$Center)%>%
      step_scale(all_numeric(), -has_role("outcome"), skip = !input$Scale) %>%
      step_dummy(all_predictors(), -all_numeric())
    rec
  })
  
  correct_na <- shiny::reactive({
      dat <- getData()
      dat[dat == -99] <- NA
      dat$GOVERN_TYPE <- as.character(dat$GOVERN_TYPE) #convert away from factor
      dat[dat == "--"] <- NA
      dat$GOVERN_TYPE <- as.factor(dat$GOVERN_TYPE) # convert back to factor
      dat
      })

  getclean <- shiny::reactive({
      dat <- getData() #this is another reactive expression
      dat[dat == -99] <- NA
      dat$GOVERN_TYPE <- as.character(dat$GOVERN_TYPE) #convert away from factor
      dat[dat == "--"] <- NA
      dat$GOVERN_TYPE <- as.factor(dat$GOVERN_TYPE) # convert back to factor
     
      #process columns
      vRatio <- apply(dat,2,pMiss)
      dat <- dat[, vRatio < input$VarThresh]
      
      #process rows
      oRatio <- apply(dat,1,pMiss) 
      dat <- dat[oRatio < input$ObsThresh, ]
      
      #Remove detected outliers in first run
      if (input$outlierdelete == TRUE){
       dat <- dat[-c(10, 44, 81, 144, 145, 156, 168, 235, 247, 256, 312, 352, 396, 409),] 
      }
      
      #Remove missingness the top 5 important variables
      if (input$top5delete == TRUE){
        dat <- dat[!is.na(dat$POP_DENSITY + dat$DOCS + dat$AGE50_PROPTN + dat$GDP + dat$VAX_RATE),]
      }
    
      #Remove missingness the top 2 important variables
      if (input$top2delete == TRUE){
        dat <- dat[!is.na(dat$POP_DENSITY + dat$DOCS),]
      }
      
      #Initial Imputation
      if (input$Impuhealthcost == TRUE){
        dat$NUM_SHADOW <- as.numeric(is.na(dat$HEALTHCARE_COST)) # create a shadow variable
        dat$HEALTHCARE_COST[is.na(dat$HEALTHCARE_COST)] <- 0 #Assign missing to zero
      }
      if (input$ImpuGovern == TRUE){
        dat$GOVERN_TYPE <- as.character(dat$GOVERN_TYPE) #convert away from factor
        dat[dat == "NA"] <- "none"
        dat$GOVERN_TYPE <- as.factor(dat$GOVERN_TYPE) # convert back to factor
      }
      dat
      })
  
  #Display removed variables by threshold
  output$RemoveVar <- renderPrint({
      dat <- correct_na()
      vRatio <- apply(dat,2,pMiss)
      cat("Variables to remove:", paste(colnames(dat)[vRatio > input$VarThresh], collapse = ","))
    })
  
  #Display removed observations by threshold
  output$RemoveObs1 <- renderPrint({
      dat <- correct_na()
      oRatio <- apply(dat,1,pMiss)
      cat("Observations to remove (First 50) :", paste(head(rownames(dat)[oRatio > input$ObsThresh], n = 50), collapse = ", "))
    })
    
    #Display removed observations by detected outliners
    output$RemoveObs2 <- renderPrint({
      dat <- correct_na()
      if (input$outlierdelete == TRUE) {
        cat("Observations to remove (First 50) : 10, 44, 81, 144, 145, 156, 168, 235, 247, 256, 312, 352, 396, 409")
      }
    })
    
    #Display removed observations by missing top 5 Var
    output$RemoveObs3 <- renderPrint({
      dat <- correct_na()
      if (input$top5delete == TRUE) {
        cat("Observations to remove (First 50) :", 
            paste(head(rownames(dat)[is.na(dat$POP_DENSITY  +dat$DOCS + dat$AGE50_PROPTN 
                                           + dat$GDP + dat$VAX_RATE)], n = 50), collapse = ", "))
      }
    })
  
    #Display removed observations by missing top 2 Var
    output$RemoveObs4 <- renderPrint({
      dat <- correct_na()
      if (input$top2delete == TRUE) {
        cat("Observations to remove (First 50) :", 
            paste(head(rownames(dat)[is.na(dat$POP_DENSITY + dat$DOCS)], n = 50), collapse = ", "))
      }
    })
  
  #Display first prepossessed data
  output$newdata <- renderPrint({
      dat <- correct_na()
      vRatio <- apply(dat,2,pMiss)
      dat <- dat[, vRatio < input$VarThresh]
      oRatio <- apply(dat,1,pMiss)
      dat <- dat[oRatio < input$ObsThresh, ]
      #Remove detected outliers in first run
      if (input$outlierdelete == TRUE){
        dat <- dat[-c(10, 44, 81, 144, 145, 156, 168, 235, 247, 256, 312, 352, 396, 409),] 
      }
      #Remove missingness the top 5 important variables
      if (input$top5delete == TRUE){
        dat <- dat[!is.na(dat$POP_DENSITY + dat$DOCS + dat$AGE50_PROPTN + dat$GDP + dat$VAX_RATE),]
      }
      #Remove missingness the top 2 important variables
      if (input$top2delete == TRUE){
        dat <- dat[!is.na(dat$POP_DENSITY + dat$DOCS),]
      }
      if (input$Impuhealthcost == TRUE){
        dat$NUM_SHADOW <- as.numeric(is.na(dat$HEALTHCARE_COST)) # create a shadow variable
        dat$HEALTHCARE_COST[is.na(dat$HEALTHCARE_COST)] <- 0 #Assign missing to zero
      }
      if (input$ImpuGovern == TRUE){
        dat$GOVERN_TYPE <- as.character(dat$GOVERN_TYPE) #convert away from factor
        dat[dat == "NA"] <- "none"
        dat$GOVERN_TYPE <- as.factor(dat$GOVERN_TYPE) # convert back to factor
      }
      str(dat)
  })
  
    #print model summary
    output$gmlnetprocess <- renderPrint({
        req(input$Go)
        isolate({
        dat <- getclean()
        dat_train <- dat[dat$OBS_TYPE == "Train",]
        dat_test <- dat[dat$OBS_TYPE == "Test",]
        rec <- getRecipe()
        glm_model <- caret::train(rec, data = dat_train, method ="glmnet")
        print(glm_model)
        })
      })
    
    #Variable Importance
    output$Importance <- renderPlot({
        req(input$Go)
        isolate({
        dat <- getclean()
        dat_train <- dat[dat$OBS_TYPE == "Train",]
        dat_test <- dat[dat$OBS_TYPE == "Test",]
        rec <- getRecipe()
        glm_model <- caret::train(rec, data = dat_train, method ="glmnet")
        plot(caret::varImp(glm_model)) 
          })
        })
    
    #Plot scatter plot for the outcome
    output$glmnetmodel <- renderPlot({
        req(input$Go)
        isolate({
        dat <- getclean()
        dat_train <- dat[dat$OBS_TYPE == "Train",]
        dat_test <- dat[dat$OBS_TYPE == "Test",]
        rec <- getRecipe()
        glm_model <- caret::train(rec, data = dat_train, method ="glmnet")
        dat_test$Prediction <- predict(glm_model, newdata = dat_test)
        # scatter plot
        rang <- range(c(dat_test$DEATH_RATE, dat_test$Prediction))
        ggplot(data = dat_test) +
          geom_point(mapping = aes(x = Prediction, y = DEATH_RATE)) +
          geom_abline(slope = 1, col = "blue") +
          labs(title = "Death rate predictions of COVID test data", y = "predicted", x = "actual") +
          coord_fixed(ratio = 1, xlim = rang, ylim = rang, expand = TRUE)
        })
      })
    
    #Print Test RMSE
    output$TestRMSE <- renderPrint({
        req(input$Go)
        isolate({
          dat <- getclean()
          dat_train <- dat[dat$OBS_TYPE == "Train",]
          dat_test <- dat[dat$OBS_TYPE == "Test",]
          rec <- getRecipe()
          glm_model <- caret::train(rec, data = dat_train, method ="glmnet")
          dat_test$Prediction <- predict(glm_model, newdata = dat_test)
          rmse_test <- rmse(dat_test$DEATH_RATE, dat_test$Prediction)
          print(paste("The test-RMSE is", rmse_test))
        })
      })
    
    #Plot Residual
    output$residualboxplot <- renderPlot({
        req(input$Go)
        isolate({
          dat <- getclean()
          dat_train <- dat[dat$OBS_TYPE == "Train",]
          dat_test <- dat[dat$OBS_TYPE == "Test",]
          rec <- getRecipe()
          glm_model <- caret::train(rec, data = dat_train, method ="glmnet")
          #Prediction
          dat_train$Prediction <- predict(glm_model, newdata = dat_train)
          dat_test$Prediction <- predict(glm_model, newdata = dat_test)
          dat$Prediction <- predict(glm_model, newdata = dat)
          #RMSE
          rmse_train <- rmse(dat_train$DEATH_RATE, dat_train$Prediction)
          rmse_test <- rmse(dat_test$DEATH_RATE, dat_test$Prediction)
          rmse_all <- rmse(dat$DEATH_RATE, dat$Prediction)
          #Residual calculation
          dat_train$Residual <- dat_train$DEATH_RATE - dat_train$Prediction
          dat_test$Residual <- dat_test$DEATH_RATE - dat_test$Prediction
          dat$Residual <- dat$DEATH_RATE - dat$Prediction
        
          #Create new data frame for residual
          all_redisual <- data.frame(dat[,c("CODE", "Residual")])
          train_redisual  <- data.frame(dat_train[,c("CODE", "Residual")])
          test_redisual  <- data.frame(dat_test[,c("CODE", "Residual")])
          
          #Merge all residual data
          data_residual1 <- full_join(all_redisual,train_redisual, by="CODE" )
          data_residual <- full_join(data_residual1,test_redisual, by="CODE" )
          colnames(data_residual) <- c("CODE", "All_Residual","Train_Residual" , "Test_Residual")
          
          data_box <- as.matrix(data_residual[-c(1)])
          
          car::Boxplot(y = data_box, ylab = NA, use.cols = TRUE, notch = FALSE, varwidth = FALSE,
                        horizontal = FALSE, outline = TRUE,
                        col = brewer.pal(n = dim(data_box)[2], name = "RdBu"),
                        range = input$re_range, main = "Boxplots of Residual",
                        id = list(n=50, labels=data_residual$CODE, location="avoid"))
          })          
        })
    
    #Residual Table
    output$Residualdata <- DT::renderDataTable({
        dat <- getclean()
        dat_train <- dat[dat$OBS_TYPE == "Train",]
        dat_test <- dat[dat$OBS_TYPE == "Test",]
        rec <- getRecipe()
        glm_model <- caret::train(rec, data = dat_train, method ="glmnet")
        #Prediction
        dat_train$Prediction <- predict(glm_model, newdata = dat_train)
        dat_test$Prediction <- predict(glm_model, newdata = dat_test)
        dat$Prediction <- predict(glm_model, newdata = dat)
        #Residual calculation
        dat_train$Residual <- dat_train$DEATH_RATE - dat_train$Prediction
        dat_test$Residual <- dat_test$DEATH_RATE - dat_test$Prediction
        dat$Residual <- dat$DEATH_RATE - dat$Prediction
  
        #Create new data frame for residual
        all_redisual <- data.frame(dat[,c("CODE", "Residual")])
        train_redisual  <- data.frame(dat_train[,c("CODE", "Residual")])
        test_redisual  <- data.frame(dat_test[,c("CODE", "Residual")])
        
        #Merge all residual data by "CODE"
        data_residual <- full_join(all_redisual,train_redisual, by="CODE" )
        data_residual <- full_join(data_residual,test_redisual, by="CODE" )
        colnames(data_residual) <- c("CODE", "All_Residual","Train_Residual" , "Test_Residual")
        
        #Join the initial data by "CODE"
        data_residual_fullinfo <- full_join(data_residual, dat[,!(names(dat) %in% c("Residual"))], by="CODE" )
    
        DT::datatable(data = as.data.frame(data_residual_fullinfo))
        })
    
      
    output$Summary <- renderUI({
      Data <- correct_na()
      summary <- summarytools::dfSummary(Data, graph.magnif = 1.5)
      summarytools::view(summary, 
                         method = 'render',
                         report.title = NA,
                         headings = FALSE,
                         bootstrap.css = FALSE,
                         footnote = NA,
                         max.tbl.height = 600,
                         collapse = TRUE,
                         silent = TRUE)
      })
    
    output$Rawdata <- DT::renderDataTable({
      DT::datatable(data = as.data.frame(correct_na()))
    })
    
    output$Missing <- renderPlot({
      visdat::vis_miss(getclean(), cluster = input$cluster) +
        labs(title = "Missingness of Assignment 1 data")
    })
    
    output$Tree <- renderPlot({
      dat <- correct_na()
      dat$MISSINGNESS <- apply(X = is.na(dat), MARGIN = 1, FUN = sum)
      tree <- caret::train(MISSINGNESS ~ .-CODE -OBS_TYPE, data = dat, method = "rpart", na.action = na.rpart)
      rpart.plot(tree$finalModel, main = "Predicting the number of missing variables in an observation", roundint = TRUE, clip.facs = TRUE)
    })
    
    
    output$Missupset <- renderPlot({
      naniar::gg_miss_upset(data = correct_na(), nsets = 6)
    })
    
    output$MixedPairs <- renderPlot({
      req(input$Go_pair)
      isolate({
      dat <- correct_na()
      cat_cols <- c(input$VariablesD)
      colour_1 <- c(input$VariablesE)
      num_cols <- c(input$VariablesF)
      data_mixed <- data.frame(dat[,cat_cols], dat[,num_cols])
      GGally::ggpairs(data = data_mixed,  mapping = ggplot2::aes(colour = dat[,colour_1]),
                      columnLabels = c(cat_cols, num_cols),
                      title = "Pairs of Assignment 1 data")
      })
    })

})
