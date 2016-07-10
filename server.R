options(shiny.maxRequestSize=30*1024^2)
library(shiny)
library(data.table)
library(NLP)
library(tm)

Dataset <- fread("NGramData.txt")
setkeyv(Dataset, c('w1', 'w2', 'w3', 'w4', 'freq'))

translateInput <- function(Text){
        modInput <- tolower(Text)
        modInput <- stripWhitespace(modInput)
        modInput <- gsub("[^\\p{L}\\s]+", "", modInput, ignore.case=F, perl=T)
        return(modInput)
}

splitTranslateInput <- function(Text){
        modInput <- tolower(Text)
        modInput <- stripWhitespace(modInput)
        modInput <- gsub("[^\\p{L}\\s]+", "", modInput, ignore.case=F, perl=T)
        splitTransInput <- unlist(strsplit(modInput, " "))
        return(splitTransInput)
}

wordCount1 <- function(TextInputA){
        NgramsTable <<- Dataset[list("<s>", TextInputA[1])]
        NgramsTable <<- NgramsTable[NgramsTable$w3!="<s>", ]
        NgramsTable <<- NgramsTable[order(NgramsTable$freq, decreasing=TRUE), ]
        AlternativeGuess <<- as.data.frame(NgramsTable)
        AlternativeGuess <<- AlternativeGuess[1:5, c("w3", "freq")]
        AlternativeGuess <<- AlternativeGuess[!is.na(AlternativeGuess$freq), ]
        AlternativeGuess <<- AlternativeGuess[!duplicated(AlternativeGuess), ]
        if(nrow(AlternativeGuess)==0){
                AlternativeGuess <<- data.frame(Word=NA, Frequency=NA)
                }
        else{
                AlternativeGuess$freq <- round(AlternativeGuess$freq/sum(AlternativeGuess$freq)*100, 1)
                AlternativeGuess <<- AlternativeGuess
                colnames(AlternativeGuess) <<- c("Word", "Frequency")
                rownames(AlternativeGuess) <<- NULL
        }
        guessOutput <- NgramsTable$w3[1]
        if(is.na(guessOutput)|is.null(guessOutput)){
                guessOutput <- "Error! Please recheck the spelling of the word or phrase that was input or try another one."
        }
        return(guessOutput)
}

wordCount2 <- function(TextInputB){
        NgramsTable <<- Dataset[list("<s>", TextInputB[1], TextInputB[2])]
        NgramsTable <<- NgramsTable[NgramsTable$w4!="<s>", ]
        NgramsTable <<- NgramsTable[order(NgramsTable$freq, decreasing=TRUE), ]
        AlternativeGuess <<- as.data.frame(NgramsTable)
        AlternativeGuess <<- AlternativeGuess[1:5, c("w4", "freq")]
        AlternativeGuess <<- AlternativeGuess[!is.na(AlternativeGuess$freq), ]
        AlternativeGuess <<- AlternativeGuess[!duplicated(AlternativeGuess), ]
        if(nrow(AlternativeGuess)==0){
                AlternativeGuess <<- data.frame(Word=NA, Frequency=NA)
                }
        else{
                AlternativeGuess$freq <- round(AlternativeGuess$freq/sum(AlternativeGuess$freq)*100, 1)
                AlternativeGuess <<- AlternativeGuess
                colnames(AlternativeGuess) <<- c("Word", "Frequency")
                rownames(AlternativeGuess) <<- NULL
        }
        guessOutput <- NgramsTable$w4[1]
        if(is.na(guessOutput)|is.null(guessOutput)){
                guessOutput <- wordCount1(TextInputB[2])
        }
        return(guessOutput)
}

wordCount3 <- function(TextInputC){
        NgramsTable <<- Dataset[list("<s>", TextInputC[1], TextInputC[2], TextInputC[3])]
        NgramsTable <<- NgramsTable[NgramsTable$w5!="<s>", ]
        NgramsTable <<- NgramsTable[order(NgramsTable$freq, decreasing=TRUE), ]
        AlternativeGuess <<- as.data.frame(NgramsTable)
        AlternativeGuess <<- AlternativeGuess[1:5, c("w5", "freq")]
        AlternativeGuess <<- AlternativeGuess[!is.na(AlternativeGuess$freq), ]
        AlternativeGuess <<- AlternativeGuess[!duplicated(AlternativeGuess), ]
        if(nrow(AlternativeGuess)==0){
                AlternativeGuess <<- data.frame(Word=NA, Frequency=NA)
        }
        else{
                AlternativeGuess$freq <- round(AlternativeGuess$freq/sum(AlternativeGuess$freq)*100, 1)
                AlternativeGuess <<- AlternativeGuess
                colnames(AlternativeGuess) <<- c("Word", "Frequency")
                rownames(AlternativeGuess) <<- NULL
        }
        
        guessOutput <- NgramsTable$w5[1]
        if(is.na(guessOutput)|is.null(guessOutput)){
                shortenedInput <- c(TextInputC[2], TextInputC[3])
                guessOutput <- wordCount2(shortenedInput)
                if(is.na(guessOutput)|is.null(guessOutput)){
                        guessOutput <- wordCount1(TextInputC[3])
                }
        }
        return(guessOutput)
}

# Server File
shinyServer(function(input, output){
        output$Original <- renderText({
                originalInput <- input$obs
                return(originalInput)
                })
        
        output$Translated <- renderText({
                originalInput <- input$obs
                translatedInput <- translateInput(originalInput)
                return(translatedInput)
                })
        
        output$BestGuess <- renderText({
                originalInput <- input$obs
                translatedInput <- translateInput(originalInput)
                bestGuessOutput <- "Predicted Word."
                splitTransInput <- splitTranslateInput(originalInput)
                wordCount <- length(splitTransInput)
                if(wordCount==1){
                        bestGuessOutput <- wordCount1(splitTransInput)
                }
                if(wordCount==2){
                        bestGuessOutput <- wordCount2(splitTransInput)
                }
                if(wordCount==3){
                        bestGuessOutput <- wordCount3(splitTransInput)
                }
                if(wordCount > 3){
                        wordsToSearch <- c(splitTransInput[wordCount - 2],
                                           splitTransInput[wordCount - 1],
                                           splitTransInput[wordCount])
                        bestGuessOutput <- wordCount3(wordsToSearch)
                }
                return(bestGuessOutput)
        })
        
        output$view <- renderTable({
                originalInput <- input$obs
                splitTransInput <- splitTranslateInput(originalInput)
                wordCount <- length(splitTransInput)
                if(wordCount==1){
                        bestGuessOutput <- wordCount1(splitTransInput)
                }
                if(wordCount==2){
                        bestGuessOutput <- wordCount2(splitTransInput)
                }
                if(wordCount==3){
                        bestGuessOutput <- wordCount3(splitTransInput)
                }
                if(wordCount > 3){
                        wordsToSearch <- c(splitTransInput[wordCount - 2],
                                             splitTransInput[wordCount - 1],
                                             splitTransInput[wordCount])
                        bestGuessOutput <- wordCount3(wordsToSearch)
                }
                if(exists("AlternativeGuess", where = -1)){
                        AlternativeGuess
                }
                else{
                        XNgramsTable <- data.frame(Word=NA, Frequency=NA)
                }
                })
        })