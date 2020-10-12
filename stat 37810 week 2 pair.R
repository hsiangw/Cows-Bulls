#Hsiang Wang and Tianheng Huang

bulls_and_cows <- function(){
  generate_computer_vector <- function(){
    #This function generates computer choice
    computer_choice <- sample(0:9,4)
    return(computer_choice)
  }
  get_guess <- function(){
    #This function inputs number of remaining choice and outputs user choice
    chance <- 3 #Allowing three errors
    while(chance>0){
      chance <- chance-1
      numbers_string <- readline("Please enter four numbers > ")#input user's guess 
      user_choice<-as.integer(unlist(strsplit(numbers_string, ""))) # as numeric vector
      if (length(user_choice)!=4){ #If the unit of inputs is not 4
        message("The length of user inputs are not correct")
        next
      }
      if(any(duplicated(user_choice))){ #If there is duplicated
        message("There are duplicated numbers in user inputs")
        next
      }
      return(user_choice)
    }
    stop("no more attempts allowed")
  }
  number_bulls_and_cows <- function(computer_choice,user_choice){
    #This function inputs computer and user choice. It outputs number of bulls and cows
    nbulls <- function(computer_choice,user_choice){ #compute number of bulls
      bull <- 0
      for (i in 1:length(computer_choice)){
        if (computer_choice[i]==user_choice[i]){
          bull=bull+1
        }
      }
      bull
    }
    ncows <- function(computer_choice,user_choice){#compute numbers of cows
      length(intersect(computer_choice,user_choice))-nbulls(computer_choice,user_choice)
    }
    nbull <- nbulls(computer_choice,user_choice)
    ncow <- ncows(computer_choice,user_choice)
    return(c(nbull,ncow))
  }
  do_response <- function(nbnc,computer_choice,numberofguesses,type){ 
    #return different response(correct, no_remaining, remaining)
    vectoint <- function(vec){
      value=0
      for (i in 1:length(vec)){
        value=value+vec[i]*10^(4-i)
      }
      value
    }
    if(type=="no_remaining"){
      print(paste(nbnc[1],"A",nbnc[2],"B"))
      print(paste("The correct answer is:",vectoint(computer_choice)))
    }
    if(type=="correct"){
      print("Correct!")
      print(paste("You guess",numberofguesses,"time(s)"))
    }
    if(type=="remaining"){
      print(paste(nbnc[1],"A",nbnc[2],"B"))
      print(paste("Number of remaining guess(es):",10-numberofguesses))
    }
  }
  computer_choice <- generate_computer_vector() # generate computer's pick
  numberofguesses <- 0 #initialize number of guesses
  user_choice <- rep(0,4) #initialize user's guess
  while(numberofguesses<10){ # while number of guesses less than 10
    user_choice <- get_guess()
    numberofguesses=numberofguesses+1 # increment number of guesses
    nbnc <- number_bulls_and_cows(computer_choice,user_choice)
    if(nbnc[1]==4){ # if the guess is correct
      do_response(nbnc,computer_choice,numberofguesses,"correct")
      break
    } 
    else{ # if the guess is wrong
      remaining <- 10-numberofguesses
      if (remaining==0){
        do_response(nbnc,computer_choice,numberofguesses,"no_remaining")
      }
      else{
        do_response(nbnc,computer_choice,numberofguesses,"remaining")
      }
    }
  }
  
}


