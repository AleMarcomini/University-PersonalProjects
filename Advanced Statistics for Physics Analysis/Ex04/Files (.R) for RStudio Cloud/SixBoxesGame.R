#FUNCTIONS

#Likelihood calculation
calc_L <- function(val, j){
  if (val == 1){return (j/5)}
  if (val == 0){return ((5-j)/5)}
  else return (NA)
}

#Ball extraction
trial <- function(p_white){
  u <- runif(1,0,1)
  if (u <= p_white){1}
  else {0}
}

#Check guess
check_guess <- function(true,guess,spent,wallet){
  if (guess==true){
    message('FANTASTICO! YOU WON! you added ',wallet,' coins to your wallet!')
    wallet = (wallet*2)+spent
    }
  else{
    message('MANNAGGIA! YOU LOST! The real box was B',true,'! You spent ',spent,' coins for nothing... retry!')
    wallet = wallet
    }
}

#Function for plotting results
red = rgb(1.0,0,0,alpha=0.125)
green = rgb(0,1.0,0,alpha=0.125)
white = rgb(1.0,1.0,1.0,alpha=0.125)

plot_pdf <- function(posteriors){
  
  par(mfrow=c(2,3), oma = c(0, 0, 2, 0))
  
  for (i in 1:dim(posteriors)[1]){
    box = rownames(posteriors)[i]
    color = ifelse((posteriors[i,dim(posteriors)[2]]==0), red,
                   ifelse((posteriors[i,dim(posteriors)[2]]==max(posteriors[,dim(posteriors)[2]])), green, white))
    plot(posteriors[box,], pch=20, cex=1, ylim=c(0,1),
         xlab = 'Trial', ylab = 'Probability',
         main = box, cex.main=1, cex.lab=1)
    rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = color)
    grid()
  }
  mtext('Six Boxes Toy Model Game', outer = TRUE, cex = 1.7)
  
}



#-----------------------------------------------------------------------------------------------




#Play game!

#Plot nstructions
print('***************************************************')
print('* Welcome, Player, to the 6 Boxes Toy Model Game! *')
print('* Follow the instructions and have fun: you will  *')
print('*    be able to choose between PLAYER MODE and    *')
print('*  BEAT THE SYSTEM. In any case, the system will  *')
print('* show you the most probable box to guess and the *')
print('* impossible ones will be marked in red. What is  *')
print('* left? Oh yess! Have fun, alone or with friends! *')
print('*                                                 *')
print('*                                  Alessandro :)  *')
print('*                                                 *')
print('***************************************************')
instructions = readline(prompt = "Would you like to see the instructions? (y/n) ")
if (instructions=='y'){
print('---------------------------------------------------')
print('-------------------- GAME MODES -------------------')
print('---------------------------------------------------')
print('|PLAYER MODE| You set the game: provide information')
print('about what you have drawn, according to the legend,')
print('and see how the probabilities liked to different   ')
print('boxes change because of this. You can either look  ')
print('at the plots or at the history matrix.             ')
print('---------------------------------------------------')
print('|BEAT THE SYSTEM| The computer will randomly select')
print('one of the boxes and you have to guess which one.  ')
print('Bayesian statistics help a little, but are you able')
print('to win? Rules are simple: you start the 1st round  ')
print('with 30 coins, each of them can buy you a trial    ')
print('from the box. You start by saying how many trials  ')
print('you are going to buy and the system will draw that ')
print('amount of balls. At this point you can guess or buy')
print('other trials: if you guess correctly, you get back ')
print('what you spent and double what you have saved, if  ')
print('you fail you lose everything you spent.            ')
print('---------------------------------------------------')
}

#Get game mode
mode = readline(prompt = "Insert game mode ('player_mode'(0) or 'beat_the_system'(1)): ")
if(mode=='0'){mode='player_mode'}
if(mode=='1'){mode='beat_the_system'}

#Code for player_mode
if (mode == 'player_mode'){
  message('NOTE: probabilities and plots will be automatically generated after the second input.')
  #First iteration
  x = strtoi(readline(prompt = "Insert color (0=Black, 1=White, 2=Stop): "))
  
  prior <- array(rep(1/6,6))
  Like <- c(calc_L(x,0),calc_L(x,1),calc_L(x,2),calc_L(x,3),calc_L(x,4),calc_L(x,5))
  Z = sum(Like*prior)
  posterior <- Like*prior/Z
  posteriors <- posterior
  
  #Later iterations                   
  while (x != 2){
    if (x == 2){break}
    x = strtoi(readline(prompt = "Insert color (0=Black, 1=White, 2=Stop): "))
    prior <- posterior
    Like <- c(calc_L(x,0),calc_L(x,1),calc_L(x,2),calc_L(x,3),calc_L(x,4),calc_L(x,5))
    Z = sum(Like*prior)
    posterior <- Like*prior/Z
    posteriors <- cbind(posteriors,posterior)
    View(posteriors)
    colnames(posteriors) <- 0:(dim(posteriors)[2]-1)
    rownames(posteriors) <- c('B0','B1','B2','B3','B4','B5')
    plot_pdf(posteriors)
  }   
print('Goodbye, and thanks for playing! :)')
}







#Code for beat_the_system
if (mode == 'beat_the_system'){
  wallet = 30
  spent = 0
  play = 'y'
  
  while(play=='y'){
    #Box draw
    message('You have ',wallet,' coins in your wallet.')
    max_tr = strtoi(readline(prompt = "How many trials do you want to buy? "))
    while(max_tr < 1 || max_tr > wallet){
      message('Invalid number: check your wallet')
      max_tr = strtoi(readline(prompt = "How many trials do you want to buy? "))
      }
    
    #First iteration
    box_j = floor(runif(1,0,6))
    p_white = box_j/5
    x = trial(p_white)
    
    prior <- array(rep(1/6,6))
    Like <- c(calc_L(x,0),calc_L(x,1),calc_L(x,2),calc_L(x,3),calc_L(x,4),calc_L(x,5))
    Z = sum(Like*prior)
    posterior <- Like*prior/Z
    posteriors <- posterior
    tr = 1
    
    #Later iterations
    while(max_tr != 0){
    wallet = wallet - max_tr
    spent = spent + max_tr
      while (tr <= max_tr){
        x = (trial(p_white))
        prior <- posterior
        Like <- c(calc_L(x,0),calc_L(x,1),calc_L(x,2),calc_L(x,3),calc_L(x,4),calc_L(x,5))
        Z = sum(Like*prior)
        posterior <- Like*prior/Z
        posteriors <- cbind(posteriors,posterior)
        print(x)
        colnames(posteriors) <- 0:(dim(posteriors)[2]-1)
        rownames(posteriors) <- c('B0','B1','B2','B3','B4','B5')
        plot_pdf(posteriors)
        tr = tr+1
        Sys.sleep(0.5)
      }   
    View(posteriors)
    
    message('You have ',wallet,' coins in your wallet.')
    max_tr = strtoi(readline(prompt = "How many more trials do you want to buy? (Type 0 to move to guess) "))
    tr = 1
    }
    
    guess = strtoi(readline(prompt = "Enter the number of your guessed box: "))
    wallet = check_guess(box_j,guess,spent,wallet)
    spent = 0
    play = readline(prompt = "Would you like to play again? (y/n) ")
  }
print('Goodbye, and thanks for playing! :)')
}

