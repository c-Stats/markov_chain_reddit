#Frame with all states
states <- expand.grid(y = c(0:2), x = c(0:8))
colnames(states) <- c("Impostor", "Crew")
states <- states[-which(states$Impostor == 0 & states$Crew == 0), ]
rownames(states) <- c(1:length(states))

#Which states are terminal?
impostor_win <- which(states$Impostor >= states$Crew)
crew_win <- which(states$Impostor == 0 & states$Crew > 0)

non_terminal <- c(1:nrow(states))[-c(impostor_win, crew_win)]

#Label each state with a number, i.e.: (i,j) -> (k)
states$State <- NA
states$State[non_terminal] <- c(1:length(non_terminal))
states$State[impostor_win] <- length(non_terminal) + 1
states$State[crew_win] <- length(non_terminal) + 2

n_states <- length(non_terminal) + 2

#Build the transition matrix
transition_matrix <- matrix(0, n_states, n_states)
colnames(transition_matrix) <- c(1:n_states)
rownames(transition_matrix) <- c(1:n_states)

for(i in 1:nrow(states)){

	coordinates <- states[i, ]
	current_state <- coordinates$State
	#If state is terminal...
	if(current_state > n_states - 2){

		transition_matrix[current_state, current_state] <- 1
		next

	}

	#Transition probabilities
	p <- unlist(coordinates[, 1:2] / sum(unlist(coordinates[, 1:2])))

	#Impostor dies
	next_state <- which(states$Impostor == coordinates$Impostor - 1 & states$Crew == coordinates$Crew)
	if(any(next_state)){

		next_state <- states$State[next_state]
		transition_matrix[current_state, next_state] <- p[1]

	}

	#Crewmate dies
	next_state <- which(states$Impostor == coordinates$Impostor & states$Crew == coordinates$Crew - 1)
	if(any(next_state)){

		next_state <- states$State[next_state]
		transition_matrix[current_state, next_state] <- p[2]

	}	


}


#Verify that all transition probabilities add up to one
any(which(apply(transition_matrix, 1, sum) != 1))

#Raise the matrix to the power 10
t_mat_pow10 <- transition_matrix
for(i in 1:9){t_mat_pow10 <- t_mat_pow10 %*% transition_matrix}

#Starting vector
v <- rep(0, nrow(transition_matrix))
starting_position <- states$State[which(states$Impostor == 2 & states$Crew == 8)]
v[starting_position] <- 1

answer <- (t(v) %*% t_mat_pow10)[(n_states - 1):n_states]
names(answer) <- colnames(states)[1:2]
print(answer)