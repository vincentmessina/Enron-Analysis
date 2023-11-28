setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Waterloo/Coop Files/F23 Resumes/Resumes/Data Science/V3.1/Projects/Enron")

##reading data
enron_emails <- read.csv("enron_emails.csv")

##subsetting email from sept 2001
emails_sep_2001 <- subset(enron_emails, Month == 9 & Year == 2001)

##subsetting emails from oct 2001
emails_oct_2001 <- subset(enron_emails, Month == 10 & Year == 2001)

##counting emails from each month
n_emails_sep_2001 <- nrow(emails_sep_2001)
n_emails_oct_2001 <- nrow(emails_oct_2001)

##printing results
cat("Number of emails exchanged in September 2001:", n_emails_sep_2001, "\n")
cat("Number of emails exchanged in October 2001:", n_emails_oct_2001, "\n")

## function to calculate the weight matrix
getWeightMatrix <- function(edgeList, N){
  W <- matrix(data = 0, nrow = N, ncol = N)
  for(i in 1:(N-1)){
    for(j in (i+1):N){
      W[i,j] <- nrow(edgeList[(edgeList$Sender==i & edgeList$Recipient == j),]) +
        nrow(edgeList[(edgeList$Sender==j & edgeList$Recipient == i),])
    }
  }
  W <- W + t(W)
  return(W)
}

## storing weights in variables
weight_sep_2001 <- getWeightMatrix(edgeList = emails_sep_2001, N = 184)
weight_oct_2001 <- getWeightMatrix(edgeList = emails_oct_2001, N = 184)

## average degree for each month
avg_degree_sep_2001 <- mean(rowSums(weight_sep_2001)) #first computing degree then taking avg
avg_degree_oct_2001 <- mean(rowSums(weight_oct_2001))

cat("Average degree in September 2001:", avg_degree_sep_2001, "\n")
cat("Average degree in October 2001:", avg_degree_oct_2001, "\n")


##Raincloud Plot(y, xlabel, title, color) takes in aforementioned parameters and
## returns raincloud plot visualisation
raincloud_plot <- function(y, xlabel, title, colour) {
  
  density_max <- max(density(y)$y)  # Find the maximum density to scale other components
  scatter_offset <- 0.7 * density_max  #increase for more space between points
  extra_space_beneath <- 0.5 * density_max  # Extra space below the scatter
  
  y_range <- c(-scatter_offset - extra_space_beneath, density_max * 1.2) # 1.2 is arbitrary num that works well
  x_range <- c(-100, 500)
  
  plot(x_range, y_range, type="n", xlab=xlabel, ylab="", yaxt="n", main=title) # Base plot
  
  
  ## Density Curve
  raincloud_density <- density(y)
  
  ## Draw the density
  polygon(raincloud_density$x, raincloud_density$y, 
          col=adjustcolor(colour, alpha.f=0.2), border=colour)
  
  ## Outline
  lines(raincloud_density$x, raincloud_density$y, col=colour, lwd=2)
  
  
  ## Boxplot
  ## Draw the boxplot at the bottom
  boxplot(y, horizontal=TRUE, add=TRUE, at=0, 
          col="white", border=colour, whisklty=1,
          whiskcol=colour, staplecol=colour, outcol=colour, 
          boxwex=0.002, outline=FALSE)  #boxwex for box width
  
  ## Scatterplot
  ## Determine Y coordinates for the scatterplot
  y_coordinates <- -scatter_offset/2 - seq_along(y) * (scatter_offset / length(y))
  
  y_jittered <- jitter(y, factor=0.5)  ##more factor more jitter
  
  ## Draw the scatterplot
  points(y_jittered, y_coordinates, pch=19, 
         col=adjustcolor(colour, alpha.f=0.5), cex=0.5)  ##cex for point size
}


##each entry represents the number of emails transmitted
##taking ROW sums finds the number of emails sent by an employee
degree_sep_2001 <- rowSums(weight_sep_2001)
degree_oct_2001 <- rowSums(weight_oct_2001)

##raincloud sept
raincloud_plot(y = degree_sep_2001, 
               xlabel = "Degree", 
               title = "Degree Distribution: September 2001", 
               colour = "blue")
            

##raincloud oct
raincloud_plot(y = degree_oct_2001, 
               xlabel = "Degree", 
               title = "Degree Distribution: October 2001", 
               colour = "red")

## We could see in both months that the data is generally right skewed. This indicates
## that a large number of emails were transmitted from a small group of people
## possibly executives worrying about the impending scandal, while the rest
## of the staff carried on business as usual.

## You could see that the peak is more steep in September as in October.
## At which point news about the scandal broke out and now all employees were transmitting
## many emails talking about this at a similar level to executives

## there are many accounts with 0 emails sent and that's as expected
## not everyone necessarily sent or received emails that given month



## Obtain the adjacency matrix
get_adjacency_matrix <- function(edge_list, N) {
  A <- matrix(data = 0, nrow = N, ncol = N)
  for(i in 1:(N-1)){
    for(j in (i+1):N){
      A[i,j] <- ifelse(nrow(edge_list[(edge_list$Sender == i & edge_list$Recipient == j),]) > 0, 1, 0)
      A[j,i] <- ifelse(nrow(edge_list[(edge_list$Sender == j & edge_list$Recipient == i),]) > 0, 1, 0)
    }
  }
  return(A)
}

adj_sep_2001 <- get_adjacency_matrix(edge_list = emails_sep_2001, N = 184)
adj_oct_2001 <- get_adjacency_matrix(edge_list = emails_oct_2001, N = 184)

## Calculate the density
calculate_density <- function(adj_matrix) {
  E <- sum(adj_matrix) # Sum of matrix gives num of edges since binary
  N <- nrow(adj_matrix)
  D <- E / (N*(N-1))
  return(D)
}

density_sep_2001 <- calculate_density(adj_sep_2001)
density_oct_2001 <- calculate_density(adj_oct_2001)

cat("Density in September 2001:", density_sep_2001, "\n")
cat("Density in October 2001:", density_oct_2001, "\n")


## we could see as expected the density nearly doubled in October. A higher
## density means that a larger proportion of possible email connections were
## actually used. In other words communication was more widespread. 



## we could see the data is quite right skewed in September. This indicates
## that a large number of emails were transmitted from a small group of people
## possibly executives worrying about the impending scandal, while the rest
## of the staff carried on business as usual

## We could see this starts to level out more in October at which point the
## news about the scandal broke out and now all employees were transmitting
## many emails talking about this at a similar level to executives

## there are many accounts with 0 emails sent and that's as expected
## not everyone necessarily sent or received emails that given month



## reading data
enron_employees <- read.csv("enron_employees.csv")

## calculate influence
compute_influence <- function(degrees, N) {
  influences <- numeric(N)
  avg_degree <- mean(degrees)
  
  for (u in 1:N) {
    ##degree without point
    remove_point <- degrees[-u]
    
    ##compute influence without point
    influences[u] <- abs(avg_degree - mean(remove_point))
  }
  
  return(influences)
}

influence_oct_2001 <- compute_influence(degree_oct_2001, 184)

# Plot the influence
plot(1:184, influence_oct_2001, type = "p", col = "red",
     main = "Influence of Employee on Average Degree (October 2001)",
     xlab = "Employee Number", ylab = "Influence")

##employee with the largest influence
employee_max_influence <- which.max(influence_oct_2001)

## job title of the influential employee
job_title_max_influence <- enron_employees$Job.Title[employee_max_influence]

cat("The employee with the highest influence in October 2001 has the job title:", 
    job_title_max_influence, "and is employee number:", employee_max_influence, "\n")

# It makes snese that the In House Lawyer had the highest influence. 
# They were likely involved in the decision making processes during the scandal. 




