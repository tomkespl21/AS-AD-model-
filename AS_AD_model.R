# AS-AD model BAN420 home exam 

# Inflation, output and the AD-AS model

## Problem 1



  
# Then we have eveything needed to write following functions: 
  
y_demand <- 
  function(pi,a,b,rbar,m,pibar){
    y_t = a - b*(m*(pi - pibar))
    return(y_t)
  }
y_supply <- 
  function(pi, pi_lag, nu, o){
    y_t = (pi - pi_lag - o) / nu
    return(y_t)
  }
  
nominal_interest_rate <- 
  function(pi, m, pibar, rbar){
    R_t = m * (pi - pibar) + rbar
    return(R_t)
  }

library(assertthat)
# Tests for assignment 1. Leave this code chunk *unchanged* and 
# ensure you run the tests *after* the chunk with you answer to 
# assignment 1. 
assert_that(
  nominal_interest_rate(pi=.0, m=1, pibar=.02, rbar=.02) <
    nominal_interest_rate(pi=.02, m=1, pibar=.02, rbar=.02),
  msg = "Interest rate isn't falling with lowered inflation"
)
assert_that(
  y_demand(pi=.02,a=0,b=.5,rbar=0.02,m=1,pibar=.02) < 
    y_demand(pi=.0,a=0,b=.5,rbar=0.02,m=1,pibar=.02),
  msg = "Demand curve isn't decreasing with higher prices"
)
assert_that(
  y_supply(pi=.00,pi_lag=.02, nu=.8, o=0) < 
    y_supply(pi=.02,pi_lag=.02, nu=.8, o=0),
  msg = "Supply isn't increasing with higher inflation"
)
assert_that(
  nominal_interest_rate(pi=.1, m=2, pibar=.02, rbar=.02)==.18,
  msg="Interest rate function returns wrong values"
)
assert_that(
  abs(y_demand(pi=1,a=10,b=10,rbar=0.02,m=1,pibar=.02)-.2)<.0001,
  msg="Demand function returns wrong values"
)
assert_that(
  abs(y_supply(pi=-.01,pi_lag=.05, nu=.3, o=.1)+0.5333)<0.001,
  msg="Supply function returns wrong values"
)

library(ggplot2) # nice graphs
library(dplyr)   # only using pipe operator 
library(grid)    # for arrow in plot
# Needed input assumption:
# Inflation interval between -5% and 15 % seems reasonable 
# central banks will do everything to prevent deflation, 
# which is therefore less likely 
# Also very high inflation of > 15% is rare.
pi <- seq(from= -0.05, to= 0.15, by= 0.001 )
# function for demand side
demand <- 
  function(pi,a,b,rbar,m,pibar){
    values = c()
    for(i in 1:length(pi)){
      values[i] = y_demand(pi[i],a,b,rbar,m,pibar)
    }
    return(values)
  }
# function for supply side
supply <- 
  function(pi,pi_lag,nu,o){
    values = c()
    for(i in 1:length(pi)){
      values[i] = y_supply(pi[i],pi_lag,nu,o)
    }
    return(values)
  }
# default values:
a <-  0
b <- 0.5
m <- 1
nu <- 0.8
pibar <- 0.02
rbar <- 0.02
o <-  0
pi_lag <- pibar
# saving values for specific models
demand1 <- demand(pi,a,b,rbar,m,pibar)      # standard demand curve
demand2 <- demand(pi,a=0.1,b,rbar,m,pibar)    # demand curve with demand shock
demand3 <- demand(pi, a=0.1,b,rbar,m=2,pibar) # demand curve with demand shock and new monetary policy
demand4 <- demand(pi,a,b,rbar,m=2,pibar)      # standard demand curve with new monetary policy
supply1 <- supply(pi,pi_lag,nu,o)             # standard supply curve
supply2 <- supply(pi,pi_lag,nu,o=0.1)         # supply curve with supply shock 
# creating Data-frame to use ggplot
df <- data.frame(cbind(pi, demand1, supply1, demand2, supply2, demand3))
# 1. basic model graph
df %>% 
  ggplot(aes(y = pi ))+
  geom_line(aes(x = demand1), color="#56B4E9",size=0.8)+    # demand curve
  geom_line(aes(x = supply1), color="#56B4E9", size=0.8)+   # supply cruve
  theme_classic()+
  ggtitle("1. Demand and Supply Curves")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab(expression(Y["t"]))+                              # using expression function to subscript Y_t
  ylab(expression("\u03c0"["t"]))+                       # using expression function and unicode to get pi symbol
  annotate("text",x=0.04 , y=-0.04, label= "AD")+        # helpful function to add text in graph
  annotate("text", x=0.16, y=0.133, label = "AS")+       
  theme(axis.line.x = element_line(size=0.8, arrow = grid::arrow(length = unit(0.2, "cm"), # arrow end of x-axis
                                                                 type = "closed")),
        axis.line.y = element_line(size=0.8,arrow = grid::arrow(length = unit(0.2, "cm"),  # arrow end of y-axis
                                                                type = "closed")),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_text(hjust=1,size = 12),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_text(angle = 360, hjust=1,size=12))
# we see both lines intersect at the equilibrium 
# which is at Y_t=0 and pi_t=0.02
# economically graph makes sense 
# high inflation --> low demand , because at high prices consumers don`t want to buy
# low  inflation --> low supply , because at low prices  producers don`t want to sell
# and vice versa
# 2. Graph for demand shock 
df %>% 
  ggplot(aes(y = pi ))+
  geom_line(aes(x = demand1),color="#56B4E9", size=0.8)+          # colors from color-blind-friendly palette
  geom_line(aes(x = supply1),color="#56B4E9", size=0.8)+
  geom_line(aes(x = demand2), color = "#D55E00", size=0.8)+       # demand line with shock in t=1
  theme_classic()+
  ggtitle("2. Demand Shock")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab(expression(Y["t"]))+
  ylab(expression("\u03c0"["t"]))+
  geom_segment(aes(x = 0.04, y = -0.01, xend = 0.07, yend = 0.015),  # to add arrow for direction
               arrow = arrow(length = unit(0.5, "cm")))+
  annotate("text",x=0.045 , y=-0.048, label= expression("AD"[1]))+
  annotate("text",x=0.147 , y=-0.048, label= expression("AD"[2]))+
  annotate("text", x=0.16, y=0.133, label = "AS")+
  theme(axis.line.x = element_line(size=0.8, arrow = grid::arrow(length = unit(0.2, "cm"), 
                                                                 type = "closed")),
        axis.line.y = element_line(size=0.8,arrow = grid::arrow(length = unit(0.2, "cm"), 
                                                                type = "closed")),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_text(hjust=1),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_text(angle = 360, hjust=1))
# We can see that the demand shock a=0.1 shifts the demand curve to the right (AD1 --> AD2) 
# resulting in a higher equilibrium (higher inflation and output-gap) 
# 3. Supply shock 
df %>% 
  ggplot(aes(y = pi ))+
  geom_line(aes(x = demand1),color="#56B4E9",size=0.8)+
  geom_line(aes(x = supply1),color="#56B4E9", size=0.8)+
  geom_line(aes(x = supply2), color = "#D55E00",size=0.8)+          # supply curve with shock in t=1
  theme_classic()+
  ggtitle("3. Supply Shock")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab(expression("Y"[t]))+
  ylab(expression("\u03c0"["t"]))+
  geom_segment(aes(x = -0.07, y = -0.01, xend = -0.1, yend = 0.015),
               arrow = arrow(length = unit(0.5, "cm")))+
  annotate("text",x=0.045 , y=-0.048, label= "AD")+
  annotate("text", x=0.16, y=0.133, label = expression("AS"[1]))+
  annotate("text", x=0.04, y=0.133, label = expression("AS"[2]))+
  theme(axis.line.x = element_line(size=0.8, arrow = grid::arrow(length = unit(0.2, "cm"), 
                                                                 type = "closed")),
        axis.line.y = element_line(size=0.8,arrow = grid::arrow(length = unit(0.2, "cm"), 
                                                                type = "closed")),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_text(hjust=1),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_text(angle = 360, hjust=1))
# for the supply shock o=0.1 the line shifts to the left (AS1 --> AS2) 
# resulting in new equilibrium with higher inflation and lower output-gap 
# Now effect of Monetary Policy:
# Since m variable is only in the demand function, 
# only the demand curve will change 
# meaning, that monetary policy does not seem to effect supply side 
# effect on figure 2
df %>% 
  ggplot(aes(y = pi ))+
  geom_line(aes(x = demand1),color="#56B4E9", size=0.8)+
  geom_line(aes(x = supply1),color="#56B4E9", size=0.8)+
  geom_line(aes(x = demand2), color = "#D55E00", size=0.8)+
  geom_line(aes(x = demand3), color = "#009E73", size=0.8)+         # demand curve with shock in t=1 and new MP
  theme_classic()+
  ggtitle("Monetary Policy effect on demand shock")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("y")+
  ylab(expression("\u03c0"["t"]))+
  annotate("text",x=0.045 , y=-0.048, label= expression("AD"[1]))+
  annotate("text",x=0.143 , y=-0.048, label= expression("AD"[2]))+
  annotate("text", x=0.17, y=0.145, label = "AS")+
  annotate("text",x=0.18 , y=-0.048, label= expression("AD"[3]))+
  theme(axis.line.x = element_line(size=0.8, arrow = grid::arrow(length = unit(0.2, "cm"), 
                                                                 type = "closed")),
        axis.line.y = element_line(size=0.8,arrow = grid::arrow(length = unit(0.2, "cm"), 
                                                                type = "closed")),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_text(hjust=1),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_text(angle = 360, hjust=1))

# see demand curve with m  = 2 is less steep (AD2 vs AD3)
# resulting in a lower equilibrium between inflation and output gap 
# MP effect on figure 3 
df %>% 
  ggplot(aes(y = pi ))+
  geom_line(aes(x = demand1),color="#56B4E9", size=0.8)+
  geom_line(aes(x = supply1),color="#56B4E9", size=0.8)+
  geom_line(aes(x = supply2), color = "#D55E00", size=0.8)+
  geom_line(aes(x = demand4), color = "#009E73", size=0.8)+
  theme_classic()+
  ggtitle("Monetary Policy effect on supply shock")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab(expression("Y"["t"]))+
  ylab(expression("\u03c0"["t"]))+
  annotate("text",x=0.045 , y=-0.048, label= expression("AD"[1]))+
  annotate("text", x=0.172, y=0.149, label = expression("AS"[1]))+
  annotate("text",x=0.082 , y=-0.048, label= expression("AD"[3]))+
  annotate("text", x=0.048, y=0.15, label = expression("AS"[2]))+
  theme(axis.line.x = element_line(size=0.8, arrow = grid::arrow(length = unit(0.2, "cm"), 
                                                                 type = "closed")),
        axis.line.y = element_line(size=0.8,arrow = grid::arrow(length = unit(0.2, "cm"), 
                                                                type = "closed")),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_text(hjust=1),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_text(angle = 360, hjust=1))

# as before monetary policy has an effect on demand curve 
# again we have a less steep demand curve (AD1 vs AD3)
# resulting in a lower equilibrium compared to the original demand curve 
# therefore in summary we can say that higher monetary responsiveness 
# decreases inflation and output-gap


# Problem 3 

library(dplyr)
# function for y_t given in problem 3 
output_gap  <-
  function(a,b,m,nu,o,pi_lag,pibar){
    y_t = 1/(1+b*m*nu)*(a-b*m*(pi_lag+o-pibar))
    return(y_t)
  }
# if we know the output-gap we can compute inflation and interest rate
# Inflation afterwards:
inflation <- 
  function(pi_lag,nu,output_gap,o){ 
    pi = pi_lag + nu * output_gap + o
    return(pi)
  }
# interest rate
nominal_interest_rate <- 
  function(pi, m, pibar, rbar){
    R_t = m * (pi - pibar) + rbar
    return(R_t)
  }
# Important to note:
# pi_lag changes output gap one period in the future 
# time steps 
t <- seq(1:15)

#########################################################################
# these functions are for problem 4                                     # 
# therefore can be skipped for now                                      #
#
interest_optimal <-                                                     #
  function(a,b,nu,o,pi_lag,pibar,rbar,lambda){                          #
    R_opt = rbar + a/b + nu/(b*(lambda+nu^2)) *(pi_lag - pibar + o)     #
    return(R_opt)                                                       #
  }                                                                     #
#
output_gap_opt <-                                                       #
  function(b,nu,lambda,pi_lag,pibar,o){                                 #
    y_opt = nu/(b*(lambda+nu^2)) *(pibar-pi_lag-o)                      #
  }                                                                     #
#########################################################################
# simulation function 
simulation <- 
  function(a,b,m,nu,o,pi_lag,pibar,rbar){
    y_t = c()     # empty vectors to fill in 
    pi_t = c()
    R_t = c()
    y_opt = c()
    R_opt = c()
    
    y_t[1] = output_gap(a=0,b,m,nu,o=0,pi_lag,pibar)     # starting values for t=0
    pi_t[1] = inflation(pi_lag,nu,y_t[1],o=0) 
    R_t[1] = nominal_interest_rate(pi_t[1],m,pibar,rbar)
    y_opt[1] = output_gap_opt(b,nu,lambda=1,pi_lag,pibar,o)           # for problem 4
    R_opt[1] = interest_optimal(a,b,nu,o,pi_lag,pibar,rbar,lambda=1)  # for problem 4
    for(i in 2:15){                                              
      if(i == 2){                                                     # shock scenario 
        y_t[i] = output_gap(a,b,m,nu,o,pi_t[1],pibar)
        pi_t[i] = inflation(pi_t[i-1],nu,y_t[i],o)
        R_t[i] = nominal_interest_rate(pi_t[i],m,pibar,rbar)
        y_opt[i] = output_gap_opt(b,nu,lambda=1,pi_lag=pi_t[i-1],pibar,o)
        R_opt[i] = interest_optimal(a,b,nu,o,pi_lag,pibar,rbar,lambda=1)
      }else {
        y_t[i] = output_gap(a=0,b,m,nu,o=0,pi_t[i-1],pibar)           # normal scenario
        pi_t[i]  = inflation(pi_t[i-1],nu,y_t[i],o=0)
        R_t[i] = nominal_interest_rate(pi_t[i],m,pibar,rbar)
        y_opt[i] = output_gap_opt(b,nu,lambda=1,pi_lag=pi_t[i-1],pibar,o=0)
        R_opt[i] = interest_optimal(a,b,nu,o=0,pi_lag=pi_t[i-1],pibar,rbar,lambda=1)
      }
    }
    return(list(y_t,pi_t,R_t,y_opt,R_opt))            # save outcomes in list
  }                                                   # so all values can be returned

sim1 <- simulation(a,b,m,nu,o=0.1,pi_lag,pibar,rbar)

df <- data.frame(t,sim1[[1]],sim1[[2]],sim1[[3]])
colnames(df) <- c("t","y_t","pi_t","R_t")
# If m=1 we can figure out by looking at the equations, that 
# interest rate and inflation result in the same values 
# since both have the same values I only put pi_t in the graphs 
# graph dynamics of supply shock
df %>% 
  ggplot(aes(x = t))+
  geom_line(aes(y = y_t ,colour="Output-gap, Y"),size=0.8)+
  geom_line(aes(y = pi_t ,colour="Inflation,\u03c0"),size=0.8)+
  scale_color_manual(name = "", values = c("Output-gap, Y" = "#56B4E9", "Inflation,\u03c0" = "#D55E00"))+
  theme_classic()+
  ggtitle("Dynamics of Supply shock")+
  theme(axis.line.x = element_line(size=0.8),
        axis.line.y = element_line(size=0.8),
        plot.title = element_text(hjust = 0.5))+
  xlab("T")+
  theme(axis.title.y = element_blank())

# similar to the example figure in the task 
# we see at t=1 when the demand shock occurs, that inflation goes up and output-gap goes down on steep gradients 
# after the shock both variables slowly and smoothly converge back to beginning equilibrium of t=0
# graph dynamics of demand shock
sim2 <- simulation(a=0.1,b,m,nu,o,pi_lag,pibar,rbar)

df2 <- data.frame(t,sim2[[1]],sim2[[2]],sim2[[3]])
colnames(df2) <- c("t","y_t","pi_t","R_t")
df2 %>% 
  ggplot(aes(x = t))+
  geom_line(aes(y = y_t ,colour="Output-gap, Y"),size=0.8)+
  geom_line(aes(y = pi_t ,colour="Inflation,\u03c0"),size=0.8)+
  scale_color_manual(name = "", values = c("Output-gap, Y" = "#56B4E9", "Inflation,\u03c0" = "#D55E00"))+
  theme_classic()+
  ggtitle("Dynamics of Demand shock")+
  theme(axis.line.x = element_line(size=0.8),
        axis.line.y = element_line(size=0.8),
        plot.title = element_text(hjust = 0.5))+
  xlab("T")+
  theme(axis.title.y = element_blank())
# here we see a different reaction compared to the supply shock
# at the demand shock in t=1 firstly both inflation and output-gap go up 
# in t=2 the output-gap rapidly goes down similar to what we saw after the supply shock 
# this is because the demand shock hits the output one period later because it depends on lagged inflation 
# after that as before both variables slowly and smoothly converge back to beginning equilibrium of t=0
# monetary policy change m=2 
# this only effects the demand side
sim3 <- simulation(a=0.1,b,m=2,nu,o,pi_lag,pibar,rbar)
df3 <- data.frame(t,sim2[[1]],sim2[[2]],sim2[[3]],sim3[[1]],sim3[[2]],sim3[[3]])
colnames(df3) = c("t","y_t","pi_t","R_t","y_tm","pi_tm","R_tm")
# dynamics m=2 demand shock: 
df3 %>% 
  ggplot(aes(x = t))+
  geom_line(aes(y = y_t ,colour="Output-gap, Y"),size=0.8)+
  geom_line(aes(y = pi_t ,colour="Inflation,\u03c0"),size=0.8)+
  geom_line(aes(y = pi_tm ,colour="Inflation2,\u03c0"),size=0.8)+
  geom_line(aes(y = y_tm ,colour="Output-gap2, Y2"),size=0.8)+
  scale_color_manual(name = "", values = c("Output-gap, Y" = "#56B4E9", "Inflation,\u03c0" = "#D55E00","Inflation2,\u03c0"="#CC79A7","Output-gap2, Y2"="#009E73"))+
  theme_classic()+
  ggtitle("Dynamics of Demand shock")+
  theme(axis.line.x = element_line(size=0.8),
        axis.line.y = element_line(size=0.8),
        plot.title = element_text(hjust = 0.5))+
  xlab("T")+
  theme(axis.title.y = element_blank())
# we see that m=2 has the effect of lowering the level of inflation (pink curve vs orange curve) 
# which is what central bankers want ! 
# this comes with the cost of slightly increasing output-gap at t=1
# dynamics m=2 supply shock
sim4 <- simulation(a,b,m=2,nu,o=0.1,pi_lag,pibar,rbar)
df4 <- data.frame(t,sim1[[1]],sim1[[2]],sim1[[3]],sim4[[1]],sim4[[2]],sim4[[3]])
colnames(df4) = c("t","y_t","pi_t","R_t","y_tm","pi_tm","R_tm")
df4 %>% 
  ggplot(aes(x = t))+
  geom_line(aes(y = y_t ,colour="Output-gap, Y"),size=0.8)+
  geom_line(aes(y = pi_t ,colour="Inflation,\u03c0"),size=0.8)+
  geom_line(aes(y = pi_tm ,colour="Inflation2,\u03c0"),size=0.8)+
  geom_line(aes(y = y_tm ,colour="Output-gap2, Y2"),size=0.8)+
  scale_color_manual(name = "", values = c("Output-gap, Y" = "#56B4E9", "Inflation,\u03c0" = "#D55E00","Inflation2,\u03c0"="#CC79A7","Output-gap2, Y2"="#009E73"))+
  theme_classic()+
  ggtitle("Dynamics of Demand shock")+
  theme(axis.line.x = element_line(size=0.8),
        axis.line.y = element_line(size=0.8),
        plot.title = element_text(hjust = 0.5))+
  xlab("T")+
  theme(axis.title.y = element_blank())
# m=2 also has the effect of lowering the inflation if a demand shock happens
# this again comes with the cost of increasing the output-gap (in absolute numbers)



# problem 4 

sim5 <- simulation(a,b,m,nu,o=0.1,pi_lag,pibar,rbar)
df5 <- data.frame(t,sim5[[1]],sim5[[2]],sim5[[3]],sim5[[4]],sim5[[5]])
colnames(df5) = c("t","y_t","pi_t","R_t","y_opt","R_opt")
df5 %>% 
  ggplot(aes(x = t))+
  geom_line(aes(y = y_t ,colour="Output-gap, Y"),size=0.8)+
  geom_line(aes(y = pi_t ,colour="Interest Rate,\u03c0"),size=0.8)+
  geom_line(aes(y = R_opt ,colour="Interest Rate2,\u03c0"),size=0.8)+
  geom_line(aes(y = y_opt ,colour="Output-gap, Y2"),size=0.8)+
  scale_color_manual(name = "", values = c("Output-gap, Y" = "#56B4E9", "Interest Rate,\u03c0" = "#D55E00","Interest Rate2,\u03c0"="#CC79A7","Output-gap, Y2"="#009E73"))+
  theme_classic()+
  ggtitle("Dynamics of Supply shock")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("T")+
  theme(axis.title.y = element_blank())



  
  
  
  