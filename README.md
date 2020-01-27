# Calibrating Models to Data: A Comparison of Methods
This MSc project seeks to compare model calibration methods to ascertain which of the methods performs best in terms of 
  - closeness to the "reference posterior"
  - Compute time.

The reference posterior is a posterior generated from Rejection ABC with a very small tolerance.

## The methods to be compared are 
  * Rejection Approximate Bayesian Computation (RABC)
  * Sequential Approximate Bayesian Computation (SABC)
  * Bayesian Maximum Likelihood Estimation (BMLE)

## Model to be calibrated / Model under which simulation is performed
    We use a simple stochastic Susceptible Infetious and Recovered (SIR) model with two 
    parameters (beta - transmission coefficient and gamma - recovery rate).

## Major input requirements of the methods
    - Model under which simulation is performed
    - Summary statistics targets
    - Tolerance (for RABC)
    - Number of simulations

## Method
     - A simulation study using a stochastic SIR model with two parameters (β = 0.2 and γ = 0.02) is performed in R. 
     - For comparison of performance, the “reference posterior”  is generated using Rejection ABC with small tolerance.
     - Percentage overlaps will be computed for all methods to compare the methods to the reference posterior.

## Expected Output
     - Posterior for each calibration method
     - Run time
     
## Packages Required
     - EasyABC
     - SimInf
     - ABCtargets*
   
 
 ## Flow diagram for SEAMS workshop
 
 ![Flow Diagram](https://github.com/zenabu-suboi/masters_project/blob/master/IMAGES/seams_flowchat.jpg)
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
