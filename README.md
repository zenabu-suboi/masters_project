# Calibrating Models to Data: A Comparison of Methods
This MSc project seeks to compare model calibration methods to ascertain which of the methods performs best in terms of 
  - closeness to the "true posterior"
  - efficiency

## The methods to be compared are 
  * Rejection Approximate Bayesian Computation (RABC)
  * Sequential Approximate Bayesian Computation (SABC)
  * Bayesian Maximum Likelihood Estimation (BMLE)

## Model to be calibrated / Model under which simulation is performed
    We use a simple stochastic Susceptible Infetious and Recovered (SIR) model with two 
    parameters (beta - transmission coefficient and gamma - recovery rate).

## Major input requirements of the methods
    - Model under which simulation id performed
    - Calibration Targets
    - Tolerance (for RABC)
    - Number of simulations

## Method
     - A simulation study using a stochastic SIR model with two parameters (β = 0.2 and γ = 0.02) was performed. 
     - For comparison of performance, the “true posterior”  is generated using Rejection ABC with small tolerance.
     - Percentage overlaps will be computed for all methods to compare the methods to the true posterior.
