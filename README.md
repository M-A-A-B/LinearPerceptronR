# LinearPerceptronR
DEVELOP AN OCR SYSTEM USING A LINEAR PERCEPTRON IN R

## ALGORITHM FOR STOCHASTIC GRADIENT DESCENT 
Dimensions of the data matrix are mxn (m rows and n column) 
1. Add a column of ones to data matrix X (this will cater for bias unit). 
2. Initialize all weights randomly #(n+1 weights because of the column of ones) 
3. Repeat each epoch as: #i is a row index and j is a column index 
  a. for i = 1 to m # for each row
  
  {
    
      i. compute 𝑦̂𝑖=Σ𝑥𝑖𝑗𝑤𝑗#prediction for one row for j=0 to n
      
      ii. update the weights as:  #each weight has to be updated.
      
                                  #In R it can be done without a loop. 
                                  
                                  # Just do matrix/vector math 
                                  
                                  # j=0..n (note n+1 weights) 
                                  
          Δ𝑤𝑗←∝Δ𝑤𝑗+ 𝜂(𝑦𝑖−𝑦𝑖̂)(𝑥𝑖𝑗)  #alpha = momentum, 
          
                                  #eata = learning rate 
                                  
          𝑤𝑗 ← 𝑤𝑗+ Δ𝑤𝑗
          
  }
