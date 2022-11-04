# Pi Simulation using Monte Carlo

Pi value can be simulated by using Monte Carlo technique by drawing random numbers around the area of square of length 2R and with a circle inside the square with a radius R.

### Calculation 
**Step 1:**
Consider a circle around the x-y axis with center as (0,0) and radius as 1(R). The square around the circle will have the length of 2 which is the diameter of circle(2R).

**Step 2:**
Imagine rain drops falling on this space. The drops falling inside the cricle will help us to estimate the area of circle and the rain drops in the space wihtin the square will help us to estimate the are of square. 

**Step 3:**
So, let's assume, the area of circle and the area of square as the total of number of drops inside the Circle(C) and Square(S). This can be interpretted as:

$area(square) \approx area(circle)$

$(2R)^2 * S \approx \pi R^2 * C$

$4R^2 *S \approx \pi R^2*C$

$4S \approx \pi C$

$\pi \approx \frac{4S}{C}$

### Simulation
**Step 1:**
Draw 2 uniform random numbers from (-1,1) and consider them as point at (x,y)

**Step 2:**
If the sum of square of coordinates of random numbers is less than 1(R of circle), then the point is within circle else, the point is outside circle but within square. So, initilize S and C as 0. For every cycle of random number generation if the sum of the of square of coordinates of random number is less than 1(R of circle), then it is counted as C as well as S else, only S is counted.

**Step 3:**
Calculate the value of pi using the above formula(4S/C). 



## Maximum relative error of the simulated Pi value

The error is checekd with the actual value of Pi for each simulation for 1000 draws(points) for a total of 100 simulations.

The Maximum Relative Error is then calculated. 



### Conclusion

**If the Maximum Relative error is less than 5% we have achieved a reasonable approximation of Pi value**
