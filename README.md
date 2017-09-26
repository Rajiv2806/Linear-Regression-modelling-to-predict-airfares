# Linear-Regression-modelling-to-predict-airfares

Statistical Modelling - Mini Project


## Variables

S_CODE: starting airport’s code
S_CITY: starting city
E_CODE: ending airport’s code
E_CITY: ending city
COUPON: average number of coupons (a one-coupon flight is a non-stop flight, a two-coupon flight is a one stop flight, etc) for that route
NEW: number of new carriers entering that route between Q3-96 and Q2-97
VACATION: whether a vacation route (Yes) or not (No); Florida and Las Vegas routes are generally considered vacation routes
SW: whether Southwest Airlines serves that route (Yes) or not (No)
HI: HerfindelIndex –measure of market concentration (refer to BMGT 681)
S_INCOME: starting city’s average personal income
E_INCOME: ending city’s average personal income
S_POP: starting city’s population
E_POP: ending city’s population
SLOT: whether either endpoint airport is slot controlled or not; this is a measure of airport congestion
GATE: whether either endpoint airport has gate constraints or not; this is another measure of airport congestion
DISTANCE: distance between two endpoint airports in miles
PAX: number of passengers on that route during period of data collection
FARE: average fare on that route

# Goal:

1. Predicting the airfares
2. Estimate the reduction in average fare on the route if in b)southwest decided to cover this route.
3. Predict the average fare on a route with the following characteristics:

COUPON=1.202, NEW=3, VACATION=No, SW=No, HI=4442.141, S_INCOME=$28,760, E_INCOME=$27,664, S_POP=4,557,004, E_POP=3195,503, SLOT=Free, GATE=Free, PAX=12782, DISTANCE=1976miles. What is the 95%Prediction interval? 
