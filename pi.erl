-module(pi).
-export([findpi/1]).
-export([findpi/5]).


%===================================================================================================================
%Main function- findpi

%First function takes in single parameter and then calls findpi functions with multiple parameters
%passes in Previous pi value, current pi value and count all set to 0
% passes in the user defined number of places with guards
% guard to stop the user entering about 7 places as on most machines will run out of memory
%guard also in places to prevent user entering negative numbers, as this would result in infinite loop

findpi(NumOfPlaces) when NumOfPlaces > 0, NumOfPlaces < 8, is_integer(NumOfPlaces) ->
	findpi(0,0,NumOfPlaces,1,0).	

%Determines  whether to add the denominator or subtract based on position in sequence
findpi(PreviousPi,CurrentPi,NumOfPlaces,Denominator,Count)->
if 
	Count rem 2 == 0.0 ->
		Pi= (CurrentPi+4*(1/Denominator));	
true -> 
		Pi= (CurrentPi-4*(1/Denominator))
end,

%Gets the old pi and new pi values truncated to x decimal places
DecimalNewPi = get_Pi_to_X_Places(Pi,NumOfPlaces),
DecimalOldPi = get_Pi_to_X_Places(PreviousPi,NumOfPlaces),

%Compare old pi to new pi. If not equal, more recursive iterations required for accuracy
%If equal stop recursion and return
if 
	DecimalNewPi /= DecimalOldPi ->
		OldPi = Pi,
		findpi(OldPi,Pi,NumOfPlaces,Denominator+2,Count+1);
	true ->

		%converts the number of places into list to format io output to print x number of decimal places
        X = integer_to_list(NumOfPlaces),
        
        io:format("The value of pi to ~w", [NumOfPlaces]),
        io:fwrite(" decimal places is: "),
		io:format("~." ++ X ++ "f~n",[DecimalNewPi])
end.
%==================================================================================================================

%=====================================================================================================================
%Takes current value of pi and truncates to allow comparison of old Pi and current pi to the desired decimal places

get_Pi_to_X_Places(Num, Precision) ->
    	
	Magnitude = math:pow(10, Precision),
	
	%Multiply current Pi by the magnitude to get Pi to x places as whole number
	X=Num*Magnitude,
	
	%Remove decimals, keep only integer number of pi
	IntPi= trunc(X),
	
	%Convert integer back to float without exccess decimal places 
	IntPi/Magnitude.
%=============================================================================================================================
