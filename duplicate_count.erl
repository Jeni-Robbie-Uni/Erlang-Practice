-module(duplicate_count).
-export([count_duplicates/1]).
-export([load/1]).

%==============================================================
%load binary from file and converts to list
load(F)->
   %loads file, reads as binary
  
   {ok, Bin} = file:read_file(F),
   %converts binary file to list
   Content = unicode:characters_to_list(Bin),
   %separates string in to separate list items by space character
   List= string:tokens(Content, " "),
   count_duplicates(List).
%==============================================================



%===================================================================================
%count_duplicates-Main function that calls auxillary function
count_duplicates(FullList) ->
    %Creates new list removing duplicates and keeping order
    UniqueList = duplicate_delete(FullList),
    
    %Function returns all tuples made of list item and their count in a list
    getTuples(UniqueList,FullList,[]).
%====================================================================================
 
%==============================================================================================
%duplicate_delete-Remove the duplicate items in list to get searchlist
%patternMatching blanks where a nums been deleted or end of list

duplicate_delete([])    -> [];
duplicate_delete([Head|Tail]) -> [Head | [X || X <- duplicate_delete(Tail), X /= Head]].
%==============================================================================================

%==============================================================================================================
%getTuples counts number of list items occurence and adds the list item and count into a tuple
% UT- unique Tail
% UH- unique Head
% Temp list is the list of only the unique head values in the original list
%Occurence- the number of times the number appears in list calculated from length of complete list
%New Tuples 

getTuples([UH|UT], FullList,TupleList)-> 
    TempList = [X||X<-FullList, X==UH],
    Occurence = length(TempList),
    NewTuples=TupleList++[{UH,Occurence}],
    getTuples(UT, FullList,NewTuples);

%Get exit condition i.e. empty list is passed as parameter 
getTuples([],_FullList,TuplesList)->
    io:fwrite("~p~n", [TuplesList]).
    
%===============================================================================================================