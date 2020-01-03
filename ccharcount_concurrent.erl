-module (ccharcount_concurrent).
-export ([load/1,count/3,go/1,join/2,split/2, process_parts_concurrently/2,countsplit/2,join_messages/3]).


%=================================================================================================================================================
%load- main function
load(F)->
   %loads file, reads as binary
   {ok, Bin} = file:read_file(F),
   
   %Convert binary from file to list (string) cause string= list of int
   List=binary_to_list(Bin),
  
   %Get number of charcters in list when file is split in 20 chunks (lists) 
   Length=round(length(List)/20),
  
   %Converts all characters to lowercase
   Ls=string:to_lower(List),
  
   %Creates 20 chunks(lists) from initial list 
   Sl=split(Ls,Length),
   %Prints status feedback
   io:fwrite("Loaded and Split~n"),

   %Places timestamp for speed calculation
   OrigTime = erlang:timestamp(),
  
   %Spawning join process takes 3 parameters- list of current count, the number of chunks, the pid value of current proccess
   Joiner_PID = spawn(ccharcount_concurrent, join_messages, [[], length(Sl), self()]),
  
   %Take in list of chunks and proccess concurrenly
   process_parts_concurrently(Sl,Joiner_PID),
   
   %Wait joiner process to be finished
   receive
      %final list of results printed 
      {Results}->
         io:fwrite("Time Taken:~ps~n",[timer:now_diff(erlang:timestamp(),OrigTime)*0.000001]),
         Results
   end.
%=================================================================================================================






%=========================================================================================================
%Takes in the list of chunks and joiner proccess ID.
%spawns new process to count number of letters in each chunk, before sending back to Joiner process
process_parts_concurrently([],_)->ok;
process_parts_concurrently([H|T], Joiner_PID)->
   Spawn_PID = spawn(ccharcount_concurrent, countsplit, [H, Joiner_PID]),
   process_parts_concurrently(T, Joiner_PID).
%=========================================================================================================

%=============================================================
%count characters of seperate chunks
%Chunk = a portion of the original list
%sends results back to joiner process

countsplit(Chunk,Joiner_PID)->
   Result=go(Chunk),
   %io:fwrite("~nprocessed 1 chunk", []),
   %sends results back to joiner process (join_messages)
   Joiner_PID ! {Result}.
%==============================================================

%======================================================================================================
%Joiner process
%Awaits worker processes to send a new result and joins it to any existing results
%Once number of chunks hits 0, sends all current results to parent process

join_messages(CurrentResults, 0, Parent_PID)-> Parent_PID ! {CurrentResults};
join_messages(CurrentResults, Length, Parent_PID)->
   %wait for new result to be sent by worker process
   receive
      {Msg}->
         %join result to existing result list
         NewResults = join(CurrentResults, Msg), 
        
         %call self again, to wait for another result from a different worker process
         join_messages(NewResults, Length-1, Parent_PID);
      
      %consume error message
     _Other -> {error, unknown}
   end.
%=========================================================================================================

%=================================================================================================================================================
%adds character counts from one list to the character count to another continuously adding the rest of the list with the other characters back on
%unchanged in current implementation
join([],[])->[];
join([],R)->R;
join([H1 |T1],[H2|T2])->
{C,N}=H1,
{C1,N1}=H2,
[{C1,N+N1}]++join(T1,T2).
%=========================================================================================================================================================

%=================================================================
%split enterterd text into list
%splits full list into charcaters
%put into list of character
%unchanged in current implementation

split([],_)->[];
split(List,Length)->
S1=string:substr(List,1,Length),
case length(List) > Length of
   true->S2=string:substr(List,Length+1,length(List));
   false->S2=[]
end,
[S1]++split(S2,Length).
%=============================================================


%====================================================
%count number of specific character in a list
%unchanged in current implementation

count(Ch, [],N)->N;
count(Ch, [H|T],N) ->
   case Ch==H of
   true-> count(Ch,T,N+1);
   false -> count(Ch,T,N)
end.
%==================================================

%========================================================================================================================================
%unchanged in current implementation
%makes list of all characters in alphabet (alph) then call rgo for each letter, passing in the current chunk being processed

go(L)->
Alph=[$a,$b,$c,$d,$e,$f,$g,$h,$i,$j,$k,$l,$m,$n,$o,$p,$q,$r,$s,$t,$u,$v,$w,$x,$y,$z],
rgo(Alph,L,[]).
%====================================================================================================================================

%==================================================================================================================================
%unchanged in current implementation
%rgo looks through list for every letter in the alphabet and calls function to count number of times it occurs in this chunk
rgo([H|T],L,Result)->
N=count(H,L,0),
Result2=Result++[{[H],N}],
rgo(T,L,Result2);

rgo([],L,Result)->Result.
%====================================================================================================================================