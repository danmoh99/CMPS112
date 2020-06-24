% Name: Danyal Mohammad
% CruzID: dimohamm
% Partner: Keyton Rogers
% Partner CruzID: jakroger
% Date: March 14, 2019
% File: functions.pl
% Description: File implementing flight scheduling code

not(X) :- X, !, fail.
not(_).

remainder(Float, Modulus, Quotient, Remainder) :-
  Quotient is truncate(Float/Modulus),
  Remainder is Float - Quotient * Modulus.

ispath(L, M) :- ispath2(L, M, []).

ispath2(L, L, Path) :-
  reverse(Path, X),
  [H|T] = X,
  writepath([H|T]).

ispath2(L, M, Path) :-
  airport(L, _, degmin(FirstLatDeg, FirstLatMin), 
    degmin(FirstLonDeg, FirstLonMin)),
  
  Lat1 is (FirstLatDeg + (FirstLatMin/60)) * pi/180,
  Lon1 is (FirstLonDeg + (FirstLonMin/60)) * pi/180,
  
  (

  length(Path, Z),
  (not(Z == 0) ->
  [H1|T1] = Path,
  [H2|T2] = H1,
  [H3|T3] = T2,
  [H4|T4] = H3,
  [Jesus|T5] = T4,
  remainder(Jesus, 60, Hour, Min)
  ;
  true),
  flight(L, M, time(Hour, Min)) -> 
  airport(M, DestName, degmin(DestLatDeg, DestLatMin), 
    degmin(DestLonDeg, DestLonMin)),
  Lat2 is (DestLatDeg + (DestLatMin/60)) * pi/180,
  Lon2 is (DestLonDeg + (DestLonMin/60)) * pi/180,
  Dlon is Lon2 - Lon1,
  Dlat is Lat2 - Lat1,
  Tmpa is ((sin(Dlat/2)) * (sin(Dlat/2)))
              + cos(Lat1)*cos(Lat2) * ((sin(Dlon/2) * (sin(Dlon/2)))),
  UnitDistance is 2*atan2(sqrt(Tmpa), sqrt(1-Tmpa)),
  DistanceMiles is 3961 * UnitDistance,

  Duration is (DistanceMiles/500)*60,
  RawTime is (Hour * 60) + Min,
    ArrivalTime is RawTime + Duration,
  ispath2(M, M, [[[L, RawTime], [M, ProspTime]]|Path])
  ;
  flight(L, X, time(Hour, Min)),
  not(member([[X,_], _], Path)),
  airport(X, DestName, degmin(DestLatDeg, DestLatMin), 
    degmin(DestLonDeg, DestLonMin)),

  Lat2 is (DestLatDeg + (DestLatMin/60)) * pi/180,
  Lon2 is (DestLonDeg + (DestLonMin/60)) * pi/180,
  Dlon is Lon2 - Lon1,
  Dlat is Lat2 - Lat1,
  Tmpa is ((sin(Dlat/2)) * (sin(Dlat/2)))
              + cos(Lat1)*cos(Lat2) * ((sin(Dlon/2) * (sin(Dlon/2)))),
  UnitDistance is 2*atan2(sqrt(Tmpa), sqrt(1-Tmpa)),
  DistanceMiles is 3961 * UnitDistance,

  Duration is (DistanceMiles/500)*60,
  RawTime is (Hour * 60) + Min,
  ArrivalTime is RawTime + Duration,
  flight(X, Y, time(NewHour, NewMin)),

  not(member([[Y,_], _], Path)),
  ProspTime is (NewHour * 60) + NewMin,
  ProspTime > RawTime,
  (ProspTime - ArrivalTime) >= 30,
  length(Path, Z),
  (not(Z == 0) ->
  [H1|T1] = Path,
  [H2|T2] = H1,
  [H3|T3] = T2,
  [H4|T4] = H3,
  [Jesus|T5] = T4,
  RawTime >= Jesus;
  true),
  ispath2(X, M, [[[L,RawTime], [X,ProspTime]]|Path])).

to_upper( Lower, Upper) :-
   atom_chars( Lower, Lowerlist),
   maplist( lower_upper, Lowerlist, Upperlist),
   atom_chars( Upper, Upperlist).

print_trip( Action, Code, Name, time( Hour, Minute)) :-
   to_upper( Code, Upper_code),
   format( "%-6s  %3s  %-16s  %02d:%02d",
           [Action, Upper_code, Name, Hour, Minute]),
   nl.



writepath([]).
writepath([Head|Tail]) :-
  [H1|T1] = Head,
  [Source|STimeH] = H1,
  [H2|_] = T1,
  [Dest|_] = H2,
  [STime|_] = STimeH,
  airport(Source, SourceName, degmin(LatDeg, LatMin), 
    degmin(LonDeg, LonMin)),
  airport(Dest, DestName, degmin(DestLatDeg, DestLatMin),
    degmin(DestLonDeg, DestLonMin)),
  Lat1 is (LatDeg + (LatMin/60)) * pi/180,
  Lon1 is (LonDeg + (LonMin/60)) * pi/180,
  Lat2 is (DestLatDeg + (DestLatMin/60)) * pi/180,
  Lon2 is (DestLonDeg + (DestLonMin/60)) * pi/180,
  Dlon is Lon2 - Lon1,
  Dlat is Lat2 - Lat1,
  Tmpa is ((sin(Dlat/2)) * (sin(Dlat/2)))
              + cos(Lat1)*cos(Lat2) * ((sin(Dlon/2) * (sin(Dlon/2)))),
  UnitDistance is 2*atan2(sqrt(Tmpa), sqrt(1-Tmpa)),
  DistanceMiles is 3961 * UnitDistance,

  Duration is (DistanceMiles/500)*60,
  remainder(STime, 60, SHour, SMin),
  RawTime is (SHour * 60) + SMin,
  ArrivalTime is RawTime + Duration,
  remainder(round(ArrivalTime), 60, Hour, Min),
  print_trip(depart, Source, SourceName, time(SHour, SMin)),
  print_trip(arrive, Dest, DestName, time(Hour, Min)),
  writepath(Tail).

fly(Source, Dest) :-
  Source == Dest ->
  fail;
  ispath(Source, Dest).
