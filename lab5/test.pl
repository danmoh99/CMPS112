portExists(Code) :-
  airport(Code, Name, degmin(Latdeg, Latmin), degmin(Longdeg, Longmin)),
  write(Name), nl.
