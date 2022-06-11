:- use_module(library(csv)).
:- use_module(library(clpfd)).
:- use_module(library(apply)).

:- dynamic relation/3.

% region util
range(Lo,Hi,Range):-findall(N,between(Lo,Hi,N),Range).
pair(A, B, A : B).
list_pair(A,B,[A,B]).
group_to_SCPairs(group(_,[S1,S2],C),[[C,S1],[C,S2]]).
member_of(X,Y):-member(Y,X).
addIf(Predicate,X,Acc0,Acc1):-
    call(Predicate,X) -> Acc1 #= Acc0 + 1; Acc1#=Acc0.
% endregion util
% region: internal numeric representation of relations
% can be used both for People and Concepts, dict has structure Number : Concept
list_to_dict(L, Dict) :-
    length(L, Len),
    range(1,Len,Nums),
    maplist(pair,Nums,L,Dict).

% numbers don't have meaning
numeric('N',0).
numeric('MHE',1).
numeric('HE',3).
numeric('CE',-6).
% better usability than relations: it's possible to maplist these
can_explain(Teacher,Concept):-relation(Teacher,Concept,-6).
really_wants_have_explained(Student,Concept):-relation(Student,Concept,3).
maybe_wants_have_explained(Student,Concept):-relation(Student,Concept,1).
wants_have_explained(Student,Concept):-
     really_wants_have_explained(Student,Concept) ;
     maybe_wants_have_explained(Student,Concept).

relation_to_numeric(PeopleDict,ConceptDict,
    relation(Person,Concept,Action),relation(PersonN,ConceptN,ActionN)):-
    member(PersonN:Person,PeopleDict),
    member(ConceptN:Concept,ConceptDict),
    numeric(Action,ActionN).

not_interested(relation(_,_,X)):- 
    numeric('N',Z),
    X #= Z.

list_of_numbers_to_people(_,[],[]).
list_of_numbers_to_people(PeopleDict,[N|Numbers],[P|People]):-
    member(N:P,PeopleDict),
    list_of_numbers_to_people(PeopleDict,Numbers,People).

filter_does_not_want(Relations,FilteredRelations):-
    exclude(not_interested,Relations,FilteredRelations).
% endregion internal numeric representation of relations

% region IO
outheader(row("concept","teacher","student1","student2")).

to_relation(Name,Concept,Action, relation(Name,Concept,Action)).

% list of rows -> list of relation functors
rowlist_to_rels([_|HeaderL],[Name|RowL],Rels):-
    maplist(to_relation(Name),HeaderL,RowL,Rels).

% converts inner rows of csv to numeric relation functors
rows_to_rels(PeopleDict,ConceptDict,Header,Rows,Rels):-
    row_to_list(Header,HeaderL),
    rows_to_lists(Rows,RowsL),
    maplist(rowlist_to_rels(HeaderL),RowsL,RelsLL),
    flatten(RelsLL,RelsS),
    maplist(relation_to_numeric(PeopleDict,ConceptDict),RelsS,Rels).

% converts an integer group to a list of Strings
group_to_string_list(PeopleDict,ConceptDict,
    group(Teacher,Students,Concept),[ConceptS,TeacherS|StudentsS]):-
    member(Teacher:TeacherS, PeopleDict),
    member(Concept:ConceptS,ConceptDict),
    list_of_numbers_to_people(PeopleDict,Students,StudentsS).

% like append/2 but puts row('') in between
join([], []).
  join([L|Ls], As) :-
  append(L, [row('')|Ws], As),
  join(Ls, Ws).

% rounds contain integer groups, need to convert them to string groups for output
rounds_to_rows(PeopleDict,ConceptDict,Rounds,Rows):-
    maplist(maplist(group_to_string_list(PeopleDict,ConceptDict)),Rounds,Lists),
    maplist(maplist(list_to_row),Lists,RoundsRows),
    % join and add spaces between rounds
    join(RoundsRows,Rows).

rows_to_lists(Rows, Lists):-
  maplist(row_to_list, Rows, Lists).

row_to_list(Row, List):-
    Row =.. [row|List].

list_to_row(List,Row):-row_to_list(Row,List).

% converting rows to dicts where keys are integers and values concepts/people    
header_to_conceptdict(Header,ConceptDict):- 
    row_to_list(Header,[_|ConceptsList]),
    list_to_dict(ConceptsList,ConceptDict).

rows_to_peopledict(Rows,PeopleDict):- 
    rows_to_lists(Rows,RowsLL),
    transpose(RowsLL,[FR|_]),
    list_to_dict(FR,PeopleDict).
% endregion IO
% the score is how many explanations are to people who only maybe wanted the concept explained

% region solution generation
% round_gen(Count,Round,People,Concepts,Teachers,FirstStudents,SecondStudents)
round_gen(1,[group(T,[S0,S1],C)],[T,S0,S1],[C],[T],[S0],[S1]).
round_gen(NumberGroups, [group(T,[S0,S1],C)|Rest],[T,S0,S1|PRest],[C|CRest],[T|TRest],[S0|S0Rest],[S1|S1Rest]):-
    length([_|Rest],NumberGroups),
    N #= NumberGroups - 1,
    round_gen(N,Rest,PRest,CRest,TRest,S0Rest,S1Rest).
    
% rounds_gen(RoundsCount,GroupCount,Rounds,RoundsPeople,RoundsConcepts,RoundsTeachers,RoundsFirstStudents,RoundsSecondStudents):-
rounds_gen(1,GroupCount,[Round1],[FirstRoundPeople],[R1Concepts],
    [R1Teachers],[R1FirstStudents],[R1SecondStudents]):-
    round_gen(GroupCount,Round1,FirstRoundPeople,R1Concepts,
        R1Teachers,R1FirstStudents,R1SecondStudents).

rounds_gen(N,GroupCount,[Round1|Rest],[FirstRoundPeople|RestPeople],[R1Concepts|RestConcepts],
    [R1Teachers|RestTeachers],[R1FirstStudents|RestFirstStudents],[R1SecondStudents|RestSecondStudents]):-
    round_gen(GroupCount,Round1,FirstRoundPeople,R1Concepts,
        R1Teachers,R1FirstStudents,R1SecondStudents),
    N0#= N-1,
    rounds_gen(N0,GroupCount,Rest,RestPeople,RestConcepts,
        RestTeachers,RestFirstStudents,RestSecondStudents).

teachers_ordered(Teachers):-
    Teachers = [_|Second],
    reverse(Teachers, [_|RFirst]),
    reverse(RFirst, First),
    maplist(#<,First,Second).

combine(A,B,C):-
    C#=A*100+B.

roundsN(RoundsCount,GroupCount,CanExplain,WantsExplain,Rounds):-
    rounds_gen(RoundsCount,GroupCount,
        Rounds,RoundsPeople,RoundsConcepts,RoundsTeachers,RoundsFirstStudents,RoundsSecondStudents),

    append(RoundsConcepts,AllConcepts),
    append(RoundsTeachers,AllTeachers),
    append(RoundsFirstStudents,AllFirstStudents),
    append(RoundsSecondStudents,AllSecondStudents),
    
    % everyone has their role 
    maplist(list_pair,AllConcepts,AllTeachers,CETuples),
    maplist(list_pair,AllConcepts,AllFirstStudents,WETuples1),
    maplist(list_pair,AllConcepts,AllSecondStudents,WETuples2),
    append(WETuples1,WETuples2,WETuples),
    tuples_in(CETuples, CanExplain),
    tuples_in(WETuples, WantsExplain),
    % no one is twice in one round 
    maplist(all_distinct,RoundsPeople),
    % order teachers to reduce search space
    maplist(teachers_ordered,RoundsTeachers),
    % order students to reduce search space
    maplist(#<,AllFirstStudents,AllSecondStudents),
    % no one learns or teaches the same concept twice
    maplist(combine,AllConcepts,AllFirstStudents,Ls1),
    maplist(combine,AllConcepts,AllSecondStudents,Ls2),
    append(Ls1,Ls2,Ls),
    all_distinct(Ls),
    maplist(combine,AllConcepts,AllTeachers,Ks),
    all_distinct(Ks),

    append([AllConcepts,AllTeachers,AllFirstStudents,AllSecondStudents],Everything),
    labeling([ff],Everything).
% endregion generation

getCE(CanExplain):-
    findall([C,T],(can_explain(T,C)),CanExplain).
getWE(WantsExplain):-
    findall([C,S],(wants_have_explained(S,C)),WantsExplain).
getHE(HaveExplain):-
    findall([C,S],(really_wants_have_explained(S,C)),HaveExplain).

score(HaveExplain,Rounds,Score):-
      maplist(maplist(group_to_SCPairs),Rounds,RoundsPairs),
      append(RoundsPairs,AllPairs),
      append(AllPairs,AllPairsFlat),
    foldl(addIf(member_of(HaveExplain)),AllPairsFlat,0,Score).

explain(InPath,OutPath,NumberOfRounds,Out) :-
    % input -> internal representation 
    csv_read_file(InPath,[Header|InRows]),
    header_to_conceptdict(Header,ConceptDict),
    rows_to_peopledict(InRows,PeopleDict),
    length(PeopleDict,PeopleCount),
    rows_to_rels(PeopleDict,ConceptDict,Header,InRows,RelationsUnfiltered), 
    filter_does_not_want(RelationsUnfiltered,Relations),
    maplist(assertz,Relations),
    getCE(CanExplain), getWE(WantsExplain),
    X #= PeopleCount div 3,
    roundsN(NumberOfRounds, X, CanExplain,WantsExplain,Rounds),
    % measuring how good is the solution in terms of how many people who really wanted something explained actually have it explained  
    getHE(HaveExplain),
    score(HaveExplain,Rounds,ReallyWantedExplainedScore),
    % generated solution -> output csv
    rounds_to_rows(PeopleDict,ConceptDict,Rounds,OutputRows),
    Out="how many explanations were not only maybe wanted (higher is better):"
        -ReallyWantedExplainedScore-"                  "-OutputRows,
    outheader(OutHeader),
    csv_write_file(OutPath, [OutHeader|OutputRows],[]).