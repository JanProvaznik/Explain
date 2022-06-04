:- use_module(library(csv)).
:- use_module(library(clpfd)).
:- use_module(library(apply)).

:- dynamic relation/3.

% region: internal numeric representation of relations
range(Lo,Hi,Range):-findall(N,between(Lo,Hi,N),Range).
pair(A, B, A : B).
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
wants_have_explained(Student,Concept):- really_wants_have_explained(Student,Concept) ; maybe_wants_have_explained(Student,Concept).

relation_to_numeric(PeopleDict,ConceptDict,relation(Person,Concept,Action),relation(PersonN,ConceptN,ActionN)):-
    member(PersonN:Person,PeopleDict),
    member(ConceptN:Concept,ConceptDict),
    numeric(Action,ActionN).

% endregion

% region: IO
% generate a header
outheader(row("concept","teacher","student1","student2","student3")).

not_interested(relation(_,_,X)):- 
    numeric('N',Z),
    X #= Z.
% creating a relation functor 
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

list_of_numbers_to_people(_,[],[]).
list_of_numbers_to_people(PeopleDict,[N|Numbers],[P|People]):-
    member(N:P,PeopleDict),
    list_of_numbers_to_people(PeopleDict,Numbers,People).

% converts an integer group to a list of Strings
group_to_string_list(PeopleDict,ConceptDict,group(Teacher,Students,Concept),[ConceptS,TeacherS|StudentsS]):-
    member(Teacher:TeacherS, PeopleDict),member(Concept:ConceptS,ConceptDict),list_of_numbers_to_people(PeopleDict,Students,StudentsS).

% rounds contain integer groups, need to convert them to string groups for output
rounds_to_rows(PeopleDict,ConceptDict,Rounds,Rows):-
    Rounds = [R1,R2,R3],
    maplist(group_to_string_list(PeopleDict,ConceptDict),R1,R1L),
    maplist(group_to_string_list(PeopleDict,ConceptDict),R2,R2L),
    maplist(group_to_string_list(PeopleDict,ConceptDict),R3,R3L),
    maplist(list_to_row,R1L,Rows1),
    maplist(list_to_row,R2L,Rows2),
    maplist(list_to_row,R3L,Rows3),
    % join and add spaces between rounds
    append(Rows1,[row('')|Rows2],Rows_),
    append(Rows_,[row('')|Rows3],Rows).

rows_to_lists(Rows, Lists):-
  maplist(row_to_list, Rows, Lists).

row_to_list(Row, List):-
    Row =.. [row|List].

list_to_row(List,Row):-row_to_list(Row,List).

filter_does_not_want(Relations,FilteredRelations):-
    exclude(not_interested,Relations,FilteredRelations).
% endregion: IO

% one student in two rounds has to have a different concept
pair_compatible((S1,C1),(S2,C2)):-
    (S1 #= S2) #==> (C1 #\= C2).
    
compatible_with(Exs,X):-
    select(X, Exs, Rest),% don't compare with the same variable
    maplist(pair_compatible(X),Rest).

no_one_twice(Exs):-
    maplist(compatible_with(Exs),Exs).

pair3(X,X-3).
% the simple case
rounds9(ConceptCount,[Round1,Round2,Round3]):-
    Round1 = [  group(T0,[S00,S01],C0),
                group(T1,[S10,S11],C1),
                group(T2,[S20,S21],C2)],

    Round2 = [  group(T3,[S30,S31],C3),
                group(T4,[S40,S41],C4),
                group(T5,[S50,S51],C5)],

    Round3 = [  group(T6,[S60,S61],C6),
                group(T7,[S70,S71],C7),
                group(T8,[S80,S81],C8)],
    AllConcepts = [C0,C1,C2,C3,C4,C5,C6,C7,C8],
    AllTeachers = [T0,T1,T2,T3,T4,T5,T6,T7,T8],
    AllFirstStudents = [S00,S10,S20,S30,S40,S50,S60,S70,S80],
    AllSecondStudents = [S01,S11,S21,S31,S41,S51,S61,S71,S81],
    AllPeople = [T0,T1,T2,T3,T4,T5,T6,T7,T8,S00,S01,S10,S11,S20,S21,S30,S31,S40,S41,S50,S51,S60,S61,S70,S71,S80,S81],
    append(AllConcepts,AllPeople,Everything),
    % restricts the domain as well as enforcing each person is assigned thrice
    range(1,9,Range),
    maplist(pair3,Range,Cardinalities),
    global_cardinality(AllPeople,Cardinalities),
    % concepts are in the domain of 1..ConceptCount
    AllConcepts ins 1..ConceptCount,
    % teachers in a round are differnt and ordered to reduce search space
    T0#<T1,T1#<T2,
    T3#<T4,T4#<T5,
    T6#<T7,T7#<T8,

    % ordering of students reduces search space
    maplist(#<,AllFirstStudents,AllSecondStudents),

    % students in rounds are different
    all_distinct([S00,S01,S10,S11,S20,S21]),
    all_distinct([S30,S31,S40,S41,S50,S51]),
    all_distinct([S60,S61,S70,S71,S80,S81]),

    % teacher in one group can't be a student in the same round
    all_distinct([T0,S10,S11,S20,S21]),
    all_distinct([T1,S00,S01,S20,S21]),
    all_distinct([T2,S00,S01,S10,S11]),

    all_distinct([T3,S40,S41,S50,S51]),
    all_distinct([T4,S30,S31,S50,S51]),
    all_distinct([T5,S30,S31,S40,S41]),

    all_distinct([T6,S70,S71,S80,S81]),
    all_distinct([T7,S60,S61,S80,S81]),
    all_distinct([T8,S60,S61,S70,S71]),

    % if a person is explained a concept C, then they should not be explained the same concept C again
    Explanations = [(S00,C0),(S01,C0),(S10,C1),(S11,C1),(S20,C2),(S21,C2),(S30,C3),(S31,C3),(S40,C4),(S41,C4),(S50,C5),(S51,C5),(S60,C6),(S61,C6),(S70,C7),(S71,C7),(S80,C8),(S81,C8)],
    no_one_twice(Explanations),

    % ideally no one should explain twice, but there is 'or' if it's inevitable
    (all_distinct(AllTeachers); 
    all_distinct([T0,T1,T2,T3,T4,T5,T6,T7]);
    all_distinct([T0,T1,T2,T3,T4,T5,T6]);
    all_distinct([T0,T1,T2,T3,T4,T5]);
    all_distinct([T0,T1,T2,T3,T4]);
    all_distinct([T0,T1,T2,T3])
    ),

    maplist(can_explain,AllTeachers,AllConcepts),
    maplist(wants_have_explained,AllFirstStudents,AllConcepts),
    maplist(wants_have_explained,AllSecondStudents,AllConcepts),

    labeling([ff],Everything).

% converting rows to dicts where keys are integers and values concepts/people    
header_to_conceptdict(Header,ConceptDict):- 
    row_to_list(Header,[_|ConceptsList]),
    list_to_dict(ConceptsList,ConceptDict).

rows_to_peopledict(Rows,PeopleDict):- 
    rows_to_lists(Rows,RowsLL),
    transpose(RowsLL,[FR|_]),
    list_to_dict(FR,PeopleDict).

explain(InPath,OutPath,OutputRows) :-
    % input -> internal representation 
    csv_read_file(InPath,[Header|InRows]),
    header_to_conceptdict(Header,ConceptDict),
    rows_to_peopledict(InRows,PeopleDict),
    % length(PeopleDict,PeopleCount), % this might be used for more people than 9
    length(ConceptDict,ConceptCount),
    rows_to_rels(PeopleDict,ConceptDict,Header,InRows,RelationsUnfiltered),
    filter_does_not_want(RelationsUnfiltered,Relations),
    maplist(assertz,Relations),
    rounds9(ConceptCount,Rounds),
    % generated solution -> output csv
    rounds_to_rows(PeopleDict,ConceptDict,Rounds,OutputRows),
    outheader(OutHeader),
    csv_write_file(OutPath, [OutHeader|OutputRows],[]).