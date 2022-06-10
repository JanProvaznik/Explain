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
unique_teachers([],0).
unique_teachers([X|Rest],Unique):-
    member(X,Rest) -> unique_teachers(Rest,Unique); unique_teachers(Rest,Unique1),Unique #= Unique1+1.

combine(A,B,C):-
    C#=A*100+B.
list_pair(A,B,[A,B]).
rounds9(CanExplain,WantsExplain,[Round1,Round2,Round3]):-
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
    FristRoundPeople = [T0,S00,S01,T1,S10,S11,T2,S20,S21],
    SecondRoundPeople = [T3,S30,S31,T4,S40,S41,T5,S50,S51],
    ThirdRoundPeople = [T6,S60,S61,T7,S70,S71,T8,S80,S81],
    append([AllConcepts,AllTeachers,AllFirstStudents,AllSecondStudents],Everything),

    maplist(list_pair,AllConcepts,AllTeachers,CETuples),
    maplist(list_pair,AllConcepts,AllFirstStudents,WETuples1),
    maplist(list_pair,AllConcepts,AllSecondStudents,WETuples2),
    append(WETuples1,WETuples2,WETuples),
    % only those who can explain do
    tuples_in(CETuples, CanExplain),
    % only those who want explained are explained to
    tuples_in(WETuples, WantsExplain),

    all_distinct(FristRoundPeople),
    all_distinct(SecondRoundPeople),
    all_distinct(ThirdRoundPeople),

    % teachers in a round are differnt and ordered to reduce search space
    maplist(#<,[T0,T1],[T1,T2]),
    maplist(#<,[T3,T4],[T4,T5]),
    maplist(#<,[T6,T7],[T7,T8]),
    % ordering of students reduces search space
    maplist(#<,AllFirstStudents,AllSecondStudents),

    % if a person is explained a concept C, then they should not be explained the same concept C again
    maplist(combine,AllConcepts,AllFirstStudents,Ls1),
    maplist(combine,AllConcepts,AllSecondStudents,Ls2),
    append(Ls1,Ls2,Ls),
    all_distinct(Ls),

    % if a person explains concept C, then they should not explain the same concept C again
    maplist(combine,AllConcepts,AllTeachers,Ks),
    all_distinct(Ks),
    

    % ideally no one should explain twice, but there is 'or' if it's inevitable
    (all_distinct(AllTeachers);
    all_distinct([T0,T1,T2,T3,T4,T5,T6,T7]);
    all_distinct([T0,T1,T2,T3,T4,T5,T6]);
    all_distinct([T0,T1,T2,T3,T4,T5]);
    true
    ),
    labeling([ff],Everything).


group_to_SCPairs(group(_,[S1,S2],C),[[C,S1],[C,S2]]).
member_of(X,Y):-member(Y,X).
addIf(Predicate,X,Acc0,Acc1):-
    call(Predicate,X) -> Acc1 #= Acc0 + 1; Acc1#=Acc0.
% the score is how many explanations are to people who only maybe wanted the concept explained
score(HaveExplain,[Round1,Round2,Round3],Score):-
    maplist(group_to_SCPairs,Round1,Round1Pairs),
    maplist(group_to_SCPairs,Round2,Round2Pairs),
    maplist(group_to_SCPairs,Round3,Round3Pairs),
    append([Round1Pairs,Round2Pairs,Round3Pairs],AllPairs),
    append(AllPairs,AllPairsFlat),
    foldl(addIf(member_of(HaveExplain)),AllPairsFlat,0,Score).

    

getCE(CanExplain):-
    findall([C,T],(can_explain(T,C)),CanExplain).
getWE(WantsExplain):-
    findall([C,S],(wants_have_explained(S,C)),WantsExplain).
getHE(HaveExplain):-
    findall([C,S],(really_wants_have_explained(S,C)),HaveExplain).

% converting rows to dicts where keys are integers and values concepts/people    
header_to_conceptdict(Header,ConceptDict):- 
    row_to_list(Header,[_|ConceptsList]),
    list_to_dict(ConceptsList,ConceptDict).

rows_to_peopledict(Rows,PeopleDict):- 
    rows_to_lists(Rows,RowsLL),
    transpose(RowsLL,[FR|_]),
    list_to_dict(FR,PeopleDict).

rounds(9,CanExplain,WantsExplain,Rounds):- rounds9(CanExplain,WantsExplain,Rounds).
rounds(15,CanExplain,WantsExplain,Rounds):- rounds15(CanExplain,WantsExplain,Rounds).
rounds(21,CanExplain,WantsExplain,Rounds):- rounds21(CanExplain,WantsExplain,Rounds).
rounds(27,CanExplain,WantsExplain,Rounds):- rounds27(CanExplain,WantsExplain,Rounds).
rounds(N,_,_,[]):-N #\=9,N#\=15,N#\=21,N#\=27,
    print("Invalid number of people: only 9,15,21,27 are supported. If you cannot divide your group to subgroups with those counts, add fake people who want everything explained or assign groups leftovers manually."),
    halt(1).

explain(InPath,OutPath,Out) :-
    % input -> internal representation 
    csv_read_file(InPath,[Header|InRows]),
    header_to_conceptdict(Header,ConceptDict),
    rows_to_peopledict(InRows,PeopleDict),
    length(PeopleDict,PeopleCount),
    rows_to_rels(PeopleDict,ConceptDict,Header,InRows,RelationsUnfiltered), 
    filter_does_not_want(RelationsUnfiltered,Relations),
    maplist(assertz,Relations),
    getCE(CanExplain),
    getWE(WantsExplain),
    rounds(PeopleCount,CanExplain,WantsExplain,Rounds),
    % measuring how good is the solution in terms of how many people who really wanted something explained actually have it explained  
    getHE(HaveExplain),
    score(HaveExplain,Rounds,ReallyWantedExplainedScore),
    % generated solution -> output csv
    rounds_to_rows(PeopleDict,ConceptDict,Rounds,OutputRows),
    Out="how many explanations were not only maybe wanted (higher is better):"-ReallyWantedExplainedScore-"                  "-OutputRows,
    outheader(OutHeader),
    csv_write_file(OutPath, [OutHeader|OutputRows],[]).

%% Bellow are other variants for the rounds method 
rounds15(CanExplain,WantsExplain,[Round1,Round2,Round3]):-
    Round1 = [  group(T0,[S00,S01],C0),
                group(T1,[S10,S11],C1),
                group(T2,[S20,S21],C2),
                group(T3,[S30,S31],C3),
                group(T4,[S40,S41],C4)
                ],
    Round2 = [  group(T5,[S50,S51],C5),
                group(T6,[S60,S61],C6),
                group(T7,[S70,S71],C7),
                group(T8,[S80,S81],C8),
                group(T9,[S90,S91],C9)
                ],
    Round3 = [  group(T10,[S100,S101],C10),
                group(T11,[S110,S111],C11),
                group(T12,[S120,S121],C12),
                group(T13,[S130,S131],C13),
                group(T14,[S140,S141],C14)
                ],
    AllConcepts = [C0,C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12,C13,C14],
    AllTeachers = [T0,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14],
    AllFirstStudents = [S00,S10,S20,S30,S40,S50,S60,S70,S80,S90,S100,S110,S120,S130,S140],
    AllSecondStudents = [S01,S11,S21,S31,S41,S51,S61,S71,S81,S91,S101,S111,S121,S131,S141],
    FristRoundPeople = [T0,S00,S01,T1,S10,S11,T2,S20,S21,T3,S30,S31,T4,S40,S41],
    SecondRoundPeople = [T5,S50,S51,T6,S60,S61,T7,S70,S71,T8,S80,S81,T9,S90,S91],   
    ThirdRoundPeople = [T10,S100,S101,T11,S110,S111,T12,S120,S121,T13,S130,S131,T14,S140,S141],

    append([AllConcepts,AllTeachers,AllFirstStudents,AllSecondStudents],Everything),

    maplist(list_pair,AllConcepts,AllTeachers,CETuples),
    maplist(list_pair,AllConcepts,AllFirstStudents,WETuples1),
    maplist(list_pair,AllConcepts,AllSecondStudents,WETuples2),
    append(WETuples1,WETuples2,WETuples),
    tuples_in(CETuples, CanExplain),
    tuples_in(WETuples, WantsExplain),

    all_distinct(FristRoundPeople),
    all_distinct(SecondRoundPeople),
    all_distinct(ThirdRoundPeople),

    maplist(#<,[T0,T1,T2,T3],[T1,T2,T3,T4]),
    maplist(#<,[T5,T6,T7,T8],[T6,T7,T8,T9]),
    maplist(#<,[T10,T11,T12,T13],[T11,T12,T13,T14]),

    maplist(#<,AllFirstStudents,AllSecondStudents),

    maplist(combine,AllConcepts,AllFirstStudents,Ls1),
    maplist(combine,AllConcepts,AllSecondStudents,Ls2),
    append(Ls1,Ls2,Ls),
    all_distinct(Ls),

    maplist(combine,AllConcepts,AllTeachers,Ks),
    all_distinct(Ks),

    labeling([ff],Everything).

rounds21(CanExplain,WantsExplain,[Round1,Round2,Round3]):-
    Round1 = [  group(T0,[S00,S01],C0),
                group(T1,[S10,S11],C1),
                group(T2,[S20,S21],C2),
                group(T3,[S30,S31],C3),
                group(T4,[S40,S41],C4),
                group(T5,[S50,S51],C5),
                group(T6,[S60,S61],C6)
                ],
    Round2 = [  group(T7,[S70,S71],C7),
                group(T8,[S80,S81],C8),
                group(T9,[S90,S91],C9),
                group(T10,[S100,S101],C10),
                group(T11,[S110,S111],C11),
                group(T12,[S120,S121],C12),
                group(T13,[S130,S131],C13)
                ],
    Round3 = [  group(T14,[S140,S141],C14),
                group(T15,[S150,S151],C15),
                group(T16,[S160,S161],C16),
                group(T17,[S170,S171],C17),
                group(T18,[S180,S181],C18),
                group(T19,[S190,S191],C19),
                group(T20,[S200,S201],C20)
                ],
    
    AllConcepts = [C0,C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12,C13,C14,C15,C16,C17,C18,C19,C20],
    AllTeachers = [T0,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20],
    AllFirstStudents = [S00,S10,S20,S30,S40,S50,S60,S70,S80,S90,S100,S110,S120,S130,S140,S150,S160,S170,S180,S190,S200],
    AllSecondStudents = [S01,S11,S21,S31,S41,S51,S61,S71,S81,S91,S101,S111,S121,S131,S141,S151,S161,S171,S181,S191,S201],
    FristRoundPeople = [T0,S00,S01,T1,S10,S11,T2,S20,S21,T3,S30,S31,T4,S40,S41,T5,S50,S51,T6,S60,S61],
    SecondRoundPeople = [T7,S70,S71,T8,S80,S81,T9,S90,S91,T10,S100,S101,T11,S110,S111,T12,S120,S121,T13,S130,S131],
    ThirdRoundPeople = [T14,S140,S141,T15,S150,S151,T16,S160,S161,T17,S170,S171,T18,S180,S181,T19,S190,S191,T20,S200,S201],


    maplist(list_pair,AllConcepts,AllTeachers,CETuples),
    maplist(list_pair,AllConcepts,AllFirstStudents,WETuples1),
    maplist(list_pair,AllConcepts,AllSecondStudents,WETuples2),
    append(WETuples1,WETuples2,WETuples),
    tuples_in(CETuples, CanExplain),
    tuples_in(WETuples, WantsExplain),

    all_distinct(FristRoundPeople),
    all_distinct(SecondRoundPeople),
    all_distinct(ThirdRoundPeople),

    maplist(#<,[T0,T1,T2,T3,T4,T5],[T1,T2,T3,T4,T5,T6]),
    maplist(#<,[T7,T8,T9,T10,T11,T12],[T8,T9,T10,T11,T12,T13]),
    maplist(#<,[T14,T15,T16,T17,T18,T19],[T15,T16,T17,T18,T19,T20]),

    maplist(#<,AllFirstStudents,AllSecondStudents),

    maplist(combine,AllConcepts,AllFirstStudents,Ls1),
    maplist(combine,AllConcepts,AllSecondStudents,Ls2),
    append(Ls1,Ls2,Ls),
    all_distinct(Ls),

    maplist(combine,AllConcepts,AllTeachers,Ks),
    all_distinct(Ks),

    append([AllConcepts,AllTeachers,AllFirstStudents,AllSecondStudents],Everything),
    labeling([ff],Everything).


% rounds27 works only sometimes

rounds27(CanExplain,WantsExplain,[Round1,Round2,Round3]):-
    Round1 = [  group(T0,[S00,S01],C0),
                group(T1,[S10,S11],C1),
                group(T2,[S20,S21],C2),
                group(T3,[S30,S31],C3),
                group(T4,[S40,S41],C4),
                group(T5,[S50,S51],C5),
                group(T6,[S60,S61],C6),
                group(T7,[S70,S71],C7),
                group(T8,[S80,S81],C8)
                ],
    Round2 = [  group(T9,[S90,S91],C9),
                group(T10,[S100,S101],C10),
                group(T11,[S110,S111],C11),
                group(T12,[S120,S121],C12),
                group(T13,[S130,S131],C13),
                group(T14,[S140,S141],C14),
                group(T15,[S150,S151],C15),
                group(T16,[S160,S161],C16),
                group(T17,[S170,S171],C17)
                ],
    Round3 = [  group(T18,[S180,S181],C18),
                group(T19,[S190,S191],C19),
                group(T20,[S200,S201],C20),
                group(T21,[S210,S211],C21),
                group(T22,[S220,S221],C22),
                group(T23,[S230,S231],C23),
                group(T24,[S240,S241],C24),
                group(T25,[S250,S251],C25),
                group(T26,[S260,S261],C26)
                ],
    
    AllConcepts = [C0,C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12,C13,C14,C15,C16,C17,C18,C19,C20,C21,C22,C23,C24,C25,C26],
    AllTeachers = [T0,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,T17,T18,T19,T20,T21,T22,T23,T24,T25,T26],
    AllFirstStudents = [S00,S10,S20,S30,S40,S50,S60,S70,S80,S90,S100,S110,S120,S130,S140,S150,S160,S170,S180,S190,S200,S210,S220,S230,S240,S250,S260],
    AllSecondStudents = [S01,S11,S21,S31,S41,S51,S61,S71,S81,S91,S101,S111,S121,S131,S141,S151,S161,S171,S181,S191,S201,S211,S221,S231,S241,S251,S261],
    FristRoundPeople = [T0,S00,S01,T1,S10,S11,T2,S20,S21,T3,S30,S31,T4,S40,S41,T5,S50,S51,T6,S60,S61,T7,S70,S71,T8,S80,S81],
    SecondRoundPeople = [T9,S90,S91,T10,S100,S101,T11,S110,S111,T12,S120,S121,T13,S130,S131,T14,S140,S141,T15,S150,S151,T16,S160,S161,T17,S170,S171],
    ThirdRoundPeople = [T18,S180,S181,T19,S190,S191,T20,S200,S201,T21,S210,S211,T22,S220,S221,T23,S230,S231,T24,S240,S241,T25,S250,S251,T26,S260,S261],


    maplist(list_pair,AllConcepts,AllTeachers,CETuples),
    maplist(list_pair,AllConcepts,AllFirstStudents,WETuples1),
    maplist(list_pair,AllConcepts,AllSecondStudents,WETuples2),
    append(WETuples1,WETuples2,WETuples),
    tuples_in(CETuples, CanExplain),
    tuples_in(WETuples, WantsExplain),

    all_distinct(FristRoundPeople),
    all_distinct(SecondRoundPeople),
    all_distinct(ThirdRoundPeople),

    maplist(#<,[T0,T1,T2,T3,T4,T5,T6,T7],[T1,T2,T3,T4,T5,T6,T7,T8]),
    maplist(#<,[T9,T10,T11,T12,T13,T14,T15,T16],[T10,T11,T12,T13,T14,T15,T16,T17]),
    maplist(#<,[T18,T19,T20,T21,T22,T23,T24,T25],[T19,T20,T21,T22,T23,T24,T25,T26]),

    maplist(#<,AllFirstStudents,AllSecondStudents),

    maplist(combine,AllConcepts,AllFirstStudents,Ls1),
    maplist(combine,AllConcepts,AllSecondStudents,Ls2),
    append(Ls1,Ls2,Ls),
    all_distinct(Ls),

    maplist(combine,AllConcepts,AllTeachers,Ks),
    all_distinct(Ks),

    append([AllConcepts,AllTeachers,AllFirstStudents,AllSecondStudents],Everything),
    labeling([ff],Everything).