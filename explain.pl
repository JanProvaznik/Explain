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

combine(A,B,C):-
    C#=A*100+B.
list_pair(A,B,[A,B]).
rounds9(CanExplain,WantsExplain,ConceptCount,[Round1,Round2,Round3]):-
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
    FristRoundPeople = [T0,S00,S01,T1,S10,S11,T2,S20,S21],
    SecondRoundPeople = [T3,S30,S31,T4,S40,S41,T5,S50,S51],
    ThirdRoundPeople = [T6,S60,S61,T7,S70,S71,T8,S80,S81],
    append(AllConcepts,AllPeople,Everything),

    maplist(list_pair,AllConcepts,AllTeachers,CETuples),
    maplist(list_pair,AllConcepts,AllFirstStudents,WETuples1),
    maplist(list_pair,AllConcepts,AllSecondStudents,WETuples2),
    append(WETuples1,WETuples2,WETuples),
    tuples_in(CETuples, CanExplain),
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



rounds18(CanExplain,WantsExplain,ConceptCount,[Round1,Round2,Round3]). 


getCE(CanExplain):-
    findall([C,T],(can_explain(T,C)),CanExplain).
getWE(WantsExplain):-
    findall([C,S],(wants_have_explained(S,C)),WantsExplain).
% converting rows to dicts where keys are integers and values concepts/people    
header_to_conceptdict(Header,ConceptDict):- 
    row_to_list(Header,[_|ConceptsList]),
    list_to_dict(ConceptsList,ConceptDict).

rows_to_peopledict(Rows,PeopleDict):- 
    rows_to_lists(Rows,RowsLL),
    transpose(RowsLL,[FR|_]),
    list_to_dict(FR,PeopleDict).

explain(InPath,OutPath,Out) :-
    % input -> internal representation 
    csv_read_file(InPath,[Header|InRows]),
    header_to_conceptdict(Header,ConceptDict),
    rows_to_peopledict(InRows,PeopleDict),
    % length(PeopleDict,PeopleCount), % this might be used for more people than 9
    length(ConceptDict,ConceptCount),
    rows_to_rels(PeopleDict,ConceptDict,Header,InRows,RelationsUnfiltered),
    filter_does_not_want(RelationsUnfiltered,Relations),
    maplist(assertz,Relations),
    getCE(CanExplain),
    getWE(WantsExplain),
    rounds9(CanExplain,WantsExplain,ConceptCount,Rounds),
    Out=Rounds,
    % generated solution -> output csv
    rounds_to_rows(PeopleDict,ConceptDict,Rounds,OutputRows),
    outheader(OutHeader),
    csv_write_file(OutPath, [OutHeader|OutputRows],[]).