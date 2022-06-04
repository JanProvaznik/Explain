% #!/usr/bin/swipl 
% :-initialization(main,main).
:- use_module(library(csv)).
:- use_module(library(clpfd)).
:- use_module(library(apply)).

:- dynamic relation/3.

% Notes: generating by groups and then choosing groups to form a solution does not work
% group: group(Teacher, Students, Concept)
% 9 People ~1000 groups 18 People ~20k groups, 36 People ~400k groups, 54 People ~2M groups
% that means generating all groups seems very bad...
% trick -> divide into subgroups of 9 people and solve for them separately!

% even if we did that selecting round is still doing 1000 choose 3 which is 166 million options

% region: internal numeric representation of relations

range(Lo,Hi,Range):-findall(N,between(Lo,Hi,N),Range).
pair(A, B, A : B).
% can be used both for People and Concepts, dict has structure Number : Concept
list_to_dict(X, Dict) :-
    length(X, Len),
      range(1,Len,Y),
      maplist(pair,Y,X,Dict).


relation_to_numeric(PeopleDict,ConceptDict,relation(Person,Concept,Action),relation(PersonN,ConceptN,ActionN)):-
    member(PersonN:Person,PeopleDict),
    member(ConceptN:Concept,ConceptDict),
    numeric(Action,ActionN)
    .

numeric('N',0).
numeric('MHE',1).
numeric('HE',3).
numeric('CE',-6).

can_explain(Teacher,Concept):-relation(Teacher,Concept,-6).
really_wants_have_explained(Student,Concept):-relation(Student,Concept,3).
maybe_wants_have_explained(Student,Concept):-relation(Student,Concept,1).
wants_have_explained(Student,Concept):- really_wants_have_explained(Student,Concept) ; maybe_wants_have_explained(Student,Concept).
rev_wants_he(Concept,Student):-wants_have_explained(Student,Concept).
% endregion


%???


% group_has_two_students(group(_,Students,_)) :-
%     length(Students, 2).

% group_has_between_1_and_3_students(Group) :- 
%     Group = group(_, Students, _),
%     length(Students, N),
%     N #> 0,
%     N #< 4.

% ['name', 'Concept1', 'Concept2', 'Concept3']
% ['Jan', 'MHE', 'HE', 'CE']
% convert this to [relation("Jan","Concept1","MHE"), relation("Jan","Concept2","HE"), relation("Jan","Concept3","CE")]


% region: IO
% generate a header
outheader(row("concept","teacher","student1","student2","student3")).

not_interested(relation(_,_,X)):- 
    numeric('N',Z),
    X #= Z.
is_teacher(relation(_,_,X)):-
    numeric('CE',Z),
    X #= Z.
% putting into a functor
to_relation(Name,Concept,Action, relation(Name,Concept,Action)).

% list of rows -> list of relation functors
rowlist_to_rels([_|HeaderL],[Name|RowL],Rels):-
    maplist(to_relation(Name),HeaderL,RowL,Rels).


rows_to_rels(PeopleDict,ConceptDict,Header,Rows,Rels):-
    row_to_list(Header,HeaderL),
    rows_to_lists(Rows,RowsL),
    maplist(rowlist_to_rels(HeaderL),RowsL,RelsLL),
    flatten(RelsLL,RelsS),
    maplist(relation_to_numeric(PeopleDict,ConceptDict),RelsS,Rels)
       . 


member_of(X,Y):-member(Y,X).
list_of_numbers_to_people(_,[],[]).
list_of_numbers_to_people(PeopleDict,[N|Numbers],[P|People]):-
    member(N:P,PeopleDict),
    list_of_numbers_to_people(PeopleDict,Numbers,People).
% converts an integer group to a list of Strings
group_to_string_list(PeopleDict,ConceptDict,group(Teacher,Students,Concept),[ConceptS,TeacherS|StudentsS]):-
member(Teacher:TeacherS, PeopleDict),member(Concept:ConceptS,ConceptDict),list_of_numbers_to_people(PeopleDict,Students,StudentsS).



% rounds contain integer groups, we need to convert them to string groups
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

% split_teacher_student(Relations,TeacherRelations,StudentRelations):-
    % partition(is_teacher, Relations, TeacherRelations, StudentRelations).
% I want the student to exist in the relation with the concept

pair_compatible((A,B),(C,D)):-
    (A #= C #/\ B #= D) #\/ (A #= C #==> B #\= C).
    
compatible_with(Exs,X):-
    maplist(pair_compatible(X),Exs).

pairs_compatible(Exs):-
    maplist(compatible_with(Exs),Exs).
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
    % each round has to contain one person once
    [T0,T1,T2,T3,T4,T5,T6,T7,T8] ins 1..9,
    [C0,C1,C2,C3,C4,C5,C6,C7,C8] ins 1..ConceptCount,
    [S00,S01,S10,S11,S20,S21,S30,S31,S40,S41,S50,S51,S60,S61,S70,S71,S80,S81] ins 1..9,
    T0#<T1,T1#<T2,%T0#<T2,
    T3#<T4,T4#<T5,%T3#<T5,
    T6#<T7,T7#<T8,%T6#<T8,

    S00#<S01,S10#<S11,S20#<S21,
    S30#<S31,S40#<S41,S50#<S51,
    S60#<S61,S70#<S71,S80#<S81,


    % all_distinct([T0,T1,T2]),               
    % all_distinct([T3,T4,T5]),
    % all_distinct([T6,T7,T8]),
    
    all_distinct([S00,S01,S10,S11,S20,S21]),
    all_distinct([S30,S31,S40,S41,S50,S51]),
    all_distinct([S60,S61,S70,S71,S80,S81]),

    all_distinct([T0,S10,S11,S20,S21]),
    all_distinct([T1,S00,S01,S20,S21]),
    all_distinct([T2,S00,S01,S10,S11]),

    all_distinct([T3,S40,S41,S50,S51]),
    all_distinct([T4,S30,S31,S50,S51]),
    all_distinct([T5,S30,S31,S40,S41]),

    all_distinct([T6,S70,S71,S80,S81]),
    all_distinct([T7,S60,S61,S80,S81]),
    all_distinct([T8,S60,S61,S70,S71])
 ,
    % if a person is explained a concept C, then they should not be explained the same concept C again

    Exs= [(S00,C0),(S01,C0),(S10,C1),(S11,C1),(S20,C2),(S21,C2),(S30,C3),(S31,C3),(S40,C4),(S41,C4),(S50,C5),(S51,C5),(S60,C6),(S61,C6),(S70,C7),(S71,C7),(S80,C8),(S81,C8)],
    pairs_compatible(Exs),
    
    
   % noone should explain two times in a row and ideally noone should explain twice
    (all_distinct([T0,T1,T2,T3,T4,T5,T6,T7,T8]); 
    all_distinct([T0,T1,T2,T3,T4,T5,T6,T7]);
    all_distinct([T0,T1,T2,T3,T4,T5,T6]);
    all_distinct([T0,T1,T2,T3,T4,T5]),
    all_distinct([T0,T1,T2,T3,T4]),
    all_distinct([T0,T1,T2,T3])
    ),

    maplist(can_explain, [T0,T1,T2,T3,T4,T5,T6,T7,T8],[C0,C1,C2,C3,C4,C5,C6,C7,C8])           ,
    maplist(wants_have_explained, [S00,S10,S20,S30,S40,S50,S60,S70,S80],[C0,C1,C2,C3,C4,C5,C6,C7,C8]),
    maplist(wants_have_explained, [S01,S11,S21,S31,S41,S51,S61,S71,S81],[C0,C1,C2,C3,C4,C5,C6,C7,C8]),

    labeling([ff],[T0,S00,S01,C0,T1,S10,S11,C1,T2,S20,S21,C2,T3,S30,S31,C3,T4,S40,S41,C4,T5,S50,S51,C5,T6,S60,S61,C6,T7,S70,S71,C7,T8,S80,S81,C8])



.
    
header2conceptdict(Header,ConceptDict):- 
    row_to_list(Header,[_|ConceptsList]),
    list_to_dict(ConceptsList,ConceptDict).

rows2peopledict(Rows,PeopleDict):- 
    rows_to_lists(Rows,RowsLL),
    transpose(RowsLL,[FR|_]),
    list_to_dict(FR,PeopleDict).

explain(InPath,OutPath,OutRows) :-
    % input -> internal representation done 
     csv_read_file(InPath,[Header|InRows]),
     header2conceptdict(Header,ConceptDict),
     rows2peopledict(InRows,PeopleDict),
    %  length(PeopleDict,PeopleCount),
     length(ConceptDict,ConceptCount),
     rows_to_rels(PeopleDict,ConceptDict,Header,InRows,RelationsUnfiltered),
     filter_does_not_want(RelationsUnfiltered,Relations),
     maplist(assertz,Relations),
     rounds9(ConceptCount,Rounds),

    % generated solution -> output csv
    rounds_to_rows(PeopleDict,ConceptDict,Rounds,OutRows),
     outheader(OutHeader),
     csv_write_file(OutPath, [OutHeader|OutRows],[]).
     
