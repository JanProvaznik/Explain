% #!/usr/bin/swipl 
% :-initialization(main,main).
:- use_module(library(csv)).
:- use_module(library(clpfd)).

% group: group(Teacher, Students, Concept)
% round(Groups) groups is a list of groups
% todo use arguments to get the file
main:-write("hi").

numeric("N",0).
numeric("MHE",1).
numeric("HE",2).
numeric("CE",3).

group_has_two_students(Group) :-
    Group = group(Teacher, Students,_),
    length(Students, 2).

group_has_between_1_and_3_students(Group) :- 
    Group = group(Teacher, Students, _),
    length(Students, N),
    N #> 0,
    N #< 4.
everyone_once(Groups,People) :-
     % every person from People is in exactly one group
    append(Groups,GroupsPeople), equal(GroupsPeople,People) , !.


isSubset([],_).
isSubset([H|T],Y):-
    member(H,Y),
    select(H,Y,Z),
    isSubset(T,Z).
equal(X,Y):-
    isSubset(X,Y),
    isSubset(Y,X).

% ["name", "Concept1", "Concept2", "Concept3"]
% ["Jan", "MHE", "HE", "CE"]
% convert this to [relation("Jan","Concept1","MHE"), relation("Jan","Concept2","HE"), relation("Jan","Concept3","CE")]

pair(A, B, A : B).
to_relation(Name,Concept,Action, relation(Name,Concept,Action)).

rowlist_to_rels([_|HeaderL],[Name|RowL],Rels):-
    maplist(to_relation(Name),HeaderL,RowL,Rels)
    % append(Name,Concept,Action)
    
    .

rows2rels(_,[],[]).
rows2rels(Header,[],[]).
rows2rels(Header,Rows,Rels):-
    row_to_list(Header,HeaderL),
    rows_to_lists(Rows,RowsL),
    maplist(rowlist_to_rels(HeaderL),RowsL,RelsLL),
    flatten(RelsLL,Rels).
    

rels2groups(Rels,Groups).
outheader(row("concept","teacher","student1","student2","student3")).

rels2rounds(_,_).
rounds2rows(_,[row("ahoj","hey")]).

rows_to_lists(Rows, Lists):-
  maplist(row_to_list, Rows, Lists).

row_to_list(Row, List):-
  Row =.. [row|List].

explain(InPath,OutPath) :-
     csv_read_file(InPath,[Header|InRows]),
     rows2rels(Header,InRows,RowsRel),
     rels2rounds(RowsRel,Rounds),
     rounds2rows(Rounds,OutRows),
     outheader(OutHeader),
     csv_write_file(OutPath, [OutHeader|OutRows])
     .