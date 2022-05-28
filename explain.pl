% #!/usr/bin/swipl 
% :-initialization(main,main).
:- use_module(library(csv)).
:- use_module(library(clpfd)).
:- use_module(library(apply)).
% :- set_prolog_flag(stack_limit, 4_147_483_648).
% :- set_prolog_flag(double_quotes, atom).
% group: group(Teacher, Students, Concept)
% round(Groups) groups is a list of groups
% todo use arguments to get the file
main:-write('hi').

relation2numeric(relation(A,B,C),relation(A,B,X)):-
    numeric(C,X).

numeric('N',0).
numeric('MHE',1).
numeric('HE',2).
numeric('CE',-6).

group_has_two_students(group(_,Students,_)) :-
    length(Students, 2).

group_has_between_1_and_3_students(Group) :- 
    Group = group(_, Students, _),
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

% ['name', 'Concept1', 'Concept2', 'Concept3']
% ['Jan', 'MHE', 'HE', 'CE']
% convert this to [relation("Jan","Concept1","MHE"), relation("Jan","Concept2","HE"), relation("Jan","Concept3","CE")]

pair(A, B, A : B).
to_relation(Name,Concept,Action, relation(Name,Concept,Action)).

rowlist_to_rels([_|HeaderL],[Name|RowL],Rels):-
    maplist(to_relation(Name),HeaderL,RowL,Rels).

not_interested(relation(_,_,X)):- % todo maybe better for numeric if not converted here
    numeric(X,Y),
    numeric('N',Z),
    Y #= Z.

filter_does_not_want(Relations,FilteredRelations):-
    exclude(not_interested,Relations,FilteredRelations).

is_teacher(relation(_,_,X)):-
    numeric(X,Y),
    numeric('CE',Z),
    Y #= Z.

split_teacher_student(Relations,TeacherRelations,StudentRelations):-
    partition(is_teacher, Relations, TeacherRelations, StudentRelations).
% I want the student to exist in the relation with the concept
myweirdmember(Relations,Concept,Student):-
    member(relation(Student,Concept,_),Relations).

string_to_numberic_hash(String,Number):-
    atom_codes(String,Codes),
    sum(Codes, #=, Number).
    

% creates all possible groups (might be a lot)
create_groups(TeacherRelations,StudentRelations,Groups):-
    findall(group(Teacher,Students,Concept),
    (
        (Students = [_,_];Students = [_,_,_];Students = [_]),
        member(relation(Teacher,Concept,_),TeacherRelations), 
        maplist(myweirdmember(StudentRelations,Concept),Students),
        maplist(string_to_numberic_hash,[Teacher|Students],Numbers), % using clpfd for better performance
        all_different(Numbers)
    ),Groups).


% first param is header
rows2rels(_,[],[]).
rows2rels(Header,Rows,Rels):-
    row_to_list(Header,HeaderL),
    rows_to_lists(Rows,RowsL),
    maplist(rowlist_to_rels(HeaderL),RowsL,RelsLL),
    flatten(RelsLL,Rels).
    

% rels2groups(Rels,Groups).
outheader(row("concept","teacher","student1","student2","student3")).


% todo this is not done
rels2rounds(Rels,Rounds):- 
    Rels = [Fst|Rest], Rest = [Snd|RestRest],
    Rounds=[[Fst],[Snd],RestRest].


% todo this is not done
rounds2rows(Rounds,Rows):-
    maplist(list_to_row,Rounds,Rows).

rows_to_lists(Rows, Lists):-
  maplist(row_to_list, Rows, Lists).

row_to_list(Row, List):-
  Row =.. [row|List].
list_to_row(List,Row):-row_to_list(Row,List).


groups2rounds(Groups,Rounds):-
    Rounds = [Round1,Round2,Round3].
    



explain(InPath,OutPath,OutRows) :-
     csv_read_file(InPath,[Header|InRows]),
     rows2rels(Header,InRows,RowsRel),
     filter_does_not_want(RowsRel,FilteredRowsRel),
        split_teacher_student(FilteredRowsRel,TeacherRels,StudentRels),
        create_groups(TeacherRels,StudentRels,Groups),
        OutRows = Groups.
    %  rels2rounds(TeacherRels,Rounds),
    %  rounds2rows(Rounds,OutRows).
    %  outheader(OutHeader).
    %  csv_write_file(OutPath, [OutHeader|OutRows])
     
