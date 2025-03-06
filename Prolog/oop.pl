%%%% -*- Mode: Prolog -*-

%%%%  oop.pl

%%% Msellek Sanaa 902325
%%% Nessuna collaborazione

% Predicati dinamici
:- dynamic class/1.
:- dynamic inst/1.

%%% PRIMITIVE RICHIESTE

%%% def_class/3:  un predicato di arietà 3 che definisce la 
%%% classe e carica i metodi
def_class(Class_Name, Parents, Parts) :-
    atom(Class_Name),
    is_list(Parents),
    is_list(Parts),
    control_class3(Class_Name);
    maplist(is_class, Parents),
    dividi_elementi(Parts, _, Methods),
    assert(class([Class_Name, Parents, Parts])),
    load_methods(Methods, Class_Name),
    write("Classe creata con successo"), nl,!.

%%% def_class/2: pedicato di arietà 2 senza Parts
def_class(Class_Name, Parents) :-
    atom(Class_Name),
    is_list(Parents),
    control_class2(Class_Name);
    maplist(is_class, Parents),
    assert(class([Class_Name, Parents])),!.

%%% make/2: Instance_name associato al termine che rappresenta
%%%la istanza
make(Instance_name, Class_Name) :-
    atom(Instance_name),
    atom(Class_Name),
    is_class(Class_Name),
    assert(inst([Instance_name, Class_Name])), !.

%%% make/2: Instance_name è una variabile, viene unificata con
%%% il termine
make(Instance_name, Class_Name) :-
    var(Instance_name),
    atom(Class_Name),
    is_class(Class_Name),
    check_instance(Instance_name, Class_Name);
    assert(inst([Instance_name, Class_Name])),
    Instance_name = inst([Instance_name, Class_Name]), !.

%%% make/2: Instance_name è un termine che unifica con la
%%%istanza creata
make(Instance_name, Class_Name) :-
    atom(Class_Name),
    is_class(Class_Name),
    check_instance(Instance_name, Class_Name);
    Instance_name =.. Instance,
    second(Instance, X),
    is_instance(X), !.

%%% make/3: Instance_name associato al termine che rappresenta
%%%la istanza
make(Instance_name, Class_Name, Fields) :-
    atom(Instance_name),
    atom(Class_Name),
    is_list(Fields),
    is_class(Class_Name),
    get_field_values(Fields),
    check_type(Fields, Class_Name),
    assert(inst([Instance_name, Class_Name, Fields])), !.

%%% make/3: Instance_name è una variabile, viene unificata con
%%%il termine
make(Instance_name, Class_Name, Fields) :-
    var(Instance_name),
    atom(Class_Name),
    is_list(Fields),
    is_class(Class_Name),
    get_field_values(Fields),
    check_instance(Instance_name, Class_Name);
    check_type(Fields, Class_Name),
    assert(inst([Instance_name, Class_Name, Fields])),
    Instance_name = inst([Instance_name, Class_Name, Fields]), !.

%%% make/3: Instance_name è un termine che unifica con
%%%la istanza creata
make(Instance_name, Class_Name, Fields) :-
    atom(Class_Name),
    is_list(Fields),
    is_class(Class_Name),
    check_instance(Instance_name, Class_Name);
    check_type(Fields, Class_Name),
    Instance_name =.. Instance,
    second(Instance, X),
    is_instance(X), !.

%%%alternativa con caricamento dei metodi
make(Instance_name, Class_Name, Fields) :-
    var(Instance_name),
    atom(Class_Name),
    is_list(Fields),
    is_class(Class_Name),
    get_method(Fields, _, _),
    check_instance(Instance_name, Class_Name);
    check_type(Fields, Class_Name),
    get_field_values(Fields),
    assert(inst([Instance_name, Class_Name, Fields])),
    Instance_name = inst([Instance_name, Class_Name, Fields]),
    bagof(Method, get_method(Fields, _, Method), Methods),
    load_methods(Methods, Instance_name), !.

%%% LOGICA DI CONTROLLO
%%% is_class/1: controlla se Class_Name è il nome di una classe
is_class(Class_Name) :-
    atom(Class_Name),
    current_predicate(class/1), !,
    class([Class_Name, _, _]).

%%% check_class/1: prima di creare una nuova classe (con Parts),
%%% controlla se esiste gia. Se esiste, allora viene eliminata e
%%% viene creata una nuova classe
control_class3(Class_Name):-
    is_class(Class_Name),
    class([Class_Name, L, X]),
    retract(class([Class_Name, L, X])),
    retract(class([Class_Name, L, []])),
    retract(class([Class_Name, [], []])).

%%% check_class/1: prima di creare una nuova classe (senza Parts)
%%% controlla se esiste gia. Se esiste, allora viene eliminata e
%%% viene creata una nuova classe
control_class2(Class_Name):-
    is_class(Class_Name),
    class([Class_Name, L]),
    retract(class([Class_Name, L])),
    retract(class([Class_Name, []])).

%%% check_istance/2: prima di creare una nuova istanza,
%%% controllo se esiste gia. Se esiste, allora viene
%%% eliminata e viene creata una nuova istanza
check_instance(Instance_name, Class_Name):-
    is_instance(Instance_name),
    inst(Instance_name, L),
    retract(inst(L)),
    retract(inst([Instance_name, Class_Name, []])),
    retract(inst([Instance_name, Class_Name])).

%%% check_type/2: controlla se i tipi dei campi sono corretti.
%%% Utilizzo anche dei predicati di appoggio
check_type(Fields, Class_Name):-
    check_class_type(Fields, Class_Name);
    get_parents(Class_Name, Parents),
    check_parents_type(Fields, Parents).

% check_parents_type/2: predicato ricorsivo per controllare i tipi
% dei campi dei genitori di una classe
check_parents_type(Fields, [C | _]):-
    check_class_type(Fields, C).
check_parents_type(Fields, [_ | Cs]):-
    check_parents_type(Fields, Cs).

check_class_type([], _).
check_class_type([X|Xs], Class) :-
    is_type_equal(X, Class),
    check_class_type(Xs, Class).

% get_field_type/3: predicato ricorsivo per ottenere il tipo
% di un campo in una lista di campi
get_field_type([X | _], Field_nameCheck, DefaultValueField) :-
    check_field_name_inst(X, Field_nameCheck, DefaultValueField),!.

get_field_type([_ | Xs], Field_nameCheck, DefaultValueField) :-
    get_field_type(Xs, Field_nameCheck, DefaultValueField).

% is_type_equal/2: controlla se il tipo del campo input è uguale
% al tipo della definizione della classe
is_type_equal(X, Class):-
    extract_value(X, Typeis),
    check_field_type_class(Class, X, Typecl),
    Typecl == Typeis.

% assign_type/2: assegna il "tipo" di un campo
assign_type(N, 'atom'):-
    not(number_string(_, N)).

assign_type(N, 'integer'):-
    number_string(Number, N),
    is_of_type(integer, Number), !.

assign_type(N, 'float'):-
    number_string(Number, N),
    is_of_type(float, Number), !.

% check_field_type_class/3: ottengo il tipo del campo della classe
check_field_type_class(Class, X, DefaultValueField) :-
    get_class(Class, ClassInfo),
    get_fields(ClassInfo, Fields),
    first(Fields, Fields_Values),
    extract_name(X, Field_NameCheck),
    get_field_type(Fields_Values, Field_NameCheck, Res),
    replace_word(Res, "\"", "", DefaultValueField).

% extract_name/2: estrae il nome del campo
extract_name(String, Name) :-
    term_to_atom(String, Atom),
    split_string(Atom, "=", "", L),
    first(L, Name).

% extract_value/2: estrae il valore del campo
% ed assegno il "tipo" associato
extract_value(String, Typeis) :-
    term_to_atom(String, Atom),
    split_string(Atom, "=", "", L),
    second(L, V),
    assign_type(V, Typeis).

% check_field_type_inst/3: dato un field della classe definita,
% controllo se i nomi dei campi sono uguali
check_field_name_inst(X, Field_nameCheck, DefaultValueField):-
    term_to_atom(X, Atom),
    % field  sono 6 caratteri, quindi prendo da 7 in poi
    sub_atom(Atom, 6, _, 0, Result),
    sub_atom(Result, 0, _, 1, Result2),
    split_string(Result2, ",", "", SC1),
    first(SC1, Field_value),
    Field_value == Field_nameCheck,
    get_third_element(SC1, DefaultValueField).

% get_third_element/2: recupera il terzo get_field_type di una lista
% se la lista ha tre elementi, altrimenti il tipo predefinito è T
get_third_element(List, ThirdElement) :-
    length(List, Len),
    (Len = 3 ->
         nth0(2, List, ThirdElement);
     ThirdElement = 'atom'
    ).

%%% inst/2: recupera una istanza dato il nome con cui è stata creata
inst(Instance_name, [Instance_name | Instance]) :-
    atom(Instance_name),
    current_predicate(inst/1), !,
    inst([Instance_name | Instance]), !.

%%% get_field_values/2: ricava i nomi dei campi
get_field_values([]).
get_field_values([Name=_ | Rest]) :-
    atom(Name),
    get_field_values(Rest).

%%% get_class/2: dato il nome di una classe, ritorna la vera e
%%% propria classe
get_class(Class_Name, [Class_Name | Rest]) :-
    current_predicate(class/1),
    class([Class_Name | Rest]), !.

%%% is_instance/1: controlla se la instanza passata con il nome è una
%%% instanza
is_instance(Value) :-
    inst(Value, Instance),
    current_predicate(inst/1),
    inst(Instance), !.

%%% is_instance/1: accetta Value come termine e controlla
%%% se è una instanza
is_instance(Value) :-
    current_predicate(inst/1),
    inst(Value), !.

%%% is_instance/2:2 ha successo se value è una istanza di una classe
%% che ha Class_name come superclasse.
is_instance(Value, Class_Name) :-
    atom(Class_Name),
    inst(Value, Instance),
    first(Instance, Instance_name),
    get_fields(Instance, Field_Values),
    inst([Instance_name, Class | Field_Values]),
    get_parents(Class, Classes),
    member(Class_Name, Classes), !.

%%% is_instance/2: ha successo se lo oggetto passatogli è
%%% la istanza di una classe.
is_instance(Value, Class_Name) :-
    atom(Class_Name),
    first(Value, Instance_name),
    get_fields(Value, Field_Values),
    inst([Instance_name, Class | Field_Values]),
    get_parents(Class, Classes),
    member(Class_Name, Classes), !.

%%% dividi_elementi/2: estrae da Parts i methods presenti
dividi_elementi([field(F, V) | R], [field(F, V) | Fs], M) :-
    dividi_elementi(R, Fs, M).
dividi_elementi([field(F, Type, V) | R], [field(F, Type, V) | Fs], M) :-
    dividi_elementi(R, Fs, M).
dividi_elementi([method(Method, Args, Body) | R], Fs,
		[method(Method, Args, Body) | M]) :-
    dividi_elementi(R, Fs, M).
dividi_elementi([], [], []).

%%% get_method/2: estrae i methods singolarmente dalla lista
get_method([method(Method, Args, Body) | Rest], Fields,
	   [method(Method, Args, Body) | Methods]) :-
    get_method(Rest, Fields, Methods).
% Caso base: la lista è vuota
get_method([], [], []).

%%% field/3: estrae da un instanza con campo il suo valore (result)
field(Instance, Field_Name, Result) :-
    atom(Field_Name),
    current_predicate(inst/1),
    is_instance(Instance),
    get_data(Instance, Field_Name, Result),!.

%%% field/3: variante che permette di ereditare
%%% da eventuali classi parents
field(Instance, Field_Name, Result) :-
    atom(Field_Name),
    current_predicate(inst/1),
    is_instance(Instance),
    inst([Instance, Class_Name | _]),
    field_parents([Class_Name], Field_Name, Result),  !.

%%% field/3: variante che permette di funzionare anche con classi
field(Instance, Field_Name, Result) :-
    atom(Field_Name),
    current_predicate(class/1),
    is_class(Instance),
    class([Instance, _, _]),
    field_parents([Instance], Field_Name, Result),!.

%%% field_parents/3: recupera il valore del field dalla
%%% class che lo ha disponibile
field_parents([Class | _], Field_Name, Result) :-
    class([Class, _, _]),
    get_data(Class, Field_Name, Result).

%%% Caso ricorsivo
field_parents([Class | Parents], Field_Name, Result) :-
    class([Class, App_parents, _]),
    append(App_parents, Parents, Parents_list),
    field_parents(Parents_list, Field_Name, Result).

%%% get_data/3: recupera data un istanza il valore di Field_Name
get_data(Instance, Field_Name, Result) :-
    inst(Instance, I),
    get_data(I, Field_Name, Result), !.

%%% get_data/3: permette la utilizzo di get_data anche su classi
get_data(Instance, Field_Name, Result) :-
    get_class(Instance, Class),
    get_data(Class, Field_Name, Result), !.

%%% get_data/3: predicato ricorsivo che recupera il valore di
%%%Field_Name
get_data(Instance, Field_Name, Result) :-
    atom(Field_Name),
    get_fields(Instance, Field_Values),
    first(Field_Values, Values),
    first(Values, Term),
    term_to_atom(Term, Atom),
    atom_string(Atom, String),
    split_string(String, '=(,' , ')' , Fields),
    rimuovi_fieldClasse(Fields, FieldN),
    first(FieldN, Field_Name_String),
    atom_string(Field_Name, Field_Name_String),
    second(FieldN, Result_string),
    atom_string(Result_atom, Result_string),
    term_to_atom(Result, Result_atom),!.

%%% Caso ricorsivo
get_data([_, _, [_ | Fields]], Field_Name, Result) :-
    get_data([_, _, Fields], Field_Name, Result).

%%% rimuovi_fieldClassel/2: rimuove i termini field e method
%%%dalla lista
rimuovi_fieldClasse(["field"|Resto], Resto) :- !.
rimuovi_fieldClasse(Lista, Lista).
rimuovi_fieldClasse(["method"|Resto], Resto) :- !.

%%% fieldx/3: estrae il valore da una classe percorrendo
%%% una catena di attributi
fieldx(Instance, [Field], Result) :-
    atom(Field),
    field(Instance, Field, Result), !.

%%% Chiamata ricorsiva
fieldx(Instance, [H | Rest], Result) :-
    field(Instance, H, Out),
    downcase_atom(Out, Out1),
    term_string(Res, Out1),
    fieldx(Res, Rest, Result), !.

%%% make_method/2: si occupa di creare e caricare il metodo
make_method(method(Method_Name, Args, Method_body), Class_Name) :-
    append([this], Args, Method_Args),
    append([Method_Name], Method_Args, Rest),
    Term =.. Rest,
    term_to_atom(Method_Name, Name),
    term_to_atom(Term, MethodHead),
    term_to_atom(Method_body, BodyNoCheck),
    term_to_atom(Class_Name, Class),
    atom_concat('field(this, ', Name, NameChecked),
    atom_concat(NameChecked, ', SC1),', This_field),
    atom_concat(This_field, 'field(', Field1_append),
    atom_concat(Field1_append, Class, Fiel2_append),
    atom_concat(Fiel2_append, ', ', Field3_append),
    atom_concat(Field3_append, Name, Field4_append),
    atom_concat(Field4_append, ', SC2),', Class_field),
    % per controllare che le classi siano uguali
    atom_concat(Class_field, 'SC1 = SC2', Checker),
    atom_concat(Checker, ',', To_append),
    atom_concat(To_append, BodyNoCheck, Body),
    atom_concat(MethodHead, ' :- ', MethodNoBody),
    atom_concat(MethodNoBody, Body, MethodNoThisNoEnd),
    atom_concat(MethodNoThisNoEnd, ', !.', MethodNoThis),
    atom_string(MethodNoThis, MethodNoThisString),
    replace_word(MethodNoThisString, "this", "This", Method_Atom),
    term_to_atom(Method, Method_Atom),
    % controllo se il metodo esiste gia. Se esiste gia, lo sostituisco
    % check_method(Method_Name, Method_Args);
    assert(Method),
    write("Metodo creato con successo : "), write(MethodHead), nl.

%%% load_methods/2: chiama make_method per ogni metodo
%%% presente nella lista
load_methods([Method | Rest], Class_Name) :-
    make_method(Method, Class_Name),
    load_methods(Rest, Class_Name).

%%% Caso base
load_methods([], _).

%%% get_parents/2: data una classe restituisce i suoi parents
get_parents(Class, Result) :-
    bagof(X, parent([Class], X), Bag),
    remove_duplicates(Bag, R),
    remove_first(R, Result).

%%% parent/2: data una classe (lista), ritorna i genitori
parent([Class | _], Result) :-
    class([Class, _, _]),
    Result = Class.

%%% Chiamata ricorsiva
parent([Class | Parents], Result) :-
    class([Class, App_parents, _]),
    append(App_parents, Parents, Parents_list),
    parent(Parents_list, Result).

%%% replace_word/3: sostituisce una parola con una altra
replace_word(Parola, Vecchia, Nuova, X) :-
    replace_n_word(Parola, 1, Vecchia, Nuova, Result),
    replace_word(Result, Vecchia, Nuova, X), !.
replace_word(Parola, _, _, Parola).

% Funzione di appoggio per replace_word.
replace_n_word(Parola, Ennesima, Vecchia, Nuova, Result) :-
    call_nth(sub_atom(Parola, Before, _Len, After, Vecchia), Ennesima),
    sub_atom(Parola, 0, Before, _, Left),
    sub_atom(Parola, _, After, 0, Right),
    atomic_list_concat([Left, Nuova, Right], Result).

%%% remove_duplicates/2: rimuove i duplicati da una lista
remove_duplicates([], []).
remove_duplicates([H | T], Result) :-
    member(H, T), !,
    remove_duplicates(T, Result).
remove_duplicates([H | T], [H | Result]) :-
    remove_duplicates(T, Result).

%%% predicati vari per la gestione delle liste
first([H | _], H).
second([_, H | _], H).
tail([_ | T], T).
get_fields([_, _ | T], T).
remove_first([_ | T], T).

%%%% end of file -- oop.pl

