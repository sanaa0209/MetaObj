
Predicati Dinamici:

:- dynamic class/1.
:- dynamic inst/1.
Queste direttive dinamiche dichiarano che i predicati class/1 e inst/1
possono essere modificati dinamicamente durante l'esecuzione del programma.
Questo significa che fatti riguardanti classi e istanze possono essere
aggiunti o rimossi in tempo reale.

def_class/3:
def_class(Class_Name, Parents, Parts) :-
   
Questo predicato definisce una classe con il nome Class_Name, eventuali
genitori Parents, e parti Parts (che possono contenere campi e metodi).
In particolare, il predicato verifica la correttezza delle informazioni e
chiama load_methods/2 per caricare eventuali metodi.
Attenzione!
?- def_class(person, [], [field(name, 'Eve'), field(age, 21, "integer")]).
Integer deve essere dichiarato con gli apici per garantirne il funzionamento.

def_class/2:
def_class(Class_Name, Parents) :-
    
Questa è una variante del predicato def_class/3 senza parti (Parts).
Definisce una classe solo con il nome e i genitori.

make/2:
make(Instance_name, Class_Name) :-
Crea un'istanza con il nome Instance_name della classe Class_Name.
Se l'istanza già esiste, la rimuove e ne crea una nuova.

make/3:
make(Instance_name, Class_Name, Fields) :-
Crea un'istanza con il nome Instance_name della classe Class_Name
con campi specificati in Fields.

is_class/1:
is_class(Class_Name) :-
Controlla se Class_Name è il nome di una classe.

check_class_type/2:
check_class_type(Fields, Class_Name) :-
Controlla se i tipi dei campi specificati in Fields corrispondono
ai tipi definiti nella classe Class_Name o nei suoi genitori.

check_type/2:
check_type(Fields, Class_Name) :-
Controlla i tipi dei campi in base alla classe specificata o ai suoi genitori.

check_instance/2:
check_instance(Instance_name, Class_Name) :-
Controlla se un'istanza con il nome Instance_name della classe
Class_Name esiste. Se esiste, la rimuove e ne crea una nuova.

field/3:
field(Instance, Field_Name, Result) :-
Estrae il valore del campo Field_Name da un'istanza Instance.

field_parents/3:
field_parents([Class | _], Field_Name, Result) :-
Recupera il valore del campo da una classe, esplorando anche le
classi genitore se necessario.

il predicato get_data/3 è utilizzato per recuperare il valore di un campo
(Field_Name) da un'istanza o da una classe.


atom(Field_Name): Verifica che Field_Name sia un'atomo, cioè il nome del
campo da recuperare.

get_fields(Instance, Field_Values): Estrae i valori dei campi (Field_Values)
dall'istanza Instance.

first(Field_Values, Values): Estrae la prima lista di valori dai campi.

first(Values, Term): Estrae il primo termine dalla lista di valori.

term_to_atom(Term, Atom): Converte il termine in un atomo.

atom_string(Atom, String): Converte l'atomo in una stringa.

split_string(String, '=(,' , ')' , Fields): Suddivide la stringa in una
 lista di campi, considerando i caratteri "=(," come separatori.

rimuovi_fieldClasse(Fields, FieldN): Rimuove eventuali occorrenze delle
parole "field" e "method" dalla lista di campi.

first(FieldN, Field_Name_String) e atom_string(Field_Name, Field_Name_String):
 Estrae il nome del campo da FieldN e lo converte in un atomo.

second(FieldN, Result_string) e atom_string(Result_atom, Result_string):
Estrae il valore del campo da FieldN e lo converte in un atomo.

term_to_atom(Result, Result_atom): Converte l'atomo del valore del campo
in un termine.

get_data/3 lavora sulle informazioni relative ai campi
di un'istanza o di una classe, estrae il valore del campo specificato
e lo converte in un termine.
