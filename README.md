# RegexEngine
Regex engine in python

Parsare: am folosit ANTLR pentru parsare

Gramatica pleaca de la definitia recursiva a expresiilor regulate. Cu toate acestea,
in ANTLR reguli de forma regex: alternation ... si alternation : regex regex genereaza
erori "rules are mutually left recursive". Pentru rezolvare am folosit strategia de la
curs: definim reguli noi, astfel ajungand la o "ierarhie" de regexuri.

Exemplu:
regex : alternation | other_regex;
alternation : other_regex ALTERNATION regex;

other_regex a fost introdus pentru a rezolva eroarea de mai sus.

Pe scurt: un regex poate sa fie un "sau" sau un other_regex.
other_regex: concatenare sau un other_regex_2.
other_regex_2: aici includem operatorii de "baza": kleene star, plus, ?, nr de aparitii 
dat printr-un interval/maxim/minim. Pe langa acestea poate sa fie un other_regex_3.
other_regex_3: (expr), ., un grup [], cifra sau litera din alfabet.

Am incercat cumva sa plecam de la o vedere high level a unui regex(combinare de alte regexuri)
si sa ajungem in final la elementele de baza(cele mai simple expresii regulate). 
De aceea grupurile sau . se afla in other_regex_3 si nu in 2: genereaza tot simboluri(. orice
simbol, grupul genereaza un nr finit de simboluri).
other_regex_2 defineste operatorii care primesc o expresie regulata si determina de cate ori
apare.

Pentru parsare nu am folosit un visitor sau un listener in mod explicit. Am gasit
explicatiile unui developer legate de ceea ce face antlr in spate(pentru un nod
de un anumit tip genereaza clase specifice deci pentru a verifica tipul unui
nod este suficient sa verificam tipul instantei).
Motivul pentru asta este ca am vrut sa construiesc obiectele de tip RegularExpression
intr-un mod recursiv - asemanator cu tipurile de date din haskell. Nu
am reusit sa gasesc o metoda usoara sa fac asta folosind structura default
a vizitatorului(ar fi prins bine un laborator de antlr...).

Functia evalParseTree primeste rootul si incepe parcurgerea. Daca
nodul curent este de tipul alternation, atunci evaluam nodul stanga
si nodul dreapta si construim o expresie regulata de tip ALTERNATION 
avand ca lhs si rhs expresiile rezultate din stanga si dreapta.

Normalizare la expresii regulate: realizata de regexToRegularExpression.
Conversia pentru cazurile alternation, concatenation, symbol, star
este evidenta: doar transformam obiectul din RegEx in RegularExpression.
Pentru celelalte:
e+ -> ee* (construim concatenare intre e si expresia e*)
e? -> (epsilon | e)
.  -> generam expresia a | b | ... | 1 | 2 | ... | A | B ... | Z
(sau intre toate simbolurile din alfabet)
e{x} -> eeee...e de x ori(concatenare e cu e de x ori)
e{1, 3} -> e | ee | eee(sau intre concatenari)
e{,3} -> epsilon | e | ee | eee (toate posibilitatile in care expresia apare de maxim 3 ori)
e{3,} -> eeee*(apare de 3 ori si apoi adaugam e* - 0 sau mai multe ori)
Pentru grupuri ori adaugam fiecare element individual(simbol) daca nu exista -, ori
prima data generam toate simbolurie din intervalul alfa beta si apoi le adaugam.

Transformare expresie regulata -> automat finit nedeterminist:
Am folosit functiile ajutatoare de laborator(de exemplu functia de 
redenumire stari sau functia de generare de noi nume de stari):

Algoritmul folosit este algoritmul de la curs si laborator.

Transformare nfa -> dfa:

Pornim cu o functie care determina inchiderile epsilon pentru fiecare stare:
Plecam din fiecare stare a automatului nedeterminist si vedem in cate
stari putem ajunge mergand doar pe tranzitii epsilon. Fiecare
stare este adaugata in inchiderea epsilon a starii curente.
Ne asiguram sa nu avem cicluri pentru a nu adauga la infinit
stari: daca am adaugat deja o stare in inchidere, nu o vizitam
iar si nici nu vedem unde putem ajunge din ea(am verificat deja).

Inchiderile epsilon, de exemplu stari de forma {1,2,3} vor fi
stari din automat, doar ca implementarea automatului accepta nume
de stari numere intregi. Vom transforma o multime intr-un sir:
{1,2,3} -> 1#2#3 si vom mentine o mapare de la fiecare sir de aceasta
forma la un numar natural.

De asemenea marcam ca stare initiala inchiderea epsilon ce contine
o stare initiala din nfa.

Ideea pentru a face conversia este sa folosim un algoritm foarte aproapiat de "BFS".
Pastram o lista de stari pe care trebuie sa le mai vizitam si o lista de stari pe care
le-am vizitat deja.

Initial doar starea initiala se afla in lista de stari pe care trebuie sa le vizitam(
numita stringState).
Algoritmul determina vecinii starii curente(in ce stari putem ajunge din aceasta 
stare), apoi reuneste inchiderile epsilon ale tuturor starilor accesibile si
genereaza o noua stare.
Este foarte probabil sa obtinem stari noi in proces(care nu apartin inchiderilor
epsilon generate anterior). Daca acest lucru se intampla, pur si simplu
adaugam o intrare in map si asociem si acestei noi stari un numar.

Aceasta metoda, chiar daca pare mai complicata ne permite sa generam doar starile
necesare(in care putem ajunge), nu mai e nevoie sa generam 2^K stari si sa le retinem degeaba.

Rularea unui cuvant pe un DFA: algoritmul este extrem de simplu.
Luam primul caracter din cuvant si verificam daca exista tranzitie din starea initiala.
Daca da, continuam sa luam caractere din cuvant si sa vedem unde putem ajunge din starea
curenta. Algoritmul se termina fie cand nu mai putem trece intr-o alta stare(nu accepta),
fie cand cuvantul nu mai are simboluri. In acest caz se pot intampla 2 lucruri:
-am ramas fara simboluri intr-o stare finala -> acceptam
-nu suntem intr-o stare finala -> nu acceptam
