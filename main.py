#!/usr/bin/env python
import sys
import pickle
import string
from antlr4 import *
from RegexGrammarLexer import *
from RegexGrammarParser import *

from antlr4 import *
from antlr4.tree.Trees import Trees
from nfa import *
from dfa import *

#constantele din ambele fisiere au acelasi nume
import regex as RegexConst
import regular_expression as RegularExpressionConst

from regex import RegEx
from regular_expression import RegularExpression

from itertools import product

"""
Primeste continutul unui nod de tip grup("lista") din arborele de parsare si intoarce un set ce contine elementele grupului
Un element din grup poate sa fie o cifra/litera sau un interval de forma alfa-beta unde alfa-beta sunt simboluri din alfabet
Grupul este construit ca o lista: Cons(1, Const(2, ...)) sau altfel spus un grup de forma "123" este elementul 1 adaugat la grupul "23"
Grupul 23 este format din elementul 2 adaugat la grupul 3, care este format din elementul 3.
"""
def buildSetFromGroup(tree):
    result = set()

    for i in range(tree.getChildCount()): #parcurge continutul grupului - fiecare copil din arbore

        #copilul este un alt grup, il parcurgem recursiv si concatenam setul rezultat la setul initial
        if isinstance(tree.getChild(i), RegexGrammarParser.Group_contentsContext):
            tempSet = buildSetFromGroup(tree.getChild(i))
            result = result | tempSet

        #copilul este un element dintr-un grup
        if isinstance(tree.getChild(i), RegexGrammarParser.Set_itemContext):
            childName = tree.getChild(i).getText()

            if not ('-' in childName):
                result.add(childName) #nu e interval, adaugam simbolul direct
            else:
                list = childName.split('-')
                result.add((list[0], list[1])) #adaugam un tuplu format din capetele intervalului

    return result

"""
Evalueaza arborele generat de ANTLR si construieste un regex in functie de nodurile acestuia
"""
def evalParseTree(tree):

    reg = RegEx(RegexConst.EMPTY_STRING)

    if isinstance(tree, RegexGrammarParser.RegexContext):
        #nod de tip "regex" - radacina ierarhiei de regexuri, trebuie sa il exploram
        return evalParseTree(tree.getChild(0))

    if isinstance(tree, RegexGrammarParser.AlternationContext):
        #nodul este un sau. Calculam recursiv expresiile regulate reprezentate de cei 2 copii(lhs si rhs din |)
        #si le combinam intr-o noua expresie regulata
        reg = RegEx(RegexConst.ALTERNATION, evalParseTree(tree.getChild(0)), evalParseTree(tree.getChild(2)))

    if isinstance(tree, RegexGrammarParser.Other_regexContext):
        if tree.concatenation() != None:
            #nodul este de tip concatenare, calculam recursiv lhs si rhs si construim un nou regex
            reg = RegEx(RegexConst.CONCATENATION, evalParseTree(tree.concatenation().getChild(0)),  evalParseTree(tree.concatenation().getChild(1)))
        else:
            #este un nod complex - un other_regex_2 care trebuie explorat, nu putem sa construim nimic in acest moment
            return evalParseTree(tree.getChild(0))

    if isinstance(tree, RegexGrammarParser.Other_regex_2Context):
        if tree.KLEENE_STAR() != None:
            #nodul este de tip *, calculam lhs si construim lhs*
            reg = RegEx(RegexConst.STAR, evalParseTree(tree.getChild(0)))
        else:
            if tree.app() != None:
                #nodul este de tip aparitie
                if tree.app().range_app() != None:
                    #expresia trebuie sa aiba intre a si b aparitii
                    a = int(tree.app().range_app().getChild(1).getText())
                    b = int(tree.app().range_app().getChild(3).getText())
                    reg = RegEx(RegexConst.RANGE, evalParseTree(tree.app().getChild(0)), (a, b))
                if tree.app().min_app() != None:
                    #expresia trebuie sa aiba un numar minim de aparitii
                    a = int(tree.app().min_app().getChild(1).getText())
                    reg = RegEx(RegexConst.RANGE, evalParseTree(tree.app().getChild(0)), (a, -1))
                if tree.app().max_app() != None:
                    #expresia trebuie sa aiba un numar maxim de aparitii
                    b = int(tree.app().max_app().getChild(2).getText())
                    reg = RegEx(RegexConst.RANGE, evalParseTree(tree.app().getChild(0)), (-1, b))
                if tree.app().fixed_app() != None:
                    #expresia are un numar fixat de aparitii
                    number = int(tree.app().fixed_app().getChild(1).getText())
                    reg = RegEx(RegexConst.RANGE, evalParseTree(tree.app().getChild(0)), (number, number))
            else:
                if tree.PLUS() != None:
                    #nodul este de tip +
                     reg = RegEx(RegexConst.PLUS, evalParseTree(tree.getChild(0)))
                else:
                    if tree.QUESTION_MARK() != None:
                        #nod de tip ?
                        reg = RegEx(RegexConst.MAYBE, evalParseTree(tree.getChild(0)))
                    else:
                        #nod de tip other_regex_3, trebuie explorat
                        reg = evalParseTree(tree.getChild(0))

        return reg;

    if isinstance(tree, RegexGrammarParser.Other_regex_3Context):
        if tree.ALPHA() != None:
            #litera din alfabet
            reg = RegEx(RegexConst.SYMBOL_SIMPLE, str(tree.ALPHA()))
        if tree.DIGIT() != None:
            #cifra din alfabet
            reg = RegEx(RegexConst.SYMBOL_SIMPLE, str(tree.DIGIT()))
        if tree.parenthesized_regex() != None:
            #(regex), evaluam ce se afla intre paranteze.
            return evalParseTree(tree.parenthesized_regex().getChild(1))
        if tree.anything() != None:
            #nod de tip .
            reg = RegEx(RegexConst.SYMBOL_ANY)
        if tree.group() != None:
            #este un nod de tip grup, trebuie sa exploram continutul - lista din interiorul []
            return evalParseTree(tree.group().getChild(1))

    if isinstance(tree, RegexGrammarParser.Group_contentsContext):
        #nod ce reprezinta continutul dintre [] al unui grup. Convertim continutul grupului intr-un set
        reg = RegEx(RegexConst.SYMBOL_SET, buildSetFromGroup(tree))

    return reg

"""
Primeste un set si construiste k1 | k2 | ... unde k1,k2,... sunt elemente ale setului
"""
def buildAternationOverSet(alphabet):
    if  len(alphabet) == 1:
        #cand ajungem la ultimul element construim o expresie regulata doar pt el, nu un sau.
        return RegularExpression(RegularExpressionConst.SYMBOL, alphabet[0])
    else:
        symbol = alphabet[0] #primul element din set
        alphabet.pop(0) #elimina elementul

        #construiste sau intre simbolul curent si expresia rezultata din celelalte n - 1 simboluri din alfabet
        tempExpr = RegularExpression(RegularExpressionConst.ALTERNATION, RegularExpression(RegularExpressionConst.SYMBOL, symbol), buildAternationOverSet(alphabet))
        return tempExpr

"""
Primeste o expresie regulata si genereaza expr expr ... expr de count ori
"""
def concatenateExprWithItself(regularExpr, count):
    if count == 0:
        #expr concatenata de 0 ori este epsilon
        return RegularExpression(RegularExpressionConst.EMPTY_STRING)
    if count == 1:
        return regularExpr
    else:
        #concatenare intre expr si rezultatul celorlalte count - 1 concatenari
        return RegularExpression(RegularExpressionConst.CONCATENATION, regularExpr, concatenateExprWithItself(regularExpr, count - 1))

"""
Primeste o expresie regulata si genereaza "count" sau-uri.
Fiecare expresie din sau reprezinta expresia "regularExpr" concatenata cu ea insasi de un numar diferit de ori, incepand de la "start"
La fiecare recursivitate se mai introduce o concatenare
"""
def buildAternationOverExpression(regularExpr, count, start):
    newExpr = concatenateExprWithItself(regularExpr, start) #genereaza expr expr ... expr de start ori

    if count == 1:
        tempExpr = newExpr
    else:
        #genereaza sau intre expresia curenta si rezultatul apelului recursiv
        tempExpr = RegularExpression(RegularExpressionConst.ALTERNATION, newExpr, buildAternationOverExpression(regularExpr, count - 1, start + 1))

    return tempExpr

"""
Converteste un regex la o expresie regulata clasica
"""
def regexToRegularExpression(regex):

    regularExpression = RegularExpression(RegularExpressionConst.EMPTY_STRING)

    if regex.type == RegexConst.ALTERNATION:
        #regex|regex -> regularExpression | regularExpression (convertim lhs si rhs la expresii regulate si le combinam)
        regularExpression = RegularExpression(RegularExpressionConst.ALTERNATION, regexToRegularExpression(regex.lhs), regexToRegularExpression(regex.rhs))

    if regex.type == RegexConst.CONCATENATION:
        #regex regex -> regularExpression regularExpression (convertim lhs si rhs la expresii regulate si le combinam)
        regularExpression = RegularExpression(RegularExpressionConst.CONCATENATION, regexToRegularExpression(regex.lhs), regexToRegularExpression(regex.rhs))

    if regex.type == RegexConst.SYMBOL_SIMPLE:
        #caz de baza, avem doar un simbol
        regularExpression = RegularExpression(RegularExpressionConst.SYMBOL, regex.symbol)

    if regex.type == RegexConst.PLUS:
        #avem un regex+ care se traduce in regex regex*. Convertim regex la regularExpression si apoi generam regularExpression regularExpression*
        tempExpr = regexToRegularExpression(regex.lhs)
        regularExpression = RegularExpression(RegularExpressionConst.CONCATENATION, tempExpr, RegularExpression(RegularExpressionConst.STAR, tempExpr))

    if regex.type == RegexConst.STAR:
        #avem regex*, devine regularExpression*
        tempExpr = regexToRegularExpression(regex.lhs)
        regularExpression = RegularExpression(RegularExpressionConst.STAR, tempExpr)

    if regex.type == RegexConst.MAYBE:
        #avem regex? care e echivalent cu (regex | epsilon). Convertim regex la regularExpression is obtinem (regularExpression | epsilon)
        tempExpr = regexToRegularExpression(regex.lhs)
        regularExpression = RegularExpression(RegularExpressionConst.ALTERNATION, tempExpr, RegularExpression(RegularExpressionConst.EMPTY_STRING))

    if regex.type == RegexConst.SYMBOL_ANY:
        #. se traduce in orice simbol din alfabet. Trebuie sa generam o expresie de forma k1 | k2 | ... | kn unde k1,k2,kn apartin lui Sigma.
        alphabet = string.digits + string.ascii_letters
        regularExpression = buildAternationOverSet(list(alphabet)) #construieste sau-uri intre toate simbolurile

    if regex.type == RegexConst.RANGE:
        #avem un regex{aparitii}. Convertim regexul la regularExpression si analizam aparitiile
        tempExpr = regexToRegularExpression(regex.lhs)
        a = regex.range[0]
        b = regex.range[1]

        if a == b:
            #expresia regulata trebuie sa apara de un numar fix de ori, adica o concatenam cu ea insasi de a ori
            #a{3} se traduce in aaa
            regularExpression = concatenateExprWithItself(tempExpr, a)
        else:
            if not (a == -1) and not (b == -1):
                #expresia regulata trebuia sa apara de minim a ori si maxim b ori, adica generam b - a + 1 sau-uri
                #expresiile folosinte in sau-uri sunt concatenari ale expresiei regulate cu ea insasi de un numar diferit de ori(incepe cu a si creste cu 1)
                #a{1,3} se traduce in a | aa | aaa
                regularExpression = buildAternationOverExpression(tempExpr, b - a + 1, a)
            else:
                if a == -1:
                    #expresia trebuie sa apara de un numar maxim de ori b.
                    #incepem de la epsilon si concatenam expresia regulata cu ea insasi la fiecare pas, pana ajungem la b + 1 sau-uri
                    #de exemplu a{,4} o sa fie epsilon | a | aa | aaa | aaaa
                    regularExpression = buildAternationOverExpression(tempExpr, b + 1, 0)
                else:
                    if b == -1:
                        #expresia trebuie sa apara de un numar minim de ori a
                        #concatenam expresia cu ea insasi de a ori si apoi adaugam expresie*
                        #de exemplu a{3,} devine aaa a* = aaaa*
                        baseExpr = concatenateExprWithItself(tempExpr, a) #expresia concatenata de a ori
                        kleenStar = RegularExpression(RegularExpressionConst.STAR, tempExpr) #expresie*
                        regularExpression = RegularExpression(RegularExpressionConst.CONCATENATION, baseExpr, kleenStar) #concatenarea celor 2

    if regex.type == RegexConst.SYMBOL_SET:
        #avem un grup de simboluri
        symbolsList = list()

        for c in regex.symbol_set: # pentru fiecare element din set(fie tuplu fie simbol)
            if not(isinstance(c, tuple)):
                #daca nu este tuplu inseamna ca este un singur simbol si il adaugam la lista
                symbolsList.append(str(c))
            else:
                #este un tuplu, fie de cifre, fie de litere
                if c[0].isdigit():
                    #tuplu de cifre, adica trebuie sa generam toate simbolurile intre c[0] si c[1]
                    #list(range(c[0], c[1] + 1)) ne da o lista de numere intregi in intervalul de mai sus
                    #mapul aplica str pe fiecare element pentru a transforma lista de int intr-o lista de string
                    symbolsList = symbolsList + list(map(lambda x: str(x), list(range(int(c[0]), int(c[1]) + 1))))
                else:
                    #tuplu de litere.
                    symbolsList = symbolsList + list(map(lambda x: chr(x), list(range(ord(c[0]), ord(c[1]) + 1))))

        #am transformat setul intr-o lista de simboluri, acum trebuie sa generam sau-uri intre fiecare simbol
        regularExpression = buildAternationOverSet(symbolsList)

    return regularExpression

"""
Primeste 2 siruri si intoarce rezultatul concatenarii, fara elemente duplicate
"""
def concatenateStringsWithoutDuplicates(str1, str2):
    result = list(str1)
    result = result + [c for c in list(str2) if c not in str1]
    return ''.join(result)

"""
Primeste un automat tinta si un automat referinta si schimba starile automatului referinta
astfel incat sa nu exista stari cu acelasi nume intre cele 2 automate
Functia este luata din laborator
"""
def renameStates(target, reference):
    off = max(reference.states) + 1
    target.start_state += off
    target.states = set(map(lambda s: s + off, target.states))
    target.final_states = set(map(lambda s: s + off, target.final_states))
    new_delta = {}
    for (state, symbol), next_states in target.delta.items():
        new_next_states = set(map(lambda s: s + off, next_states))
        new_delta[(state + off, symbol)] = new_next_states

    target.delta = new_delta

"""
Primeste un numar de automate si intoarce 2 stari care nu apar deja in vreun automat
Functia este luata din laborator
"""
def newStates(*nfas):
    state = 0
    for nfa in nfas:
        m = max(nfa.states)
        if m >= state:
            state = m + 1

    return state, state + 1

"""
Converteste o expresie regulata intr-un AFN
AFN-ul rezultat va avea mereu o singura stare finala
"""
def regularExpressionToNfa(re):

    if re.type == RegularExpressionConst.CONCATENATION:
        #expresie regulata de forma A B. Convertim A la NFA si B la NFA
        nfa1 = regularExpressionToNfa(re.lhs)
        nfa2 = regularExpressionToNfa(re.rhs)

        #ne asiguram ca nu exista stari cu acelasi nume in automate
        renameStates(nfa1, nfa2)

        resultingAlphabet   = concatenateStringsWithoutDuplicates(nfa1.alphabet, nfa2.alphabet)
        resultingStates     = nfa1.states | nfa2.states
        resultingStartState = nfa1.start_state
        resultingFinalState = nfa2.final_states
        resultingDelta      = {}
        resultingDelta.update(nfa1.delta)
        resultingDelta.update(nfa2.delta)

        #starea initiala a lui nfa trebuie sa aiba o tranzitie epsilon catre starea initiala a lui nfa2
        if (list(nfa1.final_states)[0], "ε") in resultingDelta:
            #starea finala din nfa1 are deja cateva tranzitii epsilon, adaugam si tranzitia noastra
            transitions = resultingDelta[(list(nfa1.final_states)[0], "ε")]
            transitions.add(nfa2.start_state)
            resultingDelta[(list(nfa1.final_states)[0], "ε")] = transitions
        else:
            #nu exista deja, adaugam pur si simplu
            resultingDelta[(list(nfa1.final_states)[0], "ε")] = {nfa2.start_state}

        nfa = NFA(resultingAlphabet, resultingStates, resultingStartState, resultingFinalState, resultingDelta)

    if re.type == RegularExpressionConst.ALTERNATION:
        # expresie regulata de forma A|B. Convertim A la NFA si B la NFA
        nfa1 = regularExpressionToNfa(re.lhs)
        nfa2 = regularExpressionToNfa(re.rhs)

        # ne asiguram ca nu exista stari cu acelasi nume in automate
        renameStates(nfa1, nfa2)

        # generam 2 noi stari, noua stare initiala si noua stare finala
        initialState, finalState = newStates(nfa1, nfa2)

        resultingAlphabet   = concatenateStringsWithoutDuplicates(nfa1.alphabet, nfa2.alphabet)
        resultingStates     = nfa1.states | nfa2.states
        resultingStartState = initialState
        resultingFinalState = set()
        resultingDelta      = {}
        resultingDelta.update(nfa1.delta)
        resultingDelta.update(nfa2.delta)

        resultingStates.add(initialState)
        resultingStates.add(finalState)
        resultingFinalState.add(finalState)

        #din noua stare initiala avem 2 tranzitii epsilon catre starile initiale din nfa1 si nfa2
        resultingDelta[(initialState, "ε")] = {nfa2.start_state, nfa1.start_state}

        #din starile finale din nfa1 si nfa2 avem o tranzitie epsilon catre noua stare finala
        if (list(nfa1.final_states)[0], "ε") in resultingDelta:
            transitions = resultingDelta[(list(nfa1.final_states)[0], "ε")]
            transitions.add(finalState)
            resultingDelta[(list(nfa1.final_states)[0], "ε")] = transitions
        else:
            resultingDelta[(list(nfa1.final_states)[0], "ε")] = {finalState}

        if (list(nfa2.final_states)[0], "ε") in resultingDelta:
            transitions = resultingDelta[(list(nfa2.final_states)[0], "ε")]
            transitions.add(finalState)
            resultingDelta[(list(nfa2.final_states)[0], "ε")] = transitions
        else:
            resultingDelta[(list(nfa2.final_states)[0], "ε")] = {finalState}


        nfa = NFA(resultingAlphabet, resultingStates, resultingStartState, resultingFinalState, resultingDelta)

    if re.type == RegularExpressionConst.STAR:
        #espresie de forma A*
        nfa1 = regularExpressionToNfa(re.lhs)

        transitions = set()

        if (nfa1.start_state, "ε") in nfa1.delta.keys():
            # starea initala din nfa1 are deja tranzitii pe epsilon
            transitions = nfa1.delta[(nfa1.start_state, "ε")]

        #adauga o tranzitie epsilon catre starea finala
        transitions.add(list(nfa1.final_states)[0])
        nfa1.delta[(nfa1.start_state, "ε")] = transitions

        #similar, doar ca adaugam o tranzitie epsilon de la starea finala la starea initiala
        transitions = set()

        if (list(nfa1.final_states)[0], "ε") in nfa1.delta.keys():
            transitions = nfa1.delta[(list(nfa1.final_states)[0], "ε")]

        transitions.add(nfa1.start_state)
        nfa1.delta[(list(nfa1.final_states)[0], "ε")] = transitions

        nfa = NFA(nfa1.alphabet, nfa1.states, nfa1.start_state, nfa1.final_states, nfa1.delta)

    if re.type == RegularExpressionConst.SYMBOL:
        #simbol, din starea 0 pe simbol trecem in starea finala 1
        nfa = NFA(string.digits + string.ascii_letters, {0, 1}, 0, {1}, {(0, re.symbol): {1}})

    if re.type == RegularExpressionConst.EMPTY_STRING:
        #tranzitie pe epsilon catre starea finala
        nfa = NFA(string.digits + string.ascii_letters, {0, 1}, 0, {1}, {(0, "ε"): {1}})

    if re.type == RegularExpressionConst.EMPTY_SET:
        #nu accepta nimic
        nfa = NFA(string.digits + string.ascii_letters, {0}, 0, {}, {})

    return nfa

"""
Construieste inchiderea epsilon a starii "state" in automatul "nfa"
visited este initial un set gol
"""
def buildEpsilonClosureForState(state, nfa, visited):

    epsilonClosure = set()

    for key in nfa.delta:
            if key[0] == state:
                #toate tranzitiile din stare state pe simbolul epsilon
                if key[1] == "ε":
                    for nextState in nfa.delta[key]:
                        # nextState este starea in care ajungem
                        #adauga starea in inchiderea epsilon a starii state
                        epsilonClosure.add(nextState)
                        if nextState not in visited:
                            #nextState nu a fost deja explorata
                            visited[nextState] = True

                            #construiste inchiderea epsilon a lui nextState - verifica ce stari sunt accesibile prin trazitii epsilon din nextState
                            tempClosure = buildEpsilonClosureForState(nextState, nfa, visited)

                            #combina cele 2 seturi pt a obtine inchiderea completa a starii curente
                            epsilonClosure = epsilonClosure | tempClosure

    return epsilonClosure


"""
Converteste un automat finit nedeterminist intr-un automat finit determinist
"""
def nfaToDFA(nfa):

    states = set() #starile automatului finit determinist rezultat
    final_states = set() #starile finale ale automatului finit determinist rezultat

    #pentru a lucra usor cu inchiderile epsilon ce vor deveni stari efective in automat si pt ca
    #starile pot sa fie doar numere intregi, o sa convertim o stare de forma {x,y,z} la un sir x#y#z
    #cu care vom lucra pe parcursul algoritmului.
    #fiecare sir va avea asociat un numar intreg, ce va reprezenta numele starii din noul afd
    statesMap = {}
    epsilonClosures = {}
    delta = {}

    #lista starilor din automat cu nume siruri de caractere
    stringStates = []
    stringStartState = ""

    i = 0
    for state in nfa.states:
        visited = {}
        epsilonClosures[state] = buildEpsilonClosureForState(state, nfa, visited) #inchiderea epsilon sub forma de set
        epsilonClosures[state].add(state)
        epsilonClosures[state] = '#'.join(map(str, epsilonClosures[state])) #inchiderea epsilon sub forma de sir

        #adauga starea la lista de stari
        stringStates.append(epsilonClosures[state])

        #adauga int-ul asociat in lista de stari a afd-ului
        states.add(i)

        #mapeaza sirul la int
        statesMap[epsilonClosures[state]] = i

        if state == nfa.start_state:
            stringStartState = epsilonClosures[state]

        i = i + 1

    #parcurgem automatul finit nedeterminist asemanator cu algoritmul "BFS"
    visited = {}
    while stringStates:
        #prima stare din "coada"(lista)
        state = stringStates.pop(0)

        #ia fiecare stare din afn care formeaza starea curenta. de exemplu sirul x#y#z e format din starile x y z din afn
        for s in list(state.split('#')):
            if int(s) in nfa.final_states:
                #daca cel putin una dintre stari era finala,a tunci state este finala
                final_states.add(statesMap[state])

        for c in nfa.alphabet: #pentru fiecare simbol din alfabetul automatului
             closures = []

             for s in list(state.split('#')):
                 #dintr-o stare din afn componenta a starii state exista tranzitii pe simbolul curent
                 if (int(s), c) in nfa.delta:
                     nextStates = nfa.delta[(int(s), c)] #starile in care putem ajunge din s pe simbolul c
                     for d in nextStates: #pentru fiecare stare in care putem ajunge
                        #construim stare in care putem ajunge din state reunind
                        #inchiderile epsilon ale tuturor starilor accesibile din starile ce alcatuiesc state
                        closures.append(epsilonClosures[d])

             if closures:
                #daca putem ajunge undeva din starea state pe simbolul c
                #vrem ca reuniunea sa nu contina duplicate. Apoi convertim lista de posibilitati la un sir, pentru a forma starea din automat
                #de exemplu: state = 1#2. Din starea 1 pe simbolul a se ajunge in stare 3 si din 2 pe a se ajunge in 4.
                #closures o sa fie closures(2) reunit cu closures(4)
                #deci din 1#2 ajungem in str(closures(2) reunit closures(4))
                closures = '#'.join(map(str, list(dict.fromkeys(closures))))

                #daca reuniunea a produs o stare noua
                if not (closures in statesMap):
                    statesMap[closures] = i
                    states.add(i)
                    i = i + 1

                #adaugam o tranzitie din starea "state" in starea reprezentata de "closures"
                delta[(statesMap[state], c)] = statesMap[closures]

                if not (closures in visited):
                     #starea nu a fost vizitata deja, o adaugam in coada
                     visited[closures] = True
                     stringStates.append(closures)


    alphabet = nfa.alphabet
    start_state = statesMap[stringStartState]

    #adaugam toate tranzitiile care lipsesc catreun synk state(None)- cod din laborator
  #  all_transitions = (product(states, alphabet))
  #  not_present_trans = filter(lambda x: x not in delta, all_transitions)
  #  for trans in not_present_trans:
  #      delta[trans] = None

    newDFA = DFA(alphabet, states, start_state, final_states, delta)
    return newDFA

"""
Primeste un automat finit determinist si un cuvant si verifica daca se poate ajunge
intr-o stare finala consumand tot inputul.
"""
def runDFA(dfa, word):
    state = dfa.start_state #plecam din starea initiala
    pos = 0 #plecam de la inceputul cuvantului

    #starea urmatoare
    next = dfa.delta[(state, word[pos])]

    #cat timp nu ajunge in synk si mai avem caractere in cuvant
    while next != None and (pos + 1) < len(word):
        pos = pos + 1;
        next = dfa.delta[(next, word[pos])] #executa tranzitia

    #daca am ramas intr-o stare finala si am consumat tot cuvantul inseamna ca automatul accepta
    if (next in dfa.final_states) and (pos == len(word) - 1):
        return True
    else:
        return False


if __name__ == "__main__":
    valid = (len(sys.argv) == 4 and sys.argv[1] in ["RAW", "TDA"]) or \
            (len(sys.argv) == 3 and sys.argv[1] == "PARSE")
    if not valid:
        sys.stderr.write(
            "Usage:\n"
            "\tpython3 main.py RAW <regex-str> <words-file>\n"
            "\tOR\n"
            "\tpython3 main.py TDA <tda-file> <words-file>\n"
            "\tOR\n"
            "\tpython3 main.py PARSE <regex-str>\n"
        )
        sys.exit(1)

    if sys.argv[1] == "TDA":
        tda_file = sys.argv[2]
        with open(tda_file, "rb") as fin:
            parsed_regex = pickle.loads(fin.read())
    else:
        regex_string = sys.argv[2]

        inputStream = InputStream(regex_string)
        lexer = RegexGrammarLexer(inputStream) #lexer ANTLR
        stream = CommonTokenStream(lexer)
        parser = RegexGrammarParser(stream) #parser ANTLR

        tree = parser.regex() #arborele de parsare
        parsed_regex = evalParseTree(tree.getChild(0)) #parcurgem arborele pentru a genera un regex

        if sys.argv[1] == "PARSE":
            print(str(parsed_regex))
            sys.exit(0)

    # În acest punct, fie că a fost parsat, fie citit direct ca obiect, aveți
    # la dispoziție variabila "parsed_regex" care conține un obiect de tip
    # RegEx. Aduceți-l la forma de Automat Finit Determinist, pe care să puteți
    # rula în continuare.

    regularExpression = regexToRegularExpression(parsed_regex) #converteste regex la expresie regulata
    nfa = regularExpressionToNfa(regularExpression)#converteste expresie regulata la nfa
    newDFA = nfaToDFA(nfa) #converte nfa la dfa

    newDFA.to_graphviz().render(quiet_view = True, cleanup = True )

    with open(sys.argv[3], "r") as fin:
        content = fin.readlines()

    for word in content:
        word = word.strip() #elimina spatiile goale de la inceputul si finalul cuvantului

        if runDFA(newDFA, word):
           print("True")
        else:
            print("False")
        pass
