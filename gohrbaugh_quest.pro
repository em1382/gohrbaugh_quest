% NANI SEARCH - A sample adventure game

% Nani Search is designed to illustrate Prolog programming.  It
% is an implementation of the principle example used in
% this tutorial.

:- dynamic(have/1).
:- dynamic(here/1).
:- dynamic(location/2).
:- dynamic turned_off/1.
:- dynamic turned_on/1.

start:- gohrbaugh_quest.       % main entry point

gohrbaugh_quest:-
  init_dynamic_facts,     % predicates which are not compiled
  nl,
  write('Gohrbaugh Quest: The Battle for the Department Chair'),nl,
  write('Copyright (C) Sladagan inc. 2018'),nl,
  write('No rights reserved, use it as you wish'),nl,
  nl,
  write('The year is 2153.'),nl,
  write('Our story takes place at Messiah College'),nl,nl,
  write('In the current year, the department chair is not limited like it is today'),nl,
  write('Instead, one department chair is appointed for life'),nl,
  nl,
  write('Currently, the evil Dr. Wcott Seaver'),nl,
  write('and his army of PHP-powered robots have complete control of the department,'),nl,
  write('and he has just passed a movement banning all non-web development classes'),nl,
  write('from the curriculum!'),nl,
  nl,
  write('You are Rene Gohrbaugh, a retired department chair'),nl,
  write('You must do everything in your power to stop the theory-disregarding menace!'),nl,
  write('However, before your quest, you head to Lottie for lunch'),nl,
  write('(You certainly can''t have an adventure on an empty stomach)'),nl,
  nl,
  write('Hit any key to continue.'),get0(_),
  write('Type "help" if you need more help on mechanics.'),nl,
  write('Type "hint" if you want a big hint.'),nl,
  write('Type "quit" if you give up.'),nl,
  nl,

  look,                   % give a look before starting the game
  command_loop.

% command_loop - repeats until either the nani is found or the
%     player types quit

command_loop:-
  repeat,
  get_command(X),
  do(X),
  (nanifound; X == quit).

% do - matches the input command with the predicate which carries out
%     the command.  More general approaches which might work in the
%     listener are not supported in the compiler.  This approach
%     also gives tighter control over the allowable commands.

%     The cuts prevent the forced failure at the end of "command_loop"
%     from backtracking into the command predicates.

do(goto(X)):-goto(X),!.
do(nshelp):-nshelp,!.
do(hint):-hint,!.
do(inventory):-inventory,!.
do(take(X)):-take(X),!.
do(drop(X)):-drop(X),!.
do(eat(X)):-eat(X),!.
do(look):-look,!.
do(turn_on(X)):-turn_on(X),!.
do(turn_off(X)):-turn_off(X),!.
do(look_in(X)):-look_in(X),!.
do(talk_to(X)):-talk_to(X),!.
do(quit):-quit,!.

% These are the predicates which control exit from the game.  If
% the player has taken the nani, then the call to "have(nani)" will
% succeed and the command_loop will complete.  Otherwise it fails
% and command_loop will repeat.

nanifound:-
  fail,
  write('Congratulations, you defeated Dr. Seaver'),nl,
  write('and saved Messiah College! Now people can learn'),nl,
  write('about all aspects of computer science in peace.'),nl,nl.

quit:-
  write('Giving up?  I guess you''re okay with people only learning php then'),nl,
  write('Is this what you really want?'),nl,
  write('Well, seeya!'),nl,nl.

% The help command

nshelp:-
  write('Use simple English sentences to enter commands.'),nl,
  write('The commands can cause you to:'),nl,
  nl,
  write('   go to a room          (ex. go to the office)'),nl,
  write('   look around           (ex. look)'),nl,
  write('   look in something     (ex. look in the desk)'),nl,
  write('   take something        (ex. take the apple)'),nl,
  write('   drop something        (ex. drop the apple)'),nl,
  write('   eat something         (ex. eat the apple)'),nl,
  write('   turn something on     (ex. turn on the light)'),nl,
  write('   inventory your things (ex. inventory)'),nl,
  nl,
  write('The examples are verbose, terser commands and synonyms'),nl,
  write('are usually accepted.'),nl,nl,
  write('Hit any key to continue.'),nl,
  get0(_),
  look.

hint:-
  write('No hints for you, buddy!'),nl,nl,
  look.

% Initial facts describing the world.  Rooms and doors do not change,
% so they are compiled.

room(lottie).
room('eisenhower upper hallway').
room(outside).
room('frey first floor').
room('frey first floor stairwell').
room('frey second floor').
room('frey second floor stairwell').
room('frey third floor').
room('faculty hallway').
room('seaver''s office').

door(lottie, 'eisenhower upper hallway').
door('eisenhower upper hallway', outside).
door('outside', 'frey first floor').
door('frey first floor', 'frey first floor stairwell').
door('frey first floor stairwell', 'frey second floor').
door('frey second floor', 'frey second floor stairwell').
door('frey second floor stairwell', 'frey third floor').
door('frey third floor', 'faculty hallway').
door('faculty hallway', 'seaver''s office').

connect(X,Y):-
  door(X,Y).
connect(X,Y):-
  door(Y,X).

% These facts are all subject to change during the game, so rather
% than being compiled, they are "asserted" to the listener at
% run time.  This predicate is called when "gohrbaugh_quest" starts up.

init_dynamic_facts:-
  assertz(location(buffet, lottie)),
  assertz(location('healthy meal', buffet)),
  assertz(location('unhealthy meal', buffet)),
  assertz(here(lottie)),
  assertz(location('mllis eadagan', lottie)),
  assertz(location('sik nloop', lottie)),
  assertz(location('rordon gamsey', lottie)).

character(ellis).
character(nik).
character('mllis eadagan').
character('sik nloop').
character('rordon gamsey').

is_alive(ellis).
is_alive(nik).
is_alive('mllis eadagan').
is_alive('sik nloop').
is_alive('rordon gamsey').

says(ellis, 'Hello, welcome to the demo!').
says(nik, 'Hi, welcome to the demo!').
says('mllis eadagan', 'I'' a little sick of this Lottie food').
says('sik nloop', 'Meh, I should have gone to Union').
says('rordon gamsey', 'This food is all raw..').

furniture(buffet).

edible('healthy meal').
edible('unhealthy meal').


%%%%%%%% COMMANDS %%%%%%%%%%%%%%%%%%%%%%%%%%

% goto moves the player from room to room.

goto(Room):-
  can_go(Room),                 % check for legal move
  puzzle(goto(Room)),           % check for special conditions
  moveto(Room),                 % go there and tell the player
  look.
goto(_):- look.

can_go(Room):-                  % if there is a connection it
  here(Here),                   % is a legal move.
  connect(Here,Room),!.
can_go(Room):-
  respond(['You can''t get to ',Room,' from here']),fail.

moveto(Room):-                  % update the logicbase with the
  retract(here(_)),             % new room
  asserta(here(Room)).

% look lists the things in a room, and the connections

look:-
  here(Here),
  respond(['You are in ',Here]),
  write('You can see the following characters:'),nl,
  list_characters(Here),
  write('You can see the following things:'),nl,
  list_things(Here),
  write('You can go to the following rooms:'),nl,
  list_connections(Here).

list_things(Place):-
  (furniture(X) ; edible(X)),
  location(X,Place),
  tab(2),write(X),nl,
  fail.
list_things(_).

list_connections(Place):-
  connect(Place,X),
  tab(2),write(X),nl,
  fail.
list_connections(_).

list_characters(Place):-
  character(X),
  location(X,Place),
  tab(2),write(X),nl,
  fail.
list_characters(_).

% talk_to allows the player to talk to a character

talk_to(Character):-
  is_here(Character),
  is_alive(Character),
  says(Character, X),
  nl,tab(2),respond([Character, ': ', X]).
talk_to(_):-
  write('That person is nowhere to be seen...'),nl,
  fail.

% look_in allows the player to look inside a thing which might
% contain other things

look_in(Thing):-
  location(_,Thing),               % make sure there's at least one
  write('The '),write(Thing),write(' contains:'),nl,
  list_things(Thing).
look_in(Thing):-
  respond(['There is nothing in the ',Thing]).

% take allows the player to take something.  As long as the thing is
% contained in the room it can be taken, even if the adventurer hasn't
% looked in the the container which contains it.  Also the thing
% must not be furniture.

take(Thing):-
  is_here(Thing),
  is_takable(Thing),
  move(Thing,have),
  respond(['You now have the ',Thing]).

is_here(Thing):-
  here(Here),
  contains(Thing,Here),!.          % don't backtrack
is_here(Thing):-
  respond(['There is no ',Thing,' here']),
  fail.

contains(Thing,Here):-             % recursive definition to find
  location(Thing,Here).            % things contained in things etc.
contains(Thing,Here):-
  location(Thing,X),
  contains(X,Here).

is_takable(Thing):-                % you can't take the furniture
  furniture(Thing),
  respond(['You can''t pick up a ',Thing]),
  !,fail.
is_takable(_).                     % not furniture, ok to take

move(Thing,have):-
  retract(location(Thing,_)),      % take it from its old place
  asserta(have(Thing)).            % and add to your possessions

% drop - allows the player to transfer a possession to a room

drop(Thing):-
  have(Thing),                     % you must have the thing to drop it
  here(Here),                      % where are we
  retract(have(Thing)),
  asserta(location(Thing,Here)).
drop(Thing):-
  respond(['You don''t have the ',Thing]).


% eat, because every adventure game lets you eat stuff.

eat(Thing):-
  have(Thing),
  eat2(Thing).
eat(Thing):-
  respond(['You don''t have the ',Thing]).

eat2(Thing):-
  edible(Thing),
  retract(have(Thing)),
  respond(['That ',Thing,' was good']).
eat2(Thing):-
  tastes_yuchy(Thing),
  respond(['Three year olds don''t eat ',Thing]).
eat2(Thing):-
  respond(['You can''t eat a ',Thing]).


% inventory list your possesions

inventory:-
  have(_),                         % make sure you have at least one thing
  write('You have: '),nl,
  list_possessions.
inventory:-
  write('You have nothing'),nl.

list_possessions:-
  have(X),
  tab(2),write(X),nl,
  fail.
list_possessions.

% turn_on recognizes two cases.  If the player tries to simply turn
% on the light, it is assumed this is the room light, and the
% appropriate error message is issued.  Otherwise turn_on has to
% refer to an object which is turned_off.

turn_on(light):-
  respond(['You can''t reach the switch and there''s nothing to stand on']).
turn_on(Thing):-
  have(Thing),
  turn_on2(Thing).
turn_on(Thing):-
  respond(['You don''t have the ',Thing]).

turn_on2(Thing):-
  turned_on(Thing),
  respond([Thing,' is already on']).
turn_on2(Thing):-
  turned_off(Thing),
  retract(turned_off(Thing)),
  asserta(turned_on(Thing)),
  respond([Thing,' turned on']).
turn_on2(Thing):-
  respond(['You can''t turn a ',Thing,' on']).

% turn_off - I didn't feel like implementing turn_off

turn_off(_):-
  respond(['I lied about being able to turn things off']).

% The only special puzzle in Nani Search has to do with going to the
% cellar.  Puzzle is only called from goto for this reason.  Other
% puzzles pertaining to other commands could easily be added.

puzzle(goto(cellar)):-
  have(flashlight),
  turned_on(flashlight),!.
puzzle(goto(cellar)):-
  write('You can''t go to the cellar because it''s dark in the'),nl,
  write('cellar, and you''re afraid of the dark.'),nl,
  !,fail.
puzzle(_).

% respond simplifies writing a mixture of literals and variables

respond([]):-
  write('.'),nl,nl.
respond([H|T]):-
  write(H),
  respond(T).

% Simple English command listener.  It does some semantic checking
% and allows for various synonyms.  Within a restricted subset of
% English, a command can be phrased many ways.  Also non grammatical
% constructs are understood, for example just giving a room name
% is interpreted as the command to goto that room.

% Some interpretation is based on the situation.  Notice that when
% the player says turn on the light it is ambiguous.  It could mean
% the room light (which can't be turned on in the game) or the
% flash light.  If the player has the flash light it is interpreted
% as flash light, otherwise it is interpreted as room light.

get_command(C):-
  readlist(L),        % reads a sentence and puts [it,in,list,form]
  command(X,L,[]),    % call the grammar for command
  C =.. X,!.          % make the command list a structure
get_command(_):-
  respond(['I don''t understand, try again or type help']),fail.

% The grammar doesn't have to be real English.  There are two
% types of commands in Nani Search, those with and without a
% single argument.  A special case is also made for the command
% goto which can be activated by simply giving a room name.

command([Pred,Arg]) --> verb(Type,Pred),nounphrase(Type,Arg).
command([Pred]) --> verb(intran,Pred).
command([goto,Arg]) --> noun(go_place,Arg).

% Recognize three types of verbs.  Each verb corresponds to a command,
% but there are many synonyms allowed.  For example the command
% turn_on will be triggered by either "turn on" or "switch on".

verb(go_place,goto) --> go_verb.
verb(thing,V) --> tran_verb(V).
verb(person,V) --> tran_verb(V).
verb(intran,V) --> intran_verb(V).

go_verb --> [go].
go_verb --> [go,to].
go_verb --> [g].

tran_verb(take) --> [take].
tran_verb(take) --> [pick,up].
tran_verb(drop) --> [drop].
tran_verb(drop) --> [put].
tran_verb(drop) --> [put,down].
tran_verb(eat) --> [eat].
tran_verb(turn_on) --> [turn,on].
tran_verb(turn_on) --> [switch,on].
tran_verb(turn_off) --> [turn,off].
tran_verb(look_in) --> [look,in].
tran_verb(look_in) --> [look].
tran_verb(look_in) --> [open].
tran_verb(look_in) --> [examine].
tran_verb(talk_to) --> [talk].
tran_verb(talk_to) --> [talk,to].

intran_verb(inventory) --> [inventory].
intran_verb(inventory) --> [i].
intran_verb(look) --> [look].
intran_verb(look) --> [look,around].
intran_verb(look) --> [l].
intran_verb(quit) --> [quit].
intran_verb(quit) --> [exit].
intran_verb(quit) --> [end].
intran_verb(quit) --> [bye].
intran_verb(nshelp) --> [help].
intran_verb(hint) --> [hint].

% a noun phrase is just a noun with an optional determiner in front.

nounphrase(Type,Noun) --> det,noun(Type,Noun).
nounphrase(Type,Noun) --> noun(Type,Noun).

det --> [the].
det --> [a].

% Nouns are defined as rooms, things, or characters located somewhere.  We define
% special cases for those things represented in Gohrbaugh Quest by two
% words.  We can't expect the user to type the name in quotes.

noun(go_place,R) --> [R], {room(R)}.
noun(go_place,'dining room') --> [dining,room].
noun(go_place, 'eisenhower upper hallway') --> [eisenhower, upper, hallway].
noun(go_place, 'frey first floor') --> [frey, first, floor].
noun(go_place, 'frey first floor stairwell') --> [frey, first, floor, stairwell].
noun(go_place, 'frey second floor') --> [frey,second,floor].
noun(go_place, 'frey second floor stairwell') --> [frey,second,floor,stairwell].
noun(go_place, 'frey third floor') --> [frey,third,floor].
noun(go_place, 'faculty hallway') --> [faculty,hallway].
noun(go_place, 'seaver''s office') --> ['seaver''s', office].

noun(thing,T) --> [T], {location(T,_)}.
noun(thing,T) --> [T], {have(T)}.
noun(thing,flashlight) --> [flash,light].
noun(thing,'washing machine') --> [washing,machine].
noun(thing,'dirty clothes') --> [dirty,clothes].
noun(thing, 'healthy meal') --> [healthy, meal].
noun(thing, 'unhealthy meal') --> [unhealthy, meal].

noun(person,P) --> [P], {location(P,_)}.
noun(person,P) --> [P], {character(P)}.
noun(person, 'mllis eadagan') --> [mllis, eadagan].
noun(person, 'sik nloop') --> [sik, nloop].
noun(person, 'rordon gamsey') --> [rordon, gamsey].

% If the player has just typed light, it can be interpreted three ways.
% If a room name is before it, it must be a room light.  If the
% player has the flash light, assume it means the flash light.  Otherwise
% assume it is the room light.

noun(thing,light) --> [X,light], {room(X)}.
noun(thing,flashlight) --> [light], {have(flashlight)}.
noun(thing,light) --> [light].

% readlist - read a list of words, based on a Clocksin & Mellish
% example.

readlist(L):-
  write('> '),
  read_word_list(L).

read_word_list([W|Ws]) :-
  get0(C),
  readword(C, W, C1),       % Read word starting with C, C1 is first new
  restsent(C1, Ws), !.      % character - use it to get rest of sentence

restsent(C,[]) :- lastword(C), !. % Nothing left if hit last-word marker
restsent(C,[W1|Ws]) :-
  readword(C,W1,C1),        % Else read next word and rest of sentence
  restsent(C1,Ws).

readword(C,W,C1) :-         % Some words are single characters
  single_char(C),           % i.e. punctuation
  !,
  name(W, [C]),             % get as an atom
  get0(C1).
readword(C, W, C1) :-
  is_num(C),                % if we have a number --
  !,
  number_word(C, W, C1, _). % convert it to a genuine number
readword(C,W,C2) :-         % otherwise if character does not
  in_word(C, NewC),         % delineate end of word - keep
  get0(C1),                 % accumulating them until
  restword(C1,Cs,C2),       % we have all the word
  name(W, [NewC|Cs]).       % then make it an atom
readword(_,W,C2) :-         % otherwise
  get0(C1),
  readword(C1,W,C2).        % start a new word

restword(C, [NewC|Cs], C2) :-
  in_word(C, NewC),
  get0(C1),
  restword(C1, Cs, C2).
restword(C, [], C).


single_char(0',).
single_char(0';).
single_char(0':).
single_char(0'?).
single_char(0'!).
single_char(0'.).


in_word(C, C) :- C >= 0'a, C =< 0'z.
in_word(C, L) :- C >= 0'A, C =< 0'Z, L is C + 32.
in_word(0'',0'').
in_word(0'-,0'-).

% Have character C (known integer) - keep reading integers and build
% up the number until we hit a non-integer. Return this in C1,
% and return the computed number in W.

number_word(C, W, C1, Pow10) :-
  is_num(C),
  !,
  get0(C2),
  number_word(C2, W1, C1, P10),
  Pow10 is P10 * 10,
  W is integer(((C - 0'0) * Pow10) + W1).
number_word(C, 0, C, 0.1).


is_num(C) :-
  C =< 0'9,
  C >= 0'0.

% These symbols delineate end of sentence

lastword(10).   % end if new line entered
lastword(0'.).
lastword(0'!).
lastword(0'?).
