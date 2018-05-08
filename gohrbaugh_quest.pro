% GOHRBAUGH QUEST: A Simple Adventure Game

:- dynamic(have/1).
:- dynamic(here/1).
:- dynamic(location/2).
:- dynamic(is_alive/1).
:- dynamic(locked/1).

start:- gohrbaugh_quest.       % main entry point

gohrbaugh_quest:-
  init_dynamic_facts,     % predicates which are not compiled
  nl,
  write('Gohrbaugh Quest: The Battle for the Department Chair'),nl,
  write('Copyright (C) Sladagan inc. 2018'),nl,
  write('No rights reserved, use it as you wish'),nl,
  nl,
  write('The year is 2153.'),nl,
  write('Our story takes place at Messiah College.'),nl,nl,
  write('In the current year, the department chair is not limited like it is today.'),nl,
  write('Instead, one department chair is appointed for life.'),nl,
  nl,
  write('Currently, the evil Dr. Wcott Seaver and his army of PHP-powered robots'),nl,
  write('have complete control of the department,'),nl,
  write('and he has just passed a movement banning all non-web development classes'),nl,
  write('from the curriculum!'),nl,
  nl,
  write('You are Rene Gohrbaugh, a retired department chair, and'),nl,
  write('everything in your power must be done to stop the theory-disregarding menace!'),nl,
  nl,
  write('However, before your quest, you head to Lottie for lunch.'),nl,
  write('(You certainly can''t have an adventure on an empty stomach).'),nl,
  nl,
  write('Hit any key to continue.'),get0(_),
  write('Type "help" if you need more help on mechanics.'),nl,
  write('Type "hint" if you want a big hint (heh).'),nl,
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
  (seaverdefeated; X == quit).

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
do(attack(X)):-attack(X),!.
do(look):-look,!.
do(look_in(X)):-look_in(X),!.
do(look_at(X)):-look_at(X),!.
do(talk_to(X)):-talk_to(X),!.
do(quit):-quit,!.
do(password(X)):-password(X),!.

% These are the predicates which control exit from the game.  If
% the player has taken the nani, then the call to "have(nani)" will
% succeed and the command_loop will complete.  Otherwise it fails
% and command_loop will repeat.

seaverdefeated:-
  not(is_alive('wcott seaver')),
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
  write('   attack something      (ex. attack robot)'),nl,
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
room(110).
room(141).
room(150).
room(151).
room(166).
room('finance lab').

room('frey second floor').
room(241).
room(243).
room(250).
room('frey second floor stairwell').

room('frey third floor').
room(343).
room(345).
room(347).
room(349).

room('faculty hallway').
room('seaver''s office').
room('gohrbaugh''s office').
room('rilmer''s office').

description(lottie, 'You look around. All around you, you see students eating their lunches.').
description('eisenhower upper hallway', 'You look around: it''s the upper hallway of eisenhower...Not much here.').
description(outside, 'It''s a nice day out, the sun is shining.').
description('frey first floor', 'The nerds are out in force today, they say there''s a demo going on for a new text adventure game.').
description('frey first floor stairwell', 'You look up the stairs, it appears the stairway is blocked off after the second floor.\nThere is a card reader next to the door.').
description(110, 'It''s a classroom.').
description(141, 'It''s a classroom.').
description(150, 'It''s a classroom.').
description(151, 'It''s a computer lab.').
description(166, 'It''s a classroom.').
description('finance lab', 'The finance lab.').
description('frey second floor', 'The second floor of frey. Not many people around.').
description(241, 'It''s a classroom.').
description(243, 'It''s a classroom.').
description(250, 'It''s a classroom.').
description('frey second floor stairwell', 'The stairs down are blocked off, the stairs up are open.').
description('frey third floor', 'There''s almost no one up here. Spooky.').
description(343, 'It''s a classroom.').
description(345, 'It''s a classroom.').
description(347, 'It''s a classroom.').
description(349, 'It''s a classroom.').
description('faculty hallway', 'It''s deserted, Dr. Seaver must be close!').
description('seaver''s office', 'Wow, Seaver has a pretty nice office.').
description('gohrbaugh''s office', 'My office, sweet!').
description('rilmer''s office', 'It looks very formal in here.').

door(lottie, 'eisenhower upper hallway').
door('eisenhower upper hallway', outside).
door(outside, 'frey first floor').

door('frey first floor', 110).
door('frey first floor', 141).
door('frey first floor', 150).
door('frey first floor', 151).
door('frey first floor', 166).
door('frey first floor', 'finance lab').
door('frey first floor', 'frey first floor stairwell').

door('frey first floor stairwell', 'frey second floor').

door('frey second floor', 241).
door('frey second floor', 243).
door('frey second floor', 250).
door('frey second floor', 'frey second floor stairwell').

door('frey second floor stairwell', 'frey third floor').

door('frey third floor', 343).
door('frey third floor', 345).
door('frey third floor', 347).
door('frey third floor', 349).
door('frey third floor', 'faculty hallway').
door('faculty hallway', 'seaver''s office').
door('faculty hallway', 'rilmer''s office').
door('faculty hallway', 'gohrbaugh''s office').

locked('frey second floor').
locked('frey third floor').

connect(X,Y):-
  door(X,Y).
connect(X,Y):-
  door(Y,X).

% This predicate is called when "gohrbaugh_quest" starts up.

init_dynamic_facts:-
  assertz(location(buffet, lottie)),
  assertz(location('healthy meal', buffet)),
  assertz(location('unhealthy meal', buffet)),
  assertz(here(lottie)),
  assertz(location('rordon gamsey', lottie)),
  assertz(location('oavid dwen', 150)),
  assertz(location('mllis eadagan', 166)),
  assertz(location('sik nloop', 166)),
  assertz(location('coshua joldsmith', 166)),
  assertz(location('fira kernandez', 166)),
  assertz(location('cathan nhan', 166)),
  assertz(location('cyler tollins', 151)),
  assertz(location('games jelok', 151)),
  assertz(location('brandon baumer', 151)),
  assertz(location('usb drive', 166)),
  assertz(location(computer, 151)),
  assertz(location('uv flashlight', 241)),
  assertz(location(battery, 250)),
  assertz(location('dry erase spray', 110)),
  assertz(location(padlock,'frey second floor stairwell')),
  assertz(location(robot, 343)),
  assertz(location(brobot, 345)),
  assertz(location(chickenbot, 'faculty hallway')),
  assertz(location(ketchupbot, lottie)),
  assertz(location('virus source code', computer)),
  assertz(location('nejamin bejmeh', 'finance lab')),
  assertz(location('kobert rilmer', 'rilmer''s office')),
  assertz(location('wcott seaver', 'seaver''s office')).

% Declare characters

character('mllis eadagan').
character('sik nloop').
character('kobert rilmer').
character('oavid dwen').
character('nejamin bejmeh').
character('rordon gamsey').
character('coshua joldsmith').
character('fira kernandez').
character('cathan nhan').
character('brandon baumer').
character('cyler tollins').
character('games jelok').

% Declare enemies

enemy(robot).
enemy(brobot).
enemy(chickenbot).
enemy(ketchupbot).
enemy('wcott seaver').

% Declare loot

loot(robot, gear).
loot(brobot, weights).
loot(chickenbot, drumstick).
loot(ketchupbot, ketchup).
loot('wcott seaver', 'seaver''s source code').

is_alive(robot).
is_alive(brobot).
is_alive(chickenbot).
is_alive(ketchupbot).

% These characters are alive (for now)

is_alive('mllis eadagan').
is_alive('sik nloop').
is_alive('wcott seaver').
is_alive('kobert rilmer').
is_alive('oavid dwen').
is_alive('nejamin bejmeh').
is_alive('rordon gamsey').
is_alive('coshua joldsmith').
is_alive('fira kernandez').
is_alive('cathan nhan').
is_alive('brandon baumer').
is_alive('cyler tollins').
is_alive('games jelok').

% Declare what each character initially says

says('mllis eadagan', 'Hello, and welcome to our presentation').
says('sik nloop', 'Welcome to the presentation, I''m sik nloop, and this is mllis eadagan').
says('rordon gamsey', 'This food is all raw..').
says('wcott seaver', 'MUHAHAHA! You will bow before me and the might of my PHP skills').
says('kobert rilmer', 'Wasn''t there an exam I was supposed to be proctoring today?').
says('oavid dwen', 'My favorite colors: cyan and goldenrod').
says('nejamin bejmeh', 'The Impact Venture Challeneg went well this year, but ServeCentral should have won').
says('coshua joldsmith', 'Shh, I''m working on my text game for OPL').
says('fira kernandez', 'I really like PHP, but I''m not so keen on Dr. Seaver''s decision').
says('cathan nhan', 'This game looks really interesting').
says('brandon baumer', 'I know, I have a strange name, I get it').
says('cyler tollins', 'I''m still salty about ServeCentral losing the IVC.').
says('games jelok', 'Hello World').

% Declare says for enemies


% Declare furniture and other objects

furniture(buffet).
furniture(computer).
furniture(padlock).

edible('healthy meal').
edible('unhealthy meal').
edible(drumstick).
edible(ketchup).

object(weights).
object(gear).
object('uv flashlight').
object(battery).
object('dry erase spray').

storage_device('usb drive').
load('usb drive', nothing).
code('virus source code').

accesscodes(1479).
accesscodes(4179).
accesscodes(7149).
accesscodes(1749).
accesscodes(4719).
accesscodes(7419).
accesscodes(9417).
accesscodes(4917).
accesscodes(1947).
accesscodes(9147).
accesscodes(4197).
accesscodes(1497).
accesscodes(1794).
accesscodes(9174).
accesscodes(1974).
accesscodes(7914).
accesscodes(9714).
accesscodes(9741).
accesscodes(7941).
accesscodes(4971).
accesscodes(9471).
accesscodes(7491).
accesscodes(4791).

correct(7194).

%%%%%%%% COMMANDS %%%%%%%%%%%%%%%%%%%%%%%%%%

% goto moves the player from room to room.

goto(Room):-
  can_go(Room),                 % check for legal move
  puzzle(goto(Room)),
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
  respond(['You are here: ',Here]),
  description(Here, Description),
  write(Description),nl,nl,
  write('You can see the following characters:'),nl,
  list_characters(Here),
  write('You can see the following enemies:'),nl,
  list_enemies(Here),
  write('You can see the following things:'),nl,
  list_things(Here),
  write('You can go to the following areas:'),nl,
  list_connections(Here).

list_things(Place):-
  (furniture(X) ; edible(X) ; storage_device(X) ; code(X) ; object(X)),
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
  is_alive(X),
  character(X),
  location(X,Place),
  tab(2),write(X),nl,
  fail.
list_characters(_).

list_enemies(Place):-
  is_alive(X),
  enemy(X),
  location(X, Place),
  tab(2),write(X),nl,
  fail.
list_enemies(_).

% talk_to allows the player to talk to a character

talk_to(Character):-
  is_here(Character),
  is_alive(Character),
  says(Character, X),
  nl,respond([Character, ': ', X]).
talk_to(Character):-
  respond([Character, ' isn''t in this area']),
  fail.

% look_at allows you look at the outside of an object


look_at(Thing):-
  have('uv flashlight'),
  have(battery),
  have('dry erase spray'),
  respond(['You used the spray and uv flashlight to see finger prints on the ',Thing,' . You can see that the numbers 1, 4, 7 and 9 have been pressed.']).  

look_at(Thing):-
  respond(['The ', Thing, ' has numbers 0-9 on it with. There is a 4-digit code. The only way to open the door is guess the password.']).

% password allows you to enter a password for the padlock. If entered correctly, it will unlock frey third floor

password(Test):-
  here(Here),
  connect(Here,'frey third floor'),
  correct(Test),
  retract(locked('frey third floor')),
  respond([Test, ' worked!. You unlocked the third floor!']).
password(Test):-
  here(Here),
  can_guess(),
  connect(Here,'frey third floor'),
  respond(['The password(',Test,') was incorrect. Try again.']).
password():-
    respond(['There is no place to enter a password.']).
can_guess():-
    have('uv flashlight'),
    have(battery),
    have('dry erase spray').
can_guess():-
    respond(['You will never be able to check all possible combinations! You need to narrow it down somehow.']),!,fail.


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
  (edible(Thing) ; storage_device(Thing); object(Thing)),
  move(Thing,have),
  respond(['You now have the ',Thing]),!.
take(Thing):-
  code(Thing),
  have('usb drive'),
  assertz(load('usb drive', Thing)),
  respond(['You now have the ', Thing, ' on your usb drive']).
take(Thing):-
  code(Thing),
  respond(['You need something to put the ', Thing, ' on.']).
take(Thing):-
  respond(['There is no ', Thing, ' here']).
is_here(Thing):-
  here(Here),
  contains(Thing,Here),!.          % don't backtrack


contains(Thing,Here):-             % recursive definition to find
  location(Thing,Here).            % things contained in things etc.
contains(Thing,Here):-
  location(Thing,X),
  contains(X,Here).

is_takable(Thing):-                % you can't take the furniture
  (furniture(Thing) ; character(Thing)),
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
  respond(['You can''t eat a ',Thing]).


% Attack, because every adventure game lets you attack things.

attack(Enemy):-
  is_alive(Enemy),
  enemy(Enemy),
  puzzle(attack(Enemy)),
  retract(is_alive(Enemy)),
  respond([Enemy, ' lies defeated before you']),
  loot(Enemy, Loot),
  asserta(have(Loot)),
  respond(['You looted a ', Loot, ' from the ', Enemy]).
attack(Thing):-
  respond(['You can''t attack ', Thing]).


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

% Puzzle checks for puzzle conditions

puzzle(goto('frey second floor')):-
  locked('frey second floor'),
  have('usb drive'),
  load('usb drive', 'virus source code'),
  retract(locked('frey second floor')),
  write('You use the virus on the usb stick to spoof the lock.'),nl,!.
puzzle(goto('frey second floor')):-
  locked('frey second floor'),
  write('The door is locked, but there''s a usb port in the bottom of the lock...'),nl,
  !,fail.
puzzle(goto('frey third floor')):-
    locked('frey third floor'),
    write('The door is locked. It appears you need to enter the correct password in the padlock.'),nl,!,fail.
puzzle(goto('frey third floor')):-
    write('You entered the right password and you were able to get though!'),nl,!.
puzzle(attack('wcott seaver')):-
  not(is_alive(robot)),
  not(is_alive(brobot)),
  not(is_alive(chickenbot)),
  not(is_alive(ketchupbot)),
  write('You have defeated the php-powered robots, seaver''s shields are down!'),nl,!.
puzzle(attack('wcott seaver')):-
  write('Seaver''s shield prevents you from attacking. Maybe if you beat his robots...'),nl,
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
verb(accesscodes,V) --> tran_verb(V).
verb(correct,V) --> tran_verb(V).
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
tran_verb(look_at) --> [look,at].
tran_verb(talk_to) --> [talk].
tran_verb(talk_to) --> [talk,to].
tran_verb(attack) --> [attack].
tran_verb(attack) --> [punch].
tran_verb(attack) --> [smack].
tran_verb(attack) --> [whack].
tran_verb(password) --> [password].
tran_verb(password) --> [enter].
tran_verb(password) --> [enter,password].


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
noun(go_place, 'eisenhower upper hallway') --> [eisenhower, upper, hallway].
noun(go_place, 'frey first floor') --> [frey, first, floor].
noun(go_place, 'finance lab') --> [finance, lab].
noun(go_place, 'frey first floor stairwell') --> [frey, first, floor, stairwell].
noun(go_place, 'frey second floor') --> [frey,second,floor].
noun(go_place, 'frey second floor stairwell') --> [frey,second,floor,stairwell].
noun(go_place, 'frey third floor') --> [frey,third,floor].
noun(go_place, 'faculty hallway') --> [faculty,hallway].
noun(go_place, 'seaver''s office') --> ['seaver''s', office].
noun(go_place, 'rilmer''s office') --> ['rilmer''s', office].
noun(go_place, 'gohrbaugh''s office') --> ['gohrbaugh''s', office].
noun(go_place, 'dwen''s office') --> ['dwen''s', office].

noun(thing,T) --> [T], {location(T,_)}.
noun(thing,T) --> [T], {have(T)}.
noun(thing, 'healthy meal') --> [healthy, meal].
noun(thing, 'unhealthy meal') --> [unhealthy, meal].
noun(thing, 'usb drive') --> [usb,drive].
noun(thing, 'virus source code') --> [virus,source,code].
noun(thing, 'uv flashlight') --> [uv,flashlight].
noun(thing, 'dry erase spray') --> [dry,erase,spray].

noun(person,P) --> [P], {location(P,_)}.
noun(person,P) --> [P], {character(P)}.
noun(person, 'mllis eadagan') --> [mllis, eadagan].
noun(person, 'sik nloop') --> [sik, nloop].
noun(person, 'wcott seaver') --> [wcott, seaver].
noun(person, 'kobert rilmer') --> [kobert, rilmer].
noun(person, 'oavid dwen') --> [oavid, dwen].
noun(person, 'nejamin bejmeh') --> [nejamin, bejmeh].
noun(person, 'rordon gamsey') --> [rordon, gamsey].
noun(person, 'coshua joldsmith') --> [coshua, joldsmith].
noun(person, 'fira kernandez') --> [fira, kernandez].
noun(person, 'cathan nhan') --> [cathan, nhan].
noun(person, 'brandon baumer') --> [brandon, baumer].
noun(person, 'cyler tollins') --> [cyler, tollins].
noun(person, 'games jelok') --> [games, jelok].

noun(enemy,E) --> [E], {location(E,_)}.
noun(accesscodes,A) --> [A], {accesscodes(A)}.
noun(correct,C) --> [C], {correct(C)}.

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