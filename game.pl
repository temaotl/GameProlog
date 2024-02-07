%%%% dynamic %%%%

:- dynamic currentLocation/1, have/1, room_object/2 , 
            inventory/1, decor/2, health/1, enemy_location/2,visited_location/1,fin/1.
%%%%         %%%%%

%%%% rooms %%%%

%% first faze %% 
room(street).
room(hall).
room(bedroom).
room(bar).
room(port).
%second
room(orbital).
room(barracks).
room(shuttle).
%% thrith 
room(entrance).
room(headquarter).
room(factory).
room(core).
room(obelisk).
room(elevator).
%% four
room(wasteland).
room(forest).
room(pole).
room("power house").
room(tavern).
room(orzi).
room(bris).
room(citadel).

%%%%       %%%%
%%%%% teleporter location %%%%
teleporter_location(wasteland).
teleporter_location(forest).
teleporter_location(pole).
teleporter_location("power house").
teleporter_location(tavern).
teleporter_location(orzi).
teleporter_location(bris).
%%%%%                     %%%%

%%%% location decor  %%%%
decor(bed,bedroom).
decor(freezer,hall).
decor('lawn with uncut grass',street).
decor('robot barmen',bar).
decor('table',bar).
decor('star-ship',port).
decor('terminal',orbital).
decor('cafe',orbital).
decor('space map',shuttle).
decor('armor stand',barracks).
decor('officer  room',barracks).
decor('monitor',obelisk).
decor('glowing road',entrance).
decor('alarm sign',entrance).
decor('millitary alarm message',headquarter).
decor('stange body',headquarter).
decor('ToKaMak',core).
decor('asteroid',factory).
decor('asteroid mashine',factory).
decor('strange symbols in unknown language',obelisk).
decor('music player',elevator).
decor('rocks',wasteland).
decor('black and red stone',citadel).
decor('gardens',control).
%%%%      %%%%

room_object(bedroom,[kpk,bandages]).
room_object(hall,[jacket,pants,"house key"]).
room_object(bar,[ticket]). %%% first key

room_object(orbital,[bandages,agenda]).
room_object(barracks,[rifle]).

room_object(entrance,["civil id"]).
room_object(headquarter,[rifle,flamethrower]).
room_object(core,["capitan id"]).
room_object(factory,["engineer id"]).


room_object(wasteland,[amulet]).
room_object(bris,[cat]).
room_object("power house",[lance]).
room_object(tavern,[teleporter]).
room_object(orzi,[flamethrower]).
room_object(citadel,["destructive core"]).

%door
%1 act%
door(bedroom,hall).

door(hall,bedroom).
door(hall,street).

door(street,hall).
door(street,bar).

door(bar,street).
door(bar,port).

door(port,bar).
door(port,street).
door(port,orbital).

 %2 act%
door(orbital,barracks).
door(orbital,shuttle).

door(barracks,orbital).

door(shuttle,entrance).
% 3 act
door(entrance,headquarter).
door(entrance,elevator).

door(headquarter,elevator).
door(headquarter,entrance).

door(elevator,entrance).
door(elevator,headquarter).
door(elevator,obelisk).
door(elevator,core).
door(elevator,factory).

door(obelisk,elevator).
door(core,elevator).
door(factory,elevator).
door(obelisk,wasteland).

%%%  wastland %%%
way(wasteland,[location(east,forest),location(west,pole)]).
way(forest,[location(north,"power house"),location(south,tavern),location(west,wasteland)]).
way(pole,[location(north,orzi),location(south,bris),location(east,wasteland)]).
way(orzi,[location(south,pole),location(west,citadel),location(east,"power house")]).
way(bris,[location(north,pole),location(west,citadel),location(east,tavern)]).
way("power house",[location(south,pole),location(west,orzi)]).
way(tavern,[location(north,forest)]).

%%% require %%%
require(street,["house key",jacket,pants]).
require(orbital,[ticket]).
require(shuttle,[agenda]).
require(headquarter,["civil id"]).
require(factory,["civil id"]).
require(core,["engineer id"]).
require(obelisk,["capitan id"]).
require(forest,[amulet]).
require("power house",[cat]).

%%%         %%%

%%%   enemy and fight locations      %%%%
enemy(worker,[rifle]).
enemy(woof,[flamethrower]).
enemy("shadow of evil",[lance]).
enemy_location(obelisk,[worker,woof]).
enemy_location(citadel,["shadow of evil",woof]).

%%%  started value   %%%

    currentLocation(bedroom).
    health(3).
    inventory([]).
    visited_location([bedroom]).
%%%                 %%%%
%%%%%% script   %%%%%%
 script(bedroom):-
  nl,
  write('I woke up in a cold sweat,around me was only my old house, 
     however, this dream I see it again and again, 
     today I received an advantageous offer, a meeting is scheduled in a bar.
     I need to go to the bar.
    '),
 !.
 script(bar):-
  nl,
  write(  'After two shots of vodka, things went well, I found out that they needed an engineer for some kind of expedition, 
          however, the project is not official, so the details will only be at the orbital station, 
          I have to go to the port and find the ship'
    ),!.

 script(orbital):-
  nl,
  write(  'Upon arrival at the station.I was instructed about the goals and objectives of our mission,our task is  save the kelev-class battleship. 
          I was also advised to go to the barracks and take ammunition
          then arrive to the shuttles
          '
    ),!.
 script(shuttle):-
  nl,
  write(  'Compain begin'),!.
 script(entrance):-
  nl,
  write(  'when we boarded to this ship, 
    the whole crew got goosebumps, 
    we saw a terrible picture of several people who had gone crazy. 
    Our shuttle flew away and there was no turning back'
    ),!.
 script(headquarter):-
  nl,
  write(  'Ready to meet something, 
    the room shone with guns, 
    but there were no defense systems or peoplee'
    ),!.
 script(factory):-
  nl,
  write(  'the gigantic production halls had already cooled down before our flight'),!.
 script(core):-
  nl,
  write(  'the capital lay in a strange position, 
    I would say rolling over 
   Talk to him was a futile exercise'),!.
 script(elevator):-
    nl,
    write(  'entering the elevator, 
      I felt a strange energy emanating from the room signed as a obelisk, 
      I need to go there'),!.
 script(obelisk):-
  nl,
  write(  'a strange object emitting an incredible signal, 
    all my prebors failed and in my head I heard the phrase wuf and touch me'),!.
 script(wasteland):-
    nl,
    write(  'I found myself in an incomprehensible desert in which there was only an old amulet and there was no signal, 
          I would have to navigate the area'),
    nl,
    nl,
    write('my path lies in the citadel, however, from the locals I heard
    something evil is guarding the citadel, I need a weapon that paver has'),
    nl,
    write(  'starting from here there is no map use directions(do not use go command now write directions ) of travel such as north south west east'),
          !.

 script("power house"):-
  nl,
  write(  'on the threshold of this house, a young girl was waiting for me, 
      immediately declaring that her iq is 1000 
      and she is a local president'),!.
  script(citadel):-
  nl,
  write(  'I entered the citadel there the core of all this evil was waiting for me, 
          all that was left was to take it'),!.
%%%%%%           %%%%%%


%%%%% description %%%%%
  description(street):-
    nl,
    write('a small old house, it can be seen that it has been battered by time'),
    nl.
  description(hall):-
    nl,
    write('large dusty room filled with empty cans'),
    nl.
  description(bedroom):-
    nl,
    write('a cramped shabby room with a single window,'),
    nl.
  description(bar):-
    nl,
    write('bar bathed in purple light'),
    nl.
  description(port):-
    nl,
    write('old shabby spaceport'),
    nl.
  description(orbital):-
      nl,
      write('massive orbital station'),
      nl.
  description(barracks):-
      nl,
      write('army barracks'),
      nl.
  description(shuttle):-
      nl,
      write('old but still working machine'),
      nl.
  description(entrance):-
      nl,
      write('hallway with red lights'),
      nl.
  description(headquarter):-
      nl,
      write('ship decision center'),
      nl.
  description(factory):-
      nl,
      write('giant room full of machines and asteroids ready to be recycled'),
      nl.
  description(core):-
      nl,
      write('massive power source powering the entire ship'),
      nl.
  description(obelisk):-
      nl,
      write('strange object emitting energy'),
      nl.
  description(elevator):-
      nl,
      write('ordinary elevator'),
      nl.
    description(wasteland):-
      nl,
      write('what is this'),
      nl.
    description(citadel):-
      nl,
      write('the biggest citadel in this strange place'),
      nl.
    description(control):-
      nl,
      write('incredibly creepy and beautiful place'),
      nl.
    description(forest):-
      nl,
      write('beautiful coniferous forest'),
      nl.
    description(pole):-
      nl,
      write('giant field sown with wheat'),
      nl.
    description("power house"):-
      nl,
      write('house inhabited by the local president'),
      nl.
    description(tavern):-
      nl,
      write('a pleasant aroma wafted from the local tavern'),
      nl.
    description(orzi):-
      nl,
      write('there was a closed settlement under the mountain'),
      nl.
    description(bris):-
      nl,
      write('oldest forest in the area'),
      nl.
  
  %%%%%     %%%%%%


%%%%%  start game %%%%
 start :-
    write('This game was created by Artem Otliaguzov for Bi-PPA'),nl,
    nl,
    nl,
    write('the future, unlike the dreams of mankind, turned out to be not so happy,'),nl,
    write('you are a 30-year-old engineer who turned out to be on the outskirts of history,'),nl,
    write('your money is coming to an end and your knowledge leaves much to be desired'),nl,
    write('Your mission is to find the your place in this world.'),nl,
    nl,
    write('You control the game by using commands'),nl,
    write('write help. to all command'),nl,
    nl,
    write('The hunt has begun.'),nl,
    write('Hit any key to continue.'),get0(_),nl,

    script(bedroom),
    nl,
    look,          
    command_loop.

%%%         %%%%  
 connect(X,Y) :- door(X,Y).
%%%%% look part %%%%%
 look:-
  currentLocation(Place),
  door(Place,_),
  look_loc,!.
 look:-
  look_way,!.

 look_way :-
  currentLocation(_curr),
  description(_curr),
  write('You are in the '), write(_curr), nl,
  write('You can take:'), nl,
  list_things(_curr),
  way(_curr,_loc),
  write("You can go"),nl,
  compas(_loc),
  !.

 compas([H|T]):-
  compas_help(H),
  compas(T),!.
 compas([]):-!.

 compas_help(location(X,Y)):-
  write(X),write(" -> "),write(Y),nl,!.

 look_loc :-
  currentLocation(Place),
  description(Place),
  write('You are in the '), write(Place), nl,
  write('You can see in  location'), nl,
  list_decor(Place),
  write('You can take:'), nl,
  list_things(Place),
  write('You can go to:'), nl,
  list_connections(Place).

 list_things(Place) :-
  room_object(Place,X),
  tab(2),
  write(X),
  nl,
  fail.
 list_things(_).
 list_decor(Place):-
  decor(X,Place),
  tab(2),
  write(X),
  nl,
  !.

 list_connections(Place) :-
  connect(Place, X),
  tab(2),
  write(X),
  nl,
  fail.
 list_connections(_).
%%%%%         %%%%%
%%%%  inventory    %%%%
 beu_inv:-
  write('***********    inventory     ************'),nl,!.

 inventory:-
  inventory([]),
  beu_inv,
  write('empty inventory'),
  !.

 inventory:-
  inventory(Collected),
  beu_inv,
  printListInventory(Collected),
  !.

 printListInventory([]):-!.

 printListInventory([Head|Tail]):-
     tab(2),
     write(Head),
     printListInventory(Tail).

 usr_health:-
  health(_h),
  nl,
  write('your HP is:'),write(_h),nl,!.
%%%%               %%%%

%%%% pick  and put %%%%
  pick(_obj):-
    currentLocation(_curr),
    room_object(_curr,_objects),
    member(_obj,_objects),
    inventory(_inv),
    pick_help(_obj,_inv),
    !.

 pick(_):-
    write_ln("is not here, maybe try to look"),
    fail,!.

 pick_help(_obj,_inv):-
  inventory(_inv),
  memberchk(_obj, _inv),
  write("You alrady have this"),
  !.

 pick_help(_obj,_inv):-
  currentLocation(_curr),
  room_object(_curr,_objects),
  delete(_objects,_obj,_rest),
  retract(room_object(_curr,_objects)),
  asserta(room_object(_curr,_rest)),
  pick_teleporter(_obj),
  pick_bandages(_obj,_inv),
  !.

 pick_bandages(_obj,_inv):-
  not(_obj=bandages),
  append([_obj],_inv,_newInv),
  retract(inventory(_inv)),
  assert(inventory(_newInv)),
  nl,
  write('**** picked  ****'),nl,
  write_ln(_obj),
  !.

 pick_bandages(_,_):-
  health(_usrH),
  _val is _usrH +1,
  retract(health(_usrH)),
  asserta(health(_val)),
  nl,
  write('you took the bandages. Health increased by 1'),
  !.

 pick_teleporter(_obj):-
  _obj=teleporter,
  nl,
  write('hello traveler, I am a teleporter, 
    having found me, you received a unique ability to teleport to any location in our glorious world.
    tell me the teleporter and where you want to go 
    and I\'ll drop you off but keep in mind that 
    I can\'t teleport you to the inner citadel'),nl,
    !.
  pick_teleporter(_):-!.

 unify(X, L, R):-
  append(X1, Common, X),
    append(Common, L1, L),!,
  append([X1, Common, L1], R).

 pick_all:-
  inventory(_inv),
  currentLocation(_curr),
  room_object(_curr,_objects),
  unify(_inv,_objects,_res),
  retract(room_object(_curr,_objects)),
  asserta(room_object(_curr,[])),
  retract(inventory(_inv)),
  assert(inventory(_res)),
  !.

 put(_obj):-
  currentLocation(_curr),
  inventory(_inv),
  memberchk(_obj, _inv),
  delete(_inv,_obj,_newInv),
  room_object(_curr,_objects),
  append([_obj],_objects,_newObj),
  retract(room_object(_curr,_objects)),
  asserta(room_object(_curr,_newObj)),
  retract(inventory(_inv)),
  asserta(inventory(_newInv)),
  write('**** droped  ****'),nl,
  write_ln(_obj),
  !.
 put(_):-
 write("i dont have this "),
 nl,
 fail,!.
%%%%%%%%%                   %%%%%%%
%%%%%%%%%  travel part %%%%%%%%%

 use_teleporter(_dest):-
  teleporter_location(_dest),
  currentLocation(_curr),
  inventory(_inv),
  memberchk(teleporter, _inv),
  delete(_inv,teleporter,_newInv),
  room_object(_curr,_objects),
  append([teleporter],_objects,_newObj),
  retract(room_object(_curr,_objects)),
  asserta(room_object(_curr,_newObj)),
  retract(inventory(_inv)),
  asserta(inventory(_newInv)),
  nl,
  write('Thank you for using my services, traveler'),
  nl,
  move(_dest),!.
 use_teleporter(_):-
  nl,
  write('what are you trying to do traveler'),nl,!.

 north:- goway(north).
 south:-goway(south).
 east:-goway(east).
 west:- goway(west).

 goway(Direction):-
  currentLocation(_curr),
  way(_curr,_directions),
  member(location(Direction,_destination),_directions),
  have_req(_destination),
  must_fight(_destination),
  not(be_visited(_destination)),
  retract(currentLocation(_curr)),
  assert(currentLocation(_destination)),
  nl,
  look,
  !.
 goway(_):-
  nl,write('no way'),nl,!.

 goto(Place):-
  can_go(Place),
  have_req(Place),
  must_fight(Place),
  move(Place),
  not(be_visited(Place)),
  nl,
  look,
 !.
 goto(_):-!.

 be_visited(Place):-
  visited_location(_vis),
  memberchk(Place,_vis),
  fail,!.
 be_visited(Place):-
  nl,
  write('*******************************************'),nl,
  script(Place),
  nl, write('*******************************************'),nl,
  visited_location(_vis),
  append([Place],_vis,_newVis),
  retract(visited_location(_vis)),
  assert(visited_location(_newVis)),
  fail,!.

 must_fight(Place):-
  enemy_location(Place,_enem),
  health(_bef),
  fight_all(Place,_enem),
  health(_aft),
  !,
  (_bef=_aft),
  !.
 must_fight(_):-!.

 fight_all(Place,[H|T]):-
  fight(Place,H),
  fight_all(Place,T).
 fight_all(_,_):-!.


 can_go(Place):-
  currentLocation(_curr),
  connect(_curr, Place),!.

 can_go(Place):-
  write('i cant get to the '),
  write(Place),
  write(' from here'),
  nl,
  fail.

 move(Place) :- move(Place,_).
 move(Place,X):-
  retract(currentLocation(X)),
  asserta(currentLocation(Place)).

 have_req(Place):-
  require(Place,_req),
 not( have_req_help(Place,_req)),
 write('hmm.. i dont have items to doing this '), nl,
 write('i need:'),write(_req),nl,
 !,fail.
 have_req(_):-!.
 have_req_help(_,Req):-
    inventory(_inv),
    subset(Req, _inv),
    !.
%%%%%%%%%              %%%%%%%%%

%%%%%%%%%  fight part   %%%%%%%%%
 mem(X,[X|_]):-!.
 mem(X,[_|T]):- mem(X,T).

 have_weapon([],[]):-fail,!.
 have_weapon([H|_],_inv):-mem(H,_inv),!.
 have_weapon([_|T],_inv):-have_weapon(T,_inv).

 fight(Place,_name):-
    enemy(_name,_char),
    inventory(_inv),
    nl,
    write("You fight against :"),write(_name),
    have_weapon(_inv,_char),
    enemy_location(Place,_arr),
    delete(_arr,_name,_newArr),
    retract(enemy_location(Place,_arr)),
    asserta(enemy_location(Place,_newArr)),
    nl,
    write("result: You win this fight"),nl,
    !.
 fight(_,_):-
  nl,
  write("result: You lose fight"),nl,
  nl,
  health(_usrH),
  _val is _usrH -1,
  retract(health(_usrH)),
  asserta(health(_val)),
  fight_help(_val),
  !.
 fight_help(_val):-
  not(_val = 0),!.
 fight_help(_):-
  nl,died,!,fail.
  
%%%%%%%%%               %%%%%%%%%

%%%%%%%% end of game %%%%%%%%
 quit:-
  write("come back at least sometimes engineer"),!.
 died:-
  write("You died"),!.

 wining_trig:-
    inventory(_inv),
    memberchk("destructive core",_inv),
    nl,
    write('after taking this cursed core, 
      I returned to our world, established 
    a connection with the ship and returned home, however, 
  this strange world reminds me of myself'),
    nl,
    write('You win'),!.
%%%%%%%%             %%%%%%%%

%%%% help to user %%%
help_usr:-
  write(
    'write look -> for background information, use the command '
    ),nl,
    write(
      '->go [Location name]-> use to change location. example: go hall  '
      ),nl,
      write(
        '->if you go to a location with enemies, a battle will begin if you 
        dont have the right weapon, the enemy will defeat you.'
        ),nl,
      write(
        ' inventory -> view the inventory '
        ),nl,
        write(
          '->take [object name]-> use to take object. example: take kpk '
          ),nl,
        write(
            '->put [object name]-> use to put object. example: put kpk '
            ),nl,
        write(
          'weapon -> 
          to view the list of enemies and what weapons they can use to defeat them'
          ),nl,
          write(
            '-> from a certain moment you can only move in directions, 
                the game will write to you about this, be careful,
                instead of a command GO, use directions such as WEST EAST SOUTH NORTH'
            ),nl,!.
weapon:-
  write('woof -> flamethrower'),nl,
  write('worker -> rifle'),nl,
  write('shadow of evil -> lance'),nl,!.

%%%%%%%%%  English command part %%%%%

command_loop:-
  repeat,
  get_sentence(_c),
  do(_c),
  health(_val), 
  (_c==quit -> !;
   _val<1 -> !;
   wining_trig ->!
    ),!.

  end:-
    fin(yes).
get_sentence(_c):-
  nl,
  readln(_s),
  delete(_s,.,L),
  command(_parsedList,L,[]),
  _c=.._parsedList,
  !.
get_sentence(_):-
  write_ln('i dont understand, '),fail.

do(look):-look,!.
do(inventory):-inventory,!.
do(pick_all):-pick_all,!.
do(quit):-quit,!.
do(pick(X)):- pick(X),!.
do(teleporter(X)):- use_teleporter(X),!.
do(put(X)):-  put(X),!.
do(goto(X)):- goto(X).
do(north):- goway(north).
do(south):- goway(south).
do(east):- goway(east).
do(west):- goway(west).
do(usr_health):-usr_health,!.

do(help_usr):- help_usr,!.
do(weapon):- weapon,!.

command([Pred,Arg]) --> verb(Type,Pred),nounphrase(Type,Arg).
command([Pred]) --> verb(intran,Pred).
command([goto,Arg]) --> noun(go_place,Arg).

verb(go_place,goto) --> go_verb.
verb(thing,V) --> objected_verb(V).
verb(intran,V) --> solo_verb(V).

go_verb --> [go].
go_verb --> [go,to].
go_verb --> [g].


solo_verb(north) --> [north].
solo_verb(north) --> [n].
solo_verb(south) --> [south].
solo_verb(south) --> [s].

solo_verb(east) --> [east].
solo_verb(east) --> [e].
solo_verb(west) --> [west].
solo_verb(west) --> [w].

solo_verb(inventory) --> [inventory].
solo_verb(usr_health) --> [health].
solo_verb(usr_health) --> [hp].
solo_verb(inventory) --> [inv].
solo_verb(inventory) --> [i].
solo_verb(look) --> [look].
solo_verb(look) --> [look,around].
solo_verb(look) --> [l].
solo_verb(pick_all) --> [pickall].
solo_verb(quit) --> [quit].
solo_verb(quit) --> [exit].
solo_verb(quit) --> [end].
solo_verb(quit) --> [bye].
solo_verb(help_usr) --> [help].
solo_verb(help_usr) --> [h].
solo_verb(weapon) --> [weapon].

objected_verb(pick) --> [take].
objected_verb(teleporter) --> [teleporter].
objected_verb(teleport) --> [teleport].
objected_verb(pick) --> [pick,up].
objected_verb(pick) --> [pick].
objected_verb(put) --> [put].
objected_verb(put) --> [drop].


nounphrase(Type,Noun) --> det,noun(Type,Noun).
nounphrase(Type,Noun) --> noun(Type,Noun).

det --> [the].
det --> [a].

noun(go_place,R) --> [R],{room(R)}.
noun(thing,T) --> [T].
noun(thing,"house key") --> [house,key].
noun(thing,"civil id") --> [civil,id].
noun(thing,"engineer id") --> [engineer,id].
noun(thing,"capitan id") --> [capitan,id].
noun(thing,"destructive core") --> [destructive,core].

%%%%%%%%                       %%%%%%
