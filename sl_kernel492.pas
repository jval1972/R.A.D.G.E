// radge - [RA]ndom [D]oom [GE]nerator based on SLIGE 490
// 2017 - 2019 Jim Valavanis

unit sl_kernel492;

interface

uses
  sl_helpers,
  SysUtils;

(*

   SLIGE - a random-level generator for DOOM
           by David M. Chess
           chess@theogeny.com      http://www.davidchess.com/
                                   http://users.aol.com/dmchess/
           chess@us.ibm.com        http://www.research.ibm.com/people/c/chess/

           http://www.doomworld.com/slige/

   August, 2000

   This source code is made available to the world in general,
   with the following requests:

   - If you're doing a port and you find that something or other needs
     twiddling to get it to compile, let me know and I'll stick in some
     ifdefs.  The current code assumes that Microsoft C has a horrible
     rand() function and uses str(n)icmp() instead of str(n)casecmp(),
     whereas everyone else has an ok rand() and str(n)casecmp().  And
     make sure that you compile with tight structure-packing (/Zp1 or
     -fpack_struct, I think) or DOOM won't understand the files.  Also,
     if you want to port to a non-Intel-byte-order platform, you've got
     a bit of work, but I think it's mostly isolated to DumpLevel,
     CloseDump, dump_texture_lmp, and a few others.  Good luck!  *8)

   - The current version should compile without any special switches
     except structure-packing, under both MSVC++ and DJGPP, without
     any warnings (unless you use -O3 in DJGPP, in which case you'll
     get some warnings that don't matter).  It should also compile
     and work under Linux gcc, but I've never tried it myself; if you
     do, write and tell me about it!  It should also compile for the
     RISC OS with the usual C compiler, thanks to Justin Fletcher.
     Anyone wanna do a Mac port?  I'd be glad to give advice...

   - If you make various changes and improvements to it and release
     a modified version yourself:

     - Write me and tell me about it so I can be pleased,
     - Mention me and SLIGE in the docs somewhere, and
     - Please *don't* call your new program "SLIGE".  SLIGE is
       my program.  Call yours "BLIGE" or "EGGISLES" or "MUMFO"
       or something like that.

   - If you want to do lots of wonderful things to the program to
     make it better, and then send me the result to incorporate into
     my next version, I strongly suggest telling me your plans first.
     Otherwise you may hear nothing from me for weeks after sending me
     your source mods (I don't check my mail that often sometimes), and
     then get a disappointing "thanks, but I don't want to take the
     program that direction, I have other plans" sort of reply.  Small
     bug fixes, of course, are always welcome!  All source mods sent
     to me become the property of the world at large.

   Otherwise, enjoy the program, and let me know what you think of the code!

   DC

*)

const
  SOURCE_SERIAL = 492;
(*

   New stuff (since 485): -huge switch, fixed gate-to-gate-to-arena bug
     (not as elegantly as I could have!), RISCOS source mods from JF,
     -fstart -ffstart -fend -ffend switches, getenv() var for cfg filename,
     pillar gates, attempt at SPARC compatibility (tx to O. Kraus).


   Stuff to do:
     BUG: very rarely, a slige -dm level will still not have four DM starts.
       Try more rooms than just first and last, and/or use a smaller "width"
       to allow putting a DM start on a pickable object?
     fold in Rob Ellwood's ceiling-effect and split_linedef mods,
       when he has a version he likes enough
     simple DM suggestion from some guy: more star-shaped than usual,
       with a big weapon in the middle.
     Wolfenstein-style secret levels (and switch)
     armor-damage model is wrong: green armor only absorbs 1/3 of
       damage taken; only blue armor absorbs 1/2.  Fix!
     BUG: when trigger_box() calls point_sector(), it should check
       the "danger" return (rather than passing in nil)
     more lamps/lights when night and/or dim?
     very-dark secret-level flavor?
     level flavor like "always big floordeltas"
     implement the "favorite monster" style of secret level (and switch)
     simple multi-level "ramps" in patios, for variety?
     sometimes monsters can be so tightly packed into a small room
       that none can move until you kill one.  Detect/avoid somehow.
     eliminate "ladders", by having random_basic_link() just make sure that
       if there are stairs abs(floordelta)<=linkdepth?
     switches mounted on columns/placards, not just in walls
     nukage in downsecs of falling-core traps, sometimes.  Barrels, too!
     regularize lighting (less ad-hoc, more constants), have dark
       secret levels, perhaps darker style, perhaps dark rooms with
       bright otherstuff, etc.
     secret doors/closets at the ends of ambush closets
     configify lamps and barrels
     configify other objects and monsters and anything else that's left
     unhardcode health's amounts, weapons' damage, etc
     shouldn't "error" and "gateexitsign" just be property bits?
     fixed the bug that sometimes accidentally made the monster hiding
       on top of a pillar be down in the room; but it'd be cool to
       sometimes do this on purpose!
     trees can block patio doors; fix
     BUG: A raised teleporter right next to a step-up doorway can be
       inaccessible.  Fix!
     once in awhile a barrel in a secret, to discourage the "open and
       shoot without looking" trick.  heh heh.
     have use of lights (gridlights, lightstrips, kickplate lights, etc)
       be correlated per level or config or something, not random
     use CEILING+LIGHT in even more lighting effects (link to
       light_recesses, for instance, and maybe use in windows)
     do mancubus-special in MAP07 even if not last_mission
     do arach-special in MAP07 also (somehow)
     place_timely_something() and friends need an "is it a secret?"
       switch, then could be used in various secret places.
     Secret grid-pillars need something more significant on them!
     Biggest-monster mode in DOOM 1 gives lots and lots and lots and
       lots of spectres...  (if BIG is off, I suspect)  Problem?
     Still the occasional level with many too many rooms; heh!
     BUG: the embellishment of the outersec of a rising room
       can decide to triggerbox the key, even though the rising room
       has already boxed it.  Fix.  (Actually both boxes seem to work,
       even though they're exactly coextensive, but still...)
     BUG: have seen at least one extwindow that extended orthogonal to
       the direction it should have extended.  Very odd!
     BUG: still getting a monster stuck in a floor-lamp now and then?  how?
       also perhaps a monster stuck in a grid-room pillar (or was that two
       grid-room monsters stuck to each other?)  (Those may be fixed now.)
       Also once saw a Hell Knight stuck between a new-style "pillar" and
       a wall.  Looked like.
     BUG: Sometimes a room has two different gates, both in the midtile.
       Well, you know what I mean...  This causes trouble.  Fixed?
       Nope; still can happen with a gate to an arena.  Is that OK?
       Nope; it can result in unfinishable levels.  Fixed now, I think!
     More exit-to-secret-level secret kinds.
     More special things about secret levels.  A black-and-starry
       theme using custom patches?
     For co-op, put all the weapons that we assume the player has
       into the start room (with multiplayer-only bit set).  Conflicts
       with deathmatch stuff, so only if not -dm?  Or have -coop switch?
     It would be nice to not give up on a bath whenever a monster might
       get stuck in the edge.  Try *moving* the monsters, or the edges
       of the bath, instead?
     Make arenas more interesting.  powerups/armor?  More scenarii!  Harder!!
     dead-gardens flavor
     More interesting sorts of GATE_GOAL-locked links (like just
       a grated door).  Also have the gate be one-way if the locked
       link is passible in the far-to-near direction.
     More Instaforks!  (Really just pushes, not forks at all)
     twinned links need to have their extra-painted-door bits and
       nukage-link status and stuff correlated, eh?
     read switches from config file
     constructs (computers, at least) hanging from the ceiling?
     more rational facings in grid-rooms (sort of toward the door)
     have the new-pillar probability in the style, and have it sometimes
       be very very high?
     more open-link stuff: gratings, locks, secrets, monsters, etc, etc
     sometimes have open(ish) links block sound, to avoid excess monster
       accumulation?  (Doesn't seem to be a problem?)
     open lifts sometimes activate at the top also
     sometimes blaze autodoors / lifts
     switch-activated lifts of various kinds
     monsters/pickables in sidesectors of an open link
     maybe have a minimum-link-width thing (used only on non-door links,
       maybe?), usually zero, and sometimes (often when bigification is
       high) higher (like 128).  For a non-narrow level, but not hugeness
     put nukage-edge textures (bottom-aligned) around nukage cores (and
       normal nukage pools, eh?)  Need new attribute bit/thing
     put pillars with nukage-pourers (green gunk vents, red demon
       faces, various leaking pipes) in nukage pools
     use step-textures and/or wall textures for/instead of support
       textures in various other kickplate places (below doors?)
     put decorations up on the centers of some pillars (/constructs)
     I suspect it's possible for a sequence of new-style pillars to
       block off the center of the room (thus perhaps making it impossible
       to get a key).  Implement fix if so.
     make the sunrooms and nukage-city effects less correlated (if nukage
       was forced, have sky lower-prob, and vice-versa)
     Other patio things:
       There are often no plants.  That "128" is possibly too big?
       Store floor and wall textures in style? or even level?
       Patios should count toward the level's (the quest's?) room count, eh?
       Patios would really make more sense as a kind of microquest, rather
         than a mere embellishment
       Make sure "roof" height is also big enough to accomodate the
         link->height1, and for that matter the door's height (twice),
         if there's a door
     in homogenize mode, that hack that will always add one more of the
       room's monsters if there's room for *any* monster can produce
       very painful results (the Revenant Brothers, in particular).
       Fix it?  Just separate out the "is there room for monster M in
       the model" check into a routine to call in homgen mode.
     homogenize_monsters, should be per-level, not per-WAD, eh?  or should it?
     once had a really really steep stairway with a monster on it.  The
       stairway was so narrow I couldn't get around the monster, but so
       steep that I couldn't see it to shoot it!  avoid?  (I actually
       managed to sneak around it eventually, but it got in lots of bites.)
       probably just put no link-guards on steep stairs, eh?
     y-offsets pretty good now; put some support texture in when two
       coalignable textures come together at a base-ceiling boundary?
       that's about the only place that y-offset problems remain
     some cool light-effect around pillars (new and old styles)
     find all monster-width assumptions (tough!), generalize or whatever
     merge secret-closet and plaque-closet code sections (diffs: plaques
       are recessed, and are sometimes not openable at all)
     way-cool recessed windows style
     traditional locked nop-DOOR3 sometimes in entry room?
     sometimes put a rising room at the end of tag-switch quests, too?
     allow embellishing far sides of links (w/lightboxes etc?)
     sometimes vertical lightstrips on walls / in corners (won't happen
       in Wod theme, because no "LIGHT" walls anymore; OK?)
     more extensive outdoor areas; need a theory
     non-rectangular rooms (lots of work there!)
     same texture on both sides of a door?
     before giving up finding a linedef to put a link on, split
       all too-big linedefs around the current room
     if a lift can be walked off of, leave out the WR-lower linedef
       at the top end.  sometimes
     raised sniper-closets
     make level-exits look even more obvious in DOOM I somehow?
     sometimes put keys in ambush closets, other special places
     sometimes have the back wall of a closet be a secret door (hint?)
     have big monsters / weapons (only) in later episodes / maps?
     use of has_chainsaw/berserk on !(FLIES|SHOOTS) working OK?
     use has_chaingun information for something.  remove wimp-bonus?
       chainguns tend to be ammo-inefficient; recognize?
     silly to put in the linedef box around an object before checking if
       there's enough room for the secret closet, eh?  fixed?
     have plaque-doors sometimes be triggered also
     more sophisticated backpack counting (depending on the weapon(s)
       the player has)
     model the invis and radsuit better?
     a big boss monster in a big secret room that lowers down when you
       take pickable.  also in the boss' room is the key/switch you need.
       have to make sure player has enough ammo/health first.  details?
     put a single window-grating in the center, not two half-invisible
       ones on the sides (done?)
     don't watermark the first room of non-first levels in a PWAD?
     autodoors and lifts are sort of boring currently (only 24-deep rec's).
       make them more interesting?
     plain-sky ceilings don't work, because if a nearby room has a
       slightly higher non-sky ceiling, or worse is a steep stairway
       goes up out of the room, it looks really dumb.  think about fixes.
       perhaps always raise the ceiling to be at least as high as all
       ceilings of attached rooms?  that simple?
     the support_misaligns stuff looks awful.  think about this some more.
     skyclosets are still silly-looking if you can stand in the end and
       look back at the "building".  Either fix somehow (how?), or use
       point_froms to put the skylight only at the far end of the closet,
       even in deep closets, so you can't see out from the end.  Also,
       the lip of the outer sides of the skycloset walls isn't necessary;
       would look best, I think, with a lip on the near side, and the
       other sides just ending blap like they used to.
     sometimes use a light-wall texture on walls (especially flanking
       doors/links?)  But there aren't that many of them.
     have a "minwall" in config, to usually forbid those skinny little
       4-pel-wide walls and like that.  (done a different way, but we
       might want to think about something >8 as a config option)
     multiple snipers/ambushers in a single (wider) closet?
     OK to have population before embellishment?  Should prevent
       monsters getting stuck in the wall because of swelling, yes?
       also makes haa-model accesses during embellishment more
       logical (since probably one cleans out the wandering monsters
       first, then the embellished?)  probably need both pre-pop and
       post-pop embellishments eventually for something
     random-link still returns too wide sometimes; that whole routine
       needs to be re-thought and rationalized (fixed?)
     use real door-texture widths, not the current hack, to center faces
     niches: often symettrically around doors, with lights/monsters/items
       sometimes with (secret) doors, sometimes non-walkable (lights,
       airholes to the outside, portraits, etc)
     generalize lightstrips to include counters, plaques, computer
       displays, even those little closets with monsters and stuff.
     do basic stairs by first making the core and then splitting it up; this
       will help auto-alignment, and sort of rationalize things also
       (can use stairify(), with a little work).
     swelling a room moves out the apparent corners, and that interferes
       with later find_sector_rectangle() calls used during population.  Uh-oh!!
       need true distance-from-point-to-line code.  Now that we're
       embellishing after populating, this isn't as bad.
     figure out how alignment works on left sidedefs, for split_linedef()
       (current code OK?)
     style setting for closet height?  variation here is nice, though
     more interesting window-borders (not just random) and widths
       very narrow "windows" can of course have lower floors and
       higher heights, since you can't walk through 'em anyway
       (just like gratings).  Think about how to do in style / link.
       (Slits sort of provide that now.)
     window stuff in Link, not in Style, eh?
     have generate_room_outline sometimes extend the starting
       linedef with another colinear linedef at one or both
       ends, so you can come into a room on a *long* side
     in intersect(), properly check for overlap of colinear segments.
     use texture-size data more in alignment, door-choice, etc, etc
     need a copy_link() that does small perturbations to an
       existing link (like copy_style())
     restore the ability for twinned links to have different
       door types?  Say!  How about allowing the two links
       of a twinned link to be *completely different?  Well,
       they have to have the same total depth and floordelta,
       anyway.  Hmm...
     on the other hand, it'd be logical if the door-types at
       either end of a link were the same.  That means having
       it in the link, not the room's style.  Sensible?  On
       the third hand, they needn't always be the same, eh?
     allow different-size openings at either end of a link?
       (tapered stair-walls, e.g.)
     have special deliverers for weapons if c->weapons_are_special
       (and then sometimes set that TRUE)
     more global (per-level or per-config) restrictions on style;
       never moving jambs, always soundproof doors, doorceilings
       always/never copy room ceiling, etc, etc?  Also pillars
       and windows and gunkage.  (Extend "Nukage City" concept)
     if link.height1==0, use ceiling height of main room, eh?
     pick one (at least) 64-aligned 64x64 square, make it a sector,
       change the ceiling texture to some light, maybe raise or
       lower the ceiling a bit, and change the light level (up,
       usually), and perhaps make it blink.
     need more general object-copiers (for split_linedef etc)
     generalize the idea that a monster can provide a weapon?  (eventually,
       including in config-file etc)
     in basic philosophy, we should think more about making the individual
       room fancy.  as it is, the overall flavor is likely to always
       be a chain of simple rooms.  Which is OK, but limited.  Put
       another way, the monsters standing around in a room are dull;
       the interesting stuff are the monsters in traps, on ledges,
       behind gratings, etc.  Do lots of monster-placement during
       embellishment?
     or another way, it's too easy now, just running through rooms
       killing things.  make it harder!
     one big thing we're missing is to involve the outdoors more;
       multiple rooms overlooking the same courtyard, "bay windows"
       out over pools and gunkage and stuff, etc, etc, etc.

*)

const
  HUGE_NUMBER = 1000000;

  LEVEL_MAX_BARS = 30;
  LEVEL_MAX_CRUSHERS = 2;

function TLMPSIZE(rows, columns: integer): integer;

function b_stricmp(const x, y: string): boolean;

function SameStrings(const x, y: string): boolean;

const
  DOOM0_BIT = $01;
  DOOM1_BIT = $02;
  DOOM2_BIT = $04;
// This is "clean" doom, with no "GROSS" items
  DOOMC_BIT = $08;
// "Intrinsic"; i.e. no SLIGE-special textures
  DOOMI_BIT = $10;
// JVAL
  ALL_THEMES = $FFFFFFFF;

const
  SL_FLOOR = $01;
  CEILING = $02;
  DOOR = $04;
  ERROR_TEXTURE = $08;
  WALL = $10;
  SUPPORT = $20;
  NUKAGE = $40;
  JAMB = $80;
  RED = $100;
  BLUE = $200;
  YELLOW = $400;
  GRATING = $800;
  PLAQUE = $1000;
  HALF_PLAQUE = $2000;
  LIGHT = $4000;
  BIG = $8000;
  SWITCH = $10000;
  OUTDOOR = $20000;
  GATE = $40000;
  EXITSWITCH = $80000;
  STEP = $100000;
  LIFT_TEXTURE = $200000;
  VTILES = $400000;
// and so on and so on; 32 may well not be enough!

const
// Some thing-only bits; corresponding bits above are texture/flat-only
  MONSTER = $01;
  AMMO = $02;
  HEALTH = $04;
  WEAPON = $08;
  PICKABLE = $10;
  SHOOTS = $20;
  EXPLODES = $40;
  FLIES = $80;
  BOSS = $100;
  SPECIAL = $800;

type
  theme_p = ^theme_t;
  theme_t = record
    name: string[255];
    secret: boolean;
    next: theme_p;
  end;

var
  ptheme: theme_p;

type
  texture_p = ^texture_t;
  texture_t = record
    name: string[255];
    realname: string[255]; // the DOOM name, in case the name name is an alias
    gamemask: byte;
    compatible: LongWord;
    core: LongWord;
    props: LongWord;
    width: SmallInt;
    height: SmallInt;
    y_hint: SmallInt;
    y_bias: SmallInt;    // Y offset that a switch needs
    subtle: texture_p;
    switch_texture: texture_p;
    used: boolean;
    next: texture_p;
  end;

var
  ptexture: texture_p;

type
  flat_p = ^flat_t;
  flat_t = record
    name: string[255];
    gamemask: byte;
    compatible: LongWord;
    props: LongWord;
    used: boolean;
    next: flat_p;
  end;

var
  pflat: flat_p;

const
// Values for link.type
  BASIC_LINK = 1001;
  OPEN_LINK = 1002;
  GATE_LINK = 1003;

// Bits for link.bits
  LINK_NEAR_DOOR = $01;
  LINK_RECESS = $02;
  LINK_ALCOVE = $04;
  LINK_TWIN = $08;
  LINK_CORE = $10;
  LINK_LIFT = $20;
  LINK_STEPS = $40;
// LINK_WINDOW is used only if LINK_TWIN
  LINK_WINDOW = $80;
  LINK_MAX_CEILING = $100;
  LINK_TRIGGERED = $200;
  LINK_LAMPS = $400;
  LINK_BARS = $800;
  LINK_LEFT = $1000;
  LINK_LOCK_CORE = $2000;
  LINK_FAR_TWINS = $4000;
  LINK_DECROOM = $8000;
  LINK_FAR_DOOR = $10000;

  LINK_ANY_DOOR = LINK_NEAR_DOOR or LINK_FAR_DOOR;

const
  ID_PLAYER1 = $0001;
  ID_PLAYER2 = $0002;
  ID_PLAYER3 = $0003;
  ID_PLAYER4 = $0004;
  ID_DM      = $000b;
  ID_GATEOUT = $000e;
  ID_TROOPER = $0bbc;
  ID_SERGEANT = $0009;
  ID_IMP = $0bb9;
  ID_PINK = $0bba;
  ID_SPECTRE = $003a;
  ID_COMMANDO = $041;
  ID_NAZI = $054;
  ID_SKULL = $bbe;
  ID_HEAD = $bbd;
  ID_SKEL = $042;
  ID_PAIN = $047;
  ID_ARACH = $044;
  ID_HELL = $045;
  ID_BARON = $0bbb;
  ID_MANCUB = $0043;
  ID_ARCHIE = $0040;
  ID_CYBER = $10;
  ID_SPIDERBOSS = $07;
  ID_BRAIN = $58;

const
  ID_SHOTGUN = $7d1;
  ID_SSGUN = $052;
  ID_CHAINGUN = $7d2;
  ID_CHAINSAW = $7d5;
  ID_PLASMA = $7d4;
  ID_BFG = $7d6;
  ID_CLIP = $7d7;
  ID_SHELLS = $7d8;
  ID_BULBOX = $800;
  ID_SHELLBOX = $801;
  ID_CELL = $7ff;
  ID_CELLPACK = $11;
  ID_BACKPACK = $08;
  ID_LAUNCHER = $7d3;
  ID_ROCKET = $7da;
  ID_ROCKBOX = $7fe;

  ID_STIMPACK = $7DB;
  ID_MEDIKIT = $7dc;
  ID_POTION = $7de;
  ID_SOUL = $7dd;
  ID_BERSERK = $7e7;
  ID_INVIS = $7e8;
  ID_SUIT = $7e9;
  ID_MAP = $7ea;

  ID_HELMET = $7df;
  ID_BLUESUIT = $7e3;
  ID_GREENSUIT = $7e2;

  ID_BLUEKEY = $028;
  ID_REDKEY = $026;
  ID_YELLOWKEY = $027;
  ID_BLUECARD = $0005;
  ID_REDCARD = $00d;
  ID_YELLOWCARD = $006;

  ID_LAMP = $07ec;
  ID_ELEC = $030;
  ID_TLAMP2 = $055;
  ID_LAMP2 = $056;
  ID_TALLBLUE = $002c;
  ID_SHORTBLUE = $037;
  ID_TALLGREEN = $02d;
  ID_SHORTGREEN = $038;
  ID_TALLRED = $02e;
  ID_SHORTRED = $039;
  ID_CANDLE = $022;
  ID_CBRA = $023;
  ID_BARREL = $07f3;
  ID_FBARREL = $0046;
  ID_SMIT = $002f;
  ID_TREE1 = $002b;
  ID_TREE2 = $0036;

const
  WINDOW_NORMAL = 5001;
  WINDOW_JAMBS = 5002;
  WINDOW_SUPPORT = 5003;
  WINDOW_LIGHT = 5004;

const
  LIGHTBOX_NORMAL = 6001;
  LIGHTBOX_LIGHTED = 6002;
  LIGHTBOX_DARK = 6003;

type
// Teleport gates
  gate_p = ^gate_t;
  gate_t = record
    in_tag: SmallInt;
    out_tag: SmallInt;
    gate_lock: SmallInt;  // The linedef-type, if any, to open the gate
    is_entry: boolean;    // Does one enter the room by it the first time?
    next: gate_p;
  end;

var
  pgate: gate_p;

type
  linedef_pp = ^linedef_p;
  linedef_p = ^linedef_t;
  genus_p = ^genus_t;
  style_p = ^style_t;
  texture_cell_p = ^texture_cell_t;
  flat_cell_p = ^flat_cell_t;
  construct_p = ^construct_t;
  thing_p = ^thing_t;
  sector_p = ^sector_t;
  vertex_p = ^vertex_t;
  sidedef_p = ^sidedef_t;

  link_pp = ^link_p;
  link_p = ^link_t;
  link_t = record
    typ: integer; // type
    bits: LongWord;
    height1: integer;     // Basic height, or zero for "floor-to-ceiling"
    width1: integer;      // Basic width, or zero for "wall-to-wall"
    width2: integer;      // Width of the interalcove-passage, if any
    depth1: integer;      // Depth of doors / arches (overall depth for OPEN)
    depth2: integer;      // Depth of recess sectors
    depth3: integer;      // Length (depth) of the core (if any)
    floordelta: integer;  // Far sector floorheight - near sector floorheight
    stepcount: integer;   // Number of steps to slice depth3 into (minus one)
    cld: linedef_p;       // The inner side of a twinned core, sometimes
    next: link_p;
  end;

// The kinds of things that there are

  genus_t = record
    gamemask: byte;
    compatible: LongWord;
    bits: LongWord;
    thingid: smallint;
    width: smallint;
    height: smallint;
    ammo_to_kill: array [0..2] of single;
    ammo_provides: single;          // For monsters / ammo / weapons
    damage: array[0..2] of single;  // damage[0] will be health provided by HEALTHs */
    altdamage: array[0..2] of single;
    marked: boolean;
    next: genus_p;
  end;


// The style is the dynamic architectural knowledge and stuff.
// It changes throughout the run.
  style_t = record
    theme_number: integer;
    floor0: flat_p;
    ceiling0: flat_p;
    ceilinglight: flat_p;
    doorfloor: flat_p;
    doorceiling: flat_p;
    stepfloor: flat_p;
    nukage1: flat_p;
    wall0: texture_p;
    switch0: texture_p;
    support0: texture_p;
    doorjamb: texture_p;
    widedoorface: texture_p;
    narrowdoorface: texture_p;
    twdoorface: texture_p;     // tall-wide
    tndoorface: texture_p;     // tall-narrow
    lockdoorface: texture_p;   // can be nil
    walllight: texture_p;      // can be nil
    liftface: texture_p;       // can be nil
    kickplate: texture_p;      // At least 64 tall
    stepfront: texture_p;      // May be quite short
    grating: texture_p;
    plaque: texture_p;
    redface: texture_p;
    blueface: texture_p;
    yellowface: texture_p;
    lamp0: genus_p;
    shortlamp0: genus_p;
    doorlight0: SmallInt;
    roomlight0: SmallInt;
    wallheight0: SmallInt;
    linkheight0: SmallInt;
    closet_width: SmallInt;
    closet_depth: SmallInt;
    closet_light_delta: SmallInt;
    // Shouldn't all these booleans just be in a properties bitarray?
    moving_jambs: boolean;
    secret_doors: boolean;  // a silly thing
    soundproof_doors: boolean;
    center_pillars: boolean;
    paint_recesses: boolean;  // Put keycolors on recesses, not doors?
    lightboxes: boolean;      // Ephemeral
    gaudy_locks: boolean;
    auxheight: integer;       // Height off the ground of lightboxes (etc)
    auxspecial: SmallInt;     // Special light thing for lightboxes (etc)
    doortype: SmallInt;       // Should be part of link?
    slifttype: SmallInt;      // part of link?
    sillheight: Integer;      // should be part of link?
    windowheight: Integer;    // part of link?
    windowborder: Integer;    // part of link?
    slitwindows: boolean;     // part of link?
    window_grate: boolean;    // part of link?
    window_decor: Integer;    // part of link?
    lightbox_lighting: Integer;
    light_recesses: boolean;
    light_steps: boolean;
    light_edges: boolean;
    peg_lightstrips: boolean;
    construct_family: Integer;
    do_constructs: boolean;
    link0: link_p;
    next: style_p;
  end;

// General linked list of textures, for constructs
  texture_cell_t = record
    texture: texture_p;
    marked: boolean;
    primary: boolean;
    y_offset1: SmallInt;
    y_offset2: SmallInt;
    width: SmallInt;
    next: texture_cell_p;
  end;

// General linked list of flats
  flat_cell_t = record
    flat: flat_p;
    next: flat_cell_p;
  end;

// Things that are basically boxes with sides
  construct_t = record
    gamemask: byte;
    compatible: LongWord;
    family: Integer;    // What general kind of thing is it?
    height: SmallInt;
    texture_cell_anchor: texture_cell_p;
    flat_cell_anchor: flat_cell_p;
    marked: boolean;
    next: construct_p;
  end;

  thing_t = record
    x: SmallInt;
    y: SmallInt;
    angle: SmallInt;
    genus: genus_p;
    options: SmallInt;
    number: SmallInt;
    next: thing_p;
  end;

// These are sort of two-natured; they represent both sectors in the
// DooM-engine sense, and rooms.  Split the meanings someday.
  sector_t = record
    floor_height: SmallInt;
    ceiling_height: SmallInt;
    floor_flat: flat_p;
    ceiling_flat: flat_p;
    light_level: SmallInt;
    special: SmallInt;
    tag: SmallInt;
    number: SmallInt; // Used only during dumping
    style: style_p;   // Style used to create it
    marked: boolean;
    has_key: boolean; // Has a key been placed in here?
    has_dm: boolean;  // A DM start in here?
    has_dm_weapon: boolean; // Any weapons in here yet in DM?
    middle_enhanced: boolean; // Already specially enhanced
    gate: gate_p;
    entry_x, entry_y: SmallInt;
    findrec_data_valid: boolean;
    minx, miny, maxx, maxy: SmallInt;
    next: sector_p;
  end;

  vertex_t = record
    x: SmallInt;
    y: SmallInt;
    number: SmallInt;
    marked: boolean;
    next: vertex_p;
  end;

  sidedef_t = record
    x_offset: SmallInt;
    x_misalign: SmallInt;
    y_offset: SmallInt;
    y_misalign: SmallInt;
    upper_texture: texture_p;
    lower_texture: texture_p;
    middle_texture: texture_p;
    sector: sector_p;
    number: SmallInt;
    isBoundary: boolean;
    next: sidedef_p;
  end;

  linedef_t = record
    from: vertex_p;
    _to: vertex_p;
    flags: SmallInt;
    typ: SmallInt; // Ooh, could even have linedef-type-kind records!
    tag: SmallInt;
    right: sidedef_p;
    left: sidedef_p;
    number: Integer;
    marked: boolean;
    f_misaligned: boolean;
    b_misaligned: boolean;
    group_next: linedef_p;  // Used during texture-alignment
    group_previous: linedef_p;  // A group gets aligned together
    next: linedef_p;
  end;

var
  plink: link_p;
  pgenus: genus_p;
  pstyle: style_p;
  ptexture_cell: texture_cell_p;
  pflat_cell: flat_cell_p;
  pconstruct: construct_p;
  pthing: thing_p;
  psector: sector_p;
  pvertex: vertex_p;
  psidedef: sidedef_p;
  plinedef: linedef_p;

const
// Linedef flags
  IMPASSIBLE = $01;
  BLOCK_MONSTERS = $02;
  TWO_SIDED = $04;
  UPPER_UNPEGGED = $08;
  LOWER_UNPEGGED = $10;
  SECRET_LINEDEF = $20;
  BLOCK_SOUND = $40;
  NOT_ON_MAP = $80;
  ALREADY_ON_MAP = $100;

// Linedef types
  LINEDEF_NORMAL = 0;
  LINEDEF_NORMAL_DOOR = 1;
  LINEDEF_NORMAL_S1_DOOR = 31;
  LINEDEF_BLUE_S1_DOOR = 32;
  LINEDEF_RED_S1_DOOR = 33;
  LINEDEF_YELLOW_S1_DOOR = 34;
  LINEDEF_S1_OPEN_DOOR = 103;
  LINEDEF_S1_OPEN_DOOR_BLUE = 133;
  LINEDEF_S1_OPEN_DOOR_RED = 135;
  LINEDEF_S1_OPEN_DOOR_YELLOW = 137;
  LINEDEF_WR_OPEN_DOOR = 86;
  LINEDEF_W1_OPEN_DOOR = 2;
  LINEDEF_GR_OPEN_DOOR = 46;
  LINEDEF_SR_OC_DOOR = 63;
  LINEDEF_WR_OC_DOOR = 90;
  LINEDEF_BLAZE_DOOR = 117;
  LINEDEF_BLAZE_S1_DOOR = 118;
  LINEDEF_S1_BLAZE_O_DOOR = 112;
  LINEDEF_SR_BLAZE_OC_DOOR = 114;
  LINEDEF_S1_END_LEVEL = 11;
  LINEDEF_W1_END_LEVEL = 52;
  LINEDEF_S1_SEC_LEVEL = 51;
  LINEDEF_W1_SEC_LEVEL = 124;
  LINEDEF_WR_FAST_CRUSH = 77;
  LINEDEF_WR_LOWER_LIFT = 88;
  LINEDEF_SR_LOWER_LIFT = 62;
  LINEDEF_WR_TURBO_LIFT = 120;
  LINEDEF_SR_TURBO_LIFT = 123;
  LINEDEF_S1_RAISE_AND_CLEAN_FLOOR = 20;
  LINEDEF_S1_RAISE_FLOOR = 18;
  LINEDEF_W1_RAISE_FLOOR = 119;
  LINEDEF_S1_RAISE_STAIRS = 7;
  LINEDEF_S1_LOWER_FLOOR = 23;
  LINEDEF_SCROLL = 48;
  LINEDEF_TELEPORT = 97;
// and so on and so on

// Sector specials
  RANDOM_BLINK = 1;
  SYNC_FAST_BLINK = $0c;
  SYNC_SLOW_BLINK = $0d;
  GLOW_BLINK = $08;
  SECRET_SECTOR = $09;
  NUKAGE1_SPECIAL = 5;
  DEATH_SECTOR = $0b;

// Stuff related to an open PWAD we're generating
type
  index_entry_p = ^index_entry_t;
  index_entry_t = record
    name: string[255];
    offset: LongWord;
    length: LongWord;
    next: index_entry_p;
  end;

var
  pindex_entry: index_entry_p;

type
  dump_record_p = ^dump_record_t;
  dump_record_t = record
    f: file;
    offset_to_index: LongWord;
    lmpcount: LongWord;
    index_entry_anchor: index_entry_p;
  end;

var
  pdump_record: index_entry_p;

type
  musheader_p = ^musheader_t;
  musheader_t = record
    tag: array[0..3] of char; // MUS[0x1a]
    muslength: SmallInt;
    headerlength: SmallInt;
    primchannels: SmallInt;
    secchannels: SmallInt;
    patches: SmallInt;
    dummy: SmallInt;
  end;

type
  patch_p = ^patch_t;
  patch_t = record
    number: SmallInt;
    x: SmallInt;
    y: SmallInt;
    next: patch_p;
  end;

var
  ppatch: patch_p;

type
  custom_texture_p = ^custom_texture_t;
  custom_texture_t = record
    name: string[255];
    xsize: SmallInt;
    ysize: SmallInt;
    patch_anchor: patch_p;
    next: custom_texture_p;
  end;

var
  pcustom_texture: custom_texture_p;

type
  texture_lmp_p = ^texture_lmp_t;
  texture_lmp_t = record
    name: string[255];
    custom_texture_anchor: custom_texture_p;
  end;

var
  ptexture_lmp: texture_lmp_p;

// Health, Armor, and Ammo estimates
type
  one_haa_p = ^one_haa_t;
  one_haa_t = record
    health: single;
    armor: single;
    ammo: single;
    can_use_shells: boolean;
    can_use_rockets: boolean;
    can_use_cells: boolean;
    has_chaingun: boolean;
    has_chainsaw: boolean;
    has_backpack: boolean;
    has_berserk: boolean;
    has_ssgun: boolean;
    shells_pending: boolean;
    chaingun_pending: boolean;
  end;

var
  pone_haa: one_haa_p;

const
  ITYTD = (0);
  HMP = (1);
  UV = (2);

type
  haa_p = ^haa_t;
  haa_t = record
    haas: array[0..3] of one_haa_t;
  end;

var
  phaa: haa_p;

type
  quest_p = ^quest_t;
  quest_t = record
    goal: SmallInt;       // What kind of quest?
    tag: SmallInt;        // If a linedef/switch, what's the tag?
    tag2: SmallInt;       // Another tag, if needed for GATEs etc.
    typ: SmallInt;        // What should we do to the tag?
                          // Or what's the ID of the key
    room: sector_p;       // What room will the quest let us into?
    count: SmallInt;      // How many rooms in the quest so far?
    minrooms: SmallInt;   //How many rooms at least should it have?
    auxtag: SmallInt;     // Tag of door to open when taking goal
    surprise: linedef_p;  // Linedef to populate after goal room
    thing: thing_p;       // Thing that closed a closed thing-quest
    next: quest_p;        // For the quest stack
  end;

var
  pquest: quest_p;

const
// Values for quest.goal
  LEVEL_END_GOAL = 101;
  KEY_GOAL = 102;
  SWITCH_GOAL = 103;
  NULL_GOAL = 104;
  ARENA_GOAL = 105;
  GATE_GOAL = 106;

// The Arena
const
  ARENA_ROOF = $01;
  ARENA_PORCH = $02;
  ARENA_LAMPS = $04;
  ARENA_ARRIVAL_HOLE = $08;
  ARENA_NUKAGE = $10;

type
  arena_p = ^arena_t;
  arena_t = record
    props: LongWord;
    boss: genus_p;
    boss_count: Integer;
    weapon: genus_p;
    ammo: genus_p;
    floor: flat_p;
    walls: texture_p;
    placed_health: boolean;
    placed_armor: boolean;
    placed_ammo: boolean;
    placed_weapon: boolean;
    minx, miny, maxx, maxy: SmallInt;
    innersec: sector_p;
    outersec: sector_p;
    fromtag: SmallInt;
    next: arena_p;
  end;

var
  parena: arena_p;

type
  level_p = ^level_t;
  level_t = record
    thing_anchor: thing_p;
    sector_anchor: sector_p;
    vertex_anchor: vertex_p;
    sidedef_anchor: sidedef_p;
    linedef_anchor: linedef_p;
    used_red: boolean;
    used_blue: boolean;
    used_yellow: boolean;
    last_tag_used: Integer;
    sl_tag: SmallInt;         // Tag for thing to activate to open secret level exit
    sl_type: SmallInt;        // Type for ...
    sl_open_start: sector_p;  // The first room the opener can go in
    sl_open_ok: boolean;      // Is it time to do the opener yet?
    sl_exit_sector: sector_p; // The room the exit switch is in
    sl_done: boolean;         // Did we done it yet?
    first_room: sector_p;
    goal_room: sector_p;
    secret_count: Integer;
    dm_count: Integer;
    dm_rho: Integer;
    support_misaligns: boolean;
    seen_suit: boolean;
    seen_map: boolean;
    scrolling_keylights: boolean;
    skyclosets: integer;      // Percent chance of closets being open to the sky
    p_new_pillars: Integer;
    p_stair_lamps: Integer;
    p_force_nukage: Integer;
    p_force_sky: Integer;
    p_deep_baths: Integer;
    p_falling_core: Integer;
    p_barrels: Integer;
    p_extwindow: Integer;
    p_extroom: Integer;
    p_rising_room: Integer;
    p_surprise: Integer;
    p_swcloset: Integer;
    p_rational_facing: Integer;
    p_biggest_monsters: Integer;
    p_open_link: Integer;
    p_s1_door: Integer;
    p_special_room: Integer;  // Should be in Style, maybe?
    lift_rho: Integer;        // How common are lifts?
    amcl_rho: Integer;        // How common are ambush-closets?
    maxkeys: Integer;         // How many key or switch quests, at most, to use
    barcount: Integer;        // How many door-bars so far?
    crushercount: Integer;    // How many left-on crushers so far?
    hugeness: Integer;        // A one or a two or whatever
    skullkeys: boolean;       // Use skull (not card) keys?
    use_gates: boolean;       // Allowed to use non-exit teleporters?
    raise_gates: boolean;     // Teleport flats raised a bit?
    all_wide_links: boolean;
    no_doors: boolean;
    outside_light_level: SmallInt;  // I don't know if it's cloudy or bright
    bright_light_level: SmallInt;   // How bright a bright room is
    lit_light_level: SmallInt;      // How bright a working lamp/light is
    // These lists are just for memory-freeing purposes
    style_anchor: style_p;
    link_anchor: link_p;
    gate_anchor: gate_p;
    arena_anchor: arena_p;
  end;

var
  plevel: level_p;

// The config is the static architectural knowledge and stuff.
// It's read from a config file (parts of it, anyway!).
type
  config_p = ^config_t;
  config_t = record
    configfile: ansistring;    // Name of the configuration file
    configdata: ansistring;    // Contents of the configuration file
    outfile: ansistring;       // Name of the output file
    cwadonly: boolean;     // Do we want just the customization lumps?
    fcontrol: boolean;
    fstart: boolean;
    ffstart: boolean;
    ffend: boolean;
    fend: boolean;
    ranseed: LongWord;
    themecount: byte;     // How many (non-secret) themes there are
    sthemecount: byte;    // How many secret themes there are
    secret_themes: boolean;
    lock_themes: boolean;
    major_nukage: boolean;
    required_monster_bits: LongWord;
    forbidden_monster_bits: LongWord;
    minrooms: SmallInt;
    theme_anchor: theme_p;
    genus_anchor: genus_p;
    flat_anchor: flat_p;
    texture_anchor: texture_p;
    construct_anchor: construct_p;
    sky_flat: flat_p;
    water_flat: flat_p;
    null_texture: texture_p;
    error_texture: texture_p;
    gate_exitsign_texture: texture_p;
    gamemask: byte;   // Which games must we be compatible with?
    levelcount: integer;  // How many levels to produce
    produce_null_lmps: boolean;
    do_music: boolean;
    do_slinfo: boolean;
    do_seclevels: boolean;
    do_dm: boolean;
    secret_monsters: boolean;
    force_secret: boolean;
    force_arena: boolean;
    force_biggest: boolean;
    weapons_are_special: boolean;  // Can weapons (not) be given out as ammo?
    recess_switches: boolean;
    big_weapons: boolean;
    big_monsters: boolean;
    gunk_channels: boolean;
    clights: boolean;
    allow_boring_rooms: boolean;
    both_doors: boolean;
    doorless_jambs: boolean;
    machoh: single; // Macho-factor for Hurt Me Plenty
    machou: single; // Macho-factor for Ultraviolins
    p_bigify: integer;  // Percent chance of maybe expanding rooms
    usualammo: array[0..2] of integer;  // Usual ammo/armor/health for the three hardnesses
    usualarmor: array[0..2] of integer;
    usualhealth: array[0..2] of integer;
    minhealth: array[0..2] of integer;  // Minimal OK healths for the three hardnesses
    immediate_monsters: boolean;  // OK to have monsters in first room?
    p_hole_ends_level: integer;
    p_gate_ends_level: integer;
    p_use_steps: integer;
    p_sync_doors: integer;
    p_grid_gaps: integer;
    p_pushquest: integer;
    rad_newtheme: integer;  // How likely to use a random theme beyond a lock
    norm_newtheme: integer; // How likely beyond a non-lock link
    rad_vary: integer;      // How much to vary the style beyond a lock
    norm_vary: integer;     // How much beyon a non-lock link
    monsters_can_teleport: boolean;
    window_airshafts: boolean;
    homogenize_monsters: integer; // How likely to have all room monsters ==
    minlight: integer;  // How dark is dark?
    minhuge: integer;
    // These are *not* actually static
    episode, mission, map: integer; // What map/mission we're on now.
    last_mission: boolean;  // This the last one we're doing?
  end;

// Lots and lots and lots of functions
// And this isn't even all of 'em!

function get_config(const args: TDStringList): config_p;

procedure NewLevel(const l: level_p; const ThisHaa: haa_p; const c: config_p);

procedure DumpLevel(const dh: dump_record_p; const c: config_p; const l: level_p;
  const episode, mission, map: integer);

procedure FreeLevel(const l: level_p);

function OpenDump(const c: config_p): dump_record_p;

procedure CloseDump(const dh: dump_record_p);

function starting_quest(const l: level_p; const c: config_p): quest_p;

function starting_haa: haa_p;

function random_style(const l: level_p; const c: config_p): style_p;

function enough_quest(const l: level_p; const s: sector_p; const ThisQuest: quest_p;
  const c: config_p): boolean;

function rollpercent(const percent: integer): boolean;

function roll(const zero_to_this_minus_one: integer): integer;

function starting_linedef(const l: level_p; const ThisStyle: style_p;
  const c: config_p): linedef_p;

function mark_adequate_linedefs(const l: level_p; const s: sector_p; const ThisStyle:
  style_p; const c: config_p): integer;

function mark_decent_boundary_linedefs(const l: level_p; const s: sector_p;
  const minlen: integer): integer;

function isAdequate(const l: level_p; const ld: linedef_p; const ThisStyle: style_p;
  const c: config_p): boolean;

function random_marked_linedef(const l: level_p; const i: integer): linedef_p;

procedure unmark_linedefs(const l: level_p);

procedure embellish_room(const l: level_p; const oldsector: sector_p;  const haa: haa_p;
  const ThisStyle: style_p; const ThisQuest: quest_p; const first: boolean;
  const edges_only: boolean; const c: config_p);

function grid_room(const l: level_p; const oldsector: sector_p;
  const haa: haa_p; const ThisStyle: style_p; const ThisQuest: quest_p;
  const first: boolean; const c: config_p): boolean;

procedure enhance_room(const l: level_p; const oldsector: sector_p; const ThisHaa: haa_p;
  const ThisStyle: style_p; const ThisQuest: quest_p; const first: boolean;
  const c: config_p);

procedure align_textures(const l: level_p; const oldsector: sector_p; const c: config_p);

procedure populate(const l: level_p; const oldsector: sector_p; const c: config_p;
  const ThisHaa: haa_p; const first_room: boolean);

procedure gate_populate(const l: level_p; const s: sector_p; const haa: haa_p;
  const first: boolean; const c: config_p);

function random_link(const l: level_p; const ld: linedef_p; const ThisStyle: style_p;
  const ThisQuest: quest_p; const c: config_p): link_p;

function random_open_link(const l: level_p; const ld: linedef_p; const ThisStyle: style_p;
  const ThisQuest: quest_p; const c: config_p): link_p;

function random_basic_link(const l: level_p; const ld: linedef_p; const ThisStyle: style_p;
  const ThisQuest: quest_p; const c: config_p): link_p;

function gate_link_f(const l: level_p; const c: config_p): link_p;

function generate_room_outline(const l: level_p; const ld: linedef_p; const ThisStyle: style_p;
  const try_reduction: boolean; const c: config_p): sector_p;

function lengthsquared(const ld: linedef_p): integer;

function distancesquared(const x1, y1, x2, y2: integer): integer;

function infinity_norm(const x1, y1, x2, y2: integer): integer;

function empty_rectangle(const l: level_p; const x1, y1, x2, y2, x3, y3, x4, y4: integer): boolean;

function empty_left_side(const l: level_p; const ld: linedef_p; const sdepth: integer): boolean;

procedure close_quest(const l: level_p; const s: sector_p; const q: quest_p;
  const haa: haa_p; const c: config_p);

procedure maybe_push_quest(const l: level_p; const s: sector_p; const ThisQuest: quest_p;
  const c: config_p);

function make_parallel(const l: level_p; const ld: linedef_p; const depth: integer;
  const old: linedef_p): linedef_p;

function lefthand_box_ext(const l: level_p; const ldf1: linedef_p; const depth: integer;
                                  const ThisStyle: style_p; const c: config_p;
                                  const nld1, nld2: linedef_pp): linedef_p;

function lefthand_box(const l: level_p; const ldf1: linedef_p; const depth: integer;
                      const ThisStyle: style_p; const c: config_p): linedef_p;

function facing_along(const x1, y1, x2, y2: integer): integer;

function facing_right_from(const x1, y1, x2, y2: integer): integer;

function facing_right_from_ld(const ld: linedef_p): integer;

function make_linkto(const l: level_p; const ldf: linedef_p; const ThisLink: link_p;
                     const ThisStyle: style_p; const c: config_p; const old: linedef_p): linedef_p;

procedure e_bl_inner(const l: level_p; ldf1, ldf2: linedef_p; ThisLink: link_p;
                     const ThisQuest: quest_p; const ThisStyle, NewStyle: style_p;
                     const flipstate: SmallInt; const haa: haa_p; const c: config_p);

procedure e_ol_inner(const l: level_p; ldf1, ldf2: linedef_p; const ThisLink: link_p;
                     const ThisQuest: quest_p; const ThisStyle, NewStyle: style_p;
                     const haa: haa_p; const c: config_p);

procedure establish_basic_link(const l: level_p; const ldf1, ldf2: linedef_p; const ThisLink: link_p;
                          const ThisQuest: quest_p; const ThisStyle, NewStyle: style_p;
                          const haa: haa_p; const c: config_p);

procedure establish_open_link(const l: level_p; const ldf1, ldf2: linedef_p; const ThisLink: link_p;
                         const ThisQuest: quest_p; const ThisStyle, NewStyle: style_p;
                         const haa: haa_p; const c: config_p);

procedure establish_link(const l: level_p; const ldf1, ldf2: linedef_p; const ThisLink: link_p;
                     const ThisQuest: quest_p; const ThisStyle, NewStyle: style_p;
                     const haa: haa_p; const c: config_p);

procedure stairify(const l: level_p; ldf1, ldf2: linedef_p;
  lde1, lde2: linedef_p; nearheight, farheight: SmallInt;
  const ThisQuest: quest_p; const ThisStyle: style_p; const c: config_p);

procedure paint_room(const l: level_p; const s: sector_p; const ThisStyle: style_p;
  const c: config_p);

function split_linedef(const l: level_p; const ld: linedef_p; const len: integer;
  const c: config_p): linedef_p;

function link_fitsq(const ThisLink: link_p; const ThisQuest: quest_p): boolean;

function link_fitsh(const ldf: linedef_p; const ThisLink: link_p; const c: config_p): boolean;

function link_fitsv(const l: level_p; const ldf1, ldf2: linedef_p; const ThisLink: link_p): boolean;

procedure Usage0;

procedure Usage;

procedure Usage2;

function do_switches(const args: TDStringList; const c: config_p; const s: string;
  const conly: integer): boolean;

function read_switches(const c: config_p): boolean;

function nonswitch_config(const c: config_p): boolean;

procedure load_config(const c: config_p);

procedure unload_config(const c: config_p);

function new_texture(const c: config_p; const name: string): texture_p;

function find_texture(const c: config_p; const name: string): texture_p;

function find_genus(const c: config_p; const thingid: integer): genus_p;

function new_genus(const c: config_p; const thingid: integer): genus_p;

function new_flat(const c: config_p; const name: string): flat_p;

function find_flat(const c: config_p; const name: string): flat_p;

function new_theme(const c: config_p; const name: string; const secret: boolean): theme_p;

function new_gate(const l: level_p; const intag, outtag: SmallInt; const lock: SmallInt;
  const entry: boolean; const c: config_p): gate_p;

procedure patch_upper(const ld: linedef_p; const t: texture_p; const c: config_p);

procedure patch_lower(const ld: linedef_p; const t: texture_p; const c: config_p);

function flip_linedef(const ld: linedef_p): linedef_p;

function make_box_ext(const l: level_p; const ldf1, ldf2: linedef_p;
                      const ThisStyle: style_p; const c: config_p;
                      const edge1, edge2: linedef_pp): sector_p;

function make_box(const l: level_p; const ldf1, ldf2: linedef_p;
                      const ThisStyle: style_p; const c: config_p): sector_p;

function place_object(const l: level_p;const s: sector_p;const c: config_p;
  const thingid: SmallInt; const width: integer; const angle: integer;
  const ax, ay: integer; const bits: integer): thing_p;

function place_object_in_region(const l: level_p; const minx, miny, maxx, maxy: integer;
  const c: config_p; const thingid: SmallInt; const width: integer; const angle: integer;
  const ax, ay: integer; const bits: integer): thing_p;

function place_required_pickable(const l: level_p; const s: sector_p; const c: config_p;
  const id: SmallInt): thing_p;

function timely_monster(const haa: haa_p; const c: config_p; const levels: PInteger;
  const biggest: boolean; const mno: integer): genus_p;

function timely_monster_ex(const haa: haa_p; const c: config_p; const levels: PInteger;
  biggest: boolean; const mno: integer; const req: LongWord): genus_p;

procedure update_haa_for_monster(const haa: haa_p; const m: genus_p; const levels: integer;
  const mno: integer; const c: config_p);

procedure ammo_value(const ammotype: SmallInt; const haa: haa_p; out a0, a1, a2: integer);

procedure haa_unpend(const haa: haa_p);

procedure trigger_box(const l: level_p; const t: thing_p; oldsector: sector_p;
  const tag, typ: SmallInt; const c: config_p);

procedure populate_linedef(const l: level_p; const ldnew2: linedef_p;
  const haa: haa_p; const c: config_p; const secret: boolean);

procedure find_sector_rectangle(const l: level_p; const s: sector_p;
  out minx, miny, maxx, maxy: integer);

procedure mid_tile(const l: level_p; const s: sector_p; out tlx, tly, thx, thy: SmallInt);

function line_center_part(const l: level_p; const ld: linedef_p; const ld2: linedef_pp;
  const width: integer; const ThisStyle: style_p; const c: config_p): linedef_p;

function texture_for_key(const key: SmallInt; const s: style_p; const c: config_p): texture_p;

function texture_for_bits(const pb: LongWord; const s: style_p; const c: config_p): texture_p;

function type_for_key(const key: SmallInt): SmallInt;

procedure make_lighted(const l: level_p; const s: sector_p; const c: config_p);

function locked_linedef_for(const typ: SmallInt; const key: SmallInt; const c: config_p): SmallInt;

procedure install_gate(const l: level_p;const s: sector_p; const ThisStyle: style_p;
  const ThisHaa: haa_p; const force_exit_style: boolean; const c: config_p);

procedure frame_innersec_ex(const l: level_p; const oldsector, innersec: sector_p;
                      tm, tu, tl: texture_p;
                      const x1, y1, x2, y2, x3, y3, x4, y4: integer;
                      const c: config_p;
                      const ld1, ld2, ld3, ld4: linedef_pp);

procedure frame_innersec(const l: level_p; const oldsector, innersec: sector_p;
                      tm, tu, tl: texture_p;
                      const x1, y1, x2, y2, x3, y3, x4, y4: integer;
                      const c: config_p);

procedure parallel_innersec_ex(const l: level_p; const oldsector, innersec: sector_p;
                               const tm, tu, tl: texture_p;
                               const minx, miny, maxx, maxy: integer; const c: config_p;
                               const ld1, ld2, ld3, ld4: linedef_pp);

procedure parallel_innersec(const l: level_p; const oldsector, innersec: sector_p;
                            const tm, tu, tl: texture_p;
                            const minx, miny, maxx, maxy: integer; const c: config_p);

function install_construct(const l: level_p; oldsector: sector_p;
                          const minx, miny, maxx, maxy: integer;
                          const ThisStyle: style_p; const c: config_p): boolean;

procedure make_music(const dh: dump_record_p; const c: config_p);

procedure make_slinfo(const dh: dump_record_p; const c: config_p);

procedure record_custom_textures(const dh: dump_record_p; const c: config_p);

procedure record_custom_flats(const dh: dump_record_p; const c: config_p;
  const even_unused: boolean);

procedure record_custom_patches(const dh: dump_record_p; const c: config_p;
  const even_unused: boolean);

function need_secret_level(const c: config_p): boolean;

procedure make_secret_level(const dh: dump_record_p; const oldhaa: haa_p;
  const c: config_p);

procedure secretize_config(const c: config_p);

function install_sl_exit(const l: level_p; const oldsector: sector_p;
  const ThisHaa: haa_p; const ThisStyle: style_p; const ThisQuest: quest_p;
  const opens: boolean; const c: config_p): boolean;

const
  NONE = -1;
  VERBOSE = 0;
  LOG = 1;
  NOTE = 2;
  WARNING = 3;
  ERROR = 4;

procedure announce(const announcetype: integer; const s: string);

const
  RIGHT_TURN = 90;
  LEFT_TURN = 270;

procedure point_from(const x1, y1, x2, y2: integer; const angle: integer;
                     const len: integer; const x3, y3: PInteger);

function psi_sqrt(const v: integer): SmallInt;

function linelen(const ld: linedef_p): SmallInt;

function no_monsters_stuck_on(const l: level_p; const ld: linedef_p): boolean;

function random_ceiling0(const c: config_p; const s: style_p): flat_p;

function random_ceilinglight(const c: config_p; const s: style_p): flat_p;

function random_floor0(const c: config_p; const s: style_p): flat_p;

function random_gate(const c: config_p; const s: style_p): flat_p;

function random_doorceiling(const c: config_p; const s: style_p): flat_p;

function random_doorfloor(const c: config_p; const s: style_p): flat_p;

function random_stepfloor(const c: config_p; const s: style_p): flat_p;

function random_nukage1(const c: config_p; const s: style_p): flat_p;

function random_flat0(const pmask: LongWord; const c: config_p; const s: style_p): flat_p;

function random_thing0(const pmask: LongWord;const c: config_p; const s: style_p;
  const minh, maxh: integer): genus_p;

function random_texture0(const pmask: LongWord; const c: config_p; const s: style_p): texture_p;

function random_wall0(const c: config_p; const s: style_p): texture_p;

function random_kickplate(const c: config_p; const s: style_p): texture_p;

function random_stepfront(const c: config_p; const s: style_p): texture_p;

function switch0_for(const c: config_p; const s: style_p): texture_p;

function random_support0(const c: config_p; const s: style_p): texture_p;

function random_doorjamb(const c: config_p; const s: style_p): texture_p;

function random_widedoorface(const c: config_p; const s: style_p): texture_p;

function random_widedoorface_ex(const c: config_p; const s: style_p;
  const needhigh: boolean): texture_p;

function random_narrowdoorface(const c: config_p; const s: style_p): texture_p;

function random_narrowdoorface_ex(const c: config_p; const s: style_p;
  const needhigh: boolean): texture_p;

function random_twdoorface(const c: config_p; const s: style_p): texture_p;

function random_tndoorface(const c: config_p; const s: style_p): texture_p;

function random_lockdoorface(const c: config_p; const s: style_p): texture_p;

function random_grating(const c: config_p; const s: style_p): texture_p;

function random_walllight(const c: config_p; const s: style_p): texture_p;

function random_liftface(const c: config_p; const s: style_p): texture_p;

function random_plaque(const c: config_p; const s: style_p): texture_p;

function random_lamp0(const c: config_p; const s: style_p): genus_p;

function random_shortlamp0(const c: config_p; const s: style_p): genus_p;

function random_barrel(const c: config_p; const s: style_p): genus_p;

function random_plant(const c: config_p; const s: style_p): genus_p;

function random_redface(const c: config_p; const s: style_p): texture_p;

function random_blueface(const c: config_p; const s: style_p): texture_p;

function random_yellowface(const c: config_p; const s: style_p): texture_p;

procedure place_monsters(const l: level_p; const s: sector_p; const c: config_p;
  const haa: haa_p);

procedure place_timely_something(const l: level_p; const haa: haa_p; const c: config_p;
  const x, y: integer);

procedure place_armor(const l: level_p; const s: sector_p; const c: config_p;
  const haa: haa_p);

procedure place_ammo(const l: level_p; const s: sector_p; const c: config_p;
  const haa: haa_p);

procedure place_health(const l: level_p; const s: sector_p; const c: config_p;
  const haa: haa_p);

procedure place_barrels(const l: level_p; const s: sector_p; const c: config_p;
  const haa: haa_p);

procedure place_plants(const l: level_p; const allow: integer; const s: sector_p;
  const c: config_p);

function common_texture(const sd1, sd2: sidedef_p): boolean;

function coalignable(const t1, t2: texture_p): boolean;

procedure global_align_textures(const l: level_p; const c: config_p);

procedure global_align_linedef(const l: level_p; const ld: linedef_p);

procedure global_align_forward(const l: level_p; const ld: linedef_p);

procedure global_align_backward(const l: level_p; const ld: linedef_p);

procedure global_align_group_backbone_forward(const l: level_p; const ld: linedef_p);

procedure global_align_group_backbone_backward(const l: level_p; const ld: linedef_p);

procedure global_align_group_etc_forward(const l: level_p; const ld: linedef_p);

procedure global_align_group_etc_backward(const l: level_p; const ld: linedef_p);

var
  global_verbosity: integer = 0;  // Oooh, a global variable!
  ok_to_roll: boolean = FALSE;    // Stop breaking -seed...

function _tmain_slige492(const args: TDSTringList; out outfile: string): integer;

implementation

uses
  sl_script;

function MONSTER_WIDTH(const m: thing_p): integer;
begin
{$IFDEF OK_TO_USE_REAL_MONSTER_WIDTH}
  result := m^.width
{$ELSE}
  result := 64;
{$ENDIF}
end;

function TLMPSIZE(rows, columns: integer): integer;
begin
  result := (rows + 9) * columns + 8;
end;

function b_stricmp(const x, y: string): boolean;
begin
  result := UpperCase(x) <> UpperCase(y);
end;

function SameStrings(const x, y: string): boolean;
begin
  result := UpperCase(x) = UpperCase(y);
end;

function lefthand_box(const l: level_p; const ldf1: linedef_p; const depth: integer;
                      const ThisStyle: style_p; const c: config_p): linedef_p;
begin
  result := lefthand_box_ext(l, ldf1, depth, ThisStyle, c, nil, nil);
end;

function make_box(const l: level_p; const ldf1, ldf2: linedef_p;
                      const ThisStyle: style_p; const c: config_p): sector_p;
begin
  result := make_box_ext(l, ldf1, ldf2, ThisStyle, c, nil, nil);
end;

procedure frame_innersec(const l: level_p; const oldsector, innersec: sector_p;
                      tm, tu, tl: texture_p;
                      const x1, y1, x2, y2, x3, y3, x4, y4: integer;
                      const c: config_p);
begin
  frame_innersec_ex(l, oldsector, innersec, tm, tu, tl, x1, y1, x2, y2, x3, y3, x4, y4, c, nil, nil, nil, nil);
end;

procedure parallel_innersec(const l: level_p; const oldsector, innersec: sector_p;
                            const tm, tu, tl: texture_p;
                            const minx, miny, maxx, maxy: integer; const c: config_p);
begin
  parallel_innersec_ex(l, oldsector, innersec, tm, tu, tl, minx, miny, maxx, maxy, c, nil, nil, nil, nil);
end;

function linelen(const ld: linedef_p): SmallInt;
begin
  result := psi_sqrt(lengthsquared(ld));
end;

// A stubby but functional main()
function _tmain_slige492(const args: TDSTringList; out outfile: string): integer;
var
  ThisLevel: level_t;
  ThisHaa: haa_p;
  ThisConfig: config_p;
  i: integer;
  dh: dump_record_p;
begin
  outfile := '';
  ThisHaa := nil;
  printf('RADGE (build %d)   -- [RA]ndom [D]oom [GE]nerator based on SLIGE 490'#13#10, [SOURCE_SERIAL]);
  printf('SLIGE (build 490)   -- by Dave Chess, chess@theogeny.com'#13#10#13#10);

  ThisConfig := get_config(args);
  if ThisConfig = nil then
  begin
    Usage();
    result := 100;
    exit;
  end;
  if ThisConfig^.cwadonly then
  begin
    dh := OpenDump(ThisConfig);
    if dh = nil then
    begin
      result := 28;
      exit;
    end;
    record_custom_textures(dh, ThisConfig);
    record_custom_flats(dh, ThisConfig, TRUE);   // record all flats
    record_custom_patches(dh, ThisConfig, TRUE); // and patches
    CloseDump(dh);
    printf(#13#10'Done: wrote customization WAD %s.'#13#10, [ThisConfig^.outfile]);
    outfile := ThisConfig^.outfile;
    result := 0;
    exit;
  end;
  dh := OpenDump(ThisConfig);
  if dh = nil then
  begin
    result := 28;
    exit;
  end;
  if ThisConfig^.do_slinfo then
    make_slinfo(dh, ThisConfig);
  if ThisConfig^.do_music then
    make_music(dh, ThisConfig);

  for i := 0 to ThisConfig^.levelcount - 1 do
  begin
    if (ThisHaa = nil) or (ThisConfig^.mission = 1) then
    begin
      if ThisHaa <> nil then
        SL_Free(ThisHaa);
      ThisHaa := starting_haa();
    end;
    if (i + 1) = ThisConfig^.levelcount then
      ThisConfig^.last_mission := TRUE;
    NewLevel(@ThisLevel, ThisHaa, ThisConfig);
    DumpLevel(dh, ThisConfig, @ThisLevel, ThisConfig^.episode,
      ThisConfig^.mission, ThisConfig^.map);
    if need_secret_level(ThisConfig) then
      make_secret_level(dh, ThisHaa, ThisConfig);
    if ThisConfig^.map <> 0 then
      inc(ThisConfig^.map);
    if ThisConfig^.mission <> 0 then
      inc(ThisConfig^.mission);
    if ThisConfig^.mission = 9 then // Around the corner
    begin
      inc(ThisConfig^.episode);
      ThisConfig^.mission := 1;
    end;
    FreeLevel(@ThisLevel);
  end;

  if ThisConfig^.gamemask and DOOMI_BIT = 0 then
  begin
    record_custom_textures(dh, ThisConfig);
    record_custom_flats(dh, ThisConfig, FALSE);
    record_custom_patches(dh, ThisConfig, FALSE);
  end;
  CloseDump(dh);
  printf(#13#10'Done: wrote %s.'#13#10, [ThisConfig^.outfile]);
  outfile := ThisConfig^.outfile;
  result := 0;
end;

type
  headerstuff_t = record
    tag: array[0..3] of char;
    lmpcount: LongWord;
    inxoffset: LongWord;
  end;

// Open a file ready to dump multiple levels into
function OpenDump(const c: config_p): dump_record_p;
var
  headerstuff: headerstuff_t;
begin
  result := dump_record_p(SL_Malloc(SizeOf(dump_record_t)));
  if not fopen(result^.f, c^.outfile, fCreate) then
  begin
    I_Warning('SL_OpenDump(): Error opening <%s>.'#13#10, [c^.outfile]);
    result := nil;
    exit;
  end;

  headerstuff.tag[0] := 'P';
  headerstuff.tag[1] := 'W';
  headerstuff.tag[2] := 'A';
  headerstuff.tag[3] := 'D';
  headerstuff.lmpcount := 0; // To be filled in later
  headerstuff.inxoffset := 0;
  BlockWrite(result^.f, headerstuff, SizeOf(headerstuff_t));
  result^.offset_to_index := SizeOf(headerstuff_t);  // Length of the header */
  result^.index_entry_anchor := nil;
  result^.lmpcount := 0;
end;

type
  directory_entry_t = record
    offset: LongWord;
    length: LongWord;
    lumpname: char8_t;
  end;

// Write out the directory, patch up the header, and close the file
procedure CloseDump(const dh: dump_record_p);
var
  directory_entry: directory_entry_t;
  ie: index_entry_p;
begin
  // Write the index entries
  ie := dh^.index_entry_anchor;
  while ie <> nil do
  begin
    directory_entry.offset := ie^.offset;
    directory_entry.length := ie^.length;
    directory_entry.lumpname := stringtochar8(ie^.name);
    BlockWrite(dh^.f, directory_entry, SizeOf(directory_entry_t));
    ie := ie^.next;
  end;

  // Go back and patch up the header
  seek(dh^.f, 4);
  BlockWrite(dh^.f, dh^.lmpcount, SizeOf(LongWord));
  BlockWrite(dh^.f, dh^.offset_to_index, SizeOf(LongWord));

  // and that's all!
  CloseFile(dh^.f);
end;

// Free up all the allocated structures associated with the
// level, so we can start on a new one without burning too
// much memory.
procedure FreeLevel(const l: level_p);
var
  ld, ldn: linedef_p;
  sd, sdn: sidedef_p;
  v, vn: vertex_p;
  t, tn: thing_p;
  s, sn: sector_p;
  link, linkn: link_p;
  style, stylen: style_p;
  arena, arenan: arena_p;
  gate, gaten: gate_p;
begin
  ld := l^.linedef_anchor;
  while ld <> nil do
  begin
    ldn := ld^.next;
    SL_Free(ld);
    ld := ldn;
  end;
  l^.linedef_anchor := nil;

  sd := l^.sidedef_anchor;
  while sd <> nil do
  begin
    sdn := sd^.next;
    SL_Free(sd);
    sd := sdn;
  end;
  l^.sidedef_anchor := nil;

  v := l^.vertex_anchor;
  while v <> nil do
  begin
    vn := v^.next;
    SL_Free(v);
    v := vn;
  end;
  l^.vertex_anchor := nil;

  t := l^.thing_anchor;
  while t <> nil do
  begin
    tn := t^.next;
    SL_Free(t);
    t := tn;
  end;
  l^.thing_anchor := nil;

  s := l^.sector_anchor;
  while s <> nil do
  begin
    sn := s^.next;
    SL_Free(s);
    s := sn;
  end;
  l^.sector_anchor := nil;

  link := l^.link_anchor;
  while link <> nil do
  begin
    linkn := link^.next;
    SL_Free(link);
    link :=linkn;
  end;
  l^.link_anchor := nil;

  style := l^.style_anchor;
  while style <> nil do
  begin
    stylen := style^.next;
    SL_Free(style);
    style := stylen;
  end;
  l^.style_anchor := nil;

  arena := l^.arena_anchor;
  while arena <> nil do
  begin
    arenan := arena^.next;
    SL_Free(arena);
    arena := arenan;
  end;
  l^.arena_anchor := nil;

  gate := l^.gate_anchor;
  while gate <> nil do
  begin
    gaten := gate^.next;
    SL_Free(gate);
    gate := gaten;
  end;
  l^.gate_anchor := nil;
end;

// Record the information about a new lmp of the given size
procedure RegisterLmp(const dh: dump_record_p; const s: string; const size: LongWord);
var
  ie, ie2: index_entry_p;
begin
  ie := index_entry_p(SL_Malloc(SizeOf(index_entry_t)));
  ie^.next := nil;
  // This list really has to be in frontwards order!
  if dh^.index_entry_anchor = nil then
  begin
    dh^.index_entry_anchor := ie
  end
  else
  begin
    ie2 := dh^.index_entry_anchor;
    while ie2^.next <> nil do
      ie2 := ie2^.next;
    ie2^.next := ie;
  end;
  ie^.name := s;
  ie^.offset := dh^.offset_to_index;
  ie^.length := size;
  dh^.offset_to_index := dh^.offset_to_index + size;
  inc(dh^.lmpcount);
end;

// Given a dumphandle, a music header, a music buffer, a lump name,
// and for some reason a config, do what's necessary to record it
// in the file and index and stuff.
procedure record_music(const dh: dump_record_p; mh: musheader_p; const buf: PByteArray; const s: string; const c: config_p);
var
  lsize: LongWord;
begin
  lsize := mh^.headerlength + mh^.muslength;
  RegisterLmp(dh, s,lsize);
  BlockWrite(dh^.f, mh^, SizeOf(musheader_t)); // Write fixed header
  BlockWrite(dh^.f, buf^, mh^.patches * SizeOf(SmallInt) + mh^.muslength);
end;

// Make the special SLINFO lmp, containing whatever we like
procedure make_slinfo(const dh: dump_record_p; const c: config_p);
var
  slinfo: string;
  i: integer;
begin
  sprintf(slinfo, 'SLIGE (%d) %d ', [SOURCE_SERIAL, c^.ranseed]);
  RegisterLmp(dh, 'SLINFO', Length(slinfo));
  for i := 1 to Length(slinfo) do
    BlockWrite(dh^.f, slinfo[i], SizeOf(char));
end; // end make_slinfo()

type
  rawthing_t = record
    x, y: SmallInt;
    angle: SmallInt;
    typ: SmallInt;
    options: SmallInt;
  end;

  rawsector_t = record
    floor_height: SmallInt;
    ceiling_height: SmallInt;
    floor_flat: char8_t;
    ceiling_flat: char8_t;
    light_level: SmallInt;
    special: SmallInt;
    tag: SmallInt;
  end;

  rawvertex_t = record
    x, y: SmallInt;
  end;

  rawsidedef_t = record
    x_offset: SmallInt;
    y_offset: SmallInt;
    upper_texture: char8_t;
    lower_texture: char8_t;
    middle_texture: char8_t;
    sector: SmallInt;
  end;

  rawlinedef_t = record
    from: SmallInt;
    _to: SmallInt;
    flags: SmallInt;
    typ: SmallInt;
    tag: SmallInt;
    right: SmallInt;
    left: SmallInt;
  end;


// Write out a PWAD containing just the THINGS, LINEDEFS, SIDEDEFS,
// VERTEXES, and SECTORS for the given episode/mission.  The user
// will have to run a nodebuilder and reject mapper hisself.
procedure DumpLevel(const dh: dump_record_p; const c: config_p; const l: level_p;
  const episode, mission, map: integer);
var
  i: LongWord;
  pSector: sector_p;
  pThing: thing_p;
  pVertex: vertex_p;
  pLinedef: linedef_p;
  pSidedef: sidedef_p;
  sb: string;
  rawthing: rawthing_t;
  rawsector: rawsector_t;
  rawvertex: rawvertex_t;
  rawsidedef: rawsidedef_t;
  rawlinedef: rawlinedef_t;
begin
  // Register the zero-length marker entry

  if map = 0 then
    sprintf(sb, 'E%dM%d', [episode, mission])
  else if map < 10 then
    sprintf(sb, 'MAP0%d', [map])
  else
    sprintf(sb, 'MAP%d', [map]);

  RegisterLmp(dh, sb, 0);

  // Number all items, register in the directory

  // Number and count the things, register
  i := 0;
  pThing := l^.thing_anchor;
  while pThing <> nil do
  begin
    pThing^.number := i;
    pThing := pThing^.next;
    inc(i);
  end;
  RegisterLmp(dh, 'THINGS', i * SizeOf(rawthing_t));

  // Count the number of linedefs, register
  i := 0;
  pLinedef := l^.linedef_anchor;
  while pLinedef <> nil do
  begin
    pLinedef^.number := i;
    pLinedef := pLinedef^.next;
    inc(i);
  end;
  RegisterLmp(dh, 'LINEDEFS', i * SizeOf(rawlinedef_t));

  // Count the number of sidedefs, register
  i := 0;
  pSidedef := l^.sidedef_anchor;
  while pSidedef <> nil do
  begin
    pSidedef^.number := i;
    pSidedef := pSidedef^.next;
    inc(i);
  end;
  RegisterLmp(dh, 'SIDEDEFS', i * SizeOf(rawsidedef_t));

  // Count the number of vertexes, register
  i := 0;
  pVertex := l^.vertex_anchor;
  while pVertex <> nil do
  begin
    pVertex^.number := i;
    pVertex := pVertex^.next;
    inc(i);
  end;
  RegisterLmp(dh, 'VERTEXES', i * SizeOf(rawvertex_t));

  if c^.produce_null_lmps then
  begin
    RegisterLmp(dh, 'SEGS', 0);
    RegisterLmp(dh, 'SSECTORS', 0);
    RegisterLmp(dh, 'NODES', 0);
  end;

  // Count the number of sectors, register
  i := 0;
  pSector := l^.sector_anchor;
  while pSector <> nil do
  begin
    pSector^.number := i;
    pSector := pSector^.next;
    inc(i);
  end;
  RegisterLmp(dh, 'SECTORS', i * SizeOf(rawsector_t));

  if c^.produce_null_lmps then
  begin
    RegisterLmp(dh, 'REJECT', 0);
    RegisterLmp(dh, 'BLOCKMAP', 0);
  end;

  // Now actually write all those lmps
  pThing := l^.thing_anchor;
  while pThing <> nil do
  begin
    rawthing.x := pThing^.x;
    rawthing.y := pThing^.y;
    rawthing.angle := pThing^.angle;
    rawthing.typ := pThing^.genus^.thingid;
    rawthing.options := pThing^.options;
    fwrite(@rawthing, SizeOf(rawthing_t), 1, dh^.f);
    pThing := pThing^.next;
  end;

  // and all the linedefs
  pLinedef := l^.linedef_anchor;
  while pLinedef <> nil do
  begin
    rawlinedef.from := (pLinedef^.from)^.number;
    rawlinedef._to := (pLinedef^._to)^.number;
    rawlinedef.flags := pLinedef^.flags;
    rawlinedef.typ := pLinedef^.typ;
    rawlinedef.tag := pLinedef^.tag;

    if pLinedef^.right = nil then
      rawlinedef.right := -1 // actually an error, eh?
    else
      rawlinedef.right := (pLinedef^.right)^.number;

    if pLinedef^.left = nil then
      rawlinedef.left := -1
    else
      rawlinedef.left := (pLinedef^.left)^.number;

    fwrite(@rawlinedef, SizeOf(rawlinedef), 1, dh^.f);
    pLinedef := pLinedef^.next;
  end;

  // and all the sidedefs
  pSidedef := l^.sidedef_anchor;
  while pSidedef <> nil do
  begin
    rawsidedef.x_offset := pSidedef^.x_offset;
    rawsidedef.y_offset := pSidedef^.y_offset;
    rawsidedef.upper_texture := stringtochar8(pSidedef^.upper_texture^.realname);
    pSidedef^.upper_texture^.used := TRUE;
    rawsidedef.lower_texture := stringtochar8(pSidedef^.lower_texture^.realname);
    pSidedef^.lower_texture^.used := TRUE;
    rawsidedef.middle_texture := stringtochar8(pSidedef^.middle_texture^.realname);
    pSidedef^.middle_texture^.used := TRUE;
    rawsidedef.sector := (pSidedef^.sector)^.number;
    fwrite(@rawsidedef, SizeOf(rawsidedef), 1, dh^.f);
    pSidedef := pSidedef^.next;
  end;

  // and all the vertexes
  pVertex := l^.vertex_anchor;
  while pVertex <> nil do
  begin
    rawvertex.x := pVertex^.x;
    rawvertex.y := pVertex^.y;
    fwrite(@rawvertex, SizeOf(rawvertex), 1, dh^.f);
    pVertex := pVertex^.next;
  end;

  // and finally all the sectors
  pSector := l^.sector_anchor;
  while pSector <> nil do
  begin
    rawsector.floor_height := pSector^.floor_height;
    rawsector.ceiling_height := pSector^.ceiling_height;
    rawsector.floor_flat := stringtochar8(pSector^.floor_flat^.name);
    pSector^.floor_flat^.used := TRUE;
    rawsector.ceiling_flat := stringtochar8(pSector^.ceiling_flat^.name);
    pSector^.ceiling_flat^.used := TRUE;
    rawsector.light_level := pSector^.light_level;
    rawsector.special := pSector^.special;
    rawsector.tag := pSector^.tag;
    fwrite(@rawsector, SizeOf(rawsector), 1, dh^.f);
    pSector := pSector^.next;
  end;
end; // end DumpLevel

// Get the next unused tag for the level
function new_tag(const l: level_p): SmallInt;
begin
  inc(l^.last_tag_used);
  result := l^.last_tag_used;
end;

// Get an unused-color key for the level (if any), and use it.
// Zero if all are used.
function new_key(const l: level_p; out ret: SmallInt): SmallInt;
begin
  if (not l^.used_red) and rollpercent(33) then
  begin
    l^.used_red := TRUE;
    if l^.skullkeys then
      result := ID_REDKEY
    else
      result := ID_REDCARD;
    ret := result;
    exit;
  end;

  if (not l^.used_blue) and rollpercent(50) then
  begin
    l^.used_blue := TRUE;
    if l^.skullkeys then
      result := ID_BLUEKEY
    else
      result := ID_BLUECARD;
    ret := result;
    exit;
  end;

  if not l^.used_yellow then
  begin
    l^.used_yellow := TRUE;
    if l^.skullkeys then
      result := ID_YELLOWKEY
    else
      result := ID_YELLOWCARD;
    ret := result;
    exit;
  end;

  result := 0;
  ret := 0;
end;

// Remove a vertex from the level.  Frees the memory, but
// doesn't do anything about linedefs nor nothin'.
procedure delete_vertex(const l: level_p; const v: vertex_p);
var
  v1: vertex_p;
begin
  if v = l^.vertex_anchor then
    l^.vertex_anchor := v^.next
  else
  begin
    v1 := l^.vertex_anchor;
    while v1 <> nil do
    begin
      if v1^.next = v then
      begin
        v1^.next := v^.next;
        break;
      end;
      v1 := v1^.next;
    end;
  end;
  SL_Free(v); // oh, that'll help a lot, eh?
end;

// Add a vertex to the given level at the given place.  Return it.
function new_vertex(const l: level_p; const x, y: integer): vertex_p;
begin
  result := vertex_p(SL_Malloc(SizeOf(vertex_t)));
  result^.x := x;
  result^.y := y;
  result^.marked := false;
  result^.next := l^.vertex_anchor;
  l^.vertex_anchor := result;
end;

// Remove a linedef from the level.  Frees the memory, but
// doesn't do anything about sidedefs nor nothin'.
procedure delete_linedef(const l: level_p; const ld: linedef_p);
var
  ld1: linedef_p;
begin
  if ld = l^.linedef_anchor then
    l^.linedef_anchor := ld^.next
  else
  begin
    ld1 := l^.linedef_anchor;
    while ld1 <> nil do
    begin
      if ld1^.next = ld then
      begin
        ld1^.next := ld^.next;
        break;
      end;
      ld1 := ld1^.next;
    end;
  end;
  SL_Free(ld);  // ooohhh, look, he freed something!
end;

// Add a linedef to the given level between the given vertexes.  No
// sidedefs or anything are filled in.
function new_linedef(const l: level_p; const from, _to: vertex_p): linedef_p;
begin
  result := linedef_p(SL_Malloc(SizeOf(linedef_t)));
  result^.from := from;
  result^._to := _to;
  result^.flags := 0;
  result^.typ := LINEDEF_NORMAL;
  result^.tag := 0;
  result^.left := nil;
  result^.right := nil;
  result^.group_next := nil;
  result^.group_previous := nil;
  result^.next := l^.linedef_anchor;
  result^.marked := false;
  l^.linedef_anchor := result;
end;

// Return a new sector for the given level
function new_sector(const l: level_p; const fh, ch: SmallInt; const ft, ct: flat_p): sector_p;
begin
  if ft = nil then
    announce(WARNING, 'Null floor flat in new_sector.');
  if ct = nil then
    announce(WARNING, 'Null ceiling flat in new_sector.');
  result := sector_p(SL_Malloc(SizeOf(sector_t)));
  result^.floor_height := fh;
  result^.ceiling_height := ch;
  result^.floor_flat := ft;
  result^.ceiling_flat := ct;
  result^.light_level := 0;
  result^.special := 0;
  result^.tag := 0;
  result^.marked := false;
  result^.style := nil;
  result^.entry_x := 0;
  result^.entry_y := 0;
  result^.findrec_data_valid := FALSE;
  result^.has_key := FALSE;
  result^.has_dm := FALSE;
  result^.has_dm_weapon := FALSE;
  result^.middle_enhanced := FALSE;
  result^.gate := nil;
  result^.next := l^.sector_anchor;
  l^.sector_anchor := result;
end;

// Return a new sector just like the old sector (mostly)
function clone_sector(const l: level_p; const s: sector_p): sector_p;
begin
  result := new_sector(l, s^.floor_height, s^.ceiling_height,
                          s^.floor_flat, s^.ceiling_flat);
  result^.style := s^.style;
  result^.light_level := s^.light_level;
end;

// A new sidedef, similarly, with sensible defaults
function new_sidedef(const l: level_p; const s: sector_p; const c: config_p): sidedef_p;
begin
  if s = nil then
    announce(ERROR, 'Null sector passed to new_sidedef!');
  result := sidedef_p(SL_Malloc(SizeOf(sidedef_t)));
  result^.x_offset := 0;
  result^.x_misalign := 0;
  result^.y_offset := 0;
  result^.y_misalign := 0;
  result^.upper_texture := c^.null_texture;
  result^.lower_texture := c^.null_texture;
  result^.middle_texture := c^.error_texture;
  result^.sector := s;
  result^.isBoundary := true; // Do we use this sensibly?
  result^.next := l^.sidedef_anchor;
  l^.sidedef_anchor := result;
end;

// Put down a new thing as given
function new_thing(const l: level_p; const x, y: integer; const angle: SmallInt;
                   const typ: SmallInt; const options: SmallInt; const c: config_p): thing_p;
begin
  if typ = ID_ELEC then announce(VERBOSE, 'Tech column');
  if typ = ID_CBRA then announce(VERBOSE, 'Candelabra');
  if typ = ID_LAMP2 then announce(VERBOSE, 'Lamp2');
  if typ = ID_TLAMP2 then announce(VERBOSE, 'Tlamp2');
  if typ = ID_LAMP then announce(VERBOSE, 'Lamp');
  result := thing_p(SL_Malloc(SizeOf(thing_t)));
  result^.x := SmallInt(x);
  result^.y := SmallInt(y);
  result^.angle := angle;
  result^.genus := find_genus(c, typ);
  result^.options := options;
  result^.next := l^.thing_anchor;
  l^.thing_anchor := result;
end;

// Return a new arena approprite for the level.
function new_arena(const l: level_p; const c: config_p): arena_p;
var
  bossno: integer;
begin
  result := arena_p(SL_Malloc(SizeOf(arena_t)));

  result^.boss_count := 1; // Default

  bossno := 0;
  if c^.mission = 8 then // Do episode-ends canonically
  begin
    if c^.episode = 1 then
      bossno := 0
    else if c^.episode = 2 then
      bossno := 1
    else
      bossno := 2;
  end
  else if c^.map = 7 then
    bossno := 3
  else if c^.map = 30 then  // Including the end of DooM II, eventually
    bossno := 666
  else if c^.map <> 0 then  // Otherwise a random DooM II boss,
    bossno := roll(7)
  else                      // Or a random DooM I boss.
    bossno := roll(3);

  //   How can we configify all the monsters and weapons in here??

  case bossno of
    0:  // Baron Brothers
      begin
        result^.boss := find_genus(c, ID_BARON);
        result^.boss_count := 2;
        if rollpercent(75) then
        begin
          result^.weapon := find_genus(c, ID_LAUNCHER);
          result^.ammo := find_genus(c, ID_ROCKBOX);
        end
        else
        begin
          result^.weapon := find_genus(c, ID_CHAINGUN);
          result^.ammo := find_genus(c, ID_BULBOX);
        end;
      end;
    1:  // Cybie
      begin
        result^.boss := find_genus(c, ID_CYBER);
        if rollpercent(75) then
        begin
          result^.weapon := find_genus(c, ID_LAUNCHER);
          result^.ammo := find_genus(c, ID_ROCKBOX);
        end
        else
        begin
          result^.weapon := find_genus(c, ID_BFG);
          result^.ammo := find_genus(c, ID_CELLPACK);
        end;
      end;
    2:  // Spiderboss
      begin
        result^.boss := find_genus(c, ID_SPIDERBOSS);
        if rollpercent(75) then
        begin
          result^.weapon := find_genus(c, ID_BFG);
          result^.ammo := find_genus(c, ID_CELLPACK);
        end
        else
        begin
          result^.weapon := find_genus(c, ID_LAUNCHER);
          result^.ammo := find_genus(c, ID_ROCKBOX);
        end;
      end;
    3:  // Two mancubi (for MAP07, random)
      begin
        result^.boss := find_genus(c, ID_MANCUB);
        result^.boss_count := 2;
        if rollpercent(75) then
        begin
          result^.weapon := find_genus(c, ID_LAUNCHER);
          result^.ammo := find_genus(c, ID_ROCKBOX);
        end
        else
        begin
          result^.weapon := find_genus(c, ID_PLASMA);
          result^.ammo := find_genus(c, ID_CELLPACK);
        end;
      end;
    4:  // Two pains
      begin
        result^.boss := find_genus(c, ID_PAIN);
        result^.boss_count := 2;
        if rollpercent(50) then
        begin
          result^.weapon := find_genus(c, ID_CHAINGUN);
          result^.ammo := find_genus(c, ID_BULBOX);
        end
        else
        begin
          result^.weapon := find_genus(c, ID_PLASMA);
          result^.ammo := find_genus(c, ID_CELLPACK);
        end;
      end;
    5, 6:
      begin
        if roll(2) = 0 then
          result^.boss := find_genus(c, ID_ARCHIE)
        else
          result^.boss := find_genus(c, ID_ARACH);
        result^.boss_count := 2;
        if rollpercent(75) then
        begin
          result^.weapon := find_genus(c, ID_LAUNCHER);
          result^.ammo := find_genus(c, ID_ROCKBOX);
        end
        else
        begin
          result^.weapon := find_genus(c, ID_PLASMA);
          result^.ammo := find_genus(c, ID_CELLPACK);
        end;
      end;
    666:  // Just what are we going to do here?
      begin
        result^.boss := find_genus(c, ID_BRAIN);
        result^.weapon := find_genus(c, ID_LAUNCHER);
        result^.ammo := find_genus(c, ID_ROCKBOX);
      end;
    else
      announce(ERROR, 'Arena missing a boss?');
  end;

  result^.props := 0;
  if rollpercent(20) then result^.props := result^.props or ARENA_ROOF;
  if rollpercent(20) then result^.props := result^.props or ARENA_PORCH;
  if rollpercent(20) then result^.props := result^.props or ARENA_LAMPS;
  if rollpercent(20) then result^.props := result^.props or ARENA_ARRIVAL_HOLE;
  if rollpercent(10 + l^.p_force_nukage) then result^.props := result^.props or ARENA_NUKAGE;

  if result^.props and ARENA_ROOF <> 0 then
  begin
    result^.floor := random_flat0(SL_FLOOR, c, nil); // These NULLs OK?
    result^.walls := random_texture0(WALL, c, nil);
  end
  else
  begin
    result^.floor := random_flat0(OUTDOOR, c, nil);
    result^.walls := random_texture0(OUTDOOR, c, nil);
  end;
  result^.placed_health := FALSE;
  result^.placed_armor := FALSE;
  result^.placed_ammo := FALSE;
  result^.placed_weapon := FALSE;
  result^.fromtag := 0;
  result^.next := l^.arena_anchor;
  l^.arena_anchor := result;
end;

// Between two points
function distancesquared(const x1, y1, x2, y2: integer): integer;
var
  xd, yd: integer;
begin
  xd := x2 - x1;
  yd := y2 - y1;

  result := xd * xd + yd * yd;
end;

// Between two points, simple DOOM algorithm
function infinity_norm(const x1, y1, x2, y2: integer): integer;
var
  xd, yd: integer;
begin
  xd := abs(x2 - x1);
  yd := abs(y2 - y1);

  if xd > yd then
   result := xd
  else
    result := yd;
end;

// length-sqaured of a linedef
function lengthsquared(const ld: linedef_p): integer;
var
  xd, yd: integer;
begin
  xd := ld^._to^.x - ld^.from^.x;
  yd := ld^._to^.y - ld^.from^.y;
  result := xd * xd + yd * yd;
end;

// Return a quest for the very start of the game; always level-end,
// consult the config for length and stuff.
function starting_quest(const l: level_p; const c: config_p): quest_p;
begin
  result := quest_p(SL_Malloc(SizeOf(quest_t)));
  result^.goal := LEVEL_END_GOAL;
  result^.room := nil; // won't be used, because this is always stack bottom
  result^.tag := 0;    // not a linedef goal
  result^.typ := LINEDEF_S1_END_LEVEL;
  result^.count := 0;   // no rooms yet!
  result^.minrooms := c^.minrooms;
  result^.auxtag := 0;
  result^.thing := nil;
  result^.surprise := nil;
  result^.next := nil;   // only one so far
end;

// Return a health/armor/ammo estimate for the game start
function starting_haa: haa_p;
var
  i: integer;
begin
  result := haa_p(SL_Malloc(SizeOf(haa_t)));

  for i :=ITYTD to UV do
  begin
    result^.haas[i].health := 100.0;
    result^.haas[i].ammo := 500.0;
    result^.haas[i].armor := 0.0;
    result^.haas[i].can_use_shells := false;
    result^.haas[i].shells_pending := false;
    result^.haas[i].has_chaingun := false;
    result^.haas[i].chaingun_pending := false;
    result^.haas[i].has_chainsaw := false;
    result^.haas[i].has_backpack := false;
    result^.haas[i].has_berserk := false;
    result^.haas[i].has_ssgun := false;
    result^.haas[i].can_use_rockets := false;
    result^.haas[i].can_use_cells := false;
  end;
end;

// Mark each boundary linedef from the given sector which isn't
// already in obvious use, and which is at least minlen long.
function mark_decent_boundary_linedefs(const l: level_p; const s: sector_p; const minlen: integer): integer;
var
  ld: linedef_p;
  sd: sidedef_p;
  minlensq: integer;
begin
  result := 0;
  minlensq := minlen * minlen;

  ld := l^.linedef_anchor;
  while ld <> nil do
  begin
    if ld^.left <> nil then
    begin
      ld := ld^.next;
      continue;
    end;
    if ld^.typ <> 0 then
    begin
      ld := ld^.next;
      continue;
    end;
    sd := ld^.right;
    if sd <> nil then
      if sd^.sector = s then
        if sd^.isBoundary then
          if ld^.typ = 0 then // JVAL Unused
            if lengthsquared(ld) >= minlensq then
            begin
              ld^.marked := true;
              inc(result);
            end;
    ld := ld^.next;
  end;
end;

// Look at each linedef out of the given sector.  Mark it if it's
// reasonable to consider putting a room on the other side of it.
// Return the number of linedefs marked.
function mark_adequate_linedefs(const l: level_p; const s: sector_p; const ThisStyle:
  style_p; const c: config_p): integer;
var
  ld: linedef_p;
  sd: sidedef_p;
begin
  result := 0;

  ld := l^.linedef_anchor;
  while ld <> nil do
  begin
    sd := ld^.right;
    if sd <> nil then
      if sd^.sector = s then
        if sd^.isBoundary then
          if isAdequate(l, ld, ThisStyle, c) then
          begin
            ld^.marked := true;
            inc(result);
          end;
    ld := ld^.next;
  end;
end;

// Given that there are i marked linedefs, return a
// random one of them.  nil if zero.
function random_marked_linedef(const l: level_p; const i: integer): linedef_p;
var
  ld: linedef_p;
  ii: integer;
begin
  if i = 0 then
  begin
    result := nil;
    exit;
  end;

  ii := roll(i);

  ld := l^.linedef_anchor;
  while ld <> nil do
  begin
    if ld^.marked then
    begin
      if ii = 0 then
      begin
        result := ld;
        exit;
      end;
      dec(ii);
    end;
    ld := ld^.next;
  end;

  // Gosh, I *hope* we never get here...
  announce(ERROR, 'Not enough marked linedefs!');
  result := nil;
end;

// Reset all the linedef marks
procedure unmark_linedefs(const l: level_p);
var
  ld: linedef_p;
begin
  ld := l^.linedef_anchor;
  while ld <> nil do
  begin
     ld^.marked := false;
     ld := ld^.next;
  end;
end;

var
  sl_seed: LongWord = 0;

function RandRangeInt(const Range: LongWord): LongWord;
var
  NewSeed: LongWord;
begin
  NewSeed := sl_seed * $08088405 + 1;
  sl_seed := NewSeed;
  Result := NewSeed; // (Int64(sl_seed) * Int64(Range)) shr 32;
end;

function bigrand: integer;
begin
  result := RandRangeInt(2147483647);
  if result < 0 then
    result := -result;
end;

// Roll a zero-origin n-sided die
function roll(const zero_to_this_minus_one: integer): integer;
begin
  assert(ok_to_roll);
  if zero_to_this_minus_one < 1 then
  begin
    result := 0;
    exit;
  end;
  if zero_to_this_minus_one > 134217727 then
    result := bigrand mod zero_to_this_minus_one
  else if zero_to_this_minus_one > 33554431 then
    result := (bigrand div 4) mod zero_to_this_minus_one
  else
    result := (bigrand div 16) mod zero_to_this_minus_one;
  if result < 0 then
    result := -result;
end;

// Return 1 n percent of the time, else 0
function rollpercent(const percent: integer): boolean;
begin
  result := roll(100) < percent;
end;

function psi_sqrt(const v: integer): SmallInt;
begin
  result := round(sqrt(v));
end;

// Find a flat with the given name, creating one if
// it doesn't already exist.
function find_flat(const c: config_p; const name: string): flat_p;
var
  t: flat_p;
begin
  t := c^.flat_anchor;
  while t <> nil do
  begin
    if SameStrings(name, t^.name) then
    begin
      result := t;
      exit;
    end;
    t := t^.next;
  end;
  result := new_flat(c, name);
end;

function new_flat(const c: config_p; const name: string): flat_p;
begin
  result := flat_p(SL_Malloc(SizeOf(flat_t)));
  result^.name := UpperCase(name);
  result^.gamemask := DOOM0_BIT or DOOM1_BIT or DOOM2_BIT or DOOMC_BIT or DOOMI_BIT;
  result^.compatible := 0;
  result^.props := 0;
  result^.used := FALSE;
  result^.next := c^.flat_anchor;
  c^.flat_anchor := result;
end;

// Return a new gate with the given attributes and stuff
function new_gate(const l: level_p; const intag, outtag: SmallInt; const lock: SmallInt;
  const entry: boolean; const c: config_p): gate_p;
begin
  result := gate_p(SL_Malloc(SizeOf(gate_t)));
  result^.in_tag := intag;
  result^.out_tag := outtag;
  result^.gate_lock := lock;
  result^.is_entry := entry;
  result^.next := l^.gate_anchor;
  l^.gate_anchor := result;
end;

// Return a new theme with the given name and secretness.  Non-secret
// themes go at the start of the list, secret ones at the end
function new_theme(const c: config_p; const name: string; const secret: boolean): theme_p;
var
  t: theme_p;
begin
  result := theme_p(SL_Malloc(SizeOf(theme_t)));

  result^.name := name;
  result^.secret := secret;
  if not secret then // Stick it at the end of the non-secrets
  begin
    inc(c^.themecount);
    if (c^.theme_anchor = nil) or c^.theme_anchor^.secret then
    begin
      result^.next := c^.theme_anchor;
      c^.theme_anchor := result;
    end
    else
    begin
      t := c^.theme_anchor;
      while (t^.next <> nil) and not t^.next^.secret do
        t := t^.next;
      result^.next := t^.next;
      t^.next := result;
    end;
  end
  else // Stick it at the very end
  begin
    inc(c^.sthemecount);
    result^.next := nil;
    if c^.theme_anchor <> nil then
    begin
      t := c^.theme_anchor;
      while t^.next <> nil do t := t^.next;
      t^.next := result;
    end
    else
      c^.theme_anchor := result;
  end;
end;

// Return a new monster-class with the given thingid
function new_monster(const c: config_p; const thingid: integer): genus_p;
var
  i: integer;
begin
  result := new_genus(c, thingid);
  result^.bits := result^.bits or MONSTER;
  result^.bits := result^.bits and not PICKABLE; // Can't pick up a monster!
  for i := 0 to 2 do
  begin
    result^.ammo_to_kill[i] := 1000.0; // Any reason to have defaults?
    result^.damage[i] := 1000.0;
    result^.altdamage[i] := 1000.0;
  end;
  result^.ammo_provides := 0.0;
end;

// Return a new genus with the given thingid
function new_genus(const c: config_p; const thingid: integer): genus_p;
begin
  result := genus_p(SL_Malloc(SizeOf(genus_t)));
  // Default mask
  result^.gamemask := DOOM0_BIT or DOOM1_BIT or DOOM2_BIT or DOOMC_BIT or DOOMI_BIT;
  result^.compatible := ALL_THEMES; // Assume all themes OK
  result^.thingid := thingid;
  result^.width := 65; // Sort of sensible default
  result^.height := 56; // Just "not tall"
  result^.marked := false;
  result^.next := c^.genus_anchor;
  result^.bits := PICKABLE; // A plausible default?
  c^.genus_anchor := result;
end;

// Return a monster-class with the given thingid,
// creating it first if needed.
function find_monster(const c: config_p; const thingid: integer): genus_p;
var
  g: genus_p;
begin
  g := c^.genus_anchor;
  while g <> nil do
  begin
    if g^.thingid = thingid then
    begin
      result := g;
      exit;
    end;
    g := g^.next;
  end;
  result := new_monster(c, thingid);
end;

// Return a thing-class with the given thingid,
// creating it first if needed.
function find_genus(const c: config_p; const thingid: integer): genus_p;
var
  g: genus_p;
begin
  g := c^.genus_anchor;
  while g <> nil do
  begin
    if g^.thingid = thingid then
    begin
      result := g;
      exit;
    end;
    g := g^.next;
  end;
  result := new_genus(c, thingid);
end;

// Find a texture with the given name, creating one if
// it doesn't already exist.
function find_texture(const c: config_p; const name: string): texture_p;
var
  t: texture_p;
begin
  t := c^.texture_anchor;
  while t <> nil do
  begin
    if SameStrings(name, t^.name) then
    begin
      result := t;
      exit;
    end;
    t := t^.next;
  end;
  result := new_texture(c, name);
end;

// Return a new texture with the given name
function new_texture(const c: config_p; const name: string): texture_p;
begin
  result := texture_p(SL_Malloc(SizeOf(texture_t)));
  result^.name := name;
  result^.realname := name;
  result^.gamemask := DOOM0_BIT or DOOM1_BIT or DOOM2_BIT or DOOMC_BIT or DOOMI_BIT;
  result^.compatible := 0;
  result^.core := 0;
  result^.props := 0;    // Filled in later
  result^.width := 256;  // Or some fraction thereof!
  result^.height := 128; // Except sometimes
  result^.y_hint := 5;
  result^.y_bias := 0;
  result^.subtle := nil;
  result^.switch_texture := nil;
  result^.used := FALSE;
  result^.next := c^.texture_anchor;
  c^.texture_anchor := result;
end;

// Split the given linedef at the given distance along it.
// Return the new (after-splitpoint) linedef.
function split_linedef(const l: level_p; const ld: linedef_p; const len: integer;
  const c: config_p): linedef_p;
var
  ratio: double;
  dx, dy: integer;
  v: vertex_p;
begin
  assert(len > 0);

  ratio := len / linelen(ld);
  dx := round(ratio * (ld^._to^.x - ld^.from^.x));
  dy := round(ratio * (ld^._to^.y - ld^.from^.y));
  v := new_vertex(l, ld^.from^.x + dx, ld^.from^.y + dy);
  result := new_linedef(l, v, ld^._to);
  ld^._to := v;
  result^.flags := ld^.flags;
  result^.typ := ld^.typ;
  result^.tag := ld^.tag;
  result^.group_previous := ld;
  result^.group_next := ld^.group_next;
  if result^.group_next <> nil then
    result^.group_next^.group_previous := result;
  ld^.group_next := result;

  if ld^.right <> nil then
  begin
    result^.right := new_sidedef(l, ld^.right^.sector, c);
    result^.right^.x_offset := ld^.right^.x_offset + len;
    result^.right^.y_offset := ld^.right^.y_offset;
    result^.right^.upper_texture := ld^.right^.upper_texture;
    result^.right^.lower_texture := ld^.right^.lower_texture;
    result^.right^.middle_texture := ld^.right^.middle_texture;
    result^.right^.isBoundary := ld^.right^.isBoundary;
  end
  else
    result^.right := nil;

  if ld^.left <> nil then
  begin
    result^.left := new_sidedef(l, ld^.left^.sector, c);
    result^.left^.x_offset := ld^.left^.x_offset + len; // is that right?
    result^.left^.y_offset := ld^.left^.y_offset;
    result^.left^.upper_texture := ld^.left^.upper_texture;
    result^.left^.lower_texture := ld^.left^.lower_texture;
    result^.left^.middle_texture := ld^.left^.middle_texture;
    result^.left^.isBoundary := ld^.left^.isBoundary;
  end
  else
    result^.left := nil;

  result^.marked := ld^.marked;  // I suppose
end;

// Put in any upper textures required
procedure patch_upper(const ld: linedef_p; const t: texture_p; const c: config_p);
begin
  if ld^.left = nil then
    exit;
  if ld^.right^.sector^.ceiling_height > ld^.left^.sector^.ceiling_height then
  begin
    if (ld^.right^.upper_texture = nil) or
       (Pos('-', ld^.right^.upper_texture^.name) = 1) then
    begin
      ld^.right^.upper_texture := t;
      ld^.flags := ld^.flags or UPPER_UNPEGGED; // Seems a good default
    end;
  end;

  if (ld^.left^.sector^.ceiling_height > ld^.right^.sector^.ceiling_height) then
  begin
    if (ld^.left^.upper_texture = nil) or
       (Pos('-', ld^.left^.upper_texture^.name) = 1) then
    begin
      ld^.left^.upper_texture := t;
      ld^.flags := ld^.flags or UPPER_UNPEGGED; // Seems a good default
    end;
  end;
end;

// Put in any lower textures required
procedure patch_lower(const ld: linedef_p; const t: texture_p; const c: config_p);
begin
  if ld^.left = nil then
    exit;
  if ld^.right^.sector^.floor_height < ld^.left^.sector^.floor_height then
  begin
    if (ld^.right^.lower_texture = nil) or
       (Pos('-', ld^.right^.lower_texture^.name) = 1) then
    begin
      ld^.right^.lower_texture := t;
      ld^.flags := ld^.flags or LOWER_UNPEGGED; // Seems a good default
    end;
  end;
  if ld^.left^.sector^.floor_height < ld^.right^.sector^.floor_height then
  begin
    if (ld^.left^.lower_texture = nil) or
       (Pos('-', ld^.left^.lower_texture^.name) = 1) then
    begin
      ld^.left^.lower_texture := t;
      ld^.flags := ld^.flags or LOWER_UNPEGGED; // Seems a good default
    end;
  end;
end;

// Flip the linedef end-for-end
function flip_linedef(const ld: linedef_p): linedef_p;
var
  sd: sidedef_p;
  v: vertex_p;
begin
  v := ld^.from;
  sd := ld^.left;

  ld^.from := ld^._to;
  ld^.left := ld^.right;

  ld^._to := v;
  ld^.right := sd;

  result := ld;
end;

procedure Usage0;
begin
  printf('Usage: radge [switches] [filename.ext] [switches]'#13#10);
  printf('Produces a (nodeless) PWAD file that can be completed by a'#13#10);
  printf('nodebuilder such as GLBSP, and then played using the -file'#13#10);
  printf('function of DOOM (or DOOM2).  The default output file is'#13#10);
  printf('RADGE.OUT.  Gets all sorts of data and options and stuff from'#13#10);
  printf('RADGE.CFG (or other file given with the -config switch.)'#13#10#13#10);
end;

procedure Usage;
begin
  Usage0;
  printf('For details, say "radge -?".'#13#10#13#10);
end;

// Remove anything from the config that would be dangerous if left
// in, and optionally remove anything that just benignly won't ever
// be used and might take up valuable time and space.
procedure compact_config(const c: config_p);
var
  t: texture_p;
begin
  // NULLify any texture subtles that aren't in this DOOM version
  t := c^.texture_anchor;
  while t <> nil do
  begin
    if t^.subtle <> nil then
      if (t^.subtle^.gamemask and c^.gamemask) <> c^.gamemask then
        t^.subtle := nil;
    t := t^.next;
  end;

  // Now we *could* also remove from the config all textures and flats
  // and monsters and stuff that aren't in this DOOM, and then skip the
  // checks later on.  Someday...
end;

// Alter this config to be good and strange for a secret
// level.  Add stuff to this over time!
procedure secretize_config(const c: config_p);
var
  something_special: boolean;
begin
  something_special := FALSE;

  c^.minrooms := (c^.minrooms * 2) div 3;
  if c^.minrooms < 8 then c^.minrooms := 8;
  if c^.minrooms > 20 then c^.minrooms := 20;
  c^.allow_boring_rooms := FALSE;
  c^.lock_themes := TRUE;
  if rollpercent(25) then c^.force_biggest := TRUE; // stub
  c^.big_monsters := TRUE;

  while not something_special do
  begin
    // Sometimes bizarre theme
    if rollpercent(30) then
    begin
      c^.secret_themes := TRUE;
      something_special := TRUE;
      announce(VERBOSE, 'Bizarre theme');
    end;

    // Sometimes lots and lots of nukage
    if rollpercent(30) then
    begin
      c^.major_nukage := TRUE;
      something_special := TRUE;
      announce(VERBOSE, 'Nukage everywhere');
    end;

    // Sometimes some DooM II nazis
    if rollpercent(80) and (c^.gamemask and (DOOM0_BIT or DOOM1_BIT) = 0) then
    begin
      c^.forbidden_monster_bits := c^.forbidden_monster_bits and not SPECIAL;
      something_special := TRUE;
      if rollpercent(50) then
      begin
        c^.required_monster_bits := c^.required_monster_bits or SPECIAL;
        c^.required_monster_bits := c^.required_monster_bits and not BIG;
        announce(VERBOSE, 'All nazis');
      end
      else
        announce(VERBOSE, 'Some nazis');
     end;

    // Sometimes some monster thing
    if rollpercent(30) and not something_special then
    begin
      if rollpercent(50) then
      begin
        c^.required_monster_bits := c^.required_monster_bits or BIG;
        c^.required_monster_bits := c^.required_monster_bits and not SPECIAL;
        c^.big_monsters := TRUE;
        announce(VERBOSE, 'All big monsters');
        something_special := TRUE;
      end
      else
        // Put in a favorite monster here!
        // and set something_special
        announce(VERBOSE, 'Someday a favorite monster');
    end;

  end; // end while
end;

const
  SLIGE_CONFIG = 'RADGECONFIG';

// Get the configuration data, switches, etc.  Five steps:
// 1. Parse the arglist to find out where the config file is,
// 2. Read through the config file to get other switches,
// 3. Parse the arglist to get overrides to switches,
// 4. Read the config for non-switches (flats, themes, etc).
// 5. Do postproduction defaults and calculations and such.
var
  static_c: config_t;

function get_config(const args: TDStringList): config_p;
var
  i: integer;
  m: genus_p;
begin
  result := @static_c;

  // Set various defaults and stuff
  result^.configfile := getenv(SLIGE_CONFIG);
  if result^.configfile = '' then
    result^.configfile := 'radge.cfg';
  result^.configdata:= '';
  result^.outfile := '';
  result^.cwadonly := FALSE;
  sl_seed := round(Now() * 1000000);
  for i := 1 to 20 do bigrand;
  result^.ranseed := bigrand;

  // Do initial parsing for possible other config file and ranseed
  if not do_switches(args, result, 'Command line', 1) then
  begin
    result := nil;
    exit;
  end;

  ok_to_roll := TRUE;

  // Set other defaults, possibly involving random numbers
  result^.theme_anchor := nil;
  result^.flat_anchor := nil;
  result^.texture_anchor := nil;
  result^.construct_anchor := nil;
  result^.genus_anchor := nil;
  result^.null_texture := nil;
  result^.error_texture := nil;
  result^.gate_exitsign_texture := nil;
  result^.sky_flat := nil;
  result^.themecount := 0;
  result^.sthemecount := 0;
  result^.secret_themes := FALSE;
  result^.lock_themes := FALSE;
  result^.major_nukage := FALSE;
  result^.required_monster_bits := 0;
  result^.forbidden_monster_bits := SPECIAL;
  result^.minrooms := 18;
  result^.gamemask := DOOM1_BIT; // All/Only things supported by DOOM 1.9
  result^.episode := 1;
  result^.mission := 1;
  result^.last_mission := FALSE;
  result^.levelcount := 1;
  result^.force_arena := FALSE;
  result^.force_biggest := FALSE;
  result^.do_music := false;
  result^.secret_monsters := TRUE;
  result^.do_dm := false;
  result^.do_slinfo := TRUE;
  result^.produce_null_lmps := FALSE;
  result^.do_seclevels := TRUE;
  result^.force_secret := FALSE;
  result^.fcontrol := FALSE;
  result^.fstart := FALSE;
  result^.ffstart := FALSE;
  result^.ffend := FALSE;
  result^.fend := FALSE;
  result^.map := 0;
  result^.minlight := 115;
  result^.minhuge := 1;
  // Is this the right place for all these?
  result^.immediate_monsters := rollpercent(10);
  result^.p_hole_ends_level := 0;
  if rollpercent(8) then result^.p_hole_ends_level := 100;
  if rollpercent(3) then result^.p_hole_ends_level := roll(100);
  // These should depend on lastness of the level in the PWAD?
  result^.p_gate_ends_level := 0;
  if rollpercent(8) then result^.p_gate_ends_level := 100;
  if rollpercent(3) then result^.p_gate_ends_level := roll(100);
  result^.p_use_steps := 100;
  if rollpercent(5) then result^.p_use_steps := roll(100);
  result^.p_sync_doors := 10;
  if rollpercent(50) then result^.p_sync_doors := 100;
  if rollpercent(5) then result^.p_sync_doors := roll(100);
  result^.p_grid_gaps := 0;
  if rollpercent(40) then result^.p_grid_gaps := 1;
  if rollpercent(10) then result^.p_grid_gaps := roll(20);
  result^.p_pushquest := 10;
  if rollpercent(40) then result^.p_pushquest := 50;
  if rollpercent(10) then result^.p_pushquest := roll(90);
  result^.rad_newtheme := 100;
  result^.norm_newtheme := 0;
  result^.rad_vary := 100;
  result^.norm_vary := 25;
  if rollpercent(15) then // Some older settings
  begin
    announce(VERBOSE, 'Old themeing');
    result^.rad_newtheme := 12;
    result^.norm_newtheme := 4;
    result^.rad_vary := 60;
    result^.norm_vary := 20;
  end;
  if rollpercent(15) then // Sometimes never change themes!
  begin
    announce(VERBOSE, 'One theme');
    result^.rad_newtheme := 0;
    result^.norm_newtheme := 0;
    result^.rad_vary := 100; // But change other stuff more
    result^.norm_vary := 60;
  end;
  result^.monsters_can_teleport := TRUE;
  if rollpercent(25) then result^.monsters_can_teleport := FALSE;
  result^.window_airshafts := rollpercent(50);
  result^.homogenize_monsters := 0;
  if rollpercent(8) then result^.homogenize_monsters := 90;
  if rollpercent(15) then result^.homogenize_monsters := roll(100);

  announce(VERBOSE, Format('Homogenization %d.', [result^.homogenize_monsters]));

  result^.weapons_are_special := FALSE;  // Just mix weapons in with ammo
  result^.recess_switches := rollpercent(95);
  result^.allow_boring_rooms := rollpercent(20);
  result^.both_doors := rollpercent(50);
  result^.doorless_jambs := rollpercent(10);
  result^.gunk_channels := rollpercent(70);
  result^.clights := rollpercent(50);
  result^.machoh := 1.0;
  result^.machou := 1.0;
  result^.p_bigify := roll(100); // Less uniform?

  // Initial defaults; at each level, some chance of turning on
  result^.big_weapons := rollpercent(50);
  if result^.big_weapons then
    result^.big_monsters := rollpercent(80)
  else
    result^.big_monsters := rollpercent(35);

  // Open or whatever the config file
  load_config(result);

  // Read switch-defaults from the config file
  if not read_switches(result) then
  begin
    result := nil;
    exit;
  end;

  // Now scan the command line again
  if not do_switches(args, result, 'Command line', 0) then
  begin
    result := nil;
    exit;
  end;

  // And finally read in all the hard stuff from the file
  if not nonswitch_config(result) then
  begin
    result := nil;
    exit;
  end;

  // Close/free the config stuff
  unload_config(result);

  // Then we set some final defaulty stuff
  if result^.outfile = '' then
    result^.outfile := 'radge.out';
  if result^.error_texture = nil then // Use REDWALL if none specified
    result^.error_texture := find_texture(result, 'REDWALL'); // OK default?
  if result^.sky_flat = nil then
    result^.sky_flat := find_flat(result, 'F_SKY1'); // Default
  if result^.water_flat = nil then
    result^.water_flat := find_flat(result, 'FWATER1'); // Default
  if result^.null_texture = nil then
    result^.null_texture := find_texture(result, '-'); // Always, really...
  if not result^.fcontrol then // The normal DOOM engine requires...
  begin
    result^.fstart := FALSE;
    result^.ffstart := TRUE;
    result^.ffend := TRUE;
    result^.fend := TRUE;
  end;

  // And figure some resultants
  m := result^.genus_anchor;
  while m <> nil do // Apply macho factors
  begin
    if m^.bits and MONSTER <> 0 then
    begin
      m^.ammo_to_kill[HMP] := m^.ammo_to_kill[HMP] * result^.machoh;
      m^.damage[HMP] := m^.damage[HMP] * result^.machoh;
      m^.altdamage[HMP] := m^.altdamage[HMP] * result^.machoh;
      m^.ammo_to_kill[UV] := m^.ammo_to_kill[UV] * result^.machou;
      m^.damage[UV] := m^.damage[UV] * result^.machou;
      m^.altdamage[UV] := m^.altdamage[UV] * result^.machou;
    end;
    m := m^.next;
  end;

  if result^.force_secret then secretize_config(result);

  // And finally compact out any unneeded/dangerous stuff
  compact_config(result);
end;

function make_watermark_path(const l: level_p; const v1, v2: vertex_p;
                             const rsd, lsd: sidedef_p): vertex_p;
var
  ld: linedef_p;
begin
  ld := new_linedef(l, v1, v2);
  ld^.flags := TWO_SIDED;
  ld^.left := lsd;
  ld^.right := rsd;
  result := v2;
end;

procedure watermark_sector(const l: level_p; const s: sector_p; const ThisStyle: style_p; const c: config_p);
// This isn't a stub except that find_sector_rectangle is
// Well, and it's not clear what you'd do with a
// non-rectangular sector
var
  minx, miny, maxx, maxy: integer;
  x1, x2, x3, x4: integer;
  y1, y2, y3, y4, y5, y6: integer;
  newsector:  sector_p;
  lsd, rsd: sidedef_p;
  v0, v1: vertex_p;
begin
  // Make a new sector for the S-shape
  newsector := new_sector(l, s^.floor_height, s^.ceiling_height + 16,
                          s^.floor_flat, c^.sky_flat);
  newsector^.light_level := l^.outside_light_level;
  newsector^.style := ThisStyle;

  // Figure out the relevant grid-lines
  find_sector_rectangle(l, s, minx, miny, maxx, maxy);
  x1 := minx + 4;
  x4 := maxx - 4;
  y6 := miny + 4;
  y1 := maxy - 4;
  x2 := x1 + (x4 - x1) div 3;
  x3 := x2 + (x4 - x1) div 3;
  y5 := y6 + (y1 - y6) div 5;
  y4 := y5 + (y1 - y6) div 5;
  y3 := y4 + (y1 - y6) div 5;
  y2 := y3 + (y1 - y6) div 5;

  // Make the two sidedefs that the linedefs will share
  rsd := new_sidedef(l, newsector, c);
  rsd^.middle_texture := c^.null_texture;
  rsd^.upper_texture := ThisStyle^.wall0;
  rsd^.isBoundary := FALSE;
  lsd := new_sidedef(l, s, c);
  lsd^.middle_texture := c^.null_texture;
  lsd^.isBoundary := FALSE;

  // Now make a whole buncha linedefs
  v0 := new_vertex(l, x1, y1);
  v1 := make_watermark_path(l, v0, new_vertex(l, x4, y1), rsd, lsd);
  v1 := make_watermark_path(l, v1, new_vertex(l, x4, y2), rsd, lsd);
  v1 := make_watermark_path(l, v1, new_vertex(l, x2, y2), rsd, lsd);
  v1 := make_watermark_path(l, v1, new_vertex(l, x2, y3), rsd, lsd);
  v1 := make_watermark_path(l, v1, new_vertex(l, x4, y3), rsd, lsd);
  v1 := make_watermark_path(l, v1, new_vertex(l, x4, y6), rsd, lsd);
  v1 := make_watermark_path(l, v1, new_vertex(l, x1, y6), rsd, lsd);
  v1 := make_watermark_path(l, v1, new_vertex(l, x1, y5), rsd, lsd);
  v1 := make_watermark_path(l, v1, new_vertex(l, x3, y5), rsd, lsd);
  v1 := make_watermark_path(l, v1, new_vertex(l, x3, y4), rsd, lsd);
  v1 := make_watermark_path(l, v1, new_vertex(l, x1, y4), rsd, lsd);
  v1 := make_watermark_path(l, v1, v0,rsd,lsd);
end; // end watermark_sector()

// Do segments AB and CD intersect?  Algorithm from the Net
function intersects(const XA, YA, XB, YB: integer;
                    const XC, YC, XD, YD: integer): boolean;
var
  r, s: double;
  r_top, s_top, bottom: integer;
begin
  bottom := (XB - XA) * (YD - YC) - (YB - YA) * (XD - XC);
  r_top := (YA - YC) * (XD - XC) - (XA - XC) * (YD - YC);

  if bottom = 0 then // parallel
  begin
    if r_top <> 0 then
      result := false  // proper parallel
    else               // colinear; hard case
      result := false; // This is wrong, of course!  But rarely...
    exit;
  end;

  result := false;
  s_top := (YA - YC) * (XB - XA) - (XA - XC) * (YB - YA);
  r := r_top / bottom;
  s := s_top / bottom;
  if r < 0 then exit;
  if r > 1 then exit;
  if s < 0 then exit;
  if s > 1 then exit;
  result := true;
end;

// Fix up any obvious HOMs with an obvious error texture.
// Note that you can still get HOMs with lifts and other
// floors and ceilings that move during play.
procedure global_paint_HOMs(const l: level_p; const c: config_p);
var
  ld: linedef_p;
begin
  ld := l^.linedef_anchor;
  while ld <> nil do
  begin
    if ld^.right <> nil then
      if ld^.left <> nil then
      begin
        patch_upper(ld, c^.error_texture, c);
        patch_lower(ld, c^.error_texture, c);
      end;
    ld := ld^.next;
  end;
end;

// Return the (int number of) a random theme that exists in
//* the given configuration.  Need anything fancier here?
function random_theme(const c: config_p): integer;
begin
  if c^.secret_themes then
    result := c^.themecount + roll(c^.sthemecount)
  else
    result := roll(c^.themecount);
  announce(VERBOSE, Format('Theme %d.', [result]));
end;

// Need anything fancier in these next few things?

// Linedef type for an ordinary inter-room non-secret door
function random_doortype(const l: level_p; const c: config_p; const s: style_p): SmallInt;
begin
  if rollpercent(l^.p_s1_door) then
    result := LINEDEF_NORMAL_S1_DOOR
  else
    result := LINEDEF_NORMAL_DOOR;
  if (DOOM0_BIT and c^.gamemask = 0) and rollpercent(20) then
  begin
    if result = LINEDEF_NORMAL_DOOR then
      result := LINEDEF_BLAZE_DOOR
    else if result = LINEDEF_NORMAL_S1_DOOR then
      result := LINEDEF_BLAZE_S1_DOOR;
    announce(VERBOSE, 'Blaze door type');
  end;
end;

function random_slifttype(const c: config_p; const s: style_p): SmallInt;
begin
  if (DOOM0_BIT and c^.gamemask = 0) and rollpercent(20) then
    result := LINEDEF_SR_TURBO_LIFT
  else
    result := LINEDEF_SR_LOWER_LIFT;
end;

function random_sillheight(const c: config_p; const s: style_p): integer;
begin
  if s^.window_grate then
  begin
    if rollpercent(50) then
      result := 0
    else
      result := 4 * roll(13);
  end
  else
    result := 28 + 4 * roll(6);
end;

function random_windowheight(const c: config_p; const s: style_p): integer;
begin
  if s^.window_grate then
    result := 64 + roll(16) // Does this make any sense?
  else
    result := 16 + 4 * roll(9);
end;

function random_windowborder(const c: config_p; const s: style_p): integer;
begin
  result := 4 + 4 * roll(6);
end;

function random_windowdecor(const c: config_p; const s: style_p): integer;
begin
  case roll(5) of
    0, 1:
      result := WINDOW_NORMAL;
    2:
      result := WINDOW_JAMBS;
    3:
      result := WINDOW_SUPPORT;
    else
      result := WINDOW_LIGHT;
  end;
end;

function random_lightboxlighting(const c: config_p; const s: style_p): integer;
begin
  case roll(4) of
    0:
      result := LIGHTBOX_NORMAL;
    1, 2:
      result := LIGHTBOX_LIGHTED;
    else
      result := LIGHTBOX_DARK;
  end;
end;

// Various plants etc; should be from the config also of course.
function random_plant(const c: config_p; const s: style_p): genus_p;
var
  tcount: integer;
begin
  if c^.gamemask and DOOM1_BIT <> 0 then
    tcount := 3
  else
    tcount := 4;

  case roll(tcount) of
    0:
      begin
        result := find_genus(c, ID_SMIT);
        result^.bits := result^.bits and not PICKABLE;
        result^.width := 33;
      end;
    1:
      begin
        result := find_genus(c, ID_TREE1);
        result^.bits := result^.bits and not PICKABLE;
        result^.width := 33;
      end;
    2:
      begin
        result := find_genus(c, ID_TREE2);
        result^.bits := result^.bits and not PICKABLE;
        result^.width := 65;
      end;
    else
    begin
      result := find_genus(c, ID_FBARREL);
      result^.bits := result^.bits and not PICKABLE;
      result^.width := 33;
    end;
  end;
end;

// Can return nil if there are no explodables that are
// compatible with the theme.
function random_barrel(const c: config_p; const s: style_p): genus_p;
begin
  result := random_thing0(EXPLODES, c, s, 0, 10000);
end;

// A lamp or similar decoration, tall or short
function random_lamp0(const c: config_p; const s: style_p): genus_p;
begin
  result := random_thing0(LIGHT, c, s, 70, 10000);
  if result = nil then
    result := random_thing0(LIGHT, c, s, 0, 10000);
end;

// A lamp or similar decoration, no taller than a player
function random_shortlamp0(const c: config_p; const s: style_p): genus_p;
begin
  result := random_thing0(LIGHT, c, s, 0, 56);
end;

// Return the number of a random construct family that contains at
// least one construct compatible with this style's theme.
function construct_family_for(const c: config_p; const s: style_p): integer;
const
  MAX_COMPATIBLE_FAMILIES = 5;
var
  cs: construct_p;
  tmask: LongWord;
  compats: array[0..MAX_COMPATIBLE_FAMILIES - 1] of integer;
  compat_count: integer;
  already: boolean;
  i: integer;
begin
  for i := 0 to MAX_COMPATIBLE_FAMILIES - 1 do // JVAL
    compats[i] := 0;

  compat_count := 0;
  tmask := $01 shl s^.theme_number;

  cs := c^.construct_anchor;
  while cs <> nil do
  begin
    if (cs^.compatible and tmask = 0) then
    begin
      cs := cs^.next;
      continue;
    end;
    if (cs^.gamemask and c^.gamemask) <> c^.gamemask then
    begin
      cs := cs^.next;
      continue;
    end;
    already := FALSE;
    for i := 0 to compat_count - 1 do
      if compats[i] = cs^.family then already := TRUE;
    if already then
    begin
      cs := cs^.next;
      continue;
    end;
    compats[compat_count] := cs^.family;
    inc(compat_count);
    cs := cs^.next;
  end;

  if compat_count = 0 then
  begin
    announce(WARNING, 'No compatible constructs for theme.');
    result := -1; // Whatever
    exit;
  end;

  result := compats[roll(compat_count)];
end;

// Make a new style that is in the given theme, and copies the
// given style.  Vary is a number from 0 to 100 saying how
// noisy the copy should be; not linear!  If vary is 100, old
// can be nil or whatever.
function copy_style(const l: level_p; const old: style_p; const themenumber: integer;
  const vary: integer; const c: config_p): style_p;
begin
  // Is this the correct sort of nonlinearity?

  result := style_p(SL_Malloc(SizeOf(style_t)));
  result^.next := l^.style_anchor;
  l^.style_anchor := result;
  result^.theme_number := themenumber;
  if not rollpercent(vary) then
    result^.floor0 := old^.floor0
  else
    result^.floor0 := random_floor0(c, result);
  if not rollpercent(vary) then
    result^.ceiling0 := old^.ceiling0
  else
    result^.ceiling0 := random_ceiling0(c, result);
  if not rollpercent(vary) then
    result^.ceilinglight := old^.ceilinglight
  else
    result^.ceilinglight := random_ceilinglight(c, result);
  if not rollpercent(vary) then
    result^.doorfloor := old^.doorfloor
  else
    result^.doorfloor := random_doorfloor(c, result);
  if not rollpercent(vary) then
    result^.stepfloor := old^.stepfloor
  else
    result^.stepfloor := random_stepfloor(c, result);
  if not rollpercent(vary) then
    result^.nukage1 := old^.nukage1
  else
    result^.nukage1 := random_nukage1(c, result);
  if not rollpercent(vary) then
    result^.doorceiling := old^.doorceiling
  else
    result^.doorceiling := random_doorceiling(c, result);
  if not rollpercent(vary) then
  begin
    result^.wall0 := old^.wall0;
    result^.switch0 := old^.switch0;
  end
  else
  begin
    result^.wall0 := random_wall0(c, result);
    result^.switch0 := switch0_for(c, result);
  end;
  if not rollpercent(vary) then
    result^.kickplate := old^.kickplate
  else
    result^.kickplate := random_kickplate(c, result);
  if not rollpercent(vary) then
    result^.stepfront := old^.stepfront
  else
    result^.stepfront := random_stepfront(c, result);
  if not rollpercent(vary) then
    result^.support0 := old^.support0
  else
    result^.support0 := random_support0(c, result);
  if not rollpercent(vary) then
    result^.doorjamb := old^.doorjamb
  else
    result^.doorjamb := random_doorjamb(c, result);
  if not rollpercent(vary) then
    result^.widedoorface := old^.widedoorface
  else
    result^.widedoorface := random_widedoorface(c, result);
  if not rollpercent(vary) then
    result^.narrowdoorface := old^.narrowdoorface
  else
    result^.narrowdoorface := random_narrowdoorface(c, result);
  if not rollpercent(vary) then
    result^.twdoorface := old^.twdoorface
  else
    result^.twdoorface := random_twdoorface(c, result);
  if not rollpercent(vary) then
    result^.tndoorface := old^.tndoorface
  else
    result^.tndoorface := random_tndoorface(c, result);
  if not rollpercent(vary) then
    result^.lockdoorface := old^.lockdoorface
  else
    result^.lockdoorface := random_lockdoorface(c, result);
  if not rollpercent(vary) then
    result^.walllight := old^.walllight
  else
    result^.walllight := random_walllight(c, result);
  if not rollpercent(vary) then
    result^.liftface := old^.liftface
  else
    result^.liftface := random_liftface(c, result);
  if not rollpercent(vary) then
    result^.plaque := old^.plaque
  else
    result^.plaque := random_plaque(c, result);
  if not rollpercent(vary) then
    result^.redface := old^.redface
  else
    result^.redface := random_redface(c, result);
  if not rollpercent(vary) then
    result^.blueface := old^.blueface
  else
    result^.blueface := random_blueface(c, result);
  if not rollpercent(vary) then
    result^.yellowface := old^.yellowface
  else
    result^.yellowface := random_yellowface(c, result);
  if not rollpercent(vary) then
    result^.lamp0 := old^.lamp0
  else
    result^.lamp0 := random_lamp0(c, result);
  if not rollpercent(vary) then
    result^.shortlamp0 := old^.shortlamp0
  else
    result^.shortlamp0 := random_shortlamp0(c, result);
  if not rollpercent(vary) then
    result^.grating := old^.grating
  else
    result^.grating := random_grating(c, result);
  if not rollpercent(vary) then
    result^.roomlight0 := old^.roomlight0
  else
    result^.roomlight0 := c^.minlight +
      roll((l^.bright_light_level - c^.minlight) div 2) +
      roll((l^.bright_light_level - c^.minlight) div 2);
  result^.doorlight0 := result^.roomlight0 + 20 - roll(41);
  if not rollpercent(vary) then
  begin
    result^.wallheight0 := old^.wallheight0
  end
  else
  begin
    if rollpercent(20) then // More variety in here
      result^.wallheight0 := 256
    else if rollpercent(50) then
      result^.wallheight0 := 128
    else
      result^.wallheight0 := 96;
  end;
  if not rollpercent(vary) then
    result^.linkheight0 := old^.linkheight0
  else
  begin
    if rollpercent(20) then // More variety in here
      result^.linkheight0 := 128
    else if rollpercent(50) then
      result^.linkheight0 := 64
    else
      result^.linkheight0 := 72;
    result^.linkheight0 := result^.linkheight0 * l^.hugeness;
  end;
  if not rollpercent(vary) then
    result^.closet_width := old^.closet_width
  else
  begin
    // Old narrow method
    result^.closet_width := 64 + roll(4) + roll(4) + roll(4) + roll(4);
    if rollpercent(50) then // Something wider?
      result^.closet_width := 64 + 16 * roll(5);
  end;
  if not rollpercent(vary) then
    result^.closet_depth := old^.closet_depth
  else
  begin
    result^.closet_depth := 64 + roll(4) + roll(4) + roll(4) + roll(4);
    if rollpercent(40) then
      result^.closet_depth := result^.closet_depth * 2;
  end;
  if not rollpercent(vary) then
    result^.closet_light_delta := old^.closet_light_delta
  else
    result^.closet_light_delta := roll(55) - 35;
  if not rollpercent(vary) then
    result^.moving_jambs := old^.moving_jambs
  else
    result^.moving_jambs := rollpercent(10);
  if not rollpercent(vary) then
    result^.secret_doors := old^.secret_doors
  else
    result^.secret_doors := rollpercent(5);
  // These have to co-vary, because grating determines heights
  if not rollpercent(vary) then
  begin
    result^.window_grate := old^.window_grate;
    result^.sillheight := old^.sillheight;
    result^.windowheight := old^.windowheight;
  end
  else
  begin
    result^.window_grate := rollpercent(30);
    result^.sillheight := random_sillheight(c, result);
    result^.windowheight := random_windowheight(c, result);
  end;
  if not rollpercent(vary) then
    result^.light_recesses := old^.light_recesses
  else
    result^.light_recesses := rollpercent(30);
  if not rollpercent(vary) then
    result^.do_constructs := old^.do_constructs
  else
    result^.do_constructs := rollpercent(80);
  if not rollpercent(vary) then
    result^.light_steps := old^.light_steps
  else
    result^.light_steps := rollpercent(10);   // Too low?
  if not rollpercent(vary) then
    result^.light_edges := old^.light_edges
  else
    result^.light_edges := rollpercent(20);
  if not rollpercent(vary) then
    result^.peg_lightstrips := old^.peg_lightstrips
  else
    result^.peg_lightstrips := rollpercent(50);
  if not rollpercent(vary) then
    result^.construct_family := old^.construct_family
  else
    result^.construct_family := construct_family_for(c, result);
  if not rollpercent(vary) then
    result^.window_decor := old^.window_decor
  else
    result^.window_decor := random_windowdecor(c, result);
  if not rollpercent(vary) then
    result^.lightbox_lighting := old^.lightbox_lighting
  else
    result^.lightbox_lighting := random_lightboxlighting(c, result);
  if not rollpercent(vary) then
    result^.slitwindows := old^.slitwindows
  else
    result^.slitwindows := rollpercent(20);
  if not rollpercent(vary) then
    result^.windowborder := old^.windowborder
  else
    result^.windowborder := random_windowborder(c, result);
  if not rollpercent(vary) then
    result^.soundproof_doors := old^.soundproof_doors
  else
    result^.soundproof_doors := rollpercent(30);
  if not rollpercent(vary) then
    result^.center_pillars := old^.center_pillars
  else
    result^.center_pillars := rollpercent(70);
  if not rollpercent(vary) then
    result^.paint_recesses := old^.paint_recesses
  else
    result^.paint_recesses := rollpercent(60);
  if not rollpercent(vary) then
    result^.gaudy_locks := old^.gaudy_locks
  else
    result^.gaudy_locks := rollpercent(10);
  result^.lightboxes := FALSE; // Ephemeral; usually FALSE
  if not rollpercent(vary) then
    result^.auxheight := old^.auxheight
  else
    result^.auxheight := roll(2) * (8 + 8 * roll(8));
  if not rollpercent(vary) then
    result^.auxspecial := old^.auxspecial
  else if rollpercent(80) then
    result^.auxspecial := 0
  else
    result^.auxspecial := RANDOM_BLINK;
  if not rollpercent(vary) then
    result^.doortype := old^.doortype
  else
    result^.doortype := random_doortype(l, c, result);
  if not rollpercent(vary) then
    result^.slifttype := old^.slifttype
  else
    result^.slifttype := random_slifttype(c, result);
  if not rollpercent(vary) then
    result^.link0 := old^.link0
  else
    result^.link0 := random_link(l, nil, result, nil, c);
end;

// Return a new style derived from the given one, based on the config
// If "radical", choose a whole new theme.  Else don't.
// should be.  It's not linear!
function new_style(const l: level_p; const old: style_p; const radical: boolean;
  const c: config_p): style_p;
var
  newtheme: integer;
  vary: integer;
begin
  if radical then
  begin
    newtheme := c^.rad_newtheme;
    vary := c^.rad_vary
  end
  else
  begin
    newtheme := c^.norm_newtheme;
    vary := c^.norm_vary;
  end;

  if (not c^.lock_themes) and rollpercent(newtheme) then // Sometimes entirely new
    result := copy_style(l, old, random_theme(c), 100, c)
  else if rollpercent(vary) then // Sometimes new, same theme
    result := copy_style(l, old, old^.theme_number, 100, c)
  else // else partly new, same theme
    result := copy_style(l, old, old^.theme_number, vary, c);
end;

// Return a random style structure according to the configuration
function random_style(const l: level_p; const c: config_p): style_p;
begin
  result := copy_style(l, nil, random_theme(c), 100, c);
end;

// Shockingly special-purpose routine that puts some stuff into
// a room that contains a gate in the midtile.
procedure gate_populate(const l: level_p; const s: sector_p; const haa: haa_p;
  const first: boolean; const c: config_p);
var
  minx, miny, maxx, maxy: integer;
  tlx, tly, thx, thy: SmallInt;
  m: genus_p;
  levels: integer;
begin
  if first then exit; // punt!

  find_sector_rectangle(l, s, minx, miny, maxx, maxy);
  mid_tile(l, s, tlx, tly, thx, thy);

  if tlx - minx > 63 then // "63"s are all wrong
  begin
    if rollpercent(50) then // A monster
    begin
      m := timely_monster(haa, c, @levels, rollpercent(l^.p_biggest_monsters), 1);
      if levels <> 0 then
        if place_object_in_region(
             l, minx, miny, tlx, maxy,
             c, m^.thingid, m^.width, -1, s^.entry_x, s^.entry_y, levels) <> nil then
          update_haa_for_monster(haa, m, levels, 1, c);
    end
    else
      place_timely_something(l, haa, c, (minx + tlx) div 2, (miny + maxy) div 2);
  end;

  if maxx - thx > 63 then // "63"s are all wrong
  begin
    if rollpercent(50) then // A monster
    begin
      m := timely_monster(haa, c, @levels, rollpercent(l^.p_biggest_monsters), 1);
      if levels <> 0 then
        if place_object_in_region(
             l, thx, miny, maxx, maxy,
             c, m^.thingid, m^.width, -1, s^.entry_x, s^.entry_y, levels) <> nil then
          update_haa_for_monster(haa,m,levels,1,c);
    end
    else
      place_timely_something(l, haa, c, (thx + maxx) div 2, (miny + maxy) div 2);
  end;

  // Absurd duplication!
  if tly - miny > 63 then // "63"s are all wrong
  begin
    if rollpercent(50) then // A monster
    begin
      m := timely_monster(haa, c, @levels, rollpercent(l^.p_biggest_monsters), 1);
      if levels <> 0 then
        if place_object_in_region(
             l, minx, miny, maxx, tly,
             c, m^.thingid, m^.width, -1, s^.entry_x, s^.entry_y, levels) <> nil then
          update_haa_for_monster(haa, m, levels, 1, c);
    end
    else
      place_timely_something(l, haa, c, (minx + maxx) div 2, (miny + tly) div 2);
  end;

  if maxy - thy > 63 then // "63"s are all wrong
  begin
    if rollpercent(50) then // A monster
    begin
      m := timely_monster(haa, c, @levels, rollpercent(l^.p_biggest_monsters), 1);
      if levels <> 0 then
        if place_object_in_region(
             l, minx, thy, maxx, maxy,
             c, m^.thingid, m^.width, -1, s^.entry_x, s^.entry_y, levels) <> nil then
          update_haa_for_monster(haa, m, levels, 1, c);
    end
    else
      place_timely_something(l, haa, c, (minx + maxx) div 2, (thy + maxy) div 2);
  end;

  // And finally do weapon pickups
  haa_unpend(haa);
end;

// Put monsters and health and armor and stuff in the room
// Update the health/ammo/armor estimate structure also
procedure populate(const l: level_p; const oldsector: sector_p; const c: config_p;
  const ThisHaa: haa_p; const first_room: boolean);
begin
  if not first_room or c^.immediate_monsters then
    place_monsters(l, oldsector, c, ThisHaa);
  place_health(l, oldsector, c, ThisHaa);
  place_ammo(l, oldsector, c, ThisHaa);
  place_armor(l, oldsector, c, ThisHaa);
  place_barrels(l, oldsector, c, ThisHaa);
end;

// Taking all the given stuff into account, have we put
// enough rooms into the current quest yet?
// This routine can also mess with the current quest,
// to do special end-stuff, like arenas.
function enough_quest(const l: level_p; const s: sector_p; const ThisQuest: quest_p;
  const c: config_p): boolean;
begin
  {$IFNDEF NOT_DOING_ARENAS}
  // Perhaps an arena? */
  if (ThisQuest^.goal = LEVEL_END_GOAL) and
     (s <> l^.first_room) and
     (s^.gate = nil) and       // 'cause we're lazy
     (not c^.do_dm) and
       ( (l^.sl_tag <> 0) or not need_secret_level(c) ) and
       ( (l^.sl_tag = 0) or l^.sl_done ) and
       (ThisQuest^.count >= (ThisQuest^.minrooms - 5)) then
    if (c^.mission = 8) or
       (c^.map = 30) or
         ( (c^.map = 7) and (c^.last_mission) ) or
         ( c^.last_mission and (c^.force_arena or rollpercent(3 * c^.levelcount)) ) then
    begin
      ThisQuest^.goal := ARENA_GOAL;
      result := true;
      exit;
    end;
  {$ENDIF}

  // Don't stop a GATE_QUEST at an already-gate room
  if ThisQuest^.goal = GATE_GOAL then
    if s^.gate <> nil then
    begin
      result := false;
      exit;
    end;

  // Otherwise the ordinary check.
  result := ThisQuest^.count >= ThisQuest^.minrooms;
end;

// Process the switches in the given arg array, filling in the
// given config structure.  Use s in error messages.  If conly,
// all we're parsing for here are -config and -seed.
// Print msg and return FALSE if error, else return TRUE.
function do_switches(const args: TDStringList; const c: config_p; const s: string;
  const conly: integer): boolean;
var
  i, j, ii: integer;
  stmp: string;
  stmp2: string;
  mfac: integer;
begin
  if conly <> 0 then // config, seed, -v only; imperfect algorithm! Can be fooled.
  begin
    for i := 0 to args.Count - 1 do
    begin
      if not b_stricmp(args.Strings[i], '-config') then
      begin
        if i < args.Count - 1 then // If -config is the last arg, just ignore it
          if Pos('.', args.Strings[i + 1]) <> 0 then // JVAL
            c^.configfile := args.Strings[i + 1];
      end // end if enough args
      else if not b_stricmp(args.Strings[i], '-seed') then
      begin
        if i < args.Count - 1 then // If -seed is the last arg, just ignore it
          c^.ranseed := atoi(args.Strings[i + 1]);
      end
      else if not b_stricmp(args.Strings[i], '-v') then
        global_verbosity := 1;
    end; // end for args
    // Now we have the seed, from timer or cmdline, so use it
    sl_seed := c^.ranseed;
    announce(LOG, Format('Seed: %d', [c^.ranseed]));
  end
  else // not conly
  begin
    for i := 0 to args.Count - 1 do
    begin
      stmp := strtrim(args.Strings[i]);
      if stmp = '' then
        continue;
    {  if (stmp[1] <> '-') then
      begin
        if i = 1 then
        begin
          if not b_stricmp(args.Strings[i - 1], '-outfile') then
            if Pos('.', stmp) <> 0 then // JVAL
              c^.outfile := stmp; // Just take last if multiple
        end
        else
        begin
          if not b_stricmp(args.Strings[i - 1], '-config') then
            if Pos('.', stmp) <> 0 then // JVAL
              c^.configfile := stmp; // Just take last if multiple
        end;
      end
      else} if stmp = '-?' then
      begin
        Usage2;
        result := FALSE;
        halt(1);
        exit;
       end
      else if not b_stricmp(stmp, '-outfile') then
      begin
        if i < args.Count - 1 then // If this is the last arg, just ignore it
          c^.outfile := args.Strings[i + 1];
      end
      else if not b_stricmp(stmp, '-doom') then
      begin
        c^.gamemask := DOOM1_BIT;
        if c^.episode = 0 then
        begin
          c^.episode := 1;
          c^.mission := 1;
        end;
        c^.map := 0;
      end
      else if not b_stricmp(stmp, '-doom2') then
      begin
        c^.gamemask := DOOM2_BIT;
        c^.episode := 0;
        c^.mission := 0;
        if c^.map = 0 then
          c^.map := 1;
      end
      else if not b_stricmp(stmp, '-nogross') then
        c^.gamemask := c^.gamemask or DOOMC_BIT
      else if not b_stricmp(stmp, '-v') then
        global_verbosity := 1
      else if not b_stricmp(stmp, '-cwad') then
        c^.cwadonly := TRUE
      else if not b_stricmp(stmp, '-nocustom') then
        c^.gamemask := c^.gamemask or DOOMI_BIT
      else if not b_stricmp(stmp, '-nulls') then
        c^.produce_null_lmps := TRUE
      else if not b_stricmp(stmp, '-noslinfo') then
      c^.do_slinfo := FALSE
      else if not b_stricmp(stmp, '-noseclevels') then
        c^.do_seclevels := FALSE
      else if not b_stricmp(stmp, '-bimo!') then
        c^.force_biggest := TRUE
      else if not b_stricmp(stmp, '-bimo') then
        c^.big_monsters := TRUE
      else if not b_stricmp(stmp, '-biwe') then
        c^.big_weapons := TRUE
      else if not b_stricmp(stmp, '-huge') then
        c^.minhuge := 2
      else if not b_stricmp(stmp, '-xsecret') then
        c^.force_secret := TRUE
      else if not b_stricmp(stmp, '-fstart') then
      begin
        c^.fcontrol := TRUE;
        c^.fstart := TRUE;
      end
      else if not b_stricmp(stmp, '-ffstart') then
      begin
        c^.fcontrol := TRUE;
        c^.ffstart := TRUE;
      end
      else if not b_stricmp(stmp, '-fend') then
      begin
        c^.fcontrol := TRUE;
        c^.fend := TRUE;
      end
      else if not b_stricmp(stmp, '-ffend') then
      begin
        c^.fcontrol := TRUE;
        c^.ffend := TRUE;
      end
      else if not b_stricmp(stmp, '-gross') then
        c^.gamemask := c^.gamemask and not DOOMC_BIT
      else if not b_stricmp(stmp, '-music') then
        c^.do_music := TRUE
      else if not b_stricmp(stmp, '-nosemo') then
        c^.secret_monsters := FALSE
      else if not b_stricmp(stmp, '-dm') then
        c^.do_dm := TRUE
      else if not b_stricmp(stmp, '-arena') then
        c^.force_arena := TRUE
      else if not b_stricmp(stmp ,'-levels') then
      begin
        if i < args.Count - 1 then // If this is the last arg, just ignore it
        begin
          c^.levelcount := atoi(args.Strings[i + 1]);
          if c^.levelcount <= 0 then
          begin
            announce(ERROR, Format('%s error: invalid -levels arg <%s>.', [s, stmp]));
            result := FALSE;
            exit;
          end;
        end; // end if enough args
      end
      else if not b_stricmp(stmp, '-minlight') then
      begin
        if i < args.Count - 1 then // If this is the last arg, just ignore it
        begin
          c^.minlight := atoi(args.Strings[i + 1]);
          if c^.minlight <= 0 then
          begin
            announce(ERROR, Format('%s error: invalid -minlight arg <%s>.', [s, stmp]));
            result := FALSE;
            exit;
          end;
        end; // end if enough args
      end
      else if not b_stricmp(stmp, '-macho') then
      begin
        if i < args.Count - 1 then // If this is the last arg, just ignore it
        begin
          mfac := atoi(args.Strings[i + 1]);
          if (mfac < 1) or (mfac > 100) then
          begin
            announce(ERROR, Format('%s error: -macho must be in [1,100], not <%s>.', [s, stmp]));
            result := FALSE;
            exit;
          end;
          c^.machoh := 100.0 - mfac / 4.0 / 100.0;
          c^.machou := 100.0 - mfac / 2.0 / 100.0;
        end; // end if enough args */
      end
      else if not b_stricmp(stmp, '-restrict') then
      begin
        if i < args.Count - 1 then // If this is the last arg, just ignore it
        begin
          ii := i + 1;
          stmp2 := args.Strings[ii];
          c^.gamemask := 0;
          for j := 1 to Length(stmp2) do
          begin
            if stmp2[j] = 'C' then
              c^.gamemask := c^.gamemask or DOOMC_BIT
            else if stmp2[j] = 'I' then
              c^.gamemask := c^.gamemask or DOOMI_BIT
            else if stmp2[j] = '0' then
              c^.gamemask := c^.gamemask or DOOM0_BIT or DOOM1_BIT
            else if stmp2[j] = '1' then
              c^.gamemask := c^.gamemask or DOOM1_BIT
            else if stmp2[j] = '2' then
              c^.gamemask := c^.gamemask or DOOM2_BIT
            else
            begin
              announce(ERROR, Format('%s error: invalid -restrict arg <%s>.', [s, stmp2]));
              result := FALSE;
              exit;
            end;
          end; // end for bits of next arg
        end; // end if enough args
      end
      // JVAL rewritten -ExMx switch
      else if (Length(stmp) = 5) and (stmp[1] = '-') and
              (stmp[2] in ['E', 'e']) and (stmp[4] in ['M', 'm']) then
      begin
        if (stmp[3] in ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']) and
           (stmp[5] in ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']) then
        begin
          c^.episode := atoi(stmp[3]);
          c^.mission := atoi(stmp[5]);
          c^.map := 0;
        end
        else
        begin
          announce(ERROR, Format('%s error: Invalid -ExMx switch <%s>.', [s, stmp]));
          result := FALSE;
          exit;
        end
      end
      // JVAL rewritten -MAPxx switch
      else if (Length(stmp) = 6) and (stmp[1] = '-') and
              (stmp[2] in ['M', 'm']) and (stmp[3] in ['A', 'a']) and (stmp[4] in ['P', 'p']) then
      begin
        if (stmp[5] in ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']) and
           (stmp[6] in ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']) then
        begin
          c^.episode := 0;
          c^.mission := 0;
          c^.map := atoi(stmp[5] + stmp[6]);
        end;
        if c^.map = 0 then
        begin
          announce(ERROR, Format('%s error: Invalid -MAPxx switch <%s>.', [s, stmp]));
          result := FALSE;
          exit;
        end
      end
      else if not b_stricmp(stmp, '-rooms') then
      begin
        if i < args.Count - 1 then // If this is the last arg, just ignore it
          c^.minrooms := atoi(args.Strings[i + 1]);
      end
      else if not b_stricmp(stmp, '-config') then // Ignored in this stage
      else if not b_stricmp(stmp, '-seed') then // Ignored in this stage
      else // Unknown switch */
      begin
        announce(ERROR, Format('%s error: unknown switch <%s>.', [s, stmp]));
        result := FALSE;
        exit;
      end
    end; // end for args
  end; // end not conly

  result := TRUE;
end;

// Put this object in this sector.  It's a non-obstable object
function place_required_pickable(const l: level_p; const s: sector_p; const c: config_p;
  const id: SmallInt): thing_p;
begin
  result := place_object(l, s, c, id, 48, 0, 0, 0, 7); // 48 for nice-looking-ness
  if result = nil then
    result := place_object(l, s, c, id, 1, 0, 0, 0, 7); // This had better work!
  if result = nil then
    announce(ERROR, Format('Important object <%d> could not be placed.', [id]));
end;

function place_required_small_pickable(const l: level_p;const s: sector_p;const c: config_p): thing_p;
var
  tid: SmallInt;
begin
  if rollpercent(50) then
    tid := ID_POTION
  else
    tid := ID_HELMET; // More choices?

  result := place_required_pickable(l, s, c, tid);
end;

// This is for sector-specific texture alignment.  Is there
// some reason to want to do that?
procedure align_textures(const l: level_p; const oldsector: sector_p; const c: config_p);
begin
end;

// Do these two sidedefs share any texture(s) that should
// be aligned together?
function common_texture(const sd1, sd2: sidedef_p): boolean;
begin
  // Pass along most of the job to coalignable().
  if Pos('-', sd1^.middle_texture^.name) <> 1 then
  begin
    if coalignable(sd1^.middle_texture, sd2^.middle_texture) then
    begin
      result := true;
      exit;
    end;
    if coalignable(sd1^.middle_texture, sd2^.upper_texture) then
    begin
      result := true;
      exit;
    end;
    if coalignable(sd1^.middle_texture, sd2^.lower_texture) then
    begin
      result := true;
      exit;
    end;
  end;

  if Pos('-', sd1^.upper_texture^.name) <> 1 then
  begin
    if coalignable(sd1^.upper_texture, sd2^.middle_texture) then
    begin
      result := true;
      exit;
    end;
    if coalignable(sd1^.upper_texture, sd2^.upper_texture) then
    begin
      result := true;
      exit;
    end;
    if coalignable(sd1^.upper_texture, sd2^.lower_texture) then
    begin
      result := true;
      exit;
    end;
  end;

  if Pos('-', sd1^.lower_texture^.name) <> 1 then
  begin
    if coalignable(sd1^.lower_texture, sd2^.middle_texture) then
    begin
      result := true;
      exit;
    end;
    if coalignable(sd1^.lower_texture, sd2^.upper_texture) then
    begin
      result := true;
      exit;
    end;
    if coalignable(sd1^.lower_texture, sd2^.lower_texture) then
    begin
      result := true;
      exit;
    end;
  end;
  result := false;
end;

procedure global_align_forward(const l: level_p; const ld: linedef_p);
var
  v: vertex_p;
  ld2: linedef_p;
  newoff: integer;
begin
  v := ld^._to;
  ld2 := l^.linedef_anchor;
  while ld2 <> nil do
  begin
    if ld2^.from = v then
    begin
      if common_texture(ld^.right, ld2^.right) then
      begin
        newoff := ld^.right^.x_offset + linelen(ld);
        newoff := newoff mod 256;
        if newoff < 0 then
          newoff := newoff + 256;
        if not ld2^.marked then
        begin
          ld2^.right^.x_offset := newoff;
          ld2^.marked := true;
          global_align_linedef(l, ld2);
        end else
        begin
          if ld2^.right^.x_offset <> newoff then
            ld^.f_misaligned := true;
        end;
      end; // end if common texture
    end;
    ld2 := ld2^.next;
  end; // end for ld2
end;

procedure global_align_backward(const l: level_p; const ld: linedef_p);
var
  v: vertex_p;
  ld2: linedef_p;
  newoff: integer;
begin
  v := ld^.from;
  ld2 := l^.linedef_anchor;
  while ld2 <> nil do
  begin
    if ld2^._to = v then
    begin
      if common_texture(ld^.right, ld2^.right) then
      begin
        newoff := ld^.right^.x_offset - linelen(ld2);
        newoff := newoff mod 256;
        if newoff < 0 then
          newoff := newoff + 256;
        if not ld2^.marked then
        begin
          ld2^.right^.x_offset := newoff;
          ld2^.marked := true;
          global_align_linedef(l, ld2);
        end
        else
        begin
          if ld2^.right^.x_offset <> newoff then
            ld^.b_misaligned := true;
        end;
      end; // end if common texture
    end;
    ld2 := ld2^.next;
  end; // end for ld2
end;

procedure global_align_linedef(const l: level_p; const ld: linedef_p);
begin
  ld^.marked := true;
  global_align_group_backbone_forward(l, ld);
  global_align_group_backbone_backward(l, ld);
  global_align_group_etc_forward(l, ld);
  global_align_group_etc_backward(l, ld);
end;

procedure global_align_group_backbone_forward(const l: level_p; const ld: linedef_p);
var
  ldnext: linedef_p;
  newoff: integer;
begin
  ldnext := ld^.group_next;
  if ldnext <> nil then
  begin
    if ld^._to <> ldnext^.from then
      announce(LOG, 'Yow forward!');
    if common_texture(ld^.right, ldnext^.right) then
    begin
      newoff := ld^.right^.x_offset + linelen(ld);
      newoff := newoff mod 256;
      if newoff < 0 then
        newoff := newoff + 256;
      if not ldnext^.marked then
      begin
        ldnext^.right^.x_offset := newoff;
        ldnext^.marked := true;
        global_align_group_backbone_forward(l, ldnext);
      end
      else
      begin
        announce(LOG, 'Found a locked linedef in g_a_g_b_f?');
        if ldnext^.right^.x_offset <> newoff then
          ldnext^.f_misaligned := true;
      end;
    end; // end if common texture
  end;
end;

procedure global_align_group_etc_forward(const l: level_p; const ld: linedef_p);
var
  ldnext: linedef_p;
begin
  ldnext := ld^.group_next;
  if ldnext <> nil then
    global_align_group_etc_forward(l, ldnext);
  global_align_forward(l, ld);
end;

procedure global_align_group_etc_backward(const l: level_p; const ld: linedef_p);
var
  ldnext: linedef_p;
begin
  ldnext := ld^.group_previous;
  if ldnext <> nil then
    global_align_group_etc_backward(l, ldnext);
  global_align_backward(l, ld);
end;

procedure global_align_group_backbone_backward(const l: level_p; const ld: linedef_p);
var
  ldprev: linedef_p;
  newoff: integer;
begin
  ldprev := ld^.group_previous;
  if ldprev <> nil then
  begin
    if ld^.from <> ldprev^._to then
      announce(LOG, 'Yow backward!');
    if common_texture(ld^.right, ldprev^.right) then
    begin
      newoff := ld^.right^.x_offset - linelen(ldprev);
      newoff := newoff mod 256;
      if newoff < 0 then
        newoff := newoff + 256;
      if not ldprev^.marked then
      begin
        ldprev^.right^.x_offset := newoff;
        ldprev^.marked := true;
        global_align_group_backbone_backward(l, ldprev);
      end
      else
      begin
        announce(LOG, 'Found a locked linedef in g_a_g_b_b?');
        if ldprev^.right^.x_offset <> newoff then
          ldprev^.b_misaligned := true;
      end;
    end; // end if common texture
  end;
end;

// Align textures all around the level
procedure global_align_textures(const l: level_p; const c: config_p);
// This complicated knot of recursives seem pretty good.
// It doesn't know about NOSPLITs and stuff yet, and it
// only does horizontal alignment.  Should we do Y here also?
var
  ld1, ld2: linedef_p;
  newoff: integer;
begin
  announce(LOG, 'Globally aligning...');

{$DEFINE DONT_MARK_MISALIGNS}

  ld1 := l^.linedef_anchor;
  while ld1 <> nil do
  begin
    ld1^.f_misaligned := false;
    ld1^.b_misaligned := false;
    ld1 := ld1^.next;
  end;

  unmark_linedefs(l); // just in case
  ld1 := l^.linedef_anchor;
  while ld1 <> nil do
  begin
    if not ld1^.marked then
      global_align_linedef(l, ld1);
    ld1 := ld1^.next;
  end;

  // Now put in any intentional misalignments, for hints etc
  ld1 := l^.linedef_anchor;
  while ld1 <> nil do
  begin
    if ld1^.right <> nil then
    begin
      ld1^.right^.x_offset := ld1^.right^.x_offset + ld1^.right^.x_misalign;
      ld1^.right^.y_offset := ld1^.right^.y_offset + ld1^.right^.y_misalign;
    end;
    ld1 := ld1^.next;
  end; // end for ld1

// Sometimes put in supports in the places we know didn't align
// This actually looks pretty terrible!  Think about it more
  if l^.support_misaligns then
  begin
    announce(LOG, 'Prettying up misalignments...');
    ld1 := l^.linedef_anchor;
    while ld1 <> nil do
    begin
      if ld1^.right <> nil then
      begin
        if ld1^.right^.sector^.style <> nil then
        begin
          if ld1^.b_misaligned then
          begin
            newoff := linelen(ld1);
            if newoff > 8 then
              split_linedef(l, ld1, 8, c);
            if Pos('-', ld1^.right^.upper_texture^.name) <> 1 then
              ld1^.right^.upper_texture := ld1^.right^.sector^.style^.support0;
            if Pos('-', ld1^.right^.lower_texture^.name) <> 1 then
              ld1^.right^.lower_texture := ld1^.right^.sector^.style^.support0;
            if Pos('-', ld1^.right^.middle_texture^.name) <> 1 then
              ld1^.right^.middle_texture := ld1^.right^.sector^.style^.support0;
          end;
          if ld1^.f_misaligned then
          begin
            newoff := linelen(ld1);
            if newoff > 8 then
              ld2 := split_linedef(l, ld1, newoff - 8, c)
            else
              ld2 := ld1;
            if Pos('-', ld2^.right^.upper_texture^.name) <> 1 then
              ld2^.right^.upper_texture := ld1^.right^.sector^.style^.support0;
            if Pos('-', ld2^.right^.lower_texture^.name) <> 1 then
              ld2^.right^.lower_texture := ld1^.right^.sector^.style^.support0;
            if Pos('-', ld2^.right^.middle_texture^.name) <> 1 then
              ld2^.right^.middle_texture := ld1^.right^.sector^.style^.support0;
           end;
        end;
      end;
      ld1 := ld1^.next;
    end; // end for ld1
  end; // end if supporting

{$IFDEF MARK_MISALIGNS}
// And finally for debugging mark the places we know still didn't align
  ld1 := l^.linedef_anchor;
  while ld1 <> nil then
  begin
    if ld1^.right <> nil then
    begin
      if ld1^.b_misaligned then
      begin
        newoff := linelen(ld1);
        if newoff > 8 then
          split_linedef(l, ld1, 8, c);
        if Pos('-', ld1^.right^.upper_texture^.name) <> 1 then
          ld1^.right^.upper_texture := c^.error_texture;
        if Pos('-', ld1^.right^.lower_texture^.name) <> 1 then
          ld1^.right^.lower_texture := c^.error_texture;
        if Pos('-', ld1^.right^.middle_texture^.name) <> 1 then
          ld1^.right^.middle_texture := c^.error_texture;
      end;
      if ld1^.f_misaligned then
      begin
        newoff := linelen(ld1);
        if newoff > 8 then
          ld2 := split_linedef(l, ld1, newoff - 8, c)
        else
          ld2 := ld1;
        if Pos('-', ld2^.right^.upper_texture^.name) <> 1 then
          ld2^.right^.upper_texture := c^.error_texture;  // or blue!
        if Pos('-', ld2^.right^.lower_texture^.name) <> 1 then
          ld2^.right^.lower_texture := c^.error_texture;
        if Pos('-', ld2^.right^.middle_texture^.name) <> 1 then
          ld2^.right^.middle_texture := c^.error_texture;
      end;
    end;
    ld1 := ld1^.next;
  end; // end for ld1
{$ENDIF}
end;

// Random other last-minute fixups to a level
procedure global_fixups(const l: level_p);
var
  ld: linedef_p;
begin
  ld := l^.linedef_anchor;
  while ld <> nil do
  begin
    if ld^.left = nil then
      ld^.flags := ld^.flags or IMPASSIBLE;
    ld := ld^.next;
  end;
end;

// This just paints all one-sided boundary sidedefs of the sector */
procedure paint_room(const l: level_p; const s: sector_p; const ThisStyle: style_p;
  const c: config_p);
var
  ld: linedef_p;
begin
  ld := l^.linedef_anchor;
  while ld <> nil do
  begin
    if ld^.right <> nil then
      if ld^.right^.sector = s then
        if ld^.right^.isBoundary then
        begin
          if ld^.left = nil then
            ld^.right^.middle_texture := ThisStyle^.wall0
          else
          begin
            patch_upper(ld, ThisStyle^.wall0, c);
            patch_lower(ld, ThisStyle^.kickplate, c); // Or stepfront?
          end;
        end;
    ld := ld^.next;
  end;
  s^.light_level := ThisStyle^.roomlight0;
end;

// Construct a linedef on the left side of this linedef,
// <depth> away from it and pro-parallel to it.
// If old is not nil, re-use it, just changing its to and from.
function make_parallel(const l: level_p; const ld: linedef_p; const depth: integer;
  const old: linedef_p): linedef_p;
var
  v1, v2: vertex_p;
  x, y: integer;
begin
  point_from(ld^.from^.x, ld^.from^.y, ld^._to^.x, ld^._to^.y,
             LEFT_TURN, depth, @x, @y);
  if old <> nil then
  begin
    old^._to^.x := x; // Assumes no one else is using
    old^._to^.y := y; // these vertexes.  OK?
    x := x + (ld^.from^.x - ld^._to^.x);
    y := y + (ld^.from^.y - ld^._to^.y);
    old^.from^.x := x;
    old^.from^.y := y;
    result := old;
    exit;
  end
  else
  begin
    v1 := new_vertex(l, x, y);
    x := x + (ld^.from^.x - ld^._to^.x);
    y := y + (ld^.from^.y - ld^._to^.y);
    v2 := new_vertex(l, x, y);
    result := new_linedef(l, v2, v1);
    exit;
  end;
end;

// Given two linedefs, construct a new rhomboid between them, on
// the left side of the first and the right side of the second.
// Return the sector.  If the edge1 and edge2 args are not null,
// they get the from-joining new linedef, and the to-joining one
// respectively.
function make_box_ext(const l: level_p; const ldf1, ldf2: linedef_p;
                      const ThisStyle: style_p; const c: config_p;
                      const edge1, edge2: linedef_pp): sector_p;
var
  ldnew1, ldnew2: linedef_p;
  oldsec: sector_p;
begin
  // Make the orthogonal sides
  ldnew1 := new_linedef(l, ldf1^.from, ldf2^.from);
  ldnew2 := new_linedef(l, ldf2^._to, ldf1^._to);
  if edge1 <> nil then
    edge1^ := ldnew1;
  if edge2 <> nil then
    edge2^ := ldnew2;
  // Now tie them all to a sector
  result := new_sector(l, 0, 0, c^.sky_flat, c^.sky_flat);
  result^.style := ThisStyle;
  if ldf1^.right <> nil then
  begin
    oldsec := ldf1^.right^.sector;
    result^.floor_height := oldsec^.floor_height;
    result^.ceiling_height := oldsec^.ceiling_height;
    result^.floor_flat := oldsec^.floor_flat;
    result^.ceiling_flat := oldsec^.ceiling_flat;
    result^.light_level := oldsec^.light_level; // default
    result^.special := oldsec^.special;         // or zero?
    ldf1^.right^.middle_texture := c^.null_texture;
    ldf1^.flags := ldf1^.flags or TWO_SIDED;
  end;
  ldf1^.left := new_sidedef(l, result, c);
  ldf2^.right := new_sidedef(l, result, c);
  ldnew1^.right := new_sidedef(l, result, c);
  ldnew2^.right := new_sidedef(l, result, c);
  ldf2^.right^.middle_texture := c^.null_texture; // Useful?
  if ldf2^.left <> nil then
    ldf2^.left^.middle_texture := c^.null_texture; // HOM until filled
  ldf1^.left^.middle_texture := c^.null_texture;
  ldnew1^.right^.middle_texture := ThisStyle^.wall0; // guess
  ldnew2^.right^.middle_texture := ThisStyle^.wall0; // also
end;

// Given a one-sided linedef, construct a rectangular sector on
// the left side of it, of the given depth.  Returns the other
// (parallel) long side.
function lefthand_box_ext(const l: level_p; const ldf1: linedef_p; const depth: integer;
                                  const ThisStyle: style_p; const c: config_p;
                                  const nld1, nld2: linedef_pp): linedef_p;
begin
  // Get the other side of the box
  result := make_parallel(l, ldf1, depth, nil);
  make_box_ext(l, ldf1, result, ThisStyle, c, nld1, nld2);
end;

// Find the corners of the minimal enclosing rectangle around the
// given sector.  Or something like that.  Uses a cache in the
// sector record for speed.  How dangerous is that?
// JVAL: renamed from find_rec()
procedure find_sector_rectangle(const l: level_p; const s: sector_p;
  out minx, miny, maxx, maxy: integer);
// Not a stub, but many of its callers are
var
  ld: linedef_p;
  lx, ly, hx, hy: integer;
begin
  if not s^.findrec_data_valid then
  begin
    lx := HUGE_NUMBER;
    ly := HUGE_NUMBER;
    hx := 0 - HUGE_NUMBER;
    hy := 0 - HUGE_NUMBER;
    ld := l^.linedef_anchor;
    while ld <> nil do
    begin
      if ld^.right <> nil then
        if ld^.right^.sector = s then
        begin
//          if ld^.right^.isBoundary then begin // Need to check this?
            if ld^._to^.x > hx then hx := ld^._to^.x;
            if ld^._to^.y > hy then hy := ld^._to^.y;
            if ld^._to^.x < lx then lx := ld^._to^.x;
            if ld^._to^.y < ly then ly := ld^._to^.y;
//          end;
        end;
      ld := ld^.next;
    end;
    s^.minx := lx;
    s^.miny := ly;
    s^.maxx := hx;
    s^.maxy := hy;
    s^.findrec_data_valid := TRUE;
  end;

  minx := s^.minx;
  miny := s^.miny;
  maxx := s^.maxx;
  maxy := s^.maxy;
end;

procedure dump_link(const ldf1, ldf2: linedef_p; const ThisLink: link_p; const s1: string);
var
  s: string;

  function _Itochar(const x: integer): string;
  begin
    if x <> 0 then
      result := 'Y'
    else
      result := 'M';
  end;

begin
  s := Format('%s Link', [s1]);
  if ldf1 <> nil then
  begin
    s := s + Format(' between (%d,%d)-(%d,%d) and (%d,%d)-(%d,%d).', [
              ldf1^.from^.x, ldf1^.from^.y, ldf1^._to^.x, ldf1^._to^.y,
              ldf2^.from^.x, ldf2^.from^.y, ldf2^._to^.x, ldf2^._to^.y]);
  end;
  announce(VERBOSE, s);
  announce(VERBOSE, 'T W R ND FD C A S L M  h1  w1  w2  d1  d2   d3  fd  sc ');
  s := Format('%s %s %s  %s  %s %s %s %s %s %s %03d %03d %03d %03d %03d %04d %03d %03d', [
      _Itochar(ThisLink^.bits and LINK_TWIN),
      _Itochar(ThisLink^.bits and LINK_WINDOW),
      _Itochar(ThisLink^.bits and LINK_RECESS),
      _Itochar(ThisLink^.bits and LINK_NEAR_DOOR),
      _Itochar(ThisLink^.bits and LINK_FAR_DOOR),
      _Itochar(ThisLink^.bits and LINK_CORE),
      _Itochar(ThisLink^.bits and LINK_ALCOVE),
      _Itochar(ThisLink^.bits and LINK_STEPS),
      _Itochar(ThisLink^.bits and LINK_LIFT),
      _Itochar(ThisLink^.bits and LINK_MAX_CEILING),
      ThisLink^.height1, ThisLink^.width1, ThisLink^.width2,
      ThisLink^.depth1, ThisLink^.depth2, ThisLink^.depth3,
      ThisLink^.floordelta, ThisLink^.stepcount]);
  announce(VERBOSE, s);
end;

// Push a new (defaulty) quest onto the given stack
function push_quest(const old: quest_p): quest_p;
begin
  result := quest_p(SL_Malloc(SizeOf(quest_t)));

  result^.goal := NULL_GOAL;
  result^.tag := 0;
  result^.typ := 0;
  result^.count := 0;
  result^.room := nil;
  result^.minrooms := 0;
  result^.auxtag := 0;
  result^.surprise := nil;
  result^.next := old;
end;

// Pop the top off the stack, free it, return new top
function pop_quest(const current: quest_p): quest_p;
begin
  result := current^.next;
  SL_Free(current);
end;

//****** Many routines from here on need more work ****************

procedure Usage2;
begin
  Usage0();
  printf('Switches that do something at the moment:'#13#10);
  printf('  -rooms [n]   -seed [nnnnnn]  -outfile [filename.ext]'#13#10);
  printf('  -restrict [012C] -ExMx -MAPxx -doom1 -doom2 -levels <x> '#13#10);
  printf('  -minlight <x>  -music  -macho <nn> -noslinfo -nocustom -cwad '#13#10);
  printf('  -arena  -nulls -nosemo -biwe -bimo -bimo! -huge'#13#10);
  printf('  -fstart -ffstart -fend -ffend'#13#10);
{$IFDEF SWITCHES_IN_CONFIG_FILES}
  printf('(-outfile being most useful in the config file; on the'#13#10);
  printf('command line you don''t need the switch.)'#13#10);
{$ENDIF}
  printf(#13#10'M O R E   D E T A I L S   T O   F O L L O W'#13#10#13#10);
end;

{
   Anything that calls this routine is probably too simple to
   handle non-rectangular rooms!

                    Is there any vertex that's
   not marked that occurs in the given rectangle, or do any
   of these four points lie in any unmarked sector?  Or,
   assuming that these points are in clockwise or countercwise
   order, do any existing linedefs between unmarked vertexes
   intersect any of these four lines?  Probably pretty suboptimal!
}
function empty_rectangle(const l: level_p; const x1, y1, x2, y2, x3, y3, x4, y4: integer): boolean;
var
  minx, maxx, miny, maxy: integer;
  v: vertex_p;
  s: sector_p;
  ld: linedef_p;
begin
  // Find the enclosing rectangle of these points
  if x1 > x2 then
    maxx := x1
  else
    maxx := x2;
  if x3 > maxx then maxx := x3;
  if x4 > maxx then maxx := x4;

  if y1 > y2 then
    maxy := y1
  else
    maxy := y2;
  if y3 > maxy then maxy := y3;
  if y4 > maxy then maxy := y4;

  if x1 < x2 then
    minx := x1
  else
    minx := x2;
  if x3 < minx then minx := x3;
  if x4 < minx then minx := x4;

  if y1 < y2 then
    miny := y1
  else
    miny := y2;
  if y3 < miny then miny := y3;
  if y4 < miny then miny := y4;

  // Look at all unmarked vertexes, see if any
  // are within the enclosing rectangle.
  v := l^.vertex_anchor;
  while v <> nil do
  begin
    if not v^.marked then
      if (v^.x <= maxx) and (v^.x >= minx) and
         (v^.y <= maxy) and (v^.y >= miny) then
      begin
        result := false;
        exit;
      end;
    v := v^.next;
  end;

  // Now look at all sectors, see if any of these four
  // proposed vertexes is inside the rectangular envelope
  s := l^.sector_anchor;
  while s <> nil do
  begin
    if not s^.marked then
    begin
      find_sector_rectangle(l, s, minx, miny, maxx, maxy);
      if (x1 <= maxx) and (x1 >= minx) and
         (y1 <= maxy) and (y1 >= miny) then
      begin
        result := false;
        exit;
      end;
      if (x2 <= maxx) and (x2 >= minx) and
         (y2 <= maxy) and (y2 >= miny) then
      begin
        result := false;
        exit;
      end;
      if (x3 <= maxx) and (x3 >= minx) and
         (y3 <= maxy) and (y3 >= miny) then
      begin
        result := false;
        exit;
      end;
      if (x4 <= maxx) and (x4 >= minx) and
         (y4 <= maxy) and (y4 >= miny) then
      begin
        result := false;
        exit;
      end;
    end;
    s := s^.next;
  end;

  // Now look at all linedefs, see if any intersects with
  // any of the four implied boundary lines.  Doesn't assume
  // axis-parallel lines, for a change!  Does assume there are
  // only four sides, though.  Need true polygons.
  ld := l^.linedef_anchor;
  while ld <> nil do
  begin
    if not ld^._to^.marked then
      if not ld^.from^.marked then
      begin
        if intersects(x1, y1, x2, y2, ld^.from^.x, ld^.from^.y, ld^._to^.x, ld^._to^.y) then
        begin
          result := false;
          exit;
        end;
        if intersects(x2, y2, x3, y3, ld^.from^.x, ld^.from^.y, ld^._to^.x, ld^._to^.y) then
        begin
          result := false;
          exit;
        end;
        if intersects(x3, y3, x4, y4, ld^.from^.x, ld^.from^.y, ld^._to^.x, ld^._to^.y) then
        begin
          result := false;
          exit;
        end;
        if intersects(x4, y4, x1, y1, ld^.from^.x, ld^.from^.y, ld^._to^.x, ld^._to^.y) then
        begin
          result := false;
          exit;
        end;
      end;
    ld := ld^.next;
  end;

  result := true;
end;

// Given a linedef and a point, return the distance from the
// linedef to the point, positive if the point is on the right
// side, negative if to the left.  May return HUGE_NUMBER if
// the point is not in either axis-shadow of the linedef, and
// may assume that the linedef is basically axis-parallel.
function point_from_linedef(const l: level_p; const x, y: integer; const ld: linedef_p): integer;
var
  candidate: integer;
  parity: integer;
begin
  result := HUGE_NUMBER;
  parity := 1;

  // An utter kludge; why not just do it right?

  if (ld^.from^.x <> ld^._to^.x) and (ld^.from^.x <= x) and (ld^._to^.x >= x) then
  begin
    candidate := abs(ld^._to^.y - y);
    if candidate < result then
    begin
      result := candidate;
      if ld^._to^.y < y then
        parity := -1
      else
        parity := 1;
    end;
    candidate := abs(ld^.from^.y - y);
    if candidate < result then
    begin
      result := candidate;
      if ld^.from^.y < y then
        parity := -1
      else
        parity := 1;
    end;
  end;

  if (ld^._to^.x <> ld^.from^.x) and (ld^._to^.x <= x) and (ld^.from^.x >= x) then
  begin
    candidate := abs(ld^._to^.y - y);
    if candidate < result then
    begin
      result := candidate;
      if ld^._to^.y < y then
        parity := 1
      else
        parity := -1;
    end;
    candidate := abs(ld^.from^.y - y);
    if candidate < result then
    begin
      result := candidate;
      if ld^.from^.y < y then
        parity := 1
      else
        parity := -1;
    end;
  end;

  if (ld^.from^.y <> ld^._to^.y) and (ld^.from^.y <= y) and (ld^._to^.y >= y) then
  begin
    candidate := abs(ld^._to^.x - x);
    if candidate < result then
    begin
      result := candidate;
      if ld^._to^.x < x then
        parity := 1
      else
        parity := -1;
    end;
    candidate := abs(ld^.from^.x - x);
    if candidate < result then
    begin
      result := candidate;
      if ld^.from^.x < x then
        parity := 1
      else
        parity := -1;
    end;
  end;

  if (ld^._to^.y <> ld^.from^.y) and (ld^._to^.y <= y) and (ld^.from^.y >= y) then
  begin
    candidate := abs(ld^._to^.x - x);
    if candidate < result then
    begin
      result := candidate;
      if ld^._to^.x < x then
        parity := -1
      else
        parity := 1;
    end;
    candidate := abs(ld^.from^.x - x);
    if candidate < result then
    begin
      result := candidate;
      if ld^.from^.x < x then
        parity := -1
      else
        parity := 1;
    end;
  end;

  result := result * parity;
end;

// Are any non-flying monsters so close to this linedef
// that if the sides of the linedef were unwalkable, the
// monster would be stuck?
function no_monsters_stuck_on(const l: level_p; const ld: linedef_p): boolean;
var
  m: thing_p;
  dist: integer;
begin
  m := l^.thing_anchor;
  while m <> nil do
  begin
    if m^.genus^.bits and MONSTER = 0 then
    begin
      m := m^.next;
      continue; // Only monsters
    end;
    if m^.genus^.bits and FLIES <> 0 then
    begin
      m := m^.next;
      continue; // Fliers can escape
    end;
    dist := abs(point_from_linedef(l,m^.x ,m^.y,ld));
    if dist <= (MONSTER_WIDTH(m) / 2) then
    begin
{$IFDEF ALLOW_STUCK_MONSTERS_IN_BATHS}
      announce(LOG, 'Bath Bug!');
      result := TRUE;
      exit;
{$ENDIF}
      result := FALSE;
    end;
    m := m^.next;
  end;
  result := TRUE;
end;

// Return sector that the given x,y is in, and if dist is
// not null return the distance from the nearest wall that
// it is.  Or something like that.  nil if in no sector.
// Set <danger>, if non-nil, if any non-normal linedef is
// within 48 of the point.
function point_sector(const l: level_p; const x, y: integer; const dist: PInteger; const danger: PBoolean): sector_p;
var
  thisdist, closest: integer;
  ld, ldbest: linedef_p;
begin
  result := nil;

  if danger <> nil then
    danger^ := FALSE;
  closest := HUGE_NUMBER;
  ld := l^.linedef_anchor;
  while ld <> nil do
  begin
    thisdist := point_from_linedef(l, x, y, ld);
    if abs(thisdist) < 49 then
      if ld^.typ <> LINEDEF_NORMAL then
        if danger <> nil then
          danger^ := TRUE;
    if abs(thisdist) < closest then
    begin
      if thisdist > 0 then
      begin
        result := ld^.right^.sector;
        closest := abs(thisdist);
        ldbest := ld;
      end
      else if ld^.left <> nil then
      begin
        // Actually, if we find that the closest thing is the left side
        // of a one-sided linedef, we should set answer to nil, and
        // update closest and ldbest.  But, because sometimes the crude
        // point_from_linedef() seriously underestimates distances, we'll
        // actually do nothing in that case, on the theory that some
        // linedef that gives us a non-nil answer is *really* the
        // closest one.  This is a hack; we should fix point_from_linedef
        // instead.
        result := ld^.left^.sector;
        closest := abs(thisdist);
        ldbest := ld;
      end;
    end;
    ld := ld^.next;
  end;

  if dist <> nil then
    dist^ := closest;
end;

// Return a patch array followed by MUS-format pseudo-MIDI
// for one piece of music, and fill in the given header
// with the data it needs.  Return value is SL_Free()able.
function one_piece(const pmh: musheader_p): PByteArray;
var
  patch: byte;
begin
  patch := roll(128);

  // Now this is the very definition of "stub"
  pmh^.tag[0] := 'M';
  pmh^.tag[1] := 'U';
  pmh^.tag[2] := 'S';
  pmh^.tag[3] := Chr($1a);
  pmh^.primchannels := 1;
  pmh^.secchannels := 0;
  pmh^.dummy := 0;
  pmh^.patches := 1;
  pmh^.headerlength := SizeOf(musheader_t) + pmh^.patches * SizeOf(SmallInt);
  pmh^.muslength := 17;
  result := PByteArray(SL_Malloc(pmh^.patches * SizeOf(SmallInt) + pmh^.muslength));
  result^[0] := patch;  // instrument
  result^[1] := 0;
  result^[2] := $40;    // Control change, channel zero
  result^[3] := $00;    // Select patch
  result^[4] := patch;  // that again
  result^[5] := $40;    // Control change, channel zero
  result^[6] := $07;    // volume
  result^[7] := $7f;    // loud!

  result^[8] := $80 or $10;  // Play note, channel zero, last event
  result^[9] := $80 or $50;  // Note 80, with volume
  result^[10] := $7f;  // volume
  result^[11] := 70;   // half-second delay
  result^[12] := $00;  // Note off, channel zero
  result^[13] := $50;  // Note 80 off
  result^[14] := $80 or $10;  // Play note, channel zero, last event
  result^[15] := $4b;  // Note 75, no volume
  result^[16] := 70;   // delay

  result^[17] := $60;  // end score
  result^[18] := $4d;  // maybe?
end;

// Allocate, initialize, and return a new lmp for custom textures
function new_texture_lmp(const name: string): texture_lmp_p;
begin
  result := texture_lmp_p(SL_Malloc(SizeOf(texture_lmp_t)));
  result^.name := name;
  result^.custom_texture_anchor := nil;
end;

// Allocate, initialize, register with the texture (but don't
// bother returning) a new patch for this texture
function add_patch(const ct: custom_texture_p; const patchid: SmallInt; const x, y: SmallInt): patch_p;
var
  p: patch_p;
begin
  result := patch_p(SL_Malloc(SizeOf(patch_t)));

  result^.number := patchid;
  result^.x := x;
  result^.y := y;
  result^.next := nil;
  // Simplest if these are in frontward order
  if ct^.patch_anchor = nil then
    ct^.patch_anchor := result
  else
  begin
    p := ct^.patch_anchor;
    if p = nil then
      announce(ERROR, 'Null patch anchor')
    else
    begin
      while p^.next <> nil do p := p^.next; // find last
      p^.next := result;
    end;
  end;
end;

// Allocate, initialize, register with the given lmp, and return
// a new custom texture record structure thing
function new_custom_texture(const tl: texture_lmp_p; const name: string;
                            const xsize, ysize: SmallInt): custom_texture_p;
begin
  result := custom_texture_p(SL_Malloc(SizeOf(custom_texture_t)));

  result^.name := name;
  result^.xsize := xsize;
  result^.ysize := ysize;
  result^.patch_anchor := nil;
  result^.next := tl^.custom_texture_anchor;
  tl^.custom_texture_anchor := result;
end;

// Free up all resources associated with a texture lump
procedure free_texture_lmp(const tl: texture_lmp_p);
var
  ctp: custom_texture_p;
  p: patch_p;
begin
  // Free each texture
  while true do
  begin
    ctp := tl^.custom_texture_anchor;
    if ctp = nil then
      break;
    tl^.custom_texture_anchor := tl^.custom_texture_anchor^.next;
    // Free each patch
    while true do
    begin
      p := ctp^.patch_anchor;
      if p = nil then
        break;
      ctp^.patch_anchor := ctp^.patch_anchor^.next;
      SL_Free(p);
    end;
    SL_Free(ctp);
  end;
  SL_Free(tl);
end;

// Dump the given texture lmp to the given dump-handle
procedure dump_texture_lmp(const dh: dump_record_p; const tl: texture_lmp_p);
var
  texturecount: integer;
  patchcount: integer;
  lmpsize, i: integer;
  isize: integer;
  ct: custom_texture_p;
  p: patch_p;
  buf, tbuf: PByteArray;
  name8: char8_t;
begin
  texturecount := 0;
  // First figure entire lmp size.  Four bytes of tcount...
  lmpsize := 4;

  // Plus four-plus-22 bytes per texture, plus 10 per patch
  ct := tl^.custom_texture_anchor;
  while ct <> nil do
  begin
    inc(texturecount);
    lmpsize := lmpsize + 4 + 22; // Four bytes index, 22 bytes structure
    p := ct^.patch_anchor;
    while p <> nil do
    begin
      lmpsize := lmpsize + 10;
      p := p^.next;
    end;
    ct := ct^.next;
  end;

  // Get storage for the lmp itself
  buf := PByteArray(SL_Malloc(lmpsize));
  tbuf := buf;

  // Write in the count
  _output_int(tbuf, texturecount);

  // Now traverse the textures again, and make the index
  isize := 4 + 4 * texturecount;
  ct := tl^.custom_texture_anchor;
  while ct <> nil do
  begin
    _output_int(tbuf, isize);
    // Four bytes index, 22 bytes structure (the 4 bytes added outside the while loop)
    isize := isize + 22;
    p := ct^.patch_anchor;
    while p <> nil do
    begin
      isize := isize + 10;
      p := p^.next;
    end;
    ct := ct^.next;
  end;

  {$IFDEF DEBUG}
  if lmpsize <> isize then
    announce('ERROR', 'dump_texture_lmp(): lmpsize <> isize');
  {$ENDIF}
  // Now one last time, writing the data itself
  ct := tl^.custom_texture_anchor;
  while ct <> nil do
  begin
    name8 := stringtochar8(ct^.name);
    for i := 0 to 7 do
      tbuf^[i] := Ord(name8[i]);
    tbuf := @tbuf^[8];
    _output_short(tbuf, 0);
    _output_short(tbuf, 0);
    _output_short(tbuf, ct^.xsize);
    _output_short(tbuf, ct^.ysize);
    _output_short(tbuf, 0);
    _output_short(tbuf, 0);
    patchcount := 0;
    p := ct^.patch_anchor;
    while p <> nil do
    begin
      inc(patchcount);
      p := p^.next;
    end;
    _output_short(tbuf, patchcount);
    p := ct^.patch_anchor;
    while p <> nil do
    begin
      _output_short(tbuf, p^.x);
      _output_short(tbuf, p^.y);
      _output_short(tbuf, p^.number);
      _output_short(tbuf, 1);
      _output_short(tbuf, 0);
      p := p^.next;
    end;
    ct := ct^.next;
  end;

  RegisterLmp(dh, tl^.name, lmpsize);
  fwrite(buf, lmpsize, 1, dh^.f);
end;

// Record any custom textures, made from existing patches, that
// we might want to show off by using.  Only works in DOOM2, sadly.
// In DOOM I, we'd have to recreate the entire TEXTURE2 (or 1) lump,
// and then add our stuff to it.
procedure record_custom_textures(const dh: dump_record_p; const c: config_p);
var
  tl: texture_lmp_p;
  ct: custom_texture_p;
begin
  // Return if TEXTURE2 not available
  if c^.gamemask and (DOOM0_BIT or DOOM1_BIT or DOOMI_BIT) <> 0 then
    exit;

  tl := new_texture_lmp('TEXTURE2'); // JVAL: MAKE TEXTURE3 ?

  ct := new_custom_texture(tl, 'GRAYALT', $80, $80);
  add_patch(ct, $87, 0, 0);
  add_patch(ct, $8a, 0, $40);
  add_patch(ct, $8a, $40, 0);
  add_patch(ct, $87, $40, $40);
  ct := new_custom_texture(tl, 'TEKVINE', $100, $80);
  add_patch(ct, $19b, 0, 0);
  add_patch(ct, $183, $40, 0);
  add_patch(ct, $19b, $80, 0);
  add_patch(ct, $183, $c0, 0);
  add_patch(ct, $35, 0, 0);    // Vines!
  ct := new_custom_texture(tl, 'WOODVINE', $100, $80);
  add_patch(ct, $1b1, 0, 0);    // WOOD9
  add_patch(ct, $1b1, $40, 0);  // WOOD9
  add_patch(ct, $1b1, $80, 0);  // WOOD9
  add_patch(ct, $1b1, $c0, 0);  // WOOD9
  add_patch(ct, $35, 0, 0);     // Vines!
  ct := new_custom_texture(tl, 'WOODLITE', $100, $80);
  add_patch(ct, $1ac, -4, 0);   // Copied from WOOD5
  add_patch(ct, $1ad, 124, 0);  // Copied from WOOD5
  add_patch(ct, $1ac, 252, 0);  // Copied from WOOD5
  add_patch(ct, $78, 32, 20);   // The light overlay
  ct := new_custom_texture(tl, 'DOORSKUL', $40, $48);
  add_patch(ct, $6b, 0, 0);     // The door
  add_patch(ct, $1ab, 21, 11);  // The liddle skull
  ct := new_custom_texture(tl, 'EXITSWIT', $40, $80);
  add_patch(ct, $87, 0, 0);
  add_patch(ct, $87, 0, 64);
  add_patch(ct, $177, 16, 70);
  add_patch(ct, $79, 16, 104);
  ct := new_custom_texture(tl, 'EXITSWIW', $40, $80);
  add_patch(ct, $dd, 0, 0);
  add_patch(ct, $173, $0e, $40);
  add_patch(ct, $79, 16, 104);
  ct := new_custom_texture(tl, 'EXITSWIR', $40, $80);
  add_patch(ct, $12d, 0, 0);
  add_patch(ct, $173, $0f, $42);
  add_patch(ct, $79, 16, 104);
  ct := new_custom_texture(tl, 'MARBGARG', $40, $80);
  add_patch(ct, $0BD, 0, 0);    // MWALL3_1
  add_patch(ct, $1B2, 6, 31);   // SW2_4

  dump_texture_lmp(dh, tl);
  free_texture_lmp(tl);
end; // end record_custom_textures


// A primitive not-quite-random-field image-writing thing
procedure basic_background(const fbuf: PByteArray; const bottom: byte; const range: integer);
var
  i, j: integer;
  above, below, left, right, total: integer;
begin
  for i := 0 to 63 do
  begin
    j := i and 1;
    while j < 64 do
    begin
      fbuf^[64 * i + j] := bottom + roll(range);
      j := j + 2;
    end;
  end;

  for i := 0 to 63 do
  begin
    j := 1 - (i and 1);
    while j < 64 do
    begin
      if i = 0 then above := 63 else above := i - 1;
      if i = 63 then below := 0 else below := i + 1;
      if j = 0 then left := 63 else left := j - 1;
      if j = 63 then right := 0 else right := j + 1;
      total := fbuf^[64 * above + j] +
               fbuf^[64 * below + j] +
               fbuf^[64 * i + left] +
               fbuf^[64 * i + right];
      total := total div 4;
      fbuf^[64 * i + j] := total;
      j := j  +2;
    end;
  end;
end;

// A primitive not-quite-random-field image-writing thing
procedure basic_background2(const fbuf: PByteArray; const bottom: byte; const range: integer);
var
  i, j: integer;
  above, below, left, right, total: integer;
begin
  // The randomly-set large grid
  i := 0;
  while i < 64 do
  begin
    j := 0;
    while j < 64 do
    begin
      fbuf^[64 * i + j] := bottom + roll(range);
      j := j + 2;
    end;
    i := i + 2;
  end;

  // The quinicunx points of the large grid
  i := 1;
  while i < 64 do
  begin
    j := 1;
    while j < 64 do
    begin
      if i = 0 then above := 63 else above := i - 1;
      if i = 63 then below := 0 else below := i + 1;
      if j = 0 then left := 63 else left := j - 1;
      if j = 63 then right := 0 else right := j + 1;
      total := fbuf^[64 * above + left] +
               fbuf^[64 * below + left] +
               fbuf^[64 * above + right] +
               fbuf^[64 * below + right] + 2;
      total := total div 4;
      fbuf^[64 * i + j] := total;
      j := j + 2;
    end;
    i := i + 2;
  end;

  // The remaining grid
  for i := 0 to 63 do
  begin
    j := 1 - (i and 1);
    while j < 64 do
    begin
      if i = 0 then above := 63 else above := i - 1;
      if i = 63 then below := 0 else below := i + 1;
      if j = 0 then left := 63 else left := j - 1;
      if j = 63 then right := 0 else right := j + 1;
      total := fbuf^[64 * above + j] +
               fbuf^[64 * below + j] +
               fbuf^[64 * i + left] +
               fbuf^[64 * i + right] + 2;
      total := total div 4;
      fbuf^[64 * i + j] := total;
      j := j + 2;
    end;
  end;
end;

// A primitive not-quite-random-field image-writing thing
procedure basic_background3(const fbuf: PByteArray; const bottom: byte; const range: integer);
var
  i, j: integer;
  above, below, left, right, total: integer;
begin
  i := 0;
  while i < 64 do
  begin
    j := (i and 2);
    while j < 64 do
    begin
      fbuf^[64 * i + j] := bottom + roll(range);
      j := j + 4;
    end;
    i := i + 2;
  end;

  i := 0;
  while i < 64 do
  begin
    j := 2 - (i and 2);
    while j < 64 do
    begin
      if i < 2 then above := i + 62 else above := i - 2;
      if i > 61 then below := i - 62 else below := i + 2;
      if j < 2 then left := j + 62 else left := j - 2;
      if j > 61 then right := j - 62 else right := j + 2;
      total := fbuf^[64 * above + j] +
               fbuf^[64 * below + j] +
               fbuf^[64 * i + left] +
               fbuf^[64 * i + right];
      total := total div 4;
      total := total + (roll(4) - roll(4));
      if total < bottom then total := bottom;
      if total >= bottom + range then total := bottom + range - 1;
      fbuf^[64 * i + j] := total;
      j := j + 4;
    end;
    i := i + 2;
  end;

  i := 1;
  while i < 64 do
  begin
    j := 1 - (i and 1);
    while j < 64 do
    begin
      if i = 0 then above := 63 else above := i - 1;
      if i = 63 then below := 0 else below := i + 1;
      if j = 0 then left := 63 else left := j - 1;
      if j = 63 then right := 0 else right := j + 1;
      total := fbuf^[64 * above + j] +
               fbuf^[64 * below + j] +
               fbuf^[64 * i + left] +
               fbuf^[64 * i + right];
      total := total + (roll(2) - roll(2));
      if total < bottom then total := bottom;
      if total >= bottom + range then total := bottom + range - 1;
      total := total div 4;
      fbuf^[64 * i + j] := total;
      j := j + 2;
    end;
    i := i + 2;
  end;
end;

var
  fbuf: array[0..64 * 64 + 3] of byte; // For use in making custom flats and patches; 64x64
  pbuf: array[0..($80 + 9) * $40 + 7] of byte; // Also

// Record any custom flats that we might want to show off by using.
// This is *much* simpler than textures!
procedure record_custom_flats(const dh: dump_record_p; const c: config_p;
  const even_unused: boolean);
var
  i, j, x, x2, y, dx, dy: SmallInt;
  started: boolean;
begin
  started := FALSE;

  if even_unused or find_flat(c, 'SLGRASS1')^.used then
  begin
    if not started then
    begin
      if c^.fstart then RegisterLmp(dh, 'F_START', 0);
      if c^.ffstart then RegisterLmp(dh, 'FF_START', 0);
    end;
    started := TRUE;
    announce(VERBOSE, 'SLGRASS1');

    basic_background2(@fbuf, $7c, 4);
    x := roll(64);
    y := roll(64);
    dx := 0; // JVAL: Shut up compiler warning
    dy := 0; // JVAL: Shut up compiler warning
    while true do
    begin
      dx := 1 - roll(3);
      dy := 1 - roll(3);
      if (dx and dy) <> 0 then break;
    end;
    for i := 0 to 511 do
    begin
      x := x + dx;
      y := y + dy;
      if x < 0 then x := x + 64;
      if x > 63 then x := x - 64;
      if y < 0 then y := y + 64;
      if y > 63 then y := y - 64;
      fbuf[64 * x + y] := $bc + roll(4);
      if x = 0 then x2 := 63 else x2 := x - 1;
      fbuf[64 * x2 + y] := $bc;
      if x = 63 then x2 := 0 else x2 := x + 1;
      fbuf[64 * x2 + y] := $bf;
      if roll(8) = 0 then dx := 1 - roll(3);
      if roll(8) = 0 then dy := 1 - roll(3);
      while not ((dx <> 0) or (dy <> 0)) do
      begin
        dx := 1 - roll(3);
        dy := 1 - roll(3);
      end;
    end;

    RegisterLmp(dh, 'SLGRASS1', 4096);
    fwrite(@fbuf, 4096, 1, dh^.f);
  end;

  if even_unused or find_flat(c, 'SLSPARKS')^.used then
  begin
    if not started then
    begin
      if c^.fstart then RegisterLmp(dh, 'F_START' ,0);
      if c^.ffstart then RegisterLmp(dh, 'FF_START', 0);
    end;
    started := TRUE;
    announce(VERBOSE, 'SLSPARKS');
    memset(@fbuf, 0, 4096);
    for i := 0 to 511 do
      fbuf[roll(64) + 64 * roll(64)] := $b0 + roll(16);
    RegisterLmp(dh, 'SLSPARKS', 4096);
    fwrite(@fbuf, 4096, 1, dh^.f);
  end;

  if even_unused or find_flat(c, 'SLGATE1')^.used then
  begin
    if not started then
    begin
      if c^.fstart then RegisterLmp(dh, 'F_START', 0);
      if c^.ffstart then RegisterLmp(dh, 'FF_START', 0);
    end;
    started := TRUE;
    announce(VERBOSE, 'SLGATE1');

    basic_background2(@fbuf, $9c, 4);

    for i := 4 to 59 do
    begin
      for j := 4 to 59 do
      begin
        dx := abs((i * 2) - 63) div 4;
        dy := abs((j * 2) - 63) div 4;
        x := $cf - (dx + dy) div 2;
        x := x + roll(2);
        x := x - roll(2);
        if x > $cf then x := $cf;
        if x < $c0 then x := $c0;
        fbuf[64 * i + j] := x;
      end;
    end;

    RegisterLmp(dh, 'SLGATE1', 4096);
    fwrite(@fbuf, 4096, 1, dh^.f);
  end;

  if even_unused or find_flat(c, 'SLLITE1')^.used then
  begin
    if not started then
    begin
      if c^.fstart then RegisterLmp(dh, 'F_START', 0);
      if c^.ffstart then RegisterLmp(dh, 'FF_START', 0);
    end;
    started := TRUE;
    announce(VERBOSE, 'SLLITE1');

    basic_background2(@fbuf, $94, 4);

    for i := 0 to 3 do
      for j := 0 to 3 do
        for x := 3 to 12 do
          for y := 3 to 12 do
          begin
            if ((x = 3) or (x = 12)) and ((y = 3) or (y = 12)) then
              continue;
            dx := abs((x * 2) - 15) div 4;
            dy := abs((y * 2) - 15) div 4;
            if dy > dx then dx := dy;
            x2 := $a1 + 2 * dx;
            x2 := x2 + (roll(2) - roll(2));
            if x2 > $a7 then x2 := $a7;
            if x2 < $a0 then x2 := $a0;
            fbuf[64 * (16 * i + x) + 16 * j + y] := x2;
          end;

    RegisterLmp(dh, 'SLLITE1', 4096);
    fwrite(@fbuf, 4096, 1, dh^.f);
  end;

  if even_unused or find_flat(c, 'SLFLAT01')^.used then
  begin
    if started then
    begin
      if c^.fstart then RegisterLmp(dh, 'F_START', 0);
      if c^.ffstart then RegisterLmp(dh, 'FF_START', 0);
    end;
    started := TRUE;
    announce(VERBOSE, 'SLFLAT01');

    basic_background2(@fbuf, $6b, 5);
    for i := 0 to 4095 do
      if fbuf[i] > $6d then fbuf[i] := 0;
    for i := 0 to 63 do
    begin
      fbuf[i] := $6b;
      fbuf[64 * i] := $6b;
      fbuf[63 * 64 + i] := $6f;
      fbuf[64 * i + 63] := $6f;
    end;

    RegisterLmp(dh, 'SLFLAT01', 4096);
    fwrite(@fbuf, 4096, 1, dh^.f);
  end;

  if started then
  begin
    if c^.ffend then RegisterLmp(dh, 'FF_END', 0);
    if c^.fend then RegisterLmp(dh, 'F_END', 0);
  end;
end;

// Record any custom/replacement patches that we might want to show off
// by using.
procedure record_custom_patches(const dh: dump_record_p; const c: config_p;
  const even_unused: boolean);
var
  rows, columns, i, j, lsize: integer;
  p: PByteArray;
  thispel: byte;
  started: boolean;
begin
  started := FALSE;

  if even_unused then
  begin
    if not started then
    begin
      RegisterLmp(dh, 'P_START', 0); // Which?  Both? */
      RegisterLmp(dh, 'PP_START', 0);
      started := TRUE;
    end;

    rows := $80;
    columns := $40;
    lsize := TLMPSIZE(rows, columns);
    if lsize > SizeOf(pbuf) then
      announce(ERROR, 'Buffer overflow in record_custom_patches()');
    p := @pbuf[0];
    // The picture header
    _output_short(p, columns); // Width
    _output_short(p, rows); // Height
    _output_short(p, (columns div 2) - 1); // Width offset
    _output_short(p, rows - 5); // Magic
    // The pointers to the columns
    for i := 0 to columns - 1 do
      _output_int(p, 8 + 4 * (columns) + i * (rows + 5));

    // The columns themselves
    for i := 0 to columns - 1 do
    begin
      // The column header
      _output_byte(p, 0);
      _output_byte(p, rows);
      // The column itself, including silly bytes
      for j := -1 to rows do
      begin
        if rollpercent(10) then
          thispel := $c0 + roll(16)
        else
          thispel := 0;
        _output_byte(p, thispel);
      end;
      // And finally
      _output_byte(p, $ff);
    end;
    // Whew!

    RegisterLmp(dh, 'WALL51_1', lsize); // DOOM I only
    fwrite(@pbuf, lsize, 1, dh^.f);
  end;

  if even_unused then
  begin
    if not started then
    begin
      RegisterLmp(dh, 'P_START', 0); // Which?  Both?
      RegisterLmp(dh, 'PP_START', 0);
      started := TRUE;
    end;

    rows := $80;
    columns := $40;
    lsize := TLMPSIZE(rows, columns);
    if lsize > SizeOf(pbuf) then
      announce(ERROR, 'Buffer overflow in record_custom_patches()');
    p := @pbuf[0];
    // The picture header
    _output_short(p, columns); // Width
    _output_short(p, rows); // Height
    _output_short(p, (columns div 2) - 1); // Width offset
    _output_short(p, rows - 5); // Magic
    // The pointers to the columns
    for i := 0 to columns - 1 do
      _output_int(p, 8 + 4 * (columns) + i * (rows + 5));

    // The columns themselves
    for i := 0 to columns - 1 do
    begin
      // The column header
      _output_byte(p, 0);
      _output_byte(p, rows);
      // The column itself, including silly bytes
      for j := -1 to rows do
      begin
        if rollpercent(20) then
          thispel := $d0 + roll(16)
        else
          thispel := 0;
        _output_byte(p, thispel);
      end;
      // And finally
      _output_byte(p, $ff);
    end;
    // Whew!

    RegisterLmp(dh, 'WALL51_2', lsize);
    fwrite(@pbuf, lsize, 1, dh^.f);
  end;

  // Next is the steel-rollup-door patch.  It's currently put into
  // the WALL51_3 slot, which means it appears in texture SP_DUDE5
  // (instead of the yucchy dead guy hanging on the wall.)  The
  // internal name for the texture is SLDOOR1.

  if even_unused or find_texture(c, 'SLDOOR1')^.used then
  begin
    if not started then
    begin
      RegisterLmp(dh, 'P_START', 0); // Which?  Both?
      RegisterLmp(dh, 'PP_START', 0);
      started := TRUE;
    end;

    // First a little correlated noise for "dirtying"
    basic_background2(@fbuf, 0, 5);

    // Then the actual patch
    rows := $80;
    columns := $40;
    lsize := TLMPSIZE(rows, columns);
    if lsize > SizeOf(pbuf) then
      announce(ERROR, 'Buffer overflow in record_custom_patches()');
    p := @pbuf[0];
    // The picture header
    _output_short(p, columns); // Width
    _output_short(p, rows); // Height
    _output_short(p, (columns div 2) - 1); // Width offset
    _output_short(p, rows - 5); // Magic
    // The pointers to the columns
    for i := 0 to columns - 1 do
      _output_int(p, 8 + 4 * (columns) + i * (rows + 5));

    // The columns themselves
    for i := 0 to columns - 1 do
    begin
      // The column header
      _output_byte(p, 0);
      _output_byte(p, rows);
      // The column itself, including silly bytes
      for j := -1 to rows do
      begin
        thispel := $60 + (j + 1) mod 8;
        if (j >= 0) and (j < rows) then
          thispel := thispel + 2 - fbuf[64 * i + (j and 63)];
        _output_byte(p, thispel);
      end;
      // And finally
      _output_byte(p, $ff);
    end;
    // Whew!

    RegisterLmp(dh, 'WALL51_3', lsize);
    fwrite(@pbuf, lsize, 1, dh^.f);
  end;

  if started then
  begin
    RegisterLmp(dh, 'PP_END', 0);
    RegisterLmp(dh, 'P_END', 0);
  end;
end;

// Compose replacements for the music sections used by the
// given config, and send them out the dumphandle.
procedure make_music(const dh: dump_record_p; const c: config_p);
var
  mh: musheader_t;
  musbuf: PByteArray;
begin
  // Definitely a stub!
  if c^.gamemask and DOOM1_BIT <> 0 then
  begin
    musbuf := one_piece(@mh);
    record_music(dh, @mh,musbuf, 'D_INTROA', c);
    SL_Free(musbuf);
  end;
  if c^.gamemask and DOOM2_BIT <> 0 then
  begin
    musbuf := one_piece(@mh);
    record_music(dh, @mh,musbuf, 'D_DM2TTL', c);
    SL_Free(musbuf);
  end;
end; // end stubby make_music()

// Should there be a secret level after the current level?
function need_secret_level(const c: config_p): boolean;
begin
  if not c^.do_seclevels then
  begin
    result := FALSE;
    exit;
  end;
  if (c^.map = 15) or (c^.map = 31) then
  begin
    result := TRUE;
    exit;
  end;
  case c^.episode of
    1: result := c^.mission = 3;
    2: result := c^.mission = 5;
    3: result := c^.mission = 6;
    4: result := c^.mission = 2;
  else
    result := FALSE;
  end;
end;

var
  static_sc: config_t;

// Make a secret level following the current level.  With luck,
// the current level has an exit to it!
procedure make_secret_level(const dh: dump_record_p; const oldhaa: haa_p;
  const c: config_p);
var
  SecConfig: config_p;
  SecLevel: level_t;
  SecHaa: haa_p;
begin
  SecConfig := @static_sc;
  memcpy(SecConfig, c, SizeOf(config_t));
  SecConfig^.configfile := c^.configfile;
  SecConfig^.configdata := c^.configdata;
  SecConfig^.outfile := c^.outfile; // Name of the output file

  SecHaa := haa_p(SL_Malloc(SizeOf(haa_t)));
  memcpy(SecHaa, oldhaa, SizeOf(haa_t));
  secretize_config(SecConfig);
  if SecConfig^.map = 31 then
    SecConfig^.map := 32
  else if SecConfig^.map = 15 then
    SecConfig^.map := 31;
  if SecConfig^.episode <> 0 then
    SecConfig^.mission := 9;
  NewLevel(@SecLevel, SecHaa, SecConfig);
  DumpLevel(dh, SecConfig, @SecLevel, SecConfig^.episode, SecConfig^.mission, SecConfig^.map);
  if SecConfig^.map = 31 then
  begin
    SecConfig^.map := 32;
    SecConfig^.secret_themes := TRUE;
    NewLevel(@SecLevel, SecHaa, SecConfig);
    DumpLevel(dh, SecConfig, @SecLevel, SecConfig^.episode, SecConfig^.mission, SecConfig^.map);
  end;
end;

// Can this link be locked to the given quest?   Note that this is
// only called to check if an existing link made up at random can
// be locked, so it can false-negative sometimes safely.
function link_fitsq(const ThisLink: link_p; const ThisQuest: quest_p): boolean;
begin
  if ThisQuest = nil then
  begin
    result := TRUE; // Nothing to fit
    exit;
  end;

  if ThisQuest^.goal = GATE_GOAL then
  begin
    if ThisLink^.typ = OPEN_LINK then
      result := TRUE // Easy
    else
      result := FALSE; // else punt
    exit;
  end;

  // Keys and switches require doors
  if (ThisQuest^.goal = KEY_GOAL) or (ThisQuest^.goal = SWITCH_GOAL) then
  begin
    if ThisLink^.bits and LINK_NEAR_DOOR = 0 then
    begin
      result := FALSE;
      exit;
    end;
    if ThisLink^.typ <> BASIC_LINK then
    begin
      result := FALSE;
      exit;
    end;
  end;

  // Actually because of nukage locks, SWITCH_GOALs don't require
  // doors.  Do something about that here.
  // For that matter, there are kinds of OPEN_LINK and GATE_LINK
  // that can fit a SWITCH_GOAL, also.  So fix that, too.
  result := TRUE;
end;

// Will this link fit along this linedef?
function link_fitsh(const ldf: linedef_p; const ThisLink: link_p; const c: config_p): boolean;
var
  available, required: integer;
begin
  available := linelen(ldf);
  required := ThisLink^.width1;

  case ThisLink^.typ of
   BASIC_LINK:
     begin
       if required = 0 then required := 64; // Minimum to pass, eh?
       if ThisLink^.bits and LINK_TWIN <> 0 then available := (available div 2) - 16;
       if ThisLink^.bits and LINK_ALCOVE <> 0 then
         required := required * 2 + ThisLink^.depth3;
     end;
   OPEN_LINK:
     begin
       if required = 0 then required := 33;
        required := required + 66;
     end;
   GATE_LINK:
     begin
     // No gate-links outgoing from a gate room, eh?
       if ldf^.right^.sector^.gate <> nil then
       begin
         result := FALSE;
         exit;
       end;
       result := TRUE;
       exit;
     end;
   else
     begin
       announce(WARNING, 'Funny type in link_fitsh');
       result := FALSE;
       exit;
     end;
  end;

  result := available >= required;
end;

// Make the given linedefs (which currently form an open archway)
// into a cool set-of-bars door (appropriate to the quest).  For
// the door/bar sectors, use newsector, or a new one if nil.
// Note that barwidth must be 32 or less, because the algorithm
// has some quirks that I really ought to fix; if barwidth is too
// big, it can recurse forever or something like that.
procedure barify(const l: level_p; const ldf1, ldf2: linedef_p; const ThisQuest: quest_p;
  const barwidth: integer; newsector: sector_p; const ThisStyle: style_p; const c: config_p);
var
  ld1a, ld1b, ld2a, ld2b, ldedge1, ldedge2: linedef_p;
  oldsector: sector_p;
  t1: texture_p;
  type1: SmallInt;
begin
  if linelen(ldf1) <= 32 then exit; // Already impassable

  // Get a handle on the sectors involved
  oldsector := ldf1^.left^.sector;
  if newsector = nil then
  begin
    newsector := clone_sector(l, oldsector);
    newsector^.ceiling_height := newsector^.floor_height; // close it!
    if ThisQuest <> nil then
      if ThisQuest^.goal = SWITCH_GOAL then
        newsector^.tag := ThisQuest^.tag;
    announce(VERBOSE, 'Multiple');
  end;

  // Then recurse to get the side bars, if needed
  ld1a := line_center_part(l, ldf1, @ld1b, barwidth, ThisStyle, c);
  ld2a := line_center_part(l, ldf2, @ld2b, barwidth, ThisStyle, c);
  barify(l, ldf1, ld2b, ThisQuest, barwidth, newsector, ThisStyle, c);
  barify(l, ld1b, ldf2, ThisQuest, barwidth, newsector, ThisStyle, c);
  // Now frame the center section, the a's, with linedefs
  ldedge1 := new_linedef(l, ld2a^._to, ld1a^.from);
  ldedge2 := new_linedef(l, ld1a^._to, ld2a^.from);
  // Fix up existing sidedefs
  ld1a^.left^.sector := newsector;
  ld1a^.flags := ld1a^.flags and not UPPER_UNPEGGED;
  ld1a^.right^.x_offset := 0;
  ld2a^.left^.sector := newsector;
  ld2a^.flags := ld2a^.flags and not UPPER_UNPEGGED;
  ld2a^.right^.x_offset := 0;
  // And make some new ones
  ldedge1^.left := new_sidedef(l, newsector, c);
  ldedge1^.right := new_sidedef(l, oldsector, c);
  ldedge1^.flags := ldedge1^.flags or TWO_SIDED;
  ldedge2^.left := new_sidedef(l, newsector, c);
  ldedge2^.right := new_sidedef(l, oldsector, c);
  ldedge2^.flags := ldedge2^.flags or TWO_SIDED;
  // Decide on a texture for the bar faces
  t1 := ThisStyle^.support0; // Or wall0?
  if ThisQuest <> nil then
    if ThisQuest^.goal = KEY_GOAL then
      t1 := texture_for_key(ThisQuest^.typ, ThisStyle, c);
  // and the opening linedef type
  type1 := ThisStyle^.doortype;
  if ThisQuest <> nil then
  begin
    if ThisQuest^.goal = KEY_GOAL then
      type1 := type_for_key(ThisQuest^.typ)
    else if ThisQuest^.goal = SWITCH_GOAL then
    begin
      if c^.do_dm then
        type1 := LINEDEF_NORMAL_S1_DOOR
      else
        type1 := LINEDEF_NORMAL;
    end;
  end;
  // Now fill in all the textures and stuff
  ld1a^.typ := type1;
  ld2a^.typ := type1;
  ld1a^.right^.upper_texture := t1;
  ld2a^.right^.upper_texture := t1;
  ldedge1^.left^.middle_texture := c^.null_texture;
  ldedge1^.right^.middle_texture := c^.null_texture;
  ldedge1^.right^.upper_texture := t1;
  ldedge2^.left^.middle_texture := c^.null_texture;
  ldedge2^.right^.middle_texture := c^.null_texture;
  ldedge2^.right^.upper_texture := t1;
  // Record that we did that
  inc(l^.barcount);

end; // end barify

// Take the given linedefs (which are currently antiparallel with
// just the procedure between them), and put in a nice too-narrow-to-
// pass slit.  Or, sometimes, split the current linedefs in half
// and recurse on the halves, for a set of slits.  Use the given
// sector for the sector in the slits, or make one if nil.
// Sort of like both barify() and make_window().
function slitify(const l: level_p; const ldf1, ldf2: linedef_p; const slitwidth: integer;
  newsector: sector_p; const ThisStyle: style_p; const c: config_p): boolean;
var
  ld1a, ld2a, ldedge1, ldedge2: linedef_p;
  nearsector, farsector: sector_p;
  newch, newch2, newfh, newfh2: SmallInt;
  len: integer;
begin
  len := linelen(ldf1);

  // Get a handle on the sectors involved
  nearsector := ldf1^.right^.sector;
  farsector := ldf2^.right^.sector;
  // Invent the new one, if needed
  if newsector = nil then
  begin
    newfh := nearsector^.floor_height;
    if farsector^.floor_height < newfh then
      newfh := farsector^.floor_height;
    if rollpercent(30) then
    begin
      newfh2 := newfh + 4 * roll(9);
      if newfh2 > (nearsector^.ceiling_height - 32) then
        newfh2 := newfh;
      if newfh2 > (farsector^.ceiling_height - 32) then
        newfh2 := newfh;
      newfh := newfh2;
    end;
    newch := nearsector^.ceiling_height;
    if farsector^.ceiling_height > newch then
      newch := farsector^.ceiling_height;
    if rollpercent(30) then
    begin
      newch2 := newfh + 32 + 8 * roll(9);
      if newch2 > newch then
        newch2 := newch;
      if newch < (nearsector^.floor_height + 32) then
        newch2 := newch;
      if newch < (farsector^.floor_height + 32) then
        newch2 := newch;
      newch := newch2;
    end;
    newsector := clone_sector(l, nearsector);
    newsector^.floor_height := newfh;
    newsector^.ceiling_height := newch;
  end;
  // Sometimes just recurse
  if (len > (16 + slitwidth + slitwidth)) and (rollpercent(60)) then
  begin
    ld1a := split_linedef(l, ldf1, len div 2, c);
    ld2a := split_linedef(l, ldf2, len div 2, c);
    slitify(l, ldf1, ld2a, slitwidth, newsector, ThisStyle, c);
    slitify(l, ld1a, ldf2, slitwidth, newsector, ThisStyle, c);
  end
  else
  begin
    ld1a := line_center_part(l, ldf1, nil, slitwidth, ThisStyle, c);
    ld2a := line_center_part(l, ldf2, nil, slitwidth, ThisStyle, c);
    ldedge1 := new_linedef(l, ld2a^.from, ld1a^._to);
    ldedge2 := new_linedef(l, ld1a^.from, ld2a^._to);
    // Fix up existing sidedefs
    ld1a^.right^.middle_texture := c^.null_texture; // Or grating?
    ld1a^.flags := ld1a^.flags or TWO_SIDED;
    ld2a^.right^.middle_texture := c^.null_texture; // Or grating?
    ld2a^.flags := ld2a^.flags or TWO_SIDED;
    // and make new ones
    ldedge1^.right := new_sidedef(l, newsector, c);
    ldedge1^.right^.middle_texture := ldf1^.right^.middle_texture;
    ldedge2^.right := new_sidedef(l, newsector, c);
    ldedge2^.right^.middle_texture := ldf1^.right^.middle_texture;
    ldedge1^.right^.y_offset :=
      nearsector^.ceiling_height - newsector^.ceiling_height;
    ldedge2^.right^.y_offset := ldedge1^.right^.y_offset;
    ld1a^.left := new_sidedef(l, newsector, c);
    ld1a^.left^.middle_texture := c^.null_texture;
    patch_upper(ld1a, ldf1^.right^.middle_texture, c);
    patch_lower(ld1a,ThisStyle^.support0,c);
    ld2a^.left := new_sidedef(l, newsector, c);
    ld2a^.left^.middle_texture := c^.null_texture;
    patch_upper(ld2a, ldf2^.right^.sector^.style^.wall0, c);
    patch_lower(ld2a, ldf2^.right^.sector^.style^.support0, c);
    announce(VERBOSE, 'Slit');
  end;

  result := TRUE;

end; // end slitify

// OK, so you have a single square sector bounded by ldf1 and ldf2
// at the ends, and lde1 and lde2 at the sides.  They point
// *counterclockwise* around the outside of the sector.  This
// will make it into a flight of climbable stairs from nearheight
// at the ldf1 end to farheight at the ldf2 end.
// Should level-hugeness do anything in here?
procedure stairify(const l: level_p; ldf1, ldf2: linedef_p;
  lde1, lde2: linedef_p; nearheight, farheight: SmallInt;
  const ThisQuest: quest_p; const ThisStyle: style_p; const c: config_p);
var
  ldn1, ldn2, ldl: linedef_p;
  s, nearsec: sector_p;
  len, stepdepth, i: integer;
  minstepcount, maxstepcount, stepcount, stepheight: integer;
  need_lock: boolean;
  front: texture_p;
  do_edges: boolean;
begin
  need_lock := (ThisQuest <> nil) and (ThisQuest^.goal = SWITCH_GOAL);
  front := ThisStyle^.kickplate;
  do_edges := FALSE;

  nearsec := ldf1^.right^.sector;
  len := linelen(lde1);

  // Need at least enough steps to get up, 24 at a time
  minstepcount := (farheight - nearheight) div 24;
  // Want steps at least 24 deep to fit in the space
  maxstepcount := len div 24;

  stepcount := minstepcount + roll(1 + (maxstepcount - minstepcount));
  stepdepth := len div stepcount;
  stepheight := (farheight - nearheight) div stepcount;

  // Hack, to avoid having to actually understand the above
  if stepheight > 24 then
  begin
    stepcount := stepcount + 1;
    stepdepth := len div stepcount;
    stepheight := (farheight - nearheight) div stepcount;
  end;
  if stepheight > 24 then
    announce(ERROR, 'Step too high to climb!');
  if need_lock then
  begin
    announce(LOG, 'Locked stairs');
    stepheight := 8;
    stepcount := (farheight - nearheight) div stepheight;
    stepdepth := len div stepcount;
  end;

  announce(VERBOSE,
    Format('%d steps from [%d-%d], each %d deep and %d high.'#13#10,
      [stepcount, minstepcount, maxstepcount, stepdepth, stepheight]));
  announce(VERBOSE,
    Format('Total: %d deep, %d high.'#13#10,
      [len, farheight - nearheight]));

  if ThisStyle^.stepfront <> nil then
    if abs(stepheight) <= ThisStyle^.stepfront^.height then
      front := ThisStyle^.stepfront;

  if ThisStyle^.walllight <> nil then
  begin
    if ThisStyle^.light_steps then
      front := ThisStyle^.walllight
    else if ThisStyle^.light_edges and (linelen(ldf1) >= (64 * l^.hugeness)) and (stepheight > 7) then
    begin
      do_edges := TRUE;
      announce(VERBOSE, 'Step-edge lights');
    end;
  end;

  if need_lock then
    ThisQuest^.typ := LINEDEF_S1_RAISE_STAIRS;

  ldf1^.right^.lower_texture := front;
  ldf1^.flags := ldf1^.flags and not LOWER_UNPEGGED;

  for i :=0 to stepcount - 2 do // Minus one, since one's already made
  begin
    s := clone_sector(l, nearsec);
    s^.ceiling_height := ldf2^.right^.sector^.ceiling_height;
    if need_lock then
      if i = 0 then
        s^.tag := ThisQuest^.tag;
    len := len - stepdepth;
    ldn1 := split_linedef(l, lde1, len, c);
    ldn2 := lde2;
    lde2 := split_linedef(l, ldn2, stepdepth, c);
    ldn1^.left^.sector := s;
    ldn2^.left^.sector := s;
    ldf1^.left^.sector := s;
    if do_edges then
    begin
      ldl := split_linedef(l, ldf1, linelen(ldf1) - 16 * l^.hugeness, c);
      ldl^.right^.lower_texture := ThisStyle^.walllight;
      split_linedef(l, ldf1, 16 * l^.hugeness, c);
      ldf1^.right^.lower_texture := ThisStyle^.walllight;
    end;
    nearheight := nearheight + stepheight;
    s^.floor_height := nearheight;
    ldf1 := new_linedef(l, ldn1^.from, ldn2^._to);
    ldf1^.right := new_sidedef(l, s, c);
    ldf1^.right^.lower_texture := front;
    ldf1^.flags := ldf1^.flags and not LOWER_UNPEGGED;
    ldf1^.right^.middle_texture := c^.null_texture;
    ldf1^.left := new_sidedef(l, s, c); // s is wrong; fixed above/below
    ldf1^.left^.middle_texture := c^.null_texture;
    ldf1^.flags := ldf1^.flags or TWO_SIDED;
    if need_lock then
      s^.floor_height := nearsec^.floor_height;
    if not need_lock then // recalc to avoid top-step-looks-silly bug
    begin
      stepheight := (farheight - nearheight) div (stepcount - (i + 1));
      if abs(stepheight) > front^.height then
        front := ThisStyle^.kickplate;
    end;
  end;
  ldf1^.left^.sector := ldf2^.left^.sector;
  patch_lower(ldf1, front, c);
  ldf1^.flags := ldf1^.flags and not LOWER_UNPEGGED;
  if do_edges then
  begin
    ldl := split_linedef(l, ldf1, linelen(ldf1) - 16 * l^.hugeness, c);
    ldl^.right^.lower_texture := ThisStyle^.walllight;
    split_linedef(l, ldf1, 16 * l^.hugeness, c);
    ldf1^.right^.lower_texture := ThisStyle^.walllight;
  end;
  if need_lock then
  begin
    ldf2^.left^.sector^.floor_height := nearsec^.floor_height;
    ldf2^.left^.sector^.floor_flat := nearsec^.floor_flat;
  end;
end;

// Make the given sector into a standard door, opened by the
// given linedefs.  Doesn't do any flipping, or alter jambs.
procedure doorify(s: sector_p; const ldf1, ldf2: linedef_p;
  const ThisStyle, NewStyle: style_p; const c: config_p);
// Needs to use style more, but almost done
var
  lensq: integer;
begin
  s^.ceiling_height := s^.floor_height;
  s^.floor_flat := ThisStyle^.doorfloor;
  s^.ceiling_flat := ThisStyle^.doorceiling;
  // This should be from the style or link or role or something
  ldf1^.typ := ThisStyle^.doortype;
  lensq := lengthsquared(ldf1);
  // This "100" should be determined from the width of the style textures
  if lensq > (100 * 100) then // One of the wide textures
  begin
    if ldf1^.right^.sector^.ceiling_height - s^.floor_height > ThisStyle^.widedoorface^.height then
      ldf1^.right^.upper_texture := ThisStyle^.twdoorface
    else
      ldf1^.right^.upper_texture := ThisStyle^.widedoorface;
    if lensq < (128 * 128) then // "128" is wrong
      ldf1^.right^.x_offset := (128 - linelen(ldf1)) div 2 // All of these
    else
      ldf1^.right^.x_offset := 128 - (linelen(ldf1) mod 128) div 2;
    // Avoid TFE!
    if ldf1^.right^.upper_texture^.height < 128 then
      if ldf1^.right^.sector^.ceiling_height - ldf1^.right^.sector^.floor_height > ldf1^.right^.upper_texture^.height then
        ldf1^.right^.upper_texture := ThisStyle^.twdoorface;
  end
  else
  begin
    if ldf1^.right^.sector^.ceiling_height - s^.floor_height > ThisStyle^.narrowdoorface^.height then
      ldf1^.right^.upper_texture := ThisStyle^.tndoorface
    else
      ldf1^.right^.upper_texture := ThisStyle^.narrowdoorface;
    if lensq < (64 * 64) then // Also "64"
      ldf1^.right^.x_offset := (64 - linelen(ldf1)) div 2 // All of these
    else
      ldf1^.right^.x_offset := 64 - (linelen(ldf1) mod 64) div 2;
    // Avoid TFE!
    if ldf1^.right^.upper_texture^.height < 128 then
      if ldf1^.right^.sector^.ceiling_height - ldf1^.right^.sector^.floor_height > ldf1^.right^.upper_texture^.height then
        ldf1^.right^.upper_texture := ThisStyle^.tndoorface;
  end;
  ldf2^.typ := ldf1^.typ;
  lensq := lengthsquared(ldf2);
  // This "100" should be determined from the width of the style textures?
  if lensq > (100 * 100) then // One of the wide textures
  begin
    if ldf2^.right^.sector^.ceiling_height - s^.floor_height > NewStyle^.widedoorface^.height then
      ldf2^.right^.upper_texture := NewStyle^.twdoorface
    else
      ldf2^.right^.upper_texture := NewStyle^.widedoorface;
    if lensq < (128 * 128) then
      ldf2^.right^.x_offset := (128 - linelen(ldf2)) div 2
    else
      ldf2^.right^.x_offset := 128 - (linelen(ldf2) mod 128) div 2;
    // Avoid TFE!
    if ldf2^.right^.upper_texture^.height < 128 then
      if ldf2^.right^.sector^.ceiling_height - ldf2^.right^.sector^.floor_height > ldf2^.right^.upper_texture^.height then
        ldf2^.right^.upper_texture := NewStyle^.twdoorface;
  end
  else
  begin
    if ldf2^.right^.sector^.ceiling_height - s^.floor_height > NewStyle^.narrowdoorface^.height then
      ldf2^.right^.upper_texture := NewStyle^.tndoorface
    else
      ldf2^.right^.upper_texture := NewStyle^.narrowdoorface;
    if lensq < (64 * 64) then
      ldf2^.right^.x_offset := (64 - linelen(ldf2)) div 2
    else
      ldf2^.right^.x_offset := 64 - (linelen(ldf2) mod 64) div 2;
    // Avoid TFE!
    if ldf2^.right^.upper_texture^.height < 128 then
      if ldf2^.right^.sector^.ceiling_height - ldf2^.right^.sector^.floor_height > ldf2^.right^.upper_texture^.height then
        ldf2^.right^.upper_texture := NewStyle^.tndoorface;
  end;
{$IFDEF SOMETIMES_UNPEG_DOORFACES}
  if not ThisStyle^.secret_doors    /* Doors secret-flavor? */
{$ENDIF}
  begin
    ldf1^.flags := ldf1^.flags and not UPPER_UNPEGGED;
    ldf2^.flags := ldf2^.flags and not UPPER_UNPEGGED;
  end;
  if ThisStyle^.soundproof_doors then
  begin
    ldf1^.flags := ldf1^.flags or BLOCK_SOUND;
    ldf2^.flags := ldf2^.flags or BLOCK_SOUND;
  end;
  // And in any case avoid stoop silliness
  ldf1^.flags := ldf1^.flags and not LOWER_UNPEGGED;
  ldf2^.flags := ldf2^.flags and not LOWER_UNPEGGED;

end; // end doorify

// Make a window between the given antiparallel linedefs
function make_window_inner(const l: level_p; ldf1, ldf2: linedef_p;
  const ThisLink: link_p; const ThisStyle, NewStyle: style_p; const c: config_p): boolean;
var
  ldnew1, ldnew2: linedef_p;
  nearsec, farsec, newsec: sector_p;
  t1, t2: texture_p;
  len: integer;
begin
  announce(VERBOSE, 'Making a window');

  nearsec := ldf1^.right^.sector;
  farsec := ldf2^.right^.sector;
  t1 := ldf1^.right^.middle_texture;
  t2 := NewStyle^.wall0;

  // Make sure a window is possible
  if nearsec^.floor_height + ThisStyle^.sillheight > farsec^.ceiling_height - 16 then
  begin
    result := FALSE;
    exit;
  end;
  if nearsec^.floor_height + ThisStyle^.sillheight + ThisStyle^.windowheight < farsec^.floor_height + 16 then
  begin
    result := FALSE;
    exit;
  end;

  if ThisStyle^.slitwindows then
  begin
    result := slitify(l, ldf1, ldf2, 16 + roll(17), nil, ThisStyle, c);
    exit;
  end;

  // Put a little border on it.  Very simple version for now.
  ldf1 := split_linedef(l, ldf1,ThisStyle^.windowborder, c);
  len := linelen(ldf1);
  split_linedef(l, ldf1, len - ThisStyle^.windowborder, c);
  ldf2 := split_linedef(l, ldf2, ThisStyle^.windowborder, c);
  len := linelen(ldf2);
  split_linedef(l, ldf2, len - ThisStyle^.windowborder, c);

  flip_linedef(ldf2);  // parallelize, for make_box
  newsec := make_box_ext(l, ldf1, ldf2, ThisStyle, c, @ldnew1, @ldnew2);
  flip_linedef(ldf2);

  newsec^.floor_height := nearsec^.floor_height + ThisStyle^.sillheight;
  newsec^.ceiling_height := newsec^.floor_height + ThisStyle^.windowheight;
  newsec^.light_level := ThisStyle^.doorlight0; // Wrongish
  newsec^.style := ThisStyle; // Is this right?

  // Various possibilities for window decor
  case ThisStyle^.window_decor of
    WINDOW_JAMBS:
      begin
        ldnew1^.right^.middle_texture := ThisStyle^.doorjamb;
        ldnew2^.right^.middle_texture := ThisStyle^.doorjamb;
      end;
    WINDOW_SUPPORT:
      begin
        ldnew1^.right^.middle_texture := ThisStyle^.support0;
        ldnew2^.right^.middle_texture := ThisStyle^.support0;
      end;
    WINDOW_LIGHT:
      begin
        make_lighted(l, newsec, c);
        if ThisStyle^.walllight <> nil then
        begin
          ldnew1^.right^.middle_texture := ThisStyle^.walllight;
          ldnew2^.right^.middle_texture := ThisStyle^.walllight;
          announce(VERBOSE, 'Lit window');
        end
        else
        begin
          ldnew1^.right^.middle_texture := ThisStyle^.support0;
          ldnew2^.right^.middle_texture := ThisStyle^.support0;
        end;
      end;
    else // WINDOW_NORMAL
      begin
        ldnew1^.right^.y_offset := nearsec^.ceiling_height - newsec^.ceiling_height;
        ldnew2^.right^.y_offset := ldnew1^.right^.y_offset;
      end;
  end;

  if ThisStyle^.window_grate then
  begin
    ldf1^.right^.middle_texture := ThisStyle^.grating;
    // Unpeg, to keep the texture from floating away, eh?
    ldf1^.flags := ldf1^.flags or LOWER_UNPEGGED;
    ldf1^.flags := ldf1^.flags or (TWO_SIDED or IMPASSIBLE);
{$IFDEF OLD_FUNNY_WINDOWGRATES}
    ldf2^.right^.middle_texture := NewStyle^.grating;
    ldf2^.flags := ldf2^.flags or LOWER_UNPEGGED;
    ldf2^.flags := ldf2^.flags or (TWO_SIDED or IMPASSIBLE);
{$ELSE}
    ldf1^.left^.middle_texture := ThisStyle^.grating;
    ldf2^.flags := ldf2^.flags or TWO_SIDED;
{$ENDIF}
    announce(VERBOSE, 'Window grate');
  end
  else
  begin
    ldf1^.flags := ldf1^.flags or (TWO_SIDED or IMPASSIBLE);
    ldf2^.flags := ldf2^.flags or (TWO_SIDED or IMPASSIBLE);
  end;

  ldf1^.left^.y_offset := 0;
  ldf1^.right^.y_offset := 0;
  ldf2^.left^.y_offset := 0;
  ldf2^.right^.y_offset := 0;

  // Should windows ever block sound?  Prolly not!

  // Prevent texture-bleeding bug.  Will this do it?
  // Possibly the LOWER_UNPEGging above did it alread.
  if ThisStyle^.window_grate then
  begin
    if newsec^.floor_height = nearsec^.floor_height then
      inc(newsec^.floor_height);
    if newsec^.floor_height = farsec^.floor_height then
      inc(newsec^.floor_height);
  end;

  patch_upper(ldf1, t1, c);
  patch_upper(ldf2, t2, c);
  patch_lower(ldf1, t1, c);
  patch_lower(ldf2, t2, c);

  result := TRUE;

end; // end make_window_inner

// Make a window between the given antiparallel linedefs,
// possibly elaborately.
function make_window(const l: level_p; const ldf1, ldf2: linedef_p; const ThisLink: link_p;
  const ThisStyle, NewStyle: style_p; const c: config_p): boolean;
var
  ld1n, ld2n, lde1, lde2: linedef_p;
  newsec: sector_p;
  newfh, newch: SmallInt;
  nearsec, farsec: sector_p;
  rc1, rc2: boolean;
begin
  if (distancesquared(ldf1^._to^.x, ldf1^._to^.y, ldf2^.from^.x, ldf2^.from^.y) >= (l^.hugeness * l^.hugeness * 96 * 96)) and c^.window_airshafts then
  begin
    nearsec := ldf1^.right^.sector;
    farsec := ldf2^.right^.sector;
    ld1n := make_parallel(l, ldf1, 16 * l^.hugeness, nil);
    flip_linedef(ld1n);
    ld2n := make_parallel(l, ldf2, 16 * l^.hugeness, nil);
    flip_linedef(ld2n);
    lde1 := new_linedef(l, ld1n^._to, ld2n^.from);
    lde2 := new_linedef(l, ld2n^._to, ld1n^.from);
    newfh := nearsec^.floor_height;
    if farsec^.floor_height < newfh then
      newfh := farsec^.floor_height;
    if rollpercent(50) then
      newfh := newfh - 8 * roll(10);
    newch := nearsec^.ceiling_height;
    if farsec^.ceiling_height > newch then
      newch := farsec^.ceiling_height;
    newch := newch + 16 + 8 * roll(10);
    newsec := new_sector(l, newfh, newch, random_flat0(OUTDOOR, c, nil), nearsec^.ceiling_flat);
    newsec^.style := copy_style(l, nearsec^.style,nearsec^.style^.theme_number, 0, c);
    newsec^.style^.roomlight0 := l^.outside_light_level;
    // Do we want to make the walls OUTDOOR here?
    ld1n^.right := new_sidedef(l, newsec, c);
    ld2n^.right := new_sidedef(l, newsec, c);
    lde1^.right := new_sidedef(l, newsec, c);
    lde2^.right := new_sidedef(l, newsec, c);
    paint_room(l, newsec, newsec^.style, c);
    place_plants(l, 48, newsec, c); // Put in some plants for decor
    rc1 := make_window_inner(l, ldf1, ld1n, ThisLink, ThisStyle,newsec^.style, c);
    newsec^.style^.sillheight := newsec^.style^.sillheight + farsec^.floor_height - newsec^.floor_height;
    rc2 := make_window_inner(l, ld2n, ldf2, ThisLink, newsec^.style, NewStyle, c);
    newsec^.ceiling_flat := c^.sky_flat;
    if rollpercent(l^.p_force_nukage) then
    begin
      newsec^.floor_flat := newsec^.style^.nukage1;
      newsec^.special := NUKAGE1_SPECIAL;
    end;
    if rc1 or rc2 then
      announce(LOG, 'Window airshaft');
    result := rc1 or rc2;
  end
  else
    result := make_window_inner(l, ldf1, ldf2, ThisLink, ThisStyle, NewStyle, c);
end;

// Make a decorative room between the given antiparallel linedefs
function make_decroom(const l: level_p; ldf1, ldf2: linedef_p;
  const c: config_p): boolean;
var
  ldnew1, ldnew2: linedef_p;
  nearsec, farsec, newsec: sector_p;
  t1: texture_p;
  len: integer;
  ThisStyle: style_p;
  lt1, lt2: linedef_p;
begin
  ThisStyle := ldf1^.right^.sector^.style;

  nearsec := ldf1^.right^.sector;
  farsec := ldf2^.right^.sector;
  t1 := ldf1^.right^.middle_texture;

  // Put a little border on it.  Very simple version for now.
  // Don't really need at all if recesses etc, eh?
  ldf1 := split_linedef(l, ldf1, ThisStyle^.windowborder, c);
  len := linelen(ldf1);
  split_linedef(l, ldf1, len - ThisStyle^.windowborder, c);
  ldf2 := split_linedef(l, ldf2, ThisStyle^.windowborder, c);
  len := linelen(ldf2);
  split_linedef(l, ldf2, len - ThisStyle^.windowborder, c);

  flip_linedef(ldf2); // parallelize, for make_box
  newsec := make_box_ext(l, ldf1, ldf2, ThisStyle, c, @ldnew1, @ldnew2);
  flip_linedef(ldf2);

  newsec^.floor_height := nearsec^.floor_height - 8 * (roll(4));
  newsec^.ceiling_height := nearsec^.ceiling_height + 32 + 8 * (roll(6));
  newsec^.light_level := l^.outside_light_level;
  newsec^.ceiling_flat := c^.sky_flat;
  if rollpercent(10) or rollpercent(l^.p_force_nukage) then
  begin
    newsec^.floor_flat := ThisStyle^.nukage1;
    newsec^.special := NUKAGE1_SPECIAL; // Not that you can get in there!
    announce(LOG, 'Intertwin nukage');
  end
  else
    newsec^.floor_flat := random_flat0(OUTDOOR, c, nil);

  newsec^.style := ThisStyle;

  ldnew1^.right^.y_offset := nearsec^.ceiling_height - newsec^.ceiling_height;
  ldnew2^.right^.y_offset := ldnew1^.right^.y_offset;

  ldf1^.right^.middle_texture := ThisStyle^.grating;
  ldf1^.left^.middle_texture := ThisStyle^.grating;
  ldf2^.right^.middle_texture := ThisStyle^.grating;
  ldf2^.left^.middle_texture := ThisStyle^.grating;
  // Unpeg, to keep the texture from floating away, eh?
  ldf1^.flags := ldf1^.flags or LOWER_UNPEGGED;
  ldf2^.flags := ldf2^.flags or LOWER_UNPEGGED;

  ldf1^.flags := ldf1^.flags or (TWO_SIDED or IMPASSIBLE);
  ldf2^.flags := ldf2^.flags or (TWO_SIDED or IMPASSIBLE);
  ldf1^.left^.y_offset := 0;
  ldf1^.right^.y_offset := 0;
  ldf2^.left^.y_offset := 0;
  ldf2^.right^.y_offset := 0;

  patch_upper(ldf1, t1, c);
  patch_upper(ldf2, t1, c);
  patch_lower(ldf1, t1, c);
  patch_lower(ldf2, t1, c);

  len := linelen(ldnew1);
  if len > 31 then // Inset a bit
  begin
    // Unhook from alignment groups, for simplicity
    if ldf1^.group_previous <> nil then
    begin
      ldf1^.group_previous^.group_next := nil;
      ldf1^.group_previous := nil;
    end;
    if ldf1^.group_next <> nil then
    begin
      ldf1^.group_next^.group_previous := nil;
      ldf1^.group_next := nil;
    end;
    if ldf2^.group_previous <> nil then
    begin
      ldf2^.group_previous^.group_next := nil;
      ldf2^.group_previous := nil;
    end;
    if ldf2^.group_next <> nil then
    begin
      ldf2^.group_next^.group_previous := nil;
      ldf2^.group_next := nil;
    end;
    // Do it
    lt1 := split_linedef(l, ldnew1, 8, c); // 8's should vary
    ldnew1^.right^.sector := ldf1^.right^.sector;
    ldnew1^.right^.y_offset := ldf1^.right^.y_offset;
    ldf1^.from := ldnew1^._to;
    lt2 := split_linedef(l, ldnew2, 8, c);
    ldnew2^.right^.sector := ldf2^.right^.sector;
    ldnew2^.right^.y_offset := ldf2^.right^.y_offset;
    ldf2^.from := ldnew2^._to;
    lt2 := split_linedef(l, lt2, len - 16, c);
    lt2^.right^.sector := ldf1^.right^.sector;
    lt2^.right^.y_offset := ldf1^.right^.y_offset;
    ldf1^._to := lt2^.from;
    lt1 := split_linedef(l, lt1, len - 16, c);
    lt1^.right^.sector := ldf2^.right^.sector;
    lt1^.right^.y_offset := ldf2^.right^.y_offset;
    ldf2^._to := lt1^.from;
  end;

  place_plants(l, 48, newsec, c); // Put in some plants for decor

  result := TRUE;

end; // end make_decroom

function texture_for_key(const key: SmallInt; const s: style_p; const c: config_p): texture_p;
begin
  case key of
    ID_BLUEKEY,
    ID_BLUECARD:
      begin
        result := s^.blueface;
        exit;
      end;
    ID_REDKEY,
    ID_REDCARD:
      begin
        result := s^.redface;
        exit;
      end;
    ID_YELLOWKEY,
    ID_YELLOWCARD:
      begin
        result := s^.yellowface;
        exit;
      end;
  end;

  announce(WARNING, 'Unknown key in texture_for_key()');
  result := c^.error_texture;
end;

function texture_for_bits(const pb: LongWord; const s: style_p; const c: config_p): texture_p;
begin
  result := nil;

  case pb of
    BLUE: result := s^.blueface;
    RED: result := s^.redface;
    YELLOW: result := s^.yellowface;
    LIGHT: result := s^.walllight;
  end;
  if result = nil then
    result := s^.wall0;
end;

function type_for_key(const key: SmallInt): SmallInt;
begin
  case key of
    ID_BLUEKEY,
    ID_BLUECARD:
      begin
        result := LINEDEF_BLUE_S1_DOOR;
        exit;
      end;
    ID_REDKEY,
    ID_REDCARD:
      begin
        result := LINEDEF_RED_S1_DOOR;
        exit;
      end;
    ID_YELLOWKEY,
    ID_YELLOWCARD:
      begin
        result := LINEDEF_YELLOW_S1_DOOR;
        exit;
      end;
  end;
  announce(WARNING, 'Unknown key in type_for_key()');
  result := LINEDEF_NORMAL_S1_DOOR;
end;

// Mark the given door of the given level to look like it's locked
// with the given key (thingid).
procedure mark_door_for_key(const l: level_p; const ldf1: linedef_p;
  const key: SmallInt; const ThisStyle: style_p; const c: config_p);
var
  ldf2: linedef_p;
  t1: texture_p;
begin
  if ThisStyle^.gaudy_locks then
  begin
    announce(VERBOSE, 'Gaudy door');
    ldf1^.right^.upper_texture := texture_for_key(key, ThisStyle, c);
  end
  else
  begin
    t1 := texture_for_key(key, ThisStyle, c);
    ldf2 := split_linedef(l, ldf1, 16, c); // '16' is wrong, but not bad
    ldf1^.right^.upper_texture := t1;
    ldf2 := split_linedef(l, ldf2, linelen(ldf2) - 16, c);
    ldf2^.right^.upper_texture := t1;
  end;
end;

procedure mark_door_for_lock(const l: level_p; const ldf1: linedef_p;
  const ThisStyle: style_p; const c: config_p);
begin
  if ThisStyle^.lockdoorface = nil then
    exit;

  if ThisStyle^.lockdoorface^.height <> 128 then
    if ThisStyle^.lockdoorface^.height < (ldf1^.right^.sector^.ceiling_height - ldf1^.right^.sector^.floor_height) then
      exit;
  ldf1^.right^.upper_texture := ThisStyle^.lockdoorface;
  announce(VERBOSE, 'Specially marked door');
end;


// Given a linedef type, return the equivalent linedef type,
// only locked with the given key.  If there isn't one, return 0
function locked_linedef_for(const typ, key: SmallInt; const c: config_p): SmallInt;
begin
  if typ = LINEDEF_S1_OPEN_DOOR then
  begin
    if DOOM0_BIT and c^.gamemask <> 0 then
    begin
      result := 0; // Not in ancient DooMs
      exit;
    end;
    case key of
      ID_BLUEKEY,
      ID_BLUECARD:
        begin
          result := LINEDEF_S1_OPEN_DOOR_BLUE;
          exit;
        end;
      ID_REDKEY,
      ID_REDCARD:
        begin
          result := LINEDEF_S1_OPEN_DOOR_RED;
          exit;
        end;
      ID_YELLOWKEY,
      ID_YELLOWCARD:
        begin
          result := LINEDEF_S1_OPEN_DOOR_YELLOW;
          exit;
        end;
       else
         begin
          announce(ERROR, 'Unknown key in locked_linedef_for');
          result := 0;
          exit;
         end;
    end;
  end
  else
    result := 0;
end;

// Make the sector look like it's in the range of a light
procedure make_lighted(const l: level_p; const s: sector_p; const c: config_p);
begin
  // Too many hardcoded constants!
  if rollpercent(60) then
    if s^.light_level < l^.lit_light_level then
      s^.light_level := l^.lit_light_level;
  if rollpercent(10) then
    s^.special := RANDOM_BLINK;
end;

// Make a nice box with a thing to the left of the linedef
function make_lightbox(const l: level_p; ld: linedef_p; g: genus_p;
  const ThisStyle: style_p; const c: config_p): linedef_p;
var
  len: integer;
  ldb: linedef_p;
  x, y: integer;
  oldsec, newsec: sector_p;
begin
  len := linelen(ld);

  if len < 48 then // All these "48"s should vary, eh? // Hugeness?
  begin
    result := nil;
    exit;
  end;

  if not empty_left_side(l, ld, 48) then
  begin
    result := nil;
    exit;
  end;

  announce(VERBOSE, 'make_lightbox');
  if len > 48 then
    ld := line_center_part(l, ld, nil, 48, ThisStyle, c);
  ldb := lefthand_box(l, ld, 48, ThisStyle, c); // This one too
  ldb^.right^.middle_texture := ThisStyle^.wall0;
  oldsec := ld^.right^.sector;
  newsec := ldb^.right^.sector;
  newsec^.special := ThisStyle^.auxspecial;
  newsec^.floor_height := newsec^.floor_height + ThisStyle^.auxheight;
  if newsec^.ceiling_height - newsec^.floor_height < 64 then
    newsec^.floor_height := newsec^.ceiling_height - 64;
  patch_lower(ld, ThisStyle^.wall0, c);
  point_from(ld^.from^.x, ld^.from^.y, ld^._to^.x,ld^._to^.y, LEFT_TURN, 24, @x, @y);
  point_from(ld^._to^.x, ld^._to^.y, x, y, LEFT_TURN, 24, @x, @y);
  if g^.height > newsec^.ceiling_height - newsec^.floor_height then
    g := ThisStyle^.shortlamp0;
  new_thing(l, x, y, 0, g^.thingid, 7, c);
  if g^.bits and EXPLODES <> 0 then
    announce(VERBOSE, 'Barrelbox');
  result := ld;
end;

// Make a nice bar with lights to the left of the linedef
// Actually looks pretty terrible!  Fix before using
procedure make_lightbar(const l: level_p; ld: linedef_p;
  const pb: LongWord; const ThisStyle: style_p; const c: config_p);
var
  len, wid, dep: integer;
  ldb, lde1, lde2: linedef_p;
  oldsec, newsec: sector_p;
  tx: texture_p;
begin
  len := linelen(ld);
  if len < 16 then
    exit;

  wid := 12 + roll(len - 17);
  dep := 8 + 4 * roll(5);
  if not empty_left_side(l, ld, dep) then
    exit;

  announce(VERBOSE, 'make_lightbar');
  if len > wid then
    ld := line_center_part(l, ld, nil, wid, ThisStyle, c);
  ldb := lefthand_box_ext(l, ld, dep, ThisStyle, c, @lde1, @lde2);
  tx := texture_for_bits(pb, ThisStyle, c);
  ldb^.right^.middle_texture := tx;
  lde1^.right^.middle_texture := tx;
  lde2^.right^.middle_texture := tx;

  tx := lde1^.right^.middle_texture;
  if tx <> ThisStyle^.wall0 then
    if tx^.props and LIGHT = 0 then
      announce(LOG, 'Colorbar');

  oldsec := ld^.right^.sector;
  newsec := ldb^.right^.sector;
  newsec^.special := ThisStyle^.auxspecial;
  if oldsec^.light_level <= l^.lit_light_level then
    if rollpercent(60) then
      newsec^.light_level := oldsec^.light_level + 20;
end;

// Return a <width>-long linedef which is the center of
// the given linedef.  In ld2, return the linedef that
// is the far part of the triply-split line.
function line_center_part(const l: level_p; const ld: linedef_p; const ld2: linedef_pp;
  const width: integer; const ThisStyle: style_p; const c: config_p): linedef_p;
var
  len, border: integer;
  answer, answer2: linedef_p;
begin
  len := linelen(ld);
  border := (len - width) div 2;
  border := border + (len - (width + 2 * border)); // Fix roundoff errors
  if border <= 0 then
  begin
    answer := ld;
    answer2 := ld;
  end
  else
  begin
    answer := split_linedef(l, ld, border, c);
    answer2 := split_linedef(l, answer, width, c);
  end;

  if ld2 <> nil then
    ld2^ := answer2;

  result := answer;
end;

// Return a <width>-long linedef which is the center of
// the given linedef.  Optionally embellish the borders,
// if called for in the style.
function borderize(const l: level_p; const ld: linedef_p; const width: integer;
  const fancy: boolean; const ThisStyle: style_p; const pb: LongWord;
  const keyg: genus_p; const painted_door: PBoolean; const c: config_p): linedef_p;
var
  ld2: linedef_p;
  nearsec: sector_p;
  lsec: sector_p;
  ldt: linedef_p;
  try_keybox: boolean;
  box_light_level, box_special: SmallInt;
  g: genus_p;
begin
  nearsec := ld^.right^.sector;

  result := line_center_part(l, ld, @ld2, width, ThisStyle, c);

  // Now optionally do fancy things to ld and ld2
  // Gotta think of some other fancy things!
  if nearsec^.ceiling_height - nearsec^.floor_height < 88 then
    try_keybox := FALSE
  else
    try_keybox := TRUE;

  if painted_door <> nil then
    painted_door^ := FALSE;
  if ld <> ld2 then
    if fancy then
      if ThisStyle^.lightboxes then
        if linelen(ld) >= 64 then
        begin
          box_light_level := nearsec^.light_level;
          box_special := 0;
          g := keyg;
          if g = nil then
          begin
            if rollpercent(l^.p_barrels * 2) then
              g := random_barrel(c, ThisStyle);
            if g = nil then
              g := ThisStyle^.lamp0;
          end;
          if g^.bits and LIGHT <> 0 then
          begin
            if ThisStyle^.lightbox_lighting = LIGHTBOX_LIGHTED then
              if box_light_level < l^.lit_light_level then
                box_light_level := l^.lit_light_level;
            if ThisStyle^.lightbox_lighting = LIGHTBOX_DARK then
              box_light_level := c^.minlight;
            if rollpercent(20) then
              box_special := RANDOM_BLINK; // 20?
          end;
          ldt := make_lightbox(l, ld, g, ThisStyle, c);
          // Maybe do the cool keybox thing!
          if (ldt <> nil) and (keyg <> nil) and try_keybox then
          begin
            lsec := ldt^.left^.sector;
            lsec^.floor_height := nearsec^.floor_height + 72;
            lsec^.ceiling_height := lsec^.floor_height + 32;
            patch_upper(ldt, nearsec^.style^.wall0, c);
            patch_lower(ldt, nearsec^.style^.wall0, c);
            ldt^.flags := ldt^.flags or IMPASSIBLE;
            lsec^.special := GLOW_BLINK;
            if lsec^.light_level < l^.lit_light_level then
              lsec^.light_level := l^.lit_light_level;
            if painted_door <> nil then
              painted_door^ := TRUE;
            announce(LOG, 'Keybox');
          end
          else if ldt <> nil then
          begin
            ldt^.left^.sector^.light_level := box_light_level;
            ldt^.left^.sector^.special := box_special;
          end;
          ldt := make_lightbox(l, ld2, g, ThisStyle, c);
          if (ldt <> nil) and (keyg <> nil) and try_keybox then
          begin
            lsec := ldt^.left^.sector;
            lsec^.floor_height := nearsec^.floor_height + 72;
            lsec^.ceiling_height := lsec^.floor_height + 32;
            patch_upper(ldt, nearsec^.style^.wall0, c);
            patch_lower(ldt, nearsec^.style^.wall0, c);
            ldt^.flags := ldt^.flags or IMPASSIBLE;
            lsec^.special := GLOW_BLINK;
            if lsec^.light_level < l^.lit_light_level then
              lsec^.light_level := l^.lit_light_level;
            if painted_door <> nil then
              painted_door^ := TRUE;
            announce(LOG, 'Keybox');
          end
          else if ldt <> nil then
          begin
            ldt^.left^.sector^.light_level := box_light_level;
            ldt^.left^.sector^.special := box_special;
          end;

{$IFDEF LIGHTBAR_STUFF}
        end
        else if linelen(ld) > 16 then
        begin
          // These actually look very silly; don't use!
          make_lightbar(l, ld, pb, ThisStyle, c);
          make_lightbar(l, ld2, pb, ThisStyle, c);
{$ENDIF}
        end;
end;

// Try sticking a falling-core trap into the core bounded by the
// two given linedefs.
procedure try_falling_core(const l: level_p; ld1,ld2: linedef_p; const haa: haa_p; const c: config_p);
var
  len, depth: integer;
  room1, room2: boolean;
  ldn1, ldn2, ldfar: linedef_p;
  sdtmp: sidedef_p;
  oldsec, coresec: sector_p;
  downsec1, downsec2: sector_p;
  downspec: SmallInt;
begin
  oldsec := ld1^.right^.sector;
  downsec1 := nil;
  downsec2 := nil;
  depth := l^.hugeness * (1 + 16 * ( 4 + roll(6) ));
  len := linelen(ld1) - (16 * l^.hugeness);
  ld1 := split_linedef(l, ld1, 8 * l^.hugeness, c);
  split_linedef(l, ld1, len, c);
  room1 := empty_left_side(l, ld1, depth);
  ld2 := split_linedef(l, ld2, 8 * l^.hugeness, c);
  split_linedef(l, ld2, len, c);
  room2 := empty_left_side(l, ld2, depth);
  if not (room1 or room2) then exit; // No room!
  downspec := 0;
  case roll(6) of
    0,
    1,
    2: downspec := 0;
    3: downspec := RANDOM_BLINK;
    4: downspec := SYNC_FAST_BLINK;
    5: downspec := SYNC_SLOW_BLINK;
  end;
  coresec := clone_sector(l, ld1^.right^.sector);
  coresec^.tag := new_tag(l);
  ldn1 := new_linedef(l, ld2^._to, ld1^.from);
  ldn2 := new_linedef(l, ld1^._to, ld2^.from);
  sdtmp := new_sidedef(l,coresec,c);
  ldn1^.right := sdtmp;
  ldn2^.right := sdtmp;
  sdtmp := new_sidedef(l,oldsec,c);
  ldn1^.left := sdtmp;
  ldn2^.left := sdtmp;
  ldn1^.left^.middle_texture := c^.null_texture;
  ldn1^.right^.middle_texture := c^.null_texture;
  ldn1^.right^.lower_texture := coresec^.style^.support0;
  ldn2^.right^.lower_texture := coresec^.style^.support0;
  ldn1^.flags := ldn1^.flags or TWO_SIDED;
  ldn2^.flags := ldn2^.flags or TWO_SIDED;
  ld1^.right^.sector := coresec;
  ld2^.right^.sector := coresec;
  if room1 then
  begin
    ld1^.right^.upper_texture := ld1^.right^.middle_texture;
    ld1^.right^.y_offset := coresec^.floor_height - coresec^.ceiling_height;
    ldfar := lefthand_box_ext(l, ld1, depth,coresec^.style, c, @ldn1, @ldn2);
    downsec1 := ld1^.left^.sector;
    ld1^.right^.middle_texture := c^.null_texture;
    ld1^.left^.middle_texture := c^.null_texture;
    ld1^.flags := ld1^.flags or (TWO_SIDED or SECRET_LINEDEF);
    ldn1^.tag := coresec^.tag;
    ldn1^.typ := LINEDEF_SR_LOWER_LIFT;
    ldn2^.tag := coresec^.tag;
    ldn2^.typ := LINEDEF_SR_LOWER_LIFT;
    ldfar^.right^.middle_texture := coresec^.style^.support0;
    ldn1^.right^.middle_texture := coresec^.style^.support0;
    ldn2^.right^.middle_texture := coresec^.style^.support0;
    ld1^.left^.lower_texture := coresec^.style^.support0;
    downsec1^.floor_height := coresec^.floor_height - 128;
    downsec1^.ceiling_height := coresec^.floor_height;
    downsec1^.light_level := c^.minlight + roll(40);
    downsec1^.special := downspec;
  end;
  if room2 then
  begin
    ld2^.right^.upper_texture := ld2^.right^.middle_texture;
    ld2^.right^.y_offset := coresec^.floor_height - coresec^.ceiling_height;
    ldfar := lefthand_box_ext(l, ld2, depth,coresec^.style, c, @ldn1, @ldn2);
    downsec2 := ld2^.left^.sector;
    ld2^.right^.middle_texture := c^.null_texture;
    ld2^.left^.middle_texture := c^.null_texture;
    ld2^.flags := ld2^.flags or (TWO_SIDED or SECRET_LINEDEF);
    ldn1^.tag := coresec^.tag;
    ldn1^.typ := LINEDEF_SR_LOWER_LIFT;
    ldn2^.tag := coresec^.tag;
    ldn2^.typ := LINEDEF_SR_LOWER_LIFT;
    ldfar^.right^.middle_texture := coresec^.style^.support0;
    ldn1^.right^.middle_texture := coresec^.style^.support0;
    ldn2^.right^.middle_texture := coresec^.style^.support0;
    ld2^.left^.lower_texture := coresec^.style^.support0;
    downsec2^.floor_height := coresec^.floor_height - 128;
    downsec2^.ceiling_height := coresec^.floor_height;
    downsec2^.light_level := c^.minlight + roll(40);
    downsec2^.special := downspec;
  end;
  // Make the tripwire
  split_linedef(l, ld1, len div 2,c);
  if room1 then
  begin
    downsec1^.entry_x := ld1^._to^.x;
    downsec1^.entry_y := ld1^._to^.y;
  end;
  split_linedef(l, ld2, len div 2, c);
  if room2 then
  begin
    downsec2^.entry_x := ld2^._to^.x;
    downsec2^.entry_y := ld2^._to^.y;
  end;
  ldn1 := new_linedef(l, ld1^._to, ld2^._to);
  sdtmp := new_sidedef(l, coresec, c);
  ldn1^.left := sdtmp;
  ldn1^.right := sdtmp;
  ldn1^.left^.middle_texture := c^.null_texture;
  ldn1^.flags := ldn1^.flags or TWO_SIDED;
  if c^.gamemask and DOOM0_BIT = 0 then
    ldn1^.typ := LINEDEF_WR_TURBO_LIFT
   else
    ldn1^.typ := LINEDEF_WR_LOWER_LIFT;
  ldn1^.tag := coresec^.tag;

  // Monsters and stuff (works?)
  if room1 then
  begin
    place_monsters(l, downsec1, c, haa);
    place_health(l, downsec1, c, haa);
    place_ammo(l, downsec1, c, haa);
  end;
  if room2 then
  begin
    place_monsters(l, downsec2, c, haa);
    place_health(l, downsec2, c, haa);
    place_ammo(l, downsec2, c, haa);
  end;

  // and that's all
  announce(VERBOSE, 'Falling core');
end;

// Implement the given link between the given linedefs.
// For OPEN and BASIC links, these are antiparallel.
procedure establish_link(const l: level_p; const ldf1, ldf2: linedef_p; const ThisLink: link_p;
                     const ThisQuest: quest_p; const ThisStyle, NewStyle: style_p;
                     const haa: haa_p; const c: config_p);
var
  tag1, tag2: SmallInt;
begin
  case ThisLink^.typ of
    BASIC_LINK:
      establish_basic_link(l, ldf1, ldf2, ThisLink, ThisQuest,
                           ThisStyle, NewStyle, haa, c);
    OPEN_LINK:
      establish_open_link(l, ldf1, ldf2, ThisLink, ThisQuest,
                          ThisStyle, NewStyle, haa, c);
    GATE_LINK:
      begin
        tag1 := new_tag(l);
        tag2 := new_tag(l);
        ldf1^.right^.sector^.gate := new_gate(l, tag1, tag2, 0, FALSE, c);
        ldf2^.right^.sector^.gate := new_gate(l, tag2, tag1, 0, TRUE, c);
        if ThisQuest <> nil then
        begin
          if rollpercent(50) then
            ThisQuest^.typ := LINEDEF_S1_OPEN_DOOR
          else
            ThisQuest^.typ := LINEDEF_S1_LOWER_FLOOR;
          ThisQuest^.tag := tag1;
          ldf1^.right^.sector^.gate^.gate_lock := ThisQuest^.typ;
        end;
      end;
    else
      announce(ERROR, 'Unknown linktype, sectors not linked.');
  end;
end;

// Implement the given link between the given (antiparallel) linedefs.
// Decide which way is up-going, call the inner routine.
procedure establish_open_link(const l: level_p; const ldf1, ldf2: linedef_p; const ThisLink: link_p;
                         const ThisQuest: quest_p; const ThisStyle, NewStyle: style_p;
                         const haa: haa_p; const c: config_p);
var
  newfloor: integer;
  nearsec, farsec: sector_p;
  need_lock: boolean;
begin
  if ThisQuest <> nil then
    need_lock := (ThisQuest^.goal = SWITCH_GOAL) or (ThisQuest^.goal = GATE_GOAL)
  else
    need_lock := false;

  nearsec := ldf1^.right^.sector;
  farsec := ldf2^.right^.sector;
  // Are these right?
  farsec^.entry_x := (ldf2^.from^.x + ldf2^._to^.x) div 2;
  farsec^.entry_y := (ldf2^.from^.y + ldf2^._to^.y) div 2;

  if need_lock or rollpercent(65) then // Upward-going
  begin
    // Get recommendation
    newfloor := nearsec^.ceiling_height - ThisLink^.height1;
    // Rationalize it
    if newfloor - nearsec^.floor_height < 25 then
      newfloor := nearsec^.floor_height + 25;
    if newfloor - nearsec^.floor_height > 128 then
      newfloor := nearsec^.floor_height + 128;
    // Limit step steepness
    if ThisLink^.bits and LINK_STEPS <> 0 then
      if newfloor - nearsec^.floor_height > ThisLink^.depth1 then
        newfloor := nearsec^.floor_height + ThisLink^.depth1;
    // OK, now set far sector, and do it
    farsec^.floor_height := newfloor;
    farsec^.ceiling_height := farsec^.floor_height + NewStyle^.wallheight0;
    e_ol_inner(l, ldf1, ldf2, ThisLink, ThisQuest, ThisStyle, NewStyle, haa, c);
  end
  else
  begin
    newfloor := nearsec^.floor_height + ThisLink^.height1 - NewStyle^.wallheight0;
    if nearsec^.floor_height - newfloor < 25 then
      newfloor := nearsec^.floor_height - 25;
    if nearsec^.floor_height - newfloor > 128 then
      newfloor := nearsec^.floor_height - 128;
    // Limit step steepness
    if ThisLink^.bits and LINK_STEPS <> 0 then
      if nearsec^.floor_height - newfloor > ThisLink^.depth1 then
        newfloor := nearsec^.floor_height - ThisLink^.depth1;
    farsec^.floor_height := newfloor;
    farsec^.ceiling_height := farsec^.floor_height + NewStyle^.wallheight0;
    e_ol_inner(l, ldf2, ldf1, ThisLink, ThisQuest, NewStyle, ThisStyle, haa, c);
  end;
end;

// Implement the given link between the given (antiparallel) linedefs,
// always upward-going.
procedure e_ol_inner(const l: level_p; ldf1, ldf2: linedef_p; const ThisLink: link_p;
                     const ThisQuest: quest_p; const ThisStyle, NewStyle: style_p;
                     const haa: haa_p; const c: config_p);
var
  ldf1a, ldf1b, ldf2a, ldf2b: linedef_p;
  lde1, lde2, ldes: linedef_p;
  sideseca, sidesecb, midsec: sector_p;
  nearsec, farsec: sector_p;
  midwidth, len, len2, sidefloor, dieroll: integer;
  nukage, high_sides, sidesteps: boolean;
  g: genus_p;
begin
  nukage := FALSE;
  high_sides := FALSE;
  sidesteps := FALSE;

  if ThisLink^.bits and LINK_LIFT <> 0 then
    announce(VERBOSE, 'Open lift');

  if (ThisLink^.bits and LINK_LIFT = 0)and
     (ThisLink^.bits and LINK_STEPS = 0) then
    announce(WARNING, 'Non-lift non-stair open link; oops!');

  if rollpercent(l^.p_force_nukage) or rollpercent(10) then
    nukage := TRUE;
  if (ThisLink^.bits and LINK_STEPS <> 0) and (ThisLink^.bits and LINK_ALCOVE <> 0) then
    nukage := FALSE;

  nearsec := ldf1^.right^.sector;
  farsec := ldf2^.right^.sector;

  // If a teleporter-goal, just make a simple air-connection.
  // Would(n't) this be more simply expressed as a BASIC link?
  if ThisQuest <> nil then
    if ThisQuest^.goal = GATE_GOAL then
    begin
      midsec := clone_sector(l, farsec);
      midsec^.floor_height := nearsec^.floor_height;
      midsec^.floor_flat := nearsec^.floor_flat;
      ldf1^.right^.middle_texture := c^.null_texture;
      ldf1^.left := new_sidedef(l, midsec, c);
      ldf1^.left^.middle_texture := c^.null_texture;
      ldf1^.left^.upper_texture := farsec^.style^.wall0;
      ldf1^.flags := ldf1^.flags or (TWO_SIDED or UPPER_UNPEGGED);
      patch_upper(ldf1, ThisStyle^.wall0, c);
      patch_lower(ldf1, ThisStyle^.support0, c);
      ldf2^.right^.middle_texture := c^.null_texture;
      ldf2^.left := new_sidedef(l, midsec, c);
      ldf2^.left^.middle_texture := c^.null_texture;
      ldf2^.left^.lower_texture := farsec^.style^.wall0;
      ldf2^.flags := ldf2^.flags or TWO_SIDED;
      patch_upper(ldf2, NewStyle^.wall0, c);
      patch_lower(ldf2, NewStyle^.support0, c);
      lde1 := new_linedef(l, ldf1^.from,ldf2^._to);
      lde1^.right := new_sidedef(l, midsec, c);
      lde1^.right^.middle_texture := farsec^.style^.wall0;
      lde2 := new_linedef(l, ldf2^.from, ldf1^._to);
      lde2^.right := new_sidedef(l, midsec, c);
      lde2^.right^.middle_texture := farsec^.style^.wall0;
      if nukage then
      begin
        ldf1^.left^.lower_texture := ThisStyle^.support0;
        midsec^.floor_flat := ThisStyle^.nukage1;
        midsec^.special := NUKAGE1_SPECIAL;
        if midsec^.light_level < 160 then
          midsec^.light_level := 160; // visible
        midsec^.floor_height := midsec^.floor_height - 8;
      end;
      // Now arrange for the gate and stuff
      ThisQuest^.tag := new_tag(l);
      if rollpercent(50) then
        ThisQuest^.tag2 := 0 // Can be one-way
      else
      begin
        ThisQuest^.tag2 := new_tag(l);
        ldf2^.right^.middle_texture := ThisStyle^.grating;
        ldf2^.left^.middle_texture := ThisStyle^.grating;
        ldf2^.flags := ldf2^.flags or (IMPASSIBLE or LOWER_UNPEGGED); // Lower the grating, eh?
      end;
      farsec^.gate := new_gate(l, ThisQuest^.tag, ThisQuest^.tag2, 0, TRUE, c);
      announce(LOG, 'OL Gate quest');
      exit; // and that's it
    end;

  // Otherwise it's (even) more complicated

  // Figure how wide
  len := linelen(ldf1);
  if len < 100 then
    announce(WARNING, 'Open link on a too-narrow linedef!');
  midwidth := ThisLink^.width1;
  if midwidth = 0 then
    midwidth := linelen(ldf1) div 3;
  if midwidth < 64 then
    midwidth := 64;
  if (len - midwidth) div 2 < 33 then
    midwidth := len - 66;
  if midwidth < 33 then
    midwidth := 33;

  // Decide if doing the sideways-step thing
  if (ThisLink^.bits and LINK_STEPS <> 0) and
     (ThisLink^.bits and LINK_ALCOVE <> 0) and
     (midwidth >= farsec^.floor_height - nearsec^.floor_height) then
    sidesteps := TRUE;

  // Decide about nukage and side heights and stuff
  dieroll := roll(100);
  if sidesteps then
    sidefloor := nearsec^.floor_height
  else if (dieroll < 50) or nukage then
    sidefloor := nearsec^.floor_height
  else if dieroll < 75 then
  begin
    nukage := FALSE;
    high_sides := TRUE;
    sidefloor := farsec^.floor_height;
    if sidefloor > nearsec^.ceiling_height - 57 then
      sidefloor:= nearsec^.ceiling_height - 57;
  end
  else
  begin
    nukage := FALSE;
    sidefloor := farsec^.floor_height;
    if sidefloor > nearsec^.ceiling_height - 57 then
      sidefloor := nearsec^.ceiling_height - 57;
    if farsec^.floor_height - nearsec^.floor_height > 48 then
      sidefloor := nearsec^.floor_height + roll(1 + sidefloor - nearsec^.floor_height);
  end;

  ldf1^.flags := ldf1^.flags or TWO_SIDED;
  ldf1^.right^.middle_texture := c^.null_texture;
  ldf2^.flags := ldf2^.flags or TWO_SIDED;
  ldf2^.right^.middle_texture := c^.null_texture;

  ldf1a := ldf1;
  ldf1 := split_linedef(l, ldf1,(len - midwidth) div 2, c);
  ldf1b := split_linedef(l, ldf1, midwidth, c);
  ldf2b := ldf2;
  ldf2 := split_linedef(l, ldf2,(len - midwidth) div 2, c);
  ldf2a := split_linedef(l, ldf2, midwidth, c);

  midsec := clone_sector(l, farsec);
  if ThisLink^.bits and LINK_LIFT <> 0 then
  begin
    midsec^.tag := new_tag(l);
    ldf1^.typ := NewStyle^.slifttype;
    ldf1^.tag := midsec^.tag;
  end;

  ldf1^.left := new_sidedef(l, midsec, c);
  ldf1^.left^.middle_texture := c^.null_texture;
  if nukage and (ThisLink^.bits and LINK_LIFT <> 0) then
    ldf1^.left^.lower_texture := ThisStyle^.support0;
  ldf2^.left := new_sidedef(l, midsec, c);
  ldf2^.left^.middle_texture := c^.null_texture;
  if ThisLink^.bits and LINK_LIFT <> 0 then
  begin
    ldf2^.left^.lower_texture := NewStyle^.support0;
    ldf2^.flags := ldf2^.flags or LOWER_UNPEGGED;
  end;
  patch_upper(ldf1, ThisStyle^.wall0, c);
  patch_lower(ldf1, ThisStyle^.support0, c);
  if (ThisLink^.bits and LINK_LIFT <> 0) and
      (ThisStyle^.liftface <> nil) and
      (farsec^.floor_height - nearsec^.floor_height <= ThisStyle^.liftface^.height) and
      (midwidth = ThisStyle^.liftface^.width) then
  begin
    ldf1^.right^.lower_texture := ThisStyle^.liftface;
    ldf1^.right^.x_offset := 0;
    announce(VERBOSE, 'Lift texture');
  end;
  ldf1^.flags := ldf1^.flags and not LOWER_UNPEGGED;
  patch_upper(ldf2, NewStyle^.wall0, c);
  patch_lower(ldf2, NewStyle^.wall0, c);

  flip_linedef(ldf2a);
  sideseca := make_box_ext(l, ldf1a, ldf2a, ThisStyle, c, @lde1, @lde2);
  flip_linedef(ldf2a);
  sideseca^.floor_height := sidefloor;
  sideseca^.ceiling_height := midsec^.ceiling_height;
  sideseca^.ceiling_flat := midsec^.ceiling_flat;
  lde1^.right^.middle_texture := NewStyle^.wall0;
  lde2^.left := new_sidedef(l,midsec,c);
  lde2^.flags := lde2^.flags or TWO_SIDED;
  lde2^.left^.middle_texture := c^.null_texture;
  lde2^.left^.lower_texture := NewStyle^.support0;
  lde2^.right^.lower_texture := NewStyle^.wall0;
  lde2^.right^.middle_texture := c^.null_texture;
  lde2^.right^.y_offset := farsec^.ceiling_height - farsec^.floor_height;
  ldes := lde2;  // Save for stairification
  patch_upper(ldf1a, ThisStyle^.wall0, c);
  patch_lower(ldf1a, ThisStyle^.wall0, c);
  patch_upper(ldf2a, NewStyle^.wall0, c);
  patch_lower(ldf2a, NewStyle^.wall0, c);
  if nukage then
  begin
    announce(VERBOSE, 'Open nukage');
    sideseca^.floor_height := sideseca^.floor_height - 8;
    sideseca^.floor_flat := ThisStyle^.nukage1;
    sideseca^.special := NUKAGE1_SPECIAL;
    patch_lower(ldf1a, ThisStyle^.support0, c);
    patch_lower(ldf2a, ThisStyle^.support0, c);
    nearsec^.marked := TRUE;
    farsec^.marked := TRUE;
    if c^.gunk_channels and empty_left_side(l, lde1, 32) then
    begin
      lefthand_box(l, lde1, 32, ThisStyle, c)^.right^.middle_texture := ThisStyle^.support0;
      lde1^.left^.sector^.ceiling_height := lde1^.left^.sector^.floor_height + 8;
      lde1^.left^.sector^.light_level := lde1^.right^.sector^.light_level - 20;
      lde1^.left^.sector^.floor_flat := ThisStyle^.nukage1;
      patch_upper(lde1,NewStyle^.wall0,c);
      announce(VERBOSE, 'Channel');
    end;
    nearsec^.marked := FALSE;
    farsec^.marked := FALSE;
  end;

  flip_linedef(ldf2b);
  sidesecb := make_box_ext(l, ldf1b, ldf2b, ThisStyle, c, @lde1, @lde2);
  flip_linedef(ldf2b);
  sidesecb^.floor_height := sidefloor;
  sidesecb^.ceiling_height := midsec^.ceiling_height;
  sidesecb^.ceiling_flat := midsec^.ceiling_flat;
  lde2^.right^.middle_texture := NewStyle^.wall0;
  lde1^.left := new_sidedef(l, midsec, c);
  lde1^.flags := lde1^.flags or TWO_SIDED;
  lde1^.left^.middle_texture := c^.null_texture;
  lde1^.left^.lower_texture := NewStyle^.support0;
  lde1^.right^.lower_texture := NewStyle^.wall0;
  lde1^.right^.middle_texture := c^.null_texture;
  lde1^.right^.y_offset := farsec^.ceiling_height - farsec^.floor_height;
  patch_upper(ldf1b, ThisStyle^.wall0, c);
  patch_lower(ldf1b, ThisStyle^.wall0, c);
  patch_upper(ldf2b, NewStyle^.wall0, c);
  patch_lower(ldf2b, NewStyle^.wall0, c);
  if nukage then
  begin
    sidesecb^.floor_height := sidesecb^.floor_height - 8;
    sidesecb^.floor_flat := ThisStyle^.nukage1;
    sidesecb^.special := NUKAGE1_SPECIAL;
    patch_lower(ldf1b, ThisStyle^.support0, c);
    patch_lower(ldf2b, ThisStyle^.support0, c);
    nearsec^.marked := TRUE;
    farsec^.marked := TRUE;
    if c^.gunk_channels and empty_left_side(l, lde2, 32) then
    begin
      lefthand_box(l, lde2, 32, ThisStyle, c)^.right^.middle_texture := ThisStyle^.support0;
      lde2^.left^.sector^.ceiling_height := lde2^.left^.sector^.floor_height + 8;
      lde2^.left^.sector^.light_level := lde2^.right^.sector^.light_level - 20;
      lde2^.left^.sector^.floor_flat := ThisStyle^.nukage1;
      patch_upper(lde2, NewStyle^. wall0, c);
      announce(VERBOSE, 'Channel');
    end;
    nearsec^.marked := FALSE;
    farsec^.marked := FALSE;
  end;

  // Could be more interesting...
  midsec^.light_level := ThisStyle^.roomlight0;
  sideseca^.light_level := ThisStyle^.roomlight0;
  sidesecb^.light_level := ThisStyle^.roomlight0;

  // Make center into stairs if we need them
  if (ThisLink^.bits and LINK_STEPS <> 0) and not sidesteps then
  begin
    announce(VERBOSE, 'Open stairs');
    // Maybe stick on some lights
    if rollpercent(50) then // 50 should vary?
    begin
      g := ThisStyle^.lamp0;
      if g^.height > sideseca^.ceiling_height - sideseca^.floor_height then
        g := ThisStyle^.shortlamp0;
      if (high_sides and ((len - midwidth) div 2 >= 2 * g^.width)) or
         ((len - midwidth) div 2 >= g^.width + 69) then
      begin
        announce(VERBOSE, 'and lights');
        new_thing(l,
          (ldf1a^._to^.x + ldf1a^.from^.x + ldf2a^._to^.x + ldf2a^.from^.x) div 4,
          (ldf1a^._to^.y + ldf1a^.from^.y + ldf2a^._to^.y + ldf2a^.from^.y) div 4,
          0, g^.thingid, 7, c);
        new_thing(l,
          (ldf1b^._to^.x + ldf1b^.from^.x + ldf2b^._to^.x + ldf2b^.from^.x) div 4,
          (ldf1b^._to^.y + ldf1b^.from^.y + ldf2b^._to^.y + ldf2b^.from^.y) div 4,
          0, g^.thingid, 7, c);
        if rollpercent(70) then // Should be in link/style?
        begin
          if sideseca^.light_level <= l^.bright_light_level then
            sideseca^.light_level := sideseca^.light_level + 20; // Lamps light things up
          if sidesecb^.light_level <= l^.bright_light_level then
            sidesecb^.light_level := sidesecb^.light_level + 20;
          if midsec^.light_level > c^.minlight then
            midsec^.light_level := midsec^.light_level - 20; // and throw shadows!
        end;
      end;
    end;
    // Change a few details
    lde1^.right^.y_offset := 0;
    lde1^.left^.lower_texture := NewStyle^.wall0;
    lde1^.flags := lde1^.flags or LOWER_UNPEGGED;
    ldes^.right^.y_offset := 0;
    ldes^.left^.lower_texture := NewStyle^.wall0;
    ldes^.flags := ldes^.flags or LOWER_UNPEGGED;
    if ThisStyle^.light_steps and (ThisStyle^.walllight <> nil) then
      ldf1^.right^.lower_texture := ThisStyle^.walllight
    else
      ldf1^.right^.lower_texture := ThisStyle^.kickplate;
    ldf2^.left^.lower_texture := NewStyle^.wall0; // In case of lock
    // Make the center into stairs
    stairify(l, ldf1, ldf2, ldes, lde1, nearsec^.floor_height, farsec^.floor_height,
      ThisQuest, ThisStyle, c);
  end;

  // Or make the center into *sideways* stairs
  if (ThisLink^.bits and LINK_STEPS <> 0) and sidesteps then
  begin
    announce(NONE, 'Open side-stairs');
    ldf1^.right^.lower_texture := ThisStyle^.wall0;
    ldf2^.left^.lower_texture := NewStyle^.wall0;
    ldf1^.right^.y_offset := 0;
    ldf1^.left^.lower_texture := NewStyle^.wall0;
    ldf1^.flags := ldf1^.flags or LOWER_UNPEGGED;
    if ThisLink^.bits and LINK_LEFT <> 0 then
    begin
      if ThisStyle^.light_steps and (ThisStyle^.walllight <> nil) then
        ldes^.right^.lower_texture := ThisStyle^.walllight
      else
        ldes^.right^.lower_texture := ThisStyle^.kickplate;
      ldes^.right^.y_offset := 0;
      lde1^.left^.lower_texture := NewStyle^.wall0; // In case of lock?
      sidesecb^.floor_height := farsec^.floor_height;
      sidesecb^.floor_flat := farsec^.floor_flat;
    end
    else
    begin
      if ThisStyle^.light_steps and (ThisStyle^.walllight <> nil) then
        lde1^.right^.lower_texture := ThisStyle^.walllight
      else
        lde1^.right^.lower_texture := ThisStyle^.kickplate;
      lde1^.right^.y_offset := 0;
      ldes^.left^.lower_texture := NewStyle^.wall0; // In case of lock?
      sideseca^.floor_height := farsec^.floor_height;
      sideseca^.floor_flat := farsec^.floor_flat;
    end;
    ldf2^.right^.y_offset := 0;
    ldf2^.left^.lower_texture := NewStyle^.wall0;
    ldf2^.flags := ldf2^.flags or LOWER_UNPEGGED;
    patch_lower(ldf1a, ThisStyle^.wall0, c);
    patch_lower(ldf1b, ThisStyle^.wall0, c);
    if ThisLink^.bits and LINK_LEFT <> 0 then
      stairify(l, ldes, lde1, ldf2, ldf1, nearsec^.floor_height, farsec^.floor_height,
        ThisQuest, ThisStyle, c)
    else
      stairify(l, lde1, ldes, ldf1, ldf2, nearsec^.floor_height, farsec^.floor_height,
        ThisQuest, ThisStyle, c);
  end;

  // Bells and whistles!
  len2 := linelen(ldf2a);
  if (farsec^.floor_height - sideseca^.floor_height = 128) and (len2 >= 128) then
  begin
    if len2 > 128 then
    begin
      ldf2a := line_center_part(l, ldf2a, nil, 128, ThisStyle, c);
      ldf2b := line_center_part(l, ldf2b, nil, 128, ThisStyle, c);
    end;
    ldf2a^.left^.lower_texture := ThisStyle^.plaque;
    ldf2a^.left^.x_offset := 0;
    ldf2a^.left^.y_offset := 0;
    ldf2a^.flags := ldf2a^.flags and not LOWER_UNPEGGED;
    ldf2b^.left^.lower_texture := ThisStyle^.plaque;
    ldf2b^.left^.x_offset := 0;
    ldf2b^.left^.y_offset := 0;
    ldf2b^.flags := ldf2b^.flags and not LOWER_UNPEGGED;
    announce(VERBOSE, 'Open-link plaques');
  end;
end;

// Implement the given link between the given (antiparallel) linedefs.
// Set bits for any ephemera, then call inner recursive routine.
procedure establish_basic_link(const l: level_p; const ldf1, ldf2: linedef_p; const ThisLink: link_p;
                          const ThisQuest: quest_p; const ThisStyle, NewStyle: style_p;
                          const haa: haa_p; const c: config_p);
begin
  ThisStyle^.lightboxes := rollpercent(5); // Should be from style, or?
  e_bl_inner(l, ldf1, ldf2, ThisLink, ThisQuest, ThisStyle, NewStyle, 0, haa, c);
  ThisStyle^.lightboxes := FALSE; // Just to be neat
end;

// Implement the given link between the given (antiparallel) linedefs
// Potentially recursive, for windows and twinnings.
// Needs lots of cleaning up and organizing and splitting into
// smaller functions!
procedure e_bl_inner(const l: level_p; ldf1, ldf2: linedef_p; ThisLink: link_p;
                     const ThisQuest: quest_p; const ThisStyle, NewStyle: style_p;
                     const flipstate: SmallInt; const haa: haa_p; const c: config_p);
var
  ldnew1, ldnew2: linedef_p;
  len, border, maxtop: integer;
  t1, t2: texture_p;
  nearsec, farsec, newsec: sector_p;
  need_to_doorify: boolean;
  ldflip1a: linedef_p;
  ldflip1b: linedef_p;
  ldflip2a: linedef_p;
  ldflip2b: linedef_p;
  sflip1: sector_p;
  sflip2: sector_p;
  ldedge1, ldedge2: linedef_p;
  tag1: integer;
  trigger_lift: boolean;
  trigger_door: boolean;
  painted_door: boolean;
  mminx, mminy, mmaxx, mmaxy, mangle, need: integer;
  effective_left: LongWord;
  litecol: LongWord;
  offs: SmallInt;
  ldedgeopen, ldedgeclosed: linedef_p;
  i, depth, stepdelta, x, y, levels: integer;
  front: texture_p;
  m, g: genus_p;
  add_lamps: boolean;
begin
  nearsec := nil;
  farsec := nil;
  newsec := nil;
  need_to_doorify := FALSE;
  ldflip1a := nil;
  ldflip1b := nil;
  ldflip2a := nil;
  ldflip2b := nil;
  sflip1 := nil;
  sflip2 := nil;
  tag1 := 0;
  trigger_lift := FALSE;
  trigger_door := FALSE;
  painted_door := FALSE;
  effective_left := ThisLink^.bits and LINK_LEFT;
  litecol := LIGHT;


  if (ThisLink^.bits and LINK_CORE <> 0) and (ThisLink^.bits and LINK_ANY_DOOR <> 0) then
    announce(VERBOSE, 'Core and door(s)');

  litecol := LIGHT;
  if ThisQuest <> nil then
  begin
    if ThisQuest^.goal = KEY_GOAL then
    begin
      case ThisQuest^.typ of
        ID_BLUEKEY,
        ID_BLUECARD:
          litecol := BLUE;
        ID_REDKEY,
        ID_REDCARD:
          litecol := RED;
        ID_YELLOWKEY,
        ID_YELLOWCARD:
          litecol := YELLOW;
      end;
    end;
    // The type of a SWITCH_GOAL isn't set until the link's established
    if ThisQuest^.goal = SWITCH_GOAL then
    begin
      if ThisLink^.bits and LINK_LOCK_CORE <> 0 then
        ThisQuest^.typ := LINEDEF_S1_RAISE_AND_CLEAN_FLOOR
      else
        ThisQuest^.typ := LINEDEF_S1_OPEN_DOOR;
    end;
  end;

  dump_link(ldf1, ldf2, ThisLink, 'Establishing');

  if (ThisLink^.bits and LINK_ALCOVE <> 0) and
     (ThisLink^.bits and LINK_TWIN <> 0) and
     (ThisLink^.bits and LINK_ANY_DOOR <> 0) then
    announce(VERBOSE, 'Twin door alcoves!');

  nearsec := ldf1^.right^.sector;
  farsec := ldf2^.right^.sector;

  // Figure floor and ceiling heights for new sector
  farsec^.floor_height := nearsec^.floor_height + ThisLink^.floordelta;
  farsec^.ceiling_height := farsec^.floor_height + NewStyle^.wallheight0;

  // Make sure we don't overdo the bar thing and crash the engine...
  if l^.barcount > LEVEL_MAX_BARS then
    ThisLink^.bits := ThisLink^.bits and not LINK_BARS;

  // See if we need to force the floordelta toward zero to avoid
  // impassable doorways.
  if ThisLink^.bits and LINK_STEPS <> 0 then
  begin
    // The clearance we need is 56 plus the step height times
    // the number of steps our 64ish-wide shadow is on at once
    need := 64 + (1 + (64 div (ThisLink^.depth3 div (ThisLink^.stepcount)))) * abs(ThisLink^.floordelta div (ThisLink^.stepcount - 1));
    if ThisLink^.bits and LINK_ANY_DOOR <> 0 then
      need := need + 8;  // Doors don't open all the way
    if (farsec^.ceiling_height - farsec^.floor_height < need) or
       (nearsec^.ceiling_height - nearsec^.floor_height < need) then
    begin
      // There's probably something less drastic we can do here...
      ThisLink^.floordelta := 0;
      ThisLink^.bits := ThisLink^.bits and not LINK_STEPS;
      farsec^.floor_height := nearsec^.floor_height + ThisLink^.floordelta;
      farsec^.ceiling_height := farsec^.floor_height + NewStyle^.wallheight0;
    end;
  end;

  // If we need to twin, split and recurse, or do the window thing
  if (flipstate = 0) and (ThisLink^.bits and LINK_TWIN <> 0) then
  begin
    len := linelen(ldf1) div 2;
    ldnew1 := split_linedef(l, ldf1, len, c);
    ldnew2 := split_linedef(l, ldf2, len, c);
    if ThisLink^.bits and LINK_WINDOW = 0 then // make twin links
    begin
      e_bl_inner(l, ldf1, ldnew2, ThisLink, ThisQuest, ThisStyle, NewStyle,
                 1, haa, c); // Lefthand one
      e_bl_inner(l, ldnew1, ldf2, ThisLink, ThisQuest, ThisStyle, NewStyle,
                 2, haa, c); // Righthand one
    end
    else // Make a window
    begin
      if rollpercent(50) then // left or right
      begin
        e_bl_inner(l, ldf1, ldnew2, ThisLink, ThisQuest, ThisStyle, NewStyle, 3, haa, c);
        make_window(l, ldnew1, ldf2, ThisLink, ThisStyle, NewStyle, c);
      end
      else
      begin
        // Note: always establish the link before making the window!
        e_bl_inner(l, ldnew1, ldf2, ThisLink, ThisQuest, ThisStyle, NewStyle, 3, haa, c);
        make_window(l, ldf1, ldnew2, ThisLink, ThisStyle, NewStyle, c);
      end;
    end; // end else a window
    exit;
  end;

  // If this isn't supposed to be passable at all, just winowify.
  // Another fun thing would be to make the door, but bar it.
  if ThisQuest <> nil then
    if ThisQuest^.goal = GATE_GOAL then
    begin
      make_window(l, ldf1, ldf2, ThisLink, ThisStyle, NewStyle, c);
      // Now arrange for the gate and stuff
      ThisQuest^.tag := new_tag(l);
      ThisQuest^.tag2 := new_tag(l);
      farsec^.gate := new_gate(l, ThisQuest^.tag, ThisQuest^.tag2, 0, TRUE, c);
      announce(LOG, 'BL Gate quest');
      exit; // and that's it
    end;

  // Figure out maxtop, for MAX_CEILING
  maxtop := nearsec^.floor_height + ThisLink^.height1;
  if ThisLink^.floordelta > 0 then
    maxtop := maxtop + ThisLink^.floordelta;

  // If not the whole wall, center it, or do alcove things, or etc
  len := linelen(ldf1); // Really assumes lens are ==
  if ThisLink^.width1 > len then
  begin
    announce(WARNING, 'Link-width > linedef size!  Reducing...');
    ThisLink^.width1 := len;
  end;
  if (ThisLink^.width1 <> 0) and (ThisLink^.width1 < len) then
  begin
    if ThisLink^.bits and LINK_ALCOVE <> 0 then
    begin
      border := (len - (ThisLink^.width1 * 2 + ThisLink^.depth3)) div 2;
      if border < 0 then // This should never happen
      begin
        announce(WARNING, 'A-link width too big!  Reducing...');
        ThisLink^.width1 := (len - ThisLink^.depth3) div 2;
        border := 0;
        // May not actually help enough...
        // But can't dealcove; too complicated!
      end;
      if (border <> 0) and (flipstate = 2) and rollpercent(50) then
      begin
        effective_left := effective_left xor LINK_LEFT;
        announce(VERBOSE, 'Flipping twinned alcove');
      end;
      if effective_left <> 0 then
      begin
        if border <> 0 then ldf1 := split_linedef(l, ldf1, border, c);
        split_linedef(l, ldf1, ThisLink^.width1, c);
        if border <> 0 then ldf2 := split_linedef(l, ldf2, border, c);
        split_linedef(l, ldf2, ThisLink^.width1, c);
      end
      else
      begin
        ldf1 := split_linedef(l, ldf1, (len - (border + ThisLink^.width1)), c);
        if border <> 0 then split_linedef(l, ldf1, ThisLink^.width1, c);
        ldf2 := split_linedef(l, ldf2, (len - (border + ThisLink^.width1)), c);
        if border <> 0 then split_linedef(l, ldf2, ThisLink^.width1, c);
      end;
    end
    else if (flipstate = 1) and (ThisLink^.bits and LINK_FAR_TWINS <> 0) then
    begin
      split_linedef(l, ldf1, ThisLink^.width1, c);
      ldf2 := split_linedef(l, ldf2, len - ThisLink^.width1, c);
      announce(NONE, 'Far twins');
    end
    else if (flipstate = 2) and (ThisLink^.bits and LINK_FAR_TWINS <> 0) then
    begin
      split_linedef(l, ldf2,ThisLink^.width1, c);
      ldf1 := split_linedef(l, ldf1, len - ThisLink^.width1, c);
    end
    else
    begin // No alcove or farness; simple centered borders
      if (ThisQuest <> nil) and (ThisQuest^.goal = KEY_GOAL) and l^.skullkeys then
        ldf1 := borderize(l, ldf1, ThisLink^.width1, TRUE, ThisStyle,
                          litecol, find_genus(c, ThisQuest^.typ), @painted_door, c)
      else
        ldf1 := borderize(l, ldf1,ThisLink^.width1, TRUE, ThisStyle,
                          litecol, nil, nil, c);
      // Embellish only near side of the link linedef?
      ldf2 := borderize(l, ldf2, ThisLink^.width1, FALSE, NewStyle,
                        litecol, nil, nil, c);
    end; // end else no-alcove case
  end
  else
  begin
    if ThisLink^.bits and LINK_ALCOVE <> 0 then
      announce(WARNING, 'ALCOVE with width zero, or width == linelen');
    // yikes!
  end;

  // Now we know where the player will be entering the far room,
  // more or less.  Record that for the far sector.
  farsec^.entry_x := (ldf2^.from^.x + ldf2^._to^.x) div 2;
  farsec^.entry_y := (ldf2^.from^.y + ldf2^._to^.y) div 2;

  // Get any tags we need for triggered links, and put them on
  // the linedefs we're currently working on.
  // Trigger alcoved lifts.
  if (ThisLink^.bits and LINK_LIFT <> 0) and
     (ThisLink^.bits and LINK_ALCOVE <> 0) and
     (ThisLink^.bits and LINK_ANY_DOOR = 0) and
     (ThisLink^.bits and LINK_TRIGGERED <> 0) then
  begin
    trigger_lift := TRUE;
    announce(VERBOSE, 'Walking lift');
    tag1 := new_tag(l);
    if(ThisLink^.floordelta > 0) then
    begin
      ldf1^.tag := tag1;
      ldf1^.typ := LINEDEF_WR_LOWER_LIFT;
    end
    else
    begin
      ldf2^.tag := tag1;
      ldf2^.typ := LINEDEF_WR_LOWER_LIFT;
    end;
  end;

  // Trigger deeply-recessed liftless doors also */
  if ((ThisQuest = nil) or ((ThisQuest^.goal <> SWITCH_GOAL) and (ThisQuest^.goal <> KEY_GOAL))) and
     (ThisLink^.bits and LINK_ANY_DOOR <> 0) and
     (ThisLink^.bits and LINK_BARS = 0) and
     (ThisLink^.bits and LINK_RECESS <> 0) and
     (ThisLink^.depth2 > 16) and
     (ThisLink^.bits and LINK_TRIGGERED <> 0) then
  begin
    trigger_door := TRUE;
    tag1 := new_tag(l);
    // Don't always need both of these, but couldn't hurt...
    ldf1^.tag := tag1;
    ldf1^.typ := LINEDEF_WR_OC_DOOR; // Or WR_OPEN_DOOR?
    ldf2^.tag := tag1;
    ldf2^.typ := LINEDEF_WR_OC_DOOR; // Or WR_OPEN_DOOR?
  end;

  // and deeply-recessed lifts
  if ((ThisQuest = nil)or ((ThisQuest^.goal <> SWITCH_GOAL) and (ThisQuest^.goal <> KEY_GOAL))) and
     (ThisLink^.bits and LINK_LIFT <> 0) and
     (ThisLink^.bits and LINK_RECESS <> 0) and
     (ThisLink^.bits and LINK_ALCOVE = 0) and
     (ThisLink^.bits and LINK_ANY_DOOR = 0) and
     (ThisLink^.depth2 > 16) and
     (ThisLink^.bits and LINK_TRIGGERED <> 0) then
  begin
    trigger_lift := TRUE;
    tag1 := new_tag(l);
    if ThisLink^.floordelta > 0 then
    begin
      ldf1^.tag := tag1;
      ldf1^.typ := LINEDEF_WR_LOWER_LIFT;
    end
    else
    begin
      ldf2^.tag := tag1;
      ldf2^.typ := LINEDEF_WR_LOWER_LIFT;
    end;
  end;

  // Remember these for later
  t1 := ldf1^.right^.middle_texture;
  t2 := NewStyle^.wall0;
  len := linelen(ldf1); // Really assumes lens are ==

  // If recessed, make recess sectors and stuff
  if ThisLink^.bits and LINK_RECESS <> 0 then
  begin
    ldnew1 := lefthand_box_ext(l, ldf1, ThisLink^.depth2, ThisStyle, c, @ldedge1, @ldedge2);
    // The near recess copies the near room
    ldnew1^.right^.sector^.floor_height := ldf1^.right^.sector^.floor_height;
    if ThisLink^.bits and LINK_MAX_CEILING <> 0 then
      ldnew1^.right^.sector^.ceiling_height := maxtop
    else
      ldnew1^.right^.sector^.ceiling_height := ldf1^.right^.sector^.floor_height + ThisLink^.height1;
    if nearsec^.style^.ceilinglight <> nil then
      if c^.clights then // Too often?
      begin
        ldnew1^.right^.sector^.ceiling_flat := nearsec^.style^.ceilinglight;
        announce(VERBOSE, 'rcl');
      end;
    // Paint key-color, or adjust y-offsets, for recess edges */
    if ((ThisQuest <> nil) and ThisStyle^.paint_recesses and not painted_door) and
       (ThisQuest^.goal = KEY_GOAL) and
       (ThisLink^.depth2 >= texture_for_key(ThisQuest^.typ,ThisStyle,c)^.width) then
    begin
      ldedge1^.right^.middle_texture := texture_for_key(ThisQuest^.typ, ThisStyle, c);
      ldedge2^.right^.middle_texture := ldedge1^.right^.middle_texture;

      if l^.scrolling_keylights then
      begin
        ldedge1^.typ := LINEDEF_SCROLL;
        ldedge2^.typ := LINEDEF_SCROLL;
      end;

      // Make sure the paint is visible!
      if ldedge1^.right^.sector^.light_level < l^.lit_light_level then
        ldedge1^.right^.sector^.light_level := l^.lit_light_level;
      announce(VERBOSE, 'painted recess');
      if rollpercent(75) then
        painted_door := TRUE
      else
      begin
        announce(VERBOSE, 'Extra-painted recess'); // Paint the door, too
        ldedge1^.flags := ldedge1^.flags or LOWER_UNPEGGED; // and make it all line up
        ldedge2^.flags := ldedge2^.flags or LOWER_UNPEGGED;
      end;
    end
    else if (ThisLink^.bits and LINK_NEAR_DOOR <> 0) and
            ThisStyle^.light_recesses and (ThisStyle^.walllight <> nil) then
    begin
      announce(VERBOSE, 'Lit recess');
      ldedge1^.right^.middle_texture := ThisStyle^.walllight;
      ldedge2^.right^.middle_texture := ThisStyle^.walllight;
      make_lighted(l, ldedge1^.right^.sector, c);
    end
    else
    begin
      ldedge2^.right^.y_offset := nearsec^.ceiling_height - ldnew1^.right^.sector^.ceiling_height;
      ldedge1^.right^.y_offset := ldedge2^.right^.y_offset;
    end;
    patch_upper(ldf1, t1, c);
    // and the far the far
    ldnew2 := lefthand_box_ext(l, ldf2, ThisLink^.depth2, NewStyle, c, @ldedge1, @ldedge2);
    ldnew2^.right^.sector^.floor_height := farsec^.floor_height;
    if ThisLink^.bits and LINK_MAX_CEILING <> 0 then
      ldnew2^.right^.sector^.ceiling_height := maxtop
    else
      ldnew2^.right^.sector^.ceiling_height := farsec^.floor_height + ThisLink^.height1;
    ldnew2^.right^.sector^.light_level := NewStyle^.doorlight0;
    if farsec^.style^.ceilinglight <> nil then
      if c^.clights then // Too often?
      begin
        ldnew2^.right^.sector^.ceiling_flat := farsec^.style^.ceilinglight;
        announce(VERBOSE, 'rcl');
      end;
    if (ThisLink^.bits and LINK_FAR_DOOR <> 0) and
       NewStyle^.light_recesses and (NewStyle^.walllight <> nil) then
    begin
      announce(VERBOSE, 'Lit recess');
      ldedge1^.right^.middle_texture := NewStyle^.walllight;
      ldedge2^.right^.middle_texture := NewStyle^.walllight;
      make_lighted(l, ldedge1^.right^.sector, c);
    end
    else
    begin
      offs := farsec^.ceiling_height - ldnew2^.right^.sector^.ceiling_height;
      ldedge2^.right^.y_offset := offs;
      ldedge1^.right^.y_offset := offs;
    end;
    patch_upper(ldf2, t2, c);
    // Now we're working inside the recesses
    ldf1 := ldnew1;
    ldf2 := ldnew2;
  end;

  // If no core or alcoves, make the one arch/door sector
  if (ThisLink^.bits and (LINK_CORE or LINK_ALCOVE)) = 0 then
  begin
    flip_linedef(ldf2);
    newsec := make_box_ext(l, ldf1, ldf2, ThisStyle, c, @ldnew1, @ldnew2);
    flip_linedef(ldf2);
    offs := (nearsec^.ceiling_height - nearsec^.floor_height) - ThisLink^.height1;
    ldnew2^.right^.y_offset := offs;
    ldnew1^.right^.y_offset := offs;

    if (ThisLink^.bits and LINK_ANY_DOOR <> 0) or c^.doorless_jambs then
    begin
      ldnew1^.right^.middle_texture := ThisStyle^.doorjamb;
      ldnew2^.right^.middle_texture := ThisStyle^.doorjamb;
    end;

    newsec^.floor_height := nearsec^.floor_height;
    if ThisLink^.bits and LINK_MAX_CEILING <> 0 then
      newsec^.ceiling_height := maxtop
    else
{$IFDEF STRANGE_THINGS}
    newsec^.ceiling_height := nearsec^.ceiling_height;
{$ELSE}
    newsec^.ceiling_height := newsec^.floor_height + ThisLink^.height1;
{$ENDIF}

    newsec^.light_level := ThisStyle^.doorlight0;
    newsec^.style := ThisStyle; // Is this right?

    // This is where the level-change actually happens
    patch_upper(ldf1, t1, c);
    patch_upper(ldf2, t2, c);
    patch_lower(ldf1, ThisStyle^.kickplate, c); // or stepfronts?
    patch_lower(ldf2, NewStyle^.kickplate, c);

    ldf1^.flags := ldf1^.flags or TWO_SIDED;
    ldf2^.flags := ldf2^.flags or TWO_SIDED;
  end;

  // If no core, and a door, doorify the middle sector (s)
  if (ThisLink^.bits and (LINK_CORE or LINK_ALCOVE) = 0) and (ThisLink^.bits and LINK_ANY_DOOR <> 0) then
  begin
    if ThisLink^.bits and LINK_BARS = 0 then
    begin
      doorify(newsec, ldf1, ldf2, ThisStyle, NewStyle, c);
      if trigger_door then
      begin
        ldf1^.typ := LINEDEF_NORMAL;
        if c^.do_dm then
          ldf2^.typ := LINEDEF_NORMAL_S1_DOOR
        else
          ldf2^.typ := LINEDEF_NORMAL;
        newsec^.tag := tag1;
      end;
      if not ThisStyle^.moving_jambs then
      begin
        ldnew1^.flags := ldnew1^.flags or LOWER_UNPEGGED;
        ldnew2^.flags := ldnew2^.flags or LOWER_UNPEGGED;
      end;
      if ThisQuest <> nil then
      begin
        if ThisQuest^.goal = KEY_GOAL then
        begin
          ldf1^.typ := type_for_key(ThisQuest^.typ);
          if not painted_door then
            mark_door_for_key(l, ldf1, ThisQuest^.typ, ThisStyle, c);
          ldf2^.typ := type_for_key(ThisQuest^.typ); // Prevent monsters!
        end
        else if (ThisQuest^.goal = SWITCH_GOAL) and (ThisLink^.bits and LINK_LOCK_CORE = 0) then
        begin
          ldf1^.typ := LINEDEF_NORMAL;
          if c^.do_dm then
            ldf2^.typ := LINEDEF_NORMAL_S1_DOOR
          else
            ldf2^.typ := LINEDEF_NORMAL;
          newsec^.tag := ThisQuest^.tag;
          mark_door_for_lock(l, ldf1, ThisStyle, c);
        end; // end else if tag goal
      end; // end if ThisQuest
    end
    else
    begin
      announce(VERBOSE, 'Barred door');
      if ThisLink^.bits and LINK_LOCK_CORE <> 0 then
        barify(l, ldf1, ldf2, nil, 16 * l^.hugeness, nil, ThisStyle, c)
      else
        barify(l, ldf1, ldf2, ThisQuest, 16 * l^.hugeness, nil, ThisStyle, c);
    end; // end else barred door
  end;

  // If a core, and door(s), need to make the door sector(s)
  if (ThisLink^.bits and LINK_CORE <> 0) and (ThisLink^.bits and LINK_ANY_DOOR <> 0) then
  begin
    if ThisLink^.bits and LINK_NEAR_DOOR <> 0 then
    begin
      ldnew1 := lefthand_box_ext(l, ldf1, ThisLink^.depth1, ThisStyle, c, @ldedge1, @ldedge2);
      ldedge1^.right^.middle_texture := ThisStyle^.doorjamb;
      ldedge2^.right^.middle_texture := ThisStyle^.doorjamb;
      if not ThisStyle^.moving_jambs then
      begin
        ldedge1^.flags := ldedge1^.flags or LOWER_UNPEGGED;
        ldedge2^.flags := ldedge2^.flags or LOWER_UNPEGGED;
      end;
      // Does the y offset of the doorjambs actually matter?
      offs := (nearsec^.ceiling_height - nearsec^.floor_height) - ThisLink^.height1;
      ldedge2^.right^.y_offset := offs;
      ldedge1^.right^.y_offset := offs;
    end;
    if ThisLink^.bits and LINK_FAR_DOOR <> 0 then
    begin
      ldnew2 := lefthand_box_ext(l, ldf2, ThisLink^.depth1, NewStyle, c, @ldedge1, @ldedge2);
      ldedge1^.right^.middle_texture := NewStyle^.doorjamb;
      ldedge2^.right^.middle_texture := NewStyle^.doorjamb;
      if not NewStyle^.moving_jambs then
      begin
        ldedge1^.flags := ldedge1^.flags or LOWER_UNPEGGED;
        ldedge2^.flags := ldedge2^.flags or LOWER_UNPEGGED;
      end;
      offs := (farsec^.ceiling_height - farsec^.floor_height) - ThisLink^.height1;
      ldedge2^.right^.y_offset := offs;
      ldedge1^.right^.y_offset := offs;
    end;
    // Now we're working on the other sides of the doors.
    // But we can't really doorify them yet, since they have
    // no far sectors, and their linedefs need to be flipped
    // for the rest of the alg to work.  So record fixup info
    // for later.
    need_to_doorify := TRUE; // Need to flip when done
    if ThisLink^.bits and LINK_NEAR_DOOR <> 0 then
    begin
      ldflip1a := ldf1;
      ldflip1b := ldnew1;
      sflip1 := ldf1^.left^.sector;
    end;
    if ThisLink^.bits and LINK_FAR_DOOR <> 0 then
    begin
      ldflip2a := ldf2;
      ldflip2b := ldnew2;
      sflip2 := ldf2^.left^.sector;
      // Might as well fix the light-level on the far door while we're here
      sflip2^.light_level := NewStyle^.doorlight0;
    end;
    // Now we're working on the far sides of the (future) door(s)
    if ThisLink^.bits and LINK_NEAR_DOOR <> 0 then ldf1 := ldnew1;
    if ThisLink^.bits and LINK_FAR_DOOR <> 0 then ldf2 := ldnew2;
  end;

  // If alcoves, make them now, and take new linedefs
  if ThisLink^.bits and LINK_ALCOVE <> 0 then
  begin
    announce(VERBOSE, 'Making alcoves');
    ldnew1 := lefthand_box_ext(l, ldf1, ThisLink^.width2, ThisStyle, c, @ldedge1, @ldedge2);
    if effective_left <> 0 then
    begin
      ldedgeopen := ldedge2;
      ldedgeclosed := ldedge1;
    end
    else
    begin
      ldedgeopen := ldedge1;
      ldedgeclosed := ldedge2;
    end;
    // The near alcove copies the near room
    ldnew1^.right^.middle_texture := ldedgeopen^.right^.middle_texture;
    ldedgeopen^.right^.middle_texture := c^.null_texture;
    ldedgeopen^.flags := ldedgeopen^.flags or TWO_SIDED;
    ldnew1^.right^.sector^.floor_height := ldf1^.right^.sector^.floor_height;
    if ThisLink^.bits and LINK_MAX_CEILING <> 0 then
      ldnew1^.right^.sector^.ceiling_height := maxtop
    else
      ldnew1^.right^.sector^.ceiling_height := ldf1^.right^.sector^.floor_height + ThisLink^.height1;

    offs := nearsec^.ceiling_height - ldnew1^.right^.sector^.ceiling_height;
    ldnew1^.right^.y_offset := offs;
    ldedgeclosed^.right^.y_offset := offs;

    patch_upper(ldf1, t1, c);
    ldf1 := ldedgeopen;
    ldnew2 := lefthand_box_ext(l, ldf2, ThisLink^.width2, NewStyle, c, @ldedge1, @ldedge2);
    if effective_left <> 0 then
    begin
      ldedgeopen := ldedge2;
      ldedgeclosed := ldedge1;
    end
    else
    begin
      ldedgeopen := ldedge1;
      ldedgeclosed := ldedge2;
    end;
    // and the far the far
    ldnew2^.right^.middle_texture := ldedgeopen^.right^.middle_texture;
    ldedgeopen^.right^.middle_texture := c^.null_texture;
    ldedgeopen^.flags := ldedgeopen^.flags or TWO_SIDED;
    ldnew2^.right^.sector^.floor_height := farsec^.floor_height;
    if ThisLink^.bits and LINK_MAX_CEILING <> 0 then
      ldnew2^.right^.sector^.ceiling_height := maxtop
    else
      ldnew2^.right^.sector^.ceiling_height := farsec^.floor_height + ThisLink^.height1;
    ldnew2^.right^.sector^.light_level := NewStyle^.roomlight0;

    offs := farsec^.ceiling_height - ldnew2^.right^.sector^.ceiling_height;
    ldedgeclosed^.right^.y_offset := offs;
    ldnew2^.right^.y_offset  := offs;

    patch_upper(ldf2, t2, c);
    ldf2 := ldedgeopen;
    // Now we're working out from the alcoves
    len := ThisLink^.width2;
  end;

  // Record the area to maybe put a monster in
  mminx := ldf1^.from^.x;
  if ldf1^._to^.x < mminx then mminx := ldf1^._to^.x;
  if ldf2^.from^.x < mminx then mminx := ldf2^.from^.x;
  if ldf2^._to^.x < mminx then mminx := ldf2^._to^.x;
  mminy := ldf1^.from^.y;
  if ldf1^._to^.y < mminy then mminy := ldf1^._to^.y;
  if ldf2^.from^.y < mminy then mminy := ldf2^.from^.y;
  if ldf2^._to^.y < mminy then mminy := ldf2^._to^.y;
  mmaxx := ldf1^.from^.x;
  if ldf1^._to^.x > mmaxx then mmaxx := ldf1^._to^.x;
  if ldf2^.from^.x > mmaxx then mmaxx := ldf2^.from^.x;
  if ldf2^._to^.x > mmaxx then mmaxx := ldf2^._to^.x;
  mmaxy := ldf1^.from^.y;
  if ldf1^._to^.y > mmaxy then mmaxy := ldf1^._to^.y;
  if ldf2^.from^.y > mmaxy then mmaxy := ldf2^.from^.y;
  if ldf2^._to^.y > mmaxy then mmaxy := ldf2^._to^.y;
  // and the facing
  mangle := facing_right_from_ld(ldf1);

  // If the core is stairs, put in all but the last
  if ThisLink^.bits and LINK_STEPS <> 0 then
  begin
    front := ThisStyle^.kickplate;
    g := ThisStyle^.lamp0;
    add_lamps := FALSE;
    if g^.height > ThisLink^.height1 then
      g := ThisStyle^.shortlamp0;
    depth := ThisLink^.depth3 div (ThisLink^.stepcount + 1);
    if (ThisLink^.bits and LINK_LAMPS <> 0) and (g^.width <= depth) and (g^.width * 2 + 64 <= len) then
    begin
      add_lamps := TRUE;
      announce(VERBOSE, 'stair lamps');
    end;
    stepdelta := ThisLink^.floordelta div ThisLink^.stepcount;
    if ThisStyle^.light_steps and (ThisStyle^.walllight <> nil) then
      front := ThisStyle^.walllight
    else if (ThisStyle^.stepfront <> nil) and (ThisStyle^.stepfront^.height >= abs(stepdelta)) then
      front := ThisStyle^.stepfront;
    // Looks just like the recess stuff, mostly
    for i :=0 to ThisLink^.stepcount - 1 do
    begin
      ldnew1 := lefthand_box_ext(l, ldf1, depth, ThisStyle, c, @ldedge1, @ldedge2);
      if (add_lamps) and Odd(i) then
      begin
        point_from(ldedge1^.from^.x, ldedge1^.from^.y, ldedge1^._to^.x, ldedge1^._to^.y, RIGHT_TURN, g^.width div 2, @x, @y);
        point_from(ldedge1^._to^.x, ldedge1^._to^.y, x, y, RIGHT_TURN, depth div 2, @x, @y);
        new_thing(l, x, y, 0, g^.thingid, 7, c);
        point_from(ldedge2^.from^.x, ldedge2^.from^.y, ldedge2^._to^.x, ldedge2^._to^.y, RIGHT_TURN, g^.width div 2, @x, @y);
        point_from(ldedge2^._to^.x, ldedge2^._to^.y, x, y, RIGHT_TURN, depth div 2, @x, @y);
        new_thing(l, x, y, 0, g^.thingid, 7, c);
      end;
      ldnew1^.right^.sector^.floor_height := ldf1^.right^.sector^.floor_height + stepdelta;
      if ThisLink^.bits and LINK_MAX_CEILING <> 0 then
        ldnew1^.right^.sector^.ceiling_height := maxtop
      else
        ldnew1^.right^.sector^.ceiling_height := ldnew1^.right^.sector^.floor_height + ThisLink^.height1;
      ldnew1^.right^.sector^.floor_flat := ThisStyle^.stepfloor;

      offs := nearsec^.ceiling_height - ldedge1^.right^.sector^.ceiling_height;
      ldedge1^.right^.y_offset := offs;
      ldedge2^.right^.y_offset := offs;

      patch_upper(ldf1, t1, c);
      patch_lower(ldf1, front, c);
      ldf1^.flags := ldf1^.flags and not LOWER_UNPEGGED;
      ldf1 := ldnew1;
    end; // end for each step
    // The core-making step will do the top (bottom) (far) step
  end; // end if each steps

  // If a core, need to make it
  if ThisLink^.bits and LINK_CORE <> 0 then
  begin
    flip_linedef(ldf2);
    newsec := make_box_ext(l, ldf1, ldf2, ThisStyle, c, @ldedge1, @ldedge2);
    flip_linedef(ldf2);
    if ThisLink^.bits and LINK_MAX_CEILING <> 0 then
      newsec^.ceiling_height := maxtop;
    if newsec^.ceiling_height - ldf1^.right^.sector^.floor_height < 64 then
      newsec^.ceiling_height := ldf1^.right^.sector^.floor_height + 64;
    if newsec^.ceiling_height - ldf2^.right^.sector^.floor_height < 64 then
      newsec^.ceiling_height := ldf2^.right^.sector^.floor_height + 64;

    offs := nearsec^.ceiling_height - newsec^.ceiling_height;
    ldedge2^.right^.y_offset := offs;
    ldedge1^.right^.y_offset := offs;

    if (ThisQuest <> nil) and (ThisLink^.bits and LINK_LOCK_CORE <> 0) then
    begin
      newsec^.floor_flat := ThisStyle^.nukage1;
      newsec^.special := NUKAGE1_SPECIAL;
      newsec^.tag := ThisQuest^.tag;
      newsec^.floor_height := newsec^.floor_height - 24 - roll(ThisLink^.floordelta);
      if newsec^.light_level < 160 then
        newsec^.light_level := 160; // Visible
      patch_lower(ldf1, ThisStyle^.kickplate, c); // Or stepfront?
      if rollpercent(50) then
        ldf2^.flags := ldf2^.flags or BLOCK_MONSTERS; // Can't decide!
      haa^.haas[ITYTD].health := haa^.haas[ITYTD].health - 10;
      haa^.haas[HMP].health := haa^.haas[HMP].health - 5;
      announce(VERBOSE, 'Nukage lock');
    end
    else if rollpercent(l^.p_force_nukage) and (ThisLink^.bits and LINK_LIFT = 0) and
            (ThisLink^.bits and LINK_STEPS = 0) and (ThisLink^.depth3 >= 64) and
            (ThisLink^.depth3 <= 196) then
    begin
      newsec^.floor_flat := ThisStyle^.nukage1;
      newsec^.special := NUKAGE1_SPECIAL;
      if ThisLink^.floordelta < 0 then
      begin
        newsec^.floor_height := newsec^.floor_height + ThisLink^.floordelta; // Fixed +- bug here
        newsec^.floor_height := newsec^.floor_height - roll(25 + ThisLink^.floordelta);
      end
      else
        newsec^.floor_height := newsec^.floor_height - roll(25 - ThisLink^.floordelta);
      if newsec^.light_level < 160 then
        newsec^.light_level := 160; // Visible
      patch_lower(ldf1, ThisStyle^.kickplate, c);
      haa^.haas[ITYTD].health := haa^.haas[ITYTD].health - 10;
      haa^.haas[HMP].health := haa^.haas[HMP].health - 5;
      announce(VERBOSE, 'Nukage link');
    end
    else if rollpercent(l^.p_falling_core) and
            (linelen(ldedge1) >= (120 * l^.hugeness)) and
            (ThisLink^.bits and LINK_LIFT = 0) and (flipstate = 0) then // Maybe a fun trap
      try_falling_core(l, ldedge1, ldedge2, haa, c);
    ldf2^.flags := ldf2^.flags or TWO_SIDED;
    patch_upper(ldf1, t1, c);

    // This is where the level-change actually happens, eh?
    patch_upper(ldf2, NewStyle^.wall0, c);
    patch_lower(ldf2, NewStyle^.kickplate, c); // or stepfront?

    // On the first pass, sometimes save the righthand wall of the
    // core, so that the second pass can make a decorative room.
    if flipstate = 1 then
    begin
      if (ThisLink^.bits and (LINK_LIFT or LINK_ALCOVE) = 0) and
         (ThisLink^.bits and LINK_DECROOM <> 0) and
          (linelen(ldedge2) > 63) then
        ThisLink^.cld := ldedge2
      else
        ThisLink^.cld := nil;
    end;
    // On the second pass, if we did that, make the room
    if (flipstate = 2) and (ThisLink^.cld <> nil) then
    begin
      if rollpercent(10) then
      begin
        if make_window(l, ldedge1, ThisLink^.cld, ThisLink, ThisStyle, NewStyle, c) then
          announce(LOG, 'Intertwin window');
      end
      else
      begin
        if make_decroom(l, ldedge1, ThisLink^.cld, c) then
          announce(LOG, 'Intertwin decroom');
      end;
    end;
  end; // end if a core

  // If the core is a lift, make that happen
  if ThisLink^.bits and LINK_LIFT <> 0 then
  begin
    if trigger_lift then
      newsec^.tag := tag1
    else
      newsec^.tag := new_tag(l);
    newsec^.ceiling_flat := ThisStyle^.doorceiling; // really?
    newsec^.floor_flat := ThisStyle^.doorfloor; // really?
    ldf1^.tag := newsec^.tag;
    ldf2^.tag := newsec^.tag;
    if nearsec^.floor_height > farsec^.floor_height then
    begin
      newsec^.floor_height := nearsec^.floor_height;
      ldf1^.typ := LINEDEF_WR_LOWER_LIFT;
      ldf1^.left^.lower_texture := ThisStyle^.support0; // yes?
      if not trigger_lift then
        ldf2^.typ := NewStyle^.slifttype;
      patch_lower(ldf2, NewStyle^.support0, c);
      if (NewStyle^.liftface <> nil) and
         (nearsec^.floor_height - farsec^.floor_height <= NewStyle^.liftface^.height) and
         (linelen(ldf2) = NewStyle^.liftface^.width) then
      begin
        ldf2^.right^.lower_texture := NewStyle^.liftface;
        ldf2^.right^.x_offset := 0;
        announce(VERBOSE, 'Lift texture');
      end;
      ldf2^.flags := ldf2^.flags and not LOWER_UNPEGGED; // Lift-falling must be visible!
    end
    else
    begin
      newsec^.floor_height := farsec^.floor_height;
      if not trigger_lift then
        ldf1^.typ := ThisStyle^.slifttype;
      patch_lower(ldf1, ThisStyle^.support0, c);
      if (ThisStyle^.liftface <> nil) and
         (farsec^.floor_height - nearsec^.floor_height <= ThisStyle^.liftface^.height) and
         (linelen(ldf1) = ThisStyle^.liftface^.width) then
      begin
        ldf1^.right^.lower_texture := ThisStyle^.liftface;
        ldf1^.right^.x_offset := 0;
        announce(VERBOSE, 'Lift texture');
      end;
      ldf1^.flags := ldf1^.flags and not LOWER_UNPEGGED; // Lift-falling must be visible!
      ldf2^.typ := LINEDEF_WR_LOWER_LIFT;
      ldf2^.left^.lower_texture := NewStyle^.support0; // right?
    end; // end else lift that way
    newsec^.ceiling_height := newsec^.floor_height + ThisLink^.height1;

    // and re-figure the y offsets in the core
    offs := nearsec^.ceiling_height - newsec^.ceiling_height;
    ldedge2^.right^.y_offset := offs;
    ldedge1^.right^.y_offset := offs;

    patch_upper(ldf1, ThisStyle^.wall0, c);
    patch_upper(ldf2, NewStyle^.wall0, c);
  end; // end if doing a lift

  // Maybe put a monster in the link
  if (haa <> nil) and (ThisLink^.bits and LINK_CORE <> 0) and rollpercent(40) then
  begin
    m := timely_monster(haa, c, @levels, rollpercent(l^.p_biggest_monsters), 0);
    if m <> nil then
    begin
      if rollpercent(15) then
        levels := levels or $08; // deaf
      // Try to place it
      if not rollpercent(l^.p_rational_facing) then
        mangle := 90 * roll(4);
      if place_object_in_region(l, mminx, mminy, mmaxx, mmaxy, c, m^.thingid, m^.width, mangle, 0, 0, levels) <> nil then
      begin
        if m^.thingid = ID_SKULL then announce(NONE, 'Skull')
        else if m^.thingid = ID_HEAD then announce(VERBOSE, 'HEAD')
        else if m^.thingid = ID_SKEL then announce(VERBOSE, 'SKEL')
        else if m^.thingid = ID_HELL then announce(VERBOSE, 'KNIGHT')
        else if m^.thingid = ID_ARCHIE then announce(VERBOSE, 'VILE');

        update_haa_for_monster(haa, m, levels, 0, c);
        haa_unpend(haa);

        announce(VERBOSE, 'Link guard');
      end; // end if placed it
    end; // end if timely one
  end; // end if a monster

  // Finally, if we have unmade doors, make 'em
  if need_to_doorify then
  begin
    if ThisLink^.bits and LINK_NEAR_DOOR <> 0 then
    begin
      flip_linedef(ldflip1b);
      doorify(sflip1, ldflip1a, ldflip1b, ThisStyle, ThisStyle, c);
      if trigger_door then
      begin
        ldflip1a^.typ := LINEDEF_NORMAL;
        ldflip1b^.typ := LINEDEF_NORMAL_DOOR; // S1 door would break!
        sflip1^.tag := tag1;
      end;
      if ThisQuest <> nil then
      begin
        if ThisQuest^.goal = KEY_GOAL then
        begin
          ldflip1a^.typ := type_for_key(ThisQuest^.typ);
          if not painted_door then
            mark_door_for_key(l, ldflip1a, ThisQuest^.typ, ThisStyle, c);
          ldflip1b^.typ := type_for_key(ThisQuest^.typ); // Prevent monsters!
        end
        else if (ThisQuest^.goal = SWITCH_GOAL) and (ThisLink^.bits and LINK_LOCK_CORE = 0) then
        begin
          ldflip1a^.typ := LINEDEF_NORMAL;
          if c^.do_dm then
            ldflip1b^.typ := LINEDEF_NORMAL_S1_DOOR
          else
            ldflip1b^.typ := LINEDEF_NORMAL;
          sflip1^.tag := ThisQuest^.tag;
          mark_door_for_lock(l, ldflip1a, ThisStyle, c);
        end; // end else if tag goal
      end; // end else if ThisQuest
    end;
    if ThisLink^.bits and LINK_FAR_DOOR <> 0 then
    begin
      flip_linedef(ldflip2b);
      doorify(sflip2, ldflip2a, ldflip2b, NewStyle, NewStyle, c);
      if trigger_door then
      begin
        ldflip2a^.typ := LINEDEF_NORMAL;
        ldflip2b^.typ := LINEDEF_NORMAL_DOOR; // S1 door would break!
        sflip2^.tag := tag1;
      end;
    end;
  end; // end if need to doorify

  // Whew!

end;  // end e_bl_inner()

// Return a random linedef in the plain; no sidedefs or anything.
function starting_linedef(const l: level_p; const ThisStyle: style_p;
  const c: config_p): linedef_p;
var
  v1, v2: vertex_p;
begin
  v1 := new_vertex(l, 0, 0); // Might as well
  // Should consult the style/config?
  v2 := new_vertex(l, 0, l^.hugeness * 64 * (2 + roll(9)));
//  v2 := new_vertex(l, 0, l^.hugeness * 256 * (2 + roll(9))); <- Bigger lines
  result := new_linedef(l, v1, v2);
end;

// Given a ray from the given point to the given point, turn in
// the given direction and go the given distance.  Return the
// point that we end up at.
procedure point_from(const x1, y1, x2, y2: integer; const angle: integer;
                     const len: integer; const x3, y3: PInteger);
var
  // Stub; right angles and axis-parallel lines only
  newdx, newdy: integer;
begin
  if x1 = x2 then // Parallel to the Y-axis
  begin
    newdy := 0;
    if y2 > y1 then // Up
      newdx := 1
    else // Down
      newdx := -1;
  end
  else // to the X-axis
  begin
    newdx := 0;
    if x2 > x1 then // rightward
      newdy := -1
    else
      newdy := 1;
  end;
  if angle = LEFT_TURN then
  begin
    newdx := 0 - newdx;
    newdy := 0 - newdy;
  end;
  x3^ := x2 + len * newdx;
  y3^ := y2 + len * newdy;
end;

// Print, log, whatever.  Really ought to support real log files. */
procedure announce(const announcetype: integer; const s: string);
begin
  case announcetype of
    NONE: exit;
    VERBOSE: if global_verbosity = 0 then exit; // * fallthrough
    LOG: ;
    NOTE: printf('NOTE: ');
    WARNING: printf('WARNING: ');
    ERROR: printf('ERROR: ');
  else
    printf('HEY: ');
  end;
  printf('%s'#13#10, [s]);
end;

// Install a switch on the given linedef.  If the actual
// switch linedef changes (due to recessing, say), return
// that new linedef, else the old one.  Doesn't set the
// type or tag, just the texture and stuff.   If xld is
// given, it gets the two-sided center part of ld.
function install_switch(const l: level_p; ld: linedef_p; const recess, fancy: boolean;
  const key: SmallInt; const ThisStyle: style_p; const c: config_p; const xld: linedef_pp): linedef_p;
var
  ld2, ldedge1, ldedge2: linedef_p;
  rdepth: integer;
  tx, t1: texture_p;
  Offs: SmallInt;
begin
  rdepth := 8;
  tx := nil;

  if fancy then
  begin
    ThisStyle^.lightboxes := TRUE;
    announce(VERBOSE, 'fancy switch');
  end;
  ld := borderize(l, ld, 64, TRUE, ThisStyle, LIGHT, nil, nil, c);
  if xld <> nil then
    xld^ := ld;
  ThisStyle^.lightboxes := FALSE;
  if recess then
    if key <> 0 then
    begin
      tx := texture_for_key(key, ThisStyle, c);
      rdepth := tx^.width;
      if rdepth > 8 then
       if not empty_left_side(l, ld, rdepth) then
         rdepth := 8;
    end;

  if empty_left_side(l, ld, rdepth) then
  begin
    t1 := ld^.right^.middle_texture;
    ld2 := lefthand_box_ext(l, ld, rdepth, ThisStyle, c, @ldedge1, @ldedge2);
    ld2^.right^.sector^.ceiling_height :=
      ld2^.right^.sector^.floor_height + 72;  // Should vary?
    // Maybe paint/light the recesses
    if key <> 0 then
    begin
      ldedge2^.right^.middle_texture := tx;
      ldedge1^.right^.middle_texture := tx;
    end
    else if ThisStyle^.light_recesses and (ThisStyle^.walllight <> nil) then
    begin
      announce(VERBOSE, 'Lit switch');
      ldedge2^.right^.middle_texture := ThisStyle^.walllight;
      ldedge1^.right^.middle_texture := ThisStyle^.walllight;
      make_lighted(l, ld2^.right^.sector, c);
    end
    else
    begin
      offs := (ld^.right^.sector^.ceiling_height - ld^.right^.sector^.floor_height) - 72;
      ldedge2^.right^.y_offset := offs;
      ldedge1^.right^.y_offset := offs;
    end;
    patch_upper(ld, t1, c);
    ld := ld2;
  end;

  ld^.right^.middle_texture := ThisStyle^.switch0;
  ld^.right^.x_offset := 0;
  ld^.right^.y_offset := ThisStyle^.switch0^.y_bias;
  ld^.flags := ld^.flags or LOWER_UNPEGGED;
  result := ld;
end;

// Perhaps add a deathmatch start to this sector, if it doesn't
// already have one, and we're doing deathmatch stuff.
function maybe_add_dm_start(const l: level_p; const s: sector_p; const c: config_p;
  const force: boolean): boolean;
begin
  if not c^.do_dm then
  begin
    result := FALSE;
    exit;
  end;
  if s^.has_dm and not force then
  begin
    result := FALSE;
    exit;
  end;
  if place_object(l, s, c, ID_DM, 34, -1, s^.entry_x, s^.entry_y, 7) <> nil then
  begin
    s^.has_dm := TRUE;
    inc(l^.dm_count);
    if not s^.has_dm_weapon then
      if place_object(l, s, c, ID_SHOTGUN, 24, 0, 0, 0, $17) <> nil then
        s^.has_dm_weapon := TRUE;
    result := TRUE;
    exit;
  end;
  result := FALSE;
end;

// Does anything involved with closing a quest that has to happen
// -after- the room is populated and embellished and stuff.
procedure close_quest_final(const l: level_p; const s: sector_p; const q: quest_p;
  const haa: haa_p; const c: config_p);
var
  t: thing_p;
  i: integer;
  ldf: linedef_p;
begin
  t := q^.thing;

  l^.goal_room := s;

  maybe_add_dm_start(l, s, c, FALSE);

  if (t <> nil) and (q^.auxtag <> 0) and (q^.surprise <> nil) then
  begin
    trigger_box(l, t, s, q^.auxtag, LINEDEF_WR_OPEN_DOOR, c);
    populate_linedef(l, q^.surprise, haa, c, FALSE);
  end;

  // If we've put in the SL exit, but not yet the thing that
  // opens it, do one last try at that.
  if (q^.goal = LEVEL_END_GOAL) and l^.sl_open_ok then
  begin
    t := place_required_small_pickable(l, s, c);
    if t <> nil then
    begin
      trigger_box(l, t, s, l^.sl_tag, l^.sl_type, c);
      l^.sl_done := TRUE;
      l^.sl_open_ok := FALSE;
      announce(VERBOSE, 'Did sl triggerbox');
    end;
  end;

  // On the other hand, if we haven't even put in the SL exit yet,
  // we're really desparate!
  if need_secret_level(c) and not l^.sl_done and (l^.sl_tag = 0) and (q^.goal = LEVEL_END_GOAL) then
  begin
    i := mark_decent_boundary_linedefs(l, s, 32);
    ldf := random_marked_linedef(l, i);
    unmark_linedefs(l);
    if i <> 0 then
    begin
      if ldf^.right^.middle_texture^.subtle <> nil then
        ldf^.right^.middle_texture := ldf^.right^.middle_texture^.subtle
      else
        ldf^.right^.middle_texture := s^.style^.support0;
      ldf^.typ := LINEDEF_S1_SEC_LEVEL;
      announce(LOG, 'Last-ditch SL exit!');
      l^.sl_done := TRUE;
    end;
  end;
end;

// Make one of them instant-death rooms, like at the end
// of E1M8, using the linedef in the linkto(), and the
// style for the walls 'n' stuff.
function death_room(const l: level_p; const ld: linedef_p; const ThisStyle: style_p;
  const c: config_p): SmallInt;
var
  ldnew: linedef_p;
  gatelink: link_p;
  newsector: sector_p;
  minx, miny, maxx, maxy: integer;
  x: integer;
begin
  gatelink := gate_link_f(l,c);

  ldnew := make_linkto(l, ld, gatelink, ThisStyle, c, nil);
  if ldnew = nil then
  begin
    result := 0;
    exit;
  end;
  while linelen(ldnew) < 320 do
  begin
    ldnew^._to^.x := ldnew^.from^.x + 2 *
      (ldnew^._to^.x - ldnew^.from^.x);
    ldnew^._to^.y := ldnew^.from^.y + 2 *
      (ldnew^._to^.y - ldnew^.from^.y);
  end;
  newsector := generate_room_outline(l, ldnew, ThisStyle, FALSE, c);
  newsector^.style := ThisStyle;
  paint_room(l, newsector, ThisStyle, c);
  newsector^.tag := new_tag(l);
  newsector^.special := DEATH_SECTOR;
  newsector^.light_level := 80;

  find_sector_rectangle(l, newsector, minx, miny, maxx, maxy);
  new_thing(l, (minx + maxx) div 2, (miny + maxy) div 2, 90 * roll(4), ID_GATEOUT, 7, c);

  // Worry about having *too many* sergeants?
  x := minx + 22;
  while x <= maxx - 22 do
  begin
    new_thing(l, x, miny + 22, 90, ID_SERGEANT, 7, c);
    new_thing(l, x, maxy - 22, 270, ID_SERGEANT, 7, c);
    x := x + 44;
  end;

  result := newsector^.tag;
end;

// Simple trial implementation: just an "EXIT" gate to
// an e1m8 instant-death room.
function e1m8_gate(const l: level_p; const ld: linedef_p; const s: sector_p;
  const haa: haa_p; const c: config_p): boolean;
var
  tag: SmallInt;
begin
  tag := death_room(l, ld, s^.style, c);

  if tag = 0 then
  begin
    result := FALSE;
    exit;
  end;
  s^.gate := new_gate(l, 0, tag, 0, FALSE, c);
  install_gate(l, s, s^.style, haa, TRUE, c);
  s^.middle_enhanced := TRUE;
  gate_populate(l, s, haa, FALSE, c); // safe/correct?
  result := TRUE;
end;

// Put down zero or more of the required powerups in the
// arena's prep-room.
procedure prepare_arena_gate(const l: level_p; const s: sector_p; const a: arena_p;
  const haa: haa_p; const c: config_p);
begin
end;

// Actually put down the main linedefs and sectors
// and stuff for the arena.
procedure install_arena(const l: level_p; const a: arena_p; const s: sector_p;
  const haa: haa_p; const c: config_p);
var
  maxx: integer;
  v, v1, v2, v3, v4: vertex_p;
  vt1, vt2: vertex_p;
  upness, acrossness, border, i, n: integer;
  newsec: sector_p;
  ld: linedef_p;
  ch, d: SmallInt;
  lamp: genus_p;
  light_flat: flat_p;
begin
  maxx := -HUGE_NUMBER;
  ch := 128; // Too simple
  newsec := new_sector(l, 0, ch, a^.floor, c^.sky_flat);
  newsec^.light_level := c^.minlight + roll(100);
  newsec^.style := s^.style;
  a^.outersec := newsec;

  v := l^.vertex_anchor;
  while v <> nil do
  begin
    if v^.x > maxx then
      maxx := v^.x;
    v := v^.next;
  end;
  maxx := maxx + 256;
  upness := 750 + roll(501);
  acrossness := 3 * upness;
  if a^.props and ARENA_PORCH <> 0 then
    border := 72 + 32 * roll(11)
  else
    border := 50 + roll(200);
  maxx := maxx + border + 16;
  a^.minx := maxx;
  a^.maxx := maxx + acrossness;
  a^.miny := -(upness div 2);
  a^.maxy := upness div 2;

  // Make the outer walls
  if a^.props and ARENA_PORCH <> 0 then
  begin
    // Flat with promenade
    newsec^.ceiling_flat := newsec^.style^.ceiling0;
    v1 := new_vertex(l, a^.minx - (16 + border div 2), a^.miny - (16 + border div 2));
    v2 := new_vertex(l, a^.minx - (16 + border div 2), a^.maxy + (16 + border div 2));
    v3 := new_vertex(l, a^.maxx + (16 + border div 2), a^.maxy + (16 + border div 2));
    v4 := new_vertex(l, a^.maxx + (16 + border div 2), a^.miny - (16 + border div 2));

    ld := new_linedef(l, v1, v2);
    ld^.right := new_sidedef(l, newsec, c);
    ld^.right^.middle_texture := a^.walls;

    ld := new_linedef(l, v2, v3);
    ld^.right := new_sidedef(l, newsec, c);
    ld^.right^.middle_texture := a^.walls;

    ld := new_linedef(l, v3, v4);
    ld^.right := new_sidedef(l, newsec, c);
    ld^.right^.middle_texture := a^.walls;

    ld := new_linedef(l, v4, v1);
    ld^.right := new_sidedef(l, newsec, c);
    ld^.right^.middle_texture := a^.walls;

    if a^.props and ARENA_LAMPS <> 0 then
    begin
      lamp := newsec^.style^.lamp0;
      if lamp^.height >= ch then
        lamp := newsec^.style^.shortlamp0;
      new_thing(l, v1^.x + 64, v1^.y + 64, 0, lamp^.thingid, 7, c);
      new_thing(l, v2^.x + 64, v2^.y - 64, 0, lamp^.thingid, 7, c);
      new_thing(l, v3^.x - 64, v3^.y - 64, 0, lamp^.thingid, 7, c);
      new_thing(l, v4^.x - 64, v4^.y + 64, 0, lamp^.thingid, 7, c);
    end;
  end
  else
  begin
    // or irregular
    v1 := new_vertex(l, a^.minx - (16 + roll(border div 2)), a^.miny - (16 + roll(border div 2)));
    v2 := new_vertex(l, a^.minx - (16 + roll(border div 2)), a^.maxy + (16 + roll(border div 2)));
    v3 := new_vertex(l, a^.maxx + (16 + roll(border div 2)), a^.maxy + (16 + roll(border div 2)));
    v4 := new_vertex(l, a^.maxx + (16 + roll(border div 2)), a^.miny - (16 + roll(border div 2)));
    // left north-south wallset
    n := 1 + roll(10);
    vt1 := v1;
    for i := 1 to n - 1 do
    begin
      vt2 := new_vertex(l, a^.minx - (16 + roll(border)), a^.miny + i * (upness div (n + 1)));
      ld := new_linedef(l, vt1, vt2);
      ld^.right := new_sidedef(l, newsec, c);
      ld^.right^.middle_texture := a^.walls;
      vt1 := vt2;
    end;
    ld := new_linedef(l, vt1, v2);
    ld^.right := new_sidedef(l, newsec, c);
    ld^.right^.middle_texture := a^.walls;
    // top left-right wallset
    n := 1 + roll(10);
    vt1 := v2;
    for i := 1 to n - 1 do
    begin
      vt2 := new_vertex(l, a^.minx + i * (acrossness div (n + 1)), a^.maxy + (16 + roll(border)));
      ld := new_linedef(l, vt1, vt2);
      ld^.right := new_sidedef(l, newsec, c);
      ld^.right^.middle_texture := a^.walls;
      vt1 := vt2;
    end;
    ld := new_linedef(l, vt1, v3);
    ld^.right := new_sidedef(l, newsec, c);
    ld^.right^.middle_texture := a^.walls;
    // right south-north wallset
    n := 1 + roll(10);
    vt1 := v3;
    for i := 1 to n - 1 do
    begin
      vt2 := new_vertex(l, a^.maxx + (16 + roll(border)), a^.maxy - i * (upness div (n + 1)));
      ld := new_linedef(l, vt1, vt2);
      ld^.right := new_sidedef(l, newsec, c);
      ld^.right^.middle_texture := a^.walls;
      vt1 := vt2;
    end;
    ld := new_linedef(l, vt1, v4);
    ld^.right := new_sidedef(l, newsec, c);
    ld^.right^.middle_texture := a^.walls;
    // bottom right-left wallset
    n := 1 + roll(10);
    vt1 := v4;
    for i := 1 to n - 1 do
    begin
      vt2 := new_vertex(l, a^.maxx - i * (acrossness div (n + 1)), a^.miny - (16 + roll(border)));
      ld := new_linedef(l, vt1, vt2);
      ld^.right := new_sidedef(l, newsec, c);
      ld^.right^.middle_texture := a^.walls;
      vt1 := vt2;
    end;
    ld := new_linedef(l, vt1, v1);
    ld^.right := new_sidedef(l, newsec, c);
    ld^.right^.middle_texture := a^.walls;
  end;

  // Now the inner sector

  ch := 256 + 64 * roll(3);  // Too simple?
  newsec := new_sector(l, 0, ch, a^.floor, c^.sky_flat);
  newsec^.light_level := a^.outersec^.light_level;
  newsec^.style := s^.style;
  if a^.props and ARENA_ROOF <> 0 then
  begin
    newsec^.ceiling_flat := newsec^.style^.ceiling0;
    a^.outersec^.ceiling_flat := newsec^.style^.ceiling0;
    newsec^.light_level := newsec^.light_level - 16;
    if newsec^.light_level < c^.minlight then
      newsec^.light_level := c^.minlight;
    if newsec^.light_level > l^.bright_light_level then
      newsec^.light_level := l^.bright_light_level;
  end;

  // Some sector adjustments...
  if rollpercent(30) and (a^.props and ARENA_PORCH <> 0) then
  begin
    a^.outersec^.special := RANDOM_BLINK;
    a^.outersec^.light_level := a^.outersec^.light_level + 20;
    if a^.outersec^.light_level > l^.bright_light_level then
      a^.outersec^.light_level := l^.bright_light_level;
    light_flat := random_flat0(CEILING or LIGHT, c, nil);
    if light_flat <> nil then
      a^.outersec^.ceiling_flat := light_flat;
  end;

  if a^.props and ARENA_NUKAGE <> 0 then
  begin
    a^.outersec^.floor_height := a^.outersec^.floor_height - 8;
    if a^.props and ARENA_PORCH <> 0 then
      a^.outersec^.ceiling_height := a^.outersec^.ceiling_height - 8;
    a^.outersec^.floor_flat := a^.outersec^.style^.nukage1;
    a^.outersec^.special := NUKAGE1_SPECIAL;
  end
  else if (a^.props and ARENA_PORCH <> 0) and rollpercent(50) then
  begin
    d := 8 + 8 * roll(3);
    a^.outersec^.floor_height := a^.outersec^.floor_height + d;
    a^.outersec^.ceiling_height := a^.outersec^.ceiling_height + d;
  end;

  // Now the inner sector's boundaries

  v1 := new_vertex(l, a^.minx, a^.miny);
  v2 := new_vertex(l, a^.minx, a^.maxy);
  v3 := new_vertex(l, a^.maxx, a^.maxy);
  v4 := new_vertex(l, a^.maxx, a^.miny);

  ld := new_linedef(l, v1, v2);
  ld^.flags := ld^.flags or TWO_SIDED;
  ld^.right := new_sidedef(l, newsec, c);
  ld^.right^.middle_texture := c^.null_texture;
  ld^.left := new_sidedef(l, a^.outersec, c);
  ld^.left^.middle_texture := c^.null_texture;
  patch_upper(ld, a^.walls, c);
  patch_lower(ld, a^.walls, c);

  ld := new_linedef(l, v2, v3);
  ld^.flags := ld^.flags or TWO_SIDED;
  ld^.right := new_sidedef(l, newsec, c);
  ld^.right^.middle_texture := c^.null_texture;
  ld^.left := new_sidedef(l, a^.outersec, c);
  ld^.left^.middle_texture := c^.null_texture;
  patch_upper(ld, a^.walls, c);
  patch_lower(ld, a^.walls, c);

  ld := new_linedef(l, v3, v4);
  ld^.flags := ld^.flags or TWO_SIDED;
  ld^.right := new_sidedef(l, newsec, c);
  ld^.right^.middle_texture := c^.null_texture;
  ld^.left := new_sidedef(l, a^.outersec, c);
  ld^.left^.middle_texture := c^.null_texture;
  patch_upper(ld, a^.walls, c);
  patch_lower(ld, a^.walls, c);

  ld := new_linedef(l, v4, v1);
  ld^.flags := ld^.flags or TWO_SIDED;
  ld^.right := new_sidedef(l, newsec, c);
  ld^.right^.middle_texture := c^.null_texture;
  ld^.left := new_sidedef(l, a^.outersec, c);
  ld^.left^.middle_texture := c^.null_texture;
  patch_upper(ld, a^.walls, c);
  patch_lower(ld, a^.walls, c);

  if (a^.props and ARENA_LAMPS <> 0) and (a^.props and ARENA_PORCH = 0) then
  begin
    lamp := newsec^.style^.lamp0;
    if lamp^.height >= ch then
      lamp := newsec^.style^.shortlamp0;
    new_thing(l, v1^.x + 2, v1^.y + 2, 0, lamp^.thingid, 7, c);
    new_thing(l, v2^.x + 2, v2^.y - 2, 0, lamp^.thingid, 7, c);
    new_thing(l, v3^.x - 2, v3^.y - 2, 0, lamp^.thingid, 7, c);
    new_thing(l, v4^.x - 2, v4^.y + 2, 0, lamp^.thingid, 7, c);
  end;

  a^.innersec := newsec;
end;

// Make the arrival area, where the player first enters
// the arena, as well as any powerups he needs.
procedure arena_arrival(const l: level_p; const a: arena_p; const haa: haa_p;
  const c: config_p);
var
  minx, maxx: integer;
  cx, cy: integer;
  na0, na1, na2: single; // Needed ammos
  f0, f1, f2: integer;
  mask: integer;
  newsec: sector_p;
  ld1, ld2, ld3, ld4: linedef_p;
begin
  mask := 7;
  minx := a^.minx;
  maxx := a^.minx + (a^.maxx - a^.minx) div 3;
  cx := (minx + maxx) div 2;
  cy := (a^.miny + a^.maxy) div 2;

  // a simple version
  new_thing(l, cx, cy, 90 * roll(4), ID_GATEOUT, 7, c);
  a^.innersec^.tag := a^.fromtag;
  a^.innersec^.entry_x := cx;
  a^.innersec^.entry_y := cy;

  // except for this
  if a^.props and ARENA_ARRIVAL_HOLE <> 0 then
  begin
    newsec := clone_sector(l, a^.innersec);
    newsec^.floor_height := newsec^.floor_height - 384; // Down in a hole!
    parallel_innersec_ex(l, a^.innersec,newsec,
                         nil, a^.walls, a^.walls,
                         cx - 31, cy - 31, cx + 31, cy + 31,
                         c, @ld1, @ld2, @ld3, @ld4);
    // Fix teleport-tag
    a^.innersec^.tag := 0;
    newsec^.tag := a^.fromtag;
    // Make walls touchable
    flip_linedef(ld1);
    flip_linedef(ld2);
    flip_linedef(ld3);
    flip_linedef(ld4);
    ld1^.typ := LINEDEF_S1_RAISE_FLOOR;
    ld1^.tag := newsec^.tag;
    ld2^.typ := LINEDEF_S1_RAISE_FLOOR;
    ld2^.tag := newsec^.tag;
    ld3^.typ := LINEDEF_S1_RAISE_FLOOR;
    ld3^.tag := newsec^.tag;
    ld4^.typ := LINEDEF_S1_RAISE_FLOOR;
    ld4^.tag := newsec^.tag;
  end;

  // Now weapons and ammo and stuff
  if place_object_in_region(l, minx,a^.miny, maxx,a^.maxy, c, a^.weapon^.thingid, 24, 0, 0, 0, 7) = nil then
    if place_object_in_region(l, minx,a^.miny, maxx, a^.maxy, c, a^.weapon^.thingid, 1, 0, 0, 0, 7) = nil then
      announce(ERROR, 'No room for important weapon!');
  place_object_in_region(l, minx,a^.miny, maxx,a^.maxy, c, ID_SOUL, 24, 0, 0, 0, 1);
  ammo_value(a^.weapon^.thingid, haa, f0, f1, f2);
  na0 := (a^.boss_count * a^.boss^.ammo_to_kill[0]) - f0;
  na1 := (a^.boss_count * a^.boss^.ammo_to_kill[1]) - f1;
  na2 := (a^.boss_count * a^.boss^.ammo_to_kill[2]) - f2;
  ammo_value(a^.ammo^.thingid,haa, f0, f1, f2);
  while mask <> 0 do
  begin
    if place_object_in_region(l, minx, a^.miny, maxx, a^.maxy, c, a^.ammo^.thingid, 24, 0, 0, 0, mask) = nil then
      if place_object_in_region(l, minx, a^.miny, maxx, a^.maxy, c, a^.ammo^.thingid, 1, 0, 0, 0, mask) = nil then
        announce(ERROR, 'No room for important ammo!');
    na0 := na0 - f0;
    na1 := na1 - f1;
    na2 := na2 - f2;
    if na0 <= 0 then mask := mask and not $01;
    if na1 <= 0 then mask := mask and not $02;
    if na2 <= 0 then mask := mask and not $04;
  end;

  if a^.props and ARENA_NUKAGE <> 0 then // Little stub health
    place_object_in_region(l, minx, a^.miny,maxx, a^.maxy, c, ID_MEDIKIT, 16, 0, 0, 0, 7);
end;

// Make some decorations, ponds, cover, and so on in
// the arena.
procedure arena_decor(const l: level_p; const a: arena_p; const haa: haa_p;
  const c: config_p);
var
  // STUB; just a pillar in the center
  cx, cy, xmult, ymult, zmult: integer;
  newsec: sector_p;
  tm: texture_p;
  ld1, ld2, ld3, ld4: linedef_p;
  lamp: genus_p;
begin
  if rollpercent(25) then
  begin
    xmult := 1;
    ymult := 1;
    zmult := 1;
  end
  else
  begin
    xmult := ((a^.maxx - a^.minx) div 3 - 128) div 128;
    xmult :=  1 + roll(xmult);
    ymult := ((a^.maxy - a^.miny) - 128) div 128;
    ymult :=  1 + roll(ymult);
    zmult := 1 + roll(3);
  end;

  if 128 * zmult > a^.innersec^.ceiling_height - a^.innersec^.floor_height then
    zmult := 1;

  newsec := clone_sector(l, a^.innersec);
  newsec^.floor_height := a^.innersec^.floor_height + zmult * 128;
  cx := (a^.minx + a^.maxx ) div 2 - 64 * xmult;
  cy := (a^.miny + a^.maxy ) div 2 - 64 * ymult;
  if (a^.innersec^.style^.plaque^.props and VTILES <> 0) or (zmult = 1) then
    tm := a^.innersec^.style^.plaque
  else
    tm := a^.innersec^.style^.support0;

  parallel_innersec_ex(l, a^.innersec, newsec, c^.null_texture, a^.innersec^.style^.wall0,
                       tm, cx, cy, cx + 128 * xmult, cy + 128 * ymult, c,
                       @ld1, @ld2, @ld3, @ld4);
  ld1^.flags := ld1^.flags and not LOWER_UNPEGGED;
  ld2^.flags := ld2^.flags and not LOWER_UNPEGGED;
  ld3^.flags := ld3^.flags and not LOWER_UNPEGGED;
  ld4^.flags := ld4^.flags and not LOWER_UNPEGGED;
  if (a^.props and ARENA_LAMPS <> 0) and rollpercent(50) then
  begin
    lamp := a^.innersec^.style^.lamp0;
    if (a^.innersec^.ceiling_flat <> c^.sky_flat) and
       (lamp^.height < a^.innersec^.ceiling_height - newsec^.floor_height) then
      lamp := a^.innersec^.style^.shortlamp0;
    if (a^.innersec^.ceiling_flat <> c^.sky_flat) and
       (lamp^.height < a^.innersec^.ceiling_height - newsec^.floor_height) then
      lamp := nil
    else if lamp <> nil then
    begin
      new_thing(l, cx + 16, cy + 16, 0, lamp^.thingid, 7, c);
      new_thing(l, cx + 16, cy + 128 * ymult - 16, 0, lamp^.thingid, 7, c);
      new_thing(l, cx + 128 * xmult - 16, cy + 128 * ymult - 16, 0, lamp^.thingid, 7, c);
      new_thing(l, cx + 128 * xmult - 16, cy + 16, 0, lamp^.thingid, 7, c);
      if newsec^.light_level <= l^.lit_light_level then
        newsec^.light_level := newsec^.light_level + 20;
    end;
  end;
end;

// Put down the main enemy for the arena, his structures,
// and whatever ending gates and/or switches are needed.
procedure arena_boss(const l: level_p; const a: arena_p; const haa: haa_p;
  const c: config_p);
// STUB
var
  cx, cy: integer;
  tag, facing: SmallInt;
  need_switch: boolean;
  ld, ld1, ld2, ld3, ld4: linedef_p;
  newsec: sector_p;
  tm: texture_p;
begin
  cx := a^.minx + 5 * (a^.maxx - a^.minx) div 6;
  cy := (a^.miny + a^.maxy ) div 2;
  facing := facing_along(cx, cy, a^.innersec^.entry_x, a^.innersec^.entry_y);
  new_thing(l, cx, cy, facing, a^.boss^.thingid, 7, c);
  if a^.boss_count > 1 then // Only 1 and 2 supported!
    new_thing(l, cx, cy - (a^.boss^.width + 8), facing, a^.boss^.thingid, 7, c);

  need_switch := TRUE;
  if (c^.episode = 2) and (c^.mission = 8) then need_switch := FALSE;
  if (c^.episode = 3) and (c^.mission = 8) then need_switch := FALSE;
  if ((c^.episode = 4) and (c^.mission = 8)) or (c^.map = 7) then
  begin
    need_switch := FALSE;
    cx := cx - 32;
    cx := cx and not 63;
    cy := cy + a^.boss^.width + 72;
    cy := cy and not 63;
    newsec := new_sector(l, a^.innersec^.floor_height + 64, a^.innersec^.ceiling_height,
                         random_gate(c, a^.innersec^.style), a^.innersec^.ceiling_flat);
    newsec^.style := a^.innersec^.style;
    newsec^.light_level := 250;
    newsec^.special := GLOW_BLINK;
    newsec^.tag := 666;
    parallel_innersec_ex(l, a^.innersec, newsec,
                         nil, nil, a^.innersec^.style^.wall0,
                         cx, cy, cx + 64, cy + 64, c,
                         @ld1, @ld2, @ld3, @ld4);
    ld1^.typ := LINEDEF_W1_END_LEVEL;
    ld1^.flags := ld1^.flags and not LOWER_UNPEGGED;
    ld2^.typ := LINEDEF_W1_END_LEVEL;
    ld2^.flags := ld2^.flags and not LOWER_UNPEGGED;
    ld3^.typ := LINEDEF_W1_END_LEVEL;
    ld3^.flags := ld3^.flags and not LOWER_UNPEGGED;
    ld4^.typ := LINEDEF_W1_END_LEVEL;
    ld4^.flags := ld4^.flags and not LOWER_UNPEGGED;
  end;

  if (c^.episode = 1) and (c^.mission = 8) then
  begin
    tag := death_room(l, nil, a^.innersec^.style, c);
    if tag <> 0 then
    begin
      need_switch := FALSE;
      cx := cx - 32;
      cx := cx and not 63;
      cy := cy + a^.boss^.width + 72;
      cy := cy and not 63;
      newsec := new_sector(l, a^.innersec^.floor_height + 64,
                           a^.innersec^.ceiling_height,
                           random_gate(c, a^.innersec^.style),
                           a^.innersec^.ceiling_flat);
      newsec^.style := a^.innersec^.style;
      newsec^.light_level := 250;
      newsec^.special := GLOW_BLINK;
      newsec^.tag := 666;
      parallel_innersec_ex(l, a^.innersec,newsec,
                           nil, nil, a^.innersec^.style^.wall0,
                           cx, cy, cx + 64, cy + 64, c,
                           @ld1, @ld2, @ld3, @ld4);
      ld1^.typ := LINEDEF_TELEPORT;
      ld1^.tag := tag;
      ld1^.flags := ld1^.flags and not LOWER_UNPEGGED;
      ld2^.typ := LINEDEF_TELEPORT;
      ld2^.tag := tag;
      ld2^.flags := ld2^.flags and not LOWER_UNPEGGED;
      ld3^.typ := LINEDEF_TELEPORT;
      ld3^.tag := tag;
      ld3^.flags := ld3^.flags and not LOWER_UNPEGGED;
      ld4^.typ := LINEDEF_TELEPORT;
      ld4^.tag := tag;
      ld4^.flags := ld4^.flags and not LOWER_UNPEGGED;
    end;
  end;

  if need_switch then
  begin
    cx := cx - 64;
    cy := cy + a^.boss^.width + 8;
    parallel_innersec_ex(l, a^.innersec, nil,
                         a^.innersec^.style^.wall0, nil, nil,
                         cx, cy, cx + 128, cy + 128, c,
                         nil, nil, @ld, nil);
    // This next line is a painful hack to get the switch recessed;
    // install_switch uses empty_left_side(), which doesn't grok
    // nested enclosing sectors, sigh!
    a^.outersec^.marked := TRUE;
    ld := install_switch(l, ld, TRUE, FALSE, 0, a^.innersec^.style, c, nil);
    a^.outersec^.marked := FALSE;
    ld^.typ := LINEDEF_S1_END_LEVEL;
    tm := random_texture0(EXITSWITCH, c, a^.innersec^.style);
    if tm <> nil then
    begin
      ld^.right^.middle_texture := tm;
      ld^.right^.y_offset := tm^.y_bias;
    end;
    if ld^.right^.sector <> a^.innersec then
      ld^.right^.sector^.ceiling_flat := a^.innersec^.style^.ceiling0;
  end;
end;

// Gate out to a big place to fight bosses.
// NOTE: this renders the haa invalid at the moment, and so can only
// be used in an Episode 8, or the end of a PWAD.
procedure arena_gate(const l: level_p;const s: sector_p;const haa: haa_p;
  const c: config_p);
var
  ThisArena: arena_p;
  newseed: LongInt;
begin
  ThisArena := new_arena(l, c);
  newseed := bigrand();
  if s^.gate <> nil then
    announce(WARNING, 'Stacked gates?');

  // Put in an exit-style outgoing gate
  s^.gate := new_gate(l, 0, new_tag(l), 0, FALSE, c);
  ThisArena^.fromtag := s^.gate^.out_tag;
  install_gate(l, s, s^.style, haa, FALSE, c); // Don't want EXIT style, eh?
  s^.middle_enhanced := TRUE;

  // Now put down some powerups and stuff in s...
  prepare_arena_gate(l, s, ThisArena, haa, c);

  // Make the arena...
  install_arena(l, ThisArena, s, haa, c);

  // ... put in some decor ...
  sl_seed := newseed;
  arena_decor(l, ThisArena, haa, c);

  // ... put in the player arrival zone ...
  sl_seed := newseed;
  arena_arrival(l, ThisArena, haa, c);

  // ... and the boss and exit areas.
  sl_seed := newseed;
  arena_boss(l, ThisArena, haa, c);

  // and we're done
  announce(VERBOSE, 'Arena');
end;

// A room that has a big well in the center that eventually rises
// while you fight the awful monsters with the big teeth.
function rising_room(const l: level_p; const s: sector_p; const c: config_p;
  const haa: haa_p; const ThisQuest: quest_p): boolean;
var
  minx, miny, maxx, maxy: integer;
  xborder, yborder, depth: integer;
  newsec: sector_p;
  did_trigger: boolean;
  ld1, ld2, ld3, ld4: linedef_p;
  t: thing_p;
  tid: SmallInt;
begin
  if s^.gate <> nil then
  begin
    result := FALSE;
    exit;
  end;

  did_trigger := FALSE;
  if rollpercent(50) then
    tid := ID_POTION
  else
    tid := ID_HELMET;

  // Make sure nice and huge
  find_sector_rectangle(l, s, minx, miny, maxx, maxy);
  if maxx - minx < 320 then
  begin
    result := FALSE;
    exit;
  end;
  if maxy - miny < 320 then
  begin
    result := FALSE;
    exit;
  end;

  xborder := (64 + roll((maxx - minx) - 320)) div 2;
  yborder := (64 + roll((maxy - miny) - 320)) div 2;
  case roll(3) of
    0: depth := 256;
    1: depth := 256 + 32 * roll(33);
  else
    depth := 256 + 32 * roll(13);
  end;
  newsec := clone_sector(l, s);
  newsec^.floor_height := newsec^.floor_height - depth;
  if newsec^.light_level > 160 then
    newsec^.light_level := 160;
  newsec^.tag := new_tag(l);
  parallel_innersec_ex(l, s, newsec, nil, s^.style^.wall0, s^.style^.support0,
                       minx + xborder, miny + yborder, maxx - xborder, maxy - yborder,
                       c, @ld1, @ld2, @ld3, @ld4);
  s^.middle_enhanced := TRUE;

  // Point the right sides into the well, for pushing and find_sector_rectangle
  flip_linedef(ld1);
  flip_linedef(ld2);
  flip_linedef(ld3);
  flip_linedef(ld4);

  if ThisQuest^.goal = KEY_GOAL then
  begin
    t := new_thing(l,(minx + maxx) div 2, (miny + maxy) div 2, 0, ThisQuest^.typ, 7, c);
    ThisQuest^.thing := t; // For later
    if ThisQuest^.auxtag = 0 then
      if c^.gamemask and DOOM0_BIT = 0 then
        if rollpercent(80) then
        begin
          did_trigger := TRUE;
          trigger_box(l, t, newsec,newsec^.tag, LINEDEF_W1_RAISE_FLOOR, c);
          announce(VERBOSE, 'Zlooty');
        end;
  end;

  if ThisQuest^.goal = NULL_GOAL then
  begin
    ThisQuest^.thing := place_required_small_pickable(l, newsec, c);
    if ThisQuest^.auxtag = 0 then
      if c^.gamemask and DOOM0_BIT = 0 then
        if rollpercent(50) then
        begin
          t := new_thing(l, (minx + maxx) div 2, (miny + maxy) div 2, 0, tid, 7, c);
          did_trigger := TRUE;
          trigger_box(l, t, newsec,newsec^.tag, LINEDEF_W1_RAISE_FLOOR, c);
        end;
  end;

  if not did_trigger then
  begin
    ld1^.typ := LINEDEF_S1_RAISE_FLOOR;
    ld1^.tag := newsec^.tag;
    ld2^.typ := LINEDEF_S1_RAISE_FLOOR;
    ld2^.tag := newsec^.tag;
    ld3^.typ := LINEDEF_S1_RAISE_FLOOR;
    ld3^.tag := newsec^.tag;
    ld4^.typ := LINEDEF_S1_RAISE_FLOOR;
    ld4^.tag := newsec^.tag;
  end;

  // Put a couple of things on the ledge
  if rollpercent(30) then
    place_timely_something(l, haa, c, minx + 16, miny + 16 + roll((maxy - miny) - 31));
  if rollpercent(30) then
    place_timely_something(l, haa, c, maxx - 16, miny + 16 + roll((maxy - miny) - 31));
  if rollpercent(30) then
    place_timely_something(l, haa, c, minx + 16 + roll((maxx - minx) - 31), miny + 16);
  if rollpercent(30) then
    place_timely_something(l, haa, c, minx + 16 + roll((maxx - minx) - 31), maxy - 16);

  // Now populate the well
  populate(l, newsec, c, haa, FALSE);

  if rollpercent(20) then
  begin
    newsec^.floor_flat := c^.water_flat; // Eli's idea
    announce(VERBOSE, 'Water');
  end;

  announce(VERBOSE, 'Rising room');

  result := TRUE;
end;

// Put whatever's required by this quest into this sector
procedure close_quest(const l: level_p; const s: sector_p; const q: quest_p;
  const haa: haa_p; const c: config_p);
var
  ld: linedef_p;
  i, j: integer;
  t: thing_p;
  tm: texture_p;
  done: boolean;
begin
  done := FALSE;

  s^.has_key := TRUE;

  case q^.goal of
    SWITCH_GOAL:
      begin
        // Could decide to use a walkthrough linedef or whatever
        // instead of a switch for a tag goal.
        i := mark_decent_boundary_linedefs(l, s, 64);
        ld := random_marked_linedef(l, i);
        unmark_linedefs(l);
        if ld = nil then
          announce(ERROR, 'No applicable linedef to put switch on!')
        else
        begin
          ld := install_switch(l, ld, c^.recess_switches, FALSE, 0, s^.style, c, nil);
          ld^.typ := q^.typ;
          ld^.tag := q^.tag;
        end;
        // Maybe a potion or whatever for surprises.  Always??
        t := place_required_small_pickable(l, s, c);
        q^.thing := t; // For later
      end;
    LEVEL_END_GOAL:
      begin
        // Just alter some non-tiny boundary linedef to be a switch,
        // and try to make it obvious via some light stuff.  Or,
        // sometimes, do the floor-hole thing, or a gate, or...
        i := mark_decent_boundary_linedefs(l, s, 64);
        for j := 0 to 4 do // Try a little harder to get recessible!
        begin
          ld := random_marked_linedef(l, i);
          if empty_left_side(l, ld, 16) then break;
        end;
        unmark_linedefs(l);
        if ld = nil then
          announce(ERROR, 'No applicable linedef to end level on!')
        else
        begin
          if (c^.episode = 1) and (c^.mission = 8) then // Try a fun thing!
            if e1m8_gate(l, ld, s, haa, c) then
            begin
              announce(VERBOSE, 'e1m8 finale');
              exit;
            end;

          if rollpercent(c^.p_hole_ends_level) then // Try a floor-hole
          begin
            if empty_left_side(l, ld, 128) then // 128?  Hugeness?
            begin
              if linelen(ld) > 192 then
                split_linedef(l, ld, 128, c);
              lefthand_box(l, ld, 128, s^.style, c)^.right^.middle_texture := s^.style^.wall0;
              ld^.typ := LINEDEF_W1_END_LEVEL;
              if ld^.left^.sector^.light_level < 160 then
                ld^.left^.sector^.light_level := 160;
              ld^.left^.sector^.floor_flat := c^.sky_flat;
              ld^.left^.sector^.floor_height := ld^.left^.sector^.floor_height - 16;
              announce(VERBOSE, 'Hole ends level');
              done := TRUE;
            end;
          end;

          if not done and (s^.gate = nil) and rollpercent(c^.p_gate_ends_level) then
          begin
            // Do an exit gate
            s^.gate := new_gate(l, 0, 0, 0, TRUE, c);
            install_gate(l, s, s^.style, haa, FALSE, c);
            gate_populate(l, s, haa, FALSE, c); // Some stuff
            s^.middle_enhanced := TRUE;
            if s^.light_level > 130 then
              s^.light_level := 130; // To see "EXIT"
            announce(VERBOSE, 'Gate ends level');
            done := TRUE;
          end;

          // Switch with recess, sometimes fancied-up
          if not done then
          begin
            ld := install_switch(l, ld, TRUE, rollpercent(10), 0, s^.style, c, nil);
            ld^.typ := q^.typ;
            ld^.tag := q^.tag; // Will be zero, actually
            ld^.right^.sector^.special := GLOW_BLINK;
            if s^.light_level > 190 then
              s^.light_level := 190; // So the glow shows
            ld^.right^.sector^.light_level := 255;
            done := TRUE;
            tm := random_texture0(EXITSWITCH, c, s^.style);
            if tm <> nil then
            begin
              ld^.right^.middle_texture := tm;
              ld^.right^.y_offset := tm^.y_bias;
              announce(VERBOSE, 'Custom exit switch');
            end;
          end;
          if need_secret_level(c) and not l^.sl_done and (l^.sl_tag = 0) then
          // This sets sl_done if it works
            install_sl_exit(l, s, haa, s^.style, q, TRUE, c);
        end;
      end;
    ARENA_GOAL:
      // A teleporter to a big arena in which to fight bosses
      arena_gate(l, s, haa, c);
    GATE_GOAL:
      // A teleporter to, and perhaps from, the goal room.
      s^.gate := new_gate(l, q^.tag2, q^.tag, 0, FALSE, c);
    KEY_GOAL:
      begin
        if rollpercent(l^.p_rising_room) then
          done := rising_room(l, s, c, haa, q);
        if not done then // Simple case
        begin
          t := place_required_pickable(l, s, c, q^.typ);
          q^.thing := t; // For later
        end;
      end;
    NULL_GOAL:
      begin
        if rollpercent(2 * l^.p_rising_room) then
          done := rising_room(l, s, c, haa, q);
        if not done then // Simple case; potion or whatever for surprises
        begin
          t := place_required_small_pickable(l, s, c);
          q^.thing := t; // For later
        end;
      end;
    else
      announce(ERROR, 'Unfamiliar goal type; quest not ended.');
  end;
end;

// Consider "pushing" the current quest, which really means
// putting its goal right here, but guarding it with something
// that still has to be quested for.
procedure maybe_push_quest(const l: level_p; const s: sector_p; const ThisQuest: quest_p;
  const c: config_p);
var
  newkey: SmallInt;
  ld: linedef_p;
  locked_linedef_type: SmallInt;
  i: integer;
begin
  if not rollpercent(c^.p_pushquest) then exit; // Do we want to?
  if ThisQuest^.goal <> SWITCH_GOAL then exit; // Do we know how?

  new_key(l, newkey); // Get an unused key
  if newkey = 0 then exit;

  // Figure out how to lock it
  locked_linedef_type := locked_linedef_for(ThisQuest^.typ, newkey, c);
  if locked_linedef_type = 0 then exit;

  // Find a linedef to install the switch on
  i := mark_decent_boundary_linedefs(l, s, 64);
  ld := random_marked_linedef(l, i);
  unmark_linedefs(l);
  if ld = nil then exit;

  // Install the switch and hook it up
  ld := install_switch(l, ld, TRUE, rollpercent(50), newkey, s^.style, c, nil);
  ld^.typ := locked_linedef_type;
  ld^.tag := ThisQuest^.tag;

  // Now modify the quest
  ThisQuest^.goal := KEY_GOAL;
  ThisQuest^.typ := newkey;
  ThisQuest^.tag := 0;

  // and we're done...
  announce(LOG, 'Quest push');
end;

// Construct a linedef suitable for a generate_room_outline for
// the next room.  If old is not nil, re-use it, just changing
// its to and from.
// For most links this will be an antiparallel linedef on the left
// side of this one.
function make_linkto(const l: level_p; const ldf: linedef_p; const ThisLink: link_p;
                     const ThisStyle: style_p; const c: config_p; const old: linedef_p): linedef_p;
var
  depth: integer;
  ldnew: linedef_p;
  v, v1: vertex_p;
  newsize: integer;
  minx: integer;
begin
  case ThisLink^.typ of
    BASIC_LINK:
      begin
        depth := 0;
        // Account for any recesses
        if ThisLink^.bits and LINK_RECESS <> 0 then
          depth := depth + 2 * ThisLink^.depth2;
        // Account for single door/arch, if any
        if ThisLink^.bits and (LINK_CORE or LINK_ALCOVE) = 0 then
          depth := depth + ThisLink^.depth1;
        // Account for double doors around the core, if any
        if (ThisLink^.bits and LINK_CORE <> 0) and (ThisLink^.bits and LINK_NEAR_DOOR <> 0) then
          depth := depth + ThisLink^.depth1;
        if (ThisLink^.bits and LINK_CORE <> 0) and (ThisLink^.bits and LINK_FAR_DOOR <> 0) then
          depth := depth + ThisLink^.depth1;
        // Alcove width
        if ThisLink^.bits and LINK_ALCOVE <> 0 then
          depth := depth + ThisLink^.width2
        else // Straight-through core
          if ThisLink^.bits and LINK_CORE <> 0 then
            depth := depth + ThisLink^.depth3;
      end;
  OPEN_LINK:
    depth := ThisLink^.depth1;
  GATE_LINK:
    begin
      minx := HUGE_NUMBER;
      v := l^.vertex_anchor;
      while v <> nil do
      begin
        if v^.x < minx then minx := v^.x;
        v := v^.next;
      end;
      minx := minx - 64;
      if ldf <> nil then
        newsize := linelen(ldf)
      else
        newsize := 512;
      if newsize < 256 * l^.hugeness then
        newsize := 256 * l^.hugeness;
      if old <> nil then
      begin
        old^.from^.x := minx;
        old^._to^.x := minx;
        old^.from^.y := newsize div 2;
        old^._to^.y := -(newsize div 2);
        ldnew := old;
      end
      else
      begin
        v := new_vertex(l, minx, newsize div 2);
        v1 := new_vertex(l, minx, -(newsize div 2));
        ldnew := new_linedef(l, v, v1);
      end;
      result := ldnew;
      exit;
    end;
  else
    begin
      announce(ERROR, 'Funny linktype in make_linkto.');
      depth := ThisLink^.depth1;
    end;
  end;
  result := flip_linedef(make_parallel(l, ldf, depth, old));
end;

// Given two antiparallel linedefs, does there seem to be an empty
// rectangle between their left sides, or whatever else this link
// needs?
function link_fitsv(const l: level_p; const ldf1, ldf2: linedef_p; const ThisLink: link_p): boolean;
begin
  if ThisLink^.typ = GATE_LINK then
  begin
    result := TRUE; // These don't care
    exit;
  end;

  ldf1^.from^.marked := TRUE;
  ldf1^._to^.marked := TRUE;
  ldf2^.from^.marked := TRUE;
  ldf2^._to^.marked := TRUE;
  if ldf1^.right <> nil then
    ldf1^.right^.sector^.marked := TRUE;
  if ldf2^.right <> nil then
    ldf2^.right^.sector^.marked := TRUE;
  result := empty_rectangle(l, ldf1^.from^.x, ldf1^.from^.y,
                               ldf1^._to^.x, ldf1^._to^.y,
                               ldf2^.from^.x, ldf2^.from^.y,
                               ldf2^._to^.x, ldf2^._to^.y);
  if ldf1^.right <> nil then
    ldf1^.right^.sector^.marked := FALSE;
  if ldf2^.right <> nil then
    ldf2^.right^.sector^.marked := FALSE;
  ldf1^.from^.marked := FALSE;
  ldf1^._to^.marked := FALSE;
  ldf2^.from^.marked := FALSE;
  ldf2^._to^.marked := FALSE;
end;

procedure mid_tile(const l: level_p; const s: sector_p; out tlx, tly, thx, thy: SmallInt);
var
  minx, miny, maxx, maxy: integer;
begin
  find_sector_rectangle(l, s, minx, miny, maxx, maxy);
  tlx := (minx + maxx) div 2;
  tlx := tlx and $FFC0; // Round down to 64; down?
  if tlx <= minx then tlx := minx + 1;
  tly := (miny + maxy) div 2;
  tly := tly and $FFC0; // Round down to 64; down?
  if tly <= miny then tly := miny + 1;
  thx := tlx + 64;
  if thx >= maxx then thx := maxx - 1;
  thy := tly + 64;
  if thy >= maxy then thy := maxy - 1;
end;

// Is it OK to obstruct the middle (as defined by mid_tile()) tile
// of this sector?  i.e. might it block a way or a door?
function ok_to_block_mid_tile(const l: level_p; const s: sector_p): boolean;
var
  tlx, tly, thx, thy: SmallInt;
  minx, miny, maxx, maxy: integer;
begin
  find_sector_rectangle(l, s, minx, miny, maxx, maxy);
  mid_tile(l,s, tlx, tly, thx, thy);
  // Very strong rectangle assumptions here!
  result := FALSE;
  if tlx - minx < 33 then exit;
  if tly - miny < 33 then exit;
  if maxx - thx < 33 then exit;
  if maxy - thy < 33 then exit;
  result := TRUE;
end;

// Given a bare linedef, make a room extending from its right side.
function generate_room_outline(const l: level_p; const ld: linedef_p; const ThisStyle: style_p;
  const try_reduction: boolean; const c: config_p): sector_p;
var
  newld: linedef_p;
  v1, v2: vertex_p;
  x1, y1, x2, y2, len1, len2: integer;
begin
  // Very simple squarish rooms
  len1 := linelen(ld);
  if roll(2) > 0 then
    len2 := len1
  else
    len2 := len1 + l^.hugeness * 64 * (4 - roll(9));

  if len2 < 128 then
    len2 := 128
  else if len2 > 1600 then
    len2 := 1600;

  // Bigify.
  if not try_reduction then // Not if we're constrained
    if rollpercent(c^.p_bigify) then
      if len2 < 512 then
        len2 := len2 * 2; // Keep 'em big!

  while true do // Until we find one that fits
  begin
    point_from(ld^.from^.x, ld^.from^.y, ld^._to^.x, ld^._to^.y,
               RIGHT_TURN, len2, @x1, @y1);
    point_from(ld^._to^.x, ld^._to^.y, x1, y1, RIGHT_TURN, len1, @x2, @y2);
    ld^.from^.marked := TRUE;
    ld^._to^.marked := TRUE;
    if empty_rectangle(l, ld^.from^.x, ld^.from^.y, ld^._to^.x, ld^._to^.y,
                       x1, y1, x2, y2) then
      break;

    if not try_reduction then
    begin
      result := nil;
      exit;
    end;
    len2 := len2 - 32; // If failed, try again with smaller room
    if len2 < l^.hugeness * 64 then
    begin
      announce(VERBOSE, 'No possible rectangle fits in the space.');
      ld^._to^.marked := FALSE;
      ld^.from^.marked := FALSE;
      result := nil;
      exit;
    end;
  end;
  ld^._to^.marked := FALSE;
  ld^.from^.marked := FALSE;

  printf('.');
  announce(VERBOSE, Format('New room, corners (%d %d) (%d %d) (%d %d) (%d %d).',
    [ld^.from^.x, ld^.from^.y, ld^._to^.x, ld^._to^.y, x1, y1, x2, y2]));

  result := new_sector(l, 0, ThisStyle^.wallheight0,
                       ThisStyle^.floor0, ThisStyle^.ceiling0);
  result^.style := ThisStyle;
  result^.light_level := ThisStyle^.roomlight0; // paint_room can override?
  ld^.right := new_sidedef(l, result, c);

  v1 := ld^._to;
  v2 := new_vertex(l, x1, y1);
  newld := new_linedef(l, v1, v2);
  newld^.right := new_sidedef(l, result, c);
  // If the wall is long, sometimes split it, for more outlinks
  // Should use styles here and stuff too (and config)
  if linelen(newld) > l^.hugeness * 256 then
    if rollpercent(25) then
      split_linedef(l, newld, linelen(newld) div 2, c);

  v1 := v2;
  v2 := new_vertex(l, x2, y2);
  newld := new_linedef(l, v1, v2);
  newld^.right := new_sidedef(l, result, c);
  if linelen(newld) > l^.hugeness * 256 then
    if rollpercent(25) then
      split_linedef(l, newld, linelen(newld) div 2, c);

  v1 := v2;
  v2 := ld^.from;
  newld := new_linedef(l, v1, v2);
  newld^.right := new_sidedef(l, result, c);
  if linelen(newld) > l^.hugeness * 256 then
    if rollpercent(25) then
      split_linedef(l, newld, linelen(newld) div 2, c);
end;

// Return a random link that will fit on this linedef,
// and that can be locked for this quest (if any).
// Note that ld can be nil, meaning "don't worry about it"
function random_link(const l: level_p; const ld: linedef_p; const ThisStyle: style_p;
  const ThisQuest: quest_p; const c: config_p): link_p;
var
  open_ok: boolean;
begin
  open_ok := TRUE;
  result := nil;

  if ld <> nil then
    if linelen(ld) < 100 then
      open_ok := FALSE;
  if ThisQuest <> nil then
    if ThisQuest^.goal = KEY_GOAL then
      open_ok := FALSE;

  if l^.use_gates then
    if ThisQuest <> nil then
      if rollpercent(20) then // Should vary
        if ThisQuest^.goal = SWITCH_GOAL then
          if ld^.right^.sector^.gate = nil then
            if ld^.right^.sector <> l^.first_room then
              if ok_to_block_mid_tile(l, ld^.right^.sector) then
                if not c^.do_dm then
                begin
                  result := gate_link_f(l, c); // Already in link_anchor
                  exit;
                end;

  if result = nil then
  begin
    if rollpercent(l^.p_open_link) and open_ok then
      result := random_open_link(l, ld, ThisStyle, ThisQuest, c)
    else
      result := random_basic_link(l, ld, ThisStyle, ThisQuest, c);
  end;

  result^.next := l^.link_anchor;
  l^.link_anchor := result;
end;

// Return a random open link that will fit on this linedef
// Note that ld can be nil, meaning "don't worry about it"
function random_open_link(const l: level_p; const ld: linedef_p; const ThisStyle: style_p;
  const ThisQuest: quest_p; const c: config_p): link_p;
var
  dieroll, len: integer;
begin
  result := link_p(SL_Malloc(SizeOf(link_t)));

  result^.bits := 0;
  result^.typ := OPEN_LINK;

  if (ThisQuest = nil) and rollpercent(40) then // 40 should vary
    result^.bits := result^.bits or LINK_LIFT
  else
  begin
    result^.bits := result^.bits or LINK_STEPS;
    if rollpercent(30) then
      result^.bits := result^.bits or LINK_ALCOVE; // sidesteps
    if rollpercent(50) then
      result^.bits := result^.bits or LINK_LEFT;
  end;

  if ld <> nil then
    len := linelen(ld)
  else
    len := 0;

  // Primary width; need more variety!
  dieroll := roll(100);
  if dieroll < 35 then
    result^.width1 := 64 * l^.hugeness
  else if dieroll < 70 then
    result^.width1 := 128 * l^.hugeness
  else
    result^.width1 := 0; // Means "about a third of the wall"

  if ld <> nil then
    if result^.width1 + 66 > len then
      result^.width1 := 0;

  // Primary depth
  if result^.bits and LINK_LIFT <> 0 then
    result^.depth1 := l^.hugeness * 32 * (1 + roll(5))
  else
  begin
    if (result^.bits and LINK_ALCOVE <> 0) and rollpercent(50) then
    begin
      result^.depth1 := l^.hugeness * 32 * (1 + roll(4));
      announce(VERBOSE, 'Narrow side-steps?');
    end
    else
      result^.depth1 := l^.hugeness * 64 * (2 + roll(5)); // Or something
  end;
  if result^.depth1 < 33 then
    result^.depth1 := 33;

  // Suggested height from new floor to existing ceiling
  result^.height1 := l^.hugeness * 16 * (2 + roll(7));

end;

// Return a random basic link that will fit on this linedef
// Note that ld can be nil, meaning "don't worry about it"
// This routine has grown like kudzu, and needs to be
// heavily pruned and organized and fixed.
function random_basic_link(const l: level_p; const ld: linedef_p; const ThisStyle: style_p;
  const ThisQuest: quest_p; const c: config_p): link_p;
var
  dieroll: integer;
  len: integer;
  need_door: boolean;
  nukage_core_trap: boolean;
  need: integer;
begin
  if ld <> nil then
    len := linelen(ld)
  else
    len := 0;
  need_door := FALSE;
  nukage_core_trap := FALSE;

  result := link_p(SL_Malloc(SizeOf(link_t)));

  // Should use style and config more here and there

  result^.typ := BASIC_LINK;
  result^.bits := 0;

  if ThisQuest <> nil then
  begin
    if ThisQuest^.goal = KEY_GOAL then
      need_door := TRUE;
    // So far the only tags we know of are door-opens and nukage traps
    if ThisQuest^.goal = SWITCH_GOAL then
    begin
      if rollpercent(30) or rollpercent(l^.p_force_nukage) then // Huh?
        need_door := TRUE
      else
        nukage_core_trap := TRUE;
    end;
  end;

  // Depth of the door sector, if any
  if rollpercent(50) then
    result^.depth1 := 16
  else if rollpercent(50) then
    result^.depth1 := 8
  else if rollpercent(50) then
    result^.depth1 := 32
  else
    result^.depth1 := 64; // tunneldoor...
  result^.depth1 := result^.depth1 * l^.hugeness;

  // Stairs and lifts will change this walkable default
  if rollpercent(50) then
    result^.floordelta := 0
  else
    result^.floordelta := 24 - 8 * (roll(7));

  // Primary width default
  dieroll := roll(100);
  if dieroll < 50 then
    result^.width1 := 64
  else if dieroll < 60 then
    result^.width1 := 128
  else if dieroll < 80 then
    result^.width1 := 96
  else
    result^.width1 := 0;
  result^.width1 := result^.width1 * l^.hugeness;
  if ld <> nil then
    if len < result^.width1 then
      result^.width1 := 0;
  if l^.all_wide_links then
    result^.width1 := 0;

  result^.height1 := ThisStyle^.linkheight0; // should vary some?
  case roll(3) of // alcove depth -- needs more variety
    0: result^.width2 := 64;
    1: result^.width2 := result^.width1;
  else
    result^.width2 := 64 + 8 * (roll(17));
  end;
  result^.width2 := result^.width2 * l^.hugeness;
  if result^.width2 = 0 then
    result^.width2 := 64 * l^.hugeness;
  case roll(4) of // recess depth -- also
    0: result^.depth2 := 8;
    1: result^.depth2 := 4;
    2: result^.depth2 := 16;
  else
    result^.depth2 := 20;
  end;
  if rollpercent(10) then
    result^.depth2 := result^.depth2 * 2; // Nice and deep!
  result^.depth2 := result^.depth2 * l^.hugeness;

  // In case they're needed for cores and stairs and stuff
  result^.depth3 := 32 * (1 + roll(5)) * l^.hugeness;
  result^.stepcount := 2 + roll(9);

  dieroll := roll(100);
  // <Half standard, >half whimsical, for now, for fun
  // Other numbers are also sorta high, but that gives variety
  if dieroll < 30 then // A standard thing of some kind
  begin
    dieroll := roll(100);
    if dieroll < 20 then // Nice recessed door
      result^.bits := LINK_ANY_DOOR or LINK_RECESS
    else if dieroll < 65 then // Nice arch
      result^.bits := 0
    else // Simple stairs
    begin
      result^.bits := LINK_CORE or LINK_STEPS;
      result^.depth3 := result^.depth3 * 3;
      result^.floordelta := result^.stepcount * (2 + roll(20));
    end;
  end
  else
  begin // Make something up
    result^.bits := 0;
    if roll(2) <> 0 then
      result^.bits := result^.bits or LINK_RECESS;
    if rollpercent(40) then
    begin
      if rollpercent(40) or c^.both_doors then
        result^.bits := result^.bits or LINK_ANY_DOOR
      else if rollpercent(30) then
        result^.bits := result^.bits or LINK_NEAR_DOOR
      else
        result^.bits := result^.bits or LINK_FAR_DOOR;
    end;
    if rollpercent(10) then
      result^.bits := result^.bits or LINK_BARS;
    if result^.width1 <> 0 then // Twinning a full-wall link is ugly
      if (ld = nil) or ((len div 2 - 16) > result^.width1) then
        if rollpercent(30) then
        begin
          result^.bits := result^.bits or LINK_TWIN;
          if rollpercent(60) then
            result^.bits := result^.bits or LINK_WINDOW;
        end;
    if rollpercent(30) then
      result^.bits := result^.bits or LINK_ALCOVE;
    if (ld <> nil) and ((len div 2 - 16) < result^.width1) then
      result^.bits := result^.bits and not LINK_ALCOVE;
    if  (ld <> nil) and ((len div 4 - 32) < result^.width1) and (result^.bits and LINK_TWIN <> 0) then
      result^.bits := result^.bits and not LINK_ALCOVE;
    if rollpercent(40) then
    begin
      result^.bits := result^.bits or LINK_CORE;
      if rollpercent(40) then
      begin
        result^.bits := result^.bits or LINK_STEPS;
        result^.depth3 := result^.depth3 * 3;
        result^.floordelta := result^.stepcount * (2 + roll(20));
      end
      else if (l^.lift_rho <> 0) and not need_door then
      begin
        result^.bits := result^.bits or LINK_LIFT;
        if result^.bits and LINK_ALCOVE = 0 then
          result^.bits := result^.bits and not LINK_ANY_DOOR; // not currently compatible
        if rollpercent(50) then
          result^.floordelta := 32 + 8 * roll(51) // Potentially big
        else
          result^.floordelta := 25 + 4 * roll(26); // smaller
        if result^.depth3 < 64 then
          result^.depth3 := 64;
      end;
    end;
  end;

  if l^.no_doors then
    result^.bits := result^.bits and not LINK_ANY_DOOR;

  // Make sure we have a door if we need one (to lock, etc)
  if need_door then
    result^.bits := result^.bits or LINK_NEAR_DOOR;

  // Fewer unrecessed and/or really high doors // JVAL ?
  if (result^.bits or LINK_ANY_DOOR) <> 0 then
  begin
    if rollpercent(75) then
      result^.bits := result^.bits or LINK_RECESS;
    if rollpercent(75) then
      if (result^.height1 > 72) then
        result^.height1 := 72; // Hugeness?
  end;

  // Sometimes up, sometimes down
  if roll(2) <> 0 then
    result^.floordelta := 0 - result^.floordelta;

  // More random fun stuff
  if rollpercent(l^.p_stair_lamps) then
    result^.bits := result^.bits or LINK_LAMPS;
  if rollpercent(50) then
    result^.bits := result^.bits or LINK_MAX_CEILING;
  if rollpercent(50) then
    result^.bits := result^.bits or LINK_LEFT;
  if rollpercent(75) then
    result^.bits := result^.bits or LINK_FAR_TWINS;
  if rollpercent(75) then
    result^.bits := result^.bits or LINK_TRIGGERED;
  if rollpercent(l^.p_force_sky) or rollpercent(l^.p_force_sky) or rollpercent(50) then
    result^.bits := result^.bits or LINK_DECROOM; // 50?

  // If nukage_core_trap, override much of the above!
  if nukage_core_trap then
  begin
    // A relatively simple core
    result^.bits := result^.bits and not (LINK_STEPS or LINK_ALCOVE or LINK_TWIN or LINK_LIFT);
    result^.bits := result^.bits or LINK_CORE;
    // At least 128 long
    if result^.depth3 < 128 then
      result^.depth3 := 128;
    // And going up a bit
    result^.floordelta := 4 + roll(18);
    result^.bits := result^.bits or LINK_LOCK_CORE;
  end;

  // If a gate quest, override much of the above also.  All we want
  // is a recessed archway-thing, walkable, etc, to make_window() on
  if (ThisQuest <> nil) and (ThisQuest^.goal = GATE_GOAL) then
  begin
    result^.bits := result^.bits and not (LINK_STEPS or LINK_ALCOVE or LINK_LIFT or LINK_CORE);
    result^.bits := result^.bits and not (LINK_ANY_DOOR or LINK_TRIGGERED);
    result^.bits := result^.bits or LINK_RECESS;
    if rollpercent(50) then
      result^.floordelta := 0
    else
      result^.floordelta := 24 - 8 * (roll(7));
  end;

  // Alcoves require either a door or a recess, and a non-whole width,
  // and for now at least a tiny core.
  if result^.bits and LINK_ALCOVE <> 0 then
  begin
    if LINK_ANY_DOOR <> (result^.bits and LINK_ANY_DOOR) then
    begin
      result^.bits := result^.bits or LINK_RECESS;
      if result^.depth2 < (8 * l^.hugeness) then
        result^.depth2 := 8 * l^.hugeness;
    end;
    if result^.width1 = 0 then
      result^.width1 := 64 * l^.hugeness;
    if result^.bits and LINK_CORE = 0 then
    begin
      result^.bits := result^.bits or LINK_CORE;
      result^.depth3 := 4 * l^.hugeness;
    end;
  end;

  // Some final sanity checks on stair-sector heights and stuff
  if result^.bits and LINK_STEPS <> 0 then
  begin
    // The clearance we need is 56 plus the step height times
    // the number of steps our 64ish-wide shadow is on at once
    // (plus eight more in case of doors).  Roughly!
    need := 64 + (1 + (64 div (result^.depth3 div result^.stepcount))) * abs(result^.floordelta div (result^.stepcount - 1));
    if result^.bits and LINK_ANY_DOOR <> 0 then
      need := need + 8; // Doors don't open all the way
    if result^.height1 < need then
      result^.height1 := need;
  end
  else if result^.bits and LINK_LIFT = 0 then
  begin
    if result^.height1 + result^.floordelta < 64 then
      result^.height1 := 64 - result^.floordelta;
    if result^.height1 - result^.floordelta < 64 then
      result^.height1 := result^.floordelta + 64;
  end;

  // From here on down all we do is turn off fancy bits that worry us,
  // or make sure core-depth isn't too small.

  // Make sure we're not twinning/alcoving on a too-narrow linedef
  // Although this should all be covered above already
  if ld <> nil then
  begin
    if len < 144 then
      result^.bits := result^.bits and not (LINK_TWIN or LINK_ALCOVE);
    if len < 2 * result^.width1 then
      result^.bits := result^.bits and not (LINK_TWIN or LINK_ALCOVE);
    if result^.bits and LINK_ALCOVE <> 0 then
      if not link_fitsh(ld, result, c) then // Try the Official Checker!
        result^.bits := result^.bits and not LINK_ALCOVE;
  end;

  if result^.width1 = 0 then
    result^.bits := result^.bits and not LINK_ALCOVE;

  if (result^.bits and LINK_LIFT <> 0) and (result^.bits and LINK_ALCOVE = 0) then
    result^.bits := result^.bits and not LINK_ANY_DOOR; // not currently compatible

  // Only make a window if not too much floordelta
  if result^.floordelta + 16 > ThisStyle^.sillheight + ThisStyle^.windowheight then
    result^.bits := result^.bits and not LINK_WINDOW;
  // We don't know a ceiling-delta, so guess here
  if 56 + result^.floordelta < ThisStyle^.sillheight then
    result^.bits := result^.bits and not LINK_WINDOW;

  // If two doors are too close together, they won't work.
  // Could just turn off NEAR or FAR, eh?
  if (result^.bits and LINK_ANY_DOOR <> 0) and
     (result^.bits and LINK_CORE <> 0) and
     (result^.bits and LINK_ALCOVE = 0) and
     (result^.depth3 < 24) then
    result^.depth3 := 24;

end; // end random_link()

// Make a cool recessed lightstrip in the given linedef
procedure make_lightstrip(const l: level_p; const ld: linedef_p; const ThisStyle: style_p;
  const ll: integer; const depth: integer; const spec: integer; const fh, ch: integer;
  const c: config_p);
var
  ldnew: linedef_p;
  s: sector_p;
  t: texture_p;
begin
  // should do an empty_area check here
  t := ld^.right^.middle_texture;

  ldnew := lefthand_box(l, ld, 4, ThisStyle, c);

  // Have to shorten ldnew a bit here, for tapered edges, to
  // avoid colliding with orthogonal doors and stuff, if we're
  // not gonna do a full area check.  Use rather silly shortening
  if ldnew^._to^.x > ldnew^.from^.x then
  begin
    ldnew^._to^.x := ldnew^._to^.x - 2;
    ldnew^.from^.x := ldnew^.from^.x + 2;
  end;

  if ldnew^._to^.x < ldnew^.from^.x then
  begin
    ldnew^._to^.x := ldnew^._to^.x + 2;
    ldnew^.from^.x := ldnew^.from^.x - 2;
  end;

  if ldnew^._to^.y > ldnew^.from^.y then
  begin
    ldnew^._to^.y := ldnew^._to^.y - 2;
    ldnew^.from^.y := ldnew^.from^.y + 2;
  end;

  if ldnew^._to^.y < ldnew^.from^.y then
  begin
    ldnew^._to^.y := ldnew^._to^.y + 2;
    ldnew^.from^.y := ldnew^.from^.y - 2;
  end;

  ldnew^.right^.middle_texture := ThisStyle^.walllight;

  // Sometimes use bottom of lights.
  if ThisStyle^.peg_lightstrips then
    ldnew^.flags := ldnew^.flags or LOWER_UNPEGGED;
  s := ldnew^.right^.sector;
  s^.light_level := ll;
  s^.special := spec;
  s^.floor_height := fh;
  s^.ceiling_height := ch;
  patch_upper(ld, t, c);
  patch_lower(ld, t, c);
  // That wasn't so hard!
end;

// Is there an <sdepth> empty area on the lefthand side
// of the linedef?
function empty_left_side(const l: level_p; const ld: linedef_p; const sdepth: integer): boolean;
var
  newx1, newy1, newx2, newy2: integer;
begin
  point_from(ld^.from^.x, ld^.from^.y, ld^._to^.x, ld^._to^.y,
             LEFT_TURN, sdepth, @newx1, @newy1);
  newx2 := newx1 - ld^._to^.x + ld^.from^.x;
  newy2 := newy1 - ld^._to^.y + ld^.from^.y;
  ld^.from^.marked := TRUE;
  ld^._to^.marked := TRUE;
  if ld^.right <> nil then
    ld^.right^.sector^.marked := TRUE;
  result := empty_rectangle(l, ld^.from^.x, ld^.from^.y, ld^._to^.x, ld^._to^.y,
                            newx1, newy1, newx2, newy2);
  if ld^.right <> nil then
    ld^.right^.sector^.marked := FALSE;
  ld^.from^.marked := FALSE;
  ld^._to^.marked := FALSE;
end;

// Swell the linedef outward a bit; sdepth pels to the left,
// in sno places.  sno must be 2 or 3.
// Makes boring rectangular rooms a little more interesting
// Seems to have some strange bugs
procedure swell_linedef(const l: level_p; const ld: linedef_p; const ThisStyle: style_p;
  const c: config_p; const sno, sdepth: integer);
var
  len, newx1, newy1, newx2, newy2: integer;
  ldnew1, ldnew2: linedef_p;
begin
  if not empty_left_side(l, ld, sdepth) then exit; // oh, well!

  announce(VERBOSE, Format('Swelling (%d,%d)-(%d,%d)...', [ld^.from^.x, ld^.from^.y,
    ld^._to^.x, ld^._to^.y]));

  // Now split the linedef, and jiggle the result(s)
  len := linelen(ld) div sno;
  ldnew1 := split_linedef(l, ld, len, c);
  if sno = 3 then
    ldnew2 := split_linedef(l, ldnew1, len, c);
  point_from(ld^.from^.x, ld^.from^.y, ld^._to^.x, ld^._to^.y,
             LEFT_TURN, sdepth, @newx1, @newy1);
  if sno = 3 then
    point_from(ldnew1^.from^.x, ldnew1^.from^.y, ldnew1^._to^.x, ldnew1^._to^.y,
               LEFT_TURN, sdepth, @newx2, @newy2);
  ld^._to^.x := newx1;
  ld^._to^.y := newy1;
  announce(VERBOSE, Format('Swol to (%d,%d)-(%d,%d)...', [ld^.from^.x, ld^.from^.y,
    ld^._to^.x, ld^._to^.y]));
  if sno = 3 then
  begin
    ldnew1^._to^.x := newx2;
    ldnew1^._to^.y := newy2;
    announce(VERBOSE, Format('    and (%d,%d)-(%d,%d)...', [ldnew1^.from^.x,ldnew1^.from^.y,
      ldnew1^._to^.x,ldnew1^._to^.y]));
  end;

end; // end swell_linedef

// Should these textures be aligned as if they were the same?
function coalignable(const t1, t2: texture_p): boolean;
begin
  if t1^.subtle = t2 then
  begin
    result := TRUE;
    exit;
  end;

  if t2^.subtle = t1 then
  begin
    result := TRUE;
    exit;
  end;

  result := t1 = t2;
end;

// Is there room on the given level for the given type of object
// at the given point, allowing for at the very least the given
// width?
function room_at(const l: level_p; const g: genus_p; const x, y: integer;
  const width: integer; const c: config_p): boolean;
var
  t: thing_p;
begin
  // Check for requested length
  t := l^.thing_anchor;
  while t <> nil do
  begin
    if infinity_norm(t^.x, t^.y, x, y) < width then
    begin
      result := FALSE;
      exit;
    end;
    t := t^.next;
  end;
  // If it's not pickable, make sure not stuck-together
  if g^.bits and PICKABLE <> 0 then
  begin
    t := l^.thing_anchor;
    while t <> nil do
    begin
      if t^.genus^.bits and PICKABLE <> 0 then
      begin
        // This is overly conservative; the real check should
        // be against g^.width/2 + t^.genus^.width/2, eh?
        if infinity_norm(t^.x, t^.y, x, y) < g^.width then
        begin
          result := FALSE;
          exit;
        end;
        if infinity_norm(t^.x, t^.y, x, y) < t^.genus^.width then
        begin
          result := FALSE;
          exit;
        end;
      end;
      t := t^.next;
    end;
  end;
  result := TRUE;
end;

// Try to put an object with the given thingid and width into the
// given sector.  Use the given appearance bits, and heading
// deafness.  Return the new thing, or nil if no room to be found
// If angle is -1, point it toward ax/ay.
function place_object(const l: level_p;const s: sector_p;const c: config_p;
  const thingid: SmallInt; const width: integer; const angle: integer;
  const ax, ay: integer; const bits: integer): thing_p;
var
  minx, miny, maxx, maxy: integer;
begin
  find_sector_rectangle(l,s, minx, miny, maxx, maxy);

  if maxx - minx < width then
  begin
    result := nil;
    exit;
  end;
  if maxy - miny < width then
  begin
    result := nil;
    exit;
  end;

  result := place_object_in_region(l, minx, miny, maxx, maxy, c, thingid, width, angle, ax, ay, bits);
end;

type
  deck_t = record
    x, y: integer;
    tried: boolean;
  end;

// Try to put an object with the given thingid and width into the
// given box.  Use the given appearance bits, and heading
// deafness.  Return the new thing, or nil if no room to be found
// If angle is -1, point it toward ax/ay.
function place_object_in_region(const l: level_p; const minx, miny, maxx, maxy: integer;
  const c: config_p; const thingid: SmallInt; const width: integer; const angle: integer;
  const ax, ay: integer; const bits: integer): thing_p;
// Stub assumes rectangles and stuff
var
  x, y, i, n, decksize, tangle: integer;
  g: genus_p;
  deck: array[0..15] of deck_t;
begin
  announce(NONE, Format('place_object trying to place a %04d.', [integer(thingid)]));

  g := find_genus(c, thingid);

  if g^.bits and PICKABLE = 0 then
  begin
    if maxx - minx < g^.width then
    begin
      result := nil;
      exit;
    end;
    if maxy - miny < g^.width then
    begin
      result := nil;
      exit;
    end;
  end;

  // Try the corners
  deck[0].x := minx + width div 2;
  deck[0].y := miny + width div 2;
  deck[1].x := maxx - width div 2;
  deck[1].y := maxy - width div 2;
  deck[2].x := deck[0].x;
  deck[2].y := deck[1].y;
  deck[3].x := deck[1].x;
  deck[3].y := deck[0].y;
  // And eight random spots
  for i := 4 to 11 do
  begin
    deck[i].x := minx - width div 2 + roll(maxx - minx);
    deck[i].y := miny - width div 2 + roll(maxy - miny);
  end;
  // And the center area, if there's room
  if (maxx - minx > width * 2) and (maxy - miny > width * 2) then
  begin
    x := minx + (maxx - minx) div 2;
    y := miny + (maxy - miny) div 2;
    deck[12].x := x - width div 2;
    deck[12].y := y - width div 2;
    deck[13].x := x + width div 2;
    deck[13].y := y + width div 2;
    deck[14].x := deck[12].x;
    deck[14].y := deck[13].y;
    deck[15].x := deck[13].x;
    deck[15].y := deck[12].y;
    decksize := 16;
  end
  else
    decksize := 12;

  // Now we *should* shuffle the deck and go through it in order
  // until one is OK, but shuffling is expensive, so for now
  // we'll just use probes.

  for i := 0 to decksize - 1 do
    deck[i].tried := FALSE;
  for i := 0 to 9 do
  begin
    n := roll(decksize);
    if deck[n].tried then
      continue;
    x := deck[n].x;
    y := deck[n].y;
    if room_at(l, g, x, y, width, c) then // Use first point with room we find
    begin
      if angle = -1 then
        tangle := facing_along(x, y, ax, ay)
      else
        tangle := angle;
      if not rollpercent(l^.p_rational_facing) then
        tangle := 90 * roll(4);
      result := new_thing(l, x, y, tangle, thingid, bits, c);
      announce(NONE, Format('place_object placed it at (%d,%d).', [x, y]));
      exit;
    end;
    deck[n].tried := TRUE;
  end; // end for ten probes

  announce(NONE, 'place_object failed');
  result := nil;
end;

// Maybe place some explodables.  Should this effect the haa?  Well,
// you can get hurt by an exploding one; on the other hand, you can use
// one to kill a monster and thus avoid getting hurt.  So punt.
procedure place_barrels(const l: level_p; const s: sector_p; const c: config_p;
  const haa: haa_p);
var
  i: integer;
  g: genus_p;
begin
  if not rollpercent(l^.p_barrels) then
    exit;

  g := random_barrel(c, s^.style);
  if g = nil then
    exit; // No barrels in this style!

  i := 1 + roll(5); // Sort of boring, eh?

  while i > 0 do
  begin
    if place_object(l, s, c, g^.thingid, g^.width, 0, 0, 0, 7) = nil then
      exit;
    announce(VERBOSE, 'Barrel');
    dec(i);
  end; // end forever
end; // end place_barrels

// Maybe place some plants and other lawn decorations.
procedure place_plants(const l: level_p; const allow: integer; const s: sector_p;
  const c: config_p);
var
  g: genus_p;
begin
  while true do
  begin
    g := random_plant(c, s^.style);
    if g = nil then
      exit; // No plants available!

    if rollpercent(10) then
      exit; // hmmm...

    if g^.width <= allow then
      // Next line used to have "allow',  not "g^.wdith". Why?
      if place_object(l, s, c, g^.thingid, g^.width, 0, 0, 0, 7) = nil then
        exit;

    announce(VERBOSE, 'Plant');
  end; // end forever
end; // end place_plants

// Return some random piece of armor, and a note as to which
// levels need some.
function timely_armor(const haa: haa_p; const rlevels: PInteger; const c: config_p): integer;
var
  i, levels, armortype: integer;
begin
  // See which levels need more
  levels := 0;
  for i :=0 to 2 do // for each hardness level
  begin
    levels := levels div 2;
    if haa^.haas[i].armor < c^.usualarmor[i] then
      levels := levels or $04;
  end;

  rlevels^ := levels;

  if levels = 0 then
  begin
    result := 0;
    exit;
  end;

  // Should be less primitive?
  if rollpercent(50) then
    armortype := ID_HELMET
  else if rollpercent(70) then
    armortype := ID_GREENSUIT
  else
    armortype := ID_BLUESUIT;

  result := armortype;
end;

// Update the haa in the obvious way.  Well, almost the obvious
// way.  It has to make some assumptions about how optimally
// the player will utilize a suit.  Some random parameters in here!
procedure update_haa_for_armor(const haa: haa_p; const levels: integer;
  const armortype: SmallInt);
begin
  case armortype of
    ID_HELMET:
      begin
        if levels and $01 <> 0 then
          haa^.haas[ITYTD].armor := haa^.haas[ITYTD].armor + 1;
        if levels and $02 <> 0 then
          haa^.haas[HMP].armor := haa^.haas[HMP].armor + 1;
        if levels and $04 <> 0 then
          haa^.haas[UV].armor := haa^.haas[UV].armor + 1;
      end;
    ID_GREENSUIT:
      begin
        if levels and $01 <> 0 then
        begin
          haa^.haas[ITYTD].armor := haa^.haas[ITYTD].armor + 20;
          if haa^.haas[ITYTD].armor < 100 then
            haa^.haas[ITYTD].armor := 100;
        end;
        if levels and $02 <> 0 then
        begin
          haa^.haas[HMP].armor := haa^.haas[HMP].armor + 30;
          if haa^.haas[HMP].armor < 100 then
            haa^.haas[HMP].armor := 100;
        end;
        if levels and $04 <> 0 then
        begin
          haa^.haas[UV].armor := haa^.haas[UV].armor + 50;
          if haa^.haas[UV].armor < 100 then
            haa^.haas[UV].armor := 100;
        end;
      end;
    ID_BLUESUIT:
      begin
        if levels and $01 <> 0 then
        begin
          haa^.haas[ITYTD].armor := haa^.haas[ITYTD].armor + 40;
          if haa^.haas[ITYTD].armor < 200 then
            haa^.haas[ITYTD].armor := 200;
        end;
        if levels and $02 <> 0 then
        begin
          haa^.haas[HMP].armor := haa^.haas[HMP].armor + 60;
          if haa^.haas[HMP].armor < 200 then
            haa^.haas[HMP].armor := 200;
        end;
        if levels and $04 <> 0 then
        begin
          haa^.haas[UV].armor := haa^.haas[UV].armor + 100;
          if haa^.haas[UV].armor < 200 then
            haa^.haas[UV].armor := 200;
        end;
      end;
    else
      announce(ERROR, 'Odd armortype in update_haa_for_armor');
  end;
end;

// Maybe place some armor, update the haa
procedure place_armor(const l: level_p; const s: sector_p; const c: config_p;
  const haa: haa_p);
var
  levels: integer;
  armortype: integer;
begin
  if rollpercent(10) then
    exit; // Correct?

  levels := 0;
  while true do
  begin
    announce(NONE, 'place_armor looking for needy levels');
    armortype := timely_armor(haa, @levels, c);
    if levels = 0 then
      exit; // Done if none
    announce(NONE, 'place_armor found some needy levels');
    if place_object(l, s, c, armortype, 48, 0, 0, 0, levels) = nil then
      exit;
    announce(NONE, 'place_armor placed some armor');
    update_haa_for_armor(haa, levels, armortype);
    if rollpercent(25) then
      exit; // Reasonable?
  end; // end forever */
end;

// Return some useful kind of ammo or weapon, and what levels
// it ought to be given to.
function timely_ammo(const haa: haa_p; const rlevels: PInteger;
  const c: config_p): integer;
var
  levels, i, ammotype: integer;
  need_shotgun, need_plasgun, need_launcher: boolean;
  weapcount: integer;
begin
  levels := 0;
  ammotype := 0;
  need_shotgun := FALSE;
  need_plasgun := FALSE;
  need_launcher := FALSE;

  // See which levels need more
  for i := 0 to 2 do // for each hardness level
  begin
    levels := levels div 2;
    if haa^.haas[i].ammo < c^.usualammo[i] then levels := levels or $04;
    if not haa^.haas[i].can_use_shells then need_shotgun := TRUE;
    if not haa^.haas[i].can_use_cells then need_plasgun := TRUE;
    if not haa^.haas[i].can_use_rockets then need_launcher := TRUE;
  end;

  rlevels^ := levels;

  if levels = 0 then
  begin
    result := 0;
    exit;
  end;

  // it would be logical to only put down shells if
  // the player-model can use them, only put down
  // cells if... etc.  But that's a little complex,
  // since we want to put down a single ammo that will
  // be useful for all the hardness levels.  We cheat on
  // the shotgun, by always putting down one of those if
  // any level doesn't have one.   And for the plasma gun
  // and the rocket launcher, we always give to all levels,
  // if to any, if any at all didn't have one yet.

  if not c^.weapons_are_special and need_shotgun then
  begin
    if (c^.gamemask and (DOOM0_BIT or DOOM1_BIT) = 0) and rollpercent(30) then
      ammotype := ID_SSGUN
    else
      ammotype := ID_SHOTGUN;
  end
  else if not c^.weapons_are_special and rollpercent(15) then
  begin
    if c^.gamemask and (DOOM0_BIT or DOOM1_BIT) <> 0 then
      weapcount := 4
    else
      weapcount := 5;
    case roll(weapcount) of
      0:
        begin
          if c^.big_weapons then
            ammotype := ID_PLASMA
          else
            ammotype := ID_SHOTGUN;
        end;
      1: ammotype := ID_SHOTGUN;
      2: ammotype := ID_CHAINGUN;
      3:
        begin
          if c^.big_weapons then
            ammotype := ID_LAUNCHER
          else
            ammotype := ID_SHOTGUN;
        end;
      4: ammotype := ID_SSGUN;
    end;
  end
  else if rollpercent(10) then
    ammotype := ID_CLIP
  else if haa^.haas[0].can_use_cells and rollpercent(10) then
    ammotype := ID_CELL
  else if haa^.haas[0].can_use_cells and rollpercent(15) then
    ammotype := ID_CELLPACK
  else if haa^.haas[0].can_use_rockets and rollpercent(12) then
    ammotype := ID_ROCKET
  else if haa^.haas[0].can_use_rockets and rollpercent(15) then
    ammotype := ID_ROCKBOX
  else if rollpercent(10) then
    ammotype := ID_BULBOX
  else if rollpercent(60) then
    ammotype := ID_SHELLS
  else
    ammotype := ID_SHELLBOX;

  if (ammotype = ID_PLASMA) and need_plasgun then
    levels := levels or $07; // All, if any

  if (ammotype = ID_LAUNCHER) and need_launcher then
    levels := levels or $07; // All, if any

  rlevels^ := levels; // In case we just changed them
  result := ammotype;
end;

// How much is that ammo in the window?   Three numbers, one for each
// skill level (since value can vary with what weapons you have!)
procedure ammo_value(const ammotype: SmallInt; const haa: haa_p; out a0, a1, a2: integer);
var
  answer: integer;
  special_case: boolean;
begin
  special_case := FALSE;

  // These numbers should just be stored in the config, in the genus
  case ammotype of
    ID_SSGUN,
    ID_SHOTGUN:
      begin
        answer := 560;
        special_case := TRUE;
      end;
    ID_SHELLS:
      begin
        answer := 280;
        special_case := TRUE;
      end;
    ID_SHELLBOX:
      begin
        answer := 1400;
        special_case := TRUE;
      end;
    ID_PLASMA:
        answer := 880;
    ID_BFG:
      answer := 880; // but a BFG is better, eh?
    ID_CHAINGUN:
      answer := 200;
    ID_LAUNCHER:
      answer := 200;
    ID_CLIP:
      answer := 100;
    ID_BULBOX:
      answer := 500;
    ID_CELL:
      answer := 440;
    ID_CELLPACK:
      answer := 2200;
    ID_ROCKET:
      answer := 100;
    ID_ROCKBOX:
      answer := 500;
  else
    begin
      announce(ERROR, 'Funny ammo type in ammo_value');
      answer := 0;
    end;
  end;
  a0 := answer;
  a1 := answer;
  a2 := answer;
  if special_case then // Sort of a hack!  Make more general?
  begin
    if (ammotype = ID_SSGUN) or haa^.haas[0].has_ssgun then
      a0 := round(answer * 10.0 / 7.0);
    if (ammotype = ID_SSGUN) or haa^.haas[1].has_ssgun then
      a1 := round(answer * 10.0 / 7.0);
    if (ammotype = ID_SSGUN) or haa^.haas[2].has_ssgun then
      a2 := round(answer * 10.0 / 7.0);
  end;
end;

// The obvious thing
procedure update_haa_for_ammo(const haa: haa_p; const levels: integer;
  const ammotype: SmallInt);
var
  a0, a1, a2: integer;
begin
  ammo_value(ammotype, haa, a0, a1, a2);

  if levels and $01 <> 0 then
    haa^.haas[ITYTD].ammo := haa^.haas[ITYTD].ammo + a0;
  if levels and $02 <> 0 then
    haa^.haas[HMP].ammo := haa^.haas[HMP].ammo + a1;
  if levels and $04 <> 0 then
    haa^.haas[UV].ammo := haa^.haas[UV].ammo + a2;
  if (ammotype = ID_SHOTGUN) or (ammotype = ID_SSGUN) then
  begin
    if levels and $01 <> 0 then haa^.haas[ITYTD].can_use_shells := TRUE;
    if levels and $02 <> 0 then haa^.haas[HMP].can_use_shells := TRUE;
    if levels and $04 <> 0 then haa^.haas[UV].can_use_shells := TRUE;
  end;
  if ammotype = ID_CHAINGUN then
  begin
    if levels and $01 <> 0 then haa^.haas[ITYTD].has_chaingun := TRUE;
    if levels and $02 <> 0 then haa^.haas[HMP].has_chaingun := TRUE;
    if levels and $04 <> 0 then haa^.haas[UV].has_chaingun := TRUE;
  end;
  if ammotype = ID_PLASMA then
  begin
    if levels and $01 <> 0 then haa^.haas[ITYTD].can_use_cells := TRUE;
    if levels and $02 <> 0 then haa^.haas[HMP].can_use_cells := TRUE;
    if levels and $04 <> 0 then haa^.haas[UV].can_use_cells := TRUE;
  end;
  if ammotype = ID_LAUNCHER then
  begin
    if levels and $01 <> 0 then haa^.haas[ITYTD].can_use_rockets := TRUE;
    if levels and $02 <> 0 then haa^.haas[HMP].can_use_rockets := TRUE;
    if levels and $04 <> 0 then haa^.haas[UV].can_use_rockets := TRUE;
  end;
  if ammotype = ID_SSGUN then
  begin
    if levels and $01 <> 0 then haa^.haas[ITYTD].has_ssgun := TRUE;
    if levels and $02 <> 0 then haa^.haas[HMP].has_ssgun := TRUE;
    if levels and $04 <> 0 then haa^.haas[UV].has_ssgun := TRUE;
  end;
end;

// Is this thingid a weapon?  Should use config!
function is_weapon(const thingid: integer): boolean;
begin
  case thingid of
    ID_SHOTGUN,
    ID_SSGUN,
    ID_CHAINGUN,
    ID_CHAINSAW,
    ID_PLASMA,
    ID_BFG,
    ID_LAUNCHER:
      result := TRUE;
  else
    result := FALSE;
  end;
end;

// Maybe place some ammo, update the haa
procedure place_ammo(const l: level_p; const s: sector_p; const c: config_p;
  const haa: haa_p);
var
  levels: integer;
  ammotype: SmallInt;
begin
  if c^.allow_boring_rooms and rollpercent(10) then
    exit;

  levels := 0;
  while true do
  begin
    announce(NONE, 'place_ammo looking for needy levels');
    ammotype := timely_ammo(haa, @levels, c);
    if levels = 0 then
      exit; // Done if none
    announce(NONE, 'place_ammo found some needy levels');
    // The 48 is just to avoid bunching-up and wall-illusions,
    // as well as the grab-through-wall effect.
    if place_object(l, s, c, ammotype, 48, 0, 0, 0, levels) = nil then
      exit;
    announce(NONE, 'place_ammo placed some ammo');
    if levels = 7 then
      if is_weapon(ammotype) then
        s^.has_dm_weapon := TRUE;
    update_haa_for_ammo(haa, levels, ammotype);
    if rollpercent(20) then
      exit; // Reasonable?
  end; // end forever
end;

// Update the haa in the obvious way
procedure update_haa_for_health(const haa: haa_p; const levels: integer;
  const healthtype: integer);
var
  amount: integer;
begin
  if healthtype = ID_BERSERK then
  begin
    announce(VERBOSE, 'Put in a berserk pack!');
    if levels and $01 <> 0 then
    begin
      if haa^.haas[ITYTD].health < 100 then
        haa^.haas[ITYTD].health := 100;
      haa^.haas[ITYTD].has_berserk := TRUE;
    end;
    if levels and $02 <> 0 then
    begin
      if haa^.haas[HMP].health < 100 then
        haa^.haas[HMP].health := 100;
      haa^.haas[HMP].has_berserk := TRUE;
    end;
    if levels and $04 <> 0 then
    begin
      if haa^.haas[UV].health < 100 then
        haa^.haas[UV].health := 100;
      haa^.haas[UV].has_berserk := TRUE;
    end;
  end
  else
  begin
    case healthtype of
      ID_STIMPACK: amount := 10;
      ID_MEDIKIT: amount := 25;
      ID_POTION: amount := 1;
      ID_SOUL: amount := 100;
    else
      begin
        announce(WARNING, 'Odd healthtype in update_haa_for_health');
        amount := 0;
      end;
    end;
    if levels and $01 <> 0 then
      haa^.haas[ITYTD].health := haa^.haas[ITYTD].health + amount;
    if levels and $02 <> 0 then
      haa^.haas[HMP].health := haa^.haas[HMP].health + amount;
    if levels and $04 <> 0 then
      haa^.haas[UV].health := haa^.haas[UV].health + amount;
  end;
end;

// Return a random kind of ordinary health-bonus for those levels
// that need some health.  If *levels comes back as zero, return
// value is undefined.
function timely_health(const haa: haa_p; const levels: PInteger;
  const c: config_p): SmallInt;
var
  i: integer;
  berserk_ok: boolean;
  healthtype: SmallInt;
begin
  berserk_ok := FALSE;
  // See which levels need more
  levels^ := 0;
  for i := 0 to 2 do // for each hardness level
  begin
    levels^ := levels^ div 2;
    if haa^.haas[i].health < c^.usualhealth[i] then
      levels^ := levels^ or $04;
    if not haa^.haas[i].has_berserk then
      berserk_ok := TRUE;
  end;

  if levels^ =0 then
  begin
    result := 0;
    exit;
  end;

  if rollpercent(50) then
    healthtype := ID_STIMPACK
  else if rollpercent(50) then
    healthtype := ID_MEDIKIT
  else if rollpercent(90) then
    healthtype := ID_POTION
  else if berserk_ok and rollpercent(50) then
    healthtype := ID_BERSERK
  else
    healthtype := ID_SOUL;
  result := healthtype;
end;

// Maybe place some health boni, update the haa
procedure place_health(const l: level_p; const s: sector_p; const c: config_p;
  const haa: haa_p);
var
  levels: integer;
  healthtype: SmallInt;
begin
  // Coming along.  Might want to create effects around SOULs etc, eh?
  if c^.allow_boring_rooms and rollpercent(10) then
    exit;

  levels := 0;
  while true do
  begin
    healthtype := timely_health(haa, @levels, c);
    if levels = 0 then
      exit; // Done if none
    // The 48 is just to avoid bunching-up and wall-illusions
    if place_object(l, s, c, healthtype, 48, 0, 0, 0, levels) = nil then
      exit;
    update_haa_for_health(haa, levels, healthtype);
    if rollpercent(20) then
      exit; // Reasonable?
  end; // end forever
end; // end place_health

// Probably put some random bonus that the player needs at
// the given location, and update the haa accordingly.
procedure place_timely_something(const l: level_p; const haa: haa_p;
  const c: config_p; const x, y: integer);
var
  thingtype, levels: integer;
begin
  case roll(5) of
    0:  // Armor
      begin
        thingtype := timely_armor(haa, @levels, c);
        if levels = 0 then
          exit; // Done if none
        new_thing(l, x, y, 0, thingtype, levels, c);
        update_haa_for_armor(haa, levels, thingtype);
      end;
    1, // Ammo/weapons
    2:
      begin
        thingtype := timely_ammo(haa, @levels, c);
        if levels = 0 then
          exit; // Done if none
        new_thing(l, x, y, 0, thingtype, levels, c);
        update_haa_for_ammo(haa, levels, thingtype);
      end;
    3, // Health
    4:
      begin
        thingtype := timely_health(haa, @levels, c);
        if levels = 0 then
          exit; // Done if none
        new_thing(l, x, y, 0, thingtype, levels, c);
        update_haa_for_health(haa, levels, thingtype);
      end;
  end;
end;

// Return the size of monster, and the difficulty levels, that's due
// in the current user-model (the haa).
function haa_monster_data(const haa: haa_p; const c: config_p;
  const monster_size_health, monster_size_ammo: PSingle; const levels: PInteger): boolean;
var
  excess_health: single;
  available_ammo: single;
  i: integer;
begin
  // Determine what size monster we want
  levels^ := 0;
  monster_size_health^ := 10000;
  monster_size_ammo^ := 10000;
  for i := 0 to 2 do
  begin
    levels^ := levels^ div 2; // Shift the bits over
    excess_health := haa^.haas[i].health - c^.minhealth[i];
    if excess_health > 0 then
    begin
      levels^ := levels^ or $04; // Set the bit
      // Can take more damage if armored
      if excess_health < haa^.haas[i].armor then
        excess_health := excess_health * 2
      else
        excess_health := excess_health + haa^.haas[i].armor;
      // -Will- take more if no good weapons
      if not (haa^.haas[i].can_use_shells or haa^.haas[i].can_use_cells) then
        excess_health := excess_health / 2;
      if excess_health < monster_size_health^ then
        monster_size_health^ := excess_health;
      available_ammo := haa^.haas[i].ammo;
      // If wimpy weapons, will use more ammo
      if not (haa^.haas[i].can_use_shells or haa^.haas[i].can_use_cells) then
        available_ammo := available_ammo / 2;
      if haa^.haas[i].ammo < monster_size_ammo^ then
        monster_size_ammo^:= haa^.haas[i].ammo;
    end; // end this level has excess health
  end; // end for difficulty levels determining limits
  monster_size_health^ := monster_size_health^ + 5; // A little leeway
  if levels^ = 0 then
    result := FALSE // No excess health anywhere
  else
    result := TRUE;
end;

// Find a monster that fits the given health and ammo allowance,
// for the given apearence bits.  If none, return the monster
// that's the easiest to kill.  Never return null!
function proper_monster(const health, ammo: single; const bits: integer;
  const haa: haa_p; const mno: integer; require, forbid: LongWord;
  const biggest: boolean; const c: config_p): genus_p;
var
  m, m1, m0, mx, my: genus_p;
  damage, ammo0, bx: single;
  i, count: integer;
  hl, am: single;
  thisbit: integer;
begin
  announce(NONE, Format('proper_monster looking for %f health, %f ammo, levels %d',
    [health, ammo, bits]));

  require := require or MONSTER; // Duh!
  forbid := forbid or BOSS; // No wandering bosses
  if not c^.big_monsters then
    forbid := forbid or BIG;

  // Mark eligible monsters, and find wimpiest and biggest just in case
  count := 0;
  ammo0 := 10000;
  m0 := nil;
  mx := nil;
  my := nil;
  bx := 0;
  m := c^.genus_anchor;
  while m <> nil do
  begin
    if m^.bits and require <> require then
    begin
      m := m^.next;
      continue;
    end;
    if m^.bits and forbid <> 0 then
    begin
      m := m^.next;
      continue;
    end;
{$IFDEF IMPOSSIBLE_MONSTERS_IN_CONFIG}
    if m^.gamemask and c^.gamemask <> c^.gamemask then
    begin
      m := m^.next;
      continue;
    end;
{$ENDIF}
    if m0 = nil then m0 := m;
    m^.marked := TRUE;
    i := 0;
    thisbit := 1;
    while (i < 3) and m^.marked do
    begin
      if thisbit and bits = 0 then
      begin
        inc(i);
        thisbit := thisbit * 2;
        continue;
      end;
      // If we don't have any good weapons, we'll take more
      // damage and use more ammo, so halve the limits.
      // Include chaingun here, too?
      if not (haa^.haas[i].can_use_shells or haa^.haas[i].can_use_cells) then
      begin
        hl := health / 2;
        am := ammo / 2; // This may not be reasonable / necessary
      end
      else
      begin
        hl := health;
        am := ammo;
      end;
      if mno <> 0 then
        damage := m^.damage[i]
      else
        damage := m^.altdamage[i];
      if damage>hl then
        m^.marked := FALSE;
      if m^.ammo_to_kill[i] > am then
        m^.marked := FALSE;
      if m^.ammo_to_kill[i] < ammo0 then
      begin
        m0 := m;
        ammo0 := m^.ammo_to_kill[i];
      end;
      inc(i);
      thisbit := thisbit * 2;
    end; // end for levels
    if m^.marked then
    begin
      inc(count);
      if m^.ammo_to_kill[0] + m^.damage[0] > bx then
      begin
        my := mx;
        mx := m;
        bx := m^.ammo_to_kill[0] + m^.damage[0];
      end;
    end;
    m := m^.next;
  end; // end for m over monsters

  if count = 0 then // Put down the wimpiest monster
    m := m0
  else if biggest then
  begin // Put down the biggest monster
    m := mx;
    if my <> nil then
      if rollpercent(40) then
        m := my; // Or the second-biggest
  end
  else
  begin // Choose a random one
    count := 1 + roll(count);
    m := c^.genus_anchor;
    while m <> nil do
    begin
      if m^.marked then dec(count);
      if count = 0 then break;
      m := m^.next;
    end;
  end;

  // Unmark monsters
  m1 := c^.genus_anchor;
  while m1 <> nil do
  begin
    m1^.marked := FALSE;
    m1 := m1^.next;
  end;

  result := m;
end;

// This says that the current battle is over, so any pending
// weapon-pickups can occur.  If we ever want to be kind, we
// can defer the ammo to this point as well.
procedure haa_unpend(const haa: haa_p);
var
  i: integer;
begin
  for i := ITYTD to UV do
  begin
    if haa^.haas[i].shells_pending then
    begin
      haa^.haas[i].can_use_shells := TRUE;
      haa^.haas[i].shells_pending := FALSE;
    end;
    if haa^.haas[i].chaingun_pending then
    begin
      haa^.haas[i].has_chaingun := TRUE;
      haa^.haas[i].chaingun_pending := FALSE;
    end;
  end;
end;

// This makes the model assume that, unlike in-room goodies, ammo
// from monsters is taken at once.  challenging!   Weapons, on the
// other hand, just go into the _pending bits, for haa_unpend().
procedure update_haa_for_monster(const haa: haa_p; const m: genus_p; const levels: integer;
  const mno: integer; const c: config_p);
var
  i, thisbit: integer;
  damage: single;
begin
  thisbit := 0;
  for i := 0 to 2 do
  begin
    if thisbit = 0 then
      thisbit := 1
    else
      thisbit := thisbit * 2;
    if thisbit and levels = 0 then continue;
    if mno <> 0 then
      damage := m^.damage[i]
    else
      damage := m^.altdamage[i];
    if not (haa^.haas[i].can_use_shells or haa^.haas[i].can_use_cells) then
      damage := damage * 2;
    if damage > 2 * haa^.haas[i].armor then
    begin
      haa^.haas[i].health := haa^.haas[i].health + haa^.haas[i].armor;
      haa^.haas[i].armor := 0;
      haa^.haas[i].health := haa^.haas[i].health - damage;
    end
    else
    begin
      haa^.haas[i].health := haa^.haas[i].health - (damage / 2);
      haa^.haas[i].armor := haa^.haas[i].armor - (damage / 2);
    end;
    if haa^.haas[i].health < 0 then
      announce(VERBOSE, 'Health estimate negative?');
    damage := m^.ammo_to_kill[i];
    // Takes more ammo, if no good weapons
    if not (haa^.haas[i].can_use_shells or haa^.haas[i].can_use_cells) then
      damage := damage * 2;
    // But less ammo if we can saw it or punch it!
    if haa^.haas[i].has_chainsaw and (m^.bits and (FLIES or SHOOTS) = 0) then
      damage := damage / 2
    else if haa^.haas[i].has_berserk and (m^.bits and (FLIES or SHOOTS) = 0) then
      damage := damage * 0.80;
    haa^.haas[i].ammo := haa^.haas[i].ammo - damage;
    haa^.haas[i].ammo := haa^.haas[i].ammo + m^.ammo_provides; // Should be in stage two?
    if haa^.haas[i].ammo < 0 then
      announce(VERBOSE, 'Ammo estimate negative?');
    if m^.thingid = ID_SERGEANT then
      haa^.haas[i].shells_pending := TRUE;
    if m^.thingid = ID_COMMANDO then
      haa^.haas[i].chaingun_pending := TRUE;
  end; // end for levels adjusting haa
end; // end update_haa_for_monster

// Return a monster that there's room for in the model now.
function timely_monster(const haa: haa_p; const c: config_p; const levels: PInteger;
  const biggest: boolean; const mno: integer): genus_p;
begin
  // Should just be a macro, eh?
  result := timely_monster_ex(haa, c, levels, biggest, mno, 0); // no extra reqs
end;

// Return a monster that there's room for in the model now, with
// some required bits set.
// Should really take into account the _size_ of the place you're
// planning to put the monster, eh?
function timely_monster_ex(const haa: haa_p; const c: config_p; const levels: PInteger;
  biggest: boolean; const mno: integer; const req: LongWord): genus_p;
var
  monster_size_health: single;
  monster_size_ammo: single;
begin
  // Find how big a monster we can tolerate
  if not haa_monster_data(haa, c, @monster_size_health, @monster_size_ammo, levels) then
  begin
    result := nil; // Not enough excess health in any level
    exit;
  end;

  // Find a monster of that size
  result := proper_monster(monster_size_health, monster_size_ammo, levels^, haa, mno,
                           c^.required_monster_bits + req, c^.forbidden_monster_bits,
                           biggest, c);
end;

// Maybe add some monsters, update the haa
procedure place_monsters(const l: level_p; const s: sector_p; const c: config_p;
  const haa: haa_p);
var
  mno, n: integer;
  levels: integer;
  m, lastm: genus_p;
  rc: boolean;
begin
  // Decide on a limit, if any, for the monster loop; should be config/style?
  if c^.allow_boring_rooms then
    if rollpercent(20) then
      exit; // No monsters at all

  if rollpercent(80) then
    n := 2 + roll(8) // N to M monsters
  else
    n := 1000; // As many monsters as will fit!

  n := n * l^.hugeness; // Reasonable?

  // The loop itself
  lastm := nil;
  for mno :=0 to n - 1 do
  begin
    m := timely_monster(haa, c, @levels, rollpercent(l^.p_biggest_monsters), mno);
    if m = nil then
      exit;
    if lastm <> nil then
      if rollpercent(c^.homogenize_monsters) then
        m := lastm; // Yoiks, dangerous!
    lastm := m;

    if rollpercent(15) then
      levels := levels or $08; // deaf

    announce(NONE, 'Trying to place a monster');

    // Try to place it
    rc := place_object(l, s, c, m^.thingid, m^.width, -1,
            s^.entry_x, s^.entry_y,levels) <> nil;
    if not rc then
    begin
      announce(NONE, 'Placement failed');
      haa_unpend(haa); // Might as well give up entirely
      exit;
    end;

    if m^.thingid = ID_SKULL then announce(NONE, 'Skull');
    if m^.thingid = ID_HEAD then announce(VERBOSE, 'HEAD');
    if m^.thingid = ID_SKEL then announce(VERBOSE, 'SKEL');
    if m^.thingid = ID_HELL then announce(VERBOSE, 'KNIGHT');
    if m^.thingid = ID_ARCHIE then announce(VERBOSE, 'VILE');

    update_haa_for_monster(haa, m, levels, mno, c);

  end; // end for a long time

  // NOTE: tempting as it is, we don't do this *within* the
  // loop, because the model is that the player doesn't pick
  // up the objects in the room (including dropped weapons)
  // until after everything is dead.  So we don't use the
  // new values of these data until we've placed all the
  // monsters for a room.  On the other hand, the effects
  // of the can_use_x variables are sort of down in the
  // noise, so it may not be worth *too* much effort to
  // get this exactly right...
  haa_unpend(haa);

  // all done

end; // end place_monsters


// Boy is this primitive!  On the other hand, other checks later
// mean that this can probably be somewhat optimistic.
function isAdequate(const l: level_p; const ld: linedef_p; const ThisStyle: style_p;
  const c: config_p): boolean;
begin
  // Assume all 1S longish linedefs are OK; very dangerous!
  if ld^.left <> nil then
  begin
    result := FALSE;
    exit;
  end;
  if ld^.flags and TWO_SIDED <> 0 then
  begin
    result := FALSE;
    exit;
  end;
  result := lengthsquared(ld) >= (128 * 128); // Why 128?
end;

// Fill in the default config-file data contents stuff
procedure load_default_config(const c: config_p);
begin
  c^.configdata :=
  '[THEMES] T W T M T B T R ? '#13#10 +
  't PANEL5 0 1 '#13#10 +
  't PANCASE2 0 1 '#13#10 +
  't PANCASE1 0 1 '#13#10 +
  't PANBORD2 0 1 '#13#10 +
  't PANBORD1 0 1 '#13#10 +
  't METAL7 0 1 '#13#10 +
  't METAL6 0 1 '#13#10 +
  't METAL2 0 1 '#13#10 +
  't COMP2 2 '#13#10 +
  't SILVER2 0 1 '#13#10 +
  't SILVER1 0 1 '#13#10 +
  't EXITSIGN X '#13#10 +
  't ZZWOLF1 o 0 1 '#13#10 +
  't ZIMMER3 o 0 1 '#13#10 +
  't ZIMMER5 o 0 1 '#13#10 +
  't TANROCK5 o 0 1 '#13#10 +
  't TANROCK4 o 0 1 '#13#10 +
  't TANROCK2 o 0 1 '#13#10 +
  't STUCCO o 0 1 '#13#10 +
  't STONE6 o 0 1 '#13#10 +
  't ROCK1 o 0 1 '#13#10 +
  't MODWALL1 o 0 1 '#13#10 +
  't BSTONE1 o 0 1 '#13#10 +
  't BRICK5 o 0 1 '#13#10 +
  't BRICK4 o 0 1 '#13#10 +
  't ASHWALL7 o 0 1 '#13#10 +
  't ASHWALL6 o 0 1 '#13#10 +
  't ASHWALL4 o 0 1 '#13#10 +
  't ASHWALL2 o 0 1 '#13#10 +
  't STONE3 o '#13#10 +
  't SP_ROCK1 o '#13#10 +
  't GRAYVINE o '#13#10 +
  't GRAYBIG o '#13#10 +
  't SLDOOR1 z 64 128 d L c M c B = SP_DUDE5 u '#13#10 +
  't DOORSKUL z 64 72 d L c M c B 0 1 u '#13#10 +
  't TEKBRON2 z 64 128 d c M c B 0 1 '#13#10 +
  't SPCDOOR4 z 64 128 d c M 0 1 '#13#10 +
  't SPCDOOR3 z 64 128 d c M 0 1 '#13#10 +
  't SPCDOOR2 z 64 128 d c M 0 1 '#13#10 +
  't SPCDOOR1 z 64 128 d c M 0 1 '#13#10 +
  't DOORHI z 64 128 d c M 2 '#13#10 +
  't DOOR3 z 64 72 d c M '#13#10 +
  't DOOR1 z 64 72 d c M '#13#10 +
  't WOODSKUL z 64 128 d c W c B c R 2 '#13#10 +
  't WOODMET2 z 64 128 d c W c B c R 0 1 '#13#10 +
  't WOODGARG z 64 128 d c W c B c R '#13#10 +
  't BIGDOOR4 z 128 128 d c M '#13#10 +
  't BIGDOOR3 z 128 128 d c M '#13#10 +
  't BIGDOOR2 z 128 128 d c M '#13#10 +
  't BIGDOOR1 z 128 96 d c M '#13#10 +
  't BIGDOOR7 z 128 128 d c W c B c R '#13#10 +
  't BIGDOOR6 z 128 112 d c W c B c R '#13#10 +
  't BIGDOOR5 z 128 128 d c W c B c R '#13#10 +
  't EXITSWIR E c R 0 1 u '#13#10 +
  't EXITSWIW E c W c B 0 1 u '#13#10 +
  't EXITSWIT E c M 0 1 u '#13#10 +
  't BFALL1 z 8 128 l c R 0 1 '#13#10 +
  't LITEREDL z 8 128 l c R = LITERED 2 '#13#10 +
  't TEKLITE l c M 0 1 '#13#10 +
  't LITE4 l c M c B 2 '#13#10 +
  't LITE5 l c M c B '#13#10 +
  't LITE3 l c M c B '#13#10 +
  't SILVER3 p v c M 0 1 '#13#10 +
  't SPACEW3 p v c M 0 1 '#13#10 +
  't COMPSTA2 p v H c M '#13#10 +
  't COMPSTA1 p v H c M '#13#10 +
  't COMPTALL p v c M '#13#10 +
  't COMPUTE1 p v H c M 2 '#13#10 +
  't PLANET1 p v H c M 2 '#13#10 +
  't PANBOOK p c W 0 1 '#13#10 +
  't SKIN2 p v c R '#13#10 +
  't GSTFONT1 p c R '#13#10 +
  't SKY1 p c W c B '#13#10 +
  't SKY3 p c B c R '#13#10 +
  't MARBFAC3 p v c W c B '#13#10 +
  't MARBFAC2 p v c W c B '#13#10 +
  't MARBFACE p v c W c B '#13#10 +
  't BRNBIGC g c M 2 '#13#10 +
  't MIDSPACE g c M 0 1 '#13#10 +
  't MIDVINE1 g c W c M c B c R 2 '#13#10 +
  't MIDBARS1 g c W c M c B c R 0 1 '#13#10 +
  't MIDGRATE g c W c M c B c R '#13#10 +
  't LITERED z 8 128 r c M c B 2 '#13#10 +
  't DOORYEL z 8 128 y c M c B '#13#10 +
  't DOORRED z 8 128 r c M c B '#13#10 +
  't LITEBLU4 z 16 128 b c M c B '#13#10 +
  't LITEBLU1 z 8 128 b c M c B '#13#10 +
  't DOORBLU z 8 128 b c M c B '#13#10 +
  't DOORYEL2 z 16 128 y c W c R '#13#10 +
  't DOORBLU2 z 16 128 b c W c R '#13#10 +
  't DOORRED2 z 16 128 r c W c R '#13#10 +
  't STEP6 z 256 16 e c W c M c B '#13#10 +
  't STEP5 z 256 16 e c W c M c B '#13#10 +
  't STEP4 z 256 16 e c W c M c B '#13#10 +
  't STEP3 z 256 8 e c W c M c B '#13#10 +
  't STEP2 z 256 8 e c W c M c B '#13#10 +
  't STEP1 z 256 8 e c W c M c B '#13#10 +
  't FIRELAVA j c R '#13#10 +
  't DOORTRAK j c W c M c B '#13#10 +
  't DOORSTOP j c W c M c B '#13#10 +
  't SKSNAKE2 I c R '#13#10 +
  't ROCKRED1 I c R '#13#10 +
  't ZIMMER7 I c B 0 1 '#13#10 +
  't BRICK10 I o c B 0 1 '#13#10 +
  't COMPSPAN I c M '#13#10 +
  't SUPPORT2 I c M c B '#13#10 +
  't SHAWN2 I c M c B '#13#10 +
  't ASHWALL3 I o c W 0 1 '#13#10 +
  't ASHWALL I o c W 2 '#13#10 +
  't BROWNHUG I o c W c M c B '#13#10 +
  't SUPPORT3 I c W c B '#13#10 +
  't METAL z 64 128 I d c W c B c R '#13#10 +
  't SW1HOT i c R '#13#10 +
  't SKY3_W w c R = SKY3 '#13#10 +
  't SP_HOT1 w C R '#13#10 +
  't REDWALL w ! C R '#13#10 +
  't FIREMAG1 p c B '#13#10 +
  't FIREBLU1 w C R S FIREMAG1 @ 0 '#13#10 +
  't SW1MARB i c B 0 1 '#13#10 +
  't SW1GSTON i c B '#13#10 +
  't GSTVINE1 w o c B S GSTVINE2 '#13#10 +
  't MARBGRAY w c B S GRAY5 0 1 '#13#10 +
  't GSTONE1 w o c B S GSTGARG '#13#10 +
  't MARBGARG w c B S MARBLE1 0 1 u '#13#10 +
  't MARBLE2 w C B S MARBLE3 '#13#10 +
  't MARBLE3 w C B S MARBLE1 '#13#10 +
  't MARBLE1 w C B S MARBLE3 @ 0 '#13#10 +
  't PLAT1 z 128 128 F c M '#13#10 +
  't GRAYALT w C M s SW1GRAY 0 1 u '#13#10 +
  't TEKVINE w c M S TEKGREN3 s SW1TEK @ 0 0 1 u '#13#10 +
  't SPACEW4 w c M s SW1TEK 0 1 '#13#10 +
  't SW1MET2 Y 64 '#13#10 +
  't METAL3 0 1 '#13#10 +
  't METAL5 w c M S METAL3 s SW1MET2 0 1 '#13#10 +
  't COMPUTE3 w c M s SW1STRTN 2 '#13#10 +
  't TEKWALL4 w c M S COMPWERD s SW1COMP @ 0 '#13#10 +
  't TEKWALL1 w c M S COMPWERD s SW1COMP @ 0 '#13#10 +
  't ICKWALL3 o '#13#10 +
  't GRAY1 w c M S ICKWALL3 s SW1GRAY '#13#10 +
  't BROVINE2 w c M s SW1SLAD @ 24 '#13#10 +
  't BRONZE4 w C M S BRONZE3 s SW1TEK 0 1 '#13#10 +
  't STARTAN1 w C M S STARTAN2 s SW1STRTN 2 '#13#10 +
  't STARTAN3 w C M S STARG3 s SW1STRTN '#13#10 +
  't LITEMET 2 '#13#10 +
  't METAL1 w c M S LITEMET s SW1METAL '#13#10 +
  't STARBR2 w c M S STARTAN2 s SW1STRTN '#13#10 +
  't STARTAN2 w C M S STARBR2 s SW1STRTN '#13#10 +
  't STARG3 w C M S STARGR1 s SW1STRTN '#13#10 +
  't STARG2 w C M S STARG1 s SW1STRTN '#13#10 +
  't STARG1 w C M S STARG2 s SW1STRTN '#13#10 +
  't SW1DIRT Y 72 '#13#10 +
  't BROWN144 o '#13#10 +
  't BROWN96 w C M S BROWN144 s SW1DIRT '#13#10 +
  't BROWN1 w C M s SW1BRN2 '#13#10 +
  't BROWNGRN w C M S SLADWALL s SW1BRNGN '#13#10 +
  't SLADWALL w C M S BROWNGRN s SW1SLAD '#13#10 +
  't SW1WOOD i c W c B '#13#10 +
  't SW1SATYR i c W '#13#10 +
  't SW1LION i c W '#13#10 +
  't SW1GARG i c W '#13#10 +
  't WOOD12 w c W @ 3 0 1 '#13#10 +
  't SLOPPY2 w c W S SLOPPY1 @ 0 0 1 Q '#13#10 +
  't SKULWALL w c W S SKULWAL3 @ 0 2 Q '#13#10 +
  't SKINSYMB w c W S SKINMET1 '#13#10 +
  't SKINMET2 w c W S SKINMET1 '#13#10 +
  't SKINMET1 w c W S SKINMET2 '#13#10 +
  't PIPE2 w j c W c M S PIPE4 s SW1WOOD @ 0 '#13#10 +
  't WOODVINE w c W S WOOD9 @ 0 0 1 u '#13#10 +
  't WOOD4 w c W @ 64 '#13#10 +
  't WOOD9 w C W S WOOD7 @ 0 0 1 '#13#10 +
  't WOOD5 w C W S WOOD1 '#13#10 +
  't WOOD3 w C W S WOOD1 @ 3 '#13#10 +
  't WOOD1 w C W S WOOD3 '#13#10 +
  'f FWATER1 W '#13#10 +
  'f F_SKY1 K '#13#10 +
  'f SLGATE1 G c W c M c B c R u '#13#10 +
  'f GATE4 G c W c M c B c R '#13#10 +
  'f GATE3 G c W c M c B c R '#13#10 +
  'f GATE2 G c W c M c B c R '#13#10 +
  'f GATE1 G c W c M c B c R '#13#10 +
  'f SLGRASS1 o u '#13#10 +
  'f SLIME13 o 0 1 '#13#10 +
  'f RROCK19 o 0 1 '#13#10 +
  'f RROCK16 o 0 1 '#13#10 +
  'f RROCK11 o 0 1 '#13#10 +
  'f GRNROCK o 0 1 '#13#10 +
  'f GRASS2 o 0 1 '#13#10 +
  'f GRASS1 o 0 1 '#13#10 +
  'f MFLR8_4 o '#13#10 +
  'f MFLR8_3 o '#13#10 +
  'f MFLR8_2 o '#13#10 +
  'f FLAT5_7 o '#13#10 +
  'f FLAT10 o '#13#10 +
  'f GRNLITE1 U l c B 0 1 '#13#10 +
  'f FLOOR7_2 U c B '#13#10 +
  'f SLLITE1 U l c M u '#13#10 +
  'f TLITE6_6 U l c M '#13#10 +
  'f FLOOR7_1 U o c M '#13#10 +
  'f FLOOR5_2 U c M '#13#10 +
  'f FLOOR5_1 U c M '#13#10 +
  'f CEIL3_1 U c M '#13#10 +
  'f FLOOR5_4 U o c W '#13#10 +
  'f FLOOR4_6 U c W '#13#10 +
  'f CEIL3_3 U c W '#13#10 +
  'f CEIL1_1 U c W '#13#10 +
  'f LAVA1 n c R '#13#10 +
  'f SLSPARKS D c R u '#13#10 +
  'f SFLR6_4 D U c R '#13#10 +
  'f TLITE6_5 U l c R '#13#10 +
  'f FLOOR6_1 D U r c R '#13#10 +
  'f FLOOR1_7 U l c R '#13#10 +
  'f FLOOR1_6 D U r c R '#13#10 +
  'f FLAT5_3 D U r c R '#13#10 +
  'f SLFLAT01 D c B u '#13#10 +
  'f FLAT1 D c B '#13#10 +
  'f DEM1_6 D c B '#13#10 +
  'f DEM1_5 D U c B '#13#10 +
  'f NUKAGE1 n c M c B '#13#10 +
  'f FLOOR4_1 D c M '#13#10 +
  'f FLOOR3_3 D U c M c B '#13#10 +
  'f FLOOR0_2 D c M '#13#10 +
  'f FLOOR0_1 D o c M '#13#10 +
  'f FLAT1_2 D o c M '#13#10 +
  'f FLAT5 D c M '#13#10 +
  'f SLIME09 n c W 0 1 '#13#10 +
  'f BLOOD1 n r c W c B c R '#13#10 +
  'f FLAT8 D c W '#13#10 +
  'f FLAT5_2 D c W '#13#10 +
  'f FLAT5_1 D U c W c B '#13#10 +
  'f FLAT1_1 D o c W '#13#10 +
  'f CEIL5_2 D o c W '#13#10 +
  'f MFLR8_1 D c W '#13#10 +
  'x m 4 h 128 c W c B 0 1 '#13#10 +
  'O FLAT5_1 O CRATOP2 O CEIL5_2 O CEIL3_3 O CEIL1_1 '#13#10 +
  'B PANEL5 ~ 64 '#13#10 +
  'B PANCASE2 ~ 64 '#13#10 +
  'B PANCASE1 ~ 64 '#13#10 +
  'B PANBORD2 ~ 16 '#13#10 +
  'B PANBORD1 ~ 32 '#13#10 +
  'A PANBOOK ~ 64 '#13#10 +
  'x m 3 h 64 c W c M '#13#10 +
  'O CRATOP2 '#13#10 +
  'A CRATWIDE ] 64 64 '#13#10 +
  'A CRATE1 ~ 64 '#13#10 +
  'x m 3 h 64 c W c M '#13#10 +
  'O CRATOP1 '#13#10 +
  'A CRATWIDE '#13#10 +
  'A CRATE2 ~ 64 '#13#10 +
  'x m 3 h 64 c W c M '#13#10 +
  'O CRATOP1 '#13#10 +
  'A CRATELIT ~ 32 '#13#10 +
  'x m 3 h 32 c W c M '#13#10 +
  'O CRATOP1 '#13#10 +
  'A CRATELIT ~ 32 '#13#10 +
  'x m 3 h 16 c W c M '#13#10 +
  'O CRATOP1 '#13#10 +
  'A CRATINY ~ 16 '#13#10 +
  'x m 2 h 128 c M '#13#10 +
  'O CEIL5_1 O FLAT4 O TLITE6_1 '#13#10 +
  'B METAL7 ] 0 64 ~ 64 '#13#10 +
  'B METAL6 ] 0 64 ~ 64 '#13#10 +
  'B METAL5 ] 0 64 ~ 64 '#13#10 +
  'B METAL3 ] 0 64 ~ 64 '#13#10 +
  'B METAL2 ] 0 64 ~ 64 '#13#10 +
  'B COMPWERD ~ 64 '#13#10 +
  'B COMPSPAN ~ 16 '#13#10 +
  'A SPACEW3 ~ 64 '#13#10 +
  'A COMPTALL ~ 256 '#13#10 +
  'A COMP2 ~ 256 '#13#10 +
  'x m 2 h 64 c M '#13#10 +
  'O CEIL5_1 O FLAT4 O TLITE6_1 '#13#10 +
  'B METAL7 ] 0 64 ~ 64 '#13#10 +
  'B METAL6 ] 0 64 ~ 64 '#13#10 +
  'B METAL5 ] 0 64 ~ 64 '#13#10 +
  'B METAL3 ] 0 64 ~ 64 '#13#10 +
  'B METAL2 ] 0 64 ~ 64 '#13#10 +
  'B COMPWERD ~ 64 '#13#10 +
  'B COMPSPAN ~ 16 '#13#10 +
  'A SPACEW3 ] 0 64 ~ 64 '#13#10 +
  'A COMPTALL ] 0 64 ~ 256 '#13#10 +
  'A COMP2 ] 0 64 ~ 256 '#13#10 +
  'x m 1 h 128 c M c B '#13#10 +
  'O FLAT9 O FLAT4 O FLAT23 O FLAT19 O FLAT18 O CRATOP1 O COMP01 '#13#10 +
  'B SILVER2 ~ 64 '#13#10 +
  'B SILVER1 ~ 64 '#13#10 +
  'B SUPPORT2 ~ 16 '#13#10 +
  'B SHAWN2 ~ 16 '#13#10 +
  'A SILVER3 '#13#10 +
  'A COMPUTE1 ] 0 64 '#13#10 +
  'x m 1 h 64 c M c B '#13#10 +
  'O FLAT9 O FLAT4 O FLAT23 O FLAT19 O FLAT18 O CRATOP1 O COMP01 '#13#10 +
  'B SUPPORT2 ~ 16 '#13#10 +
  'B SHAWN2 ~ 16 '#13#10 +
  'A COMPUTE1 ] 0 64 '#13#10 +
  'A COMPSTA2 '#13#10 +
  'A COMPSTA1 '#13#10 +
  '. 2035 c M '#13#10 +
  '. 34 c W c M c B '#13#10 +
  '. 44 c W c B '#13#10 +
  '. 45 c W c B '#13#10 +
  '. 46 c W c B c R '#13#10 +
  '. 55 c W c B '#13#10 +
  '. 56 c W c B '#13#10 +
  '. 57 c W c B c R '#13#10 +
  '. 48 c M '#13#10 +
  '. 2028 c M '#13#10 +
  '. 85 c M '#13#10 +
  '. 86 c M '#13#10 +
  '. 70 c M c W '#13#10 +
  '. 35 c W '#13#10 +
  '# ';
end;

// Make the config-file data accessible
procedure load_config(const c: config_p);
var
  slist: TDStringList;
  i, j: integer;
  incomment: boolean;
  stmp: string;
begin
  // JVAL: Pascal makes it easy
  if FileExists(c^.configfile) then
  begin
    slist := TDStringList.Create;
    slist.LoadFromFile(c^.configfile);
    // jval: get rid of comments, prepare configdata for the scriptengine's parser
    for i := 0 to slist.Count - 1 do
    begin
      incomment := false;
      stmp := slist.Strings[i];
      for j := 1 to length(stmp) do
      begin
        if stmp[j] = ';' then
          incomment := true;
        if incomment then
          stmp[j] := ' ';
      end;
      slist.Strings[i] := stmp;
    end;
    c^.configdata := slist.Text;
    slist.Free;
  end
  else
    load_default_config(c);
end;

// Free up config-file resources
procedure unload_config(const c: config_p);
begin
  c^.configdata := ''; // JVAL: Pascal makes it easy
end;

// Look through the config's config file, and fill in values for //
// the switch lines therein.  Return FALSE if error.  These are  //
// of course overridable by command-line switches.               //
function read_switches(const c: config_p): boolean;
begin
  // Dis here is a STUB
  result := TRUE;
end;

// Allocate and return a new, empty construct
function new_construct(const c: config_p): construct_p;
begin
  result := construct_p(SL_Malloc(SizeOf(construct_t)));

  result^.height := 64;
  result^.gamemask := DOOM1_BIT or DOOM0_BIT or DOOM2_BIT or DOOMI_BIT or DOOMC_BIT;
  result^.compatible := 0;
  result^.texture_cell_anchor := nil;
  result^.flat_cell_anchor := nil;
  result^.family := 0;
  result^.marked := FALSE;
  result^.next := c^.construct_anchor;
  c^.construct_anchor := result;
end;

function add_flat_cell(const cn: construct_p; const name: string; const c: config_p): flat_cell_p;
begin
  result := flat_cell_p(SL_Malloc(SizeOf(flat_cell_t)));

  result^.flat := find_flat(c, name);
  result^.next := cn^.flat_cell_anchor;
  cn^.flat_cell_anchor := result;
end;

function add_texture_cell(const cn: construct_p; const name: string;
  const primary: boolean; const y1, y2: SmallInt; const c: config_p): texture_cell_p;
begin
  result := texture_cell_p(SL_Malloc(SizeOf(texture_cell_t)));

  result^.texture := find_texture(c, name);
  result^.width := 128; // A nicer default, as it happens
  result^.y_offset1 := y1;
  result^.y_offset2 := y2;
  result^.primary := primary;
  result^.marked := FALSE;
  result^.next := cn^.texture_cell_anchor;
  cn^.texture_cell_anchor := result;
end;

// Get the hardwired nonswitch-nontheme config stuff (like
// monsters and obstables and such.
function hardwired_nonswitch_nontheme_config(const c: config_p): boolean;
var
  m: genus_p;
begin
  // get these obstacles registered as non-pickables
  m := find_genus(c, ID_LAMP);
  m^.bits := m^.bits and not PICKABLE;
  m^.bits := m^.bits or LIGHT;
  m^.width := 33;
  m := find_genus(c, ID_ELEC);
  m^.bits := m^.bits and not PICKABLE;
  m^.bits := m^.bits or LIGHT;
  m^.width := 33;
  m^.height := 127; // About
  m := find_genus(c, ID_LAMP2);
  m^.bits := m^.bits and not PICKABLE;
  m^.bits := m^.bits or LIGHT;
  m^.width := 33;
  m^.gamemask := DOOM2_BIT or DOOMC_BIT or DOOMI_BIT;
  m := find_genus(c, ID_TLAMP2);
  m^.bits := m^.bits and not PICKABLE;
  m^.bits := m^.bits or LIGHT;
  m^.width := 33;
  m^.height := 72; // Very roughly
  m^.gamemask := DOOM2_BIT or DOOMC_BIT or DOOMI_BIT;
  m := find_genus(c, ID_SHORTRED);
  m^.bits := m^.bits and not PICKABLE;
  m^.bits := m^.bits or LIGHT;
  m^.width := 33;
  m := find_genus(c, ID_SHORTBLUE);
  m^.bits := m^.bits and not PICKABLE;
  m^.bits := m^.bits or LIGHT;
  m^.width := 33;
  m := find_genus(c, ID_SHORTGREEN);
  m^.bits := m^.bits and not PICKABLE;
  m^.bits := m^.bits or LIGHT;
  m^.width := 33;
  m := find_genus(c, ID_TALLRED);
  m^.bits := m^.bits and not PICKABLE;
  m^.bits := m^.bits or LIGHT;
  m^.width := 33;
  m^.height := 127; // sure
  m := find_genus(c, ID_TALLBLUE);
  m^.bits := m^.bits and not PICKABLE;
  m^.bits := m^.bits or LIGHT;
  m^.width := 33;
  m^.height := 127; // sure
  m := find_genus(c, ID_TALLGREEN);
  m^.bits := m^.bits and not PICKABLE;
  m^.bits := m^.bits or LIGHT;
  m^.width := 33;
  m^.height := 127; // sure
  m := find_genus(c,ID_CBRA);
  m^.bits := m^.bits and not PICKABLE;
  m^.bits := m^.bits or LIGHT;
  m^.width := 33;
  m^.height := 72; // about
  m := find_genus(c, ID_FBARREL);
  m^.gamemask := DOOM2_BIT or DOOMC_BIT or DOOMI_BIT;
  m^.bits := m^.bits and not PICKABLE;
  m^.bits := m^.bits or LIGHT;
  m^.width := 33;
  m := find_genus(c, ID_BARREL);
  m^.bits := m^.bits and not PICKABLE;
  m^.bits := m^.bits or EXPLODES;
  m^.width := 33;
  // pretend the candle is pickable; really just "not blocking"
  m := find_genus(c, ID_CANDLE);
  m^.bits := m^.bits or PICKABLE;
  m^.bits := m^.bits or LIGHT;
  m^.width := 16;
  // and register the weapons and ammos and healths
  // at least the ones that we need for arenas!
  m := find_genus(c, ID_ROCKBOX);
  m^.bits := m^.bits or AMMO;
  m^.ammo_provides := 500;
  m := find_genus(c, ID_BULBOX);
  m^.bits := m^.bits or AMMO;
  m^.ammo_provides := 500;
  m := find_genus(c, ID_CELLPACK);
  m^.bits := m^.bits or AMMO;
  m^.ammo_provides := 2000; // Hoo-hoo!  Same for BFG as plasgun?
  // violence and mayhem
  c^.usualammo[ITYTD] := 5000;
  c^.usualammo[HMP] := 3500; // or 3k?
  c^.usualammo[UV] := 3500; // or 2k? or 3k?
  c^.usualarmor[ITYTD] := 100;
  c^.usualarmor[HMP] := 50;
  c^.usualarmor[UV] := 30; // or 20
  c^.usualhealth[ITYTD] := 80;
  c^.usualhealth[HMP] := 65;
  c^.usualhealth[UV] := 55;
  c^.minhealth[ITYTD] := 50;
  c^.minhealth[HMP] := 35;
  c^.minhealth[UV] := 20;
  m := find_monster(c, ID_TROOPER);
  m^.width := 42;
  m^.ammo_provides := 100;
  m^.ammo_to_kill[ITYTD] := 55;
  m^.ammo_to_kill[HMP] := 35;
  m^.ammo_to_kill[UV] := 30;
  m^.damage[ITYTD] := 15;
  m^.damage[HMP] := 3;
  m^.damage[UV] := 1;
  m^.altdamage[ITYTD] := 10;
  m^.altdamage[HMP] := 1;
  m^.altdamage[UV] := 1;
  m^.bits := m^.bits or SHOOTS;
  m := find_monster(c, ID_SERGEANT);
  m^.width := 42;
  m^.ammo_provides := 280;
  m^.ammo_to_kill[ITYTD] := 80;
  m^.ammo_to_kill[HMP] := 50;
  m^.ammo_to_kill[UV] := 40;
  m^.damage[ITYTD] := 25;
  m^.damage[HMP] := 6;
  m^.damage[UV] := 2;
  m^.altdamage[ITYTD] := 20;
  m^.altdamage[HMP] := 2;
  m^.altdamage[UV] := 1;
  m^.bits := m^.bits or SHOOTS;
  m := find_monster(c, ID_IMP);
  m^.width := 42;
  m^.ammo_provides := 0;
  m^.ammo_to_kill[ITYTD] := 160;
  m^.ammo_to_kill[HMP] := 95;
  m^.ammo_to_kill[UV] := 80;
  m^.damage[ITYTD] := 20;
  m^.damage[HMP] := 6;
  m^.damage[UV] := 3;
  m^.altdamage[ITYTD] := 20;
  m^.altdamage[HMP] := 5;
  m^.altdamage[UV] := 2;
  m^.bits := m^.bits or SHOOTS;
  m := find_monster(c, ID_PINK);
  m^.width := 62;
  m^.ammo_provides := 0;
  m^.ammo_to_kill[ITYTD] := 385;
  m^.ammo_to_kill[HMP] := 236;
  m^.ammo_to_kill[UV] := 195;
  m^.damage[ITYTD] := 25;
  m^.damage[HMP] := 10;
  m^.damage[UV] := 8;
  m^.altdamage[ITYTD] := 20;
  m^.altdamage[HMP] := 8;
  m^.altdamage[UV] := 4;
  m := find_monster(c, ID_SPECTRE);
  m^.width := 62;
  m^.ammo_provides := 0;
  m^.ammo_to_kill[ITYTD] := 410;
  m^.ammo_to_kill[HMP] := 260;
  m^.ammo_to_kill[UV] := 220;
  m^.damage[ITYTD] := 25;
  m^.damage[HMP] := 10;
  m^.damage[UV] := 8;
  m^.altdamage[ITYTD] := 25;
  m^.altdamage[HMP] := 8;
  m^.altdamage[UV] := 6;
  m := find_monster(c, ID_SKULL);
  m^.width := 34;
  m^.bits := m^.bits or BIG; // Well, sort of!
  m^.ammo_provides := 0;
  m^.ammo_to_kill[ITYTD] := 260;
  m^.ammo_to_kill[HMP] := 165;
  m^.ammo_to_kill[UV] := 130;
  m^.damage[ITYTD] := 22;
  m^.damage[HMP] := 8;
  m^.damage[UV] := 5;
  m^.altdamage[ITYTD] := 18;
  m^.altdamage[HMP] := 5;
  m^.altdamage[UV] := 2;
  m^.bits := m^.bits or FLIES;
  m := find_monster(c, ID_HEAD);
  m^.width := 63; // Or 62 or maybe 64
  m^.bits := m^.bits or BIG;
  m^.ammo_provides := 0;
  m^.ammo_to_kill[ITYTD] := 1050;
  m^.ammo_to_kill[HMP] := 630;
  m^.ammo_to_kill[UV] := 590;
  m^.damage[ITYTD] := 60;
  m^.damage[HMP] := 35;
  m^.damage[UV] := 18;
  m^.altdamage[ITYTD] := 50;
  m^.altdamage[HMP] := 20;
  m^.altdamage[UV] := 10;
  m^.bits := m^.bits or SHOOTS;
  m^.bits := m^.bits or FLIES;

  m := find_monster(c,ID_BARON);
  m^.width := 50; // Roughly
  m^.height := 64;
  m^.bits := m^.bits or BIG or BOSS; // Not placed randomly
  m^.ammo_provides := 0;
  m^.ammo_to_kill[ITYTD] := 1900; // Numbers are all guesses; fix
  m^.ammo_to_kill[HMP] := 1600;
  m^.ammo_to_kill[UV] := 1500;
  m^.damage[ITYTD] := 80;
  m^.damage[HMP] := 40;
  m^.damage[UV] := 25;
  m^.altdamage[ITYTD] := 70;
  m^.altdamage[HMP] := 25;
  m^.altdamage[UV] := 18;
  m^.bits := m^.bits or SHOOTS;

  // Other bosses; need to fill in data!
  m := find_monster(c, ID_CYBER);
  m^.width := 84;
  m^.height := 110;
  m^.bits := m^.bits or BIG or BOSS;
  m^.ammo_provides := 0;
  m^.ammo_to_kill[ITYTD] := 8000; // Numbers are all guesses; fix
  m^.ammo_to_kill[HMP] := 6500;
  m^.ammo_to_kill[UV] := 6200;
  m := find_monster(c, ID_SPIDERBOSS);
  m^.width := 260;
  m^.height := 100;
  m^.bits := m^.bits or BIG or BOSS;
  m^.ammo_provides := 0;
  m^.ammo_to_kill[ITYTD] := 6000; // Numbers are all guesses; fix
  m^.ammo_to_kill[HMP] := 5000;
  m^.ammo_to_kill[UV] := 4500;

  // DOOM2 monsters
  if c^.gamemask and (DOOM0_BIT or DOOM1_BIT) = 0 then
  begin
    m := find_monster(c, ID_NAZI);
    m^.gamemask := DOOM2_BIT;
    m^.width := 42;
    m^.ammo_to_kill[ITYTD] := 117;
    m^.ammo_to_kill[HMP] := 78;
    m^.ammo_to_kill[UV] := 65;
    m^.damage[ITYTD] := 40;
    m^.damage[HMP] := 14;
    m^.damage[UV] := 7;
    m^.altdamage[ITYTD] := 27;
    m^.altdamage[HMP] := 10;
    m^.altdamage[UV] := 4;
    m^.bits := m^.bits or SHOOTS or SPECIAL;
    m := find_monster(c,ID_COMMANDO);
    m^.gamemask := DOOM2_BIT;
    m^.width := 42;
    m^.ammo_provides := 100;
    m^.ammo_to_kill[ITYTD] := 155;
    m^.ammo_to_kill[HMP] := 106;
    m^.ammo_to_kill[UV] := 90;
    m^.damage[ITYTD] := 60;
    m^.damage[HMP] := 25;
    m^.damage[UV] := 15;
    m^.altdamage[ITYTD] := 40;
    m^.altdamage[HMP] := 20;
    m^.altdamage[UV] := 10;
    m^.bits := m^.bits or SHOOTS;
    m := find_monster(c, ID_SKEL);
    m^.gamemask := DOOM2_BIT;
    m^.width := 42;
    m^.bits := m^.bits or BIG;
    m^.ammo_provides := 0;
    m^.ammo_to_kill[ITYTD] := 800;
    m^.ammo_to_kill[HMP] := 500;
    m^.ammo_to_kill[UV] := 400;
    m^.damage[ITYTD] := 125;
    m^.damage[HMP] := 70;
    m^.damage[UV] := 40;
    m^.altdamage[ITYTD] := 100;
    m^.altdamage[HMP] := 40;
    m^.altdamage[UV] := 25;
    m^.bits := m^.bits or SHOOTS;
    m := find_monster(c, ID_HELL);
    m^.gamemask := DOOM2_BIT;
    m^.width := 50;
    m^.bits := m^.bits or BIG;
    m^.ammo_provides := 0;
    m^.ammo_to_kill[ITYTD] := 1400;
    m^.ammo_to_kill[HMP] := 850;
    m^.ammo_to_kill[UV] := 666;
    m^.damage[ITYTD] := 140;
    m^.damage[HMP] := 80;
    m^.damage[UV] := 50;
    m^.altdamage[ITYTD] := 120;
    m^.altdamage[HMP] := 50;
    m^.altdamage[UV] := 35;
    m^.bits := m^.bits or SHOOTS;

    // DOOM2 bosses and underbosses; need to fill in data!
    m := find_monster(c, ID_MANCUB);
    m^.gamemask := DOOM2_BIT;
    m^.width := 100;
    m^.height := 64;
    m^.bits := m^.bits or BIG or BOSS;
    m^.ammo_provides := 0;
    m^.ammo_to_kill[ITYTD] := 1200; // Numbers are all guesses; fix
    m^.ammo_to_kill[HMP] := 1000;
    m^.ammo_to_kill[UV] := 900;
    m := find_monster(c, ID_ARCHIE);
    m^.gamemask := DOOM2_BIT;
    m^.width := 42;
    m^.height := 56;
    m^.bits := m^.bits or BIG or BOSS;
    m^.ammo_provides := 0;
    m^.ammo_to_kill[ITYTD] := 1300; // Numbers are all guesses; fix
    m^.ammo_to_kill[HMP] := 1100;
    m^.ammo_to_kill[UV] := 1000;
    m := find_monster(c, ID_PAIN);
    m^.gamemask := DOOM2_BIT;
    m^.width := 63;
    m^.bits := m^.bits or BIG or BOSS;
    m^.ammo_provides := 0;
    m^.ammo_to_kill[ITYTD] := 1900; // Numbers are all guesses; fix
    m^.ammo_to_kill[HMP] := 1600;
    m^.ammo_to_kill[UV] := 1500;
    m := find_monster(c, ID_ARACH);
    m^.gamemask := DOOM2_BIT;
    m^.width := 130;
    m^.bits := m^.bits or BIG or BOSS;
    m^.ammo_provides := 0;
    m^.ammo_to_kill[ITYTD] := 1300; // Numbers are all guesses; fix
    m^.ammo_to_kill[HMP] := 800;
    m^.ammo_to_kill[UV] := 700;
  end;

  result := TRUE;
end;

// Absorb a property-word, return the corresponding bit, and
// update *r to point to the last string we used (but since all
// the properties we handle are one token long, we never actually
// change *r).
function absorb_propertybit(const r: string; out bit: LongWord): LongWord;
begin
  if not b_stricmp(r, 'wall') or (r = 'w') then result := WALL
  else if not b_stricmp(r, 'isswitch') or (r = 'i') then result := SWITCH
  else if not b_stricmp(r, 'lift') or (r = 'F') then result := LIFT_TEXTURE
  else if not b_stricmp(r, 'support') or (r = 'I') then result := SUPPORT
  else if not b_stricmp(r, 'jamb') or (r = 'j') then result := JAMB
  else if not b_stricmp(r, 'step') or (r = 'e') then result := STEP
  else if not b_stricmp(r, 'grating') or (r = 'g') then result := GRATING
  else if not b_stricmp(r, 'plaque') or (r = 'p') then result := PLAQUE
  else if not b_stricmp(r, 'vtiles') or (r = 'v') then result := VTILES
  else if not b_stricmp(r, 'half_plaque') or (r = 'H') then result := HALF_PLAQUE
  else if not b_stricmp(r, 'light') or (r = 'l') then result := LIGHT
  else if not b_stricmp(r, 'exitswitch') or (r = 'E') then result := EXITSWITCH
  else if not b_stricmp(r, 'door') or (r = 'd') then result := DOOR
  else if not b_stricmp(r, 'locked') or (r = 'L') then result := GATE
  else if not b_stricmp(r, 'outside') or (r = 'o') then result := OUTDOOR
  else if not b_stricmp(r, 'red') or (r = 'r') then result := RED
  else if not b_stricmp(r, 'blue') or (r = 'b') then result := BLUE
  else if not b_stricmp(r, 'yellow') or (r = 'y') then result := YELLOW
  else if not b_stricmp(r, 'floor') or (r = 'D') then result := SL_FLOOR
  else if not b_stricmp(r, 'ceiling') or (r = 'U') then result := CEILING
  else if not b_stricmp(r, 'nukage') or (r = 'n') then result := NUKAGE
  else if not b_stricmp(r, 'gate') or (r = 'G') then result := GATE
  else result := 0;
  bit := result;
end;

// Absorb a restriction-word, return the corresponding bit, and
// update *r to point to the last string we used (but since all
// the restrictions we handle are one token long, we never actually
// change *r).
function absorb_gamebit(const r: string; out bit: byte): byte;
begin
  if not b_stricmp(r, 'nodoom0') or (r = '0') then result := DOOM0_BIT
  else if not b_stricmp(r, 'nodoom1') or (r = '1') then result := DOOM1_BIT
  else if not b_stricmp(r, 'nodoom2') or (r = '2') then result := DOOM2_BIT
  else if not b_stricmp(r, 'gross') or (r = 'Q') then result := DOOMC_BIT
  else if not b_stricmp(r, 'custom') or (r = 'u') then result := DOOMI_BIT
  else result := 0;
  bit := result;
end;

// Absorb a Theme record from the config data, updating then script engine
// postition to the last string that we actually used.
function absorb_theme(const sc: TScriptEngine; const c: config_p): boolean;
var
  token: string;
  utoken: string;
  themename: string;
  issecret: boolean;
begin
  if not sc.GetString then
  begin
    result := false;
    exit;
  end;

  token := sc._String;
  utoken := UpperCase(token);
  if (token = 'T') or (utoken = 'THEME') then
  begin
    sc.MustGetString;
    themename := sc._String;
    issecret := false;
    if sc.GetString then
    begin
      token := sc._String;
      utoken := UpperCase(token);
      if (token = '?') or (utoken = 'SECRET') then
        issecret := true
      else
        sc.UnGet;
    end;
    new_theme(c, themename, issecret);
    result := true;
  end
  else
  begin
    sc.UnGet;
    result := false;
  end;

end;

// Return a themebit for the given name, or zero if none
function themebit_for_name(const name: string; const c: config_p): LongWord;
var
  t: theme_p;
begin
  result := 1;
  t := c^.theme_anchor;
  while t <> nil do
  begin
    if not b_stricmp(t^.name, name) then
      exit;
    result := result * 2;
    t := t^.next;
  end;
  result := 0;
end;

// Absorb a Texture record from the config data, update script engine to the
// last token
procedure absorb_texture(const sc: TScriptEngine; const c: config_p);
var
  token: string;
  utoken: string;
  t: texture_p;
  pb, tb: LongWord;
  gb: byte;
  name: string;
  m, n: integer;
begin
  if not sc.GetString then
    exit;

  token := sc._String; // That's the name
  t := find_texture(c, token);

  while true do
  begin
    if not sc.GetString then
      break;

    token := sc._String;
    utoken := UpperCase(token);

    if absorb_propertybit(token, pb) <> 0 then
    begin
      t^.props := t^.props or pb;
      continue;
    end;

    if absorb_gamebit(token, gb) > 0 then
    begin
      t^.gamemask := t^.gamemask and not gb;
      continue;
    end;

    if (token = 'C') or (utoken = 'CORE') then
    begin
      if sc.GetString then
      begin
        name := sc._String;
        tb := themebit_for_name(name, c);
      end
      else
      begin
        name := '';
        tb := 0;
      end;
      if tb = 0 then
      begin
        announce(ERROR, Format('Unknown theme <%s> in core.', [name]));
        if name <> '' then
          sc.UnGet;
        break;
      end;
      t^.core := t^.core or tb;
      t^.compatible := t^.compatible or tb;
      continue;
    end;

    if (token = 'c') or (utoken = 'COMP') then
    begin
      if sc.GetString then
      begin
        name := sc._String;
        tb := themebit_for_name(name, c);
      end
      else
      begin
        name := '';
        tb := 0;
      end;
      if tb = 0 then
      begin
        announce(ERROR, Format('Unknown theme <%s> in comp.', [name]));
        if name <> '' then
          sc.UnGet;
        break;
      end;
      t^.compatible := t^.compatible or tb;
      continue;
    end;

    if (token = 's') or (utoken = 'SWITCH') then
    begin
      if sc.GetString then
        name := sc._String
      else
        break;

      t^.switch_texture := find_texture(c, name);
      continue;
    end;

    if (token = 'S') or (utoken = 'SUBTLE') then
    begin
      if sc.GetString then
        name := sc._String
      else
        break;

      t^.subtle := find_texture(c, name);
      continue;
    end;

    if (token = '=') or (utoken = 'REALNAME') then
    begin
      if sc.GetString then
        name := sc._String
      else
        break;

      t^.realname := name;
      continue;
    end;

    if (token = '@') or (utoken = 'YHINT') then
    begin
      if sc.GetString then
        t^.y_hint := atoi(sc._String)
      else
        break;

      continue;
    end;

    if (token = 'Y') or (utoken = 'YBIAS') then
    begin
      if sc.GetString then
        t^.y_bias := atoi(sc._String)
      else
        exit;

      continue;
    end;

    if (token = 'z') or (utoken = 'SIZE') then
    begin
      m := 64;  // Shut up compiler warning
      n := 128; // Shut up compiler warning
      if sc.GetString then
        m := atoi(sc._String)
      else
        exit;
      if sc.GetString then
        n := atoi(sc._String)
      else
        exit;

      t^.width := m;
      t^.height := n;
      continue;
    end;

    if (token = '!') or (utoken = 'ERROR') then
    begin
      c^.error_texture := t;
      continue;
    end;

    if (utoken = 'X') or (utoken = 'GATEEXITSIGN') then
    begin
      c^.gate_exitsign_texture := t;
      continue;
    end;

    sc.UnGet;
    break;
  end;
end;

// Absorb a Flat record from the config data, update script engine to the
// last token
procedure absorb_flat(const sc: TScriptEngine; const c: config_p);
var
  token: string;
  utoken: string;
  f: flat_p;
  pb, tb: LongWord;
  gb: byte;
  name: string;
begin
  if not sc.GetString then
    exit;

  token := sc._String; // That's the name
  f := find_flat(c, token);

  while true do
  begin
    if not sc.GetString then
      break;

    token := sc._String;
    utoken := UpperCase(token);

    if absorb_propertybit(token, pb) <> 0 then
    begin
      f^.props:= f^.props or pb;
      continue;
    end;

    if absorb_gamebit(token, gb) > 0 then
    begin
      f^.gamemask := f^.gamemask and not gb;
      continue;
    end;

    if (token = 'c') or (utoken = 'COMP') then
    begin
      if sc.GetString then
      begin
        name := sc._String;
        tb := themebit_for_name(name, c);
      end
      else
      begin
        name := '';
        tb := 0;
      end;
      if tb = 0 then
      begin
        announce(ERROR, Format('Unknown theme <%s> in comp.', [name]));
        if name <> '' then
          sc.UnGet;
        break;
      end;
      f^.compatible := f^.compatible or tb;
      continue;
    end;

    if (utoken = 'K') or (utoken = 'SKY') then
    begin
      c^.sky_flat := f;
      continue;
    end;

    if (utoken = 'W') or (utoken = 'WATER') then
    begin
      c^.water_flat := f;
      continue;
    end;

    sc.UnGet;
    break;
  end;
end;

// Absorb a cell subrecord of a construct record, returning TRUE if
// there is one there, or FALSE if not.  Update r to point to the
// last string we actually used.
function absorb_cell(const x: construct_p; const sc: TScriptEngine;
  const b: boolean; const c: config_p): boolean;
var
  token: string;
  utoken: string;
  tc: texture_cell_p;
  o1, o2: SmallInt;
  width: SmallInt;
  name: string;
begin
  o1 := 0;
  o2 := 0;
  width := 128;

  if not sc.GetString then
  begin
    result := FALSE;
    exit;
  end;

  name := sc._String;

  while true do
  begin
    if not sc.GetString then
      break;

    token := sc._String;
    utoken := UpperCase(token);

    if (token = '~') or (utoken = 'WIDTH') then
    begin
      if sc.GetString then
        width := atoi(sc._String, 128)
      else
        break;

      continue;
    end;

    if (token = ']') or (utoken = 'YOFFSETS') then
    begin
      if sc.GetString then
        o1 := atoi(sc._String, 0)
      else
        break;

      if sc.GetString then
        o2 := atoi(sc._String, 0)
      else
        break;

      continue;
    end;

    sc.UnGet;
    break;
  end;

  tc := add_texture_cell(x, name, b, o1, o2, c);
  tc^.width := width;

  result := TRUE;
end;

// Absorb a Construct record from the config data, update script engine to the
// last token
procedure absorb_construct(const sc: TScriptEngine; const c: config_p);
var
  token: string;
  utoken: string;
  x: construct_p;
  tb: LongWord;
  gb: byte;
  name: string;
begin
  x := new_construct(c);

  while true do
  begin
    if not sc.GetString then
      break;

    token := sc._String;
    utoken := UpperCase(token);

    if (token = 'm') or (utoken = 'FAMILY') then
    begin
      if sc.GetString then
        x^.family := atoi(sc._String)
      else
        break;

      continue;
    end;

    if (token = 'h') or (utoken = 'HEIGHT') then
    begin
      if sc.GetString then
        x^.height := atoi(sc._String)
      else
        break;

      continue;
    end;

    if absorb_gamebit(token, gb) > 0 then
    begin
      x^.gamemask := x^.gamemask and not gb;
      continue;
    end;

    if (token = 'c') or (utoken = 'COMP') then
    begin
      if sc.GetString then
      begin
        name := sc._String;
        tb := themebit_for_name(name, c);
      end
      else
      begin
        name := '';
        tb := 0;
      end;
      if tb = 0 then
      begin
        announce(ERROR, Format('Unknown theme <%s> in comp.', [name]));
        if name <> '' then
          sc.UnGet;
        break;
      end;
      x^.compatible := x^.compatible or tb;
      continue;
    end;

    if (token = 'O') or (utoken = 'TOP') then
    begin
      if sc.GetString then
        name := sc._String
      else
        break;

      add_flat_cell(x, name, c);
      continue;
    end;

    if (token = 'A') or (utoken = 'PRIMARY') then
    begin
      if absorb_cell(x, sc, TRUE, c) then
        continue
      else
        break;
    end;

    if (token = 'B') or (utoken = 'SECONDARY') then
    begin
      if absorb_cell(x, sc, FALSE, c) then
        continue
      else
        break;
    end;

    sc.UnGet;
    break;
  end;
end;

// Absorb a Thing record from the config data, update script engine to the
// last token
procedure absorb_thing(const sc: TScriptEngine; const c: config_p);
var
  token: string;
  utoken: string;
  g: genus_p;
  tb: LongWord;
  name: string;
begin
  if not sc.GetString then
    exit;

  token := sc._String; // That's the number

  g := find_genus(c, atoi(token));

  if g^.compatible = ALL_THEMES then
    g^.compatible := 0;

  while true do
  begin
    if not sc.GetString then
      break;

    token := sc._String;
    utoken := UpperCase(token);

    if (token = 'c') or (utoken = 'COMP') then
    begin
      if sc.GetString then
      begin
        name := sc._String;
        tb := themebit_for_name(name, c);
      end
      else
      begin
        name := '';
        tb := 0;
      end;
      if tb = 0 then
      begin
        announce(ERROR, Format('Unknown theme <%s> in comp.', [name]));
        if name <> '' then
          sc.UnGet;
        break;
      end;
      g^.compatible := g^.compatible or tb;
      continue;
    end;

    sc.UnGet;
    break;
  end;
end;

// Look through the config's config file, and fill in values for
// all the non-switch lines therein.  Return FALSE if error.
// JVAL: rewritten using TScriptEngine()
function nonswitch_config(const c: config_p): boolean;
var
  sc: TScriptEngine;
  token, utoken: string;
begin
  sc := TScriptEngine.Create(c^.configdata);
  result := true;
  while sc.GetString do
  begin
    token := sc._String;
    utoken := UpperCase(token);
    if utoken = '[THEMES]' then
    begin
      repeat until not absorb_theme(sc, c);
    end
    else if (token = 't') or (utoken = 'TEXTURE') then
    begin
      absorb_texture(sc, c);
    end
    else if (token = 'f') or (utoken = 'FLAT') then
    begin
      absorb_flat(sc, c);
    end
    else if (token = 'x') or (utoken = 'CONSTRUCT') then
    begin
      absorb_construct(sc, c);
    end
    else if (token = '.') or (utoken = 'THING') then
    begin
      absorb_thing(sc, c);
    end
    else if (token = '#') or (utoken = 'HARDWIRED1') then
     hardwired_nonswitch_nontheme_config(c)
    else
    begin
      announce(ERROR, Format('Nonsensical token <%s> in config file.', [token]));
      result := false;
      exit;
    end;
  end;
end;

// Random parts for the style, based on the config, and other
// parts of the style.

// Return a random thing in the given style and config,
// satisfying the given pmask, in the given height-range.
// Ignore s if nil.
function random_thing0(const pmask: LongWord;const c: config_p; const s: style_p;
  const minh, maxh: integer): genus_p;
var
  tmask, tcount: integer;
begin
  if s <> nil then
    tmask := $01 shl s^.theme_number
  else
    tmask := 0;

  tcount := 0;

  result := c^.genus_anchor;
  while result <> nil do
  begin
    if result^.bits and pmask <> pmask then
    begin
      result := result^.next;
      continue;
    end;
    if s <> nil then
      if result^.compatible and tmask = 0 then
      begin
        result := result^.next;
        continue;
      end;
    if result^.gamemask and c^.gamemask <> c^.gamemask then
    begin
      result := result^.next;
      continue;
    end;
    if result^.height > maxh then
    begin
      result := result^.next;
      continue;
    end;
    if result^.height < minh then
    begin
      result := result^.next;
      continue;
    end;
    result := result^.next;
    inc(tcount);
  end;

  if tcount = 0 then
  begin
    announce(NONE, 'No compatible things for theme'); // This is OK
    result := nil;
    exit;
  end;

  tcount := 1 + roll(tcount);
  result := c^.genus_anchor;
  while result <> nil do
  begin
    if (result^.bits and pmask = pmask) and
       ((s = nil) or (result^.compatible and tmask <> 0)) and
       (result^.height <= maxh) and
       (result^.height >= minh) and
       (result^.gamemask and c^.gamemask = c^.gamemask) then
    begin
      dec(tcount);
      if tcount = 0 then
        exit;
    end;
    result := result^.next;
  end;
  result := nil;
end;

// Return a random flat in the given style and config,
// satisfying the given pmask.  If s is nil, ignore it.
function random_flat0(const pmask: LongWord; const c: config_p; const s: style_p): flat_p;
var
  fmask, fcount: integer;
begin
  if s <> nil then
    fmask := $01 shl s^.theme_number
  else
    fmask := 0;

  fcount := 0;
  result := c^.flat_anchor;
  while result <> nil do
  begin
    if result^.props and pmask <> pmask then
    begin
      result := result^.next;
      continue;
    end;
    if s <> nil then
      if result^.compatible and fmask = 0 then
      begin
        result := result^.next;
        continue;
      end;
    if result^.gamemask and c^.gamemask <> c^.gamemask then
    begin
      result := result^.next;
      continue;
    end;
    result := result^.next;
    inc(fcount);
  end;

  if fcount = 0 then
  begin
    announce(NONE, 'No compatible flats for theme'); // This is OK
    result := nil;
    exit;
  end;

  fcount := 1 + roll(fcount);
  result := c^.flat_anchor;
  while result <> nil do
  begin
    if (result^.props and pmask = pmask) and
       ((s = nil) or (result^.compatible and fmask <> 0)) and
       (result^.gamemask and c^.gamemask = c^.gamemask) then
    begin
      dec(fcount);
      if fcount = 0 then
        exit;
    end;
    result := result^.next;
  end;
  result := nil;
end;

function random_floor0(const c: config_p; const s: style_p): flat_p;
begin
  result := random_flat0(SL_FLOOR, c, s);
end;

function random_gate(const c: config_p; const s: style_p): flat_p;
begin
  result := random_flat0(GATE, c, s);
end;

function random_ceiling0(const c: config_p; const s: style_p): flat_p;
begin
  result := random_flat0(CEILING, c, s);
end;

function random_ceilinglight(const c: config_p; const s: style_p): flat_p;
begin
  result := random_flat0(CEILING + LIGHT, c, s);
end;

function random_nukage1(const c: config_p; const s: style_p): flat_p;
begin
  result := random_flat0(NUKAGE, c, s);
end;

function random_doorceiling(const c: config_p; const s: style_p): flat_p;
begin
  if rollpercent(50) then
    result := s^.ceiling0
  else
    result := random_ceiling0(c, s);
end;

function random_doorfloor(const c: config_p; const s: style_p): flat_p;
begin
  if rollpercent(50) then
    result := s^.floor0
  else
    result := random_floor0(c, s); // stub
end;

function random_stepfloor(const c: config_p; const s: style_p): flat_p;
begin
  result := random_doorfloor(c, s); // stub stub
end;

// Return a random texture in the given style and config,
// satisfying the given pmask.  If s is nil, ignore it.
function random_texture0(const pmask: LongWord; const c: config_p; const s: style_p): texture_p;
var
  tmask, tcount: integer;
begin
  if s <> nil then
    tmask := $01 shl s^.theme_number
  else
    tmask := 0;

  tcount := 0;
  result := c^.texture_anchor;
  while result <> nil do
  begin
    if result^.props and pmask <> pmask then
    begin
      result := result^.next;
      continue;
    end;
    if s <> nil then
      if result^.compatible and tmask = 0 then
      begin
        result := result^.next;
        continue;
      end;
    if result^.gamemask and c^.gamemask <> c^.gamemask then
    begin
      result := result^.next;
      continue;
    end;
    result := result^.next;
    inc(tcount);
  end;

  if tcount = 0 then
  begin
    announce(NONE, 'No compatible textures for theme'); // It's OK!
    result := nil;
    exit;
  end;

  tcount := 1 + roll(tcount);
  result := c^.texture_anchor;
  while result <> nil do
  begin
    if (result^.props and pmask = pmask) and
       ((s = nil) or (result^.compatible and tmask <> 0)) and
       (result^.gamemask and c^.gamemask = c^.gamemask) then
    begin
      dec(tcount);
      if tcount = 0 then
        exit;
    end;
    result := result^.next;
  end;
  result := nil;
end;

function random_support0(const c: config_p; const s: style_p): texture_p;
begin
  result := random_texture0(SUPPORT, c, s);
end;

function random_wall0(const c: config_p; const s: style_p): texture_p;
var
  tmask, tcount: integer;
begin
  if s <> nil then
    tmask := $01 shl s^.theme_number
  else
    tmask := 0;

  if rollpercent(80) then // Use a core wall texture
  begin
    tcount := 0;
    result := c^.texture_anchor;
    while result <> nil do
    begin
      if result^.props and WALL = 0 then
      begin
        result := result^.next;
        continue;
      end;
      if result^.core and tmask = 0 then
      begin
        result := result^.next;
        continue;
      end;
      if result^.gamemask and c^.gamemask <> c^.gamemask then
      begin
        result := result^.next;
        continue;
      end;
      inc(tcount);
      result := result^.next;
    end;

    if tcount = 0 then
    begin
      announce(WARNING, 'No core wall textures for theme');
      result := c^.error_texture;
      exit;
    end;

    tcount := 1 + roll(tcount);
    result := c^.texture_anchor;
    while result <> nil do
    begin
      if (result^.props and WALL <> 0) and
         (result^.core and tmask <> 0) and
         (result^.gamemask and c^.gamemask = c^.gamemask ) then
      begin
        dec(tcount);
        if tcount = 0 then
          exit;
      end;
      result := result^.next;
    end;
  end
  else // Use any compatible wall texture
  begin
    result := random_texture0(WALL, c, s);
    exit;
  end;
  result := nil;
end;

function random_kickplate(const c: config_p; const s: style_p): texture_p;
begin
  result := random_support0(c, s);
end;

function random_stepfront(const c: config_p; const s: style_p): texture_p;
begin
  if not rollpercent(c^.p_use_steps) then
    result := random_kickplate(c, s)
  else
    result := random_texture0(STEP, c, s);
  if result = nil then
    result := random_kickplate(c,s);
end;

function switch0_for(const c: config_p; const s: style_p): texture_p;
begin
  if s^.wall0^.switch_texture <> nil then
    result := s^.wall0^.switch_texture
  else
    result := random_texture0(SWITCH, c, s);
end;

function random_doorjamb(const c: config_p; const s: style_p): texture_p;
begin
  result := random_texture0(JAMB, c, s);
end;

function random_redface(const c: config_p; const s: style_p): texture_p;
begin
  result := random_texture0(RED, c, s);
end;

function random_blueface(const c: config_p; const s: style_p): texture_p;
begin
  result := random_texture0(BLUE, c, s);
end;

function random_yellowface(const c: config_p; const s: style_p): texture_p;
begin
  result := random_texture0(YELLOW, c, s);
end;

function random_walllight(const c: config_p; const s: style_p): texture_p;
begin
  result := random_texture0(LIGHT, c, s);
end;

function random_liftface(const c: config_p; const s: style_p): texture_p;
begin
  result := random_texture0(LIFT_TEXTURE, c, s);
end;

// should consult the lists in the config like the others do


// Return a door-face that looks good on a wide door.
function random_widedoorface(const c: config_p; const s: style_p): texture_p;
begin
  result := random_widedoorface_ex(c, s, FALSE);
end;

// Return a door-face that looks good on a wide door.  If needhigh,
// also require 128 high.
function random_widedoorface_ex(const c: config_p; const s: style_p;
  const needhigh: boolean): texture_p;
var
  tmask, tcount: integer;
begin
  if s <> nil then
    tmask := $01 shl s^.theme_number
  else
    tmask := 0;

  tcount := 0;
  result := c^.texture_anchor;
  while result <> nil do
  begin
    if result^.props and DOOR = 0 then
    begin
     result := result^.next;
     continue;
    end;
    if result^.props and GATE <> 0 then
    begin
      result := result^.next;
      continue;
    end;
    if result^.width < 128 then
    begin
      result := result^.next;
      continue;
    end;
    if needhigh and (result^.height < 128) then
    begin
      result := result^.next;
      continue;
    end;
    if result^.compatible and tmask = 0 then
    begin
      result := result^.next;
      continue;
    end;
    if result^.gamemask and c^.gamemask <> c^.gamemask then
    begin
      result := result^.next;
      continue;
    end;
    result := result^.next;
    inc(tcount);
  end;

  if tcount = 0 then
  begin
    announce(ERROR, 'No wide doorfaces for theme'); // Bad!
    result := c^.error_texture;
    exit;
  end;

  tcount := 1 + roll(tcount);
  result := c^.texture_anchor;
  while result <> nil do
  begin
    if (result^.props and DOOR <> 0) and
       (result^.props and GATE = 0) and
       (result^.width >= 128) and
       (not (needhigh and (result^.height < 128))) and
       (result^.compatible and tmask <> 0) and
       (result^.gamemask and c^.gamemask = c^.gamemask) then
    begin
      dec(tcount);
      if tcount = 0 then
        exit;
    end;
    result := result^.next;
  end;
  result := c^.error_texture;
end;

// Return a door-face that looks good on a narrow door.
function random_narrowdoorface(const c: config_p; const s: style_p): texture_p;
begin
  result := random_narrowdoorface_ex(c, s, FALSE);
end;

// Return a door-face that looks good on a wide door.  If needhigh,
// also require 128 high.
function random_narrowdoorface_ex(const c: config_p; const s: style_p;
  const needhigh: boolean): texture_p;
var
  tmask, tcount: integer;
begin
  if s <> nil then
    tmask := $01 shl s^.theme_number
  else
    tmask := 0;

  tcount := 0;
  result := c^.texture_anchor;
  while result <> nil do
  begin
    if result^.props and DOOR = 0 then
    begin
     result := result^.next;
     continue;
    end;
    if result^.props and GATE <> 0 then
    begin
      result := result^.next;
      continue;
    end;
    if result^.width >= 128 then
    begin
      result := result^.next;
      continue;
    end;
    if needhigh and (result^.height < 128) then
    begin
      result := result^.next;
      continue;
    end;
    if result^.compatible and tmask = 0 then
    begin
      result := result^.next;
      continue;
    end;
    if result^.gamemask and c^.gamemask <> c^.gamemask then
    begin
      result := result^.next;
      continue;
    end;
    result := result^.next;
    inc(tcount);
  end;

  if tcount = 0 then
  begin
    announce(ERROR, 'No narrow doorfaces for theme'); // Bad!
    result := c^.error_texture;
    exit;
  end;

  tcount := 1 + roll(tcount);
  result := c^.texture_anchor;
  while result <> nil do
  begin
    if (result^.props and DOOR <> 0) and
       (result^.props and GATE = 0) and
       (result^.width < 128) and
       (not (needhigh and (result^.height < 128))) and
       (result^.compatible and tmask <> 0) and
       (result^.gamemask and c^.gamemask = c^.gamemask) then
    begin
      dec(tcount);
      if tcount = 0 then
        exit;
    end;
    result := result^.next;
  end;
  result := c^.error_texture;
end;


// Looks good wide, and is 128 high.  Note this should only be called
// after the style's "widedoorface" has been set.
function random_twdoorface(const c: config_p; const s: style_p): texture_p;
begin
  if s^.widedoorface^.height >= 128 then
    result := s^.widedoorface
  else
    result := random_widedoorface_ex(c, s, TRUE);
end;

// Looks good narrow, and is 128 high.  Note this should only be called
// after the style's "narrowdoorface" has been set.
function random_tndoorface(const c: config_p; const s: style_p): texture_p;
begin
  if s^.narrowdoorface^.height >= 128 then
    result := s^.narrowdoorface
  else
    result := random_narrowdoorface_ex(c, s, TRUE);
end;

// Special texture to use (if it fits) on locked doors
// May return nil for a given style
function random_lockdoorface(const c: config_p; const s: style_p): texture_p;
var
  tmask, tcount: integer;
begin
  if s <> nil then
    tmask := $01 shl s^.theme_number
  else
    tmask := 0;

  tcount := 0;
  result := c^.texture_anchor;
  while result <> nil do
  begin
    if result^.props and DOOR = 0 then
    begin
      result := result^.next;
      continue;
    end;
    if result^.props and GATE = 0 then
    begin
      result := result^.next;
      continue;
    end;
    if result^.compatible and tmask = 0 then
    begin
      result := result^.next;
      continue;
    end;
    if result^.gamemask and c^.gamemask <> c^.gamemask then
    begin
      result := result^.next;
      continue;
    end;
    result := result^.next;
    inc(tcount);
  end;

  if tcount = 0 then
  begin
    announce(NONE, 'No locked doorfaces for theme'); // That's OK
    result := nil;
    exit;
  end;

  tcount := 1 + roll(tcount);
  result := c^.texture_anchor;
  while result <> nil do
  begin
    if (result^.props and DOOR <> 0) and
       (result^.props and GATE <> 0) and
       (result^.compatible and tmask <> 0) and
       (result^.gamemask and c^.gamemask = c^.gamemask) then
    begin
      dec(tcount);
      if tcount = 0 then
        exit;
    end;
    result := result^.next;
  end;
  result := nil;
end;

function random_grating(const c: config_p; const s: style_p): texture_p;
begin
  result := random_texture0(GRATING, c, s);
end;

function random_plaque(const c: config_p; const s: style_p): texture_p;
begin
  result := random_texture0(PLAQUE, c, s);
end;

// Return the angle (east is zero, north is ninety) of a thing
// that's standing in the linedef, facing right from it.
function facing_along(const x1, y1, x2, y2: integer): integer;
begin
  result := facing_right_from(x1, y1, x2, y2);
  if result = 270 then
    result := 0
  else
    result := result + 90;
end;

// Return the angle (east is zero, north is ninety) of a thing
// that's standing in the linedef, facing right from it.
function facing_right_from(const x1, y1, x2, y2: integer): integer;
begin
  // Best at right angles and axis-parallel lines

  if abs(x1 - x2) < abs(y1 - y2) then // More parallel to the Y-axis
  begin
    if y2 > y1 then // Up
      result := 0
    else // Down
      result := 180;
  end
  else // to the X-axis
  begin
    if x2 > x1 then // rightward
      result := 270
    else
      result := 90;
  end;
end;

// Return the angle (east is zero, north is ninety) of a thing
// that's standing in the linedef, facing right from it.
function facing_right_from_ld(const ld: linedef_p): integer;
begin
  // could be a macro
  result := facing_right_from(ld^.from^.x, ld^.from^.y, ld^._to^.x, ld^._to^.y);
end;

// Wall up the given clear-walled sector inside the given outer sector.
// Or if innersec is nil, put a procedure sector in there, and use the given
// texture on the walls.  If the four last pointers are given, returns
// the four linedefs it makes.
procedure frame_innersec_ex(const l: level_p; const oldsector, innersec: sector_p;
                      tm, tu, tl: texture_p;
                      const x1, y1, x2, y2, x3, y3, x4, y4: integer;
                      const c: config_p;
                      const ld1, ld2, ld3, ld4: linedef_pp);
var
  ld: linedef_p;
  v0, v1, v2: vertex_p;
  newflags: SmallInt;
begin
  if innersec <> nil then
     newflags := TWO_SIDED
  else
     newflags := 0;
  if tm = nil then
    tm := c^.null_texture;

  v1 := new_vertex(l, x1, y1);
  v0 := v1;
  v2 := new_vertex(l, x2, y2);
  ld := new_linedef(l, v2, v1);
  ld^.flags := ld^.flags or newflags;
  ld^.right := new_sidedef(l, oldsector, c);
  ld^.right^.isBoundary := FALSE;
  ld^.right^.middle_texture := tm;
  if innersec <> nil then
  begin
    ld^.left := new_sidedef(l, innersec, c);
    ld^.left^.middle_texture := tm;
    patch_upper(ld, tu, c);
    patch_lower(ld, tl, c);
  end;
  if ld1 <> nil then
    ld1^ := ld;

  v1 := v2;
  v2 := new_vertex(l, x3, y3);
  ld := new_linedef(l, v2, v1);
  ld^.flags := ld^.flags or newflags;
  ld^.right := new_sidedef(l, oldsector, c);
  ld^.right^.isBoundary := FALSE;
  ld^.right^.middle_texture := tm;
  if innersec <> nil then
  begin
    ld^.left := new_sidedef(l, innersec, c);
    ld^.left^.middle_texture := tm;
    patch_upper(ld, tu, c);
    patch_lower(ld, tl, c);
  end;
  if ld2 <> nil then
    ld2^ := ld;

  v1 := v2;
  v2 := new_vertex(l, x4, y4);
  ld := new_linedef(l, v2, v1);
  ld^.flags := ld^.flags or newflags;
  ld^.right := new_sidedef(l, oldsector, c);
  ld^.right^.isBoundary := FALSE;
  ld^.right^.middle_texture := tm;
  if innersec <> nil then
  begin
    ld^.left := new_sidedef(l, innersec, c);
    ld^.left^.middle_texture := tm;
    patch_upper(ld, tu, c);
    patch_lower(ld, tl, c);
  end;
  if ld3 <> nil then
    ld3^ := ld;

  v1 := v2;
  v2 := v0;
  ld := new_linedef(l, v2, v1);
  ld^.flags := ld^.flags or newflags;
  ld^.right := new_sidedef(l, oldsector, c);
  ld^.right^.isBoundary := FALSE;
  ld^.right^.middle_texture := tm;
  if innersec <> nil then
  begin
    ld^.left := new_sidedef(l, innersec, c);
    ld^.left^.middle_texture := tm;
    patch_upper(ld, tu, c);
    patch_lower(ld, tl, c);
  end;
  if ld4 <> nil then
    ld4^ := ld;

end; // frame_innersec_ex()

// The common axis-parallel case of frame_innersec.  If the four pointers
// are given, the first and third will be y-parallel, the second and
// fourth x-parallel.
procedure parallel_innersec_ex(const l: level_p; const oldsector, innersec: sector_p;
                               const tm, tu, tl: texture_p;
                               const minx, miny, maxx, maxy: integer; const c: config_p;
                               const ld1, ld2, ld3, ld4: linedef_pp);
begin
  frame_innersec_ex(l, oldsector, innersec, tm, tu, tl,
                    minx, miny, minx, maxy, maxx, maxy, maxx, miny, c,
                    ld1, ld2, ld3, ld4);
end;

// Your basic visual room-center embellishments
// Square in the middle of the room with higher/lower/no ceiling,
// possibly different floor, light level, nukage, etc.
function ceiling_effect(const l: level_p; const oldsector: sector_p;
  const ThisStyle: style_p; const haa: haa_p; const c: config_p): boolean;
var
  minx, miny, maxx, maxy, offset, deltah: integer;
  g: genus_p;
  thing_id: SmallInt;
  beamsize, maxbeam: integer;
  innersec: sector_p;
  force_nukage: boolean;
  force_sky: boolean;
  force_quad: boolean;
  edge_lights: boolean;
  center_light: boolean;
  upt: texture_p;
  xsize, ysize: integer;
  ld1, ld2, ld3, ld4: linedef_p;
  fancied: boolean;
  ld2new, ld4new, ldnew: linedef_p;
  newsec: sector_p;
  inner2: sector_p;
begin
  g := ThisStyle^.lamp0;
  if g^.height > oldsector^.ceiling_height - oldsector^.floor_height then
    g := ThisStyle^.shortlamp0;
  thing_id := g^.thingid;
  force_nukage := rollpercent(l^.p_force_nukage);
  force_sky := rollpercent(l^.p_force_sky);

  // Only do this sometimes!
  if not (rollpercent(5) or force_nukage or force_sky) then
  begin
    result := FALSE;
    exit;
  end;

  force_quad := rollpercent(15);
  edge_lights := FALSE;
  center_light := FALSE;
  upt := oldsector^.style^.wall0;


  // Find the inner sector corners
  find_sector_rectangle(l, oldsector, minx, miny, maxx, maxy);
  offset := maxx - minx;
  if maxy - miny < offset then offset := maxy - miny;
  if offset < 96 then
  begin
    result := FALSE; // No sense making a really teeny one
    exit;
  end;
  offset := 16 + roll((offset div 2) - 48);
  minx := minx + offset;
  miny := miny + offset;
  maxx := maxx - offset;
  maxy := maxy - offset;

  // Sometimes do four little effects
  if maxx - minx < 144 then
    force_quad := FALSE
  else if maxy - miny < 144 then
    force_quad := FALSE;
  maxbeam := maxx - minx - 128;
  if maxy - miny - 128 < maxbeam then
    maxbeam := maxy - miny - 128;
  beamsize := 16 + roll(maxbeam - 15);
  if beamsize > 64 then
    beamsize := 64;
  // In that case, almost always force sky or nukage (why?)
  if force_quad and not force_nukage and not force_sky then
  begin
    if rollpercent(45) then
      force_nukage := TRUE
    else if rollpercent(82) then
      force_sky := TRUE;
  end;

  // With the box effect, sometimes put a Thing or Things,
  // if there's room (don't want stuck monsters!)
  if rollpercent(50) then
    if maxx - minx > 170 then
      if maxy - miny > 170 then
      begin
        if rollpercent(80) then
          edge_lights := TRUE
        else
          center_light := TRUE;
        if force_quad and rollpercent(50) then
        begin
          edge_lights := TRUE;
          center_light := TRUE;
        end;
        if offset < (38 + 8 + g^.width) then
          edge_lights := FALSE;
        if force_quad and (beamsize < g^.width) then
          center_light := FALSE;
        if edge_lights and
           room_at(l, g, minx - 8, miny - 8, g^.width, c) and
           room_at(l, g, minx - 8, maxy + 8, g^.width, c) and
           room_at(l, g, maxx + 8, miny - 8, g^.width, c) and
           room_at(l, g, maxx + 8, maxy + 8, g^.width, c) then
        begin
          new_thing(l, minx - 8, miny - 8, 0, thing_id, 7, c);
          new_thing(l, minx - 8, maxy + 8, 0, thing_id, 7, c);
          new_thing(l, maxx + 8, miny - 8, 0, thing_id, 7, c);
          new_thing(l, maxx + 8, maxy + 8, 0, thing_id, 7, c);
          announce(VERBOSE, 'edgelights');
        end;
        if center_light and
            room_at(l, g, minx + (maxx - minx) div 2, miny + (maxy - miny) div 2, g^.width, c) then
        begin
          new_thing(l, minx + (maxx - minx) div 2, miny + (maxy - miny) div 2, 0, thing_id, 7, c);
          announce(VERBOSE, 'centerlight');
        end;
      end; // end if big enough square for lights */

  announce(VERBOSE, Format('Ceiling effect between (%d,%d) and (%d,%d).',
                     [minx, miny, maxx, maxy]));

  // Make the sector itself
  innersec := clone_sector(l, oldsector);

  if rollpercent(50) or force_sky then // Ceiling hole
  begin
    innersec^.ceiling_height := innersec^.ceiling_height + 16 * (1 + roll(3));
    innersec^.light_level := l^.outside_light_level - 20;  // Minus 20?
    innersec^.ceiling_flat := c^.sky_flat;
  end
  else // Just a difference
  begin
    innersec^.ceiling_flat := random_ceiling0(c, ThisStyle);
    deltah := 32 - roll(65);
    // Don't lower the ceiling too near a wall (door)!
    if (offset < 64) and (deltah < 0) then
      deltah := -deltah;
    innersec^.ceiling_height := innersec^.ceiling_height + deltah;
    if innersec^.ceiling_height - innersec^.floor_height < 64 then
      innersec^.ceiling_height := innersec^.floor_height + 64;
  end; // end not a hole

  // Fun recessed ceiling lights?
  if rollpercent(20) then
    if innersec^.ceiling_height > oldsector^.ceiling_height then
      if oldsector^.style^.walllight <> nil then
      begin
        if innersec^.ceiling_height < oldsector^.ceiling_height + 16 then
          innersec^.ceiling_height := oldsector^.ceiling_height + 16;
        upt := oldsector^.style^.walllight;
        if innersec^.ceiling_flat <> c^.sky_flat then
        begin
          innersec^.light_level := oldsector^.light_level + 20;
          if rollpercent(90) then
            innersec^.ceiling_flat := oldsector^.ceiling_flat;
        end;
        announce(VERBOSE, 'Indirect lighting');
      end;

  // If no light-level decision made yet, make one
  if (innersec^.ceiling_flat <> c^.sky_flat) and
     (upt = oldsector^.style^.wall0) then // Poor test!
  begin
    innersec^.light_level := oldsector^.light_level + roll(41) - 20;
    if innersec^.light_level < 100 then
      innersec^.light_level := 100; // Minlight?
  end;

  // If not open-air, maybe make the lights blink/flash
  if innersec^.ceiling_flat <> c^.sky_flat then
    if rollpercent(20) then
      case roll(4) of
        0: innersec^.special := RANDOM_BLINK;
        1: innersec^.special := SYNC_FAST_BLINK;
        2: innersec^.special := SYNC_SLOW_BLINK;
        3: innersec^.special := GLOW_BLINK;
      end; // end if light effects

  if force_nukage or rollpercent(30) then // Floor thing also
  begin
    innersec^.floor_flat := random_floor0(c, ThisStyle);
    deltah := 24 - (roll(49));
    // Don't raise the floor too near a wall (door)!
    if (offset < 64) and (deltah > 0) then
      deltah := -deltah;
    // and if forced nukage, always lower the floor
    if force_nukage and (deltah > 0) then
      deltah := -deltah;
    innersec^.floor_height := innersec^.floor_height + deltah;
    if innersec^.ceiling_height - innersec^.floor_height < 64 then
      innersec^.floor_height := innersec^.ceiling_height - 64;
    if oldsector^.ceiling_height - innersec^.floor_height < 64 then
      innersec^.floor_height := oldsector^.ceiling_height - 64;
    if innersec^.ceiling_height - oldsector^.floor_height < 64 then
      innersec^.ceiling_height := oldsector^.ceiling_height + 64;

    // And maybe some nukage! */
    if deltah < 0 then
      if force_nukage or rollpercent(30) then
      begin
        announce(VERBOSE, 'Nukage');
        innersec^.floor_flat := ThisStyle^.nukage1;
        innersec^.special := NUKAGE1_SPECIAL;
        haa^.haas[ITYTD].health := haa^.haas[ITYTD].health - 10;
        haa^.haas[HMP].health := haa^.haas[HMP].health - 5;
      end;
  end
  else
    deltah := 0; // Note we didn't do it

  // Make the necessary linedefs and sidedefs
  if force_quad then
  begin
    xsize := ((maxx - minx) - beamsize) div 2;
    ysize := ((maxy - miny) - beamsize) div 2;
    parallel_innersec(l, oldsector, innersec,
                      nil, upt, oldsector^.style^.wall0,
                      minx, miny, minx + xsize, miny + ysize, c);
    parallel_innersec(l, oldsector, innersec,
                      nil, upt, oldsector^.style^.wall0,
                      minx, maxy - ysize, minx + xsize, maxy, c);
    parallel_innersec(l, oldsector, innersec,
                      nil, upt, oldsector^.style^.wall0,
                      maxx - xsize, miny, maxx, miny + ysize, c);
    parallel_innersec(l, oldsector, innersec,
                      nil, upt, oldsector^.style^.wall0,
                      maxx - xsize, maxy - ysize, maxx, maxy, c);
  end
  else if (maxx - minx > 128) and (maxy - miny > 128) and
          ((maxx - minx) <= (2 * (maxy - miny))) and
          ((maxy - miny) <= (2 * (maxx - minx))) and
          rollpercent(10) then
  begin
    // A diamond!  Is this safe?  Not axis-parallel!
    announce(LOG, 'Diamond');
    frame_innersec(l, oldsector, innersec,
                   nil, upt, oldsector^.style^.wall0,
                   (minx + maxx) div 2, miny,
                   minx, (miny + maxy) div 2,
                   (minx + maxx) div 2, maxy,
                   maxx, (miny + maxy) div 2,
                   c);
  end
  else
  begin
    // Just an old fashioned square
    fancied := FALSE;
    parallel_innersec_ex(l, oldsector,innersec,
                         nil, upt, oldsector^.style^.wall0,
                         minx, miny, maxx, maxy, c,
                         @ld1, @ld2, @ld3, @ld4);
    // but maybe with fancy stuff!
    // Stairs to get out?
    if (deltah < 0) and not fancied and
        no_monsters_stuck_on(l, ld1) and
        no_monsters_stuck_on(l, ld2) and
        no_monsters_stuck_on(l, ld4) and
        rollpercent(l^.p_deep_baths) then
    begin
      xsize := maxx - minx;
      deltah := -24; // 24?
      innersec^.floor_height := oldsector^.floor_height + deltah;
      while xsize >= 128 * l^.hugeness do
      begin
        xsize := xsize - 48 * l^.hugeness;
        ld2new := ld2;
        ld2 := split_linedef(l, ld2, 48 * l^.hugeness, c); // 48?
        ld4new := split_linedef(l, ld4, xsize, c);
        newsec := clone_sector(l, innersec);
        newsec^.floor_flat := oldsector^.floor_flat;
        innersec^.floor_height := innersec^.floor_height + deltah;
        ld3^.left^.sector := newsec;
        ld2new^.left^.sector := newsec;
        ld4new^.left^.sector := newsec;
        ldnew := new_linedef(l, ld4new^.from, ld2new^._to);
        ldnew^.left := new_sidedef(l, innersec, c);
        ldnew^.right := new_sidedef(l, newsec, c);
        ldnew^.flags := ldnew^.flags or TWO_SIDED;
        ldnew^.right^.middle_texture := c^.null_texture;
        ldnew^.left^.middle_texture := c^.null_texture;
        patch_lower(ldnew, newsec^.style^.wall0, c);
        ld3 := ldnew;
        fancied := TRUE;
        if rollpercent(30) then break;
      end; // end loop-thing
      if innersec^.floor_flat <> ThisStyle^.nukage1 then
        if rollpercent(75) then
        begin
          innersec^.floor_flat := c^.water_flat;
          announce(LOG, 'Water pool');
        end;
      if fancied then
        announce(LOG, 'Bath');
    end;
    // Sometimes more layers!  How dangerous is this?
    if rollpercent(20) and not fancied and // Generalize "20"
       (maxx - minx > 128) and (maxy - miny > 128) and
       (innersec^.floor_flat <> ThisStyle^.nukage1) then
    begin
      deltah := 12 + roll(13); // Generalize this?
      if rollpercent(50) then
        deltah := -deltah;
      if (offset < 64) and (deltah > 0) then
        deltah := -deltah;
      while (maxx - minx > 128) and (maxy - miny > 128) do
      begin
        if innersec^.ceiling_height - oldsector^.floor_height - deltah < 64 then break;
        if innersec^.ceiling_height - innersec^.floor_height - deltah < 64 then break;
        if oldsector^.ceiling_height - innersec^.floor_height - deltah < 64 then break;
        inner2 := clone_sector(l, innersec);
        inner2^.special := innersec^.special;
        inner2^.floor_height := inner2^.floor_height + deltah;
        announce(VERBOSE, 'Sunk');
        minx := minx + 32; // Generalize "32"s
        maxx := maxx - 32;
        miny := miny + 32;
        maxy := maxy - 32;
        parallel_innersec(l, innersec, inner2,
                          nil, upt, oldsector^.style^.wall0,
                          minx, miny, maxx, maxy, c);
        innersec := inner2;
      end;
    end;
  end;

  result := TRUE;

end; // end ceiling_effect()

// Perhaps place a timely monster just to the right of the center
// of the givenly-ended line.  Update the haa if.
procedure righthand_monster(const l: level_p; const xa, ya, xb, yb: integer;
  const haa: haa_p; const c: config_p);
var
  m: genus_p;
  x1, y1, x, y, flags: integer;
  angle: SmallInt;
begin
  // See if the model wants a monster
  m := timely_monster(haa, c, @flags, rollpercent(l^.p_biggest_monsters), 1); // 1 correct?
  if m = nil then
    exit;

  // Figure out where we want it
  x1 := (xa + xb) div 2;
  y1 := (ya + yb) div 2;
  point_from(xa, ya, x1, y1, RIGHT_TURN, 1 + m^.width div 2, @x, @y);
  if not room_at(l, m, x, y, m^.width, c) then
    exit;

  // Fill in other details
  angle := facing_right_from(xa, ya, xb, yb); // Correct?
  if rollpercent(50) then
    flags := flags or $08; // deaf; how often?
  // And finally create it and update the haa
  new_thing(l, x, y, angle, m^.thingid, flags, c);
  update_haa_for_monster(haa, m, flags, 1, c); // 1 correct?
end; // end righthand_monster

// Stick in a pillar (or post).  Assumes rectangles lots.
// Now with sometimes monsters!
// Should shrink the candidate pillar, not just give up, if
// it finds an existing thing in the way.
procedure do_pillar(const l: level_p; const oldsector: sector_p;
  const ThisStyle: style_p; const haa: haa_p; const c: config_p);
var
  minx, miny, maxx, maxy: integer;
  xsize, ysize, xoff, yoff: integer;
  t: thing_p;
  t1: texture_p;
begin
  // Figure out where we might want to put it
  find_sector_rectangle(l, oldsector, minx, miny, maxx, maxy);
  // The room has to be >192 in each direction, for now
  // 64 for the pillar, and 64 on every side for monster checks
  if maxx - minx <= 192 then exit;
  if maxy - miny <= 192 then exit;
  // random sizes within allowable range
  xsize := 64 + roll((maxx - minx) - 192);
  ysize := 64 + roll((maxy - miny) - 192);
  if rollpercent(50) then
    if xsize > 127 then
      if ysize > 127 then
      begin
        xsize := 128;
        ysize := 128;
      end;
  // Now the offsets from min.  Hmm...
  if ThisStyle^.center_pillars then
  begin
    xoff := 64 + (((maxx - minx) - 128) - xsize ) div 2;
    yoff := 64 + (((maxy - miny) - 128) - ysize ) div 2;
  end
  else
  begin
    xoff := 64 + roll(((maxx - minx) - 128) - xsize);
    yoff := 64 + roll(((maxy - miny) - 128) - ysize);
  end;
  // Now we have the corners of the candidate pillar
  // but go out an extra 64 for the check
  minx := minx + xoff - 64;
  miny := miny + yoff - 64;
  maxx := minx + xsize + 128; // Should be 64?
  maxy := miny + ysize + 128; // Should be 64?
  // Now we need to see if any Thing is in the area
  t := l^.thing_anchor;
  while t <> nil do
  begin
    if (t^.x >= minx) and (t^.x <= maxx) and
       (t^.y >= miny) and (t^.y <= maxy) then
    begin
      announce(VERBOSE, 'Too many things for a pillar');
      exit;
    end;
    t := t^.next;
  end;
  // None!  A miracle.  Define the space.
  t1 := ThisStyle^.wall0;
  if rollpercent(80) then
    t1 := random_wall0(c, ThisStyle);
  if xsize = 128 then
    if ysize = 128 then
      if oldsector^.ceiling_height - oldsector^.floor_height = 128 then
        t1 := ThisStyle^.plaque;
  minx := minx + 64;
  miny := miny + 64;
  maxx := maxx - 64;
  maxy := maxy - 64;
  if ThisStyle^.do_constructs then
    install_construct(l, oldsector, minx, miny, maxx, maxy, ThisStyle, c)
  else
    parallel_innersec(l, oldsector, nil, t1, nil, nil, minx, miny, maxx, maxy, c);
  announce(VERBOSE, 'Made a pillar');

  // Consider putting some monsters around it
  righthand_monster(l, minx, maxy, minx, miny, haa, c);
  righthand_monster(l, minx, miny, maxx, miny, haa, c);
  righthand_monster(l, maxx, miny, maxx, maxy, haa, c);
  righthand_monster(l, maxx, maxy, minx, maxy, haa, c);
  haa_unpend(haa);

  // Whew, I guess that's all!

end; // end do_pillar

// Does this construct fit into this sector, on sides of these
// sizes, in this style?
function construct_fits(const cs: construct_p; const xsize, ysize: integer;
  const s: sector_p; const ThisStyle: style_p; const c: config_p): boolean;
var
  good_primary: boolean;
  x_fit: boolean;
  y_fit: boolean;
  tc: texture_cell_p;
begin
  // Needs to be room between the floor and ceiling
  if cs^.height > s^.ceiling_height - s^.floor_height then
  begin
    result := FALSE;
    exit;
  end;

  // Needs to be in the right family
  if cs^.family <> ThisStyle^.construct_family then
  begin
    result := FALSE;
    exit;
  end;

  good_primary := FALSE;
  x_fit := FALSE;
  y_fit := FALSE;

  // Need to have at least one primary texture that can fit on
  // one side, and at least one texture of any kind that can
  // fit on each side.
  tc := cs^.texture_cell_anchor;
  while tc <> nil do
  begin
    if tc^.texture^.gamemask and c^.gamemask = c^.gamemask then
    begin
      if tc^.width <= xsize then
      begin
        x_fit := TRUE;
        if tc^.primary then
          good_primary := TRUE;
      end;
      if tc^.width <= ysize then
      begin
        y_fit := TRUE;
        if tc^.primary then
          good_primary := TRUE;
      end;
    end;
    tc := tc^.next;
  end;
  result := x_fit and y_fit and good_primary;
end;

// Return a texture-cell from the given construct that
// fits the given size.  If accept_secondaries, then do
function fitting_tc(const cs: construct_p; const size: integer;
  const accept_secondaries:  boolean; const c: config_p): texture_cell_p;
var
  tc1: texture_cell_p;
  ccount: integer;
begin
  tc1 := cs^.texture_cell_anchor;
  while tc1 <> nil do
  begin
    tc1^.marked := FALSE;
    tc1 := tc1^.next;
  end;

  ccount := 0;
  tc1 := cs^.texture_cell_anchor;
  while tc1 <> nil do
  begin
    if (accept_secondaries or tc1^.primary) and (tc1^.width <= size) and
       (tc1^.texture^.gamemask and c^.gamemask = c^.gamemask) then
    begin
      tc1^.marked := TRUE;
      inc(ccount);
    end;
    tc1 := tc1^.next;
  end;

  result := nil;
  if ccount > 0 then
  begin
    ccount := roll(ccount);
    tc1 := cs^.texture_cell_anchor;
    while tc1 <> nil do
    begin
      if tc1^.marked then
      begin
        tc1^.marked := FALSE;
        if ccount = 0 then
          result := tc1
        else
          dec(ccount);
      end;
      tc1 := tc1^.next;
    end;
  end;

end;

// Install, if possible, some construct that fits the style,
// in the given place.
function install_construct(const l: level_p; oldsector: sector_p;
                          const minx, miny, maxx, maxy: integer;
                          const ThisStyle: style_p; const c: config_p): boolean;
var
  cs, cs2: construct_p;
  innersec: sector_p;
  ccount: integer;
  floor_to_ceiling, primary_on_x: boolean;
  ld1, ld2, ld3, ld4: linedef_p;
  tc1, tc2, tc3, tc4, tcp: texture_cell_p;
  fc: flat_cell_p;
  xsize, ysize, mult: integer;
begin
  // Mark just those constructs that fit
  cs := c^.construct_anchor;
  while cs <> nil do
  begin
    cs^.marked := FALSE;
    cs := cs^.next;
  end;

  ccount := 0;
  primary_on_x := FALSE;

  cs := c^.construct_anchor;
  while cs <> nil do
  begin
    if construct_fits(cs, maxx - minx, maxy - miny, oldsector, ThisStyle, c) then
    begin
      cs^.marked := TRUE;
      inc(ccount);
    end;
    cs := cs^.next;
  end;

  if ccount = 0 then
  begin
    result := FALSE; // Give up if none
    exit;
  end;

  // Otherwise pick a random marked one
  ccount := roll(ccount);
  cs := c^.construct_anchor;
  while cs <> nil do
  begin
    if cs^.marked then
    begin
      if ccount = 0 then break;
      dec(ccount);
    end;
    cs := cs^.next;
  end;

  // Clean up
  cs2 := c^.construct_anchor;
  while cs2 <> nil do
  begin
    cs2^.marked := FALSE;
    cs2 := cs2^.next;
  end;

  floor_to_ceiling := oldsector^.ceiling_height - oldsector^.floor_height = cs^.height;

  // Iff we need an innersec, make sure we have one
  if not floor_to_ceiling then
  begin
    innersec := clone_sector(l, oldsector);
    // Set the top of the object
    ccount := 0;
    fc := cs^.flat_cell_anchor;
    while fc <> nil do
    begin
      inc(ccount);
      fc := fc^.next;
    end;
    ccount := roll(ccount);
    fc := cs^.flat_cell_anchor;
    while fc <> nil do
    begin
      if ccount = 0 then break;
      dec(ccount);
      fc := fc^.next;
    end;
    innersec^.floor_flat := fc^.flat;
    innersec^.light_level := oldsector^.light_level;
    innersec^.ceiling_height := oldsector^.ceiling_height;
    innersec^.floor_height := oldsector^.floor_height + cs^.height;
  end
  else
    innersec := nil;

  // Pick a primary texture (cell)
  tcp := nil;
  if rollpercent(50) then // Try X first
  begin
    tcp := fitting_tc(cs, maxy - miny, FALSE, c);
    if tcp <> nil then
      primary_on_x := TRUE;
  end;
  if tcp = nil then // Nothing yet, try Y
  begin
    tcp := fitting_tc(cs, maxx - minx, FALSE, c);
    if tcp <> nil then
      primary_on_x := FALSE;
  end;
  if tcp = nil then // Nothing yet, try X (again)
  begin
    tcp := fitting_tc(cs, maxy - miny, FALSE, c);
    if tcp <> nil then
      primary_on_x := TRUE;
  end;
  if tcp = nil then // Impossible!
  begin
    announce(WARNING, 'Some impossible error in construct-construction.');
    result := FALSE;
    exit;
  end;

  // Set all four cells
  if primary_on_x then
  begin
    if rollpercent(50) then
    begin
      tc2 := tcp;
      tc4 := fitting_tc(cs, maxy - miny, TRUE, c);
    end
    else
    begin
      tc4 := tcp;
      tc2 := fitting_tc(cs, maxy - miny, TRUE, c);
    end;
    tc1 := fitting_tc(cs, maxx - minx, TRUE, c);
    tc3 := fitting_tc(cs, maxx - minx, TRUE, c);
  end
  else
  begin
    if rollpercent(50) then
    begin
      tc1 := tcp;
      tc3 := fitting_tc(cs, maxx - minx, TRUE, c);
    end
    else
    begin
      tc3 := tcp;
      tc1 := fitting_tc(cs, maxx - minx, TRUE, c);
    end;
    tc2 := fitting_tc(cs, maxy - miny, TRUE, c);
    tc4 := fitting_tc(cs, maxy - miny, TRUE, c);
  end;

  // Now decide how large the X and Y dimensions should actually be
  xsize := tc1^.width;
  if tc3^.width > xsize then
    xsize := tc3^.width;
  mult := (maxx - minx) div xsize;
  if mult > 4 then
    mult := 4; // Not too huge!
  if rollpercent(50) then
    mult := 1 + roll(mult);
  if mult > 0 then // JVAL
    xsize := xsize * mult;

  ysize := tc2^.width;
  if tc4^.width > ysize then
    ysize := tc4^.width;
  mult := (maxy - miny) div ysize;
  if mult > 4 then
    mult := 4; // Not too huge!
  if rollpercent(50) then
    mult := 1 + roll(mult);
  if mult > 0 then // JVAL
    ysize := ysize * mult;

  // Finally!  Make the sector
  parallel_innersec_ex(l, oldsector, innersec, nil, nil, nil,
                       minx, miny, minx + xsize, miny + ysize, c,
                       @ld2, @ld1, @ld4, @ld3);

  // And fix up the linedefs
  if floor_to_ceiling then
  begin
    ld1^.right^.middle_texture := tc1^.texture;
    ld2^.right^.middle_texture := tc2^.texture;
    ld3^.right^.middle_texture := tc3^.texture;
    ld4^.right^.middle_texture := tc4^.texture;
  end
  else
  begin
    ld1^.right^.middle_texture := c^.null_texture;
    ld2^.right^.middle_texture := c^.null_texture;
    ld3^.right^.middle_texture := c^.null_texture;
    ld4^.right^.middle_texture := c^.null_texture;
    ld1^.right^.lower_texture := tc1^.texture;
    ld2^.right^.lower_texture := tc2^.texture;
    ld3^.right^.lower_texture := tc3^.texture;
    ld4^.right^.lower_texture := tc4^.texture;
    ld1^.flags := ld1^.flags and not LOWER_UNPEGGED;
    ld2^.flags := ld2^.flags and not LOWER_UNPEGGED;
    ld3^.flags := ld3^.flags and not LOWER_UNPEGGED;
    ld4^.flags := ld4^.flags and not LOWER_UNPEGGED;
  end;

  if rollpercent(50) then
    ld1^.right^.y_offset := tc1^.y_offset1
  else
    ld1^.right^.y_offset := tc1^.y_offset2;

  if rollpercent(50) then
    ld2^.right^.y_offset := tc2^.y_offset1
  else
    ld2^.right^.y_offset := tc2^.y_offset2;

  if rollpercent(50) then
    ld3^.right^.y_offset := tc3^.y_offset1
  else
    ld3^.right^.y_offset := tc3^.y_offset2;

  if rollpercent(50) then
    ld4^.right^.y_offset := tc4^.y_offset1
  else
    ld4^.right^.y_offset := tc4^.y_offset2;

  announce(VERBOSE, 'Construct');
  result := TRUE;

end; // end install_construct

// Put in a single pillarish thing, with a much cleverer
// algorithm than do_pillar.  Use texture t if given,
// else if nil use a random one, or sometimes use a
// plaque texture.  If innersec is not null, use that
// inside the pillar (else void).
function do_new_pillar(const l: level_p; const oldsector, innersec: sector_p;
  t1: texture_p; const ThisStyle: style_p; const haa: haa_p; const c: config_p): boolean;
var
  minx, miny, maxx, maxy, tx, ty: integer;
  t: thing_p;
  v: vertex_p;
  ld: linedef_p;
  tm: texture_p;
begin
  // Initialize the 64-enclosing range
  find_sector_rectangle(l, oldsector, minx, miny, maxx, maxy);

  // The room has to be >192 in each direction, for now
  // 64 for the pillar, and 64 on every side for monster checks
  if maxx - minx <= 192 then
  begin
    result := FALSE;
    exit;
  end;
  if maxy - miny <= 192 then
  begin
    result := FALSE;
    exit;
  end;

  // Pick a point we'd like the pillar to contain,
  // to guide the following algorithm
  tx := minx + 1 + roll(maxx - (minx + 1));
  ty := miny + 1 + roll(maxy - (miny + 1));
  // If that point is inside some existing pillar, fail
  if oldsector <> point_sector(l, tx, ty, nil, nil) then
  begin
    result := FALSE;
    exit;
  end;

  // For each vertex, if the vertex is in the current range,
  // shrink the range so it misses the vertex, but still
  // contains the t point.
  v := l^.vertex_anchor;
  while v <> nil do
  begin
    if infinity_norm(tx, ty, v^.x, v^.y) < 64 then
    begin
      result := FALSE; // Failure!
      exit;
    end;
    if (v^.x < minx) or (v^.x > maxx) or (v^.y < miny) or (v^.y > maxy) then
    begin
      v := v^.next;
      continue;
    end;
    if v^.x > tx then
      maxx := v^.x - 1
    else
      minx := v^.x + 1;
    if v^.y > ty then
      maxy := v^.y - 1
    else
      miny := v^.y + 1;
    v := v^.next;
  end;

  // and the same for each thing, although in fact the requirement
  // to be 64 away from even pickables is overconservative
  t := l^.thing_anchor;
  while t <> nil do
  begin
    if infinity_norm(tx, ty, t^.x, t^.y) < 64 then
    begin
      result := FALSE; // Failure!
      exit;
    end;
    if (t^.x<minx) or (t^.x > maxx) or (t^.y < miny) or (t^.y > maxy) then
    begin
      t := t^.next;
      continue;
    end;
    if t^.x > tx then
      maxx := t^.x - 1
    else
      minx := t^.x + 1;
    if t^.y > ty then
      maxy := t^.y - 1
    else
      miny := t^.y + 1;
    t := t^.next;
  end;

  // Now reduce the enclosing range
  minx := minx + 64;
  maxx := maxx - 64;
  miny := miny + 64;
  maxy := maxy - 64;
  if minx >= maxx - 15 then
  begin
    result := FALSE;
    exit;
  end;
  if miny >= maxy - 15 then
  begin
    result := FALSE;
    exit;
  end;
  // See if the result has any nasty intersections
  ld := l^.linedef_anchor;
  while ld <> nil do
  begin
    if intersects(minx, miny, minx, maxy, ld^.from^.x, ld^.from^.y, ld^._to^.x, ld^._to^.y) then
    begin
      result := FALSE;
      exit;
    end;
    if intersects(minx, maxy, maxx, maxy, ld^.from^.x, ld^.from^.y, ld^._to^.x, ld^._to^.y) then
    begin
      result := FALSE;
      exit;
    end;
    if intersects(maxx, maxy, maxx, miny, ld^.from^.x, ld^.from^.y, ld^._to^.x, ld^._to^.y) then
    begin
      result := FALSE;
      exit;
    end;
    if intersects(maxx, miny, minx, miny, ld^.from^.x, ld^.from^.y, ld^._to^.x, ld^._to^.y) then
    begin
      result := FALSE;
      exit;
    end;
    ld := ld^.next;
  end;

  // If we made it this far, we found room!
  // Now decide how much to use (i.e. should sometimes shrink/narrow here)
  // and finally make the pillar (or whatever!)
  // Perhaps a construct
  if ThisStyle^.do_constructs then
    install_construct(l, oldsector, minx, miny, maxx, maxy, ThisStyle, c)
  else
  begin
    if t1 = nil then
      t1 := random_wall0(c, ThisStyle);
    // Sometimes do a special plaque thing
    if (innersec = nil) and (maxx - minx >= 128) and (maxy - miny >= 128) and
         (oldsector^.ceiling_height - oldsector^.floor_height = 128) then
    begin
      minx := minx + ((maxx - minx) - 128) div 2;
      maxx := minx + 128;
      miny := miny + ((maxy - miny) - 128) div 2;
      maxy := miny + 128;
      t1 := ThisStyle^.plaque;
      announce(VERBOSE, 'Plaque-pillar');
    end;
    if innersec <> nil then
    begin
      announce(VERBOSE, 'Inner pillar');
      tm := nil;
    end
    else
      tm := t1;
    parallel_innersec(l, oldsector, innersec, tm, t1, t1, minx, miny, maxx, maxy, c);
    announce(VERBOSE, 'New pillar');
  end;

  // Consider putting some monsters around it
  if rollpercent(50) then righthand_monster(l, minx, maxy, minx, miny, haa, c);
  if rollpercent(50) then righthand_monster(l, minx, miny, maxx, miny, haa, c);
  if rollpercent(50) then righthand_monster(l, maxx, miny, maxx, maxy, haa, c);
  if rollpercent(50) then righthand_monster(l, maxx, maxy, minx, maxy, haa, c);
  haa_unpend(haa);

  result := TRUE;

end; // end do_new_pillar

// Put in a bunch of pillarish things, with a much cleverer
// algorithm than do_pillar.
// No monsters at present
procedure do_new_pillars(const l: level_p; const oldsector: sector_p;
  const ThisStyle: style_p; const haa: haa_p; const c: config_p);
var
   want, tried, delta: integer;
   t: texture_p;
   s: sector_p;
begin
   t := nil;
   s := nil;

   case roll(4) of
     0,
     1: t := ThisStyle^.wall0;
     2: t := random_wall0(c, ThisStyle);
   end;

   // Number of tries should really be sensitive to
   // room-size or something, eh?
   case roll(3) of
     0: want := 1;
     1: want := 20;
     2: want := 1 + roll(6);
   else
     want := 40; // Impossible!
   end;

   // Decide if the pillars should have a non-procedure inside sector
   if not ThisStyle^.do_constructs and rollpercent(100) then // 100?
   begin
     s := clone_sector(l, oldsector);
     if rollpercent(30) then
     begin
       s^.light_level := s^.light_level + 30 - roll(61);
       if s^.light_level > 240 then s^.light_level := 240;
       if s^.light_level < 80 then s^.light_level := 80;
     end;
     if rollpercent(80) then
     begin
       delta := roll((s^.ceiling_height - 32) - s^.floor_height);
       if delta > 128 then delta := 128;
       s^.floor_height := s^.floor_height + delta;
     end
     else if rollpercent(50) then
       s^.ceiling_height := s^.ceiling_height - roll((s^.ceiling_height - 32) - s^.floor_height)
     else
     begin
       delta := roll((s^.ceiling_height - 32) - s^.floor_height);
       if delta > 128 then delta := 128;
       s^.floor_height := s^.floor_height + delta;
       s^.ceiling_height := s^.ceiling_height - roll((s^.ceiling_height - 32) - s^.floor_height);
     end;
   end;

   tried := 0;
   while (tried < 100) and (want > 0) do
   begin
     if do_new_pillar(l, oldsector, s, t, ThisStyle, haa, c) then dec(want);
     inc(tried);
   end;
end;

// Put some appropriate monster(s) and bonus(es) along the right
// side of the given linedef.  Adjust the haa (haa adjustment
// assumes that ITYTD doesn't find the bonuses, and HMP only
// finds them half the time, if <secret> is true).
procedure populate_linedef(const l: level_p; const ldnew2: linedef_p;
  const haa: haa_p;const c: config_p; const secret: boolean);
var
  x, y, x1, y1: integer;
  bonustype: SmallInt;
  bonusamount: integer;
  m: genus_p;
  levels, farness, plen: integer;
  angle: SmallInt;
begin
  point_from(ldnew2^.from^.x, ldnew2^.from^.y, ldnew2^._to^.x, ldnew2^._to^.y,
             RIGHT_TURN, 32, @x1, @y1); // "32" should be improved
  plen := linelen(ldnew2);
  case roll(4) of
    1: farness := plen - 32;
    2: farness := plen div 2;
    3: farness := 32 + roll(plen - 63);
  else
    farness := 32;
  end;
  point_from(ldnew2^._to^.x, ldnew2^._to^.y, x1, y1, RIGHT_TURN, farness, @x, @y);
  // pick a prize; stubby
  bonustype := ID_POTION; // Just in case!
  bonusamount := 1; // JVAL: The same
  if rollpercent(50) then // Health or whatever
  begin
    case roll(4) of
      0:
        begin
          bonustype := ID_MEDIKIT;
          bonusamount := 25;
        end;
      1:
        begin
          bonustype := ID_MEDIKIT;
          bonusamount := 25;
        end;
      2:
        begin
          bonustype := ID_STIMPACK;
          bonusamount := 10;
        end;
      3:
        begin
          if not l^.seen_suit and rollpercent(l^.p_force_nukage) then
          begin
            bonustype := ID_SUIT;
            bonusamount := 10; // Guess
            l^.seen_suit := TRUE;
          end
          else if not l^.seen_map and rollpercent(30) then
          begin
            bonustype := ID_MAP;
            bonusamount := 0;
            l^.seen_map := TRUE;
            announce(VERBOSE, 'Area map');
          end
          else
          begin
            bonustype := ID_INVIS;
            bonusamount := 10; // Also guess
          end;
        end;
    end; // case
    // We assume ITYTD didn't find the closet!
    haa^.haas[1].health := haa^.haas[1].health + bonusamount / 2; // and HMP might not have
    haa^.haas[2].health := haa^.haas[2].health + bonusamount;
    if not secret then // Unless it's not a secret
    begin
      haa^.haas[0].health := haa^.haas[0].health + bonusamount;
      haa^.haas[1].health := haa^.haas[1].health + bonusamount / 2;
    end
  end
  else // Some ammo or whatever
  begin
    if haa^.haas[0].can_use_cells and rollpercent(20) then
    begin
      bonustype := ID_CELLPACK;
      bonusamount := 2000; // yow!
    end
    else if haa^.haas[0].can_use_rockets and rollpercent(20) then
    begin
      bonustype := ID_ROCKBOX;
      bonusamount := 500;
    end
    else if not haa^.haas[2].has_chainsaw and rollpercent(20) then
    begin
      bonustype := ID_CHAINSAW;
      bonusamount := 0;
      haa^.haas[2].has_chainsaw := TRUE;
    end
    else if rollpercent(2) then
    begin
      bonustype := ID_CHAINSAW;
      bonusamount := 0;
      haa^.haas[2].has_chainsaw := TRUE;
    end
    else
    begin
      case roll(3) of
        1:
          begin
            bonustype := ID_SHELLBOX;
            bonusamount := 1400;
          end;
        2:
          begin
            bonustype := ID_BACKPACK;
            bonusamount := 380;
            if haa^.haas[1].can_use_rockets then
              bonusamount := bonusamount + 100;
            if haa^.haas[1].can_use_cells then
              bonusamount := bonusamount + 400;
            haa^.haas[1].has_backpack := TRUE;
            haa^.haas[2].has_backpack := TRUE;
          end;
      else
        begin
          bonustype := ID_BULBOX;
          bonusamount := 500;
        end;
      end; // end switch
    end;
    // We assume ITYTD didn't find the closet!
    haa^.haas[1].ammo := haa^.haas[1].ammo + bonusamount / 2; // And HMP only prolly did
    haa^.haas[2].ammo := haa^.haas[2].ammo + bonusamount;
    if not secret then // Unless it's not a secret
    begin
      haa^.haas[0].ammo := haa^.haas[0].ammo + bonusamount;
      haa^.haas[1].ammo := haa^.haas[1].ammo + bonusamount / 2;
    end;
    // Account for chainsaws; primitive
    if bonustype = ID_CHAINSAW then
    begin
      haa^.haas[1].has_chainsaw := TRUE; // OK?
      haa^.haas[2].has_chainsaw := TRUE;
    end;
  end; // end ammo bonuses
  new_thing(l, x, y, 0, bonustype, 7, c); // Place the bonus
  // Now monsters!
  if (not secret or c^.secret_monsters) and rollpercent(90) then
  begin
    farness := 32; // mwidth here
    point_from(ldnew2^._to^.x, ldnew2^._to^.y, x1, y1, RIGHT_TURN, farness, @x, @y);
    while true do
    begin
      m := timely_monster(haa, c, @levels, rollpercent(l^.p_biggest_monsters), 0);
      if m <> nil then
      begin
        angle := facing_right_from_ld(ldnew2);
        new_thing(l, x, y, angle, m^.thingid, levels, c); // not deaf
        update_haa_for_monster(haa, m, levels, 0, c); // zero?  one?
        // Note that for monster purposes, ALL levels find the closet!
      end
      else
        break;
      farness := farness + 64; // Should be mwidths stuff here
      if farness + 32 > plen then
        break;
      point_from(ldnew2^._to^.x, ldnew2^._to^.y, x1, y1, RIGHT_TURN, farness, @x, @y);
    end; // end forever while monsters and space
    haa_unpend(haa);
  end; // end roll for having a monster
end;


// Put a secret closet behind the given linedef, with something
// or other of interest in it.  Doesn't do anything about hints.
// If tag is -1, makes the door faces normal doors.  Otherwise,
// makes the inner doorface a door, but leaves the outer one
// functionless, and tags the door sector with the tag.
// h is the height of the closet, unless it's zero in which case
// the existing height is used.  Returns nil for failure, or
// the linedef of the far wall of the closet.  If a haa is
// given, will also populate the closet.  Use the given
// ceiling_height (ch) for y-alignment.
function secret_closet(const l: level_p; const ld: linedef_p;
  const ThisStyle: style_p; const h: integer; const haa: haa_p; const c: config_p;
  const inside_sr: boolean; const tag: integer; const ch: SmallInt;
  const secret: boolean): linedef_p;
var
  ldnew, ldnew2, ldedge1, ldedge2: linedef_p;
  s: sector_p;
  doortype: SmallInt;
  offs: SmallInt;
begin
  if not empty_left_side(l, ld, 72) then
  begin
    result := nil; // Room?
    exit;
  end;

  doortype := LINEDEF_NORMAL_DOOR;
  if (DOOM0_BIT and c^.gamemask = 0) and rollpercent(80) then
    doortype := LINEDEF_BLAZE_DOOR;

  // Modify the outermost linedef to be doory
  ld^.right^.upper_texture := ld^.right^.middle_texture; // Door face
  ld^.flags := ld^.flags or SECRET_LINEDEF;
  if tag = -1 then
    ld^.typ := doortype;
  // Correct?
  ld^.right^.y_offset := 128 -
    (ld^.right^.sector^.ceiling_height - ld^.right^.sector^.floor_height);
  // Make the door sector itself -- "8" should be variable
  ldnew := lefthand_box_ext(l, ld, 8, ThisStyle, c, @ldedge1, @ldedge2);
  if tag <> -1 then
    ldnew^.right^.sector^.tag := tag;
  ldedge1^.flags := ldedge1^.flags or LOWER_UNPEGGED;
  ldedge2^.flags := ldedge2^.flags or LOWER_UNPEGGED;
  offs := ch - ldedge1^.right^.sector^.floor_height;;
  ldedge1^.right^.y_offset := offs;
  ldedge2^.right^.y_offset := offs;

  // Make the closet sector -- "64" should be variable
  ldnew2 := lefthand_box_ext(l, ldnew, 64, ThisStyle, c, @ldedge1, @ldedge2);
  if h <> 0 then
    ldnew2^.right^.sector^.ceiling_height := ldnew2^.right^.sector^.floor_height + h;
  offs := ch - ldnew2^.right^.sector^.ceiling_height;
  ldedge1^.right^.y_offset := offs;
  ldedge2^.right^.y_offset := offs;
  ldnew2^.right^.y_offset := offs;

  // Finish making the door doory
  s := ldnew^.right^.sector;
  flip_linedef(ldnew);
  if secret then
    s^.special := SECRET_SECTOR;
  if inside_sr then
    ldnew^.typ := doortype // reopenable
  else
    ldnew^.typ := LINEDEF_NORMAL_S1_DOOR; // Triggered doors never close
  s^.ceiling_height := s^.floor_height;
  s^.light_level := ThisStyle^.doorlight0;
  ldnew^.right^.upper_texture := ThisStyle^.support0;
  ld^.flags := ld^.flags or BLOCK_SOUND; // Always?
  ldnew^.flags := ldnew^.flags or BLOCK_SOUND;
  // and polish up the closet
  ldnew2^.right^.middle_texture := ThisStyle^.wall0;
  s := ldnew2^.right^.sector;
  if s^.light_level > 160 then
    s^.light_level := 160;
  if s^.style^.ceilinglight <> nil then
    if c^.clights then
    begin
      s^.ceiling_flat := s^.style^.ceilinglight;
      announce(VERBOSE, 'ccl');
      make_lighted(l,s,c);
    end;

  // Sometimes a nukage floor on triggered ones, just
  // for fun.  Note that these aren't SECRET anymore.
  if (tag <> -1) and (rollpercent(10) or rollpercent(l^.p_force_nukage)) then
  begin
    s^.floor_height := s^.floor_height - 8;
    patch_lower(ldnew, ldnew^.right^.upper_texture, c);
    s^.floor_flat := ThisStyle^.nukage1;
    s^.special := NUKAGE1_SPECIAL;
  end;

  if s^.special = SECRET_SECTOR then
    inc(l^.secret_count);

  if haa <> nil then
    populate_linedef(l, ldnew2, haa, c, secret);

  result := ldnew2;

end; // end secret_closet()

// Put a box around the given thing, with the given tag and */
// type on each of the linedefs.                            */
procedure trigger_box(const l: level_p; const t: thing_p; oldsector: sector_p;
  const tag, typ: SmallInt; const c: config_p);
var
  v1, v2, v3, v4: vertex_p;
  ldnew: linedef_p;
  dist: integer;
  ns: sector_p;
begin
  // Incoming sector is just a guess; confirm it
  ns := point_sector(l, t^.x, t^.y, @dist, nil); // Should check "danger"!
  if ns <> nil then
    oldsector := ns
  else
    // This shouldn't ever happen anymore, but just in case...
    announce(WARNING, 'point_sector returned nil in trigger_box');
  if dist > 24 then dist := 24;
  if dist < 4 then
  begin
    announce(LOG, 'Tiny triggerbox');
    dist := 4;
  end
  else if dist < 24 then
    announce(LOG, 'Small triggerbox');

  v1 := new_vertex(l, t^.x - dist, t^.y - dist);
  v2 := new_vertex(l, t^.x + dist, t^.y - dist);
  v3 := new_vertex(l, t^.x + dist, t^.y + dist);
  v4 := new_vertex(l, t^.x - dist, t^.y + dist);
  ldnew := new_linedef(l, v1, v2);
  ldnew^.right := new_sidedef(l, oldsector, c);
  ldnew^.left := new_sidedef(l, oldsector, c);
  ldnew^.tag := tag;
  ldnew^.typ := typ;
  ldnew^.flags := ldnew^.flags or TWO_SIDED;
  ldnew^.right^.middle_texture := c^.null_texture;
  ldnew^.left^.middle_texture := c^.null_texture;
  ldnew := new_linedef(l, v2, v3);
  ldnew^.right := new_sidedef(l, oldsector, c);
  ldnew^.left := new_sidedef(l, oldsector, c);
  ldnew^.tag := tag;
  ldnew^.typ := typ;
  ldnew^.flags := ldnew^.flags or TWO_SIDED;
  ldnew^.right^.middle_texture := c^.null_texture;
  ldnew^.left^.middle_texture := c^.null_texture;
  ldnew := new_linedef(l, v3, v4);
  ldnew^.right := new_sidedef(l, oldsector, c);
  ldnew^.left := new_sidedef(l, oldsector, c);
  ldnew^.tag := tag;
  ldnew^.typ := typ;
  ldnew^.flags := ldnew^.flags or TWO_SIDED;
  ldnew^.right^.middle_texture := c^.null_texture;
  ldnew^.left^.middle_texture := c^.null_texture;
  ldnew := new_linedef(l, v4, v1);
  ldnew^.right := new_sidedef(l, oldsector, c);
  ldnew^.left := new_sidedef(l, oldsector, c);
  ldnew^.tag := tag;
  ldnew^.typ := typ;
  ldnew^.flags := ldnew^.flags or TWO_SIDED;
  ldnew^.right^.middle_texture := c^.null_texture;
  ldnew^.left^.middle_texture := c^.null_texture;
end;

// Make a small floor-preserving link that fits on the given */
// linedef.  Suitable for walking out onto the patio.        */
function random_patio_link(const l: level_p; const ld: linedef_p;
  const ThisStyle: style_p; const c: config_p): link_p;
begin
  result := link_p(SL_Malloc(SizeOf(link_t)));

  result^.typ := BASIC_LINK;
  result^.bits := 0;
  result^.floordelta := 0;
  if rollpercent(50) then
    result^.height1 := 72
  else
    result^.height1 := 64 + 8 * roll(9);

  if rollpercent(50) then
    result^.width1 := 64 * l^.hugeness
  else
    result^.width1 := 64 + roll(linelen(ld) - 79);

  if rollpercent(50) then
    result^.depth1 := 16 * l^.hugeness // Door/arch depth
  else
    result^.depth1 := (8 + 4 * roll(15)) * l^.hugeness;

  if rollpercent(50) then
    result^.depth2 := 8 * l^.hugeness // Recess depth
  else
    result^.depth2 := 20 * l^.hugeness;

  if rollpercent(50) then
    result^.depth3 := 16 * l^.hugeness // Core depth
  else
    result^.depth3 := (8 + 4 * roll(15)) * l^.hugeness;

  if rollpercent(50) then result^.bits := result^.bits or LINK_RECESS;
  if rollpercent(20) then result^.bits := result^.bits or LINK_CORE;
  if rollpercent(5) then result^.bits := result^.bits or LINK_BARS;
  if rollpercent(20) then // Single door
  begin
    result^.bits := result^.bits or LINK_RECESS or LINK_ANY_DOOR;
    result^.bits := result^.bits and not LINK_CORE;
  end;
end;

// Try to make a little patio out of the given room */
procedure make_extroom(const l: level_p; const oldsector: sector_p;
  const haa: haa_p; const ThisStyle: style_p; const c: config_p);
var
  i, depth, x, y, fenceh, saveh: integer;
  cthick: SmallInt;
  ld, newldf, ldfar, lde1, lde2, ldt: linedef_p;
  t1: texture_p;
  ThisLink: link_p;
  hisec, losec: sector_p;
  v: vertex_p;
  outtex: boolean;
begin
  outtex := rollpercent(70);

  fenceh := 96; // Should vary
  i := mark_decent_boundary_linedefs(l, oldsector, 256);
  ld := random_marked_linedef(l, i);
  unmark_linedefs(l);
  if ld <> nil then
  begin
    if not empty_left_side(l, ld, 256) then
      exit;
    t1 := ld^.right^.middle_texture;
    ThisLink := random_patio_link(l, ld, ThisStyle, c);
    newldf := make_linkto(l, ld, ThisLink, ThisStyle, c, nil);
    if newldf = nil then
      exit; // Shouldn't happen
    depth := linelen(ld);
    if (depth <= 512) and rollpercent(25) then
      depth := depth * 2;
    flip_linedef(newldf); // Just so we can use the lefthand functions
    while true do
    begin
      if empty_left_side(l, newldf, depth) then break;
      depth := depth - 64;
      if depth < 128 then
      begin
        delete_vertex(l, newldf^.from);
        delete_vertex(l, newldf^._to);
        delete_linedef(l, newldf);
        exit; // How'd that happen?
      end;
    end;
    ldfar := lefthand_box_ext(l, newldf, depth, ThisStyle, c, @lde1, @lde2);
    flip_linedef(newldf); // Fix it
    ldfar^.right^.middle_texture := t1;
    newldf^.right^.middle_texture := t1;
    if outtex then
    begin
      lde2^.right^.middle_texture := random_texture0(OUTDOOR,c,nil);
      ldfar^.right^.middle_texture := lde2^.right^.middle_texture;
      lde1^.right^.middle_texture := lde2^.right^.middle_texture;
    end;
    losec := newldf^.right^.sector;
    losec^.floor_height := oldsector^.floor_height;
    losec^.floor_flat := oldsector^.floor_flat;
    losec^.light_level := l^.outside_light_level; // Minus twenty?
    cthick := 32;
    if rollpercent(30) then cthick := cthick + 8 * roll(10);
    hisec := clone_sector(l, losec);
    losec^.ceiling_height := losec^.floor_height + fenceh;
    hisec^.ceiling_flat := oldsector^.ceiling_flat; // For e-l; fixed later
    newldf^.right^.sector := hisec;
    // And now the little triangle thing to look good
    x := (newldf^._to^.x + newldf^.from^.x) div 2;
    y := (newldf^._to^.y + newldf^.from^.y) div 2;
    point_from(newldf^.from^.x, newldf^.from^.y, x, y,
      RIGHT_TURN, 32, @x, @y);
    v := new_vertex(l, x, y);
    ldt := new_linedef(l, newldf^._to, v);
    ldt^.right := new_sidedef(l, hisec, c);
    ldt^.right^.middle_texture := c^.null_texture;
    ldt^.left := new_sidedef(l, losec, c);
    ldt^.left^.middle_texture := c^.null_texture;
    ldt^.flags := ldt^.flags or TWO_SIDED or NOT_ON_MAP;
    ldt := new_linedef(l, v, newldf^.from);
    ldt^.right := new_sidedef(l, hisec, c);
    ldt^.right^.middle_texture := c^.null_texture;
    ldt^.left := new_sidedef(l, losec, c);
    ldt^.left^.middle_texture := c^.null_texture;
    ldt^.flags := ldt^.flags or TWO_SIDED or NOT_ON_MAP;
    // Adjust stuff
    hisec^.ceiling_height := oldsector^.ceiling_height + cthick;
    if hisec^.ceiling_height < losec^.ceiling_height then
      hisec^.ceiling_height := losec^.ceiling_height + cthick;
    ldfar^.right^.y_offset := oldsector^.ceiling_height - losec^.ceiling_height;
    lde1^.right^.y_offset := ldfar^.right^.y_offset;
    lde2^.right^.y_offset := ldfar^.right^.y_offset;
    newldf^.right^.y_offset := oldsector^.ceiling_height - hisec^.ceiling_height;
    // Actually make the link
    saveh := hisec^.ceiling_height;
    establish_link(l, ld, newldf, ThisLink, nil, ThisStyle, ThisStyle, haa, c);
    hisec^.ceiling_flat := c^.sky_flat;
    hisec^.ceiling_height := saveh;
    // A hack to fix some quasi-HOMs
    ldt := l^.linedef_anchor;
    while ldt <> nil do
    begin
      if ldt^.left <> nil then
        if ldt^.right <> nil then
          if ldt^.right^.sector = hisec then
            patch_upper(ldt, t1, c);
      ldt := ldt^.next;
    end;
    if outtex then
    begin
      hisec^.floor_flat := random_flat0(OUTDOOR, c, nil);
      losec^.floor_flat := hisec^.floor_flat;
    end;
    // Now populate it and stuff
    populate(l, losec, c, haa, FALSE);
    place_plants(l, 128, losec, c); // 128?
    announce(VERBOSE, 'Patio');
  end;

end; // end make_extroom

// Try to make an external window out of the given room */
procedure make_extwindow(const l: level_p; const oldsector: sector_p;
  const ThisStyle: style_p; const c: config_p);
var
  wlen, wheight, i, depth, ldlen, border: integer;
  ld, ldnew: linedef_p;
  t1: texture_p;
  e1, e2: linedef_p;
  yoff: SmallInt;
begin
  i := mark_decent_boundary_linedefs(l, oldsector, 64);
  ld := random_marked_linedef(l, i);
  unmark_linedefs(l);
  if ld <> nil then
  begin
    t1 := ld^.right^.middle_texture;
    ldlen := linelen(ld);
    wlen := 32 + roll(ldlen - 31);
    if wlen > ldlen then
      wlen := ldlen;
    border := (ldlen - wlen) div 2;
    if border <> 0 then // JVAL > 0 ?
    begin
      ld := split_linedef(l, ld, border, c);
      split_linedef(l, ld, wlen, c);
    end;
    depth := 40;
    // The "48" in the next line is the two eight-deep little sectors,
    // plus another 32 just so windows won't be placed *too* absurdly.
    if empty_left_side(l, ld, depth + 48) then
    begin
      wheight := oldsector^.ceiling_height - oldsector^.floor_height;
      if wheight > 128 then
        wheight := 128;
      wheight := oldsector^.floor_height + 48 + roll(wheight - 47);
      if wheight > oldsector^.ceiling_height - 32 then
        wheight := oldsector^.ceiling_height - 32;
      ldnew := lefthand_box_ext(l, ld, depth, ThisStyle, c, @e1, @e2);
      ldnew^.right^.sector^.light_level := l^.outside_light_level;
      ldnew^.right^.sector^.special := 0;
      ldnew^.right^.sector^.floor_height := wheight;
      ldnew^.right^.sector^.ceiling_height := wheight + 32 + roll(97);
      if (ldnew^.right^.sector^.ceiling_height > oldsector^.ceiling_height) or
          rollpercent(20) then
        ldnew^.right^.sector^.ceiling_height := oldsector^.ceiling_height;
      if ThisStyle^.window_grate and rollpercent(50) then
      begin
        announce(VERBOSE, 'Grated extwindow');
        ld^.right^.middle_texture := ThisStyle^.grating;
        if ldnew^.right^.sector^.ceiling_height - ldnew^.right^.sector^.floor_height < 128 then
        begin
          // Do something about ld^.right^.y_offset?
        end;
      end;
      patch_upper(ld, t1, c);
      patch_lower(ld, t1, c);
      yoff := oldsector^.ceiling_height - ldnew^.right^.sector^.ceiling_height;
      e1^.right^.y_offset := yoff;
      e2^.right^.y_offset := yoff;
      ldnew := lefthand_box_ext(l, ldnew, 8, ThisStyle, c, @e1, @e2);
      e1^.right^.y_offset := yoff;
      e2^.right^.y_offset := yoff;
      ldnew^.right^.sector^.floor_height := wheight - 4;
      ldnew^.right^.sector^.ceiling_flat := c^.sky_flat;
      ldnew := lefthand_box(l, ldnew, 8, ThisStyle, c);
      ldnew^.right^.sector^.floor_height := wheight - 16;
      ldnew^.right^.sector^.ceiling_height := wheight - 8;
      ldnew^.right^.sector^.ceiling_flat := c^.sky_flat;
      announce(VERBOSE, 'Outside Window');
    end; // end if enough room
  end; // end if found a linedef
end;

// Special room all full of pillars and stuff.  TRUE if works. */
function grid_room(const l: level_p; const oldsector: sector_p;
  const haa: haa_p; const ThisStyle: style_p; const ThisQuest: quest_p;
  const first: boolean; const c: config_p): boolean;
var
  minx, miny, maxx, maxy: integer;
  x1, y1, xi, yi: integer;
  xcount, ycount: integer;
  xwidth, ywidth: integer;
  xspace, yspace: integer;
  secretx, secrety: integer;
  sx, sy: integer;
  m: genus_p;
  levels: integer;
  facing: SmallInt;
  t: texture_p;
  trying_constructs: boolean;
  ld1, ld2, ld3, ld4: linedef_p;
  tx: texture_p;
  newsec: sector_p;
begin
  secretx := -1;
  secrety := -1;
  sx := 0;
  sy := 0;

  find_sector_rectangle(l, oldsector, minx, miny, maxx, maxy);
  if maxx - minx < 192 then
  begin
    result := FALSE;
    exit;
  end;
  if maxy - miny < 192 then
  begin
    result := FALSE;
    exit;
  end;
  xcount := (maxx - minx) div 96;
  xcount := 2 + roll(xcount - 1);
  ycount := (maxy - miny) div 96;
  ycount := 2 + roll(ycount - 1);
  if xcount * ycount > 100 then
  begin
    xcount := xcount div 2;
    ycount := ycount div 2;
  end;
  xspace := (maxx - minx) div xcount;
  xwidth := 30 + roll(xspace - 95);
  yspace := (maxy - miny) div ycount;
  ywidth := 30 + roll(yspace - 95);
  if rollpercent(40) then // Square pillars
  begin
    if xwidth < ywidth then ywidth := xwidth;
    if ywidth < xwidth then xwidth := ywidth;
  end;

  case roll(6) of
    0,
    1,
    2: t := ThisStyle^.wall0;
    3,
    4: t := random_wall0(c, ThisStyle);
  else
    t := ThisStyle^.support0;
  end;

  if (ThisStyle^.walllight <> nil) and rollpercent(3) then
  begin
    announce(LOG, 'Gridlight');
    t := ThisStyle^.walllight;
    oldsector^.light_level := 240; // Or so
    case roll(3) of
      0: oldsector^.special := RANDOM_BLINK;
      1: oldsector^.special := SYNC_FAST_BLINK;
    else
      oldsector^.special := SYNC_SLOW_BLINK;
    end;
  end;

  trying_constructs := ThisStyle^.do_constructs and rollpercent(25);
  if c^.secret_monsters and rollpercent(75) and
    (xwidth > 63) and (ywidth > 63) and not trying_constructs then
  begin
    secretx := roll(xcount);
    secrety := roll(ycount);
  end;

  xi := 0;
  x1 := minx + (xspace - xwidth) div 2;
  while xi < xcount do
  begin
    yi := 0;
    y1 := miny + (yspace - ywidth) div 2;
    while yi < ycount do
    begin
      if (xi = secretx) and (yi = secrety) then
      begin
        tx := ThisStyle^.support0;
        newsec := clone_sector(l, oldsector);
        newsec^.floor_height := newsec^.ceiling_height;
        newsec^.ceiling_height := newsec^.ceiling_height + 96; // Fixed?
        if tx = t then
          tx := ThisStyle^.wall0;
        newsec^.tag := new_tag(l);
        newsec^.special := SECRET_SECTOR;
        parallel_innersec_ex(l, oldsector, newsec, nil, ThisStyle^.wall0, t,
                             x1, y1, x1 + xwidth, y1 + ywidth, c, @ld1, @ld2, @ld3, @ld4);
        ld2^.flags := ld2^.flags or SECRET_LINEDEF;
        ld3^.flags := ld3^.flags or SECRET_LINEDEF;
        ld4^.flags := ld4^.flags or SECRET_LINEDEF;
        ld1^.flags := ld1^.flags and not (LOWER_UNPEGGED or UPPER_UNPEGGED); // Ought to
        ld2^.flags := ld2^.flags and not (LOWER_UNPEGGED or UPPER_UNPEGGED); // re-y-align,
        ld3^.flags := ld3^.flags and not (LOWER_UNPEGGED or UPPER_UNPEGGED); // also,
        ld4^.flags := ld4^.flags and not (LOWER_UNPEGGED or UPPER_UNPEGGED); // eh?
        if rollpercent(50) then
        begin
          ld1^.right^.lower_texture := tx;
          ld1^.flags := ld1^.flags or SECRET_LINEDEF;
        end;
        ld1^.tag := newsec^.tag;
        ld1^.typ := ThisStyle^.slifttype;
        flip_linedef(ld3);
        ld3^.tag := newsec^.tag;
        ld3^.typ := ThisStyle^.slifttype;
        ld3^.flags := ld3^.flags and not UPPER_UNPEGGED;
        if linelen(ld3) > 64 then
          split_linedef(l, ld3, 64, c);
        ld3^.right^.upper_texture := ThisStyle^.switch0;
        ld3^.right^.x_offset := 0;
        ld3^.right^.y_offset := ld3^.right^.y_offset + ThisStyle^.switch0^.y_bias;
        sx := x1 + xwidth div 2;
        sy := y1 + ywidth div 2;
        announce(VERBOSE, 'Secret grid-pillar');
        y1 := y1 + yspace;
        inc(yi);
        continue;
      end;
      if rollpercent(c^.p_grid_gaps) then
      begin
        y1 := y1 + yspace;
        inc(yi);
        continue;
      end;
      if trying_constructs then
      begin
        trying_constructs :=
          install_construct(l, oldsector, x1, y1, x1 + xwidth, y1 + ywidth, ThisStyle, c);
        if trying_constructs then
          announce(VERBOSE, 'Grid construct');
      end;
      if not trying_constructs then
        parallel_innersec(l, oldsector, nil, t, nil, nil,
                          x1, y1, x1 + xwidth, y1 + ywidth, c);
      // Pretty primitive; monster-width assumptions etc.
      if (xi <> 0) and rollpercent(50) then
      begin
        m := timely_monster(haa, c, @levels, rollpercent(l^.p_biggest_monsters), 1);
        if m <> nil then
        begin
          if yi < ycount div 2 then
            facing := 90
          else
            facing := 270;
          levels := levels or $08; // deaf
          new_thing(l, x1 - 32, y1 + (ywidth div 2), facing, m^.thingid, levels, c);
          update_haa_for_monster(haa, m, levels, 1, c);
        end;
      end;
      if (yi <> 0) and rollpercent(50) then
      begin
        m := timely_monster(haa, c, @levels, rollpercent(l^.p_biggest_monsters), 1);
        if m <> nil then
        begin
          if xi < xcount div 2 then
            facing := 0
          else
            facing := 180;
          levels := levels or $08; // deaf
          new_thing(l, x1 + (xwidth div 2), y1 - 32, facing, m^.thingid, levels, c);
          update_haa_for_monster(haa, m, levels, 1, c);
        end;
      end;
      y1 := y1 + yspace;
      inc(yi);
    end; // end for y
    x1 := x1 + xspace;
    inc(xi);
  end; // end for x
  haa_unpend(haa);

  // Rather primitive bonus-depositing
  xi := 0;
  x1 := minx + (xspace - xwidth) div 2;
  while xi < xcount do
  begin
    yi := 0;
    y1 := miny + (yspace - ywidth) div 2;
    while yi < ycount do
    begin
      if (yi <> 0) and rollpercent(30) then
        place_timely_something(l, haa, c, x1 + (xwidth div 2), y1 - 23);
      y1 := y1 + yspace;
      inc(yi);
    end;
    x1 := x1 + xspace;
    inc(xi);
  end;

  if secretx <> -1 then
  begin
    m := timely_monster(haa, c, @levels, rollpercent(l^.p_biggest_monsters), 0);
    if m <> nil then
    begin
      facing := 180;
      levels := levels or $08; // deaf
      new_thing(l, sx, sy, facing, m^.thingid, levels, c);
      update_haa_for_monster(haa, m, levels, 1, c);
    end;
    // This doesn't account for the secretness!!
    // It'll also sometimes place nothing at all!
    place_timely_something(l, haa, c, sx, sy);
    haa_unpend(haa);
  end;

  announce(VERBOSE, 'Grid room');
  result := TRUE;
end;

// Install a teleport gate in the room, and any attendant monsters */
// and stuff.  Even nukage! */
procedure install_gate(const l: level_p;const s: sector_p; const ThisStyle: style_p;
  const ThisHaa: haa_p; const force_exit_style: boolean; const c: config_p);
var
  lowx, lowy, hix, hiy: SmallInt;
  innersec, outersec: sector_p;
  ld1, ld2, ld3, ld4: linedef_p;
  gateflat: flat_p;
  exit_style: boolean;
  exit_gate: boolean;
  front: texture_p;
  rise: integer;
  tag_mask: SmallInt;
  minx, miny, maxx, maxy, leeway: integer;
begin
  gateflat := random_gate(c, s^.style);
  exit_style := force_exit_style;
  exit_gate := (s^.gate^.in_tag = 0) and (s^.gate^.out_tag = 0);
  rise := 0;

  mid_tile(l,s, lowx, lowy, hix, hiy);

  outersec := s;

  if rollpercent(l^.p_force_nukage) or rollpercent(10) then
  begin
    find_sector_rectangle(l,s, minx, miny, maxx, maxy);
    leeway := lowx - minx;
    if lowy - miny < leeway then leeway := lowy - miny;
    if maxx - hix < leeway then leeway := maxx - hix;
    if maxy - hiy < leeway then leeway := maxy - hiy;
    if leeway > 48 then
    begin
      leeway := 16 + roll(leeway - 48);
      outersec := clone_sector(l, s);
      outersec^.floor_height := outersec^.floor_height - 8;
      outersec^.floor_flat := s^.style^.nukage1;
      if outersec^.light_level < 160 then
        outersec^.light_level := 160;
      outersec^.special := NUKAGE1_SPECIAL;
      rise := rise + 8;
      parallel_innersec(l, s, outersec, nil, s^.style^.wall0, s^.style^.support0,
        minx + leeway, miny + leeway, maxx - leeway, maxy - leeway, c);
      if s^.gate^.is_entry then
        announce(VERBOSE, 'Nukage arrival')
      else
        announce(VERBOSE, 'Nukage gate');
      ThisHaa^.haas[ITYTD].health := ThisHaa^.haas[ITYTD].health - 10;
      ThisHaa^.haas[HMP].health := ThisHaa^.haas[HMP].health - 5;
      if s^.gate^.is_entry then
      begin
        ThisHaa^.haas[ITYTD].health := ThisHaa^.haas[ITYTD].health - 10;
        ThisHaa^.haas[HMP].health := ThisHaa^.haas[HMP].health - 5;
        ThisHaa^.haas[UV].health := ThisHaa^.haas[UV].health - 5;
      end;
    end;
  end;

  innersec := clone_sector(l, s);
  if (s^.gate^.out_tag <> 0) or exit_gate then
  begin
    innersec^.floor_flat := gateflat;
    if (innersec^.ceiling_flat <> c^.sky_flat) and (ThisStyle^.ceilinglight <> nil) then
    begin
      innersec^.ceiling_flat := ThisStyle^.ceilinglight;
      announce(VERBOSE, 'gcl');
    end;
    innersec^.light_level := 240; // Should vary by style or level
    innersec^.special := GLOW_BLINK; // Also
  end;

  if s^.gate^.in_tag <> 0 then
  begin
    innersec^.tag := s^.gate^.in_tag;
    new_thing(l, (lowx + hix) div 2, (lowy + hiy) div 2, 90 * roll(4), ID_GATEOUT, 7, c);
    if s^.gate^.is_entry then
    begin
      s^.entry_x := lowx + 32;
      s^.entry_y := lowy + 32;
    end;
  end;

  parallel_innersec_ex(l, outersec, innersec, nil, nil, nil, lowx, lowy, hix, hiy,
                       c, @ld1, @ld2, @ld3, @ld4);
  if s^.gate^.gate_lock <> 0 then
  begin
    case s^.gate^.gate_lock of
      LINEDEF_S1_OPEN_DOOR:
        begin
          if rollpercent(30) then // Put in style or config or...?
          begin
            innersec^.ceiling_height := innersec^.floor_height + 32;
            announce(LOG, 'Uplocked gate');
          end
          else
          begin
            innersec^.ceiling_height := innersec^.floor_height;
            announce(LOG, 'Pillar gate');
          end;
        end;
      LINEDEF_S1_LOWER_FLOOR:
        begin
          innersec^.floor_height := innersec^.floor_height + 32;
          rise := rise + 32;
          announce(LOG, 'Downlocked gate');
        end;
      else
        announce(ERROR, 'Odd lock-type in install_gate');
    end;
    patch_upper(ld1, s^.style^.wall0, c);
    patch_upper(ld2, s^.style^.wall0, c);
    patch_upper(ld3, s^.style^.wall0, c);
    patch_upper(ld4, s^.style^.wall0, c);
  end;

  if l^.raise_gates then // Don't really want to do this for downlocked?
  begin
    rise := rise + 8;
    innersec^.floor_height := innersec^.floor_height + 8;
  end;

  if c^.p_use_steps <> 0 then
    front := s^.style^.stepfront
  else
    front := s^.style^.support0;
  if rise > front^.height then
    front := s^.style^.support0;
  patch_lower(ld1, front, c);
  patch_lower(ld2, front, c);
  patch_lower(ld3, front, c);
  patch_lower(ld4, front, c);
  if c^.monsters_can_teleport then
    tag_mask := 0
  else
    tag_mask := BLOCK_MONSTERS;
  if c^.monsters_can_teleport then
    announce(VERBOSE, 'Possible teleporting monsters');

  if s^.gate^.out_tag <> 0 then
  begin
    ld1^.typ := LINEDEF_TELEPORT;
    ld1^.flags := ld1^.flags or tag_mask; // Always?
    ld1^.tag := s^.gate^.out_tag;
    ld2^.typ := LINEDEF_TELEPORT;
    ld2^.flags := ld2^.flags or tag_mask; // Always?
    ld2^.tag := s^.gate^.out_tag;
    ld3^.typ := LINEDEF_TELEPORT;
    ld3^.flags := ld3^.flags or tag_mask; // Always?
    ld3^.tag := s^.gate^.out_tag;
    ld4^.typ := LINEDEF_TELEPORT;
    ld4^.flags := ld4^.flags or tag_mask; // Always?
    ld4^.tag := s^.gate^.out_tag;
  end
  else if s^.gate^.in_tag = 0 then // Must be a level-end gate
  begin
    exit_style := TRUE;
    ld1^.typ := LINEDEF_W1_END_LEVEL;
    ld1^.flags := ld1^.flags or tag_mask;
    ld2^.typ := LINEDEF_W1_END_LEVEL;
    ld2^.flags := ld2^.flags or tag_mask;
    ld3^.typ := LINEDEF_W1_END_LEVEL;
    ld3^.flags := ld3^.flags or tag_mask;
    ld4^.typ := LINEDEF_W1_END_LEVEL;
    ld4^.flags := ld4^.flags or tag_mask;
  end; // Otherwise an in-only gate

  if exit_style then
  begin
    innersec^.floor_height := outersec^.floor_height + 16;
    if c^.gate_exitsign_texture <> nil then
    begin
      ld1^.right^.lower_texture := c^.gate_exitsign_texture;
      ld2^.right^.lower_texture := c^.gate_exitsign_texture;
      ld3^.right^.lower_texture := c^.gate_exitsign_texture;
      ld4^.right^.lower_texture := c^.gate_exitsign_texture;
    end
    else
    begin
      ld1^.right^.lower_texture := ThisStyle^.support0;
      ld2^.right^.lower_texture := ThisStyle^.support0;
      ld3^.right^.lower_texture := ThisStyle^.support0;
      ld4^.right^.lower_texture := ThisStyle^.support0;
    end;
    s^.middle_enhanced := TRUE;
    innersec^.ceiling_flat := gateflat;
  end;

  if (s^.gate^.out_tag <> 0) or exit_gate then
    if innersec^.ceiling_flat^.props and LIGHT <> 0 then
      if innersec^.ceiling_height - innersec^.floor_height >= 96 then
        if s^.gate^.gate_lock = 0 then
        begin
          innersec^.ceiling_height := innersec^.ceiling_height - 16;
          ld1^.right^.upper_texture := ThisStyle^.support0;
          ld2^.right^.upper_texture := ThisStyle^.support0;
          ld3^.right^.upper_texture := ThisStyle^.support0;
          ld4^.right^.upper_texture := ThisStyle^.support0;
        end;

  ld1^.flags := ld1^.flags and not LOWER_UNPEGGED;
  ld2^.flags := ld2^.flags and not LOWER_UNPEGGED;
  ld3^.flags := ld3^.flags and not LOWER_UNPEGGED;
  ld4^.flags := ld4^.flags and not LOWER_UNPEGGED;

end; // end install_gate

// Install the locked/hidden thing that contains the exit that
// leads to a Secret Level.  If it works, set the sl_tag and
// sl_type things in the level.  If <opens>, make it openable,
// and immediately set sl_done.
function install_sl_exit(const l: level_p; const oldsector: sector_p;
  const ThisHaa: haa_p; const ThisStyle: style_p; const ThisQuest: quest_p;
  const opens: boolean; const c: config_p): boolean;
var
  i, tries: integer;
  ld, ld2, ld3: linedef_p;
  tag: SmallInt;
  newsec: sector_p;
  found: boolean;
begin
  found := FALSE;
  tries := 0;
  ld := nil;
  while not found and (tries < 5) do
  begin
    i := mark_decent_boundary_linedefs(l, oldsector, 72);
    ld := random_marked_linedef(l, i);
    unmark_linedefs(l);
    if ld = nil then
    begin
      result := FALSE;
      exit;
    end;
    if empty_left_side(l, ld, 8) then
      found := TRUE;
    inc(tries);
  end;

  ld2 := install_switch(l, ld, TRUE, FALSE, 0, ThisStyle, c, @ld3);
  if ld = ld2 then // Didn't recess after all??
  begin
    announce(WARNING, 'Silly switch left sitting around?');
    // Try to fix it
    ld^.right^.middle_texture := ThisStyle^.wall0;
    ld^.typ := LINEDEF_NORMAL;
    result := FALSE;
    exit;
  end;

  tag := new_tag(l);
  ld2^.typ := LINEDEF_S1_SEC_LEVEL;
  newsec := ld2^.right^.sector;
  newsec^.special := GLOW_BLINK;
  newsec^.light_level := 255;
  newsec^.ceiling_height := newsec^.floor_height;
  l^.sl_tag := tag;
  if opens then
  begin
    ld3^.typ := LINEDEF_NORMAL_S1_DOOR;
    announce(VERBOSE, 'Openable sl exit');
    l^.sl_done := TRUE;
  end
  else
  begin
    newsec^.tag := tag;
    l^.sl_type := LINEDEF_W1_OPEN_DOOR; // Or S1, eh?  So...
    ld^.flags := ld^.flags or SECRET_LINEDEF;
    if ThisQuest^.goal = LEVEL_END_GOAL then
      l^.sl_open_ok := TRUE
    else
    begin
      l^.sl_open_ok := FALSE;
      l^.sl_open_start := ThisQuest^.room;
    end;
    l^.sl_exit_sector := oldsector;
    announce(VERBOSE, 'Installed sl exit');
  end;

  result := TRUE;
end;

// Try to put a triggerbox around something in this sector,
// to open the sl exit thing.
procedure try_sl_triggerbox(const l: level_p; const oldsector: sector_p; const c: config_p);
var
  danger: boolean;
  t: thing_p;
  border: integer;
begin
  t := l^.thing_anchor;
  while t <> nil do
  begin
    if t^.genus^.bits and PICKABLE = 0 then
    begin
      t := t^.next;
      continue;
    end;
    if oldsector <> point_sector(l, t^.x, t^.y, @border, @danger) then
    begin
      t := t^.next;
      continue;
    end;
    if border < 32 then
    begin
      t := t^.next;
      continue;
    end;
    if danger then
    begin
      t := t^.next;
      continue;
    end;
    if t^.options and $07 <> $07 then
    begin
      t := t^.next;
      continue;
    end;
    break;
  end; // end for things

  if t <> nil then
  begin
    trigger_box(l, t, oldsector, l^.sl_tag, l^.sl_type, c);
    l^.sl_done := TRUE;
    l^.sl_open_ok := FALSE;
    announce(VERBOSE, 'Did sl triggerbox');
  end; // if found a good thing
end;

// Fancy up the room, put stuff in it, install gates, etc.
procedure enhance_room(const l: level_p; const oldsector: sector_p; const ThisHaa: haa_p;
  const ThisStyle: style_p; const ThisQuest: quest_p; const first: boolean;
  const c: config_p);
var
  done_room, did_dm: boolean;
begin
  done_room := FALSE;
  did_dm := FALSE;

  if (ThisQuest <> nil) and (ThisQuest^.goal <> NULL_GOAL) and
      (need_secret_level(c)) and (l^.sl_tag = 0) and
      (rollpercent(20) or (ThisQuest^.count + 4 > ThisQuest^.minrooms)) then
    install_sl_exit(l, oldsector, ThisHaa, ThisStyle, ThisQuest, FALSE, c);

  if first and (oldsector^.gate <> nil) then
    announce(WARNING, 'Gate and watermark do not mix!');

  if not done_room and oldsector^.middle_enhanced then
  begin
    // Someone else did everything else
    embellish_room(l, oldsector, ThisHaa, ThisStyle, ThisQuest, first, TRUE, c);
    done_room := TRUE;
  end;

  if not done_room and (oldsector^.gate <> nil) then
  begin
    install_gate(l, oldsector, ThisStyle, ThisHaa, FALSE, c);
    gate_populate(l, oldsector, ThisHaa, first, c); // Very special-purpose!
    embellish_room(l, oldsector, ThisHaa, ThisStyle, ThisQuest, first, TRUE, c);
    done_room := TRUE;
  end;

  if not done_room and not first and not oldsector^.has_key and rollpercent(l^.p_special_room) then
    if grid_room(l, oldsector, ThisHaa, ThisStyle, ThisQuest, first, c) then
    begin
      embellish_room(l, oldsector, ThisHaa, ThisStyle, ThisQuest, first, TRUE, c);
      done_room := TRUE;
    end;

  if not done_room then // Simple randomly-enhanced room
  begin
    populate(l, oldsector, c, ThisHaa, first); // or after embellish?
    embellish_room(l, oldsector, ThisHaa, ThisStyle, ThisQuest, first, FALSE, c);
  end;

  if first or rollpercent(l^.dm_rho) then
    did_dm := maybe_add_dm_start(l, oldsector, c, FALSE);

  if did_dm then
    l^.dm_rho := 10
  else if l^.dm_rho < 80 then
    l^.dm_rho := l^.dm_rho + (400 div c^.minrooms);

  align_textures(l, oldsector, c);

end; // end enhance_room()

// Fancy-up the room, after all links are established, and after
// populating with Things.  If <first>, do the obvious
// SLIGE-mark to the room, and prolly no monsters.
procedure embellish_room(const l: level_p; const oldsector: sector_p;  const haa: haa_p;
  const ThisStyle: style_p; const ThisQuest: quest_p; const first: boolean;
  const edges_only: boolean; const c: config_p);
// Just some random fun things; assumes rectangles etc
var
  i, j, border, ldlen, depth: integer;
  switch_tag: integer;
  switch_ld: linedef_p;
  ld: linedef_p;
  did_ceiling, install_closet, switch_closet: boolean;
  n, k, clen, yoff, x1, y1: integer;
  ldnew, ldnew2, ldedge1, ldedge2: linedef_p;
  t1, tplaque: texture_p;
  sky_thing, crushing: boolean;
  m, g: genus_p;
  levels: integer;
  angle, bonustype: SmallInt;
  innersec, outersec: sector_p;
  minx, miny, maxx, maxy: integer;
  outersecheight: SmallInt; // JVAL
  sync_doors: boolean;
  sync_tag: integer;
  sync_count: integer; // Just for announcing
  pheight, pup: integer;
  t: thing_p;
  tag: SmallInt;
  plen: integer;
  goal_trigger, had_map: boolean;
  ldc: linedef_p;
  hinted: boolean;
  x, y: integer;
  danger: boolean;
  ll, sdepth, sno, spec, fh, ch: integer;
begin
  switch_tag := 0;
  switch_ld := nil;
  did_ceiling := FALSE;

  if rollpercent(10) then
    if oldsector^.special = 0 then
      oldsector^.special := RANDOM_BLINK;

  if first then // Watermark
  begin
    // Mark the first room as SLIGE generated
    watermark_sector(l, oldsector, ThisStyle, c);

    l^.first_room := oldsector; // for later

  end
  else if not edges_only then
  begin

    did_ceiling := ceiling_effect(l, oldsector, ThisStyle, haa, c);

    if not did_ceiling then
      if rollpercent(0) then // Looks silly often!
        oldsector^.ceiling_flat := c^.sky_flat; // Just a plain open top

    if not did_ceiling then
      if rollpercent(80) then // high, because often fails
        do_pillar(l, oldsector, ThisStyle, haa, c);

    if not did_ceiling then
      if rollpercent(l^.p_new_pillars) then
        do_new_pillars(l, oldsector, ThisStyle, haa, c);

  end; // end things we can't do if watermarking or edges_only

  // Edgy-things we can do even if watermarking follow

  // Perhaps triggerbox a thing in here, to open the
  // secret-level-exit thing, if any
  if l^.sl_open_ok and rollpercent(30) and (oldsector <> l^.sl_exit_sector) then
    try_sl_triggerbox(l, oldsector, c);

  // One or more ambush closets
  // This should really be a separate routine!
  if not first or c^.immediate_monsters then // Not right off!
    if rollpercent(l^.amcl_rho) then
    begin
      sky_thing := rollpercent(l^.skyclosets);
      crushing := FALSE;
      n := 1 + roll(3);
      for k := 0 to n - 1 do
      begin
        i := mark_decent_boundary_linedefs(l, oldsector, 64);
        ld := random_marked_linedef(l, i);
        unmark_linedefs(l);
        if ld <> nil then
        begin
          t1 := ld^.right^.middle_texture;
          clen := ThisStyle^.closet_width;
{$IFNDEF WIDE_SKIES_OK}
          if sky_thing then
            if clen > 72 then
              clen := 72; // Wide skyc's look lame
{$ENDIF}
          ldlen := linelen(ld);
          if clen > ldlen then clen := ldlen;
          border := (ldlen - clen) div 2;
          if border <> 0 then
          begin
            ld := split_linedef(l, ld, border, c);
            split_linedef(l, ld, clen, c);
          end;
          depth := ThisStyle^.closet_depth;
          if empty_left_side(l, ld, depth) then
          begin
            ldnew := lefthand_box_ext(l, ld, depth, ThisStyle, c, @ldedge1, @ldedge2);
            outersec := ldnew^.right^.sector;
            ldnew^.right^.middle_texture := ldedge1^.right^.middle_texture;
            if rollpercent(50) then
              if oldsector^.ceiling_height - oldsector^.floor_height > 72 then
              begin
                outersec^.ceiling_height := outersec^.floor_height + 72;
                yoff := (oldsector^.ceiling_height - oldsector^.floor_height) - 72;
                ldnew^.right^.y_offset := yoff;
                ldedge1^.right^.y_offset := yoff;
                ldedge2^.right^.y_offset := yoff;
                patch_upper(ld,t1,c);
              end; // end if bigger'n 72
            if sky_thing then
            begin
              announce(VERBOSE, 'Sky closet');
              innersec := clone_sector(l,outersec);
              innersec^.ceiling_height := innersec^.ceiling_height + 16; // 8?  16?
              find_sector_rectangle(l, outersec, minx, miny, maxx, maxy);
              minx := minx + 8;
              miny := miny + 8;
              maxx := maxx - 8;
              maxy := maxy - 8;
              parallel_innersec(l, outersec,innersec,
                                nil, outersec^.style^.wall0, outersec^.style^.wall0,
                                minx, miny, maxx, maxy, c);
              innersec^.ceiling_flat := c^.sky_flat;
              innersec^.light_level := l^.outside_light_level;
              if outersec^.light_level < l^.bright_light_level then
                outersec^.light_level := oldsector^.light_level + roll(l^.bright_light_level - oldsector^.light_level);
            end
            else // Not sky closet; maybe nukage and/or crushing
            begin
              outersec^.light_level := oldsector^.light_level + ThisStyle^.closet_light_delta;
              if outersec^.light_level < 80 then
                outersec^.light_level := 80;
              if outersec^.light_level > 240 then
                outersec^.light_level := 240;
              if rollpercent(2 + l^.p_force_nukage div 2) then
              begin
                outersec^.floor_height := outersec^.floor_height - 8;
                outersec^.floor_flat := ThisStyle^.nukage1;
                outersec^.special := NUKAGE1_SPECIAL;
                if outersec^.light_level < 120 then
                  outersec^.light_level := 120;
                patch_lower(ld, ThisStyle^.support0, c);
                announce(VERBOSE, 'Nukage ambush');
              end; // end nukage
              if rollpercent(2) and // "2"?  Crush!
                  (outersec^.ceiling_height - outersec^.floor_height <= 72) and
                  (l^.crushercount < LEVEL_MAX_CRUSHERS) then
              begin
                l^.crushercount := l^.crushercount + 1;
                ld^.typ := LINEDEF_WR_FAST_CRUSH;
                ld^.tag := new_tag(l);
                ld^.right^.upper_texture := ThisStyle^.wall0;
                ld^.flags := ld^.flags and not UPPER_UNPEGGED;
                ld^.right^.y_offset := ld^.right^.y_offset - outersec^.ceiling_height - oldsector^.ceiling_height;
                ldnew^.flags := ldnew^.flags or LOWER_UNPEGGED;
                ldedge1^.flags := ldedge1^.flags or LOWER_UNPEGGED;
                ldedge2^.flags := ldedge2^.flags or LOWER_UNPEGGED;
                outersecheight := outersec^.ceiling_height - outersec^.floor_height;
                ldnew^.right^.y_offset := ldnew^.right^.y_offset + outersecheight;
                ldedge1^.right^.y_offset := ldedge1^.right^.y_offset + outersecheight;
                ldedge2^.right^.y_offset := ldedge2^.right^.y_offset + outersecheight;
                outersec^.tag := ld^.tag;
                outersec^.ceiling_flat := random_flat0(RED, c, nil);
                if outersec^.light_level > 120 then
                  outersec^.light_level := 120;
                crushing := TRUE;
                announce(VERBOSE, 'Crush ambush');
              end; // end if crushing
              if oldsector^.light_level - outersec^.light_level >= 16 then
              begin
                announce(VERBOSE, 'shadow');
                innersec := clone_sector(l, outersec);
                innersec^.tag := outersec^.tag;
                innersec^.style := oldsector^.style; // Why?
                innersec^.light_level := oldsector^.light_level;
                if rollpercent(50) then
                begin
                  split_linedef(l, ldedge1, 16 + roll(20), c); // OK depth?
                  ldedge1^.right^.sector := innersec;
                  ldnew2 := new_linedef(l, ldedge1^._to, ld^._to);
                end
                else
                begin
                  ldedge2 := split_linedef(l, ldedge2, linelen(ldedge2) - (16 + roll(20)), c);
                  ldedge2^.right^.sector := innersec;
                  ldnew2 := new_linedef(l, ld^.from, ldedge2^.from);
                end;
                ldnew2^.flags := ldnew2^.flags or TWO_SIDED or NOT_ON_MAP;
                ldnew2^.right := new_sidedef(l, innersec, c);
                ldnew2^.right^.middle_texture := c^.null_texture;
                ldnew2^.left := new_sidedef(l, outersec, c);
                ldnew2^.left^.middle_texture := c^.null_texture;
                ld^.left^.sector := innersec;
              end
              else if (outersec^.style^.ceilinglight <> nil) and c^.clights then
              begin
                outersec^.ceiling_flat := outersec^.style^.ceilinglight;
                announce(VERBOSE, 'acl');
              end;
            end; // end general lighting effects
            // Put something in the closet
            point_from(ldnew^.from^.x, ldnew^.from^.y, ldnew^._to^.x, ldnew^._to^.y,
                       RIGHT_TURN, 32, @x1, @y1);
            point_from(ldnew^._to^.x, ldnew^._to^.y, x1, y1, RIGHT_TURN, 32, @x1, @y1);
            // Eek, a monster!
            m := timely_monster(haa, c, @levels, rollpercent(l^.p_biggest_monsters), 1);
            // Should check for monster width here!!
            if m = nil then
              new_thing(l, x1, y1, 0, ID_POTION, 7, c) // Punt.  Stub.
            else
            begin
              angle := facing_right_from_ld(ldnew);
              new_thing(l, x1, y1, angle,m^.thingid, levels or $08, c); // Deaf
              if rollpercent(50) then
                if m^.bits and SHOOTS <> 0 then
                  ld^.flags := ld^.flags or BLOCK_MONSTERS;
              update_haa_for_monster(haa, m, levels, 0, c); // zero?  one?
            end; // end there was a monster
            // Maybe some small bonus also
            if rollpercent(15) and (ld^.typ = 0) then // Not if ld is a trap?
            begin
              if rollpercent(50) then // Health or whatever
              begin
                case roll(3) of
                  0: bonustype := ID_MEDIKIT;
                  1: bonustype := ID_STIMPACK;
                else bonustype := ID_POTION;
                end;
                update_haa_for_health(haa, 7, bonustype);
              end
              else // Some ammo or whatever
              begin
                if not haa^.haas[2].has_chainsaw and rollpercent(5) then
                begin
                  bonustype := ID_CHAINSAW;
                  haa^.haas[0].has_chainsaw := TRUE;
                  haa^.haas[1].has_chainsaw := TRUE;
                  haa^.haas[2].has_chainsaw := TRUE;
                end
                else
                begin
                  case roll(2) of // What about a cell? / a rocket */
                    0: bonustype := ID_CLIP;
                  else bonustype := ID_SHELLS;
                  end; // end switch
                  update_haa_for_ammo(haa, 7, bonustype);
                end;
              end; // end ammo bonuses */
              new_thing(l, x1 + 1, y1 + 1, 0, bonustype, 7, c); // Place the bonus
            end; // end some small bonus
          end; // end if empty space
        end; // end if found a linedef
      end; // end for k
    end; // end if 1/n

  if rollpercent(15) then
  begin
    // One or more wall-plaques,
    // with occasional secrets and monsters
    sync_doors := rollpercent(c^.p_sync_doors);
    sync_tag := -1;
    sync_count := 0; // Just for announcing
    pheight := ThisStyle^.plaque^.height;
    if ThisStyle^.plaque^.props and HALF_PLAQUE <> 0 then
      if rollpercent(80) then
        pheight := pheight div 2;
    pup := ((oldsector^.ceiling_height - oldsector^.floor_height) - pheight) div 2;
    pup := roll(pup);
    tplaque := ThisStyle^.plaque;
    if oldsector^.ceiling_height - oldsector^.floor_height > pheight then
    begin
      for j := 1 to 10000 do // JVAL
      begin
        i := mark_decent_boundary_linedefs(l, oldsector, 128); // 128 is wrong
        ld := random_marked_linedef(l, i);
        unmark_linedefs(l);
        if ld <> nil then
        begin
          t1 := ld^.right^.middle_texture;
          ldlen := linelen(ld);
          // Use borderize(TRUE), to get possible lightboxes etc
          if rollpercent(5) then
          begin
            ThisStyle^.lightboxes := TRUE;
            ThisStyle^.auxheight := pup; // "pheight" here is a nice bug!
            announce(VERBOSE, 'fancy plaque');
          end;
          ld := borderize(l, ld, 128, TRUE, ThisStyle, LIGHT, nil, nil, c);
          ThisStyle^.lightboxes := FALSE; // Neaten up
          depth := 4 + roll(5) + roll(5);
          if empty_left_side(l, ld, depth) then
          begin
            announce(VERBOSE, 'Putting in a plaque');
            ldnew := lefthand_box_ext(l, ld, depth, ThisStyle, c, @ldedge1, @ldedge2);
            ldnew^.right^.middle_texture := tplaque;
            if tplaque <> ThisStyle^.plaque then
              announce(VERBOSE, 'Multiplaque');
            ldnew^.right^.sector^.floor_height := oldsector^.floor_height + pup;
            ldnew^.right^.sector^.ceiling_height := ldnew^.right^.sector^.floor_height + pheight;
            // Maybe light the recesses
            if ThisStyle^.light_recesses and (ThisStyle^.walllight <> nil) then
            begin
              announce(VERBOSE, 'Lit plaque');
              ldedge2^.right^.middle_texture := ThisStyle^.walllight;
              ldedge1^.right^.middle_texture := ThisStyle^.walllight;
            end
            else
            begin
              yoff := oldsector^.ceiling_height - ldnew^.right^.sector^.ceiling_height;
              ldedge1^.right^.y_offset := yoff;
              ldedge2^.right^.y_offset := yoff;
            end;
            patch_upper(ld, t1, c);
            patch_lower(ld, t1, c);
            if rollpercent(60) then
            begin
              ldnew^.right^.sector^.light_level := oldsector^.light_level + roll(21);
              if ldnew^.right^.sector^.light_level > l^.bright_light_level then
                ldnew^.right^.sector^.light_level := oldsector^.light_level;
              if rollpercent(40) then
                case roll(4) of
                  0: ldnew^.right^.sector^.special := RANDOM_BLINK;
                  1: ldnew^.right^.sector^.special := SYNC_FAST_BLINK;
                  2: ldnew^.right^.sector^.special := SYNC_SLOW_BLINK;
                  3: ldnew^.right^.sector^.special := GLOW_BLINK;
                end; // end switch */
            end; // end if doing lights */
            if pup < 25 then
              if rollpercent(80) then // Put a secret thingie behind it!
              begin
                if sync_doors then
                  if sync_tag = -1 then
                    sync_tag := new_tag(l);
                if secret_closet(l, ldnew, ThisStyle, 0, haa, c, TRUE, sync_tag, oldsector^.ceiling_height, TRUE) <> nil then
                begin
                  announce(VERBOSE, 'Plaque closet');
                  if sync_doors then
                  begin
                    ldnew^.tag := sync_tag;
                    ldnew^.typ := LINEDEF_SR_OC_DOOR;
                    if c^.gamemask and DOOM0_BIT = 0 then
                      ldnew^.typ := LINEDEF_SR_BLAZE_OC_DOOR;
                    if sync_count <> 0 then
                      announce(VERBOSE, 'Synced doors');
                    inc(sync_count);
                  end;
                end;
              end; // end if secret closet */
          end; // end if empty space */
        end; // end if found a linedef */
        if rollpercent(50) then
          break;
        if j > 4 then
          break;
        tplaque := random_plaque(c, ThisStyle);
        if (tplaque^.height <> pheight) and
            ((tplaque^.props and HALF_PLAQUE = 0) or (tplaque^.height <> 2 * pheight))  then
          tplaque := ThisStyle^.plaque;
      end; // end for j
    end; // end if tall enough room
  end; // end if 1/10

  // The other kind(s) of secret closet
  install_closet := FALSE;
  switch_closet := FALSE;
  if rollpercent(l^.p_surprise) then
    install_closet := TRUE
  else if rollpercent(l^.p_swcloset) then
  begin
    i := mark_decent_boundary_linedefs(l, oldsector, 72);
    switch_ld := random_marked_linedef(l, i);
    unmark_linedefs(l);
    if (switch_ld <> nil) and empty_left_side(l, switch_ld, 8) then
    begin
      install_closet := TRUE;
      switch_closet := TRUE;
    end;
  end;

  if install_closet then
  begin
    t := nil;
    tag := -1;

    i := mark_decent_boundary_linedefs(l, oldsector, 128);
    ld := random_marked_linedef(l, i);
    unmark_linedefs(l);
    if switch_closet and (ld = switch_ld) then
      ld := nil; // Give up
    if ld <> nil then
    begin
      ldlen := linelen(ld);
      plen := ldlen - 64;
      if rollpercent(50) then
        if plen > 64 then
          plen := plen - roll( 1 + plen - 64);
      if plen > 256 then
        plen := 256;
      border := (ldlen - plen) div 2;
      if border <> 0 then
      begin
        ld := split_linedef(l, ld, border, c);
        split_linedef(l, ld, plen, c);
      end;
      goal_trigger := FALSE;
      if empty_left_side(l, ld, 72) then // "72" is from secret_closet()
      begin
        if ((ThisQuest^.goal = KEY_GOAL) or (ThisQuest^.goal = NULL_GOAL)) and
           not switch_closet and
           (ThisQuest^.auxtag = 0) and
           (ThisQuest^.surprise = nil) then
        begin
          // Goal-triggered, if we can
          goal_trigger := TRUE;
          tag := new_tag(l);
          ThisQuest^.auxtag := tag;
        end
        else if switch_closet then // Switch-triggered
        begin
          switch_tag := new_tag(l);
          tag := switch_tag;
        end
        else if rollpercent(60) then // Immediate-triggered
        begin
          t := l^.thing_anchor;
          while t <> nil do
          begin
            if t^.genus^.bits and PICKABLE = 0 then
            begin
              t := t^.next;
              continue;
            end;
            // Old bug: &border in next line was nil
            if oldsector <> point_sector(l, t^.x, t^.y, @border, @danger) then
            begin
              t := t^.next;
              continue;
            end;
            if border < 32 then
            begin
              t := t^.next;
              continue;
            end;
            if danger then
            begin
              t := t^.next;
              continue;
            end;
            if t^.options and $07 <> $07 then
            begin
              t := t^.next;
              continue;
            end;
            break;
          end; // end for things */
          if t <> nil then
          begin
            tag := new_tag(l);
            trigger_box(l, t, oldsector, tag, LINEDEF_WR_OPEN_DOOR, c);
          end; // if found a good thing
        end; // if triggered closet
        pheight := 72 + roll( 1 + (oldsector^.ceiling_height - oldsector^.floor_height) - 72);
        had_map := l^.seen_map;
        if goal_trigger then
          ldc := secret_closet(l, ld, ThisStyle, pheight, nil, c, tag = -1, tag,
                               oldsector^.ceiling_height, tag = -1)
        else
          ldc := secret_closet(l, ld, ThisStyle, pheight, haa, c, tag = -1, tag,
                               oldsector^.ceiling_height, tag = -1);
        if ldc <> nil then
        begin
          if switch_closet then
          begin
            switch_ld := install_switch(l, switch_ld, TRUE, FALSE, 0, ThisStyle, c, nil);
            switch_ld^.tag := switch_tag;
            if DOOM0_BIT and c^.gamemask <> 0 then
              switch_ld^.typ := LINEDEF_S1_OPEN_DOOR
            else
              switch_ld^.typ := LINEDEF_S1_BLAZE_O_DOOR;
            announce(VERBOSE, 'Switch closet');
          end;
          ld^.right^.y_offset :=
            (oldsector^.ceiling_height - oldsector^.floor_height) - 128; // 128 should be tex-height
          ld^.flags := ld^.flags or SECRET_LINEDEF;
          if tag = -1 then // Need a subtle hint here
          begin
            hinted := FALSE;
            if rollpercent(5) then
            begin
              // Use a barrel or candle */
              g := random_barrel(c, ThisStyle);
              x := (ld^.from^.x + ld^._to^.x) div 2;
              y := (ld^.from^.y + ld^._to^.y) div 2;
              if (g = nil) or rollpercent(50) then
                g := find_genus(c, ID_CANDLE);
              point_from(ld^.from^.x, ld^.from^.y, x, y, RIGHT_TURN, g^.width div 2, @x, @y);
              if room_at(l, g, x, y, g^.width div 2, c) then
              begin
                hinted := TRUE;
                new_thing(l, x, y, 0, g^.thingid, 7, c);
              end;
            end;
            if not hinted and had_map and rollpercent(15) then
            begin
              // Make 'em use the map!
              hinted := TRUE;
            end;
            if not hinted and rollpercent(40) and
                (ld^.right^.upper_texture^.y_hint <> 0) then
            begin
              // Typical misalign-hint
              ld^.right^.y_misalign := ld^.right^.upper_texture^.y_hint;
              hinted := TRUE;
            end;
            if not hinted and rollpercent(90) and
                (ld^.right^.upper_texture^.subtle <> nil) then
            begin
              // Subtly different texture
              ld^.right^.upper_texture := ld^.right^.upper_texture^.subtle;
              announce(VERBOSE, 'subtle');
              hinted := TRUE;
            end;
            if not hinted then
            begin
              // Make it just show on the automap (always possible)
              ld^.flags := ld^.flags and not SECRET_LINEDEF;
              announce(VERBOSE, 'Map hint');
              hinted := TRUE;
            end;
          end;
          if goal_trigger then
          begin
            ThisQuest^.surprise := ldc;
            announce(VERBOSE, 'Goal-trigger');
          end
          else if tag <> -1 then
            announce(VERBOSE, 'Trigger');
        end
        else if goal_trigger then
        begin
          ThisQuest^.auxtag := 0;
        end; // end else if secret closet failed and we were goal triggered */
      end; // end if enough room */
    end; // end if found a linedef */
  end; // end if try a closet

  if rollpercent(l^.p_extroom) then
    make_extroom(l, oldsector, haa, ThisStyle, c);

  if rollpercent(l^.p_extwindow) then // Trivial windows to the outside
  begin
    make_extwindow(l, oldsector, ThisStyle, c);
  end; // end if try a window */

  if rollpercent(3) and (ThisStyle^.walllight <> nil) then
  begin
    // Lightstrips on the walls
    announce(VERBOSE, 'Doing the lightstrip thing.');
    sdepth := 4 + 4 * roll(2);
    ll := oldsector^.light_level;
    if ll < l^.lit_light_level then
      ll := ll + 20;
    // Mess with the light-motion sometimes
    if rollpercent(15) then
    begin
      case roll(4) of
        0: spec := RANDOM_BLINK;
        1: spec := SYNC_FAST_BLINK;
        2: spec := SYNC_SLOW_BLINK;
      else spec := GLOW_BLINK;
      end;
    end
    else
      spec := 0;
    fh := oldsector^.floor_height + 8 * roll(8); // Boring?
    ch := fh + roll(24) + 24 * roll(2);
    if ch > oldsector^.ceiling_height then
      ch := oldsector^.ceiling_height;
    ld := l^.linedef_anchor;
    while ld <> nil do
    begin
      if ld^.typ = 0 then
        if ld^.right <> nil then
          if ld^.left = nil then
            if ld^.right^.sector = oldsector then
              if lengthsquared(ld) > (8 * 8) then
                if ld^.right^.isBoundary then
                  make_lightstrip(l, ld, ThisStyle, ll, sdepth, spec, fh, ch, c);
      ld := ld^.next;
    end;
  end
  else if rollpercent(20) then
  begin
    // swell some boundaries; don't do if lightstripped!
    // NOTE: produces some non-square lines!   Terrible bugs!
    // But not as bad if you populate() first!
    announce(VERBOSE, 'Swelling the room boundaries');
    sno := 1 + roll(2);
    sdepth := 4 + 4 * roll(4);
    ld := l^.linedef_anchor;
    while ld <> nil do
    begin
      if ld^.typ = 0 then
        if ld^.left = nil then
          if ld^.right <> nil then
            if ld^.right^.sector = oldsector then
              if lengthsquared(ld) > (16 * 16) then
                if lengthsquared(ld) > (sdepth * sdepth) then
                  if ld^.right^.isBoundary then
                    swell_linedef(l, ld, ThisStyle, c, sno, sdepth);
      ld := ld^.next;
    end;
  end; // end if swell-embellish

end;

// Return a gate-type link.  Not many properties!
function gate_link_f(const l: level_p; const c: config_p): link_p;
begin
  result := link_p(SL_Malloc(SizeOf(link_t)));

  result^.bits := 0;
  result^.typ := GATE_LINK;
  result^.next := l^.link_anchor;
  l^.link_anchor := result;
end;

// From the given sector of the given level, make a next room nearby,
// and return the linedefs by which they should be joined.  nil if
// no room could be placed.  The returned linedef is the one in the
// new room; the **ldf one is the one in oldsector.
function make_next_room(const l: level_p; const oldsector: sector_p;
  const radical: boolean; const c: config_p; const ldf: linedef_pp;
  const ThisLink: link_pp; const ThisQuest: quest_p): linedef_p;
var
  newldf: linedef_p;
  i, tries: integer;
  try_reduction: boolean;
  newsector: sector_p;
  ThisStyle, NewStyle: style_p;
begin
  ThisStyle := oldsector^.style;
  newldf := nil;
  newsector := nil;
  NewStyle := new_style(l, ThisStyle, radical, c);
  for try_reduction := FALSE to TRUE do // this loop is a hack, eh? // JVAL: stick to pascal hacks :)
  begin
    for tries := 0 to 19 do
    begin
      i := mark_adequate_linedefs(l, oldsector, ThisStyle, c);
      ldf^ := random_marked_linedef(l, i);
      unmark_linedefs(l);
      if i = 0 then
      begin
        result := nil;
        exit;
      end;
      // Decide on a link-style for this link
      if (roll(3) <> 0) and link_fitsq(ThisStyle^.link0, ThisQuest) then
        ThisLink^ := ThisStyle^.link0 // Often use style default
      else // Sometimes not
        ThisLink^ := random_link(l, ldf^, ThisStyle, ThisQuest, c);
      // If we're getting really desparate, maybe use a gate
      if l^.use_gates and try_reduction and (tries > 15) and (ThisQuest = nil) then
      begin
        i := mark_adequate_linedefs(l, oldsector, ThisStyle, c);
        ldf^ := random_marked_linedef(l, i);
        unmark_linedefs(l);
        ThisLink^ := gate_link_f(l, c);
        announce(VERBOSE, 'Gate link');
      end;
      if not link_fitsh(ldf^, ThisLink^, c) then // Fix if doesn't fit
        ThisLink^ := random_link(l, ldf^, ThisStyle, ThisQuest, c);
      if not link_fitsh(ldf^, ThisLink^, c) then
        announce(WARNING, 'random_link() returned too wide!!');
      newldf := make_linkto(l, ldf^, ThisLink^, NewStyle, c, newldf);
      if not link_fitsv(l, ldf^, newldf, ThisLink^) then
      begin
        announce(VERBOSE, 'Retrying because link didn''t fit...');
        continue;
      end;
      newsector := generate_room_outline(l, newldf, NewStyle, try_reduction, c);
      if newsector <> nil then
        break;
      announce(VERBOSE, 'Retrying because new room didn''t fit...');
    end; // end until one works
    if newsector <> nil then
      break;
  end; // end with and without reduction

  if newsector = nil then
  begin
    if newldf <> nil then // Avoid engine crashes!
    begin
      delete_vertex(l, newldf^.from);
      delete_vertex(l, newldf^._to);
      delete_linedef(l, newldf);
    end;
    newldf := nil;
  end;

  result := newldf;

end; // end make_next_room()

// Place the start positions for Players 1-4 in the given sector
procedure place_start_things(const l: level_p; const s: sector_p; const c: config_p);
var
  minx, miny, maxx, maxy: integer;
  angle: SmallInt;
  rational_angles: boolean;
begin
  find_sector_rectangle(l, s, minx, miny, maxx, maxy);

  rational_angles := rollpercent(90);

  // this is invisible, but ensures that it won't work w/shareware DOOM
  new_thing(l, (minx + maxx) div 2, (miny + maxy) div 2, 90, $0017, 7, c); // Dead skull.

  // Now the start positions
  if rational_angles then
    angle := 0
  else
    angle := 90 * roll(4);
  new_thing(l, minx + 32, miny + 32, angle, ID_PLAYER1, 7, c); // 1-player start
  // In the first room, *lie* about where the player comes in
  s^.entry_x := maxx - 32;
  s^.entry_y := maxy - 32;
  if (maxx - minx < 128) or (maxy - miny < 128) then
  begin
    announce(WARNING, 'Not enough room for co-op start positions');
    exit;
  end;
  if rational_angles then
    angle := 0
  else
    angle := 90 * roll(4);
  new_thing(l, minx + 32, maxy - 32, angle, ID_PLAYER2, 7, c); // 2-player start
  if rational_angles then
    angle := 180
  else
    angle := 90 * roll(4);
  new_thing(l, maxx - 32, miny + 32, angle, ID_PLAYER3, 7, c); // 3-player start
  if rational_angles then
    angle := 180
  else
    angle := 90 * roll(4);
  new_thing(l, maxx - 32, maxy - 32, angle, ID_PLAYER4, 7, c); // 4-player start
end;

// Set all the fields of the given level to empty things
procedure empty_level(const l: level_p; const c: config_p);
var
   dieroll: integer;
begin
   l^.thing_anchor := nil;
   l^.vertex_anchor := nil;
   l^.sector_anchor := nil;
   l^.linedef_anchor := nil;
   l^.sidedef_anchor := nil;
   l^.style_anchor := nil;
   l^.link_anchor := nil;
   l^.arena_anchor := nil;
   l^.gate_anchor := nil;
   l^.used_red := FALSE;
   l^.used_blue := FALSE;
   l^.used_yellow := FALSE;
   l^.last_tag_used := 0;
   l^.sl_tag := 0;
   l^.sl_type := 0;
   l^.sl_done := FALSE;
   l^.sl_open_ok := FALSE;
   l^.sl_open_start := nil;
   l^.sl_exit_sector := nil;
   l^.first_room := nil;
   l^.goal_room := nil;
   l^.seen_suit := FALSE;
   l^.seen_map := FALSE;
   l^.scrolling_keylights := rollpercent(5);
   l^.support_misaligns := rollpercent(2); // Looks crummy!
   l^.skyclosets := 2;
   if rollpercent(10) then l^.skyclosets := roll(100);
   l^.lift_rho := 10;
   if rollpercent(25) then l^.lift_rho := 0;
   if rollpercent(15) then l^.lift_rho := roll(100);
   l^.amcl_rho := 30;
   if rollpercent(25) then l^.amcl_rho := 0;
   if rollpercent(15) then l^.amcl_rho := roll(100);
   l^.p_new_pillars := 30;
   if rollpercent(10) then l^.p_new_pillars := 0;
   if rollpercent(8) then l^.p_new_pillars := 80 + roll(40);
   l^.p_stair_lamps := 20;
   if rollpercent(5) then l^.p_stair_lamps := 50 + roll(60);
   l^.p_force_sky := 0;
   if rollpercent(5) then l^.p_force_sky := 20 + roll(60);
   if l^.p_force_sky > 30 then announce(LOG, 'Sunrooms');
   l^.p_force_nukage := 0;
   if rollpercent(8) then l^.p_force_nukage := 20 + roll(60);
   if c^.major_nukage then l^.p_force_nukage := 85;
   if l^.p_force_nukage > 30 then announce(LOG, 'Nukage city!!');
   l^.p_deep_baths := 20;
   if rollpercent(50) then l^.p_deep_baths := l^.p_deep_baths + l^.p_force_nukage;
   if rollpercent(8) then l^.p_deep_baths := 75 + roll(30);
   if rollpercent(8) then l^.p_deep_baths := 0;
   l^.p_falling_core := 0;
   if rollpercent(25) then l^.p_falling_core := 5;
   if rollpercent(5) then l^.p_falling_core := 5 + roll(30);
   l^.p_barrels := 10;
   if rollpercent(8) then l^.p_barrels := 20 + roll(30);
   if l^.p_force_nukage > 30 then
     if rollpercent(50) then
       l^.p_barrels := l^.p_force_nukage;
   l^.p_extwindow := 8;
   if rollpercent(5) then l^.p_extwindow := 15 + roll(75);
   if rollpercent(5) then l^.p_extwindow := 0;
   l^.p_extroom := 2;
   if rollpercent(5) then l^.p_extroom := 15 + roll(75);
   if rollpercent(10) then l^.p_extroom := 0;
   l^.p_rising_room := 0;
   if rollpercent(50) then l^.p_rising_room := 6;
   if rollpercent(5) then l^.p_rising_room := 25 + roll(75);
   if l^.p_force_sky > 30 then
   begin
     if rollpercent(60) then
       l^.p_extwindow := l^.p_force_sky;
     if rollpercent(60) then
       l^.skyclosets := l^.p_force_sky;
     if rollpercent(60) then
       l^.p_extroom := l^.p_force_sky;
   end;
   l^.p_surprise := 30;
   if rollpercent(10) then l^.p_surprise := 30 + roll(60);
   l^.p_swcloset := 0;
   if rollpercent(20) then l^.p_swcloset := 5;
   if rollpercent(10) then l^.p_swcloset := 5 + roll(20);
   l^.p_rational_facing := 90;
   if rollpercent(2) then l^.p_rational_facing := roll(100);
   if rollpercent(10) then l^.p_rational_facing := 100;

   announce(VERBOSE, Format('p_rational_facing %d.', [l^.p_rational_facing]));

   l^.p_biggest_monsters := 0;
   if rollpercent(5) and c^.big_monsters then l^.p_biggest_monsters := 100;
   if c^.force_biggest then l^.p_biggest_monsters := 100;
   if l^.p_biggest_monsters = 100 then announce(LOG, 'Biggest monsters');
   l^.p_open_link := 15;
   if rollpercent(15) then l^.p_open_link := 0;
   if rollpercent(20) then l^.p_open_link := roll(100);
   l^.p_s1_door := 20;
   if rollpercent(10) then l^.p_s1_door := roll(100);
   if rollpercent(5) then l^.p_s1_door := 100;
   if l^.p_s1_door > 95 then
     announce(VERBOSE, 'Doors stick');
   l^.p_special_room := 2 + roll(5);
   if rollpercent(5) then l^.p_special_room := 0;
   if rollpercent(5) then l^.p_special_room := 20 + roll(20);
   l^.secret_count := 0;
   l^.dm_count := 0;
   l^.dm_rho := 10;
   l^.first_room := nil;
   l^.skullkeys := rollpercent(50);
   l^.use_gates := rollpercent(60);
   l^.raise_gates := rollpercent(60);
   l^.no_doors := FALSE;
   l^.all_wide_links := FALSE;
   if rollpercent(15) then
   begin
     case roll(6) of
       0,
       1,
       2:
         begin
           l^.all_wide_links := TRUE;
           l^.no_doors := TRUE;
         end;
       3: l^.all_wide_links := TRUE;
       4: l^.no_doors := TRUE;
     end;
   end;
   if l^.all_wide_links then
     announce(VERBOSE, 'All wide links');
   if l^.no_doors then
     announce(VERBOSE, 'No doors');
   l^.hugeness := 1;
   if rollpercent(decide(c^.do_dm, 30, 8)) then l^.hugeness := 2;
   if l^.hugeness < c^.minhuge then l^.hugeness := c^.minhuge;
   if l^.hugeness > 1 then announce(LOG, 'Extra hugeness');
   l^.outside_light_level := 240;
   if rollpercent(20) then
   begin
     l^.outside_light_level := c^.minlight + 5;
     announce(VERBOSE, 'Night');
   end;
   l^.bright_light_level := 220;
   if rollpercent(20) then
   begin
     l^.bright_light_level := c^.minlight + roll((221 - c^.minlight) div 2);
     announce(VERBOSE, 'Dim');
   end;
   l^.lit_light_level := 220; // Always?
   dieroll := roll(100);
   if dieroll < 30 then
     l^.maxkeys := 0
   else if dieroll < 60 then
     l^.maxkeys := 1
   else if dieroll < 80 then
     l^.maxkeys := 2
   else
     l^.maxkeys := 3;
   l^.barcount := 0;
   l^.crushercount := 0;
end;

// Make a whole new level, assuming the player starts with the given
// amount of health, ammo, and armor, using the given config.
procedure NewLevel(const l: level_p; const ThisHaa: haa_p; const c: config_p);
var
   ThisStyle, NewStyle: style_p; // basic decors
   ThisQuest: quest_p; // stack of pending goals
   ThisLink, ForkLink: link_p; // Particular instances
   ldf, newldf: linedef_p;
   oldsector, newsector: sector_p;
   i, forks, nullforks: integer;
   done_quest, first_room: boolean;
   keys_used: integer;
   lld1, lld2: linedef_p;
   radical: boolean;
   newkey: SmallInt;
begin
  newldf := nil;
  newsector := nil;
  done_quest := FALSE;
  first_room := TRUE;
  keys_used := 0;

  empty_level(l, c);

  ThisStyle := random_style(l, c);
  ThisQuest := starting_quest(l, c);

  ldf := starting_linedef(l, ThisStyle, c);
  oldsector := generate_room_outline(l, ldf, ThisStyle, TRUE, c);
  l^.first_room := oldsector;

  // Make starting position(s) in the first sector
  place_start_things(l, oldsector, c);

  // first call to embellish() will add the SLIGE watermark

  NewStyle := nil;

  while true do
  begin
    // Are we done by virtue of room-count or similar?
    inc(ThisQuest^.count);
    done_quest := enough_quest(l, oldsector, ThisQuest, c);

    if not done_quest then
    begin
      newldf := make_next_room(l, oldsector, FALSE, c, @ldf, @ThisLink, nil);
      if newldf = nil then
      begin
        done_quest := true;
        if ThisQuest^.next = nil then
          announce(LOG, 'Self-collision; may be fewer rooms than expected.')
        else
          ThisQuest^.next^.minrooms := ThisQuest^.next^.minrooms + ThisQuest^.minrooms - ThisQuest^.count;
      end
      else
      begin
        newsector := newldf^.right^.sector;
        NewStyle := newsector^.style;
      end;
    end;

    paint_room(l, oldsector, ThisStyle, c);

    if not done_quest then
    begin
      establish_link(l, ldf, newldf, ThisLink, nil, ThisStyle, NewStyle, ThisHaa, c);
      maybe_push_quest(l, oldsector, ThisQuest, c);
    end
    else
      close_quest(l, oldsector, ThisQuest, ThisHaa, c);

    (*
       forking in here.  design: if we want to fork, do
       another make_next_room.  give up if fails.  if it
       works, do an establish_locked_link, and push the
       new quest onto the stack.  if the fork works, the
       establish_link above will have established a link
       to newsector in the new quest.

       pull out all the meat into a separate function sometime.
    *)

    // Fork some number of times
    nullforks := 0;
    for forks := 0 to 3 do
    begin
      if done_quest then break;
      if nullforks <> 0 then break; // Only one of these at a time
      if (forks = 0) and not rollpercent(15) then break; // 85% of rooms don't fork
      if (forks <> 0) and not rollpercent(30) then break; // 70% don't fork any more
      // This next bit should be in a routine
      ThisQuest := push_quest(ThisQuest);
      if (keys_used >= l^.maxkeys) or
         rollpercent(16) or
         (ThisQuest^.next^.goal = NULL_GOAL) then
        ThisQuest^.goal := NULL_GOAL
      else if rollpercent(60) and (new_key(l, newkey) <> 0) then
      begin
        ThisQuest^.goal := KEY_GOAL;
        ThisQuest^.typ := newkey;
      end
      else if rollpercent(10) then
      begin
        ThisQuest^.goal := GATE_GOAL;
        // Everything else decided later
      end
      else
      begin
        ThisQuest^.goal := SWITCH_GOAL;
        ThisQuest^.tag := new_tag(l);
        announce(LOG, 'switch quest');
      end;
      radical := ThisQuest^.goal <> NULL_GOAL;
      lld1 := make_next_room(l, oldsector, radical, c, @lld2, @ForkLink, ThisQuest);
      if lld1 <> nil then
      begin
        announce(VERBOSE, 'Fork');
        if forks <> 0 then
          announce(LOG, 'Multifork');
        establish_link(l, lld2, lld1, ForkLink, ThisQuest, ThisStyle,
                       lld1^.right^.sector^.style, ThisHaa, c);
        ThisQuest^.room := lld1^.right^.sector;
        if ThisQuest^.goal = NULL_GOAL then
        begin
          ThisQuest^.minrooms := 1 + roll(4);
          inc(nullforks);
        end
        else
        begin
          ThisQuest^.minrooms :=
            1 + roll(ThisQuest^.next^.minrooms - ThisQuest^.next^.count);
        end;
        ThisQuest^.next^.minrooms := ThisQuest^.next^.minrooms - ThisQuest^.minrooms;
        if ThisQuest^.next^.minrooms < 1 then
          ThisQuest^.next^.minrooms := 1;
        if ThisQuest^.goal = KEY_GOAL then
        begin
          announce(LOG, 'Key thing');
          inc(keys_used);
        end
      end
      else
      begin
        ThisQuest := pop_quest(ThisQuest); // Oh, well!
        break; // No sense in trying any more, eh?
      end;
    end; // end for

    // See if it's OK to put in a secret-level exit-opener yet */
    if oldsector = l^.sl_open_start then
      l^.sl_open_ok := TRUE;

    // Fancy up and fill in the room itself
    enhance_room(l, oldsector, ThisHaa, ThisStyle, ThisQuest, first_room, c);
    first_room := FALSE;

    // Now get ready for the next pass
    if not done_quest then
    begin
      oldsector := newsector;
      ThisStyle := NewStyle;
    end
    else
    begin
      close_quest_final(l, oldsector, ThisQuest, ThisHaa, c);
      if ThisQuest^.next = nil then
        break; // We're done!
      oldsector := ThisQuest^.room;
      ThisStyle := oldsector^.style;
      ThisQuest := pop_quest(ThisQuest);
    end;

  end; // end forever

   // Hack to avoid ammo-starvation in megawads due to leftbehinds
   // Also turn off berserk effect
   for i := ITYTD to UV do
   begin
     ThisHaa^.haas[i].ammo := ThisHaa^.haas[i].ammo * 0.75; // awful!
     ThisHaa^.haas[i].has_berserk := FALSE;
   end;

  // Sometimes turn on big stuff; probably too simple */
  if c^.big_weapons then
    c^.big_monsters := c^.big_monsters or rollpercent(50)
  else
    c^.big_monsters := c^.big_monsters or rollpercent(15);
  if c^.big_monsters then
    c^.big_weapons := c^.big_weapons or rollpercent(50)
  else
    c^.big_weapons := c^.big_weapons or rollpercent(15);

  // Do some final global twiddles
  global_paint_HOMs(l, c);
  global_align_textures(l, c);
  global_fixups(l);

  // Warn if we failed to do a secret level
  if need_secret_level(c) and not l^.sl_done then
    announce(WARNING, 'Secret level(s) may be unreachable; durn!');

  // Add emergency deathmatch starts, if -dm and needed, announce count
  if c^.do_dm then
  begin
    while l^.dm_count < 4 do
    begin
      if maybe_add_dm_start(l, l^.first_room, c, TRUE) then
        continue;
      if maybe_add_dm_start(l, l^.goal_room, c, TRUE) then
        continue;
      announce(ERROR, 'Not enough deathmatch starts!');
      break;
    end;
    announce(LOG, Format('%d deathmatch starts.', [l^.dm_count]));
  end;

  // and finally, always have at least one "secret",  for the 100%
  if l^.secret_count = 0 then
    if l^.first_room <> nil then
      l^.first_room^.special := SECRET_SECTOR;
end;

//****** the end of SLIGE.C ********* please come again *********/

//
//   Now admit it; what was more fun than farting around with
//   level-editors, wasn't it?
//

end.
