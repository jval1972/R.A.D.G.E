// radge - [RA]ndom [D]oom [GE]nerator based on SLIGE 490
// 2017 - 2019 Jim Valavanis

unit sl_nodes;

interface

uses
  sl_helpers;

//- Doom Structures .. Colin Reed 1994 -------------------------------------

type
  uint32_t = LongWord;
  uint32_a = array[0..$FFFF] of uint32_t;
  uint32_pa = ^uint32_a;

  int32_t = integer;
  int32_a = array[0..$FFFF] of int32_t;
  int32_pa = ^int32_a;

  uint16_t = Word;
  uint16_a = array[0..$FFFF] of uint16_t;
  uint16_pa = ^uint16_a;

  int16_t = SmallInt;
  int16_a = array[0..$FFFF] of int16_t;
  int16_pa = ^int16_a;

  int8_t = byte;
  int8_a = array[0..$FFFF] of int8_t;
  int8_pa = ^int8_a;

type
  wadheader_p = ^wadheader_t;
  wadheader_t = packed record // Linked wad files list.
    _type: packed array[0..3] of char;
    num_entries: uint32_t;
    dir_start: uint32_t;
  end;

  directory_p = ^directory_t;
  directory_t = packed record // The directory_t entry header
    start: uint32_t;
    len: uint32_t;
    name: packed array[0..7] of char;
  end;
  directory_a = array[0..$FFFF] of directory_t;
  directory_pa = ^directory_a;

  block_p = ^block_t;
  block_t = packed record
    minx: int16_t;
    miny: int16_t;
    xblocks: int16_t;
    yblocks: int16_t;
  end;

  lumplist_pp = ^lumplist_p;
  lumplist_p = ^lumplist_t;
  lumplist_t = packed record
    next: lumplist_p;
    dir: directory_p;
    data: pointer;
    level: lumplist_p;
  end;

//- The level structures ---------------------------------------------------
  thing_p = ^thing_t;
  thing_t = packed record
    xpos: int16_t;      // x position
    ypos: int16_t;      // y position
    angle: int16_t;     // facing angle
    _type: int16_t;     // thing type
    when: int16_t;      // appears when?
  end;

  vertex_p = ^vertex_t;
  vertex_t = packed record
    x: int16_t;         // X coordinate
    y: int16_t;         // Y coordinate
  end;
  vertex_a = packed array[0..$FFFF] of vertex_t;
  vertex_pa = ^vertex_a;

  linedef_p = ^linedef_t;
  linedef_t = packed record
    start: uint16_t;    // from this vertex ...
    _end: uint16_t;     // ... to this vertex
    flags:uint16_t;     // see NAMES.C for more info
    _type: uint16_t;    // see NAMES.C for more info
    tag: uint16_t;      // crossing this linedef activates the sector with the same tag
    sidedef1: int16_t;  // sidedef
    sidedef2: int16_t;  // only if this line adjoins 2 sectors
  end;
  linedef_a = packed array[0..$FFFF] of linedef_t;
  linedef_pa = ^linedef_a;

  sidedef_p = ^sidedef_t;
  sidedef_t = packed record
    xoff: int16_t;      // X offset for texture
    yoff: int16_t;      // Y offset for texture
    tex1: packed array[0..7] of char;  // texture name for the part above
    tex2: packed array[0..7] of char;  // texture name for the part below
    tex3: packed array[0..7] of char;  // texture name for the regular part
    sector: int16_t;    // adjacent sector
  end;
  sidedef_a = packed array[0..$FFFF] of sidedef_t;
  sidedef_pa = ^sidedef_a;

  sector_p = ^sector_t;
  sector_t = packed record
    floorh: int16_t;    // floor height
    ceilh: int16_t;     // ceiling height
    floort: packed array[0..7] of char; // floor texture
    ceilt: packed array[0..7] of char;  // ceiling texture
    light: int16_t;     // light level (0-255)
    special: int16_t;   // special behaviour (0 = normal, 9 = secret, ...)
    tag: int16_t;       // sector activated by a linedef with the same tag
  end;
  sector_a = packed array[0..$FFFF] of sector_t;
  sector_pa = ^sector_a;

//--------------------------------------------------------------------------
// These are the structure used for creating the NODE bsp tree.
//--------------------------------------------------------------------------
  seg_p = ^seg_t;
  seg_t = packed record
    start: int16_t;   // from this vertex ...
    _end:int16_t;     // ... to this vertex
    angle: uint16_t;  // angle (0 = east, 16384 = north, ...)
    linedef: int16_t; // linedef that this seg goes along
    flip: int16_t;    // true if not the same direction as linedef
    dist: uint16_t;   // distance from starting point
    next: seg_p;
    psx, psy, pex, pey: int16_t;  // Start, end coordinates
    pdx, pdy, ptmp: int32_t;      // Used in intersection calculations
    len: int32_t;
    sector: int16_t;
  end;
  seg_a = packed array[0..$FFFF] of seg_t;
  seg_pa = ^seg_a;

  pseg_p = ^pseg_t;
  pseg_t = packed record
    start: int16_t;   // from this vertex ...
    _end: int16_t;    // ... to this vertex
    angle: uint16_t;  // angle (0 = east, 16384 = north, ...)
    linedef: int16_t; // linedef that this seg goes along
    flip: int16_t;    // true if not the same direction as linedef
    dist: uint16_t;   // distance from starting point
  end;
  pseg_a = packed array[0..$FFFF] of pseg_t;
  pseg_pa = ^pseg_a;

// cph - dedicated type for bounding boxes, as in the Doom source
  bbox_t = packed array[0..3] of int16_t;

const
  BB_TOP = 0;
  BB_BOTTOM = 1;
  BB_LEFT = 2;
  BB_RIGHT = 3;

type
  node_p = ^node_t;
  node_t = packed record
    x, y: int16_t;            // starting point
    dx, dy: int16_t;          // offset to ending point
    rightbox: bbox_t;         // bounding rectangle 1
    leftbox: bbox_t;          // bounding rectangle 2
    chright, chleft: int16_t; // node_t or subsector_t (if high bit is set)
    nextr, nextl: node_p;
    node_num: int16_t;        // starting at 0 (but reversed when done)
    ptmp: int32_t;
  end;
  node_a = packed array[0..$FFFF] of node_t;
  node_pa = ^node_a;

  pnode_p = ^pnode_t;
  pnode_t = packed record
    x, y: int16_t;            // starting point
    dx, dy: int16_t;          // offset to ending point
    rightbox: bbox_t;         // bounding rectangle 1
    leftbox: bbox_t;          // bounding rectangle 2
    chright, chleft: int16_t; // node_t or subsector_t (if high bit is set)
  end;
  pnode_a = packed array[0..$FFFF] of pnode_t;
  pnode_pa = ^pnode_a;

  subsector_p = ^subsector_t;
  subsector_t = packed record
    num: uint16_t;       // number of Segs in this Sub-sector_t
    first: uint16_t;     // first seg_t
  end;
  subsector_a = packed array[0..$FFFF] of subsector_t;
  subsector_pa = ^subsector_a;

//--------------------------------------------------------------------------
function PickNode_traditional(ts: seg_p; const bbox: bbox_t): seg_p;
function PickNode_visplane(ts: seg_p; const bbox: bbox_t): seg_p;
//--------------------------------------------------------------------------
procedure CreateBlockmap_old(const bbox: bbox_t);
procedure CreateBlockmap_compressed(const bbox: bbox_t);
//--------------------------------------------------------------------------
procedure DoLevel(const current_level_name: string; const current_level: lumplist_p);
function DoLinesIntersect: integer;
procedure ComputeIntersection(var outx, outy: int16_t);

function _tmain_nodes51(const args: TDSTringList): integer;

implementation

uses
  Math;

//- BSP.C ----------------------------------------------------------------------
//
// node_t builder for DOOM levels (c) 1998 Colin Reed, version 3.0 (dos extended)
//
// Performance increased 200% over 1.2x
//
// Many thanks to Mark Harrison for finding a bug in 1.1 which caused some
// texture align problems when a flipped SEG was split.
//
// Credit to:-
//
// Raphael Quinet (A very small amount of code has been borrowed from DEU).
//
// Matt Fell for the doom specs.
//
// Lee Killough for performance tuning, support for multilevel wads, special
// effects, and wads with lumps besides levels.
//
// Also, the original idea for some of the techniques where also taken from the
// comment at the bottom of OBJECTS.C in DEU, and the doc by Matt Fell about
// the nodes.
//
// Use this code for your own editors, but please credit me.
//
//------------------------------------------------------------------------------

const
  O_BINARY = 0;

//- Global Vars ------------------------------------------------------------
var
  outfile: file;
  testwad: string;
  outwad: string;

  direc: directory_p = nil;

type
  picknode_t = function(seg: seg_p; const bbox: bbox_t): seg_p;
  createblockmap_t = procedure(const bbox: bbox_t);

var
  PickNode: picknode_t;
  CreateBlockmap: createblockmap_t;

  visplane: boolean = false;
  noreject: integer;
  pcnt: byte;

  lumplist, current_level: lumplist_p;
  wad: wadheader_t;

//- Prototypes -------------------------------------------------------------

  infile: file;

// fcopy - function to completely copy one stream to another
procedure fcopy(var fin, fout: file);
var
  buf: packed array[0..1023] of byte;
  nr, nw: integer;
begin
  repeat
    BlockRead(fin, buf, SizeOf(buf), nr);
    BlockWrite(fout, buf, nr, nw);
  until (nr = 0) or (nw <> nr);
end;

//--------------------------------------------------------------------------
// Print stuff if verbose output
var
  verbosity: integer;

procedure Verbose(const str: string); overload;
begin
  if verbosity > 0 then
    printf(str);
end;

procedure Verbose(const Fmt: string; const Args: array of const); overload;
begin
  if verbosity > 0 then
    printf(Fmt, Args);
end;

procedure progress;
begin
  inc(pcnt);
  if (verbosity > 1) and (pcnt and 31 = 0) then
    Verbose('.');
end;

const
  IWAD = integer(Ord('I') or
                (Ord('W') shl 8) or
                (Ord('A') shl 16) or
                (Ord('D') shl 24));

  PWAD = integer(Ord('P') or
                (Ord('W') shl 8) or
                (Ord('A') shl 16) or
                (Ord('D') shl 24));

function doomstr(const A: array of char; const sz: integer): string;
var
  i: integer;
begin
  result := '';
  for i := 0 to sz - 1 do
  begin
    if A[i] = #0 then
      exit;
    result := result + A[i];
  end;
end;

procedure setdoomstr(var A: array of char; const str: string; const sz: integer);
var
  i: integer;
begin
  for i := 0 to sz - 1 do
    A[i] := #0;
  for i := 1 to length(str) do
  begin
    A[i - 1] := str[i];
    if i = sz then
      break;
  end;
end;

//- find the pointer to a resource -----------------------

function FindDir(const name: string): lumplist_p;
begin
  result := current_level;
  while (result <> nil) and (doomstr(result.dir.name, 8) <> name) do
    result := result.next;
end;

//- get the directory_t from a wad file --------------------------------------
// rewritten by Lee Killough to support multiple levels and extra lumps
function OpenWadFile(const filename: string): integer;
var
  i: integer;
  dir: directory_p;
  levelp: lumplist_p;
  levels: integer;
  l: lumplist_p;
  islevel: boolean;
  isdoom1, isdoom2: boolean;
begin
  levelp := nil;
  levels := 0;

  if not fopen(infile, filename, fOpenReadOnly) then
  begin
    I_Warning('OpenWadFile(): Cannot open WAD file %s', [filename]);
    result := 0;
    exit;
  end;

  if (fread(@wad, 1, SizeOf(wad), infile) <> SizeOf(wad)) or
     ((PInteger(@wad._type)^ <> IWAD) and (PInteger(@wad._type)^ <> PWAD)) then
  begin
    I_Warning('OpenWadFile(): %s does not appear to be a WAD file -- bad magic', [filename]);
    result := 0;
    exit;
  end;

  Verbose('Opened %sWAD file: %s. %d dir entries at %d'#13#10,
    [wad._type[0], filename, wad.num_entries, wad.dir_start]);

  direc := SL_Malloc(SizeOf(directory_t) * wad.num_entries);

  fseek(infile, wad.dir_start);
  fread(direc, SizeOf(directory_t), wad.num_entries, infile);

  current_level := nil;
  dir := direc;
  for i := 0 to wad.num_entries - 1 do
  begin
    l := SL_Malloc(SizeOf(lumplist_t));
    islevel := false;

    l.dir := dir;
    l.data := nil;
    l.next := nil;
    l.level := nil;

    isdoom1 := (dir.name[0] = 'E') and
               CIsInteger(dir.name[1]) and
               (dir.name[2] = 'M') and
               CIsInteger(dir.name[3]) and
               (dir.name[4] = #0);
    isdoom2 := (dir.name[0] = 'M') and
               (dir.name[1] = 'A') and
               (dir.name[2] = 'P') and
               CIsInteger(dir.name[3]) and
               CIsInteger(dir.name[4]) and
               (dir.name[5] = #0);

    if isdoom1 or isdoom2 then
    begin
      // Begin new level
      islevel := true;
      inc(levels);
    end
    else if levelp <> nil then // The previous lump was part of a level, is this one?
    begin
      if (doomstr(dir.name, 8) = 'SEGS') or
         (doomstr(dir.name, 8) = 'SSECTORS') or
         (doomstr(dir.name, 8) = 'NODES') or
         (doomstr(dir.name, 8) = 'BLOCKMAP') or
         (doomstr(dir.name, 8) = 'BEHAVIOR') or
         (doomstr(dir.name, 8) = 'SCRIPTS') or
         ((noreject = 0) and (doomstr(dir.name, 8) = 'REJECT')) then
        continue;  // Ignore these since we're rebuilding them anyway

      if FindDir(dir.name) <> nil then
      begin
        Verbose('Warning: Duplicate entry "%s" ignored in %s'#13#10,
          [doomstr(dir.name, 8), doomstr(current_level.dir.name, 8)]);
        inc(dir);
        continue;
      end;

      if (doomstr(dir.name, 8) = 'THINGS') or
         (doomstr(dir.name, 8) = 'LINEDEFS') or
         (doomstr(dir.name, 8) = 'SIDEDEFS') or
         (doomstr(dir.name, 8) = 'VERTEXES') or
         (doomstr(dir.name, 8) = 'SECTORS') or
         ((noreject <> 0) and (doomstr(dir.name, 8) = 'REJECT')) then
      begin // Store in current level
        levelp.next := l;
        levelp := l;
        inc(dir);
        continue;
      end;
      // Otherwise, it's the end of the level.
    end;

    // If that's the end of the level, move its list of lumps to the subtree.
    if levelp <> nil then
      current_level.level := current_level.next;

    // Is this lump the start of a new level?
    if islevel then
      levelp := l
     else
      levelp := nil;

    // Add this lump or completed level to the lump list
    if current_level <> nil then
      current_level.next := l
    else
      lumplist := l;
    current_level := l;
    inc(dir);
  end;

  //  If very last lump was a level, we have to do th same housekeeping here,
  //  moving its lumps to the subtree and terminating the list.
  if levelp <> nil then
  begin
    current_level.level := current_level.next;
    current_level.next := nil;
  end;

  result := levels;
end;

// ReadLump - read a lump into memory
function ReadLump(const l: lumplist_p): pointer;
var
  dir: directory_p;
begin
  dir := l.dir;
  if (l.data = nil) and (dir.len > 0) then
  begin
    l.data := SL_Malloc(dir.len);
    if not fseek(infile, dir.start) or
       (fread(l.data, 1, dir.len, infile) <> dir.len) then
      I_Warning('ReadLump() Unable to read wad directory_t entry "%s" in %s'#13#10,
                [doomstr(dir.name, 8), doomstr(current_level.dir.name, 8)]);
  end;
  result := l.data;
end;

// Add a lump to current level
//   by Lee Killough

procedure add_lump(const name: string; const data: pointer; const len: integer);
var
  l: lumplist_p;
begin
  l := current_level;
  while l <> nil do
  begin
    if name = doomstr(l.dir.name, 8) then
      break;
    l := l.next;
  end;
  if l = nil then
  begin
    l := current_level;
    while l.next <> nil do
      l := l.next;
    l.next := SL_Malloc(SizeOf(lumplist_t));
    l := l.next;
    l.next := nil;
    l.dir := SL_Malloc(SizeOf(directory_t));
    setdoomstr(l.dir.name, name, 8);
  end;
  l.dir.len := len;
  l.level := nil;
  l.data := data;
end;

function write_lump(const lump: lumplist_p): directory_t;
var
  sz: integer;
begin
  ReadLump(lump); // cph - fetch into memory if not there already
  sz := ftell(outfile);
  lump.dir.start := sz;
  if (sz = -1) or
     ((lump.dir.len > 0) and not fwrite(lump.data, 1, lump.dir.len, outfile)) then
    I_Warning('write_lump(): Failure writing %s'#13#10, [doomstr(lump.dir.name, 8)]);
  if lump.data <> nil then
  begin
    SL_Free(lump.data);
    lump.data := nil;
  end;

  result := lump.dir^;
end;

procedure sortlump(const link: lumplist_pp);
const
  lumps: array[0..9] of string = (
    'THINGS',
    'LINEDEFS',
    'SIDEDEFS',
    'VERTEXES',
    'SEGS',
    'SSECTORS',
    'NODES',
    'SECTORS',
    'REJECT',
    'BLOCKMAP'
  );
var
  i: integer;
  l: lumplist_pp;
  t: lumplist_p;
begin
  i := 10;
  repeat
    dec(i);
    l := link;
    while l^ <> nil do
    begin
      if lumps[i] = doomstr((l^).dir.name, 8) then
      begin
        t := (l^).next;
        (l^).next := link^;
        link^ := l^;
        l^ := t;
        break;
      end;
      l := @(l^).next;
    end;
  until i = 0;
end;

const
  BSPVERSION = '5.1 (Pascal)';

procedure usage;
begin
  printf(#13#10);
  printf('BSP v' + BSPVERSION + #13#10#13#10);
  printf('See the file AUTHORS for a complete list of credits and contributors'#13#10#13#10);
  printf('Usage: bsp [options] input.wad [[-o] <output.wad>]'#13#10);
  printf('       (If no output.wad is specified, tmp.wad is written)'#13#10#13#10);
  printf('Options:'#13#10#13#10);
  printf('  -factor <nnn>  Changes the cost assigned to SEG splits'#13#10);
  printf('  -picknode {traditional|visplane}'#13#10);
  printf('                 Selects either the traditional nodeline choosing algorithm'#13#10);
  printf('                 (balance the tree and minimise splits) or Lee''s algorithm'#13#10);
  printf('                 to minimise visplanes (try to balance distinct sector refs)'#13#10);
  printf('  -blockmap {old|comp}'#13#10);
  printf('                 Selects either the old straightforward blockmap'#13#10);
  printf('                 generation, or the new compressed blockmap code'#13#10);
  printf('  -noreject      Does not clobber reject map'#13#10);
  printf('  -q             Quiet mode (only errors are printed)'#13#10);
  halt(1);
end;

// This is the original "factor" used by previous versions of the code -- it
// must be maintained in a macro to avoid mistakes if we are to keep the
// tradition of using it, and being able to modify it.
const
  DEFFACTOR = 17;

var
  factor: integer = DEFFACTOR;
  quiet: boolean;


// JVAL: simplified
procedure parse_options(const inpargs: TDStringList);
var
  p: integer;
  args: TDStringList;
  chk: string;
begin
  args := TDStringList.Create;
  args.Text := inpargs.Text;

  // PickNode
  p := args.IndexOf('-picknode');
  if (p >= 0) and (p < args.Count - 1) then
  begin
    if args.Strings[p + 1] = 'traditional' then
    begin
      PickNode := PickNode_traditional;
      verbose('%s: %s'#13#10, ['-picknode', 'Optimising for SEG splits and balance']);
      args.Delete(p + 1);
    end
    else if args.Strings[p + 1] = 'visplane' then
    begin
      PickNode := PickNode_visplane;
      verbose('%s: %s'#13#10, ['-picknode', 'Optimising for fewest visplanes']);
      visplane := true;
      args.Delete(p + 1);
    end;
  end;

  // CreateBlockmap
  p := args.IndexOf('-blockmap');
  if (p >= 0) and (p < args.Count - 1) then
  begin
    if args.Strings[p + 1] = 'old' then
    begin
      CreateBlockmap := CreateBlockmap_old;
      verbose('%s: %s'#13#10, ['-blockmap', 'BSP v3.0 blockmap algorithm']);
      args.Delete(p + 1);
    end
    else if args.Strings[p + 1] = 'comp' then
    begin
      CreateBlockmap := CreateBlockmap_compressed;
      verbose('%s: %s'#13#10, ['-blockmap', 'Compressed blockmap']);
      args.Delete(p + 1);
    end;
  end;

  // noreject
  p := args.IndexOf('-noreject');
  if (p >= 0) and (p < args.Count) then
    noreject := 1
  else
    noreject := 0;

  // quiet
  p := args.IndexOf('-q');
  if (p >= 0) and (p < args.Count) then
    quiet := true
  else
    quiet := false;

  // factor
  p := args.IndexOf('-factor');
  if (p >= 0) and (p < args.Count - 1) then
  begin
    factor := atoi(args.Strings[p + 1], -1);
    args.Delete(p + 1);
  end
  else
    factor := DEFFACTOR;

  // output filename
  p := args.IndexOf('-o');
  if (p >= 0) and (p < args.Count - 1) then
  begin
    outwad := args.Strings[p + 1];
    args.Delete(p + 1);
  end
  else
    outwad := 'tmp.wad';  // Default output name

  // input filename
  testwad := '';
  p := args.IndexOf('-i');
  if (p >= 0) and (p < args.Count - 1) then
  begin
    testwad := args.Strings[p + 1];
    args.Delete(p + 1);
  end
  else
  begin
    for p := 0 to args.Count - 1 do
    begin
      chk := args.Strings[p];
      if chk <> '' then
        if chk[1] <> '-' then
          if testwad = '' then
            testwad := chk
          else
            I_Warning('parse_options(): Unknown command line parameter "%s"'#13#10, [chk]);
    end;
  end;

  if (testwad = '') or (factor < 0) then
    usage;
end;

//- Main Program -----------------------------------------------------------
function _tmain_nodes51(const args: TDSTringList): integer;
var
  lump: lumplist_p;
  newdirec: directory_pa;
  levels: integer;
  current_level_name: string;
  l: lumplist_p;
  sz: integer;
begin
  parse_options(args);

  if quiet then
    verbosity := 0
  else
    verbosity := 2;


  if verbosity <> 0 then
    Verbose('* Doom BSP node builder ver ' + BSPVERSION + #13#10 +
            'Copyright (c)    1998 Colin Reed, Lee Killough'#13#10 +
            '        2001 Simon Howard'#13#10 +
            '        2000,2001,2002,2006 Colin Phipps <cph@moria.org.uk>'#13#10 +
            '        2019 Valavanis Jim'#13#10#13#10);

  levels := OpenWadFile(testwad); // Opens and reads directory_t

  Verbose('Creating nodes using tunable factor of %d'#13#10, [factor]);

  if visplane then
  begin
    Verbose(#13#10'Taking special measures to reduce the chances of visplane overflow');
    PickNode := PickNode_visplane;
  end;

  if not fopen(outfile, outwad, fCreate) then
  begin
    printf('Error: Could not open output PWAD file "%s"'#13#10, [outwad]);
    halt(1);
  end;

  // Allocate space for existing lumps plus some extra for each level
  newdirec := SL_Malloc((wad.num_entries + 10 * levels) * SizeOf(directory_t));

  wad.num_entries := 0;

  fwrite(@wad, SizeOf(wadheader_t), 1 , outfile);

  lump := lumplist;
  while lump <> nil do
  begin
    newdirec[wad.num_entries] := write_lump(lump);
    inc(wad.num_entries);
    if lump.level <> nil then
    begin
      current_level_name := doomstr(lump.dir.name, 8);
      current_level := lump.level;
      DoLevel(current_level_name, current_level);
      sortlump(@lump.level);
      l := lump.level;
      while l <> nil do
      begin
        newdirec[wad.num_entries] := write_lump(l);
        inc(wad.num_entries);
        l := l.next;
      end;
    end;  // if (lump->level)
    lump := lump.next;
  end;

  sz := ftell(outfile);
  wad.dir_start := sz;
  if (sz = -1) or not fwrite(newdirec, SizeOf(directory_t), wad.num_entries, outfile) then
    I_Warning('Failure writing lump directory');
  SL_Free(newdirec);

  if not fseek(outfile, 0) or not fwrite(@wad, 1, 12, outfile) then
    I_Warning('Failure writing wad header');

  closefile(outfile);

  Verbose(#13#10'Saved WAD as %s'#13#10, [outwad]);

  result := 0;
end;

var
  vertices: vertex_pa;
  num_verts: integer = 0;

  linedefs: linedef_pa;
  num_lines: integer = 0;

  sidedefs: sidedef_pa;
  num_sides: integer = 0;

  sectors: sector_pa;
  num_sects: integer = 0;

  ssectors: subsector_pa;
  num_ssectors: integer = 0;

  psegs: pseg_pa = nil;
  num_psegs: integer = 0;

  pnodes: pnode_pa = nil;
  num_pnodes: integer = 0;
  pnode_indx: integer = 0;
  num_nodes: integer = 0;

  SectorHits: int8_pa;

function IsLineDefInside(const ldnum, xmin, ymin, xmax, ymax: integer): boolean;
var
  x1, y1, x2, y2: integer;
  t, count: integer;
begin
  result := false;
  x1 := vertices[linedefs[ldnum].start].x;
  y1 := vertices[linedefs[ldnum].start].y;
  x2 := vertices[linedefs[ldnum]._end].x;
  y2 := vertices[linedefs[ldnum]._end].y;
  count := 2;

  while true do
    if y1 > ymax then
    begin
      if y2 > ymax then
        exit;
      x1 := x1 + round((x2 - x1) * (ymax - y1) / (y2 - y1));
      y1 := ymax;
      count := 2;
    end
    else if y1 < ymin then
    begin
      if y2 < ymin then
        exit;
      x1 := x1 + round((x2 - x1) * (ymin - y1) / (y2 - y1));
      y1 := ymin;
      count := 2;
    end
    else if x1 > xmax then
    begin
      if x2 > xmax then
        exit;
      y1 := y1 + round((y2 - y1) * (xmax - x1) / (x2 - x1));
      x1 := xmax;
      count := 2;
    end
    else if x1 < xmin then
    begin
      if x2 < xmin then
        exit;
      y1 := y1 + round((y2 - y1) * (xmin - x1) / (x2 - x1));
      x1 := xmin;
      count := 2;
    end
    else
    begin
      dec(count);
      if count = 0 then
      begin
        result := TRUE;
        exit;
      end;
      t := x1;
      x1 := x2;
      x2 := t;
      t := y1;
      y1 := y2;
      y2 := t;
    end;
end;

//- Create blockmap --------------------------------------------------------
function divless(const inp: integer; const frac: integer): integer;
begin
  if inp = 0 then
    result := 0
  else if inp > 0 then
    result := (inp div frac) * frac
  else
    result := -(((0 - inp) div frac) * frac);
end;

procedure CreateBlockmap_old(const bbox: bbox_t);
var
  blockhead: block_t;
  blockptrs_old: int16_pa;
  blocklists: int16_pa;
  blockptrs_old_size: integer;
  blockoffs: integer;
  x, y, n: integer;
  blocknum: integer;
  blockmap_size: integer;
  data: int8_pa;
begin
  Verbose('Creating Blockmap... ');
  blocklists := nil;
  blockoffs := 0;
  blocknum := 0;


  blockhead.minx := divless(bbox[BB_LEFT], 8);
  blockhead.miny := divless(bbox[BB_BOTTOM], 8);
  blockhead.xblocks := ((bbox[BB_RIGHT] - divless(bbox[BB_LEFT], 8)) div 128) + 1;
  blockhead.yblocks := ((bbox[BB_TOP] - divless(bbox[BB_BOTTOM], 8)) div 128) + 1;

  blockptrs_old_size := (blockhead.xblocks * blockhead.yblocks) * 2;
  blockptrs_old := SL_Malloc(blockptrs_old_size);

  for y := 0 to blockhead.yblocks - 1 do
  begin
    for x := 0 to blockhead.xblocks - 1 do
    begin
      progress();

      blockptrs_old[blocknum] := (blockoffs + 4 + (blockptrs_old_size div 2));

      blocklists := SL_Realloc(pointer(blocklists), ((blockoffs + 1) * 2));
      blocklists[blockoffs] := 0;
      inc(blockoffs);
      for n := 0 to num_lines - 1 do
      begin
        if IsLineDefInside(n, (blockhead.minx + (x * 128)), (blockhead.miny + (y * 128)), (blockhead.minx + (x * 128)) + 127, (blockhead.miny + (y * 128)) + 127) then
        begin
          blocklists := SL_Realloc(pointer(blocklists), ((blockoffs + 1) * 2));
          blocklists[blockoffs] := n;
          inc(blockoffs);
        end;
      end;
      blocklists := SL_Realloc(pointer(blocklists), ((blockoffs + 1) * 2));
      blocklists[blockoffs] := -1;
      inc(blockoffs);
      inc(blocknum);
    end;
  end;

  blockmap_size := blockoffs * 2;
  data := SL_Malloc(blockmap_size + blockptrs_old_size + 8);
  memcpy(data, @blockhead, 8);
  memcpy(@data[8], blockptrs_old, blockptrs_old_size);
  memcpy(@data[8 + blockptrs_old_size], blocklists, blockmap_size);
  SL_Free(blockptrs_old);
  SL_Free(blocklists);
  add_lump('BLOCKMAP', data, blockmap_size + blockptrs_old_size + 8);
  Verbose('done.'#13#10);
end;

//- Create blockmap (compressed) ----------------------------------------
// Contributed by Simon "fraggle" Howard <sdh300@ecs.soton.ac.uk>
// Merged 2001/11/17
type
  blocklist_p = ^blocklist_t;
  blocklist_t = packed record
    num_lines: integer;
    offset: uint16_t;
    lines: uint16_pa;
    next: blocklist_p;  // for hash table
  end;

  blocklist_ap = array[0..$FFFF] of blocklist_p;
  blocklist_pap = ^blocklist_ap;

var
  blockhash: blocklist_pap;
  blockhash_size: integer;
  blocklist_size: integer;  // size, in bytes of the blocklists

// offset pointers
  blockptrs: blocklist_pap;
  num_blockptrs: integer;

// hashkey function for search
function blockhash_key(const bl: blocklist_p): integer;
var
  i: integer;
  hash: integer;
begin
  hash := 0;

// this is a pretty lame hash function
// it has to be associative though (ie, 1 2 3 == 3 2 1)
  for i := 0 to bl.num_lines - 1 do
    hash := hash + bl.lines[i];

  result := hash mod blockhash_size;
end;

// compare two struct blocklist_t's
// like strcmp: a 0 return value means they are identical
function blocklist_cmp(const bl1, bl2: blocklist_p): integer;
var
  i: integer;
begin
  if bl1.num_lines <> bl2.num_lines then
  begin
    result := 1;
    exit;
  end;

  for i := 0 to bl1.num_lines - 1 do
    if bl1.lines[i] <> bl2.lines[i] then
    begin
      result := 1;
      exit;
    end;

  result := 0;
end;

// search for an identical blocklist
function blockhash_search(const bl: blocklist_p): blocklist_p;
var
  hash: integer;
  search: blocklist_p;
begin
  hash := blockhash_key(bl);
  result := blockhash[hash];
  while result <> nil do
  begin
    if blocklist_cmp(bl, search) = 0 then
      exit;
    result := result.next;
  end;
end;

// add a new blocklist to the hashtable
function blockhash_add(const newbl: blocklist_p): blocklist_p;
var
  bl: blocklist_p;
  hash: integer;
begin
  // first, check an identical one doesnt already exist
  bl := blockhash_search(newbl);

  if bl <> nil then
  begin
    result := bl;
    exit;
  end;

  // need to add new blocklist

  // make a copy

  bl := SL_Malloc(SizeOf(blocklist_t));
  bl.num_lines := newbl.num_lines;
  bl.lines := SL_Malloc(SizeOf(bl.lines[0]) * bl.num_lines);
  memcpy(bl.lines, newbl.lines, SizeOf(bl.lines[0]) * bl.num_lines);

  // link into hash table
  hash := blockhash_key(bl);

  bl.next := blockhash[hash];
  blockhash[hash] := bl;

  // maintain blocklist count
  blocklist_size := blocklist_size + SizeOf(bl.lines[0]) * bl.num_lines;

  result := bl;
end;

procedure blockmap_assemble(const blockmaphead: block_p);
var
  i, l, offset: integer;
  blockmap_size: integer;
  blockmap: int16_pa;
  bl: blocklist_p;
begin
  // build the lump itself
  blockmap_size := SizeOf(block_t) + num_blockptrs * SizeOf(int16_t) + blocklist_size;
  blockmap := SL_Malloc(blockmap_size);

  // header
  memcpy(blockmap, blockmaphead, SizeOf(block_t));

  // every hash chain
  offset := num_blockptrs + SizeOf(block_t) div SizeOf(int16_t);
  for i := 0 to blockhash_size - 1 do
  begin
    // every blocklist in the chain
    bl := blockhash[i];
    while bl <> nil do
    begin
      // write
      // offset is in short ints, not bytes
      bl.offset := offset;

      // write each line
      for l := 0 to bl.num_lines - 1 do
      begin
        blockmap[offset] := bl.lines[l];
        inc(offset);
      end;
      bl := bl.next;
    end;

  end;

  offset := SizeOf(block_t) div SizeOf(int16_t);

  for i := 0 to num_blockptrs - 1 do
    blockmap[offset + i] := blockptrs[i].offset;

  add_lump('BLOCKMAP', blockmap, blockmap_size);
end;

procedure CreateBlockmap_compressed(const bbox: bbox_t);
var
  x, y, n: integer;
  blocknum: integer;
  templines: uint16_pa;
  num_templines: integer;
  blockhead: block_t;
  tempbl: blocklist_t;
  del: blocklist_p;
begin
  Verbose('Creating compressed blockmap... '#13#10);
  blocknum := 0;

  // header
  blockhead.minx := divless(bbox[BB_LEFT], 8);
  blockhead.miny := divless(bbox[BB_BOTTOM], 8);
  blockhead.xblocks := ((bbox[BB_RIGHT] - divless(bbox[BB_LEFT], 8)) div 128) + 1;
  blockhead.yblocks := ((bbox[BB_TOP] - divless(bbox[BB_BOTTOM], 8)) div 128) + 1;

  // build hash table
  blockhash_size := blockhead.xblocks * blockhead.yblocks;
  blockhash := SL_Malloc(blockhash_size * SizeOf(pointer));
  for n := 0 to blockhash_size - 1 do
    blockhash[n] := nil;

  // pointers
  num_blockptrs := blockhead.xblocks * blockhead.yblocks;
  blockptrs := SL_Malloc(num_blockptrs * SizeOf(pointer));

  num_templines := 10240;
  templines := SL_Malloc(num_templines * SizeOf(uint16_t));

  // build all blocklists
  for y := 0 to blockhead.yblocks - 1 do
    for x := 0 to blockhead.xblocks - 1 do
    begin
      tempbl.num_lines := 0;
      tempbl.lines := templines;

      progress();

      // first line is a 0
      tempbl.lines[tempbl.num_lines] := 0;
      inc(tempbl.num_lines);

      for n := 0 to num_lines - 1 do
      begin
        if IsLineDefInside(n, blockhead.minx + (x * 128), blockhead.miny + (y * 128), blockhead.minx + (x * 128) + 127, blockhead.miny + (y * 128) + 127) then
        begin
          if tempbl.num_lines >= num_templines - 5 then
          begin
            num_lines := num_lines * 2;
            templines := SL_Realloc(pointer(templines), num_templines * SizeOf(uint16_t));
            tempbl.lines := templines;
          end;

          tempbl.lines[tempbl.num_lines] := n;
          inc(tempbl.num_lines);
        end
      end;

      // last is 0xffff
      tempbl.lines[tempbl.num_lines] := $FFFF;
      inc(tempbl.num_lines);
      blockptrs[blocknum] := blockhash_add(@tempbl);
      inc(blocknum);
    end;

  SL_Free(templines);

  // assemble
  blockmap_assemble(@blockhead);

  // deconstruct the hash table
  for n := 0 to blockhash_size - 1 do
    while blockhash[n] <> nil do
    begin
      del := blockhash[n];
      blockhash[n] := blockhash[n].next;
      SL_Free(del.lines);
      SL_Free(del);
    end;

  SL_Free(blockhash);
  SL_Free(blockptrs);

  Verbose('done.'#13#10);
end;

var
  psx, psy, pex, pey, pdx, pdy: integer;
  lsx, lsy, lex, ley: integer;

//--------------------------------------------------------------------------
// Find limits from a list of segs, does this by stepping through the segs
// and comparing the vertices at both ends.
//--------------------------------------------------------------------------

const
  BIG_POSITIVE = 2000000000;
  BIG_NEGATIVE = -2000000000;

procedure FindLimits(ts: seg_p; var box: bbox_t);
var
  minx, miny, maxx, maxy: integer;
  fv, tv: integer;
begin
  minx := BIG_POSITIVE;
  miny := BIG_POSITIVE;
  maxx := BIG_NEGATIVE;
  maxy := BIG_NEGATIVE;

  while ts <> nil do
  begin
    fv := ts.start;
    tv := ts._end;
    if vertices[fv].x < minx then
      minx := vertices[fv].x;
    if vertices[fv].x > maxx then
      maxx := vertices[fv].x;
    if vertices[fv].y < miny then
      miny := vertices[fv].y;
    if vertices[fv].y > maxy then
      maxy := vertices[fv].y;
    if vertices[tv].x < minx then
      minx := vertices[tv].x;
    if vertices[tv].x > maxx then
      maxx := vertices[tv].x;
    if vertices[tv].y < miny then
      miny := vertices[tv].y;
    if vertices[tv].y > maxy then
      maxy := vertices[tv].y;
    ts := ts.next;
  end;
  // cph - top, bottom, left, right
  box[BB_TOP] := maxy;
  box[BB_BOTTOM] := miny;
  box[BB_LEFT] := minx;
  box[BB_RIGHT] := maxx;
end;

//--------------------------------------------------------------------------
function assignint16(var a: int16_t; const b: int16_t): int16_t;
begin
  a := b;
  result := a;
end;

//- translate (dx, dy) into an integer angle value (0-65535) ---------------
function ComputeAngle(const dx, dy: integer): uint16_t;
var
  w: double;
begin
  w := arctan2(dy, dx) * (65536 / (pi * 2));

  if w < 0 then
    w := 65536 + w;
  if w >= 65536 then
    w := w - 65536;

  result := round(w);
end;

function add_seg(const cs: seg_p; n, fv, tv: integer; var fs: seg_p; const sd: sidedef_p): seg_p;
var
  ang: LongWord;
begin
  result := SL_Malloc(SizeOf(seg_t)); // get mem for seg_t

  if cs <> nil then
    cs.next := result
  else
    fs := result;

  result.next := nil;
  result.start := fv;
  result._end := tv;
  result.pdx := assignint16(result.pex, vertices[tv].x) - assignint16(result.psx, vertices[fv].x);
  result.pdy := assignint16(result.pey, vertices[tv].y) - assignint16(result.psy, vertices[fv].y);
  result.ptmp := result.pdx * result.psy - result.psx * result.pdy;
  result.len := round(sqrt(result.pdx * result.pdx + result.pdy * result.pdy));

  if assignint16(result.sector, sd.sector) = -1 then
    I_Warning('add_seg(): Bad sidedef in linedef %d (Z_CheckHeap error)'#13#10, [n]);

  result.angle := ComputeAngle(result.pdx, result.pdy);

  if linedefs[n].tag = 999 then
  begin
    ang := (result.angle + round(sd.xoff * (65536.0 / 360.0)));
    result.angle := ang and $FFFF;
  end;

  result.linedef := n;
  result.dist := 0;
end;

//- initially creates all segs, one for each line def ----------------------
function CreateSegs: seg_p;
var
  cs: seg_p;  // current seg_t
  fs: seg_p;  // first seg_t in list
  l: linedef_p;
  n: integer;
begin
  Verbose('Creating Segs...'#13#10);
  cs := nil;
  fs := nil;
  for n := 0 to num_lines - 1 do  // step through linedefs and get side numbers
  begin
    l := @linedefs[n];
    // If line is 0 len, don't generate any segs
    if (vertices[l.start].x = vertices[l._end].x) and (vertices[l.start].y = vertices[l._end].y) then
      Continue;
    if l.sidedef1 <> -1 then
    begin
      cs := add_seg(cs, n, l.start, l._end, fs, @sidedefs[l.sidedef1]);
      cs.flip := 0;
    end
    else
    I_Warning('CreateSegs(): Linedef %d has no right sidedef'#13#10, [n]);

    if l.sidedef2 <> -1 then
    begin
      cs := add_seg(cs, n, l._end, l.start, fs, @sidedefs[l.sidedef2]);
      cs.flip := 1;
    end
    else if l.flags and 4 <> 0 then
      I_Warning('CreateSegs(): Linedef %d is 2s but has no left sidedef'#13#10, [n]);
  end;
  Verbose('done.'#13#10);
  result := fs;
end;

//- read the vertices from the wad file and place in 'vertices' ------------
//  Rewritten by Lee Killough, to speed up performance

var
  vertlmp: lumplist_p;

procedure GetVertexes;
var
  n, used_verts: integer;
  s, e: integer;
  translate: PIntegerArray;
  l: lumplist_p;
begin
  l := FindDir('VERTEXES');

  if (l = nil) or (l.dir.len < SizeOf(vertex_t)) then
  begin
    I_Warning('GetVertexes(): Couldn''t find any Vertices'#13#10);
    num_verts := 0;
    exit;
  end;

  num_verts := l.dir.len div SizeOf(vertex_t);

  vertlmp := l;

  vertices := ReadLump(l);

  translate := SL_Malloc(num_verts * SizeOf(integer));

  for n := 0 to num_verts - 1 do  // Unmark all vertices
    translate[n] := -1;

  for n := 0 to num_lines - 1 do  // Mark all used vertices
  begin
    s := linedefs[n].start;
    e := linedefs[n]._end;
    if (s < 0) or (s >= num_verts) or (e < 0) or (e >= num_verts) then
      I_Warning('GetVertexes(): Linedef %d has vertex out of range'#13#10, [n]);
    translate[s] := 0;
    translate[e] := 0;
  end;

  used_verts := 0;
  for n := 0 to num_verts - 1 do  // Shift up all unused vertices
    if translate[n] = 0 then
    begin
      translate[n] := used_verts;
      vertices[used_verts] := vertices[n];
      inc(used_verts);
    end;

  for n := 0 to num_lines - 1 do  // Renumber vertices
  begin
    s := translate[linedefs[n].start];
    e := translate[linedefs[n]._end];
    if (s < 0) or (s >= used_verts) or (e < 0) or (e >= used_verts) then
      I_Warning('GetVertexes(): Trouble in GetVertexes: Renumbering'#13#10);
    linedefs[n].start := s;
    linedefs[n]._end := e;
  end;

  SL_Free(translate);

  Verbose('Loaded %d vertices', [num_verts]);
  if num_verts > used_verts then
    Verbose(', but %d were unused'#13#10'(this is normal if the nodes were built before).'#13#10,
               [num_verts - used_verts])
  else
    Verbose('.'#13#10);
  num_verts := used_verts;
  if num_verts = 0 then
    I_Warning('GetVertexes(): Couldn''t find any used Vertices'#13#10);
end;

//- read the linedefs from the wad file and place in 'linedefs' ------------
procedure GetLinedefs;
var
  l: lumplist_p;
begin
  l := FindDir('LINEDEFS');

  if (l = nil) or (l.dir.len < SizeOf(linedef_t)) then
  begin
    I_Warning('GetLinedefs(): Couldn''t find any Linedefs'#13#10);
    num_lines := 0;
    exit;
  end;

  num_lines := l.dir.len div SizeOf(linedef_t);

  linedefs := ReadLump(l);
end;

//- read the sidedefs from the wad file and place in 'sidedefs' ------------
procedure GetSidedefs;
var
  l: lumplist_p;
begin
  l := FindDir('SIDEDEFS');

  if (l = nil) or (l.dir.len < SizeOf(sidedef_t)) then
  begin
    I_Warning('GetSidedefs(): Couldn''t find any Sidedefs'#13#10);
    num_sides := 0;
    exit;
  end;

  num_sides := l.dir.len div SizeOf(sidedef_t);
  sidedefs := ReadLump(l);
end;

//- read the sectors from the wad file and place count in 'num_sectors' ----
procedure GetSectors;
var
  l: lumplist_p;
begin
  l := FindDir('SECTORS');

  if (l = nil) or (l.dir.len < SizeOf(sector_t)) then
  begin
    I_Warning('GetSectors(): Couldn''t find any Sectors'#13#10);
    num_sects := 0;
    exit;
  end;

  num_sects := l.dir.len div SizeOf(sector_t);
  sectors := ReadLump(l);
end;

//--------------------------------------------------------------------------
// Converts the nodes from a btree into the array format for inclusion in
// the .wad. Frees the btree as it goes
function ReverseNodes(const tn: node_p): int16_t;
var
  pn: pnode_p;
begin
  if tn.nextr <> nil then
    tn.chright := ReverseNodes(tn.nextr)
  else
    tn.chright :=  tn.chright or $8000;

  if tn.nextl <> nil then
    tn.chleft := ReverseNodes(tn.nextl)
  else
    tn.chleft := tn.chleft or $8000;

  pn := @pnodes[pnode_indx];
  inc(pnode_indx);

  pn.x := tn.x;
  pn.y := tn.y;
  pn.dx := tn.dx;
  pn.dy := tn.dy;
  pn.leftbox := tn.leftbox;
  pn.rightbox := tn.rightbox;
  pn.chright := tn.chright;
  pn.chleft := tn.chleft;

  // Free the node, it's in our array now
  memset(tn, 0, SizeOf(node_t));
  SL_Free(tn);
  result := num_pnodes;
  inc(num_pnodes);
end;

// Height of nodes
function height(const tn: node_p): LongWord;
var
  r, l: LongWord;
begin
  if tn <> nil then
  begin
    l := height(tn.nextl);
    r := height(tn.nextr);
    if l > r then
      result := l + 1
    else
      result := r + 1;
  end
  else
    result := 1;
end;

var
  node_x, node_y, node_dx, node_dy: int16_t;

//--------------------------------------------------------------------------
// JVAL Cleaned up
function SplitDist(const ts: seg_p): integer;
var
  dx, dy: integer;
begin
  if ts.flip = 0 then
  begin
    dx := vertices[linedefs[ts.linedef].start].x - vertices[ts.start].x;
    dy := vertices[linedefs[ts.linedef].start].y - vertices[ts.start].y;
  end
  else
  begin
    dx := vertices[linedefs[ts.linedef]._end].x - vertices[ts.start].x;
    dy := vertices[linedefs[ts.linedef]._end].y - vertices[ts.start].y;
  end;
  if (dx = 0) and (dy = 0) then
    I_Warning('SplitDist(): Trouble in SplitDist dx=%d,dy=%d'#13#10, [dx, dy]);
  result := round(sqrt((dx * dx) + (dy * dy)));
end;

//---------------------------------------------------------------------------*
// Split a list of segs (ts) into two using the method described at bottom of
// file, this was taken from OBJECTS.C in the DEU5beta source.
//
// This is done by scanning all of the segs and finding the one that does
// the least splitting and has the least difference in numbers of segs on either
// side.
// If the ones on the left side make a subsector_t, then create another subsector_t
// else put the segs into lefts list.
// If the ones on the right side make a subsector_t, then create another subsector_t
// else put the segs into rights list.
//---------------------------------------------------------------------------*

procedure DivideSegs(const ts: seg_p; var rs, ls: seg_p; const bbox: bbox_t);
var
  rights, lefts: seg_p;
  tmps, best, news, prev: seg_p;
  add_to_rs, add_to_ls: seg_p;
  new_best, new_rs, new_ls: seg_p;
  strights, stlefts: seg_p;
  num_new: integer;
  x, y, val: int16_t;
begin
  new_best := nil;
  num_new := 0;
  best := PickNode(ts, bbox); // Pick best node to use.

  if best = nil then
  begin
    I_Warning('DivideSegs(): Couldn''t pick nodeline!'#13#10);
    exit;
  end;

  node_x := vertices[best.start].x;
  node_y := vertices[best.start].y;
  node_dx := vertices[best._end].x - vertices[best.start].x;
  node_dy := vertices[best._end].y - vertices[best.start].y;

// When we get to here, best is a pointer to the partition seg.
// Using this partition line, we must split any lines that are intersected
// into a left and right half, flagging them to be put their respective sides
// Ok, now we have the best line to use as a partitioning line, we must
// split all of the segs into two lists (rightside & leftside).

  rights := nil;   // Start off with empty
  lefts := nil;    // lists.
  strights := nil; // Start off with empty
  stlefts := nil;  // lists.

  psx := vertices[best.start].x;  // Partition line coords
  psy := vertices[best.start].y;
  pex := vertices[best._end].x;
  pey := vertices[best._end].y;
  pdx := psx - pex;               // Partition line DX,DY
  pdy := psy - pey;

  tmps := ts;
  while tmps <> nil do
  begin
    progress(); // Something for the user to look at.
    add_to_rs := nil;
    add_to_ls := nil;
    if tmps <> best then
    begin
      lsx := vertices[tmps.start].x;  // Calculate this here, cos it doesn't
      lsy := vertices[tmps.start].y;  // change for all the interations of
      lex := vertices[tmps._end].x;   // the inner loop!
      ley := vertices[tmps._end].y;
      val := DoLinesIntersect;
      if ((val and 2 <> 0) and (val and 64 <> 0)) or ((val and 4 <> 0) and (val and 32 <> 0)) then // If intersecting !!
      begin
        ComputeIntersection(x, y);
        vertices := SL_Realloc(pointer(vertices), SizeOf(vertex_t) * (num_verts + 1));
        vertices[num_verts].x := x;
        vertices[num_verts].y := y;

        news := SL_Malloc(SizeOf(seg_t));
        news^ := tmps^;
        tmps.next := news;
        news.start := num_verts;
        tmps._end := num_verts;
        news.dist := SplitDist(news);
        if val and 32 <> 0 then add_to_ls := tmps;
        if val and 64 <> 0 then add_to_rs := tmps;
        if val and 2 <> 0 then add_to_ls := news;
        if val and 4 <> 0 then add_to_rs := news;
        tmps := news;
        inc(num_verts);
        inc(num_new);
      end
      else
      begin // Not split, which side ?
        if val and 34 <> 0 then add_to_ls := tmps;
        if val and 68 <> 0 then add_to_rs := tmps;
        if (val and 1 <> 0) and (val and 16 <> 0) then  // On same line
        begin
// 06/01/97 Lee Killough: this fixes a bug ever since 1.2x,
// probably 1.0, of BSP: when partitioning a parallel seg,
// you must take its vertices' orientation into account, NOT the
// flip bits, to determine which side of the partitioning line a
// parallel seg should go on. If you simply flip the linedef in
// question, you will be flipping both its vertices and sidedefs,
// and the flip bits as well, even though the basic geometry has
// not changed. Thus you need to use the vertices' orientation
// (whether the seg is in the same direction or not, regardless
// of its original linedef's being flipped or not), into account.
//
// Originally, some segs were partitioned backwards, and if
// it happened that there were different sectors on either
// side of the seg being partitioned, it could leave holes
// in space, causing either invisible barriers or disappearing
// Things, because the ssector would be associated with the
// wrong sector.
//
// The old logic of tmps->flip != best->flip seems to rest on
// the assumption that if two segs are parallel, they came
// from the same linedef. This is clearly not always true.

//  if (tmps->flip != best->flip)   old logic -- wrong!!!

// We know the segs are parallel or nearly so, so take their
// dot product to determine their relative orientation.

          if (lsx - lex) * pdx + (lsy - ley) * pdy < 0 then
            add_to_ls := tmps
          else
            add_to_rs := tmps;
        end;
      end;
    end
    else
      add_to_rs := tmps; // This is the partition line

    if add_to_rs <> nil then // CHECK IF SHOULD ADD RIGHT ONE
    begin
      new_rs := SL_Malloc(SizeOf(seg_t));
      new_rs^ := add_to_rs^;
      if add_to_rs = best then
        new_best := new_rs;
      new_rs.next := nil;
      if rights = nil then
      begin
        rights := new_rs;
        strights := new_rs;
      end
      else
      begin
        rights.next := new_rs;
        rights := new_rs;
      end;
    end;

    if add_to_ls <> nil then // CHECK IF SHOULD ADD LEFT ONE
    begin
      new_ls := SL_Malloc(SizeOf(seg_t));
      new_ls^ := add_to_ls^;
      if add_to_ls = best then
        new_best := new_ls;
      new_ls.next := nil;
      if lefts = nil then
      begin
        lefts := new_ls;
        stlefts := new_ls;
      end
      else
      begin
        lefts.next := new_ls;
        lefts := new_ls;
      end;
    end;
    tmps := tmps.next;
  end;

  if strights = nil then
  begin
    rights := new_best;
    strights := new_best;
    prev := nil;
    tmps := stlefts;
    while tmps <> nil do
    begin
      if tmps = new_best then
      begin
        if prev <> nil then
          prev.next := tmps.next
        else
          stlefts := tmps.next;
      end;
      prev := tmps;
      tmps := tmps.next;
    end;
    prev.next := nil;
  end;

  if stlefts = nil then
  begin
    lefts := new_best;
    stlefts := new_best;
    prev := nil;
    tmps := strights;
    while tmps <> nil do
    begin
      if tmps = new_best then
      begin
        if prev <> nil then
          prev.next := tmps.next
        else
          strights := tmps.next;
      end;
      prev := tmps;
      tmps := tmps.next;
    end;
    stlefts.next := nil;
    prev.next := nil;  // Make sure end of list = nil
  end;

  if rights.next <> nil then rights.next := nil;
  if lefts.next <> nil then lefts.next := nil;

  tmps := ts;
  while tmps <> nil do //;)
  begin
    best := tmps.next;
    SL_Free(tmps);
    tmps := best;
  end;

  rs := strights;
  ls := stlefts;
end;

//--------------------------------------------------------------------------
function IsItConvex(const ts: seg_p): boolean;
var
  line, check: seg_p;
  sector, val, nsec: integer;
begin
// All ssectors must come from same sector unless it's marked
// "special" with sector tag >= 900. Original idea, Lee Killough
  line := ts;
  if ts.flip <> 0 then
    sector := sidedefs[linedefs[ts.linedef].sidedef2].sector
  else
    sector := sidedefs[linedefs[ts.linedef].sidedef1].sector;

  if sectors[sector].tag < 900 then
    while true do
    begin
      line := line.next;
      if line = nil then
        break;
      if line.flip <> 0 then
        nsec := sidedefs[linedefs[line.linedef].sidedef2].sector
      else
        nsec := sidedefs[linedefs[line.linedef].sidedef1].sector;

      if (nsec <> sector) and (sectors[nsec].tag < 900) then
      begin
        result := true;
        exit;
      end;
    end;

  // all of the segs must be on the same side all the other segs
  line := ts;
  while line <> nil do
  begin
    psx := vertices[line.start].x;
    psy := vertices[line.start].y;
    pex := vertices[line._end].x;
    pey := vertices[line._end].y;
    pdx := (psx - pex); // Partition line DX,DY
    pdy := (psy - pey);
    check := ts;
    while check <> nil do
    begin
      if line <> check then
      begin
        lsx := vertices[check.start].x; // Calculate this here, cos it doesn't
        lsy := vertices[check.start].y; // change for all the interations of
        lex := vertices[check._end].x;  // the inner loop!
        ley := vertices[check._end].y;
        val := DoLinesIntersect;
        if val and 34 <> 0 then
        begin
          result := true;
          exit;
        end;
      end;
      check := check.next;
    end;
    line := line.next;
  end;

  // no need to split the list: these Segs can be put in a subsector_t
  result := false;
end;

//--------------------------------------------------------------------------
function CreateSSector(tmps: seg_p): integer;
var
  next: seg_p;
  n: integer;
begin
  if num_ssectors = 0 then
    ssectors := SL_Malloc(SizeOf(subsector_t))
  else
    ssectors := SL_Realloc(pointer(ssectors), SizeOf(subsector_t) * (num_ssectors + 1));

  ssectors[num_ssectors].first := num_psegs;

  n := num_psegs;

  while tmps <> nil do
  begin
    if num_psegs = 0 then
      psegs := SL_Malloc(SizeOf(pseg_t))
    else
      psegs := SL_Realloc(pointer(psegs), SizeOf(pseg_t) * (num_psegs + 1));

    psegs[num_psegs].start := tmps.start;
    psegs[num_psegs]._end := tmps._end;
    psegs[num_psegs].angle := tmps.angle;
    psegs[num_psegs].linedef := tmps.linedef;
    psegs[num_psegs].flip := tmps.flip;
    psegs[num_psegs].dist := tmps.dist;
    inc(num_psegs);
    next := tmps.next;
    SL_Free(tmps); // This seg is done
    tmps := next;
  end;

  ssectors[num_ssectors].num := num_psegs - n;

  inc(num_ssectors);

  result := num_ssectors - 1;
end;

function CreateNode(const ts: seg_p; const bbox: bbox_t): node_p;
var
  tn: node_p;
  rights: seg_p;
  lefts: seg_p;
begin
  rights := nil;
  lefts := nil;

  tn := SL_Malloc(SizeOf(node_t)); // Create a node
  DivideSegs(ts, rights, lefts, bbox);      // Divide node in two

  inc(num_nodes);

  tn.x := node_x; // store node line info
  tn.y := node_y;
  tn.dx := node_dx;
  tn.dy := node_dy;

  FindLimits(lefts, tn.leftbox);  // Find limits of vertices

  if IsItConvex(lefts) then // Check lefthand side
  begin
    tn.nextl := CreateNode(lefts, tn.leftbox);// still segs remaining
    tn.chleft := 0;
  end
  else
  begin
    tn.nextl := nil;
    tn.chleft := CreateSSector(lefts) or $8000;
  end;

  FindLimits(rights, tn.rightbox);  // Find limits of vertices

  if IsItConvex(rights) then  // Check righthand side
  begin
    tn.nextr := CreateNode(rights, tn.rightbox);  // still segs remaining
    tn.chright := 0;
  end
  else
  begin
    tn.nextr := nil;
    tn.chright := CreateSSector(rights) or $8000;
  end;

  result := tn;
end;

//---------------------------------------------------------------------------*
//
//  This message has been taken, complete, from OBJECTS.C in DEU5beta source.
//  It outlines the method used here to pick the nodelines.
//
//  IF YOU ARE WRITING A DOOM EDITOR, PLEASE READ THIS:
//
//  I spent a lot of time writing the Nodes builder.  There are some bugs in
//  it, but most of the code is OK.  If you steal any ideas from this program,
//  put a prominent message in your own editor to make it CLEAR that some
//  original ideas were taken from DEU.  Thanks.
//
//  While everyone was talking about LineDefs, I had the idea of taking only
//  the Segs into account, and creating the Segs directly from the SideDefs.
//  Also, dividing the list of Segs in two after each call to CreateNodes makes
//  the algorithm faster.  I use several other tricks, such as looking at the
//  two ends of a seg_t to see on which side of the nodeline it lies or if it
//  should be split in two.  I took me a lot of time and efforts to do this.
//
//  I give this algorithm to whoever wants to use it, but with this condition:
//  if your program uses some of the ideas from DEU or the whole algorithm, you
//  MUST tell it to the user.  And if you post a message with all or parts of
//  this algorithm in it, please post this notice also.  I don't want to speak
//  legalese; I hope that you understand me...  I kindly give the sources of my
//  program to you: please be kind with me...
//
//  If you need more information about this, here is my E-mail address:
//   Raphael.Quinet@eed.ericsson.se (Rapha‰l Quinet).
//
//  Short description of the algorithm:
//   1 - Create one seg_t for each sidedef_t: pick each linedef_t in turn.  If it
//       has a "first" sidedef_t, then create a normal seg_t.  If it has a
//       "second" sidedef_t, then create a flipped seg_t.
//   2 - Call CreateNodes with the current list of Segs.  The list of Segs is
//       the only argument to CreateNodes.
//   3 - Save the Nodes, Segs and SSectors to disk.  Start with the leaves of
//       the Nodes tree and continue up to the root (last node_t).
//
//  CreateNodes does the following:
//   1 - Pick a nodeline amongst the Segs (minimize the number of splits and
//       keep the tree as balanced as possible).
//   2 - Move all Segs on the right of the nodeline in a list (segs1) and do
//       the same for all Segs on the left of the nodeline (in segs2).
//   3 - If the first list (segs1) contains references to more than one
//       sector_t or if the angle between two adjacent Segs is greater than
//       180?, then call CreateNodes with this (smaller) list.  Else, create
//       a SubSector with all these Segs.
//   4 - Do the same for the second list (segs2).
//   5 - Return the new node (its two children are already OK).
//
//  Each time CreateSSector is called, the Segs are put in a global list.
//  When there is no more seg_t in CreateNodes' list, then they are all in the
//  global list and ready to be saved to disk.
//
//---------------------------------------------------------------------------

function PickNode_traditional(ts: seg_p; const bbox: bbox_t): seg_p;
label
  leftside, prune;
var
  best: seg_p;
  bestcost: integer;
  part: seg_p;
  cnt: integer;
  check: seg_p;
  cost, tot, diff: integer;
  a, b, l, d: integer;
begin
  best := ts;
  bestcost := BIG_POSITIVE;

  cnt := 0;
  part := ts;
  while part <> nil do // Count once and for all
  begin
    inc(cnt);
    part := part.next;
  end;

  part := ts;
  while part <> nil do // Use each seg_t as partition
  begin
    cost := 0;
    tot := 0;
    diff := cnt;

    progress();  // Something for the user to look at.

    check := ts;
    while check <> nil do // Check partition against all Segs
    begin         //     get state of lines' relation to each other
      a := part.pdy * check.psx - part.pdx * check.psy + part.ptmp;
      b := part.pdy * check.pex - part.pdx * check.pey + part.ptmp;
      if (a xor b) < 0 then
      begin
        if (a <> 0) and (b <> 0) then
        begin // Line is split; a,b nonzero, opposite sign
          l := check.len;
          d := (l * a) div (a - b); // Distance from start of intersection
          if d >= 2 then
          begin
            // If the linedef associated with this seg has a sector tag >= 900,
            // treat it as precious; i.e. don't split it unless all other options
            // are exhausted. This is used to protect deep water and invisible
            // lifts/stairs from being messed up accidentally by splits.

            if linedefs[check.linedef].tag >= 900 then
              cost := cost + factor * 64;

            cost := cost + factor;

            if cost > bestcost then // This is the heart of my pruning idea -
              goto prune;           // it catches bad segs early on. Killough

            inc(tot);
          end
          else if decide(l - d < 2, check.pdx * part.pdx + check.pdy * part.pdy < 0, b < 0) then
            goto leftside;
        end
        else
          goto leftside;
      end
      else if (a <= 0) and ((a <> 0) or ((b = 0) and (check.pdx * part.pdx + check.pdy * part.pdy < 0))) then
      begin
leftside:
        diff := diff - 2;
      end;
      check := check.next;
    end;

    // Take absolute value. diff is being used to obtain the
    // min/max values by way of: min(a,b)=(a+b-abs(a-b))/2
    diff := diff - tot;
    if diff < 0 then
      diff := -diff;

    // Make sure at least one seg_t is on each side of the partition
    if tot + cnt > diff then
    begin
      cost := cost + diff;
      if cost < bestcost then // We have a new better choice
      begin
        bestcost := cost;
        best := part; // Remember which seg_t
      end;
    end;
prune:              // Early exit and skip past the tests above
    part := part.next;
  end;
  result := best; // All finished, return best seg_t
end;


// Lee Killough 06/1997:
//
// The chances of visplane overflows can be reduced by attemping to
// balance the number of distinct sector references (as opposed to
// SEGS) that are on each side of the node line, and by rejecting
// node lines that cut across wide open space, as measured by the
// proportion of the node line which is incident with segs, inside
// the bounding box.
//
// node_t lines which are extensions of linedefs whose vertices are
// on the boundary of the bounding box, are therefore preferable,
// as long as the number of sectors referenced on either side is
// not too unbalanced.
//
// Contrary to what many say, visplane overflows are not simply
// caused by too many sidedefs, linedefs, light levels, etc. All
// of those factors are correlated with visplane overflows, but
// most importantly, so is how the node builder selects node
// lines. The number of visible changes in flats is the main
// cause of visplane overflows, with visible changes not being
// counted if only invisible regions separate the visible areas.


function PickNode_visplane(ts: seg_p; const bbox: bbox_t): seg_p;
label
  leftside, prune;
var
  best: seg_p;
  bestcost: integer;
  part: seg_p;
  cnt: integer;
  check: seg_p;
  cost, slen, tot, diff: integer;
  a, b, l, d: integer;
  mask: byte;
  t, t1, t2, t3, t4: double;

  function slen_inc: boolean;
  begin
    slen := slen + check.len;
    result := true;
  end;

begin
  best := ts;
  bestcost := BIG_POSITIVE;
  cnt := 0;

  part := ts;
  while part <> nil do  // Count once and for all
  begin
    inc(cnt);
    part := part.next;
  end;

  part := ts;
  while part <> nil do  // Use each seg_t as partition
  begin
    cost := 0;
    slen := 0;
    tot := 0;
    diff := cnt;
    memset(SectorHits, 0, num_sects);
    progress(); // Something for the user to look at.

    check := ts;
    while check <> nil do // Check partition against all Segs
    begin
      // get state of lines' relation to each other
      a := part.pdy * check.psx - part.pdx * check.psy + part.ptmp;
      b := part.pdy * check.pex - part.pdx * check.pey + part.ptmp;
      mask := 2;

      if (a xor b) < 0 then
      begin
        if (a <> 0) and (b <> 0) then
        begin // Line is split; a,b nonzero, opposite sign
          l := check.len;
          d := (l * a) div (a - b); // Distance from start of intersection
          if d >= 2 then
          begin
            // If the linedef associated with this seg has a sector tag >= 900,
            // treat it as precious; i.e. don't split it unless all other options
            // are exhausted. This is used to protect deep water and invisible
            // lifts/stairs from being messed up accidentally by splits.

            if linedefs[check.linedef].tag >= 900 then
              cost := cost + factor * 64;

            cost := cost + factor;

            if cost > bestcost then // This is the heart of my pruning idea -
              goto prune;           // it catches bad segs early on. Killough

            inc(tot); // seg_t is clearly split
            mask := 4;
          end
          else if decide(l - d < 2, check.pdx * part.pdx + check.pdy * part.pdy < 0, b < 0) then  // Distance from start < 2; check distance from end
            goto leftside;
        end
        else
          goto leftside;
      end
      else if (a <= 0) and ((a <> 0) or ((b = 0) and (slen_inc and (check.pdx * part.pdx + check.pdy * part.pdy < 0)))) then
      begin
leftside:
        diff := diff - 2;
        mask := 1;
      end;
      SectorHits[check.sector] := SectorHits[check.sector] or mask;
      check := check.next;
    end;

    // Take absolute value. diff is being used to obtain the
    // min/max values by way of: min(a,b)=(a+b-abs(a-b))/2
    diff := diff - tot;
    if diff < 0 then
      diff := -diff;

    // Make sure at least one seg_t is on each side of the partition
    if tot + cnt <= diff then
      goto prune;

    // Compute difference in number of sector
    // references on each side of node line
    diff := 0;
    for tot := 0 to num_sects - 1 do
      case SectorHits[tot] of
       1: inc(diff);
       2: dec(diff);
      end;

    if diff < 0 then
      diff := -diff;

    cost := cost + diff;
    if cost >= bestcost then
      goto prune;

    // If the node line is incident with SEGS in less than 1/2th of its
    // length inside the bounding box, increase the cost since this is
    // likely a node line cutting across a large room but only sharing
    // space with a tiny SEG in the middle -- this is another contributor
    // to visplane overflows.
    if part.pdx = 0 then
      l := bbox[BB_TOP] - bbox[BB_BOTTOM]
    else if part.pdy = 0 then
      l := bbox[BB_RIGHT] - bbox[BB_LEFT]
    else
    begin
      t1 := (part.psx - bbox[BB_RIGHT]) / part.pdx;
      t2 := (part.psx - bbox[BB_LEFT]) / part.pdx;
      t3 := (part.psy - bbox[BB_TOP]) / part.pdy;
      t4 := (part.psy - bbox[BB_BOTTOM]) / part.pdy;
      if part.pdx > 0 then
      begin
        t := t1;
        t1 := t2;
        t2 := t;
      end;
      if part.pdy > 0 then
      begin
        t := t3;
        t3 := t4;
        t4 := t;
      end;
      l := round((decide(t1 > t3, t3, t1) - decide(t2 < t4, t4, t2)) * part.len);
    end;
    if slen < l then
    begin
      cost := cost + factor;
      if cost >= bestcost then
        goto prune;
    end;

    // We have a new better choice

    bestcost := cost;
    best := part; // Remember which seg_t
prune:  // Early exit and skip past the tests above
    part := part.next;
  end;
  result := best; // All finished, return best seg_t
end;


//---------------------------------------------------------------------------*
// Calculate the point of intersection of two lines. ps?->pe? & ls?->le?
// returns int xcoord, int ycoord
//---------------------------------------------------------------------------*
procedure ComputeIntersection(var outx, outy: int16_t);
var
  a, b, a2, b2, l2, w, d: double;
  dx, dy, dx2, dy2: integer;
begin
  dx := pex - psx;
  dy := pey - psy;
  dx2 := lex - lsx;
  dy2 := ley - lsy;

  if (dx = 0) and (dy = 0) then I_Warning('ComputeIntersection(): Trouble in ComputeIntersection dx,dy'#13#10);
  if (dx2 = 0) and (dy2 = 0) then I_Warning('ComputeIntersection(): Trouble in ComputeIntersection dx2,dy2'#13#10);
  l2 := sqrt(dx2 * dx2 + dy2 * dy2);

  a := dx;  // no normalization of a,b necessary,
  b := dy;  // since division by d in formula for w
  a2 := dx2 / l2; // cancels it out.
  b2 := dy2 / l2;
  d := b * a2 - a * b2;
  if d <> 0 then
  begin
    w := ((a * (lsy - psy)) + (b * (psx - lsx))) / d;

    a := lsx + (a2 * w);
    b := lsy + (b2 * w);
    outx := round(decide(a < 0, -trunc(0.5 - a), trunc(0.5 + a)));
    outy := round(decide(b < 0, -trunc(0.5 - b), trunc(0.5 + b)));
  end
  else
  begin
    outx := lsx;
    outy := lsy;
  end;
end;

//---------------------------------------------------------------------------*
// Because this is (was) used a horrendous amount of times in the inner loops, the
// coordinate of the lines are setup outside of the routine in global variables
// psx,psy,pex,pey = partition line coordinates
// lsx,lsy,lex,ley = checking line coordinates
// The routine returns 'val' which has 3 bits assigned to the the start and 3
// to the end. These allow a decent evaluation of the lines state.
// bit 0,1,2 = checking lines starting point and bits 4,5,6 = end point
// these bits mean     0,4 = point is on the same line
//                     1,5 = point is to the left of the line
//                     2,6 = point is to the right of the line
// There are some failsafes in here, these mainly check for small errors in the
// side checker.
//---------------------------------------------------------------------------*

function DoLinesIntersect: integer;
var
  x, y, val: int16_t;
  dx2, dy2, dx3, dy3, a, b, l: integer;
begin
  val := 0;
  dx2 := psx - lsx; // Checking line -> partition
  dy2 := psy - lsy;
  dx3 := psx - lex;
  dy3 := psy - ley;

  a := pdy * dx2 - pdx * dy2;
  b := pdy * dx3 - pdx * dy3;
  if ((a xor b) < 0) and (a <> 0) and (b <> 0) then // Line is split, just check that
  begin
    ComputeIntersection(x, y);
    dx2 := lsx - x; // Find distance from line start
    dy2 := lsy - y; // to split point
    if (dx2 = 0) and (dy2 = 0) then
      a := 0
    else
    begin
      l := dx2 * dx2 + dy2 * dy2; // If either ends of the split
      if l < 4 then a := 0;       // are smaller than 2 pixs then
    end;                          // assume this starts on part line
    dx3 := lex - x; // Find distance from line end
    dy3 := ley - y; // to split point
    if (dx3 = 0) and (dy3 = 0) then
      b := 0
    else
    begin
      l := dx3 * dx3 + dy3 * dy3; // same as start of line
      if l < 4 then b := 0;
    end;
  end;

  if a = 0 then
    val := val or 16  // start is on middle
  else if a < 0 then
    val := val or 32  // start is on left side
  else
    val := val or 64; // start is on right side

  if b = 0 then
    val := val or 1   // end is on middle
  else if b < 0 then
    val := val or 2   // end is on left side
  else
    val := val or 4;  // end is on right side

  result := val;
end;

// * DoLevel -
// *
// * Process a level, building NODES, etc
procedure DoLevel(const current_level_name: string; const current_level: lumplist_p);
var
  tsegs: seg_p;
  nodelist: node_p;
  mapbound: bbox_t;
  reject_size: integer;
  reject_data: pointer;
begin
  Verbose('Building nodes on %s'#13#10#13#10, [current_level_name]);

  num_ssectors := 0;
  num_psegs := 0;
  num_nodes := 0;

  GetLinedefs();  // Get and convert linedefs first
  GetVertexes();  // vertices here.
  GetSidedefs();
  GetSectors();

  tsegs := CreateSegs();  // Initially create segs

  FindLimits(tsegs, mapbound);  // Find limits of vertices

  Verbose('Map goes from (%d,%d) to (%d,%d)'#13#10,
        [mapbound[BB_TOP   ], mapbound[BB_LEFT  ],
         mapbound[BB_BOTTOM], mapbound[BB_RIGHT ]]);

  SectorHits := SL_Malloc(num_sects);

  nodelist := CreateNode(tsegs, mapbound);  // recursively create nodes

  Verbose('%d NODES created, with %d SSECTORS.'#13#10, [num_nodes, num_ssectors]);

  Verbose('Found %d used vertices'#13#10, [num_verts]);

  Verbose('Heights of left and right subtrees = (%d,%d)'#13#10,
           [height(nodelist.nextl), height(nodelist.nextr)]);

  vertlmp.dir.len := num_verts * SizeOf(vertex_t);
  vertlmp.data := vertices;

  add_lump('SEGS', psegs, SizeOf(pseg_t) * num_psegs);

  add_lump('SSECTORS', ssectors, SizeOf(subsector_t) * num_ssectors);

  if FindDir('REJECT') = nil then
  begin
    reject_size := (num_sects * num_sects + 7) div 8;
    reject_data := SL_Malloc(reject_size);
    memset(reject_data, 0, reject_size);
    add_lump('REJECT', reject_data, reject_size);
  end;
  CreateBlockmap(mapbound);

  pnodes := SL_Malloc(SizeOf(pnode_t) * num_nodes);
  num_pnodes := 0;
  pnode_indx := 0;
  ReverseNodes(nodelist);
  add_lump('NODES', pnodes, SizeOf(pnode_t) * num_pnodes);

  SL_Free(SectorHits);
end;
//- end of file ------------------------------------------------------------

initialization
  PickNode := PickNode_traditional;
  CreateBlockmap := CreateBlockmap_old;

end.
