// radge - [RA]ndom [D]oom [GE]nerator based on SLIGE 490
// 2017 - 2019 Jim Valavanis

unit sl_helpers;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils;

type
  TDSTringList = class(TStringList);

procedure printf(const str: string); overload;

procedure printf(const Fmt: string; const Args: array of const); overload;

procedure I_Warning(const str: string); overload;

procedure I_Warning(const Fmt: string; const Args: array of const); overload;

procedure SL_Free(p: pointer);

function SL_Malloc(const size: integer): pointer;

function SL_Realloc(var p: pointer; const newsize: integer): pointer;

const
  fCreate = 0;
  fOpenReadOnly = 1;
  fOpenReadWrite = 2;

  sFromBeginning = 0;
  sFromCurrent = 1;
  sFromEnd = 2;

type
  char8_t = array[0..7] of char;
  Pchar8_t = ^char8_t;

function char8tostring(src: char8_t): ansistring;

function stringtochar8(src: ansistring): char8_t;

procedure sprintf(var s: string; const Fmt: string; const Args: array of const);

function fopen(var f: file; const FileName: string; const mode: integer): boolean;

function fwrite(const data: pointer; const sz1, sz2: integer; var f: file): boolean;

function fread(const data: pointer; const sz1, sz2: integer; var f: file): integer;

function fseek(var f: file; const p: integer): boolean;

function ftell(var f: file): integer;

function memset(const dest: pointer; const val: integer; const count: integer): pointer;

procedure memcpy(const dest: pointer; const src: pointer; count: integer);

function getenv(const env: string): string;

function atoi(const s: string): integer; overload;

function atoi(const s: string; const default: integer): integer; overload;

function strtrim(const S: string): string;

procedure _output_int(var A: PByteArray; const x: integer);

procedure _output_short(var A: PByteArray; const x: SmallInt);

procedure _output_byte(var A: PByteArray; const x: byte);

function decide(const condition: boolean;
  const iftrue: integer; const iffalse: integer): integer; overload;

function decide(const condition: boolean;
  const iftrue: boolean; const iffalse: boolean): boolean; overload;

function decide(const condition: boolean;
  const iftrue: double; const iffalse: double): double; overload;

function CIsInteger(const c: char): boolean;

implementation

procedure printf(const str: string);
begin
  write(str);
end;

procedure printf(const Fmt: string; const Args: array of const);
var
  s: string;
begin
  s := Format(Fmt, Args);
  printf(s);
end;

procedure I_Warning(const str: string); overload;
begin
  printf(str);
end;

procedure I_Warning(const Fmt: string; const Args: array of const); overload;
begin
  printf(Fmt, Args);
end;

procedure SL_Free(p: pointer);
begin
  freemem(p);
end;

function SL_Realloc(var p: pointer; const newsize: integer): pointer;
begin
  ReallocMem(p, newsize);
  result := p;
end;

function SL_Malloc(const size: integer): pointer;
begin
  GetMem(result, size);
end;

function char8tostring(src: char8_t): ansistring;
var
  i: integer;
begin
  result := '';
  i := 0;
  while (i < 8) and (src[i] <> #0) do
  begin
    result := result + src[i];
    inc(i);
  end;
end;

function stringtochar8(src: ansistring): char8_t;
var
  i: integer;
  len: integer;
begin
  len := length(src);
  if len > 8 then
    I_Warning('stringtochar8(): length of %s is > 8', [src]);

  i := 1;
  while (i <= len) do
  begin
    result[i - 1] := src[i];
    inc(i);
  end;

  for i := len to 7 do
    result[i] := #0;
end;

procedure sprintf(var s: string; const Fmt: string; const Args: array of const);
begin
  FmtStr(s, Fmt, Args);
end;

function fopen(var f: file; const FileName: string; const mode: integer): boolean;
begin
  assign(f, FileName);
  {$I-}
  if mode = fCreate then
  begin
    FileMode := 2;
    rewrite(f, 1);
  end
  else if mode = fOpenReadOnly then
  begin
    FileMode := 0;
    reset(f, 1);
  end
  else if mode = fOpenReadWrite then
  begin
    FileMode := 2;
    reset(f, 1);
  end
  else
  begin
    result := false;
    exit;
  end;
  {$I+}
  result := IOresult = 0;
end;

function fwrite(const data: pointer; const sz1, sz2: integer; var f: file): boolean;
var
  N1: integer;
  N2: integer;
begin
  N1 := sz1 * sz2;
  N2 := 0;
  {$I-}
  BlockWrite(f, PByteArray(data)^, N1, N2);
  {$I+}
  if IOResult <> 0 then
    result := false
  else
    result := N1 = N2;
end;

function fread(const data: pointer; const sz1, sz2: integer; var f: file): integer;
begin
  {$I-}
  BlockRead(f, PByteArray(data)^, sz1 * sz2, result);
  {$I+}
  if IOResult <> 0 then
    result := 0;
end;

function fseek(var f: file; const p: integer): boolean;
begin
  {$I-}
  seek(f, p);
  {$I+}
  result := IOResult = 0;
end;

function ftell(var f: file): integer;
begin
  {$I-}
  result := filesize(f);
  {$I+}
  if IOResult <> 0 then
    result := -1;
end;

function memset(const dest: pointer; const val: integer; const count: integer): pointer;
begin
  FillChar(dest^, count, val);
  result := dest;
end;

procedure memcpy(const dest: pointer; const src: pointer; count: integer);
begin
  Move(src^, dest^, count);
end;

function getenv(const env: string): string;
begin
  result := GetEnvironmentVariable(env);
end;

function atoi(const s: string): integer;
var
  code: integer;
  ret2: integer;
begin
  val(s, result, code);
  if code <> 0 then
  begin
    if Pos('0x', s) = 1 then
      val('$' + Copy(s, 3, Length(s) - 2), ret2, code)
    else if Pos('-0x', s) = 1 then
    begin
      val('$' + Copy(s, 4, Length(s) - 3), ret2, code);
      ret2 := -ret2;
    end
    else
      val('$' + s, ret2, code);
    if code = 0 then
      result := ret2
    else
      result := 0;
  end;
end;

function atoi(const s: string; const default: integer): integer; overload;
var
  code: integer;
  ret2: integer;
begin
  val(s, result, code);
  if code <> 0 then
  begin
    if Pos('0x', s) = 1 then
      val('$' + Copy(s, 3, Length(s) - 2), ret2, code)
    else if Pos('-0x', s) = 1 then
    begin
      val('$' + Copy(s, 4, Length(s) - 3), ret2, code);
      ret2 := -ret2;
    end
    else
      val('$' + s, ret2, code);
    if code = 0 then
      result := ret2
    else
      result := default;
  end;
end;

function strtrim(const S: string): string;
var
  I, L: Integer;
  len: integer;
begin
  len := Length(S);
  L := len;
  I := 1;
  while (I <= L) and (S[I] <= ' ') do inc(I);
  if I > L then
    result := ''
  else
  begin
    while S[L] <= ' ' do dec(L);
    if (I = 1) and (L = len) then
      result := S
    else
      result := Copy(S, I, L - I + 1);
  end;
end;

procedure _output_int(var A: PByteArray; const x: integer);
begin
  PInteger(@A^[0])^ := x;
  A := @A^[SizeOf(integer)];
end;

procedure _output_short(var A: PByteArray; const x: SmallInt);
begin
  PSmallInt(@A^[0])^ := x;
  A := @A^[SizeOf(SmallInt)];
end;

procedure _output_byte(var A: PByteArray; const x: byte);
begin
  A^[0] := x;
  A := @A^[SizeOf(byte)];
end;

function decide(const condition: boolean;
  const iftrue: integer; const iffalse: integer): integer;
begin
  if condition then
    result := iftrue
  else
    result := iffalse;
end;

function decide(const condition: boolean;
  const iftrue: boolean; const iffalse: boolean): boolean; overload;
begin
  if condition then
    result := iftrue
  else
    result := iffalse;
end;

function decide(const condition: boolean;
  const iftrue: double; const iffalse: double): double; overload;
begin
  if condition then
    result := iftrue
  else
    result := iffalse;
end;

function CIsInteger(const c: char): boolean;
begin
  result := c in ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
end;


end.

