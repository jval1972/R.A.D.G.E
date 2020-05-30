// radge - [RA]ndom [D]oom [GE]nerator based on SLIGE 490
// 2017 - 2019 Jim Valavanis

program radge492;

{$APPTYPE CONSOLE}

uses
  sl_helpers in 'sl_helpers.pas',
  sl_kernel492 in 'sl_kernel492.pas',
  sl_script in 'sl_script.pas',
  sl_nodes in 'sl_nodes.pas';

var
  args: TDSTringList;
  i: integer;
  sligefile: string;
  nodesfile: string;
begin
  args := TDSTringList.Create;
  for i := 1 to ParamCount do
    args.Add(ParamStr(i));
  if _tmain_slige492(Args, sligefile) = 0 then
  begin
    nodesfile := sligefile + '.wad';
    args.Clear;
    args.Add(sligefile);
    args.Add('-o');
    args.Add(nodesfile);
    _tmain_nodes51(args);
  end;
  args.Free;
  readln;
end.

