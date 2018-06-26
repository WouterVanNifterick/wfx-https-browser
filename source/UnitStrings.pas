unit UnitStrings;

interface

uses Classes, Windows, SysUtils;

{$DEFINE NOFORMS}

function TrimSlashes(s: string; all: Boolean = False): string;
function DeleteUntilLast(const Text, UntilLastStr: string): string;

implementation

function TrimSlashes(s: string; all: Boolean = False): string;
begin
  while (s <> '') and CharInSet(s[1], ['/', '\']) do
    Delete(s, 1, 1);
  if all then
    while (s <> '') and CharInSet(s[length(s)], ['/', '\']) do
      Delete(s, length(s), 1)
  else
    while (s <> '') and (s[length(s)] = '\') do
      Delete(s, length(s), 1);
  Result := s;
end;

function DeleteUntilLast(const Text, UntilLastStr: string): string;
var
  i: Integer;
begin
  Result := Text;
  i      := pos(UntilLastStr, Result);
  while i > 0 do
  begin
    Delete(Result, 1, i);
    i := pos(UntilLastStr, Result);
  end;
end;

end.
