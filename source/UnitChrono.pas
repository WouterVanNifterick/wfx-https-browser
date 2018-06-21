unit UnitChrono;

interface

uses Classes, SysUtils, Windows, ShellApi;

type
  TChrono = class(TObject)
    ids : TStringList;
    Totals : array of Cardinal;
    Values : array of Cardinal;
    SavePath : string;
    Infos : TStringList;
    constructor Create(Path : string);
    destructor Destroy; override;
    procedure Start(Name : string);
    procedure Stop(Name : string);
  public
    procedure AddInfo(Info : string); overload;
    procedure AddInfo(Line : string; Args : array of const); overload;
  private
    procedure Save;
end;

var
  Chrono : TChrono;

implementation

{ TChrono }

procedure TChrono.AddInfo(Info: string);
begin
  Infos.Add(Info);
  Save;
end;
//______________________________________________________________________________
procedure TChrono.AddInfo(Line: string; Args: array of const);
begin
  Infos.Add( format(Line, Args) );
  Save;
end;

constructor TChrono.Create(Path : string);
begin
  ids := TStringList.Create;
  Infos := TStringList.Create;
  SavePath := Path;
end;
//______________________________________________________________________________
destructor TChrono.Destroy;
begin
  Infos.Free;
  ids.Free;
  values := nil;
  inherited;
end;
//______________________________________________________________________________
procedure TChrono.Save;
var
  i : Integer;
  sl : TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Add('_______________________');
    sl.Add('');
    sl.Add('Timers:');
    sl.Add('_______________________');
    for i := 0 to ids.Count - 1 do
      sl.Add( Format('%s=%d', [ ids[i], Totals[i] ]) );
    sl.Add('');
    sl.Add('_______________________');
    sl.Add('');
    sl.Add('Infos:');
    sl.Add('_______________________');
    sl.AddStrings(Infos);
    sl.SaveToFile(SavePath);
  finally
    sl.Free;
  end;
end;
//______________________________________________________________________________
procedure TChrono.Start(Name: string);
var
  i : Integer;
begin
  i := ids.IndexOf(Name);
  if i = -1 then begin
    ids.Add(Name);
    i := ids.Count - 1;
    SetLength(Values, i + 1);
    SetLength(Totals, i + 1);
    Totals[i] := 0;
  end;
  Values[i] := GetTickCount;
end;
//______________________________________________________________________________
procedure TChrono.Stop(Name: string);
var
  i : Integer;
begin
  i := ids.IndexOf(Name);
  if i = -1 then Exit;
  Values[i] := GetTickCount - Values[i];
  Totals[i] := Totals[i] + Values[i];
  Save;
end;

initialization
  Chrono := TChrono.Create('C:\chronoreport.txt');

finalization
  ShellExecute(0, 'open', PChar(Chrono.SavePath), PChar(''), PChar(''), SW_SHOWNORMAL);
  Chrono.Free;

end.
