unit ObjectLines;

interface

uses Classes, SysUtils;

{$DEFINE NOFORMS}

type
  TLines = class(TObject)
    constructor Create;
    destructor Destroy; override;
    procedure SetLanguage(Path : string);
    function GetIcon(Path : string) : string;
  protected
    function Get(Index: Integer): string;
    procedure Put(Index: Integer; Item: string);
  public
    property Items[Index: Integer]: string read Get write Put; default;
  private
    InternalList : TStringList;
    List : TStringList;
    IconsDef : TStringList;
    procedure SetIcons;
    procedure AddIconDef(Icon : string; LineIndex : Integer; SubLineIndex : Integer = -1);
end;

implementation

uses UnitStrings;

constructor TLines.Create;
begin
  List := TStringList.Create;
  IconsDef := TStringList.Create;
  InternalList := TStringList.Create;
  InternalList.Add('[Connect]');              //0
  InternalList.Add('Get URL');
  InternalList.Add('[Add to bookmark]');
  InternalList.Add('Loading ');
  InternalList.Add('Loading complete');
  InternalList.Add('[%s] added to bookmark'); //5
  InternalList.Add('Choose Language');
end;
//______________________________________________________________________________
destructor TLines.Destroy;
begin
  List.Free;
  InternalList.Free;
  IconsDef.Free;
  inherited;
end;
//______________________________________________________________________________
function TLines.Get(Index: Integer): string;
begin
  if Index < InternalList.Count then
    if Index < List.Count then Result := List[Index]
    else Result := InternalList[Index]
  else Result := '';
  Result := Replace(Result, '<br>', #13#10);
end;
//______________________________________________________________________________
procedure TLines.Put(Index: Integer; Item: string);
begin
  if Index < List.Count then
    List[Index] := Item;
end;
//______________________________________________________________________________
procedure TLines.AddIconDef(Icon: string; LineIndex, SubLineIndex: Integer);
var
  s : string;
begin
  if (LineIndex > List.Count - 1) or (SubLineIndex > List.Count - 1) then Exit;
  s := '\' + List[LineIndex];
  if SubLineIndex > -1 then s := s + '\' + List[SubLineIndex];
  s := s + '=' + Icon;
  IconsDef.Add(s);
end;

//______________________________________________________________________________
procedure TLines.SetIcons;
begin
  IconsDef.Clear;
  AddIconDef('ZLEFT', 0, 1);
end;
//______________________________________________________________________________
procedure TLines.SetLanguage(Path: string);
var
  DefLangFile: string;
begin
  if FileExists(Path) then List.LoadFromFile(Path)
  else begin
    List.Clear;
    List.AddStrings(InternalList);
    DefLangFile := ExtractFilePath(Path) + 'english.lng';
    try
      List.SaveToFile(DefLangFile);
      DefLangFile := ExtractFilePath(Path) + 'hb_english.lng';
      if not FileExists(DefLangFile) then
        List.SaveToFile(DefLangFile);
    except
    end;
  end;
  SetIcons;
end;
//______________________________________________________________________________
function TLines.GetIcon(Path: string): string;
var
  p : Integer;
  s : string;
  i : Integer;
begin
  for i := 0 to IconsDef.Count - 1 do begin
    p := Pos('=', IconsDef[i]);
    s := Copy(IconsDef[i], 1, p - 1);
    if Pos(s, Path) = 1 then begin
      Result := Copy(IconsDef[i], p + 1, Length(IconsDef[i]) );
      Exit;
    end;
  end;
  Result := 'MAINICON';
end;

end.
 