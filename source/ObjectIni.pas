unit ObjectIni;

interface

uses Classes;

{$DEFINE NOFORMS}

type
  TIni = Class(TStringList)
    constructor Create(Path: string = '');
    function GetB(Name: string; DefaultValue: Boolean = False): Boolean; overload;
    function GetS(Name: string; DefaultValue: string = ''): string; overload;
    function GetI(Name: string; DefaultValue: Integer = 0): Integer; overload;
    function GetF(Name: string; DefaultValue: Double = 0.0): Double; overload;
  public
    procedure SetValue(Name: string; Value: Boolean); overload;
    procedure SetValue(Name: string; Value: string); overload;
    procedure SetValue(Name: string; Value: Integer); overload;
    procedure SetValue(Name: string; Value: Double); overload;
  private
    IniFilePath: string;
    procedure Load;
    procedure Save;
  end;

implementation

uses SysUtils;

constructor TIni.Create(Path: string);
begin
  inherited Create;
  IniFilePath := Path;
  Load;
end;

// ______________________________________________________________________________
function TIni.GetB(Name: string; DefaultValue: Boolean = False): Boolean;
var
  s: string;
begin
  s := Values[Name];
  if s = '' then
    Result := DefaultValue
  else
    Result := (LowerCase(s) = 'true') or (s = '1');
end;

// ______________________________________________________________________________
function TIni.GetF(Name: string; DefaultValue: Double = 0.0): Double;
begin
  try
    Result := StrToFloat(Values[Name]);
  except
    Result := DefaultValue;
  end;
end;

// ______________________________________________________________________________
function TIni.GetI(Name: string; DefaultValue: Integer = 0): Integer;
begin
  try
    Result := StrToInt(Values[Name]);
  except
    Result := DefaultValue;
  end;
end;

// ______________________________________________________________________________
function TIni.GetS(Name: string; DefaultValue: string = ''): string;
begin
  Result := Values[Name];
  if Result = '' then
    Result := DefaultValue;
end;

// ______________________________________________________________________________
procedure TIni.Load;
begin
  if FileExists(IniFilePath) then
    LoadFromFile(IniFilePath);
end;

// ______________________________________________________________________________
procedure TIni.Save;
begin
  SaveToFile(IniFilePath);
end;

// ______________________________________________________________________________
procedure TIni.SetValue(Name: string; Value: Boolean);
begin
  if Value then
    Values[Name] := 'True'
  else
    Values[Name] := 'False';
  Save;
end;

// ______________________________________________________________________________
procedure TIni.SetValue(Name: string; Value: Integer);
begin
  Values[Name] := format('%d', [Value]);
  Save;
end;

// ______________________________________________________________________________
procedure TIni.SetValue(Name, Value: string);
begin
  Values[Name] := Value;
  Save;
end;

// ______________________________________________________________________________
procedure TIni.SetValue(Name: string; Value: Double);
begin
  Values[Name] := format('%f', [Value]);
end;

end.
