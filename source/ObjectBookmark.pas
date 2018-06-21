unit ObjectBookmark;

interface

uses Classes, UnitStrings, SysUtils;

{$DEFINE NOFORMS}

type TBookmark = class(TObject)
    BMList : TStringList;
    procedure Add(URL : string);
    procedure Delete(URL : string);
    constructor Create;
    destructor Destroy; override;
  private
    procedure Save;
end;

implementation

uses ObjectLines, ObjectIni;

{ TBookmark }

procedure TBookmark.Add(URL: string);
begin
  if URL = '' then Exit;
  URL := Replace(URL, ';', '%3B');
  BMList.Add(URL);

  TCShowMessage(Strings[2], Format(Strings[5], [URL]));
//  MessageDlg(Format(Strings[5], [URL]), mtInformation, [mbOK], 0);
  Save;
end;

procedure TBookmark.Save;
var
  s : string;
begin
  s := merge(BMList);
  Ini.SetValue('Bookmark', s);
end;

constructor TBookmark.Create;
var
  s : string;
begin
  BMList := TStringList.Create;
  BMList.Sorted := True;
  BMList.Duplicates := dupIgnore;
  s := Ini.GetS('Bookmark');
  if s <> '' then split(BMList, s);
end;

procedure TBookmark.Delete(URL: string);
var
  i : Integer;
begin
  i := BMList.IndexOf(URL);
  if i > -1 then BMList.Delete(i);
  Save;
end;

destructor TBookmark.Destroy;
begin
  FreeAndNil(BMList);
  inherited;
end;

end.
