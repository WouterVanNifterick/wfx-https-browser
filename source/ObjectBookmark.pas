unit ObjectBookmark;

interface

uses Classes, WfxPlugin, SysUtils;

{$DEFINE NOFORMS}

type
  TBookmark = class(TObject)
    BMList: TStringList;
    Plugin:TWfxPlugin;
    procedure Add(URL: string);
    procedure Delete(URL: string);
    constructor Create(APlugin:TWFXPlugin);
    destructor Destroy; override;
  private
    procedure Save;
  end;

implementation

uses ObjectLines, ObjectIni;

{ TBookmark }

procedure TBookmark.Add(URL: string);
begin
  if URL = '' then
    Exit;

  URL := URL.Replace(';', '%3B');
  BMList.Add(URL);

  Plugin.TCShowMessage(Plugin.Strings[2], Format(Plugin.Strings[5], [URL]));
  // MessageDlg(Format(Strings[5], [URL]), mtInformation, [mbOK], 0);
  Save;
end;

procedure TBookmark.Save;
var
  s: string;
begin
  s := BMList.DelimitedText;
  Plugin.Ini.SetValue('Bookmark', s);
end;

constructor TBookmark.Create(APlugin:TWFXPlugin);
begin
  Plugin            := APlugin;
  BMList            := TStringList.Create;
  BMList.Sorted     := True;
  BMList.Duplicates := dupIgnore;
  BMList.Delimiter  := ';';
  BMList.DelimitedText := Plugin.Ini.GetS('Bookmark');
end;

procedure TBookmark.Delete(URL: string);
var
  i: Integer;
begin
  i := BMList.IndexOf(URL);
  if i > -1 then
    BMList.Delete(i);

  Save;
end;

destructor TBookmark.Destroy;
begin
  FreeAndNil(BMList);
  inherited;
end;

end.
