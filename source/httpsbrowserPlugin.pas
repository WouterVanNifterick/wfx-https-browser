unit httpsbrowserPlugin;

interface

uses
  System.SysUtils,
  System.Classes,
  WinApi.Windows,
  WinApi.ShellApi,
  System.AnsiStrings,
  System.Net.HttpClient,
  ObjectIni,
  ObjectLines,
  UnitStrings,
  ObjectBookmark,
  ObjectTimer,
  ObjectHistory;


type
  TWFXPlugin = class

  end;

  THTTPSBrowserPlugin = class(TWFXPlugin)
    HttpCli: THTTPClient;
    HttpRes: IHTTPResponse;

    PreviousPath: string;
    CurrentPath : string;
    FLIndex     : Integer;
    PathExe     : string;

    LocalFile: string;
    RemoteFile: string;

    Received         : Int64;
    FileDate         : _FILETIME;
    links            : string;
    FileList         : TStringList;

    GetSizeExtensions: string;
    BookMark         : TBookMark;
    History          : THistory;
    LastPage         : string;
    // IsGettingFile    : Boolean;
    AbortCopy       : Boolean;
    GetFileNow      : string;
    Parsed          : Boolean;
    OriginalLastPage: string;
    constructor Create;
    destructor Destroy;override;
    procedure Init;
    function GetFileSize(url: string): Integer;
    function GetPage(var url: string): string;
    procedure GetBinaryFile(const url, FileName: string);
    procedure ParsePage(var sl: TStringList; url: string);
    procedure BuildFindData(var FD: TWin32FindData; FileName: string);
    procedure PageList(var sl: TStringList);
    procedure RootList(var sl: TStringList);
    procedure LangList(var sl: TStringList);
    procedure CreateFileList;
  end;

function IsWebPage(url:string; const links: string): Boolean;
function OnlyFileName(const RemoteName: string): string;
function CompletePath(CurrentPath, RemoteName: string): string;
function IsRoot(var Name: string; const RemoteName: string; const links:string): Boolean;
function CreateFileName(Path: string): string;


implementation

function DateTimeToFileTime(MyDateTime: TDateTime): TFileTime;
var
  MyFileAge      : Integer;
  MyLocalFileTime: _FILETIME;
begin
  MyFileAge := DateTimeToFileDate(MyDateTime);
  DosDateTimeToFileTime(LongRec(MyFileAge).Hi, LongRec(MyFileAge).Lo, MyLocalFileTime);
  LocalFileTimeToFileTime(MyLocalFileTime, Result);
end;

procedure SplitURL(url: string; out domain, folders, page: string);
var
  s   : string;
  i   : Integer;
  prot: string;
begin
  domain  := '';
  folders := '';
  page    := '';
  s       := '';
  i       := Pos('//', url);
  if i > 0 then
  begin
    prot := Copy(url, 1, i + 1);
    Delete(url, 1, i + 1);
  end;
  i := Pos('/', url);
  if i = 0 then
  begin
    domain := prot + TrimSlashes(url);
    Exit;
  end;
  domain := prot + TrimSlashes(Copy(url, 1, i - 1));
  Delete(url, 1, i);
  i := Pos('/', url);
  while i > 0 do
  begin
    s := s + Copy(url, 1, i);
    Delete(url, 1, i);
    i := Pos('/', url);
  end;
  folders := s;
  if Pos('.', url) > 0 then
    page := TrimSlashes(url)
  else
    folders := folders + url;
  folders   := TrimSlashes(folders);
end;

function GetURLRoot(url: string): string;
var
  d, f, p: string;
begin
  Result := DeleteUntilLast(url, '\');
  SplitURL(Result, d, f, p);
  Result := d + '/' + f;
end;

function CompletePath(CurrentPath, RemoteName: string): string;
var
  root   : string;
  d, f, p: string;
begin
  Result := RemoteName;
  Result := DeleteUntilLast(Result, '\');
  if Pos('//', Result) = 0 then
  begin
    root := GetURLRoot(CurrentPath);
    if Result[1] = '/' then
    begin
      SplitURL(root, d, f, p);
      root := d;
    end;
    if (root <> '') and (root[Length(root)] = '/') then
      Result := root + TrimSlashes(Result)
    else
      Result := root + '/' + TrimSlashes(Result);
  end;
end;

function OnlyFileName(const RemoteName: string): string;
begin
  Result := DeleteUntilLast(RemoteName, '\');
end;

function FilterFileName(FileName: string): string;
var
  i: Integer;
begin
  Result := FileName;

  Result := DeleteUntilLast(Result, '/');
  Result := DeleteUntilLast(Result, '\');

  i := Pos('?', Result);
  if i > 0 then
    Delete(Result, i, Length(Result));
end;

function CreateFileName(Path: string): string;
var
  d, f, p: string;
begin
  if Pos('//', Path) > 0 then
  begin
    SplitURL(Path, d, f, p);
    if p <> '' then
      Result := p
    else if f <> '' then
      Result := f + '.htm'
    else
    begin
      d      := Replace(d, '.', '_');
      Result := d + '.htm';
    end;
  end
  else
  begin
    Result := FilterFileName(Path);
  end;
  if Pos('http://', Result) = 1 then
    Delete(Result, 1, 7);
  Result := Replace(Result, '/', '_');
  Result := Replace(Result, '?', '_');
end;

function MakeRelative(CompleteText, root: string): string;
begin
  if Pos(LowerCase(root), LowerCase(CompleteText)) = 1 then
    Result := Copy(CompleteText, Length(root) + 1, Length(CompleteText))
  else
    Result := CompleteText;
end;

function CheckValue(const line: string): Boolean;
var
  ll: string;
begin
  if line = '' then
    Exit(False);

  ll     := LowerCase(line);
  Result := (ll <> '/')
        and (Pos('mailto', ll) <> 1)
        and (Pos('javascript', ll) <> 1)
        and (Pos('+', ll) = 0)
        and (Pos('"', ll) = 0)
        and (Pos('\', ll) = 0)
        and (Pos('''', ll) = 0)
        and (Pos('#', ll) <> 1);
end;

function IsWebPage(url:string; const links: string): Boolean;
var
  i      : Integer;
  d, f, p: string;
  Ext    : string;
begin
  i := Pos('?', url);
  if i > 0 then
    Delete(url, i, Length(url));

  SplitURL(url, d, f, p);
  Ext := ExtractFileExt(p);

  i := Pos(';', Ext);
  if i > 0 then
    Delete(Ext, i, Length(Ext));

  i := Pos('#', Ext);
  if i > 0 then
    Delete(Ext, i, Length(Ext));

  Result := (p = '') or (Pos(' ' + LowerCase(Ext) + ' ', links) > 0);
end;

function IsRoot(var Name: string; const RemoteName: string; const links:string): Boolean;
begin
  Result := Copy(RemoteName, 2, Length(RemoteName)) = Copy(Name, 2, Length(Name));
  if Result then
  begin
    Name   := Copy(Name, 2, Length(Name));
    Result := IsWebPage(Name, Links)
  end;
end;

function GetFirstString(var Text, LText: string; key: string; var ResultStr: string): Boolean;
var
  c   : Char;
  l   : Integer;
  j   : Integer;
  i   : Integer;
  Code: Integer;
begin
  ResultStr := '';

  i := Pos(key, LText);
  if i = 0 then
  begin
    Result := False;
    Exit;
  end;

  Result := True;
  Inc(i, Length(key) - 1);
  Delete(Text, 1, i);
  Delete(LText, 1, i);

  l := Length(Text);
  i := 1;

  // suppress spaces before and after "="
  while (i < l) and (Text[i] = ' ') do
    Inc(i);
  if (i >= l) or (Text[i] <> '=') then
    Exit;
  Inc(i);
  while (i < l) and (Text[i] = ' ') do
    Inc(i);
  if i >= l then
    Exit;

  // Cas de code jscript avec des \' au lieu de '
  if Text[i] = '\' then
    Code := 1
  else
    Code := 0;

  c := Text[i + Code];

  // mot simple sans guillemets
  if not CharInSet(c, ['"', '''']) then
  begin
    Delete(Text, 1, i - 1);
    Delete(LText, 1, i - 1);
    j := Pos('>', Text);
    i := Pos(' ', Text);
    if i = 0 then
      i := j
    else if (j <> 0) and (j < i) then
      i := j;
  end
  // Mot entouré de séparateurs
  else
  begin
    Delete(Text, 1, i + Code);
    Delete(LText, 1, i + Code);
    j := Pos('>', Text);
    i := Pos(c, Text);
    if i = 0 then
      Exit
    else if (j > 0) and (j < i) then
      Exit;
  end;

  ResultStr := Copy(Text, 1, i - 1 - Code);

  Delete(Text, 1, i);
  Delete(LText, 1, i);
  ResultStr := Replace(ResultStr, #13, '');
  ResultStr := Replace(ResultStr, #10, '');
end;

constructor THTTPSBrowserPlugin.Create;
begin
  inherited;
  HttpCli             := THTTPClient.Create;
  FileList            := TStringList.Create;
  FileList.Sorted     := True;
  FileList.Duplicates := dupIgnore;
  PathExe             := ExtractFilePath(GetDllPathName);
  ini                 := TIni.Create(PathExe + 'httpbrowser.ini');
  Strings             := TLines.Create;
  Strings.SetLanguage(PathExe + ini.GetS('Language') + '.lng');
  GetSizeExtensions := ' ' + LowerCase(ini.GetS('GetSizeExtensions', '')) + ' ';
  links             := LowerCase(' ' + ini.GetS('LinksExtensions', '.php .htm .html .shtm .shtml .php3 .php4 .php5 .cfm .asp .jsp .swf .jhtm .jhtml .phtml .phtm .chtml .chtml .adp .dml .xml .xhtml .xhtm .xiti') + ' ');
  BookMark          := TBookMark.Create;
  History           := THistory.Create;
  // HttpReq.ProxySettings.Host := ini.GetS('Proxy');
  // HttpCli.ProxySettings.Port := ini.GetS('ProxyPort', '80');
  // HttpCli.ProxySettings.UserName := ini.GetS('ProxyLogin');
  // HttpCli.ProxySettings.Password := ini.GetS('ProxyPassword');
  // HttpCli.OnHeaderEnd   := ProgressObject.HeaderEnd;
end;

destructor THTTPSBrowserPlugin.Destroy;
begin
  FreeAndNil(FileList);
  FreeAndNil(ini);
  FreeAndNil(HttpCli);
  FreeAndNil(BookMark);
  FreeAndNil(Strings);
  FreeAndNil(History);
  inherited;
end;

procedure THTTPSBrowserPlugin.Init;
begin
  PreviousPath := '';
  FileDate     := DateTimeToFileTime(Now);
  GetFileNow   := '';
end;

function THTTPSBrowserPlugin.GetFileSize(url: string): Integer;
begin
  try
    Result := HttpCli.Head(url).ContentLength;
  except
    Result := 0;
  end;
end;

function THTTPSBrowserPlugin.GetPage(var url: string): string;
var res:IHTTPResponse;
begin
  res := HttpCli.Get(url);
  if res.StatusCode = 200 then
    Result := res.ContentAsString(TEncoding.ASCII)
  else
    Result := '';
  ProgressBar(PluginNumber, PChar(RemoteFile), PChar(LocalFile), 100);
end;


procedure THTTPSBrowserPlugin.ParsePage(var sl: TStringList; url: string);
var
  page : string;
  LPage: string;
  s    : string;
  ls   : string;
  l    : string;
begin
  RemoteFile := url;
  LocalFile  := Strings[1];

  page  := LastPage;
  LPage := LowerCase(page);

  if page = '' then
    Exit;
  s  := page;
  ls := LPage;

  while GetFirstString(s, ls, 'href', l) do
  begin
    if CheckValue(l) then
      sl.Add(l);
  end;

  s  := page;
  ls := LPage;
  // while GetFirstString(s, ls, ' src=', '"', '"', l) do
  while GetFirstString(s, ls, 'src', l) do
    if CheckValue(l) then
      sl.Add(l);
end;


procedure THTTPSBrowserPlugin.BuildFindData(var FD: TWin32FindData; FileName: string);
var
  Ext: string;
  s  : string;
begin
  FillChar(FD, SizeOf(FD), 0);
  StrCopy(FD.cFileName, PChar(FileName));
  FD.nFileSizeLow := 100;

  if FileName <> Strings[0] then
  begin
    if Pos('http', FileName) = 1 then
      s := FileName
    else
      s := GetURLRoot(CurrentPath) + '/' + FileName;

    Ext := LowerCase(ExtractFileExt(FileName));
    if (Ext <> '') and (Ext[1] = '.') and (Pos(' ' + Ext + ' ', GetSizeExtensions) > 0) then
      FD.nFileSizeLow   := GetFileSize(s);
    FD.ftCreationTime   := FileDate;
    FD.ftLastWriteTime  := FileDate;
    FD.ftLastAccessTime := FileDate;
  end;
end;

procedure THTTPSBrowserPlugin.PageList(var sl: TStringList);
begin
  sl.Add('...');
  sl.Add(Strings[2]);
  CurrentPath := DeleteUntilLast(CurrentPath, '\');
  ParsePage(sl, CurrentPath);
end;

procedure THTTPSBrowserPlugin.RootList(var sl: TStringList);
begin
  sl.Add(Strings[0]); // Connect
  sl.Add(Strings[6]); // Choose Language
  sl.AddStrings(BookMark.BMList);
end;

procedure THTTPSBrowserPlugin.LangList(var sl: TStringList);
var
  sr: TSearchRec;
begin
  if FindFirst(PathExe + 'hb_*.lng', faAnyFile, sr) = 0 then
  begin
    sl.Add(ChangeFileExt(sr.Name, ''));
    while FindNext(sr) = 0 do
      sl.Add(ChangeFileExt(sr.Name, ''));
  end;
end;


procedure THTTPSBrowserPlugin.CreateFileList;
begin
  FileList.Clear;
  if CurrentPath = '\' then
    RootList(FileList)
  else if CurrentPath = '\' + Strings[6] then
    LangList(FileList)
  else
    PageList(FileList);
end;

procedure THTTPSBrowserPlugin.GetBinaryFile(const url, FileName: string);
var
  fs : TFileStream;
begin
  fs  := TFileStream.Create(FileName, fmCreate);
  try
    HttpRes := HttpCli.Get(url);
    fs.CopyFrom(HttpRes.ContentStream, HttpRes.ContentStream.Size);
  finally
    fs.Free;
  end;
  ProgressBar(PluginNumber, PChar(RemoteFile), PChar(LocalFile), 100);
end;



end.
