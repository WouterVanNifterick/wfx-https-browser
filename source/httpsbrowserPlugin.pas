unit HttpsbrowserPlugin;

interface

uses
  System.SysUtils,
  System.Classes,
  WinApi.Windows,
  WinApi.ShellApi,
  System.AnsiStrings,
  System.Net.HttpClient,
  WfxPlugin,
  ObjectIni,
  ObjectLines,
  UnitStrings,
  ObjectBookmark,

  ObjectHistory;


type
  THTTPSBrowserPlugin = class(TWFXPlugin)
  public
    HttpCli          : THTTPClient;
    HttpRes          : IHTTPResponse;
    Received         : Int64;
    links            : string;
    GetSizeExtensions: string;
    BookMark         : TBookMark;
    History          : THistory;
    LastPage         : string;
    AbortCopy        : Boolean;
    URL              : string;
    Parsed           : Boolean;
    OriginalLastPage : string;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Init;

    function GetFileSize(URL: string): Integer;
    function GetPage(var URL: string): string;
    procedure GetBinaryFile(const URL, FileName: string);
    procedure ParsePage(var sl: TStringList; URL: string);
    procedure BuildFindData(var FD: TWin32FindData; FileName: string);
    procedure PageList(var sl: TStringList);
    procedure RootList(var sl: TStringList);
    procedure LangList(var sl: TStringList);
    procedure CreateFileList;
    procedure GoBack(RemoteName: PWideChar);
    procedure Connect(RemoteName: PWideChar);
    function LoadFromWeb(Name: string; RemoteName: PWideChar): Integer;
    procedure Download;
    function ExecuteFile(MainWin: THandle; RemoteName, Verb: PWideChar): Integer; override;
    function FindFirstFile(var FindData: _WIN32_FIND_DATAW; Path: PWideChar): Cardinal; override;
    function GetFile(RemoteName,LocalName:string):Integer;
    function GetIcon(RemoteName:string; var TheIcon:HICON):Integer;
    function Delete(const RemoteName: string):Boolean; override;
    function GetPluginName: string; override;
  end;

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

function GetURLRoot(const url: string): string;
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
      d      := d.Replace('.', '_');
      Result := d + '.htm';
    end;
  end
  else
  begin
    Result := FilterFileName(Path);
  end;
  if Pos('http://', Result) = 1 then
    Delete(Result, 1, 7);
  Result := Result.Replace('/', '_');
  Result := Result.Replace('?', '_');
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

  ll := LowerCase(line);

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
    else
      if (j <> 0) and (j < i) then
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

  System.Delete(Text, 1, i);
  System.Delete(LText, 1, i);
  ResultStr := ResultStr
                 .Replace(#13, '')
                 .Replace(#10, '');
end;

procedure THTTPSBrowserPlugin.Connect(RemoteName: PWideChar);
var LocalURL: string;
begin
  // Connect
  FileList.Clear;
  LocalURL := ini.GetS('LastURL', 'http://www.google.com');
  if Input('', '', LocalURL, RT_URL) then
  begin
    ini.SetValue('LastURL', LocalURL);
    if not IsWebPage(LocalURL, links) then
    begin
      URL := LocalURL;
      StrPCopy(RemoteName, '\');
      Exit;
    end;
    History.Add(LocalURL);
    LastPage := GetPage(LocalURL);
    Parsed := True;
    StrPCopy(RemoteName, '\' + LocalURL);
    CurrentPath := RemoteName;
  end
  else
    StrPCopy(RemoteName, '\');
end;

constructor THTTPSBrowserPlugin.Create;
begin
  inherited;
  HttpCli             := THTTPClient.Create;
  GetSizeExtensions := ' ' + LowerCase(ini.GetS('GetSizeExtensions', '')) + ' ';
  links             := LowerCase(' ' + ini.GetS('LinksExtensions', '.php .htm .html .shtm .shtml .php3 .php4 .php5 .cfm .asp .jsp .swf .jhtm .jhtml .phtml .phtm .chtml .chtml .adp .dml .xml .xhtml .xhtm .xiti') + ' ');
  BookMark          := TBookMark.Create(Self);
  History           := THistory.Create;
  // HttpReq.ProxySettings.Host := ini.GetS('Proxy');
  // HttpCli.ProxySettings.Port := ini.GetS('ProxyPort', '80');
  // HttpCli.ProxySettings.UserName := ini.GetS('ProxyLogin');
  // HttpCli.ProxySettings.Password := ini.GetS('ProxyPassword');
  // HttpCli.OnHeaderEnd   := ProgressObject.HeaderEnd;
end;

function THTTPSBrowserPlugin.Delete(const RemoteName: string):Boolean;
var
  s: string;
begin
  inherited;
  Result := False;
  if CurrentPath <> '\' then
    Exit;
  Result := True;
  s      := RemoteName;
  if s <> '' then
    System.Delete(s, 1, 1);
  BookMark.Delete(s);
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

procedure THTTPSBrowserPlugin.Download;
var
  ofn: string;
begin
  if URL <> '' then
  begin
    try
      ofn := PathExe + CreateFileName(URL);
      RemoteFile := URL;
      LocalFile := ofn;
      GetBinaryFile(URL, ofn);
      if not AbortCopy then
        ShellExecute(HInstance, 'Open', PChar(ofn), PChar(''), PChar(''), SW_SHOWNORMAL);
      AbortCopy := False;
    finally
      URL := '';
    end;
  end;
end;

function THTTPSBrowserPlugin.ExecuteFile(MainWin: THandle; RemoteName,
  Verb: PWideChar): Integer;
var
  Name     : string;
  FileName : string;
  vb       : string;
begin
  try
    // @@@    if IsGettingFile then
    // begin
    // Result := FS_EXEC_OK;
    // Exit;
    // end;

    Name   := RemoteName;
    Result := FS_EXEC_SYMLINK;

    vb := Verb;
    if vb.StartsWith('quote') then
      Exit;

    if Name = '\...' then
    begin
      GoBack(RemoteName);
      Exit;
    end;

    System.Delete(Name, 1, 1);

    if Name = Strings[0] then
    begin
      Connect(RemoteName);
      Exit;
    end;

    if Name = '' then
      Exit(FS_EXEC_OK);

    FileName := OnlyFileName(RemoteName);
    Name     := CompletePath(CurrentPath, RemoteName);

    if FileName = Strings[2] then // Bookmark
    begin
      BookMark.Add(CurrentPath);
      Result := FS_EXEC_OK;
    end
    else
      if FileName = Strings[6] then // Language
        StrPCopy(RemoteName, '\' + Strings[6])
      else
        if RemoteName = '\' + Strings[6] + '\' + FileName then // language 2
        begin
          ini.SetValue('Language', FileName);
          Strings.SetLanguage(PathExe + FileName + '.lng');
          StrPCopy(RemoteName, '\');
        end
        else
          Exit(LoadFromWeb(Name, RemoteName));
  except
    on E: Exception do
    begin
      Result := FS_EXEC_ERROR;
      TCShowMessage('', E.Message);
    end;
  end;
end;

function THTTPSBrowserPlugin.FindFirstFile(var FindData: _WIN32_FIND_DATAW; Path: PWideChar):Cardinal;
begin
  FindData := default(tWIN32FINDDATAW);
  CurrentPath := Path;
  if not Parsed then
    ExecuteFile(0, Path, '');
  Parsed := False;
  CreateFileList;
  if FileList.Count = 0 then
    Result := INVALID_HANDLE_VALUE
  else
  begin
    Result := 0;
    FLIndex := 0;
    BuildFindData(FindData, FileList[0]);
  end;
  Download;
end;

procedure THTTPSBrowserPlugin.Init;
begin
  PreviousPath := '';
  FileDate     := DateTimeToFileTime(Now);
  URL   := '';
end;

function THTTPSBrowserPlugin.GetFile(RemoteName, LocalName: string): Integer;
var
  i             : Integer;
  url           : string;
  Name          : string;
  LocalPath     : string;
  RemoteFileName: string;
  sStream       : TStringStream;
begin
  try
    Name := LocalName;

    // calcule local path
    RemoteFileName := OnlyFileName(RemoteName);
    LocalPath      := Name;
    i              := Length(Name);
    if RemoteFileName = '...' then
    begin
      System.Delete(LocalPath, i - 2, 3);
      StrPCopy(PChar(LocalName), IncludeTrailingPathDelimiter(LocalPath) + CreateFileName(CurrentPath));
      StrPCopy(PChar(RemoteName), '\' + CurrentPath);

      sStream := TStringStream.Create(OriginalLastPage);
      try
        sStream.SaveToFile(LocalName);
      finally
        sStream.Free;
      end;

      Result := FS_FILE_OK;
      Exit;
    end
    else
    begin
      LocalPath := ExtractFileDir(LocalPath);
    end;
    url := CompletePath(CurrentPath, RemoteName);
    Name := IncludeTrailingPathDelimiter(LocalPath) + CreateFileName(url);

    LocalName := Name;
    RemoteName := url;

    if (CurrentPath = '\') or (CurrentPath = '\' + Strings[6]) then
    begin
      Result := FS_FILE_NOTSUPPORTED
    end
    else
    begin
      RemoteFile := RemoteName;
      LocalFile  := LocalName;
      if not AbortCopy then
        GetBinaryFile(url, PChar(Name));
      Result := FS_FILE_OK;
    end;
  except
    on E: Exception do
    begin
      Result := FS_FILE_READERROR;
      TCShowMessage('', E.Message);
    end;
  end;
end;

function THTTPSBrowserPlugin.GetFileSize(url: string): Integer;
begin
  try
    Result := HttpCli.Head(url).ContentLength;
  except
    Result := 0;
  end;
end;

function THTTPSBrowserPlugin.GetIcon(RemoteName: string; var TheIcon:HICON): Integer;
var
  Name: string;
  url : string;
  ofn : string;
begin
  Result := FS_ICON_USEDEFAULT;
  Name   := RemoteName;
  if Copy(Name, Length(Name) - 3, 4) = '\..\' then
  begin
    Exit;
  end;

  ofn := OnlyFileName(RemoteName);
  url := CompletePath(CurrentPath, RemoteName);
  if Name = '\' + Strings[0] then
  begin
    TheIcon := LoadIcon(HInstance, 'ZCONNECT');
    Result  := FS_ICON_EXTRACTED;
  end
  else if Pos('\...', Name) > 0 then
  begin
    TheIcon := LoadIcon(HInstance, 'ZBACK');
    Result  := FS_ICON_EXTRACTED;
  end
  else if Pos('\' + Strings[6], Name) = 1 then
  begin
    TheIcon := LoadIcon(HInstance, 'ZLANG');
    Result  := FS_ICON_EXTRACTED;
  end
  else if (ofn = Strings[2]) or (BookMark.BMList.IndexOf(ofn) > -1) then
  begin
    TheIcon := LoadIcon(HInstance, 'ZBOOK');
    Result  := FS_ICON_EXTRACTED;
  end
  else if (ofn <> '') and IsWebPage(url,Links) then
  begin
    TheIcon := LoadIcon(HInstance, 'ZLINK');
    Result  := FS_ICON_EXTRACTED;
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


function THTTPSBrowserPlugin.GetPluginName: string;
begin
  Result := 'HTTPS Browser';
end;

procedure THTTPSBrowserPlugin.GoBack(RemoteName: PWideChar);
var s:string;
begin
  s := History.GoBack;
  StrPCopy(RemoteName, s);
  System.Delete(s, 1, 1);
  LastPage := GetPage(s);
  Parsed := True;
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
  if System.SysUtils.FindFirst(PathExe + 'hb_*.lng', faAnyFile, sr) = 0 then
  begin
    sl.Add(ChangeFileExt(sr.Name, ''));
    while FindNext(sr) = 0 do
      sl.Add(ChangeFileExt(sr.Name, ''));
  end;
end;


function THTTPSBrowserPlugin.LoadFromWeb(Name: string; RemoteName: PWideChar): Integer;
begin
  if IsWebPage(Name, Links) or IsRoot(Name, RemoteName, Links) then
  begin
    // Webpage
    History.Add(Name);
    LastPage := GetPage(Name);
    Parsed := True;
    StrPCopy(RemoteName, '\' + Name);
    Result := FS_EXEC_SYMLINK;
  end
  else
    Result := FS_EXEC_YOURSELF;
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
