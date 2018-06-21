library httpsbrowser;

{$DEFINE NOFORMS}

uses
  System.SysUtils,
  Vcl.Dialogs,
  System.Classes,
  WinApi.Windows,
  WinApi.ShellApi,
  System.AnsiStrings,
  System.Net.HttpClient,
  ObjectIni in 'ObjectIni.pas',
  ObjectLines in 'ObjectLines.pas',
  UnitStrings in 'UnitStrings.pas',
  ObjectBookmark in 'ObjectBookmark.pas',
  ObjectTimer in 'ObjectTimer.pas',
  ObjectHistory in 'ObjectHistory.pas';

{$E wfx}
{$IFDEF WIN64}
{$E w64}
{$IFNDEF VER230}
{$E wfx64}
{$ENDIF}
{$ENDIF}
{$R hbrowser.res}
{$R httpbrowser.res}

type
  TPlugin = class
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

var
  Plugin: TPlugin;

  { TProgress }

function DateTimeToFileTime(MyDateTime: TDateTime): TFileTime;
var
  MyFileAge      : Integer;
  MyLocalFileTime: _FILETIME;
begin
  MyFileAge := DateTimeToFileDate(MyDateTime);
  DosDateTimeToFileTime(LongRec(MyFileAge).Hi, LongRec(MyFileAge).Lo, MyLocalFileTime);
  LocalFileTimeToFileTime(MyLocalFileTime, Result);
end;

function TPlugin.GetFileSize(url: string): Integer;
begin
  try
    Result := HttpCli.Head(url).ContentLength;
  except
    Result := 0;
  end;
end;

function TPlugin.GetPage(var url: string): string;
var res:IHTTPResponse;
begin
  res := HttpCli.Get(url);
  if res.StatusCode = 200 then
    Result := res.ContentAsString(TEncoding.ASCII)
  else
    Result := '';
  ProgressBar(PluginNumber, PChar(RemoteFile), PChar(LocalFile), 100);
end;

procedure TPlugin.Init;
begin
  PreviousPath := '';
  FileDate     := DateTimeToFileTime(Now);
  GetFileNow   := '';
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

function IsWebPage(url,links: string): Boolean;
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

procedure TPlugin.GetBinaryFile(const url, FileName: string);
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

function FsInit(PluginNr: Integer; pProgressProc: TProgressProc; pLogProc: TLogProc; pRequestProc: TRequestProc): Integer; stdcall;
begin
  Result := -1;
end;

function FsInitW(PluginNr: Integer; pProgressProcW: tProgressProcW; pLogProcW: tLogProcW; pRequestProcW: tRequestProcW): Integer; stdcall;
begin
  PluginNumber := PluginNr;
  EnterText    := pRequestProcW;
  ProgressBar  := pProgressProcW;
  LogProc      := pLogProcW;
  Result       := 0;
  Plugin.Init;

end;


function MakeRelative(CompleteText, root: string): string;
begin
  if Pos(LowerCase(root), LowerCase(CompleteText)) = 1 then
    Result := Copy(CompleteText, Length(root) + 1, Length(CompleteText))
  else
    Result := CompleteText;
end;


function CheckValue(line: string): Boolean;
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


procedure TPlugin.ParsePage(var sl: TStringList; url: string);
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


procedure TPlugin.BuildFindData(var FD: TWin32FindData; FileName: string);
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

procedure TPlugin.PageList(var sl: TStringList);
begin
  sl.Add('...');
  sl.Add(Strings[2]);
  CurrentPath := DeleteUntilLast(CurrentPath, '\');
  ParsePage(sl, CurrentPath);
end;

procedure TPlugin.RootList(var sl: TStringList);
begin
  sl.Add(Strings[0]); // Connect
  sl.Add(Strings[6]); // Choose Language
  sl.AddStrings(BookMark.BMList);
end;

procedure TPlugin.LangList(var sl: TStringList);
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

constructor TPlugin.Create;
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

procedure TPlugin.CreateFileList;
begin
  FileList.Clear;
  if CurrentPath = '\' then
    RootList(FileList)
  else if CurrentPath = '\' + Strings[6] then
    LangList(FileList)
  else
    PageList(FileList);
end;

destructor TPlugin.Destroy;
begin
  FreeAndNil(FileList);
  FreeAndNil(ini);
  FreeAndNil(HttpCli);
  FreeAndNil(BookMark);
  FreeAndNil(Strings);
  FreeAndNil(History);
  inherited;
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

function FsExecuteFile(MainWin: THandle; RemoteName, Verb: PAnsiChar): Integer; stdcall;
begin
  Result := FS_EXEC_ERROR
end;

function FsExecuteFileW(MainWin: THandle; RemoteName, Verb: PWideChar): Integer; stdcall;
var
  i   : Integer;
  s   : string;
  Name: string;
  ofn : string;
  vb  : string;
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
    if Pos('quote', vb) = 1 then
      Exit;

    i := Pos('\...', Name);
    if (i > 0) and (i = Length(Name) - 3) then
    begin
      s := Plugin.History.GoBack;
      StrPCopy(RemoteName, s);
      Delete(s, 1, 1);
      Plugin.LastPage := Plugin.GetPage(s);
      Plugin.Parsed   := True;
      Exit;
    end;

    Delete(Name, 1, 1);

    if (Name = Strings[0]) then
    begin // Connect
      Plugin.FileList.Clear;
      s := ini.GetS('LastURL', 'http://www.google.com');
      if Input('', '', s, RT_URL) then
      begin
        ini.SetValue('LastURL', s);
        if not IsWebPage(s,Plugin.links) then
        begin
          Plugin.GetFileNow := s;
          StrPCopy(RemoteName, '\');
          Exit;
        end;
        Plugin.History.Add(s);
        Plugin.LastPage := Plugin.GetPage(s);
        Plugin.Parsed   := True;
        StrPCopy(RemoteName, '\' + s);
        Plugin.CurrentPath := RemoteName;
      end
      else
        StrPCopy(RemoteName, '\');
    end
    else if Name <> '' then
    begin // Autres que connect
      ofn  := OnlyFileName(RemoteName);
      Name := CompletePath(Plugin.CurrentPath, RemoteName);
      if ofn = Strings[2] then
      begin // Bookmark
        Plugin.BookMark.Add(Plugin.CurrentPath);
        Result := FS_EXEC_OK;
      end
      else if ofn = Strings[6] then
        StrPCopy(RemoteName, '\' + Strings[6]) // Language
      else if RemoteName = '\' + Strings[6] + '\' + ofn then
      begin // language 2
        ini.SetValue('Language', ofn);
        Strings.SetLanguage(Plugin.PathExe + ofn + '.lng');
        StrPCopy(RemoteName, '\');
      end
      else
      begin
        if IsWebPage(Name, Plugin.Links) or IsRoot(Name, RemoteName, Plugin.Links) then
        begin // Webpage
          Plugin.History.Add(Name);
          Plugin.LastPage := Plugin.GetPage(Name);
          Plugin.Parsed   := True;
          StrPCopy(RemoteName, '\' + Name);
        end
        else
          Result := FS_EXEC_YOURSELF;
      end
    end
    else
      Result := FS_EXEC_OK;
  except
    on E: Exception do
    begin
      Result := FS_EXEC_ERROR;
      TCShowMessage('', E.Message);
    end;
  end;
end;

function FsFindFirst(Path: PAnsiChar; var FindData: TWin32FindData): THandle; stdcall;
begin
  Result := 0;
end;

function FsFindFirstW(Path: PWideChar; var FindData: tWIN32FINDDATAW): THandle; stdcall;
var
  ofn: string;
begin
  FillChar(FindData, SizeOf(FindData), 0);
  Plugin.CurrentPath := Path;

  if not Plugin.Parsed then
  begin
    FsExecuteFileW(0, Path, PChar(''));
  end;
  Plugin.Parsed := False;
  Plugin.CreateFileList;
  if Plugin.FileList.Count = 0 then
    Result := INVALID_HANDLE_VALUE
  else
  begin
    Result  := 0;
    Plugin.FLIndex := 0;
    Plugin.BuildFindData(FindData, Plugin.FileList[0]);
  end;

  if Plugin.GetFileNow <> '' then
  begin
    try
      ofn                       := Plugin.PathExe + CreateFileName(Plugin.GetFileNow);
      Plugin.RemoteFile := Plugin.GetFileNow;
      Plugin.LocalFile  := ofn;
      Plugin.GetBinaryFile(Plugin.GetFileNow, ofn);
      if not Plugin.AbortCopy then
        ShellExecute(HInstance, 'Open', PChar(ofn), PChar(''), PChar(''), SW_SHOWNORMAL);
      Plugin.AbortCopy := False;
    finally
      Plugin.GetFileNow := '';
    end;
  end;
end;

function FsFindNext(Hdl: THandle; var FindData: TWin32FindData): bool; stdcall;
begin
  Result := False;
end;

function FsFindNextW(Hdl: THandle; var FindDataW: tWIN32FINDDATAW): bool; stdcall;
begin
  FillChar(FindDataW, SizeOf(FindDataW), 0);
  Inc(Plugin.FLIndex);
  Result := Plugin.FileList.Count > Plugin.FLIndex;
  if Result then
    Plugin.BuildFindData(FindDataW, Plugin.FileList[Plugin.FLIndex]);
end;

function FsFindClose(Hdl: THandle): Integer; stdcall;
begin
  Result := 0;
end;

function FsDeleteFile(RemoteName: PAnsiChar): bool; stdcall;
begin
  Result := False;
end;

function FsDeleteFileW(RemoteName: PWideChar): bool; stdcall;
var
  s: string;
begin
  Result := False;
  if Plugin.CurrentPath <> '\' then
    Exit;
  Result := True;
  s      := RemoteName;
  if s <> '' then
    Delete(s, 1, 1);
  Plugin.BookMark.Delete(s);
end;

function FsGetFile(RemoteName, LocalName: PAnsiChar; CopyFlags: Integer; RemoteInfo: pRemoteInfo): Integer; stdcall;
begin
  Result := FS_FILE_OK;
end;

function FsGetFileW(RemoteName, LocalName: PWideChar; CopyFlags: Integer; RemoteInfo: pRemoteInfo): Integer; stdcall;
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
      Delete(LocalPath, i - 2, 3);
      StrPCopy(LocalName, IncludeTrailingPathDelimiter(LocalPath) + CreateFileName(Plugin.CurrentPath));
      StrPCopy(RemoteName, '\' + Plugin.CurrentPath);

      sStream := TStringStream.Create(Plugin.OriginalLastPage);
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
      {
        if AnsiPos(LocalPath, RemoteFileName) > 0 then
        begin
        Delete( LocalPath, i - Length(RemoteFileName) , i );
        url := CompletePath(RemoteName);
        Name := LocalPath + '\' + CreateFileName(url);
        end;
      }
      // Delete( LocalPath, i - Length(RemoteFileName) , i );
      LocalPath := ExtractFileDir(LocalPath);
    end;
    // fin path local

    url := CompletePath(Plugin.CurrentPath, RemoteName);
    // Name := LocalPath + '\' + CreateFileName(url);
    Name := IncludeTrailingPathDelimiter(LocalPath) + CreateFileName(url);

    StrPCopy(LocalName, Name);
    StrPCopy(RemoteName, url);

    if (Plugin.CurrentPath = '\') or (Plugin.CurrentPath = '\' + Strings[6]) then
    begin
      Result := FS_FILE_NOTSUPPORTED
    end
    else
    begin
      Plugin.RemoteFile := RemoteName;
      Plugin.LocalFile  := LocalName;
      if not Plugin.AbortCopy then
        Plugin.GetBinaryFile(url, PChar(Name));
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


procedure FsGetDefRootName(DefRootName: PAnsiChar; MaxLen: Integer); stdcall;
begin
  System.Ansistrings.StrLCopy(DefRootName, 'HTTPS Browser', MaxLen - 1);
end;


procedure FsStatusInfo(RemoteDir: PAnsiChar; InfoStartEnd, InfoOperation: Integer); stdcall;
begin
end;

procedure FsStatusInfoW(RemoteDir: PWideChar; InfoStartEnd, InfoOperation: Integer); stdcall;
begin
  if (InfoStartEnd = FS_STATUS_END) and (InfoOperation = FS_STATUS_OP_GET_MULTI) then
    Plugin.AbortCopy := False;
  // ShowMessageFmt('info op = %d', [InfoOperation]);
end;

function FsExtractCustomIcon(RemoteName: PAnsiChar; ExtractFlags: Integer; var TheIcon: HIcon): Integer; stdcall;
begin
  Result := 0;
end;

function FsExtractCustomIconW(RemoteName: PWideChar; ExtractFlags: Integer; var TheIcon: HIcon): Integer; stdcall;
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
  url := CompletePath(Plugin.CurrentPath, RemoteName);
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
  else if (ofn = Strings[2]) or (Plugin.BookMark.BMList.IndexOf(ofn) > -1) then
  begin
    TheIcon := LoadIcon(HInstance, 'ZBOOK');
    Result  := FS_ICON_EXTRACTED;
  end
  else if (ofn <> '') and IsWebPage(url,Plugin.Links) then
  begin
    TheIcon := LoadIcon(HInstance, 'ZLINK');
    Result  := FS_ICON_EXTRACTED;
  end;
end;

procedure DLLEntryPoint(dwReason: DWORD);
begin
  case dwReason of
    DLL_PROCESS_ATTACH : Plugin := TPlugin.Create;
    DLL_PROCESS_DETACH : FreeAndNil(Plugin);
  end;
end;



exports
  FsInit, FsInitW,
  FsFindFirst, FsFindFirstW,
  FsFindNext, FsFindNextW,
  FsFindClose,
  FsGetDefRootName,
  FsExecuteFile, FsExecuteFileW,
  FsGetFile, FsGetFileW,
  FsStatusInfo, FsStatusInfoW,
  FsExtractCustomIcon, FsExtractCustomIconW,
  FsDeleteFile, FsDeleteFileW;

begin
  DllProc := @DLLEntryPoint;
  DLLEntryPoint(DLL_PROCESS_ATTACH);

end.
