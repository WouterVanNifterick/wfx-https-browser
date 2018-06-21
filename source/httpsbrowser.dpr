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
  TProgress = class(TObject)
    LocalFile: string;
    RemoteFile: string;
    tmr: TTimer;
    constructor Create;
    destructor Destroy; override;
    procedure UpdateProgressBar(Sender: TObject);
    procedure HeaderEnd(Sender: TObject);
  end;

var
  PreviousPath: string;
  CurrentPath : string;
  FLIndex     : Integer;
  PathExe     : string;

  HttpCli: THTTPClient;
  HttpRes: IHTTPResponse;

  Received         : Int64;
  FileDate         : _FILETIME;
  links            : string;
  FileList         : TStringList;
  ProgressObject   : TProgress;
  GetSizeExtensions: string;
  BookMark         : TBookMark;
  History          : THistory;
  LastPage         : string;
  // IsGettingFile    : Boolean;
  AbortCopy       : Boolean;
  GetFileNow      : string;
  Parsed          : Boolean;
  OriginalLastPage: string;

  { TProgress }

constructor TProgress.Create;
begin
  inherited Create;
  tmr          := TTimer.Create(nil);
  tmr.Enabled  := False;
  tmr.OnTimer  := UpdateProgressBar;
  tmr.Interval := 200;
end;

destructor TProgress.Destroy;
begin
  FreeAndNil(tmr);
  inherited;
end;

procedure TProgress.HeaderEnd(Sender: TObject);
begin
  tmr.Enabled := True;
end;

procedure TProgress.UpdateProgressBar(Sender: TObject);
var
  i: Integer;
begin
  if AbortCopy then
    Exit;

  i := HttpRes.ContentLength;

  // Received := HttpCli.OnReceiveData( . .. RcvdCount;
  if (i > 0) and (i > Received) then
    i := Trunc(100 * Received / i)
  else
    i := 100;

  if ProgressBar(PluginNumber, pChar(RemoteFile), pChar(LocalFile), i) = 1 then
  begin
    AbortCopy := True;
    // @@@    HttpCli.Abort;
  end;
end;

// ______________________________________________________________________________
function GetFileSize(url: string): Integer;
begin
  try
    Result := HttpCli.Head(url).ContentLength;
  except
    Result := 0;
  end;
end;

// ______________________________________________________________________________
function GetPage(var url: string): string;
var res:IHTTPResponse;
begin
  res := HttpCli.Get(url);
  if res.StatusCode = 200 then
    Result := res.ContentAsString(TEncoding.ASCII)
  else
    Result := '';
  ProgressBar(PluginNumber, PChar(ProgressObject.RemoteFile), PChar(ProgressObject.LocalFile), 100);
  ProgressObject.tmr.Enabled := False;
end;

// ______________________________________________________________________________
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
  if i > 0 then begin
    prot := Copy(url, 1, i + 1);
    Delete(url, 1, i + 1);
  end;
  i := Pos('/', url);
  if i = 0 then begin
    domain := prot + TrimSlashes(url);
    Exit;
  end;
  domain := prot + TrimSlashes(Copy(url, 1, i - 1));
  Delete(url, 1, i);
  i := Pos('/', url);
  while i > 0 do begin
    s := s + Copy(url, 1, i);
    Delete(url, 1, i);
    i := Pos('/', url);
  end;
  folders := s;
  if Pos('.', url) > 0 then page := TrimSlashes(url)
  else folders := folders + url;
  folders   := TrimSlashes(folders);
end;
// ______________________________________________________________________________
function GetURLRoot(URL : string) : string;
var
  d, f, p: string;
begin
  Result := DeleteUntilLast(URL, '\');
  SplitURL(Result, d, f, p);
  Result := d + '/' + f;
end;
// ______________________________________________________________________________
function CompletePath(RemoteName: PChar): string;
var
  root   : string;
  d, f, p: string;
begin
  Result := StrPas(RemoteName);
  Result := DeleteUntilLast(Result, '\');
  if Pos('//', Result) = 0 then begin
    root := GetURLRoot(CurrentPath);
    if Result[1] = '/' then begin
      SplitURL(Root, d, f, p);
      Root := d;
    end;
    if (root <> '') and (root[Length(root)] = '/') then
      Result := root + TrimSlashes(Result)
    else Result := root + '/' + TrimSlashes(Result);
  end;
end;
// ______________________________________________________________________________
function OnlyFileName(const RemoteName: string): string;
begin
  Result := DeleteUntilLast(RemoteName, '\');
end;
// ______________________________________________________________________________
function FilterFileName(FileName: string): string;
var
  i: Integer;
begin
  Result := FileName;

  Result := DeleteUntilLast(Result, '/');
  Result := DeleteUntilLast(Result, '\');

  i := Pos('?', Result);
  if i > 0 then Delete(Result, i, Length(Result));
end;
// ______________________________________________________________________________
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

// ______________________________________________________________________________
function IsWebPage(url: string): Boolean;
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

// ______________________________________________________________________________
procedure GetBinaryFile(const url, FileName: string);
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
  ProgressBar(PluginNumber, PChar(ProgressObject.RemoteFile), PChar(ProgressObject.LocalFile), 100);
end;

// ______________________________________________________________________________
function DateTimeToFileTime(MyDateTime: TDateTime): TFileTime;
var
  MyFileAge      : Integer;
  MyLocalFileTime: _FILETIME;
begin
  MyFileAge := DateTimeToFileDate(MyDateTime);
  DosDateTimeToFileTime(LongRec(MyFileAge).Hi, LongRec(MyFileAge).Lo, MyLocalFileTime);
  LocalFileTimeToFileTime(MyLocalFileTime, Result);
end;
// ______________________________________________________________________________
function GetFirstString(var Text, LText: string; key: string; var ResultStr: string): Boolean;
var
  c   : Char;
  l   : Integer;
  j   : Integer;
  i   : Integer;
  Code: Integer;
begin
  ResultStr := '';
  // Key := ' ' + Key;

  i := Pos(Key, LText);
  if i = 0 then begin
    Result := False;
    Exit;
  end;

  Result := True;
  Inc(i, Length(Key) - 1);
  Delete(Text, 1, i);
  Delete(LText, 1, i);

  l := Length(Text);
  i := 1;

  // Suppression des espaces avant et après "="
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
  c      := Text[i + Code];

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
// ______________________________________________________________________________

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
  PreviousPath := '';
  FileDate     := DateTimeToFileTime(Now);
  GetFileNow   := '';
end;

// ______________________________________________________________________________
function MakeRelative(CompleteText, root: string): string;
begin
  if Pos(LowerCase(root), LowerCase(CompleteText)) = 1 then
    Result := Copy(CompleteText, Length(root) + 1, Length(CompleteText))
  else
    Result := CompleteText;
end;

// ______________________________________________________________________________
function CheckValue(line: string): Boolean;
var
  ll: string;
begin
  Result := False;
  if line = '' then
    Exit;
  ll     := LowerCase(line);
  Result := (ll <> '/') and (Pos('mailto', ll) <> 1) and (Pos('javascript', ll) <> 1) and (Pos('+', ll) = 0) and (Pos('"', ll) = 0) and (Pos('\', ll) = 0) and (Pos('''', ll) = 0) and (Pos('#', ll) <> 1);
end;

// ______________________________________________________________________________
procedure ParsePage(var sl: TStringList; url: string);
var
  page : string;
  LPage: string;
  s    : string;
  ls   : string;
  l    : string;
begin
  ProgressObject.RemoteFile := PChar(url);
  ProgressObject.LocalFile  := PChar(Strings[1]);
  // Page := GetPage(URL);
  page  := LastPage;
  LPage := LowerCase(page);

  if page = '' then
    Exit;
  s  := page;
  ls := LPage;
  // while GetFirstString(s, ls, ' href=', '"', '"', l) do
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

// ______________________________________________________________________________
procedure BuildFindData(var FD: TWin32FindData; FileName: string);
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

// ______________________________________________________________________________
procedure PageList(var sl: TStringList);
begin
  sl.Add('...');
  sl.Add(Strings[2]);
  CurrentPath := DeleteUntilLast(CurrentPath, '\');
  ParsePage(sl, CurrentPath);
end;

// ______________________________________________________________________________
procedure RootList(var sl: TStringList);
begin
  sl.Add(Strings[0]); // Connect
  sl.Add(Strings[6]); // Choose Language
  sl.AddStrings(BookMark.BMList);
end;

// ______________________________________________________________________________
procedure LangList(var sl: TStringList);
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

// ______________________________________________________________________________
procedure CreateFileList;
begin
  FileList.Clear;
  if CurrentPath = '\' then
    RootList(FileList)
  else if CurrentPath = '\' + Strings[6] then
    LangList(FileList)
  else
    PageList(FileList);
end;

function IsRoot(var Name: string; const RemoteName: string): Boolean;
begin
  Result := Copy(RemoteName, 2, Length(RemoteName)) = Copy(Name, 2, Length(Name));
  if Result then
  begin
    Name   := Copy(Name, 2, Length(Name));
    Result := IsWebPage(Name)
  end;
end;

// ______________________________________________________________________________
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
      s := History.GoBack;
      StrPCopy(RemoteName, s);
      Delete(s, 1, 1);
      LastPage := GetPage(s);
      Parsed   := True;
      Exit;
    end;

    Delete(Name, 1, 1);

    if (Name = Strings[0]) then
    begin // Connect
      FileList.Clear;
      s := ini.GetS('LastURL', 'http://www.google.com');
      if Input('', '', s, RT_URL) then
      begin
        ini.SetValue('LastURL', s);
        if not IsWebPage(s) then
        begin
          GetFileNow := s;
          StrPCopy(RemoteName, '\');
          Exit;
        end;
        History.Add(s);
        LastPage := GetPage(s);
        Parsed   := True;
        StrPCopy(RemoteName, '\' + s);
        CurrentPath := RemoteName;
      end
      else
        StrPCopy(RemoteName, '\');
    end
    else if Name <> '' then
    begin // Autres que connect
      ofn  := OnlyFileName(RemoteName);
      Name := CompletePath(RemoteName);
      if ofn = Strings[2] then
      begin // Bookmark
        BookMark.Add(CurrentPath);
        Result := FS_EXEC_OK;
      end
      else if ofn = Strings[6] then
        StrPCopy(RemoteName, '\' + Strings[6]) // Language
      else if RemoteName = '\' + Strings[6] + '\' + ofn then
      begin // language 2
        ini.SetValue('Language', ofn);
        Strings.SetLanguage(PathExe + ofn + '.lng');
        StrPCopy(RemoteName, '\');
      end
      else
      begin
        if IsWebPage(Name) or IsRoot(Name, RemoteName) then
        begin // Webpage
          History.Add(Name);
          LastPage := GetPage(Name);
          Parsed   := True;
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

// ______________________________________________________________________________
function FsFindFirst(Path: PAnsiChar; var FindData: TWin32FindData): THandle; stdcall;
begin
  Result := 0;
end;

function FsFindFirstW(Path: PWideChar; var FindData: tWIN32FINDDATAW): THandle; stdcall;
var
  ofn: string;
begin
  FillChar(FindData, SizeOf(FindData), 0);
  CurrentPath := Path;

  if not Parsed then
  begin
    FsExecuteFileW(0, Path, PChar(''));
  end;
  Parsed := False;
  CreateFileList;
  if FileList.Count = 0 then
    Result := INVALID_HANDLE_VALUE
  else
  begin
    Result  := 0;
    FLIndex := 0;
    BuildFindData(FindData, FileList[0]);
  end;

  if GetFileNow <> '' then
  begin
    try
      ofn                       := PathExe + CreateFileName(GetFileNow);
      ProgressObject.RemoteFile := PChar(GetFileNow);
      ProgressObject.LocalFile  := PChar(ofn);
      GetBinaryFile(GetFileNow, ofn);
      if not AbortCopy then
        ShellExecute(HInstance, 'Open', PChar(ofn), PChar(''), PChar(''), SW_SHOWNORMAL);
      AbortCopy := False;
    finally
      GetFileNow := '';
    end;
  end;
end;

// ______________________________________________________________________________
function FsFindNext(Hdl: THandle; var FindData: TWin32FindData): bool; stdcall;
begin
  Result := False;
end;

function FsFindNextW(Hdl: THandle; var FindDataW: tWIN32FINDDATAW): bool; stdcall;
begin
  FillChar(FindDataW, SizeOf(FindDataW), 0);
  Inc(FLIndex);
  Result := FileList.Count > FLIndex;
  if Result then
    BuildFindData(FindDataW, FileList[FLIndex]);
end;

// ______________________________________________________________________________
function FsFindClose(Hdl: THandle): Integer; stdcall;
begin
  Result := 0;
end;

// ______________________________________________________________________________
function FsDeleteFile(RemoteName: PAnsiChar): bool; stdcall;
begin
  Result := False;
end;

function FsDeleteFileW(RemoteName: PWideChar): bool; stdcall;
var
  s: string;
begin
  Result := False;
  if CurrentPath <> '\' then
    Exit;
  Result := True;
  s      := RemoteName;
  if s <> '' then
    Delete(s, 1, 1);
  BookMark.Delete(s);
end;

// ______________________________________________________________________________
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

    // calcule le path local
    RemoteFileName := OnlyFileName(RemoteName);
    LocalPath      := Name;
    i              := Length(Name);
    if RemoteFileName = '...' then
    begin
      Delete(LocalPath, i - 2, 3);
      StrPCopy(LocalName, IncludeTrailingPathDelimiter(LocalPath) + CreateFileName(CurrentPath));
      StrPCopy(RemoteName, '\' + CurrentPath);

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

    url := CompletePath(RemoteName);
    // Name := LocalPath + '\' + CreateFileName(url);
    Name := IncludeTrailingPathDelimiter(LocalPath) + CreateFileName(url);

    StrPCopy(LocalName, Name);
    StrPCopy(RemoteName, url);

    if (CurrentPath = '\') or (CurrentPath = '\' + Strings[6]) then
    begin
      Result := FS_FILE_NOTSUPPORTED
    end
    else
    begin
      ProgressObject.RemoteFile := RemoteName;
      ProgressObject.LocalFile  := LocalName;
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

// ______________________________________________________________________________
procedure FsGetDefRootName(DefRootName: PAnsiChar; MaxLen: Integer); stdcall;
begin
  System.Ansistrings.StrLCopy(DefRootName, 'HTTP Browser', MaxLen - 1);
end;

// ______________________________________________________________________________
procedure FsStatusInfo(RemoteDir: PAnsiChar; InfoStartEnd, InfoOperation: Integer); stdcall;
begin
end;

procedure FsStatusInfoW(RemoteDir: PWideChar; InfoStartEnd, InfoOperation: Integer); stdcall;
begin
  if (InfoStartEnd = FS_STATUS_END) and (InfoOperation = FS_STATUS_OP_GET_MULTI) then
    AbortCopy := False;
  // ShowMessageFmt('info op = %d', [InfoOperation]);
end;

// ______________________________________________________________________________
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
  url := CompletePath(RemoteName);
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
  else if (ofn <> '') and IsWebPage(url) then
  begin
    TheIcon := LoadIcon(HInstance, 'ZLINK');
    Result  := FS_ICON_EXTRACTED;
  end;
end;

// ______________________________________________________________________________
procedure DLLEntryPoint(dwReason: DWORD);
begin
  if dwReason = DLL_PROCESS_ATTACH then
  begin
    ProgressObject      := TProgress.Create;
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
  end
  else if dwReason = DLL_PROCESS_DETACH then
  begin
    try
      FreeAndNil(FileList);
      FreeAndNil(ini);
      FreeAndNil(HttpCli);
      FreeAndNil(ProgressObject);
      FreeAndNil(BookMark);
      FreeAndNil(Strings);
      FreeAndNil(History);
    except
    end;
  end;
end;

// ______________________________________________________________________________

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
