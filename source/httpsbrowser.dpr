library httpsbrowser;

{$DEFINE NOFORMS}

{$R 'httpsbrowser.res' '..\res\httpsbrowser.rc'}

uses
  System.SysUtils,
  Vcl.Dialogs,
  System.Classes,
  WinApi.Windows,
  WinApi.ShellApi,
  System.AnsiStrings,
  ObjectIni in 'ObjectIni.pas',
  ObjectLines in 'ObjectLines.pas',
  UnitStrings in 'UnitStrings.pas',
  ObjectBookmark in 'ObjectBookmark.pas',
  ObjectTimer in 'ObjectTimer.pas',
  ObjectHistory in 'ObjectHistory.pas',
  httpsbrowserPlugin in 'httpsbrowserPlugin.pas';

{$E wfx}
{$IFDEF WIN64}
{$E w64}
{$IFNDEF VER230}
{$E wfx64}
{$ENDIF}
{$ENDIF}

var
  Plugin: THTTPSBrowserPlugin;



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
    FsExecuteFileW(0, Path, '');
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
    DLL_PROCESS_ATTACH : Plugin := THTTPSBrowserPlugin.Create;
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
