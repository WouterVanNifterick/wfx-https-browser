library httpsbrowser;

{$DEFINE NOFORMS}

{$R 'httpsbrowser.res' '..\res\httpsbrowser.rc'}

uses
{  FastMM4,}
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
  ObjectHistory in 'ObjectHistory.pas',
  httpsbrowserPlugin in 'httpsbrowserPlugin.pas',
  WfxPlugin in 'WfxPlugin.pas';

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
  Plugin.PluginNumber := PluginNr;
  Plugin.EnterText    := pRequestProcW;
  Plugin.ProgressBar  := pProgressProcW;
  Plugin.LogProc      := pLogProcW;
  Result       := 0;
  Plugin.Init;
end;


function FsExecuteFile(MainWin: THandle; RemoteName, Verb: PAnsiChar): Integer; stdcall;
begin
  Result := FS_EXEC_ERROR
end;

function FsExecuteFileW(MainWin: THandle; RemoteName, Verb: PWideChar): Integer; stdcall;
begin
  Result := Plugin.ExecuteFile(MainWin, RemoteName, Verb);
end;

function FsFindFirst(Path: PAnsiChar; var FindData: TWin32FindData): THandle; stdcall;
begin
  Result := 0;
end;

function FsFindFirstW(Path: PWideChar; var FindData: tWIN32FINDDATAW): THandle; stdcall;
begin
  Result := Plugin.FindFirstFile(FindData, Path);
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
begin
  Result := Plugin.Delete(RemoteName);
end;

function FsGetFile(RemoteName, LocalName: PAnsiChar; CopyFlags: Integer; RemoteInfo: pRemoteInfo): Integer; stdcall;
begin
  Result := FS_FILE_OK;
end;

function FsGetFileW(RemoteName, LocalName: PWideChar; CopyFlags: Integer; RemoteInfo: pRemoteInfo): Integer; stdcall;
begin
  Result := Plugin.GetFile(RemoteName,LocalName)
end;

procedure FsGetDefRootName(DefRootName: PAnsiChar; MaxLen: Integer); stdcall;
begin
  System.Ansistrings.StrLCopy(DefRootName, PAnsiChar(AnsiString(Plugin.GetPluginName)), MaxLen - 1);
end;

procedure FsStatusInfo(RemoteDir: PAnsiChar; InfoStartEnd, InfoOperation: Integer); stdcall;
begin
end;

procedure FsStatusInfoW(RemoteDir: PWideChar; InfoStartEnd, InfoOperation: Integer); stdcall;
begin
  if (InfoStartEnd = FS_STATUS_END) and (InfoOperation = FS_STATUS_OP_GET_MULTI) then
    Plugin.AbortCopy := False;
end;

function FsExtractCustomIcon(RemoteName: PAnsiChar; ExtractFlags: Integer; var TheIcon: HIcon): Integer; stdcall;
begin
  Result := 0;
end;

function FsExtractCustomIconW(RemoteName: PWideChar; ExtractFlags: Integer; var TheIcon: HIcon): Integer; stdcall;
begin
  Result := Plugin.GetIcon(RemoteName, TheIcon);
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
