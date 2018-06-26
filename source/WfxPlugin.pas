unit WfxPlugin;

interface

uses
  System.SysUtils,
  System.Classes,
  WinApi.Windows,
  ObjectIni,
  ObjectLines;

{ ids for FsGetFile }
const
  FS_FILE_OK                  = 0;
  FS_FILE_EXISTS              = 1;
  FS_FILE_NOTFOUND            = 2;
  FS_FILE_READERROR           = 3;
  FS_FILE_WRITEERROR          = 4;
  FS_FILE_USERABORT           = 5;
  FS_FILE_NOTSUPPORTED        = 6;
  FS_FILE_EXISTSRESUMEALLOWED = 7;

const
  FS_EXEC_OK       = 0;
  FS_EXEC_ERROR    = 1;
  FS_EXEC_YOURSELF = -1;
  FS_EXEC_SYMLINK  = -2;

const
  FS_COPYFLAGS_OVERWRITE            = 1;
  FS_COPYFLAGS_RESUME               = 2;
  FS_COPYFLAGS_MOVE                 = 4;
  FS_COPYFLAGS_EXISTS_SAMECASE      = 8;
  FS_COPYFLAGS_EXISTS_DIFFERENTCASE = 16;

  { flags for tRequestProc }
const
  RT_Other            = 0;
  RT_UserName         = 1;
  RT_Password         = 2;
  RT_Account          = 3;
  RT_UserNameFirewall = 4;
  RT_PasswordFirewall = 5;
  RT_TargetDir        = 6;
  RT_URL              = 7;
  RT_MsgOK            = 8;
  RT_MsgYesNo         = 9;
  RT_MsgOKCancel      = 10;

  { flags for tLogProc }
const
  msgtype_connect           = 1;
  msgtype_disconnect        = 2;
  msgtype_details           = 3;
  msgtype_transfercomplete  = 4;
  msgtype_connectcomplete   = 5;
  msgtype_importanterror    = 6;
  msgtype_operationcomplete = 7;

  { flags for FsStatusInfo }
const
  FS_STATUS_START = 0;
  FS_STATUS_END   = 1;

const
  FS_STATUS_OP_LIST          = 1;
  FS_STATUS_OP_GET_SINGLE    = 2;
  FS_STATUS_OP_GET_MULTI     = 3;
  FS_STATUS_OP_PUT_SINGLE    = 4;
  FS_STATUS_OP_PUT_MULTI     = 5;
  FS_STATUS_OP_RENMOV_SINGLE = 6;
  FS_STATUS_OP_RENMOV_MULTI  = 7;
  FS_STATUS_OP_DELETE        = 8;
  FS_STATUS_OP_ATTRIB        = 9;
  FS_STATUS_OP_MKDIR         = 10;
  FS_STATUS_OP_EXEC          = 11;
  FS_STATUS_OP_CALCSIZE      = 12;
  FS_STATUS_OP_SEARCH        = 13;
  FS_STATUS_OP_SEARCH_TEXT   = 14;

  { Flags for FsExtractCustomIcon }
const
  FS_ICONFLAG_SMALL         = 1;
  FS_ICONFLAG_BACKGROUND    = 2;
  FS_ICON_USEDEFAULT        = 0;
  FS_ICON_EXTRACTED         = 1;
  FS_ICON_EXTRACTED_DESTROY = 2;
  FS_ICON_DELAYED           = 3;

type
  tRemoteInfo = record
    SizeLow, SizeHigh: longint;
    LastWriteTime: TFileTime;
    Attr: longint;
  end;

  pRemoteInfo = ^tRemoteInfo;

type
  tFsDefaultParamStruct = record
    size, PluginInterfaceVersionLow, PluginInterfaceVersionHi: longint;
    DefaultIniName: array [0 .. MAX_PATH - 1] of char;
  end;

  pFsDefaultParamStruct = ^tFsDefaultParamStruct;

  { callback functions }
type
  TProgressProc  = function(PluginNr: Integer; SourceName, TargetName: pAnsichar; PercentDone: Integer) : Integer; stdcall;
  TProgressProcW = function(PluginNr: Integer; SourceName, TargetName: pwidechar; PercentDone: Integer): Integer; stdcall;
  TLogProc       = procedure(PluginNr, MsgType: Integer; LogString: pAnsichar); stdcall;
  TLogProcW      = procedure(PluginNr, MsgType: Integer; LogString: pwidechar); stdcall;
  TRequestProc   = function(PluginNr, RequestType: Integer; CustomTitle, CustomText, ReturnedText: pAnsichar; maxlen: Integer) : bool; stdcall;
  TRequestProcW  = function(PluginNr, RequestType: Integer; CustomTitle, CustomText, ReturnedText: pwidechar; maxlen: Integer): bool; stdcall;

type
  TWFXPlugin = class abstract
  public
    EnterText   : TRequestProcW;
    PluginNumber: Integer;
    ProgressBar : TProgressProcW;
    LogProc     : TLogProcW;
    Strings     : TLines;
    ini         : TIni;

    PathExe     : string;
    LocalFile   : string;
    RemoteFile  : string;
    FileDate    : _FILETIME;
    FLIndex     : Integer;
    FileList    : TStringList;
    PreviousPath: string;
    CurrentPath : string;
    constructor Create; virtual;
    class function GetDLLPathName:string;
    function ExecuteFile(MainWin: THandle; RemoteName, Verb: PWideChar): Integer; virtual; abstract;
    function FindFirstFile(var FindData: _WIN32_FIND_DATAW; Path: PWideChar): Cardinal; virtual; abstract;
    procedure TCShowMessage(const Title, Text: string);
    function Input(const Title, Question: string; var Text: string; InputType: Integer = RT_Other): Boolean;
  end;

implementation


{ TWFXPlugin }

constructor TWFXPlugin.Create;
begin
  PathExe             := ExtractFilePath(GetDllPathName);
  FileList            := TStringList.Create;
  FileList.Sorted     := True;
  FileList.Duplicates := dupIgnore;
  ini                 := TIni.Create(PathExe + 'httpbrowser.ini');
  Strings             := TLines.Create;
  Strings.SetLanguage(PathExe + ini.GetS('Language') + '.lng');
end;

class function TWFXPlugin.GetDLLPathName: string;
begin
  Result := GetModuleName(HInstance);
end;

procedure TWFXPlugin.TCShowMessage(const Title, Text: string);
var
  a: array [0 .. 255] of char;
begin
  EnterText(PluginNumber, RT_MsgOK, PChar(Title), PChar(Text), @a, 256);
end;

// ______________________________________________________________________________
function TWFXPlugin.Input(const Title, Question: string; var Text: string; InputType: Integer = RT_Other): Boolean;
var
  a: array [0 .. 255] of char;
begin
  StrCopy(@a, PChar(Text));
  Result := EnterText(PluginNumber, InputType, PChar(Title), PChar(Question), @a, 256);
  Text   := a;
end;


end.
