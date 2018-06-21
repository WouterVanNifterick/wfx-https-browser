unit UnitStrings;

interface

uses Classes, Windows, SysUtils, ObjectLines, ObjectIni;

{$DEFINE NOFORMS}

{ ids for FsGetFile }
const FS_FILE_OK=0;
      FS_FILE_EXISTS=1;
      FS_FILE_NOTFOUND=2;
      FS_FILE_READERROR=3;
      FS_FILE_WRITEERROR=4;
      FS_FILE_USERABORT=5;
      FS_FILE_NOTSUPPORTED=6;
      FS_FILE_EXISTSRESUMEALLOWED=7;

      FS_EXEC_OK=0;
      FS_EXEC_ERROR=1;
      FS_EXEC_YOURSELF=-1;
      FS_EXEC_SYMLINK=-2;

      FS_COPYFLAGS_OVERWRITE=1;
      FS_COPYFLAGS_RESUME=2;

      FS_COPYFLAGS_MOVE=4;
      FS_COPYFLAGS_EXISTS_SAMECASE=8;
      FS_COPYFLAGS_EXISTS_DIFFERENTCASE=16;

{ flags for tRequestProc }
const
  RT_Other=0;
  RT_UserName=1;
  RT_Password=2;
  RT_Account=3;
  RT_UserNameFirewall=4;
  RT_PasswordFirewall=5;
  RT_TargetDir=6;
  RT_URL=7;
  RT_MsgOK=8;
  RT_MsgYesNo=9;
  RT_MsgOKCancel=10;

{ flags for tLogProc }
const msgtype_connect=1;
      msgtype_disconnect=2;
      msgtype_details=3;
      msgtype_transfercomplete=4;
      msgtype_connectcomplete=5;
      msgtype_importanterror=6;
      msgtype_operationcomplete=7;

{ flags for FsStatusInfo }
const FS_STATUS_START=0;
      FS_STATUS_END=1;

      FS_STATUS_OP_LIST=1;
      FS_STATUS_OP_GET_SINGLE=2;
      FS_STATUS_OP_GET_MULTI=3;
      FS_STATUS_OP_PUT_SINGLE=4;
      FS_STATUS_OP_PUT_MULTI=5;
      FS_STATUS_OP_RENMOV_SINGLE=6;
      FS_STATUS_OP_RENMOV_MULTI=7;
      FS_STATUS_OP_DELETE=8;
      FS_STATUS_OP_ATTRIB=9;
      FS_STATUS_OP_MKDIR=10;
      FS_STATUS_OP_EXEC=11;
      FS_STATUS_OP_CALCSIZE=12;
      FS_STATUS_OP_SEARCH=13;
      FS_STATUS_OP_SEARCH_TEXT=14;

{Flags for FsExtractCustomIcon}
const FS_ICONFLAG_SMALL=1;
      FS_ICONFLAG_BACKGROUND=2;
      FS_ICON_USEDEFAULT=0;
      FS_ICON_EXTRACTED=1;
      FS_ICON_EXTRACTED_DESTROY=2;
      FS_ICON_DELAYED=3;


type
  tRemoteInfo=record
    SizeLow,SizeHigh:longint;
    LastWriteTime:TFileTime;
    Attr:longint;
  end;
  pRemoteInfo=^tRemoteInfo;

type
  tFsDefaultParamStruct=record
    size,
    PluginInterfaceVersionLow,
    PluginInterfaceVersionHi:longint;
    DefaultIniName:array[0..MAX_PATH-1] of char;
  end;
  pFsDefaultParamStruct=^tFsDefaultParamStruct;

{ callback functions }
type
  TProgressProc=function(PluginNr:integer;SourceName,
    TargetName:pAnsichar;PercentDone:integer):integer; stdcall;
  TProgressProcW=function(PluginNr:integer;SourceName,
    TargetName:pwidechar;PercentDone:integer):integer; stdcall;
  TLogProc=procedure(PluginNr,MsgType:integer;LogString:pAnsichar); stdcall;
  TLogProcW=procedure(PluginNr,MsgType:integer;LogString:pwidechar); stdcall;
  TRequestProc=function(PluginNr,RequestType:integer;CustomTitle,CustomText,
    ReturnedText:pAnsichar;maxlen:integer):bool; stdcall;
  TRequestProcW=function(PluginNr,RequestType:integer;CustomTitle,CustomText,
    ReturnedText:pwidechar;maxlen:integer):bool; stdcall;

var
  EnterText : TRequestProcW;
  PluginNumber : Integer;
  ProgressBar : TProgressProcW;
  LogProc : TLogProcW;
  Strings : TLines;
  ini : TIni;

procedure split(var List : TStringList; AString: string; Delimiter: string = ';');
function merge(AList: TStrings; Delimiter: string = ';'): string;
function Replace(Source: string; SubStr: string; ReplaceStr: string): string;
function TrimSlashes(s : string; all : Boolean = False) : string;
function DeleteUntilLast(Text, UntilLastStr : string) : string;
function GetDllPathName: string;
procedure TCShowMessage(Title, Text : string);
function Input(Title, Question : string; var Text : string; InputType : Integer = RT_OTHER) : Boolean;


implementation

procedure TCShowMessage(Title, Text : string);
var
  a : array[0..255] of Char;
begin
  EnterText(PluginNumber, RT_MsgOK, PChar(Title), PChar(Text), @a, 256);
end;
//______________________________________________________________________________
function Input(Title, Question : string; var Text : string; InputType : Integer = RT_OTHER) : Boolean;
var
  a : array[0..255] of Char;
begin
  StrCopy(@a, PChar(Text));
  Result := EnterText(PluginNumber, InputType, PChar(Title), PChar(Question), @a, 256);
  Text := a;
end;

procedure split(var List : TStringList; AString: string; Delimiter: string = ';');
var
  p: Integer;
begin
  List.Clear;
  p := pos(Delimiter, AString);
  while p > 0 do begin
    List.Add(Copy(AString, 1, pred(p)));
    Delete(AString, 1, p);
    p := pos(Delimiter, AString);
  end;
  List.Add(AString);
end;
//______________________________________________________________________________
function merge(AList: TStrings; Delimiter: string = ';'): string;
var
  i: Integer;
begin
  if AList.Count = 0 then begin
    Result := '';
    Exit;
  end;
  Result := AList[0];
  i := 1;
  while i < AList.Count do begin
    Result := Result + Delimiter + AList[i];
    Inc(i);
  end;
end;
//______________________________________________________________________________
function Replace(Source: string; SubStr: string; ReplaceStr: string): string;
var
  T: string;
  p: Integer;
  l: Integer;
  s: string;
begin
  Result := '';
  T := Source;
  s := LowerCase(subStr);
  l := length(s);
  p := Pos(s, lowerCase(T));
  while p > 0 do begin
    Result := Result + copy(T, 1, pred(p)) + ReplaceStr;
    Delete(T, 1, Pred(p + l));
    p := Pos(s, LowerCase(T));
  end;
  Result := Result + T;
end;
//______________________________________________________________________________
function TrimSlashes(s : string; all : Boolean = False) : string;
begin
  while (s <> '') and (s[1] in ['/', '\']) do Delete(s, 1, 1);
  if all then
    while (s <> '') and (s[Length(s)] in ['/', '\']) do Delete(s, Length(s), 1)
  else
    while (s <> '') and (s[Length(s)] = '\') do Delete(s, Length(s), 1);
  Result := s;
end;
//______________________________________________________________________________
function DeleteUntilLast(Text, UntilLastStr : string) : string;
var
  i : Integer;
begin
  Result := Text;
  i := Pos(UntilLastStr, Result);
  while i > 0 do begin
    Delete(Result, 1, i);
    i := Pos(UntilLastStr, Result);
  end;
end;
//______________________________________________________________________________
function GetDllPathName: String;
begin
  Result := GetModuleName(HInstance);
end;

end.
 