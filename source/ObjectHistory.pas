unit ObjectHistory;

interface

uses Classes;

type
  THistory = class(TStringList)
    function GoBack : string;
end;

implementation

{ THistory }

function THistory.GoBack: string;
var
  i : Integer;
begin
  i := Count;
  if i = 0 then Result := '\'
  else if i > 1 then begin
    Result := '\' + Self[i - 2];
    Self.Delete(i - 1);
  end;


//    i := History.Count;
//    if i < 2 then
//      s := '\';
//    else begin
//      s := '\' + History[i - 1];
//      History.Delete(i - 1);
//    end;
//    StrPCopy(RemoteName, s);
//    Result := FS_EXEC_SYMLINK;
//    Exit;
end;

end.
 