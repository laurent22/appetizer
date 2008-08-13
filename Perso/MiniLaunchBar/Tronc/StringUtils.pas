unit StringUtils;

interface

uses Classes;

function IsInStringList(const iStringList: TStringList; const iString: String):Boolean;

implementation


function IsInStringList(const iStringList: TStringList; const iString: String):Boolean;
var i: LongWord;
begin
	result := false;
	for i := 0 to iStringList.Count - 1 do begin
  	if iStringList[i] = iString then begin
    	result := true;
      break;
    end;
  end;
end;


end.
