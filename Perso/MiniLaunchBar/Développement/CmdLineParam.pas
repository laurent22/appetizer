unit CmdLineParam;

interface

uses Dialogs, SysUtils;

type TParameter = record
                    Name : string;
                    Value : string;
                  end;

type TCmdLineParam = class(TObject)
  private
    pParamList : array of TParameter;
    procedure MakeParamList();
  public
    constructor Create();
    function GetValue(const paramName : string) : string;
    function HasArgument(const argName : string) : boolean;
    function print() : string;
  end;

implementation

constructor TCmdLineParam.Create();
begin
  self.MakeParamList();
end;

procedure TCmdLineParam.MakeParamList();
var currentParam : ^TParameter;
    i : word;
    arrayIndex : word;
begin
  SetLength(pParamList, 0);
  currentParam := nil;
  i := 1;
  arrayIndex := 1;
  while i <= paramCount() do begin
    if paramStr(i)[1] = '-' then begin
      SetLength(pParamList, arrayIndex);
      pParamList[arrayIndex-1].Name := upperCase(copy(paramStr(i), 2, length(paramStr(i))));
      currentParam := @(pParamList[arrayIndex-1]);
      arrayIndex := arrayIndex + 1;
    end else begin
      currentParam.Value := paramStr(i);
    end;
    i := i + 1;
  end;
end;

function TCmdLineParam.getValue(const paramName : string) : string;
var i : word;
begin
  i := 0;
  result := '';
  while (i < length(pParamList)) and (Result = '') do begin
    if UpperCase(paramName) = pParamList[i].Name then
      Result := pParamList[i].Value;
    i := i + 1;
  end;
end;


function TCmdLineParam.HasArgument;
var i : Integer;
begin
	for i := 0 to Length(pParamList) - 1 do begin
    if UpperCase(pParamList[i].Name) = UpperCase(argName) then begin
      result := true;
      Exit;
    end;
  end;

  result := false;
end;


function TCmdLineParam.print() : string;
var i : word;
begin
  i := 0;
  result := '';
  while i < length(pParamList) do begin
    if i > 0 then result := result + #13#10;
    result := result + 'Name : ' + pParamList[i].Name + #13#10;
    result := result + 'Value : ' + pParamList[i].Value;
    i := i + 1;
  end;
end;

end.
