{*------------------------------------------------------------------------------
  Some utilities to easily manage the command line arguments.
  The arguments must be in this format:

  -name [value]

  For example, if the application is launched this way:

  App.exe -output "myfile.txt" -debug

  Use this code to handle the arguments:

  var cmdLine := TCmdLineParam.Create();
  ShowMessage('Output file is ' + cmdLine.getValue('output'));
  if cmdLine.HasArgument('debug') then showMessage('This is the debug version')

  @Author Laurent Cozic
-------------------------------------------------------------------------------}

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
  end;

implementation


constructor TCmdLineParam.Create();
begin
  MakeParamList();
end;


{*------------------------------------------------------------------------------
  Build the list of name / value pairs for the current application
-------------------------------------------------------------------------------}
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


{*------------------------------------------------------------------------------
  Get the value of an argument or an empty string if none exist. Use
  HasArgument() to check if an argument exists.
  @param paramName Name of argument
  @return Value of argument
-------------------------------------------------------------------------------}
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


{*------------------------------------------------------------------------------
  Returns TRUE if the given argument exists
  @param paramName Name of argument
  @param argName Name of argument
-------------------------------------------------------------------------------}
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


end.
