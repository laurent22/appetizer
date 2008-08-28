unit Logger;

interface


uses SysUtils, Windows;


procedure ShowLoggerWindow();
procedure HideLoggerWindow();
procedure LogToFile(const filePath:String);
procedure log(const s: String);
procedure elog(const s: String);
procedure wlog(const s: String);
procedure ilog(const s: String);


implementation


uses DebugWindow;

var debugWin: TDebugWindow;
	logFilePath: String = '';


procedure LogToFile(const filePath:String);
begin
	logFilePath := filePath;
  DeleteFile(PAnsiChar(logFilePath));
end;


procedure ShowLoggerWindow();
begin
	if debugWin = nil then debugWin := TDebugWindow.Create(nil);
  debugWin.Left := 20;
  debugWin.Top := 20;
  debugWin.Visible := true;
end;


procedure HideLoggerWindow();
begin
	if debugWin = nil then Exit;
  debugWin.Visible := false;
end;


procedure log(const s: String);
var f: TextFile;
begin
	if debugWin <> nil then debugWin.log(s);

  if logFilePath <> '' then begin
    AssignFile(f, logFilePath);
    try
    	if not FileExists(logFilePath) then Rewrite(f);
    	Append(f);
      WriteLn(f, s);
    finally
    	CloseFile(f);
    end;
  end;
end;


procedure elog(const s: String);
begin
  if (debugWin = nil) and (logFilePath = '') then Exit;
	log('[Error ' + TimeToStr(Now) + '] ' + s);
end;


procedure wlog(const s: String);
begin
	if (debugWin = nil) and (logFilePath = '') then Exit;
	log('[Warning ' + TimeToStr(Now) + '] ' + s);
end;


procedure ilog(const s: String);
begin
	if (debugWin = nil) and (logFilePath = '') then Exit;
	log('[Info ' + TimeToStr(Now) + '] ' + s);
end;


end.
