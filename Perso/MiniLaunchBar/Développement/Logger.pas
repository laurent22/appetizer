unit Logger;

interface


procedure ShowLoggerWindow();
procedure HideLoggerWindow();
procedure log(const s: String);
procedure elog(const s: String);
procedure wlog(const s: String);
procedure ilog(const s: String);


implementation


uses DebugWindow;

var debugWin: TDebugWindow;


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
begin
	if debugWin <> nil then debugWin.log(s);
end;


procedure elog(const s: String);
begin
	log('[Error] ' + s);
end;


procedure wlog(const s: String);
begin
	log('[Warning] ' + s);
end;


procedure ilog(const s: String);
begin
	log('[Info] ' + s);
end;


end.
