unit LocalizationUtils;

interface

uses SysUtils, Classes, Dialogs;

type

  TLocale = class

  	public
      Code: String;
      StringIDs: TStringList;
      Strings: TStringList;
      function GetString(const stringID: String): String;
    published
    	constructor Create(const localeCode: String);

  end;

	TLocalizationUtils = class

		private
      pLoadedLocales: array of TLocale;

  	public
    	CurrentLocale: String;
      function LocaleLoaded(const localeCode: String): Boolean;
    	procedure LoadLocale(const localeCode: String; const localeFolderPath: String);
      function GetString(const stringID: String; const param1: String = ''; const param2: String = ''; const param3: String = ''; const param4: String = ''; const param5: String = ''): String;
      constructor Create();

  end;

implementation


constructor TLocale.Create(const localeCode: String);
begin
	Code := localeCode;
  Strings := TStringList.Create();
  StringIDs := TStringList.Create();
end;


function TLocale.GetString(const stringID: String): String;
var i: Integer;
begin
	result := '';
	for i := 0 to StringIDs.Count - 1 do begin
  	if stringID = StringIDs[i] then begin
    	result := Strings[i];
    end;
  end;
end;


constructor TLocalizationUtils.Create();
begin
	CurrentLocale := 'en';
end;


function TLocalizationUtils.LocaleLoaded(const localeCode: String): Boolean;
var i: Word;
begin
  result := false;

  if Length(pLoadedLocales) > 0 then begin
    for i := 0 to Length(pLoadedLocales) - 1 do begin
      if pLoadedLocales[i].Code = localeCode then begin
        result := true;
        break;
      end;
    end;
  end;
end;


procedure TLocalizationUtils.LoadLocale(const localeCode: String; const localeFolderPath: String);
var localeFile: TextFile;
	line: String;
  locale: TLocale;
  currentString: String;
  equalPos: Integer;
  currentStringID: String;
begin
	if LocaleLoaded(localeCode) then Exit;

  AssignFile(localeFile, localeFolderPath + '/' + localeCode + '.txt');
  {$I-} Reset(localeFile); {$I+}
  if IOResult <> 0 then Exit;

  locale := TLocale.Create(localeCode);

  currentString := '';

  while not Eof(localeFile) do begin
    ReadLn(localeFile, line);
    if line = '' then continue;

    if line[1] = ' ' then begin
			// Append to the current string

    	currentString := currentString + #13#10 + Copy(line, 1, Length(line));
    end else begin

    	// Start a new string

      if currentString <> '' then begin
      	locale.Strings.Add(currentString);
        locale.StringIDs.Add(currentStringID);

        currentStringID := '';
        currentString := '';
      end;

      equalPos := Pos('=', line);

     	if equalPos = 0 then begin
        // Couldn't parse this line - no equal sign found
      	continue;
      end;

      currentStringID := Copy(line, 1, equalPos - 1);
      currentStringID := Trim(currentStringID);

      if currentStringID = '' then begin
      	// Couldn't parse this line - no identifier
      	continue;
      end;

      currentString := Trim(Copy(line, equalPos + 1, Length(line)));

    end;

  end;

  if currentString <> '' then begin
  	locale.Strings.Add(currentString);
    locale.StringIDs.Add(currentStringID);
  end;

  SetLength(pLoadedLocales, Length(pLoadedLocales) + 1);
  pLoadedLocales[Length(pLoadedLocales) - 1] := locale;

  CloseFile(localeFile);
end;


function TLocalizationUtils.GetString(const stringID: String; const param1: String = ''; const param2: String = ''; const param3: String = ''; const param4: String = ''; const param5: String = ''): String;
var i: Integer;
	locale: TLocale;
begin
	result := '';

	for i := 0 to Length(pLoadedLocales) - 1 do begin
  	locale := pLoadedLocales[i];
    if locale.Code <> CurrentLocale then continue;
    result := locale.GetString(stringID);
  end;

  if (result = '') and (CurrentLocale <> 'en') then begin
    for i := 0 to Length(pLoadedLocales) - 1 do begin
      locale := pLoadedLocales[i];
      if locale.Code <> 'en' then continue;
      result := locale.GetString(stringID);
    end;
  end;

  result := StringReplace(result, '%0%', param1, [rfReplaceAll, rfIgnoreCase]);
  result := StringReplace(result, '%1%', param2, [rfReplaceAll, rfIgnoreCase]);
  result := StringReplace(result, '%2%', param3, [rfReplaceAll, rfIgnoreCase]);
  result := StringReplace(result, '%3%', param4, [rfReplaceAll, rfIgnoreCase]);
  result := StringReplace(result, '%4%', param5, [rfReplaceAll, rfIgnoreCase]);
end;



end.
