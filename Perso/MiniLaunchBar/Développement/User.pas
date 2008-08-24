unit User;

interface

uses XMLDoc, Logger, ComObj, MSXML2_TLB, SysUtils, ExtCtrls;



type

	TUser = class(TObject)

  	private

    	pFilePath: String;
    	pXML: IXMLDomDocument;
      pSaveTimer: TTimer;

      procedure ScheduleSave();
      procedure ScheduleSave_Timer(Sender: TObject);

  	public

    	procedure Save();
    	function GetUserSetting(const name: String): String;
      procedure SetUserSetting(const name: String; const value: String);
      constructor Create(const filePath: String);

  end;


const

	DEFAULT_SETTINGS: Array[0..2] of Array[0..1] of String = (
		('PortableAppsPath', 'd:\temp\test portableapp\PortableApps'),
    ('DocumentsPath', 'd:\temp\test portableapp\Documents'),
    ('Locale', 'en')
  );


implementation


constructor TUser.Create(const filePath: String);
var eRoot, eSettings, eSetting: IXMLDOMElement;
	eTextNode: IXMLDOMText;
	i: Integer;
  success: Boolean;
begin
	pFilePath := filePath;
  success := false;

 	pXML := CreateOleObject('Microsoft.XMLDOM') as IXMLDomDocument;
  if FileExists(filePath) then begin
  	success := pXML.Load(filePath);
    if success then begin
    	ilog('Settings loaded successfully from: ' + pFilePath);
    end else begin
    	elog('Coulnd''t load settings from: ' + pFilePath);
    end;
  end;

  if not success then begin
  	ilog('Creating new setting file...');
  	eRoot := pXML.createElement('MiniLaunchBar');
    eRoot.setAttribute('version', '1.0');
    pXML.appendChild(eRoot);
  end;
end;


procedure TUser.ScheduleSave_Timer(Sender: TObject);
begin
  Save();
end;


procedure TUser.ScheduleSave();
begin
	if pSaveTimer = nil then begin
  	pSaveTimer := TTimer.Create(nil);
    pSaveTimer.Interval := 2000;
    pSaveTimer.OnTimer := ScheduleSave_Timer;
  end;

  pSaveTimer.Enabled := true;
end;


procedure TUser.Save();
begin
  ilog('Saving use settings to: ' + pFilePath);
	pSaveTimer.Enabled := false;
  pXML.save(pFilePath);
end;


function TUser.GetUserSetting(const name: String): String;
var eSetting, eSettings: IXMLDOMElement;
	i, j: Integer;
begin
	for i := 0 to pXML.documentElement.childNodes.length - 1 do begin
  	eSettings := pXML.documentElement.childNodes.item[i] as IXMLDOMElement;
  	if eSettings.nodeName = 'Settings' then begin
      for j := 0 to eSettings.childNodes.length - 1 do begin
      	eSetting := eSettings.childNodes.item[j] as IXMLDOMElement;
        if eSetting.getAttribute('name') = name then begin
        	eSetting := eSetting.nextSibling as IXMLDOMElement;
        	result := eSetting.text;
          Exit;
        end;
      end;

      break;
    end;
  end;

  for i := 0 to Length(DEFAULT_SETTINGS) - 1 do begin
  	if DEFAULT_SETTINGS[i][0] = name then begin
    	result := DEFAULT_SETTINGS[i][1];
      Exit;
    end;
  end;

  result := '';
end;


procedure TUser.SetUserSetting(const name: String; const value: String);
var eSetting, eSettings: IXMLDOMElement;
	eOldTextNode, eNewTextNode: IXMLDOMText;
	i, j: Integer;
  done: Boolean;
begin
	done := false;

	for i := 0 to pXML.documentElement.childNodes.length - 1 do begin
  	eSettings := pXML.documentElement.childNodes.item[i] as IXMLDOMElement;

  	if eSettings.nodeName = 'Settings' then begin
    
      for j := 0 to eSettings.childNodes.length - 1 do begin
      	eSetting := eSettings.childNodes.item[j] as IXMLDOMElement;
        ilog(eSetting.getAttribute('name'));
        if eSetting.getAttribute('name') = name then begin
        	if eSetting.text = value then Exit;
          eSetting.text := value;
          done := true;
          break;
        end;
      end;

      if not done then begin
      	eSetting := pXML.createElement('Setting');
        eSetting.setAttribute('name', name);
        eSetting.text := value;
        eSettings.appendChild(eSetting);
      end;

      break;
    end;
  end;

  ScheduleSave();
end;


end.
