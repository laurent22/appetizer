unit Main;

interface

uses Dialogs, MainForm, FileSystemUtils, SysUtils, Classes, StringUtils,
	LocalizationUtils, IniFiles, Forms, Windows, Logger, Graphics, ShellAPI,
  User, FileCtrl, CmdLineParam;


const

  DATA_FOLDER_NAME = 'Data';

	SETTING_FOLDER_PATH = DATA_FOLDER_NAME + '\Settings';
  SKIN_FOLDER_PATH = DATA_FOLDER_NAME + '\Skin';
  LOCALE_FOLDER_PATH = DATA_FOLDER_NAME + '\Locales';
  EXCLUSION_FILE_PATH = SETTING_FOLDER_PATH + '\Exclusions.txt';
  INI_FILE_PATH = SETTING_FOLDER_PATH + '\Config.ini';
  USER_SETTINGS_FILE_PATH = SETTING_FOLDER_PATH + '\MiniLaunchBar.xml';



type

  TBarMainPanelStyle = record
    paddingLeft: Integer;
    paddingRight: Integer;
    paddingTop: Integer;
    paddingBottom: Integer;
    paddingH: Integer;
    paddingV: Integer;
  end;

  TBarInnerPanelStyle = record
    paddingLeft: Integer;
    paddingRight: Integer;
    paddingTop: Integer;
    paddingBottom: Integer;
    paddingH: Integer;
    paddingV: Integer;
  end;

  TOptionPanelStyle = record
    paddingLeft: Integer;
    paddingRight: Integer;
    paddingTop: Integer;
    paddingBottom: Integer;
    paddingH: Integer;
    paddingV: Integer;
  end;

  TStyle = record
    barMainPanel: TBarMainPanelStyle;
    barInnerPanel: TBarInnerPanelStyle;
    optionPanel: TOptionPanelStyle;
  end;


	TMain = class   

  private
    
    pUser: TUser;

  public

  	Debug: Boolean;
  	Loc: TLocalizationUtils;
  	style: TStyle;
    skinPath: String;
    mainForm : TMainForm;

    CommandLineArgs: TCmdLineParam;

  	constructor Create();
  	class function Instance: TMain;

    procedure EjectDrive();
    function ErrorMessage(const text: String; const buttons: TMsgDlgButtons = [mbOk]): Integer;
    function ConfirmationMessage(const text: String; const buttons: TMsgDlgButtons = [mbYes, mbNo]): Integer;
   	property User: TUser read pUser;

  end;

implementation


var __singletonInstance: TMain;

class function TMain.Instance(): TMain;
begin
	if __singletonInstance = nil then begin
  	__singletonInstance := TMain.Create();
  end;
  result := __singletonInstance;
end;


function TMain.ErrorMessage;
begin
	result := MessageDlg(text, mtError, buttons, 0);
end;


function TMain.ConfirmationMessage;
begin
	result := MessageDlg(text, mtConfirmation, buttons, 0);
end;


procedure TMain.EjectDrive;
begin
	ShellExecute(0, 'open', PChar('RunDll32.exe'), 'shell32.dll,Control_RunDLL hotplug.dll', nil, SW_SHOWNORMAL) ;
end;


constructor TMain.Create();
var applicationDirectory: String;
begin

	CommandLineArgs := TCmdLineParam.Create();

  Debug := CommandLineArgs.HasArgument('debug');

	if Debug then begin
  	ShowLoggerWindow();
  end;

  applicationDirectory := GetApplicationDirectory();

  ilog('Version: ' + GetFmtFileVersion(ParamStr(0)));
  ilog('Application directory: ' + applicationDirectory);

  // ---------------------------------------------------------------------------
  // Initialize INI default settings
  // ---------------------------------------------------------------------------

  ForceDirectories(SETTING_FOLDER_PATH);
  pUser := TUser.Create(applicationDirectory + '\' + USER_SETTINGS_FILE_PATH);
  
  // ---------------------------------------------------------------------------
  // Initialize localization manager
  // ---------------------------------------------------------------------------

	Loc := TLocalizationUtils.Create();
  Loc.LoadLocale('en', LOCALE_FOLDER_PATH);
  Loc.CurrentLocale := pUser.GetUserSetting('Locale');
  Loc.LoadLocale(Loc.CurrentLocale, LOCALE_FOLDER_PATH);

  ilog('Current locale: ' + Loc.CurrentLocale);

  // ---------------------------------------------------------------------------
  // Initialize style
  // ---------------------------------------------------------------------------

  skinPath := applicationDirectory + '\' + SKIN_FOLDER_PATH + '\Default';

	style.barMainPanel.paddingTop := 8;
  style.barMainPanel.paddingBottom := 8;
  style.barMainPanel.paddingLeft := 8;
  style.barMainPanel.paddingRight := 8;
  style.barMainPanel.paddingH := style.barMainPanel.paddingLeft + style.barMainPanel.paddingRight;
  style.barMainPanel.paddingV := style.barMainPanel.paddingTop + style.barMainPanel.paddingBottom;

	style.barInnerPanel.paddingTop := 4;
  style.barInnerPanel.paddingBottom := 4;
  style.barInnerPanel.paddingLeft := 8;
  style.barInnerPanel.paddingRight := 8;
  style.barInnerPanel.paddingH := style.barInnerPanel.paddingLeft + style.barInnerPanel.paddingRight;
  style.barInnerPanel.paddingV := style.barInnerPanel.paddingTop + style.barInnerPanel.paddingBottom;

	style.optionPanel.paddingTop := 5;
  style.optionPanel.paddingBottom := 5;
  style.optionPanel.paddingLeft := 7;
  style.optionPanel.paddingRight := 7;
  style.optionPanel.paddingH := style.optionPanel.paddingLeft + style.optionPanel.paddingRight;
  style.optionPanel.paddingV := style.optionPanel.paddingTop + style.optionPanel.paddingBottom;

	ilog('Settings folder: ' + SETTING_FOLDER_PATH);
  ilog('Skin folder: ' + skinPath);
  ilog('Locale folder: ' + LOCALE_FOLDER_PATH);
end;


end.
