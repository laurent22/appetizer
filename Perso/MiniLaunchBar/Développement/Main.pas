unit Main;

interface

uses Dialogs, MainForm, FileSystemUtils, SysUtils, Classes, StringUtils,
	LocalizationUtils, IniFiles, Forms, Windows, Logger, Graphics, ShellAPI,
  User, FileCtrl, CmdLineParam;


const

  DATA_FOLDER_NAME = 'Data';
  SETTING_FOLDER_NAME = 'Settings';
  SKIN_FOLDER_NAME = 'Skin';
  LOCALES_FOLDER_NAME = 'Locales';
  USER_SETTINGS_FILE_NAME = 'MiniLaunchBar.xml';
  ICONS_FOLDER_NAME = 'Icons';



type

	TFilePaths = record
    DataDirectory: String;
    SettingsDirectory: String;
    SkinDirectory: String;
    LocalesDirectory: String;
    UserSettingsFile: String;
    IconsDirectory: String;
  end;

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

  	Loc: TLocalizationUtils;
  	Style: TStyle;
    MainForm : TMainForm;
    FilePaths: TFilePaths;

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
	applicationDirectory := GetApplicationDirectory();

	FilePaths.DataDirectory := applicationDirectory + '\' + DATA_FOLDER_NAME;
  FilePaths.SettingsDirectory := FilePaths.DataDirectory + '\' + SETTING_FOLDER_NAME;
  FilePaths.SkinDirectory := FilePaths.DataDirectory + '\' + SKIN_FOLDER_NAME + '\Default';
  FilePaths.LocalesDirectory := FilePaths.DataDirectory + '\' + LOCALES_FOLDER_NAME;
  FilePaths.UserSettingsFile := FilePaths.SettingsDirectory + '\' + USER_SETTINGS_FILE_NAME;
  FilePaths.IconsDirectory := FilePaths.SkinDirectory + '\' + ICONS_FOLDER_NAME;

	CommandLineArgs := TCmdLineParam.Create();

	if CommandLineArgs.HasArgument('showDebugWindow') then ShowLoggerWindow();
  if CommandLineArgs.HasArgument('logToFile') then LogToFile(FilePaths.SettingsDirectory + '\Log.txt');

  ilog('Version: ' + GetFmtFileVersion(ParamStr(0)));
  ilog('Application directory: ' + applicationDirectory);

  // ---------------------------------------------------------------------------
  // Initialize INI default settings
  // ---------------------------------------------------------------------------

  ForceDirectories(FilePaths.SettingsDirectory);
  pUser := TUser.Create(FilePaths.UserSettingsFile);
  
  // ---------------------------------------------------------------------------
  // Initialize localization manager
  // ---------------------------------------------------------------------------

	Loc := TLocalizationUtils.Create();
  Loc.LoadLocale('en', FilePaths.LocalesDirectory);
  Loc.CurrentLocale := pUser.GetUserSetting('Locale');
  Loc.LoadLocale(Loc.CurrentLocale, FilePaths.LocalesDirectory);

  ilog('Current locale: ' + Loc.CurrentLocale);

  // ---------------------------------------------------------------------------
  // Initialize style
  // ---------------------------------------------------------------------------

	Style.barMainPanel.paddingTop := 8;
  Style.barMainPanel.paddingBottom := 8;
  Style.barMainPanel.paddingLeft := 8;
  Style.barMainPanel.paddingRight := 8;
  Style.barMainPanel.paddingH := Style.barMainPanel.paddingLeft + Style.barMainPanel.paddingRight;
  Style.barMainPanel.paddingV := Style.barMainPanel.paddingTop + Style.barMainPanel.paddingBottom;

	Style.barInnerPanel.paddingTop := 4;
  Style.barInnerPanel.paddingBottom := 4;
  Style.barInnerPanel.paddingLeft := 8;
  Style.barInnerPanel.paddingRight := 8;
  Style.barInnerPanel.paddingH := Style.barInnerPanel.paddingLeft + Style.barInnerPanel.paddingRight;
  Style.barInnerPanel.paddingV := Style.barInnerPanel.paddingTop + Style.barInnerPanel.paddingBottom;

	Style.optionPanel.paddingTop := 5;
  Style.optionPanel.paddingBottom := 5;
  Style.optionPanel.paddingLeft := 7;
  Style.optionPanel.paddingRight := 7;
  Style.optionPanel.paddingH := Style.optionPanel.paddingLeft + Style.optionPanel.paddingRight;
  Style.optionPanel.paddingV := Style.optionPanel.paddingTop + Style.optionPanel.paddingBottom;

	ilog('Settings folder: ' + FilePaths.SettingsDirectory);
  ilog('Skin folder: ' + FilePaths.SkinDirectory);
  ilog('Locale folder: ' + FilePaths.LocalesDirectory);
end;


end.
