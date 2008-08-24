program MiniLaunchBar;

{%TogetherDiagram 'ModelSupport_MiniLaunchBar\default.txaPackage'}

uses
  Forms,
  MainForm in 'MainForm.pas' {MainForm},
  WImageButton in 'Components\WImageButton.pas',
  pngextra in 'PNGButton\pngextra.pas',
  pngimage in 'PNGButton\pngimage.pas',
  Imaging in 'Components\Imaging.pas',
  WNineSlicesPanel in 'Components\WNineSlicesPanel.pas',
  WComponent in 'Components\WComponent.pas',
  FileSystemUtils in 'FileSystemUtils.pas',
  WFileIcon in 'Components\WFileIcon.pas',
  Main in 'Main.pas',
  StringUtils in 'StringUtils.pas',
  DebugWindow in 'DebugWindow.pas' {Form1},
  LocalizationUtils in 'LocalizationUtils.pas',
  ControlTest in 'ControlTest.pas' {Form2},
  WContainer in 'Components\WContainer.pas',
  WButtonBase in 'Components\WButtonBase.pas',
  WImage in 'Components\WImage.pas',
  IconPanel in 'IconPanel.pas',
  User in 'User.pas',
  MSXML2_TLB in 'MSXML2_TLB.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, theMainForm);
  Application.Run;
end.
