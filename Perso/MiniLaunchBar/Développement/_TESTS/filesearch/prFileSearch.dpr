program prFileSearch;

uses
  Forms,
  uMain in 'uMain.pas' {frMain},
  uFileInfo in 'uFileInfo.pas' {frFileInfo};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrMain, frMain);
  Application.Run;
end.
