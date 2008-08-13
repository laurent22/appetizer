unit DebugWindow;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TDebugWindow = class(TForm)
    Memo1: TMemo;
  private
    { Private declarations }
  public
    { Public declarations }
    procedure Log(const s: String);
  end;

var
  form1: TDebugWindow;

implementation

{$R *.dfm}


procedure TDebugWindow.Log(const s: String);
begin
	Memo1.Lines.Add(s);
end;

end.
