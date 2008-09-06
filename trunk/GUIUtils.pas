unit GUIUtils;

interface

uses StdCtrls, Forms, SysUtils;

procedure FitButtonToText(const button: TButton; const buttonForm: TForm; const minWidth: Integer = -1);

implementation


procedure FitButtonToText(const button: TButton; const buttonForm: TForm; const minWidth: Integer = -1);
var l: TLabel;
    newWidth: Integer;
begin
  l := TLabel.Create(buttonForm);
  l.Visible := false;
  l.Parent := buttonForm;
  l.Font.Name := button.Font.Name;
  l.Font.Size := button.Font.Size;
  l.Caption := button.Caption;

  newWidth := l.Width + 20;

  if newWidth < minWidth then newWidth := minWidth;

  button.Width := newWidth;

  FreeAndNil(l);
end;

end.
