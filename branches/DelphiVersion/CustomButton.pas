unit CustomButton;

interface

uses
  SysUtils, Classes, Controls, StdCtrls, Dialogs, Graphics ;

type
  TCustomButton = class(TButton)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    procedure Click; Override;
  published
    { Published declarations }
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Windows Forms', [TCustomButton]);
end;


procedure TCustomButton.Click;
begin
  inherited Click;

  
end;







end.
