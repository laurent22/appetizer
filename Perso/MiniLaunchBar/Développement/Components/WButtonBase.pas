unit WButtonBase;

interface

uses WComponent, Controls, Classes;

type

	TWButtonBase = class(TWComponent)

  	protected

      procedure Click; override;
      procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
      procedure MouseEnter(); override;
      procedure MouseLeave(); override;
      procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override; 
      procedure MouseMove(Shift: TShiftState; X, Y: Integer);
  end;


implementation


procedure TWButtonBase.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  Repaint();
end;


procedure TWButtonBase.Click();
begin
	inherited;
  Repaint();
end;


procedure TWButtonBase.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited
  Repaint();
end;


procedure TWButtonBase.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;

end;


procedure TWButtonBase.MouseEnter();
begin
	inherited;
  Repaint();
end;


procedure TWButtonBase.MouseLeave();
begin
	inherited;
  Repaint();
end;

end.
