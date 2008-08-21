unit WComponent;

interface

uses
  Controls, Classes, Messages, Dialogs, SysUtils, Logger, Windows;


type

 	TPNGButtonState = (pbsNormal, pbsDown, pbsOver);

	TWComponent = class(TGraphicControl)

  
	private
    { Private declarations }
    FButtonState: TPNGButtonState;
    fMouseOverControl: Boolean;
    fOnMouseEnter, fOnMouseExit: TNotifyEvent;

    pParentContainer: TObject;

    pTop: Integer;
    pLeft: Integer;

    pID: Integer;
    class var pUniqueID : Integer;
    
  	procedure SetParentContainer(const value: TObject);
    procedure SetButtonState(const Value: TPNGButtonState);

  protected
  	fOnClick: TNotifyEvent;
    fOnParentChange: TNotifyEvent;

    procedure Paint; override;

    // Should be TWContainer but can't do that because
    // of circular reference error, so it has to
    // be type-casted to TWContainer before use.
    function GetLeft(): Integer;
    procedure SetLeft(const value: Integer);
    function GetScreenLoc(): TPoint;


    function GetTop(): Integer;
    procedure SetTop(const value: Integer);

    
    {Clicked}
    procedure Click; override;
    {Mouse pressed}
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    {Mouse entering or leaving}
    procedure MouseEnter(); virtual;
    procedure MouseLeave(); virtual;

    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    
    {Being enabled or disabled}
    procedure CMEnabledChanged(var Message: TMessage);
      message CM_ENABLEDCHANGED;

    //function GetParent(): TWinControl;
    //procedure SetParent(const value: TWinControl);

    //procedure ParentChange();

  public
    Tag: Integer;
    Pushed: Boolean;

    {Returns if the mouse is over the control}
    property ButtonState: TPNGButtonState read FButtonState write SetButtonState;
    property IsMouseOver: Boolean read fMouseOverControl;

    procedure RemoveFromContainer();

    function GetAbsoluteTop(): Integer;
    function GetAbsoluteLeft(): Integer;

    procedure ResetButtonState();

  published
    { Published declarations }
    {Default events}

    procedure UpdateLocation();

    property AbsoluteTop: Integer read GetAbsoluteTop;
    property Top: Integer read GetTop write SetTop;

  	property AbsoluteLeft: Integer read GetAbsoluteLeft;
    property Left: Integer read GetLeft write SetLeft;
    property ScreenLoc: TPoint read GetScreenLoc;

    //property Parent: TWinControl read GetParent write SetParent;

    property ID: Integer read pID;

    property ParentContainer: TObject read pParentContainer write SetParentContainer;

    property OnMouseDown;
    property OnClick: TNotifyEvent read fOnClick write fOnClick;
    property OnParentChange: TNotifyEvent read fOnParentChange write fOnParentChange;
    property OnMouseUp;
    property OnMouseMove;
    property OnDblClick;
    property OnMouseEnter: TNotifyEvent read fOnMouseEnter write fOnMouseEnter;
    property OnMouseExit:  TNotifyEvent read fOnMouseExit  write fOnMouseExit;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    
  end;


implementation


uses WContainer;



constructor TWComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  pUniqueID := pUniqueID + 1;
	pID := pUniqueID;
end;


destructor TWComponent.Destroy();
begin
  inherited Destroy;
end;


procedure TWComponent.ResetButtonState();
begin
	self.buttonState := pbsNormal;
  Pushed := false;
  fMouseOverControl := false;
  Repaint();
end;


//procedure TWComponent.ParentChange();
//begin
//	OnParentChange(self);
//end;


//function TWComponent.GetParent(): TWinControl;
//begin
//	inherited Parent;
//end;
//
//
//procedure TWComponent.SetParent(const value: TWinControl);
//begin
//	inherited Parent := value;
//  Invalidate();
//end;


procedure TWComponent.Paint();
begin
  UpdateLocation();
end;


// Should NOT be called directly. WContainer will call this setter
// and do some additional tasks in AddChild()
procedure TWComponent.SetParentContainer(const value: TObject);
begin
	pParentContainer := value;

  if pParentContainer = nil then begin
  	Parent := nil;
  end else begin
  	Parent := TWinControl(Owner);
  end;

	UpdateLocation();
end;


procedure TWComponent.RemoveFromContainer();
begin
	if ParentContainer = nil then Exit;

  (ParentContainer as TWContainer).RemoveChild(self);
end;


procedure TWComponent.UpdateLocation();
begin
	if ParentContainer <> nil then begin
  	inherited Top := pTop + TWContainer(ParentContainer).AbsoluteTop;
    inherited Left := pLeft + TWContainer(ParentContainer).AbsoluteLeft;
  end else begin
		inherited Top := pTop;
  	inherited Left := pLeft;
	end;
end;


function TWComponent.GetScreenLoc(): TPoint;
var c: TWContainer;
begin
	result.X := Left;
  result.Y := Top;

  c := ParentContainer as TWContainer;

  while (c <> nil) do begin
  	result.X := result.X + c.Left;
    result.Y := result.Y + c.Top;

    c := c.ParentContainer as TWContainer;
  end;

  if Owner <> nil then begin
    result.X := result.X + (Owner as TControl).Left;
    result.Y := result.Y + (Owner as TControl).Top;
  end;


//  if Parent <> nil then begin
//  	result := Parent.ClientToScreen(result);
//  end;
end;


function TWComponent.GetAbsoluteLeft(): Integer;
begin
	result := inherited Left;
end;


procedure TWComponent.SetLeft(const value: Integer);
begin
	pLeft := value;
  Invalidate();
end;


function TWComponent.GetLeft(): Integer;
begin
	result := pLeft;
end;


function TWComponent.GetAbsoluteTop(): Integer;
begin
	result := inherited Top;
end;


procedure TWComponent.SetTop(const value: Integer);
begin
	pTop := value;
  Invalidate();
end;


function TWComponent.GetTop(): Integer;
begin
	result := pTop;
end;


procedure TWComponent.CMEnabledChanged(var Message: TMessage);
begin
  //if Enabled then ButtonState := pbsNormal else ButtonState := pbsDisabled;
end;


procedure TWComponent.SetButtonState(const Value: TPNGButtonState);
begin
  FButtonState := Value;
end;


procedure TWComponent.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;

  if not self.Enabled then Exit;
  if Button <> mbLeft then Exit;

  Pushed := true;
  ButtonState := pbsDown;
end;


procedure TWComponent.Click();
begin
	Pushed := false;

  if IsMouseOver then
  	ButtonState := pbsOver
  else
  	ButtonState := pbsNormal;

  if Assigned(FOnClick) then FOnClick(Self);
end;


procedure TWComponent.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
	Pushed := false;

  if IsMouseOver then
  	ButtonState := pbsOver
  else
  	ButtonState := pbsNormal;

  inherited
end;


procedure TWComponent.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight) and
    (fMouseOverControl = False) then
  begin
    fMouseOverControl := True;
  end;

  inherited;
end;


procedure TWComponent.MouseEnter();
begin
	if Pushed then
  	ButtonState := pbsDown
  else
  	ButtonState := pbsOver;
end;


procedure TWComponent.MouseLeave();
begin
	ButtonState := pbsNormal;
end;


procedure TWComponent.CMMouseEnter(var Message: TMessage);
begin
  if Enabled then
  begin

    if Assigned(fOnMouseEnter) then fOnMouseEnter(Self);
    fMouseOverControl := True;
    MouseEnter();
  end
end;


procedure TWComponent.CMMouseLeave(var Message: TMessage);
begin
  if Enabled then
  begin

    if Assigned(fOnMouseExit) then FOnMouseExit(Self);
    fMouseOverControl := False;
    MouseLeave();
  end
end;


end.
