unit WComponent;

interface

uses
  Controls, Classes, Messages, Dialogs, SysUtils;


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

  	procedure SetParentContainer(const value: TObject);
    procedure SetButtonState(const Value: TPNGButtonState);

  protected
  	fOnClick: TNotifyEvent;

    procedure Paint; override;

    // Should be TWContainer but can't do that because
    // of circular reference error, so it has to
    // be type-casted to TWContainer before use.
    function GetLeft(): Integer;
    procedure SetLeft(const value: Integer);
    function GetAbsoluteLeft(): Integer;

    function GetTop(): Integer;
    procedure SetTop(const value: Integer);
    function GetAbsoluteTop(): Integer;
    
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

  public
    Tag: Integer;
    Pushed: Boolean;

    {Returns if the mouse is over the control}
    property ButtonState: TPNGButtonState read FButtonState write SetButtonState;
    property IsMouseOver: Boolean read fMouseOverControl;


  published
    { Published declarations }
    {Default events}

    procedure UpdateLocation();

    property AbsoluteTop: Integer read GetAbsoluteTop;
    property Top: Integer read GetTop write SetTop;

  	property AbsoluteLeft: Integer read GetAbsoluteLeft;
    property Left: Integer read GetLeft write SetLeft;

    property ParentContainer: TObject read pParentContainer write SetParentContainer;

    property OnMouseDown;
    property OnClick: TNotifyEvent read fOnClick write fOnClick;
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
end;


destructor TWComponent.Destroy();
begin
  inherited Destroy;
end;


procedure TWComponent.Paint;
begin
 //	inherited;
  UpdateLocation();
end;


procedure TWComponent.SetParentContainer(const value: TObject);
begin
	pParentContainer := value;
  Parent := TWinControl(Owner);
	UpdateLocation();
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
