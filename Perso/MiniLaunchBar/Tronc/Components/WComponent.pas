unit WComponent;

interface

uses
  Controls, Classes, Messages, Dialogs, SysUtils;


type

 	TPNGButtonState = (pbsNormal, pbsDown, pbsDisabled);

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

    // Should be TWContainer but can't do that because
    // of Delphi's stupid circular reference error, so it has to
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
  UpdateLocation();
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
  UpdateLocation();
end;


function TWComponent.GetTop(): Integer;
begin
	result := pTop;
end;


 {Being enabled or disabled}
procedure TWComponent.CMEnabledChanged(var Message: TMessage);
begin
  //if not Enabled then MakeImageHalfTransparent(fImageNormal, fImageDisabled);
  if Enabled then ButtonState := pbsNormal else ButtonState := pbsDisabled
end;


{Changing the button state property}
procedure TWComponent.SetButtonState(const Value: TPNGButtonState);
begin
  FButtonState := Value;
end;


{Mouse pressed}
procedure TWComponent.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  {Changes the state and repaints}
  if (ButtonState = pbsNormal) and (Button = mbLeft) then
    ButtonState := pbsDown;
  {Calls ancestor}
  inherited
end;

{Being clicked}
procedure TWComponent.Click();
begin
  if ButtonState = pbsDown then ButtonState := pbsNormal;

  if Assigned(FOnClick) then FOnClick(Self);

  //ShowMessage(IntToStr(Tag));
  //inherited Click;
end;

{Mouse released}
procedure TWComponent.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  {Changes the state and repaints}
  if ButtonState = pbsDown then ButtonState := pbsNormal;
  {Calls ancestor}
  inherited
end;

{Mouse moving over the control}
procedure TWComponent.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  {In case cursor is over the button}
  if (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight) and
    (fMouseOverControl = False) and (ButtonState <> pbsDown)  then
  begin
    fMouseOverControl := True;
  end;

  {Calls ancestor}
  inherited;

end;


procedure TWComponent.MouseEnter();
begin
end;


procedure TWComponent.MouseLeave();
begin
end;


{Mouse is now over the control}
procedure TWComponent.CMMouseEnter(var Message: TMessage);
begin
  if Enabled then
  begin

    if Assigned(fOnMouseEnter) then fOnMouseEnter(Self);
    fMouseOverControl := True;
    MouseEnter();
  end
end;

{Mouse has left the control}
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
