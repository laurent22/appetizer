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

    pMinWidth: Integer;
    pMinHeight: Integer;
    pMaxWidth: Integer;
    pMaxHeight: Integer;

    pID: Integer;
    class var pUniqueID : Integer;
    
  	procedure SetParentContainer(const value: TObject);
    procedure SetButtonState(const Value: TPNGButtonState);

    function GetAbsoluteTop(): Integer;
    function GetAbsoluteLeft(): Integer;

    function GetScreenLeft(): Integer;
    function GetScreenTop(): Integer;
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);


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

    procedure Click; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseEnter(); virtual;
    procedure MouseLeave(); virtual;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;

    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;

  public
  
    Tag: Integer;
    Pushed: Boolean;

    property ButtonState: TPNGButtonState read FButtonState write SetButtonState;
    property IsMouseOver: Boolean read fMouseOverControl;

    procedure RemoveFromContainer();
    procedure ResetButtonState();  
    procedure UpdateLocation();

    property AbsoluteTop: Integer read GetAbsoluteTop;
    property Top: Integer read GetTop write SetTop;

  	property AbsoluteLeft: Integer read GetAbsoluteLeft;
    property Left: Integer read GetLeft write SetLeft;
    property ScreenLoc: TPoint read GetScreenLoc;

    property ScreenLeft: Integer read GetScreenLeft;
    property ScreenTop: Integer read GetScreenTop;

    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;

    property MaxWidth: Integer read pMaxWidth write pMaxWidth;
    property MaxHeight: Integer read pMaxHeight write pMaxHeight;
    property MinWidth: Integer read pMinWidth write pMinWidth;
    property MinHeight: Integer read pMinHeight write pMinHeight;

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
    property OnResize;

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

  pMinWidth := -999;
  pMinHeight := -999;
  pMaxWidth := -999;
  pMaxHeight := -999;
end;


function TWComponent.GetHeight: Integer;
begin
 result := 	inherited Height;
end;

destructor TWComponent.Destroy();
begin
	RemoveFromContainer();
  inherited Destroy;
end;


procedure TWComponent.ResetButtonState();
begin
	self.buttonState := pbsNormal;
  Pushed := false;
  fMouseOverControl := false;
  Repaint();
end;



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
  	Parent := TWinControl(Owner);//TWinControl(Owner);
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


function TWComponent.GetScreenLeft: Integer;
begin
	result := ScreenLoc.X;
end;

function TWComponent.GetScreenTop: Integer;
begin
	result := ScreenLoc.Y;
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


procedure TWComponent.SetWidth(const Value: Integer);
var w: Integer;
begin
	w := value;
	if pMinWidth > 0 then begin
  	if value < pMinWidth then w := pMinWidth;
  end;
  if pMaxWidth > 0 then begin
  	if value > pMaxWidth then w := pMaxWidth;
  end;
  inherited Width := w;
end;

function TWComponent.GetTop(): Integer;
begin
	result := pTop;
end;


function TWComponent.GetWidth: Integer;
begin
	result := inherited Width;
end;

procedure TWComponent.CMEnabledChanged(var Message: TMessage);
begin
  //if Enabled then ButtonState := pbsNormal else ButtonState := pbsDisabled;
end;


procedure TWComponent.SetButtonState(const Value: TPNGButtonState);
begin
  FButtonState := Value;
end;


procedure TWComponent.SetHeight(const Value: Integer);
var h: Integer;
begin
	h := value;
	if pMinHeight > 0 then begin
  	if value < pMinHeight then h := pMinHeight;
  end;
  if pMaxHeight > 0 then begin
  	if value > pMaxHeight then h := pMaxHeight;
  end;
  inherited Height := h;
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
	inherited;

	Pushed := false;

  if IsMouseOver then
  	ButtonState := pbsOver
  else
  	ButtonState := pbsNormal;
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
