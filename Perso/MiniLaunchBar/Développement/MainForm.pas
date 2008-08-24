unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, ShellAPI,
  Dialogs, StdCtrls, ExtCtrls, PNGExtra, PNGImage, WImageButton, Imaging, WNineSlicesPanel,
  FileSystemUtils, WFileIcon, Menus, ControlTest, WComponent, WImage, MathUtils,
  Logger, IconPanel, xmldom, XMLIntf, msxmldom, XMLDoc;

type

  TDragData = record
    dragging: Boolean;
    startMousePos: TPoint;
    startWindowPos: TPoint;
  end;





  TOptionButtonDatum = class public
    ID: Integer;
    Name: String;
    IconFilePath: String;
    Separator: Boolean;
		SeparatorObject: TWNineSlicesPanel;
  end;

  
  TMainForm = class(TForm)
    iconPopupMenu: TPopupMenu;
    cddd1: TMenuItem;
    N1: TMenuItem;
    Properties1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure barBackground_down(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure barBackground_up(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure barBackground_move(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure UpdateLayout();
    procedure arrowButton_click(sender: TObject);
    procedure OpenOptionPanel();
    procedure OpenCloseOptionPanel(const iOpen: Boolean);
    procedure optionPanelAnimTimer_timer(sender: TObject);
    procedure SetOptionPanelWidth(const iWidth: Integer);
    procedure UpdateFormMask();
    procedure ToggleOptionPanel();
    procedure UpdateOptionPanel();
    procedure optionButton_Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

    private
      { Private declarations }

      optionPanelOpen: Boolean;
      optionPanelAnimTimer: TTimer;
      optionPanelOpenWidth: Integer;
      optionPanelCloseWidth: Integer;
      optionPanelAnimationStartTime: Int64;
      optionPanelAnimationDuration: Int64;
      optionPanelCurrentWidth: Integer;

      optionButtons: Array of TWImageButton;
      optionButtonGap: Byte;
      optionButtonData: Array of TOptionButtonDatum;

      function OptionPanelTotalWidth():Word;
      function AddOptionButtonData():TOptionButtonDatum;
      function GetButtonDataByID(ID: Integer): TOptionButtonDatum;
      procedure UpdateOptionButtonsLayout(const cornerX, cornerY: Integer);
      procedure CalculateOptionPanelOpenWidth();

    public
      { Public declarations }
      
  end;

var
  theMainForm: TMainForm;
  barBackground: TWNineSlicesPanel;
  barInnerPanel: TIconPanel;
  optionPanel: TWNineSlicesPanel;
  arrowButton: TWImageButton;
  button: TWImageButton;
  windowDragData: TDragData;
  testwin: TForm2;


implementation

{$R *.dfm}

uses Main;


procedure TMainForm.barBackground_down(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
	mouse: TMouse;
begin
	ilog('Form dragging started');

	mouse := TMouse.Create();
	windowDragData.dragging := true;
	windowDragData.startMousePos.X := mouse.CursorPos.X;
  windowDragData.startMousePos.Y := mouse.CursorPos.Y;
  windowDragData.startWindowPos.X := Left;
  windowDragData.startWindowPos.Y := Top;
end;


procedure TMainForm.barBackground_up(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
	ilog('Dragging stopped');
  windowDragData.dragging := false;
end;


procedure TMainForm.barBackground_move(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
	mouse: TMouse;
begin
	if windowDragData.dragging then begin
  	ilog('Dragging...');
    mouse := TMouse.Create();
    Left := windowDragData.startWindowPos.X + (mouse.CursorPos.X - windowDragData.startMousePos.X);
    Top := windowDragData.startWindowPos.Y + (mouse.CursorPos.Y - windowDragData.startMousePos.Y);
  end;
end;


procedure TMainForm.UpdateFormMask();
var bmp: TBitmap;
	 region: THandle;
   rect: TRect;
begin
	ilog('Updating form mask...');

  Width := barBackground.Width + OptionPanelTotalWidth;
  Height := barBackground.Height;

  bmp := TBitmap.Create();
  try
  	bmp.Width := Width;
    bmp.Height := Height;

    rect.Top := 0;
    rect.Left := 0;
    rect.Bottom := bmp.Height;
  	rect.Right := bmp.Width;

    bmp.Canvas.Brush := TBrush.Create();
    bmp.Canvas.Brush.Color := RGB(255, 0, 255);
    bmp.Canvas.FillRect(rect);

    DrawNineSlices(bmp.Canvas, TMain.instance.skinPath + '\BarBackgroundRegion', optionPanelOpenWidth - optionPanelCurrentWidth, 0, bmp.Width - optionPanelOpenWidth + optionPanelCurrentWidth, bmp.Height);

    region := CreateRegion(Bmp);
    SetWindowRGN(Handle, region, True);
  finally
    bmp.Free;
  end;
end;


function TMainForm.OptionPanelTotalWidth():Word;
begin
	result := optionPanelOpenWidth + arrowButton.Width;
end;


procedure TMainForm.UpdateLayout();
begin
	ilog('Updating layout...');

  barInnerPanel.UpdateLayout();

  barBackground.Width := barInnerPanel.Width + TMain.instance.style.barMainPanel.paddingH;
  barBackground.Height := barInnerPanel.Height + TMain.instance.style.barMainPanel.paddingV;

  barBackground.Left := OptionPanelTotalWidth;

  barInnerPanel.Left := TMain.instance.style.barMainPanel.paddingLeft;
  barInnerPanel.Top := TMain.instance.style.barMainPanel.paddingTop;

  optionPanel.Height := barBackground.Height;

  UpdateOptionPanel();
  UpdateFormMask();
end;


function TMainForm.GetButtonDataByID(ID: Integer): TOptionButtonDatum;
var i: Byte;
begin
	result := nil;
  for i := 0 to Length(optionButtonData) - 1 do begin
  	if optionButtonData[i].ID = ID then begin
    	result := optionButtonData[i];
      break;
    end;
  end;
end;


procedure TMainForm.optionButton_Click(Sender: TObject);
var d: TOptionButtonDatum;
  button: TWImageButton;
begin
	button := sender as TWImageButton;

  d := GetButtonDataByID(button.Tag);

  if d = nil then Exit;

  ilog('Option button click: ' + d.Name);

  if d.Name = 'Close' then begin
  	Close();
  end else begin
  if d.Name = 'Config' then begin

  end else begin
  if d.Name = 'Help' then begin

  end else begin
  if d.Name = 'Encrypt' then begin

  end; end; end; end;
  
end;


procedure TMainForm.SetOptionPanelWidth(const iWidth: Integer);
begin
	if iWidth = optionPanelCurrentWidth then Exit;

	optionPanelCurrentWidth := iWidth;

  UpdateOptionPanel();
  UpdateFormMask();
end;


procedure TMainForm.CalculateOptionPanelOpenWidth();
var buttonX: Integer;
	buttonData: TOptionButtonDatum;
	button: TWImageButton;
  i: Byte;
  vButtonCount: Integer;
begin
	buttonX := 0;
  vButtonCount := 0;

  for i := 0 to Length(optionButtons) - 1 do begin
    button := optionButtons[i];

    buttonData := GetButtonDataByID(button.Tag);
    if buttonData = nil then continue;

    if buttonData.Separator then begin

      buttonX := buttonX + optionButtons[i - 1].Width + optionButtonGap * 4;
      vButtonCount := 0;

    end else begin

    	vButtonCount := vButtonCount + 1;

      if vButtonCount > 3  then begin
        buttonX := buttonX + button.Width + optionButtonGap;
        vButtonCount := 0;
      end;

    end;

    optionPanelOpenWidth := buttonX + button.Width;

  end;

  optionPanelOpenWidth := optionPanelOpenWidth + TMain.Instance.Style.OptionPanel.PaddingH;

end;


procedure TMainForm.UpdateOptionButtonsLayout(const cornerX, cornerY: Integer);
var buttonX, buttonY: Integer;
	buttonData: TOptionButtonDatum;
	button: TWImageButton;
  i: Byte;
begin
  buttonX := cornerX;
  buttonY := cornerY;

  for i := 0 to Length(optionButtons) - 1 do begin
    button := optionButtons[i];

    buttonData := GetButtonDataByID(button.Tag);
    if buttonData = nil then continue;

    if buttonData.Separator then begin

    	buttonY := optionPanel.Top + TMain.Instance.Style.OptionPanel.PaddingTop;

    	buttonData.SeparatorObject.Left := buttonX + optionButtons[i - 1].Width;
      buttonData.SeparatorObject.Top := buttonY;
      buttonData.SeparatorObject.Width := 4;
      buttonData.SeparatorObject.Height := optionPanel.Height - TMain.Instance.style.optionPanel.paddingV;


      buttonX := buttonX + optionButtons[i - 1].Width + optionButtonGap * 4;

      buttonData.SeparatorObject.Left := Round(buttonData.SeparatorObject.Left + (buttonX - buttonData.SeparatorObject.Left) / 2) - 1;
      buttonData.SeparatorObject.Visible := true;

    end else begin

      button.Visible := true;
      button.Left := buttonX;
      button.Top := buttonY;

      if button.Top + button.Height >= optionPanel.Height then begin
        buttonY := optionPanel.Top + TMain.Instance.Style.OptionPanel.PaddingTop;
        buttonX := buttonX + button.Width + optionButtonGap;
        button.Left := buttonX;
        button.Top := buttonY;
      end;

      buttonY := buttonY + optionButtons[i].Height + optionButtonGap;

    end;

  end;

end;


procedure TMainForm.UpdateOptionPanel();
begin
	ilog('Updating option panel');

	if optionPanel.Visible then begin

  	optionPanel.Left := optionPanelOpenWidth - optionPanelCurrentWidth + arrowButton.Width;
    arrowButton.Left := optionPanel.Left - arrowButton.Width;

  	UpdateOptionButtonsLayout(
    	TMain.Instance.Style.OptionPanel.PaddingLeft,
      TMain.Instance.Style.OptionPanel.PaddingTop
    );

  end else begin
		arrowButton.Left := optionPanelOpenWidth - optionPanelCurrentWidth;
  end;
end;


procedure TMainForm.optionPanelAnimTimer_timer(sender: TObject);
var percent: Real;
	i: Byte;
begin
	percent := (GetTickCount() - optionPanelAnimationStartTime) / optionPanelAnimationDuration;

  if percent >= 1.0 then begin
  	ilog('Panel animation complete');

  	percent := 1.0;
    optionPanelAnimTimer.Enabled := false;
    if not optionPanelOpen then optionPanel.Visible := false;
    if optionPanelOpen then begin
    	arrowButton.IconImagePath := TMain.instance.skinPath + '\ArrowButtonIconRight.png';
    end else begin
     	arrowButton.IconImagePath := TMain.instance.skinPath + '\ArrowButtonIconLeft.png';

      for i := 0 to Length(optionButtons) - 1 do begin
        optionButtons[i].Visible := false;
      end;
    end;
  end;

  if optionPanelOpen then begin
  	SetOptionPanelWidth(Round(percent * (optionPanelCloseWidth + (optionPanelOpenWidth - optionPanelCloseWidth))));
	end else begin
  	SetOptionPanelWidth(Round((1.0 - percent) * (optionPanelCloseWidth + (optionPanelOpenWidth - optionPanelCloseWidth))));
  end;
end;


procedure TMainForm.OpenCloseOptionPanel(const iOpen: Boolean);
begin
	if iOpen and optionPanelOpen then Exit;
  if (not iOpen) and (not optionPanelOpen) then Exit;

  if optionPanelAnimTimer = nil then begin
  	ilog('Creating option panel timer');

  	optionPanelAnimTimer := TTimer.Create(self);
    optionPanelAnimTimer.Interval := 1;
    optionPanelAnimTimer.Enabled := false;
    optionPanelAnimTimer.OnTimer := optionPanelAnimTimer_timer;
  end else begin
  	if optionPanelAnimTimer.Enabled then Exit;
  end;

  ilog('Opening / Closing timer');

  optionPanelAnimTimer.Enabled := true;
  optionPanelAnimationStartTime := GetTickCount();

  optionPanelOpen := iOpen;
  
  optionPanel.Visible := true;
end;


procedure TMainForm.OpenOptionPanel();
begin
	OpenCloseOptionPanel(true);
end;


procedure TMainForm.ToggleOptionPanel();
begin
  OpenCloseOptionPanel(not optionPanelOpen);
end;


procedure TMainForm.arrowButton_click(sender: TObject);
begin
  ToggleOptionPanel();
end;


function TMainForm.AddOptionButtonData():TOptionButtonDatum;
begin
	SetLength(optionButtonData, Length(optionButtonData) + 1);
  result := TOptionButtonDatum.Create();
  optionButtonData[Length(optionButtonData) - 1] := result;
end;


procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TMain.Instance.User.Save();
end;

procedure TMainForm.FormCreate(Sender: TObject);
var optionButton: TWImageButton;
	i: Word;
  d: TOptionButtonDatum;
begin
  // ---------------------------------------------------------------------------
  // Initialize form settings
  // ---------------------------------------------------------------------------

  TMain.Instance.MainForm := self;

  ilog('Initializing form settings');

	DoubleBuffered := true;
  windowDragData.dragging := false;
  
  optionPanelOpen := false;    
  optionPanelCloseWidth := 0;
  optionPanelAnimationStartTime := 0;
  optionPanelAnimationDuration := 200;
  optionPanelCurrentWidth := optionPanelCloseWidth;

  optionButtonGap := 3;

  // ---------------------------------------------------------------------------
  // Initialize option buttons' data
  // ---------------------------------------------------------------------------

  d := AddOptionButtonData();
  d.ID := 4;
  d.Name := 'Close';
  d.IconFilePath := TMain.instance.skinPath + '\ButtonIcon_Close.png';
  d.Separator := false;

  d := AddOptionButtonData();
  d.ID := 5;
  d.Name := 'Close';
  d.IconFilePath := TMain.instance.skinPath + '\ButtonIcon_Close.png';
  d.Separator := false;

  d := AddOptionButtonData();
  d.ID := 6;
  d.Name := 'Close';
  d.IconFilePath := TMain.instance.skinPath + '\ButtonIcon_Close.png';
  d.Separator := false;

  d := AddOptionButtonData();
  d.Separator := true;

  d := AddOptionButtonData();
  d.ID := 1;
  d.Name := 'Encrypt';
  d.IconFilePath := TMain.instance.skinPath + '\ButtonIcon_Key.png';
  d.Separator := false;

  d := AddOptionButtonData();
  d.ID := 2;
  d.Name := 'Config';
  d.IconFilePath := TMain.instance.skinPath + '\ButtonIcon_Config.png';
	d.Separator := false;

  d := AddOptionButtonData();
  d.ID := 3;
  d.Name := 'Help';
  d.IconFilePath := TMain.instance.skinPath + '\ButtonIcon_Help.png';
  d.Separator := false;

  // ---------------------------------------------------------------------------
  // Create form controls
  // ---------------------------------------------------------------------------

  ilog('Creating option panel');

  { OPTION PANEL }

  optionPanel := TWNineSlicesPanel.Create(self);
  optionPanel.ImagePathPrefix := TMain.instance.skinPath + '\OptionPanel';
  optionPanel.Width := optionPanelOpenWidth;
  optionPanel.Visible := false;
  optionPanel.Parent := self;

  optionPanel.OnMouseDown := barBackground_down;
  optionPanel.OnMouseUp := barBackground_up;
  optionPanel.OnMouseMove := barBackground_move;

  { OPTION BUTTONS }

  for i := 0 to Length(optionButtonData) - 1 do begin
    optionButton := TWImageButton.Create(self);
    optionButton.Tag := optionButtonData[i].ID;

  	if not optionButtonData[i].Separator then begin
      optionButton := TWImageButton.Create(self);
      optionButton.ImagePathPrefix := TMain.instance.skinPath + '\OptionButton';
      optionButton.Visible := false;
      optionButton.IconImagePath := optionButtonData[i].IconFilePath;
      optionButton.Cursor := crHandPoint;
      optionButton.Tag := optionButtonData[i].ID;
      optionButton.OnClick := optionButton_Click;

      optionPanel.AddChild(optionButton);
    end else begin
    	optionButtonData[i].SeparatorObject := TWNineSlicesPanel.Create(self);
      optionButtonData[i].SeparatorObject.ImagePathPrefix := TMain.instance.skinPath + '\VerticalSeparator';
      optionButtonData[i].SeparatorObject.Visible := false;

      optionPanel.AddChild(optionButtonData[i].SeparatorObject);
    end;

    SetLength(optionButtons, Length(optionButtons) + 1);
    optionButtons[Length(optionButtons) - 1] := optionButton;
  end;

  CalculateOptionPanelOpenWidth();

  optionPanel.Width := optionPanelOpenWidth;

  { BAR BACKGROUND PANEL }

  ilog('Creating background panel');

	barBackground := TWNineSlicesPanel.Create(self);
  barBackground.ImagePathPrefix := TMain.instance.skinPath + '\BarBackground';
  barBackground.Visible := true;
  barBackground.Parent := self;

  barBackground.OnMouseDown := barBackground_down;
  barBackground.OnMouseUp := barBackground_up;
  barBackground.OnMouseMove := barBackground_move;

  { BAR INNER PANEL }

  ilog('Creating inner panel');

  barInnerPanel := TIconPanel.Create(self);
  barInnerPanel.Visible := true;
  barBackground.AddChild(barInnerPanel);

  barInnerPanel.OnMouseDown := barBackground_down;
  barInnerPanel.OnMouseUp := barBackground_up;
  barInnerPanel.OnMouseMove := barBackground_move;

  { ARROW BUTTON }

  ilog('Creating arrow button');

  arrowButton := TWImageButton.Create(self);
  arrowButton.ImagePathPrefix := TMain.instance.skinPath + '\ArrowButton';
  arrowButton.Visible := true;
  arrowButton.Parent := self;
  arrowButton.IconImagePath := TMain.instance.skinPath + '\ArrowButtonIconLeft.png';
  arrowButton.Cursor := crHandPoint;

  arrowButton.OnClick := arrowButton_click;

  // ---------------------------------------------------------------------------
  // Draw and update layout
  // ---------------------------------------------------------------------------

  TMain.Instance.User.RefreshFolderItems();
  barInnerPanel.LoadFolderItems();
	UpdateLayout();
end;


end.
