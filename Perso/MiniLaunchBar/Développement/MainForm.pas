unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, ShellAPI,
  Dialogs, StdCtrls, ExtCtrls, PNGExtra, PNGImage, WImageButton, Imaging, WNineSlicesPanel,
  FileSystemUtils, WFileIcon, Menus, WComponent, WImage, MathUtils,
  Logger, IconPanel, xmldom, XMLIntf, msxmldom, XMLDoc;


const

	Wm_CallBackMessage = wm_user + 1;
  APPLICATION_TITLE = 'Mini Launch Bar';


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
    trayIconPopupMenu: TPopupMenu;
    trayIconPopupMenuClose: TMenuItem;
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
    procedure trayIconPopupMenuCloseClick(Sender: TObject);

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

      pOptionButtonDataID: Integer;

      pNotifyIconData : TNotifyIconData;

      procedure ShowPopupMenu(f : TForm; p : TPopupMenu);
      procedure AppHideInTrayIcon(sender: TObject);
      function OptionPanelTotalWidth():Word;
      function AddOptionButtonData():TOptionButtonDatum;
      function GetButtonDataByID(ID: Integer): TOptionButtonDatum;
      procedure UpdateOptionButtonsLayout(const cornerX, cornerY: Integer);
      procedure CalculateOptionPanelOpenWidth();
      procedure barInnerPanel_Resize(Sender: TObject);
      function GetMaxWidth():Integer;
      function GetIconPanelMaxWidth(): Integer;
      procedure WMCallBackMessage(var msg : TMessage); message Wm_CallBackMessage;

    public
      { Public declarations }
      property MaxWidth: Integer read GetMaxWidth;
      property IconPanelMaxWidth: Integer read GetIconPanelMaxWidth;
      
  end;

var
  theMainForm: TMainForm;
  barBackground: TWNineSlicesPanel;
  barInnerPanel: TIconPanel;
  optionPanel: TWNineSlicesPanel;
  arrowButton: TWImageButton;
  button: TWImageButton;
  windowDragData: TDragData;


implementation

{$R *.dfm}

uses Main, DebugWindow;


procedure TMainForm.barBackground_down(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
	mouse: TMouse;
begin
	if Button <> mbLeft then Exit;

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
	if Button <> mbLeft then Exit;

	ilog('Dragging stopped');
  windowDragData.dragging := false;
end;


procedure TMainForm.barBackground_move(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
	mouse: TMouse;
begin
	if windowDragData.dragging then begin
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

  if (csDestroying in ComponentState) then Exit;

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


procedure TMainForm.barInnerPanel_Resize;
begin
	if (csDestroying in ComponentState) then Exit;

  barBackground.Width := barInnerPanel.Width + TMain.instance.style.barMainPanel.paddingH;
  barBackground.Height := barInnerPanel.Height + TMain.instance.style.barMainPanel.paddingV;

  barBackground.Left := OptionPanelTotalWidth;

  barInnerPanel.Left := TMain.instance.style.barMainPanel.paddingLeft;
  barInnerPanel.Top := TMain.instance.style.barMainPanel.paddingTop;

  optionPanel.Height := barBackground.Height;

  UpdateOptionPanel();
  UpdateFormMask();
end;


procedure TMainForm.UpdateLayout();
begin
	ilog('Updating layout...');

  // Once barInnerPanel.UpdateLayout() has been called
  // its OnResize event should be called, which in turns is going
  // to trigger the update of the rest of the form
  barInnerPanel.UpdateLayout();
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


function TMainForm.GetIconPanelMaxWidth: Integer;
begin
	result := MaxWidth;
  result := result - arrowButton.Width;
  result := result - TMain.Instance.style.barMainPanel.paddingH;
end;


function TMainForm.GetMaxWidth: Integer;
begin
	result := Screen.WorkAreaWidth;
  result := result - optionPanelOpenWidth - arrowButton.Width; 
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
  	Exit;
  end;

  if d.Name = 'Eject' then begin
  	TMain.Instance.EjectDrive();
    Close();
    Exit;
  end;
  
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


procedure TMainForm.trayIconPopupMenuCloseClick(Sender: TObject);
begin
	Close();
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
  result.ID := pOptionButtonDataID;
  result.Separator := false;
  pOptionButtonDataID := pOptionButtonDataID + 1;
  optionButtonData[Length(optionButtonData) - 1] := result;
end;


procedure TMainForm.AppHideInTrayIcon;
begin
  Visible := FALSE;
end;


procedure TMainForm.ShowPopupMenu(f : TForm; p : TPopupMenu);
var
  pt : TPoint;
begin
  GetCursorPos(pt);
  SetForegroundWindow(f.handle);
  p.Popup(pt.x, pt.y);
end;



procedure TMainForm.WMCallBackMessage;
var Owner : HWND;
begin
  case msg.lParam of

    Wm_RButtonDown : begin
    	ShowPopupMenu(self, trayIconPopupMenu);
    end;

    Wm_LButtonDown : begin
      if not Application.Active then begin
        Visible := TRUE;
        Application.Restore;
        Application.BringToFront;
      end else begin
        Visible := not Visible;
        if Visible then begin
          Application.Restore;
          Application.BringToFront;
        end else begin
          Application.Minimize;
          Visible := FALSE;
        end;
      end;
      Owner := GetWindow(Handle, GW_OWNER);
      ShowWindow(Owner, SW_HIDE);
    end;

    Wm_LButtonDblClk : ;
    Wm_MouseMove : ;
  end;
end;


procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
	Shell_NotifyIcon(NIM_DELETE, @pNotifyIconData);
  TMain.Instance.User.Save();
end;

procedure TMainForm.FormCreate(Sender: TObject);
var optionButton: TWImageButton;
	i: Word;
  d: TOptionButtonDatum;
  applicationIcon: TIcon;
begin
  // ---------------------------------------------------------------------------
  // Initialize form settings
  // ---------------------------------------------------------------------------

  TMain.Instance.MainForm := self;

  ilog('Initializing form settings');

	DoubleBuffered := true;
  windowDragData.dragging := false;
  pOptionButtonDataID := 1;
  
  optionPanelOpen := false;    
  optionPanelCloseWidth := 0;
  optionPanelAnimationStartTime := 0;
  optionPanelAnimationDuration := 200;
  optionPanelCurrentWidth := optionPanelCloseWidth;

  optionButtonGap := 3;

  applicationIcon := TIcon.Create();
  applicationIcon.LoadFromFile(TMain.Instance.IconsPath + '\Application.ico');
  Icon := applicationIcon;
  Application.Icon := applicationIcon;

  // ---------------------------------------------------------------------------
  // Hide form in tray
  // ---------------------------------------------------------------------------
    
  Application.OnMinimize := AppHideInTrayIcon;

  // on initialise la structure TNotifyIconData
  with pNotifyIconData do begin
    cbSize := sizeof(pNotifyIconData);                       // taille de la structure
    wnd := handle;                               // fenêtre du Tray Icon
    uID := 1;
    uCallBackMessage := wm_CallBackMessage;      // message envoyé par le système
    hIcon := applicationIcon.handle;                        // l'îcône du Tray Icon
    szTip := APPLICATION_TITLE;                   // Message d'aide
    uFlags := nif_message or nif_Icon or nif_tip;// Indique que notre Tray Icon
                                               	// reçoit un message,
                                               // a une icône et un conseil
  end;
  
  // enregistre le Tray Icon
  Shell_NotifyIcon(NIM_ADD, @pNotifyIconData);

	// cache l'application
  //if not TMain.Instance.CommandLineArgs.HasArgument('showInTaskBar') then
  //	ShowWindow(Application.Handle, SW_HIDE);

  { TODO: A corriger }
  ShowWindow(GetWindow(Application.Handle, GW_OWNER), SW_HIDE);


  // ---------------------------------------------------------------------------
  // Localization
  // ---------------------------------------------------------------------------

  trayIconPopupMenuClose.Caption := TMain.Instance.Loc.GetString('Global.Close');

  // ---------------------------------------------------------------------------
  // Initialize option buttons' data
  // ---------------------------------------------------------------------------

  d := AddOptionButtonData();
  d.Name := 'Close';
  d.IconFilePath := TMain.instance.skinPath + '\ButtonIcon_Close.png';

  d := AddOptionButtonData();
  d.Name := 'Eject';
  d.IconFilePath := TMain.instance.skinPath + '\ButtonIcon_Eject.png';

//  d := AddOptionButtonData();
//  d.Name := 'Close';
//  d.IconFilePath := TMain.instance.skinPath + '\ButtonIcon_Close.png';
//
//  d := AddOptionButtonData();
//  d.Name := 'Close';
//  d.IconFilePath := TMain.instance.skinPath + '\ButtonIcon_Close.png';

//  d := AddOptionButtonData();
//  d.Separator := true;

//  d := AddOptionButtonData();
//  d.Name := 'Encrypt';
//  d.IconFilePath := TMain.instance.skinPath + '\ButtonIcon_Key.png';

//  d := AddOptionButtonData();
//  d.Name := 'Config';
//  d.IconFilePath := TMain.instance.skinPath + '\ButtonIcon_Config.png';

//  d := AddOptionButtonData();
//  d.Name := 'Help';
//  d.IconFilePath := TMain.instance.skinPath + '\ButtonIcon_Help.png';

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
  barInnerPanel.OnResize := barInnerPanel_Resize;
  barBackground.AddChild(barInnerPanel);

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

  TMain.Instance.User.AutomaticallyAddNewApps();
  barInnerPanel.LoadFolderItems();
	UpdateLayout();
  TMain.Instance.User.DoQuickLaunch();
end;


end.
