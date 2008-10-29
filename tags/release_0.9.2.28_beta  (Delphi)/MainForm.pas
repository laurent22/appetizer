unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, ShellAPI,
  Dialogs, StdCtrls, ExtCtrls, PNGImage, WImageButton, Imaging, WNineSlicesPanel,
  FileSystemUtils, WFileIcon, Menus, WComponent, WImage, MathUtils,
  Logger, IconPanel, xmldom, XMLIntf, msxmldom, XMLDoc, StringUtils, WNineSlicesButton;


const

	Wm_CallBackMessage = wm_user + 1;
  APPLICATION_TITLE = 'Mini Launch Bar';


type

  TDragData = record
    dragging: Boolean;
    draggingType: String;
    startMousePos: TPoint;
    startWindowPos: TPoint;
    startPanelWidth: Integer;
    startPanelHeight: Integer;
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
    trayIconPopupMenuHideShow: TMenuItem;
    N1: TMenuItem;
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
    procedure FormShow(Sender: TObject);
    procedure trayIconPopupMenuHideShowClick(Sender: TObject);
    procedure trayIconPopupMenuPopup(Sender: TObject);

    private
      { Private declarations }

      resizer: TWImage;

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
      pFirstShown: Boolean;
      pNotifyIconData : TNotifyIconData;
      pFormMaskNineSlices: TPNGNineSlices;

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
      procedure Localize(Sender: TObject);

      procedure Resizer_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      procedure Resizer_MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      procedure Resizer_MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

    public
      { Public declarations }
      barInnerPanel: TIconPanel;
      property MaxWidth: Integer read GetMaxWidth;
      property IconPanelMaxWidth: Integer read GetIconPanelMaxWidth;
      
  end;

var
  theMainForm: TMainForm;
  barBackground: TWNineSlicesPanel;

  optionPanel: TWNineSlicesPanel;
  arrowButton: TWNineSlicesButton;
  button: TWImageButton;
  windowDragData: TDragData;


implementation

{$R *.dfm}

uses Main, DebugWindow;


procedure TMainForm.Resizer_MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
	if Button <> mbLeft then Exit;

	ilog('Form resizing started');

	mouse := TMouse.Create();
	windowDragData.dragging := true;
  windowDragData.draggingType := 'resize';
	windowDragData.startMousePos.X := mouse.CursorPos.X;
  windowDragData.startMousePos.Y := mouse.CursorPos.Y;
  windowDragData.startWindowPos.X := Left;
  windowDragData.startWindowPos.Y := Top;
  windowDragData.startPanelWidth := barInnerPanel.Width;
  windowDragData.startPanelHeight := barInnerPanel.Height;
end;

procedure TMainForm.Resizer_MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var newWidth, newHeight: Integer;
begin
	if (windowDragData.dragging) and (windowDragData.draggingType = 'resize') then begin
    //mouse := TMouse.Create();

    newWidth := windowDragData.startPanelWidth + (mouse.CursorPos.X - windowDragData.startMousePos.X);
    newHeight := windowDragData.startPanelHeight + (mouse.CursorPos.y - windowDragData.startMousePos.Y);

    if (newWidth = barInnerPanel.Width) and (newHeight = barInnerPanel.Height) then Exit;

    barInnerPanel.Width := newWidth;
    barInnerPanel.Height := newHeight;

    UpdateLayout();
    Repaint();
  end;
end;

procedure TMainForm.Resizer_MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
	if Button <> mbLeft then Exit;

	ilog('Dragging stopped');
  windowDragData.dragging := false;
end;


procedure TMainForm.barBackground_down(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
	mouse: TMouse;
begin
	if Button <> mbLeft then Exit;

	ilog('Form dragging started');

	mouse := TMouse.Create();
	windowDragData.dragging := true;
  windowDragData.draggingType := 'move';
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
var mouse: TMouse;
begin
	if (windowDragData.dragging) and (windowDragData.draggingType = 'move') then begin
    mouse := TMouse.Create();
    Left := windowDragData.startWindowPos.X + (mouse.CursorPos.X - windowDragData.startMousePos.X);
    Top := windowDragData.startWindowPos.Y + (mouse.CursorPos.Y - windowDragData.startMousePos.Y);
  end;
end;


procedure TMainForm.UpdateFormMask();
var bmp: TBitmap;
    region: THandle;
    rect: TRect;
    sourceImage: TPNGObject;
begin
	ilog('Updating form mask...');

  if (csDestroying in ComponentState) then Exit;

  if pFormMaskNineSlices[0] = nil then begin
    sourceImage := TPNGObject.Create();
    try
      sourceImage.LoadFromFile(TMain.Instance.FilePaths.SkinDirectory + '\BarBackgroundRegion.png');
      pFormMaskNineSlices := PNG_ExtractNineSlices(sourceImage);
    finally
      FreeAndNil(sourceImage);
    end;
  end;

  if pFormMaskNineSlices[0] = nil then Exit;

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

    PNG_DrawNineSlices(
      bmp.Canvas,
      pFormMaskNineSlices,
      optionPanelOpenWidth - optionPanelCurrentWidth,
      0,
      bmp.Width - optionPanelOpenWidth + optionPanelCurrentWidth,
      bmp.Height
    );

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
//	if (csDestroying in ComponentState) then Exit;
//
//  barBackground.Width := barInnerPanel.Width + TMain.instance.Style.barMainPanel.paddingH;
//  barBackground.Height := barInnerPanel.Height + TMain.instance.Style.barMainPanel.paddingV;
//
//  barBackground.Left := OptionPanelTotalWidth;
//
//  barInnerPanel.Left := TMain.instance.Style.barMainPanel.paddingLeft;
//  barInnerPanel.Top := TMain.instance.Style.barMainPanel.paddingTop;
//
//  optionPanel.Height := barBackground.Height;
//
//  UpdateOptionPanel();
//  UpdateFormMask();
end;


procedure TMainForm.UpdateLayout();
begin
	ilog('Updating layout...');

  if (csDestroying in ComponentState) then Exit;
  
  barInnerPanel.UpdateLayout();

  barBackground.Width := barInnerPanel.Width + TMain.instance.Style.barMainPanel.paddingH;
  barBackground.Height := barInnerPanel.Height + TMain.instance.Style.barMainPanel.paddingV;

  barBackground.Left := OptionPanelTotalWidth;

  barInnerPanel.Left := TMain.instance.Style.barMainPanel.paddingLeft;
  barInnerPanel.Top := TMain.instance.Style.barMainPanel.paddingTop;

  resizer.Left := barBackground.Width - resizer.Width;
  resizer.Top := barBackground.Height - resizer.Height;

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


function TMainForm.GetIconPanelMaxWidth: Integer;
begin
	result := MaxWidth;
  result := result - arrowButton.Width;
  result := result - TMain.Instance.Style.barMainPanel.paddingH;
end;


function TMainForm.GetMaxWidth: Integer;
begin
	result := Screen.WorkAreaWidth;
  result := result - optionPanelOpenWidth - arrowButton.Width; 
end;




procedure TMainForm.Localize;
begin
  trayIconPopupMenuClose.Caption := TMain.Instance.Loc.GetString('TrayIcon.Close');
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

  if d.Name = 'Minimize' then begin
    Visible := false;
    Exit;
  end;

  if d.Name = 'AddShortcut' then begin
    barInnerPanel.StartAddingFolderItem();
    Exit;
  end;

  if d.Name = 'MultiLaunch' then begin
    TMain.Instance.User.DoQuickLaunch();
    Exit;
  end;

  if d.Name = 'Config' then begin
    TMain.Instance.ShowConfigForm();
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
var buttonX, buttonY: Integer;
	buttonData: TOptionButtonDatum;
	button: TWImageButton;
  i: Byte;
begin
  buttonX := 0;
  buttonY := 0;

  for i := 0 to Length(optionButtons) - 1 do begin
    button := optionButtons[i];

    buttonData := GetButtonDataByID(button.Tag);
    if buttonData = nil then continue;

    if buttonData.Separator then begin

    	buttonY := optionPanel.Top + TMain.Instance.Style.OptionPanel.PaddingTop;
      buttonX := buttonX + optionButtons[i - 1].Width + optionButtonGap * 4;

    end else begin

      if buttonY + button.Height >= optionPanel.Height then begin
        buttonY := optionPanel.Top + TMain.Instance.Style.OptionPanel.PaddingTop;
        buttonX := buttonX + button.Width + optionButtonGap;
      end;

      buttonY := buttonY + optionButtons[i].Height + optionButtonGap;

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
      buttonData.SeparatorObject.Height := optionPanel.Height - TMain.Instance.Style.optionPanel.paddingV;
                                                 
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
var lastButtonRight: Integer;
    lastButton: TWImageButton;
begin
	ilog('Updating option panel');

  arrowButton.Height := barBackground.Height;

	if optionPanel.Visible then begin

  	optionPanel.Left := optionPanelOpenWidth - optionPanelCurrentWidth + arrowButton.Width;
    arrowButton.Left := optionPanel.Left - arrowButton.Width; 
    optionPanel.Height := arrowButton.Height;

  	UpdateOptionButtonsLayout(
    	TMain.Instance.Style.OptionPanel.PaddingLeft,
      TMain.Instance.Style.OptionPanel.PaddingTop
    );

    lastButton := optionButtons[Length(optionButtons) - 1];
    lastButtonRight := lastButton.Left + lastButton.Width;

    if lastButtonRight + TMain.Instance.Style.optionPanel.paddingRight <> optionPanelOpenWidth then begin
      optionPanelOpenWidth := lastButtonRight + TMain.Instance.Style.optionPanel.paddingRight;
      ilog(optionPanelOpenWidth);
      SetOptionPanelWidth(optionPanelOpenWidth);
      UpdateLayout();
    end;

  end else begin
		arrowButton.Left := optionPanelOpenWidth - optionPanelCurrentWidth;
  end;
end;


procedure TMainForm.optionPanelAnimTimer_timer(sender: TObject);
var percent: Real;
	i: Byte;
begin
	if optionPanelAnimationDuration > 0 then
		percent := (GetTickCount() - optionPanelAnimationStartTime) / optionPanelAnimationDuration
  else
  	percent := 1.0;

  if percent >= 1.0 then begin
  	ilog('Panel animation complete');

  	percent := 1.0;
    optionPanelAnimTimer.Enabled := false;
    if not optionPanelOpen then optionPanel.Visible := false;
    if optionPanelOpen then begin
    	arrowButton.IconImagePath := TMain.Instance.FilePaths.SkinDirectory + '\ArrowButtonIconRight.png';
    end else begin
     	arrowButton.IconImagePath := TMain.Instance.FilePaths.SkinDirectory + '\ArrowButtonIconLeft.png';

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

  optionPanelOpen := iOpen;
  optionPanel.Visible := true;

  if TMain.Instance.User.GetUserSetting('AnimationsEnabled') <> 'true' then begin
  	if iOpen then begin
    	SetOptionPanelWidth(Round(optionPanelCloseWidth + (optionPanelOpenWidth - optionPanelCloseWidth)));
      optionPanel.Visible := true;
      arrowButton.IconImagePath := TMain.Instance.FilePaths.SkinDirectory + '\ArrowButtonIconRight.png';
    end else begin
    	SetOptionPanelWidth(0);
      optionPanel.Visible := false;
      arrowButton.IconImagePath := TMain.Instance.FilePaths.SkinDirectory + '\ArrowButtonIconLeft.png';
    end;
    Exit;
  end;

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

  if TMain.Instance.User.GetUserSetting('AnimationsEnabled') <> 'true' then
		optionPanelAnimationDuration := 0
	else
  	optionPanelAnimationDuration := 200;
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


procedure TMainForm.trayIconPopupMenuHideShowClick(Sender: TObject);
begin
  Visible := not Visible;
  if Visible then BringToFront();
end;


procedure TMainForm.trayIconPopupMenuPopup(Sender: TObject);
begin
  if Visible then
    trayIconPopupMenuHideShow.Caption := TMain.Instance.Loc.GetString('TrayIcon.Hide')
  else
    trayIconPopupMenuHideShow.Caption := TMain.Instance.Loc.GetString('TrayIcon.Show')
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

      Application.BringToFront();
      Visible := true;

    	//Visible := not Visible;

      //if Visible then Application.BringToFront();

//    	ilog(StringConv(Application.Active));
//
//      if not Application.Active then begin
//        Visible := true;
//        Application.BringToFront;
//      end else begin
//        Visible := not Visible;
//        if Visible then begin
//          //Application.BringToFront;
//        end else begin
//          //Visible := false;
//        end;
//      end;

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

  TMain.Instance.User.SetUserSetting('LastWindowSettings',
    IntToStr(Left) + ',' + IntToStr(Top) + ',' + IntToStr(BarInnerPanel.Width) + ',' + IntToStr(BarInnerPanel.Height));

  TMain.Instance.User.Save();
end;

procedure TMainForm.FormCreate(Sender: TObject);
var optionButton: TWImageButton;
    i: Word;
    d: TOptionButtonDatum;
    applicationIcon: TIcon;
    lastWindowSettings: TStringList;
    nineSliceStrings: TStringList;
    nineSlicesGrid: TRect;
begin
  // ---------------------------------------------------------------------------
  // Initialize form settings
  // ---------------------------------------------------------------------------
  TMain.Instance.MainForm := self;

  ilog('Initializing form settings');
                           
  pFirstShown := true;
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
  applicationIcon.LoadFromFile(TMain.Instance.FilePaths.IconsDirectory + '\Application.ico');
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

  // ---------------------------------------------------------------------------
  // Localization
  // ---------------------------------------------------------------------------

  Localize(Self);
  TMain.Instance.RegisterLocalizableObject(Localize);

  // ---------------------------------------------------------------------------
  // Initialize option buttons' data
  // ---------------------------------------------------------------------------

  d := AddOptionButtonData();
  d.Name := 'Close';
  d.IconFilePath := TMain.Instance.FilePaths.SkinDirectory + '\ButtonIcon_Close.png';

  d := AddOptionButtonData();
  d.Name := 'Minimize';
  d.IconFilePath := TMain.Instance.FilePaths.SkinDirectory + '\ButtonIcon_Minimize.png';

  d := AddOptionButtonData();
  d.Name := 'Eject';
  d.IconFilePath := TMain.Instance.FilePaths.SkinDirectory + '\ButtonIcon_Eject.png';

  d := AddOptionButtonData();
  d.Separator := true;

  d := AddOptionButtonData();
  d.Name := 'Config';
  d.IconFilePath := TMain.Instance.FilePaths.SkinDirectory + '\ButtonIcon_Config.png';

  d := AddOptionButtonData();
  d.Name := 'AddShortcut';
  d.IconFilePath := TMain.Instance.FilePaths.SkinDirectory + '\ButtonIcon_AddShortcut.png';

  d := AddOptionButtonData();
  d.Name := 'MultiLaunch';
  d.IconFilePath := TMain.Instance.FilePaths.SkinDirectory + '\ButtonIcon_MultiLaunch.png';

  // ---------------------------------------------------------------------------
  // Create form controls
  // ---------------------------------------------------------------------------

  ilog('Creating option panel');

  { OPTION PANEL }

  optionPanel := TWNineSlicesPanel.Create(self);
  optionPanel.ImagePath := TMain.Instance.FilePaths.SkinDirectory + '\OptionPanel.png';
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
      optionButton.ImagePathPrefix := TMain.Instance.FilePaths.SkinDirectory + '\OptionButton';
      optionButton.Visible := false;
      optionButton.IconImagePath := optionButtonData[i].IconFilePath;
      optionButton.Cursor := crHandPoint;
      optionButton.Tag := optionButtonData[i].ID;
      optionButton.OnClick := optionButton_Click;

      optionPanel.AddChild(optionButton);
    end else begin
    	optionButtonData[i].SeparatorObject := TWNineSlicesPanel.Create(self);
      optionButtonData[i].SeparatorObject.ImagePath := TMain.Instance.FilePaths.SkinDirectory + '\VerticalSeparator.png';
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

//  nineSliceStrings := TMain.Instance.GetSkinAttributeAsStringList('BarBackground', 'nineScaleGrid');
//
//  if nineSliceStrings.Count >= 4 then begin
//    nineSlicesGrid.Left := StrToInt(nineSliceStrings[0]);
//    nineSlicesGrid.Top := StrToInt(nineSliceStrings[1]);
//    nineSlicesGrid.Right := nineSlicesGrid.Left + StrToInt(nineSliceStrings[2]);  { -1 ??? }
//    nineSlicesGrid.Bottom := nineSlicesGrid.Top + StrToInt(nineSliceStrings[3]);  { -1 ??? }
//
//    barBackground.NineSlicesGrid := nineSlicesGrid;
//  end;

  barBackground.ImagePath := TMain.Instance.FilePaths.SkinDirectory + '\BarBackground.png';
  barBackground.Visible := true;
  barBackground.Parent := self;

  barBackground.OnMouseDown := barBackground_down;
  barBackground.OnMouseUp := barBackground_up;
  barBackground.OnMouseMove := barBackground_move;

  { BAR INNER PANEL }

  ilog('Creating inner panel');

  barInnerPanel := TIconPanel.Create(self);
  barBackground.AddChild(barInnerPanel);
  barInnerPanel.Visible := true;
  barInnerPanel.Width := 200;
  barInnerPanel.Height := 50;
  barInnerPanel.OnResize := barInnerPanel_Resize;

  { ARROW BUTTON }

  ilog('Creating arrow button');

  nineSliceStrings := TMain.Instance.GetSkinAttributeAsStringList('ArrowButton', 'nineScaleGrid');

  arrowButton := TWNineSlicesButton.Create(self);

  if nineSliceStrings.Count >= 4 then begin
    nineSlicesGrid.Left := StrToInt(nineSliceStrings[0]);
    nineSlicesGrid.Top := StrToInt(nineSliceStrings[1]);
    nineSlicesGrid.Right := nineSlicesGrid.Left + StrToInt(nineSliceStrings[2]);  { -1 ??? }
    nineSlicesGrid.Bottom := nineSlicesGrid.Top + StrToInt(nineSliceStrings[3]);  { -1 ??? }

    arrowButton.NineSlicesGrid := nineSlicesGrid;
  end;

  arrowButton.ImagePathPrefix := TMain.Instance.FilePaths.SkinDirectory + '\ArrowButton';
  arrowButton.Visible := true;
  arrowButton.Parent := self;
  arrowButton.Width := 16;
  arrowButton.Height := 64;
  arrowButton.IconImagePath := TMain.Instance.FilePaths.SkinDirectory + '\ArrowButtonIconLeft.png';
  arrowButton.Cursor := crHandPoint;

  arrowButton.OnClick := arrowButton_click;

  resizer := TWImage.Create(Self);
  resizer.FilePath := TMain.Instance.FilePaths.SkinDirectory + '\Resizer.png';
  resizer.StretchToFit := false;
  resizer.MaintainAspectRatio := true;
  resizer.FitToContent();
  resizer.OnMouseDown := Resizer_MouseDown;
  resizer.OnMouseUp := Resizer_MouseUp;
  resizer.OnMouseMove := Resizer_MouseMove;
  barBackground.AddChild(resizer);

  // ---------------------------------------------------------------------------
  // Draw and update layout
  // ---------------------------------------------------------------------------

  lastWindowSettings := SplitString(',', TMain.Instance.User.GetUserSetting('LastWindowSettings'));
  if lastWindowSettings.Count = 4 then begin
    try
      Left := StrToInt(lastWindowSettings[0]);
      Top := StrToInt(lastWindowSettings[1]);
      BarInnerPanel.Width := StrToInt(lastWindowSettings[2]);
      BarInnerPanel.Height := StrToInt(lastWindowSettings[3]);
    except
      on E: Exception do begin
        Left := 0;
        Top := 0;
      end;
    end;
  end;

  // Make sure that the bar is not off-screen
  if Left >= Screen.DesktopWidth - 64 then Left := Screen.DesktopWidth - 64;
  if Top >= Screen.DesktopHeight - 64 then Top := Screen.DesktopHeight - 64;

  TMain.Instance.User.AutomaticallyAddNewApps();
  barInnerPanel.LoadFolderItems();
	UpdateLayout();

  Visible := true;
  ShowWindow(Application.Handle, SW_HIDE);

  { HACK: The size of the window is not calculated correctly at this stage.
    Opening and closing the option panel fixes the issue, however it would
    be nice to find out a proper fix }
  ToggleOptionPanel();
  ToggleOptionPanel();
end;


procedure TMainForm.FormShow(Sender: TObject);
begin
	if pFirstShown then begin
    pFirstShown := false;


  end;
end;

end.
