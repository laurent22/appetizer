unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, ShellAPI,
  Dialogs, StdCtrls, ExtCtrls, PNGExtra, PNGImage, WImageButton, Imaging, WNineSlicesPanel,
  FileSystemUtils, WFileIcon, Menus;

type

  TDragData = record
    dragging: Boolean;
    startMousePos: TPoint;
    startWindowPos: TPoint;
  end;


  TOptionButtonDatum = class
  	public
    ID: Integer;
    Name: String;
    IconFilePath: String;
  end;

  
  TMainForm = class(TForm)
  	procedure icon_click(sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure barBackground_down(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure barBackground_up(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure barBackground_move(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure CreateIconsFromFolderItems();
    procedure UpdateLayout();
    function IconCount():Word;
    procedure arrowButton_click(sender: TObject);
    procedure OpenOptionPanel();
    procedure OpenCloseOptionPanel(const iOpen: Boolean);
    procedure optionPanelAnimTimer_timer(sender: TObject);
    procedure SetOptionPanelWidth(const iWidth: Integer);
    procedure UpdateFormMask();
    procedure ToggleOptionPanel();
    procedure UpdateOptionPanel();
    procedure optionButton_Click(Sender: TObject);

    private
      { Private declarations }
      icons: Array[0..255] of TWFileIcon;
      iconSize: Byte;
      iconGap: Byte;

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
    public
      { Public declarations }
      
  end;

var
  Form1: TMainForm;
  barBackground: TWNineSlicesPanel;
  barInnerPanel: TWNineSlicesPanel;
  optionPanel: TWNineSlicesPanel;
  arrowButton: TWImageButton;
  button: TWImageButton;
  windowDragData: TDragData;

  

implementation

{$R *.dfm}


uses Main;


procedure TMainForm.barBackground_down(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
	mouse: TMouse;
begin
	TMain.Instance.ilog('Form dragging started');

	mouse := TMouse.Create();
	windowDragData.dragging := true;
	windowDragData.startMousePos.X := mouse.CursorPos.X;
  windowDragData.startMousePos.Y := mouse.CursorPos.Y;
  windowDragData.startWindowPos.X := Left;
  windowDragData.startWindowPos.Y := Top;
end;


procedure TMainForm.barBackground_up(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
	TMain.Instance.ilog('Dragging stopped');
  windowDragData.dragging := false;
end;


procedure TMainForm.barBackground_move(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
	mouse: TMouse;
begin
	if windowDragData.dragging then begin
  	TMain.Instance.ilog('Dragging...');
    mouse := TMouse.Create();
    Left := windowDragData.startWindowPos.X + (mouse.CursorPos.X - windowDragData.startMousePos.X);
    Top := windowDragData.startWindowPos.Y + (mouse.CursorPos.Y - windowDragData.startMousePos.Y);
  end;
end;


procedure TMainForm.UpdateFormMask();
var bmp: TBitmap;
	 region: THandle;
begin
	TMain.Instance.ilog('Updating form mask...');

  Width := barBackground.Width + OptionPanelTotalWidth;
  Height := barBackground.Height;

  bmp := TBitmap.Create;
  try
  	bmp.Width := Width;
    bmp.Height := Height;

    DrawNineSlices(bmp.Canvas, TMain.instance.skinPath + '\BarBackgroundRegion', optionPanelOpenWidth - optionPanelCurrentWidth, 0, bmp.Width, bmp.Height);

    region := CreateRegion(Bmp);
    SetWindowRGN(Handle, region, True);
    application.ProcessMessages;
  finally
    bmp.Free;
  end;
end;


function TMainForm.OptionPanelTotalWidth():Word;
begin
	result := optionPanelOpenWidth + arrowButton.Width;
end;


procedure TMainForm.UpdateLayout();
var iconAreaWidth: Word;
  i: Word;
  iconX, iconY: Word;
begin
	TMain.Instance.ilog('Updating layout...');

	iconAreaWidth := IconCount * iconSize + (IconCount - 1) * iconGap;

  barInnerPanel.Width := iconAreaWidth + TMain.instance.style.barInnerPanel.paddingH;
  barInnerPanel.Height := iconSize + TMain.instance.style.barInnerPanel.paddingV;

  barBackground.Width := barInnerPanel.Width + TMain.instance.style.barMainPanel.paddingH;
  barBackground.Height := barInnerPanel.Height + TMain.instance.style.barMainPanel.paddingV;

  barBackground.Left := OptionPanelTotalWidth;
  barInnerPanel.Left := OptionPanelTotalWidth +  TMain.instance.style.barMainPanel.paddingLeft;

  barInnerPanel.Top := TMain.instance.style.barMainPanel.paddingTop;

  //arrowButton.Left := optionPanelCurrentWidth;

  optionPanel.Height := barBackground.Height;

  iconX := TMain.instance.style.barInnerPanel.paddingLeft + barInnerPanel.Left;
  iconY := TMain.instance.style.barInnerPanel.paddingTop + barInnerPanel.Top;

  TMain.Instance.ilog('Updating icon positions...');

  for i := 0 to Length(icons)-1 do begin
  	if icons[i] = nil then break;

  	icons[i].Left := iconX;
    icons[i].Top := iconY;
    iconX := iconX + icons[i].Width + iconGap;
  end;

  UpdateOptionPanel();
  UpdateFormMask();
end;


procedure TMainForm.optionButton_Click(Sender: TObject);
var d: TOptionButtonDatum;
  button: TWImageButton;
	i: Byte;
begin
	button := sender as TWImageButton;

  d.ID := 0;

  for i := 0 to Length(optionButtonData) - 1 do begin
  	if optionButtonData[i].ID = button.Tag then begin
    	d := optionButtonData[i];
      break;
    end;
  end;

  if d.ID = 0 then Exit;

  TMain.Instance.ilog('Option button click: ' + d.Name);

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


function TMainForm.IconCount():Word;
var i:Word;
begin
	result := 0;
  for i := 0 to Length(icons) - 1 do begin
  	if icons[i] = nil then break;
    result := result + 1;
  end;
end;


procedure TMainForm.icon_click(sender: TObject);
var icon: TWFileIcon;
	folderItem: TFolderItem;
  ansiCharPath: PAnsiChar;
  r: HINST;
begin
	icon := Sender as TWFileIcon;
  folderItem := TMain.Instance.GetFolderItemByID(icon.Tag);

  TMain.Instance.ilog('Icon click: ' + folderItem.FilePath);

  r := ShellExecute(Handle, 'open', PChar(folderItem.FilePath), nil, nil, SW_SHOWNORMAL) ;
	if Integer(r) <= 32 then begin
  	TMain.Instance.ErrorMessage(
    	TMain.Instance.Loc.GetString('MainForm.LaunchFileError', IntToStr(r))
    );
  end;
end;


procedure TMainForm.CreateIconsFromFolderItems();
var i: Word;
  folderItem: TFolderItem;
  icon: TWFileIcon;
  iconIndex : Word;
begin
	TMain.Instance.ilog('Creating icons');

	for i := 1 to Length(icons) do begin
  	if icons[i] = nil then break;
    icons[i].Destroy();
  end;

  iconIndex := 0;

  for i := 1 to TMain.instance.FolderItemCount do begin
  	folderItem := TMain.instance.getFolderItemAt(i);
    
    icon := TWFileIcon.Create(self);
    icon.Tag := folderItem.ID;
    icon.FilePath := folderItem.filePath;
    icon.OverlayImagePath := TMain.instance.skinPath + '\IconOverlay.png';
    icon.Width := iconSize;
    icon.Height := iconSize;
    icon.Parent := self;
    icon.Visible := true;
    icon.Cursor := crHandPoint;
    icon.OnClick := icon_click;

    icons[iconIndex] := icon;
    iconIndex := iconIndex + 1;
  end;
end;


procedure TMainForm.SetOptionPanelWidth(const iWidth: Integer);
begin
	if iWidth = optionPanelCurrentWidth then Exit;

	optionPanelCurrentWidth := iWidth;

  UpdateOptionPanel();
  UpdateFormMask();
end;


procedure TMainForm.UpdateOptionPanel();
var i: Byte;
	buttonX, buttonY: Integer;
begin
	TMain.Instance.ilog('Updating option panel');

	if optionPanel.Visible then begin
  	optionPanel.Left := optionPanelOpenWidth - optionPanelCurrentWidth + arrowButton.Width;
    arrowButton.Left := optionPanel.Left - arrowButton.Width;

    buttonX := optionPanel.Left + TMain.Instance.Style.OptionPanel.PaddingLeft;
    buttonY := optionPanel.Top + TMain.Instance.Style.OptionPanel.PaddingTop;

    for i := 0 to Length(optionButtons) - 1 do begin
      optionButtons[i].Visible := true;
      optionButtons[i].Left := buttonX;
      optionButtons[i].Top := buttonY;
      if ((i + 1) mod 3) = 0 then begin
      	buttonY := optionPanel.Top + TMain.Instance.Style.OptionPanel.PaddingTop;
        buttonX := buttonX + optionButtons[i].Width + optionButtonGap;
      end else begin
      	buttonY := buttonY + optionButtons[i].Height + optionButtonGap;
      end;
    end;
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
  	TMain.Instance.ilog('Panel animation complete');

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
var i: Word;
begin
	if iOpen and optionPanelOpen then Exit;
  if (not iOpen) and (not optionPanelOpen) then Exit;

  if optionPanelAnimTimer = nil then begin
  	TMain.Instance.ilog('Creating option panel timer');

  	optionPanelAnimTimer := TTimer.Create(self);
    optionPanelAnimTimer.Interval := 1;
    optionPanelAnimTimer.Enabled := false;
    optionPanelAnimTimer.OnTimer := optionPanelAnimTimer_timer;
  end else begin
  	if optionPanelAnimTimer.Enabled then Exit;
  end;

  TMain.Instance.ilog('Opening / Closing timer');

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


procedure TMainForm.FormCreate(Sender: TObject);
var optionButton: TWImageButton;
	i: Word;
	optionButtonsCol: Byte;
  d: TOptionButtonDatum;
begin
  // ---------------------------------------------------------------------------
  // Initialize form settings
  // ---------------------------------------------------------------------------

  TMain.Instance.MainForm := self;

  TMain.Instance.ilog('Initializing form settings');

	DoubleBuffered := true;
  windowDragData.dragging := false;
  iconSize := 40;
  iconGap := 0;
  
  optionPanelOpen := false;    
  optionPanelCloseWidth := 0;
  optionPanelAnimationStartTime := 0;
  optionPanelAnimationDuration := 100;
  optionPanelCurrentWidth := optionPanelCloseWidth;

  optionButtonGap := 3;

  // ---------------------------------------------------------------------------
  // Initialize option buttons' data
  // ---------------------------------------------------------------------------

  d := AddOptionButtonData();
  d.ID := 1;
  d.Name := 'Encrypt';
  d.IconFilePath := TMain.instance.skinPath + '\ButtonIcon_Key.png';

  d := AddOptionButtonData();
  d.ID := 2;
  d.Name := 'Config';
  d.IconFilePath := TMain.instance.skinPath + '\ButtonIcon_Config.png';

  d := AddOptionButtonData();
  d.ID := 3;
  d.Name := 'Help';
  d.IconFilePath := TMain.instance.skinPath + '\ButtonIcon_Help.png';

  d := AddOptionButtonData();
  d.ID := 4;
  d.Name := 'Close';
  d.IconFilePath := TMain.instance.skinPath + '\ButtonIcon_Close.png';

  // ---------------------------------------------------------------------------
  // Create form controls
  // ---------------------------------------------------------------------------

  TMain.Instance.ilog('Creating option panel');

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
    optionButton.ImagePathPrefix := TMain.instance.skinPath + '\OptionButton';
    optionButton.Visible := false;
    optionButton.Parent := self;
    optionButton.IconImagePath := optionButtonData[i].IconFilePath;
    optionButton.Cursor := crHandPoint;
    optionButton.Tag := optionButtonData[i].ID;
    optionButton.OnClick := optionButton_Click;

    SetLength(optionButtons, Length(optionButtons) + 1);
    optionButtons[Length(optionButtons) - 1] := optionButton;
  end;

  optionButtonsCol := (Trunc(Length(optionButtons) / 3) + 1);

  optionPanelOpenWidth :=
  	optionButtons[0].Width * optionButtonsCol +
    optionButtonGap * (optionButtonsCol - 1) +
    TMain.Instance.Style.OptionPanel.PaddingH;

  optionPanel.Width := optionPanelOpenWidth;

  { BAR BACKGROUND PANEL }

  TMain.Instance.ilog('Creating background panel');

	barBackground := TWNineSlicesPanel.Create(self);
  barBackground.ImagePathPrefix := TMain.instance.skinPath + '\BarBackground';
  barBackground.Visible := true;
  barBackground.Parent := self;

  barBackground.OnMouseDown := barBackground_down;
  barBackground.OnMouseUp := barBackground_up;
  barBackground.OnMouseMove := barBackground_move;

  { BAR INNER PANEL }

  TMain.Instance.ilog('Creating inner panel');

  barInnerPanel := TWNineSlicesPanel.Create(self);
  barInnerPanel.ImagePathPrefix := TMain.instance.skinPath + '\BarInnerPanel';
  barInnerPanel.Visible := true;
  barInnerPanel.Parent := self;

  barInnerPanel.OnMouseDown := barBackground_down;
  barInnerPanel.OnMouseUp := barBackground_up;
  barInnerPanel.OnMouseMove := barBackground_move;

  { ARROW BUTTON }

  TMain.Instance.ilog('Creating arrow button');

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

  TMain.Instance.RefreshFolderItems();
  CreateIconsFromFolderItems();
	UpdateLayout();
end;


end.
