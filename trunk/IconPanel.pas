unit IconPanel;

interface

uses Windows, WNineSlicesPanel, Classes, WFileIcon, WImage, Logger, Contnrs, Controls,
	Types, WComponent, ExtCtrls, Forms, SysUtils, Graphics, MathUtils, Imaging,
  ShellAPI, WImageButton, Menus, User, StringUtils, EditFolderItemUnit, SystemUtils,
  IconToolTipUnit;

type

  TComponentDragData = record
  	Component: TWComponent;
  	Timer: TTimer;
    MouseDownLoc: TPoint;
    Started: Boolean;
    StartIconLoc: TPoint;
    IconForm: TForm;
    InsertionCursor: TWImage;
  end;


	TIconPanel = class(TWNineSlicesPanel)

  private

    pComponents: TObjectList;
    pIconSize: Word;
    pIconDragData: TComponentDragData;
    pTooltipForm: TIconTooltipForm;
    pBrowseButton: TWImageButton;
    pLastVisibleIconIndex: Integer;

    function FolderItemToComponent(const folderItem:TFolderItem): TWComponent;
    function CreateFormPopupMenu():TPopupMenu;
    procedure Icon_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Icon_MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure IconDragData_timer(Sender: TObject);
    procedure IconForm_Paint(Sender: TObject);

    procedure MenuItem_Click_NewShortcut(Sender: TObject);
    procedure MenuItem_Click_NewSeparator(Sender: TObject);
    procedure MenuItem_Click_Delete(Sender: TObject);
    procedure MenuItem_Click_Properties(Sender: TObject);
    procedure MenuItem_Click_AddItToQuickLaunch(Sender: TObject);

    procedure UpdateFolderItemsOrder();

    function GetInsertionIndexAtPoint(const aPoint: TPoint): Integer;
    procedure Icon_Click(sender: TObject);
    procedure Icon_MouseEnter(sender: TObject);
    procedure Icon_MouseExit(sender: TObject);
    procedure Self_MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    function GetComponentByID(const componentID: Integer): TWComponent;
    procedure BrowseButton_Click(Sender: TObject);
    procedure BrowseButtonPopupMenu_Click(Sender: TObject);
    procedure FitToContent;

  public

  	constructor Create(AOwner: TComponent); override;
  	procedure LoadFolderItems();
    procedure UpdateLayout();

  end;

implementation


uses Main;



constructor TIconPanel.Create(AOwner: TComponent);
begin
  inherited;

  pLastVisibleIconIndex := -1;

  pIconSize := 40;
  ImagePath := TMain.Instance.FilePaths.SkinDirectory + '\BarInnerPanel.png';

  pComponents := TObjectList.Create();
  pComponents.OwnsObjects := false;

  MinWidth := pIconSize + TMain.Instance.Style.barInnerPanel.paddingH;
  MinHeight := pIconSize + TMain.Instance.Style.barInnerPanel.paddingV;
  
  self.OnMouseDown := Self_MouseDown;
end;


procedure TIconPanel.BrowseButton_Click(Sender: TObject);
var pPopupMenu: TPopupMenu;
	menuItem: TMenuItem;
	i: Integer;
  folderItem: TFolderItem;
  component: TWComponent;
  mouse: TMouse;
  menuItemBitmap: TBitmap;
begin
  pPopupMenu := TPopupMenu.Create(self);

  for i := pLastVisibleIconIndex + 1 to pComponents.Count - 1 do begin
  	if i >= pComponents.Count then break;

    component := TWComponent(pComponents[i]);

    folderItem := TMain.Instance.User.GetFolderItemByID(component.Tag);

    menuItem := TMenuItem.Create(self);

    if folderItem.IsSeparator then begin
    	menuItem.Caption := '-';
      menuItem.Enabled := false;
    end else begin
    	menuItem.Caption := folderItem.Name;
      menuItem.Tag := folderItem.ID;

      if folderItem.SmallIcon <> nil then begin
      	menuItemBitmap := TBitmap.Create();
        menuItemBitmap.Width := 16;
        menuItemBitmap.Height := 16;
        menuItemBitmap.Canvas.Draw(0, 0, folderItem.SmallIcon);
        menuItem.OnClick := BrowseButtonPopupMenu_Click;
      	menuItem.Bitmap := menuItemBitmap;
      end;

    end;

    pPopupMenu.Items.Add(menuItem);
  end;

  mouse := TMouse.Create();

  pPopupMenu.Popup(mouse.CursorPos.X, mouse.CursorPos.Y);
end;


procedure TIconPanel.BrowseButtonPopupMenu_Click(Sender: TObject);
var folderItem: TFolderItem;
begin
	folderItem := TMain.Instance.User.GetFolderItemByID((Sender as TMenuItem).Tag);
  folderItem.Launch();
end;



function TIconPanel.GetComponentByID(const componentID: Integer): TWComponent;
var i: Integer;
begin
	for i := 0 to pComponents.Count do begin
  	if TWComponent(pComponents[i]).ID = componentID then begin
      result := TWComponent(pComponents[i]);
      Exit;
    end;
  end;
  result := nil;
end;


procedure TIconPanel.Self_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
	self.PopupMenu := CreateFormPopupMenu();
end;


function TIconPanel.CreateFormPopupMenu;
var menuItem: TMenuItem;
begin
  result := TPopupMenu.Create(self);

  menuItem := TMenuItem.Create(result);
  menuItem.Caption := TMain.Instance.Loc.GetString('IconPanel.PopupMenu.NewShortcut');
  menuItem.OnClick := MenuItem_Click_NewShortcut;
  result.Items.Add(menuItem);

//  menuItem := TMenuItem.Create(result);
//  menuItem.Caption := TMain.Instance.Loc.GetString('IconPanel.PopupMenu.NewSeparator');
//  menuItem.OnClick := MenuItem_Click_NewSeparator;
//  result.Items.Add(menuItem);
end;


procedure TIconPanel.MenuItem_Click_NewShortcut;
var folderItem: TFolderItem;
	component: TWComponent;
  filePath: String;
begin
	filePath := '';
  if OpenSaveFileDialog(Application.Handle, '', TMain.Instance.Loc.GetString('Global.OpenDialog.AllFiles') + '|*.*', '', TMain.Instance.Loc.GetString('IconPanel.NewShorcut.OpenDialog'), filePath, true) then begin
  	folderItem := TFolderItem.Create();
    folderItem.FilePath := TFolderItem.ConvertToRelativePath(filePath);
    folderItem.AutoSetName();
    TMain.Instance.User.AddFolderItem(folderItem);

    component := FolderItemToComponent(folderItem);
    AddChild(component);
    pComponents.Add(TObject(component));

    UpdateLayout();
  end;
end;


procedure TIconPanel.MenuItem_Click_NewSeparator;
var folderItem: TFolderItem;
	component: TWComponent;
begin
 	folderItem := TFolderItem.Create();
  folderItem.IsSeparator := true;
  TMain.Instance.User.AddFolderItem(folderItem);
  component := FolderItemToComponent(folderItem);
  AddChild(component);
  pComponents.Add(TObject(component));
  UpdateLayout();
end;


procedure TIconPanel.MenuItem_Click_AddItToQuickLaunch;
var component: TWComponent;
	menuItem: TMenuItem;
	folderItem: TFolderItem;
begin
	menuItem := Sender as TMenuItem;
	component := GetComponentByID(menuItem.Tag);
  folderItem := TMain.Instance.User.GetFolderItemByID(component.Tag);

  folderItem.QuickLaunch := not folderItem.QuickLaunch;
  TMain.Instance.User.InvalidateFolderItems();
end;


procedure TIconPanel.MenuItem_Click_Delete(Sender: TObject);
var component: TWComponent;
	menuItem: TMenuItem;
	answer: Integer;
begin
	answer := TMain.Instance.ConfirmationMessage(TMain.Instance.Loc.GetString('IconPanel.DeleteConfirmation'));
  if answer <> mrYes then Exit;

	menuItem := Sender as TMenuItem;

  component := GetComponentByID(menuItem.Tag);
  if component = nil then begin
  	elog('Couldn''t find component with ID: ' + IntToStr(menuItem.Tag));
    Exit;
  end;

  pComponents.Remove(TObject(component));

	component.Destroy();

  UpdateFolderItemsOrder();
  UpdateLayout();
end;


procedure TIconPanel.MenuItem_Click_Properties(Sender: TObject);
var folderItem: TFolderItem;
  component: TWComponent;
  hasChanged: Boolean;
begin
  component := GetComponentByID(TMenuItem(sender).Tag);
  folderItem := TMain.Instance.User.GetFolderItemByID(component.Tag);
 	hasChanged := TMain.Instance.User.EditFolderItem(folderItem);
	if hasChanged then begin
  	if component is TWFileIcon then begin
    	TWFileIcon(component).FilePath := folderItem.FilePath;
    end;
  end;
end;


function TIconPanel.FolderItemToComponent;
var icon: TWFileIcon;
	separatorImage: TWImage;
begin
  if not folderItem.IsSeparator then begin

    icon := TWFileIcon.Create(Owner);
    icon.Tag := folderItem.ID;
    icon.Width := pIconSize;
    icon.Height := pIconSize;
    icon.Visible := false;
    icon.OnMouseDown := icon_mouseDown;
    icon.OnMouseUp := icon_mouseUp;
    icon.OnClick := Icon_Click;
    icon.OnMouseEnter := Icon_MouseEnter;
    icon.OnMouseExit := Icon_MouseExit;

    result := TWComponent(icon);

  end else begin

//    separatorImage := TWImage.Create(Owner);
//    separatorImage.FilePath := TMain.Instance.FilePaths.SkinDirectory + '\InnerPanelSeparator.png';
//    separatorImage.Visible := false;
//    separatorImage.Tag := folderItem.ID;
//    separatorImage.FitToContent();
//    separatorImage.Height := pIconSize;
//    separatorImage.StretchToFit := true;
//    separatorImage.MaintainAspectRatio := false;
//    separatorImage.OnMouseDown := icon_mouseDown;
//    separatorImage.OnMouseUp := icon_mouseUp;

//    result := TWComponent(separatorImage);

  end;
end;


function TIconPanel.GetInsertionIndexAtPoint(const aPoint: TPoint): Integer;
var i:Integer;
	component: TWComponent;
  point: TPoint;
begin
	point.X := aPoint.X;
  point.Y := aPoint.Y;

	result := -1;

	for i := 0 to pComponents.Count - 1 do begin
  	component := TWComponent(pComponents[i]);

    // Early exits
    if point.Y < component.ScreenTop then Continue;
    if point.Y >= component.ScreenTop + component.Height then Continue;
    if point.X < component.ScreenLeft then Continue;
    if point.X >= component.ScreenLeft + component.Width then Continue;

    if point.X < component.ScreenLeft + Round(component.Width / 2) then begin
    	result := i;
      Exit;
    end;

    result := i + 1;
    Exit;
  end;	
end;


procedure TIconPanel.IconDragData_timer(Sender: TObject);
var mouse: TMouse;
  formMask: Graphics.TBitmap;
  rect: TRect;
  region: THandle;
  mouseOffset: TPoint;
  formCenter: Tpoint;
  indexUnderCursor: Integer;
  currentIndex: Integer;
  saveItem: TWComponent;
  componentUnderCursor: TWComponent;
begin
	mouse := TMouse.Create();

  if not pIconDragData.Started then begin
  	if PointDistance(mouse.CursorPos, pIconDragData.MouseDownLoc) >= 5 then begin
    	ilog('Starting to drag icon...');
    	pIconDragData.Started := true;

      if pIconDragData.IconForm = nil then begin
      	pIconDragData.IconForm := TForm.Create(self);
        pIconDragData.IconForm.Visible := false;
        pIconDragData.IconForm.BorderStyle := bsNone;
        pIconDragData.IconForm.OnPaint := iconForm_paint;
        SetTransparentForm(pIconDragData.IconForm.Handle, 100);
      end;

      if pIconDragData.InsertionCursor = nil then begin
      	pIconDragData.InsertionCursor := TWImage.Create(Owner);
        pIconDragData.InsertionCursor.FilePath := TMain.Instance.FilePaths.SkinDirectory + '\InsertionCursor.png';
        pIconDragData.InsertionCursor.StretchToFit := true;
        pIconDragData.InsertionCursor.MaintainAspectRatio := false;
        pIconDragData.InsertionCursor.FitToContent();
        pIconDragData.InsertionCursor.Height := pIconSize;
        AddChild(pIconDragData.InsertionCursor);
      end;

      pIconDragData.InsertionCursor.Visible := true;

      pIconDragData.Component.Visible := false;

      if pIconDragData.Component is TWFileIcon then begin

        formMask := Graphics.TBitmap.Create();

        try
          formMask.Width := pIconDragData.IconForm.Width;
          formMask.Height := pIconDragData.IconForm.Height;

          rect.Top := 0;
          rect.Left := 0;
          rect.Bottom := formMask.Height;
          rect.Right := formMask.Width;

          formMask.LoadFromFile(TMain.Instance.FilePaths.SkinDirectory + '/IconOverlayMask.bmp');

          region := CreateRegion(formMask);
          SetWindowRGN(pIconDragData.IconForm.Handle, region, True);
        finally
          formMask.Free();
        end;

      end;

      pIconDragData.IconForm.Width := pIconDragData.Component.Width;
    	pIconDragData.IconForm.Height := pIconDragData.Component.Height;

    end;
  end else begin

  	mouseOffset.X := mouse.CursorPos.X - pIconDragData.MouseDownLoc.X;
    mouseOffset.Y := mouse.CursorPos.Y - pIconDragData.MouseDownLoc.Y;

   	pIconDragData.IconForm.Left := pIconDragData.StartIconLoc.X + mouseOffset.X;
    pIconDragData.IconForm.Top := pIconDragData.StartIconLoc.Y + mouseOffset.Y;

    formCenter.X := pIconDragData.IconForm.Left + Round(pIconDragData.IconForm.Width / 2);
    formCenter.Y := pIconDragData.IconForm.Top + Round(pIconDragData.IconForm.Height / 2);

    indexUnderCursor := GetInsertionIndexAtPoint(formCenter);

    if indexUnderCursor < 0 then begin
    	componentUnderCursor := TWComponent(pComponents[0]);
      pIconDragData.InsertionCursor.Left := componentUnderCursor.Left;
    end else begin
    	if indexUnderCursor >= pComponents.Count then begin
        componentUnderCursor := TWComponent(pComponents[pComponents.Count - 1]);
        pIconDragData.InsertionCursor.Left := componentUnderCursor.Left + componentUnderCursor.Width;
      end else begin
        componentUnderCursor := TWComponent(pComponents[indexUnderCursor]);
        pIconDragData.InsertionCursor.Left := componentUnderCursor.Left;
    	end;
    end;

    pIconDragData.InsertionCursor.Top := componentUnderCursor.Top;

    pIconDragData.IconForm.Visible := true;

  end;

end;


procedure TIconPanel.IconForm_Paint(Sender: TObject);
var rect:TRect;
begin
	if pIconDragData.IconForm = nil then Exit;

  if pIconDragData.Component is TWFileIcon then begin
  	pIconDragData.IconForm.Canvas.Brush.Style := bsClear;
    TWFileIcon(pIconDragData.Component).DrawToCanvas(pIconDragData.IconForm.Canvas, 0, 0);
  end else begin
  	rect.Top := 0;
    rect.Left := 0;
    rect.Bottom := pIconDragData.IconForm.Height;
    rect.Right := pIconDragData.IconForm.Width;
    pIconDragData.IconForm.Canvas.StretchDraw(rect, TWImage(pIconDragData.Component).ImageObject);
  end;
end;


procedure TIconPanel.Icon_MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var screenMouseLoc: TPoint;
	component: TWComponent;
  mouse: TMouse;
  popupMenu: TPopupMenu;
  menuItem: TMenuItem;
  folderItem: TFolderItem;
begin
	component := Sender as TWComponent;

  mouse := TMouse.Create();

  screenMouseLoc.X := mouse.CursorPos.X;
  screenMouseLoc.Y := mouse.CursorPos.Y;

	if Button = mbRight then begin
    ilog('Showing icon popup menu...');

    popupMenu := CreateFormPopupMenu();

    menuItem := TMenuItem.Create(popupMenu);
    menuItem.Caption := '-';
    menuItem.Enabled := false;
    popupMenu.Items.Add(menuItem);

    menuItem := TMenuItem.Create(popupMenu);
    menuItem.Caption := TMain.Instance.Loc.GetString('IconPanel.PopupMenu.Delete');
    menuItem.Tag := component.ID;
    menuItem.OnClick := MenuItem_Click_Delete;
    popupMenu.Items.Add(menuItem);

    if component is TWFileIcon then begin
    	folderItem := TMain.Instance.User.GetFolderItemByID(component.Tag);

      menuItem := TMenuItem.Create(popupMenu);
      menuItem.Caption := '-';
      menuItem.Enabled := false;
      popupMenu.Items.Add(menuItem);

      menuItem := TMenuItem.Create(popupMenu);
      menuItem.Caption := TMain.Instance.Loc.GetString('IconPanel.PopupMenu.AddToQuickLaunch');
      menuItem.Tag := component.ID;
      menuItem.Checked := folderItem.QuickLaunch;
      menuItem.OnClick := MenuItem_Click_AddItToQuickLaunch;
      popupMenu.Items.Add(menuItem);

      menuItem := TMenuItem.Create(popupMenu);
      menuItem.Caption := TMain.Instance.Loc.GetString('IconPanel.PopupMenu.Properties');
      menuItem.Tag := component.ID;
      menuItem.OnClick := MenuItem_Click_Properties;
      popupMenu.Items.Add(menuItem);
    end;

		popupMenu.Popup(screenMouseLoc.X, screenMouseLoc.Y);
  end else begin
  	ilog('Initializing drag data...');

  	if pIconDragData.Timer = nil then begin
      pIconDragData.Timer := TTimer.Create(self);
      pIconDragData.Timer.Interval := 50;
      pIconDragData.Timer.OnTimer := iconDragData_timer;
    end;

    pIconDragData.MouseDownLoc := screenMouseLoc;
    pIconDragData.Component := component as TWComponent;
    pIconDragData.Started := false;
    pIconDragData.Timer.Enabled := true;

    pIconDragData.StartIconLoc := component.ScreenLoc;
  end;
end;


procedure TIconPanel.Icon_Click(sender: TObject);
var icon: TWFileIcon;
	folderItem: TFolderItem;
begin
  if pIconDragData.Started then Exit;

  pIconDragData.Timer.Enabled := false;

	icon := Sender as TWFileIcon;
  folderItem := TMain.Instance.User.GetFolderItemByID(icon.Tag);

  if folderItem = nil then begin
  	elog('No FolderItem with ID: ' + IntToStr(icon.Tag));
    Exit;
  end;

  ilog('Icon click: ' + folderItem.FilePath);

  folderItem.Launch(); 
end;


procedure TIconPanel.Icon_MouseEnter(sender: TObject);
var icon: TWFileIcon;
	folderItem: TFolderItem;
begin
	icon := TWFileIcon(sender);
  folderItem := TMain.Instance.User.GetFolderItemByID(icon.Tag);

  if pTooltipForm = nil then begin
  	pTooltipForm := TIconTooltipForm.Create(Self);
  end;
  pTooltipForm.ShowAbove(icon, folderItem.Name);
end;


procedure TIconPanel.Icon_MouseExit(sender: TObject);
begin
	pTooltipForm.Hide();
end;


procedure TIconPanel.Icon_MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var insertionIndex: Integer;
	formCenter: TPoint;
  i: Integer;
	component: TWComponent;
  added: Boolean;
  tempList: TObjectList;
begin
	if Button <> mbLeft then Exit;
  if not pIconDragData.Started then Exit;

  formCenter.X := pIconDragData.IconForm.Left + Round(pIconDragData.IconForm.Width / 2);
  formCenter.Y := pIconDragData.IconForm.Top + Round(pIconDragData.IconForm.Height / 2);

  insertionIndex := GetInsertionIndexAtPoint(formCenter);

  if insertionIndex >= 0 then begin

  	added := false;

    tempList := TObjectList.Create(false);

    for i := 0 to pComponents.Count - 1 do begin
      component := TWComponent(pComponents[i]);

      if component.ID = pIconDragData.Component.ID then Continue;

      if i = insertionIndex then begin
        tempList.Add(TObject(pIconDragData.Component));
        added := true;
      end;

      tempList.Add(pComponents[i]);
      
    end;

    if not added then tempList.Add(TObject(pIconDragData.Component));

    FreeAndNil(pComponents);

    pComponents := tempList;

    UpdateFolderItemsOrder();
    UpdateLayout();
  end;


  pIconDragData.Started := false;

  if pIconDragData.Timer <> nil then pIconDragData.Timer.Enabled := false;
  if pIconDragData.IconForm <> nil then FreeAndNil(pIconDragData.IconForm);
  if pIconDragData.Component <> nil then pIconDragData.Component.Visible := true;
  if pIconDragData.InsertionCursor <> nil then pIconDragData.InsertionCursor.Visible := false;

end;


procedure TIconPanel.UpdateFolderItemsOrder();
var folderItemIDs: Array of Integer;
	i: Integer;
begin
	SetLength(folderItemIDs, pComponents.Count);
	for i := 0 to pComponents.Count - 1 do begin
  	folderItemIDs[i] := TWComponent(pComponents[i]).Tag;
  end;
  TMain.Instance.User.ReorderAndDeleteFolderItems(folderItemIDs);
end;


procedure TIconPanel.LoadFolderItems();
var i: Integer;
  folderItem: TFolderItem;
  component: TWComponent;
begin
	ilog('Creating icons');

	for i := 0 to (pComponents.Count - 1) do begin
    component := TWComponent(pComponents.Items[i]);
    component.Free();
  end;

  pComponents.Clear();

  for i := 0 to TMain.instance.User.FolderItemCount - 1 do begin
  	folderItem := TMain.instance.User.getFolderItemAt(i);
    if folderItem.IsSeparator then Continue;

    component := FolderItemToComponent(folderItem);

		AddChild(component);
    pComponents.Add(TObject(component));
  end;
end;


procedure TIconPanel.FitToContent();
begin
  UpdateLayout();
  
end;


procedure TIconPanel.UpdateLayout;
var i: Integer;
  componentX, componentY: Integer;
  component: TWComponent;
  maxRight: Integer;
  lastIconBottom: Integer;
  lastIconRight: Integer;
  isWrapping: Boolean;
  folderItem: TFolderItem;
begin

	if pBrowseButton = nil then begin
    pBrowseButton := TWImageButton.Create(Owner);
    pBrowseButton.Visible := true;
    pBrowseButton.ImagePathPrefix := TMain.Instance.FilePaths.SkinDirectory + '\BrowseArrowButton';
    pBrowseButton.FitToContent();
    pBrowseButton.OnClick := BrowseButton_Click;
    AddChild(pBrowseButton);
  end;

	componentX := TMain.Instance.Style.barInnerPanel.paddingLeft;
  componentY := TMain.Instance.Style.barInnerPanel.paddingTop;

  pLastVisibleIconIndex := -1;

  isWrapping := false;

  maxRight := Width - TMain.Instance.Style.barInnerPanel.paddingRight;

	for i := 0 to pComponents.Count - 1 do begin
    component := TWComponent(pComponents[i]);

    if component is TWFileIcon then begin
      with TWFileIcon(component) do begin
      	if FilePath = '' then begin
          folderItem := TMain.Instance.User.GetFolderItemByID(Tag);
          FilePath := folderItem.ResolvedFilePath;
          OverlayImageUpPath := TMain.Instance.FilePaths.SkinDirectory + '\IconOverlayUp.png';
          OverlayImageDownPath := TMain.Instance.FilePaths.SkinDirectory + '\IconOverlayDown.png';
        end;
      end;
    end;

    component.Left := componentX;
    component.Top := componentY;
    component.Visible := true;

    if (componentX + component.Width >= maxRight) and (i <> 0) then begin
    	componentX := TMain.Instance.Style.barInnerPanel.paddingLeft;
      componentY := componentY + component.Height;
      component.Left := componentX;
      component.Top := componentY;
      isWrapping := true;
    end;

    componentX := component.Left + component.Width;

    if component.Top + component.Height > Height then begin
      if pLastVisibleIconIndex < 0 then pLastVisibleIconIndex := i - 1;
    	component.Visible := false;
    end;

    lastIconBottom := component.Top + component.Height;
    lastIconRight := component.Left + component.Width;
  end;


  if pLastVisibleIconIndex >= 0 then begin
    pBrowseButton.Visible := true;
    pBrowseButton.Left := Width - pBrowseButton.Width;
    pBrowseButton.Top := Height - pBrowseButton.Height;
  end else begin
  	pBrowseButton.Visible := false;
  end;

  MaxHeight := lastIconBottom + TMain.Instance.Style.barInnerPanel.paddingBottom;

//  if isWrapping then begin
//    MaxWidth := -1;
//  end else begin
//  	MaxWidth := lastIconRight + TMain.Instance.Style.barInnerPanel.paddingRight;
//  end;
  
end;

end.
