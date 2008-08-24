unit IconPanel;

interface

uses Windows, WNineSlicesPanel, Classes, WFileIcon, WImage, Logger, Contnrs, Controls,
	Types, WComponent, ExtCtrls, Forms, SysUtils, Graphics, MathUtils, Imaging,
  ShellAPI, WImageButton, Menus;


type

  TIconDragData = record
  	Icon: TWComponent;
  	Timer: TTimer;
    MouseDownLoc: TPoint;
    Started: Boolean;
    StartIconLoc: TPoint;
    IconForm: TForm;
  end;


	TIconPanel = class(TWNineSlicesPanel)


  	private

    	pIcons: TObjectList;
      pIconSize: Word;
      pIconDragData: TIconDragData;
      pAutoSize: Boolean;
      pBrowseButton: TWImageButton;
      pLastVisibleIconIndex: Integer;

      function GetInsertionIndexAtPoint(const aPoint: TPoint; replacementBehavior: Boolean):Integer;

      procedure iconDragData_timer(Sender: TObject);
      procedure icon_mouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      procedure icon_mouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      procedure icon_click(sender: TObject);
      procedure iconForm_paint(Sender: TObject);

      //procedure BrowseButton_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      procedure BrowseButton_Click(Sender: TObject);

  	public

    	constructor Create(AOwner: TComponent); override;
      procedure LoadFolderItems();
      function IconCount():Word;
      procedure UpdateLayout();


  end;


implementation


uses Main;


constructor TIconPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Width := 200;

  pLastVisibleIconIndex := -1;

  pAutoSize := not false;
  pIconSize := 40;
  ImagePathPrefix := TMain.instance.skinPath + '\BarInnerPanel';

  pIcons := TObjectList.Create();
  pIcons.OwnsObjects := false;
end;


procedure TIconPanel.iconForm_paint(Sender: TObject);
var rect:TRect;
begin
	if pIconDragData.IconForm = nil then Exit;

  if pIconDragData.Icon is TWFileIcon then begin
  	pIconDragData.IconForm.Canvas.Brush.Style := bsClear;
    TWFileIcon(pIconDragData.Icon).DrawToCanvas(pIconDragData.IconForm.Canvas, 0, 0);

		//pIconDragData.IconForm.Canvas.Draw(0, 0, TWFileIcon(pIconDragData.Icon).Icon);
  end else begin
  	rect.Top := 0;
    rect.Left := 0;
    rect.Bottom := pIconDragData.IconForm.Height;
    rect.Right := pIconDragData.IconForm.Width;
    pIconDragData.IconForm.Canvas.StretchDraw(rect, TWImage(pIconDragData.Icon).ImageObject);
  end;
end;


function TIconPanel.GetInsertionIndexAtPoint(const aPoint: TPoint; replacementBehavior: Boolean):Integer;
var p: TPoint;
	i :Integer;
	c: TWComponent;
begin
  p := ScreenToClient(aPoint);

  result := 0;

  for i := 0 to pIcons.Count - 1 do begin
    c := TWComponent(pIcons.Items[i]);

    if replacementBehavior then begin

    	if i = pIcons.Count - 1 then begin
      	result := i;
        break;
      end;

      if (p.X >= c.Left) and (p.X < (c.Left + c.Width)) then begin
        result := i;
        break;
      end;

    end else begin

      if (p.X >= c.Left) and (p.X < c.Left + c.Width / 2) then begin
        result := i;
        break;
      end else begin
      	if i = pIcons.Count - 1 then begin
        	result := i + 1;
          break;
        end else begin
          if (p.X >= c.Left + c.Width / 2) and (p.X < c.Left + c.Width) then begin
            result := i + 1;
            break;
          end;
        end;
      end;

    end;

  end;

  if not pAutoSize then
  	if result > pLastVisibleIconIndex then result := pLastVisibleIconIndex;

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

  for i := pLastVisibleIconIndex + 1 to pIcons.Count - 1 do begin
  	if i >= pIcons.Count then break;

    component := TWComponent(pIcons[i]);

    folderItem := TMain.Instance.GetFolderItemByID(component.Tag);

    menuItem := TMenuItem.Create(self);

    if folderItem.IsSeparator then begin
    	menuItem.Caption := '-';
      menuItem.Enabled := false;
    end else begin
    	menuItem.Caption := folderItem.Name;

      if folderItem.SmallIcon <> nil then begin
      	menuItemBitmap := TBitmap.Create();
        menuItemBitmap.Width := 16;
        menuItemBitmap.Height := 16;
        menuItemBitmap.Canvas.Draw(0, 0, folderItem.SmallIcon);
      	menuItem.Bitmap := menuItemBitmap;
      end;

    end;

    pPopupMenu.Items.Add(menuItem);
  end;

  mouse := TMouse.Create();

  pPopupMenu.Popup(mouse.CursorPos.X, mouse.CursorPos.Y);
end;


procedure TIconPanel.UpdateLayout();
var iconX, iconY: Word;
	i:Word;
  icon: TWFileIcon;
  iconMaxX, iconAreaWidth: Integer;
  folderItem: TFolderItem;
  component: TWComponent;
begin
  iconX := TMain.instance.style.barInnerPanel.paddingLeft;
  iconY := TMain.instance.style.barInnerPanel.paddingTop;

  if pBrowseButton = nil then begin
    pBrowseButton := TWImageButton.Create(Owner);
    pBrowseButton.Visible := true;
    pBrowseButton.ImagePathPrefix := TMain.instance.skinPath + '\BrowseArrowButton';
    pBrowseButton.FitToContent();
    pBrowseButton.OnClick := BrowseButton_Click;
    //pBrowseButton.OnMouseDown := BrowseButton_MouseDown;
    AddChild(pBrowseButton);
  end;

  for i := 0 to (pIcons.Count - 1) do begin
  	if (i >= pIcons.Count) then break;
    component := TWComponent(pIcons[i]);
    component.Visible := false;
  end;

  iconMaxX := 0;

  for i := 0 to (pIcons.Count - 1) do begin
  	if (i >= pIcons.Count) then break;

    icon := TWFileIcon(pIcons[i]);

  	icon.Left := iconX;
    icon.Top := iconY;

    if not pAutoSize then begin
    	if icon.Left + icon.Width >= Width - pBrowseButton.Width then begin
      	pLastVisibleIconIndex := i - 1;
      	break;
      end;
    end;

    if icon is TWFileIcon then begin
      if icon.FilePath = '' then begin
        folderItem := TMain.Instance.GetFolderItemByID(icon.Tag);
        icon.FilePath := folderItem.filePath;
        icon.OverlayImageUpPath := TMain.instance.skinPath + '\IconOverlayUp.png';
        icon.OverlayImageDownPath := TMain.instance.skinPath + '\IconOverlayDown.png';
      end;
    end;

    if (pIconDragData.Started) and (pIconDragData.Icon = TWComponent(icon)) then begin
    end else begin
    	icon.Visible := true;
    end;

    iconX := iconX + icon.Width;

    iconMaxX := icon.Left + icon.Width;

    if not pAutoSize then begin
      if iconMaxX > Width then break;
    end;
  end;


  if pAutoSize then pLastVisibleIconIndex := pIcons.Count - 1;


	iconAreaWidth := iconMaxX - TMain.instance.style.barInnerPanel.paddingLeft;

  if iconAreaWidth < 10 then iconAreaWidth := 10;

  if pAutoSize then
		Width := iconAreaWidth + TMain.instance.style.barInnerPanel.paddingH;
  Height := pIconSize + TMain.instance.style.barInnerPanel.paddingV;


  if not pAutoSize then begin
    pBrowseButton.Visible := true;
    pBrowseButton.Left := Width - pBrowseButton.Width;
    pBrowseButton.Top := Round(Height / 2 - pBrowseButton.Height / 2);
  end else begin
  	pBrowseButton.Visible := false;
  end;
end;


procedure TIconPanel.LoadFolderItems();
var i: Integer;
  folderItem: TFolderItem;
  icon: TWFileIcon;
  separatorImage: TWImage;
begin
	ilog('Creating icons');

	for i := 0 to (pIcons.Count - 1) do begin
  	if (i >= pIcons.Count) then break;
    icon := TWFileIcon(pIcons.Items[i]);
    icon.Free();
  end;

  pIcons.Clear();

  for i := 0 to TMain.instance.FolderItemCount - 1 do begin
  	folderItem := TMain.instance.getFolderItemAt(i);

    if not folderItem.IsSeparator then begin
    
      icon := TWFileIcon.Create(Owner);
      icon.Tag := folderItem.ID;
      icon.Width := pIconSize;
      icon.Height := pIconSize;
      icon.Visible := false;
      icon.OnClick := icon_click;
      icon.OnMouseDown := icon_mouseDown;
      icon.OnMouseUp := icon_mouseUp;

      AddChild(icon);

      pIcons.Add(TObject(icon));

    end else begin

      separatorImage := TWImage.Create(Owner);
      separatorImage.FilePath := TMain.instance.skinPath + '\InnerPanelSeparator.png';
      separatorImage.Visible := false;
      separatorImage.Tag := folderItem.ID;
      separatorImage.FitToContent();
      separatorImage.Height := pIconSize;
      separatorImage.StretchToFit := true;
      separatorImage.MaintainAspectRatio := false;
      separatorImage.OnMouseDown := icon_mouseDown;
      separatorImage.OnMouseUp := icon_mouseUp;

      AddChild(separatorImage);

      pIcons.Add(TObject(separatorImage));
    end;



  end;
end;



procedure TIconPanel.icon_mouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if pIconDragData.Timer <> nil then pIconDragData.Timer.Enabled := false;
  pIconDragData.Started := false;

  if pIconDragData.IconForm <> nil then FreeAndNil(pIconDragData.IconForm);

  pIconDragData.Icon.Visible := true;
end;


procedure TIconPanel.iconDragData_timer(Sender: TObject);
var mouse: TMouse;
  formMask: Graphics.TBitmap;
  rect: TRect;
  region: THandle;
  mouseOffset: TPoint;
  formCenter: Tpoint;
  indexUnderCursor: Integer;
  currentIndex: Integer;
  saveItem: TWComponent;
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
      end;

      pIconDragData.Icon.Visible := false;

      if pIconDragData.Icon is TWFileIcon then begin

        formMask := Graphics.TBitmap.Create();

        try
          formMask.Width := pIconDragData.IconForm.Width;
          formMask.Height := pIconDragData.IconForm.Height;

          rect.Top := 0;
          rect.Left := 0;
          rect.Bottom := formMask.Height;
          rect.Right := formMask.Width;

          formMask.LoadFromFile(TMain.Instance.skinPath + '/IconOverlayMask.bmp');

          region := CreateRegion(formMask);
          SetWindowRGN(pIconDragData.IconForm.Handle, region, True);
        finally
          formMask.Free();
        end;

      end;

      pIconDragData.IconForm.Width := pIconDragData.Icon.Width;
    	pIconDragData.IconForm.Height := pIconDragData.Icon.Height;

    end;
  end else begin

  	mouseOffset.X := mouse.CursorPos.X - pIconDragData.MouseDownLoc.X;
    mouseOffset.Y := mouse.CursorPos.Y - pIconDragData.MouseDownLoc.Y;

   	pIconDragData.IconForm.Left := pIconDragData.StartIconLoc.X + mouseOffset.X;
    pIconDragData.IconForm.Top := pIconDragData.StartIconLoc.Y + mouseOffset.Y;

    formCenter.X := pIconDragData.IconForm.Left + Round(pIconDragData.IconForm.Width / 2);
    formCenter.Y := pIconDragData.IconForm.Top + Round(pIconDragData.IconForm.Height / 2);

    if not (pIconDragData.Icon is TWFileIcon) then begin
    	formCenter.X := pIconDragData.IconForm.Left;
    end;


    indexUnderCursor := GetInsertionIndexAtPoint(formCenter, pIconDragData.Icon is TWFileIcon);
    currentIndex := pIcons.IndexOf(TObject(pIconDragData.Icon));
    
    if indexUnderCursor <> currentIndex then begin

    	if pIconDragData.Icon is TWFileIcon then begin

      	saveItem := TWComponent(pIcons.Items[indexUnderCursor]);
        pIcons.Items[indexUnderCursor] := TObject(pIconDragData.Icon);
        pIcons.Items[currentIndex] := saveItem;

      end else begin

      	if (indexUnderCursor >= pIcons.Count) and (currentIndex = pIcons.Count - 1) then begin

        end else begin

          pIcons.Remove(TObject(pIconDragData.Icon));

          if currentIndex > indexUnderCursor then begin
            pIcons.Insert(indexUnderCursor, TObject(pIconDragData.Icon));
          end else begin
            pIcons.Insert(indexUnderCursor-1, TObject(pIconDragData.Icon));
          end;

        end;

      end;
      
      UpdateLayout();
    end;



    pIconDragData.IconForm.Visible := true;

  end;

end;


procedure TIconPanel.icon_mouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var screenMouseLoc: TPoint;
	icon: TWComponent;
  mouse: TMouse;
begin
	icon := Sender as TWComponent;

  mouse := TMouse.Create();

  screenMouseLoc.X := mouse.CursorPos.X;
  screenMouseLoc.Y := mouse.CursorPos.Y;

	if Button = mbRight then begin
//    ilog('Showing icon popup menu...');
//
//    iconPopupMenu.Popup(screenMouseLoc.X, screenMouseLoc.Y);
  end else begin
  	ilog('Initializing drag data...');

  	if pIconDragData.Timer = nil then begin
      pIconDragData.Timer := TTimer.Create(self);
      pIconDragData.Timer.Interval := 50;
      pIconDragData.Timer.OnTimer := iconDragData_timer;
    end;

    pIconDragData.MouseDownLoc := screenMouseLoc;
    pIconDragData.Icon := icon as TWComponent;
    pIconDragData.Started := false;
    pIconDragData.Timer.Enabled := true;

    pIconDragData.StartIconLoc := icon.ScreenLoc;
  end;
end;


function TIconPanel.IconCount():Word;
begin
	result := pIcons.Count;
end;


procedure TIconPanel.icon_click(sender: TObject);
var icon: TWFileIcon;
	folderItem: TFolderItem;
  r: HINST;
begin
	icon := Sender as TWFileIcon;
  folderItem := TMain.Instance.GetFolderItemByID(icon.Tag);

  //ilog('Icon click: ' + folderItem.FilePath);

//  r := ShellExecute(Handle, 'open', PChar(folderItem.FilePath), nil, nil, SW_SHOWNORMAL) ;
//	if Integer(r) <= 32 then begin
//  	TMain.Instance.ErrorMessage(
//    	TMain.Instance.Loc.GetString('MainForm.LaunchFileError', IntToStr(r))
//    );
//  end;
end;





end.
