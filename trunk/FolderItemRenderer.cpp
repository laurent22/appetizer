/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "stdafx.h"

#include "FolderItemRenderer.h"
#include "FolderItem.h"
#include "utilities/IconGetter.h"
#include "imaging/Imaging.h"
#include "FilePaths.h"
#include "Styles.h"
#include "MiniLaunchBar.h"
#include "Enumerations.h"
#include "MessageBoxes.h"
#include "ExtendedMenuItem.h"
#include "IconPanel.h"
#include "gui/TreeViewDialog.h"





int FolderItemRenderer::uniqueID_ = 0;
int FolderItemRenderer::addToGroupMenuItemOffset_ = 10000;



BEGIN_EVENT_TABLE(FolderItemRenderer, BitmapControl)
  EVT_MOTION(FolderItemRenderer::OnMotion)
  EVT_ENTER_WINDOW(FolderItemRenderer::OnEnterWindow)
  EVT_LEAVE_WINDOW(FolderItemRenderer::OnLeaveWindow)
  EVT_LEFT_DOWN(FolderItemRenderer::OnLeftDown)
  EVT_LEFT_UP(FolderItemRenderer::OnLeftUp)
  EVT_RIGHT_DOWN(FolderItemRenderer::OnRightDown)
  EVT_MENU(wxID_ANY, FolderItemRenderer::OnMenuItemClick)
  EVT_MOUSE_CAPTURE_LOST(FolderItemRenderer::OnMouseCaptureLost)
END_EVENT_TABLE()


FolderItemRenderer::FolderItemRenderer(wxWindow *owner, int id, wxPoint point, wxSize size):
BitmapControl(owner, id, point, size) {

  // Make sure that each renderer has a unique ID
  FolderItemRenderer::uniqueID_++;
  SetId(FolderItemRenderer::uniqueID_);
  
  labelFont_ = NULL;
  label_ = NULL;

  iconOverlayPainterDown_ = NULL;
  iconOverlayPainterUp_ = NULL;
  iconOverlayPainterInactive_ = NULL;
  multiLaunchIcon_ = NULL;
  popupMenu_ = NULL;
  folderItemId_ = -1;
  mouseInside_ = false;
  mousePressed_ = false;
  draggingStarted_ = false;
  selected_ = false;
}


bool FolderItemRenderer::GetSelected() {
  return selected_;
}


void FolderItemRenderer::SetSelected(bool selected) {
  selected_ = selected;
  InvalidateControlBitmap();
}


int FolderItemRenderer::GetIconSize() {
  IconPanel* parent = dynamic_cast<IconPanel*>(GetParent());
  return parent->GetIconSize();
}


wxString FolderItemRenderer::GetLabelPosition() {
  IconPanel* parent = dynamic_cast<IconPanel*>(GetParent());
  return parent->GetLabelPosition();
}


void FolderItemRenderer::UpdateInnerLabel() {
  if (!label_) return;

  appFolderItem* folderItem = GetFolderItem();

  if (!labelFont_) {
    labelFont_ = wxFont::New(Styles::Font.Size, wxFONTFAMILY_DEFAULT, Styles::Font.Weight == wxBOLD ? wxFONTFLAG_BOLD : wxFONTFLAG_DEFAULT, Styles::Font.Face, wxFONTENCODING_DEFAULT);
  }

  label_->SetFont(*labelFont_);

  wxString labelText = folderItem ? folderItem->GetName() : _T("");
  label_->SetLabel(labelText);

  int maxWidth = GetSize().GetWidth() - Styles::Icon.Padding.Width;

  if (GetLabelPosition() == _T("right")) {
    maxWidth = RIGHT_ICON_LABEL_WIDTH;
  }

  while (label_->GetBestSize().GetWidth() > maxWidth) {
    if (labelText.Length() == 0) break;

    labelText = labelText.Mid(0, labelText.Length() - 1);
    label_->SetLabel(labelText + _T("..."));
  }  
}


wxStaticText* FolderItemRenderer::GetLabel() {
  if (!label_) {
    label_ = new wxStaticText(this, wxID_ANY, _T("m")); 
    label_->Hide();
  }
  
  return label_;
}


FolderItemRenderer::~FolderItemRenderer() {
  DeleteSkinObjects();
  wxDELETE(label_);
  wxDELETE(labelFont_);
}


void FolderItemRenderer::DeleteSkinObjects() {
  wxDELETE(multiLaunchIcon_);
  wxDELETE(iconOverlayPainterUp_);
  wxDELETE(iconOverlayPainterDown_);
  wxDELETE(iconOverlayPainterInactive_);
}


wxMenu* FolderItemRenderer::GetPopupMenu() {
  return popupMenu_;
}


void FolderItemRenderer::ApplySkin() {
  // All the renderer's assets are lazily created on UpdateControlBitmap()
  // so just reset everything here, and invalidate the bitmap.
  wxDELETE(labelFont_);
  DeleteSkinObjects();
  InvalidateControlBitmap();
}


appFolderItem* FolderItemRenderer::GetFolderItem() {
  appFolderItem* output = appFolderItem::GetFolderItemById(folderItemId_);
  if (!output) return NULL;
  if (!output->GetParent()) return NULL;
  return output;
}


void FolderItemRenderer::OnMenuItemClick(wxCommandEvent& evt) {
  ExtendedMenuItem* menuItem = GetClickedMenuItem(evt);
  if (!menuItem) return;

  wxMenu* menu = dynamic_cast<wxMenu*>(evt.GetEventObject());

  bool handled = wxGetApp().GetPluginManager()->HandleMenuItemClick(menuItem);
  
  if (handled) {
    evt.Skip();
    return;
  }

  wxString name = menuItem->GetMetadata(_T("name"));

  if (name == _T("remove")) {

    wxGetApp().GetUtilities().RemoveFolderItemWithConfirmation(GetFolderItem());

  } else if (name == _T("properties")) {

    wxGetApp().GetUser()->EditFolderItem(GetFolderItem());

  } else if (name == _T("multiLaunch")) {

    appFolderItem* folderItem = GetFolderItem();
    if (!folderItem) return;
    
    if (folderItem->BelongsToMultiLaunchGroup()) {
      GetFolderItem()->RemoveFromMultiLaunchGroup();
    } else {
      GetFolderItem()->AddToMultiLaunchGroup();
    }

    wxGetApp().FolderItems_FolderItemChange(folderItem);

  } else if (name == _T("organizeGroup")) {

    wxGetApp().GetUtilities().ShowTreeViewDialog(GetFolderItem()->GetId());

  } else {

    evt.Skip();

  }

}


void FolderItemRenderer::OnRightDown(wxMouseEvent& evt) {
  appFolderItem* folderItem = GetFolderItem();
  if (!folderItem) return;

  wxMenu* menu = wxGetApp().GetMainFrame()->GetIconPanel()->GetContextMenu();
  popupMenu_ = menu;

  menu->AppendSeparator();

  ExtendedMenuItem* menuItem = NULL;
  
  menuItem = new ExtendedMenuItem(menu, wxGetApp().GetUniqueInt(), _("Remove..."));
  menuItem->SetMetadata(_T("name"), _T("remove"));
  menu->Append(menuItem);

  if (!folderItem->IsGroup()) {
    menuItem = new ExtendedMenuItem(menu, wxGetApp().GetUniqueInt(), _("Add to Multi Launch group"), wxEmptyString, wxITEM_CHECK);
    menuItem->SetMetadata(_T("name"), _T("multiLaunch"));
    menu->Append(menuItem);
    menuItem->Check(GetFolderItem()->BelongsToMultiLaunchGroup());
  }

  menu->AppendSeparator();

  if (folderItem->IsGroup()) {
    menuItem = new ExtendedMenuItem(menu, wxGetApp().GetUniqueInt(), _("Organize group shortcuts"));
    menuItem->SetMetadata(_T("name"), _T("organizeGroup"));
    menu->Append(menuItem);
  }

  menuItem = new ExtendedMenuItem(menu, wxGetApp().GetUniqueInt(), _("Properties"));
  menuItem->SetMetadata(_T("name"), _T("properties"));
  menu->Append(menuItem);

  LuaHostTable table;
  table[_T("dockItem")] = new LuaHostTableItem(folderItem, LHT_wxObject);
  table[_T("menu")] = new LuaHostTableItem(menu, LHT_wxObject);
  
  wxGetApp().GetPluginManager()->DispatchEvent(&(wxGetApp()), _T("dockItemMenuOpening"), table);  

  PopupMenu(menu, wxDefaultPosition);

  wxDELETE(menu);
  popupMenu_ = NULL;
}


void FolderItemRenderer::OnEnterWindow(wxMouseEvent& evt) {
  mouseInside_ = true;
  InvalidateControlBitmap();
}


void FolderItemRenderer::OnLeaveWindow(wxMouseEvent& evt) {
  mouseInside_ = false;
  InvalidateControlBitmap();
}


void FolderItemRenderer::OnMouseCaptureLost(wxMouseCaptureLostEvent& evt) {
  // Any MSW application that uses wxWindow::CaptureMouse() must implement an 
  // wxEVT_MOUSE_CAPTURE_LOST event handler as of wxWidgets 2.8.0.
  if (HasCapture()) ReleaseMouse();
}


void FolderItemRenderer::OnLeftDown(wxMouseEvent& evt) {
  if (!HasCapture()) CaptureMouse();

  mousePressed_ = true;
  draggingStarted_ = false;

  pressPosition_.x = evt.m_x;
  pressPosition_.y = evt.m_y;

  InvalidateControlBitmap();
}


void FolderItemRenderer::OnLeftUp(wxMouseEvent& evt) {
  if (HasCapture()) ReleaseMouse();  

  if (mouseInside_ && mousePressed_) {    
    appFolderItem* folderItem = GetFolderItem();

    if (folderItem) {

      if (!folderItem->IsGroup()) {
        folderItem->Launch();
        wxGetApp().GetMainFrame()->DoAutoHide();
      
        LuaHostTable table;
        table[_T("dockItem")] = new LuaHostTableItem(folderItem, LHT_wxObject);
        wxGetApp().GetPluginManager()->DispatchEvent(&(wxGetApp()), _T("dockItemClick"), table);  

      } else {
        wxMenu* menu = folderItem->ToMenu();
        PopupMenu(menu, wxPoint(0, GetSize().GetHeight()));
        wxDELETE(menu);
      }

    }

  }

  mousePressed_ = false;

  InvalidateControlBitmap();
}


void FolderItemRenderer::OnMotion(wxMouseEvent& evt) {
  if (!mousePressed_) return;

  if (!draggingStarted_) {
    wxPoint p(evt.m_x, evt.m_y);
    double distance = MathUtil::GetPointDistance(p, pressPosition_);

    if (distance > 5.0) {
      if (HasCapture()) ReleaseMouse(); 
      draggingStarted_ = true;

      appFolderItem* folderItem = GetFolderItem();
      if (!folderItem) return;

      // Tell the main controller that we've started dragging
      // a folder item. Other objects can then do GetDraggedFolderItem()
      // to know if a folder item is being dragged.
      wxGetApp().SetDraggedFolderItem(folderItem->GetId());

      wxFileDataObject fileData;
      fileData.AddFile(folderItem->GetResolvedPath());

      wxDropSource dragSource(this);
      dragSource.SetData(fileData);
      wxDragResult result = dragSource.DoDragDrop(true);

      // Tell the main controller that we've finished dragging
      // the folder item
      wxGetApp().SetDraggedFolderItem(-1);
      mousePressed_ = false;
      draggingStarted_ = false;

      InvalidateControlBitmap();
    }

  } 

}


void FolderItemRenderer::FitToContent() {
  int iconSize = GetIconSize();
  wxString labelPosition = GetLabelPosition();

  wxSize s = wxGetApp().GetIconAreaSize(iconSize, labelPosition);

  SetSize(s.GetWidth(), s.GetHeight());
}


void FolderItemRenderer::FT_DrawTextToImage(wxImage& image, FT_Bitmap &fontBitmap, int targetX, int targetY, wxColor& color, float alpha) {
  int labelColorR = color.Red();
  int labelColorG = color.Green();
  int labelColorB = color.Blue();

  for (int x = 0; x < fontBitmap.width; x++) {
		for (int y = 0; y < fontBitmap.rows; y++) {
			int bitmapIndex = x + y * fontBitmap.pitch;
			int color = fontBitmap.buffer[bitmapIndex];

      int destX = targetX + x;
      int destY = targetY + y;
      if (destX > image.GetWidth() - 1) continue;
      if (destY > image.GetHeight() - 1) continue;
      if (destX < 0) continue;
      if (destY < 0) continue;

      int oldAlpha = image.GetAlpha(destX, destY);

      if (oldAlpha == 0) {

        image.SetRGB(destX, destY, labelColorR,labelColorG,labelColorB);      
        image.SetAlpha(destX, destY, floor((float)color * alpha));

      } else {

        float p = (float)color / 255.0;

        int r = floor(((1.0 - p) * (float)image.GetRed(destX, destY)) + p * (float)labelColorR);
        int g = floor(((1.0 - p) * (float)image.GetGreen(destX, destY)) + p * (float)labelColorG);
        int b = floor(((1.0 - p) * (float)image.GetBlue(destX, destY)) + p * (float)labelColorB);

        int newAlpha = floor((float)color * alpha) + oldAlpha;
        if (newAlpha > 255) newAlpha = 255;
        
        image.SetAlpha(destX, destY, newAlpha);
        image.SetRGB(destX, destY, r, g, b);

      }

		}
	}
}




void FolderItemRenderer::UpdateControlBitmap() {
  BitmapControl::UpdateControlBitmap();  

  appFolderItem* folderItem = GetFolderItem();

  if (!folderItem) {
    ELOG(_T("FolderItemRenderer::UpdateControlBitmap: Folder item is null"));
    return;
  }

  UserSettings* userSettings = wxGetApp().GetUser()->GetSettings();
  int userSettingsIconSize = GetIconSize();

  wxMemoryDC destDC;
  destDC.SelectObject(*controlBitmap_);

  wxString labelPosition = GetLabelPosition();


  if (mouseInside_ || selected_) {
    // If the mouse is inside the control,
    // draw the icon overlay

    if (mousePressed_) { // DOWN state      
      if (!iconOverlayPainterDown_) {
        iconOverlayPainterDown_ = new NineSlicesPainter();
        iconOverlayPainterDown_->LoadImage(FilePaths::GetSkinFile(_T("/IconOverlayDown.png")));
      }
      iconOverlayPainterDown_->Draw(&destDC, 0, 0, GetClientRect().GetWidth(), GetClientRect().GetHeight());
    } else { // UP state      
      if (!iconOverlayPainterUp_) {
        iconOverlayPainterUp_ = new NineSlicesPainter();
        iconOverlayPainterUp_->LoadImage(FilePaths::GetSkinFile(_T("/IconOverlayUp.png")));
      }
      iconOverlayPainterUp_->Draw(&destDC, 0, 0, GetClientRect().GetWidth(), GetClientRect().GetHeight());
    }

  } else {

    if (!iconOverlayPainterInactive_) {
      iconOverlayPainterInactive_ = new NineSlicesPainter();
      iconOverlayPainterInactive_->LoadImage(FilePaths::GetSkinFile(_T("IconOverlayInactive.png")));
    }

    iconOverlayPainterInactive_->Draw(&destDC, 0, 0, GetClientRect().GetWidth(), GetClientRect().GetHeight());

  }

  // Get the icon from the folder item
  wxIcon* icon = folderItem->GetIcon(userSettingsIconSize);
  wxASSERT_MSG(icon, _T("Folder item icon cannot be NULL"));

  int drawnSize = userSettingsIconSize;
  if (mouseInside_) drawnSize = userSettingsIconSize;

  if (icon && icon->IsOk()) {  

    int x = (GetSize().GetWidth() - drawnSize) / 2;
    if (labelPosition == _T("right")) x = Styles::Icon.Padding.Left;
    int y = Styles::Icon.Padding.Top;

    if (Imaging::IsBadIcon(*icon)) {
      wxBitmap* bitmap = Imaging::IconToBitmapWithAlpha(*icon);
      Imaging::StretchDrawBitmap(&destDC, *bitmap, x, y, drawnSize, drawnSize);
      wxDELETE(bitmap);
    } else {
      Imaging::StretchDrawIcon(&destDC, *icon, x, y, drawnSize, drawnSize);
    }

  }

  if (folderItem->BelongsToMultiLaunchGroup()) {
    if (!multiLaunchIcon_) {
      multiLaunchIcon_ = new wxBitmap(FilePaths::GetSkinFile(_T("/MultiLaunchIcon.png")), wxBITMAP_TYPE_PNG);
    }
    if (multiLaunchIcon_->IsOk()) {
      destDC.DrawBitmap(
        *multiLaunchIcon_,
        Styles::Icon.Padding.Left + userSettingsIconSize / 2 - multiLaunchIcon_->GetWidth() / 2,
        Styles::Icon.Padding.Top + userSettingsIconSize - multiLaunchIcon_->GetHeight() / 2
      );
    }
  }
  

  if (labelPosition != _T("hidden")) {

    wxStaticText* label = GetLabel();
    UpdateInnerLabel();

    int labelWidth = GetSize().GetWidth();
    int labelHeight = label->GetBestSize().GetHeight();

    if (labelPosition == _T("right")) labelWidth = RIGHT_ICON_LABEL_WIDTH;

    FT_Error error;

    FT_Library ftLibrary = wxGetApp().GetFreeTypeLibrary();
    FT_Face ftFace = wxGetApp().GetFreeTypeFace();

    if (ftLibrary && ftFace) {

      error = FT_Set_Char_Size(ftFace, 0, Styles::Font.Size * 64, 96,96);
      FT_GlyphSlot slot = ftFace->glyph;

      int pen_x = 0;
      int pen_y = labelHeight - floor(Styles::Font.Size / 2.0);

      wxImage targetImage(labelWidth > GetSize().GetWidth() ? GetSize().GetWidth() : labelWidth, labelHeight);
      targetImage.InitAlpha();
      for (int x = 0; x < targetImage.GetWidth(); x++) {
        for (int y = 0; y < targetImage.GetHeight(); y++) {
          targetImage.SetAlpha(x, y, 0);
        }
      }      

      wxColor labelColor = Styles::Font.Color;

      for ( int n = 0; n < label->GetLabel().Length(); n++ ) {
        FT_ULong c = label->GetLabel()[n];
        if (c == 0) break;

        /* retrieve glyph index from character code */ 
	      FT_UInt glyph_index; 
	      glyph_index = FT_Get_Char_Index(ftFace, c); /* load glyph image into the slot (erase previous one) */ 
	      error = FT_Load_Glyph(ftFace, glyph_index, FT_LOAD_DEFAULT);
	      if (error) continue; /* ignore errors */

	      /* convert to an anti-aliased bitmap */ 
	      error = FT_Render_Glyph(ftFace->glyph, FT_RENDER_MODE_NORMAL);
	      if (error) continue;

	      /* now, draw to our target surface */ 
        FT_DrawTextToImage(targetImage, slot->bitmap, pen_x + slot->bitmap_left, pen_y - slot->bitmap_top + Styles::Font.ShadowOffset, Styles::Font.ShadowColor, Styles::Font.ShadowAlpha);
        FT_DrawTextToImage(targetImage, slot->bitmap, pen_x + slot->bitmap_left, pen_y - slot->bitmap_top, labelColor);

	      /* increment pen position */ 
	      pen_x += slot->advance.x >> 6;
	      pen_y += slot->advance.y >> 6; /* not useful for now */ 
      } 

      int destX = (GetSize().GetWidth() - pen_x) / 2;
      int destY = userSettingsIconSize + Styles::Icon.LabelGap;

      if (labelPosition == _T("right")) {
        destX = Styles::Icon.Padding.Left + userSettingsIconSize + Styles::Icon.LabelGap;
        destY = ((Styles::Icon.Padding.Height + userSettingsIconSize) - labelHeight) / 2;
      }

      destDC.DrawBitmap(wxBitmap(targetImage), destX, destY);

    }

  }

  destDC.SelectObject(wxNullBitmap);

  SetToolTip(folderItem->GetName(true));

}


void FolderItemRenderer::LoadData(int folderItemId) {  
  folderItemId_ = folderItemId;  
  wxDELETE(labelFont_);

  InvalidateControlBitmap();
}