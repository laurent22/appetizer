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
#include "gui/TreeViewDialog.h"
#include "lua_glue/azIcon.h"




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

  iconOverlayPainterDown_ = NULL;
  iconOverlayPainterUp_ = NULL;
  multiLaunchIcon_ = NULL;
  popupMenu_ = NULL;
  folderItemId_ = -1;
  mouseInside_ = false;
  mousePressed_ = false;
  draggingStarted_ = false;
}


FolderItemRenderer::~FolderItemRenderer() {
  DeleteSkinObjects();
}


void FolderItemRenderer::DeleteSkinObjects() {
  wxDELETE(multiLaunchIcon_);
  wxDELETE(iconOverlayPainterUp_);
  wxDELETE(iconOverlayPainterDown_);
}


wxMenu* FolderItemRenderer::GetPopupMenu() {
  return popupMenu_;
}


void FolderItemRenderer::ApplySkin() {
  // All the renderer's assets are lazily created on UpdateControlBitmap()
  // so just reset everything here, and invalidate the bitmap.
  DeleteSkinObjects();
  InvalidateControlBitmap();
}


FolderItem* FolderItemRenderer::GetFolderItem() {
  FolderItem* output = FolderItem::GetFolderItemById(folderItemId_);
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

  wxString name = menuItem->GetMetadata(_("name"));

  if (name == _T("remove")) {

    wxGetApp().GetUtilities().RemoveFolderItemWithConfirmation(GetFolderItem());

  } else if (name == _T("properties")) {

    wxGetApp().GetUser()->EditFolderItem(GetFolderItem());

  } else if (name == _T("multiLaunch")) {

    FolderItem* folderItem = GetFolderItem();
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
  FolderItem* folderItem = GetFolderItem();
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
  table[_T("icon")] = new LuaHostTableItem(new azIcon(this), LHT_wxObject);
  
  wxGetApp().GetPluginManager()->DispatchEvent(&(wxGetApp()), _T("iconMenuOpening"), table);  

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
    FolderItem* folderItem = GetFolderItem();

    if (folderItem) {

      if (!folderItem->IsGroup()) {
        folderItem->Launch();
        wxGetApp().GetMainFrame()->DoAutoHide();
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

      FolderItem* folderItem = GetFolderItem();
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
  int iconSize = wxGetApp().GetUser()->GetSettings()->GetValidatedIconSize();
  SetSize(iconSize + Styles::Icon.Padding.Width,
          iconSize + Styles::Icon.Padding.Height);
}


void FolderItemRenderer::UpdateControlBitmap() {
  BitmapControl::UpdateControlBitmap();  

  FolderItem* folderItem = GetFolderItem();

  if (!folderItem) {
    ELOG(_T("FolderItemRenderer::UpdateControlBitmap: Folder item is null"));
    return;
  }

  UserSettings* userSettings = wxGetApp().GetUser()->GetSettings();
  int userSettingsIconSize = userSettings->GetValidatedIconSize();

  wxMemoryDC destDC;
  destDC.SelectObject(*controlBitmap_);

  if (mouseInside_) {
    // If the mouse is inside the control,
    // draw the icon overlay

    if (mousePressed_) { // DOWN state      
      if (!iconOverlayPainterDown_) {
        iconOverlayPainterDown_ = new NineSlicesPainter();
        iconOverlayPainterDown_->LoadImage(FilePaths::GetSkinDirectory() + _T("/IconOverlayDown.png"));
      }
      iconOverlayPainterDown_->Draw(&destDC, 0, 0, GetClientRect().GetWidth(), GetClientRect().GetHeight());
    } else { // UP state      
      if (!iconOverlayPainterUp_) {
        iconOverlayPainterUp_ = new NineSlicesPainter();
        iconOverlayPainterUp_->LoadImage(FilePaths::GetSkinDirectory() + _T("/IconOverlayUp.png"));
      }
      iconOverlayPainterUp_->Draw(&destDC, 0, 0, GetClientRect().GetWidth(), GetClientRect().GetHeight());
    }

  }

  // Get the icon from the folder item
  wxIcon* icon = folderItem->GetIcon(userSettingsIconSize);
  wxASSERT_MSG(icon, _T("Folder item icon cannot be NULL"));

  if (icon && icon->IsOk()) {  

    // The commented code below converts the icon to a usable wxImage object
    //
    //wxImage image;
    //
    //if (wxBitmap(*icon).GetMask()) {
    //  wxBitmap* tempBitmap = Imaging::IconToBitmapWithAlpha(*icon);      
    //  image = tempBitmap->ConvertToImage();
    //  wxDELETE(tempBitmap);
    //} else {
    //  image = wxBitmap(*icon).ConvertToImage();
    //}
    //destDC.DrawBitmap(wxBitmap(image), Styles::Icon.Padding.Left, Styles::Icon.Padding.Top);   

    int x = (GetSize().GetWidth() - icon->GetWidth()) / 2;
    int y = (GetSize().GetHeight() - icon->GetHeight()) / 2;

    Imaging::DrawIconWithTransparency(&destDC, *icon, x, y);
  }

  if (folderItem->BelongsToMultiLaunchGroup()) {
    if (!multiLaunchIcon_) {
      multiLaunchIcon_ = new wxBitmap(FilePaths::GetSkinFile(_T("/MultiLaunchIcon.png")), wxBITMAP_TYPE_PNG);
    }
    if (multiLaunchIcon_->IsOk()) {
      destDC.DrawBitmap(
        *multiLaunchIcon_,
        Styles::Icon.Padding.Left + userSettingsIconSize / 2 - multiLaunchIcon_->GetWidth() / 2,
        Styles::Icon.Padding.Top + userSettingsIconSize - multiLaunchIcon_->GetHeight() / 2);
    }
  }

  destDC.SelectObject(wxNullBitmap);

  SetToolTip(folderItem->GetName(true));
}


void FolderItemRenderer::LoadData(int folderItemId) {  
  folderItemId_ = folderItemId;  

  InvalidateControlBitmap();
}