/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "stdafx.h"

#include "FolderItemRenderer.h"
#include "FolderItem.h"
#include "Log.h"
#include "utilities/IconGetter.h"
#include "imaging/Imaging.h"
#include "FilePaths.h"
#include "Styles.h"
#include "MiniLaunchBar.h"
#include "Enumerations.h"
#include "MessageBoxes.h"
#include "gui/TreeViewDialog.h"


int FolderItemRenderer::uniqueID_ = 0;


BEGIN_EVENT_TABLE(FolderItemRenderer, BitmapControl)
  EVT_MOTION(FolderItemRenderer::OnMotion)
  EVT_ENTER_WINDOW(FolderItemRenderer::OnEnterWindow)
  EVT_LEAVE_WINDOW(FolderItemRenderer::OnLeaveWindow)
  EVT_LEFT_DOWN(FolderItemRenderer::OnLeftDown)
  EVT_LEFT_UP(FolderItemRenderer::OnLeftUp)
  EVT_RIGHT_DOWN(FolderItemRenderer::OnRightDown)
  EVT_MENU(ID_MENU_Delete, FolderItemRenderer::OnMenuDelete)
  EVT_MENU(ID_MENU_Properties, FolderItemRenderer::OnMenuProperties)
  EVT_MENU(ID_MENU_AddToMultiLaunch, FolderItemRenderer::OnMenuAddToMultiLaunch)
  EVT_MENU(ID_MENU_EditShortcutGroup, FolderItemRenderer::OnMenuEditShortcutGroup)
  EVT_MOUSE_CAPTURE_LOST(FolderItemRenderer::OnMouseCaptureLost)
END_EVENT_TABLE()


FolderItemRenderer::FolderItemRenderer(wxWindow *owner, int id, wxPoint point, wxSize size):
BitmapControl(owner, id, point, size) {

  // Make sure that each renderer has a unique ID
  FolderItemRenderer::uniqueID_++;
  SetId(FolderItemRenderer::uniqueID_);

  folderItemId_ = -1;
  mouseInside_ = false;
  mousePressed_ = false;
  draggingStarted_ = false;
}


FolderItemRenderer::~FolderItemRenderer() {

}


void FolderItemRenderer::ApplySkin() {
  // All the renderer's assets are lazily created on UpdateControlBitmap()
  // so just reset everything here, and invalidate the bitmap.
  iconOverlayPainterDown_.reset();
  iconOverlayPainterUp_.reset();
  multiLaunchIcon_.reset();
  InvalidateControlBitmap();
}


FolderItemSP FolderItemRenderer::GetFolderItem() {
  return wxGetApp().GetUser()->GetRootFolderItem()->GetChildById(folderItemId_);
}


void FolderItemRenderer::OnMenuEditShortcutGroup(wxCommandEvent& evt) {
  wxGetApp().GetUtilities().ShowTreeViewDialog(GetFolderItem()->GetId());
}


void FolderItemRenderer::OnMenuAddToMultiLaunch(wxCommandEvent& evt) {
  FolderItemSP folderItem = GetFolderItem();
  if (!folderItem.get()) return;
  
  if (folderItem->BelongsToMultiLaunchGroup()) {
    GetFolderItem()->RemoveFromMultiLaunchGroup();
  } else {
    GetFolderItem()->AddToMultiLaunchGroup();
  }

  wxGetApp().FolderItems_FolderItemChange(folderItem);
}


void FolderItemRenderer::OnMenuDelete(wxCommandEvent& evt) {
  if (wxGetApp().GetUser()->GetSettings()->ShowDeleteIconMessage) {
    int result = MessageBoxes::ShowConfirmation(_("Do you wish to remove this icon?"), wxYES | wxNO, _("Don't show this message again"), false);
    if (!result) return;

    wxGetApp().GetUser()->GetSettings()->ShowDeleteIconMessage = !MessageBoxes::GetCheckBoxState();
    wxGetApp().GetUser()->ScheduleSave();
    if (result != wxID_YES) return;
  }

  FolderItemSP folderItem = GetFolderItem();
  FolderItem* parent = folderItem->GetParent();
  if (!parent) return;

  parent->RemoveChild(GetFolderItem());
}


void FolderItemRenderer::OnMenuProperties(wxCommandEvent& evt) {
  wxGetApp().GetUser()->EditFolderItem(GetFolderItem());
}


void FolderItemRenderer::OnRightDown(wxMouseEvent& evt) {
  FolderItemSP folderItem = GetFolderItem();

  wxMenu* menu = wxGetApp().GetMainFrame()->GetIconPanel()->GetContextMenu();

  menu->AppendSeparator();
  menu->Append(ID_MENU_Delete, _("Remove..."));

  if (!folderItem->IsGroup()) {
    wxMenuItem* menuItem = new wxMenuItem(menu, ID_MENU_AddToMultiLaunch, _("Add to Multi Launch group"), wxEmptyString, wxITEM_CHECK);
    menu->Append(menuItem);
    menuItem->Check(GetFolderItem()->BelongsToMultiLaunchGroup());
  }

  menu->AppendSeparator();

  if (folderItem->IsGroup()) {
    wxMenuItem* menuItem = new wxMenuItem(menu, ID_MENU_EditShortcutGroup, _("Organize group shortcuts"));
    menu->Append(menuItem);
  }

  menu->Append(ID_MENU_Properties, _("Properties"));

  PopupMenu(menu, wxDefaultPosition);

  wxDELETE(menu);
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
    FolderItemSP folderItem = GetFolderItem();

    if (folderItem.get()) {

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

      FolderItemSP folderItem = GetFolderItem();
      if (!folderItem.get()) return;

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

  } else { // Folder item is being dragged

  }

}


void FolderItemRenderer::FitToContent() {
  SetSize(wxGetApp().GetUser()->GetSettings()->GetValidatedIconSize() + Styles::Icon.Padding.Width,
          wxGetApp().GetUser()->GetSettings()->GetValidatedIconSize() + Styles::Icon.Padding.Height);
}


void FolderItemRenderer::UpdateControlBitmap() {
  BitmapControl::UpdateControlBitmap();  

  FolderItemSP folderItem = GetFolderItem();

  if (!folderItem.get()) {
    elog("FolderItemRenderer::UpdateControlBitmap: Folder item is null");
    return;
  }

  UserSettingsSP userSettings = wxGetApp().GetUser()->GetSettings();
  int userSettingsIconSize = userSettings->GetValidatedIconSize();

  wxMemoryDC destDC;
  destDC.SelectObject(*controlBitmap_);

  if (mouseInside_) {
    // If the mouse is inside the control,
    // draw the icon overlay

    if (mousePressed_) { // DOWN state      
      if (!iconOverlayPainterDown_.get()) {
        iconOverlayPainterDown_.reset(new NineSlicesPainter());
        iconOverlayPainterDown_->LoadImage(FilePaths::GetSkinDirectory() + _T("/IconOverlayDown.png"));
      }
      iconOverlayPainterDown_->Draw(&destDC, 0, 0, GetClientRect().GetWidth(), GetClientRect().GetHeight());
    } else { // UP state      
      if (!iconOverlayPainterUp_.get()) {
        iconOverlayPainterUp_.reset(new NineSlicesPainter());
        iconOverlayPainterUp_->LoadImage(FilePaths::GetSkinDirectory() + _T("/IconOverlayUp.png"));
      }
      iconOverlayPainterUp_->Draw(&destDC, 0, 0, GetClientRect().GetWidth(), GetClientRect().GetHeight());
    }

  }

  // Get the icon from the folder item
  wxIconSP icon = folderItem->GetIcon(userSettingsIconSize);
  wxASSERT_MSG(icon, _T("Folder item icon cannot be NULL"));

  if (icon->IsOk()) {  

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

    Imaging::DrawIconWithTransparency(&destDC, *icon, Styles::Icon.Padding.Left, Styles::Icon.Padding.Top);
  }

  if (folderItem->BelongsToMultiLaunchGroup()) {
    if (!multiLaunchIcon_.get()) {
      multiLaunchIcon_.reset(new wxBitmap(FilePaths::GetSkinDirectory() + _T("/MultiLaunchIcon.png"), wxBITMAP_TYPE_PNG));
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