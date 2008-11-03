/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "FolderItemRenderer.h"
#include "FolderItem.h"
#include "Log.h"
#include "utilities/IconGetter.h"
#include "imaging/Imaging.h"
#include <wx/log.h>
#include <wx/menu.h>
#include "FilePaths.h"
#include "Localization.h"
#include "Styles.h"
#include "MainFrame.h"
#include "Enumerations.h"
#include "Controller.h"
#include "MessageBoxes.h"
#include "IconPanelFrame.h"
#include "gui/TreeViewDialog.h"


extern Controller gController;
extern MainFrame* gMainFrame;


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


FolderItemSP FolderItemRenderer::GetFolderItem() {
  return gController.GetUser()->GetRootFolderItem()->GetChildById(folderItemId_);
}


void FolderItemRenderer::OnMenuAddToMultiLaunch(wxCommandEvent& evt) {
  FolderItemSP folderItem = GetFolderItem();
  if (!folderItem.get()) return;
  
  if (folderItem->BelongsToMultiLaunchGroup()) {
    GetFolderItem()->RemoveFromMultiLaunchGroup();
  } else {
    GetFolderItem()->AddToMultiLaunchGroup();
  }

  gController.FolderItems_FolderItemChange(folderItem);
}


void FolderItemRenderer::OnMenuDelete(wxCommandEvent& evt) {
  int result = MessageBoxes::ShowConfirmation(LOC(_T("IconPanel.DeleteConfirmation")));
  if (result != wxID_YES) return;

  FolderItemSP folderItem = GetFolderItem();
  FolderItem* parent = folderItem->GetParent();
  if (!parent) return;

  parent->RemoveChild(GetFolderItem());
}


void FolderItemRenderer::OnMenuProperties(wxCommandEvent& evt) {
  gController.GetUser()->EditFolderItem(GetFolderItem());
}


void FolderItemRenderer::OnRightDown(wxMouseEvent& evt) {
  wxMenu* menu = gMainFrame->GetIconPanel()->GetContextMenu();

  menu->AppendSeparator();
  menu->Append(ID_MENU_Delete, LOC(_T("IconPanel.PopupMenu.Delete")));

  wxMenuItem* menuItem = new wxMenuItem(menu, ID_MENU_AddToMultiLaunch, LOC(_T("IconPanel.PopupMenu.AddToQuickLaunch")), wxEmptyString, wxITEM_CHECK);
  menu->Append(menuItem);
  menuItem->Check(GetFolderItem()->BelongsToMultiLaunchGroup());

  menu->AppendSeparator();
  menu->Append(ID_MENU_Properties, LOC(_T("IconPanel.PopupMenu.Properties")));

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

  //TreeViewDialog* d = new TreeViewDialog();
  //d->SetSize(200,200);
  //d->LoadUser(gController.GetUser());
  //d->ShowModal();
  //return;

  if (mouseInside_ && mousePressed_) {    
    FolderItemSP folderItem = GetFolderItem();

    if (folderItem.get()) {
      wxLogDebug(_T("%d"), folderItem->ChildrenCount());

      if (!folderItem->IsGroup()) {

        folderItem->Launch();

      } else if (GetParent()) {

        IconPanelFrame* f = new IconPanelFrame(NULL, wxID_ANY);
        f->GetIconPanel()->SetFolderItemSource(ICON_PANEL_SOURCE_CUSTOM);
        for (int i = 0; i < folderItem->ChildrenCount(); i++) {
          FolderItemSP child = folderItem->GetChildAt(i);
          if (!child.get()) continue;
          f->GetIconPanel()->AddFolderItem(child->GetId());
        }
        f->GetIconPanel()->SetWidthInIcons(2);
        f->GetIconPanel()->SetHeightInIcons(2);
        f->FitToIconPanel();

        int windowX = GetRect().GetLeft();
        int windowY = GetRect().GetTop();

        GetParent()->ClientToScreen(&windowX, &windowY);

        windowX -= f->GetSize().GetWidth() / 2 - GetSize().GetWidth() / 2;
        windowY -= f->GetSize().GetHeight();
        
        f->Move(windowX, windowY);

        f->Show();
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
      gController.SetDraggedFolderItem(folderItem->GetId());

      wxFileDataObject fileData;
      fileData.AddFile(folderItem->GetResolvedPath());

      wxDropSource dragSource(this);
      dragSource.SetData(fileData);
      wxDragResult result = dragSource.DoDragDrop(true);

      // Tell the main controller that we've finished dragging
      // the folder item
      gController.SetDraggedFolderItem(-1);
      mousePressed_ = false;
      draggingStarted_ = false;

      InvalidateControlBitmap();
    }

  } else { // Folder item is being dragged

  }

}


void FolderItemRenderer::FitToContent() {
  SetSize(gController.GetUser()->GetSettings()->IconSize + Styles::Icon.Padding.Width,
          gController.GetUser()->GetSettings()->IconSize + Styles::Icon.Padding.Height);
}


void FolderItemRenderer::UpdateControlBitmap() {
  BitmapControl::UpdateControlBitmap();

  FolderItemSP folderItem = GetFolderItem();

  if (!folderItem.get()) {
    elog("FolderItemRenderer::UpdateControlBitmap: Folder item is null");
    return;
  }

  UserSettingsSP userSettings = gController.GetUser()->GetSettings();
  wxMemoryDC destDC;
  destDC.SelectObject(*controlBitmap_);

  if (mouseInside_) {
    // If the mouse is inside the control,
    // draw the icon overlay

    if (mousePressed_) { // DOWN state      
      if (!iconOverlayPainterDown_.get()) {
        iconOverlayPainterDown_.reset(new NineSlicesPainter());
        iconOverlayPainterDown_->LoadImage(FilePaths::SkinDirectory + _T("/IconOverlayDown.png"));
      }
      iconOverlayPainterDown_->Draw(&destDC, 0, 0, GetClientRect().GetWidth(), GetClientRect().GetHeight());
    } else { // UP state      
      if (!iconOverlayPainterUp_.get()) {
        iconOverlayPainterUp_.reset(new NineSlicesPainter());
        iconOverlayPainterUp_->LoadImage(FilePaths::SkinDirectory + _T("/IconOverlayUp.png"));
      }
      iconOverlayPainterUp_->Draw(&destDC, 0, 0, GetClientRect().GetWidth(), GetClientRect().GetHeight());
    }

  }

  // Get the icon from the folder item
  wxIconSP icon = folderItem->GetIcon(gController.GetUser()->GetSettings()->IconSize);
  wxASSERT_MSG(icon, _T("Folder item icon cannot be NULL"));

  if (icon->IsOk()) {  
    int yOffset = 0;
    if (mouseInside_ && !mousePressed_) {
      yOffset = -1;
    }
    Imaging::DrawIconWithTransparency(&destDC, *icon, Styles::Icon.Padding.Left, Styles::Icon.Padding.Top + yOffset);
  }

  if (folderItem->BelongsToMultiLaunchGroup()) {
    if (!multiLaunchIcon_.get()) {
      multiLaunchIcon_.reset(new wxBitmap(FilePaths::IconsDirectory + _T("/MultiLaunchIcon.png"), wxBITMAP_TYPE_PNG));
    }
    if (multiLaunchIcon_->IsOk()) {
      destDC.DrawBitmap(
        *multiLaunchIcon_,
        Styles::Icon.Padding.Left + userSettings->IconSize / 2 - multiLaunchIcon_->GetWidth() / 2,
        Styles::Icon.Padding.Top + userSettings->IconSize - multiLaunchIcon_->GetHeight() / 2);
    }
  }

  destDC.SelectObject(wxNullBitmap);

  SetToolTip(folderItem->GetName());
}


void FolderItemRenderer::LoadData(int folderItemId) {  
  folderItemId_ = folderItemId;  

  InvalidateControlBitmap();
}