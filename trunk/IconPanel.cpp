/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "IconPanel.h"
#include <wx/cursor.h>
#include <wx/filename.h>
#include "FolderItem.h"
#include "FolderItemRenderer.h"
#include "boost/shared_ptr.hpp"
#include "FilePaths.h"
#include "Log.h"
#include "Styles.h"
#include "Enumerations.h"
#include "Localization.h"
#include "Controller.h"
#include "MainFrame.h"


extern Controller gController;
extern MainFrame* gMainFrame;


BEGIN_EVENT_TABLE(IconPanel, NineSlicesPanel)
  EVT_RIGHT_DOWN(IconPanel::OnRightDown)
  EVT_MENU(ID_MENU_NewShortcut, IconPanel::OnMenuNewShortcut)
END_EVENT_TABLE()


IconPanel::IconPanel(wxWindow *owner, int id, wxPoint point, wxSize size):
NineSlicesPanel(owner, id, point, size) {

  IconPanelDropTarget* dropTarget = new IconPanelDropTarget();
  dropTarget->SetAssociatedIconPanel(this);
  SetDropTarget(dropTarget);
  
  LoadImage(FilePaths::SkinDirectory + _T("/BarInnerPanel.png"));
  SetGrid(Styles::InnerPanel.ScaleGrid);

  folderItemSource_ = ICON_PANEL_SOURCE_USER;
  firstOffScreenIconIndex_ = -1; // Means all the icons are visible
  layoutInvalidated_ = true;
  iconsInvalidated_ = true;

  browseButton_ = new ImageButton(this);
  browseButton_->LoadImage(FilePaths::SkinDirectory + _T("/BrowseArrowButton"));
  browseButton_->FitToImage();
  browseButton_->Hide();
  browseButton_->SetCursor(wxCursor(wxCURSOR_HAND));
  browseButton_->Connect(
    wxID_ANY,
    wxeEVT_CLICK,
    wxCommandEventHandler(IconPanel::OnBrowseButtonClick),
    NULL,
    this);
}


void IconPanel::ApplySkin(const wxString& skinName) {
  LoadImage(FilePaths::SkinDirectory + _T("/BarInnerPanel.png"));
  browseButton_->LoadImage(FilePaths::SkinDirectory + _T("/BrowseArrowButton"));
  browseButton_->FitToImage();

  InvalidateLayout();
}


void IconPanel::AddFolderItem(int folderItemId) {
  folderItemIds_.push_back(folderItemId);
}


void IconPanel::OnBrowseButtonMenu(wxCommandEvent& evt) {
  FolderItemSP folderItem = gController.GetUser()->GetFolderItemById(evt.GetId());
  if (!folderItem.get()) {
    evt.Skip();
  } else {
    folderItem->Launch();
  }
}


void IconPanel::OnBrowseButtonClick(wxCommandEvent& evt) {
  if (firstOffScreenIconIndex_ < 0) return;

  wxMenu menu;
  
  for (int i = firstOffScreenIconIndex_; i < folderItemRenderers_.size(); i++) {
    FolderItemRendererSP renderer = folderItemRenderers_.at(i);
    FolderItemSP folderItem = renderer->GetFolderItem();

    wxMenuItem* menuItem = folderItem->ToMenuItem(&menu);*
    menu.Append(menuItem);
  }

  menu.Connect(
    wxID_ANY,
    wxEVT_COMMAND_MENU_SELECTED,
    wxCommandEventHandler(IconPanel::OnBrowseButtonMenu),
    NULL,
    this);

  PopupMenu(&menu, wxDefaultPosition);
}


wxMenu* IconPanel::GetContextMenu() {
  wxMenu* menu = new wxMenu();
  
  menu->Append(ID_MENU_NewShortcut, LOC(_T("IconPanel.PopupMenu.NewShortcut")));
  
  return menu;
}


void IconPanel::OnMenuNewShortcut(wxCommandEvent& evt) {
  gController.GetUser()->EditNewFolderItem();
}


void IconPanel::OnRightDown(wxMouseEvent& evt) {
  wxMenu* menu = GetContextMenu();
  PopupMenu(menu, wxDefaultPosition);
  wxDELETE(menu);
}


int IconPanel::GetInsertionIndexAtPoint(const wxPoint& point) {  
  for (int i = 0; i < folderItemRenderers_.size(); i++) {
    FolderItemRendererSP renderer = folderItemRenderers_.at(i);

    int rendererScreenX = renderer->GetRect().GetLeft();
    int rendererScreenY = renderer->GetRect().GetTop();
    int rendererWidth = renderer->GetRect().GetWidth();
    int rendererHeight = renderer->GetRect().GetHeight();

    ClientToScreen(&rendererScreenX, &rendererScreenY);

    // Early exits
    if (point.y < rendererScreenY) continue;
    if (point.y >= rendererScreenY + rendererHeight) continue;
    if (point.x < rendererScreenX) continue;
    if (point.x >= rendererScreenX + rendererWidth) continue;

    // The point is somewhere in the first half of the renderer
    if (point.x < rendererScreenX + floor((double)(rendererWidth / 2))) return i;

    // The point is somewhere in the second half of the renderer
    return i + 1;
  }

  // The point is off bounds
  return -1;
}


int IconPanel::GetRendererIndexAtPoint(const wxPoint& point) {  
  for (int i = 0; i < folderItemRenderers_.size(); i++) {
    FolderItemRendererSP renderer = folderItemRenderers_.at(i);

    int rendererScreenX = renderer->GetRect().GetLeft();
    int rendererScreenY = renderer->GetRect().GetTop();
    int rendererWidth = renderer->GetRect().GetWidth();
    int rendererHeight = renderer->GetRect().GetHeight();

    ClientToScreen(&rendererScreenX, &rendererScreenY);

    // Early exits
    if (point.y < rendererScreenY) continue;
    if (point.y >= rendererScreenY + rendererHeight) continue;
    if (point.x < rendererScreenX) continue;
    if (point.x >= rendererScreenX + rendererWidth) continue;

    return i;
  }

  // The point is off bounds
  return -1;
}


FolderItemRendererSP IconPanel::GetRendererFromFolderItem(const FolderItem& folderItem) {
  for (int i = 0; i < folderItemRenderers_.size(); i++) {
    FolderItemRendererSP renderer = folderItemRenderers_.at(i);
    if (renderer->GetFolderItem()->GetId() == folderItem.GetId()) return renderer;
  }
  FolderItemRendererSP nullOuput;
  return nullOuput;
}


bool IconPanel::OnDropFiles(wxCoord x, wxCoord y, const wxArrayString& filenames) {
  FolderItemSP folderItem = gController.GetDraggedFolderItem();  

  int screenX = x;
  int screenY = y;
  ClientToScreen(&screenX, &screenY);

  if (folderItem) {
    // If a folder item is being dragged, and the panel receives a drop
    // event, it means that a folder item has been dragged from the app to the app.
    // In that case, we just change the position of the folder item.
    ilog(_T("A FolderItem has been dropped: ") + folderItem->GetResolvedPath());

    int index = GetInsertionIndexAtPoint(wxPoint(screenX, screenY));

    ilog(wxString::Format(_T("Drop index: %d"), index));

    if (index >= 0) {
      gController.GetUser()->MoveFolderItem(folderItem->GetId(), index);
    }

    return true;

  } else {

    bool didSomething = false;
    int index = GetRendererIndexAtPoint(wxPoint(screenX, screenY));
    
    if (index >= 0) {
      // The files have been dropped on an icon    
      // Launch the icon and give the files as parameters
      
      FolderItemSP folderItem = folderItemRenderers_.at(index)->GetFolderItem();
      if (!folderItem.get()) return false;

      for (int i = 0; i < filenames.Count(); i++) {
        wxFileName filename(filenames[i]);
        filename.Normalize();
        folderItem->LaunchWithArguments(filename.GetFullPath());
        didSomething = true;
      }      

    } else {
      // The files have been dropped on the icon panel itself
      // Create a new folder item for each file

      for (int i = 0; i < filenames.Count(); i++) {
        wxFileName filename(filenames[i]);
        filename.Normalize();
        gController.GetUser()->AddNewFolderItemFromPath(filename.GetFullPath());
        didSomething = true;
      }
    }

    return didSomething;
  }

  return false;
}


void IconPanel::SetWidthInIcons(int numberOfIcons) {
  int w = gController.GetUser()->GetSettings()->IconSize;
  w += Styles::Icon.Padding.Width;
  w *= numberOfIcons;
  w += Styles::InnerPanel.Padding.Width;
  
  SetSize(w, GetSize().GetHeight());
}


void IconPanel::SetHeightInIcons(int numberOfIcons) {
  int h = gController.GetUser()->GetSettings()->IconSize;
  h += Styles::Icon.Padding.Height;
  h *= numberOfIcons;
  h += Styles::InnerPanel.Padding.Height;
  
  SetSize(GetSize().GetWidth(), h);
}


int IconPanel::GetMinWidth() {
  return 
    gController.GetUser()->GetSettings()->IconSize +
    Styles::Icon.Padding.Width +
    Styles::InnerPanel.Padding.Width;
}


int IconPanel::GetMinHeight() {
  return 
    gController.GetUser()->GetSettings()->IconSize +
    Styles::Icon.Padding.Height +
    Styles::InnerPanel.Padding.Height;
}


int IconPanel::GetMaxWidth() {
  int minWidth = GetMinWidth();
  if (maxWidth_ < minWidth) return minWidth;
  return maxWidth_;
}


int IconPanel::GetMaxHeight() {
  int minHeight = GetMinHeight();
  if (maxHeight_ < minHeight) return minHeight;
  return maxHeight_;
}


void IconPanel::OnSize(wxSizeEvent& evt) {
  BitmapControl::OnSize(evt);

  InvalidateIcons();
}


void IconPanel::OnPaint(wxPaintEvent& evt) {
  BitmapControl::OnPaint(evt);
  
  if (iconsInvalidated_) {
    RefreshIcons();
  } else {
    if (layoutInvalidated_) UpdateLayout();
  }
}


void IconPanel::InvalidateIcons() {
  iconsInvalidated_ = true;
  Refresh();
}


void IconPanel::InvalidateLayout() {
  layoutInvalidated_ = true;
  Refresh();
}


void IconPanel::ClearIcons() {
  folderItemRenderers_.clear();
}


void IconPanel::SetFolderItemSource(int source) {
  folderItemSource_ = source;
}


void IconPanel::RefreshIcons() {
  iconsInvalidated_ = false;

  std::vector<FolderItemSP> folderItems;

  if (folderItemSource_ == ICON_PANEL_SOURCE_USER) {  
    // @todo: It shouldn't be possible to access the FolderItem vector directly.
    // Need to implement GetFolderItemAt() and GetFolderItemCount() to iterate
    // through the folder items.
    folderItems = gController.GetUser()->GetFolderItems();
  } else {
    
    for (int i = 0; i < folderItemIds_.size(); i++) {
      FolderItemSP folderItem = gController.GetUser()->GetFolderItemById(folderItemIds_.at(i));
      if (!folderItem.get()) {
        folderItemIds_.erase(folderItemIds_.begin() + i);
        i--;
        continue;
      }
      
      folderItems.push_back(folderItem);
    }

  }

  /****************************************************************************
   * Remove renderers that match a folder item that has been deleted
   ***************************************************************************/

  for (int i = folderItemRenderers_.size() - 1; i >= 0; i--) {
    FolderItemRendererSP renderer = folderItemRenderers_.at(i);
    if (!renderer->GetFolderItem().get()) {
      folderItemRenderers_.erase(folderItemRenderers_.begin() + i);
    }
  }

  /****************************************************************************
   * Create new renderers for new folder items
   ***************************************************************************/

  for (int i = 0; i < folderItems.size(); i++) {
    FolderItemSP folderItem = folderItems.at(i);

    bool found = false;
    for (int j = 0; j < folderItemRenderers_.size(); j++) {
      if (folderItemRenderers_.at(j)->GetFolderItem()->GetId() == folderItem->GetId()) {
        found = true;
        break;
      }
    }

    if (found) continue;

    FolderItemRendererSP renderer(new FolderItemRenderer(this, wxID_ANY, wxPoint(0,0), wxSize(0, 0)));
    
    renderer->LoadData(folderItem->GetId());
    renderer->FitToContent();
    renderer->SetCursor(wxCursor(wxCURSOR_HAND));

    folderItemRenderers_.push_back(renderer);
  }

  /****************************************************************************
   * Sort the renderers
   ***************************************************************************/

  std::vector<FolderItemRendererSP> newRenderers;

  for (int i = 0; i < folderItems.size(); i++) {
    FolderItemSP folderItem = folderItems.at(i);

    for (int j = 0; j < folderItemRenderers_.size(); j++) {
      FolderItemRendererSP renderer = folderItemRenderers_.at(j);

      if (renderer->GetFolderItem()->GetId() == folderItem->GetId()) {
        newRenderers.push_back(renderer);
        break;
      }
    }
  }

  folderItemRenderers_.clear();
  folderItemRenderers_ = newRenderers;

  wxASSERT_MSG(folderItemRenderers_.size() == folderItems.size(), _T("Number of folder items must match number of renderers"));  
    
  InvalidateLayout();
}


void IconPanel::UpdateLayout() {
  layoutInvalidated_ = false;

  // Loop through the icons and move them
  // at their right position

  int x = Styles::InnerPanel.Padding.Left;
  int y = Styles::InnerPanel.Padding.Top;
  int width = GetRect().GetWidth();
  int height = GetRect().GetHeight();

  int iconSize = -1;
  firstOffScreenIconIndex_ = -1;
  
  for (int i = 0; i < folderItemRenderers_.size(); i++) {
    FolderItemRendererSP renderer = folderItemRenderers_.at(i);

    int newX = x;
    int newY = y;

    if (iconSize < 0) iconSize = renderer->GetRect().GetWidth();

    if (i > 0 && newX + iconSize > width - Styles::InnerPanel.Padding.Right) {
      newX = Styles::InnerPanel.Padding.Left;
      newY += iconSize;
      y = newY;
    }

    if (newY + iconSize > height - Styles::InnerPanel.Padding.Bottom) {
      if (firstOffScreenIconIndex_ < 0) firstOffScreenIconIndex_ = i;
      renderer->Hide();
    } else {
      renderer->Show();
    }
    
    renderer->Move(newX, newY);
    
    maxWidth_ = newX + iconSize;
    maxHeight_ = newY + iconSize;

    x = newX + iconSize;
  }  

  maxWidth_ += Styles::InnerPanel.Padding.Right;
  maxHeight_ += Styles::InnerPanel.Padding.Bottom;

  if (firstOffScreenIconIndex_ >= 0) {
    // If there are some offscreen icons, show the browse button
    browseButton_->Show();
    browseButton_->Move(
      width - browseButton_->GetSize().GetWidth() - Styles::InnerPanel.Padding.Right,
      height - browseButton_->GetSize().GetHeight() - Styles::InnerPanel.Padding.Bottom);

    if (firstOffScreenIconIndex_ > 0) {
      // If the browse button overlaps the last icon,
      // hide this icon and decrement firstOffScreenIconIndex_
      FolderItemRendererSP r = folderItemRenderers_.at(firstOffScreenIconIndex_ - 1);
      if (r->GetRect().GetRight() > browseButton_->GetRect().GetLeft() && r->GetRect().GetBottom() > browseButton_->GetRect().GetTop()) {
        r->Hide();
        firstOffScreenIconIndex_--;
      }
    }
  } else {
    browseButton_->Hide();
  }
}



//*****************************************************
// IconPanelDropTarget
//*****************************************************

IconPanelDropTarget::IconPanelDropTarget() {
  associatedIconPanel_ = NULL;
}


void IconPanelDropTarget::SetAssociatedIconPanel(IconPanel* iconPanel) {
  associatedIconPanel_ = iconPanel;
}


IconPanel* IconPanelDropTarget::GetAssociatedIconPanel() {
  return associatedIconPanel_;
}


bool IconPanelDropTarget::OnDropFiles(wxCoord x, wxCoord y, const wxArrayString& filenames) {
  wxASSERT_MSG(associatedIconPanel_, _T("An icon panel must be associated"));
  if (!associatedIconPanel_) return false;

  // Forward the event to the icon panel, so that we
  // can have a useful "this" pointer to work with
  return associatedIconPanel_->OnDropFiles(x, y, filenames);
}