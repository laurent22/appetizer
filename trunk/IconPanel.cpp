/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/


#include "stdafx.h"

#include "IconPanel.h"
#include "FolderItem.h"
#include "FolderItemRenderer.h"
#include "FilePaths.h"
#include "Styles.h"
#include "User.h"
#include "Enumerations.h"
#include "gui/FileOrFolderDialog.h"
#include "MiniLaunchBar.h"


BEGIN_EVENT_TABLE(IconPanel, NineSlicesPanel)
  EVT_RIGHT_DOWN(IconPanel::OnRightDown)
  EVT_MENU(wxID_ANY, IconPanel::OnMenuItemClick)
END_EVENT_TABLE()


IconPanel::IconPanel(wxWindow *owner, int id, wxPoint point, wxSize size):
NineSlicesPanel(owner, id, point, size) {

  SetName(_T("IconPanel"));

  rotated_ = false;
  lastRect_ = wxRect(0,0,0,0);

  IconPanelDropTarget* dropTarget = new IconPanelDropTarget();
  dropTarget->SetAssociatedIconPanel(this);
  SetDropTarget(dropTarget);

  folderItemSource_ = ICON_PANEL_SOURCE_USER;
  firstOffScreenIconIndex_ = -1; // Means all the icons are visible
  layoutInvalidated_ = true;
  iconsInvalidated_ = true;

  browseButton_ = new ImageButton(this);
  browseButton_->Hide();
  browseButton_->SetCursor(wxCursor(wxCURSOR_HAND));
  browseButton_->Connect(
    wxID_ANY,
    wxeEVT_CLICK,
    wxCommandEventHandler(IconPanel::OnBrowseButtonClick),
    NULL,
    this);
}


void IconPanel::SetRotated(bool rotated) {
  if (rotated == rotated_) return;
  rotated_ = rotated;
  
  SetBitmapRotation(rotated_ ? -90 : 0);

  InvalidateLayout();
}


wxBitmap* IconPanel::GetControlBitmap() {
  BitmapControl* parent = dynamic_cast<BitmapControl*>(GetParent());
  if (!parent) return NineSlicesPanel::GetControlBitmap();

  wxBitmap* parentBitmap = parent->GetControlBitmap(); 

  wxRect rect = this->GetRect();
  if (rect.GetLeft() < 0) rect.SetLeft(0);
  if (rect.GetTop() < 0) rect.SetTop(0);
  if (rect.GetRight() >= parentBitmap->GetWidth()) rect.SetRight(parentBitmap->GetWidth() - 1);
  if (rect.GetBottom() >= parentBitmap->GetHeight()) rect.SetBottom(parentBitmap->GetHeight() - 1);

  // Don't rebuild the bitmap if the rectangle hasn't changed
  if (rect.GetX() == lastRect_.GetX() && rect.GetY() == lastRect_.GetY() && rect.GetWidth() == lastRect_.GetWidth() && rect.GetHeight() == lastRect_.GetHeight())
    return controlBitmap_;

  wxDELETE(controlBitmap_);

  if (rect.GetWidth() > 0 && rect.GetHeight() > 0) {
    // Copy the parent sub bitmap as a background of this control
    controlBitmap_ = new wxBitmap(parentBitmap->GetSubBitmap(rect));
    lastRect_ = wxRect(rect.GetX(), rect.GetY(), rect.GetWidth(), rect.GetHeight());
  }

  return controlBitmap_;
}


void IconPanel::ApplySkin(wxBitmap* mainBackgroundBitmap) {

  wxBitmap* browseButtonBitmapUp = new wxBitmap(FilePaths::GetSkinFile(_T("BrowseArrowButton.png")), wxBITMAP_TYPE_PNG);  

  browseButton_->LoadImages(browseButtonBitmapUp);
  browseButton_->SetStateColors(wxNullColour, Styles::BrowseButton.ColorOver, Styles::BrowseButton.ColorDown);
  browseButton_->FitToImage();

  for (int i = 0; i < folderItemRenderers_.size(); i++) {
    FolderItemRenderer* renderer = folderItemRenderers_.at(i);
    renderer->ApplySkin();
  }

  InvalidateLayout();
}


void IconPanel::AddFolderItem(int folderItemId) {
  folderItemIds_.push_back(folderItemId);
}


void IconPanel::OnBrowseButtonMenu(wxCommandEvent& evt) {
  FolderItem* folderItem = wxGetApp().GetUser()->GetRootFolderItem()->GetChildById(evt.GetId());
  if (!folderItem) {
    evt.Skip();
  } else {
    if (folderItem->IsGroup()) {
      wxGetApp().GetUtilities().ShowTreeViewDialog(evt.GetId());
    } else {
      folderItem->Launch();
    }
  }
}


void IconPanel::OnBrowseButtonClick(wxCommandEvent& evt) {
  if (firstOffScreenIconIndex_ < 0) return;

  wxMenu menu;
  
  for (int i = firstOffScreenIconIndex_; i < folderItemRenderers_.size(); i++) {
    FolderItemRenderer* renderer = folderItemRenderers_.at(i);
    FolderItem* folderItem = renderer->GetFolderItem();

    folderItem->AppendAsMenuItem(&menu, SMALL_ICON_SIZE, _T("browseButtonFolderItem"));
  }

  PopupMenu(&menu, wxDefaultPosition);
}


wxMenu* IconPanel::GetContextMenu() {
  wxMenu* menu = new wxMenu();
  ExtendedMenuItem* menuItem = NULL;

  menuItem = new ExtendedMenuItem(menu, wxGetApp().GetUniqueInt(), _("New shortcut..."));
  menuItem->SetMetadata(_T("name"), _T("newShortcut"));
  menu->Append(menuItem);
  
  menuItem = new ExtendedMenuItem(menu, wxGetApp().GetUniqueInt(), _("New group..."));
  menuItem->SetMetadata(_T("name"), _T("newGroup"));
  menu->Append(menuItem);

  wxMenu* specialMenu = new wxMenu();
  wxIcon* specialIcon = NULL;

  #define ADD_SPECIAL_ITEM_TO_MENU(text, specialItemMacro) \
    menuItem = new ExtendedMenuItem(specialMenu, wxGetApp().GetUniqueInt(), text); \
    menuItem->SetMetadata(_T("name"), _T("addSpecialItem")); \
    menuItem->SetMetadata(_T("specialItemMacro"), specialItemMacro); \
    specialIcon = FolderItem::CreateSpecialItemIcon(specialItemMacro, 16); \
    menuItem->SetBitmap(*specialIcon); \
    wxDELETE(specialIcon); \
    specialMenu->Append(menuItem);

  ADD_SPECIAL_ITEM_TO_MENU(_("Control Panel"), _T("$(ControlPanel)"))
  ADD_SPECIAL_ITEM_TO_MENU(_("My Computer"), _T("$(MyComputer)"))
  ADD_SPECIAL_ITEM_TO_MENU(_("My Network Places"), _T("$(MyNetwork)"))
  ADD_SPECIAL_ITEM_TO_MENU(_("Recycle Bin"), _T("$(RecycleBin)"))
  ADD_SPECIAL_ITEM_TO_MENU(_("Show Desktop"), _T("$(ShowDesktop)"))
  ADD_SPECIAL_ITEM_TO_MENU(_("Windows Explorer"), _T("$(Explorer)"))
  ADD_SPECIAL_ITEM_TO_MENU(_("Search"), _T("$(Search)"))

  #undef ADD_SPECIAL_ITEM_TO_MENU

  menu->AppendSubMenu(specialMenu, _("Add special item"));
  
  return menu;
}


void IconPanel::OnMenuItemClick(wxCommandEvent& evt) {
  ExtendedMenuItem* menuItem = GetClickedMenuItem(evt);
  wxString name = menuItem->GetMetadata(_T("name"));

  User* user = wxGetApp().GetUser();
  FolderItem* rootFolderItem = user->GetRootFolderItem();

  if (name == _T("addSpecialItem")) {

    user->AddNewFolderItemFromPath(rootFolderItem, menuItem->GetMetadata(_T("specialItemMacro")));

  } else if (name == _T("newShortcut")) {

    FileOrFolderDialog* d = new FileOrFolderDialog(this);
    int result = d->ShowModal();
    if (result == wxID_OK) {
      wxGetApp().GetUser()->AddNewFolderItemFromPath(wxGetApp().GetUser()->GetRootFolderItem(), d->GetPath());
    }  

    d->Destroy();

  } else if (name == _T("newGroup")) {

    wxGetApp().GetUser()->EditNewFolderItem(wxGetApp().GetUser()->GetRootFolderItem(), true);

  } else if (name == _T("browseButtonFolderItem")) {

    FolderItem* folderItem = wxGetApp().GetUser()->GetRootFolderItem()->GetChildById(menuItem->GetMetadataInt(_T("folderItemId")));
    if (!folderItem) {
      evt.Skip();
    } else {
      if (folderItem->IsGroup()) {
        wxGetApp().GetUtilities().ShowTreeViewDialog(evt.GetId());
      } else {
        folderItem->Launch();
      }
    }

  } else {

    evt.Skip();

  }
}


void IconPanel::OnRightDown(wxMouseEvent& evt) {
  wxMenu* menu = GetContextMenu();
  PopupMenu(menu, wxDefaultPosition);
  wxDELETE(menu);
}


int IconPanel::GetInsertionIndexAtPoint(const wxPoint& point) {  
  for (int i = 0; i < folderItemRenderers_.size(); i++) {
    FolderItemRenderer* renderer = folderItemRenderers_.at(i);

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

  if (folderItemRenderers_.size() <= 0) return -1;

  FolderItemRenderer* firstRenderer = folderItemRenderers_.at(0);
  int x = firstRenderer->GetRect().GetRight();
  int y = firstRenderer->GetRect().GetBottom();
  ClientToScreen(&x, &y);
  if (point.x <= x && point.y <= y) return 0;

  FolderItemRenderer* lastRenderer = folderItemRenderers_.at(folderItemRenderers_.size() - 1);
  x = lastRenderer->GetRect().GetLeft();
  y = lastRenderer->GetRect().GetTop();
  ClientToScreen(&x, &y);
  if (point.x >= x && point.y >= y) return folderItemRenderers_.size();

  // The point is off bounds
  return -1;
}


int IconPanel::GetRendererIndexAtPoint(const wxPoint& point) {  
  for (int i = 0; i < folderItemRenderers_.size(); i++) {
    FolderItemRenderer* renderer = folderItemRenderers_.at(i);

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


FolderItemRenderer* IconPanel::GetRendererFromFolderItem(const FolderItem& folderItem) {
  for (int i = 0; i < folderItemRenderers_.size(); i++) {
    FolderItemRenderer* renderer = folderItemRenderers_.at(i);
    if (renderer->GetFolderItem()->GetId() == folderItem.GetId()) return renderer;
  }

  return NULL;
}


bool IconPanel::OnDropFiles(wxCoord x, wxCoord y, const wxArrayString& filenames) {
  FolderItem* folderItem = wxGetApp().GetDraggedFolderItem();  

  int screenX = x;
  int screenY = y;
  ClientToScreen(&screenX, &screenY);

  if (folderItem) {
    // If a folder item is being dragged, and the panel receives a drop
    // event, it means that a folder item has been dragged from the app to the app.
    // In that case, we just change the position of the folder item.
    ILOG(_T("A FolderItem has been dropped: ") + folderItem->GetResolvedPath());

    int index = GetInsertionIndexAtPoint(wxPoint(screenX, screenY));

    ILOG(wxString::Format(_T("Drop index: %d"), index));

    if (index >= 0) {
      wxGetApp().GetUser()->GetRootFolderItem()->MoveChild(folderItem, index);
    }

    return true;

  } else {

    bool didSomething = false;
    int index = GetRendererIndexAtPoint(wxPoint(screenX, screenY));
    
    if (index >= 0) {
      // The files have been dropped on an icon    
      // Launch the icon and give the files as parameters
      
      FolderItem* folderItem = folderItemRenderers_.at(index)->GetFolderItem();
      if (!folderItem) return false;

      if (folderItem->GetFilePath().Index(_T("$(")) != wxNOT_FOUND) {
        if (folderItem->GetFilePath() == _T("$(RecycleBin)")) {
          // Delete the files to the recycle bin

          for (int i = 0; i < filenames.Count(); i++) {
            wxFileName filename(filenames[i]);
            filename.Normalize();           

            #ifdef __WINDOWS__

            wchar_t* cFullPath = new wchar_t[filename.GetFullPath().Len() + 2];
            mbstowcs(cFullPath, filename.GetFullPath().mb_str(), filename.GetFullPath().Len());
            cFullPath[filename.GetFullPath().Len()] = _T('\0');
            cFullPath[filename.GetFullPath().Len()+1] = _T('\0');

            SHFILEOPSTRUCT operation;
            operation.wFunc = FO_DELETE;
            operation.pFrom = cFullPath;
            operation.fFlags = FOF_ALLOWUNDO;
            SHFileOperation(&operation);

            wxDELETE(cFullPath);

            #endif // __WINDOWS__
          }   

        } else {
          folderItem->Launch();
        }
        return true;
      }

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
        wxString fullPath = filename.GetFullPath();

        wxGetApp().GetUser()->AddNewFolderItemFromPath(
          wxGetApp().GetUser()->GetRootFolderItem(),
          fullPath);
        didSomething = true;
      }
    }

    return didSomething;
  }

  return false;
}


void IconPanel::SetWidthInIcons(int numberOfIcons) {
  int w = wxGetApp().GetUser()->GetSettings()->GetValidatedIconSize();
  w += Styles::Icon.Padding.Width;
  w *= numberOfIcons;
  w += Styles::InnerPanel.Padding.Width;
  
  SetSize(w, GetSize().GetHeight());
}


void IconPanel::SetHeightInIcons(int numberOfIcons) {
  int h = wxGetApp().GetUser()->GetSettings()->GetValidatedIconSize();
  h += Styles::Icon.Padding.Height;
  h *= numberOfIcons;
  h += Styles::InnerPanel.Padding.Height;
  
  SetSize(GetSize().GetWidth(), h);
}


int IconPanel::GetMinWidth() {
  return 
    wxGetApp().GetUser()->GetSettings()->GetValidatedIconSize() +
    Styles::Icon.Padding.Width +
    Styles::InnerPanel.Padding.Width;
}


int IconPanel::GetMinHeight() {
  return 
    wxGetApp().GetUser()->GetSettings()->GetValidatedIconSize() +
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

  InvalidateLayout();
  //InvalidateIcons();
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
  for (int i = 0; i < folderItemRenderers_.size(); i++) wxDELETE(folderItemRenderers_[i]);
  folderItemRenderers_.clear();
}


void IconPanel::SetFolderItemSource(int source) {
  folderItemSource_ = source;
}


FolderItemRenderer* IconPanel::GetFolderItemRendererById(int id) {
  for (int i = 0; i < folderItemRenderers_.size(); i++) {
    FolderItemRenderer* r = folderItemRenderers_[i];
    if (!r) continue;
    if (r->GetId() == id) return r;
  }
  return NULL;
}


void IconPanel::RefreshIcons() {
  iconsInvalidated_ = false;

  FolderItemVector folderItems;
  FolderItem* rootFolderItem = wxGetApp().GetUser()->GetRootFolderItem();

  if (folderItemSource_ == ICON_PANEL_SOURCE_USER) {      

    folderItems = rootFolderItem->GetChildren();

  } else {
    
    for (int i = 0; i < folderItemIds_.size(); i++) {
      FolderItem* folderItem = rootFolderItem->GetChildById(folderItemIds_.at(i));
      if (!folderItem) {
        folderItemIds_.erase(folderItemIds_.begin() + i);
        i--;
        continue;
      }
      
      folderItems.push_back(folderItem);
    }

  }

  /****************************************************************************
   * Remove renderers that match a folder item that has been deleted
   * Also remove renderers that are no longer at the root
   ***************************************************************************/

  for (int i = folderItemRenderers_.size() - 1; i >= 0; i--) {
    FolderItemRenderer* renderer = folderItemRenderers_.at(i);

    bool removeIt = false;
    FolderItem* folderItem = renderer->GetFolderItem();

    if (!folderItem) {
      removeIt = true;
    } else {
      if (folderItem->GetParent() == NULL) {
        removeIt = true;
      } else {
        if (folderItem->GetParent()->GetId() != rootFolderItem->GetId()) {
          removeIt = true;
        }
      }
    }

    if (removeIt) {
      renderer->Destroy();
      folderItemRenderers_.erase(folderItemRenderers_.begin() + i);
    }
  }

  /****************************************************************************
   * Create new renderers for new folder items
   ***************************************************************************/

  int folderItemCount = folderItems.size();

  for (int i = 0; i < folderItemCount; i++) {
    FolderItem* folderItem = folderItems.at(i);

    // Check if the folder item is already loaded in a renderer
    bool found = false;
    int folderItemRendererCount = folderItemRenderers_.size();

    for (int j = 0; j < folderItemRendererCount; j++) {
      FolderItem* rFolderItem = folderItemRenderers_.at(j)->GetFolderItem();
      
      if (!rFolderItem) {
        ELOG(_T("Folder item shouldn't be null"));
        continue;
      }

      if (rFolderItem->GetId() == folderItem->GetId()) {
        found = true;
        break;
      }
    }
    
    if (found) continue; // The folder item is already on the dock

    // Create a new renderer and add it to the panel
    FolderItemRenderer* renderer = new FolderItemRenderer(this, wxGetApp().GetUniqueInt(), wxPoint(0,0), wxSize(0, 0));
    
    renderer->LoadData(folderItem->GetId());
    renderer->Hide();
    renderer->SetCursor(wxCursor(wxCURSOR_HAND));

    folderItemRenderers_.push_back(renderer);
  }

  /****************************************************************************
   * Sort the renderers
   ***************************************************************************/

  std::vector<FolderItemRenderer*> newRenderers;

  for (int i = 0; i < folderItems.size(); i++) {
    FolderItem* folderItem = folderItems.at(i);

    for (int j = 0; j < folderItemRenderers_.size(); j++) {
      FolderItemRenderer* renderer = folderItemRenderers_.at(j);

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

  bool firstHiddenFound = false;
  
  for (int i = 0; i < folderItemRenderers_.size(); i++) {
    FolderItemRenderer* renderer = folderItemRenderers_.at(i);

    if (!firstHiddenFound) renderer->FitToContent();

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
      firstHiddenFound = true;
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
      FolderItemRenderer* r = folderItemRenderers_.at(firstOffScreenIconIndex_ - 1);
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