#include "IconPanel.h"
#include "FolderItem.h"
#include "FolderItemRenderer.h"

#include "Controller.h"
extern Controller gController;


IconPanel::IconPanel(wxWindow *owner, int id, wxPoint point, wxSize size):
NineSlicesPanel(owner, id, point, size) {
  dropTarget_ = new IconPanelDropTarget();
  SetDropTarget(dropTarget_);

  LoadImage(gController.GetFilePaths().SkinDirectory + _T("/BarInnerPanel.png"));
  layoutInvalidated_ = true;
  iconsInvalidated_ = true;
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


IconPanelDropTarget::IconPanelDropTarget() {

}


bool IconPanel::OnDropFiles(wxCoord x, wxCoord y, const wxArrayString& filenames) {
  FolderItem* folderItem = gController.GetDraggedFolderItem();  

  if (folderItem) {
    // If a folder item is being dragged, and the panel receives a drop
    // event, it means that a folder item has been dragged from the app to the app.
    // In that case, we just change the position of the folder item.
    wxLogDebug(_T("A FolderItem has been dropped: ") + folderItem->GetResolvedFilePath());

    int screenX = x;
    int screenY = y;
    ClientToScreen(&screenX, &screenY);

    int index = GetInsertionIndexAtPoint(wxPoint(screenX, screenY));

    wxLogDebug(_T("Drop index: %d"), index);

    if (index >= 0) {

      FolderItemRenderer* folderItemRenderer = GetRendererFromFolderItem(*folderItem);
      wxASSERT_MSG(folderItemRenderer, _T("FolderItemRenderer must be defined"));

      std::vector<FolderItemRenderer*> newRenderers;

      bool isPushed = false;

      for (int i = 0; i < folderItemRenderers_.size(); i++) {
        FolderItemRenderer* renderer = folderItemRenderers_.at(i);
        if (renderer->GetId() == folderItemRenderer->GetId()) continue;

        if (i == index) {
          newRenderers.push_back(folderItemRenderer);
          isPushed = true;
        }
        newRenderers.push_back(renderer);
      }

      if (!isPushed) newRenderers.push_back(folderItemRenderer);

      folderItemRenderers_.clear();
      folderItemRenderers_ = newRenderers;

      InvalidateLayout();

      wxLogDebug(_T("%d"), folderItemRenderers_.size());

    }


  } else {
    // Some files from outside the app have been dropped on the panel.
    // Currently we ignore them, but later on we should create some FolderItems
    // from these files and add them to the dock.
    wxLogDebug(_T("Some files have been dropped"));

    return false;
  }

  return true;
}


bool IconPanelDropTarget::OnDropFiles(wxCoord x, wxCoord y, const wxArrayString& filenames) {
  IconPanel* iconPanel = gController.GetMainFrame()->GetIconPanel();
  wxASSERT_MSG(iconPanel, _T("Icon panel must be defined"));

  return iconPanel->OnDropFiles(x, y, filenames);
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


void IconPanel::RefreshIcons() {
  iconsInvalidated_ = false;

  std::vector<FolderItem*> folderItems = gController.GetUser()->GetFolderItems();

  /****************************************************************************
   * Remove renderers that match a folder item that has been deleted
   ***************************************************************************/

  for (int i = folderItemRenderers_.size() - 1; i >= 0; i--) {
    FolderItemRenderer* renderer = folderItemRenderers_.at(i);
    if (!renderer->GetFolderItem()) {
      folderItemRenderers_.erase(folderItemRenderers_.begin() + i);
      wxDELETE(renderer);
    }
  }

  /****************************************************************************
   * Create new renderers for new folder items
   ***************************************************************************/

  for (int i = 0; i < folderItems.size(); i++) {
    FolderItem* folderItem = folderItems.at(i);

    bool found = false;
    for (int j = 0; j < folderItemRenderers_.size(); j++) {
      if (folderItemRenderers_.at(j)->GetFolderItem()->GetId() == folderItem->GetId()) {
        found = true;
        break;
      }
    }

    if (found) continue;

    FolderItemRenderer* renderer = new FolderItemRenderer(this, wxID_ANY, wxPoint(0,0), wxSize(32, 32));
    
    renderer->LoadData(folderItem->GetId());
    renderer->FitToContent();

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

  int x = gController.GetStyles().InnerPanel.PaddingLeft;
  int y = gController.GetStyles().InnerPanel.PaddingTop;

  int iconSize = -1;
  
  for (int i = 0; i < folderItemRenderers_.size(); i++) {
    FolderItemRenderer* renderer = folderItemRenderers_.at(i);

    renderer->Move(x, y);

    if (iconSize < 0) iconSize = renderer->GetRect().GetWidth();

    if (renderer->GetRect().GetRight() > GetRect().GetWidth() - gController.GetStyles().InnerPanel.PaddingRight) {
      x = gController.GetStyles().InnerPanel.PaddingLeft;
      y += iconSize;

      renderer->Move(x, y);
    }

    x += iconSize;
  }  
}
