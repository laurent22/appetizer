#include "IconPanel.h"
#include "FolderItem.h"
#include "FolderItemRenderer.h"

#include "Controller.h"
extern Controller gController;


IconPanel::IconPanel(wxWindow *owner, int id, wxPoint point, wxSize size):
NineSlicesPanel(owner, id, point, size) {
  LoadImage(wxT("Data/Skin/Default/BarInnerPanel.png"));
  layoutInvalidated_ = true;
}


void IconPanel::ReloadIcons() {
  // @todo Should each icon be explicitely deleted?
  folderItemRenderers_.clear();

  std::vector<FolderItem*> folderItems = gController.GetUser()->GetFolderItems();

  for (int i = 0; i < folderItems.size(); i++) {
    FolderItemRenderer* renderer = new FolderItemRenderer(this, wxID_ANY, wxPoint(0,0), wxSize(32, 32));
    
    renderer->LoadData(folderItems.at(i));
    renderer->FitToContent();

    folderItemRenderers_.push_back(renderer);
  }

  layoutInvalidated_ = true;
  Refresh();
}


void IconPanel::OnSize(wxSizeEvent& evt) {
  BitmapControl::OnSize(evt);

  layoutInvalidated_ = true;
  Refresh();
}


void IconPanel::OnPaint(wxPaintEvent& evt) {
  BitmapControl::OnPaint(evt);
  
  if (layoutInvalidated_) UpdateLayout();
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
