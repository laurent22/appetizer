#include "IconPanel.h"
#include "FolderItem.h"
#include "FolderItemRenderer.h"

#include "Controller.h"
extern Controller gController;


IconPanel::IconPanel(wxWindow *owner, int id, wxPoint point, wxSize size):
NineSlicesPanel(owner, id, point, size) {
  LoadImage(wxT("Data/Skin/Default/BarInnerPanel.png"));
}


void IconPanel::ReloadIcons() {
  // @todo Should each icon be explicitely deleted?
  folderItemRenderers_.clear();

  //std::vector<FolderItem*>::iterator iter;
  std::vector<FolderItem*> folderItems = gController.GetUser()->GetFolderItems();

  for (int i = 0; i < folderItems.size(); i++) {
    //FolderItem folderItem = *(folderItems.at(i));

    FolderItemRenderer* renderer = new FolderItemRenderer(this, wxID_ANY, wxPoint(0,0), wxSize(32, 32));
    
    renderer->LoadData(folderItems.at(i));

    folderItemRenderers_.push_back(renderer);
  }

}