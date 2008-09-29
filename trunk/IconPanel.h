#ifndef __IconPanel_H
#define __IconPanel_H

#include "wx/wx.h"
#include "NineSlicesPanel.h"
#include "FolderItemRenderer.h"
#include <vector>
using namespace std;

class IconPanel : public NineSlicesPanel {

public:
  
  IconPanel(wxWindow *owner, int id, wxPoint point, wxSize size);
  void ReloadIcons();
  void UpdateLayout();

  void OnSize(wxSizeEvent& evt);
  void OnPaint(wxPaintEvent& evt);

private:

  bool layoutInvalidated_;
  std::vector<FolderItemRenderer*> folderItemRenderers_;

};

#endif