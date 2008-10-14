#ifndef __IconPanel_H
#define __IconPanel_H

#include "wx/wx.h"
#include "NineSlicesPanel.h"
#include "FolderItemRenderer.h"
#include <vector>
#include <wx/dnd.h>
using namespace std;


class IconPanelDropTarget : public wxFileDropTarget {

public:

  IconPanelDropTarget();
  virtual bool OnDropFiles(wxCoord x, wxCoord y, const wxArrayString& filenames); 

};


class IconPanel : public NineSlicesPanel {

public:
  
  IconPanel(wxWindow *owner, int id, wxPoint point, wxSize size);
  void ReloadIcons();
  void UpdateLayout();

  /**
   * Get the insertion index under the given point. Useful for drag & drop
   * operations, in order to know between which icons the object should
   * be dropped.
   * @param point The point in screen coordinates
   * @return The insertion index or -1 if the point is off bounds
   */
  int GetInsertionIndexAtPoint(const wxPoint& point);

  FolderItemRenderer* GetRendererFromFolderItem(const FolderItem& folderItem);

  void OnSize(wxSizeEvent& evt);
  void OnPaint(wxPaintEvent& evt);
  bool OnDropFiles(wxCoord x, wxCoord y, const wxArrayString& filenames); 

private:

  IconPanelDropTarget* dropTarget_;
  bool layoutInvalidated_;
  std::vector<FolderItemRenderer*> folderItemRenderers_;

};

#endif