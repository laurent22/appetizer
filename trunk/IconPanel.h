#ifndef __IconPanel_H
#define __IconPanel_H

#include <wx/wx.h>
#include <wx/dnd.h>
#include <wx/menu.h>
#include "bitmap_controls/NineSlicesPanel.h"
#include "bitmap_controls/ImageButton.h"
#include "FolderItemRenderer.h"
#include <vector>
using namespace std;


class IconPanelDropTarget : public wxFileDropTarget {

public:

  IconPanelDropTarget();
  virtual bool OnDropFiles(wxCoord x, wxCoord y, const wxArrayString& filenames); 

};


class IconPanel : public NineSlicesPanel {

public:
  
  IconPanel(wxWindow *owner, int id, wxPoint point, wxSize size);
  void RefreshIcons();
  void UpdateLayout();
  void InvalidateIcons();
  void InvalidateLayout();

  int GetMinWidth();
  int GetMinHeight();
  int GetMaxWidth();
  int GetMaxHeight();

  /**
   * Get the insertion index under the given point. Useful for drag & drop
   * operations, in order to know between which icons the object should
   * be dropped.
   * @param point The point in screen coordinates
   * @return The insertion index or -1 if the point is off bounds
   */
  int GetInsertionIndexAtPoint(const wxPoint& point);

  FolderItemRendererSP GetRendererFromFolderItem(const FolderItem& folderItem);

  wxMenu* GetContextMenu();

  void OnRightDown(wxMouseEvent& evt);
  void OnMenuNewShortcut(wxCommandEvent& evt);
  void OnBrowseButtonMenu(wxCommandEvent& evt);

  void OnSize(wxSizeEvent& evt);
  void OnPaint(wxPaintEvent& evt);
  bool OnDropFiles(wxCoord x, wxCoord y, const wxArrayString& filenames); 
  void OnBrowseButtonClick(wxCommandEvent& evt);

private:

  int firstOffScreenIconIndex_;
  int maxWidth_;
  int maxHeight_;
  bool iconsInvalidated_;
  bool layoutInvalidated_;
  std::vector<FolderItemRendererSP> folderItemRenderers_;
  ImageButton* browseButton_;

  DECLARE_EVENT_TABLE()

};


#endif