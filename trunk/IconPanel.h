/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "precompiled.h"

#ifndef __IconPanel_H
#define __IconPanel_H


#include "bitmap_controls/NineSlicesPanel.h"
#include "bitmap_controls/ImageButton.h"
#include "FolderItemRenderer.h"


enum {
  ICON_PANEL_SOURCE_USER,
  ICON_PANEL_SOURCE_CUSTOM
};


class IconPanel : public NineSlicesPanel {

public:
  
  IconPanel(wxWindow *owner, int id, wxPoint point, wxSize size);
  void ClearIcons();
  void RefreshIcons();
  void UpdateLayout();
  void InvalidateIcons();
  void InvalidateLayout();

  void SetWidthInIcons(int numberOfIcons);
  void SetHeightInIcons(int numberOfIcons);

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
  int GetRendererIndexAtPoint(const wxPoint& point);

  void SetRotated(bool rotated);

  FolderItemRendererSP GetRendererFromFolderItem(const FolderItem& folderItem);

  wxMenu* GetContextMenu();
  void ApplySkin();

  void AddFolderItem(int folderItemId);
  void SetFolderItemSource(int source);

  void OnRightDown(wxMouseEvent& evt);
  void OnMenuNewShortcut(wxCommandEvent& evt);
  void OnMenuNewGroup(wxCommandEvent& evt);
  void OnBrowseButtonMenu(wxCommandEvent& evt);

  void OnSize(wxSizeEvent& evt);
  void OnPaint(wxPaintEvent& evt);
  bool OnDropFiles(wxCoord x, wxCoord y, const wxArrayString& filenames); 
  void OnBrowseButtonClick(wxCommandEvent& evt);

private:

  bool rotated_;
  int folderItemSource_;
  std::vector<int> folderItemIds_;
  int firstOffScreenIconIndex_;
  int maxWidth_;
  int maxHeight_;
  bool iconsInvalidated_;
  bool layoutInvalidated_;
  std::vector<FolderItemRendererSP> folderItemRenderers_;
  ImageButton* browseButton_;

  DECLARE_EVENT_TABLE()

};


class IconPanelDropTarget : public wxFileDropTarget {

public:

  IconPanelDropTarget();
  void SetAssociatedIconPanel(IconPanel* iconPanel);
  IconPanel* GetAssociatedIconPanel();
  virtual bool OnDropFiles(wxCoord x, wxCoord y, const wxArrayString& filenames); 

private:

  IconPanel* associatedIconPanel_;

};


#endif