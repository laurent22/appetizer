#ifndef __FolderItemRenderer_H
#define __FolderItemRenderer_H

#include "wx/wx.h"
#include "BitmapControl.h"
#include "NineSlicesPainter.h"
#include "FolderItem.h"
#include <wx/dataobj.h>
#include <wx/dnd.h>
#include "utilities/MathUtil.h"


class FolderItemRenderer: public BitmapControl {

public:

  FolderItemRenderer(wxWindow *owner, int id, wxPoint point, wxSize size);
  void LoadData(int folderItemId);
  void UpdateControlBitmap();
  void FitToContent();
  FolderItem* GetFolderItem();

private:

  static int uniqueID_;

  bool mouseInside_;
  bool mousePressed_;
  bool draggingStarted_;
  int folderItemId_;
  NineSlicesPainter* iconOverlayPainterUp_;
  NineSlicesPainter* iconOverlayPainterDown_;
  wxPoint* pressPosition_;

  void OnEnterWindow(wxMouseEvent& evt);
  void OnLeaveWindow(wxMouseEvent& evt);
  void OnLeftDown(wxMouseEvent& evt);
  void OnLeftUp(wxMouseEvent& evt);
  void OnMotion(wxMouseEvent& evt);

  DECLARE_EVENT_TABLE()

};


#endif // __FolderItemRenderer_H