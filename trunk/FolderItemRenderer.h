#ifndef __FolderItemRenderer_H
#define __FolderItemRenderer_H

#include "wx/wx.h"
#include "BitmapControl.h"
#include "NineSlicesPainter.h"
#include "FolderItem.h"
#include <wx/dataobj.h>
#include <wx/dnd.h>
#include <wx/timer.h>
#include "utilities/MathUtil.h"


class FolderItemRenderer: public BitmapControl {

public:

  FolderItemRenderer(wxWindow *owner, int id, wxPoint point, wxSize size);
  void LoadData(FolderItem* folderItem);
  void UpdateControlBitmap();
  void FitToContent();

private:

  static const int PRESS_TIMER_ID = 1;

  bool mouseInside_;
  bool mousePressed_;
  FolderItem* folderItem_;
  NineSlicesPainter* iconOverlayPainterUp_;
  NineSlicesPainter* iconOverlayPainterDown_;
  wxPoint* pressPosition_;
  wxTimer* pressTimer_;

  void OnEnterWindow(wxMouseEvent& evt);
  void OnLeaveWindow(wxMouseEvent& evt);
  void OnLeftDown(wxMouseEvent& evt);
  void OnLeftUp(wxMouseEvent& evt);
  void OnTimer(wxTimerEvent& evt);

  DECLARE_EVENT_TABLE()

};


#endif // __FolderItemRenderer_H