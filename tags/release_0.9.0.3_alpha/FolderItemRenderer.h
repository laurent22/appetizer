#ifndef __FolderItemRenderer_H
#define __FolderItemRenderer_H

#include "wx/wx.h"
#include "bitmap_controls/BitmapControl.h"
#include "imaging/NineSlicesPainter.h"
#include "FolderItem.h"
#include <wx/dataobj.h>
#include <wx/dnd.h>
#include "utilities/MathUtil.h"
#include "boost/shared_ptr.hpp"
#include "TypeDefinitions.h"



class FolderItemRenderer: public BitmapControl {

public:

  FolderItemRenderer(wxWindow *owner, int id, wxPoint point, wxSize size);
  void LoadData(int folderItemId);
  void UpdateControlBitmap();
  void FitToContent();
  FolderItemSP GetFolderItem();

private:

  static int uniqueID_;

  bool mouseInside_;
  bool mousePressed_;
  bool draggingStarted_;
  int folderItemId_;
  NineSlicesPainterSP iconOverlayPainterUp_;
  NineSlicesPainterSP iconOverlayPainterDown_;
  wxPoint pressPosition_;

  void OnEnterWindow(wxMouseEvent& evt);
  void OnLeaveWindow(wxMouseEvent& evt);
  void OnLeftDown(wxMouseEvent& evt);
  void OnLeftUp(wxMouseEvent& evt);
  void OnMotion(wxMouseEvent& evt);
  void OnRightDown(wxMouseEvent& evt);

  void OnMenuDelete(wxCommandEvent& evt);
  void OnMenuProperties(wxCommandEvent& evt);
  void OnMenuAddToMultiLaunch(wxCommandEvent& evt);

  DECLARE_EVENT_TABLE()

};

typedef boost::shared_ptr<FolderItemRenderer> FolderItemRendererSP;



#endif // __FolderItemRenderer_H