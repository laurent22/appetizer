#ifndef __OptionFrame_H
#define __OptionFrame_H

#include "wx/wx.h" 
#include "bitmap_controls/ImageButton.h"


class OptionFrame: public wxFrame {

public:

  OptionFrame();
  void InvalidateLayout();
  void InvalidateMask();
  void UpdateLayout(int width, int height);
  void UpdateLayout();
  void UpdateMask();

private:

  bool maskInvalidated_;
  bool layoutInvalidated_;
  ImageButton* arrowButton_;
  NineSlicesPainter maskNineSlices_;
  int openWidth_;

  void OnPaint(wxPaintEvent& evt);
  void OnSize(wxSizeEvent& evt);
  void OnMove(wxMoveEvent& evt);
  void OnEraseBackground(wxEraseEvent &evt);  

  DECLARE_EVENT_TABLE()

};

#endif // __OptionFrame_H