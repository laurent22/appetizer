#ifndef __MainFrame_H
#define __MainFrame_H

#include "wx/wx.h" 
#include "imaging/NineSlicesPainter.h"
#include "bitmap_controls/ImagePanel.h"
#include "bitmap_controls/NineSlicesPanel.h"
#include "IconPanel.h"
#include <wx/log.h>
#include "TypeDefinitions.h"


class MainFrame: public wxFrame {

  private:

    struct WindowDragDataStruct {
      bool DraggingStarted;
      bool Resizing;
      wxPoint InitWindowPos;
      wxPoint InitMousePos;
      wxSize InitWindowSize;
    };
    
    bool needLayoutUpdate_;
    bool needMaskUpdate_;
    NineSlicesPanel* backgroundPanel_;
    IconPanel* iconPanel_;
    NineSlicesPainter maskNineSlices_;
    WindowDragDataStruct windowDragData_;
    ImagePanel* resizerPanel_;
    wxLogWindow* logWindow_;

    void UpdateMask();
    void UpdateLayout();
    void UpdateLayout(int width, int height);

  public:

    MainFrame();

    IconPanel* GetIconPanel();
    void AttachOptionPanel();

    void OnPaint(wxPaintEvent& evt);
    void OnMouseDown(wxMouseEvent& evt);
    void OnMouseUp(wxMouseEvent& evt);
    void OnMouseMove(wxMouseEvent& evt);
    void OnResizerMouseDown(wxMouseEvent& evt);
    void OnResizerMouseUp(wxMouseEvent& evt);
    void OnResizerMouseMove(wxMouseEvent& evt);
    void OnSize(wxSizeEvent& evt);
    void OnMove(wxMoveEvent& evt);
    void OnEraseBackground(wxEraseEvent &evt);  
    void OnClose(wxCloseEvent& evt);

  DECLARE_EVENT_TABLE()

};

#endif