#ifndef __MainFrame_H
#define __MainFrame_H

#include "wx/wx.h" 
#include "NineSlicesPainter.h"
#include "ImagePanel.h"
#include "NineSlicesPanel.h"
#include "IconPanel.h"
#include <wx/log.h>


class MainFrame: public wxFrame {

  private:

    struct WindowDragDataStruct {
      bool DraggingStarted;
      wxPoint InitWindowPos;
      wxPoint InitMousePos;
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

    // Constructor
    MainFrame();

    IconPanel* GetIconPanel();

    // Event handlers
    void OnPaint(wxPaintEvent& evt);
    void OnWindowCreate(wxWindowCreateEvent& evt);
    void OnMouseDown(wxMouseEvent& evt);
    void OnMouseUp(wxMouseEvent& evt);
    void OnMouseMove(wxMouseEvent& evt);
    void OnSize(wxSizeEvent& evt);
    void OnEraseBackground(wxEraseEvent &evt);  

  DECLARE_EVENT_TABLE()

};

#endif