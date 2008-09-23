#ifndef __MainFrame_H
#define __MainFrame_H

#include "wx/wx.h" 
#include "NineSlicesPainter.h"
#include "ImagePanel.h"
#include "NineSlicesPanel.h"
#include "IconPanel.h"

class MainFrame: public wxFrame {

  private:

    struct WindowDragDataStruct {
      bool DraggingStarted;
      wxPoint InitWindowPos;
      wxPoint InitMousePos;
    };
    
    bool pNeedLayoutUpdate;
    bool pNeedMaskUpdate;
    wxBitmap pMaskBitmap;
    NineSlicesPanel* pBackgroundPanel;
    IconPanel* pIconPanel;
    NineSlicesPainter pMaskNineSlices;
    WindowDragDataStruct pWindowDragData;
    wxImage* pResizerImage;
    ImagePanel* pResizerPanel;

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