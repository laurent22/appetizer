#ifndef __ImagePanel_H
#define __ImagePanel_H

#include "wx/wx.h"
#include "BitmapControl.h"

class ImagePanel: public BitmapControl {

  public:

    ImagePanel(wxWindow *owner, int id, wxPoint point, wxSize size);
    void LoadImage(const wxString& filePath);
    void UpdateControlBitmap();
    void FitToContent();

  private:

    struct WindowDragDataStruct {
      bool DraggingStarted;
      wxPoint InitWindowPos;
      wxPoint InitMousePos;
    };

    wxString pFilePath;
    wxBitmap pBitmap;
    WindowDragDataStruct pWindowDragData;

};


#endif