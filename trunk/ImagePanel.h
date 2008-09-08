#ifndef __ImagePanel_H
#define __ImagePanel_H

#include "wx/wx.h"

class ImagePanel: public wxPanel {

  private:

    wxString pFilePath;
    wxBitmap pBitmap;

  public:

    ImagePanel(wxWindow *owner, int id, wxPoint point, wxSize size);

    void LoadImage(const wxString& filePath);
    void FitToContent();
    void OnPaint(wxPaintEvent& evt);
    void OnEraseBackground(wxEraseEvent &evt);

  DECLARE_EVENT_TABLE()
     
};

#endif