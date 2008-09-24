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

    wxString filePath_;
    wxBitmap bitmap_;

};


#endif