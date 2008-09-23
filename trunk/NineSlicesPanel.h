#ifndef __NineSlicesPanel_H
#define __NineSlicesPanel_H

#include "wx/wx.h"
#include "NineSlicesPainter.h"
#include "BitmapControl.h"

class NineSlicesPanel: public BitmapControl {

  private:

    wxString pFilePath;
    NineSlicesPainter* pNineSlicesPainter;

  public:

    NineSlicesPanel(wxWindow *owner, int id, wxPoint point, wxSize size);
    void LoadImage(const wxString& filePath);
    void UpdateControlBitmap();

};

#endif