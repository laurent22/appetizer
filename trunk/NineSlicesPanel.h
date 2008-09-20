#ifndef __NineSlicesPanel_H
#define __NineSlicesPanel_H

#include "wx/wx.h"
#include "NineSlicesPainter.h"
#include "BitmapControlInterface.h"

class NineSlicesPanel: public wxPanel {

  private:

    wxString pFilePath;
    NineSlicesPainter* pNineSlicesPainter;

  public:

    NineSlicesPanel(wxWindow *owner, int id, wxPoint point, wxSize size);

    void LoadImage(const wxString& filePath);
    void OnPaint(wxPaintEvent& evt);
    void OnEraseBackground(wxEraseEvent &evt);  

  DECLARE_BITMAP_CONTROL_INTERFACE()

  DECLARE_EVENT_TABLE()  

};

#endif