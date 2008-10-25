#ifndef __OptionPanel_H
#define __OptionPanel_H

#include "wx/wx.h" 
#include "bitmap_controls/NineSlicesPanel.h"


class OptionPanel: public NineSlicesPanel {

  public:

    OptionPanel(wxWindow *owner, int id, wxPoint point, wxSize size);

};

#endif // __OptionPanel_H