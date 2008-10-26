#ifndef __OptionButton_H
#define __OptionButton_H

#include "wx/wx.h" 
#include "bitmap_controls/ImageButton.h"


class OptionButton: public ImageButton {

  public:

    OptionButton(wxWindow *owner, int id, wxPoint point, wxSize size);

};

#endif // __OptionButton_H