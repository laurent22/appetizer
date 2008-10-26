#ifndef __OptionPanel_H
#define __OptionPanel_H

#include "wx/wx.h" 
#include "bitmap_controls/NineSlicesPanel.h"
#include <vector>
#include "OptionButton.h"


typedef std::vector<OptionButton*> OptionButtonVector;


class OptionPanel: public NineSlicesPanel {

public:

  OptionPanel(wxWindow *owner, int id, wxPoint point, wxSize size);

private:

  OptionButtonVector buttons_;

};

#endif // __OptionPanel_H