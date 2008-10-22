#ifndef __DelphiToolsInterface_H
#define __DelphiToolsInterface_H

#include "wx/wx.h"


class DelphiToolsInterface {

public:

  static void GetFileDescription(const wxString& filePath, wxString& fileDescription);

};


#endif // __DelphiToolsInterface_H