#ifndef __User_H
#define __User_H

#include "wx/wx.h"


class User {

  public:

    User();

  private:

    void LoadSettings(const wxString& settingFilePath);

};


#endif