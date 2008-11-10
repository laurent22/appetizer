/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#ifndef __MiniLaunchBar_H
#define __MiniLaunchBar_H


#include <wx/wx.h>
#include <wx/snglinst.h>
#include <wx/cmdline.h>


// The application class. An instance is created and initialized
// below in IMPLEMENT_APP()
class MiniLaunchBar: public wxApp {

public:

  const wxCmdLineParser& GetCommandLine();

private:
  
  virtual bool OnInit();
  virtual int OnExit();
  wxSingleInstanceChecker* singleInstanceChecker_;
  wxCmdLineParser commandLine_;

};


DECLARE_APP(MiniLaunchBar)


#endif // __MiniLaunchBar_H