/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "stdafx.h"

#ifndef __FolderItemProcess_H
#define __FolderItemProcess_H


class FolderItemProcess : public wxProcess {

public:

  FolderItemProcess();
  FolderItemProcess(wxEvtHandler * parent, int id = -1);
  virtual void OnTerminate(int pid, int status); 

};


typedef std::vector<FolderItemProcess*> FolderItemProcessVector;


#endif // __FolderItemProcess_H