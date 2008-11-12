/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "precompiled.h"

#ifndef __FolderItemProcess_H
#define __FolderItemProcess_H


class FolderItemProcess;
typedef std::vector<FolderItemProcess*> FolderItemProcessVector;


class FolderItemProcess : public wxProcess {

public:

  FolderItemProcess(wxEvtHandler * parent = NULL, int id = -1);

private:

  void OnTerminate(int pid, int status);

};


#endif // __FolderItemProcess_H