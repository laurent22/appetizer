/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "stdafx.h"

#include "FolderItemProcess.h"


FolderItemProcess::FolderItemProcess(wxEvtHandler* parent, int id) :
wxProcess(parent, id) {
  terminated_ = false;
}


void FolderItemProcess::OnTerminate(int pid, int status) {
  wxLogDebug(_T("Process terminated: %d %d"), pid, status);
  terminated_ = true;
}


bool FolderItemProcess::IsTerminated() {
  return terminated_;
}