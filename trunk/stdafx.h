/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#ifndef __MiniLaunchBarPrecompiled_H
#define __MiniLaunchBarPrecompiled_H

#include <wx/wx.h>

#ifdef __WINDOWS__
#include <windows.h>
#include <winver.h>
#include <Rpc.h>
#pragma comment(lib, "Rpcrt4.lib")
#include <wx/msw/registry.h>
#ifdef __WXDEBUG__
// To find memory leaks, add _CrtSetBreakAlloc(int memoryBlock)
// just at the beginning of MiniLaunchBar::OnInit
#define _CRTDBG_MAP_ALLOC
#include <crtdbg.h>
#endif // __WXDEBUG__
#endif // __WINDOWS__

#include <wx/treebase.h>
#include <wx/msgdlg.h>
#include <wx/object.h>
#include <wx/generic/dirctrlg.h>
#include <wx/progdlg.h>
#include <wx/treectrl.h>
#include <wx/checkbox.h>
#include <wx/filesys.h>
#include <wx/combobox.h>
#include <wx/imaglist.h>
#include <wx/stdpaths.h>
#include <wx/file.h>
#include <wx/bitmap.h>
#include <wx/cmdline.h>
#include <wx/notebook.h>
#include <wx/colour.h>
#include <wx/taskbar.h>
#include <wx/menu.h>
#include <wx/menuitem.h>
#include <wx/process.h>
#include <wx/timer.h>
#include <wx/log.h>
#include <wx/icon.h>
#include <wx/display.h>
#include <wx/statbmp.h>
#include <wx/snglinst.h>
#include <wx/cmdline.h>
#include <wx/hashmap.h>
#include <wx/event.h>
#include <wx/list.h>
#include <wx/arrstr.h>
#include <wx/sysopt.h>
#include <wx/dynarray.h>
#include <wx/cursor.h>
#include <wx/datetime.h>
#include <wx/dataobj.h>
#include <wx/filename.h>
#include <wx/dnd.h>
#include <wx/listctrl.h>
#include <wx/dcbuffer.h>
#include <wx/font.h>
#include <wx/busyinfo.h>
#include <wx/artprov.h>
#include <wx/dir.h>
#include <wx/clntdata.h>
#include <wx/dirdlg.h>
#include <wx/filedlg.h>
#include <wx/mimetype.h>
#include <wx/stopwatch.h>
#include <wx/url.h>
#include <wx/sstream.h>
#include <wx/protocol/http.h>
#include <wx/stattext.h>

#include <math.h>
#include <vector>
#include <boost/shared_ptr.hpp>

#include "third_party/tinyxml/tinyxml.h"
#include "third_party/simpleini/SimpleIni.h"
#include "third_party/wxMD5/wxMD5.h"

#endif // __MiniLaunchBarPrecompiled_H