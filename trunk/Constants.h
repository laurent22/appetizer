/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include "stdafx.h"

#ifndef __Constants_H
#define __Constants_H

const wxString APPLICATION_NAME = _T("Appetizer");
const wxColor MASK_COLOR = wxColor(255, 0, 255);
const wxString DATA_FOLDER_NAME = _T("Data");
const wxString SETTING_FILE_NAME = _T("Settings.xml");
const wxString FOLDER_ITEMS_FILE_NAME = _T("FolderItems.xml");
const wxString SETTING_FOLDER_NAME = _T("Settings");
const wxString SKIN_FOLDER_NAME = _T("Skin");
const wxString LOCALES_FOLDER_NAME = _T("Locales");
const wxString ICONS_FOLDER_NAME = _T("Icons");
const wxString HELP_FOLDER_NAME = _T("Help");
const wxString PLUGINS_FOLDER_NAME = _T("Plugins");
const wxString TOOLS_FOLDER_NAME = _T("Tools");
const wxString WINDOW_FILE_NAME = _T("Window.xml");
const wxString HELP_FILE_NAME = _T("Appetizer.chm");
const wxString ICON_CACHE_FOLDER_NAME = _T("IconCache");
const wxString SKIN_FILE_NAME = _T("Skin.xml");
const wxString DEFAULT_SKIN = _T("Default");
const wxString CHECK_VERSION_URL = _T("http://app.etizer.org/VersionInfo.xml");
const wxString ISO_DATE_FORMAT = _T("%Y-%m-%d %H:%M:%S");

const int SHELL32_ICON_INDEX_MY_COMPUTER = 15;
const int SHELL32_ICON_INDEX_MY_NETWORK = 17;
const int SHELL32_ICON_INDEX_CONTROL_PANEL = 21;
const int SHELL32_ICON_INDEX_RECYCLE_BIN = 31;
const int SHELL32_ICON_INDEX_DESKTOP = 34;
const int SHELL32_ICON_INDEX_EXPLORER = 45;
const int SHELL32_ICON_INDEX_SEARCH = 22;
const int SHELL32_ICON_INDEX_MY_DOCUMENTS = 126;
const int SHELL32_ICON_INDEX_MY_PICTURES = 127;
const int SHELL32_ICON_INDEX_MY_MUSIC = 128;
const int SHELL32_ICON_INDEX_MY_VIDEOS = 129;
const int SHELL32_ICON_INDEX_FIXED_DRIVE = 8;
const int SHELL32_ICON_INDEX_CDROM = 11;
const int SHELL32_ICON_INDEX_REMOVABLE_DRIVE = 7;
const int SHELL32_ICON_INDEX_FLOPPY_DRIVE = 6;

const int HOT_KEY_ID = 0xBAFF;
const int CHECK_VERSION_DAY_INTERVAL = 2;
const int WINDOW_VISIBILITY_BORDER = 20;
const int MAIN_FRAME_DEFAULT_WIDTH = 300;
const int MAIN_FRAME_DEFAULT_HEIGHT = 80;
const int SMALL_ICON_SIZE = 16;
const int LARGE_ICON_SIZE = 32;
const int EXTRA_LARGE_ICON_SIZE = 48;
const int JUMBO_ICON_SIZE = 256;

#endif // __Constants_H