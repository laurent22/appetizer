/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#ifndef __Constants_H
#define __Constants_H
namespace appetizer {

const QString APPLICATION_NAME = "Appetizer";
const QColor MASK_COLOR = QColor(255, 0, 255);
const QString DATA_FOLDER_NAME = "Data";
const QString SETTING_FILE_NAME = "Settings.xml";
const QString FOLDER_ITEMS_FILE_NAME = "FolderItems.xml";
const QString SETTING_FOLDER_NAME = "Settings";
const QString SKIN_FOLDER_NAME = "Skin";
const QString LOCALES_FOLDER_NAME = "Locales";
const QString ICONS_FOLDER_NAME = "Icons";
const QString HELP_FOLDER_NAME = "Help";
const QString PLUGINS_FOLDER_NAME = "Plugins";
const QString TOOLS_FOLDER_NAME = "Tools";
const QString PLUGINS_FILE_NAME = "Plugins.xml";
const QString WINDOW_FILE_NAME = "Window.xml";
const QString HELP_FILE_NAME = "Appetizer.pdf";
const QString ICON_CACHE_FOLDER_NAME = "IconCache";
const QString SKIN_FILE_NAME = "Skin.xml";
const QString BASE_SKIN_ASSETS_FOLDER_NAME = "Base";
const QString DEFAULT_SKIN = "Default";
const QString PLUGINS_PREFERENCES_FOLDER_NAME = "PluginPreferences";
const QString CHECK_VERSION_URL = "http://app.etizer.org/VersionInfo.xml";
const QString ISO_DATE_FORMAT = "%Y-%m-%d %H:%M:%S";

const int SHELL32_ICON_INDEX_MY_COMPUTER = 15;
const int SHELL32_ICON_INDEX_MY_NETWORK = 18;
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
const int SHELL32_ICON_INDEX_PRINTERS = 16;
const int SHELL32_ICON_INDEX_NETWORK_CONNECTIONS = 17;

const int HOT_KEY_ID = 0xBAFF;
const int CHECK_VERSION_DAY_INTERVAL = 2;
const int WINDOW_VISIBILITY_BORDER = 20;
const int MAIN_FRAME_DEFAULT_WIDTH = 300;
const int MAIN_FRAME_DEFAULT_HEIGHT = 80;
const int SMALL_ICON_SIZE = 16;
const int MEDIUM_ICON_SIZE = 24;
const int LARGE_ICON_SIZE = 32;
const int EXTRA_LARGE_ICON_SIZE = 48;
const int JUMBO_ICON_SIZE = 256;
const int MIN_BOTTOM_ICON_LABEL_WIDTH = 64;
const int RIGHT_ICON_LABEL_WIDTH = 128;

const int LAUNCHPAD_TEXTBOX_HEIGHT = 24;
const int LAUNCHPAD_TEXTBOX_GAP = 6;
const int LAUNCHPAD_VISIBLE_ICONS = 4;

const int FOLDER_ITEM_TYPE_FILE = 0;
const int FOLDER_ITEM_TYPE_GROUP = 1;
const int FOLDER_ITEM_TYPE_TAB = 2;

}

#endif // __Constants_H