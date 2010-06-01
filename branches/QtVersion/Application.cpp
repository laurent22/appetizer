/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#include <Application.h>
#include <FilePaths.h>
#include <Style.h>
#include <XmlUtil.h>

#include <PathVariables.h>

using namespace appetizer;

Application::Application(int argc, char *argv[]) : QApplication(argc, argv) {

  #ifdef __WINDOWS__
  osInfo_.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
  BOOL gotInfo = GetVersionEx(&osInfo_);
  if (!gotInfo) {
    osInfo_.dwMajorVersion = 5; // Assume Windows 2000
    osInfo_.dwMinorVersion = 0;
  }  
  #endif // __WINDOWS__

  rootFolderItem_ = NULL;

  FilePaths::InitializePaths();
  FilePaths::CreateSettingsDirectory();

  settings_.Load();

  
  loadFolderItems();

  Style::loadSkinFile("s:\\Docs\\PROGS\\C++\\Appetizer\\source\\branches\\QtVersion\\Data\\Skin\\Default\\Skin.xml");

  mainWindow_ = new MainWindow();
  mainWindow_->show();
  mainWindow_->resize(200, 200);
}


FolderItem* Application::rootFolderItem() const {
  return rootFolderItem_;
}


UserSettings Application::settings() const {
  return settings_;
}


OSVERSIONINFO Application::osInfo() {
  return osInfo_;
}


void Application::loadFolderItems() {
  rootFolderItem_ = FolderItem::createFolderItem(true);

  TiXmlDocument doc("s:\\Docs\\PROGS\\C++\\Appetizer\\source\\branches\\QtVersion\\Data\\Settings\\FolderItems.xml");
  doc.LoadFile(TIXML_ENCODING_UTF8);

  TiXmlElement* root = doc.FirstChildElement("FolderItems");
  if (!root) {
    qWarning() << "Application::loadFolderItems: Could not load XML. No FolderItems element found.";
    return;
  }

  rootFolderItem_ = FolderItem::createFolderItem(true);
  
  for (TiXmlElement* element = root->FirstChildElement(); element; element = element->NextSiblingElement()) {
    QString elementName = QString::fromUtf8(element->Value());

    if (elementName == "FolderItem" || elementName == "appFolderItem") {
      FolderItem* folderItem = FolderItem::createFolderItem();
      folderItem->fromXml(element);

      rootFolderItem_->addChild(folderItem);
    } else if (elementName == "ExcludedPath") {
      //const char* cString = element->GetText();
      //if (!cString) continue;
      //wxString path = wxString::FromUTF8(cString);
      //path.Trim(true).Trim(false);
      //if (path == wxEmptyString) continue;
      //autoAddExclusions_.Add(appFolderItem::ConvertToRelativePath(path));
    } else {
      qWarning() << QString("User::Load: Unknown element: %s").arg(elementName);
    }
  }

  qDebug() << "done";
}


Application* Application::instance() {
  Application* application = static_cast<Application*>(QApplication::instance());
  return application;
}


int Application::getValidIconSize(int requiredIconSize) const {
  #ifdef __WINDOWS__

  int major = osInfo_.dwMajorVersion;
  int minor = osInfo_.dwMinorVersion;

  if (major < 5) {

    // Before Windows 2000
    if (requiredIconSize > 32) return 32;

  } else if (major == 5) {

    if (minor < 1) {
      // Windows 2000
      if (requiredIconSize > 32) return 32;
    } else {
      // Windows XP
      if (requiredIconSize > 48) return 48;
    }

  } else {
    
    // Vista and above
    if (requiredIconSize > 256) return 256;
  }

  #endif // __WINDOWS__

  return requiredIconSize;
}


int Application::getNextValidIconSize(int requiredIconSize) const {
  if (requiredIconSize <= SMALL_ICON_SIZE) return Application::instance()->getValidIconSize(SMALL_ICON_SIZE);
  if (requiredIconSize <= MEDIUM_ICON_SIZE) return Application::instance()->getValidIconSize(MEDIUM_ICON_SIZE);
  if (requiredIconSize <= LARGE_ICON_SIZE) return Application::instance()->getValidIconSize(LARGE_ICON_SIZE);
  if (requiredIconSize <= EXTRA_LARGE_ICON_SIZE) return Application::instance()->getValidIconSize(EXTRA_LARGE_ICON_SIZE);
  if (requiredIconSize <= JUMBO_ICON_SIZE) return Application::instance()->getValidIconSize(JUMBO_ICON_SIZE);

  return Application::instance()->getValidIconSize(JUMBO_ICON_SIZE);
}