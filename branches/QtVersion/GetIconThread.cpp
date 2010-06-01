/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>
#include <GetIconThread.h>

using namespace appetizer;


GetIconThread::GetIconThread() {
  iconData_ = NULL;
}


GetIconThread::~GetIconThread() {

}


int GetIconThread::iconSize() const {
  return iconSize_;
}


IconData* GetIconThread::iconData() const {
  return iconData_;
}


void GetIconThread::setIconData(const QString& path, int iconSize) {
  iconPath_ = path;
  iconSize_ = iconSize;
}


void GetIconThread::run() {
  iconData_ = IconUtil::getFolderItemIcon(iconPath_, iconSize_);
  qDebug() << "GetIconThread::run:" << (iconData_ ? "OUI" : "NON");
}