/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>
#include <Constants.h>
#include <IconUtil.h>
#include <IconData.h>

#ifndef GetIconThread_H
#define GetIconThread_H
namespace appetizer {

class GetIconThread : public QThread {

public:

    GetIconThread();
    ~GetIconThread();
    void setIconData(const QString& path, int iconSize);
    IconData* iconData() const;
    int iconSize() const;

protected:

  void run();

private:

  QString iconPath_;
  int iconSize_;
  IconData* iconData_;

};

}
#endif // GetIconThread_H
