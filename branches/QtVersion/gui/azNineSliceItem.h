/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#ifndef AZNINESLICEITEM_H
#define AZNINESLICEITEM_H

#include <azGraphicsItem.h>
#include <azNineSlicePainter.h>

class azNineSliceItem : public azGraphicsItem {

public:

  azNineSliceItem();
  void loadBackgroundImage(QString backgroundFilePath);
  void paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget);

private:

  azNineSlicePainter nineSlicePainter_;
  QString backgroundFilePath_;

};

#endif // AZNINESLICEITEM_H
