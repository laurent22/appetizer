/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>
#include <azNineSliceItem.h>


azNineSliceItem::azNineSliceItem() {
  
}


void azNineSliceItem::loadBackgroundImage(QString backgroundFilePath) {
  backgroundFilePath_ = backgroundFilePath;
  nineSlicePainter_.loadImage(backgroundFilePath);
  invalidate();
}


void azNineSliceItem::paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget) {
  nineSlicePainter_.drawImage(painter, 0, 0, width(), height());
}