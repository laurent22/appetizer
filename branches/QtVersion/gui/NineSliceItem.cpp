/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>
#include <NineSliceItem.h>
using namespace appetizer;


NineSliceItem::NineSliceItem() {
  
}


NineSlicePainter& NineSliceItem::nineSlicePainter() {
  return nineSlicePainter_;
}


void NineSliceItem::loadBackgroundImage(QString backgroundFilePath) {
  backgroundFilePath_ = backgroundFilePath;
  nineSlicePainter_.loadImage(backgroundFilePath);
  invalidate();
}


void NineSliceItem::paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget) {
  GraphicsItem::paint(painter, option, widget);

  nineSlicePainter_.drawImage(painter, 0, 0, width(), height());
}