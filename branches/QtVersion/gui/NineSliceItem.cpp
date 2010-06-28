/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#include <NineSliceItem.h>

using namespace appetizer;

NineSliceItem::NineSliceItem(GraphicsWindow* parentWindow): GraphicsItem(parentWindow) {
  pixmap_ = NULL;
  setCacheMode(QGraphicsItem::DeviceCoordinateCache);
}


NineSliceItem::~NineSliceItem() {
  SAFE_DELETE(pixmap_);
}


NineSlicePainter& NineSliceItem::nineSlicePainter() {
  return nineSlicePainter_;
}


void NineSliceItem::loadBackgroundImage(QString backgroundFilePath) {
  backgroundFilePath_ = backgroundFilePath;
  nineSlicePainter_.loadImage(backgroundFilePath);
  invalidate();
}


void NineSliceItem::resizeEvent() {
  GraphicsItem::resizeEvent();
  SAFE_DELETE(pixmap_);
}


void NineSliceItem::paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget) {
  GraphicsItem::paint(painter, option, widget);

  if (!pixmap_) {
    pixmap_ = new QPixmap(width(), height());
    pixmap_->fill(QColor(0,0,0,0));
    QPainter p(pixmap_);
    nineSlicePainter_.drawImage(&p, 0, 0, width(), height());
    p.end();
  }

  painter->drawPixmap(QPoint(0,0), *pixmap_);
}