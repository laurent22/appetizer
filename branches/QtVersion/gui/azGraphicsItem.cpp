/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>
#include <azGraphicsItem.h>

azGraphicsItem::azGraphicsItem() {
    width_ = 100;
    height_ = 100;
}

int azGraphicsItem::width() const {
    return width_;
}

int azGraphicsItem::height() const {
    return height_;
}

void azGraphicsItem::setWidth(int width) {
    if (width == width_) return;
    width_ = width;
    invalidate();
}

void azGraphicsItem::setHeight(int height) {
    if (height == height_) return;
    height_ = height;
    invalidate();
}

QRectF azGraphicsItem::boundingRect() const {
    return QRectF(0, 0, width(), height());
}

void azGraphicsItem::invalidate() {
    update(0, 0, width_, height_);
}
