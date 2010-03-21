/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>
#include <GraphicsItem.h>
using namespace appetizer;

GraphicsItem::GraphicsItem() {
    width_ = 100;
    height_ = 100;
}

int GraphicsItem::width() const {
    return width_;
}

int GraphicsItem::height() const {
    return height_;
}

void GraphicsItem::setWidth(int width) {
    if (width == width_) return;
    width_ = width;
    invalidate();
}

void GraphicsItem::setHeight(int height) {
    if (height == height_) return;
    height_ = height;
    invalidate();
}

QRectF GraphicsItem::boundingRect() const {
    return QRectF(0, 0, width(), height());
}

void GraphicsItem::invalidate() {
    update(0, 0, width_, height_);
}
